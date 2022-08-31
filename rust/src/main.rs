#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use anyhow::{Context, Result};
use cmd_lib::run_fun;
use futures::future::try_join_all;
use octocrab::{models::pulls::PullRequest, OctocrabBuilder};
use std::{
    collections::HashSet,
    fs::{self, File},
    io::Write,
    path::PathBuf,
    string::ToString,
};
use structopt::StructOpt;

const SUPPORTED_KEYWORDS: &[&str] = &[
    "@title",
    "@url",
    "@user_login",
    "@number",
    "@body",
    "@html_url",
];
const EMPTY_PLACEHOLDER: &str = "<NO VALUE>";

#[derive(StructOpt, Debug)]
struct Opt {
    #[structopt(
        parse(from_os_str),
        default_value = ".",
        help = "Path to the git project"
    )]
    project_path: PathBuf,

    #[structopt(
        parse(from_os_str),
        short = "o",
        long,
        default_value = "release_notes.md",
        help = "Path to the output file"
    )]
    output: PathBuf,

    #[structopt(
        parse(from_os_str),
        short = "p",
        long,
        default_value = "release_notes.tmpl",
        help = "Path to the template file"
    )]
    template: PathBuf,

    #[structopt(
        short,
        long,
        help = "Tag from which to gather PRs [default: latest tag]"
    )]
    tag: Option<String>,

    #[structopt(
        short,
        long,
        default_value = "develop",
        help = "Branch that will be released"
    )]
    branch: String,
}

fn pr_to_tmpl(pr: &PullRequest, tmpl: &str) -> String {
    let mut output = tmpl.to_owned();
    let empty_string = EMPTY_PLACEHOLDER.to_owned();
    let pr_nb = pr.number.to_string();

    // `url` is the api url
    // `html_url` will return the classic url found in browser
    let html_url = pr
        .html_url
        .as_ref()
        .map_or(empty_string.clone(), ToString::to_string);

    for kw in SUPPORTED_KEYWORDS {
        let f = match *kw {
            "@url" => &pr.url,
            "@html_url" => &html_url,
            "@number" => &pr_nb,
            "@body" => pr.body.as_ref().unwrap_or(&empty_string),
            "@title" => pr.title.as_ref().unwrap_or(&empty_string),
            "@user_login" => pr.user.as_ref().map_or(&empty_string, |user| &user.login),
            _ => unimplemented!(),
        };
        output = output.replace(kw, f);
    }

    output
}

/// Get the latest tag from the repo
fn latest_tag() -> Result<String> {
    run_fun!(git tag --sort=v:refname)?
        .trim()
        .split('\n')
        .last()
        .map(std::string::ToString::to_string)
        .context(format!(
            "no git tags found in {:?}",
            &std::env::current_dir()?
        ))
}

/// Get the org and the repo from a git url (https or ssh)
fn org_repo(url: &str) -> Result<(String, String)> {
    let (org, repo) = url.trim().rsplit_once('/').context(format!(
        "no git origin url found in {:?}",
        &std::env::current_dir()?
    ))?;

    let org = if org.contains("https") {
        org.rsplit_once('/')
    } else {
        org.rsplit_once(':')
    }
    .map(|org| org.1)
    .context(format!(
        "no git origin url found in {:?}",
        &std::env::current_dir()?
    ))?;

    let repo = repo.trim_end_matches(".git");

    Ok((org.to_owned(), repo.to_owned()))
}

/// Get the merged PR numbers from `tag` to `branch`
fn pr_nbrs_from_to(tag: &str, branch: &str) -> Result<Vec<u64>> {
    run_fun!(git log --pretty=format:%s $tag..$branch)?
        .split('\n')
        .filter(|c| c.starts_with("Merge pull request #") && !c.contains("dependabot"))
        .map(|c| {
            c.split_whitespace()
                .nth(3)
                .with_context(|| format!("couldn't find PR number in `{c}`"))
                .and_then(|nb| {
                    nb.trim_start_matches('#')
                        .parse::<u64>()
                        .with_context(|| format!("`{nb}` cannot be parsed as a number"))
                })
        })
        .collect::<Result<Vec<u64>>>()
}

#[tokio::main]
async fn main() -> Result<()> {
    let opt = Opt::from_clap(
        &Opt::clap()
        .long_about(
            &*format!("\
                Will get all the merged PRs from `<tag>` to `<branch>` of the git project at `<project_path>`\n\
                and fill the template file at `<template>` with the corresponding information.\n\
                Must have `GH_TOKEN` env variable set to a personal access github token.\n\n\
                Supported template keywords:\n\
                {:?}\n\n\
                Example template:\n\
                ```\n\
                ## @title [@number](@html_url) by @user_login\n\n\
                @body\n\
                ```",
                SUPPORTED_KEYWORDS
            )
        )
        .get_matches()
    );

    std::env::set_current_dir(&opt.project_path)?;

    let tag = match opt.tag {
        Some(tag) => tag,
        None => latest_tag()?,
    };
    let git_url = run_fun!(git remote get-url origin)?;
    let (org, repo) = org_repo(git_url.trim())?;

    let github_handle = OctocrabBuilder::new()
        .personal_token(std::env::var("GH_TOKEN").context("`GH_TOKEN` env variable is not set")?)
        .build()?;
    let get_pull = github_handle.pulls(org, repo);

    // failing if one of the request failed as we need the whole data
    let prs = try_join_all(
        pr_nbrs_from_to(&tag, &opt.branch)?
            .iter()
            .map(|pr_nb| get_pull.get(*pr_nb)),
    )
    .await?;

    let template = fs::read_to_string(opt.template)?;
    let mut output = File::create(opt.output)?;

    let contributors = prs
        .iter()
        .map(|pr| {
            pr.user
                .as_ref()
                .map_or(EMPTY_PLACEHOLDER, |user| &user.login)
        })
        .map(|user| format!("* {user}"))
        .collect::<HashSet<String>>()
        .into_iter()
        .collect::<Vec<String>>()
        .join("\n");

    writeln!(output, "## Contributors:\n\n{}\n\n", &contributors)?;

    for pr in prs {
        writeln!(output, "{}", pr_to_tmpl(&pr, &template))?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn org_repo_https() {
        let url = "https://github.com/my_org/my_repo.git";
        assert_eq!(
            org_repo(url).unwrap(),
            ("my_org".to_owned(), "my_repo".to_owned())
        );
    }

    #[test]
    fn org_repo_ssh() {
        let url = "git@github.com:my_org/my_repo.git";
        assert_eq!(
            org_repo(url).unwrap(),
            ("my_org".to_owned(), "my_repo".to_owned())
        );
    }
}
