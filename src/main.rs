#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

use anyhow::{Context, Result};
use cmd_lib::run_fun;
use futures::future::try_join_all;
use octocrab::{models::pulls::PullRequest, OctocrabBuilder};
use std::{
    fs::{self, File},
    io::Write,
    path::PathBuf,
    collections::HashSet,
};
use structopt::StructOpt;

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
        default_value = "release_notes.md",
        help = "Path to the output file"
    )]
    output: PathBuf,

    #[structopt(
        parse(from_os_str),
        default_value = "release_notes.tmpl",
        help = "Path to the template file"
    )]
    template: PathBuf,
}

const SUPPORTED_KEYWORDS: &[&str] = &[
    "@title",
    "@url",
    "@user_login",
    "@number",
    "@body",
    "@contributors",
];
const EMPTY_PLACEHOLDER: &str = "<NO VALUE>";

fn pr_to_tmpl(pr: &PullRequest, tmpl: &str) -> String {
    let mut output = tmpl.to_owned();
    let pr_nb = pr.number.to_string();
    let empty_string = EMPTY_PLACEHOLDER.to_owned();

    for kw in SUPPORTED_KEYWORDS {
        let f = match *kw {
            "@contributors" => continue,
            "@url" => &pr.url,
            "@number" => &pr_nb,
            "@body" => pr.body.as_ref().unwrap_or(&empty_string),
            "@title" => pr.title.as_ref().unwrap_or(&empty_string),
            "@user_login" => pr
                .user
                .as_ref()
                .map_or(EMPTY_PLACEHOLDER, |user| &user.login),
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

/// Get the merged PR numbers from `tag` to `develop`
fn pr_nbrs_from(tag: &str) -> Result<Vec<u64>> {
    run_fun!(git log --pretty=format:%s $tag..develop)?
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
    let opt = Opt::from_args();
    std::env::set_current_dir(&opt.project_path)?;

    let tag = latest_tag()?;

    let github_handle = OctocrabBuilder::new()
        .personal_token(std::env::var("GH_TOKEN")?)
        .build()?;
    let get_pull = github_handle.pulls("argyle-systems", "argyle-loom");

    // failing if one of the request failed as we need the whole data
    let prs = try_join_all(pr_nbrs_from(&tag)?.iter().map(|pr_nb| get_pull.get(*pr_nb))).await?;

    // let prs_by_user = prs
    //     .into_iter()
    //     .into_group_map_by(|pr| {
    //         pr.user
    //             .as_ref()
    //             .map_or("no login".to_owned(), |user| user.login.clone())
    //     });
    let template = fs::read_to_string(opt.template)?;
    let mut output = File::create(opt.output)?;

    let contributors = prs
        .iter()
        .map(|pr| pr.user.as_ref().map_or(EMPTY_PLACEHOLDER, |user| &user.login))
        .map(|user| format!("* {user}"))
        .collect::<HashSet<String>>()
        .into_iter()
        .collect::<Vec<String>>()
        .join("\n");

    let template = template.replace("@contributors", &contributors);

    for pr in prs {
        writeln!(output, "{}", pr_to_tmpl(&pr, &template))?;
    }

    Ok(())
}
