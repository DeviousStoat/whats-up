use anyhow::{Context, Result};
use cmd_lib::run_fun;
use futures::future::try_join_all;
use itertools::Itertools;
use octocrab::OctocrabBuilder;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
struct Opt {
    #[structopt(
        parse(from_os_str),
        default_value = ".",
        help = "Path to the git project"
    )]
    path: PathBuf,
}

/// Get the latest tag from the repo
fn latest_tag() -> Result<String> {
    run_fun!(git tag --sort=v:refname)?
        .trim()
        .split('\n')
        .last()
        .map(|t| t.to_string())
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
    std::env::set_current_dir(&opt.path)?;

    let tag = latest_tag()?;

    let github_handle = OctocrabBuilder::new()
        .personal_token(std::env::var("GH_TOKEN")?)
        .build()?;
    let get_pull = github_handle.pulls("argyle-systems", "argyle-loom");

    // failing if one of the request failed as we need the whole data
    let prs_by_user = try_join_all(pr_nbrs_from(&tag)?.iter().map(|pr_nb| get_pull.get(*pr_nb)))
        .await?
        .into_iter()
        .into_group_map_by(|pr| {
            pr.user
                .as_ref()
                .map_or("no login".to_owned(), |user| user.login.clone())
        });

    println!("{:#?}", prs_by_user);

    Ok(())
}
