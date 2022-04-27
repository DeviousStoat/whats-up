module main

import os

fn get_git_url(path string) ?string {
	cwd := os.getwd()
	os.chdir(path) ?
	result := os.execute_or_exit('git remote get-url origin')
	os.chdir(cwd) ?
	return result.output.trim_space()
}

fn get_git_tags(path string) ?string {
	cwd := os.getwd()
	os.chdir(path) ?
	result := os.execute_or_exit('git tag --sort=v:refname')
	os.chdir(cwd) ?
	return result.output.trim_space()
}

fn get_git_commits_between(path string, tag string, branch string) ?string {
	cwd := os.getwd()
	os.chdir(path) ?
	result := os.execute_or_exit('git log --pretty=format:%s ${tag}..$branch')
	os.chdir(cwd) ?
	return result.output.trim_space()
}

// latest_tag takes a sorted new line separated list of tags
// and return the last one or none if empty
fn get_latest_tag(tags string) ?string {
	l_tags := tags.split_into_lines().filter(it != '')
	if l_tags.len == 0 {
		return none
	}
	return l_tags.last()
}

// org_repo gets the organisation and the repo from a git url (https or ssh)
// `https://github.com/my_org/my_repo.git` -> `my_org`, `my_repo`
// `git@github.com:my_org/my_repo.git` -> `my_org`, `my_repo`
fn org_repo(url string) ?(string, string) {
	without_repo, repo := url.all_before_last('/').trim_space(), url.all_after_last('/').trim_space().trim_string_right('.git')
	if without_repo == url.trim_space() {
		return none
	}

	org := if without_repo.starts_with('https') {
		without_repo.all_after_last('/')
	} else {
		without_repo.all_after_last(':')
	}

	if org == without_repo {
		return none
	}

	return org, repo
}

// pr_nbrs_from_to returns a list of PR numbers from a git log output
// the log output must have merge commits including PR numbers
fn get_pr_nbrs(log string) []int {
	merge_commit := 'Merge pull request #'
	// TODO: maybe use `strconv.atoi` as `.int()` simply returns `0` if not an int
	return log.split_into_lines().filter(it.starts_with(merge_commit) && !it.contains('dependabot')).map(it.trim_string_left(merge_commit).all_before(' ').int())
}
