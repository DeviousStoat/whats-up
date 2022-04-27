module main

fn test_latest_tag_empty_returns_none() {
	tags := ''
	got := get_latest_tag(tags) or { 'None' }
	assert got == 'None'
}

fn test_latest_tag() ? {
	tags := '\nv0.0.1\nv0.0.2\nv0.0.3'
	assert get_latest_tag(tags) ? == 'v0.0.3'
}

fn test_org_repo_https() ? {
	url := 'https://github.com/my_org/my_repo.git'
	org, repo := org_repo(url) ?
	assert org == 'my_org'
	assert repo == 'my_repo'
}

fn test_org_repo_ssh() ? {
	url := 'git@github.com:my_org/my_repo.git'
	org, repo := org_repo(url) ?
	assert org == 'my_org'
	assert repo == 'my_repo'
}

fn test_org_repo_empty() {
	url := ''
	org, repo := org_repo(url) or { 'None', 'None' }
	assert org == 'None'
	assert repo == 'None'
}

fn test_pr_nbrs_empty() {
	log := ''
	nbrs := get_pr_nbrs(log)
	assert nbrs == []
}

fn test_pr_nbrs_filter_dependabot_prs() {
	log := 'Merge pull request #05\nSome commit\nMerge pull request #07 from dependabot'
	nbrs := get_pr_nbrs(log)
	assert nbrs == [5]
}

fn test_pr_nbrs() {
	log := 'Merge pull request #05 from some_guy\nSome commit\nMerge pull request #128 from some_other_guy'
	nbrs := get_pr_nbrs(log)
	assert nbrs == [5, 128]
}
