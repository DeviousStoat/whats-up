module main

fn main() {
	params := parse_args() or {
		println(err)
		return
	}
	git_url := get_git_url(params.project_path) or {
		println(err)
		return
	}
	org, repo := org_repo(git_url) or {
		println('`$git_url` doesn\'t seem like a valid git url')
		return
	}
	logs := get_git_commits_between(params.project_path, params.tag, params.branch) or {
		println(err)
		return
	}
	pr_nbrs := get_pr_nbrs(logs)
	prs := get_prs(org, repo, pr_nbrs, params.gh_token) or {
		println(err)
		return
	}

	template := get_template(params.template_path) or {
		println(err)
		return
	}

	write_prs_to_output(prs, template, params.output_path) or {
		println(err)
		return
	}

	if params.verbose {
		println('Output has been written to $params.output_path')
	}
}
