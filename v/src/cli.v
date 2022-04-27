module main

import flag
import os

const gh_token_env = 'GH_TOKEN'

struct Params {
	project_path  string
	output_path   string
	template_path string
	tag           string
	branch        string
	gh_token      string
	verbose       bool
}

fn parse_args() ?Params {
	mut fp := flag.new_flag_parser(os.args)

	fp.application('velease')
	fp.version('v0.0.1')
	fp.limit_free_args(0, 0) ?
	fp.description('Release notes generator')
	fp.skip_executable()

	proj_path := fp.string('proj_path', `p`, '.', 'Path to the git project. [default: `.`]')
	output_path := fp.string('output', `o`, 'velease_notes.md', 'Path to output file. [default:`velease_notes.md`]')
	template_path := fp.string('template', `t`, 'velease_notes.tmpl', 'Path to the template file. [default: `velease_notes.tmpl]')
	tag := fp.string_opt('tag', `g`, 'Tag from which to gather PRs [default: latest tag]') or {
		git_tags := get_git_tags(proj_path) ?
		get_latest_tag(git_tags) or {
			return error('No tags were found in `$proj_path` while trying to get the latest tag')
		}
	}
	branch := fp.string('branch', `b`, 'develop', 'Branch that will be released [default: `develop`]')
	gh_token := fp.string_opt('gh_token', `k`, 'Github token to use to communicate with github API [default: env var `\$$gh_token_env`]') or {
		os.getenv_opt(gh_token_env) or {
			return error('No `gh_token` provided nor `GH_TOKEN` env var set')
		}
	}
	verbose := fp.bool('verbose', `v`, false, 'Want me to talk?')

	fp.finalize() or {
		println('$fp.usage()\n')
		return err
	}
	return Params{proj_path, output_path, template_path, tag, branch, gh_token, verbose}
}
