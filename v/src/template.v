module main

import arrays
import os

const supported_keywords = ['@title', '@url', '@user_login', '@number', '@body']

fn get_template(path string) ?string {
	return os.read_file(path)
}

fn pr_to_template(pr Pr, template string) string {
	mut fmt_template := template

	for kw in supported_keywords {
		with := match kw {
			'@title' { pr.title }
			'@url' { pr.url }
			'@user_login' { pr.user.login }
			'@number' { pr.number.str() }
			'@body' { pr.body }
			else { panic('wowow, map a value to $kw, please') }
		}
		fmt_template = fmt_template.replace(kw, with)
	}

	return fmt_template
}

fn write_prs_to_output(prs []Pr, template string, output_path string) ? {
	mut output_file := os.open_file(output_path, 'w') ?

	prs_by_user := arrays.group_by<string, Pr>(prs, fn (pr Pr) string {
		return pr.user.login
	})
	contributors := prs_by_user.keys().map('* $it').join('\n') + '\n'
	output_file.writeln('# Contributors') ?
	output_file.writeln(contributors) ?

	for pr in prs {
		to_write := pr_to_template(pr, template)
		output_file.writeln(to_write) ?
	}
}
