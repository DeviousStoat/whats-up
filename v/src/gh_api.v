module main

import json
import net.http

const (
	base_url      = 'https://api.github.com'
	accept_header = 'application/vnd.github.v3+json'
)

struct User {
	login string
}

struct Label {
	name        string
	description string
	color       string
}

struct Pr {
	title  string
	number int
	url    string
	user   User
	body   string
	labels []Label
}

fn get_header(gh_token string) http.Header {
	return http.new_header_from_map({
		.authorization: 'Bearer $gh_token',
		.accept:        accept_header,
	})
}

// get_pr returns a `Pr` from the github API
fn get_pr(org string, repo string, pr_nb int, gh_token string) ?Pr {
	request := http.Request{
		method: .get
		url: '$base_url/repos/$org/$repo/pulls/$pr_nb'
		header: get_header(gh_token)
	}

	resp := request.do() ?
	return json.decode(Pr, resp.text) or {}
}

// get_prs concurrently returns a list of `Pr` from the github API
fn get_prs(org string, repo string, pr_nbs []int, gh_token string) ?[]Pr {
	mut prs := []Pr{}

	mut threads := []thread ?Pr{}
	for pr_nb in pr_nbs {
		threads << go get_pr(org, repo, pr_nb, gh_token)
	}

	// TODO: not sure how this work, might not be the best way to do this
	for thread in threads {
		prs << thread.wait() ?
	}

	return prs
}
