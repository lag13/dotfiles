Github
======

API
---

https://developer.github.com/v3/

Returns the contents of a particular file. I believe there are other ways but
this is one:

   curl -H "Accept: application/vnd.github.v3.raw" https://api.github.com/repos/lag13/dotfiles/contents/README.md
   
## Clone all repos
```
#!/bin/bash

page=0
while true
do
    clone_strs=$(curl --silent --user 'lag13:OAUTHTOKEN \
		      -H "Accept: application/vnd.github.v3+json" \
		      'https://api.github.com/orgs/Guaranteed-Rate/repos?per_page=100&page='$page | \
		     jq --raw-output '.[].ssh_url')
    num_repos=$(echo $clone_strs | tr ' ' '\n' | wc -l)
    echo "there are $num_repos to clone"
    for c in $clone_strs
    do
	echo "cloning $c"
	git clone $c
	sleep 1
    done
    if [ $num_repos -lt 100 ]
    then
	break
    fi
    page=$((page+1))
done
```

Faster Pull Requests
--------------------

When making pull requests for release-4.0 for the luceo repository,
the website sometimes slows down to a crawl and it actually takes a
while until I can make the PR. So instead of creating the PR through
the github interface, use this link structure to do it:
https://github.com/cbdr/luceo/compare/release-4.0...add-test-dbs-to-repo40?expand=1
