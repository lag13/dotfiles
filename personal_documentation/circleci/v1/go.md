Go
==

I was trying to get circleci set up for my scalp repository and was having
some trouble so I thought I'd write down what was going on.

Go is very... opinionated about where code should be. All code has to live in
$GOPATH/src and it'll probably be nested deeper than that under
"github.com/organization/reponame". By default circleci in the "dependencies"
section will run something called "Setting up Go environment" then it runs `go
get` and `go build`. This document outlines more specifically what circle does
https://discuss.circleci.com/t/overriding-go-inference-in-the-dependencies-phase/660.
Since you oftentimes might not want this default behavior, you'll have to do
what they do in that step:

1. `mkdir -p $HOME/.go_project/src/github.com/$CIRCLE_PROJECT_USERNAME`
2. `ln -fs $HOME/$CIRCLE_PROJECT_REPONAME
   $HOME/.go_project/src/github.com/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME`
3. Add that symlink to the GOPATH. Apparently you could do `echo 'export
   GOPATH=$GOPATH:$HOME/.go_project' >> ~/.circlerc` (I think all commands run
   in a new shell and that shell probably loads `~/.circlerc` on startup) or
   maybe more clear you could define `GOPATH` in the `machine:` `environment:`
   section of circle.yml.
4. Prefix each go command that needs it with `cd`ing to the symlink.
   Originally I thought you might be able to specify the build_dir option and
   it did kind of work but it seems wrong because until the directory gets
   created then the cd commands that circlci tries to do will error out.

Here's an example circle.yml from hotpot-delete-customer-system. Notice in the
machine section we set up the `GOPATH`, in the dependencies section we run
commands 1 and 2, and in the test section we have to `cd` before each `go`
command. We don't have to do it for `go fmt` because that just checks that the
code adheres to a certain style, it doesn't need to build anything like `go
test` does.

```
machine:
  services:
    - docker
  environment:
    GOPATH: $HOME/.go_project
    PROJECT_PATH: $HOME/.go_project/src/github.com/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME

dependencies:
  override:
    - mkdir -p $HOME/.go_project/src/github.com/$CIRCLE_PROJECT_USERNAME
    - ln -fs $HOME/$CIRCLE_PROJECT_REPONAME $PROJECT_PATH
    - docker pull centurylink/golang-builder:latest

test:
  override:
    - if [[ -n "$(go fmt ./...)" ]]; then exit 1; fi
    - cd $PROJECT_PATH && go vet $(go list ./... | grep -v 'vendor')
    - cd $PROJECT_PATH && go test -v ./...
    - cd $PROJECT_PATH && ./generate-coverage

deployment:
  staging:
    branch: master
    commands:
      - echo "LABEL GIT_HASH=$CIRCLE_SHA1" >> $HOME/$CIRCLE_PROJECT_REPONAME/Dockerfile
      - docker run -v $(pwd):/src centurylink/golang-builder:latest
      - docker build -t quay.io/cbone/hotpot-delete-customer-system .
      - docker login -e="." -u=$QUAY_USER -p=$QUAY_PASS quay.io
      - docker push quay.io/cbone/hotpot-delete-customer-system
```

Version 1.6
-----------

If you want version 1.6 by default (I'm writing this on 06/21/2016) then go to
Project Settings > Build Environment and choose the OS Trusty to build.
