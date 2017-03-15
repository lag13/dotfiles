CircleCI
========

https://circleci.com/docs/

CircleCI is a tool used for continuous integration. What is continuous
integration you ask? Well I'm not exactly sure. I think its basically the
practice of pushing the code you are working on into the main repository and
then a CI server will run all the unit/functional tests in the code to make
sure everything passes. You could always just run those tests yourself I
guess? But it's probably a good thing that you'll have a machine always do it
for you no matter what happens.

Setting Up a Repository With CircleCI
-------------------------------------

First you login to circleci and go to the dashboard:
https://circleci.com/dashboard. From there you can add a project, find your
github project, and that's almost it! Now whenever you push to your code to
that repository circleci will try and run tests and such.

But how does it know what tests to run? Well, circleci is fairly smart and
will try to detect what language/framework is being used and then call the
appropriate commands to fetch dependencies, run tests, etc... For example, for
go repositories it calls `go get` to fetch dependencies and then `go test
./...` to run tests. But what if you want to do something different? Thats
when you would create the `circle.yml` file. In that file you can specify
exactly the commands that circleci should be running to build, test, etc...
your repository.

Starting Directory
------------------

When I did a `pwd` inside the "test" part of the circle.yml file it outputted:
"/home/ubuntu/tsr-api" so it would appear that your working directory is
always at the root of your repository.

Environment Variables
---------------------

Circlci defines some environment variables off the bat:
https://circleci.com/docs/environment-variables/. You can define your own
environment variables in the machine: environment: section. It seems that
environment variables CAN be defined in terms of variables declared previously
for example:

```
machine:
  environment:
    GOPATH: $HOME/.go_project
    PROJECT_PATH: $GOPATH/src/github.com/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME
```

I think what this environment directive is really doing is adding `export`
statements to the ~/.circlerc file. This file gets sourced before each command
gets run.

SSH
---

You can configure a build so you can ssh into it. This is useful for debugging
something (for instance you want to see with your own eyes the state of the
machine). I think before ssh works you need to add your ssh public key.

build_dir
---------

Set this option like this:

```
general:
  build_dir:
    directory/to/cd/to
```

Now every command that runs will first cd to
~/$CIRCLE_PROJECT_USERNAME/directory/to/cd/to. I'm not sure what this looks
like on their end. I was thinking that maybe it would add a command to
~/.circlerc like this:

```
echo 'cd "$PROJECT_PATH"' >> ~/.circlerc
```

But that doesn't seem to be the case. I noticed that the error in circleci
was:

```
/opt/circleci/.rvm/scripts/extras/bash_zsh_support/chpwd/function.sh: line 5: cd: /home/ubuntu/scalp/../../..//home/ubuntu/.go_project/src/github.com/cbdr/scalp/: No such file or directory
```

And I did manage to find that file when I was ssh'ed into the machine. In the
directory of that file there was a README that said:

```
# Bash support for Zsh like chpwd hook

Implemented based on the description from
http://zsh.sourceforge.net/Doc/Release/Functions.html#Hook-Functions
```

Yeah, I don't really know whats going on... I guess its not too important that
I do understand but I feel like it would be nice to know how they make it so
each command gets run in a new directory.

Run In The Background
---------------------

I guess you're not supposed to just put '&' at the end of a command to run it
in the background, you are supposed to do this:
https://circleci.com/docs/background-process/

Debugging With a Browser
------------------------

Turns out that you can use a browser to debug circleci. So you can get
a view into what is actually happening:
https://circleci.com/docs/1.0/browser-debugging