# dotfiles
I'm using the strategy outlined here to manage my dotfiles:
https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/

## New Machine Setup
Run these commands to set up a new machine. Note the `dotgit` command is an
alias in `.bash_profile`:

```
git clone --bare https://github.com/lag13/dotfiles.git $HOME/.dotgit
alias dotgit='/usr/bin/git --git-dir=$HOME/.dotgit/ --work-tree=$HOME'
dotgit config --local status.showUntrackedFiles no
dotgit checkout

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew cask install emacs

brew install vim

brew install go

go get golang.org/x/tools/cmd/goimports

go get github.com/rogpeppe/godef

brew cask install docker

brew install terraform

brew install kubernetes-cli

brew install awscli

# When I tried to install clojure it failed and instructed me to install java first
brew cask install java

brew install clojure

brew install leiningen

brew install plantuml

Ubuntu:

sudo apt install -y vim

sudo apt install -y emacs

remac capslock to control: https://askubuntu.com/questions/969053/map-caps-lock-to-control-on-ubuntu-17-10?rq=1

install go https://golang.org/dl/ (using apt by default does not install the latest version)
I guess I could also do this: https://github.com/golang/go/wiki/Ubuntu
```

## TODO
- script more stuff
- on ubuntu it looks like .bashrc is supposed to be configured by the
  system for non-login shells only so I think I'm gonna remove .bashrc
  from this repo. Need to look into that more though.

## Update Dotfiles
The dotfiles are now in the home directory where they should be. Use the
`dotgit` to check if any files have changed and commit/push/... just like
normal. New files will not be shown in the output of `dotgit status` because
of the `showUntrackedFiles` option. This is to reduce clutter.
