dotfiles
========

I'm using the strategy outlined here to manage my dotfiles:
https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/

New Machine Setup
-----------------

Run these commands to set up a new machine. Note the `dotgit` command is an
alias in `.bash_profile`:

```
git clone --bare git@github.com:lag13/dotfiles.git $HOME/.dotgit
alias dotgit='/usr/bin/git --git-dir=$HOME/.dotgit/ --work-tree=$HOME'
dotgit config --local status.showUntrackedFiles no
dotgit checkout

# TODO: Put these in a script of some sort
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
```

Update Dotfiles
---------------

The dotfiles are now in the home directory where they should be. Use the
`dotgit` to check if any files have changed and commit/push/... just like
normal. New files will not be shown in the output of `dotgit status` because
of the `showUntrackedFiles` option. This is to reduce clutter.
