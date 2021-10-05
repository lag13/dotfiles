# dotfiles
I'm using the strategy outlined here to manage my dotfiles:
https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/

## New Machine Setup
Run these commands to set up a new machine. Note the `dotgit` command is an
alias in `.bash_profile`:

```bash
git clone --bare https://github.com/lag13/dotfiles.git $HOME/.dotgit
alias dotgit='/usr/bin/git --git-dir=$HOME/.dotgit/ --work-tree=$HOME'
dotgit config --local status.showUntrackedFiles no
dotgit checkout

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew install --cask emacs

# If I use a standalone terminal application (as opposed to doing something like running a terminal emulator via emacs) I prefer using iterm2 on mac instead of the native terminal app because I had one experience surrounding the history feature of bash where what I was observing was NOT lining up with what the bash source code said should be happening (I was expecting that when I ran more than HISTSIZE commands, it would overwrite the ~/.bash_history file INSTEAD OF appending to it: https://unix.stackexchange.com/questions/226214/why-does-history-not-overwrite-but-append-when-histappend-is-set-to-off-in-bash/428208#428208). Turns out, the terminal mac app messes with the shell a bit (https://apple.stackexchange.com/a/219825) and that just made me not trust it because maybe it messes with other things and I'd prefer it if my tools don't mess with the behavior of other tools unless I explicitly say so.
brew install --cask iterm2

# For compiling vterm: https://github.com/akermu/emacs-libvterm#installation
brew install cmake

brew install go

go get golang.org/x/tools/cmd/goimports

go get github.com/rogpeppe/godef

brew install --cask docker

brew install graphviz

brew install warrensbox/tap/tfswitch

brew install kubernetes-cli

brew install awscli

brew install rclone

# When I tried to install clojure it failed and instructed me to install java first
brew install openjdk@8

brew install leiningen

brew install plantuml

brew install jq

# For emacs to do spellcheck: https://emacs.stackexchange.com/questions/19175/where-is-ispell
brew install ispell

brew install ripgrep

Ubuntu:

sudo apt install -y vim

sudo apt install -y emacs

sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs26

remac capslock to control: https://askubuntu.com/questions/969053/map-caps-lock-to-control-on-ubuntu-17-10?rq=1

install go https://golang.org/dl/ (using apt by default does not install the latest version)
I guess I could also do this: https://github.com/golang/go/wiki/Ubuntu

sudo snap install slack --classic

https://www.dropbox.com/install-linux

To keep time stuff in sync. When I originally installed ubuntu things worked fine, I shut the computer and the next day when
I opened it the year was 2025 for some reason. When I turned it off and on again things were fine but Summit tried to help
me debug he said I should install this so I did.
sudo apt install -y ntp

For VPN stuff: https://www.infradead.org/openconnect/
sudo apt install -y openconnect

to connect to VPN you'll run something like openconnect --user=asdf --servercert sha256:lsklldfsjkldfslkjfalkjasdfljk --script=/usr/share/vpnc-scripts/vpnc-script vpn.server.thingy

Note that on ubuntu the --script parameter defaults to the correct file already but I like the idea of specifying it.

the snap binary that I originally installed failed: https://stackoverflow.com/questions/54406076/terraform-init-fails-git-must-be-available-and-on-the-path so I downloaded the executable from hashicorp, moved it to /usr/bin and that worked
sudo snap install terraform

to install ansible: https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html#latest-releases-via-apt-ubuntu
What is a PPA?

Need to hit "TAB" to be able to click the 
sudo apt install virtualbox virtualbox-ext-pack

sudo apt install vagrant

sudo apt install awscli

install kubectl version 1.10.11 https://kubernetes.io/docs/tasks/tools/install-kubectl/

sudo apt install docker.io
then follow these instructions https://docs.docker.com/install/linux/linux-postinstall/
to run docker as non-root. Or these:
https://askubuntu.com/questions/477551/how-can-i-use-docker-without-sudo
sudo groupadd docker
sudo usermod -aG docker $USER
newgrp docker

https://clojure.org/guides/getting_started#_installation_on_linux
https://leiningen.org/

Should also install bfg: https://rtyley.github.io/bfg-repo-cleaner/
for cleaning bad git history.
```

## TODO
- script more stuff
- on ubuntu it looks like .bashrc is supposed to be configured by the
  system for non-login shells only so I think I'm gonna remove .bashrc
  from this repo. Need to look into that more though.
- See how to run the docker thingy automatically when I start this
  machine.
- There are some differences between ubuntu and mac which are a little
  annoying. Most bothersome is how I run apt as sudo in ubuntu which
  causes binaries to get installed in /usr/bin but on mac I use brew
  which installs them in /usr/local/bin (or it creates a symlink there
  or something). Also, my PATH is super messy at the moment and it got
  me thinking that I wonder if it would be useful to try and have path
  point to fewer places and have a couple custom shims? Then again
  maybe I'd just be moving the complication into the shims. Or maybe
  everything I install should just go into my home directory by
  default and have PATH look there first? Part of me likes that
  isolation (not that I have any use for it I suppose). One of the
  problems for example is that I configure the editor for git commit
  messages to be the vim binary from /usr/local/vim (where brew would
  install it) but on ubuntu it would not get installed there. Not a
  huge deal (I just move it to /usr/local/bin) but its still kind of
  annoying.
- It feels like a lot of the time installing things on ubuntu might
  consist of downloading a tarball, extracting it, and putting the
  binary into the appropriate location. I feel like that should be
  scripted more somehow?

## Update Dotfiles
The dotfiles are now in the home directory where they should be. Use the
`dotgit` to check if any files have changed and commit/push/... just like
normal. New files will not be shown in the output of `dotgit status` because
of the `showUntrackedFiles` option. This is to reduce clutter.
