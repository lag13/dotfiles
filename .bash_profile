# To make C-o work, not sure why the terminal driver discards this character
# http://apple.stackexchange.com/questions/3253/ctrl-o-behavior-in-terminal-app
stty discard undef

export GOPATH="$HOME/gocode"
# So C-xC-e will start vim
export EDITOR=vim
# Ignore duplicate history entries.
export HISTCONTROL=ignoredups
export COMPOSER_AUTH_TOKEN="9a911e8aa50ffdf7c9a26ba42446a053c6c81420"

# Restart virtualbox if something is funky
alias restart_vb="sudo /Library/Application\\ Support/VirtualBox/LaunchDaemons/VirtualBoxStartup.sh restart"
# Manage dotfiles in the home directory
alias dotgit='git --git-dir=$HOME/.dotgit/ --work-tree=$HOME'

# Quickly generate a coverage report for the specified package and view it in
# the browser.
cover () {
    t="/tmp/go-cover.$$.tmp"
    go test -covermode=count -coverprofile="$t" "$@" && go tool cover -html="$t" && unlink "$t"
}

# source ~/luceo-docker/docker-script.sh
export PATH="$GOPATH/bin:$PATH"
export PATH="$HOME/.rbenv/shims:$PATH"
export PATH="$PATH:$HOME/tr-testing-and-deployment-scripts/hotpot"
export PATH="/usr/local/php5/bin:$PATH"

if [ ! "$DO_IT_ONLY_ONCE" ]
then
    export DO_IT_ONLY_ONCE=1


    # ssh-add ~/.ssh/id_rsa
    # ssh-add ~/.ssh/deployment_rsa
    # ssh-add ~/.ssh/coreos-dev.pem
    # ssh-add ~/.ssh/CBOneCoreOS.pem
fi

