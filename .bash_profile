echo ".bash_profile"

source ~/docker-script.sh

export GOPATH="$HOME/gocode"
export PATH="$GOPATH/bin:$PATH"
export PATH="$HOME/.rbenv/shims:$PATH"
export PATH="$PATH:$HOME/tr-testing-and-deployment-scripts/hotpot"
export PATH="/usr/local/php5/bin:$PATH"

# Restart virtualbox if something is funky
alias restart_vb="sudo /Library/Application\\ Support/VirtualBox/LaunchDaemons/VirtualBoxStartup.sh restart"
# So C-xC-e will start vim
EDITOR=vim
# To make C-o work, not sure why the terminal driver discards this character
# http://apple.stackexchange.com/questions/3253/ctrl-o-behavior-in-terminal-app
stty discard undef
# Ignore duplicate history entries
export HISTCONTROL=ignoredups
