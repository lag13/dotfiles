# To make C-o work, not sure why the terminal driver discards this character
# http://apple.stackexchange.com/questions/3253/ctrl-o-behavior-in-terminal-app
stty discard undef
# Allows us actually use the C-s key (which does the opposite of C-r)
# instead of having C-s stop all terminal output.
stty -ixon

# TODO: I'm confused about why this function doesn't seem to wipe out
# the $? variable. Because after the prompt is set I guess I would
# have expected $? to get reset to 0 or someting.
function dynamic_prompt() {
    curr_exit=$?
    git_branch="$(git branch --show-current 2>/dev/null || echo "")"
    if [ "$git_branch" != "" ]
    then
	# TODO: Also show a different color based on if there are no
	# changes?
	echo -n " [git: $git_branch]"
    fi
    if [ "$curr_exit" != "0" ]
    then
	# https://linuxcommand.org/lc3_adv_tput.php
	echo -n " $(tput setaf 1)[exit code: $curr_exit]$(tput sgr0)"
    fi
}
export PS1='\u \w$(dynamic_prompt)
$ '

# ignorespace makes it so commands leading with a space do not get
# saved into the history and erasedups will make it so any time a
# command is executed, duplicates of that command are deleted from the
# in memory history which I like because I don't primarily use the
# history as a log of all commands, I just want to remember the unique
# ones I've run so I can run them again when I inevitably have to.
export HISTCONTROL=ignorespace:erasedups
export HISTFILESIZE=5000
export HISTSIZE=5000
# Setting the histappend option when HISFILESIZE and HISTSIZE are
# equal is basically a no op because the logic is: if we have executed
# LESS than HISTSIZE commands OR histappend is set, then append to
# history (and then truncate to HISTFILESIZE) OTHERWISE overwrite the
# history file:
# https://unix.stackexchange.com/questions/226214/why-does-history-not-overwrite-but-append-when-histappend-is-set-to-off-in-bash/428208#428208
# So, if HISTFILESIZE == HISTSIZE and we execute more than HISTSIZE
# commands, an append then truncate will have the same effect as a
# straight overwrite anyway. Even so, I like to set histappend to
# better guarantee that history doesn't get accidentally overwritten
# (like if I set HISTSIZE < HISTFILESIZE for whatever reason) OR maybe
# here's some other weird logic I didn't understand in the bash code.
# Also (side note) the bash documentation (man bash) on the histappend
# option is very un-nuanced with regards to the actual logic:
#
# histappend
#       If set, the history list is appended to the  file  named
#       by  the  value  of  the HISTFILE variable when the shell
#       exits, rather than overwriting the file.
#
# That's kind of unfortunate I feel since the actual behavior is more
# nuanced. Maybe it doesn't matter though :shrug:
shopt -s histappend

# Manage dotfiles in the home directory
alias dotgit='git --git-dir=$HOME/.dotgit/ --work-tree=$HOME'

export GOPATH=$HOME
export PATH="$GOPATH/bin:$PATH"

work_stuff="$HOME/work-customizations"
if [ -f "$work_stuff" ]
then
    source "$HOME/work-customizations"
fi
