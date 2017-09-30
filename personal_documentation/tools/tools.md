Tools
=====

### devd

- https://github.com/cortesi/devd
- https://corte.si/posts/devd/intro/index.html

Command line http server for developers. The only thing I used about it was
it's "live reload" feature which injects some script into each html page so
that if the file serving that html page changes, you'll see the change
instantly.

### ngrok

- https://ngrok.com

"I want to expose a local server behind a NAT or firewall to the
internet.". In other words, creates a publically available URL for
your server. Very nifty. Doing a simple `ngrok http 8080` will create
a publically available URL which points to a server running at
localhost:8080.

### entr

- http://entrproject.org
- https://bitbucket.org/eradman/entr/

"Run arbitrary commands when files change.". Pretty generic and adhering very
well to the unix philosophy which makes it very useful. One common thing is to
run tests when source code changes. Example which echo's "hey" every time one
of the listed files changes: `find . | entr echo "hey"`.

### tmux

- https://tmux.github.io
- https://github.com/tmux/tmux

A "terminal multiplexer". Basically just a program which manages your terminal
with the goal of having things be more keyboard driven. You start up your
terminal (like iterm) then start the tmux program. Most anything you can do
through typing, you can also script.

I learned my initial tmux stuff through this guy:
http://minimul.com/teaches/tmux.
