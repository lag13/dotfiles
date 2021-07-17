# Tools

Useful tools (anything from a bash command to a binary that needs to
be downloaded) for programming.

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

### BFG

- https://rtyley.github.io/bfg-repo-cleaner/
- https://github.com/rtyley/bfg-repo-cleaner

Clean github repos of things like passwords

### rclone

- https://rclone.org/
- https://github.com/rclone/rclone

sync files between your computer and things like Dropbox or Google
Drive. Does not currently auto sync things but with a little scripting
it could: https://github.com/rclone/rclone/issues/249. Works on any
platform!

Does a dry run syncing the contents of the local directory `~/Drive`
to the root of the remote named `LucasGoogleDrive`:

`rclone sync --progress --dry-run ~/Drive LucasGoogleDrive:`

### croc

- https://github.com/schollz/croc
- https://schollz.com/blog/croc6/

Transfer files between two computers.

### tmux

- https://tmux.github.io
- https://github.com/tmux/tmux

A "terminal multiplexer". Basically just a program which manages your terminal
with the goal of having things be more keyboard driven. You start up your
terminal (like iterm) then start the tmux program. Most anything you can do
through typing, you can also script.

I learned my initial tmux stuff through this guy:
http://minimul.com/teaches/tmux.

### watch

Command on linux by default which repeatedly runs a command and
displays it's output. Good if you are waiting for something to happen
and don't want to keep rerunning the command manually.

### graphviz

- https://www.graphviz.org/

Open source tool for rendering graphs from a description language of
some sort. First learned about it through land of lisp!

### PlantUML

- https://plantuml.com/
- https://github.com/plantuml/plantuml

For drawing diagrams. Kind of similar to graphviz I suppose.

### Remark

- https://remarkjs.com/#1
- https://github.com/gnab/remark

Tool for making slideshows off of markdown.

### A little service for looking up good tools for the job
https://landscape.cncf.io/

### Document things in a reproducible manner
https://nextjournal.com/

I haven't actually tried this yet but Paul Rutledge recommended it.

### Hey - load testing servers
https://github.com/rakyll/hey

### AD client
https://directory.apache.org/studio/

### Gource

Not useful per se but it's hilarous. You can visualize git commit's
getting made to repos. Little avatars dash around beaming commits and
files into existence.
