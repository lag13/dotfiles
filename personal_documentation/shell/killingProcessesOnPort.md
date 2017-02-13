Killing Processes On Port
=========================

I had an issue where an http server was running on my machine but I couldn't
find it because I forgot how to search for processes by port. I ended up
finding it but that process wasn't the problem, I had to kill the parent of
that process. But then it turned out that that parent was being managed by
`launchd` so even when I killed the parent `launchd` just restarted it.

Can list all processes including the port number being used (pid is second to
last column):

```
netstat -a -n -v
```

This also seems to be a way to get processes and ports and such:

```
sudo lsof -i -P
```

You could also use the Network Utility application which can scan ports and
get the associated programs. I wish I knew how it works because when I was
trying to figure out what program was using port 5900 the other two approaches
combined with `ps aux -l` just said it was launchd but Network Utility said it
was `rfb` which seems to be accurate:
https://en.wikipedia.org/wiki/RFB_protocol. I'm so confused.

Including the `-l` option will also list the parent ID of the process (the
PPID).

```
ps aux -l
```

```
# To get the names for services
launchctl list
# To get my UID
id -u
# To disable this service for my user so it won't start on startup
launchctl disable user/135778445/org.apache.httpd
# To disable this service for the whole system
sudo launchctl disable system/org.apache.httpd
# Tried stopping it
sudo launchctl stop org.apache.httpd
# Tried killing it
sudo launchctl kill -15 pid/3435/org.apache.httpd
# That didn't work though. Ended up finding out that I could stop it directly.
sudo apachectl stop
```

I hope that it doesn't start up again when I restart!
