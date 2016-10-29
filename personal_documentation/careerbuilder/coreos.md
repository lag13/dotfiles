CoreOS
======

Hooking Up URL To Container
---------------------------

I think we use something called `vulcand` which is probably one (or both) of
these:

- https://github.com/cbdr/vulcand
- https://github.com/cbdr/vulcand-coreos

```
core@ip-172-10-50-151 ~ $ etcdctl ls /vulcand/
/vulcand/backends
/vulcand/frontends
```

I think any key in the `publication` key will define something that can be
routed to. So define the value here first. I think what happens is that the
vulcand will use a regex to try and find the container to route the url to. By
default it is the name you specify followed by "-app". For example, the value
here should be 'my-cool-thing' iff the name of the container is
'my-cool-thing-app':

```
core@ip-172-10-50-151 ~ $ etcdctl get /publication/patchwerk
{"type": "http", "container_port": 8000}

core@ip-172-10-50-151 ~ $ etcdctl get /publication/scalp
{"type": "http", "container_port": 8000, "name_pattern": "^scalp-\\S+"}
```

### Start to Finish, How I Set Up tsr-featureflags

For my service which is called `tsr-featureflags-app` these are the two
commands I had to run to make the service accessible through a url:

```
# Tell vulcand about the container and how to route to it (note that I didn't
# include the "-app" part. I'm not sure if you are supposed to leave it out or
# what but I noticed other services not using it so I didn't either. The 8000
# is the exposed port in the dockerfile/the port your sevice is listening on.
etcdctl set /publication/tsr-featureflags '{"type": "http", "container_port": 8000}'

# I guess it tells vulcand when this url is hit then route to
# tsr-featureflags?
etcdctl set /vulcand/frontends/tsr-featureflags/frontend '{"Type": "http", "BackendId": "tsr-featureflags", "Route": "Host(`featureflags.cb1tools.com`)"}'
```

And thats it! There is also a `/vulcand/backends` key but that gets generated
automatically.
