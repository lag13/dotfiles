Coreos
======

Unit File
---------

```
# The name of a service file
hp-run-command@.service
```

```
[Unit]
Description=TSR's Feature Flagging Solution
Requires=docker.service
After=docker.service
Requires=etcd.service
After=etcd.service

[Service]
Restart=always
TimeoutStartSec=10 min
User=core

ExecStartPre=/usr/bin/env bash -c "etcdctl --no-sync get /environments/%p > '/tmp/%p.env'"
ExecStartPre=-/usr/bin/env docker stop "%p-%i"
ExecStartPre=-/usr/bin/env docker rm "%p-%i"
ExecStartPre=/usr/bin/env docker pull quay.io/cbone/scalp
ExecStartPre=/usr/bin/env docker create --name='%p-%i' \
                                        --env-file="/tmp/%p.env" \
                                        -P \
                                        quay.io/cbone/scalp

ExecStart=/usr/bin/env docker start -a %p-%i

ExecStop=-/usr/bin/env docker stop "%p-%i"

[X-Fleet]
Conflicts=%p@*.service
```

fleetctl and etcdctl
--------------------

Keep in mind that if you use etcdctl files for things like setting environment
variables, if an environment variable's values has spaces or other odd
characters, there is no need to escape those characters. That only needs to
happen on the command line because the interpreter handles those characters.
So no:

```
SCALP_URLS="https://scalp-dev.cb1tools.com https://scalp-us.cb1tools.com https://scalp-eu.cb1tools.com https://scalp-ap.cb1tools.com"
```

Yes:

```
SCALP_URLS=https://scalp-dev.cb1tools.com https://scalp-us.cb1tools.com https://scalp-eu.cb1tools.com https://scalp-ap.cb1tools.com
```

```
# Sets the key environments/hp-run-command in etcd to the contents of
# filename.
etcdctl --no-sync set environments/hp-run-command < filename
# Submits the service unit file so actual services can be instantiated.
fleetctl submit hp-run-command@.service
# Starts a service. Using the above unit file.
fleetctl start hp-run-command@1
# Remove service instance
fleetctl destroy hp-run-command@1
# Remove service file
fleetctl destroy hp-run-command@.service
```

Problems Starting + Stopping Containers
---------------------------------------

It seems to be a fairly often problem for us where we are unable to start/stop
containers. Oftentimes I think the issue is that etcd which synchronizes all
the behavior is broken on one of the clusters. So the solution is to ssh to
that specific machine and run:

```
systemctl restart etcd
```
