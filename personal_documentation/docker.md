Docker
======

Build
-----

```
docker build path/to/Dockerfile

# You can also tag while building
docker build -t quay.io/cbone/scalp:http path/to/Dockerfile
```

Run
---

```
docker run <img_identifier>
```

Tagging
-------

See `docker help tag`

Example:

```
docker tag <image_id> quay.io/cbone/scalp:http
```

Pushing
-------

See `docker help push`

Example:

```
# This will push just this tagged image
docker push quay.io/cbone/scalp:http
# This will push ALL tagged images of this one
docker push quay.io/cbone/scalp
```

Dangling Volumes
----------------

You can remove all volumes by doing something like:

```
docker volume ls -qf dangling=true | xargs docker volume rm
```

About Images and Containers
---------------------------

http://merrigrove.blogspot.co.uk/2015/10/visualizing-docker-containers-and-images.html

Echo UDP Server
---------------

Useful to test that things like Datadog or statsd are actually sending data:
https://hub.docker.com/r/eexit/dumudp-server/

Volumes
-------

More information on volumes:
http://container-solutions.com/understanding-volumes-docker/

docker-compose
--------------

version 2 apparently by default creates a network through which all containers
can talk to one another so you don't really need the "links" option anymore
(this would create the network between 2 containers and do startup order). All
you need is "depends_on":
https://medium.com/@giorgioto/docker-compose-yml-from-v1-to-v2-3c0f8bb7a48e#.1u9f16kj1

version 2 you can configure aspects about the network over which docker
containers communicate with. I'm really not familiar with it:
https://docs.docker.com/compose/networking/.

Create And Run Example
---------------------

docker container create --name lucas2 -it busybox
docker start -i lucas2
