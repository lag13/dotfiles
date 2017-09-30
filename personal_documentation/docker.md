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

## Inject Environment Variables Into Docker Image
Whenever you build a docker image the ONLY way to pass in arbitrary
arguments to it is with the `ARG` directive. For example, if you have
this dockerfile:

```
FROM alpine
ARG SOME_ARG
RUN echo "$SOME_ARG"
```

Then if you do `docker build --build-arg SOME_ARG=hello .` `hello`
will be echo'd when the image is being built. I think `ARG`
essentially lets you create environment variables which are ONLY
accessible during the build process.

Another directive is the `ENV` directive. What this does is create an
environment variable in the image that will be built. Like `ARG` this
environment variable also exists during the build process (but
persisting an environment variable into the resulting image is the
most important thing). For example if you have this dockerfile:

```
FROM alpine
ENV SOME_ENV_VAR="wow there"
RUN echo "$SOME_ENV_VAR"
```

Then when you build it with `docker build .` it will echo out `wow
there` AND (more importantly) any containers you start from this image
will have the environment variable `SOME_ENV_VAR` with the value `wow
there`.

If you combine these two concepts you get the ability to inject
environment variables into the image:

```
FROM alpine
ARG MY_FIRST_ARG
ARG MY_SECOND_ARG
ENV MY_FIRST_ENV="$MY_FIRST_ARG" \
    MY_SECOND_ENV="$MY_SECOND_ARG" \
    MY_THIRD_ENV="$PWD" \
    MY_FOURTH_ENV="HELLO"
```

If you build this image `docker build -t lucas-test --build-arg
MY_FIRST_ARG=ooweee --build-arg MY_SECOND_ARG=goshhh .` then the
environment variables will be:
- MY_FIRST_ENV=oowee
- MY_SECOND_ENV=goshhh
- MY_THIRD_ENV=
- MY_FOURTH_ENV=HELLO

`MY_THIRD_ENV` is empty because when building the docker image it does
NOT have access to your local environment variables. You can have the
`ARG` and environment variable have the same name if you want:

```
FROM alpine
ARG MY_ENV_VAR
ENV MY_ENV_VAR="$MY_ENV_VAR"
```

This is a very useful technique! One application is to inject
environment variables indicating which github hash or version is
deployed which could hypothetically help when debugging.

When passing `--build-arg` through docker-compose you'll use this sort
of code:

```
build:
  context: .
  args:
    - buildno=1
    - password=secret
	- value_from_env
```

`buildno` and `password` will have the values you see above but since
you left out a value for `value_from_env` it will get its value from
the environment variable with the same name. So that snippet of code
is equivalent to: `docker build --build-arg buildno=1 --build-arg
password=secret --build-arg value_from_env=$value_from_env`.
