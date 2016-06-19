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
docker push quay.io/cbone/scalp:http
```

Dangling Volumes
----------------

You can remove all volumes by doing something like:

```
docker volume ls -qf dangling=true | xargs docker volume rm
```
