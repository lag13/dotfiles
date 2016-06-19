Redis
=====

Redis is a NoSQL database. Viewing it as simply as possible, redis is a key
value store. So you tell it to store some data D at key K. Then when you ask
redis to get the value at K it will return it. It also tries to store all of
that information in memory so its very quick. There's more to it than that but
thats the basics.

Recommended Language Clients
----------------------------

Looks like this page has a list of redis clients for specific languages which
redis recommends to use:

http://redis.io/clients


Installing
----------

Mac:

```
brew install redis
```

Starting A Server
-----------------

```
redis-server
```

Connecting To the Local Server
------------------------------

```
redis-cli
```
