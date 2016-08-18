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

Connecting to a Server
----------------------

```
# host:port defaults to 127.0.0.1:6379
redis-cli [-h host] [-p port]
```

Databases
---------

Originally I thought that redis was just one giant pool of key value pairs and
as a programmer you had to be cautious and come up with good namespacing rules
so different applications setting values in redis won't step on eachother's
toes. It turns out that redis has multiple different pools/keyspaces called
databases where keys can be stored:
http://www.rediscookbook.org/multiple_databases.html

The two commands you need to know about are:

1. select `<id>` - Start using database `<id>`.
2. flushdb - Erase all keys in the database the client is currently attached
   to.
3. dbsize - Returns the number of keys in the current database.

The number of databases can be configured with the `databases` value.

```
config set databases 42
config get databases # returns 42
```

Keep Alive Connections
----------------------

There are two configuration options which determine whether or not connections
to redis stay open:

1. tcp-keepalive - If non-zero, send ACKs every given number of seconds.
2. timeout - Close connection if client is idle for a given number of seconds,
   or never if 0.

So if timeout is 100 and tcp-keepalive is 50 then every 50 seconds an ACK will
be sent from redis to the client and if they respond then the connection is
still open. If two ACK's are sent (so in this example 100 seconds will have
passed) and there was no response then the timeout will kick in and the
connection will be closed.
