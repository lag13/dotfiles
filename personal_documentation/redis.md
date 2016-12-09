Redis
=====

- https://redis.io/
- https://github.com/antirez/redis

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

Data Types
----------

Redis is a LOT more than just a key-value store. It has native data types such
as hashes, lists, and sets.

- https://redis.io/topics/data-types
- https://redis.io/topics/data-types-intro

### Hashes

Source code: https://github.com/antirez/redis/blob/unstable/src/t_hash.c

There is a hash data type in redis (which may sound odd because normally you
might think of redis as already being a big hash). There are commands like
`HSET hash-name key value` and `HGET hash-name key` that let us do this. This
can be useful for, say, storing information about an object. The object
identifier could be the name of the hash and then the keys inside that hash
could be various properties.

It turns out that hashes are also MORE memory efficient than doing plain SET's
and GET's IF the hashes are "small" enough because if the hash is "small"
enough it will use a more efficient structure they call a "ziplist":

- https://redis.io/topics/memory-optimization,
- http://stackoverflow.com/questions/12779372/hset-vs-set-memory-usage
- https://groups.google.com/forum/#!topic/redis-db/90K3UqciAx0
- http://developers-club.com/posts/271205/

Being "small" means that the number of keys in the hash and the size of each
value in the hash are below a certain threshold. Those values are
configurable:

```
# Hashes are encoded using a memory efficient data structure when they have a
# small number of entries, and the biggest entry does not exceed a given
# threshold. These thresholds can be configured using the following directives.
hash-max-ziplist-entries 512
hash-max-ziplist-value 64
```

- hash-max-ziplist-entries - The maximum number of hash entries in order for
  the dataset to be compressed.
- hash-max-ziplist-value - The threshold of biggest hash entries in order for
  the dataset to be compressed. If a value is 

You can see this in action:

```
127.0.0.1:6379> config set hash-max-ziplist-value 10
OK
127.0.0.1:6379> hset myhash maxsize 1234567890
(integer) 1
127.0.0.1:6379> debug object myhash
Value at:0x7fa4ca629760 refcount:1 encoding:ziplist serializedlength:69 lru:4828951 lru_seconds_idle:1
127.0.0.1:6379> hset myhash toolarge 12345678901
(integer) 1
127.0.0.1:6379> debug object myhash
Value at:0x7fa4ca629760 refcount:1 encoding:hashtable serializedlength:70
lru:4829058 lru_seconds_idle:1
```

You see that at the start that when all the values were below a certain size
the hash were stored as "encoding:ziplist" but when a larger value was stored
it got stored as "encoding:hashtable".

If the hash grows beyond one of these configuration values then it gets
converted to a "dict" type. So a hash in redis is really a combination of two
types:

- ziplist - When the hash is "small enough". This data type is, apparently, a
  very efficient dually linked list which consumes MUCH less memory than a
  typical hash/dict. I think lookups are technically slower than a true
  hash/dictionary (perhaps O(n)) but if the ziplist is small then those lookup
  times are negligible. Apparently instagram opted to use a very large ziplist
  sacrificing a little bit of time+CPU for huge gains in memory:
  instagram-engineering.tumblr.com/post/12202313862/storing-hundreds-of-millions-of-simple-key-value.
- dict - When the hash is "too big" it gets turned into a dict to maintain
  O(1) lookup times.

It turns out that the "dict" type is the same type that redis uses to do
normal `set`'s and `get`'s, cool! So I would imagine that even when a hash
gets converted to a dict it's still not too much overhead in terms of memory,
since it uses the same data structure that redis normally uses. As far as I
can see, the only extra data involved when using a hash is:

- the key representing the name of the hash that is stored in the global dict
  of keys.
- the metadata associated with the new dict used by the hash.

Once those two initial pieces of data are created, adding keys to this new
dict using `hset` would probably use the same amount of memory as doing a
`set` since the same kind of data structure gets used. You might think that
`hset` would be slower because there is an extra level of indirection (not
that one more pointer dereference would make a noticable difference) but
looking at the source code I'm not so sure. From the looks of it, the `set`
code does more than just a simple set, `hset` is much simpler.

In addition to various articles here are a bunch of relevant files which I
looked at to figure this out:

- src/server.h 845 - The data type for the entire redis server. It is stored
  in a global variable called "server".
- src/server.h 586 - The data type for a redis database. Looking at that
  struct you'll see that it uses the "dict" type to store the data.
- src/dict.h 76 - Where the "dict" type is defined (for the curious).
- src/server.c 199 - Where the hset command is added as an available command.
- src/t_hash.c 514 - Where the hset command is defined.
- src/t_hash.c 202 - Where the set actually takes place. You'll see a big
  if/else for doing a different kind of set depending on if the hash is a
  ziplist or dict. If you compare this with the `set` command code you'll see
  it also uses dictionary functions to perform the set (in particular they
  both call the `dictAdd()` command. I didn't look too much into it but it
  looks like the `set` command is actually much more involved than the `hset`
  command. So the `hset` might actually be a tad faster.
- src/server.c 128 - Where the `set` command is registered.
- src/t_string.c 96 - Where the `set` command is defined. It ends up calling
  `setGenericCommand` defined in the same file and that ends up calling
  `setKey()`. 
- src/db.c 194 - This is where `setKey()` is defined. If the key does not
  exist it ends up calling `dbAdd()` defined in the same file which ends up
  calling good old `dictAdd()`. If the key does exist then it ends up calling
  `dbOverwrite()` which ends up calling `dictReplace()`. Its odd that it
  seemingly does so much more than `hset` which appears to do a simple
  assignment: `dictGetVal(de) = value;` (`dictGetVal` is a macro).

