AWS
===

https://github.com/open-guides/og-aws

I guess the world relies on amazon's AWS services to get there sites working.
AWS allows you to "rent" servers and provides a lot of other services.

SQS
---

A queueing service. You can make a request to put items on a queue and another
process an take items off of said queue.

Elasticache
-----------

Management of redis and memcached services (both are in memory storage
systems).

### Parameter Groups

This is AWS's name for the configuration for redis/memcached.

EC2
---

Servers/machines to rent. I think you'd have redis and memcached run on one of
these servers.

S3
--

Storage. You can basically think of it as a big file system. In reality though
I think its just a big key+value store. "Buckets" are basically the name for
the top level directories.

Inside of buckets you store "objects" which you can think of as being either a
directory or a file. I think in reality there are no folders, just key names.
I think folders are just a concept built on top of key+value pairs.

    In Amazon S3, buckets and objects are the primary resources, where objects are
    stored in buckets. Amazon S3 has a flat structure with no hierarchy like you
    would see in a typical file system. However, for the sake of organizational
    simplicity, the Amazon S3 console supports the folder concept as a means of
    grouping objects. Amazon S3 does this by using key name prefixes for objects.

Folders are just an object that ends with a '/' character.

Lambda
------

Normally when you deploy code you usually need an actual machine to deploy to.
I think the idea with lambda is that you can just give it code and it will be
executed. You can then set up the code to execute periodically (a cron job).

Route53
-------

DNS stuff so you can easily create/register domain names?

SES
---

Send emails through amazon. Honestly, I'm still unsure how emailing works in
general. It seems that you send email through an "email server" and that is
what SES is but why do emails need to be sent through an email server? Can't
they just be sent directly to wherever it is they need to go?
