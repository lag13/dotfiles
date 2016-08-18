AWS
===

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

Storage. Basically a big file system. "Buckets" are basically the name for
directories.

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
