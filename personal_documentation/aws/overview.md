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

Lambda almost sounds like magic. I think all you do is you upload your code
and Lambda will somehow magically provision it for you and run it. So it
basically seems like pure computation without having to worry about details
like hardware and operating systems and such.

Various AWS services like S3 and SNS can be configured to invoke lambda
functions. Lambda is pretty neat that it apparently automatically scales up
(so invokes more lambda functions) if you need more computing resources.

The only languages it supports is Node.js, Java, and Python.

Route53
-------

DNS stuff so you can easily create/register domain names?

SES
---

Send emails through amazon. Honestly, I'm still unsure how emailing works in
general. It seems that you send email through an "email server" and that is
what SES is but why do emails need to be sent through an email server? Can't
they just be sent directly to wherever it is they need to go?

SNS
---

Send out messages to other services. This can take the form of:

- Adding items to SQS queues
- Starting Lambda functions
- Sending emails (does it use SES to do this?)
- Sending texts
- Sending a request to a service through http/https

### HTTP/HTTPS

SNS can send http requests to any service you'd like! It sounds like SNS will
always send a POST request and you can configure it to use Basic
Authentication if you want.

It sounds like you need to set up your application so it will respond
appropriately to a:

- Subscription confirmation request (until the service agrees to receive
  requests SNS will not send to it).
- The actual messages that SNS sends out.

CloudWatch
----------

Basically the AWS version of datadog. As you can kind of figure out by the
name, CloudWatch is used to keep an eye on (watch) your services in the cloud.
Some AWS products (or maybe all of them?) such as SNS and SQS automatically
send metrics to CloudWatch and you can set up your applications to send custom
metrics to CloudWatch if you want. Based on those metrics you can do things
similar to datadog such as creating graphs or alarms if certain conditions are
met. I think alarms can do some pretty cool things like starting up EC2
instances or sending SNS messages (which, since you can pretty much put any
service on the other side of an SNS message, alarms could end up triggering
almost anything!).

Cloudwatch can also be used to schedule automated actions. For example
you can configure cloudwatch to emit an event every 5 hours and when
said event is emitted a lambda function will be executed or something
like that. So using cloudwatch in combination with some other AWS
infrastructure (lambda, SNS, SQS, etc...) you can perform a task on
some sort of schedule.

### Alarms

#### Understanding The Alarm Rules

```
Whenever Metric: my-cool-metric
is: <|<=|>=|> <num>
for: [1-9][0-9]* consecutive periods
```

You set the period length separately.

For example, if:

- period length = 4 min
- num periods = 2
- > 0 is the condition

Then there will have to be at least one metrics that comes through in each of
the periods. The metric coming in at 3 minutes and 5 minutes should trigger
the alarm. Two metrics coming in at 1 and 3 minutes should not do anything
since that error metric needs to be sustained over two periods.

One thing I was confused about was that I wasn't exactly sure what triggered
an alarm. Basically I wasn't sure if the metric had to be above the interval
only once in each of the specified periods? Or if the metric somehow needed to
be constantly above the threshold for the specified number of periods? (which
then raises the question, how often to metrics have to fire to be considered
"constantly"?). I think a lot of it depends on how you are aggregating the
metric. Like if you do average vs sum.

### Graphing

I think when graphing there are 3 things to take into account:

1. The amount of time to look back (1h 3h ...) is I think how far we should
   look back for metrics.
2. The period I think effects the aggregation. I think it specifies that when
   combining values, divide the metrics in buckets based on time. Then maybe
   the average would be the average of the individual buckets and then you
   average all the buckets together?
3. The aggregation strategy.

To be honest though I'm still confused about how all of this math works out...
I still don't actually understand it.

### Frustration

God I hate metrics. Don't get me wrong, they're super useful but it seems like
I always struggle with them. It always seems like I run into some weird
caching problem where the data doesn't appear or alerts don't get triggered.
Gahhhh. I guess I just don't understand the math/algorithms behind it all.

It seems that AWS has the same recommendation as it relates to alarms and
metrics namely that you should select a period that is greater or equal to the
frequency of the metric being monitored
(http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html):

    When creating an alarm, select a period that is greater than or equal to the
    frequency of the metric to be monitored. For example, basic monitoring for
    Amazon EC2 provides metrics for your instances every 5 minutes. When setting
    an alarm on a basic monitoring metric, select a period of at least 300 seconds
    (5 minutes). Detailed monitoring for Amazon EC2 provides metrics for your
    instances every 1 minute. When setting an alarm on a detailed monitoring
    metric, select a period of at least 60 seconds (1 minute).

But what about scarce data? What if you just want to monitor on a metric that
represents errors? That metric might never come in (if you're lucky) so the
frequency would be very low. But when it does come in you would want to get an
alert.
