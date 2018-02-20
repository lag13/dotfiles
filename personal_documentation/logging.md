Logging
=======

Logging, at its essence, is really: "An emitted stream of distinct messages
from a running application" (http://gregoryszorc.com/blog/category/logging/).
A common use of logs is for developers to figure out what went wrong with an
application after the error has occurred.

I think for a long time, logs were just strings appended to a file somewhere
and if something went wrong a human would pull up that file and go through the
logs to find out what sequence of events led to the failure. The logs could
look something like this:

```
2012:11:24T17:32:23.3435 INFO gps@mozilla.com successfully logged in
```

Now somewhere along the line someone had the idea that "hey, logs are data and
if we want machines to be able to parse logs then our logs should have a
structure". And thus structured logging was born.

Structured Logging
------------------

It seems that structured logging is a practice that is recommended by
technology radar:
https://www.thoughtworks.com/radar/techniques/structured-logging.

The idea of structured logging is that instead of logging a simple string as
we did above, we should log something which can be easily parsed by a machine.
go-kit (https://github.com/go-kit/kit/tree/master/log) puts it better:

```
Structured logging is, basically, conceding to the reality that logs are data,
and warrant some level of schematic rigor. Using a stricter,
key/value-oriented message format for our logs, containing contextual and
semantic information, makes it much easier to get insight into the operational
activity of the systems we build.
```

So instead of that string above, it could be some json like this:

```
{"time": "2012:11:24T17:32:23.3435", "level": "INFO", "msg": "gps@mozilla.com successfully logged in"}
```

Or the logfmt structure (which is basically just key value pairs
https://brandur.org/logfmt) can be used:

```
time=2012:11:24T17:32:23.3435 level=INFO msg="gps@mozilla.com successfully logged in"
```

The point is that structured logging gives us the information we already had
but now machines can also parse that information which gives us greater
flexibility for how we deal with data.

Scalyr
------

Scalyr helps aggregate logs in one place. I noticed when I was
debugging something that lines I "echo"d in a bash script were not
always showing up and I *think* that is because they did not end in a
newline. In short, I think logs sometimes have trouble making it to
their destination unless they end in a newline. This would make sense
as you see this sort of feature in many places. I bet the "echo" in
question did not even make it to stdout because it was buffered.
