Pub/Sub
=======

A pattern used to communicate messages between different system components
without these components knowing anything about each other's identity.

- https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern
- https://www.toptal.com/ruby-on-rails/the-publish-subscribe-pattern-on-rails
- https://en.wikipedia.org/wiki/Message_oriented_middleware
- https://en.wikipedia.org/wiki/Message_broker
- https://en.wikipedia.org/wiki/Advanced_Message_Queuing_Protocol
- https://en.wikipedia.org/wiki/Message-oriented_middleware
- https://robots.thoughtbot.com/redis-pub-sub-how-does-it-work <<--- Who knew
  that redis had a pub/sub feature!

About
-----

The publish subscribe pattern (or pub/sub for short) is a communication
pattern where senders of a message (publishers) do not directly send messages
to specific entities (subscribers). Instead they "publish" their message and
any interested receiver will get those messages. In essence, this pattern
decouples the publisher/sender from the subscriber/receiver.

For a real world example you could picture a message board where people
(publishers) can post an ad, or something similar, and people (receivers) can
view the message board independently of you posting that advertisement. The
poster of the advertisement knows nothing about the people who are viewing
said advertisement and similarly the people viewing the advertisement don't
know anything about the person who posted it.

If you've seen the term SNS come up, that is amazon's take on the pub/sub
pattern.

I'm Looking at the Man in the [Middle]
--------------------------------------

Publishers publishing messages and subscribers magically getting those
messages is all fine and dandy in a high level sort of way but there must be
*something* that sends messages to subscribers or there must be *some way* for
subscribers to get messages. So how does that work?

Well, since the pub/sub pattern is so abstract there are probably a lot of
ways to go about implementing it, but as long as there is a "sender" of data
and a "reviver" of data and those entities don't directly communicate with
each other, then it probably could be classified as a pub/sub pattern. The key
thing is that there is something sitting between the publisher(s) and
subscriber(s) which the publisher(s) and subscriber(s) directly talk to.
Wikipedia calls this the "message broker" or "event bus":

```
publisher 1 ---+                                      +--- subscriber 1
               |                                      |
publisher 2    +------ Message Broker/Event Bus ------+--- subscriber 2
               |                                      |
               ...                                    ...
publisher n ---+                                      +--- subscriber m
```

That message broker in the middle can (and does) take on a number of different
forms. Here are some concrete examples of pub/sub or message brokers that I've
found or came up with:

1. I've seen RabbitMQ described in a way consistent with the definition of a
   "Message Broker": http://www.rabbitmq.com/.
2. (I'm just theorizing based on things I've seen). The whole "event" system
   that javascript taps into can probably be thought of as a pub/sub model.
   When you perform a "click" on a page the browser probably publishes some
   sort of "on click" action. Receivers of that "on click" action will be the
   functions that have been registered as "on click" event handlers. More
   generally, any sort of event based programming where you register event
   handlers probably has some pub/sub magic going on.
3. (My own invention). A server which maintains a list of receivers. That list
   of receivers can be modified with typical API calls. Whenever a publisher
   hits a certain endpoint, that server will loop through its list of
   receivers and send them the message.
4. AWS SNS: https://aws.amazon.com/sns/

Practical Advice: When is it Useful
-----------------------------------

### Useful When

- When the publisher does not care who (if anyone) is receiving its messages.
  The publisher is simply spitting out information which could be useful to
  some people but if it isn't, no big deal. The publisher is still able to do
  its thing if the information it published was not consumed.
- When the receiver(s) can change over time. In this case it is rather nice if
  the publisher does not have to care about those details and gets to focus on
  the message itself. The process of getting that message to the receivers is
  then offloaded to the message broker.
- If a publisher needs a job to be done but doesn't care when it gets done
  (perhaps it is some batch process) then this pub/sub model can be useful
  because the publisher can put that job on a queue and let a worker deal with
  it later.

### Not Useful

- Whenever the "publisher"/"sender of a message" cares about the response from
  the entity which ultimately receives the message. This scenario implies that
  a synchronous communication is taking place and so the asynchronous nature
  of pub/sub does not jive well. In general, I think whenever two entities
  need to talk to each other (rather than just a one sided conversation) then
  use a direct connection rather than going through a message broker.

One Thought
-----------

Originally whenever I'd hear about the pub/sub pattern I kind of got the
impression that when a message is published, the subscribers are unable to
delete that message. It's almost as if when a message is published it is just
held up for all subscribers to see but not touch. A lot of the stuff I'm
seeing though does not follow this paradigm. Instead, when a new subscriber
wants to subscribe to a publisher, a new queue (or something similar) will be
created specifically for that client. The message broker will then push to
that queue and the subscriber will read from it. Doing it like this actually
makes fine sense even though I originally thought it felt weird.

Alternatively the message broker might send the message directly to the
subscriber.
