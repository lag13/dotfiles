Testing Code
============

I'm still learning but it seems like writing tests for your code is a pretty
neat thing to do. Some benefits:

1. Promotes the application being broken up into manageable chunks (because
   when a "chunk" becomes too complicated it becomes very difficult to test).
   In general I think testing can tell you a good deal about your code and
   "offer" suggestions on how to improve it if you listen carefully. :).
2. Encourages you to think about the "behavior" you want from some piece of
   code instead of getting bogged down in the details of the implementation
   (which, interestingly, can lead you to forget what you were trying to do in
   the first place!). Specifying the behavior you want by writing a test first
   (i.e TDD) can be especially nice because then it gives you something to
   work towards and you'll never forget what you originally intended to do.
3. Allows you to refactor with confidence!!! This is probably one of my
   favorite benefits, if you have good unit tests and you refactor the
   internal implementation (assuming the public functions remain the same) and
   your tests still pass then you've got good confidence that your unit is
   still working as expected. Even better, if you have good acceptance tests
   you can refactor the whole codebase and as long as those acceptance tests
   still pass you can be fairly confident that you have not broken anything!
4. You can catch bugs just due to the fact that you're looking at your code
   from a different perspective.

- http://blog.thecodewhisperer.com/ (I read a couple from here but all seem
  good http://blog.thecodewhisperer.com/series#integrated-tests-are-a-scam)
- http://www.agitar.com/downloads/TheWayOfTestivus.pdf
- http://programmers.stackexchange.com/questions/301479/are-database-integration-tests-bad

TSR-API Inspired Acceptance Test Musings
----------------------------------------

These musings might apply to testing in general but these questions originally
came up in the context of acceptance testing for the tsr-api. These
questions+answers were inspired by this task I was set out to do:

    Right now the local tsr-api talks to a fake product instance API using
    http requests+responses. The real product instance API uses oauth2 though
    so I need to add oauth2 to the tsr-api and make sure that we can still run
    acceptance tests. This might mean that we have to switch out the oauth2
    client for a fake one when running acceptance tests because I don't think
    I know how to mock an oauth2 server.

### Questions

#### One

    The tsr-api in production will talk to other API's such as the product
    instance API and redis. Why in our testing do we use a mock of the product
    instance API and not redis? Why do we want to use mocks at all when we run
    acceptance tests? If there exists a docker image of the real service, why
    not just use that image and thereby use the real service? Or if no such
    image exists, why not use the *real* service? For the hal-cache-service
    for example, when doing acceptance tests, I had it talk to mock scalps
    even though I have an image for the real scalp. Why did I do this? Why not
    use the real scalp image?

I feel like redis is sort of a special case. We could hypothetically create a
mock redis which accepts requests and then make sure the data we sent over the
wire matches what we expect. But I'm not familiar with redis's protocol enough
to do that (are there situations where redis talks back and forth with a
client before getting a command executed? Maybe some sort of "commit" action
where multiple commands run as one?). With redis it seems simpler to check the
actual redis server to see if the expected changes took effect. It does get a
bit messy when you do that because you have to make sure to clean data
before/after the test but I think the trade off is okay. I would argue the
same sort of thing for mysql. You could hypothetically create a mock service
but I'm not familiar with the protocol enough to do so and it might be easier
to just check that mysql has the expected data when you're done. In general,
it feels like for any service which you are storing things in, it is simpler
to make use of the real service and then check that the data is there. In an
ideal world I feel like checking that the request is what you expect is best
(then you wouldn't have to deal with cleaning or seeding data between tests)
but I'm not familiar enough with the protocol to mock it. I suppose this goes
back to that quote "Only mock types that you own" or a potentially more
accurate quote: "Only mock types that you understand".

We use mock services so we can inspect that the requests we send are what we
expect them to be (we can add an endpoint to the mock that returns last
request). Let's say that instead we decided to use the real service when
testing. Hypothetically I could imagine a bad scenario where we refactor and
end up sending the wrong request but the request is still accepted by the real
service. In that case the acceptance test might think that everything is fine
but it is not since we sent the wrong request. For example maybe we tell scalp
to do a "set" instead of an "hset" by accident. If we used the real scalp
everything would appear fine since the request is still valid. But if we used
a mock we could inspect the request and seen that we sent the wrong request.
In that scalp example, I suppose we could inspect redis to make sure the data
appears as we expect it to (this is what I did with feature flagging) but it
sort of feels like you should trust scalp to do it's job and so checking that
the request is what you expect it to be should be sufficient. I think, to keep
your sanity, you should only check that the people you directly talk to got
the expected messages and trust them to do their jobs. There are probably
exceptions to this but it feels like a nice idea to adhere to. In general it
feels like many http services are easy to mock because:

- Often you just send a request and get back a response, no complicated
  interactions.
- We are used to working with http more since we use it all the time making it
  easier to mock (as opposed to things like redis an mysql).

We don't use the real service because then we could be changing real data
which is no good. But what if we had a staging service? I suppose then we
could use that but it seems bad to rely on that service being up when you run
tests. And who knows what state that service would be in? Perhaps people are
messing with it and now your tests are failing? Not great, tests should be
more reliable than that which suggests using something local.

#### Two

    For the hal-cache-service, why did I make completely separate mock
    containers? Why not just have a switch in the main.go file that would pass
    in mock clients to contact those services (those mocks wouldn't actually
    contact anything, they would just emulate the service in some fashion)?

Having these sorts of switches clutters the codebase with code that is not
going to run in production. This seems bad to do. It could make things
confusing for one and it could even cause you to make a mistake and end up
with mock services being used in production.

I don't think we would ever use these sorts of mock clients over creating a
separate mock service because there would be no way for the acceptance test to
check that the expected request was made (the acceptance test can't "reach
into" the mock clients inside the service). So it seems that creating separate
servers representing the other API's this server talks to (and exposing an
endpoint on those servers to get the last request) is a good idea.

#### Three

    For the hal-cache-service, why did I have a switch for the SNS stuff that
    would pass in a mock SNS handler which would always say that the request
    signature was valid? What are the reasons that I could not construct a
    request with a proper signature myself?

I guess that is purely a matter of a lack of knowledge about how to create a
signature that could be successfully verified. Hypothetically, I could set up
the testing environment and the test itself so that I send a properly signed
request but maybe that would be more trouble than it is worth? Maybe it
wouldn't even be possible? I'm not sure. But since we're assuming that AWS
will send properly signed requests maybe we shouldn't worry about sending them
ourselves? We really just need to get past that signature verification and
make sure that the code behaves as expected. So perhaps having this switch in
the code is not such a terrible thing.

In general it kind of seems like security related code (this signature
verification, using oauth2, etc...) can be hard to test because when you
create your test, all the infrastructure that those security processes rely on
need to be there. I know I don't know a lot about security and maybe others
don't either so maybe doing all that is more trouble than it's worth? I
suppose you could also make the argument that security is not important in the
context of acceptance testing? You want to test your functionality and
security is just a locked door that you need to get past to get to that
functionality. So perhaps security related code is an exception where you can
pass in mocks inside the actual code being tested.

#### Four

    Why can't we have acceptance tests which test unhappy paths? I mean, we
    can, strictly speaking, but why haven't I done it very much? For instance,
    would it be possible to start up the service, run some acceptance tests,
    bring down a service it relies on, and then test that the service performs
    as expected when that service is down? Is that easy to do? How would I do
    that? The tests run in a container right now, is it possible to contact
    the docker daemon inside a container and stop a different container? Or
    perhaps I need a man "on the outside" as it were. Like maybe I need a
    script which knows which tests run in certain situations. So this script
    would start up the application and all its dependencies, run some tests,
    bring down a service, run the tests for when that service is down, bring
    down a different service, run the tests for when that service is down,
    etc...

It's a good question and I do not know the answer. TODO: Perhaps I should
investigate it. It seems like something that is possible and potentially
useful.

What to Do About Other Services Your Application Talks To
---------------------------------------------------------

This discussion is sort of a refined+elaborated+generalized version of answers
to the above questions. The goal is to refine, in my mind, some "rules" for
what to do when setting up the environment which runs acceptance tests. I want
some "rules" for things like:

1. When is it better to use a real instance of a service that your application
   talks to and when is it better to use a fake one?
2. When is it okay to modify the actual code of your application to make
   acceptance tests possible? For example, in main, if your application is
   being run for the purposes of acceptance tests you could inject some
   mock/fake client to make acceptance tests possible. When is it
   okay/necessary to do this?

If I can better understand the answer to those questions I can better evaluate
the situation at hand to determine which approach will give the most value.

### The Purpose of Acceptance tests

Before going too deep into it, let's remind ourselves of what an acceptance
test *is*:

    An acceptance test is running your entire application, giving it input,
    and checking that it behaves as expected.

If we do the above then we get confidence that the application is still
working. So the *purpose* of acceptance tests is to ensure that your
application still works as it evolves.

A consequence of "running your application" is that you'll need to put the
application in an environment that resembles it's production environment.
Ideally this environment resembles production as closely as possible. This
will involve, in some form, spinning up instances of services that your
application talks to (more on that later).

Checking that the application "behaves as expected" could take many forms
because there are a lot of different kinds of applications out there. Since
this discussion is motivated by HTTP APIs I'll probably stick to that domain.
Checking that an HTTP server behaves as expected will probably involve:

Given a request...

1. ...the response (status code, body, headers) is what we expect.
2. ...the application effects its "world" (i.e the services it talks to) in
   the way we expect. This involves things like checking that the application
   sends the expected messages to the expected services. Or checking a data
   storage service, like mysql, to make sure that the application modified
   that storage service in the expected way.

Now let's talk about the different approaches to setting up a testing
environment. Specifically how to set up services that your application talks
to.

### Setting Up The Environment For an Acceptance Test

Chances are, if you made some piece of software, it is going to talk to other
pieces of software and other pieces of software will talk to it. If this does
not apply to you then neither does this discussion (I'm a bit curious though,
what are examples of such an application? And how would you test those? Would
acceptance tests look a little more like unit tests in such a scenario? Is a
video game an example of this? A video game could only have humans sending
messages to it and it might not talk to any other software. Anyway). So,
since:

- Your application talks to other services.
- Other services talk to your application.
- You run your entire application when doing acceptance tests.

Then, in some form, you must give your application services to talk to.
Additionally, when you build the requests to send to your application in your
acceptance tests, they must look like requests coming from the real services
which will talk to your application.

Setting up your application to talk to other services could take many forms:

- Use a real deployed instance of the service that your application needs.
  This could be a production instance or a staging one.
- Locally run a real instance of the service and have your application talk to
  that. This could take the form of a docker image which you run, running the
  service directly on your machine (so you'd probably have to compile it), or
  however else you can get that service running!
- You could create a separate tiny application which emulates the service and
  have your application talk to it.
- If the application uses some client to talk to this external service. You
  could modify the application's code so when it is being run for the purposes
  of acceptance tests it will pass in a mock interface for the client. This
  mock interface would emulate the service in some way.

These are all the possible ways I can see in which you can have your
application talk to *something* which resembles the service. Looking at them
they sort of boil down to either:

1. Using a real instance of the service. This amounts to using a publicly
   available deployed instance of the service or running the service locally.
2. Using a mock of the real service. This amounts to creating a separate
   program to emulate the service or injecting fake clients, which emulate the
   service, into the application when it starts up.

Let's analyze each of these approaches to setting up services for the purposes
of acceptance testing including some real life examples and hopefully some
takeaways for situations where each should be used.

#### When To Use A Real Deployed Instance Of the Service

For acceptance tests? I think never, this isn't really even a choice. First
off, if you use a deployed production instance you run the risk of messing
with real production data. Not good. Even if you use a "staging" instance of
the service now your test could fail because something is wrong with that
deployed service (perhaps people are messing with it or perhaps the internet
connection is faulty). Also not good, your tests should only fail if your code
is bad. Working with a deployed service can be tricky as well because it can
be hard to control the state of that service. For example, my delete worker
would delete systems. In some sense the service I relied on was the software I
was deleting. In that scenario you'd have to deploy another instance of the
software every time you wanted to delete it which sounds tricky and error
prone.

But what if there is no way to locally run the real service (it is too
complicated to set up) or a mock of the real service (the interactions you
have with this service are a bit too complex to mock)? It's a good question
and I am not sure of the answer. Probably my preferred answer would be that if
neither of those things can be done then your priority should be making one of
those things possible. So either make it possible to have the real instance
running locally or simplify the interaction between your application and the
service so you can create a mock. In other words, you shouldn't be working on
your application yet, you have other technical debt to clean up.

But sometimes we don't have the time or the resources to deal with such
technical debt and we must forge ahead because the business requires us to. I
bet sometimes it just isn't possible to have a local service that resembles
the production service we need. So what do we do? I would say test anything
and everything you can. Have acceptance tests for the areas which don't use
this service and have unit tests covering as much if not all of the
application. This should give you some confidence that the application as a
whole works. You can also inject mocks, in place of the things that talk to
the service, when the application starts up (more on that later). After that,
manually test the things you weren't able to test using real deployed
instances of the service you're relying on. Be careful!

##### Example Case: hotpot-delete-worker

An example application that I was unable to get good tests for (in part
because I was new to the concept of tests) was the hotpot-delete-worker which
deleted customer systems. To delete a customer system it needed to perform the
following tasks:

1. Poll an AWS SQS queue for deletion requests.
2. Get the customer system information from hal.
3. Send a HTTP request to hal to mark the system as deleted.
4. SSH into a server and `rm -rf` the customer system directory.
5. Contact AWS route53 to delete the DNS for that customer.
6. Run a mysql command to drop the database.
7. Move attachments from one s3 bucket to another.
8. Send out emails for people to manually delete other resources that have no
   automated way to be deleted.

This is nasty business from a testing perspective. Almost everything it's
doing is using some other service which either cannot be run locally (the AWS
stuff for instance) or I am not familiar enough with the protocol to mock it
myself (mysql, emails, AWS, etc...). So what do we do? Reading through this
list it feels like the right thing would have been to have nice simple API's
to delete each of the resources that needing deleting. So we'd have an API
call to remove the customer system directory, delete DNS, delete the database,
and move s3 attachments. I'm not sure how we'd test sending the emails (we
used SES for that by the way). Although creating small http services for all
those things probably would have been the "right" approach, we needed this
deletion functionality soon and doing all that work probably would have taken
too long. In some sense, testing is a luxury (it adds no new features, only
stability as the software evolves) when faced with the needs of the business.
When I was developing this I just made sure to manually test it very
carefully.

I'm not sure even now what I would do to get this application in an
environment where I could run acceptance tests. I guess I would start by
getting more unit tests and going from there. I could have some integration
tests for things like removing the customer system directory (which would ssh
into a docker container with a file system or something like that). I could
have something similar for dropping the mysql server. Or maybe those sorts of
things would have been part of the acceptance testing. TODO: Think about what
I would do to get this system under test and actually try to do it to see what
it takes. Testing is important and I'd like to be able to have tests in as
many situations as I can.

#### When to Use the Real Service Locally

One reminder of what I said before. One thing acceptance tests need to do is
check to make sure that your application effected the "world" in the way we
expected. When checking that the "world" was effected I think you should only
(if you can help it) have the acceptance tests check the services your
application directly talks to. Anything more and you are (to some extent)
relying on the implementation of that service. For example, if the application
talks to a http service that updates mysql, don't have the acceptance test
check that mysql server! Just check that the request sent to that service is
well formed and trust that service to do the rest. This is purely to make your
life easier. As with most things there are probably exceptions to this but I
think it's a good rule to follow.

Back to the topic. I think you should use a real service locally iff creating
a mock one is too difficult AND you are able to check in the acceptance test
that the application affected the service in the way we expect. Using a mock
has the advantage of simplicity, because you don't have to set up a real
instance of the service (which might be complicated to do and if that service
changes it's infrastructure you have to change your repository too), and you
have more control over it (you can make the mock return bad data or specific
data). So make a mock if possible. As for the second part of the AND
conditional, if we cannot check that the real service was effected in the way
we expect then the acceptance test loses a bit of usefulness (because one of
the jobs of the acceptance test is to check that your application affected the
"world" in the way we expect). So again, use a mock one if possible. The
reason you might not be able to create a mock service is because the service
is too complicated to emulate. This difficulty could be due to unfamiliarity
with the protocol of that service or perhaps the service just has really
complicated logic which is hard to emulate without re-implementing the service
you are trying to mock.

A perfect example of a service for which you would want to run the real thing
locally is redis. Redis has an unfamiliar protocol that might be hard to mock
and even if you understood it, you'd probably start implementing a clone of
redis if you started mocking it. Furthermore, with redis you can check that
your application effected it, just check if the data that you expect is there.
Redis satisfies the two conditions of being complicated to mock and possible
for acceptance tests to check that our application did what we expected so it
is okay (and probably preferable) to use the real thing when running
acceptance tests.

#### When to Use a Mock Service Locally

Use a mock service for acceptance tests iff the parts of the service you rely
on is simple to mock. Using a mock gives you more control over how this
service is going to behave (what data to return, return some bad data,
etc...). Additionally you can add an endpoint to this mock service which
allows the acceptance test to view the requests that were made which is
perfect for checking if the application sent the expected requests to the
expected services.

One negative aspect of using a mock service is that if the real service you
are emulating changes, then the acceptance tests (since they rely on a mock
and not the real thing) but when your service gets deployed it might fail
since the real service you rely on has changed. In this scenario I suppose
it's more the fault of the service (since they broke their API) but still, it
would be nice if that could have been caught earlier.

#### When to Inject Different Functionality Into The Application When Testing

This should probably be a last resort when it comes to creating a reliable
environment for writing acceptance tests. The main thing that is bad about
this is that you are adding code to your application which will NEVER be run
in production, it is just used to get an environment set up in which you can
run acceptance tests. This could make the production code a bit more confusing
and there is a risk that you make a mistake and deploy code which uses mock
functionality instead of the real functionality. Not good.

So when do you want to use this strategy? I think you want to do this when you
cannot have anything resembling the service running locally. So you cannot
have a real instance and you cannot have a mock. In such a scenario your only
real option (assuming you want acceptance testing) is to change the
application slightly so it doesn't try to use the real service. The main
examples I have of doing this is related to security:

- For the hal-cache-service it received a request from SNS. As per the SNS
  documentation they recommend that you verify the signature in the request
  they send. Probably for security purposes. In my acceptance testing I did
  not know how to create a mock SNS request with a valid signature (or if it
  was even possible) so when the application was under test I injected some
  functionality which always said the request had a valid signature.
- For the tsr-api it needed to talk to the product instance API using oauth2.
  I do not know how to emulate oauth2. I don't even know if it is possible to
  do so. So I've opted for injecting a mock client which does not use oauth2
  into the service. This client will talk, just using http, to a local mock
  instance of the product instance api.

In general it kind of seems like security related code can be hard to test
because when you create your test, all the infrastructure that those security
processes rely on need to be there. I know I don't know a lot about security
and maybe others don't either so maybe doing all that is more trouble than
it's worth? I suppose you could also make the argument that security is not
important in the context of acceptance testing? You want to test your
functionality and to do that you need to get *past* the security. So perhaps
security related code is an exception where you can pass in mocks inside the
actual code being tested.

Just remember that if you go with this strategy for acceptance testing it is
*imperative* that you test using the real services before deploying. I guess
this is always important but it feels a bit more so in this case.

#### Recap + One Trick Which I thought Might Be Useful

Recap of the Rules:

- Never use deployed services in your acceptance testing environment.
- Use a real service locally for acceptance tests iff creating a mock one is
  too difficult AND you are able to check in the acceptance test that the
  application affected the service in the way we expect. 
- Use a mock service locally for acceptance tests iff the parts of the service
  you rely on is simple to mock.
- Inject different functionality into your application to emulate the service
  iff you cannot have anything resembling the service running locally. 

Trick: If you decide to locally use a real instance of a service for
acceptance testing I think you could still use a mock as well. Just make the
mock be a middleman between the application and the service. So it would just
forward the requests you give it unchanged to the service. I think the benefit
of doing this is that you can check the mock that the requests that flowed
through it are what we expected and you could selectively have the mock return
some "bad" data when given specific inputs to see how your application
behaves.

Modularity
----------

### The Problem

I'm having a bit of a "problem" right now with the tsr-api. I'm working on
bringing in versioning capabilities into the API. The way I thought about it
is that having slightly different versions for things is basically just a
dispatch; a request comes in requesting a specific version of some resource
and the appropriate handler gets dispatched based on that version. My plan was
essentially to have a named map type like this:

```
type VersionDispatcher map[string]http.Handler
```

Then on VersionDispatcher I would define a `ServeHTTP()` method which extracts
the version from the `Accept` header and dispatch to the appropriate handler.
When an invalid version is specified I would like to respond with an error
response indicating their failure to select a proper version. With any API you
want responses to have the same format so a client doesn't have to do a lot of
hoop jumping. That suggests that you'll end up creating some sort of
"response" package which has a type which can be marshaled to produce a
response. The thing I don't like about this is that if we're relying on this
"response" package, then if that package changes then the unit tests could
break. Another problem is that we'll probably have some acceptance tests in
place to check that the error response is expected. I don't know if I like
that duplication because it would mean updating code in two places if the
response changes.

To recap, the essential issues are:

1. This "version dispatcher" package is relying on a "response" package to
   produce a response consistent with the API when things go wrong. Now unit
   tests for the version dispatcher package will break if the response package
   changes in a breaking manner. This feels wrong. On the one hand the
   ServeHTTP being tested is producing a different output so maybe it's good
   that it fails. On the other hand though it feels like tests should not
   break if an external package changes. How do we get around this? Maybe we
   just don't use the external package? But we need to use that external
   package to get consistant responses. Is the only way to decouple this by
   using an interface or a function parameter which does the job of formatting
   the response? It kind of feels like that is what needs to be done. Is that
   overkill though? Part of me likes doing things like this, a little
   "buffer"/"converter" between two packages allowing them to be independent
   units, because then if one package changes it doesn't break the unit tests
   of the other package. But another part of me thinks this makes the code
   more confusing because it adds this sort of "boilerplate" code which serves
   no functional purpose, it only helps decouple things. I suppose there are
   pros and cons to coupling. A pro for coupling is that code can be more
   concise and straightforward because all the pieces are tied together. A con
   is that if all the pieces are tied together then maintenance can be hard
   because changing something in one package can break something in a
   different package. These sorts of things are very annoying to deal with.
2. In addition to the unit tests on this package we'll also most likely have
   acceptance tests which check for the error responses that this package
   ultimately generates. That is duplication which I don't really like because
   if the response changes then it will have to be fixed in two places. Can
   anything be done about this? It's really not the end of the world but at
   the same time I don't like it. And what about the "Content-type" header?
   This package needs to set this value. Should the unit tests and the
   acceptance tests check for this header? Now we're back to the duplication
   issue. I feel like this package should check the Content-type though
   because it directly sets it so if it changes how it sets it then it makes
   sense for the unit tests to change. But the response body is going to be
   generated by a separate package and we don't want external packages to mess
   up this package's unit tests hence this package doesn't worry about it.

### My Proposed Solution

Separate the logic that is being tested in the version package from the
responses package because I really don't want changes to the reponse package
to change unit tests in this package. So we're going to have some sort of
function which the version package relies on instead of directly relying on
the response package. Then in the tests we can pass in a mock function just to
make sure that a response is being written. So the version package will be
directly responsible for writing the Content-type, the http status code, and
for providing the basic response body text (which will probably appear in some
"message" json key). The function getting passed in will deal with
constructing the appropriate format for the response body.

It feels like when writing code it can kind of be divided into two camps:

1. The code that is unit tested.
2. The code that will be covered by acceptance tests.

The unit tests should just test the logic of the unit. You should structure
your code so that the main logic does NOT rely on any external packages. That
way a change in one of those external packages can never break the logic of
the unit being tested. The way you'll accomplish this is probably having a
function or interface parameter to something and you'll end up passing in a
mock when testing but something real when the code actually runs. When a mock
is passed in the logic of your unit can be tested completely. When the "real"
thing gets passed in then acceptance tests will handle that. Maybe you could
divide all code into "units" and "the parts which inject real functionality
into the units which will end up running when the actual application is run".
I wonder if it would be nice to append each file name with "unit" or
"acceptance" (or something like that) to indicate which files are totally
independent and which rely on other packages. Something like that would
probably be better served by good tooling though which could detect when a
package/file uses external packages.

To Unit Test Or Not To Unit Test
--------------------------------

I've run into a dilemma a couple times where I don't feel like unit testing a
package because it is directly used by the main program i.e the results of
this package directly influence the output. I don't feel like unit testing it
because it will already be covered by acceptance tests. Here are my thoughts
on that matter:

- Unit test something if there is a potential error that is hard to re-create
  in an acceptance testing scenario. For example, if your package makes an
  HTTP call, that API call has a chance of failure. But it is hard to recreate
  that failure in an acceptance test because that service will be up and
  running. Or perhaps in this scenario you should try to structure your
  acceptance tests to bring down necessary services and see that your api
  responds appropriately? That might be ideal but unit testing the package and
  injecting mocks which will fail is a nice simple way to go about doing it.
- Unit test something which is used by other packages and not directly by
  main.

If one of those things don't hold then then your code will be covered by
acceptance testing and you needn't worry about unit testing it.
