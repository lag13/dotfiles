Testing
=======

I want to learn how to write more than just unit tests for go. I'd like to
write tests which:

1. Tests that make sure that 2 or more components work together (integration
   testing)
2. Tests that make sure when we make some external call (like a write to a
   database, a write to redis, an http request) that those external calls
   actually work and have the desired effect. For instance, if we insert a row
   into a database I want to check that that row was actually inserted (also
   integration testing although this is between a component I own and an
   external one).
3. Acceptance/end to end/function tests. I want to write tests which run the
   whole application (including any dependencies like a database) and make
   sure that a couple key pieces of the application work as expected.

Here are some posts I'd like to read in more depth:

- http://modocache.io/restful-go
- https://blog.codeship.com/implementing-a-bdd-workflow-in-go/
- http://agouti.org/
- http://googletesting.blogspot.com/2015/04/just-say-no-to-more-end-to-end-tests.html
- https://github.com/mediocregopher/radix.v2#testing
- http://www.hydrogen18.com/blog/golang-orms-and-why-im-still-not-using-one.html
- https://www.google.com/search?sourceid=chrome-psyapi2&ion=1&espv=2&ie=UTF-8&q=golang%20test%20database&oq=golang%20test%20database&aqs=chrome..69i57j69i64.3921j0j7



OOOOH! So with BDD testing I think they just give more descrpitive messages
when things error out about the behavior you wanted to see. When writing these
go table driven tests I've been adding little comments above each tests to
clarify what they intend to do, but it would be even better if those comments
were a string and then the error could printed out. That errorMsg() function
could do it! I think at the top of the test I could have another string that
specifies what the test is trying to accomplish and maybe that errorMsg()
function could use that string in its output as well.
