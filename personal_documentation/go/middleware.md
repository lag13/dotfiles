Middleware
==========

http://www.alexedwards.net/blog/making-and-using-middleware

I don't think I'm a big fan of middleware populating data in the Context
object for future handlers to pick up and use. This is what we are currently
doing (01/18/2017) in tsr-api, we are grabbing database information in the
middleware and putting that in the Context object for a future handler to pick
up and use. Perhaps it's convenient but I don't like this "action from a
distance" sort of thing. Or rather, I don't like the idea that we can change
code in one part of the code base (the middleware section) and cause code in
other parts of the codebase (the handlers section) to panic. I feel like
handlers shouldn't have to know anything about the middlware that the request
goes through to reach said handler. This implies that middlware will only be
things like (and there are probably more):

1. security for the endpoint.
2. logging/metrics for requests that come through.
3. Panic/exception handlers so your server never goes down even when unforseen
   things happen.
