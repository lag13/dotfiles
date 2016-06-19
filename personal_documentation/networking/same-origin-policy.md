Same-origin policy
==================

https://en.wikipedia.org/wiki/Same-origin_policy

TLDR
----

Web browsers permit scripts contained in page A to access data (so make API
requests) in page B if both pages have the same origin where origin is the
protocol (http, https, ftp...), host (www.example.com), and port number.

The exception is if the server B implements the Cross-origin resource sharing
(CORS) protocol then A can make requests to B.

Some Backstory
--------------

I learned about this when I was trying to make an anuglar 2 UI for TSR's
feature flagging. I wanted my angular2 app to make a GET request to scalp to
fetch all the feature flags. When I originally tried, it failed with an error
looking something like this:

		no access-control-allow-origin header is present

It seems that programming is mostly slowed down by:

1. You don't understand the tool you're using.
2. There is a security protocol in place that you were not aware of.

In this case number 2 was the culprit. Browsers only allow requests from the
web page to go to the same server where the page was served from. I think the
idea here is that if browsers did not have this restriction then this scenario
could occur:

1. You login to a website with secure info.
2. You do not logout of said website, instead you visit another web page.
3. That web page turns out to have malicious javascript code which makes a GET
   request to the secure website. Maybe it could even modify data on that
   secure website by making the appropriate POST requests or by manipulating
   the DOM. It is able to do this because you are still logged in and it is
   making requests you could normally make if you were clicking around and
   using said secure site.

But with this security measure in place the request to the secure website will
fail.

So if you want to make requests from the browser to another server (an API
perhaps) then the page that is served up on the browser must have the same
protocol, host, and port as the server.

For example if a page was served up at this url:

		http://www.example.com/index.html

then that page would be able to make an request to this page:

		http://www.example.com/api/cool_stuff
		
but not these pages:

- https://www.example.com/api/cool_stuff (https instead of http)
- http://example.com/api/cool_stuff (different host example.com instead of
  www.example.com)
- http://www.example.com:91/api/cool_stuff (different port 91 instead of 80).

There are some exceptions to this rule. If the server you are making a request
to implements the CORS policy or other similar policies then the request will
go through just fine.
