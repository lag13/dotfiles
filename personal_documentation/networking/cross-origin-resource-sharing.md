Cross-Origin Resource Sharing (CORS)
====================================

https://en.wikipedia.org/wiki/Cross-origin_resource_sharing
https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS

TLDR
----

Normally javascript can only make requests to the same server it came from for
sercurity purposes. CORS defines a protocol between the page in the browser
and the server, where the page served in the browser and the server are on
different domains, to determine whether the browser can in fact make a request
to the server.

Some More Details
-----------------

Basically what happens is that when making a GET/PUT/POST request, the browser
will first make an extra "preflight" request to determine whether the action
can be performed. This amounts to sending an OPTIONS request with an Origin
header specifying the domain where the request came from:

```
OPTIONS /
Origin: http://foo.com
```

If the server is willing to accept requests it will return a response with
headers like this:

```
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: PUT, DELETE
```

This say that requests coming from *any* origin different than the server
`Access-Control-Allow-Origin: *` can perform PUT or DELETE requests
`Access-Control-Allow-Methods: PUT, DELETE`.

After writing this I looked back and was thinking that this doesn't seem very
secure, couldn't the browser just ignore this altogether and make a
GET/POST... request? The reason it can't though is because the API javascript
uses to make requests (XMLHttpRequest) adheres to this protocol.
