Cookies
=======

- https://en.wikipedia.org/wiki/HTTP_cookie

Cookies are a way to have "state" with successive http requests. Because http
itself is a stateless protocol, you just send data and get back data and
neither party really has to know "who" the other person is. But sometimes you
do need to know who they are like if they log into a site and are moving
around in it. If the site did not know who the person was then it would need
them to login every time they went to a new page. So cookies are the solution.

Basically all it is, the server tells the browser to set a cookie with the
`Set-Cookie` header(s):

```
Set-Cookie: theme=light
Set-Cookie: sessionToken=abc123; Expires=Wed, 09 Jun 2021 10:18:14 GMT
```

Then the browser will save those cookies and send them on all subsequent
requests to this site:

```
Cookie: theme=light; sessionToken=abc123
```

And that's pretty much it I think! To use cookies to login to something the
server will probably tell the browser to set a cookie who's value identifies
the user in some way (perhaps the ID of the user).
