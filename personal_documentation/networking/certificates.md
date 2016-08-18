Certificates
============

I've always been suspicious of certificates because I'm not exactly sure how
they work, if you need them, etc...

Here is an error I encountered because of a lack of certificates. This was my
feature flagging project for TSR and this :

Aug 10 20:01:36 ip-172-10-51-106 env[6564]: time="2016-08-10T20:01:36Z"
level=error msg="error setting flag \"lucas-test-8\" in cache: could not make
the http request: Put https://scalp-ap.cb1tools.com/flag: x509: failed to load
system roots and no roots provided\n"

I guess this does seem to be a problem:
http://blog.cloud66.com/x509-error-when-using-https-inside-a-docker-container/
