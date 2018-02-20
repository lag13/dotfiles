JSON Web Tokens (JWT)
=====================
- https://jwt.io
- https://medium.facilelogin.com/jwt-jws-and-jwe-for-not-so-dummies-b63310d201a3#.f53hgew8t
- https://auth0.com/learn/json-web-tokens
- http://self-issued.info/docs/draft-ietf-oauth-json-web-token.html

About
-----
JWT is a standard for securely (in the sense that you can check if the
data has been modified) transmitting information as a JSON object.
Basically it's just a JSON object with a particular structure that is
signed. A client will send you a JWT, you verify the signature, and
optionally check that it has certain fields that only the client
should know. If this all happens then you can trust that this "token"
came from the client.

JWT's are usually used for authentication i.e verifying WHO YOU ARE. I
liked this guy's answer regarding if JWT's can be stolen:
http://stackoverflow.com/questions/34259248/what-if-jwt-is-stolen.
Basically he was saying how JWT's themselves are not secure and that
JWT's really have nothing to do with security. They are really only
used for authentication i.e being able to tell if the client is who
they say they are. If you were transmitting JWT's over http and an
attacker was listening then they CAN take your JWT and use it.
SSL/https is necessary if you want to be secure. And keep in mind that
even though JWT's have this structure and they're signed and all that,
"For all intents-and-purposes, JWTs are more or less the same thing as
API keys: just random strings that you use to authenticate against
some server somewhere.". Granted, I think they are a little nicer than
just API keys because you can have arbitrary metadata about the token. For example, the Oauth2 protocol issues JWT tokens and those tokens probably have a timestamp 
such as an expiration. An expiration is especially cool because if the
token is stolen it can only be used for a limited time.

This also seems to have good information about the tradeoffs of JWT's:
https://auth0.com/forum/t/stealing-jwt-from-authenticated-user/352.

Also, oauth2 can USE JWTs as the format of the tokens distributed:
https://stackoverflow.com/questions/39909419/jwt-vs-oauth-authentication.
So that's the relationship between them if you were wondering. JWTs
are "lower level".

Structure
---------
JWT's are literally just a string consisting of 3 Base64Url encoded
parts separated by `.`. So a JWT looks like `xxxxx.yyyyy.zzzzz` where:

- xxxxx - the "header"
- yyyyy - the "payload"
- zzzzz - the "signature"

### Header
A Base64Url encoded json object. The json object looks like:

```
{
  "alg": "HS256",
  "typ": "JWT"
}
```

- `alg` - specifies the algorithm to be used when generating the
  signature. Two algorithms listed were HMAC and RSA but I suppose any
  algorithm could be used.
- `typ` - specifies the "type" of JWT. I'm currently unsure what kind
  of values go here.

One question I have, why does this "alg" field exist at all? I feel
like the process accepting the JWT shouldn't be deciding which
encryption scheme to use on the fly. Then hypothetically maybe people
could send a JWT with some bogus algorithm and then the process
validating that token would just use that bogus algorithm? I feel like
the sender and reciever of the token should already agree on how to
encrypt it before it ever gets sent. It's just confusing to me. It
looks like it confused other people too and lead to a vulnerability in
some libraries:
https://auth0.com/blog/critical-vulnerabilities-in-json-web-token-libraries.

### Payload
A Base64Url encoded json object. It contains the "claims" which are
statements about the entity making the request and any other data you
want. An example json object:

```
{
  "sub": "1234567890",
  "name": "John Doe",
  "admin": true
}
```

Some "claims" are reserved such as "iss" (issuer). As far as I've
seen, many of these claims are just strings and you just check that
the string is what you're expecting. I'm a little confused on this one
too simply because I don't understand why you'd need to verify that
fields match some agreed upon string. If the signature is valid then
that means it could only have come from one person so why are you
doing this extra work of verifying that some strings match? Right now
the only things that make sense to me to be in this payload are
expiration time (so the token will expire eventually) and metadata
about the user sending the token such as their ID (which you can use
to look up more data about them).

### Signature
You take the `xxxxx.yyyyy` part of the JWT and encrypt it using
whatever algorithm you specified in the header. I believe this portion
of the JWT must also be Base64Url encoded. This is the only part of
the JWT which is encrypted. If you are recieving a JWT you will decode
this signature and if the decoded signature equals `xxxxx.yyyyy` then
you know that the token is valid and the person is who they say they
are.

Use Case
--------
In general, I think use cases for JWT revolve around being able to
know that the sender is who they say they are.

### Logging Into a Website
I really don't know the state of things but I think currently, a lot
of websites will use cookies to keep a user logged into a website. I
believe this process involves the server generating some random unique
hash and storing it in a database where that hash is associated with a
user. The user then sends that cookie on every request and the server
can look it up and verify that the user is logged in and they are who
they say they are. The only annoying thing about this approach is that
the server must store data (i.e the cookie). When thinking about this
I wondered, "why can't the server just always use the ID of the user
as the cookie value?". But then I realized that if people knew this
then an attacker could send a request pretending to be any other user
by just supplying the appropriate ID as a cookie value. So the session
cookies do have to be generated and then stored which isn't too bad
but would be nice if we didn't have to store anything. JWT's to the
rescue!!!

With a JWT sort of "flow" when the user logs in, the server can create
a new JWT and return it to the user who will send it on subsequent
requests. NOTE!!! In this scenario the client (i.e the browser) never
does anything to the JWT, it just sends it with every request it
makes. The documentation said that this JWT should be put in the
`Authorization` header using the `Bearer` schema so: `Authorization:
Bearer <jwt-token>` (but I suppose it could also be sent in a `Cookie`
header or anywhere else for that matter). Then I think the server just
needs to verify the signature of the token it recieves and if the JWT
is valid then user information can be pulled from the payload and the
server can trust that user. So now the server does not need to save
any "state" on it's end (except for the public/private key pair or
whatever "secret" is necessary for encryption+decryption). Of course
if someone else gets that token then there is a problem because they
can impersonate the user but SSL should solve that and that same issue
exists with the cookie approach.

Some More Technical Stuff
-------------------------
Technically, a JWT is sort of like an abstract class. I don't think
it's a concrete thing. When we are talking about JWT's we are actually
talking about:

- JSON Web Signature (JWS) - a "signed" JWT (it is what I described
  above)
- JSON Web Encryption (JWE) - an encrypted form of JWT which I believe
  would be safe to transmit over http connections. I don't believe
  they are used as much as JWS's but I'm unsure.

So JWS's and JWE's share common structure (like the header) and these
common structures are defined by the JWT spec. But they each do other
things a little differently.
