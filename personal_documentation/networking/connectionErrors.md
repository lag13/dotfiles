I remember when I was dealing with some timeout errors on my scalp repository.
This was the list of errors and my observations about what each of them meant:

So it seems there are a number of different reasons a connection can fail.
From what I've observed:

1. i/o timeout - The request takes longer than some specified timeout
   interval.
2. no route to host - Within the timeout period, it was somehow determined
   that there is no route to the host.
3. connection refused - Maybe for this one we were able to get to the machine
   we wanted to get too but when we tried to write to the socket or whatever
   we were refused.
4. EOF - I think the socket on the client side was closed by the OS so when we
   try to write to it we get that the socket is not there.

http vs https
-------------

I guess http defaults to port 80 while https defaults to port 443. So if you
ever see 443 come up in an error message you'll know why.

Golang Http Error Examples
--------------------------

### https

If you see this sort of error:

```
dial tcp 172.20.0.3:443: getsockopt: connection refused
```

I believe it happens because of "https" being used and the server answering
your request does not support https.

You could see this sort of response if you send a request to an https server
with this code:

```
_, err = http.Get("https://localhost:8080")
if err != nil {
       fmt.Println("https protocol:", err)
}
```

### No protocol specified (i.e no http or https)

If you see this sort of error:

```
unsupported protocol scheme \"\"
```

Then it's expecting a protocol like "http" or "https". You can see tat sorte
of error with this code:

```
_, err := http.Get("localhost:8080")
if err != nil {
       fmt.Println("no protocol:", err)
}
```


### http response to https client

I'm not really sure how to make an https server but if you have a regular http
server and try to send an https request to it, you can get this response:

```
Get https://localhost:8080: http: server gave HTTP response to HTTPS client
```

That came from this code:

```
_, err = http.Get("https://localhost:8080")
if err != nil {
       fmt.Println("https protocol:", err)
}
```
