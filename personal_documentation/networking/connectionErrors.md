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

