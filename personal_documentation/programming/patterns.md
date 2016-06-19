Programming Patterns
====================

These are real design patterns an others that might be real and I just sort of
made up and want to notate.

Template Pattern
----------------

Maybe you have two pieces of code which look exactly the same but in a couple
key places they use a different function to do slightly different but related
things. For example maybe this function gets data and the only difference is
where the data comes from. The solution is to define a function which takes an
interface as a parameter. Then in the places where that special function gets
called you'll replace it with the interface call. Then you can just pass in
the appropriate interface and it will work!

The Avengers Initiatve (help eachother out)
-------------------------------------------

This is one I'm totally making up and have never even used before but I feel
like it is a good idea.

The thought is that when we try to design a program we usually try and make it
air tight; the logic perfectly covers and handles every situation we could
ever encounter. But creating that logic is hard and could lead to complicated
code because of all those edge cases that must be handled. So maybe a good
approach would be to have different threads handle different related tasks and
handle those tasks in a naive manner. When run individually, those threads
might miss some edge cases but when those threads work together they handle
all those edge cases.

For example, in my TSR feature flagging solution I've had to go to pretty
decent lengths to try and keep data consistant between the redis and sql
instances but if they go out of sync (and no one sets the flag that went out
of sync) we'll still need human intervention in the form of restarting redis.
I feel like it probably won't happen but you never know! So there are sort of
two problems with my code. One is that its fairly complicated and the other is
that it still could require human intervention. I believe these two problems
could be solved with having go routines dedicated to keeping data consistant.
We would simplify the logic involved with setting flags so that it assumes the
"naive" case always happens i.e both sets work correctly and there are no race
conditions to screw things up. Then we'd have a dedicated go routine which
chekcs for data discrepancies and if any happen it either tries to fix them or
alerts another go routine who will actually fix the issue. The combination of
these go routines working together, I think, fixes the same problem as the
version with complicated code (and maybe works even better because there might
be no human intervention needed) AND the code is simpler (setting flags takes
the naive approach which is easier to read and there are probably other
aspects which can be simplified like how we set the flag in redis in the GET
request of the flag was not found in redis but found in sql).

Just like the avengers. Separately the superheros are lacking in certain
traits but when you put them together they really kick ass.
