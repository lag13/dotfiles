Recursion
=========

Read "The Little Schemer" it is fantastic.

This article also had a great discussion about the y combinator:
http://mvanier.livejournal.com/2897.html.

Checklist
---------

Here's a quick mental checklist I came up with to help with the writing of a
recursive function:

1. What type of data are we recurring on? This effects what our base cases
   should be.
2. Which argument are we recurring on? This tells us which argument we should
   be making "smaller".
3. What kind of value are we trying to return? This tells us how we collect
   our values together.
