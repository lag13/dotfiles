Testing Code
============

I'm still learning but it seems like testing code is a pretty neat thing to
do. Some benefits:

1. Promotes the application being broken up into managble chunks (because when
   a "chunk" becomes too complicated it becomes very difficult to test)
2. Encourages you to think about the "behavior" you want from some piece of
   code instead of getting bogged down in the details of the implementation.
3. Allows you to refactor with confidence!!! This is probably one of my
   favorite benefits, if you have good unit tests and you refactor the
   internal implementation (assuming the public functions remain the same) and
   your tests still pass then you've got good confidence that your unit is
   still working as expected.

- http://blog.thecodewhisperer.com/ (I read a couple from here but all seem
  good http://blog.thecodewhisperer.com/series#integrated-tests-are-a-scam)
- http://www.agitar.com/downloads/TheWayOfTestivus.pdf
- http://programmers.stackexchange.com/questions/301479/are-database-integration-tests-bad
