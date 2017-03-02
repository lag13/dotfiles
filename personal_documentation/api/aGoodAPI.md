What Makes a Good Network API
=============================

First note that having a good API does not imply that the application must be
have a microservices architecture. You can have a monolith and still have good
APIs. I think that maybe having a microservices architecture would encourage
you to have good API's because you actually consume your own APIs to have
things work but you can still have good APIs with a monolith architecture.

Easy to use
-----------

1. Quick to integrate with
2. Less likely to make a mistake with the integration

Coverage of the Application the API Operates On
-----------------------------------------------

The functionality the API provides covers most if not all of the system that
the API is an interface for. Because if a system can do 10 things but using
the API you can only do 1 of those things, then its not a very good API.

If we had the nicest API to list candidates and requisitions in TSR that would
satisfy the "Easy to use" case but since that's all they can do and TSR itself
can do a lot more, then this is not a good API for TSR because it doesn't let
you do a lot.

Diagram
-------

- Good API:
  - Easy for clients to use it
    - Quick for clients to integrate with it.
    - Less likely to make a mistake with the integration.
  - The API covers most if not all of the functionality of the application. In
    other words, anything the application can do, the API is able to tell the
    application to do that.
    - The application is now flexible. It is no longer one block of
      functionality it is more like a collection of services and those
      services can be pieced together however they need to be pieced together.
      In some sense you could picture just a vast sea of API's offering
      functionality but no applications, then you can use whichever API's you
      want to build whatever application you want.
