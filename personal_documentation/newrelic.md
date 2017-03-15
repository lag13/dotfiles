Newrelic
========

- https://docs.newrelic.com/
- https://learn.newrelic.com/
- https://docs.newrelic.com/docs/agents

It does a lot of things related to making sure your applications aren't
destroying themselves.

Application Performance Monitoring (APM)
----------------------------------------

- https://docs.newrelic.com/docs/apm
- https://learn.newrelic.com/courses/intro_apm

Monitoring for your web applications. It can tell you how long each
transaction (request+response) is taking per endpoint. It can also provide a
detailed breakdown per endpoint, which can see things like sql query times,
other api calls, etc... You could probably get the same sort of functionality
as APM by having a lot of datadog metrics and creating a lot of dashboards,
but this is nice and standardized *and* some of their language libraries will
literally do all the metric work for you (it depends on the language+framework
though).

### Definitions

- Transaction - I believe this is the whole request+response on a per endpoint
  basis.
- Segment - A transaction is broken down into a series of "segments". I think
  a segment can technichally be any piece of code that you deem to be a
  segment. Some common examples are sql queries and various code files in the
  language of your choice. It's pretty neat because you can get a high level
  view of which endoints are taking longest with transactions and then with
  segments you can identify what part of the transaction takes the longest.
- Apdex - Application performance index is a number in the range [0,1] which
  tries to measure user satisfaction with an application in regards to speed
  of the application. 0 is no customers satisfied while 1 is all customers
  satisfied. I think you could technichally have an apdex score for anything
  which involves timings. For example newrelic appears to have an application
  apdex score (so the application itself) and a browser apdex score (so what
  the user is experiencing). http://apdex.org/overview.html,
  https://docs.newrelic.com/docs/apm/new-relic-apm/apdex/apdex-measuring-user-satisfaction.

### Monitoring Everywhere

APM provides 5 types of monitoring:

1. Application - monitors your "back end" code and how long everything takes.
2. Server - Monitors the server at the OS level (disk space, RAM, cpu, etc...)
3. Browser - There is a newrelic javascript agent which provides data from the
   users perspective on how responsive your site is being.
4. Mobile - I think similar to browser monitoring but for mobile applications.
5. Monitors/Pingers - You can set up pings to your applications in APM but I
   think you can also do that with newrelic synthetics. I'm not sure if one is
   better than the other.

### Monitor Deployments

Looks like there is a newrelic API for recording deployments. That seems
pretty neat. I guess sending out a particular request on app startup probably
designates a deployment. I think a deployment is considered to be an "event"
but I'm not sure of this.

### Tabs In the UI

Apparently each UI could have slightly different information depending on
which language agent is being used. Neat!

#### Transactions

There is a tab in the UI which details all the transactions. You can drill
down into those transactions to view all the segments making up those
transactions.

#### Databases

There is a tab in the UI which details just database queries.

#### External Services

There is a tab in the UI which details just external service requests such as
a call to an external API.

Creating a Monitor
------------------

https://docs.newrelic.com/docs/synthetics/new-relic-synthetics/using-monitors/add-edit-monitors

A monitor is something that can interact with an application (which could be
as simple as ping'ing an endpoint) with the goal of verifying that the
application is up and running.

Alerting
--------

- https://docs.newrelic.com/docs/alerts

