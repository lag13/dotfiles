PagerDuty
=========

https://support.pagerduty.com/hc/en-us/articles/202828690-PagerDuty-Quick-Start-Guide-Core-Concepts

My Quick Guide
--------------

### Schedule

Defines basically just a list of users who are considered "on-call" for some
amount of time. By itself schedules do nothing, they just define shich user is
on call at a given time.

### Escalation Policy

This defines who should be contacted when things go wrong. It can use
schedules for this. For example, a basica escalation policy might just be to
contact whoever is on-call according to a particular schedule. But it can get
more complicated because you can set up rules to the effect of, "if the person
we just notified did not address the issue for 10 minutes then notify this
other person" or "keep notifying the person who is on-call until they
respond". By itself this also does nothing.

### Service

This entity is what actually triggers alerts. A service is the middleman
between your monitoring tools and an escalation policy. When your monitoring
tools send an alert to the service it will invoke the escalation policy to
alert someone that something is wrong. You will set up an integration between
any number of monitoring tools (including a simple email send) and the service
you are working on.

### Team

This is really just a filtering mechanism. You add users and escalation
policies to a team and then based on that, pagerduty will be able to filter
lists of incidents, services, escalation policies, schedules, and users
depending on which team you pick.
