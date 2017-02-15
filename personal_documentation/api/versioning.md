Versioning An API
=================

You Should Make Version Mandatory
---------------------------------

This discussion only pertains to an http API where the API can support
multiple versions and the client gets to pick which one to use. At the moment,
I can't really say how you should version your API (url, Accept header, custom
version header, query string, etc...) but whatever scheme you choose I think
that specifying the version should be mandatory for the client. Here are my
thoughts as to why.

First off, we have two choices. We can either make the version mandatory or
make it optional.

### Mandatory

Nothing much to talk about. If the client does not specify the version then
some sort of 4XX status code will be returned with a message saying that a
version must be specified.

### Optional

If the version specification is optional then we need to decide what version
to use when it is not included. The two choices are probably going to be:

- The oldest version of the API
- The newest version of the API

But you could technically route to any version. For instance you could route
them to the second newest version. Whatever you choose I think automatically
routing them to the newest version is silly since something is bound to break
eventually. So your only real option is to route them to some older version of
the API.

### Which To Choose

So our two choices are:

- Make version mandatory.
- Make version optional and default to a non-latest version of the API.

I vote for making the version mandatory because:

- In general, I like being more explicit when possible instead of relying on
  defaults because it means everything is more out in the open which makes
  things easier to understand. For example, I have an easier time reading a
  function if everything the function receives came from a parameter.
- If we default the version then our clients are relying on a defaulted value.
  I don't like programs relying on a defaulted value if that defaulted value
  has a good probability to change AND that change could break the program. In
  this case the default version has a good probability to change AND a version
  change implies that there are breaking changes meaning that it could break
  the program. An example of a default that I'm okay relying on: as of Go 1.7
  if you don't specify a Transport field when building an http client then it
  will use a default transport struct. Relying on this is okay to me because
  that thing is a little complicated and any changes to those defaults are
  unlikely to hurt me.
- If version is optional then SOMEONE will not specify it (either they
  overlooked it in the documentation or were lazy and didn't want to write the
  code to specify the version). At this point they are relying on a defaulted
  version which has the issues I described above. But if people are forced to
  specify the version then they are aware of the versioning and if they need
  to update the version it should be more simple to do, all they have to do is
  change a number.
