Vendoring
=========

Vendoring (as I know it) is the act of committing dependencies along with your
own code instead of using a tool like composer to download the dependencies. I
think I like it basically because of the reasons outlined here:

http://devcenter.bitrise.io/docs/should-i-commit-my-dependencies-into-my-repository

Vendoring In Libraries
----------------------

- https://groups.google.com/forum/#!topic/golang-dev/WebP4dLV1b0
- https://github.com/mattfarina/golang-broken-vendor
- https://github.com/golang/go/issues/12432
- https://groups.google.com/forum/#!topic/golang-dev/nMWoEAG55v8%5B1-25%5D
- https://www.reddit.com/r/golang/comments/471ckr/how_to_fix_the_go_package_management_problem/
- https://github.com/golang/go/issues/12302#issuecomment-159017534
- https://github.com/golang/go/issues/13517

Once place you should probably not vendor is when you are writing library
code. I think you can do this if you really want to but only if those library
dependencies are in no way exposed to the outside world. Why you ask? Because
say you're using library X in your repository and X has version Y of some
dependency and as a parameter to one of its exported functions, it accepts a
parameter which is a defined type in Y. Now, in your code you also need to use
this repository Y and so you end up vendoring your own version Z. Now, to call
that exported function you need to pass in this type coming from Z but maybe
the type you construct using Z is not compatible with the type defined in Y.
The result is you need to vendor the same version of that dependency. Here's
an example of that error in action:

```
lls-lgroenendaa:hotpot-delete-customer-system lgroenendaal$ go build .
# github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/route53
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/route53/service.go:62: cannot use restxml.BuildHandler (type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler) as type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler in argument to svc.Client.Handlers.Build.PushBackNamed
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/route53/service.go:63: cannot use restxml.UnmarshalHandler (type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler) as type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler in argument to svc.Client.Handlers.Unmarshal.PushBackNamed
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/route53/service.go:64: cannot use restxml.UnmarshalMetaHandler (type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler) as type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler in argument to svc.Client.Handlers.UnmarshalMeta.PushBackNamed
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/route53/service.go:65: cannot use restxml.UnmarshalErrorHandler (type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler) as type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler in argument to svc.Client.Handlers.UnmarshalError.PushBackNamed
# github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/sqs
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/sqs/api.go:28: cannot use query.UnmarshalHandler (type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler) as type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler in argument to req.Handlers.Unmarshal.Remove
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/sqs/api.go:74: cannot use query.UnmarshalHandler (type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler) as type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler in argument to req.Handlers.Unmarshal.Remove
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/sqs/api.go:223: cannot use query.UnmarshalHandler (type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler) as type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler in argument to req.Handlers.Unmarshal.Remove
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/sqs/api.go:310: cannot use query.UnmarshalHandler (type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler) as type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler in argument to req.Handlers.Unmarshal.Remove
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/sqs/api.go:510: cannot use query.UnmarshalHandler (type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler) as type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler in argument to req.Handlers.Unmarshal.Remove
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/sqs/api.go:615: cannot use query.UnmarshalHandler (type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler) as type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler in argument to req.Handlers.Unmarshal.Remove
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/sqs/api.go:735: cannot use query.UnmarshalHandler (type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler) as type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler in argument to req.Handlers.Unmarshal.Remove
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/sqs/service.go:86: cannot use query.BuildHandler (type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler) as type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler in argument to svc.Client.Handlers.Build.PushBackNamed
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/sqs/service.go:87: cannot use query.UnmarshalHandler (type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler) as type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler in argument to svc.Client.Handlers.Unmarshal.PushBackNamed
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/sqs/service.go:88: cannot use query.UnmarshalMetaHandler (type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler) as type "github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler in argument to svc.Client.Handlers.UnmarshalMeta.PushBackNamed
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/sqs/service.go:88: too many errors
```

I think the way to "fix" this in general is just for libries to never vendor
their dependencies. If they really do need some external library it would be
best if they just had a glide.yml file (or something similar) specifying what
version they need. And if they really do need to vendor a library, then make
sure none of the types/functions in that vendor'd code are exposed in the API.


### To Fix the Error I Tried

### Making sure that the hotpot-worker wrapper pacakges did not expose any types from aws

DID NOT WORK

### Updating hotpot-delete-customer-system to use the exact same version of the aws dependency as hotpot-worker

DID NOT WORK

After doing this I did a diff on the two vendore'd folders and found this:

```
lls-lgroenendaa:hotpot-delete-customer-system lgroenendaal$ diff -r
vendor/github.com/aws/
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/
Only in vendor/github.com/aws/aws-sdk-go: LICENSE.txt
Only in vendor/github.com/aws/aws-sdk-go: NOTICE.txt
Only in vendor/github.com/aws/aws-sdk-go/private/protocol: query
Only in vendor/github.com/aws/aws-sdk-go/private/protocol: rest
Only in vendor/github.com/aws/aws-sdk-go/private/protocol: restxml
Only in vendor/github.com/aws/aws-sdk-go/private/protocol: xml
Only in vendor/github.com/aws/aws-sdk-go/private: waiter
Only in vendor/github.com/aws/aws-sdk-go/service: s3
```

The missing folder `query` was the one I was interested in since it showed up
in the errors. Strangely enough, when I looked at the hotpot-worker
dependency, that code did show up there:

```
</cbdr/hotpot-worker/
▸ .git/
▸ datadog/
▸ Godeps/
▸ hal/
▸ hotpot/
▸ route53/
▸ ses/
▸ sqs/
▸ ssh/
▾ vendor/
  ▾ github.com/
    ▾ aws/aws-sdk-go/
      ▸ aws/
      ▾ private/
        ▸ endpoints/
        ▾ protocol/
          ▸ query/      <--THIS GUY
          ▸ rest/
          ▸ restxml/
          ▸ xml/
            idempotency.go
            unmarshal.go
        ▸ signer/
      ▸ service/
      ▸ vendor/
        LICENSE.txt
        NOTICE.txt
    ▸ DataDog/
    ▸ go-ini/
    ▸ jmespath/
    ▸ kr/
    ▸ pkg/
    ▸ Sirupsen/
  ▸ golang.org/
  circle.yml
  README.md
```

So it seems that the issue is that when the vendor'ed aws inside hotpot-worker
imports `"github.com/aws/aws-sdk-go/private/protocol/query"` in the file
`vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/sqs/api.go
10`, it is actually referring to the "query" package that is defined in
`vendor/github.com/aws/aws-sdk-go/service/sqs/api.go`. I'm not sure exactly
how it determines whether or not types are the same but there is a type
conflict. I suppose that for a repository A when a package B inside A imports
another package C and uses its types and then B expects the C's types to be
A/C, if not then it is not the same type. That appears to be what is happening
here namely a package in hotpot-worker is importing this dependency
`"github.com/aws/aws-sdk-go/private/protocol/query"` and using its types.
Since it was imported from a file inside of hotpot-worker then it is expecting
the type to be something like
`"hotpot-worker/github.com/aws/aws-sdk-go/private/protocol/query"` but instead
the only thing it can find is
`vendor/github.com/aws/aws-sdk-go/service/sqs/api.go` which leads to our
conflict. When things are getting compiled a type probably becomes
"full-import-path.type" or something like that.

So, I wanted to see this problem in action. What I did was:

1. Removed the hotpot-worker dependency from the vendor folder and deleted
   references to it from Godeps.json.
2. Removed the aws dependency from the vendor folder and deleted references to
   it from Godeps.json.
3. Ran `godep save github.com/cbdr/hotpot-worker/...`

After doing that the directory structure looked like this:

```
<hotpot-delete-customer-system/
▸ .coreos/
▸ Godeps/
▾ vendor/
  ▾ github.com/
    ▾ aws/aws-sdk-go/
      ▸ aws/
      ▾ private/
        ▸ endpoints/
        ▾ protocol/
          ▸ query/
          ▸ rest/
          ▸ restxml/
          ▸ xml/
            idempotency.go
            unmarshal.go
        ▸ signer/v4/
      ▸ service/
        LICENSE.txt
        NOTICE.txt
    ▾ cbdr/hotpot-worker/
      ▸ datadog/
      ▸ hal/
      ▸ hotpot/
      ▸ route53/
      ▸ ses/
      ▸ sqs/
      ▸ ssh/
    ▸ DataDog/
    ▸ go-ini/
    ▸ jmespath/
    ▸ kr/
    ▸ pkg/
    ▸ Sirupsen/
  ▸ golang.org/
▸ work/
```

Notice there is no vendor folder in the hotpot-worker repository. It would
seem that this is a bug where godeps will vendor dependencies' vendored
dependencies rather than use the vendor folder inside the dependency. I'm not
sure when this changed? Because we used to have this structure of maintaining
the "vendor" folder inside of the hotpot-worker repository. Unless I used to
do things differently?

Then I tried this command `godep update github.com/cbdr/hotpot-worker/...` and
that seemed to add the "vendor" folder from hotpot-worker but not everything
from the vendor folder was copied over (that query package is still missing).
I have no idea what is going on there. Here is what the directory tree looked
like:

```
▾ github.com/
  ▾ aws/aws-sdk-go/
    ▸ aws/
    ▾ private/
      ▸ endpoints/
      ▾ protocol/
        ▸ query/
        ▸ rest/
        ▸ restxml/
        ▸ xml/
          idempotency.go
          unmarshal.go
      ▸ signer/v4/
    ▾ service/
      ▸ route53/
      ▸ sqs/
      LICENSE.txt
      NOTICE.txt
  ▾ cbdr/hotpot-worker/
    ▸ datadog/
    ▸ hal/
    ▸ hotpot/
    ▸ route53/
    ▸ ses/
    ▸ sqs/
    ▸ ssh/
    ▾ vendor/
      ▾ github.com/
        ▾ aws/aws-sdk-go/
          ▸ aws/
          ▾ private/
            ▸ endpoints/
            ▾ protocol/             <--- Notice no query package
                idempotency.go
                unmarshal.go
            ▸ signer/v4/
          ▸ service/
          ▸ vendor/github.com/
        ▸ DataDog/datadog-go/statsd/
        ▸ kr/
        ▸ pkg/
        ▸ Sirupsen/
```

Then I tried removing the aws dependency `rm -fr vendor/github.com/aws/` and
running `godep update github.com/cbdr/hotpot-worker/...` again. Doing that
appeared to change literally nothing.

It looks like godep has "trouble" with nested vendor directories:
https://github.com/tools/godep/issues/444. And actually it appears that godep
tries to produce a single top level vendor folder as indicated by the last
comment on this issue: https://github.com/tools/godep/pull/446. But this is
confusing because running that "update" command seems to add the vendor folder
into the hotpot-worker dependency, perhaps that is a bug. Here is another
issue describing this whole issue: https://github.com/tools/godep/issues/428.
Here is another good thread on the topic:
https://github.com/Masterminds/glide/issues/303.

Holy shit!! It looks like some go people made a library for package managers:
https://github.com/sdboyer/gps. It looks like this repository does not vendor
its dependencies and uses glide to specify them. I think the fact that they
use glide says something about how good glide is?

I wanted to learn more about how import paths get resolved so I looked at the
language spec and did not find it there. That is pretty cool I think, so the
language spec does not force those imports to mean anything at all and its
just the tooling that determines what they mean. That makes things flexible.
Doing `go help gopath | less` gives some good information as does `go help
importpath | less`.
