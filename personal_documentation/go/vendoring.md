Vendoring
=========

- https://groups.google.com/forum/#!topic/golang-dev/WebP4dLV1b0
- https://github.com/mattfarina/golang-broken-vendor
- https://github.com/golang/go/issues/12432
- https://groups.google.com/forum/#!topic/golang-dev/nMWoEAG55v8%5B1-25%5D
- https://www.reddit.com/r/golang/comments/471ckr/how_to_fix_the_go_package_management_problem/
- https://github.com/golang/go/issues/12302#issuecomment-159017534
- https://github.com/golang/go/issues/13517

Here's what I know about vendoring with go:

How Vendoring Works In Go
-------------------------

Before we can talk about aspects of vendoring, we should first describe how it
works which is described here: https://golang.org/s/go15vendor.

Say we have this structure in our code:

```
$GOPATH/src
+- a
|  +- a.go
+- b
|  +- b.go
|  +- vendor
|     +- a
|     |  +- a.go
|     +- c
|     |  +- c.go
|     |  +- vendor
|     |     +- a
|     |     |  +- a.go
|     |     +- d
|     |        +- d.go
|     +- e
|        +- e.go
+- d
   +- d.go
```

We've got a lot going on here. One thing to note is that we've got three
packages `a` which are exactly the same but their location is different:

- One is in the GOPATH
- One is vendored directly insided of b
- One is vendored in c which is vendored in b

Here is the description of what vendoring is:

```
If there is a source directory d/vendor, then, when compiling a source file
within the subtree rooted at d, import "p" is interpreted as import
"d/vendor/p" if that path names a directory containing at least one file with
a name ending in “.go”.
```

Let's relate that to the above diagram. If we are inside the `b` repository
and we are compiling code, if `b.go` does an `import "a"` then, since there is
a `b/vendor` folder *and* since `b/vendor/a` has ".go" files in it, the
`import "a"` is rewritten/interpreted as `import "b/vendor/a"`. 

Now lets say that `c.go` as the statment `import "a"`, what does that get
rewritten to? Well, since `c.go` is inside `c` and `c/vendor` exists we look
there first. We find `c/vendor/a` with ".go" files inside so we have found our
dependency! The import path will then be rewritten/interpreted as `import
"b/vendor/c/vendor/a"` (a mouthfull I know). This illustrates that when a
package is found inside a `vendor` folder go probably walks up all the way to
"$GOPATH/src" to determine the import path.

Now lets say that for some reason, `c.go` has an `import "e"` statement (I
know its odd that `c` does not vendor `e` but just roll with it). Since we are
in `c` and there is a `vendor` in `c` we look there first. We look for a
directory `c/vendor/e` with ".go" files inside. Since we do not find any we go
*up* above `c` to the next directory that has a `vendor` directory and look
there. In this case we find that `b` has a `vendor` directory so we look for
`b/vendor/e`. We find it! So now the `import "e"` lines will be
interpreted/rewritten as `import "b/vendor/e"`. If we couldn't find `e` in any
vendor directories then we would look in the `GOPATH` and if we couldn't find
it there we'd look at `GOROOT` and if its not there then its not anywhere.

This "walking up through vendor folders looking for the dependency" was not
clearly indicated to me in the documentation. A quote from this thread
(https://github.com/Masterminds/glide/issues/303) sums it up nicely though:

```
The way go build and the other go tools work is to look in the vendor/
directory a requesting package is in to find a dependency. If it doesn't find
it there it walks up the directory tree searching in each vendor/ directory
for it. If not found in a vendor/ directory it goes back to the traditional
methods of the GOAPTH and then GOROOT.
```

One more small example, if `b.go` does `import "d"` then the `d` will come
from `GOPATH/src/d`. It will *not* come from `b/vendor/c/vendor/d` because you
only check vendor at the current level and if you don't find it there you keep
going up, not down.

That should, hopefully, cover how imports work in go.

In *general*, vendoring code in libraries is bad (but its not the end of the world)
-----------------------------------------------------------------------------------

First off, get the thought out of your head that vendoring code in a library
automatically results in a big mess of code that is very hard to manage, will
not compile, and if it does compile it will panic when it runs. These things
could happen but if you know what is happening behind the scenes you should be
okay. The thing to keep in mind is that vendoring code in a library can work
just fine, but it has the potential to cause problems down the road. So try to
understand the consequences and determine the best choice for your situation.

Honestly, after reading about the issue part of me doesn't really know what is
so bad about it. I feel like people talk about it like its the worst thing in
the world but after looking into it, if you are aware of the implications then
you should be just fine. I suppose that's the rub though isn't it? Everyone
will never be aware of all the implications and its only a matter of time
before something breaks because of it. Its probably a better strategy to have
these hard rules like "never vendor in a library" because that is simple to
adhere to and will protect people later on. I just spent almost 2 whole days
researching this stuff for pete's sake! Not everyone has that luxury.

### Vendoring code inside a library is (probably) safe if:

```
The vendored code is not exposed in the library's public API.
```

If this happens then the vendored code cannot be seen outside of that library
so there cannot be any conflict. I can think of 2 situations (there could be
more) where having a library vendor code makes sense:

- The vendored code is used as some sort of utility.
- The library is wrapping up some other dependency and making the interface to
  it more clean/straightforward.

I say "probably" because even if the vendored code is not exposed in the
library's public API, there is a chance that things still can go wrong. I'll
elaborate on that in the next section.

#### One Issue I Had With godep and vendored Dependencies

I personally ran into an issue where I vendored library `A` and another
library `B` also vendored `A`. Even though `B` did not expose any types from
its vendored version of `A` (so it "should" have been safe) I still ran into
compilation errors. The fault though lay with the `godep` dependency manager
and NOT with the go tooling. Basically `godep` was doing some weird stuff
where part of the aws dependency got put in the vendor folder in the root of
the repository and part of it was in the vendor folder of a vendored
dependency. This lead to errors like this:

```
vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/service/sqs/service.go:88:
cannot use query.UnmarshalMetaHandler (type
"github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler)
as type
"github.com/cbdr/hotpot-delete-customer-system/vendor/github.com/cbdr/hotpot-worker/vendor/github.com/aws/aws-sdk-go/aws/request".NamedHandler
in argument to svc.Client.Handlers.UnmarshalMeta.PushBackNamed
```

In the above error the `query` package was defined in the root vendor folder
but not in the vendor folder of the dependency. So when the import statement
`"github.com/aws/aws-sdk-go/private/protocol/query"` was made in the vendored
dependency of the vendored library, it did not find it in the vendor folder it
was in and went up a directory and found it in the next vendor folder and I
guess that package's `query.UnmarshalMeta` is considered to be a different
type than what is expected. Here's a picture:

```
<hotpot-delete-customer-system/
▸ Godeps/
▾ vendor/
  ▾ github.com/
    ▾ aws/aws-sdk-go/
      ▾ private/
        ▾ protocol/
          ▸ query/
            idempotency.go
            unmarshal.go
    ▾ cbdr/hotpot-worker/
      ▾ vendor/
        ▾ github.com/
          ▾ aws/aws-sdk-go/
            ▾ private/
              ▸ endpoints/
              ▾ protocol/         <---- Notice that this folder does not have the "query" folder like it should
                  idempotency.go
                  unmarshal.go
```

Again though, I don't think this should reflect badly on vendoring code in
libraries. It should only reflect badly `godep`. Here are some issues related
to vendoring stuff (not all of them are directly related to my issue):

- https://github.com/tools/godep/issues/498
- https://github.com/tools/godep/issues/400 (lol the last comment)
- https://github.com/tools/godep/issues/444
- https://github.com/tools/godep/issues/428
- https://github.com/tools/godep/issues/224

### Vendoring code inside a library could lead to heartache when:

Your library accepts or returns types from a vendored package. This issue is
best illustrated here: https://github.com/lag13/golang-broken-vendor. Even if
you expose types though, you won't necessarily run into issues, always keep
that in mind. You *might* though and in programming, if something might happen
then it probably will happen. Also, the fact that you are directly using the
type of a dependency's dependency just feels weird.

I think the really bad thing about libraries vendoring (and why people
advocate against it) is because of global state changes. For example, the
`github.com/go-sql-driver/mysql` package uses an `init()` function which
changes global state in the `database/sql` package by calling this line
`sql.Register("mysql", &MySQLDriver{})`. Note that if this `Register` function
registers the same name twice, `database/sql` will panic. So if you vendor
`github.com/go-sql-driver/mysql` and you also vendor a dependency which
vendors `github.com/go-sql-driver/mysql` then you'll get a panic because two
calls to the `sql.Register("mysql", &MySQLDriver{})` will occur. You can
imagine other scenarios as well like if two vendored copies of the same
library modify `http.DefaultServeMux`. This sort of global state change sounds
like it could become a huge headache to deal with.

