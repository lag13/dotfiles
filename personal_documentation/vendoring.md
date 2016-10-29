Vendoring
=========

Vendoring is the practice of storing the code of your dependencies directly
inside of your repository as opposed to using a package management tool to
pull the necessary dependencies from their sources. Whether you vendor or not,
you should still use some sort of package management tool so you know exactly
what versions of dependencies that you have.

Vendoring in general should not happen in libraries because you run the risk
of the library exposing a vendored type in its public API. The issue that can
cause is that library A vendors version B of some library and the argument to
one of its publically available functions takes a type from this library B.
Now in your repository C you use A and vendor your own different version of
that same library D. Now if you pass that type from D into A, it might not
work if the types have become incompatible. So don't vendor in library code.

I do like vendoring in application code because it means the application is
one working unit that needs no other tooling to start up. The only issue with
it was reviewing PR's that had vendor changes but I think we found a
workaround. Just have one branch with all the vendor changes
"branch-with-changes-vendor" then have another branch with all your code
changes "branch-with-changes". When you make the PR have the base branch of
this PR be "branch-with-changes-vendor", that way you'll only see the code
changes. Then when you're ready to merge, switch the base branch to master!
Easy.
