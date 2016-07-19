Composer
========

Composer is an evil bastard. I'm probably being too harsh though. He's
probably not so bad, I just don't understand him very well.

https://getcomposer.org/doc/01-basic-usage.md
https://getcomposer.org/doc/articles/troubleshooting.md
https://gist.github.com/Xeoncross/8059819
https://getcomposer.org/doc/articles/versions.md

Commands
--------

https://getcomposer.org/doc/03-cli.md

composer show package/name - shows information about the package

Private Git Repository
----------------------

I had a lot of trouble installing a private git repository. Here are some of
the errors I encountered and how they were fixed.

### Could Not Find Package

Github limits how many requests a particular person can make. Not sure exactly
why, its probably a good practice to not let people just download tons of
stuff. To avoid this rate limit problem you'll need to download an oauth token
from github and add it to auth.json. 

When I was trying though I was running into an issue where I had a token but
it asked me for another one:

```
Your GitHub credentials are required to fetch private repository metadata
(git@github.com:cbdr/featureflags.git)
Head to
https://github.com/settings/tokens/new?scopes=repo&description=Composer+on+lls-lgroenendaal1.local+2016-05-17+2241
to retrieve a token. It will be stored in
"/Users/lgroenendaal/luceo_dockermaster/auth.json" for future use by Composer.
Token (hidden):
Token stored successfully.

  [Composer\Downloader\TransportException]
  The "https://api.github.com/repos/cbdr/featureflags" file could not be
  downloaded (HTTP/1.1 404 Not Found)
```

I don't know why they asked for a token but that exception message says it
all. The package I wanted could not be found. The problem here is that this
repository is private so it cannot be found over the public API. The solutions
is that for any private repository you need to add a line like this:

```
{ "type": "vcs",    "url": "git@github.com:cbdr/php-featureflags.git" }
```

Inside this array:

```
"repositories": [
]
```

So that composer knows how to find the repository.

### The Package Could Not Be Found In Any Version

After fixing that problem above I ran into this one:

```
Your requirements could not be resolved to an installable set of packages.

  Problem 1
    - The requested package cbdr/php-featureflags could not be found in any version, there may be a typo in the package name.

Potential causes:
 - A typo in the package name
 - The package is not available in a stable-enough version according to your minimum-stability setting
   see <https://getcomposer.org/doc/04-schema.md#minimum-stability> for more details.

Read <https://getcomposer.org/doc/articles/troubleshooting.md> for further common problems.
```

This one tripped me up for a long time. It turns out the issue is that the
name of the package defined in composer.json was cbdr/featureflags instead of
cbdr/php-featureflags. I don't know why Christian did this, it seems like
giving the package a different name than the repository is confusing. So in
general, when adding the repository the list of `require`d things (or
`require-dev` if this tool is only being used for development purposes) add
the string that appears in the `name` value at the top of the composer.json
file.

### Renaming a Package

https://getcomposer.org/doc/05-repositories.md#vcs

When working with a vcs like github.com the name of the package is the name
specified in the composer.json file in the default/master branch. So if you
want to rename a package, that change has to be pushed to master. This took me
so long to figure out...

### Stability

https://getcomposer.org/doc/04-schema.md#minimum-stability

Composer has a notion of stablity. If a library/repository is found to have a
lower stablity than the "minimum-stablity" defined in composer.json then
composer will complian. These are the stablity levels ordered from most stable
to least:

1. stable
2. RC
3. beta
4. alpha
5. dev

So if youre minimun-stablity is set to be "stable" and you try to bring in a
repository marked with a "beta" level of stability then it will fail. However!
You can set the minimum-stability on a per-package basis. For example:

```
"mycool/package": "0.1@dev"
```

This means that we will import this specific package even though it could have
the lowest stability level.

One "gotcha" here is that if mycool/package imports a package which relies on
a package with a low stability setting then it will error out. This is
probably just to prevent you from unknowingly installing dependencies with a
low stability level. The solution is to install those dependencies with the
@dev flag as well. Here is an example of that failing in action:

```
lls-lgroenendaal1:php-featureflags lgroenendaal$ composer install
Loading composer repositories with package information
Updating dependencies (including require-dev)
Your requirements could not be resolved to an installable set of packages.

  Problem 1
    - Installation request for protobuf-php/protobuf-plugin ^0.1@beta ->
      satisfiable by protobuf-php/protobuf-plugin[v0.1-beta].
    - protobuf-php/protobuf-plugin v0.1-beta requires protobuf-php/protobuf
      ~0.1@dev -> satisfiable by protobuf-php/protobuf[0.1.x-dev, v0.1-beta]
      but these conflict with your requirements or minimum-stability.
```

So what's happening here is that in our composer.json file we are trying to
bring in the dependency:

```
"protobuf-php/protobuf-plugin": "^0.1@beta"
```

But in that package's composer.json file it relies on another package:

```
"protobuf-php/protobuf": "~0.1@dev",
```

Composer sees this and probably thinks to itself, "I know they are willing to
install 'protobuf-php/protobuf-plugin' with a dev level stability but what if
they don't realize that that package also imports a package with a dev level
of stability. I'll just let them know and they'll deal with it". I guess doing
this kind of makes sense but I think I'd prefer it if it just installed the
package with no fuss. If you're installing a package with a dev level of
stability then you are probably expecting it to be unstable and so if they
import unstable things it wouldn't be unexpected.

### Use a specific branch

If you want composer to use a specific git branch just prefix it with "dev".
For example 

```
"cbdr/php-code-coverage-reporter":"dev-my-cool-branch",
```

### Add/Update Package

https://getcomposer.org/doc/01-basic-usage.md#installing-dependencies
http://stackoverflow.com/questions/15212381/composer-how-can-i-install-another-dependency-without-updating-old-ones

You can call composer's `update` command on a specific package which will add
the package if it does not exist and update it otherwise. Note that before
doing this you must add the package in composer.json:

```
composer update cbdr/package
```

You can also run composer's "require" command which will add the dependency to
composer.json and install it:

```
composer require new/package [version_constraint]
```

The composer.lock file has two fields "hash" and "content-hash". These are
apparently used to detect if composer.lock is out of date and should be
updated or something like that. If you want to update the hash's but not any
packages you can run
(https://groups.google.com/forum/#!topic/composer-dev/5xEd3UPIps0):

```
composer update nothing
```

Memory
------

In my experience, composer is a memory grubbing monster. If you run into a
memory error (Fatal error: Allowed memory size of 1073741824 bytes exhausted)
then change the `memory_limit` option in php.ini.

Cache
-----

There is a cache in ~/.composer/cache and I think there might be some sort of
cache in the "vendor" directory of a particular repository.

PSR-0
-----

All it is using namespaces to represent paths on the file system. If you do
that then classes can be found easily.

ONLY Update Dependencies
------------------------

When I was working on featureflagging for TSR at careerbuilder sometimes I
wanted to update a composer dependency. The "problem" was that after an update
composer was configured to run some scripts. If I ran composer update locally
those scripts could not run. So the only way I could run composer update was
to start up the tsr docker container and run composer from there. Not a big
deal really but I just wanted to do it locally. This command is how I did
that:

```
# Updates dependencies and does nothing else
composer update --no-scripts
```

composer.lock hash merge conflict
---------------------------------

I've run into the situation where there is a merge conflict on the hash of the
composer.lock file. The hash is used to detect if composer.lock is out of date
(https://groups.google.com/forum/#!topic/composer-dev/5xEd3UPIps0). To update
the hash (so the correct one is chosen) just run:

```
composer update nothing
```
