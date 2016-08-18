TSR
===

Information about our beloved application Luceo... ehem I mean cb1, dammit I
mean TSR.

Anatomy of a TSR System
-----------------------

### Customer system Directory

The important bits here are:

1. The xml files - These configure aspects about the system's behavior.
2. settings.yml - This tells the system how to connect to its database and a
   couple other miscellaneous things like where to pull assets from.
3. .htpasswd - Contains passwords for connecting to things like webservices.

### Database

### External Assets

I think the images, css, and other miscellaneous things come from "cloudfront"
whatever that is.

Testing
-------

https://github.com/cbdr/luceo/blob/master/doc/tools/testing/testing.md

It is recommended to test things locally. When doing that you'll need the
appropriate local-conf.js file: 

```
exports.config = {

  capabilities: {
    browserName: "chrome"
  },

  // Specify the path to the chrome executable here. After installing protractor globally with
  // 'npm install protractor -g', running 'webdriver-manager update' creates the chromedriver
  // executable in the selenium directory. To aid in finding this directory, the default path
  // containing the chromedriver executable is output on the line beginning with '--out-dir' by
  // the 'webdriver-manager' command.
  //
  // Ex: chromeDriver: "/Users/jpalmour/.nvm/v0.10.33/lib/node_modules/protractor/selenium/chromedriver"
  chromeDriver: "/usr/local/lib/node_modules/protractor/selenium/chromedriver",
  directConnect: true,

  baseUrl: 'http://cb1-luceo-test.dev/bo_test.php/',

  specs: ['tests/*.js'],

  framework: "jasmine2",

  // Options to be passed to Jasmine-node.
  jasmineNodeOpts: {
    showColors: true,
    isVerbose: true,
    includeStackTrace: true,
    defaultTimeoutInterval: 30000
  },

  onPrepare: function () {
    require('./helpers/onPrepare');
  }
};
```

To run a specific test you do:

```
npm run func-test -- -s path/to/test
# For example
npm run func-test -- -s tests/functional/tests/managerView.js
```

Sandboxes
---------

### Assets

Sandboxes should load local assets. So the settings.yml file for
/www/php56-sandbox-13.luceosolutions-dev.com/settings.yml should have these
sorts of lines in it:

```
platform_settings:
	region: us

	cdn:
		url: http://cdn.php56-sandbox-13.luceosolutions-dev.com
```

### DB Connection Info

The settings.yml file should look something like this:

```
db:
	host: pike.cluster-ci7amtl2mm8i.us-east-1.rds.amazonaws.com
	replication: pike-b.ci7amtl2mm8i.us-east-1.rds.amazonaws.com
	user: php56_sandbox_13
	password: php56_sandbox_13
	db_name: php56_sandbox_13
```

Redis
-----

https://github.com/cbdr/luceo/blob/master/doc/components/technical/redis.md

One way to connect to redis is through telnet:

```
telnet luceo-redis.he32ni.ng.0001.use1.cache.amazonaws.com 6379
```

Quit by doing ctrl-] then typing "quit".

Migrations
----------

https://github.com/cbdr/luceo/blob/master/doc/process/migration/migration-scripts.md

Generate a migration, go to a customer directory and run:

```
./bin/console migration:generate "migration short description"
```

TODO
----

1. Can we add a check for tabs in the code that is being pushed to TSR and if
   there are some then circleci fails or something like that?
2. Go through TSR and try to turn any occurrence of tabs into spaces.

CoreOS
------

Before Christian Blades left he did some work allowing us to deploy docker
containers to coreos.

### Logging

I think how logs get sent to sumologic is that this repository (that Christian
also created: https://github.com/cbdr/hoseclamp) collects the output from
docker containers and forwards it to sumologic. It seems that it sends the
logs to sumologic as json
(https://github.com/cbdr/hoseclamp/blob/master/sumo/sumo.go#L46). Looking at
it more, it seems that it will interpret logs in the form logfmt and then add
them to the json object which gets sent to sumologic.
