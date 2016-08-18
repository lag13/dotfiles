Node
====

Is some backend javascript framework? I'm not really sure how it works. The
reason I'm confused is that the code is (I think) supposed to be just
javascript but their are aspects of the code which look really different than
plain old javascript. So is it just a javascript library? Or is it more of a
new language entirely?

https://www.sitepoint.com/understanding-module-exports-exports-node-js/
http://radar.oreilly.com/2011/07/what-is-node.html

Node is supposed to run stand alone javascript programs.

What is It
----------

Node is a program which runs javascript programs without the web browser. It's
like doing `php my-php-script.php` or `go run main.go` but for javascript.

The node binary, which actually runs javascript files, is written in C. I
think if you write a node application then you ARE writing javascript but you
have more tools at your disposal. Like if you took a node javascript file and
sent it to the web browser, maybe it would not work.

For example, the 'require' function is defined by node.js but not standard
javascript:

  http://stackoverflow.com/questions/9901082/what-is-this-javascript-require

What I'm unsure of though is whether the 'require' function is a normal
function defined in a library somewhere or whether it is something more
"special". Like its something defined in the node interpreter or something.

require and module.exports = something
--------------------------------------

You'll see code like this all over node programs:

```
let Sequelize = require('sequelize');
module.exports = User;
```

module.exports is some global object for a file. The value of this variable
gets returned from doing a `require` function. So this is how you import
functions and classes and such.
