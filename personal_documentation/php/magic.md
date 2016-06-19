There are "magic methods" which are methods which get called when no method
actually exist. Very interesting.
http://php.net/manual/en/language.oop5.magic.php

So for example if we have an object and we define a `__call()` method and we
try to call a method on an object which does not exist then the `__call()`
method will be triggered.
