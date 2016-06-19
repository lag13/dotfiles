<?php

// This command outputs a bunch of information about the php installation like
// where the php.ini file is located. You can also run the command `php -i`
// which does the same thing.
echo phpinfo();

/*
The php.ini file is a configuration file which controls aspects of PHP's
behavior. For mac, php is installed by default but it seems there is no
php.ini file used. Instead it looks like there is a default php.ini in
/etc/php.ini.default. So if you wanted to change the behavior of the default
mac php then rename that file to php.ini and change what you need. I have no
idea why they did this. Why not just have php.ini file to begin with?

When I installed a newer version of php from here http://php-osx.liip.ch/ I
seemed unable to change the memory limit in php.ini. It turned out that there
was ANOTHER php.ini file which was changing values called
99-liip-developer.ini. I guess I could have found it if I looked a little
harder. If you run this command then it tells you a directory where additional
.ini files might exist:

```
php -i | grep "Scan this"
```

At least for the version I installed that file could be deleted or altered as
needed.
*/
