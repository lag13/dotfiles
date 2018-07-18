<?php

/*
Error handling is... confusing in php to say the least. It appears
they did try to simplify things in php 7 which is good but as I still
work with php 5.6 this is all still relevant. In general it appears
that PHP is trying to move away from its concept of errors:
https://stackoverflow.com/questions/841500/php-exceptions-vs-errors.

With PHP there is the concept of both an "error" and an "exception".
An exception is what you think it is namely a way to signify some sort
of error and when one is thrown it will keep unwinding the callstack
until it gets caught.

Errors seem to be more closely tied to the inner workings of php. A
better name for them would probably be "signal". In other words it is
very much a PHP specific thing. Keep in mind that they are COMPLETELY
DIFFERENT than exceptions. These are all the errors that PHP can raise
http://php.net/manual/en/errorfunc.constants.php. Note that most
appear to be about things like alerting about "deprecation" and
"warnings" rather than actual "errors" that need to be addressed right
then and there. Since I hate run time warnings and the like when it
comes to programming I really hate this feature already. Other things
like E_ERROR will get generated when something *really* bad happens.
If an error of this level gets generated the PHP process will have a
"fatal" error and abort. You can raise errors yourself with
trigger_error() but you cannot do that for all error conditions.

So, my initial impression/summary of error handling in PHP is that it
sucks because:

- The naming is confusing: You have the concept of an "exception" and
  "error" which sound like they could be similar based on the name but
  really are not. Note that this has been remedied a bit as of php 7.0
  where they converted more things to exceptions however they added a
  new exception type called "Error" which is just damn confusing
  because now you have "PHP Errors" and the "Error" exception:
  http://php.net/manual/en/language.errors.php7.php.

- PHP errors are inconsistent in how they work which is confusing.
  Some errors are warnings so they might cause a log to be written but
  otherwise the code execution continues as normal. But then you've
  got things like E_ERROR which cause a fatal error and termination of
  process. And the fact that you can register a callback to execute
  whenever an error occurs EXCEPT for certain errors like E_ERROR is
  confusing as well. WHY would you group these two things under the
  same umbrella of "error" if they are so different. I hate that they
  even include E_ERROR as a constant if you cannot really program with
  it.

In php 7.0 more things that were "php errors" have been converted to
exceptions. For instance in php 5.6 if you called a method on a "null"
object then it would raise an E_ERROR which would end up terminating
the script. But in 7.0 it just generates an exception.

*/

// When an uncaught exception occurs the function custExceptionHandler
// will be invoked. Think of this as activating a global catch
// statement:
// http://php.net/manual/en/function.set-exception-handler.php. Since
// the exception is "caught" the exit code of php will be 0 unless the
// "custExceptionHandler" function kills the script with a specific
// exit code.
set_exception_handler("custExceptionHandler");
// Whenever a "php error" occurs then this function will be called
// instead of the default one (which basically just prints out
// everything it can about the error PHP error):
// http://php.net/manual/en/function.set-error-handler.php. Even
// though you can configure this function to get called on "all"
// errors it does not actually get invoked for all of them as the
// documentation points out.
set_error_handler("custErrHandler", E_ALL);
// This will ALWAYS be invoked before the php script terminates:
// http://php.net/manual/en/function.register-shutdown-function.php.
// It CANNOT alter the exit code of the php script. Its strictly for
// doing random side effects.
register_shutdown_function("custShutdownHandler");

try {
    trigger_error("some random php error but code execution will continue!", E_USER_WARNING);
    trigger_error("E_ERROR is invalid in this context and it feels dumb that it can even be specified in the first place", E_ERROR);
    $obj = null;
    $obj->hello();
} catch (\Exception $e) {
    echo "caught exception:\n";
    var_dump($e);
} catch (\Throwable $e) {
    echo "caught throwable:\n";
    var_dump($e);
} catch (\Error $e) {
    echo "caught error:\n";
    var_dump($e);
}

// http://php.net/manual/en/errorfunc.constants.php (the first 16
// constants happen to be the error types).
function readableErrorConstant($errno) {
    return array_flip(array_slice(get_defined_constants(true)['Core'], 0, 15))[$errno];
}

function errorConstantAndCode($errno) {
    return $errno . " (" . readableErrorConstant($errno) . ")";
}

function printErrConst($errno) {
    echo "err num is: " . errorConstantAndCode($errno) . "\n";
}

function custExceptionHandler($e) {
    echo "entered custExceptionHandler\n";
    echo "exception is\n";
    var_dump($e);
}

function custErrHandler($errno, $errstr) {
    echo "entered custErrHandler\n";
    printErrConst($errno);
    echo "err str is $errstr\n";
}

function custShutdownHandler() {
    echo "entered shutdown handler\n";
    $error = error_get_last();
    if ($error !== NULL) {
        $errno = $error["type"];
        $errstr = $error["message"];
        printErrConst($errno);
        echo "err str is $errstr\n";
    } else {
        echo "no error reached the shutdown handler\n";
    }
}
