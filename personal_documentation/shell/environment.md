Environment Variables
=====================

If you have a list of variables in a file `.env` like this:

```
a=you
b=hey
```

Then you can make all those variables environment variables by doing:

```
set -a
. .env
set +a
```
