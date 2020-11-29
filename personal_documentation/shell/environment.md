Environment Variables
=====================

If you have a list of variables in a file `.env` like this:

```
a=you
b=hey
```

Then you can make all those variables environment variables by doing:

```
export $(cat /password/file | xargs)
```


Get the environment of a running process `strings /proc/<PID>/environ`:
https://ma.ttias.be/show-the-environment-variables-of-a-running-process-in-linux/
