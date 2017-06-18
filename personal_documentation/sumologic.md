Sumologic
=========

- https://help.sumologic.com/

Custom Date range
-----------------

05/23/2016 12:00:00 AM - 05/23/2016 11:59:00 PM

Plain Search In Text
--------------------

Sometimes its really faster to just search for plain texts in the logs rather
than trying to break them into a structure and search in the fields of that
structure.

In this search the word "panic" is looked for in the logs.

```
_source=cb1-coreos-cluster* and panic
| json "_image" nodrop
| where _image matches "*hotpot*"
```

Or this will just search for these two strings in the logs.

```
"quay.io/cbone/hotpot-delete-customer-system" and "cleaning hal resources"
```

https://help.sumologic.com/Search/Search_Query_Language/Parse_Operators/Parsing_JSON_Logs

I guess it is better to filter out the logs as much as possible before
doing anything like json parsing. It seems that some fields like
"_sourceHost" are indexed by sumologic making those a good choice for
filtering. Doing that followed by some raw strings is also easy for
sumologic to narrow things down quickly. Then after that you can do
som parsing. `nodrop` is there so any logs that are missing all the
fields you require will not be dropped. `_raw` is sumologic's name for
the raw log message. With that in mind, this query searches for all
error logs or request logs (which were not the health-check ones) in
the tsr-api api.

```
_sourceHost=cbone-coreos-east1 AND "quay.io/cbone/tsr-api"
| json field=_raw "level", "request" nodrop
| where %level = "error" or %request != "HEAD /health-check HTTP/1.1"
```

Here's another one similar to the above:

```
_sourceHost=cbone-coreos-east1 AND "quay.io/cbone/tsr-api"
| json field=_raw "level", "request" nodrop
| where %level = "error" or %request matches "POST /tsr/*"
```

I'm not entirely sure how "matches" works though because if I do
`matches "POST /tsr/*/candidates"` it does not work. It seems that `*`
only works at the beginning and end of a string. So I'm not really
sure how you do regular expression stuff.
