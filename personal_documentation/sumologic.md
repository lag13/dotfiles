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
