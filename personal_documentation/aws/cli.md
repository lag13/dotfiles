AWS Command Line
================

https://aws.amazon.com/cli/

AWS has a command line tool for interacting with aws services which is neat
because then you can script things if you want. Its also just neat sometimes
to be able to do things without needing to click around the UI on the aws
console.

Configure
---------

To make API requests to any amazon services you need 2 pieces of information:

1. Access key ID which looks something like AKIXIVJALUIKYN7ODJSB
2. Secret access key which looks something like
   xrvcai2uTgxMbtlwyw8ZHrZPbWajy8U9V4BJ6ePl

So your `aws` command line tool must have that information. Running `aws
configure` is a quick way to get your configuration set up. The result of that
command will be the creation of an ~/.aws directory with the configuration
information.


Example Commands
----------------

### S3

I'm not quite sure but s3 seems to be a little different than normal bash
commands. I think the thing to keep in mind that when you specify a
"recursive" option that it will act on the *contents* of the "folder"/bucket
you specified. So doing something like `aws s3 mv --recursive
s3://bucket-name/folder s3://bucket-name1` will move everything inside of
"bucket-name/folder" to the bucket "bucket-name1".

```
# List all your buckets
aws s3 ls [--recursive] [s3://bucket-name/path]

# remove a file
aws s3 rm s3://bucket-name/path/to/file

# remove everything in a bucket with the prefix "prefix"
aws s3 rm --recursive s3://bucket-name/[prefix]

# Recursively copy files in the current directory into the folder called
# "foldername" in the bucket.
aws s3 sync . s3://bucket-name/foldername
aws s3 cp --recursive . s3://lucas-eu-testing-attachments

# Recursively copy files with the prefix "folder" from one bucket to another.
# The destination files will not have the prefix "folder" but instead will have
# the prefix "otherfolder"
aws s3 cp --recursive s3://lucas-eu-testing-attachments/folder s3://lucas-eu-testing-customer-attachments/otherfolder

# The same as above but move them instead.
aws s3 mv --recursive s3://lucas-eu-testing-attachments/folder s3://lucas-eu-testing-customer-attachments/otherfolder
```


Size of an s3 bucket: https://serverfault.com/questions/84815/how-can-i-get-the-size-of-an-amazon-s3-bucket

aws cloudwatch get-metric-statistics --namespace AWS/S3 --start-time 2020-04-21T00:00:00 --end-time 2020-04-22T00:00:00 --period 86400 --statistics Average --region us-east-1 --metric-name BucketSizeBytes --dimensions Name=BucketName,Value=grinfrastructure Name=StorageType,Value=StandardStorage
