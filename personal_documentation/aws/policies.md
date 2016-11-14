Policies
========

Policies seems to be the name used to describe permissions. Each user can be
given permissions for what they can and can't do with AWS.

S3
--

I just had a bit of a tough time trying to figure out what permissions s3
needed to be happy.

### Copying From "src-bucket" to "dest-bucket":

```
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": [
                "s3:ListBucket",
                "s3:GetObject",
                "s3:DeleteObject"
            ],
            "Resource": [
                "arn:aws:s3:::src-bucket",
                "arn:aws:s3:::src-bucket/*"
            ]
        },
        {
            "Effect": "Allow",
            "Action": [
                "s3:PutObject"
            ],
            "Resource": [
                "arn:aws:s3:::dest-bucket/*"
            ]
        }
    ]
}
```

"move"ing some objects from a bucket "src" to a bucket "dest" basically
involves:

1. Listing the objects you want to move from "src". If you know which objects
   you need to move then you probably don't need to have this permission.
2. GET'ting each object from "src" that you need.
3. PUT'ting that object into the "dest" bucket
4. DELETE'ing the object from "src"

With that in mind, the above permission JSON breaks down like this:

- Action: `s3:ListBucket` + Resource: `arn:aws:s3:::src-bucket` == able to
  list the objects inside of `arn:aws:s3:::src-bucket`.
- Action: `s3:GetObject` + Resource: `src-bucket/*` == able to GET the
  contents of any object inside of `src-bucket`.
- Action: `s3:PutObject` + Resource: `arn:aws:s3:::dest-bucket/*` == able to
  create any object inside of `dest-bucket`.
- Action: `s3:DeleteObject` + Resource: `arn:aws:s3:::src-bucket/*` == able to
  delete any object from `src-bucket.
