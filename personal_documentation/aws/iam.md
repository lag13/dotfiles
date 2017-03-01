IAM
===

I think:

- associate access keys with a particular user.
- you also associate permissions with a particular user.
- I think the key associated with the user just takes on whatever permissions
  that the user has? This feels weird to me because then if you have multiple
  keys per user they all have the same permission and in that case why
  wouldn't you just give the same key to everyone? I suppose it's okay though.
- Groups are a collection of polices
- Policies are a collection of AWS rules for who has access to what.
- You can associate groups or policies with a user (and you can also add rules
  directly to the user if you want).
