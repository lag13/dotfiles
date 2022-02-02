SSH
===

Agent Forwarding
----------------

http://www.unixwiz.net/techtips/ssh-agent-forwarding.html


Ignore hosts and not get prompted to add the host to known hosts:

-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null

## Sample config

```
Host cool-gateway
    User lgroenendaal@cool.local
    Hostname gateway.cool.local
    IdentityFile ~/.ssh/id_ed25519

Host cool-bastion
    User lgroenendaal@cool.local
    Hostname bastion.cool.local
    IdentityFile ~/.ssh/id_ed25519
    ProxyJump cool-gateway
```

## No password

Put your public key into the .ssh/authorized_keys files on the server:

```
cat ~/.ssh/id_rsa.pub | ssh cool-gateway "mkdir -p ~/.ssh && cat >> ~/.ssh/authorized_keys"
```
