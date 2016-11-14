Glide
=====

Package management for Go: https://glide.sh/

For private repositories you'll have to edit the `glide.yml` file. You can see
an example with the `go-hal-client` repository
(https://github.com/cbdr/go-hal-client):

```yaml
package: github.com/cbdr/tsr-api
import:
- package: github.com/go-sql-driver/mysql
- package: github.com/go-xorm/builder
- package: github.com/go-xorm/xorm
- package: github.com/newrelic/go-agent
- package: github.com/cbdr/go-hal-client
  repo: git@github.com:/cbdr/go-hal-client
  vcs: git
testImport:
- package: github.com/cbdr/tsr-api-client-go
  repo: git@github.com:/cbdr/tsr-api-client-go
  vcs: git
- package: gopkg.in/check.v1
```

So basically I needed to add these lines to the glide.yaml file before I was
able to get the repository:

```
- package: github.com/cbdr/go-hal-client
  repo: git@github.com:/cbdr/go-hal-client
  vcs: git
```
