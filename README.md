# qc-server-demo

Demonstration version of a QC web server for GSI.

## Overview

- Server runs in docker-compose
- Apache front-end with user authenticatation via OICR LDAP server
- Rshiny back-end with demonstration of QC pages


## How to run

```
docker-compose build
docker-compose up
```

Navigate to [http://0.0.0.0/shiny] and login with your LDAP username and password.


## Notes

Apache authenticates to LDAP using a GSI service account. Credentials for this
account are in the `echo_password.sh` scripy referenced in `ldap-gsi.conf`,
which is _not_ to be pushed to Github.

The `unshiny` app is a simple Python webapp, useful as a proof-of-concept for Dash.
