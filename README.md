# qc-server-demo

Demonstration version of a QC web server for GSI.

## Overview

- Server runs in docker-compose
- Apache front-end with user authenticatation via OICR LDAP server
- Rshiny back-end with demonstration of QC pages
- "Unshiny" back-end is a simple Python webapp, useful as a proof-of-concept for Dash.


## How to run

```
docker-compose build
docker-compose up
```

Navigate to [http://0.0.0.0/shiny](http://0.0.0.0/shiny) or 
[http://0.0.0.0/shiny](http://0.0.0.0/shiny) and login with your LDAP
username and password.


## Notes

Apache authenticates to LDAP using a GSI service account. Credentials for this
account are in the `echo_password.sh` script referenced in `ldap-gsi.conf`,
which is _not_ to be pushed to Github.

