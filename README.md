# qc-server-demo

Demonstration version of a QC web server for GSI.

## Overview

- Server runs in docker-compose
- Apache front-end with SSL enabled, and user authenticatation via OICR LDAP server
- Rshiny back-end with demonstration of QC pages
- "Unshiny" back-end is a simple Python webapp, useful as a proof-of-concept for Dash.


## SSL configuration

For basic authentication, create a self-signed certificate in the `auth/` directory:

```
openssl req -new -newkey rsa:4096 -days 3650 -nodes -x509 -subj \
    "/C=ca/ST=ontario/L=toronto/O=oicr/CN=qc-dev.oicr.on.ca" \
    -keyout auth/ssl.key -out auth/ssl.crt
```

The `auth` directory is bound as a volume to the docker-compose instance. So the Apache
server can read the key and certificate files without any need to copy them into the Docker
container.


## How to run

```
docker-compose build
docker-compose up
```

Navigate to [http://0.0.0.0/shiny](https://0.0.0.0/shiny) or 
[http://0.0.0.0/unshiny](https://0.0.0.0/unshiny) and login with your LDAP
username and password.

It may be necessary to click through a browser warning if the security certificate cannot be
validated (as with the self-signed example above).


## Notes

Apache authenticates to LDAP using a GSI service account. Credentials for this
account are in the `echo_password.sh` script referenced in `ldap-gsi.conf`,
which is _not_ to be pushed to Github.

