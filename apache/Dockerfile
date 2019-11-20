# php apache docker image
# includes ldap modules; could later modify to use httpd
FROM php:7-apache

# Enable Apache ldap auth and proxy modules
RUN a2enmod authnz_ldap
RUN a2enmod proxy
RUN a2enmod proxy_http
RUN a2enmod proxy_wstunnel
RUN a2enmod rewrite
RUN a2enmod headers

# Add ldap rules to apache's conf-enabled dir
COPY ldap-gsi.conf /etc/apache2/conf-enabled/
COPY .echo/echo_password.sh /etc/apache2/

# Add debugging help if you need it
RUN echo "LogLevel debug" >> /etc/apache2/apache2.conf

