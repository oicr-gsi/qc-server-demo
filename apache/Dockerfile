# php apache docker image
# includes ldap modules; could later modify to use httpd
FROM php:7-apache

# Enable Apache ldap auth module
RUN a2enmod authnz_ldap

# Add ldap rules to apache's conf-enabled dir
COPY ldap-gsi.conf /etc/apache2/conf-enabled/
COPY .echo/echo_password.sh /etc/apache2/

# Add debugging help if you need it
RUN echo "LogLevel debug" >> /etc/apache2/apache2.conf


#########################
# copies an html file to the 'demo' directory as a test

# Convenient working directory
#WORKDIR /var/www/html/demo

# Copy the project files we need
#COPY index.html ./

