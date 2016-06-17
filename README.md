# Fortran.io

A web stack written in Fortran

Major credit due to:

- authors of <a href="http://fortranwiki.org/fortran/show/FLIBS">FLIBS</a> (Arjen Markus and Michael Baudin)
- Ricolindo Carino and Arjen Markus's Fortran FastCGI program and tutorial - in many ways this is just an update to the commands in the tutorial :  http://flibs.sourceforge.net/fortran-fastcgi-nginx.html
- String utils by George Benthian http://www.gbenthien.net/strings/index.html


## Create an Ubuntu server

Log in and install dependencies

```
sudo apt-get update
sudo apt-get upgrade -y

sudo apt-get install nginx gfortran spawn-fcgi git libfcgi-dev
```

Go to your IP address - you should see the "Welcome to nginx!" page

```
# create the user and home directory
useradd fortran
passwd fortran
# set password
mkdir /home/fortran
cd /home/fortran

# clone the repo
git clone https://github.com/mapmeld/fortran-machine.git
```

Change the location in /etc/nginx/sites-available/default :

```
server_name 101.101.101.101; <- your IP address

location / {
	root /home/fortran/fortran-machine;
	index index.html;
}
```

Restart nginx to make these settings for real:

```
sudo service nginx restart
```

You should now see the test page on your IP address.

```
Test doc
```

## Use Fortran CGI script

Let's go from test page to Fortran script:

```
# enter the directory
cd fortran-machine

# compile the modules with your version of Fortran
gfortran -c flibs-0.9/flibs/src/cgi/cgi_protocol.f90
gfortran -c flibs-0.9/flibs/src/cgi/fcgi_protocol.f90
gfortran -c string_helpers.f90
gfortran -c jade.f90

# compile the test server
gfortran -o fortran_fcgi fortran_fcgi.F90 jade.o string_helpers.o cgi_protocol.o fcgi_protocol.o -lfcgi -Wl,--rpath -Wl,/usr/local/lib
```

Now change nginx config /etc/nginx/sites-available/default

```
location / {
	root /home/fortran/fortran-machine;
	fastcgi_pass 127.0.0.1:9000;
	fastcgi_index index.html;
	include fastcgi_params;
}
```

Then run ```sudo service nginx restart```

```
# spawn the server
spawn-fcgi -a 127.0.0.1 -p 9000 ./fortran_fcgi
```

### Restarting the script

After changing the source code, you can recompile and restart your server with:

```
ps aux | grep fcgi
# returns process ID of running server, if it's running

kill -9 _process_id_of_running_server

# recompile
gfortran -o fortran_fcgi fortran_fcgi.F90 cgi_protocol.o fcgi_protocol.o -lfcgi -Wl,--rpath -Wl,/usr/local/lib

# respawn fcgi
spawn-fcgi -a 127.0.0.1 -p 9000 ./fortran_fcgi
```

## Add a static folder

Add to nginx config /etc/nginx/sites-available/default

```
location /static {
    root /home/fortran/fortran-machine;
}
```

And restart nginx

```
sudo service nginx restart
```


## Jade Templates

In the template folder, you can put Jade templates (similar to HAML) but only in the simplest
syntax. Template strings are not yet available.

If you want to have a loop or other structure, it's better to create a Jade partial and run the loop in Fortran code.

```jade
.container
  .col-sm-6
    h3 Hello World
  .col-sm-6
    h3 Link
    a(href="http://example.com") A link
```

Don't make blank lines in the middle of divs.

#### Todo

Test id and class together, test multiple ids and classes, templating

## SQLite Database

You can connect to a SQLite database. The example on <a href="https://fortran.io">Fortran.io</a>
lets you search through marsupials!

## HTTPS Certificate

Don't forget to get a free HTTPS Certificate using LetsEncrypt!

https://www.digitalocean.com/community/tutorials/how-to-secure-nginx-with-let-s-encrypt-on-ubuntu-14-04

# License

This library, like FLIBS which it's based on, is available under the BSD license
