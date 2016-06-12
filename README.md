# Fortran-Machine

A web stack written in Fortran

Credit to:

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

In your user home:

```
# clone the repo
git clone https://github.com/mapmeld/fortran-machine.git
```

Change the location in /etc/nginx/sites-available/default :

```
server_name 101.101.101.101; <- your IP address

location / {
	root /root/fortran-machine;
	index index.html;
}
```

Change permission on your home directory

```
chmod 701 /root
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

# compile the cgi_protocol and fcgi_protocol modules
gfortran -c flibs-0.9/flibs/src/cgi/cgi_protocol.f90
gfortran -c flibs-0.9/flibs/src/cgi/fcgi_protocol.f90

# compile the test server
gfortran -o fortran_fcgi fortran_fcgi.F90 cgi_protocol.o fcgi_protocol.o -lfcgi -Wl,--rpath -Wl,/usr/local/lib
```

Now change nginx config /etc/nginx/sites-available/default

```
location / {
	root /root/fortran-machine;
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
				root /root/fortran-machine;
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

#### Todo

Read in whitespace levels, test id and class together, test multiple ids and classes, templating

## SQLite Database

You can connect to a SQLite database... follow the example in flibs-0.9/flibs/src

That's yet to be figured out.


# License

This library, like FLIBS which it's based on, is available under the BSD license

