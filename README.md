All credit to:

- authors of FLIB (cgi protocol for Fortran)
- this tutorial: http://flibs.sourceforge.net/fortran-fastcgi-nginx.html


Create Ubuntu server / droplet.

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

# enter the directory
cd fortran-machine

# compile the cgi_protocol and fcgi_protocol modules
gfortran -c flibs-0.9/flibs/src/cgi/cgi_protocol.f90
gfortran -c flibs-0.9/flibs/src/cgi/fcgi_protocol.f90

# compile the test server
gfortran -o fortran_fcgi fortran_fcgi.F90 cgi_protocol.o fcgi_protocol.o -lfcgi -Wl,--rpath -Wl,/usr/local/lib
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
# spawn the test server
spawn-fcgi -a 127.0.0.1 -p 9000 ./fortran_fcgi

```
