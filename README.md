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

sudo apt-get install nginx gfortran spawn-fcgi git libfcgi-dev sqlite3
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
gfortran -c flibs-0.9/flibs/src/sqlite/fsqlite.f90
gfortran -c string_helpers.f90
gfortran -c jade.f90
gfortran -c marsupial.f90

# find your sqlite
find / -name 'libsqlite3.a'
> /usr/lib/x86_64-linux-gnu/libsqlite3.a
# copy sqlite to local directory
cp /usr/lib/x86_64-linux-gnu/libsqlite3.a ./

# compile the test server
gfortran -o fortran_fcgi fortran_fcgi.F90 marsupial.f90 jade.o string_helpers.o fsqlite.o csqlite.o libsqlite3.a cgi_protocol.o fcgi_protocol.o -ldl -lfcgi -pthread -Wl,--rpath -Wl,/usr/local/lib
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
gfortran -o fortran_fcgi fortran_fcgi.F90 marsupial.f90 jade.o string_helpers.o fsqlite.o csqlite.o libsqlite3.a cgi_protocol.o fcgi_protocol.o -ldl -lfcgi -pthread -Wl,--rpath -Wl,/usr/local/lib

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

## Fortran controller

The controller is written in Fortran in the fortran_fcgi.f90 file:

```fortran
case ('/')
	! most pages look like this
	templatefile = 'template/index.jade'
	call jadefile(templatefile, unitNo)

case ('/search')
	write(unitNo,AFORMAT) '<div class="container">'

	templatefile = 'template/search.jade'
	call jadefile(templatefile, unitNo)

	write(unitNo,AFORMAT) '</div>'
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

You should have created the SQLite database already.

```fortran
subroutine getOneMarsupial(query, name, latinName, wikiLink, description)
	! parameters
	character(len=*)		        :: query

	! columns
	character(len=50)			:: name, latinName, wikiLink, description

  ! connect to the database
	call sqlite3_open('marsupials.sqlite3', db)

  ! prepare to receive column data types
	allocate( column(4) )
	call sqlite3_column_query( column(1), 'name', SQLITE_CHAR )
	call sqlite3_column_query( column(2), 'latinName', SQLITE_CHAR )
	call sqlite3_column_query( column(3), 'wikiLink', SQLITE_CHAR )
	call sqlite3_column_query( column(4), 'description', SQLITE_CHAR )

  ! make the query
	call sqlite3_prepare_select( db, 'marsupials', column, stmt, "WHERE name = '" // trim(query) // "' LIMIT 4")

  ! iterate through any results
	i = 1
	do
		call sqlite3_next_row(stmt, column, finished)
		if (finished) exit

		call sqlite3_get_column(column(1), name)
		call sqlite3_get_column(column(2), latinName)
		call sqlite3_get_column(column(3), wikiLink)
		call sqlite3_get_column(column(4), description)
		i = i + 1
	end do
endsubroutine
```

## HTTPS Certificate

Don't forget to get a free HTTPS Certificate using LetsEncrypt!

https://www.digitalocean.com/community/tutorials/how-to-secure-nginx-with-let-s-encrypt-on-ubuntu-14-04

# License

This library, like FLIBS which it's based on, is available under the BSD license
