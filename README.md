# Fortran.io

An MVC web stack written in Fortran 90 (so you get arrays, and it's not punchcards)

Major credit due to:

- authors of <a href="http://fortranwiki.org/fortran/show/FLIBS">FLIBS</a> (Arjen Markus and Michael Baudin)
- Ricolindo Carino and Arjen Markus's Fortran FastCGI program and tutorial - in many ways this started as an update to this tutorial :  http://flibs.sourceforge.net/fortran-fastcgi-nginx.html
- String utils by George Benthian http://www.gbenthien.net/strings/index.html
- <a href="https://github.com/branning">Philip Branning</a> for improving and documenting the install process, especially the SQLite parts
- <a href="https://github.com/divVerent">divVerent</a> for fixing a server crash bug due to URL params and SQL

## Create an Ubuntu server

Log in and install dependencies

```
# update Ubuntu
sudo apt-get update
sudo apt-get upgrade

# create the user and home directory
adduser fortran --gecos ""
usermod -a -G sudo fortran

# switch to new user
su fortran
cd ~

# install git and clone the repo
sudo apt-get install -y git
git clone https://github.com/mapmeld/fortran-machine.git

# Install dependencies
cd fortran-machine
sudo ./install_deps_ubu.sh
```

Go to your IP address - you should see the "Welcome to nginx!" page

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
# compile the test server
make
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
./restart.sh
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

In the template folder, you can write HTML templates similar to Jade or HAML.

If you want to have a loop or other structure, it's better to create a partial and run the loop in the Fortran controller.

```jade
.container
  .col-sm-6
    h3 Hello #{name}!
  .col-sm-6
    h3 Link
    a(href="http://example.com/profile/#{id}") A link
```

## SQLite Database

You can connect to a SQLite database. The example on <a href="https://fortran.io">Fortran.io</a>
lets you search through marsupials!

Here's how the getAllMarsupials subroutine loads data into arrays:

```fortran
subroutine getAllMarsupials(name, latinName, wikiLink, description)
	! columns
	character(len=50), dimension(8)	:: name, latinName, wikiLink, description

	call sqlite3_open('marsupials.sqlite3', db)

	allocate( column(4) )
	call sqlite3_column_query( column(1), 'name', SQLITE_CHAR )
	call sqlite3_column_query( column(2), 'latinName', SQLITE_CHAR )
	call sqlite3_column_query( column(3), 'wikiLink', SQLITE_CHAR )
	call sqlite3_column_query( column(4), 'description', SQLITE_CHAR )

	call sqlite3_prepare_select( db, 'marsupials', column, stmt, "WHERE 1=1 LIMIT 8")

	i = 1
	do
		call sqlite3_next_row(stmt, column, finished)
		if (finished) exit

		call sqlite3_get_column(column(1), name(i))
		call sqlite3_get_column(column(2), latinName(i))
		call sqlite3_get_column(column(3), wikiLink(i))
		call sqlite3_get_column(column(4), description(i))
		i = i + 1
	end do
endsubroutine
```

Then in the Fortran controller, you loop through:

```fortran
call getAllMarsupials(names, latinNames, wikiLinks, descriptions)

i = 1
do
	pagevars(1,2) = names(i)
	pagevars(2,2) = latinNames(i)
	pagevars(3,2) = wikiLinks(i)
	pagevars(4,2) = descriptions(i)
	if (len(trim(pagevars(1,2))) == 0 .or. i == 5) then
		exit
	else
		! template with string
		templatefile = 'template/result.jade'
		call jadetemplate(templatefile, unitNo, pagevars)
		i = i + 1
	endif
enddo
```

Then the individual result template:

```jade
.row
  .col-sm-12
    h4
      a(href="https://en.wikipedia.org#{wikiLink}") #{name}
    em #{latinName}
    hr
    p #{description}
```

## HTTPS Certificate

Don't forget to get a free HTTPS Certificate using LetsEncrypt!

https://www.digitalocean.com/community/tutorials/how-to-secure-nginx-with-let-s-encrypt-on-ubuntu-14-04

# License

This library, like FLIBS which it's based on, is available under the BSD license
