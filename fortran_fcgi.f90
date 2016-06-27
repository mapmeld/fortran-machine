!
! Fortran FastCGI stack
! Based on Fortran FastCGI by Ricolindo.Carino@gmail.com and arjen.markus895@gmail.com
!
! Requires:
!    - the FLIBS modules cgi_protocol and fcgi_protocol
!    - the FastCGI library
! See 'readme' for setup instructions
!

program test_fcgi

    use fcgi_protocol
    use jade
    use marsupial

    implicit none

    type(DICT_STRUCT), pointer  :: dict => null() ! Initialisation is important!
    logical                     :: stopped = .false. ! set to true in respond() to terminate program
    integer                     :: unitNo ! unit number  for a scratch file

    ! open scratch file
    open(newunit=unitNo, status='scratch')
    ! comment previous line AND uncomment next line for debugging;
    !open(newunit=unitNo, file='fcgiout', status='unknown') ! file 'fcgiout' will show %REMARKS%

    ! wait for environment variables from webserver
    do while (fcgip_accept_environment_variables() >= 0)

        ! build dictionary from GET or POST data, environment variables
        call fcgip_make_dictionary( dict, unitNo )

        ! give dictionary to the user supplied routine
        ! routine writes the response to unitNo
        ! routine sets stopped to true to terminate program
        call respond(dict, unitNo, stopped)

        ! copy file unitNo to the webserver
        call fcgip_put_file( unitNo, 'text/html' )

        ! terminate?
        if (stopped) exit

    end do !  while (fcgip_accept_environment_variables() >= 0)

    ! before termination, it is good practice to close files that are open
    close(unitNo)

    ! webserver will return an error since this process will now terminate
    unitNo = fcgip_accept_environment_variables()


contains
    subroutine respond ( dict, unitNo, stopped )

        type(DICT_STRUCT), pointer        :: dict
        integer, intent(in)               :: unitNo
        logical, intent(out)              :: stopped

        ! the following are defined in fcgi_protocol
        !character(len=3), parameter :: AFORMAT = '(a)'
        !character(len=2), parameter :: CRLF = achar(13)//achar(10)
        !character(len=1), parameter :: NUL = achar(0)

        ! retrieve params from model and pass them to view
        character(len=50), dimension(10,2) :: pagevars
        character(len=50), dimension(8) :: names, latinNames, wikiLinks, descriptions

        ! the script name
        character(len=80)  :: scriptName, query
        character(len=12000) :: templatefile

        logical                           :: okInputs

        ! start of response
        ! lines starting with %REMARK% are for debugging & will not be copied to webserver
        write(unitNo, AFORMAT) &
            '%REMARK% respond() started ...', &
            	'<!DOCTYPE html>', &
		'<html>', &
            '<head>', &
            '<meta charset="utf-8"/>', &
            '<meta name="viewport" content="width=device-width, initial-scale=1"/>', &
            '<title>FORTRAN.io</title>', &
            '<link rel="stylesheet" type="text/css" href="/static/bootstrap.min.css"/>', &
            '</head>', &
            '<body>'

        ! retrieve script name (key=DOCUMENT_URI) from dictionary
        call cgi_get( dict, "DOCUMENT_URI", scriptName )

        select case (trim(scriptName))
          case ('/')
            ! most pages look like this
            templatefile = 'template/index.jade'
            call jadefile(templatefile, unitNo)
          case ('/test')
            templatefile = 'template/test.jade'
            call jadefile(templatefile, unitNo)
          case ('/search')
            ! tags which contain multiple templates must be written around them
            ! in the fortran controller
            write(unitNo,AFORMAT) '<div class="container">'

            ! header
            templatefile = 'template/search.jade'
            call jadefile(templatefile, unitNo)

            pagevars(1,1) = 'name'
            pagevars(2,1) = 'latinName'
            pagevars(3,1) = 'wikiLink'
            pagevars(4,1) = 'description'
            query = ''
            call cgi_get( dict, 'q', query)
            call getOneMarsupial(query, pagevars(1,2), pagevars(2,2), pagevars(3,2), pagevars(4,2))

            if (len(trim(pagevars(1,2))) == 0) then
              write(unitNo,AFORMAT) '<p>No results in this database :-(</p>'
            else
              ! template with string
              templatefile = 'template/result.jade'
              call jadetemplate(templatefile, unitNo, pagevars)
            endif

            ! close .container
            write(unitNo,AFORMAT) '</div>'
          case ('/all')
            write(unitNo,AFORMAT) '<div class="container">'
            templatefile = 'template/search.jade'
            call jadefile(templatefile, unitNo)

            pagevars(1,1) = 'name'
            pagevars(2,1) = 'latinName'
            pagevars(3,1) = 'wikiLink'
            pagevars(4,1) = 'description'

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
            write(unitNo,AFORMAT) '</div>'
          case DEFAULT
            ! your 404 page
            write(unitNo,AFORMAT) 'Page not found!'
        end select

        ! end of response
        write(unitNo,AFORMAT) '</body>', &
            '</html>', &
            '%REMARK% respond() completed ...'

        return

    end subroutine respond

end program test_fcgi
