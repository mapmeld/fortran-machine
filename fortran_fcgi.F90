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
    subroutine removesp(str)
      ! Removes spaces, tabs, and control characters in string str
      character(len=*):: str
      character(len=1):: ch
      character(len=len_trim(str))::outstr

      str=adjustl(str)
      lenstr=len_trim(str)
      outstr=' '
      k=0

      do i=1,lenstr
        ch=str(i:i)
        ich=iachar(ch)
        select case(ich)
          case(0:32)  ! space, tab, or control character
               cycle
          case(33:)
            k=k+1
            outstr(k:k)=ch
        end select
      end do

      str=adjustl(outstr)

    end subroutine removesp

    subroutine respond ( dict, unitNo, stopped )

        type(DICT_STRUCT), pointer        :: dict
        integer, intent(in)               :: unitNo
        logical, intent(out)              :: stopped

        ! the following are defined in fcgi_protocol
        !character(len=3), parameter :: AFORMAT = '(a)'
        !character(len=2), parameter :: CRLF = achar(13)//achar(10)
        !character(len=1), parameter :: NUL = achar(0)

        ! the script name
        character(len=80)  :: scriptName, spaceless, tag
        character(len=200) :: inputLine, outputLine, innerContent

        integer                           :: templater, io, spaceCount
        logical                           :: okInputs

        ! start of response
        ! lines starting with %REMARK% are for debugging & will not be copied to webserver
        write(unitNo, AFORMAT) &
            '%REMARK% respond() started ...', &
            '<html>', &
            '<head>', &
            '<meta charset="utf-8"/>', &
            '<title>Fortran FastCGI</title>', &
            '<link rel="stylesheet" type="text/css" href="/static/bootstrap.min.css"/>', &
            '</head>', &
            '<body>'

        ! retrieve script name (key=DOCUMENT_URI) from dictionary
        call cgi_get( dict, "DOCUMENT_URI", scriptName )

        select case (trim(scriptName))
          case ('/')
            open(newunit=templater, file="template/index.jade")
            do
              read(templater, '(A)', IOSTAT=io) inputLine
              if (io < 0) exit

              spaceless = trim(inputLine)
              call removesp(spaceless)
              spaceless = trim(spaceless) // '   '
              spaceCount = index(inputLine, trim(spaceless))
              innerContent = spaceless(index(spaceless, ' '): 150)

              if (spaceless(1:1) == '.') then
                outputLine = '<div class="' // &
                  spaceless(2: index(spaceless, ' ') - 1) // &
                  '">' // &
                  trim(innerContent) // &
                  '</div>'
              else
                if (spaceless(1:1) == '#') then
                  outputLine = '<div id="' // &
                    spaceless(2: index(spaceless, ' ') - 1) // &
                    '">' // &
                    trim(innerContent) // &
                    '</div>'
                else
                  tag = spaceless(1: index(spaceless, ' ') - 1)
                  outputLine = '<' // &
                    trim(tag) // &
                    '>' // &
                    trim(innerContent) // &
                    '</' // &
                    trim(tag) // &
                    '>'
                endif
              endif

              write(unitNo, AFORMAT) outputLine
            end do
            close(templater)
          case DEFAULT
            write(unitNo,AFORMAT) 'Page not found!'
        end select

        ! end of response
        write(unitNo,AFORMAT) '</body>', &
            '</html>', &
            '%REMARK% respond() completed ...'

        return

    end subroutine respond

end program test_fcgi
