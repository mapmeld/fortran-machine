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
    subroutine compact(str)
      ! Converts multiple spaces and tabs to single spaces; deletes control characters;
      ! removes initial spaces.

      character(len=*):: str
      character(len=1):: ch
      character(len=len_trim(str)):: outstr
      integer::isp,k,ich,lenstr,i
      str=adjustl(str)
      lenstr=len_trim(str)
      outstr=' '
      isp=0
      k=0
      do i=1,lenstr
        ch=str(i:i)
        ich=iachar(ch)
        select case(ich)
          case(9,32)     ! space or tab character
            if(isp==0) then
              k=k+1
              outstr(k:k)=' '
            end if
            isp=1
          case(33:)      ! not a space, quote, or control character
            k=k+1
            outstr(k:k)=ch
            isp=0
        end select
      end do
      str=adjustl(outstr)
    end subroutine compact

    subroutine string_insert( string, pos, second )
        character(len=*), intent(inout) :: string
        integer, intent(in)             :: pos
        character(len=*), intent(in)    :: second

        integer                         :: length

        length = len( second )
        string(pos+length:)      = string(pos:)
        string(pos:pos+length-1) = second

    end subroutine string_insert

    subroutine string_delete( string, pos, length )
        character(len=*), intent(inout) :: string
        integer, intent(in)             :: pos
        integer, intent(in)             :: length

        string(pos:)             = string(pos+length:)

    end subroutine string_delete

    subroutine string_replace( string, substr, replace )
        character(len=*), intent(inout) :: string
        character(len=*), intent(in)    :: substr
        character(len=*), intent(in)    :: replace

        integer                         :: k

        k = index( string, substr )
        if ( k > 0 ) then
            call string_delete( string, k, len(substr) )
            call string_insert( string, k, replace )
        endif

    end subroutine string_replace

    subroutine respond ( dict, unitNo, stopped )

        type(DICT_STRUCT), pointer        :: dict
        integer, intent(in)               :: unitNo
        logical, intent(out)              :: stopped

        ! the following are defined in fcgi_protocol
        !character(len=3), parameter :: AFORMAT = '(a)'
        !character(len=2), parameter :: CRLF = achar(13)//achar(10)
        !character(len=1), parameter :: NUL = achar(0)

        ! the script name
        character(len=80)  :: scriptName, spaceless, tag, closeTag, className, elemID
        character(len=200) :: inputLine, outputLine, innerContent

        integer                           :: templater, io, spaceCount
        logical                           :: okInputs

        ! start of response
        ! lines starting with %REMARK% are for debugging & will not be copied to webserver
        write(unitNo, AFORMAT) &
            '%REMARK% respond() started ...', &
            	'<!DOCTYPE html>', &
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
              call compact(spaceless)
              spaceless = trim(spaceless) // '   '
              spaceCount = index(inputLine, trim(spaceless))
              className = ''
              elemID = ''
              innerContent = spaceless(index(spaceless, ' ') + 1:)

              if (spaceless(1:1) == '.') then
                ! starts with a class definition
                tag = 'div'
                className = spaceless(2: index(spaceless, ' ') - 1)
                if (index(className, '#') > 0) then
                  className = className(1 : index(className, '#') - 1)
                  elemID = spaceless(index(spaceless, '#') : index(spaceless, ' '))
                endif
              else
                ! starts with an ID definition
                if (spaceless(1:1) == '#') then
                  tag = 'div'
                  elemID = spaceless(2: index(spaceless, ' ') - 1)
                  if (index(elemID, '.') > 0) then
                    elemID = elemID(1 : index(elemID, '.') - 1)
                    className = spaceless(index(spaceless, '.') : index(spaceless, ' '))
                  endif
                else
                  if (spaceless(1:1) == '(') then
			! starts with a div attributes
                    tag = 'div' // spaceless(1: index(spaceless, ' ') - 1)
                  else
			! custom tag
                    tag = spaceless(1: index(spaceless, ' ') - 1)
                    if (index(tag, '.') > 0 .and. index(tag, '.') < index(tag, '(')) then
                      tag = tag(1: index(tag, '.') - 1)
                      className = spaceless(index(spaceless, '.') : index(spaceless, ' '))
                    endif
                    if (index(tag, '#') > 0 .and. index(tag, '#') < index(tag, '(')) then
                      tag = tag(1: index(tag, '#') - 1)
                      elemID = elemID(index(spaceless, '#') : index(spaceless, ' '))
                    endif
                  endif
                endif
              endif

              ! handle multiple classes
              call string_replace(className, '.', ' ')
              ! just make sure I don't have a # in the ID
              call string_replace(elemID, '#', '')

              if (index(tag, '(') > 0) then
                ! todo: substitute values, handle multiple attributes
                call string_replace(tag, '(', ' ')
                call string_replace(tag, ')', ' ')
              endif

              outputLine = '<' // &
                trim(tag) // &
                ' id="' // trim(elemID) // '"' // &
                ' class="' // trim(className) // '"' // &
                '>' // &
                trim(innerContent)

              if (index(tag, 'img') == 1 .or. index(tag, 'link') == 1) then
                ! single closing tag
                outputLine = trim(outputLine) // &
                  '/>'
              else
                ! add close tag
                closeTag = tag
                if (index(closeTag, '(') > 0) then
                  closeTag = closeTag(1 : index(closeTag, '(') - 1)
                endif
                outputLine = trim(outputLine) // &
                  '</' // &
                  trim(closeTag) // &
                  '>'
              endif

              write(unitNo, AFORMAT) outputLine
            end do
            close(templater)
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
