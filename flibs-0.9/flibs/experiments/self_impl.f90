! self_impl --
!     Implementation of the body of the value_method function
!
! Declarations
!
    type(self_object) :: dummy
    integer           :: i
    real              :: x, y, dx, dy

!     TODO

    r%type = self_type_empty  ! This is the result!

    select case( impl )
        case( 'print' )
            do i = 1,size(obj%obj)
                select case( obj%obj(i)%type )
                    case ( self_type_integer )
                        write(*,*) '    ', trim(obj%obj(i)%name), ' = ', obj%obj(i)%ivalue
                    case ( self_type_real )
                        write(*,*) '    ', trim(obj%obj(i)%name), ' = ', obj%obj(i)%rvalue
                    case ( self_type_object )
                        write(*,*) '    ', trim(obj%obj(i)%name), ' = subobject'
                        dummy = value( obj%obj(i), 'print' )
                    case default
                        write(*,*) '    ', trim(obj%obj(i)%name), ' -> type ', obj%obj(i)%type
                end select
             enddo

        case( 'move' )
            dx = args(1)
            dy = args(2)
            x  = value(obj,'x')
            y  = value(obj,'y')
            x  = x + dx
            y  = y + dy
            call set_value( obj, 'x', x )
            call set_value( obj, 'y', y )

        case default
            write(*,*) 'Method: ', trim(impl), ' - object: ', trim(obj%name), '- not implemented'
    end select

