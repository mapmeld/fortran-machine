! self.f90 --
!     Experiment to implement a subset of the prototype-based object
!     oriented programming paradigm, as exemplified by the SELF language,
!     in Fortran
!
!     The idea is that at the start of a program there is only one
!     object with a very limited set of properties and methods. From
!     this object others can be cloned and all objects can be given
!     methods and properties without reference to any class. If you
!     clone an object, the new object has the same properties and
!     methods as the original one.
!
module self
    implicit none

    type self_method
        character(len=40)    :: name           ! Name of the method
        character(len=40)    :: implementation ! Implementation of the method
        type(self_object), dimension(:), pointer :: extra_args    ! Extra arguments
    end type self_method

    type self_object
        integer                    :: type      ! Type of value
        integer                    :: ivalue    ! Integer value
        real                       :: rvalue    ! Real value
        character(len=80)          :: svalue    ! String value
        character(len=40)          :: name      ! Name of the object
        type(self_method)          :: method    ! Information on methods
        type(self_object), pointer :: objvalue  ! Cloned object
        type(self_object), pointer :: reference ! Referenced object

        type(self_object), dimension(:), pointer :: obj    ! Subobjects/properties/methods
    end type self_object

    type(self_object) :: BASIC_OBJECT
    logical, private  :: initialised = .false.

    integer, parameter :: self_type_error     = -1
    integer, parameter :: self_type_empty     =  0
    integer, parameter :: self_type_object    =  1
    integer, parameter :: self_type_integer   =  2
    integer, parameter :: self_type_logical   =  3
    integer, parameter :: self_type_real      =  4
    integer, parameter :: self_type_double    =  5
    integer, parameter :: self_type_string    =  6
    integer, parameter :: self_type_reference =  7
    integer, parameter :: self_type_method    =  8

    interface assignment(=)
        module procedure assign_value_to_object_int
        module procedure assign_value_to_object_real
        module procedure assign_obj_value_to_int
        module procedure assign_obj_value_to_real
    end interface

    interface set_value
        module procedure set_value_int
        module procedure set_value_real
        module procedure set_value_obj
        module procedure set_value_method
    end interface

    interface object
        module procedure real_obj
        module procedure int_obj
    end interface

contains

! self_initialise --
!     Create the initial object
!
subroutine self_initialise
    initialised   = .true.
    basic_object%type   =  self_type_object
    basic_object%name   = 'OBJECT'
    allocate( basic_object%obj(0) )
end subroutine self_initialise

! clone --
!     Clone the object
! Arguments:
!     obj            Object to be cloned
!     name           Name for the new object (optional)
!
recursive function clone( obj, name ) result(new)
    type(self_object)          :: obj
    character(len=*), optional :: name

    type(self_object) :: new
    integer           :: i
    integer           :: j

    new = obj
    allocate( new%obj(0) )
    if ( present(name) ) then
        new%name = name
    endif

    do i = 1,size(obj%obj)
        call set_value( new, obj%obj(i)%name, obj%obj(i) )
    enddo
end function clone

! assign_value_to_object --
!     Assign a basic value to the object
! Arguments:
!     obj            Object whose value will be set
!     value          The value to be added (note: overloaded!)
!
subroutine assign_value_to_object_int( obj, value )
    type(self_object), intent(inout) :: obj
    integer, intent(in)              :: value

    obj%type   = self_type_integer
    obj%ivalue = value
end subroutine assign_value_to_object_int

subroutine assign_value_to_object_real( obj, value )
    type(self_object), intent(inout) :: obj
    real, intent(in)                 :: value

    obj%type   = self_type_real
    obj%rvalue = value
end subroutine assign_value_to_object_real

! assign_obj_value --
!     Assign the basic value in an object to a basic variable
! Arguments:
!     var            The variable to be filled
!     obj            Object to be examined
! Note:
!     The implementation could be made more tolerant!
!
subroutine assign_obj_value_to_int( var, obj )
    integer, intent(out)          :: var
    type(self_object), intent(in) :: obj

    var = 0
    if ( obj%type == self_type_integer ) then
        var = obj%ivalue
    endif
end subroutine assign_obj_value_to_int

subroutine assign_obj_value_to_real( var, obj )
    real, intent(out)             :: var
    type(self_object), intent(in) :: obj

    var = 0.0
    if ( obj%type == self_type_integer ) then
        var = obj%ivalue
    endif
    if ( obj%type == self_type_real ) then
        var = obj%rvalue
    endif
end subroutine assign_obj_value_to_real

! index_value --
!     Find the index of a value
! Arguments:
!     obj            Object to be cloned
!     name           Name of the value
!     add            Add the value if necessary or not
!
integer function index_value( obj, name, add )
    type(self_object)            :: obj
    character(len=*), intent(in) :: name
    logical                      :: add

    integer                      :: i
    logical                      :: found
    type(self_object), dimension(:), pointer :: values

    index_value = -1
    do i = 1,size(obj%obj)
        if ( obj%obj(i)%name == name ) then
            index_value = i
            return
        endif
    enddo

    !
    ! Create a new entry, if necessary
    !
    if ( add ) then
        allocate( values(1:size(obj%obj)+1) )
        values(1:size(obj%obj)) = obj%obj
        index_value = size(values)
        deallocate( obj%obj ) ! Just deallocate the array - all objects
                              ! in it remain intact
        obj%obj => values
    endif
end function index_value

! new_method --
!     Fill a self_method object (to create a method)
! Arguments:
!     name             Name of the method
!     implementation   Name of the implementation method
!     extra_args       Array of extra arguments (optional)
!
type(self_method) function new_method( name, implementation, extra_args )
    type(self_object)                         :: obj
    character(len=*), intent(in)              :: name
    character(len=*), intent(in)              :: implementation
    type(self_object), dimension(:), optional :: extra_args

    new_method%name           = name
    new_method%implementation = implementation
    allocate( new_method%extra_args(0) )
end function new_method

! set_value --
!     Set/add a new value (or method!) to the object
! Arguments:
!     obj            Object to be cloned
!     name           Name for the new object (optional)
!     value          The value to be added (note: overloaded!)
!
subroutine set_value_int( obj, name, value )
    type(self_object)            :: obj
    character(len=*), intent(in) :: name
    integer                      :: value

    type(self_object) :: vobj

    vobj = value
    call set_value( obj, name, vobj )
end subroutine set_value_int

subroutine set_value_real( obj, name, value )
    type(self_object)            :: obj
    character(len=*), intent(in) :: name
    real                         :: value

    type(self_object) :: vobj

    vobj = value
    call set_value( obj, name, vobj )
end subroutine set_value_real

subroutine set_value_method( obj, method, extra_args )
    type(self_object)                         :: obj
    type(self_method)                         :: method
    type(self_object), dimension(:), optional :: extra_args ! TODO!

    type(self_object) :: vobj

    vobj%type   = self_type_method
    vobj%method = method

    ! TODO: clone the arguments

    call set_value( obj, method%name, vobj )
end subroutine set_value_method

recursive subroutine set_value_obj( obj, name, value )
    type(self_object)            :: obj
    character(len=*), intent(in) :: name
    type(self_object)            :: value

    integer                      :: idx

    idx = index_value( obj, name, .true. )

    if ( value%type == self_type_object ) then
        !
        ! Clone the object to get all properties
        !
        obj%obj(idx) = clone( value )
    else if ( value%type == self_type_method ) then
        !
        ! For the moment: a simple copy will do
        !
        obj%obj(idx) = value
    else
        !
        ! No special action required - only the "raw" value
        !
        obj%obj(idx) = value
    endif

    obj%obj(idx)%name = name

end subroutine set_value_obj

! value --
!     Get the value (or the result of a method!) from the object
! Arguments:
!     obj            Object to be cloned
!     name           Name of the value/method
!     args           Array of arguments (optional)
!
type(self_object) function value( obj, name, args )
    type(self_object)                         :: obj
    character(len=*), intent(in)              :: name
    type(self_object), dimension(:), optional :: args

    integer                      :: idx

    idx = index_value( obj, name, .false. )

    if ( idx > 0 ) then
        if ( obj%obj(idx)%type /= self_type_method ) then
            value = obj%obj(idx)
        else
            if ( present(args) ) then
                value = value_method( obj, obj%obj(idx)%method%implementation, &
                            (/ obj%obj(idx)%method%extra_args, args /) )
            else
                value = value_method( obj, obj%obj(idx)%method%implementation, &
                            obj%obj(idx)%method%extra_args )
            endif
        endif
    else
        value%type   = self_type_error
        value%svalue = 'Object ('//trim(obj%name)//'): value/method unknown - '// trim(name)
    endif
end function value

! object --
!     Convert a value into an object
! Arguments:
!     value          Value for the object
!
type(self_object) function int_obj( value )
    integer                   :: value

    int_obj%type   = self_type_integer
    int_obj%ivalue = value
end function int_obj

type(self_object) function real_obj( value )
    real                      :: value

    real_obj%type   = self_type_real
    real_obj%rvalue = value
end function real_obj

! value_method --
!     Invoke the method and return the result
! Arguments:
!     obj            Object to be cloned
!     impl           Name of the implementation method
!     args           Array of arguments (optional)
!
recursive function value_method( obj, impl, args ) result(r)
    type(self_object)               :: obj
    character(len=*), intent(in)    :: impl
    type(self_object), dimension(:) :: args
    type(self_object)               :: r

    !
    ! This is the variable part of the module, so implement it via
    ! a separate file
    !

    include 'self_impl.f90'

end function value_method

end module self

! Test program:
!     Define an object with a general print method
!     Define a point with two properties and one new method (move)
!     Define a line consisting of two points and one new method (passon)
!
program test_self
    use self

    type(self_object) :: printable
    type(self_object) :: point
    type(self_object) :: point2
    type(self_object) :: line
    type(self_object) :: dummy

    !
    ! Make sure the SELF module is initialised
    !
    call self_initialise

    !
    ! Create a prototypical object that can print its contents
    !
    printable = clone( BASIC_OBJECT, 'Printable' )

    call set_value( printable, new_method('print', 'print') )

    !
    ! Create a point object
    !
    point = clone( printable, 'Point' )
    call set_value( point, 'x', 10.0 )
    call set_value( point, 'y', 20.0 )
    call set_value( point, new_method('move', 'move') )

    !
    ! Create a second point
    point2 = clone( point )
    call set_value( point2, 'x', 100.0 )

    !
    ! Create a line
    !
    line = clone( printable, 'Line' )
    call set_value( line, 'start', point )
    call set_value( line, 'end', point2 )
    call set_value( line, new_method('move', 'passon') )

    !
    ! Now start moving the points and the line around:
    ! First print the coordinates
    ! Then move the first point and print the new coordinates
    !
    write(*,*) 'Original point:'
    dummy = value( point, 'print' )
    write(*,*) 'Original line:'
    dummy = value( line, 'print' )
    dummy = value( point, 'move', (/ object(10.0), object(10.0) /) )
    write(*,*) 'Moving the point:'
    dummy = value( point, 'print' )
    write(*,*) 'Moving the line:'
    dummy = value( line, 'move', (/ object(10.0), object(10.0) /) )
end program
