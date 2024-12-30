module parser 
    ! initial code from FortranPEG grammar
    type :: node	
        integer :: value
        type(node), pointer :: next => null()
    end type node

    type(node), pointer :: head => null()
    character(len=:), allocatable :: input

contains

subroutine push(value)
    integer, intent(in) :: value
    type(node), pointer :: tmp
    if (associated(head)) then
        allocate(tmp)
        tmp%value = value
        tmp%next => head
        head => tmp
    else
        allocate(head)
        head%value = value
        head%next => null()
    end if	
end subroutine push

subroutine show()
    type(node), pointer :: tmp
    tmp => head
    do while (associated(tmp))
        print *, tmp%value
        tmp => tmp%next
    end do
end subroutine show

function parse(inputstr) result(res)
    character(len=:), intent(in), allocatable :: inputstr
    type(node), pointer :: res
    input = inputstr
    res => parses()
end function parse

function parses() result(res)
    type(node), pointer :: res
    integer :: val
    
    ! Simplificado - ahora procesa directamente los números
    val = parsee()
    do while (val /= -999)
        call push(val)
        val = parsee()
    end do
    
    res => head
end function parses

function parsee() result(res)
    integer :: res
    integer :: num_val
    character(len=:), allocatable :: sep
    
    res = -999
    num_val = parsenum()
    
    if (num_val /= -999) then
        sep = parsesep()
        if (sep /= "") then
            res = num_val
        end if
    end if
end function parsee
 
function parsenum() result(res)
    integer :: res
    integer :: cursor
    character(len=:), allocatable :: num_str
    
    res = -999
    cursor = 1
    num_str = ""
     
    if (cursor > len(input)) return
    
    do while (cursor <= len_trim(input) .and. &
             (iachar(input(cursor:cursor)) >= iachar("0") .and. &
              iachar(input(cursor:cursor)) <= iachar("9")))
        cursor = cursor + 1
    end do
    
    if (cursor > 1) then
        num_str = input(1:cursor-1)
        read(num_str, *) res
        input = input(cursor:)
    end if
end function parsenum

function parsesep() result(res)
    character(len=:), allocatable :: res
    integer :: cursor
    
    res = ""
    cursor = 1
    
    if (cursor > len(input)) return
    
    if (cursor <= len_trim(input) .and. (char(10) == input(cursor:cursor))) then
        cursor = cursor + 1
        res = input(1:cursor-1)
        input = input(cursor:)
    end if
end function parsesep

end module parser