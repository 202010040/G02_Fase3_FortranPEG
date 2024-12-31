module parser
    implicit none
    ! Variables globales para el parsing
    character(len=:), allocatable, private :: input
    integer, private :: savePoint, lexemeStart, cursor

    ! Interfaz para conversión de tipos
    interface toStr
        module procedure intToStr
        module procedure strToStr
    end interface
    
    ! Definición del nodo
    type :: node
        integer :: value
        type(node), pointer :: next => null()
    end type node

    type(node), pointer :: head => null()

contains
    ! Funciones de lista enlazada
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

    ! Nueva función parse que retorna la pila
    function parse(str) result(res)
        character(len=:), allocatable :: str
        type(node), pointer :: res
        
        ! Aseguramos que head empiece nulo
        if (associated(head)) then
            nullify(head)
        end if
        
        input = str
        cursor = 1
        res => parses()
    end function parse

    ! Función parses modificada
    function parses() result(res)
        type(node), pointer :: res
        integer, allocatable :: s0(:)
        integer, allocatable :: s1(:)
        integer :: s2, len

        savePoint = cursor
        allocate(s0(0))
        s2 = parsee()
        
        do while (s2 /= -999)
            len = size(s0) + 1
            if (allocated(s1)) deallocate(s1)
            allocate(s1(len))
            s1(1:size(s0)) = s0
            s1(len) = s2
            if (allocated(s0)) deallocate(s0)
            allocate(s0(len))
            s0(1:len) = s1
            
            savePoint = cursor
            s2 = parsee()
        end do
        
        if (size(s0) > 0) then
            res => f0(s0)
        else
            res => null()
        end if
    end function parses

    ! Función parsee
    function parsee() result(res)
        integer :: res
        integer :: s1
        character(len=:), allocatable :: s2
        
        savePoint = cursor
        s1 = parsenum()
        if (s1 /= -999) then
            s2 = parsesep()
            if (s2 /= "") then
                res = f1(s1)
                return
            end if
        end if
        cursor = savePoint
        res = -999
    end function parsee

    ! Función parsenum
    function parsenum() result(res)
        integer :: res
        character(len=:), allocatable :: s0
        
        if (cursor > len(input)) then
            res = -999
            return
        end if
        
        lexemeStart = cursor
        if (.not. acceptRange('0', '9')) then
            res = -999
            return
        end if
        
        do while (cursor <= len(input))
            if (.not. acceptRange('0', '9')) exit
        end do
        
        s0 = consumeInput()
        res = f2(s0)
    end function parsenum

    ! Función parsesep
    function parsesep() result(res)
        character(len=:), allocatable :: res
        
        if (cursor > len(input)) then
            res = ""
            return
        end if
        
        lexemeStart = cursor
        if (.not. acceptString(char(10))) then  ! Cambio explícito a char(10) para \n
            res = ""
            return
        end if
        
        res = consumeInput()
    end function parsesep

    ! Funciones auxiliares
    function acceptString(str) result(accept)
        character(len=*) :: str
        logical :: accept

        if (cursor > len(input) .or. str /= input(cursor:cursor)) then
            accept = .false.
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptString

    function acceptRange(bottom, top) result(accept)
        character(len=1) :: bottom, top
        logical :: accept

        if (cursor > len(input) .or. .not. (input(cursor:cursor) >= bottom .and. input(cursor:cursor) <= top)) then
            accept = .false.
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptRange

    function consumeInput() result(substr)
        character(len=:), allocatable :: substr
        substr = input(lexemeStart:cursor - 1)
    end function consumeInput

    ! Funciones de conversión
    function intToStr(int) result(cast)
        integer :: int
        character(len=31) :: tmp
        character(len=:), allocatable :: cast
        write(tmp, '(I0)') int
        cast = trim(adjustl(tmp))
    end function intToStr

    function strToStr(str) result(cast)
        character(len=:), allocatable :: str
        character(len=:), allocatable :: cast
        cast = str
    end function strToStr

    ! Funciones semánticas
    function f0(n) result(res)
        integer, dimension(:), intent(in) :: n
        type(node), pointer :: res    
        integer :: i
        
        ! Aseguramos que head empiece nulo
        if (associated(head)) then
            nullify(head)
        end if
        
        ! Construimos la pila en el orden correcto
        do i = 1, size(n)
            call push(n(i))
        end do
        
        ! Retornamos el head de la pila
        res => head
    end function f0

    function f1(n) result(res)
        integer, intent(in) :: n
        integer :: res
        res = n
    end function f1

    function f2(n) result(res)
        character(len=:), allocatable :: n
        integer :: res
        read(n, *) res
    end function f2

end module parser