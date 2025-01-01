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
        
        ! Asignamos la entrada y inicializamos el cursor
        input = str
        cursor = 1
        
        ! Procesamos la entrada
        res => parses()
    end function parse
        

    ! Función parses modificada
    function parses() result(res)
        type(node), pointer :: res
        integer :: expr_0_0
        integer :: i
        integer, allocatable :: values(:)
        logical :: continue_parsing
        
        savePoint = cursor
        allocate(values(0))
        
        ! Loop principal para procesar múltiples números
        continue_parsing = .true.
        do while (continue_parsing)
            do i = 0, 1
                select case(i)
                case(0)
                    cursor = savePoint
                    
                    expr_0_0 = parsee()
                    
                    ! Agregar valor al array
                    values = [values, expr_0_0]
                    
                    ! Guardar punto para próxima iteración
                    savePoint = cursor
                    
                    ! Verificar si llegamos al final
                    if (acceptEOF()) then
                        continue_parsing = .false.
                        res => f0(values)
                        exit
                    end if
                    
                    exit
                    
                case default
                    continue_parsing = .false.
                    if (size(values) > 0) then
                        res => f0(values)
                    else
                        call pegError()
                    end if
                end select
            end do
        end do
        
        if (allocated(values)) deallocate(values)
    end function parses
        

    function parsee() result(res)
        integer :: res
        integer :: expr_0_0
        character(len=:), allocatable :: expr_0_1
        integer :: i
        
        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            case(0)
                cursor = savePoint
                
                expr_0_0 = parsenum()
                expr_0_1 = parsesep()
                
                res = f1(expr_0_0)
                exit
                
            case default
                call pegError()
            end select
        end do
    end function parsee
    
    function parsenum() result(res)
        integer :: res
        character(len=:), allocatable :: expr_0_0
        integer :: i
        
        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            case(0)
                cursor = savePoint
                
                lexemeStart = cursor
                if (.not. acceptRange('0', '9')) cycle
                
                do while (cursor <= len(input))
                    if (.not. acceptRange('0', '9')) exit
                end do
                
                expr_0_0 = consumeInput()
                res = f2(expr_0_0)
                exit
                
            case default
                call pegError()
            end select
        end do
    end function parsenum
              

    ! Función parsesep
    function parsesep() result(res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
        integer :: i
    
        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            case(0)
                cursor = savePoint
                lexemeStart = cursor
                
                ! Intenta aceptar el salto de línea
                if (.not. acceptString(char(10))) cycle
                expr_0_0 = consumeInput()
                res = expr_0_0
                exit
                
            case default
                call pegError()
            end select
        end do
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

    function acceptEOF() result(accept)
        logical :: accept

        if(.not. cursor > len(input)) then
            accept = .false.
            return
        end if
        accept = .true.
    end function acceptEOF

    function consumeInput() result(substr)
        character(len=:), allocatable :: substr
        substr = input(lexemeStart:cursor - 1)
    end function consumeInput
    
    subroutine pegError()
        print '(A,I1,A)', "Error at ", cursor, ": '"//input(cursor:cursor)//"'"

        call exit(1)
    end subroutine pegError

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