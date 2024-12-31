
module parser
    implicit none
    integer, private :: cursor
    character(len=:), allocatable, private :: input, expected
    
    ! Declaraciones de tipos necesarios
    logical :: semanticExp_2

    ! Codigo antes de contains autogenerado INICIO
    type :: node	
		integer :: value
		type(node), pointer :: next => null()
	end type node

	type(node), pointer :: head => null()
    ! Codigo antes de contains autogenerado FIN
    contains
    ! Codigo despues de contains autogenerado INICIO
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
    ! Codigo despues de contains autogenerado FIN

    subroutine parse(str)
        character(len=:), allocatable, intent(in) :: str

        input = str
        cursor = 1
        expected = ''
        if (peg_sum()) then
            print *, "Parsed input succesfully!"
        else
            call error()
        end if
    end subroutine parse

    subroutine error()
        if (cursor > len(input)) then
            print *, "Error: Expected '"//expected//"', but found <EOF>"
            call exit(1)
        end if
        print *, "Error: Expected '"//expected//"', but found '"//input(cursor:cursor)//"'"
        call exit(1)
    end subroutine error

    ! -----------> Funciones que corresponden a producciones
    
        function peg_sum() result(accept)
            logical :: accept
            integer :: i
            expected = "sum"
            accept = .false.
    
            if (len(input) == 0) then
                expected = "<NON-EMPTY INPUT>"
                return
            end if
    
            
        do i = 0, 2
            select case(i)
                
                        case(0)
                            
                if (cursor > len(input) .or. .not. (peg_num())) then
                    cycle
                end if
                do while (cursor <= len(input))
                    if (.not. (peg_num())) then
                        exit
                    end if
                end do
                
                

                if (cursor > len(input) .or. .not. (acceptString('+'))) then
                    cycle
                end if
                
                

                if (cursor > len(input) .or. .not. (peg_num())) then
                    cycle
                end if
                do while (cursor <= len(input))
                    if (.not. (peg_num())) then
                        exit
                    end if
                end do
                
                

                if (cursor > len(input) .or. .not. (acceptPeriod())) then
                    cycle
                end if
                
                
                            exit
                        

                        case(1)
                            
                if (cursor > len(input) .or. .not. (peg_letra())) then
                    cycle
                end if
                
                

                if (cursor > len(input) .or. .not. (acceptString('+'))) then
                    cycle
                end if
                
                

                if (cursor > len(input) .or. .not. (peg_letra())) then
                    cycle
                end if
                
                
                            exit
                        
            case default
                return
            end select
        end do
        
            
                    if (.not. acceptEOF()) then
                        return
                    end if
                    
            accept = .true.
        end function peg_sum
        

        function peg_num() result(accept)
            logical :: accept
            integer :: i
            expected = "num"
            accept = .false.
    
            if (len(input) == 0) then
                expected = "<NON-EMPTY INPUT>"
                return
            end if
    
            
        do i = 0, 1
            select case(i)
                
                        case(0)
                            
                if (cursor > len(input) .or. .not. ( acceptRange('0', '9') )) then
                    cycle
                end if
                
        semanticExp_2 =  semanticAction_2()
        
                
                            exit
                        
            case default
                return
            end select
        end do
        
            
            accept = .true.
        end function peg_num
        

        function peg_letra() result(accept)
            logical :: accept
            integer :: i
            expected = "letra"
            accept = .false.
    
            if (len(input) == 0) then
                expected = "<NON-EMPTY INPUT>"
                return
            end if
    
            
        do i = 0, 1
            select case(i)
                
                        case(0)
                            
                if (cursor > len(input) .or. .not. ( acceptRange('a', 'z') )) then
                    cycle
                end if
                
                
                            exit
                        
            case default
                return
            end select
        end do
        
            
            accept = .true.
        end function peg_letra
        

    ! -----------> Funciones que corresponden a acciones semanticas

            
        function semanticAction_2() result(res)
            
    logical :: res
    res = .true.  ! Reemplaza con lógica real si es necesario
    
        end function semanticAction_2

    ! -----------> Funciones predeterminadas
    function acceptString(str) result(accept)
        character(len=*) :: str
        logical :: accept
        integer :: offset

        offset = len(str) - 1
        if (cursor + offset > len(input) .or. str /= input(cursor:cursor + offset)) then
            accept = .false.
            expected = str
        else
            cursor = cursor + len(str)
            accept = .true.
        end if
    end function acceptString


    function acceptRange(bottom, top) result(accept)
        character(len=1) :: bottom, top
        logical :: accept

        if(.not. (input(cursor:cursor) >= bottom .and. input(cursor:cursor) <= top)) then
            accept = .false.
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptRange

    function acceptSet(set) result(accept)
        character(len=1), dimension(:) :: set
        logical :: accept

        if(.not. (findloc(set, input(cursor:cursor), 1) > 0)) then
            accept = .false.
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptSet

    function acceptPeriod() result(accept)
        logical :: accept

        if (cursor > len(input)) then
            accept = .false.
            expected = "<ANYTHING>"
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptPeriod

    function acceptEOF() result(accept)
        logical :: accept

        if(.not. (cursor > len(input))) then
            accept = .false.
            expected = "<EOF>"
            return
        end if
        accept = .true.
    end function acceptEOF
end module parser
    