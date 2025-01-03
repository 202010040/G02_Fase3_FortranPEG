
!auto-generated
module parser
    implicit none
    character(len=:), allocatable, private :: input
    integer, private :: savePoint, lexemeStart, cursor

    interface toStr
        module procedure intToStr
        module procedure strToStr
    end interface
    
    
	type :: node	
		integer :: value
		type(node), pointer :: next => null()
	end type node

	type(node), pointer :: head => null()

  

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



    function parse(str) result(res)
        character(len=:), allocatable :: str
        type(node), pointer :: res
        
        input = str
        cursor = 1

        res => peg_Rule_s()
    end function parse

    
    function peg_Rule_s() result (res)
        type(node), pointer :: res
        integer :: expr_0_0
integer, allocatable :: values_0_0(:)

        integer :: i
        logical :: peg_continue_parsing 

        allocate(values_0_0(0)) 
        savePoint = cursor
        
         
    
        peg_continue_parsing = .true.
        do while (peg_continue_parsing)
        
            do i = 0, 1
                select case(i)
                
                case(0)
                    cursor = savePoint
                    
        expr_0_0 = peg_Rule_e() 
                    values_0_0 = [values_0_0, expr_0_0] 
                    savePoint = cursor
        if (acceptEOF()) then
                        peg_continue_parsing = .false.
                        
                res => peg_SemanticAction_s_f0(values_0_0 )

                        exit
                    end if 

                    exit
                
                case default
                peg_continue_parsing = .false.

                if (size(values_0_0) > 0 ) then 
                        
                res => peg_SemanticAction_s_f0(values_0_0 )

                    
                    else
                        call pegError()
                    end if
                    
                         
                end select
            end do
        
        end do
        
    
        if (allocated(values_0_0)) deallocate(values_0_0) 
    end function peg_Rule_s


    function peg_Rule_e() result (res)
        integer :: res
        integer :: expr_0_0
character(len=:), allocatable :: expr_0_1

        integer :: i
        logical :: peg_continue_parsing 

         
        savePoint = cursor
        
         
    
            do i = 0, 1
                select case(i)
                
                case(0)
                    cursor = savePoint
                    
        expr_0_0 = peg_Rule_num()
expr_0_1 = peg_Rule_sep()
        
                res = peg_SemanticAction_e_f0(expr_0_0 )
 

                    exit
                
                case default
                peg_continue_parsing = .false.

                call pegError()
                    
                         
                end select
            end do
        
    
        
    end function peg_Rule_e


    function peg_Rule_num() result (res)
        integer :: res
        character(len=:), allocatable :: expr_0_0

        integer :: i
        logical :: peg_continue_parsing 

         
        savePoint = cursor
        
         
    
            do i = 0, 1
                select case(i)
                
                case(0)
                    cursor = savePoint
                    
        
                lexemeStart = cursor
                if (.not. (acceptRange('0', '9'))) cycle
                do while (cursor <= len(input))
                    if (.not. ( (acceptRange('0', '9')) )) exit
                end do
                expr_0_0 = consumeInput()
            
        
                res = peg_SemanticAction_num_f0(expr_0_0 )
 

                    exit
                
                case default
                peg_continue_parsing = .false.

                call pegError()
                    
                         
                end select
            end do
        
    
        
    end function peg_Rule_num


    function peg_Rule_sep() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0

        integer :: i
        logical :: peg_continue_parsing 

         
        savePoint = cursor
        
         
    
            do i = 0, 1
                select case(i)
                
                case(0)
                    cursor = savePoint
                    
        
                lexemeStart = cursor
                if(.not. acceptString(char(10))) cycle
                expr_0_0 = consumeInput()
        
        
                res = toStr(expr_0_0 )
 

                    exit
                
                case default
                peg_continue_parsing = .false.

                call pegError()
                    
                         
                end select
            end do
        
    
        
    end function peg_Rule_sep


     
    function peg_SemanticAction_s_f0(n) result(res)
        integer , dimension(:), intent(in) :: n 
        type(node), pointer :: res
        	
	integer i
	do i = 1, size(n)
		call push(n(i))
	end do
	res => head

    end function peg_SemanticAction_s_f0
    
 
    function peg_SemanticAction_e_f0(n) result(res)
        integer  :: n 
        integer :: res
        
	res = n

    end function peg_SemanticAction_e_f0
    
 
    function peg_SemanticAction_num_f0(n) result(res)
        character(len=:), allocatable  :: n 
        integer :: res
        
  	read(n, *) res

    end function peg_SemanticAction_num_f0
    

    function acceptString(str) result(accept)
        character(len=*) :: str
        logical :: accept
        integer :: offset

        offset = len(str) - 1
        if (cursor > len(input) .or. cursor + offset > len(input) .or. &
            str /= input(cursor:cursor + offset)) then
            accept = .false.
            return
        end if
        cursor = cursor + len(str)
        accept = .true.
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
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptPeriod

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
end module parser
