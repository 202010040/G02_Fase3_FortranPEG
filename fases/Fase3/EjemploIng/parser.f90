
!auto-generated
module parser
    implicit none
    character(len=:), allocatable, private :: input
    integer, private :: savePoint, lexemeStart, cursor

    interface toStr
        module procedure intToStr
        module procedure strToStr
    end interface
    
    
    type :: operation
        character(len=:), allocatable :: operator
        integer :: operand
    end type

   

    contains
    
    

    function parse(str) result(res)
        character(len=:), allocatable :: str
        integer :: res
        
        input = str
        cursor = 1

        res = peg_Rule_Expression()
    end function parse

    
    function peg_Rule_Expression() result (res)
        integer :: res
        integer :: expr_0_0
        integer, allocatable :: values_0_0(:)
type(operation) :: expr_0_1
type(operation), allocatable :: values_0_1(:)

        integer :: i
        logical :: peg_continue_parsing 

        allocate(values_0_1(0)) 
        savePoint = cursor
        
         
    
        peg_continue_parsing = .true.
        do while (peg_continue_parsing)
        
            do i = 0, 1
                select case(i)
                
                case(0)
                    cursor = savePoint
                    
        expr_0_0 = peg_Rule_Term() 
                    values_0_0 = [values_0_0, expr_0_0] 
                    savePoint = cursor
expr_0_1 = peg_Rule_ExpressionTail() 
                    values_0_1 = [values_0_1, expr_0_1] 
                    savePoint = cursor
        if (acceptEOF()) then
                        peg_continue_parsing = .false.
                        
                res = peg_SemanticAction_Expression_f0(values_0_0 , values_0_1 )

                        exit
                    end if 

                    exit
                
                case default
                peg_continue_parsing = .false.

                if (size(values_0_1) > 0 ) then 
                        res = peg_SemanticAction_Expression_f0(values_0_0 , values_0_1 )
                    
                    else
                        call pegError()
                    end if
                    
                         
                end select
            end do
        
        end do
        
    
        if (allocated(values_0_1)) deallocate(values_0_1) 
    end function peg_Rule_Expression


    function peg_Rule_ExpressionTail() result (res)
        type(operation) :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
integer :: expr_0_3

        integer :: i
        logical :: peg_continue_parsing 

         
        savePoint = cursor
        
         
    
            do i = 0, 1
                select case(i)
                
                case(0)
                    cursor = savePoint
                    
        expr_0_0 = peg_Rule__()
expr_0_1 = peg_Rule_peg_parentesis_1()
expr_0_2 = peg_Rule__()
expr_0_3 = peg_Rule_Term()
        
                res = peg_SemanticAction_ExpressionTail_f0(expr_0_1 , expr_0_3 )
 

                    exit
                
                case default
                peg_continue_parsing = .false.

                call pegError()
                    
                         
                end select
            end do
        
    
        
    end function peg_Rule_ExpressionTail


    function peg_Rule_Term() result (res)
        integer :: res
        character(len=:), allocatable :: expr_0_0
type(operation) :: expr_0_1
type(operation), allocatable :: values_0_1(:)

        integer :: i
        logical :: peg_continue_parsing 

        allocate(values_0_1(0)) 
        savePoint = cursor
        
         
    
        peg_continue_parsing = .true.
        do while (peg_continue_parsing)
        
            do i = 0, 1
                select case(i)
                
                case(0)
                    cursor = savePoint
                    
        expr_0_0 = peg_Rule_Factor() 
                    values_0_0 = [values_0_0, expr_0_0] 
                    savePoint = cursor
expr_0_1 = peg_Rule_peg_parentesis_3() 
                    values_0_1 = [values_0_1, expr_0_1] 
                    savePoint = cursor
        
                res = peg_SemanticAction_Term_f0(values_0_0 , values_0_1 )
 

                    exit
                
                case default
                peg_continue_parsing = .false.

                if (size(values_0_1) > 0 ) then 
                        res = peg_SemanticAction_Term_f0(values_0_0, values_0_1)
                    
                    else
                        call pegError()
                    end if
                    
                         
                end select
            end do
        
        end do
        
    
        if (allocated(values_0_1)) deallocate(values_0_1) 
    end function peg_Rule_Term


    function peg_Rule_Factor() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
integer :: expr_0_2
character(len=:), allocatable :: expr_0_3
character(len=:), allocatable :: expr_0_4
character(len=:), allocatable :: expr_1_0

        integer :: i
        logical :: peg_continue_parsing 

         
        savePoint = cursor
        
         
    
            do i = 0, 2
                select case(i)
                
                case(0)
                    cursor = savePoint
                    
        
                lexemeStart = cursor
                if(.not. acceptString("(")) cycle
                expr_0_0 = consumeInput()
        
expr_0_1 = peg_Rule__()
expr_0_2 = peg_Rule_Expression()
expr_0_3 = peg_Rule__()

                lexemeStart = cursor
                if(.not. acceptString(")")) cycle
                expr_0_4 = consumeInput()
        
        
                res = toStr(expr_0_2 )
 

                    exit
                
                case(1)
                    cursor = savePoint
                    
        expr_1_0 = peg_Rule_Integer()
        
                res = toStr(expr_1_0 )
 

                    exit
                
                case default
                peg_continue_parsing = .false.

                call pegError()
                    
                         
                end select
            end do
        
    
        
    end function peg_Rule_Factor


    function peg_Rule_Integer() result (res)
        integer :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1

        integer :: i
        logical :: peg_continue_parsing 

         
        savePoint = cursor
        
         
    
            do i = 0, 1
                select case(i)
                
                case(0)
                    cursor = savePoint
                    
        expr_0_0 = peg_Rule__()

                lexemeStart = cursor
                if (.not. (acceptRange('0', '9'))) cycle
                do while (cursor <= len(input))
                    if (.not. ( (acceptRange('0', '9')) )) exit
                end do
                expr_0_1 = consumeInput()
            
        
                res = peg_SemanticAction_Integer_f0(expr_0_1 )
 

                    exit
                
                case default
                peg_continue_parsing = .false.

                call pegError()
                    
                         
                end select
            end do
        
    
        
    end function peg_Rule_Integer


    function peg_Rule__() result (res)
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
                do while (cursor <= len(input))
                    if (.not. ( (acceptSet([' ','\t','\n','\r'])) )) exit
                end do
                expr_0_0 = consumeInput()
            
        
                res = toStr(expr_0_0 )
 

                    exit
                
                case default
                peg_continue_parsing = .false.

                call pegError()
                    
                         
                end select
            end do
        
    
        
    end function peg_Rule__


    function peg_Rule_peg_parentesis_1() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_1_0

        integer :: i
        logical :: peg_continue_parsing 

         
        savePoint = cursor
        
         
    
            do i = 0, 2
                select case(i)
                
                case(0)
                    cursor = savePoint
                    
        
                lexemeStart = cursor
                if(.not. acceptString("+")) cycle
                expr_0_0 = consumeInput()
        
        
                res = toStr(expr_0_0 )
 

                    exit
                
                case(1)
                    cursor = savePoint
                    
        
                lexemeStart = cursor
                if(.not. acceptString("-")) cycle
                expr_1_0 = consumeInput()
        
        
                res = toStr(expr_1_0 )
 

                    exit
                
                case default
                peg_continue_parsing = .false.

                call pegError()
                    
                         
                end select
            end do
        
    
        
    end function peg_Rule_peg_parentesis_1


    function peg_Rule_peg_parentesis_2() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_1_0

        integer :: i
        logical :: peg_continue_parsing 

         
        savePoint = cursor
        
         
    
            do i = 0, 2
                select case(i)
                
                case(0)
                    cursor = savePoint
                    
        
                lexemeStart = cursor
                if(.not. acceptString("*")) cycle
                expr_0_0 = consumeInput()
        
        
                res = toStr(expr_0_0 )
 

                    exit
                
                case(1)
                    cursor = savePoint
                    
        
                lexemeStart = cursor
                if(.not. acceptString("/")) cycle
                expr_1_0 = consumeInput()
        
        
                res = toStr(expr_1_0 )
 

                    exit
                
                case default
                peg_continue_parsing = .false.

                call pegError()
                    
                         
                end select
            end do
        
    
        
    end function peg_Rule_peg_parentesis_2


    function peg_Rule_peg_parentesis_3() result (res)
        type(operation) :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2

        integer :: i
        logical :: peg_continue_parsing 

         
        savePoint = cursor
        
         
    
            do i = 0, 1
                select case(i)
                
                case(0)
                    cursor = savePoint
                    
        expr_0_0 = peg_Rule_peg_parentesis_2()
expr_0_1 = peg_Rule__()
expr_0_2 = peg_Rule_Factor()
        
                res = peg_SemanticAction_peg_parentesis_3_f0(expr_0_0 , expr_0_2 )
 

                    exit
                
                case default
                peg_continue_parsing = .false.

                call pegError()
                    
                         
                end select
            end do
        
    
        
    end function peg_Rule_peg_parentesis_3


     
    function peg_SemanticAction_Expression_f0(head, tail) result(res)
        integer , dimension(:), intent(in) :: head
type(operation) , dimension(:), intent(in) :: tail 
        integer :: res
        
        integer :: i

        if (size(tail) < 0) then
            res = head
            return
        end if

        do i = 1, size(tail)
            if (tail(i)%operator == '+') then
                head = head + tail(i)%operand
            else
                head = head - tail(i)%operand
            end if
        end do

        res = head
    
    end function peg_SemanticAction_Expression_f0
    
 
    function peg_SemanticAction_ExpressionTail_f0(operator, operand) result(res)
        character(len=:), allocatable  :: operator
integer  :: operand 
        type(operation) :: res
        

        res = operation(operator, operand)
    
    end function peg_SemanticAction_ExpressionTail_f0
    
 
    function peg_SemanticAction_Term_f0(head, tail) result(res)
        integer, dimension(:), intent(in) :: head
        type(operation) , dimension(:), intent(in) :: tail 
        integer :: res
        
        integer :: i

        if (size(tail) < 0) then
            res = head
            return
        end if

        do i = 1, size(tail)
            if (tail(i)%operator == '*') then
                head = head * tail(i)%operand
            else
                head = head / tail(i)%operand
            end if
        end do

        res = head
    
    end function peg_SemanticAction_Term_f0
    
 
    function peg_SemanticAction_Integer_f0(num) result(res)
        character(len=:), allocatable  :: num 
        integer :: res
        

        read(num, *) res
    
    end function peg_SemanticAction_Integer_f0
    
 
    function peg_SemanticAction_peg_parentesis_3_f0(operator, operand) result(res)
        character(len=:), allocatable  :: operator
character(len=:), allocatable  :: operand 
        type(operation) :: res
        

        res = operation(operator, operand)
    
    end function peg_SemanticAction_peg_parentesis_3_f0
    

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
