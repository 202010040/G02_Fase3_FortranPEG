
module parser
    implicit none
    integer, private :: cursor
    character(len=:), allocatable, private :: input, expected

    contains

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

    
        function peg_sum() result(accept)
            logical :: accept
            integer :: i

            accept = .false.
            
        do i = 0, 2
            select case(i)
                
                        case(0)
                            
                if (.not. (peg_num())) then
                    cycle
                end if
                do while (.not. cursor > len(input))
                    if (.not. (peg_num())) then
                        exit
                    end if
                end do
                

                if (.not. (acceptString(' + '))) then
                    cycle
                end if
                

                if (.not. (peg_num())) then
                    cycle
                end if
                do while (.not. cursor > len(input))
                    if (.not. (peg_num())) then
                        exit
                    end if
                end do
                

                if (.not. (acceptPeriod())) then
                    cycle
                end if
                
                            exit
                        

                        case(1)
                            
                if (.not. (peg_letra())) then
                    cycle
                end if
                

                if (.not. (acceptString(' + '))) then
                    cycle
                end if
                

                if (.not. (peg_letra())) then
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

            accept = .false.
            
        do i = 0, 1
            select case(i)
                
                        case(0)
                            
                if (.not. (acceptRange('0', '9'))) then
                    cycle
                end if
                
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

            accept = .false.
            
        do i = 0, 1
            select case(i)
                
                        case(0)
                            
                if (.not. (acceptRange('a', 'z'))) then
                    cycle
                end if
                
                            exit
                        
            case default
                return
            end select
        end do
        
            
            accept = .true.
        end function peg_letra
        

    function acceptString(str) result(accept)
        character(len=*) :: str
        logical :: accept
        integer :: offset

        offset = len(str) - 1
        if (str /= input(cursor:cursor + offset)) then
            accept = .false.
            expected = str
            return
        end if
        cursor = cursor + len(str);
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
            expected = "<ANYTHING>"
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptPeriod

    function acceptEOF() result(accept)
        logical :: accept

        if(.not. cursor > len(input)) then
            accept = .false.
            expected = "<EOF>"
            return
        end if
        accept = .true.
    end function acceptEOF
end module parser
    