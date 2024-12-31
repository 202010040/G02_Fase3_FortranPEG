module parser
    implicit none
    integer, private :: cursor
    character(len=:), allocatable, private :: input, expected

    contains

    subroutine parse(str)
        character(len=:), allocatable, intent(in) :: str

        input = trim(str)
        cursor = 1
        expected = ''
        if (peg_sum()) then
            if (acceptEOF()) then
                print *, "Parsed input successfully!"
            else
                call error()
            end if
        else
            call error()
        end if
    end subroutine parse

    subroutine error()
        if (cursor > len(input)) then
            print *, "Error: Expected '"//expected//"', but found <EOF>"
        else
            print *, "Error: Expected '"//expected//"', but found '"//input(cursor:cursor)//"'"
        end if
        call exit(1)
    end subroutine error

    function peg_sum() result(accept)
        logical :: accept

        accept = .false.

        ! Case 1: num + '+' num + '.'
        if (peg_num() .and. acceptString('+') .and. peg_num() .and. acceptPeriod()) then
            accept = .true.
            return
        end if

        ! Case 2: letra + '+' letra
        if (peg_letra() .and. acceptString('+') .and. peg_letra()) then
            accept = .true.
            return
        end if
    end function peg_sum

    function peg_num() result(accept)
        logical :: accept
        accept = acceptRange('0', '9')
    end function peg_num

    function peg_letra() result(accept)
        logical :: accept
        accept = acceptRange('a', 'z')
    end function peg_letra

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

        if (cursor > len(input) .or. input(cursor:cursor) < bottom .or. input(cursor:cursor) > top) then
            accept = .false.
        else
            cursor = cursor + 1
            accept = .true.
        end if
    end function acceptRange

    function acceptPeriod() result(accept)
        logical :: accept

        if (cursor > len(input) .or. input(cursor:cursor) /= '.') then
            accept = .false.
            expected = "."
        else
            cursor = cursor + 1
            accept = .true.
        end if
    end function acceptPeriod

    function acceptEOF() result(accept)
        logical :: accept
        accept = (cursor > len(input))
    end function acceptEOF
end module parser
