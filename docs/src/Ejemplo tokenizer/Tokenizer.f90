module tokenizer
    implicit none

    contains

    function nextSym(input, cursor) result(lexeme)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: cursor
        character(len=:), allocatable :: lexeme

        if (cursor > len(input)) then
            allocate(character(len=3) :: lexeme)
            lexeme = "EOF"
            return
        end if

        select case (input(cursor:cursor))
        case ('=', '+', '-', '*', '/', '(', ')', ',', ':', ';')
            allocate(character(len=1) :: lexeme)
            lexeme = input(cursor:cursor)
            cursor = cursor + 1
            return
        case ('"')
            call extractString(input, cursor, lexeme)
            return
        case (' ')
            call extractSpace(input, cursor, lexeme)
            return
        case default
            if (isAlpha(input(cursor:cursor))) then
                call extractToken(input, cursor, lexeme, "identifier")
                return
            else if (isDigit(input(cursor:cursor))) then
                call extractToken(input, cursor, lexeme, "number")
                return
            else
                allocate(character(len=5) :: lexeme)
                lexeme = "ERROR"
                cursor = cursor + 1
                return
            end if
        end select
    end function nextSym

    subroutine extractToken(input, cursor, lexeme, tokenType)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: cursor
        character(len=:), allocatable :: lexeme
        character(len=*), intent(in) :: tokenType
        integer :: start

        start = cursor

        if (tokenType == "identifier") then
            do while (cursor <= len(input) .and. isAlphaNum(input(cursor:cursor)))
                cursor = cursor + 1
            end do
        else if (tokenType == "number") then
            do while (cursor <= len(input) .and. isDigit(input(cursor:cursor)))
                cursor = cursor + 1
            end do
        end if

        allocate(character(len=cursor - start) :: lexeme)
        lexeme = input(start:cursor - 1)
    end subroutine extractToken

    subroutine extractString(input, cursor, lexeme)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: cursor
        character(len=:), allocatable :: lexeme
        integer :: start

        start = cursor + 1
        cursor = cursor + 1

        do while (cursor <= len(input) .and. input(cursor:cursor) /= '"')
            cursor = cursor + 1
        end do

        if (cursor <= len(input)) then
            allocate(character(len=cursor - start) :: lexeme)
            lexeme = input(start:cursor - 1)
            cursor = cursor + 1
        else
            allocate(character(len=5) :: lexeme)
            lexeme = "ERROR"
        end if
    end subroutine extractString

    subroutine extractSpace(input, cursor, lexeme)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: cursor
        character(len=:), allocatable :: lexeme
        integer :: start

        start = cursor

        do while (cursor <= len(input) .and. input(cursor:cursor) == ' ')
            cursor = cursor + 1
        end do

        allocate(character(len=cursor - start) :: lexeme)
        lexeme = "SPACE"
    end subroutine extractSpace

    logical function isAlpha(c)
        character(len=1), intent(in) :: c
        isAlpha = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')
    end function isAlpha

    logical function isDigit(c)
        character(len=1), intent(in) :: c
        isDigit = (c >= '0' .and. c <= '9')
    end function isDigit

    logical function isAlphaNum(c)
        character(len=1), intent(in) :: c
        isAlphaNum = isAlpha(c) .or. isDigit(c)
    end function isAlphaNum

end module tokenizer