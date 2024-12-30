import Tokenizer from "./Tokenizador.js"

export let gramatica;

export const generarParser = (grammar) => {
    gramatica = grammar;
    const tokenizer = new Tokenizer()
    return `
module parser

implicit none
implicit none
integer, private :: cursor
character(len=:), allocatable, private :: input, expected

contains

    function parse(inputstr) result(res)
        character(len=:), intent(in), allocatable :: inputstr
        type(node), pointer :: res
        input = inputstr
        res => fortranPEG_${gramatica[0].id}()
    end function parse

    ${(() => {
        let result = '';
        do {
            
            result += grammar.map((produccion) => produccion.accept(tokenizer)).join('\n');
        } while (tokenizer.pendingRules.length > 0);

        return result;  
    })()}

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"



function tolower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i

        lower_str = str 
        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                lower_str(i:i) = achar(iachar(str(i:i)) + 32)
            end if
        end do
end function tolower

function replace_special_characters(input_string) result(output_string)
    implicit none
    character(len=:), allocatable, intent(in) :: input_string
    character(len=:), allocatable :: temp_string
    character(len=:), allocatable :: output_string
    integer :: i, length

    temp_string = ""
    length = len(input_string)

    do i = 1, length
        select case (ichar(input_string(i:i)))
        case (10) ! Nueva línea
            temp_string = temp_string // '\\n'
        case (9)  ! Tabulación
            temp_string = temp_string // '\\t'
        case (13) ! Retorno de carro
            temp_string = temp_string // '\\r'
        case (32) ! Espacio
            if (input_string(i:i) == " ") then
                temp_string = temp_string // "_"
            else
                temp_string = temp_string // input_string(i:i)
            end if
        case default
            temp_string = temp_string // input_string(i:i)
        end select
    end do
    allocate(character(len=len(temp_string)) :: output_string)
    output_string = temp_string
end function

end module parser 

`;
}

export const renderQuantifierOption = (qty, condition, length) => {
    var resultOneMore = `
    initialCursor = cursor
    do while (cursor <= len_trim(input) .and. (${condition}))
        cursor = cursor + ${length}
    end do
    if (cursor > initialCursor) then
        buffer = buffer // input(initialCursor:cursor-1) 
        buffer = replace_special_characters(buffer)
    else
        cursor = initialCursor
        concat_failed = .true.
        buffer = ""
    end if`      ;

    var resultZeroMore = `
    initialCursor = cursor
    do while (cursor <= len_trim(input) .and. (${condition}))
        cursor = cursor + ${length}
    end do
    if (cursor > initialCursor) then
        buffer = buffer // input(initialCursor:cursor-1) 
        buffer = replace_special_characters(buffer)
    end if`      ;

    var resultZeroOrOne = `
    if (cursor <= len_trim(input) .and. (${condition})) then 
        buffer = buffer // input(cursor:cursor + ${length - 1})
        buffer = replace_special_characters(buffer)
        cursor = cursor + ${length}
    end if` ;

    var one = `
    if (cursor <= len_trim(input) .and. (${condition})) then 
        buffer = buffer // input(cursor:cursor + ${length - 1})
        buffer = replace_special_characters(buffer)
        cursor = cursor + ${length}
    else
        concat_failed = .true.
        buffer = ""
    end if` ;

    
    switch (qty) {
        case '+': return resultOneMore;
        case '*': return resultZeroMore;
        case '?': return resultZeroOrOne;
        default: return one;
    }   

}

export const renderAny = () => {
    return `
    ! Cualquier carácter es aceptado como lexema
    if (cursor <= len_trim(input)) then
        buffer = replace_special_characters(buffer)
        cursor = cursor + 1
    else
        concat_failed = .true.
        buffer = ""
    end if
    `;
}

export const renderRango = (node) => {
    const condition = node.isCase 
    ? `iachar(tolower(input(cursor:cursor))) >= iachar("${node.start}") .and. &
    iachar(tolower(input(cursor:cursor))) <= iachar("${node.end}")`
    : `iachar(input(cursor:cursor)) >= iachar("${node.start}") .and. &
    iachar(input(cursor:cursor)) <= iachar("${node.end}")`;

    return "(" + condition + ")";
}