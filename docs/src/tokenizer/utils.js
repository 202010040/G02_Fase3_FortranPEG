import Tokenizer from "./Tokenizer.js"

export let gramatica;

export function generateTokenizer(grammar){
    //console.log(grammar);
    gramatica = grammar;
    const tokenizer = new Tokenizer()
    return `
module parser
implicit none

contains
function nextSym(input, cursor) result(lexeme)
    character(len=*), intent(in) :: input
    integer, intent(inout) :: cursor
    character(len=:), allocatable :: lexeme
    integer :: i
    logical :: ejecuta_ciclo
    integer :: start_cursor
    character(len=:), allocatable :: lexeme_accumulated

    
    ${grammar.map((produccion) => produccion.accept(tokenizer)).join('\n')}

    if (cursor > len(input)) then
        allocate( character(len=3) :: lexeme )
        lexeme = "EOF"
        return
    end if

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"
end function nextSym

!Autogenerado por default para validacion Case insensitive
function to_lower(str) result(lower_str)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: lower_str
    integer :: i

    lower_str = str
    do i = 1, len(str)
        if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
            lower_str(i:i) = char(iachar(str(i:i)) + 32)
        end if
    end do
end function to_lower
end module parser 

    `
}

export function generateCaracteres(chars) {
    if (chars.length === 0) return '';
    return `
    if (findloc([${chars
        .map((char) => `"${char}"`)
        .join(', ')}], input(i:i), 1) > 0) then
        lexeme = input(cursor:i)
        cursor = i + 1
        return
    end if
    `;
}

export function CondicionalStrSencilla(node){
    let condicional = node.isCase 
    ? `cursor <= len(input) - ${node.val.length - 1} .and. to_lower(input(cursor:cursor + ${node.val.length - 1})) == "${node.val.toLowerCase()}"`
    : `cursor <= len(input) - ${node.val.length - 1} .and. input(cursor:cursor + ${node.val.length - 1}) == "${node.val}"`
    return condicional;
}

export function KleeneLiterales(node){
    let condicional = CondicionalStrSencilla(node);
    return `
    ! * en literales
    ejecuta_ciclo = .true.
    start_cursor = cursor  
    allocate(character(len=0) :: lexeme_accumulated)  
    do while (ejecuta_ciclo)	
        if ( ${condicional} ) then
            cursor = cursor + ${node.val.length}
            lexeme_accumulated = lexeme_accumulated // "${node.val}"
        else
            ejecuta_ciclo = .false.
        end if
    end do
    if (len(lexeme_accumulated) > 0) then
        allocate(character(len=len(lexeme_accumulated)) :: lexeme)
        lexeme = lexeme_accumulated
        return
    end if
`    
}

export function PositivaLiterales(node){
    let condicional = CondicionalStrSencilla(node);
    return `
    ! + en literales
    ejecuta_ciclo = .true.
    start_cursor = cursor  
    allocate(character(len=0) :: lexeme_accumulated)  
    if ( ${condicional} ) then
        cursor = cursor + ${node.val.length}
        lexeme_accumulated = "${node.val}"
    else
        print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
        allocate(character(len=5) :: lexeme)
        lexeme = "ERROR"
        return
    end if
    do while (ejecuta_ciclo)	
        if ( ${condicional} ) then
            cursor = cursor + ${node.val.length}
            lexeme_accumulated = lexeme_accumulated // "${node.val}"
        else
            ejecuta_ciclo = .false.
        end if
    end do
    if (len(lexeme_accumulated) > 0) then
        allocate(character(len=len(lexeme_accumulated)) :: lexeme)
        lexeme = lexeme_accumulated
        return
    end if
`    
}

export function TernariaLiterales(node){
    let condicional = CondicionalStrSencilla(node);
    return `
    ! ? en literales
    start_cursor = cursor  
    allocate(character(len=0) :: lexeme_accumulated)  
    if ( ${condicional} ) then
        cursor = cursor + ${node.val.length}
        lexeme_accumulated = "${node.val}"
    end if
    if (len(lexeme_accumulated) > 0) then
        allocate(character(len=len(lexeme_accumulated)) :: lexeme)
        lexeme = lexeme_accumulated
        return
    end if
`    
}

export function CondicionalCorchete(node) {
    let caseInsensitive = node.isCase != null;

    // Generar condiciones basadas en rangos y caracteres específicos
    let condiciones = node.chars.map(char => {
        if (char.rangoInicial && char.rangoFinal) {
            // Rango de caracteres
            if (caseInsensitive) {
                return `( to_lower(input(cursor:cursor)) >= '${char.rangoInicial.toLowerCase()}' .and. to_lower(input(cursor:cursor)) <= '${char.rangoFinal.toLowerCase()}' )`;
            } else {
                return `( input(cursor:cursor) >= '${char.rangoInicial}' .and. input(cursor:cursor) <= '${char.rangoFinal}' )`;
            }
        } else {
            // Carácter específico
            if (caseInsensitive) {
                return `to_lower(input(cursor:cursor)) == '${char.toLowerCase()}'`;
            } else {
                return `input(cursor:cursor) == '${char}'`;
            }
        }
    });

    // Combinar las condiciones con un límite para no exceder el tamaño del input
    let condicional = `cursor <= len(input) .and. (` + condiciones.join(" .or. ") + `)`;
    return condicional;
}


export function KleeneCorchetes(node) {
    let condicional = CondicionalCorchete(node);
    
    return `
    ! * en []
    ejecuta_ciclo = .true.
    start_cursor = cursor
    allocate(character(len=0) :: lexeme_accumulated)
    do while (ejecuta_ciclo)
        if (${condicional}) then
            lexeme_accumulated = lexeme_accumulated // input(cursor:cursor)
            cursor = cursor + 1
        else
            ejecuta_ciclo = .false.
        end if
    end do
    if (len(lexeme_accumulated) > 0) then
        allocate(character(len=len(lexeme_accumulated)) :: lexeme)
        lexeme = lexeme_accumulated
        return
    end if
    `;
}

export function PositivaCorchetes(node) {
    // Generar condiciones basadas en rangos y caracteres específicos
    let condicional = CondicionalCorchete(node);
    // Validar si el cursor supera el tamaño del input antes de cualquier acceso
    return `
    ! + en []
    ejecuta_ciclo = .true.
    start_cursor = cursor
    allocate(character(len=0) :: lexeme_accumulated)

    ! Verificar EOF antes de cualquier operación
    if (cursor > len(input)) then
        allocate(character(len=3) :: lexeme)
        lexeme = "EOF"
        return
    end if

    ! Validar al menos un carácter
    if (${condicional}) then
        lexeme_accumulated = lexeme_accumulated // input(cursor:cursor)
        cursor = cursor + 1
    else
        print *, "Error léxico en col ", cursor, ', "'//input(cursor:cursor)//'"'
        allocate(character(len=5) :: lexeme)
        lexeme = "ERROR"
        return
    end if

    ! Procesar caracteres adicionales
    do while (ejecuta_ciclo)
        if (cursor > len(input)) exit
        if (${condicional}) then
            lexeme_accumulated = lexeme_accumulated // input(cursor:cursor)
            cursor = cursor + 1
        else
            ejecuta_ciclo = .false.
        end if
    end do

    ! Verificar si se acumuló algún lexema válido
    if (len(lexeme_accumulated) > 0) then
        allocate(character(len=len(lexeme_accumulated)) :: lexeme)
        lexeme = lexeme_accumulated
        return
    end if

    ! Manejo de EOF explícito
    if (cursor > len(input)) then
        allocate(character(len=3) :: lexeme)
        lexeme = "EOF"
        return
    end if
    `;
}

export function TernariaCorchetes(node) {
    // Generar condiciones basadas en rangos y caracteres específicos
    let condicional = CondicionalCorchete(node);

    return `
    ! ? en []
    start_cursor = cursor
    allocate(character(len=0) :: lexeme_accumulated)

    ! Validar si el carácter cumple la condición
    if (${condicional}) then
        lexeme_accumulated = lexeme_accumulated // input(cursor:cursor)
        cursor = cursor + 1
    end if

    ! Generar el lexema acumulado
    if (len(lexeme_accumulated) > 0) then
        allocate(character(len=len(lexeme_accumulated)) :: lexeme)
        lexeme = lexeme_accumulated
        return
    end if

    `;
}
