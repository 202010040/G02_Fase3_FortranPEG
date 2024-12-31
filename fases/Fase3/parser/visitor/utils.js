import FortranTranslator from './Translator.js';
import * as CST from './CST.js';

/** @typedef {import('../visitor/CST.js').Producciones} Produccion*/
/** @typedef {import('../visitor/Visitor.js').default<string>} Visitor*/
/**
 *
 * @param {Produccion[]} cst
 */
export const generateParser = (cst) => {
    /** @type(Visitor) */
    const translator = new FortranTranslator();
    console.log(cst)
    return `
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
        if (peg_${cst.producciones[0].id}()) then
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
    ${cst.producciones.map((rules) => rules.accept(translator)).join('\n')}

    ! -----------> Funciones que corresponden a acciones semanticas

    ${encontrarBloquesCodigo(cst.producciones)
        .map((node) => `        
        function semanticAction_${node.indice}() result(res)
            ${node.contenido}
        end function semanticAction_${node.indice}`)
        .join('\n')}

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

const encontrarBloquesCodigo = (node) => {
    const blocks = [];
    
    if (node instanceof CST.BloqueDeCodigo) {
      blocks.push(node);
    }
    
    Object.values(node).forEach(child => {
      if (child && typeof child === 'object') {
        if (Array.isArray(child)) {
          child.forEach(item => {
            blocks.push(...encontrarBloquesCodigo(item));
          });
        } else {
          blocks.push(...encontrarBloquesCodigo(child));
        }
      }
    });
    
    return blocks;
  };
  
