import {detectFortranType} from './compiler/utils.js';

export const main = (data) => {
return ( 
`
!auto-generated
module parser
    implicit none
    character(len=:), allocatable, private :: input
    integer, private :: savePoint, lexemeStart, cursor

    interface toStr
        module procedure intToStr
        module procedure strToStr
    end interface
    
    ${data.beforeContains}

    contains
    
    ${data.afterContains}

    function parse(str) result(res)
        character(len=:), allocatable :: str
        ${data.startingRuleType} :: res
        
        input = str
        cursor = 1

        res ${detectFortranType(data.startingRuleType) == 'pointer' ? '=>' : '=' } ${data.startingRuleId}()
    end function parse

    ${data.rules.join('\n')}

    ${data.actions.join('\n')}

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
        
        if (cursor > len(input)) then
            accept = .false.
            return
        end if
        
        accept = any(set == input(cursor:cursor))
        if (accept) cursor = cursor + 1
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
`)};

export const rule = (data) => `
    function peg_Rule_${data.id}() result (res)
        ${data.returnType} :: res
        ${data.exprDeclarations.join('\n')}

        integer :: i
        logical :: peg_continue_parsing 

        ${data.allocateStmts.join('\n')} 
        savePoint = cursor
        
        ${data.expr}
        ${data.deallocateStmts.join('\n')}
    end function peg_Rule_${data.id}
`;

function extractReturn(input) {
    const regex = /continue_parsing\s*=\s*\.false\.\s*(.*?)\s*exit/gs;
    const matches = [];
    let match;

    while ((match = regex.exec(input)) !== null) {
        matches.push(match[1].trim());
    }

    return matches;
}

export const election = (data, cuantificadores) => {
    console.log('Elecciones: ', data, cuantificadores)
    return ` 
    ${cuantificadores 
        ? `
        peg_continue_parsing = .true.
        do while (peg_continue_parsing)
        ` 
        :''}
            do i = 0, ${data.exprs.length}
                select case(i)
                ${data.exprs.map(
                    (expr, i) => `
                case(${i})
                    cursor = savePoint
                    ${expr}
                    exit
                `
                ).join('')}
                case default
                peg_continue_parsing = .false.

                ${cuantificadores 
                    ? 
                    `${data.sizeValidators.map(x => (
                        `if (size(values_${x[0]}_${x[1]}) > 0 ) then 
                        ${extractReturn(data.exprs[0])[0]}
                    `
                    )).join('else\n')}
                    else
                        call pegError()
                    end if`
                    : `call pegError()`}
                    
                         
                end select
            end do
        ${cuantificadores 
        ? `
        end do
        ` 
        :''}
    `;
};


export const union = (data) => { 
    //console.log('Data Union', data, data.resultExpr )
    return (
`
        ${data.exprs.join('\n')}
        ${data.startingRule 
            ? `if (acceptEOF()) then
                        peg_continue_parsing = .false.
                        ${data.resultExpr}
                        exit
                    end if` 
            : `${data.resultExpr}`} 
`
) };

export const strExpr = (data) => {
    if (!data.quantifier) {
        return `
                lexemeStart = cursor
                if(.not. ${data.expr}) cycle
                ${data.destination} = consumeInput()
        `;
    }
    switch (data.quantifier) {
        case '+': // Cerradura positiva
            return `
                lexemeStart = cursor
                if (.not. ${data.expr}) cycle
                do while (cursor <= len(input))
                    if (.not. ( ${data.expr} )) exit
                end do
                ${data.destination} = consumeInput()
            `;
        case '*': // Cerradura de Kleene
            return `
                lexemeStart = cursor
                do while (cursor <= len(input))
                    if (.not. ( ${data.expr} )) exit
                end do
                ${data.destination} = consumeInput()
            `;
        default:
            throw new Error(
                `'${data.quantifier}' quantifier needs implementation`
            );
    }
};


export const strResultExpr = (data) => `
                res = ${data.exprs.map((expr) => `toStr(${expr})`).join('//')}
`;

export const fnResultExpr = (data) => {
return (
`
                res ${detectFortranType(data.tipo) == 'pointer' ? '=>' : '=' } ${data.fnId}(${data.exprs.join(', ')})
`)};

export const action = (data) => {
    const signature = data.signature.join(', ');
    return ` 
    function peg_SemanticAction_${data.ruleId}_f${data.choice}(${signature}) result(res)
        ${data.paramDeclarations.join('\n')} 
        ${data.returnType} :: res
        ${data.code}
    end function peg_SemanticAction_${data.ruleId}_f${data.choice}
    `;
};


