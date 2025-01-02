import * as CST from './CST.js';

export default class FortranTranslator {

    visitProducciones(node) {
        return `
        function peg_${node.id}() result(accept)
            logical :: accept
            integer :: i
            expected = "${node.id}"
            accept = .false.
    
            if (len(input) == 0) then
                expected = "<NON-EMPTY INPUT>"
                return
            end if
    
            ${node.expr.accept(this)}
            ${
                node.start 
                    ? `
                    if (.not. acceptEOF()) then
                        return
                    end if
                    `
                    : ''
            }
            accept = .true.
        end function peg_${node.id}
        `;
    }
    

    visitOpciones(node) {
        const template = `
        do i = 0, ${node.exprs.length}
            select case(i)
                ${node.exprs
                    .map(
                        (expr, i) => `
                        case(${i})
                            ${expr.accept(this)}
                            exit
                        `
                    )
                    .join('\n')}
            case default
                return
            end select
        end do
        `;
        return template;
    }

    visitUnion(node) {
        return node.exprs.map((expr) => expr.accept(this)).join('\n');
    }

    visitExpresion(node) {
        const condition = node.expr.accept(this);
        let a = node.bloque !== null ? node.bloque.accept(this) : ''
        //console.log('Bloque, ' , a)
        switch (node.qty) {
            case '+':
                return `
                if (cursor > len(input) .or. .not. (${condition})) then
                    cycle
                end if
                do while (cursor <= len(input))
                    if (.not. (${condition})) then
                        exit
                    end if
                end do
                ${node.bloque !== null ? node.bloque.accept(this) : ''}
                `;
            case '*':
                return `
                do while (cursor <= len(input))
                    if (.not. (${condition})) then
                        exit
                    end if
                end do
                ${node.bloque !== null ? node.bloque.accept(this) : ''}
                `;                
            default:
                return `
                if (cursor > len(input) .or. .not. (${condition})) then
                    cycle
                end if
                ${node.bloque  !== null ? node.bloque.accept(this) : ''}
                `;
        }
    }
    

    visitString(node) {
        return `acceptString('${node.val}')`;
    }
    

    visitCorchetes(node) {
        ////console.log('Clases ', node)
        let characterClass = [];
        const set = node.exprs
            .filter((char) => char instanceof CST.literalRango)
            .map((char) => `'${char.val}'`);
        const ranges = node.exprs
            .filter((char) => char instanceof CST.rango)
            .map((range) => range.accept(this));
        if (set.length !== 0) {
            characterClass = [`acceptSet([${set.join(',')}])`];
        }
        if (ranges.length !== 0) {
            characterClass = [...characterClass, ...ranges];
        }
        return characterClass.join(' .or. '); // acceptSet(['a','b','c']) .or. acceptRange('0','9') .or. acceptRange('A','Z')
    }

    visitrango(node) {
        return ` acceptRange('${node.start}', '${node.end}') `;
    }
    

    visitIdentificador(node) {
        return `peg_${node.id}()`;
    }

    visitPunto(node) {
        return 'acceptPeriod()';
    }

    visitidRel(node) { 
        return `peg_${node.val}()`;
    }

    visitAny(node) {
        return 'acceptPeriod()';
    }

    visitFin(node) {
        return `acceptEOF()`;
    }
    

    visitgrupo(node) {
        node.expr.qty = node.qty
        return node.expr.accept(this);
    }

    visitliteralRango(node) {
        const literalMap = {
            "\\t": "char(9)",
            "\\n": "char(10)",
            " ": "char(32)",
            "\\r": "char(13)"
        };
        const literalFortran = literalMap[node.val] || `"${node.val}"`;
    
        return `
        if (cursor > len(input)) then
            accept = .false.
        else
            accept = (${node.isCase 
                ? `tolower(input(cursor:cursor)) == tolower(${literalFortran})`
                : `input(cursor:cursor) == ${literalFortran}`})
        end if
        `;
    }
    

    visitBloqueDeCodigo(node) {
        if (node.start){ // Ignorar si es la primera produccion
            return ''
        }
        return`
        semanticExp_${node.indice} =  semanticAction_${node.indice}()
        `
    }
}