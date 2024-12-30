import Visitor from './Visitor.js';
import * as n from './CST.js';
import { renderAny, renderProduction, renderQuantifierOption, renderRango } from './utils.js';

export default class Tokenizer extends Visitor {
    constructor() {
        super();
        this.calledRules = [];
        this.pendingRules = [];
        this.isFisrtRule = true; 
        this.nameProduction = '';
    }
 

    visitProducciones(node) {
        let produccionRenderizada = renderProduction(node, this);
        return `
        function fortranPEG_${node.id}() result(accept)
            logical :: accept
            integer :: i

            accept = .false.
            ${produccionRenderizada}
            accept = .true.
        end function fortranPEG_${node.id}
        `;
    }
    visitOpciones(node) {
        return node.exprs.map((expr) => expr.accept(this)).join('\n');
    }
    
    visitUnion(node) {
        const grupos = [];
        let grupoActual = [];
        let resultadoFinal = '';
        let resultadotmp = '';
        for (let i = 0; i < node.exprs.length; i++) {
            const expr = node.exprs[i];
            if (expr.expr instanceof n.String || expr.expr instanceof n.Corchetes || expr.expr instanceof n.Any) { // Si es instancia de String, Corchete o Any, se agrega al grupo
                grupoActual.push(expr);
            } else { // Si no, cerramos el grupo y comenzamos uno nuevo
                if (grupoActual.length > 0) {
                    grupos.push(grupoActual);
                    grupoActual = [];
                }
                resultadotmp += expr.accept(this) + "\n" // igual recorrer 
            }
        }
        if (grupoActual.length > 0) {
            grupos.push(grupoActual);
        }

        for (let grupo of grupos) {
            const resultadoGrupo = grupo.map((expr) => expr.accept(this)).join('\n');
            resultadoFinal += `
    concat_failed = .false.
    buffer = ""
    ${resultadoGrupo}

        `

        /* 

    Agregar para obtener el valor del lexema
    if (.not. concat_failed .and. len(buffer) > 0) then
        allocate( character(len=len(buffer)) :: lexeme)
        lexeme = buffer
        lexeme = lexeme // " -" // ${this.nameProduction}
        return
    end if
        */
        }
        return resultadoFinal + resultadotmp;
    }

    visitExpresion(node) {
        if ( node.qty && //there is a quantifier
            (node.expr instanceof n.String 
            || node.expr instanceof n.Corchetes
            || node.expr instanceof n.grupo)
        ){
            node.expr.qty = node.qty // inherit quantifier
        }
        return node.expr.accept(this);
    }

    visitString(node) {
       
        let var1= node.val.split("");
        let cambioNodeVal="";
        let cambioLength=0;
        if(var1.length==2 && var1[0]=="\\"){
            cambioLength=1;
            //var1 chanage to ascci number
            let transformedStr = node.val.replace(/\\(.)/, (match, p1) => {
                if (p1 === 'n') return '\n'; // Nueva línea (Line Feed)
                if (p1 === 't') return '\t'; // Tabulación horizontal (Horizontal Tab)
                if (p1 === 'r') return '\r'; // Retorno de carro (Carriage Return)
                if (p1 === 'f') return '\f'; // Avance de página (Form Feed)
                if (p1 === 'v') return '\v'; // Tabulación vertical (Vertical Tab)
                if (p1 === 'b') return '\b'; // Retroceso (Backspace)
                if (p1 === '0') return '\0'; // Nulo (Null character)
                // Agrega más casos según sea necesario
                return p1;
            });
            let ascii = transformedStr.charCodeAt(0);
            cambioNodeVal=`char(${ascii})`;
        }else{
            cambioNodeVal = `"${node.val}"`;
            cambioLength = node.val.length;
        }
        const condition = node.isCase 
        ? `tolower(${cambioNodeVal}) == tolower(input(cursor:cursor + ${ cambioLength - 1} ))`
        :  `${cambioNodeVal} == input(cursor:cursor + ${cambioLength - 1} )`;
        return renderQuantifierOption(node.qty, condition, cambioLength)
    }

    visitAny(node) { 
        return renderAny();
    }

    visitCorchetes(node) {
        node.exprs.forEach(expr => { expr.isCase = node.isCase });
        let conditions = "(" + node.exprs.map((expr) => expr.accept(this)).join(')& \n    .or. (') + ")"
        return renderQuantifierOption(node.qty, conditions, 1)
    }

    //Solo devuelve las condiciones a cumplirse
    visitrango(node) {
        return renderRango(node);
    }

    //Solo devuelve las condiciones a cumplirse
    visitliteralRango(node) {
        const literalMap = {
            "\\t": "char(9)",  // Tabulación
            "\\n": "char(10)", // Nueva línea
            " ": "char(32)",   // Espacio
            "\\r": "char(13)",  // Retorno de carro
        };
    
        // Verifica si el literal es especial y tradúcelo, de lo contrario usa comillas
        const literalFortran = literalMap[node.val] || `"${node.val}"`;
    
        const condition = node.isCase
        ? `tolower(input(cursor:cursor)) == tolower(${literalFortran})`
        : `input(cursor:cursor) == ${literalFortran}`
        return "(" + condition + ")";
    }

    visitidRel(node) {
        if (!this.calledRules.includes(node.val)) {
            this.calledRules.push(node.val);
            this.pendingRules.push(node.val);
        }
        return '';
    }

    visitgrupo(node) {
        node.expr.qty = node.qty
        return node.expr.accept(this);
    }

    visitfinCadena(node) {
        return '';
    }


}
