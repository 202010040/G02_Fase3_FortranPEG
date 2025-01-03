{{
    
    let parentesis_id = 0

    // import { identificadores } from '../index.js'

    import { ids, usos, reglas_ficticias} from '../index.js' 
    import { ErrorReglas } from './error.js';
    import { errores } from '../index.js'

    import * as n from '../visitor/CST.js';
}}

gramatica
  = _ code:globalCode? prods:regla+ _ {
    let duplicados = ids.filter((item, index) => ids.indexOf(item) !== index);
    if (duplicados.length > 0) {
        errores.push(new ErrorReglas("Regla duplicada: " + duplicados[0]));
    }

    // Validar que todos los usos están en ids
    let noEncontrados = usos.filter(item => !ids.includes(item));
    if (noEncontrados.length > 0) {
        errores.push(new ErrorReglas("Regla no encontrada: " + noEncontrados[0]));
    }
    prods[0].start = true;
    
    // Añadir las reglas ficticias al conjunto de reglas
    const todas_las_reglas = [...prods, ...reglas_ficticias];
    
    return new n.Grammar(todas_las_reglas, code);
  }
  

globalCode
  = "{" before:$(. !"contains")* [ \t\n\r]* "contains" [ \t\n\r]* after:$[^}]* "}" {
    return after ? {before, after} : {before}
  }

regla
  = _ id:identificador _ alias:(literales)? _ "=" _ expr:opciones (_";")? {
    ids.push(id);
    return new n.Regla(id, expr, alias);
  }

opciones
  = expr:union rest:(_ "/" _ @union)* {
    return new n.Opciones([expr, ...rest]);
  }

union
  = expr:parsingExpression rest:(_ @parsingExpression !(_ literales? _ "=") )* action:(_ @predicate)? {
    const exprs = [expr, ...rest];
    const labeledExprs = exprs
        .filter((expr) => expr instanceof n.Pluck)
        .filter((expr) => expr.labeledExpr.label);
    if (labeledExprs.length > 0 && action) {  // Verificamos que action existe
        action.params = labeledExprs.reduce((args, labeled) => {
            const expr = labeled.labeledExpr.annotatedExpr.expr;
            args[labeled.labeledExpr.label] = {
                name: expr instanceof n.Identificador ? expr.id : '',
                isArray: expr instanceof n.Identificador
            };
            return args;
        }, {});
    }
    return new n.Union(exprs, action);
  }

parsingExpression
  = pluck
  / '!' assertion:(match/predicate) {
    return new n.NegAssertion(assertion);
  }
  / '&' assertion:(match/predicate) {
    return new n.Assertion(assertion);
  }
  / "!." {
    return new n.Fin();
  }

pluck
  = pluck:"@"? _ expr:label {
    return new n.Pluck(expr, pluck ? true : false);
  }

label
  = label:(@identificador _ ":")? _ expr:annotated {
    return new n.Label(expr, label);
  }

annotated
  = text:"$"? _ expr:match _ qty:([?+*]/conteo)? {
    return new n.Annotated(expr, qty, text ? true : false);
  }

match
  = id:identificador {
    usos.push(id)
    return new n.Identificador(id);
  }
  / val:$literales isCase:"i"? {
    return new n.String(val.replace(/['"]/g, ''), isCase ? true : false);
  }
  / "(" _ regla_parentesis:opciones _ ")" {
    parentesis_id += 1;
    let id_temporal = `peg_parentesis_${parentesis_id}`
    ids.push(id_temporal);
    
    // Crear la regla ficticia y almacenarla
    const regla_ficticia = new n.Regla(id_temporal, regla_parentesis, '');
    reglas_ficticias.push(regla_ficticia);
    
    return new n.Identificador(id_temporal);
  }
  / chars:clase isCase:"i"? {
    return new n.Clase(chars, isCase ? true : false);
  }
  / "." {
    return new n.Punto();
  }

conteo
  = "|" _ (numero / id:identificador / predicate) _ "|"
  / "|" _ (numero / id:identificador / predicate)? _ ".." _ (numero / id2:identificador / predicate)? _ "|"
  / "|" _ (numero / id:identificador / predicate)? _ "," _ opciones _ "|"
  / "|" _ (numero / id:identificador / predicate)? _ ".." _ (numero / id2:identificador / predicate)? _ "," _ opciones _ "|"

predicate
  = "{" [ \t\n\r]* returnType:predicateReturnType code:$[^}]* "}" {
    return new n.Predicate(returnType.type, returnType.isArray, code, {})
  }

predicateReturnType
  = t:$(. !"::")+ [ \t\n\r]* "::" [ \t\n\r]* "res" {
    return {
        type: t.trim(),
        isArray: false // Este valor será actualizado en Union basado en el tipo de match
    };
  }
  
clase
  = "[" @contenidoClase+ "]"

contenidoClase
  = bottom:$caracter "-" top:$caracter {
    return new n.Rango(bottom, top);
  }
  / $caracter

caracter
  = [^\[\]\\]
  / "\\" .

literales
  = '"' @stringDobleComilla* '"'
  / "'" @stringSimpleComilla* "'"

stringDobleComilla = !('"' / "\\" / finLinea) .
                    / "\\" escape
                    / continuacionLinea

stringSimpleComilla = !("'" / "\\" / finLinea) .
                    / "\\" escape
                    / continuacionLinea

continuacionLinea = "\\" secuenciaFinLinea

finLinea = [\n\r\u2028\u2029]

escape = "'"
        / '"'
        / "\\"
        / "b"
        / "f"
        / "n"
        / "r"
        / "t"
        / "v"
        / "u"

secuenciaFinLinea = "\r\n" / "\n" / "\r" / "\u2028" / "\u2029"

// literales = 
//     "\"" [^"]* "\""
//     / "'" [^']* "'"
    
numero = [0-9]+

identificador = [_a-z]i[_a-z0-9]i* { return text() }

_ = (Comentarios /[ \t\n\r])*

Comentarios = 
    "//" [^\n]* 
    / "/*" (!"*/" .)* "*/"
