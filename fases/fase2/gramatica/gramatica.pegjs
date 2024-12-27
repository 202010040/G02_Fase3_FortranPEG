{{
    import { ids, usos } from '../index.js'
    import { ErrorReglas } from './error.js';
    import { errores } from '../index.js';
    import * as n from '../visitor/CST.js';
}} 

gramatica = _ prods:producciones+ _ {

    let duplicados = ids.filter((item, index) => ids.indexOf(item) !== index);
    if (duplicados.length > 0) {
        errores.push(new ErrorReglas("Regla duplicada: " + duplicados[0]));
    }

    // Validar que todos los usos están en ids
    let noEncontrados = usos.filter(item => !ids.includes(item));
    if (noEncontrados.length > 0) {
        errores.push(new ErrorReglas("Regla no encontrada: " + noEncontrados[0]));
    }

    // Validacion de reglas huerfanas
    let huerfanos = [];

    let usoCounts = usos.reduce((countMap, uso) => {
        countMap[uso] = (countMap[uso] || 0) + 1;
        return countMap;
    }, {});

    ids.slice(1).forEach(id => {
        if (usoCounts[id] === 1) {
            huerfanos.push(id);
        }
    });

    if (huerfanos.length > 0) {
        errores.push(new ErrorReglas("Una o mas reglas huerfanas encontradas: " + huerfanos.join(', ')));
    }

    return prods;
}

producciones = _ id:identificador _ alias:(literales)? _ "=" _ expr:opciones (_";")? { 
    ids.push(id);
    return new n.Producciones(id, expr, alias);
}

// Producción principal que reconoce opciones dentro de paréntesis y operadores
opciones = expr:union rest:(_ "/" _ @union)* {
    return new n.Opciones([expr, ...rest]); // Crea un arreglo con las expresiones
}

union = expr:expresion rest:(_ @expresion !(_ literales? _ "="))* {
    return new n.Union([expr, ...rest]); // Une varias expresiones
}

expresion = label:$(etiqueta/varios)? _ expr:expresiones _ qty:$([?+*]/conteo)? {
    return new n.Expresion(expr, label, qty); // Asocia una cantidad o repetición a la expresión
}

etiqueta = ("@")? _ id:identificador _ ":" (varios)? // Manejo de etiquetas
varios = ("!"/"$"/"@"/"&") // Reconoce símbolos adicionales en etiquetas

// Producción de expresiones
expresiones =
    id:identificador {
        usos.push(id); 
        return new n.Referencia(id); // Referencia a otro identificador
    }
    / valor:$literales isCase:"i"? {
        return new n.String(String(valor).replace(/['"]/g, ''), isCase); // Quita comillas y valida case insensitive
    }
    / "(" _ op:opciones _ ")" qty:$([?+*]/conteo)? {
        // Maneja expresiones entre paréntesis y aplica operadores
        return new n.Expresion(op, null, qty);
    }
    / chars:clase isCase:"i"? {
        return new n.Clase(chars, isCase); // Clase de caracteres
    }
    / "." { 
        
    }
    / "!." { 
        
    }

conteo = "|" _ (numero / id:identificador) _ "|"
        / "|" _ (numero / id:identificador)? _ ".." _ (numero / id2:identificador)? _ "|"
        / "|" _ (numero / id:identificador)? _ "," _ opciones _ "|"
        / "|" _ (numero / id:identificador)? _ ".." _ (numero / id2:identificador)? _ "," _ opciones _ "|"

clase
  = "[" @contenidoClase+ "]"

contenidoClase
  = rangoInicial:$caracter "-" rangoFinal:$caracter {
    return new n.Rango(rangoInicial, rangoFinal);
  }
  / $caracter

caracter
  = [^\[\]\\]
  / "\\" .

//caracter
//    = [a-zA-Z0-9_ ] { return text()}

contenido
    = (corchete / texto)+

corchete
    = "[" contenido "]"

texto
    = [^\[\]]+

literales = '"' @stringDobleComilla* '"'
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

numero = [0-9]+

identificador = [_a-z]i[_a-z0-9]i* { return text() }

_ = (Comentarios /[ \t\n\r])*

Comentarios = 
    "//" [^\n]* 
    / "/*" (!"*/" .)* "*/"

