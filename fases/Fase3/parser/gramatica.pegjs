{{
    import { ids, usos} from '../index.js'
    import { ErrorReglas } from './error.js';
    import { errores } from '../index.js'
    import * as n from '../parser/visitor/CST.js';

    let indice = 0;
}}
 
gramatica
  = _ inicio:BloqueDeCodigo? _ prods:(producciones)+ _ {
    let duplicados = ids.filter((item, index) => ids.indexOf(item) !== index);

    if (duplicados.length > 0) {
        errores.push(new ErrorReglas("Regla duplicada: " + duplicados[0]));
    }

    let noEncontrados = usos.filter(item => !ids.includes(item));
    if (noEncontrados.length > 0) {
        errores.push(new ErrorReglas("Regla no encontrada: " + noEncontrados[0]));
    }
    prods[0].start = true;
    return {
      inicio: inicio,
      producciones: prods
    };
  }

producciones
  = _ id:identificador _ alias:$(literales)? _ "=" _ expr:opciones (_";")? {
    ids.push(id);
    return new n.Producciones(id, expr, alias);
  }

opciones
  = expr:union rest:(_ "/" _ @union)* {
    return new n.Opciones([expr, ...rest]);
  }

union
  = expr:expresion rest:(_ @expresion !(_ literales? _ "=") )* {
    return new n.Union([expr, ...rest]);
  }

expresion
  = label:$(etiqueta/varios)? _ expr:expresionSinCodigo _ qty:$([?+*]/conteo)? bloque:(BloqueDeCodigo)? {
    return new n.Expresion(expr, label, qty, bloque);
  }

expresionSinCodigo
  = id:identificador {
    usos.push(id);
    return new n.idRel(id);
  }
  / val:$literales isCase:"i"? {
    return new n.String(val.replace(/['"]/g, ''), isCase);
  }
  / "(" _ opciones:opciones _ ")"{
    return new n.grupo(opciones);
  }
  / exprs:corchetes isCase:"i"?{
    return new n.Corchetes(exprs, isCase);
  }
  / "." {
    return new n.Any(true);
  }
  / "!."{
    return new n.finCadena();
  }

etiqueta = ("@")? _ id:identificador _ ":" (varios)?

varios = ("!"(!".") /"$"/"@"/"&")

conteo = "|" _ (numero / id:identificador) _ "|"
        / "|" _ (numero / id:identificador)? _ ".." _ (numero / id2:identificador)? _ "|"
        / "|" _ (numero / id:identificador)? _ "," _ opciones _ "|"
        / "|" _ (numero / id:identificador)? _ ".." _ (numero / id2:identificador)? _ "," _ opciones _ "|"

corchetes
    = "[" contenido:(rango / contenido)+ "]" {
        return contenido;
    }

rango
    = inicio:$caracter "-" fin:$caracter {
        return new  n.rango(inicio, fin);
    }

caracter
    = [a-zA-Z0-9_ ] 

contenido
    = contenido: (corchete / @$texto){
        return new n.literalRango(contenido);
    }

corchete
    = "[" contenido "]"

texto
    = "\\" escape
    /[^\[\]]

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

numero = [0-9]+

identificador = [_a-z]i[_a-z0-9]i* { return text() }

_ "espacios"
  = ([ \t\n\r] / Comentarios)*

BloqueDeCodigo
  = "{" contenido:ContenidoCodigo "}" {
      indice += 1;
      return new n.BloqueDeCodigo(indice, contenido);
  }

ContenidoCodigo
  = $(ContenidoCodigoInterno)*

ContenidoCodigoInterno
  = !("{" / "}") (ComentarioCodigo / CaracterCodigo)
  / "{" ContenidoCodigoInterno* "}"

ComentarioCodigo
  = "!" [^\n]* "\n"

CaracterCodigo
  = [^\n] / "\n"

Comentarios = 
    "//" [^\n]* 
    / "/*" (!"*/" .)* "*/"