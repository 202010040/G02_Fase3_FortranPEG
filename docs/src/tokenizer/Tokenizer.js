import Visitor from "../visitor/Visitor.js";
import { Clase, Rango } from "../visitor/CST.js";
import { generateCaracteres, KleeneCorchetes, PositivaCorchetes, TernariaCorchetes, TernariaLiterales, CondicionalCorchete } from "./utils.js";
import { CondicionalStrSencilla } from "./utils.js";
import { KleeneLiterales, PositivaLiterales } from "./utils.js";
import { gramatica } from "./utils.js";

export default class Tokenizer extends Visitor {
	
    visitProducciones(node) {
		//console.log(this);
        return node.expr.accept(this);
    }
	visitOpciones(node) {
		console.log('Opciones: ', node)
		return node.exprs.map(node => node.accept(this)).join('\n');
	}
	visitUnion(node) {
		//console.log('Expresion: ', node)
		return node.exprs.map(node => node.accept(this)).join('\n');
	}
	visitExpresion(node) {
		//console.log('Expresion: ', node)
		// Validacion de Str sencillos
		if (node.expr instanceof String){
			switch (node.qty) {
			case "*":
				return KleeneLiterales(node.expr);
			case "+":
				return PositivaLiterales(node.expr);
			case "?":
				return TernariaLiterales(node.expr);
			default:
				return node.expr.accept(this);
			}	
		}
		if (node.expr instanceof Clase){
			switch (node.qty) {
			case "*":
				return KleeneCorchetes(node.expr);
			case "+":
				return PositivaCorchetes(node.expr);
			case "?":
				return TernariaCorchetes(node.expr);
			default:
				return node.expr.accept(this);
			}	
		}
		
		return node.expr.accept(this);

	}

    visitString(node) {
        let condicional = CondicionalStrSencilla(node);

        return `
        if ( ${condicional} ) then
            allocate(character(len=${node.val.length}) :: lexeme)
            lexeme = input(cursor:cursor + ${node.val.length - 1})
            cursor = cursor + ${node.val.length}
            return
        end if
        `;
    }

    visitClase(node) {
        return `
        i = cursor
        ${generateCaracteres(
            node.chars.filter((node) => typeof node === "string")
        )}
        ${
            node.chars
                .filter((node) => node instanceof Rango)
                .map((range) => range.accept(this))
                .join("\n")
        }
        `;
    }

    visitRango(node) {
        return `
        if (input(i:i) >= "${node.rangoInicial}" .and. input(i:i) <= "${node.rangoFinal}") then
            lexeme = input(cursor:i)
            cursor = i + 1
            return
        end if
        `;
    }


	visitReferencia(node) {
		// Asume que la producción referenciada será visitada y procesada
		//console.log(gramatica)
		const produccionReferenciada = gramatica.find(prod => prod.id === node.id);
		if (!produccionReferenciada) {
			console.error(`Referencia no resuelta: ${node.id}`);
			return `
			print *, "error: referencia no resuelta a '${node.id}' en col ", cursor
			lexeme = "ERROR"
			`;
		}
		//console.log(produccionReferenciada.expr.exprs[0])
		this.visitOpciones(produccionReferenciada.expr);
	}
}