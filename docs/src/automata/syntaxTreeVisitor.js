import Visitor from "../visitor/Visitor";
import * as Syntax from './syntaxTree.js'
import * as CST from '../visitor/CST.js'

export default class SintaxTreeVisitor extends Visitor{
    visitProducciones(node) {
        return node.expr.accept(this);
    }
	visitOpciones(node) {
        return node.exprs.map(node => node.accept(this));
    }
	visitUnion(node) {
        const grupos = []
        for (const expr in node.exprs){
            const result = expr.accept(this);
            if (result){ // Si no retorna nulo...
                
            }
        }
    }
	visitExpresion(node) {
        switch (node.qty) {
            case '*':
                return new Syntax.Kleene(node.expr.accept(this)); // Cerradura de kleene
            case '+':
                return new Syntax.Positiva(node.expr.accept(this)); // Cerradura Positiva
            case '?':
                return new Syntax.Kleene(node.expr.accept(this)); // Puede venir cero o uno
            default:
                return node.expr.accept(this);
        }
    }
	visitString(node) {
        return new Syntax.Hoja(node.val);
    }
	visitClase(node) {
        return new Syntax.Hoja(node.chars.join('')); // TO DO: Validar Case Insensitive
    }
	visitRango(node) {
        return `${node.rangoInicial}-${node.rangoFinal}`;
    }
	visitIdentificador(node) {
        return;
    }
	visitPunto(node) {
        return;
    }
	visitFin(node) {
        return;
    }
}