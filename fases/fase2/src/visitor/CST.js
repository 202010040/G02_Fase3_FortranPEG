
// Auto-generated
import Node from './Node.js';

export class Producciones extends Node {
    constructor(id, expr, alias) {
        super();
        this.id = id;
		this.expr = expr;
		this.alias = alias;
    }

    accept(visitor) {
        return visitor.visitProducciones(this);
    }
}
    
export class Opciones extends Node {
    constructor(exprs) {
        super();
        this.exprs = exprs;
    }

    accept(visitor) {
        return visitor.visitOpciones(this);
    }
}
    
export class Union extends Node {
    constructor(exprs) {
        super();
        this.exprs = exprs;
    }

    accept(visitor) {
        return visitor.visitUnion(this);
    }
}
    
export class Expresion extends Node {
    constructor(expr, label, qty) {
        super();
        this.expr = expr;
		this.label = label;
		this.qty = qty;
    }

    accept(visitor) {
        return visitor.visitExpresion(this);
    }
}
    
export class String extends Node {
    constructor(val, isCase) {
        super();
        this.val = val;
		this.isCase = isCase;
    }

    accept(visitor) {
        return visitor.visitString(this);
    }
}
    
export class Clase extends Node {
    constructor(chars, isCase) {
        super();
        this.chars = chars;
		this.isCase = isCase;
    }

    accept(visitor) {
        return visitor.visitClase(this);
    }
}
    
export class Rango extends Node {
    constructor(rangoInicial, rangoFinal) {
        super();
        this.rangoInicial = rangoInicial;
		this.rangoFinal = rangoFinal;
    }

    accept(visitor) {
        return visitor.visitRango(this);
    }
}
    
export class Identificador extends Node {
    constructor(id) {
        super();
        this.id = id;
    }

    accept(visitor) {
        return visitor.visitIdentificador(this);
    }
}
    
export class Punto extends Node {
    constructor() {
        super();
        
    }

    accept(visitor) {
        return visitor.visitPunto(this);
    }
}
    
export class Fin extends Node {
    constructor() {
        super();
        
    }

    accept(visitor) {
        return visitor.visitFin(this);
    }
}
    
export class Etiqueta extends Node {
    constructor(id, varios) {
        super();
        this.id = id;
		this.varios = varios;
    }

    accept(visitor) {
        return visitor.visitEtiqueta(this);
    }
}
    
export class Varios extends Node {
    constructor(symbol) {
        super();
        this.symbol = symbol;
    }

    accept(visitor) {
        return visitor.visitVarios(this);
    }
}
    
export class Conteo extends Node {
    constructor(min, max, opciones) {
        super();
        this.min = min;
		this.max = max;
		this.opciones = opciones;
    }

    accept(visitor) {
        return visitor.visitConteo(this);
    }
}
    
export class ContenidoClase extends Node {
    constructor(rangoInicial, rangoFinal) {
        super();
        this.rangoInicial = rangoInicial;
		this.rangoFinal = rangoFinal;
    }

    accept(visitor) {
        return visitor.visitContenidoClase(this);
    }
}
    
export class Literal extends Node {
    constructor(value) {
        super();
        this.value = value;
    }

    accept(visitor) {
        return visitor.visitLiteral(this);
    }
}
    
export class Caracter extends Node {
    constructor(char) {
        super();
        this.char = char;
    }

    accept(visitor) {
        return visitor.visitCaracter(this);
    }
}
    
export class Referencia extends Node {
    constructor(id) {
        super();
        this.id = id;
    }

    accept(visitor) {
        return visitor.visitReferencia(this);
    }
}
    