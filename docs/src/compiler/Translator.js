// Translator 2

import * as CST from '../visitor/CST.js';
import * as Template from '../Templates.js';
import { getActionId, getReturnType, getExprId, getRuleId, getArrayAsignation, detectFortranType } from './utils.js';

export default class FortranTranslator {

    actionReturnTypes;
    actions;
    translatingStart;
    currentRule;
    currentChoice;
    currentExpr;
    hasQuantifiedNonTerminal; 
    validacionesIFRegla;
    currentRes;
    validacionesValues;

    constructor(returnTypes) {
        this.actionReturnTypes = returnTypes;
        this.actions = [];
        this.translatingStart = false;
        this.currentRule = '';
        this.currentChoice = 0;
        this.currentExpr = 0;
        this.hasQuantifiedNonTerminal = false;
        this.validacionesIFRegla=[];
        this.currentRes = '';
        this.validacionesValues = []
    }

    visitGrammar(node) {
        const rules = node.rules.map((rule) => rule.accept(this));

        return Template.main({
            beforeContains: node.globalCode?.before ?? '',
            afterContains: node.globalCode?.after ?? '',
            startingRuleId: getRuleId(node.rules[0].id),
            startingRuleType: getReturnType(
                getActionId(node.rules[0].id, 0),
                this.actionReturnTypes
            ),
            actions: this.actions,
            rules,
        });
    }

    contienePalabra(cadena, palabra) {
        // Convertir ambas cadenas a minúsculas para una búsqueda insensible a mayúsculas y minúsculas
        const cadenaMinuscula = cadena.toLowerCase();
        const palabraMinuscula = palabra.toLowerCase();
        // Verificar si la palabra está incluida en la cadena
        return cadenaMinuscula.includes(palabraMinuscula);
    }
    
    
    visitRegla(node) {
        
        this.currentRule = node.id;
        this.currentChoice = 0;
        this.hasQuantifiedNonTerminal = false; // Reset en cada regla
    
        if (node.start) this.translatingStart = true;
    
        // Función auxiliar para detectar si una expresión tiene cuantificador
        const hasQuantifier = (expr) => {
            return ((expr.annotatedExpr?.qty === "+" || expr.annotatedExpr?.qty === "*") && (expr.annotatedExpr?.expr instanceof CST.Identificador));
        };
    
        const allocateInstructions = [];
        const deallocateInstructions = [];
        this.validacionesIFRegla=[]
    
        const ruleTranslation = Template.rule({
            id: node.id,
            returnType: getReturnType(
                getActionId(node.id, this.currentChoice),
                this.actionReturnTypes
            ),
            exprDeclarations: node.expr.exprs.flatMap((election, i) =>
                election.exprs
                    .filter((expr) => expr instanceof CST.Pluck)
                    .flatMap((label, j) => {
                        const expr = label.labeledExpr.annotatedExpr.expr;
                        const baseType = expr instanceof CST.Identificador
                            ? getReturnType(
                                  getActionId(expr.id, i),
                                  this.actionReturnTypes
                              )
                            : 'character(len=:), allocatable';
                        
                        const declarations = [];

                        declarations.push(`${baseType} :: expr_${i}_${j}`);
                        //console.log(`----------- ${i}_${j}-----------------`)
                        //console.log('Label', label, hasQuantifier(label.labeledExpr))
                        if (hasQuantifier(label.labeledExpr)) { 
                            declarations.push(`${baseType} ${ this.contienePalabra(baseType, 'allocatable') ? '' : `, allocatable`} :: values_${i}_${j}(:)`); // Valida si ya es una cadena
                            allocateInstructions.push(` ${ this.contienePalabra(baseType, 'allocatable') ? `allocate(values_${i}_${j}(0), source="" )` : `allocate(values_${i}_${j}(0))`}   `);
                            deallocateInstructions.push(`if (allocated(values_${i}_${j})) deallocate(values_${i}_${j}) `);
                            this.validacionesIFRegla.push([i,j])
                            if (expr instanceof CST.Identificador) {
                                this.hasQuantifiedNonTerminal = true; // Activar la variable global
                            }
                        }
                        
                        return declarations;
                    })
            ),
            deallocateStmts: deallocateInstructions,
            allocateStmts: allocateInstructions,
            expr: node.expr.accept(this),
        });
        
        console.log(node.expr)
        this.translatingStart = false;
    
        return ruleTranslation;
    }

    getCurrentRuleReturnType() {
        return getReturnType(
            getActionId(this.currentRule, this.currentChoice),
            this.actionReturnTypes
        );
    }

        retornoDefault(tipo){
        switch (tipo) {
            case 'integer':
                return 'res = -999'
            case 'character(len=:), allocatable':
                return `res = ""`;
            case 'logical':
                return 'res = .false.'
            case 'type(node), pointer':
                return 'res => null()'
            default:
                return 'call pegError()';
        }
    }

    visitOpciones(node) {
        let tipoRetorno = this.getCurrentRuleReturnType();
        let retDefault = this.retornoDefault(tipoRetorno)

        return Template.election({
            exprs: node.exprs.map((expr) => {
                const translation = expr.accept(this);
                this.currentChoice++;
                return translation;
            }),
            sizeValidators: this.validacionesIFRegla,
            responseDefault: this.currentRes,
            rDefault: retDefault,
        }, this.hasQuantifiedNonTerminal);
    }


    visitUnion(node) {
        this.validacionesValues = []
        const matchExprs = node.exprs.filter(
            (expr) => expr instanceof CST.Pluck
        );
        const exprVars = [];

        matchExprs.forEach((_, i) => {
        
            const isQuantifiedNonTerminal = this.hasQuantifiedNonTerminal;
            const isNotClaseInstance = !(_.labeledExpr.annotatedExpr.expr instanceof CST.Clase);
            const hasValidQuantifier = _.labeledExpr.annotatedExpr?.qty === "+" || _.labeledExpr.annotatedExpr?.qty === "*";
            this.validacionesValues.push((this.currentChoice, i))
            const expr = `${
                (isQuantifiedNonTerminal && isNotClaseInstance && hasValidQuantifier) 
                    ? `values_${this.currentChoice}_${i}` 
                    : `expr_${this.currentChoice}_${i}`
            }`;
        
            exprVars.push(expr);
        });
        

        let neededExprs;
        let resultExpr;
        let tipoRetorno = node.action?.returnType ?? '' ;
        const currFnId = getActionId(this.currentRule, this.currentChoice);
        if (currFnId in this.actionReturnTypes) {
            neededExprs = exprVars.filter(
                (_, i) => matchExprs[i].labeledExpr.label
            );
            resultExpr = Template.fnResultExpr({
                fnId: getActionId(this.currentRule, this.currentChoice),
                exprs: neededExprs.length > 0 ? neededExprs : [],
                tipo: tipoRetorno
            });
        } else {
            neededExprs = exprVars.filter((_, i) => matchExprs[i].pluck);
            resultExpr = Template.strResultExpr({
                exprs: neededExprs.length > 0 ? neededExprs : exprVars,
            });
        }
        this.currentExpr = 0;

        if (node.action) this.actions.push(node.action.accept(this));

        this.currentRes = resultExpr; 

        return Template.union({
            exprs: node.exprs.map((expr) => {
                const translation = expr.accept(this);
                if (expr instanceof CST.Pluck) this.currentExpr++;
                return translation;
            }),
            startingRule: this.translatingStart,
            resultExpr,
        });
    }

    visitPluck(node) {
        return node.labeledExpr.accept(this);
    }

    visitLabel(node) {
        return node.annotatedExpr.accept(this);
    }

    visitAnnotated(node) {
        // Verificación de seguridad
        if (!node || !node.expr) {
            console.error('Node or node.expr is undefined:', node);
            return '';
        }
    
        // Manejo de repeticiones específicas
        if (node.qty) {
            // Para repeticiones como |3| o |3, ', '|
            if (typeof node.qty === 'object') {
                // Verificación de seguridad para el separador
                const count = node.qty.count || 1;
                let separator = null;
                
                // Verificar si hay un separador y si tiene el método accept
                if (node.qty.separator && typeof node.qty.separator.accept === 'function') {
                    separator = node.qty.separator.accept(this);
                }
    
                return Template.strExpr({
                    quantifier: {
                        count: count,
                        separator: separator
                    },
                    expr: node.expr.accept(this),
                    destination: getExprId(this.currentChoice, this.currentExpr),
                });
            }
            
            // Para cuantificadores simples (*, +, ?)
            if (typeof node.qty === 'string') {
                if (node.expr instanceof CST.Identificador) {
                    if (this.hasQuantifiedNonTerminal  && node.qty != null) {
                        return `${getExprId(
                            this.currentChoice,
                            this.currentExpr
                        )} = ${node.expr.accept(this)} 
                        ${getArrayAsignation(
                            this.currentChoice,
                            this.currentExpr
                        )} 
                        savePoint = cursor`;
                    }
                    return `${getExprId(
                        this.currentChoice,
                        this.currentExpr
                    )} = ${node.expr.accept(this)}`;
                }
                return Template.strExpr({
                    quantifier: node.qty,
                    expr: node.expr.accept(this),
                    destination: getExprId(this.currentChoice, this.currentExpr),
                });
            }
        }
    
        // Expresiones sin cuantificador
        if (node.expr instanceof CST.Identificador) {
            if (this.hasQuantifiedNonTerminal) {
                return `${getExprId(
                    this.currentChoice,
                    this.currentExpr
                )} = ${node.expr.accept(this)} 
                ${getArrayAsignation(
                    this.currentChoice,
                    this.currentExpr
                )} 
                savePoint = cursor`;
            }
            return `${getExprId(
                this.currentChoice,
                this.currentExpr
            )} = ${node.expr.accept(this)}`;
        }
    
        return Template.strExpr({
            expr: node.expr.accept(this),
            destination: getExprId(this.currentChoice, this.currentExpr),
        });
    }
 

    visitPredicate(node) {
        return Template.action({
            ruleId: this.currentRule,
            choice: this.currentChoice,
            signature: Object.keys(node.params),
            returnType: node.returnType,
            paramDeclarations: Object.entries(node.params).map(
                ([label, paramInfo]) => {
                    const needsArrayParams = this.hasQuantifiedNonTerminal && paramInfo.isArray;
                    return `${getReturnType(
                        getActionId(paramInfo.name, this.currentChoice),
                        this.actionReturnTypes
                    )} ${needsArrayParams ? ', dimension(:), intent(in)' : ''} :: ${label}`;
                }
            ),
            code: node.code,
        });
    }

    visitString(node) {
        const literalMap = {
            "\\t": "char(9)",
            "\\n": "char(10)",
            " ": "char(32)",
            "\\r": "char(13)"
        };
        const literalFortran = literalMap[node.val] || `"${node.val}"`;
        
        return `acceptString(${literalFortran})`; 
    }

    visitClase(node) {
        let characterClass = [];
        const literalMap = {
            "\\t": "char(9)",
            "\\n": "char(10)",
            " ": "char(32)",
            "\\r": "char(13)"
        };
        const set = node.chars
            .filter((char) => typeof char === 'string')
            .map((char) => literalMap[char] || `'${char}'`);
        const ranges = node.chars
            .filter((char) => char instanceof CST.Rango)
            .map((range) => range.accept(this));
        if (set.length !== 0) {
            characterClass = [`acceptSet([${set.join(',')}])`];
        }
        if (ranges.length !== 0) {
            characterClass = [...characterClass, ...ranges];
        }
        return `(${characterClass.join(' .or. ')})`;
    }

    visitRango(node) {
        return `acceptRange('${node.bottom}', '${node.top}')`;
    }

    visitIdentificador(node) {
        return getRuleId(node.id) + '()';
    }

    visitPunto(node) {
        return 'acceptPeriod()';
    }

    visitFin(node) {
        return 'if (.not. acceptEOF()) cycle';
    }


    visitAssertion(node) {
        console.log('=== visitAssertion inicio ===');
        console.log('Nodo recibido:', JSON.stringify(node, null, 2));
    
        try {
            // Si el nodo es undefined o null
            if (!node) {
                return '';
            }
    
            // Caso 1: Nodo simple con val
            if (typeof node === 'object' && node.val !== undefined) {
                return ` if (.not. assertion("${node.val}") ) then\n    call pegError()\nend if `;
            }
    
            // Caso 2: Nodo con assertion anidado
            if (node.assertion) {
                if (typeof node.assertion === 'object' && node.assertion.val !== undefined) {
                    return ` if (.not. assertion("${node.assertion.val}") ) then\n    call pegError()\nend if`;
                }
                if (typeof node.assertion.accept === 'function') {
                    return ` if (.not. assertion(${node.assertion.accept(this)}) ) then\n    call pegError()\nend if`;
                }
            }
    
            // Caso 3: Nodo con expr
            if (node.expr && typeof node.expr.accept === 'function') {
                return ` if (.not. assertion(${node.expr.accept(this)}) ) then\n    call pegError()\nend if`;
            }
    
            console.error('Estructura no reconocida en visitAssertion:', node);
            return ' if (.not. assertion("") ) then\n    call pegError()\nend if';
    
        } catch (error) {
            console.error('Error en visitAssertion:', error);
            console.error('Nodo problemático:', node);
            return ' if (.not. assertion("") ) then\n    call pegError()\nend if';
        }
    }
    
    

    visitNegAssertion(node) {
        console.log('=== visitNegAssertion inicio ===');
        console.log('Nodo recibido:', JSON.stringify(node, null, 2));
    
        try {
            // Validación básica
            if (!node) {
                throw new Error('Nodo indefinido');
            }
    
            // Caso 1: Aserción simple con valor directo
            if (typeof node === 'string') {
                return `if (.not. negAssertion("${node}")) then\n    call pegError()\nend if`;
            }
    
            // Caso 2: Aserción con propiedad 'val'
            if (node.assertion?.val !== undefined) {
                const assertion = `negAssertion("${node.assertion.val}")`;
                return `if (.not. ${assertion}) then\n    call pegError()\nend if`;
            }
    
            // Caso 3: Aserción con método accept
            if (node.assertion && typeof node.assertion.accept === 'function') {
                const assertion = `negAssertion(${node.assertion.accept(this)})`;
                return `if (.not. ${assertion}) then\n    call pegError()\nend if`;
            }
    
            // Caso 4: Expresión con método accept
            if (node.expr && typeof node.expr.accept === 'function') {
                const assertion = `negAssertion(${node.expr.accept(this)})`;
                return `if (.not. ${assertion}) then\n    call pegError()\nend if`;
            }
    
            // Si llegamos aquí, la estructura no es válida
            console.error('Estructura del nodo no reconocida:', node);
            return 'if (.not. negAssertion("")) then\n    call pegError()\nend if';
    
        } catch (error) {
            console.error('Error en visitNegAssertion:', error);
            console.error('Nodo problemático:', node);
            return 'if (.not. negAssertion("")) then\n    call pegError()\nend if';
        }
    }
    
    
    
}
