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
        const matchExprs = node.exprs.filter(
            (expr) => expr instanceof CST.Pluck
        );
        const exprVars = matchExprs.map( 
            (_, i) => `${ (this.hasQuantifiedNonTerminal && !(_.labeledExpr.annotatedExpr.expr instanceof CST.Clase)) ? `values_${this.currentChoice}_${i}` : `expr_${this.currentChoice}_${i}`} `
        );

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
        if (node.qty && typeof node.qty === 'string') {
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
                quantifier: node.qty,
                expr: node.expr.accept(this),
                destination: getExprId(this.currentChoice, this.currentExpr),
            });
        } else if (node.qty) {
            throw new Error('Repetitions not implemented.');
        } else {
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
    }
 
    visitAssertion(node) {
        throw new Error('Method not implemented.');
    }

    visitNegAssertion(node) {
        throw new Error('Method not implemented.');
    }

    visitPredicate(node) {
        return Template.action({
            ruleId: this.currentRule,
            choice: this.currentChoice,
            signature: Object.keys(node.params),
            returnType: node.returnType,
            paramDeclarations: Object.entries(node.params).map(
                ([label, paramInfo]) => {
                    console.log(this.hasQuantifiedNonTerminal, paramInfo.isArray)
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
        // Detecta si la aserción tiene una acción semántica
        const hasAction = !!node.action;
    
        if (hasAction) {
            const actionFn = getActionId(this.currentRule, this.currentChoice);
            return `posAssertionWithAction(${node.expr.accept(this)}, ${actionFn})`;
        } else {
            return `posAssertion(${node.expr.accept(this)})`;
        }
    }
    
    visitNegAssertion(node) {
        // Detecta si la aserción negativa tiene una acción semántica
        const hasAction = !!node.action;
    
        if (hasAction) {
            const actionFn = getActionId(this.currentRule, this.currentChoice);
            return `negAssertionWithAction(${node.expr.accept(this)}, ${actionFn})`;
        } else {
            return `negAssertion(${node.expr.accept(this)})`;
        }
    }
    
}
