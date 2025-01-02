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

    constructor(returnTypes) {
        this.actionReturnTypes = returnTypes;
        this.actions = [];
        this.translatingStart = false;
        this.currentRule = '';
        this.currentChoice = 0;
        this.currentExpr = 0;
        this.hasQuantifiedNonTerminal = false;
        this.validacionesIFRegla=[];
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

    visitRegla(node) {
        
        this.currentRule = node.id;
        this.currentChoice = 0;
        this.hasQuantifiedNonTerminal = false; // Reset en cada regla
    
        if (node.start) this.translatingStart = true;
    
        // Función auxiliar para detectar si una expresión tiene cuantificador
        const hasQuantifier = (expr) => {
            //console.log("Tipor de Regla", expr.annotatedExpr?.expr, )
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
                            declarations.push(`${baseType}, allocatable :: values_${i}_${j}(:)`);
                            allocateInstructions.push(`allocate(values_${i}_${j}(0))`);
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


    visitOpciones(node) {
        console.log('Opcion: ', node);
                
        return Template.election({
            exprs: node.exprs.map((expr) => {
                const translation = expr.accept(this);
                this.currentChoice++;
                return translation;
            }),
            sizeValidators: this.validacionesIFRegla
        }, this.hasQuantifiedNonTerminal);
    }

    visitUnion(node) {
        const matchExprs = node.exprs.filter(
            (expr) => expr instanceof CST.Pluck
        );
        const exprVars = matchExprs.map(
            (_, i) => `${ this.hasQuantifiedNonTerminal ? `values_${this.currentChoice}_${i}` : `expr_${this.currentChoice}_${i}`} `
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
                ([label, ruleId]) =>
                    `${getReturnType(
                        getActionId(ruleId, this.currentChoice),
                        this.actionReturnTypes
                    )} ${ this.hasQuantifiedNonTerminal ? ', dimension(:), intent(in)' : '' } :: ${label}`
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
        const set = node.chars
            .filter((char) => typeof char === 'string')
            .map((char) => `'${char}'`);
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
}
