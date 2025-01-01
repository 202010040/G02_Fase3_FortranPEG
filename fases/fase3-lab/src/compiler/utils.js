import FortranTranslator from './Translator.js';

export default function generateParser(cst) {
    const ruleReturnTypes = {};
    for (const rule of cst.rules) {
        rule.expr.exprs.forEach((concat, i) => {
            if (concat.action) {
                const functionId = `peg_${rule.id}_f${i}`;
                ruleReturnTypes[functionId] = concat.action.returnType;
            }
        });
    }

    const translator = new FortranTranslator(ruleReturnTypes);
    return cst.accept(translator);
}

export function getActionId(ruleId, choice) {
    return `peg_${ruleId}_f${choice}`;
}

export function getReturnType(functionId, actionReturnTypes) {
    return actionReturnTypes[functionId] ?? 'character(len=:), allocatable';
}

export function getExprId(choice, index) {
    return `expr_${choice}_${index}`;
}

export function getRuleId(rule) {
    return `peg_${rule}`;
}
