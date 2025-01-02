import { main } from '../Templates.js';
import FortranTranslator from './Translator.js';

export default function generateParser(cst) {
    const ruleReturnTypes = {};
    for (const rule of cst.rules) {
        rule.expr.exprs.forEach((concat, i) => {
            if (concat.action) {
                const functionId = `peg_SemanticAction_${rule.id}_f${i}`;
                ruleReturnTypes[functionId] = concat.action.returnType;
            }
        });
    }

    const translator = new FortranTranslator(ruleReturnTypes);
    return cst.accept(translator);
}

export function detectFortranType(line) {
    // Expresión regular para capturar el tipo y los modificadores
    const regex = /^\s*(\w+(\(\s*\w+\s*(?:=\w+)?\))?|type\(\s*\w+\s*\))\s*,?\s*(.*?)\s*(::\s*\w+)?$/i;

    // Intentar hacer coincidir la línea con la expresión regular
    const match = line.match(regex);

    if (match) {
        const baseType = match[1].trim();       // Tipo base (e.g., integer, type(node), etc.)
        const modifiers = match[3].trim();     // Modificadores (e.g., pointer, dimension, etc.)
        return modifiers ? modifiers : baseType; // Si hay modificadores, retornarlos; de lo contrario, el tipo base
    }

    return null; // Retornar null si no es válido
}

export function getActionId(ruleId, choice) {
    return `peg_SemanticAction_${ruleId}_f${choice}`;
}

export function getReturnType(functionId, actionReturnTypes) {
    return actionReturnTypes[functionId] ?? 'character(len=:), allocatable';
}

export function getExprId(choice, index) {
    return `expr_${choice}_${index}`; 
}

export function getArrayAsignation(choice, index) {
    return `values_${choice}_${index} = [values_${choice}_${index}, expr_${choice}_${index}]`; 
}

export function getRuleId(rule) {
    return `peg_Rule_${rule}`;
}

