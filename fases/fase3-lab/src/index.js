import * as monaco from 'https://cdn.jsdelivr.net/npm/monaco-editor@0.50.0/+esm';
import { parse } from './parser/gramatica.js';
import generateParser  from './compiler/utils.js';

export let ids = [];
export let usos = [];
export let errores = [];

// Crear el editor principal
const editor = monaco.editor.create(document.getElementById('editor'), {
    value: '',
    language: 'java',
    theme: 'tema',
    automaticLayout: true,
});

// Crear el editor para la salida
const salida = monaco.editor.create(document.getElementById('salida'), {
    value: '',
    language: 'java',
    readOnly: true,
    automaticLayout: true,
});

let decorations = [];

// Analizar contenido del editor
const analizar = () => {
    const entrada = editor.getValue();
    ids.length = 0;
    usos.length = 0;
    errores.length = 0;
    try {
        const cst = parse(entrada);
        //console.log(' Grmatica ', cst)
        if (errores.length > 0) {
            salida.setValue(`Error: ${errores[0].message}`);
            return;
        } else {
            const fileContents = generateParser(cst);
            const blob = new Blob([fileContents], { type: 'text/plain' });
            const url = URL.createObjectURL(blob);
            const button = document.getElementById('BotonDescarga');
            button.href = url;
            button.disabled = false;
            salida.setValue(fileContents);
        }

        decorations = editor.deltaDecorations(decorations, []);
    } catch (e) {
        console.log(e);

        if (e.location === undefined) {
            salida.setValue(`Error: ${e.message}`);
        } else {
            salida.setValue(
                `Error: ${e.message}\nEn línea ${e.location.start.line} columna ${e.location.start.column}`
            );

            decorations = editor.deltaDecorations(decorations, [
                {
                    range: new monaco.Range(
                        e.location.start.line,
                        e.location.start.column,
                        e.location.start.line,
                        e.location.start.column + 1
                    ),
                    options: {
                        inlineClassName: 'errorHighlight',
                    },
                },
                {
                    range: new monaco.Range(
                        e.location.start.line,
                        e.location.start.column,
                        e.location.start.line,
                        e.location.start.column
                    ),
                    options: {
                        glyphMarginClassName: 'warningGlyph',
                    },
                },
            ]);
        }
    }
};

// Escuchar cambios en el contenido del editor
editor.onDidChangeModelContent(() => {
    analizar();
});

// CSS personalizado para resaltar el error y agregar un warning
const style = document.createElement('style');
style.innerHTML = `
    .errorHighlight {
        color: red !important;
        font-weight: bold;
    }
    .warningGlyph {
        background: url('data:image/svg+xml;utf8,<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16"><path fill="orange" d="M8 1l7 14H1L8 1z"/></svg>') no-repeat center center;
        background-size: contain;
    }
`;
document.head.appendChild(style);
