Prism.languages.mcore = {
    'comment': {
        pattern: /\-\-[^\r\n]*|\/\-[\s\S]*?\-\//,
        greedy: true
    },
    'string': [
        {
            pattern: /"(?:\\(?:[\s\S]|\r\n)|[^\\"])*"/,
            greedy: true
        },
        // // Below is actually char, but using string name since highlighter does not like characters
        {
            pattern: /['](?:[^\\])?[\s\S][']/,
            greedy: true
        },
    ],
    'number': [
        // binary and octal
        /\b(?:0b[01][01_]*|0o[0-7][0-7_]*)\b/i,
        // hexadecimal
        /\b0x[a-f0-9][a-f0-9_]*(?:\.[a-f0-9_]*)?(?:p[+-]?\d[\d_]*)?(?!\w)/i,
        // decimal
        /\b\d[\d_]*(?:\.[\d_]*)?(?:e[+-]?\d[\d_]*)?(?!\w)/i,
    ],
    'directive': {
        //pattern: /\B#\w+/,
        pattern: /#\b(?:var|con|type|label|frozen)\b/,
        alias: 'property'
    },
    // For the list of keywords and operators,
    // see: http://caml.inria.fr/pub/docs/manual-ocaml/lex.html#sec84
    'keyword': /\b(?:if|then|else|match|with|let|recursive|utest|lam|lang|syn|sem|con|type|use|in|using|end|mexpr|never|switch|case|all|external|include|accelerate)\b/,
    'boolean': /\b(?:false|true)\b/,

    'builtin': [
        // Types
        /\b(?:Unknown|Bool|Int|Float|Char|String)\b/,
        // Operators
        /\b(?:addi|subi|muli|divi|modi|negi|lti|leqi|gti|geqi|eqi|neqi|slli|srli|srai|arity|addf|subf|mulf|divf|negf|ltf|leqf|gtf|geqf|eqf|neqf|char2int|int2char|int2float|string2float|length|concat|reverse|splitAt|makeSeq|cons|snoc|get|set|print|dprint|argv|readFile|writeFile|fileExists|deleteFile|error|floorfi|ceilfi|roundfi)\b/,
    ],

    'class-name': /[A-Z][a-zA-Z0-9_]*/,

    'operator': /\-\>|[=]/i,
    'punctuation': /[.;]/,
};
Prism.languages.mc = Prism.languages.mcore;
