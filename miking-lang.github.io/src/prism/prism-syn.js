Prism.languages.syn = {
    'comment': {
        pattern: /\-\-[^\r\n]*|\/\-[\s\S]*?\-\//,
        greedy: true
    },
    'string': [
        {
            pattern: /"(?:\\(?:[\s\S]|\r\n)|[^\\"])*"/,
            greedy: true
        },
    ],
    'directive': {
        //pattern: /\B#\w+/,
        pattern: /#\b(?:var|con|type|label|frozen)\b/,
        alias: 'property'
    },
    // For the list of keywords and operators,
    // see: http://caml.inria.fr/pub/docs/manual-ocaml/lex.html#sec84

    'keyword': /\b(?:empty|except|include|(?:infix|prod)(?: (?:left|right))?|language|postfix|precedence|prefix|start|token|type)\b/,

    'property': {
        pattern: /\b[a-z][a-zA-Z0-9_]*\b/,
    },

    'operator': /\-\>|[=:]/i,
    'punctuation': /[.;]/
};
