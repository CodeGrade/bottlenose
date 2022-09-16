(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

  CodeMirror.defineMode("lean", function(_config, modeConfig) {
  function switchState(source, setState, state, f) {
    setState(f);
    return f(source, setState, state);
  }

  const digitRE = /\d/;
  const hexitRE = /[0-9A-Fa-f]/;
  const whiteCharRE = /[ \t\v\f]/; // newlines are handled in tokenizer
  const defBeginRE = /^(inductive|coinductive|structure|theorem|axiom|axioms|abbreviation|lemma|definition|def|instance|class|constant)/;
  const escapedSingleCharRE = /^'(\\\\(x[0-9A-Fa-f][0-9A-Fa-f]|u[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]|.))'/;
  const keywordOtherRE = /^(import|prelude|theory|definition|def|abbreviation|instance|renaming|hiding|exposing|parameter|parameters|begin|constant|constants|lemma|variable|variables|theorem|example|open|axiom|inductive|coinductive|with|structure|universe|universes|alias|precedence|reserve|postfix|prefix|infix|infixl|infixr|notation|end|using|namespace|section|local|set_option|extends|include|omit|class|classes|instances|raw|run_cmd|restate_axiom|calc|have|this|match|do|suffices|show|by|in|at|let|forall|fun|exists|assume|from|λ *|∀ *|Π *|∃ *|Σ *|if|then|else)(?!\.)\b/;
  // const operatorsRE = /^==?|>{0,3}=|>{1,3}|<[\|$]>|<[\*=<]?|<<=|≥|≤|!|~|\?|:=?|←|→|\$|\+|\*|&[&=]?|\|[\|=]?/;

  function normal(source, setState, state) {
    if (source.sol()) state.lastChar = null; // must reset lastChar at start of new line

    if (source.eatWhile(whiteCharRE)) {
      state.lastChar = source.peek() && ' ';
      return null;
    }

    if (source.match(/^--/)) {
      source.skipToEnd();
      state.lastChar = null;
      return 'comment';
    }

    if (source.match('/-')) {
      return switchState(source, setState, state, ncomment("comment", 1, state));
    }

    if (!state.lastChar || !state.lastChar.match(/\w/)) {
      // \b
      if (source.match(/^(Prop|Type|Sort)\b/)) {
        state.lastChar = source.peek() && 'a';
        return "builtin";
      }

      // \b
      if (source.match(/^attribute\b\s*\[[^\]]*\]/)) {
        state.lastChar = source.peek() && ']';
        return "attribute";
      }
      // \b
      if (source.match(/^@\[[^\]]*\]/)) {
        state.lastChar = source.peek() && ']';
        return "attribute";
      }

      // \b
      if (source.match(/^sorry\b/)) {
        state.lastChar = source.peek() && 'y';
        return "error";
      }

      if (state.lastChar !== '.') {
        // \b no '.'
        if (source.match(/^(private|meta|mutual|protected|noncomputable)\b/)) {
          state.lastChar = source.peek() && 'a';
          return "keyword";
        }

        // \b no '.'
        if (source.match(defBeginRE)) {
          setState(defCommand);
          return "keyword";
        }

        // \b no '.'
        if (source.match(keywordOtherRE)) {
          state.lastChar = source.peek() && 'a';
          return 'keyword';
        }
      }
    }

    // if (source.match(operatorsRE)) {
    //   state.lastChar = null;
    //   return "operator";
    // }

    if (source.match(/^#print\s+(def|definition|inductive|instance|structure|axiom|axioms|class)\b/)) {
      state.lastChar = source.peek() && 'a';
      return 'meta';
    }

    if (source.match(/^#(print|eval|reduce|check|help|exit|find|where)\b/)) {
      state.lastChar = source.peek() && 'a';
      return 'meta';
    }

    if (source.eat('"')) {
      state.lastChar = source.peek() && '"';
      return switchState(source, setState, state, stringLiteral);
    }

    if (source.match(/^'[^\\\\']'/) || 
        source.match(escapedSingleCharRE)) {
      state.lastChar = source.peek() && '\'';
      return "string-2";
    }

    if (source.match(/^([0-9]+|0([xX][0-9a-fA-F]+))\b/)) {
      state.lastChar = source.peek() && '0';
      return "number";
    }

    const ch = source.next();
    state.lastChar = ch;
    return null;
  }

  function defCommand(source, setState, state) {
    if (source.eatWhile(whiteCharRE)) return null;
    if (source.sol()) state.lastChar = null;

    if (source.match(/^--/)) {
      source.skipToEnd();
      state.lastChar = null;
      return 'comment';
    }
    if (source.match('/-')) {
      return switchState(source, setState, state, ncomment("comment", 1, state, defCommand));
    }

    if (source.match(/^with\b|^extends\b|^[:\|\(\[\{⦃<>]/, false)) {
      setState(normal);
      state.lastChar = source.peek() && 'a';
      return null;
    }

    if (!state.lastChar || !state.lastChar.match(/\w/)) {
      // \b
      if (source.match(/^[^:«»\(\)\{\} \t\v\f=→λ∀?][^:«»\(\)\{\} \t\v\f]*/)) {
        setState(normal);
        state.lastChar = source.peek() && 'a';
        return 'def';
      }
    }
    if (source.eat("«")) {
      state.lastChar = source.peek() && "»";
      return switchState(source, setState, state, defInDoubleAngle);
    }

    return null;
  }

  function defInDoubleAngle(source, setState, state) {
    if (source.eat("»")) {
      setState(defCommand);
      return null;
    }

    source.next();
    return 'def';
  }

  function ncomment(type, nest, state, returnState=normal) {
    if (nest == 0) {
      state.lastChar = '/';
      return returnState;
    }
    return function(source, setState, state) {
      let currNest = nest;
      while (!source.eol()) {
        const ch = source.next();
        if (ch == '/' && source.eat('-')) {
          ++currNest;
        }
        else if (ch == '-' && source.eat('/')) {
          --currNest;
          if (currNest == 0) {
            setState(returnState);
            state.lastChar = source.peek() && '/';
            return type;
          }
        }
      }
      setState(ncomment(type, currNest, state, returnState));
      return type;
    };
  }

  function stringLiteral(source, setState, state) {
    while (!source.eol()) {
      const ch = source.next();
      if (ch == '"') {
        setState(normal);
        return "string";
      }
      if (ch == '\\') {
        if (source.eat('\\') || source.eat('\"') || source.eat('n') || source.eat('t') || source.eat('\''))
          return "string";
        else if (source.eat('x')) {
          source.next(); // should really match only 0-9a-fA-F here and below...
          source.next();
          return "string";
        }
        else if (source.eat('u')) {
          source.next();
          source.next();
          source.next();
          source.next();
          return "string";
        }
      }
    }
    setState(normal);
    return "string error";
  }

  return {
    startState: function ()  { return { f: normal, lastChar:null }; },
    copyState:  function (s) { return { f: s.f, lastChar: s.lastChar }; },

    token: function(stream, state) {
      // second argument below is "setState"; it changes the state function (but not lastChar)
      return state.f(stream, function(s) { state.f = s; }, state);
    },

    blockCommentStart: "/-",
    blockCommentEnd: "-/",
    lineComment: "--",
  };

});

CodeMirror.defineMIME("text/lean", "lean");
});
