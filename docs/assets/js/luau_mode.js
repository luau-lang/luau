// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: https://codemirror.net/LICENSE

// Luau mode. Based on Lua mode from CodeMirror and Franciszek Wawrzak (https://codemirror.net/mode/lua/lua.js)

(function(mod) {
    if (typeof exports == "object" && typeof module == "object") // CommonJS
      mod(require("../../lib/codemirror"));
    else if (typeof define == "function" && define.amd) // AMD
      define(["../../lib/codemirror"], mod);
    else // Plain browser env
      mod(CodeMirror);
})(function(CodeMirror) {
    "use strict";
  
  CodeMirror.defineMode("luau", function(_, parserConfig) {
    var indentUnit = 4;
  
    function prefixRE(words) {
        return new RegExp("^(?:" + words.join("|") + ")");
    }
    function wordRE(words) {
        return new RegExp("^(?:" + words.join("|") + ")$");
    }
    var specials = wordRE(parserConfig.specials || ["type"]);
  
    // long list of standard functions from lua manual
    var builtins = wordRE([
      "_G","_VERSION","assert","error","getfenv","getmetatable","ipairs","load", "loadstring","next","pairs","pcall",
      "print","rawequal","rawget","rawset","require","select","setfenv","setmetatable","tonumber","tostring","type","typeof",
      "unpack","xpcall",
  
      "coroutine.create","coroutine.resume","coroutine.running","coroutine.status","coroutine.wrap","coroutine.yield",
  
      "debug.info","debug.traceback",
  
      "math.abs","math.acos","math.asin","math.atan","math.atan2","math.ceil","math.cos","math.cosh","math.deg",
      "math.exp","math.floor","math.fmod","math.frexp","math.huge","math.ldexp","math.log","math.log10","math.max",
      "math.min","math.modf","math.pi","math.pow","math.rad","math.random","math.randomseed","math.sin","math.sinh",
      "math.sqrt","math.tan","math.tanh",
  
      "os.clock","os.date","os.difftime","os.time",

      "string.byte","string.char","string.find","string.format","string.gmatch","string.gsub",
      "string.len","string.lower","string.match","string.rep","string.reverse","string.sub","string.upper",
  
      "table.concat","table.clone","table.create","table.freeze","table.isfrozen","table.insert","table.maxn","table.move","table.remove","table.sort","table.unpack"
    ]);
    var keywords = wordRE(["and","break","elseif","false","nil","not","or","return",
                           "true","function", "end", "if", "then", "else", "do",
                           "while", "repeat", "until", "for", "in", "local", "continue" ]);
  
    var indentTokens = wordRE(["function", "if","repeat","do", "\\(", "{"]);
    var dedentTokens = wordRE(["end", "until", "\\)", "}"]);
    var dedentPartial = prefixRE(["end", "until", "\\)", "}", "else", "elseif"]);
  
    function readBracket(stream) {
        var level = 0;
        while (stream.eat("=")) ++level;
        stream.eat("[");
        return level;
    }
  
    function normal(stream, state) {
        var ch = stream.next();
        if (ch == "-" && stream.eat("-")) {
            if (stream.eat("[") && stream.eat("["))
                return (state.cur = bracketed(readBracket(stream), "comment"))(stream, state);
            stream.skipToEnd();
            return "comment";
        }
        if (ch == "\"" || ch == "'" || ch == "`")
            return (state.cur = string(ch))(stream, state);
        if (ch == "[" && /[\[=]/.test(stream.peek()))
            return (state.cur = bracketed(readBracket(stream), "string"))(stream, state);
        if (/\d/.test(ch)) {
            stream.eatWhile(/[\w.%]/);
            return "number";
        }
        if (/[\w_]/.test(ch)) {
            stream.eatWhile(/[\w\\\-_.]/);
            return "variable";
        }
        return null;
    }
  
    function bracketed(level, style) {
        return function(stream, state) {
            var curlev = null, ch;
            while ((ch = stream.next()) != null) {
                if (curlev == null) {
                    if (ch == "]") curlev = 0;
                } else if (ch == "=") {
                    ++curlev;
                } else if (ch == "]" && curlev == level) {
                    state.cur = normal;
                    break; 
                } else {
                    curlev = null;
                }
            }
            return style;
        };
    }
  
    function string(quote) {
        return function(stream, state) {
            var escaped = false, ignoreWhitespace = false, ch;
            while ((ch = stream.next()) != null) {
                if (ch == quote && !escaped) {
                    break;
                }
                if (ch == "z" && escaped) {
                    stream.eatSpace();
                    ignoreWhitespace = stream.eol();
                }
                escaped = !escaped && ch == "\\";
            }

            if (!ignoreWhitespace) {
                state.cur = normal;
            }
            return "string";
        };
    }
  
    return {
        startState: function(basecol) {
            return {basecol: basecol || 0, indentDepth: 0, cur: normal};
        },
    
        token: function(stream, state) {
            if (stream.eatSpace()) {
                return null;
            }
            var style = state.cur(stream, state);
            var word = stream.current();
            if (style == "variable") {
                if (keywords.test(word)) {
                    style = "keyword";
                } else if (builtins.test(word)) {
                    style = "builtin";
                } else if (specials.test(word)) {
                    style = "variable-2";
                }
            }
            if ((style != "comment") && (style != "string")) {
                if (indentTokens.test(word)) {
                    ++state.indentDepth;
                } else if (dedentTokens.test(word)) {
                    --state.indentDepth;
                }
            }
            return style;
        },
    
        indent: function(state, textAfter) {
            var closing = dedentPartial.test(textAfter);
            return state.basecol + indentUnit * (state.indentDepth - (closing ? 1 : 0));
        },

        electricInput: /^\s*(?:end|until|else|\)|\})$/,
        lineComment: "--",
        blockCommentStart: "--[[",
        blockCommentEnd: "]]"
    }});
    CodeMirror.defineMIME("text/x-luau", "luau");
});
