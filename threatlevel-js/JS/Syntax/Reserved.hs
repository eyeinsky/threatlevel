module JS.Syntax.Reserved where

import Data.Text

-- * Reserved by ECMA

-- | Source: https://mathiasbynens.be/notes/reserved-keywords

ecma1 :: [Text]
ecma1 = [ "do", "if", "in", "for", "new", "try", "var", "case", "else", "enum",
   "null", "this", "true", "void", "with", "break", "catch", "class", "const",
   "false", "super", "throw", "while", "delete", "export", "import", "return",
   "switch", "typeof", "default", "extends", "finally", "continue", "debugger",
   "function"
   ]

ecma2 :: [Text]
ecma2 = ["do", "if", "in", "for", "int", "new", "try", "var", "byte", "case",
   "char", "else", "enum", "goto", "long", "null", "this", "true", "void", "with",
   "break", "catch", "class", "const", "false", "final", "float", "short",
   "super", "throw", "while", "delete", "double", "export", "import", "native",
   "public", "return", "static", "switch", "throws", "typeof", "boolean",
   "default", "extends", "finally", "package", "private", "abstract", "continue",
   "debugger", "function", "volatile", "interface", "protected", "transient",
   "implements", "instanceof", "synchronized"
   ]

ecma5 :: [Text]
ecma5 = [ "do", "if", "in", "for", "let", "new", "try", "var", "case", "else",
   "enum", "eval", "null", "this", "true", "void", "with", "break", "catch",
   "class", "const", "false", "super", "throw", "while", "yield", "delete",
   "export", "import", "public", "return", "static", "switch", "typeof",
   "default", "extends", "finally", "package", "private", "continue", "debugger",
   "function", "arguments", "interface", "protected", "implements", "instanceof",
   "NaN", "Infinity", "undefined"
   ]

ecma6 :: [Text]
ecma6 = [ "do", "if", "in", "for", "let", "new", "try", "var", "case", "else",
   "enum", "eval", "null", "this", "true", "void", "with", "await", "break",
   "catch", "class", "const", "false", "super", "throw", "while", "yield",
   "delete", "export", "import", "public", "return", "static", "switch", "typeof",
   "default", "extends", "finally", "package", "private", "continue", "debugger",
   "function", "arguments", "interface", "protected", "implements", "instanceof"
   ]
