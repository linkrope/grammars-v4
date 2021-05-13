//          Copyright D Language Foundation 1999-2021.
//          Copyright Mario Kröplin 2021.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          https://www.boost.org/LICENSE_1_0.txt)

// D lexer grammar for ANTLR 4 derived from https://dlang.org/spec/lex.html
// v2.096.0

lexer grammar DLexer;

// 2.3 End of File

// TODO

// 2.4 End of Line

// TODO

// 2.5 White Space

WhiteSpace
    : [ \t\r\n]+ -> skip
    ;

// TODO: Space

// 2.6 Comments

BlockComment
    : '/*' .*? '*/' -> skip
    ;

LineComment
    : '//' ~[\r\n]* -> skip
    ;

NestingBlockComment
    : '/+' (NestingBlockComment | .)*? '+/'	-> skip
    ;

// 2.7 Tokens

fragment Token
    : Identifier
    ;

Div: '/';
DivAssign: '/=';
Dot: '.';
DotDot: '..';
Ellipsis: '...';
And: '&';
AndAssign: '&=';
AndAnd: '&&';
Or: '|';
OrAssign: '|=';
OrOr: '||';
Minus: '-';
MinusAssign: '-=';
MinusMinus: '--';
Plus: '+';
PlusAssign: '+=';
PlusPlus: '++';
Less: '<';
LessEqual: '<=';
LeftShift: '<<';
LeftShiftAssign: '<<=';
Greater: '>';
GreaterEqual: '>=';
RightShiftAssign: '>>=';
UnisgnedRightShiftAssign: '>>>=';
RightShift: '>>';
UnsignedRightShift: '>>>';
Exclamation: '!';
NotEqual: '!=';
LeftParen: '(';
RightParen: ')';
LeftBracket: '[';
RightBracket: ']';
LeftBrace: '{';
RightBrace: '}';
Question: '?';
Comma: ',';
Semicolon: ';';
Colon: ':';
Dollar: '$';
Assign: '=';
Equal: '==';
Mul: '*';
MulAssign: '*=';
Mod: '%';
ModAssign: '%=';
Xor: '^';
XorAssign: '^=';
Pow: '^^';
PowAssign: '^^=';
Tilde: '~';
CatAssign: '~=';
At: '@';
Arrow: '=>';
Hash: '#';

// 2.14 Keywords

ABSTRACT: 'abstract';
ALIAS: 'alias';
ALIGN: 'align';
ASM: 'asm';
ASSERT: 'assert';
AUTO: 'auto';
BODY: 'body';
BOOL:'bool';
BREAK: 'break';
BYTE: 'byte';
CASE: 'case';
CAST: 'cast';
CATCH: 'catch';
CDOUBLE: 'cdouble';
CENT: 'cent';
CFLOAT: 'cfloat';
CHAR: 'char';
CLASS: 'class';
CONST: 'const';
CONTINUE: 'continue';
CREAL: 'creal';
DCHAR: 'dchar';
DEBUG: 'debug';
DEFAULT: 'default';
DELEGATE: 'delegate';
DELETE: 'delete'; // deprecated
DEPRECATED: 'deprecated';
DO: 'do';
DOUBLE: 'double';
ELSE: 'else';
ENUM: 'enum';
EXPORT: 'export';
EXTERN: 'extern';
FALSE: 'false';
FINAL: 'final';
FINALLY: 'finally';
FLOAT: 'float';
FOR: 'for';
FOREACH: 'foreach';
FOREACH_REVERSE: 'foreach_reverse';
FUNCTION: 'function';
GOTO: 'goto';
IDOUBLE: 'idouble';
IF: 'if';
IFLOAT: 'ifloat';
IMMUTABLE: 'immutable';
IMPORT: 'import';
IN: 'in';
INOUT: 'inout';
INT: 'int';
INTERFACE: 'interface';
INVARIANT: 'invariant';
IREAL: 'ireal';
IS: 'is';
LAZY: 'lazy';
LONG: 'long';
MACRO: 'macro'; // reserved
MIXIN: 'mixin';
MODULE: 'module';
NEW: 'new';
NOTHROW: 'nothrow';
NULL: 'null';
OUT: 'out';
OVERRIDE: 'override';
PACKAGE: 'package';
PRAGMA: 'pragma';
PRIVATE: 'private';
PROTECTED: 'protected';
PUBLIC: 'public';
PURE: 'pure';
REAL: 'real';
REF: 'ref';
RETURN: 'return';
SCOPE: 'scope';
SHARED: 'shared';
SHORT: 'short';
STATIC: 'static';
STRUCT: 'struct';
SUPER: 'super';
SWITCH: 'switch';
SYNCHRONIZED: 'synchronized';
TEMPLATE: 'template';
THIS: 'this';
THROW: 'throw';
TRUE: 'true';
TRY: 'try';
TYPEID: 'typeid';
TYPEOF: 'typeof';
UBYTE: 'ubyte';
UCENT: 'ucent';
UINT: 'uint';
ULONG: 'ulong';
UNION: 'union';
UNITTEST: 'unittest';
USHORT: 'ushort';
VERSION: 'version';
VOID: 'void';
WCHAR: 'wchar';
WHILE: 'while';
WITH: 'with';
FILE: '__FILE__';
FILE_FULL_PATH: '__FILE_FULL_PATH__';
MODULE_: '__MODULE__';
LINE: '__LINE__';
FUNCTION_: '__FUNCTION__';
PRETTY_FUNCTION: '__PRETTY_FUNCTION__';
GSHARED: '__gshared';
TRAITS: '__traits';
VECTOR: '__vector';
PARAMETERS: '__parameters';

// 2.8 Identifiers

Identifier
    : IdentifierStart IdentifierChar*
    ;

fragment IdentifierChar
    : IdentifierStart
    | [0-9]
    ;

fragment IdentifierStart
    : '_'
    | [\p{Alpha}]
    ;

// 2.9 String Literals

StringLiteral
    : WysiwygString
    | AlternateWysiwygString
    | DoubleQuotedString
    | HexString
    | DelimitedString
    | TokenString
    ;

fragment WysiwygString
    : 'r"' .*? '"' StringPostfix?
    ;

fragment AlternateWysiwygString
    : '`' .*? '`' StringPostfix?
    ;

fragment DoubleQuotedString
    : '"' (EscapeSequence | ~["\\])* '"'
    ;

fragment EscapeSequence
    : '\\' ['"?\\abfnrtv]
    | '\\x' HexDigit HexDigit
    | '\\' OctalDigit OctalDigit? OctalDigit?
    | '\\u' HexDigit HexDigit HexDigit HexDigit
    | '\\U' HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit
    | '\\' NamedCharacterEntity
    ;

fragment OctalDigit
    : [0-7]
    ;

fragment NamedCharacterEntity
    : '&' Identifier ';'
    ;

// FIXME: 'x' is not an Identifier
fragment HexString
    : 'x"' HexStringChar* '"' StringPostfix?
    ;

fragment HexStringChar
    : HexDigit
    | [ \t\r\n]+
    ;

fragment StringPostfix
    : [cwd]
    ;

// FIXME: 'q' is not an Identifier
fragment DelimitedString
    : 'q"' /* TODO: Delimiter WysiwygCharacters? MatchingDelimiter */ '"'
    ;

// FIXME: 'q' is not an Identifier
fragment TokenString
    : 'q{' Token* '}'
    ;

// 2.11 Character Literals

CharacterLiteral
    : '\'' (EscapeSequence | ~['\\\r\n]) '\''
    ;

// 2.12 Integer Literals

IntegerLiteral
    : Integer IntegerSuffix?
    ;

fragment Integer
    : DecimalInteger
    | BinaryInteger
    | HexadecimalInteger
    ;

fragment IntegerSuffix
    : [L] [uU]?
    | [uU] [L]?
    ;

fragment DecimalInteger
    : '0'
    | [1-9] DecimalDigitUS*
    ;

fragment BinaryInteger
    : BinPrefix BinaryDigitUS* BinaryDigit BinaryDigitUS*
    ;

fragment BinPrefix
    : '0' [bB]
    ;

fragment HexadecimalInteger
    : HexPrefix HexDigitUS* HexDigit HexDigitUS*
    ;

fragment DecimalDigitsNoSingleUS
    : DecimalDigitUS* DecimalDigit DecimalDigitUS*
    ;

fragment DecimalDigitsNoStartingUS
    : DecimalDigit DecimalDigitUS*
    ;

fragment DecimalDigit
    : [0-9]
    ;

fragment DecimalDigitUS
    : DecimalDigit
    | '_'
    ;

fragment BinaryDigit
    : [01]
    ;

fragment BinaryDigitUS
    : BinaryDigit
    | '_'
    ;

fragment HexDigitsNoSingleUS
    : HexDigitUS* HexDigit HexDigitUS*
    ;

fragment HexDigitsNoStartingUS
    :  HexDigit HexDigitUS*
    ;


fragment HexDigit
    : [0-9a-fA-F]
    ;

fragment HexDigitUS
    : HexDigit
    | '_'
    ;

// 2.13 Floating Point Literals

FloatLiteral
    : Float
    | Float Suffix
    | Integer FloatSuffix
    | Integer ImaginarySuffix
    | Integer FloatSuffix ImaginarySuffix
    | Integer RealSuffix ImaginarySuffix
    ;

fragment Float
    : DecimalFloat
    | HexFloat
    ;

fragment DecimalFloat
    : LeadingDecimal '.'
    | LeadingDecimal '.' DecimalDigit+
    | DecimalDigit+ '.' DecimalDigitsNoStartingUS DecimalExponent
    | '.' DecimalInteger
    | '.' DecimalInteger DecimalExponent
    | LeadingDecimal DecimalExponent
    ;

fragment DecimalExponent
    : DecimalExponentStart DecimalDigitsNoSingleUS
    ;

fragment DecimalExponentStart
    : [eE] [+-]?
    ;

fragment HexFloat
    : HexPrefix HexDigitsNoSingleUS '.' HexDigitsNoStartingUS HexExponent
    | HexPrefix '.' HexDigitsNoStartingUS HexExponent
    | HexPrefix HexDigitsNoSingleUS HexExponent
    ;

fragment HexPrefix
    : '0' [xX]
    ;

fragment HexExponent
    : HexExponentStart DecimalDigitsNoSingleUS
    ;

fragment HexExponentStart
    : [pP] [+-]?
    ;

fragment Suffix
    : FloatSuffix ImaginarySuffix?
    | RealSuffix ImaginarySuffix?
    | ImaginarySuffix
    ;

fragment FloatSuffix
    : [fF]
    ;

fragment RealSuffix
    : 'L'
    ;

fragment ImaginarySuffix
    : 'i'
    ;

fragment LeadingDecimal
    : DecimalInteger
    | '0' DecimalDigitsNoSingleUS
    ;
