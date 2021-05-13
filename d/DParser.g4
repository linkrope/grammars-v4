//          Copyright D Language Foundation 1999-2021.
//          Copyright Mario Kröplin 2021.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          https://www.boost.org/LICENSE_1_0.txt)

// D parser grammar for ANTLR 4 derived from https://dlang.org/spec/grammar.html
// v2.096.0

parser grammar DParser;

options
{
   tokenVocab = DLexer;
}

// 3.2 Modules

module_
    : moduleDeclaration? declDef+ EOF
    ;

declDef
    : attributeSpecifier
    | declaration
    | constructor
    | destructor
    | postblit
    | allocator
    | deallocator
    | classInvariant
    | structInvariant
    | unitTest
    | aliasThis
    | staticConstructor
    | staticDestructor
    | sharedStaticConstructor
    | sharedStaticDestructor
    | conditionalDeclaration
    | debugSpecification
    | versionSpecification
    | staticAssert
    | templateDeclaration
    | templateMixinDeclaration
    | templateMixin
    | mixinDeclaration
    | ';'
    ;

moduleDeclaration
    : moduleAttribute* 'module' moduleFullyQualifiedName ';'
    ;

moduleAttribute
    : deprecatedAttribute
    | userDefinedAttribute
    ;

moduleFullyQualifiedName
    : (packages '.')? moduleName
    ;

moduleName
    : Identifier
    ;

packages
    : packageName ('.' packageName)*
    ;

packageName
    : Identifier
    ;

importDeclaration
    : 'static'? 'import' importList ';'
    ;

importList
    : (import_ ',')* (import_ | importBindings)
    ;

import_
    : (moduleAliasIdentifier '=')? moduleFullyQualifiedName
    ;

importBindings
    : import_ ':' importBindList
    ;

importBindList
    : importBind (',' importBind)*
    ;

importBind
    : (Identifier '=')? Identifier
    ;

moduleAliasIdentifier
    : Identifier
    ;

mixinDeclaration
    : 'mixin' '(' argumentList ')'
    ;

// 3.3 Declarations

declaration
    : funcDeclaration
    | varDeclarations
    | aliasDeclaration
    | aggregateDeclaration
    | enumDeclaration
    | importDeclaration
    | conditionalDeclaration
    | staticForeachDeclaration
    | staticAssert
    ;

varDeclarations
    : storageClass* basicType declarators ';'
    | autoDeclaration
    ;

declarators
    : declaratorInitializer
    | declaratorInitializer ',' declaratorIdentifierList
    ;

declaratorInitializer
    : varDeclarator
    | varDeclarator templateParameters? '=' initializer
    | altDeclarator
    | altDeclarator '=' initializer
    ;

declaratorIdentifierList
    : declaratorIdentifier (',' declaratorIdentifier)*
    ;

declaratorIdentifier
    : varDeclaratorIdentifier
    | altDeclaratorIdentifier
    ;

varDeclaratorIdentifier
    : Identifier
    | Identifier templateParameters? '=' initializer
    ;

altDeclaratorIdentifier
    : typeSuffix+ Identifier altDeclaratorSuffix* ('=' initializer)?
    | typeSuffix* Identifier altDeclaratorSuffix+ ('=' initializer)?
    ;

declarator
    : varDeclarator
    | altDeclarator
    ;

varDeclarator
    : typeSuffix* Identifier
    ;

altDeclarator
    : typeSuffix* Identifier altDeclaratorSuffix+
    | typeSuffix* '(' altDeclaratorInner ')'
    | typeSuffix* '(' altDeclaratorInner ')' altFuncDeclaratorSuffix
    | typeSuffix* '(' altDeclaratorInner ')' altDeclaratorSuffix+
    ;

altDeclaratorInner
    : typeSuffix* Identifier
    | typeSuffix* Identifier altFuncDeclaratorSuffix
    | altDeclarator
    ;

altDeclaratorSuffix
    : '[' ']'
    | '[' assignExpression ']'
    | '[' type ']'
    ;

altFuncDeclaratorSuffix
    : parameters memberFunctionAttribute*
    ;

storageClass
    : linkageAttribute
    | alignAttribute
    | 'deprecated'
    | 'enum'
    | 'static'
    | 'extern'
    | 'abstract'
    | 'final'
    | 'override'
    | 'synchronized'
    | 'auto'
    | 'scope'
    | 'const'
    | 'immutable'
    | 'inout'
    | 'shared'
    | '__gshared'
    | property
    | 'nothrow'
    | 'pure'
    | 'ref'
    ;

initializer
    : voidInitializer
    | nonVoidInitializer
    ;

nonVoidInitializer
    : expInitializer
    | arrayInitializer
    | structInitializer
    ;

expInitializer
    : assignExpression
    ;

arrayInitializer
    : '[' arrayMemberInitializations? ']'
    ;

arrayMemberInitializations
    : arrayMemberInitialization (',' arrayMemberInitialization)* ','?
    ;

arrayMemberInitialization
    : nonVoidInitializer
    | assignExpression ':' nonVoidInitializer
    ;

structInitializer
    : '{' structMemberInitializers? '}'
    ;

structMemberInitializers
    : structMemberInitializer (',' structMemberInitializer)* ','?
    ;

structMemberInitializer
    : (Identifier ':')* nonVoidInitializer
    ;

autoDeclaration
    : storageClass+ autoAssignments ';'
    ;

autoAssignments
    : autoAssignment (',' autoAssignment)*
    ;

autoAssignment
    : Identifier templateParameters? '=' initializer
    ;

aliasDeclaration
    : 'alias' storageClass* basicType declarators ';'
    | 'alias' storageClass* basicType funcDeclarator ';'
    | 'alias' aliasAssignments ';'
    ;

aliasAssignments
    : aliasAssignment (',' aliasAssignment)*
    ;

aliasAssignment
    : Identifier templateParameters? '=' storageClass* type
    | Identifier templateParameters? '=' functionLiteral
    | Identifier templateParameters? '=' storageClass* basicType parameters memberFunctionAttribute*
    ;

voidInitializer
    : 'void'
    ;

// 3.4 Types

type
    : typeCtor* basicType typeSuffix*
    ;

typeCtor
    : 'const'
    | 'immutable'
    | 'inout'
    | 'shared'
    ;

basicType
    : fundamentalType
    | qualifiedIdentifier
    | '.' qualifiedIdentifier
    | typeof_ ('.' qualifiedIdentifier)?
    | typeCtor '(' type ')'
    | vector
    | traitsExpression
    | mixinType
    ;

vector
    : '__vector' '(' vectorBaseType ')'
    ;

vectorBaseType
    : type
    ;

fundamentalType
    : 'bool'
    | 'byte'
    | 'ubyte'
    | 'short'
    | 'ushort'
    | 'int'
    | 'uint'
    | 'long'
    | 'ulong'
    | 'cent'
    | 'ucent'
    | 'char'
    | 'wchar'
    | 'dchar'
    | 'float'
    | 'double'
    | 'real'
    | 'ifloat'
    | 'idouble'
    | 'ireal'
    | 'cfloat'
    | 'cdouble'
    | 'creal'
    | 'void'
    ;

typeSuffix
    : '*'
    | '[' (assignExpression ('..' assignExpression)?)? ']'
    | '[' type ']'
    | 'delegate' parameters memberFunctionAttribute*
    | 'function' parameters functionAttribute*
    ;

qualifiedIdentifier
    : Identifier ('.' qualifiedIdentifier)?
    | templateInstance ('.' qualifiedIdentifier)?
    | Identifier '[' assignExpression ']' ('.' qualifiedIdentifier)?
    ;

typeof_
    : 'typeof' '(' expression ')'
    | 'typeof' '(' 'return' ')'
    ;

mixinType
    : 'mixin' '(' argumentList ')'
    ;

// 3.5 Attributes

attributeSpecifier
    : attribute ':'
    | attribute declarationBlock
    ;

attribute
    : linkageAttribute
    | alignAttribute
    | deprecatedAttribute
    | visibilityAttribute
    | pragma_
    | 'static'
    | 'extern'
    | 'abstract'
    | 'final'
    | 'override'
    | 'synchronized'
    | 'auto'
    | 'scope'
    | 'const'
    | 'immutable'
    | 'inout'
    | 'shared'
    | '__gshared'
    | atAttribute
    | functionAttributeKwd
    | 'ref'
    | 'return'
    ;

functionAttributeKwd
    : 'nothrow'
    | 'pure'
    ;

// AtAttribute:
//     @ disable
//     @ nogc
//     @ live
//     Property
//     @ safe
//     @ system
//     @ trusted
//     UserDefinedAttribute
atAttribute
    : userDefinedAttribute
    ;

// Property:
//     @ property
property
    : '@' Identifier
    ;

declarationBlock
    : declDef
    | '{' declDef* '}'
    ;

linkageAttribute
    : 'extern' '(' linkageType ')'
    | 'extern' '(' Identifier '++' ',' qualifiedIdentifier ')' // 'C++'
    | 'extern' '(' Identifier '++' ',' namespaceList ')' // 'C++'
    ;

linkageType
    : Identifier // 'C' | 'D' | 'Windows' | 'System'
    | Identifier '++' // 'C++'
    | Identifier '-' Identifier // 'Objective-C'
    ;

namespaceList
    : conditionalExpression (',' conditionalExpression)* ','?
    ;

alignAttribute
    : 'align'
    | 'align' '(' assignExpression ')'
    ;

deprecatedAttribute
    : 'deprecated'
    | 'deprecated' '(' assignExpression ')'
    ;

visibilityAttribute
    : 'private'
    | 'package'
    | 'package' '(' qualifiedIdentifier ')'
    | 'protected'
    | 'public'
    | 'export'
    ;

userDefinedAttribute
    : '@' '(' argumentList ')'
    | '@' Identifier
    | '@' Identifier '(' argumentList? ')'
    | '@' templateInstance
    | '@' templateInstance '(' argumentList? ')'
    ;

// 3.6 Pragmas

pragmaDeclaration // FIXME: unused
    : pragma_ ';'
    | pragma_ declarationBlock
    ;

pragmaStatement
    : pragma_ ';'
    | pragma_ noScopeStatement
    ;

pragma_
    : 'pragma' '(' Identifier ')'
    | 'pragma' '(' Identifier ',' argumentList ')'
    ;

// 3.7 Expressions

expression
    : assignExpression (',' assignExpression)*
    ;

commaExpression // FIXME: unused
    : assignExpression (',' assignExpression)*
    ;

assignExpression
    : conditionalExpression
    | conditionalExpression assignmentOperator assignExpression
    ;

assignmentOperator
    : '=' | '+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' | '~=' | '<<=' | '>>=' | '>>>=' | '^^='
    ;

conditionalExpression
    : orOrExpression
    | orOrExpression '?' expression ':' conditionalExpression
    ;

orOrExpression
    : andAndExpression
    | orOrExpression '||' andAndExpression
    ;

andAndExpression
    : orExpression
    | andAndExpression '&&' orExpression
    ;

orExpression
    : xorExpression
    | orExpression '|' xorExpression
    ;

xorExpression
    : andExpression
    | xorExpression '^' andExpression
    ;

andExpression
    : cmpExpression
    | andExpression '&' cmpExpression
    ;

cmpExpression
    : shiftExpression
    | equalExpression
    | identityExpression
    | relExpression
    | inExpression
    ;

equalExpression
    : shiftExpression ('==' | '!=') shiftExpression
    ;

identityExpression
    : shiftExpression ('is' | '!' 'is') shiftExpression
    ;

relExpression
    : shiftExpression ('<' | '<=' | '>' | '>=') shiftExpression
    ;

inExpression
    : shiftExpression ('in' | '!' 'in') shiftExpression
    ;

shiftExpression
    : addExpression
    | shiftExpression ('<<' | '>>' | '>>>') addExpression
    ;

addExpression
    : mulExpression
    | addExpression ('+' | '-') mulExpression
    // CatExpression:
    | addExpression '~' mulExpression
    ;

mulExpression
    : unaryExpression
    | mulExpression ('*' | '/' | '%') unaryExpression
    ;

unaryExpression
    : '&' unaryExpression
    | '++' unaryExpression
    | '--' unaryExpression
    | '*' unaryExpression
    | '-' unaryExpression
    | '+' unaryExpression
    | '!' unaryExpression
    | complementExpression
    | '(' type ')' '.' Identifier
    | '(' type ')' '.' templateInstance
    | deleteExpression
    | castExpression
    | powExpression
    ;

complementExpression
    : '~' unaryExpression
    ;

newExpression
    : 'new' allocatorArguments? type
    | 'new' allocatorArguments? type '[' assignExpression ']'
    | 'new' allocatorArguments? type '(' argumentList? ')'
    | newAnonClassExpression
    ;

allocatorArguments
    : '(' argumentList? ')'
    ;

argumentList
    : assignExpression (',' assignExpression)* ','?
    ;

deleteExpression
    : 'delete' unaryExpression
    ;

castExpression
    : 'cast' '(' type ')' unaryExpression
    | 'cast' '(' typeCtor* ')' unaryExpression
    ;

powExpression
    : postfixExpression
    | postfixExpression '^^' unaryExpression
    ;

postfixExpression
    : primaryExpression
    | postfixExpression '.' Identifier
    | postfixExpression '.' templateInstance
    | postfixExpression '.' newExpression
    | postfixExpression '++'
    | postfixExpression '--'
    | postfixExpression '(' argumentList? ')'
    | typeCtor* basicType '(' argumentList? ')'
    // IndexExpression:
    | postfixExpression '[' argumentList ']'
    // SliceExpression
    | postfixExpression '[' ']'
    | postfixExpression '[' slice ','? ']'
    ;

slice
    : assignExpression (',' slice)?
    | assignExpression '..' assignExpression (',' slice)?
    ;

primaryExpression
    : '.'? Identifier
    | '.'? templateInstance
    | 'this'
    | 'super'
    | 'null'
    | 'true'
    | 'false'
    | '$'
    | IntegerLiteral
    | FloatLiteral
    | CharacterLiteral
    | StringLiteral+
    | arrayLiteral
    | assocArrayLiteral
    | functionLiteral
    | assertExpression
    | mixinExpression
    | importExpression
    | newExpression
    | fundamentalType '.' Identifier
    | fundamentalType '(' argumentList? ')'
    | typeCtor '(' type ')' . Identifier
    | typeCtor '(' type ')' '(' argumentList? ')'
    | typeof_
    | typeidExpression
    | isExpression
    | '(' expression ')'
    | specialKeyword
    | traitsExpression
    ;

arrayLiteral
    : '[' argumentList? ']'
    ;

assocArrayLiteral
    : '[' keyValuePairs ']'
    ;

keyValuePairs
    : keyValuePair (',' keyValuePair)*
    ;

keyValuePair
    : keyExpression ':' valueExpression
    ;

keyExpression
    : assignExpression
    ;

valueExpression
    : assignExpression
    ;

functionLiteral
    : 'function' 'ref'? type? parameterWithAttributes? functionLiteralBody2
    | 'delegate' 'ref'? type? parameterWithMemberAttributes? functionLiteralBody2
    | 'ref'? parameterWithMemberAttributes functionLiteralBody2
    | functionLiteralBody
    | Identifier '=>' assignExpression
    ;

parameterWithAttributes
    : parameters functionAttribute*
    ;

parameterWithMemberAttributes
    : parameters memberFunctionAttribute*
    ;

functionLiteralBody2
    : '=>' assignExpression
    | functionLiteralBody
    ;

assertExpression
    : 'assert' '(' assertArguments ')'
    ;

assertArguments
    : assignExpression (',' assignExpression)? ','?
    ;

mixinExpression
    : 'mixin' '(' argumentList ')'
    ;

importExpression
    : 'import' '(' assignExpression ')'
    ;

typeidExpression
    : 'typeid' '(' type ')'
    | 'typeid' '(' expression ')'
    ;

isExpression
    : 'is' '(' type ')'
    | 'is' '(' type ':' typeSpecialization ')'
    | 'is' '(' type '==' typeSpecialization ')'
    | 'is' '(' type ':' typeSpecialization ',' templateParameterList ')'
    | 'is' '(' type '==' typeSpecialization ',' templateParameterList ')'
    | 'is' '(' type Identifier ')'
    | 'is' '(' type Identifier ':' typeSpecialization ')'
    | 'is' '(' type Identifier '==' typeSpecialization ')'
    | 'is' '(' type Identifier ':' typeSpecialization ',' templateParameterList ')'
    | 'is' '(' type Identifier '==' typeSpecialization ',' templateParameterList ')'
    ;

typeSpecialization
    : type
    | 'struct'
    | 'union'
    | 'class'
    | 'interface'
    | 'enum'
    | '__vector'
    | 'function'
    | 'delegate'
    | 'super'
    | 'const'
    | 'immutable'
    | 'inout'
    | 'shared'
    | 'return'
    | '__parameters'
    | 'module'
    | 'package'
    ;

specialKeyword
    : '__FILE__'
    | '__FILE_FULL_PATH__'
    | '__MODULE__'
    | '__LINE__'
    | '__FUNCTION__'
    | '__PRETTY_FUNCTION__'
    ;

// 3.8 Statements

statement
    : ';'
    | nonEmptyStatement
    | scopeBlockStatement
    ;

noScopeNonEmptyStatement
    : nonEmptyStatement
    | blockStatement
    ;

noScopeStatement
    : ';'
    | nonEmptyStatement
    | blockStatement
    ;

nonEmptyOrScopeBlockStatement
    : nonEmptyStatement
    | scopeBlockStatement
    ;

nonEmptyStatement
    : nonEmptyStatementNoCaseNoDefault
    | caseStatement
    | caseRangeStatement
    | defaultStatement
    ;

nonEmptyStatementNoCaseNoDefault
    : labeledStatement
    | expressionStatement
    | declarationStatement
    | ifStatement
    | whileStatement
    | doStatement
    | forStatement
    | foreachStatement
    | switchStatement
    | finalSwitchStatement
    | continueStatement
    | breakStatement
    | returnStatement
    | gotoStatement
    | withStatement
    | synchronizedStatement
    | tryStatement
    | scopeGuardStatement
    | throwStatement
    | asmStatement
    | mixinStatement
    | foreachRangeStatement
    | pragmaStatement
    | conditionalStatement
    | staticForeachStatement
    | staticAssert
    | templateMixin
    | importDeclaration
    ;

scopeStatement
    : nonEmptyStatement
    | blockStatement
    ;

scopeBlockStatement
    : blockStatement
    ;

labeledStatement
    : Identifier ':' (noScopeStatement | statement)?
    ;

blockStatement
    : '{' statement* '}'
    ;

expressionStatement
    : expression ';'
    ;

declarationStatement
    : storageClass* declaration
    ;

ifStatement
    : 'if' '(' ifCondition ')' thenStatement ('else' elseStatement)?
    ;

ifCondition
    : expression
    | 'auto' Identifier '=' expression
    | typeCtor+ Identifier '=' expression
    | typeCtor* basicType declarator '=' expression
    ;

thenStatement
    : scopeStatement
    ;

elseStatement
    : scopeStatement
    ;

whileStatement
    : 'while' '(' ifCondition ')' scopeStatement
    ;

doStatement
    : 'do' scopeStatement  'while' '(' expression ')' ';'
    ;

forStatement
    : 'for' '(' initialize test? ';' increment? ')' scopeStatement
    ;

initialize
    : ';'
    | noScopeNonEmptyStatement
    ;

test
    : expression
    ;

increment
    : expression
    ;

aggregateForeach
    : foreach_ '(' foreachTypeList ';' foreachAggregate ')'
    ;

foreachStatement
    : aggregateForeach noScopeNonEmptyStatement
    ;

foreach_
    : 'foreach'
    | 'foreach_reverse'
    ;

foreachTypeList
    : foreachType (',' foreachType)*
    ;

foreachType
    : foreachTypeAttribute* basicType declarator
    | foreachTypeAttribute* Identifier
    | foreachTypeAttribute* 'alias' Identifier
    ;

foreachTypeAttribute
    : 'ref'
    | typeCtor
    | 'enum'
    ;

foreachAggregate
    : expression
    ;

rangeForeach
    : foreach_ '(' foreachType ';' lwrExpression '..' uprExpression ')'
    ;

lwrExpression
    : expression
    ;

uprExpression
    : expression
    ;

foreachRangeStatement
    : rangeForeach scopeStatement
    ;

switchStatement
    : 'switch' '(' expression ')' scopeStatement
    ;

caseStatement
    : 'case' argumentList ':' scopeStatementList
    ;

caseRangeStatement
    : 'case' firstExp ':' '..' 'case' lastExp ':' scopeStatementList
    ;

firstExp
    : assignExpression
    ;

lastExp
    : assignExpression
    ;

defaultStatement
    : 'default' ':' scopeStatementList
    ;

scopeStatementList // FIXME: A ScopeStatementList must either be empty [...]
    : statementNoCaseNoDefault*
    ;

statementNoCaseNoDefault
    : ';'
    | nonEmptyStatementNoCaseNoDefault
    | scopeBlockStatement
    ;

finalSwitchStatement
    : 'final' 'switch' '(' expression ')' scopeStatement
    ;

continueStatement
    : 'continue' Identifier? ';'
    ;

breakStatement
    : 'break' Identifier? ';'
    ;

returnStatement
    : 'return' expression? ';'
    ;

gotoStatement
    : 'goto' Identifier ';'
    | 'goto' 'default' ';'
    | 'goto' 'case' expression? ';'
    ;

withStatement
    : 'with' '(' expression ')' scopeStatement
    | 'with' '(' symbol ')' scopeStatement
    | 'with' '(' templateInstance ')' scopeStatement
    ;

synchronizedStatement
    : 'synchronized' scopeStatement
    | 'synchronized' '(' expression ')' scopeStatement
    ;

tryStatement
    : 'try' scopeStatement catch_+
    | 'try' scopeStatement catch_+ finallyStatement
    | 'try' scopeStatement finallyStatement
    ;

catch_
    : 'catch' '(' catchParameter ')' noScopeNonEmptyStatement
    ;

catchParameter
    : basicType Identifier?
    ;

finallyStatement
    : 'finally' noScopeNonEmptyStatement
    ;

scopeGuardStatement
    : 'scope' '(' scopeIdentifier ')' nonEmptyOrScopeBlockStatement
    ;

scopeIdentifier
    : Identifier // 'exit' | 'success' | 'failure'
    ;

throwStatement
    : 'throw' expression ';'
    ;

mixinStatement
    : 'mixin' '(' argumentList ')' ';'
    ;

// 3.9 Structs and Unions

aggregateDeclaration
    : classDeclaration
    | interfaceDeclaration
    | structDeclaration
    | unionDeclaration
    ;

structDeclaration
    : 'struct' Identifier ';'
    | 'struct' Identifier aggregateBody
    | structTemplateDeclaration
    | anonStructDeclaration
    ;

anonStructDeclaration
    : 'struct' aggregateBody
    ;

unionDeclaration
    : 'union' Identifier ';'
    | 'union' Identifier aggregateBody
    | unionTemplateDeclaration
    | anonUnionDeclaration
    ;

anonUnionDeclaration
    : 'union' aggregateBody
    ;

aggregateBody
    : '{' declDef* '}'
    ;

postblit
    : 'this' '(' 'this' ')' memberFunctionAttribute* ';'
    | 'this' '(' 'this' ')' memberFunctionAttribute* functionBody
    ;

structInvariant
    : 'invariant' '(' ')' blockStatement
    | 'invariant' blockStatement
    | 'invariant' '(' assertArguments ')' ';'
    ;

// 3.10 Classes

classDeclaration
    : 'class' Identifier ';'
    | 'class' Identifier baseClassList? aggregateBody
    | classTemplateDeclaration
    ;

baseClassList
    : ':' superClass
    | ':' superClass ',' interfaces
    | ':' interfaces
    ;

superClass
    : basicType
    ;

interfaces
    : interface_ (',' interface_)*
    ;

interface_
    : basicType
    ;

constructor
    : 'this' parameters memberFunctionAttribute* ';'
    | 'this' parameters memberFunctionAttribute* functionBody
    | constructorTemplate
    ;

destructor
    : '~' 'this' '(' ')' memberFunctionAttribute* ';'
    | '~' 'this' '(' ')' memberFunctionAttribute* functionBody
    ;

staticConstructor
    : 'static' 'this' '(' ')' memberFunctionAttribute* ';'
    | 'static' 'this' '(' ')' memberFunctionAttribute* functionBody
    ;

staticDestructor
    : 'static' '~' 'this' '(' ')' memberFunctionAttribute* ';'
    | 'static' '~' 'this' '(' ')' memberFunctionAttribute* functionBody
    ;

sharedStaticConstructor
    : 'shared' 'static' 'this' '(' ')' memberFunctionAttribute* ';'
    | 'shared' 'static' 'this' '(' ')' memberFunctionAttribute* functionBody
    ;

sharedStaticDestructor
    : 'shared' 'static' '~' 'this' '(' ')' memberFunctionAttribute* ';'
    | 'shared' 'static' '~' 'this' '(' ')' memberFunctionAttribute* functionBody
    ;

classInvariant
    : 'invariant' '(' ')' blockStatement
    | 'invariant' blockStatement
    | 'invariant' '(' assertArguments ')' ';'
    ;

allocator
    : 'new' parameters ';'
    | 'new' parameters functionBody
    ;

deallocator
    : 'delete' parameters ';'
    | 'delete' parameters functionBody
    ;

aliasThis
    : 'alias' Identifier 'this' ';'
    ;

newAnonClassExpression
    : 'new' allocatorArguments? 'class' constructorArgs? superClass? interfaces? aggregateBody
    ;

constructorArgs
    : '(' argumentList? ')'
    ;

// 3.11 Interfaces

interfaceDeclaration
    : 'interface' Identifier ';'
    | 'interface' Identifier baseInterfaceList? aggregateBody
    | interfaceTemplateDeclaration
    ;

baseInterfaceList
    : ':' interfaces
    ;

// 3.12 Enums

enumDeclaration
    : 'enum' Identifier enumBody
    | 'enum' Identifier ':' enumBaseType enumBody
    | anonymousEnumDeclaration
    ;

enumBaseType
    : type
    ;

enumBody
    : '{' enumMembers '}'
    | ';'
    ;

enumMembers
    : enumMember (',' enumMember)* ','?
    ;

enumMemberAttribute
    : deprecatedAttribute
    | userDefinedAttribute
    | '@' Identifier // '@disable'
    ;

enumMember
    : enumMemberAttribute* Identifier ('=' assignExpression)?
    ;

anonymousEnumDeclaration
    : 'enum' ':' enumBaseType '{' enumMembers '}'
    | 'enum' '{' enumMembers '}'
    | 'enum' '{' anonymousEnumMembers '}'
    ;

anonymousEnumMembers
    : anonymousEnumMember (',' anonymousEnumMember)* ','?
    ;

anonymousEnumMember
    : enumMember
    | type Identifier '=' assignExpression
    ;

// 3.13 Functions

funcDeclaration
    : storageClass* basicType funcDeclarator functionBody
    | autoFuncDeclaration
    ;

autoFuncDeclaration
    : storageClass+ Identifier funcDeclaratorSuffix functionBody
    ;

funcDeclarator
    : typeSuffix* Identifier funcDeclaratorSuffix
    ;

funcDeclaratorSuffix
    : parameters memberFunctionAttribute*
    | templateParameters parameters memberFunctionAttribute* constraint?
    ;

parameters
    : '(' parameterList? ')'
    ;

parameterList
    : parameter
    | parameter ',' parameterList
    | variadicArgumentsAttribute+ '...'
    ;

parameter
    : parameterAttribute* basicType declarator
    | parameterAttribute* basicType declarator '...'?
    | parameterAttribute* basicType declarator '=' assignExpression
    | parameterAttribute* type
    | parameterAttribute* type '...'
    ;

parameterAttribute
    : inOut
    | userDefinedAttribute
    ;

inOut
    : 'auto'
    | typeCtor
    | 'final'
    | 'in'
    | 'lazy'
    | 'out'
    | 'ref'
    | 'return' 'ref'
    | 'scope'
    ;

variadicArgumentsAttribute
    : 'const'
    | 'immutable'
    | 'return'
    | 'scope'
    | 'shared'
    ;

functionAttribute
    : functionAttributeKwd
    | property
    ;

memberFunctionAttribute
    : 'const'
    | 'immutable'
    | 'inout'
    | 'return'
    | 'shared'
    | functionAttribute
    ;

functionBody
    : specifiedFunctionBody
    | missingFunctionBody
    | shortenedFunctionBody
    ;

functionLiteralBody
    : specifiedFunctionBody
    ;

specifiedFunctionBody
    : 'do'? blockStatement
    | functionContract* inOutContractExpression 'do'? blockStatement
    | functionContract* inOutStatement 'do' blockStatement
    ;

missingFunctionBody
    : ';'
    | functionContract* inOutContractExpression ';'
    | functionContract* inOutStatement
    ;

shortenedFunctionBody
    : '=>' assignExpression ';'
    ;

functionContract
    : inOutContractExpression
    | inOutStatement
    ;

inOutContractExpression
    : inContractExpression
    | outContractExpression
    ;

inOutStatement
    : inStatement
    | outStatement
    ;

inContractExpression
    : 'in' '(' assertArguments ')'
    ;

outContractExpression
    : 'out' '(' Identifier? ';' assertArguments ')'
    ;

inStatement
    : 'in' blockStatement
    ;

outStatement
    : 'out' ('(' Identifier ')')? blockStatement
    ;

// 3.14 Templates

templateDeclaration
    : 'template' Identifier templateParameters constraint? '{' declDef* '}'
    ;

templateParameters
    : '(' templateParameterList? ')'
    ;

templateParameterList
    : templateParameter (',' templateParameter)* ','?
    ;

templateParameter
    : templateTypeParameter
    | templateValueParameter
    | templateAliasParameter
    | templateSequenceParameter
    | templateThisParameter
    ;

templateInstance
    : Identifier templateArguments
    ;

templateArguments
    : '!' '(' templateArgumentList? ')'
    | '!' templateSingleArgument
    ;

templateArgumentList
    : templateArgument (',' templateArgument)* ','?
    ;

templateArgument
    : type
    | assignExpression
    | symbol
    ;

symbol
    : '.'? symbolTail
    ;

symbolTail
    : Identifier ('.' symbolTail)?
    | templateInstance ('.' symbolTail)?
    ;

templateSingleArgument
    : Identifier
    | fundamentalType
    | CharacterLiteral
    | StringLiteral
    | IntegerLiteral
    | FloatLiteral
    | 'true'
    | 'false'
    | 'null'
    | 'this'
    | specialKeyword
    ;

templateTypeParameter
    : Identifier
    | Identifier templateTypeParameterSpecialization
    | Identifier templateTypeParameterDefault
    | Identifier templateTypeParameterSpecialization templateTypeParameterDefault
    ;

templateTypeParameterSpecialization
    : ':' type
    ;

templateTypeParameterDefault
    : '=' type
    ;

templateThisParameter
    : 'this' templateTypeParameter
    ;

templateValueParameter
    : basicType declarator
    | basicType declarator templateValueParameterSpecialization
    | basicType declarator templateValueParameterDefault
    | basicType declarator templateValueParameterSpecialization templateValueParameterDefault
    ;

templateValueParameterSpecialization
    : ':' conditionalExpression
    ;

templateValueParameterDefault
    : '=' assignExpression
    | '=' specialKeyword
    ;

templateAliasParameter
    : 'alias' Identifier templateAliasParameterSpecialization? templateAliasParameterDefault?
    | 'alias' basicType declarator templateAliasParameterSpecialization? templateAliasParameterDefault?
    ;

templateAliasParameterSpecialization
    : ':' type
    | ':' conditionalExpression
    ;

templateAliasParameterDefault
    : '=' type
    | '=' conditionalExpression
    ;

templateSequenceParameter
    : Identifier '...'
    ;

constructorTemplate
    : 'this' templateParameters parameters memberFunctionAttribute* constraint? ':'
    | 'this' templateParameters parameters memberFunctionAttribute* constraint? functionBody
    ;

classTemplateDeclaration
    : 'class' Identifier templateParameters ';'
    | 'class' Identifier templateParameters constraint? baseClassList? aggregateBody
    | 'class' Identifier templateParameters baseClassList? constraint? aggregateBody
    ;

interfaceTemplateDeclaration
    : 'interface' Identifier templateParameters ';'
    | 'interface' Identifier templateParameters constraint? baseInterfaceList? aggregateBody
    | 'interface' Identifier templateParameters baseInterfaceList constraint aggregateBody
    ;

structTemplateDeclaration
    : 'struct' Identifier templateParameters ';'
    | 'struct' Identifier templateParameters constraint? aggregateBody
    ;

unionTemplateDeclaration
    : 'union' Identifier templateParameters ';'
    | 'union' Identifier templateParameters constraint? aggregateBody
    ;

constraint
    : 'if' '(' expression ')'
    ;

// 3.15 Template Mixins

templateMixinDeclaration
    : 'mixin' 'template' Identifier templateParameters constraint? '{' declDef* '}'
    ;

templateMixin
    : 'mixin' mixinTemplateName templateArguments? Identifier? ';'
    ;

mixinTemplateName
    : '.'? mixinQualifiedIdentifier
    | typeof_ '.' mixinQualifiedIdentifier
    ;

mixinQualifiedIdentifier
    : Identifier
    | Identifier '.' mixinQualifiedIdentifier
    | templateInstance '.' mixinQualifiedIdentifier
    ;

// 3.16 Conditional Compilation

conditionalDeclaration
    : condition declarationBlock ('else' declarationBlock)?
    | condition ':' declDef*
    | condition declarationBlock 'else' ':' declDef*
    ;

conditionalStatement
    : condition noScopeNonEmptyStatement ('else' noScopeNonEmptyStatement)?
    ;

condition
    : versionCondition
    | debugCondition
    | staticIfCondition
    ;

versionCondition
    : 'version' '(' IntegerLiteral ')'
    | 'version' '(' Identifier ')'
    | 'version' '(' 'unittest' ')'
    | 'version' '(' 'assert' ')'
    ;

versionSpecification
    : 'version' '=' Identifier ';'
    | 'version' '=' IntegerLiteral ';'
    ;

debugCondition
    : 'debug'
    | 'debug' '(' IntegerLiteral ')'
    | 'debug' '(' Identifier ')'
    ;

debugSpecification
    : 'debug' '=' Identifier ';'
    | 'debug' '=' IntegerLiteral ';'
    ;

staticIfCondition
    : 'static' 'if' '(' assignExpression ')'
    ;

staticForeach
    : 'static' aggregateForeach
    | 'static' rangeForeach
    ;

staticForeachDeclaration
    : staticForeach declarationBlock
    | staticForeach ':' declDef*
    ;

staticForeachStatement
    : staticForeach noScopeNonEmptyStatement
    ;

staticAssert
    : 'static' 'assert' '(' assertArguments ')' ';'
    ;

// 3.17 Traits

traitsExpression
    : '__traits' '(' traitsKeyword ',' traitsArguments ')'
    ;

// TraitsKeyword:
//     isAbstractClass
//     isArithmetic
//     isAssociativeArray
//     isFinalClass
//     isPOD
//     isNested
//     isFuture
//     isDeprecated
//     isFloating
//     isIntegral
//     isScalar
//     isStaticArray
//     isUnsigned
//     isDisabled
//     isVirtualFunction
//     isVirtualMethod
//     isAbstractFunction
//     isFinalFunction
//     isStaticFunction
//     isOverrideFunction
//     isTemplate
//     isRef
//     isOut
//     isLazy
//     isReturnOnStack
//     isZeroInit
//     isModule
//     isPackage
//     hasMember
//     hasCopyConstructor
//     hasPostblit
//     identifier
//     getAliasThis
//     getAttributes
//     getFunctionAttributes
//     getFunctionVariadicStyle
//     getLinkage
//     getLocation
//     getMember
//     getOverloads
//     getParameterStorageClasses
//     getPointerBitmap
//     getCppNamespaces
//     getVisibility
//     getProtection
//     getTargetInfo
//     getVirtualFunctions
//     getVirtualMethods
//     getUnitTests
//     parent
//     child
//     classInstanceSize
//     getVirtualIndex
//     allMembers
//     derivedMembers
//     isSame
//     compiles
//     toType
traitsKeyword
    : Identifier
    ;

traitsArguments
    : traitsArgument (',' traitsArgument)*
    ;

traitsArgument
    : assignExpression
    | type
    ;

// 3.18 Unit Tests

unitTest
    : 'unittest' blockStatement
    ;

// 3.19 D x86 Inline Assembler

asmStatement
    : 'asm' functionAttribute* '{' asmInstructionList? '}'
    ;

asmInstructionList
    : (asmInstruction ';')+
    ;

asmInstruction
    : // TODO
    ;
