%token OPEN_CURLY
%token CLOSE_CURLY
%token OPEN_ROUND
%token CLOSE_ROUND
%token SEMICOLON
%token KEYWORD_INT
%token KEYWORD_RETURN
%token <string> IDENTIFIER
%token <int> INT_LITERAL 
%token NEGATION
%token LOGICAL_NEGATION
%token COMPLEMENT
%token ADDITION
%token MULTIPLICATION
%token DIVISION
%token LOGICAL_AND
%token LOGICAL_OR
%token EQUAL
%token NOT_EQUAL
%token LESS_THAN
%token LESS_THAN_OR_EQUAL
%token GREATER_THAN
%token GREATER_THAN_OR_EQUAL
%token ASSIGNMENT
%token KEYWORD_IF
%token KEYWORD_ELSE
%token QUESTION_MARK
%token COLON
%token EOF

%start <Ast.program> main
%%

main:
| EOF       { {name="main"; body=[]} }
