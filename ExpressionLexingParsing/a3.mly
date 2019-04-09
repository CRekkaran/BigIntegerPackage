%{
    open A1
%}

%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF EOL
%start main
%type <A1.exptree> main 
%%




main:
	|dis			{$1}
;
dis:
	|dis DISJ con 	{Disjunction($1,$3)}
	|con  			{$1}
;
con:
	|con CONJ e 	{Conjunction($1,$3)}
	|e 				{$1}
e:
	|e EQ a 		{Equals($1,$3)}
	|e LT EQ a 		{LessTE($1,$4)}
	|e LT a 		{LessT($1,$3)}
	|e GT a 		{GreaterT($1,$3)}
	|e GT EQ a  	{GreaterTE($1,$4)}
	|a 				{$1}
;

a:
	|a PLUS b  		{Add($1,$3)}
	|a MINUS b 		{Sub($1,$3)}
	|b 				{$1}
;
b:
	|b TIMES c 		{Mult($1,$3)}
	|b DIV c 		{Div($1,$3)}
	|b REM c 		{Rem($1,$3)}
	|c 				{$1}
;
c:
	|ABS c			{Abs($2)}
	|d 				{$1}
;
d:
	|TILDA d 		{Negative($2)}
	|f 				{$1}
;

f:
	|IF main THEN main ELSE main FI 	{IfThenElse($2,$4,$6)}
	|g 		{$1}
;

g:
	|PROJ LP INT COMMA INT RP main 		{Project(($3,$5),$7)}
	|h	{$1}
;

h:
	|LP j RP	{Tuple((List.length $2),(List.rev $2))}
	|k 			{$1}
;

j:
	|j COMMA main 	{$3::$1}
	|main COMMA main 	{$3::[$1]}
	|k 			{[$1]}
;

k:
	| LP main RP 	{InParen($2)}
	| NOT dis 		{Not($2)}
	| l 			{$1}
;
l:
	|INT  			{N($1)}
	|BOOL 			{B($1)}
	|ID 			{Var($1)} 
;
