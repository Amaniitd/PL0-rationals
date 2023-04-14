structure Tokens=Tokens

	type pos = int
	type pol = int
	type svalue = Tokens.svalue
	type ('a,'b) token = ('a,'b) Tokens.token
	type lexresult = (svalue, pos) token

	val linenum = ref 1;
	val column = ref 1;
	val eof = fn() => Tokens.EOF(!column,!column)

%%

%header (functor AssignLexFun(structure Tokens:Parser_TOKENS));
alpha = [A-Za-z];
whitespace = [\ \t];
digit = [0-9];
vchar = [0-9A-Za-z];

%%
\n => (linenum := !linenum + 1; column := 1; lex());
{whitespace}+ => (column := !column + size(yytext); lex());


"if" => (column := !column + size(yytext); Tokens.IF(!linenum,!column));
"then" => (column := !column + size(yytext); Tokens.THEN(!linenum,!column));
"else" => (column := !column + size(yytext); Tokens.ELSE(!linenum,!column));
"fi" => (column := !column + size(yytext); Tokens.FI(!linenum,!column));

"while" => (column := !column + size(yytext); Tokens.WHILE(!linenum,!column));
"do" => (column := !column + size(yytext); Tokens.DO(!linenum,!column));
"od" => (column := !column + size(yytext); Tokens.OD(!linenum,!column));


"rational" => (column := !column + size(yytext); Tokens.RATIONAL(!linenum,!column));
"integer" => (column := !column + size(yytext); Tokens.INTEGER(!linenum,!column));
"boolean" => (column := !column + size(yytext); Tokens.BOOLEAN(!linenum,!column));

"tt" => (column := !column + size(yytext); Tokens.TRUE(!linenum,!column));
"ff" => (column := !column + size(yytext); Tokens.FALSE(!linenum,!column));

"var" => (column := !column + size(yytext); Tokens.VAR(!linenum,!column));

"procedure" => (column := !column + size(yytext); Tokens.PROCEDURE(!linenum,!column));
"print" => (column := !column + size(yytext); Tokens.PRINT(!linenum,!column));
"read" => (column := !column + size(yytext); Tokens.READ(!linenum,!column));
"call" => (column := !column + size(yytext); Tokens.CALL(!linenum,!column));

"inverse" => (column := !column + size(yytext); Tokens.INVERSE(!linenum,!column));
"make_rat" => (column := !column + size(yytext); Tokens.MAKERAT(!linenum,!column));
"rat" => (column := !column + size(yytext); Tokens.RAT(!linenum,!column));
"showRat" => (column := !column + size(yytext); Tokens.SHOWRAT(!linenum,!column));
"showDecimal" => (column := !column + size(yytext); Tokens.SHOWDECIMAL(!linenum,!column));
"fromDecimal" => (column := !column + size(yytext); Tokens.FROMDECIMAL(!linenum,!column));
"toDecimal" => (column := !column + size(yytext); Tokens.TODECIMAL(!linenum,!column));


"!" => (column := !column + size(yytext); Tokens.NOT(!linenum,!column));
"&&" => (column := !column + size(yytext); Tokens.AND(!linenum,!column));
"||" => (column := !column + size(yytext); Tokens.OR(!linenum,!column));

"=" => (column := !column + size(yytext); Tokens.EQ(!linenum,!column));
"<>" => (column := !column + size(yytext); Tokens.NE(!linenum,!column));
"<" => (column := !column + size(yytext); Tokens.LT(!linenum,!column));
"<=" => (column := !column + size(yytext); Tokens.LE(!linenum,!column));
">" => (column := !column + size(yytext); Tokens.GT(!linenum,!column));
">=" => (column := !column + size(yytext); Tokens.GE(!linenum,!column));

":=" => (column := !column + size(yytext); Tokens.ASSIGN(!linenum,!column));



"(" => (column := !column + size(yytext); Tokens.LPAREN(!linenum,!column));
")" => (column := !column + size(yytext); Tokens.RPAREN(!linenum,!column));
"{" => (column := !column + size(yytext); Tokens.LBRACE(!linenum,!column));
"}" => (column := !column + size(yytext); Tokens.RBRACE(!linenum,!column));

";" => (column := !column + size(yytext); Tokens.SEMICOLON(!linenum,!column));
"," => (column := !column + size(yytext); Tokens.COMMA(!linenum,!column));


"(*" {vchar}+ "*)" => (column := !column + size(yytext); lex());



"+" => (column := !column + size(yytext); Tokens.ADD(!linenum,!column));
"-" => (column := !column + size(yytext); Tokens.SUB(!linenum,!column));
"*" => (column := !column + size(yytext); Tokens.MUL(!linenum,!column));
"/" => (column := !column + size(yytext); Tokens.DIV(!linenum,!column));


"~" {digit}+ "." {digit}+ "(" {digit}+ ")" => (column := !column + size(yytext); Tokens.INT(R.fromDecimal yytext ,!linenum,!column));
{digit}+ "." {digit}+ "(" {digit}+ ")" => (column := !column + size(yytext); Tokens.INT(R.fromDecimal yytext ,!linenum,!column));
["~"] {digit}+ => (column := !column + size(yytext); Tokens.INT(R.fromDecimal yytext ,!linenum,!column));
{digit}+ => (column := !column + size(yytext); Tokens.INT(R.fromDecimal yytext ,!linenum,!column));
