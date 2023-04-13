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

"(" => (column := !column + size(yytext); Tokens.LPAREN(!linenum,!column));
")" => (column := !column + size(yytext); Tokens.RPAREN(!linenum,!column));
"+" => (column := !column + size(yytext); Tokens.ADD(!linenum,!column));
"-" => (column := !column + size(yytext); Tokens.SUB(!linenum,!column));
"*" => (column := !column + size(yytext); Tokens.MUL(!linenum,!column));
"/" => (column := !column + size(yytext); Tokens.DIV(!linenum,!column));


"~" {digit}+ "." {digit}+ "(" {digit}+ ")" => (column := !column + size(yytext); Tokens.INT(R.fromDecimal yytext ,!linenum,!column));
{digit}+ "." {digit}* "(" {digit}+ ")" => (column := !column + size(yytext); Tokens.INT(R.fromDecimal yytext ,!linenum,!column));
["~"] {digit}+ => (column := !column + size(yytext); Tokens.INT(R.fromDecimal yytext ,!linenum,!column));
{digit}+ => (column := !column + size(yytext); Tokens.INT(R.fromDecimal yytext ,!linenum,!column));
