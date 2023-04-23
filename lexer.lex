structure Tokens=Tokens

	type pos = int
	type pol = int
	type svalue = Tokens.svalue
	type ('a,'b) token = ('a,'b) Tokens.token
	type lexresult = (svalue, pos) token

	val linenum = ref 1;
	val column = ref 1;
	val eof = fn() => Tokens.EOF(!column,!column)

fun fromMakeRat s = 
	(* make_rat(a,b) call R.make_rat(a, b) *)
	(* first remove "make_rat(" *)
	let val s = String.substring(s, 9, String.size (s) - 10)
	in
		(* then split on "," *)
		let val x = String.tokens (fn c => c = #"," ) s
			val a = hd x
			val b = hd (tl x)
		in
			valOf(R.make_rat (Bigint.make_bigint a, Bigint.make_bigint b))
		end
	end

fun fromDecimalHelper s = 
	(* fromDecimal(a) call R.fromDecimal(a) *)
	(* first remove "fromDecimal(" *)
	let val s = String.substring(s, 12, String.size (s) - 13)
	in
		R.fromDecimal(s)
	end

%%

%header (functor AssignLexFun(structure Tokens:Parser_TOKENS));
alpha = [A-Za-z];
wh = [\ \t];
digit = [0-9];
vchar = [0-9A-Za-z];

%%
\n => (linenum := !linenum + 1; column := 1; lex());
{wh}+ => (column := !column + size(yytext); lex());


"tt" => (column := !column + size(yytext); Tokens.TT(!linenum,!column));
"ff" => (column := !column + size(yytext); Tokens.FF(!linenum,!column));

"||" => (column := !column + size(yytext); Tokens.OR(!linenum,!column));
"&&" => (column := !column + size(yytext); Tokens.AND(!linenum,!column));
"!" => (column := !column + size(yytext); Tokens.NOT(!linenum,!column));
":=" => (column := !column + size(yytext); Tokens.ASSIGN(!linenum,!column));
"=" => (column := !column + size(yytext); Tokens.EQ(!linenum,!column));
"<>" => (column := !column + size(yytext); Tokens.NEQ(!linenum,!column));
"<" => (column := !column + size(yytext); Tokens.LT(!linenum,!column));
"<=" => (column := !column + size(yytext); Tokens.LEQ(!linenum,!column));
">" => (column := !column + size(yytext); Tokens.GT(!linenum,!column));
">=" => (column := !column + size(yytext); Tokens.GEQ(!linenum,!column));

"call" => (column := !column + size(yytext); Tokens.CALL(!linenum,!column));
"read" => (column := !column + size(yytext); Tokens.READ(!linenum,!column));
"print" => (column := !column + size(yytext); Tokens.PRINT(!linenum,!column));

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

"procedure" => (column := !column + size(yytext); Tokens.PROCEDURE(!linenum,!column));


"inverse" => (column := !column + size(yytext); Tokens.INVERSE(!linenum,!column));

";" => (column := !column + size(yytext); Tokens.SEMICOLON(!linenum,!column));
"," => (column := !column + size(yytext); Tokens.COMMA(!linenum,!column));

".*." => (column := !column + size(yytext); Tokens.MUL(!linenum,!column));
"./." => (column := !column + size(yytext); Tokens.DIV(!linenum,!column));
"%" => (column := !column + size(yytext); Tokens.MOD(!linenum,!column));

"{" => (column := !column + size(yytext); Tokens.LBRACE(!linenum,!column));
"}" => (column := !column + size(yytext); Tokens.RBRACE(!linenum,!column));
"(" => (column := !column + size(yytext); Tokens.LPAREN(!linenum,!column));
")" => (column := !column + size(yytext); Tokens.RPAREN(!linenum,!column));
"+" => (column := !column + size(yytext); Tokens.ADD(!linenum,!column));
"-" => (column := !column + size(yytext); Tokens.SUB(!linenum,!column));
"*" => (column := !column + size(yytext); Tokens.MUL(!linenum,!column));
"/" => (column := !column + size(yytext); Tokens.DIV(!linenum,!column));

"make_rat" => (column := !column + size(yytext); Tokens.MAKERAT(!linenum,!column));


"fromDecimal" {wh}* "(" {wh}* "~" {digit}+ "." {digit}* "(" {digit}+ ")" {wh}* ")" => (column := !column + size(yytext); Tokens.RAT(fromDecimalHelper yytext ,!linenum,!column));
"fromDecimal" {wh}* "(" {wh}* {digit}+ "." {digit}* "(" {digit}+ ")" {wh}* ")" => (column := !column + size(yytext); Tokens.RAT(fromDecimalHelper yytext ,!linenum,!column));
["~"] {digit}+ => (column := !column + size(yytext); Tokens.INT(Bigint.make_bigint yytext,!linenum,!column));
{digit}+ => (column := !column + size(yytext); Tokens.INT(Bigint.make_bigint yytext ,!linenum,!column));

{alpha}+ {vchar}* => (column := !column + size(yytext); Tokens.ID(yytext,!linenum,!column));
