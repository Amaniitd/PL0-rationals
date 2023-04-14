structure ParserLrVals = ParserLrValsFun(structure Token = LrParser.Token);
structure AssignLex = AssignLexFun(structure Tokens = ParserLrVals.Tokens);
structure ParserParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = ParserLrVals.ParserData
     	       structure Lex = AssignLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Error, line " ^ (Int.toString pos) ^ "," ^ s ^ "\n")
		in
		    ParserParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  ParserParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	

fun fileToLexer(filename : string) =
    let 
        val instream = TextIO.openIn filename;
        val lexer =  ParserParser.makeLexer (fn _ => TextIO.inputAll(instream))
    in
        lexer
    end
		
fun parse (lexer) =
    let val dummyEOF = ParserLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	    val (nextToken, lexer) = ParserParser.Stream.get lexer
    in
        if ParserParser.sameToken(nextToken, dummyEOF) then result
 	    else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end


val parseString = parse o stringToLexer
val parseFile = parse o fileToLexer
fun evaluateString str = "jo"

(* cli interface to continuously take input and output the result on pressing enter *)

fun cli() = 
    (print("Enter an expression to evaluate, or 'quit' to exit: ");
    let
    val input = TextIO.inputLine(TextIO.stdIn);
    in
    case input of
        NONE => ()
    |   SOME "quit" => ()
    |  SOME str => (print( " = " ^ evaluateString str ^ " \n"); cli())
    end
    )        
