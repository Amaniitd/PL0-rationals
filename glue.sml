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



(* sml like comment *)

fun removeComments (str:string) =
    let
        val commentCount = ref 0
        val output = ref ""
        val chars = String.explode str
        fun head (c::cs) = c
        |  head [] = #" "
        fun tail (c::cs) = cs
        fun tail [] = []
        fun  loop [] = ()
        |  loop((#"(")::(#"*")::cs) = (commentCount := !commentCount + 1; loop(cs))
        |  loop((#"*")::(#")")::cs) = (commentCount := !commentCount - 1; loop(cs))
        |  loop(x::xs) = (if (!commentCount = 0) then output := !output ^ (String.implode [x]) else (); loop(xs))


    in
        loop(chars);
        !output
    end



fun stringToLexer s =
    let val done = ref false
        val str = removeComments s
    	val lexer=  ParserParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	

fun readFile(filename: string) : string = 
    let
        val stream = TextIO.openIn filename
        val output = ref ""
        val done = ref false
    in
        while not(!done) do
            (case TextIO.inputLine(stream) of
                NONE => done := true
            |   SOME line => output := !output ^ line 
            )
        ;
        TextIO.closeIn stream;
        !output
    end


fun fileToLexer(filename : string) = stringToLexer (readFile filename)

		
fun parse (lexer) =
    let val dummyEOF = ParserLrVals.Tokens.EOF(0,0)
    	val (result:AST.Block, lexer) = invoke lexer
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
