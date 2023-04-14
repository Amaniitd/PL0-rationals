

fun fileToLexer(filename : string) =
    let 
        val instream = TextIO.openIn filename;
        val lexer =  ParserParser.makeLexer (fn _ => TextIO.inputAll(instream))
    in
        lexer
    end