package org.bohao.decaf.compile

import java.io._

import org.antlr.v4.runtime._
import org.bohao.decaf.parser.ParserLexer
import org.bohao.decaf.util.CLI


import scala.Console

// Begin parser/scanner imports

object Compiler {
    val tokenMap = Map(
        ParserLexer.BOOLEANLITERAL -> "Bool",
        ParserLexer.INTLITERAL -> "INT_LITERAL",
        ParserLexer.IDENTIFIER -> "IDENTIFIER",
        ParserLexer.CHARLITERAL -> "CHAR_LITERAL",
        ParserLexer.STRINGLITERAL -> "STRING_LITERAL"
    )
    var outFile = if (CLI.outfile == null) Console.out
    else new java.io.PrintStream(
            new java.io.FileOutputStream(CLI.outfile))

    def main(args: Array[String]): Unit = {
        CLI.parse(args, Array[String]())
        if (CLI.target == CLI.Action.SCAN) {
            scan(CLI.infile)
            System.exit(0)
        }
//        else if (CLI.target == CLI.Action.PARSE) {
//            if (parse(CLI.infile) == null) {
//                System.exit(1)
//            }
//            System.exit(0)
//        }
    }

    def scan(fileName: String) {
        try {
            val inputStream: FileInputStream = new java.io.FileInputStream(fileName)
            val lexer = new ParserLexer(new ANTLRInputStream(inputStream))
            var done = false
            while (!done) {
                try {
                    val head = lexer.nextToken()
                    if (head.getType == Token.EOF) {
                        done = true
                    } else {
                        val tokenType = tokenMap.getOrElse(head.getType, "")
                        outFile.println(head.getLine + (if (tokenType == "") "" else " ") + tokenType + " " + head.getText)
                    }
                } catch {
                    case ex: Exception =>
                        Console.out.println(CLI.infile + " " + ex)
                        //                        lexer.emit()
                }
            }
        } catch {
            case ex: Exception => Console.err.println(ex)
        }
    }

//    def parse(fileName: String): CommonAST = {
//        /**
//          * Parse the file specified by the filename. Eventually, this method
//          * may return a type specific to your compiler.
//          */
//        var inputStream: java.io.FileInputStream = null
//        try {
//            inputStream = new java.io.FileInputStream(fileName)
//        } catch {
//            case f: FileNotFoundException => {
//                Console.err.println("File " + fileName + " does not exist"); return null
//            }
//        }
//        try {
//            val scanner = new DecafScanner(new DataInputStream(inputStream))
//            val parser = new DecafParser(scanner);
//
//            parser.setTrace(CLI.debug)
//            parser.program()
//            val t = parser.getAST().asInstanceOf[CommonAST]
//            if (parser.getError()) {
//                print("[ERROR] Parse failed\n")
//                return null
//            } else if (CLI.debug) {
//                print(t.toStringList())
//            }
//            t
//        } catch {
//            case e: Exception => Console.err.println(CLI.infile + " " + e)
//                null
//        }
//    }
}
