package org.bohao.decaf.compile

import java.io._

import org.antlr.v4.runtime._
import org.bohao.decaf.ast.ProgramNode
import org.bohao.decaf.parser.{ParserParser, ParserLexer}
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
        else if (CLI.target == CLI.Action.PARSE) {
            if (parse(CLI.infile) == null) {
                System.exit(1)
            }
            System.exit(0)
        }
    }

    def scan(fileName: String) {
        try {
            val inputStream: FileInputStream = new java.io.FileInputStream(fileName)
            val lexer = new ParserLexer(new ANTLRInputStream(inputStream))
            var done = false
            while (!done) {
                try {
                    val head = lexer.nextToken()
                    head.getType match {
                        case Token.EOF => done = true
                        case ParserLexer.ErrorChar =>
                            Console.err.println("%d:%d unrecognized %s".format(
                                head.getLine,
                                head.getCharPositionInLine,
                                head.getText))
                        case ParserLexer.UNTERMINATED_STRING =>
                            Console.err.println("%d:%d missing quotations: %s".format(
                                head.getLine,
                                head.getCharPositionInLine,
                                head.getText))
                        case ParserLexer.BADCHAR =>
                            Console.err.println("%d:%d invalid char: %s".format(
                                head.getLine,
                                head.getCharPositionInLine,
                                head.getText))
                        case _ =>
                            val tokenType = tokenMap.getOrElse(head.getType, "")
                            outFile.println("%d%s%s %s".format(
                                head.getLine,
                                if (tokenType == "") "" else " ",
                                tokenType,
                                head.getText))
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

    private def unwrap(str: String): String = {
        if (str.length < 2) ""
        else str.substring(1, str.length)
    }

    def parse(fileName: String): ProgramNode = {
        /**
          * Parse the file specified by the filename. Eventually, this method
          * may return a type specific to your compiler.
          */
        var inputStream: java.io.FileInputStream = null
        try {
            inputStream = new java.io.FileInputStream(fileName)
        } catch {
            case f: FileNotFoundException =>
                Console.err.println("File " + fileName + " does not exist")
                return null
        }
        try {
            val inputStream: FileInputStream = new java.io.FileInputStream(fileName)
            val lexer = new ParserLexer(new ANTLRInputStream(inputStream))
            val commonToken = new CommonTokenStream(lexer)
            val parser = new ParserParser(commonToken)

            val t = parser.program().ast
            t
        } catch {
            case e: Exception => Console.err.println(CLI.infile + " " + e)
                null
        }
    }
}