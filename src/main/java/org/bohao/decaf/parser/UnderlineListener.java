package org.bohao.decaf.parser;

import org.antlr.v4.runtime.*;

/**
 *
 * Created by bohao on 2016/8/10.
 */
public class UnderlineListener extends BaseErrorListener {
    protected int count = 0;

    public int getCount() {
        return count;
    }

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol,
                            int line,
                            int charPositionInLine,
                            String msg,
                            RecognitionException e) {
        count++;
        System.err.println("line " + line + ":" + charPositionInLine + " " + msg);
        underlineError(recognizer, (Token) offendingSymbol,
                       line, charPositionInLine);
    }

    protected void underlineError(Recognizer recognizer,
                                  Token offendingToken, int line,
                                  int charPositionInLine) {
        CommonTokenStream tokens =
                (CommonTokenStream) recognizer.getInputStream();
        String input = tokens.getTokenSource().getInputStream().toString();
        String[] lines = input.split("\n");
        String errorLine = lines[line - 1];
        System.err.println(errorLine);
        for (int i = 0; i < charPositionInLine; i++)
            System.err.print(" ");

        int start = offendingToken.getStartIndex();
        int stop = offendingToken.getStopIndex();
        if (start >= 0 && stop >= 0) {
            for (int i = start; i <= stop; i++) System.err.print("^");
        }
        System.err.println();
    }
}