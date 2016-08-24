package org.bohao.decaf.util;

/**
 *
 * Created by bohao on 2016/8/24.
 */
public class Converter {
    public static char toChar(String text) {
        if (text.length() >= 2 && text.startsWith("\\")) {
            char ch = text.charAt(1);
            switch (ch) {
                case 't':
                    return '\t';
                case 'n':
                    return '\n';
                case 'r':
                    return '\r';
                default:
                    return ch;
            }
        }
        return text.charAt(0);
    }

    public static boolean toBool(String text) {
        return text.equals("true");
    }

    public static int toInt(String text) {
        if (text.startsWith("0x")) {
            return Integer.valueOf(text.substring(2), 16);
        } else {
            return Integer.valueOf(text);
        }
    }
}
