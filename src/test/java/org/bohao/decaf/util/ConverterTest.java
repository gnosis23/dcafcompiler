package org.bohao.decaf.util;

import org.junit.Assert;
import org.junit.Test;


/**
 *
 * Created by bohao on 2016/8/24.
 */
public class ConverterTest {

    @Test
    public void testToChar() throws Exception {
        Assert.assertEquals('\\', Converter.toChar("\\"));
        Assert.assertEquals('\'', Converter.toChar("\\\'"));
        Assert.assertEquals('\"', Converter.toChar("\\\""));
        Assert.assertEquals('\\', Converter.toChar("\\\\"));
        Assert.assertEquals('\t', Converter.toChar("\\\t"));
        Assert.assertEquals('\n', Converter.toChar("\\\n"));
        Assert.assertEquals('\r', Converter.toChar("\\\r"));
        Assert.assertEquals('!', Converter.toChar("!"));
        Assert.assertEquals(' ', Converter.toChar(" "));
    }

    @Test
    public void testToBool() throws Exception {
        Assert.assertEquals(true, Converter.toBool("true"));
        Assert.assertEquals(false, Converter.toBool("false"));
    }

    @Test
    public void testToInt() throws Exception {
        Assert.assertEquals(1234, Converter.toInt("01234"));
        Assert.assertEquals(0xff, Converter.toInt("0xff"));
    }

    @Test
    public void testToString() throws Exception {
        Assert.assertEquals("%s\n", Converter.toString("\"%s\\\n\""));
        Assert.assertEquals(" ", Converter.toString("\" \""));
        Assert.assertEquals("", Converter.toString("\"\""));
    }
}