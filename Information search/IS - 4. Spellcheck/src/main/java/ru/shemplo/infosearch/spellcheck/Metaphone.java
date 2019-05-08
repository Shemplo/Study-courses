package ru.shemplo.infosearch.spellcheck;

import static java.util.regex.Pattern.*;

import java.util.regex.Pattern;

public class Metaphone {
    
    public static void main (String ... args) {
        System.out.println (encodeRU ("Шворцинегир".toUpperCase ()));
    }
    
    private static final Pattern REPEATED_EXEPT_C = compile ("([^c])\\1", CASE_INSENSITIVE);
    private static final Pattern WORD_START_1     = compile ("^[kgp](n)", CASE_INSENSITIVE);
    private static final Pattern WORD_START_2     = compile ("^a(e)", CASE_INSENSITIVE);
    private static final Pattern WORD_START_3     = compile ("^w(r)", CASE_INSENSITIVE);
    private static final Pattern WORD_END         = compile ("mb$", CASE_INSENSITIVE);
    private static final Pattern C_RULES_1        = compile ("sch", CASE_INSENSITIVE);
    private static final Pattern C_RULES_2        = compile ("c(ia|h)", CASE_INSENSITIVE);
    private static final Pattern C_RULES_3        = compile ("c([iey])", CASE_INSENSITIVE);
    private static final Pattern D_RULES_1        = compile ("d(ge|gy)", CASE_INSENSITIVE);
    private static final Pattern D_RULES_2        = compile ("dgi", CASE_INSENSITIVE);
    private static final Pattern GH_RULE          = compile ("([^aeiou])GH(.)", CASE_INSENSITIVE);
    private static final Pattern GN_RULES         = compile ("g(n|ned)$", CASE_INSENSITIVE);
    private static final Pattern G_RULES          = compile ("g([iey])", CASE_INSENSITIVE);
    private static final Pattern H_RULES          = compile ("([aeiou])H([^aeiou])", CASE_INSENSITIVE);
    private static final Pattern S_RULES          = compile ("s(h|io|ia)", CASE_INSENSITIVE);
    private static final Pattern T_RULES          = compile ("t(io|ia)", CASE_INSENSITIVE);
    private static final Pattern W_RULE_1         = compile ("^wh([^aeiou])", CASE_INSENSITIVE);
    private static final Pattern W_RULE_2         = compile ("^wh([aeiou])", CASE_INSENSITIVE);
    private static final Pattern X_RULE           = compile ("^x", CASE_INSENSITIVE);
    private static final Pattern Y_RULE           = compile ("y([^aeiou])", CASE_INSENSITIVE);
    private static final Pattern VOWELS_RULE      = compile ("(.)[aeiou]", CASE_INSENSITIVE);
    
    public static String encodeEN (String word) {
        word = REPEATED_EXEPT_C.matcher (word).replaceAll ("$1");
        word = WORD_START_1.matcher (word).replaceAll ("$1");
        word = WORD_START_2.matcher (word).replaceAll ("$1");
        word = WORD_START_3.matcher (word).replaceAll ("$1");
        word = WORD_END.matcher (word).replaceAll ("M");
        word = C_RULES_1.matcher (word).replaceAll ("SKH");
        word = C_RULES_2.matcher (word).replaceAll ("X$1");
        word = C_RULES_3.matcher (word).replaceAll ("S$1");
        word = word.replace ('C', 'K');
        word = D_RULES_1.matcher (word).replaceAll ("J$1");
        word = D_RULES_2.matcher (word).replaceAll ("JGY");
        word = word.replace ('D', 'T');
        word = GH_RULE.matcher (word).replaceAll ("$1H$2");
        word = GN_RULES.matcher (word).replaceAll ("$1");
        word = word.replace ('G', 'K');
        word = G_RULES.matcher (word).replaceAll ("$1");
        word = H_RULES.matcher (word).replaceAll ("$1$2");
        word = word.replace ("CK", "K").replace ("PH", "F");
        word = word.replace ("Q", "K").replace ("V", "F");
        word = word.replace ("Z", "S");
        word = S_RULES.matcher (word).replaceAll ("X$1");
        word = T_RULES.matcher (word).replaceAll ("T$1");
        word = W_RULE_1.matcher (word).replaceAll ("$1");
        word = W_RULE_2.matcher (word).replaceAll ("W$1");
        word = word.replace ("TH", "0").replace ("TCH", "CH");
        word = X_RULE.matcher (word).replaceAll ("S");
        word = word.replace ("X", "KS");
        word = Y_RULE.matcher (word).replaceAll ("$1");
        word = VOWELS_RULE.matcher (word).replaceAll ("$1");
        return word;
    }
    
    private static final Pattern RU_VOWELS_RULE_1 = compile ("(йо|ио|йе|ие)", CASE_INSENSITIVE | UNICODE_CHARACTER_CLASS);
    private static final Pattern RU_VOWELS_RULE_2 = compile ("(о|ы|я)", CASE_INSENSITIVE | UNICODE_CHARACTER_CLASS);
    private static final Pattern RU_VOWELS_RULE_3 = compile ("(е|ё|э)", CASE_INSENSITIVE | UNICODE_CHARACTER_CLASS);
    private static final Pattern RU_B_RULE = compile ("б([^млнуеыаоэяию])", CASE_INSENSITIVE | UNICODE_CHARACTER_CLASS);
    private static final Pattern RU_Z_RULE = compile ("з([^млнуеыаоэяию])", CASE_INSENSITIVE | UNICODE_CHARACTER_CLASS);
    private static final Pattern RU_D_RULE = compile ("д([^млнуеыаоэяию])", CASE_INSENSITIVE | UNICODE_CHARACTER_CLASS);
    private static final Pattern RU_V_RULE = compile ("в([^млнуеыаоэяию])", CASE_INSENSITIVE | UNICODE_CHARACTER_CLASS);
    private static final Pattern RU_G_RULE = compile ("г([^млнуеыаоэяию])", CASE_INSENSITIVE | UNICODE_CHARACTER_CLASS);
    
    public static String encodeRU (String word) {
        word = RU_VOWELS_RULE_1.matcher (word).replaceAll ("И");
        word = RU_VOWELS_RULE_2.matcher (word).replaceAll ("А");
        word = RU_VOWELS_RULE_3.matcher (word).replaceAll ("И");
        word = word.replace ("Ю", "У");
        word = RU_B_RULE.matcher (word).replaceAll ("П$1");
        word = word.replaceAll ("Б$", "П");
        word = RU_Z_RULE.matcher (word).replaceAll ("С$1");
        word = word.replaceAll ("З$", "С");
        word = RU_D_RULE.matcher (word).replaceAll ("Т$1");
        word = word.replaceAll ("Д$", "Т");
        word = RU_V_RULE.matcher (word).replaceAll ("Ф$1");
        word = word.replaceAll ("В$", "Ф");
        word = RU_G_RULE.matcher (word).replaceAll ("К$1");
        word = word.replaceAll ("Г$", "К");
        word = word.replaceAll ("(ТС|ДС)", "Ц");
        return word;
    }
    
}
