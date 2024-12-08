abstract class bnfdcl implements bnfdef
{
    public final static byte rhs[] = {0,
            0,2,1,3,3,0,2
    };

    public final static short check_table[] = {
            -2,-5,0,0,0,-1,-3,0,-6,0,
            -4,-7,-8
    };

    public final static short base_check(int i) 
    {
        return check_table[i - (NUM_RULES + 1)];
    }

    public final static char lhs[] = {0,
            2,2,3,3,4,1,1,

            9,21,19,14,3,17,11,8,21,20,
            14,15,19
    };

    public final static char base_action[] = lhs;


    public final static byte term_check[] = {0,
            0,1,2,3,0,1,2,3,0,1,0,3,2,0,0,
            1,0,4,0,1,0,0,0,0,0,0
    };

    public final static char term_action[] = {0,
            4,29,29,29,5,29,29,29,22,18,
            2,21,9,22,4,30,1,16,5,34,
            6
    };

    public final static char asb[] = {0,
            2,2,1,5,1,1,1,1
    };

    public final static byte asr[] = {0,
            2,3,1,0,4,0
    };

    public final static char nasb[] = {0,
            5,1,4,4,3,3,4,4
    };

    public final static char nasr[] = {0,
            3,0,1,0,2,0
    };

    public final static byte terminal_index[] = {0,
            3,5,6,4,7
    };

    public final static byte non_terminal_index[] = {0,
            11,8,9,10,1
    };

    public final static String name[] = { null,
            "",
            "%empty",
            "SYMBOL",
            "PRODUCES",
            "OR",
            "EOF",
            "ERROR",
            "bnf",
            "rules",
            "rule",
            "symbol_list"
    };
}
