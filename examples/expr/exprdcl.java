abstract class exprdcl implements exprdef
{
    public final static byte rhs[] = {0,
            0,1,3,3,1,3,3,1,1,2,3
    };

    public final static short check_table[] = {
            -1,0,0,0,0,-3,0,0,0,-10,
            0,0,-11,0,0,-8,0,-9,0,-2,
            -4,-5,-6,-7,-12,-13
    };

    public final static short base_check(int i) 
    {
        return check_table[i - (NUM_RULES + 1)];
    }

    public final static char lhs[] = {0,
            4,4,3,3,3,2,2,2,1,1,1,

            1,8,33,34,31,1,8,33,35,1,
            8,36,1,8,37,1,7,1,6,22,
            26,10,14,4,17,21
    };

    public final static char base_action[] = lhs;


    public final static byte term_check[] = {0,
            0,1,0,0,1,5,6,4,0,0,7,2,3,0,1,
            0,0,4,2,3,0,0,2,3,0,0,0,0,0,8,
            5,0,0,0,0
    };

    public final static char term_action[] = {0,
            39,32,39,39,21,48,17,24,39,5,
            50,29,27,39,21,39,4,24,29,27,
            3,39,29,27,39,39,39,39,39,38,
            49
    };

    public final static char asb[] = {0,
            1,5,1,3,7,14,13,1,1,1,
            1,7,7
    };

    public final static byte asr[] = {0,
            1,6,5,0,8,0,9,8,6,5,
            2,3,7,4,1,0
    };

    public final static char nasb[] = {0,
            1,4,1,4,4,4,4,3,3,1,
            1,4,4
    };

    public final static char nasr[] = {0,
            2,0,1,0
    };

    public final static byte terminal_index[] = {0,
            4,5,6,3,7,8,9,10,11
    };

    public final static byte non_terminal_index[] = {0,
            15,14,13,12,1
    };

    public final static String name[] = { null,
            "",
            "%empty",
            "PLUS",
            "MINUS",
            "STAR",
            "SLASH",
            "NUMBER",
            "LPAREN",
            "RPAREN",
            "EOF",
            "ERROR",
            "Goal",
            "Expression",
            "Term",
            "Factor"
    };
}
