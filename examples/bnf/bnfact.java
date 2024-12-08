class bnfact
{
    Parser parser;

    bnfact(Parser parser)
    {
        this.parser = parser;
    }

    void print_rule(int rule_no)
    {
        String rule = new String();
        rule = parser.name[parser.non_terminal_index[parser.lhs[rule_no]]] + " ::=";
        if (parser.rhs[rule_no] == 0)
            rule += " %empty";
        else
        {
            for (int i = 1; i <= parser.rhs[rule_no]; i++)
            {
                int non_term = parser.SYM(i),
                    term = parser.lex_stream.Kind(parser.TOKEN(i));
                rule += (" " + (non_term == 0 ? parser.name[parser.terminal_index[term]]
                                              : parser.name[parser.non_terminal_index[non_term]]));
            }
        }

        System.out.println("Reducing rule number " + rule_no + ": " + rule);

        return;
    }

    interface Action
    {
        public void action();
    }

    final class NoAction implements Action
    {
        public void action() {}
    }

    final class NullAction implements Action
    {
        public void action()
        {
            System.out.println("A null production");
        }
    }
    // 
    // Rule 1:  bnf ::=
    //
    final class act1 implements Action
    {
        public void action()
        {
            print_rule(1);
            parser.setSYM1((int) parser.lhs[1]);
        }
    }
  
    // 
    // Rule 2:  bnf ::= bnf rules
    //
    final class act2 implements Action
    {
        public void action()
        {
            print_rule(2);
            parser.setSYM1((int) parser.lhs[2]);
        }
    }
  
    // 
    // Rule 3:  rules ::= rule
    //
    final class act3 implements Action
    {
        public void action()
        {
            print_rule(3);
            parser.setSYM1((int) parser.lhs[3]);
        }
    }
  
    // 
    // Rule 4:  rules ::= rules OR symbol_list
    //
    final class act4 implements Action
    {
        public void action()
        {
            print_rule(4);
            parser.setSYM1((int) parser.lhs[4]);
        }
    }
  
    // 
    // Rule 5:  rule ::= SYMBOL PRODUCES symbol_list
    //
    final class act5 implements Action
    {
        public void action()
        {
            print_rule(5);
            parser.setSYM1((int) parser.lhs[5]);
        }
    }
  
    // 
    // Rule 6:  symbol_list ::=
    //
    final class act6 implements Action
    {
        public void action()
        {
            print_rule(6);
            parser.setSYM1((int) parser.lhs[6]);
        }
    }
  
    // 
    // Rule 7:  symbol_list ::= symbol_list SYMBOL
    //
    final class act7 implements Action
    {
        public void action()
        {
            print_rule(7);
            parser.setSYM1((int) parser.lhs[7]);
        }
    }
  
}
