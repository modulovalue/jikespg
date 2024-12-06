class expract
{
    Parser parser;

    expract(Parser parser)
    {
        this.parser = parser;
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
        public void action() { parser.setSYM1(null); }
    }

    // 
    // Rule 3:  Expression ::= Expression PLUS Term
    //
    final class act3 implements Action
    {
        public void action()
        {
            AstPlus node = new AstPlus();
            node.left  = parser.SYM(1);
            node.op    = parser.TOKEN(2);
            node.right = parser.SYM(3);

            parser.setSYM1(node);
            return;
        }
    }
  
    // 
    // Rule 4:  Expression ::= Expression MINUS Term
    //
    final class act4 implements Action
    {
        public void action()
        {
            AstMinus node = new AstMinus();
            node.left  = parser.SYM(1);
            node.op    = parser.TOKEN(2);
            node.right = parser.SYM(3);

            parser.setSYM1(node);
        }
    }
  
    // 
    // Rule 6:  Term ::= Term STAR Factor
    //
    final class act6 implements Action
    {
        public void action()
        {
            AstStar node = new AstStar();
            node.left  = parser.SYM(1);
            node.op    = parser.TOKEN(2);
            node.right = parser.SYM(3);

            parser.setSYM1(node);
        }
    }
  
    // 
    // Rule 7:  Term ::= Term SLASH Factor
    //
    final class act7 implements Action
    {
        public void action()
        {
            AstSlash node = new AstSlash();
            node.left  = parser.SYM(1);
            node.op    = parser.TOKEN(2);
            node.right = parser.SYM(3);

            parser.setSYM1(node);
        }
    }
  
    // 
    // Rule 9:  Factor ::= NUMBER
    //
    final class act9 implements Action
    {
        public void action()
        {
            AstNumber node = new AstNumber();
            node.token = parser.TOKEN(1);
            node.value = Integer.parseInt(parser.lex_stream.Name(node.token));

            parser.setSYM1(node);
        }
    }
  
    // 
    // Rule 10:  Factor ::= MINUS NUMBER
    //
    final class act10 implements Action
    {
        public void action()
        {
            AstNegativeNumber node = new AstNegativeNumber();
            node.op = parser.TOKEN(1);
            node.token = parser.TOKEN(2);
            node.value = - Integer.parseInt(parser.lex_stream.Name(node.token));

            parser.setSYM1(node);
        }
    }
  
    // 
    // Rule 11:  Factor ::= LPAREN Expression RPAREN
    //
    final class act11 implements Action
    {
        public void action()
        {
            AstParen node = new AstParen();
            node.expression = parser.SYM(2);

            parser.setSYM1(node);
        }
    }
  
}
