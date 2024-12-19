%Options LALR=2
%Options GENERATEPARSER=java
%Options ACTFILENAME=bnfact.java
%Options HACTFILENAME=bnfhdr.java
%Options TABLE=space
%Options FILEPREFIX=bnf
%Options PREFIX=TK_
%Options NOGOTODEFAULT

-- This grammar has been augmented with productions that captures
-- most errors that a user is likely to make. This saves the need
-- to have an error recovery system.

%Define

-- This macro is used to initialize the rule_action array
-- to the null_action function.
%null_action
/.
        new NullAction(),
./
 
-- This macro is used to initialize the rule_action array
-- to the no_action function.
%no_action
/.
        new NoAction(),
./
 
%Terminals

    SYMBOL PRODUCES OR EOF ERROR

%Alias

    '::='  ::= PRODUCES
    '|'    ::= OR
    %EOF   ::= EOF
    %ERROR ::= ERROR

%Rules
/:
class bnfhdr extends bnfact
{
    Action rule_action[] = {
        null, // no element 0
:/

/.
class bnfact
{
    Parser parser;

    bnfact(Parser parser) {
        this.parser = parser;
    }

    void print_rule(int rule_no) {
        String rule = new String();
        rule = parser.name[parser.non_terminal_index[parser.lhs[rule_no]]] + " ::=";
        if (parser.rhs[rule_no] == 0) {
            rule += " %empty";
        } else {
            for (int i = 1; i <= parser.rhs[rule_no]; i++) {
                int non_term = parser.SYM(i),
                    term = parser.lex_stream.Kind(parser.TOKEN(i));
                rule += (" " + (non_term == 0 ? parser.name[parser.terminal_index[term]]
                                              : parser.name[parser.non_terminal_index[non_term]]));
            }
        }
        System.out.println("Reducing rule number " + rule_no + ": " + rule);
        return;
    }

    interface Action {
        public void action();
    }

    final class NoAction implements Action {
        public void action() {}
    }

    final class NullAction implements Action {
        public void action()
        {
            System.out.println("A null production");
        }
    }
./

bnf ::= %empty
  /:        new act%rule_number(),:/
  /.
    // Rule %rule_number:  %rule_text
    final class act%rule_number implements Action {
        public void action() {
            print_rule(%rule_number);
            parser.setSYM1((int) parser.lhs[%rule_number]);
        }
    }
  ./

bnf ::= bnf rules
  /:        new act%rule_number(),:/
  /.
    // 
    // Rule %rule_number:  %rule_text
    //
    final class act%rule_number implements Action {
        public void action() {
            print_rule(%rule_number);
            parser.setSYM1((int) parser.lhs[%rule_number]);
        }
    }
  ./

rules ::= rule
  /:        new act%rule_number(),:/
  /.
    // Rule %rule_number:  %rule_text
    final class act%rule_number implements Action {
        public void action() {
            print_rule(%rule_number);
            parser.setSYM1((int) parser.lhs[%rule_number]);
        }
    }
  ./

rules ::= rules '|' symbol_list
  /:        new act%rule_number(),:/
  /.
    // Rule %rule_number:  %rule_text
    final class act%rule_number implements Action {
        public void action() {
            print_rule(%rule_number);
            parser.setSYM1((int) parser.lhs[%rule_number]);
        }
    }
  ./

rule ::= SYMBOL '::=' symbol_list
  /:        new act%rule_number(),:/
  /.
    // Rule %rule_number:  %rule_text
    final class act%rule_number implements Action {
        public void action() {
            print_rule(%rule_number);
            parser.setSYM1((int) parser.lhs[%rule_number]);
        }
    }
  ./

symbol_list ::= %empty
  /:        new act%rule_number(),:/
  /.
    // Rule %rule_number:  %rule_text
    final class act%rule_number implements Action {
        public void action() {
            print_rule(%rule_number);
            parser.setSYM1((int) parser.lhs[%rule_number]);
        }
    }
  ./

symbol_list ::= symbol_list SYMBOL
  /:        new act%rule_number(),:/
  /.
    // Rule %rule_number:  %rule_text
    final class act%rule_number implements Action {
        public void action() {
            print_rule(%rule_number);
            parser.setSYM1((int) parser.lhs[%rule_number]);
        }
    }
  ./

/:
    };

    bnfhdr(Parser parser) {
        super(parser);
    }
}
:/

/.
}
./
%End
