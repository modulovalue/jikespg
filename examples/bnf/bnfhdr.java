class bnfhdr extends bnfact
{
    Action rule_action[] = {
        null, // no element 0
        new act1(),
        new act2(),
        new act3(),
        new act4(),
        new act5(),
        new act6(),
        new act7(),
    };

    bnfhdr(Parser parser) {
        super(parser);
    }
}
