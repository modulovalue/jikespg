class exprhdr extends expract
{
    Action rule_action[] = {
        null, // no element 0
        new NullAction(),

        new NoAction(),

        new act3(),
        new act4(),
        new NoAction(),

        new act6(),
        new act7(),
        new NoAction(),

        new act9(),
        new act10(),
        new act11(),
    };

    exprhdr(Parser parser)
    {
        super(parser);
    }
}
