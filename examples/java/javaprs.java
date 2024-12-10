abstract class javaprs extends javadcl implements javadef
{
    public final static int original_state(int state) { return -base_check(state); }
    public final static int asi(int state) { return asb[original_state(state)]; }
    static int nasi(int state) { return nasb[original_state(state)]; }
    public final static int in_symbol(int state) { return in_symb[original_state(state)]; }

    public final static int nt_action(int state, int sym)
    {
        return base_action[state + sym];
    }

    public final static int t_action(int state, int sym, LexStream stream)
    {
        return term_action[term_check[base_action[state]+sym] == sym
                               ? base_action[state] + sym
                               : base_action[state]];
    }
}
