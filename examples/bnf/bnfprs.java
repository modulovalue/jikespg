abstract class bnfprs extends bnfdcl implements bnfdef
{
    public final static int original_state(int state) { return -base_check(state); }
    public final static int asi(int state) { return asb[original_state(state)]; }
    static int nasi(int state) { return nasb[original_state(state)]; }

    public final static int nt_action(int state, int sym)
    {
        return base_action[state + sym];
    }

    public final static int t_action(int act, int sym, LexStream stream)
    {
        act = base_action[act];
        int i = act + sym;

        act = term_action[term_check[i] == sym ? i : act];

        if (act > LA_STATE_OFFSET)
        {
            for (int tok = stream.Peek();
                 ;
                 tok = stream.Next(tok))
            {
               act -= LA_STATE_OFFSET;
               sym = stream.Kind(tok);
               i = act + sym;
               act = term_action[term_check[i] == sym ? i : act];
               if (act <= LA_STATE_OFFSET)
                   break;
            } 
        }

        return act;
    }
}
