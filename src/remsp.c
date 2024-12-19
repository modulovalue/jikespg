#include <stdlib.h>
#include "common.h"

struct SPData {
  int top;
  int rule_root;
};

struct remsp_action_element {
  struct remsp_action_element *next;
  int symbol;
  int action;
};

struct sp_state_element {
  struct sp_state_element *next;
  struct sp_state_element *link;
  struct remsp_action_element *action_root;
  struct node *rule_root;
  struct node *complete_items;
  short state_number;
  short rule_count;
  short action_count;
};

struct update_action_element {
  struct update_action_element *next;
  short symbol;
  int action;
  short state;
};

struct AEPool {
  struct remsp_action_element *action_element_pool;
};

struct StateMap {
  int max_sp_state;
  struct sp_state_element *sp_state_root;
};

static bool IS_SP_RHS(const long symbol, ArrayShort sp_rules) {
  return sp_rules.raw[symbol] != NIL;
}

static bool IS_SP_RULE(const long rule_no, ArrayShort rule_list) {
  return rule_list.raw[rule_no] != OMEGA;
}

/// This function first tries to recycle an action_element node from a
/// free list. If the list is empty a new node is allocated from
/// temporary storage.
static struct remsp_action_element *allocate_action_element(struct AEPool* pool) {
  struct remsp_action_element *p = pool->action_element_pool;
  if (p != NULL) {
    pool->action_element_pool = p->next;
  } else {
    talloc0p(&p, struct remsp_action_element);
  }
  return p;
}

/// This routine returns a list of action_element structures to the
/// free list.
static void free_action_elements(struct remsp_action_element *head, struct remsp_action_element *tail, struct AEPool* pool) {
  tail->next = pool->action_element_pool;
  pool->action_element_pool = head;
}

/// Compute_sp_map is an instantiation of the digraph algorithm. It
/// is invoked repeatedly by remove_single_productions to:
///
///   1) Partially order the right-hand side of all the single
///      productions (SP) into a list [A1, A2, A3, ..., An]
///      such that if Ai -> Aj then i < j.
///
///   2) As a side effect, it uses the ordering above to order all
///      the SP rules.
static void compute_sp_map(const int symbol, struct SPData *spd, ArrayShort sp_rules, ArrayShort rule_list, ArrayShort index_of, ArrayShort next_rule, ArrayShort stack, struct ruletab_type *rules) {
  stack.raw[++spd->top] = symbol;
  const int indx = spd->top;
  index_of.raw[symbol] = indx;
  // In this instantiation of the digraph algorithm, two symbols (A, B)
  // are related if  A -> B is an SP and A is the right-hand side of
  // some other SP rule.
  for (int rule_no = sp_rules.raw[symbol]; rule_no != NIL; rule_no = next_rule.raw[rule_no]) {
    const int lhs_symbol = rules[rule_no].lhs;
    if (IS_SP_RHS(lhs_symbol, sp_rules)) {
      if (index_of.raw[lhs_symbol] == OMEGA) {
        compute_sp_map(lhs_symbol, spd, sp_rules, rule_list, index_of, next_rule, stack, rules);
      }
      index_of.raw[symbol] = MIN(index_of.raw[symbol], index_of.raw[lhs_symbol]);
    }
  }
  // If the index of symbol is the same index it started with then
  // symbol if the root of a SCC...
  if (index_of.raw[symbol] == indx) {
    // If symbol is on top of the stack then it is the only
    // symbol in its SCC (thus it is not part of a cycle).
    // Note that since remove_single_productions is only invoked
    // when the input grammar is conflict-free, the graph of
    // the single productions will never contain any cycle.
    // Thus, this test will always succeed and all single
    // productions associated with the symbol being processed
    // are added to the list of SP rules here...
    if (stack.raw[spd->top] == symbol) {
      for (int rule_no = sp_rules.raw[symbol]; rule_no != NIL; rule_no = next_rule.raw[rule_no]) {
        if (spd->rule_root == NIL) {
          rule_list.raw[rule_no] = rule_no;
        } else {
          rule_list.raw[rule_no] = rule_list.raw[spd->rule_root];
          rule_list.raw[spd->rule_root] = rule_no;
        }
        spd->rule_root = rule_no;
      }
    }
    // As all SCC contains exactly one symbol (as explained above)
    // this loop will always execute exactly once.
    int i;
    do {
      i = stack.raw[spd->top--];
      index_of.raw[i] = INFINITY;
    } while (i != symbol);
  }
}

/// When the parser enters STATE_NO, and it is processing SYMBOL, its
/// next move is ACTION. Given these 3 parameters, compute_sp_action
/// computes the set of reduce actions that may be executed after
/// SYMBOL is shifted in STATE_NO.
///
/// NOTE that this algorithm works only for the LALR(1) case. When the
/// transition on SYMBOL is a lookahead-shift, indicating that the
/// parser requires extra lookahead on a particular symbol, the set of
/// reduce actions for that symbol is calculated as the empty set.
static void compute_sp_action(const int state_no, const short symbol, const short action, JBitset look_ahead, bool *is_conflict_symbol, ArrayShort index_of, ArrayShort* sp_action, struct StackRoot* sr, JBitset first, struct LAIndex* lai, struct node **conflict_symbols, JBitset la_set, struct node **adequate_item, struct node **in_stat, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table) {
  const struct goto_header_type go_to = statset[state_no].go_to;
  if (sp_action[symbol].raw == NULL) {
    sp_action[symbol] = Allocate_short_array2(num_terminals + 1);
  }
  for ALL_TERMINALS3(i) {
    // initialize sp_action to the empty map
    sp_action[symbol].raw[i] = OMEGA;
  }
  int i;
  // Note that before this routine is invoked, the global vector
  // index_of identifies the index of each symbol in the goto map of
  // state_no.
  if (is_conflict_symbol[symbol]) {
    // do nothing
  } else if (action > 0) {
    // transition action (shift or goto)
    struct node *p;
    for (const struct node *item_ptr = statset[action].complete_items; item_ptr != NULL; item_ptr = item_ptr->next) {
      const int item_no = item_ptr->value;
      int rule_no = item_table[item_no].rule_number;
      int lhs_symbol = rules[rule_no].lhs;
      if (RHS_SIZE(rule_no, rules) == 1 && lhs_symbol != accept_image) {
        i = index_of.raw[lhs_symbol];
        int k = go_to.map[i].laptr;
        if (lai->la_index.raw[k] == OMEGA) {
          int stack_top = 0;
          la_traverse(state_no, i, &stack_top, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
        }
        ASSIGN_SET(look_ahead, 0, la_set, k);
        RESET_BIT(look_ahead, empty); /* empty not valid look-ahead */
        for ALL_TERMINALS3(i) {
          if (IS_ELEMENT(look_ahead, i)) {
            sp_action[symbol].raw[i] = rule_no;
          }
        }
      }
    }
    // Remove all lookahead symbols on which conflicts were
    // detected from consideration.
    for (bool end_node = (p = conflict_symbols[action]) == NULL; !end_node; end_node = p == conflict_symbols[action]) {
      p = p->next;
      sp_action[symbol].raw[p->value] = OMEGA;
    }
  } else {
    // read-reduce action
    int rule_no = -action;
    if (RHS_SIZE(rule_no, rules) == 1) {
      int lhs_symbol = rules[rule_no].lhs;
      i = index_of.raw[lhs_symbol];
      int k = go_to.map[i].laptr;
      if (lai->la_index.raw[k] == OMEGA) {
        int stack_top = 0;
        la_traverse(state_no, i, &stack_top, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
      }
      ASSIGN_SET(look_ahead, 0, la_set, k);
      RESET_BIT(look_ahead, empty); /* empty not valid look-ahead */
      for ALL_TERMINALS3(i) {
        if (IS_ELEMENT(look_ahead, i)) {
          sp_action[symbol].raw[i] = rule_no;
        }
      }
    }
  }
}

/// Sp_default_action takes as parameter a state, state_no and a rule,
/// rule_no that may be reduced when the parser enters state_no.
/// Sp_default_action tries to determine the highest rule that may be
/// reached via a sequence of SP reductions.
static short sp_default_action(const short state_no, short rule_no, ArrayShort rule_list, struct SRTable* srt, struct statset_type *statset, struct ruletab_type *rules) {
  const struct goto_header_type go_to = statset[state_no].go_to;
  // While the rule we have at hand is a single production, ...
  while (IS_SP_RULE(rule_no, rule_list)) {
    const int lhs_symbol = rules[rule_no].lhs;
    int ii;
    for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {
    }
    int action = go_to.map[ii].action;
    if (action < 0) {
      // goto-reduce action?
      action = -action;
      if (RHS_SIZE(action, rules) != 1) {
        break;
      }
      rule_no = action;
    } else {
      int best_rule = OMEGA;
      // Enter the state action and look for preferably a SP rule
      // or some rule with right-hand size 1.
      const struct reduce_header_type red = srt->reduce[action];
      for (ii = 1; ii <= red.size; ii++) {
        action = red.map[ii].rule_number;
        if (IS_SP_RULE(action, rule_list)) {
          best_rule = action;
          break;
        }
        if (RHS_SIZE(action, rules) == 1) {
          best_rule = action;
        }
      }
      if (best_rule == OMEGA) {
        break;
      }
      rule_no = best_rule;
    }
  }
  return rule_no;
}

/// This routine takes as parameter a state, state_no, a nonterminal,
/// lhs_symbol (that is the right-hand side of a SP or a rule with
/// right-hand size 1, but not identified as a SP) on which there is
/// a transition in state_no and a lookahead symbol la_symbol that may
/// be processed after taking the transition. It returns the reduce
/// action that follows the transition if an action on la_symbol is
/// found, otherwise it returns the most suitable default action.
static int sp_nt_action(const short state_no, const int lhs_symbol, const short la_symbol, ArrayShort rule_list, struct SRTable* srt, struct statset_type *statset) {
  const struct goto_header_type go_to = statset[state_no].go_to;
  int ii;
  for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {
  }
  int action = go_to.map[ii].action;
  if (action < 0) {
    action = -action;
  } else {
    const struct reduce_header_type red = srt->reduce[action];
    action = OMEGA;
    for (ii = 1; ii <= red.size; ii++) {
      const int rule_no = red.map[ii].rule_number;
      if (red.map[ii].symbol == la_symbol) {
        action = rule_no;
        break;
      } else if (action == OMEGA && IS_SP_RULE(rule_no, rule_list)) {
        action = rule_no;
      }
    }
  }
  return action;
}

/// Let BASE_RULE be a rule  A -> X.  The item [A -> .X] is in STATE1
/// and STATE2.  After shifting on X (in STATE1 and STATE2), if the
/// lookahead is LA_SYMBOL then BASE_RULE is reduced. In STATE1, a
/// sequence of single-production reductions is executed ending with
/// a reduction of RULE1. In STATE2, a sequence of single-productions
/// is also executed ending with RULE2.
/// The goal of this function is to find the greatest ancestor of
/// BASE_RULE that is also a descendant of both RULE1 and RULE2.
static int greatest_common_ancestor(const short base_rule, const short la_symbol, const short state1, const short rule1, const short state2, const short rule2, ArrayShort rule_list, struct SRTable* srt, struct statset_type *statset, struct ruletab_type *rules) {
  int act1 = base_rule;
  int act2 = base_rule;
  int rule_no;
  while (act1 == act2) {
    rule_no = act1;
    if (act1 == rule1 || act2 == rule2) {
      break;
    }
    const int lhs_symbol = rules[rule_no].lhs;
    act1 = sp_nt_action(state1, lhs_symbol, la_symbol, rule_list, srt, statset);
    act2 = sp_nt_action(state2, lhs_symbol, la_symbol, rule_list, srt, statset);
  }
  return rule_no;
}

/// In SOURCE_STATE there is a transition on SYMBOL into STATE_NO.
/// SYMBOL is the right-hand side of a SP rule and the global map
/// sp_action[SYMBOL] yields a set of update reduce actions that may
/// follow the transition on SYMBOL into STATE_NO.
static void compute_update_actions(const int source_state, const int state_no, const int symbol, ArrayShort rule_list, ArrayShort* sp_action, struct update_action_element **update_action, struct SRTable* srt, struct statset_type *statset, struct ruletab_type *rules) {
  const struct reduce_header_type red = srt->reduce[state_no];
  for (int i = 1; i <= red.size; i++) {
    if (IS_SP_RULE(red.map[i].rule_number, rule_list)) {
      int rule_no = sp_action[symbol].raw[red.map[i].symbol];
      if (rule_no == OMEGA) {
        rule_no = sp_default_action(source_state, red.map[i].rule_number, rule_list, srt, statset, rules);
      }
      // Lookup the update map to see if a previous update was made
      // in STATE_NO on SYMBOL...
      struct update_action_element *p;
      for (p = update_action[state_no]; p != NULL; p = p->next) {
        if (p->symbol == red.map[i].symbol) {
          break;
        }
      }
      // If no previous update action was defined on STATE_NO and
      // SYMBOL, simply add it. Otherwise, chose as the greatest
      // common ancestor of the initial reduce action and the two
      // contending updates as the update action.
      if (p == NULL) {
        talloc0p(&p, struct update_action_element);
        p->next = update_action[state_no];
        update_action[state_no] = p;
        p->symbol = red.map[i].symbol;
        p->action = rule_no;
        p->state = source_state;
      } else if (rule_no != p->action && p->action != red.map[i].rule_number) {
        p->action = greatest_common_ancestor(red.map[i].rule_number,
                                             red.map[i].symbol,
                                             source_state,
                                             rule_no,
                                             p->state,
                                             p->action, rule_list, srt, statset, rules);
      }
    }
  }
}

/// Sp_state_map is invoked to create a new state using the reduce map
/// sp_symbol[SYMBOL]. The new state will be entered via a transition
/// on SYMBOL which is the right-hand side of the SP rule of which
/// ITEM_NO is the final item.
///
/// RULE_HEAD is the root of a list of rules in the global vector
/// next_rule.  This list of rules identifies the range of the reduce
/// map sp_symbol[SYMBOL]. The value SP_RULE_COUNT is the number of
/// rules in the list. The value SP_ACTION_COUNT is the number of
/// actions in the map sp_symbol[SYMBOL].
static short sp_state_map(const int rule_head, const int item_no, const int sp_rule_count, const int sp_action_count, const int symbol, ArrayShort next_rule, ArrayShort* sp_action, struct sp_state_element **sp_table, struct StateMap* state_map, struct AEPool* pool) {
  // These new SP states are defined by their reduce maps. Hash the
  // reduce map based on the set of rules in its range - simply add
  // them up and reduce modulo STATE_TABLE_SIZE.
  unsigned long hash_address = 0;
  for (int rule_no = rule_head; rule_no != NIL; rule_no = next_rule.raw[rule_no]) {
    hash_address += rule_no;
  }
  hash_address %= STATE_TABLE_SIZE;
  // Search the hash table for a compatible state. Two states S1
  // and S2 are compatible if
  //     1) the set of rules in their reduce map is identical.
  //     2) for each terminal symbol t, either
  //            reduce[S1][t] == reduce[S2][t] or
  //            reduce[S1][t] == OMEGA         or
  //            reduce[S2][t] == OMEGA
  struct sp_state_element *state;
  for (state = sp_table[hash_address]; state != NULL; state = state->link) {
    if (state->rule_count == sp_rule_count) {
      // same # of rules?
      struct node *p;
      for (p = state->rule_root; p != NULL; p = p->next) {
        if (next_rule.raw[p->value] == OMEGA) {
          // not in list?
          break;
        }
      }
      // If the set of rules are identical, we proceed to compare the
      // actions for compatibility. The idea is to make sure that all
      // actions in the hash table do not clash in the actions in the
      // map sp_action[SYMBOL].
      if (p == NULL) {
        // all the rules match?
        struct remsp_action_element *actionp;
        for (actionp = state->action_root; actionp != NULL; actionp = actionp->next) {
          if (sp_action[symbol].raw[actionp->symbol] != OMEGA && sp_action[symbol].raw[actionp->symbol] != actionp->action) {
            break;
          }
        }
        // If the two states are compatible merge them into the map
        // sp_action[SYMBOL]. (Note that this effectively destroys
        // the original map.) Also, keep track of whether or not an
        // actual merging action was necessary with the boolean
        // variable no_overwrite.
        if (actionp == NULL) {
          // compatible states
          struct remsp_action_element *action_tail;
          bool no_overwrite = true;
          for (actionp = state->action_root; actionp != NULL; action_tail = actionp, actionp = actionp->next) {
            if (sp_action[symbol].raw[actionp->symbol] == OMEGA) {
              sp_action[symbol].raw[actionp->symbol] = actionp->action;
              no_overwrite = false;
            }
          }
          // If the item was not previously associated with this
          // state, add it.
          for (p = state->complete_items; p != NULL; p = p->next) {
            if (p->value == item_no) {
              break;
            }
          }
          if (p == NULL) {
            p = Allocate_node();
            p->value = item_no;
            p->next = state->complete_items;
            state->complete_items = p;
          }
          // If the two maps are identical (there was no merging),
          // return the state number otherwise, free the old map
          // and break out of the search loop.
          if (no_overwrite && state->action_count == sp_action_count) {
            return state->state_number;
          }
          free_action_elements(state->action_root, action_tail, pool);
          break; /* for (state = sp_table[hash_address]; ... */
        }
      }
    }
  }
  // If we did not find a compatible state, construct a new one.
  // Add it to the list of state and add it to the hash table.
  if (state == NULL) {
    talloc0p(&state, struct sp_state_element);
    state->next = state_map->sp_state_root;
    state_map->sp_state_root = state;
    state->link = sp_table[hash_address];
    sp_table[hash_address] = state;
    state_map->max_sp_state++;
    state->state_number = state_map->max_sp_state;
    state->rule_count = sp_rule_count;
    struct node *p = Allocate_node();
    p->value = item_no;
    p->next = NULL;
    state->complete_items = p;
    state->rule_root = NULL;
    for (int rule_no = rule_head; rule_no != NIL; rule_no = next_rule.raw[rule_no]) {
      p = Allocate_node();
      p->value = rule_no;
      p->next = state->rule_root;
      state->rule_root = p;
    }
  }
  // If the state is new or had its reduce map merged with another
  // map, we update the reduce map here.
  state->action_count = sp_action_count;
  state->action_root = NULL;
  for ALL_TERMINALS3(i) {
    if (sp_action[symbol].raw[i] != OMEGA) {
      struct remsp_action_element *actionp = allocate_action_element(pool);
      actionp->symbol = i;
      actionp->action = sp_action[symbol].raw[i];
      actionp->next = state->action_root;
      state->action_root = actionp;
    }
  }
  return state->state_number;
}

/// This program is invoked to remove as many single production
/// actions as possible for a conflict-free automaton.
void remove_single_productions(struct DetectedSetSizes *dss, struct StackRoot* sr, JBitset first, struct LAIndex* lai, struct node **conflict_symbols, JBitset la_set, struct node **adequate_item, struct SRTable* srt, struct statset_type *statset, struct lastats_type *lastats, ArrayShort gd_index, struct node **in_stat, struct ruletab_type *rules, struct itemtab *item_table, ArrayShort rhs_sym, struct LAState* ls) {
  struct AEPool pool = {
    .action_element_pool = NULL,
  };
  // Set up a pool of temporary space.
  reset_temporary_space();
  // Allocate all other necessary temporary objects.
  ArrayShort sp_rules = Allocate_short_array2(num_symbols + 1);
  ArrayShort stack = Allocate_short_array2(num_symbols + 1);
  ArrayShort index_of = Allocate_short_array2(num_symbols + 1);
  ArrayShort next_rule = Allocate_short_array2(num_rules + 1);
  ArrayShort rule_list = Allocate_short_array2(num_rules + 1);
  ArrayShort symbol_list = Allocate_short_array2(num_symbols + 1);
  ArrayShort shift_transition = Allocate_short_array2(num_symbols + 1);
  ArrayShort rule_count = Allocate_short_array2(num_rules + 1);
  struct new_shift_element {
    short link;
    short shift_number;
  } *new_shift;
  calloc0p(&new_shift, ls->max_la_state + 1, struct new_shift_element);
  JBitset look_ahead;
  calloc0_set(look_ahead, 1, dss->term_set_size);
  ArrayShort* sp_action;
  calloc0p(&sp_action, num_symbols + 1, ArrayShort);
  bool *is_conflict_symbol;
  calloc0p(&is_conflict_symbol, num_symbols + 1, bool);
  struct sp_state_element **sp_table;
  calloc0p(&sp_table, STATE_TABLE_SIZE, struct sp_state_element *);
  struct remsp_action_element **new_action;
  calloc0p(&new_action, ls->num_states + 1, struct remsp_action_element *);
  struct update_action_element **update_action;
  calloc0p(&update_action, ls->num_states + 1, struct update_action_element *);
  // Initialize all relevant sets and maps to the empty set.
  short symbol_root = NIL;
  for ALL_RULES3(rule_no) {
    rule_list.raw[rule_no] = OMEGA;
  }
  for ALL_SYMBOLS3(symbol) {
    symbol_list.raw[symbol] = OMEGA;
    sp_rules.raw[symbol] = NIL;
  }
  // Construct a set of all symbols used in the right-hand side of
  // single production in symbol_list. The variable symbol_root
  // points to the root of the list. Also, construct a mapping from
  // each symbol into the set of single productions of which it is
  // the right-hand side. sp_rules is the base of that map and the
  // relevant sets are stored in the vector next_rule.
  for ALL_RULES3(rule_no) {
    if (rules[rule_no].sp) {
      int i = rhs_sym.raw[rules[rule_no].rhs];
      next_rule.raw[rule_no] = sp_rules.raw[i];
      sp_rules.raw[i] = rule_no;
      if (symbol_list.raw[i] == OMEGA) {
        symbol_list.raw[i] = symbol_root;
        symbol_root = i;
      }
    }
  }
  // Initialize the index_of vector and clear the stack (top).
  // Next, iterate over the list of right-hand side symbols used in
  // single productions and invoke compute_sp_map to partially
  // order these symbols (based on the ::= (or ->) relationship) as
  // well as their associated rules. (See compute_sp_map for detail)
  // As the list of rules is constructed as a circular list to keep
  // it in proper order, it is turned into a linear list here.
  for (int i = symbol_root; i != NIL; i = symbol_list.raw[i]) {
    index_of.raw[i] = OMEGA;
  }
  struct SPData spd = {
    .rule_root = NIL,
    .top = 0,
  };
  for (int i = symbol_root; i != NIL; i = symbol_list.raw[i]) {
    if (index_of.raw[i] == OMEGA) {
      compute_sp_map(i, &spd, sp_rules, rule_list, index_of, next_rule, stack, rules);
    }
  }
  if (spd.rule_root != NIL) {
    // make rule_list non-circular
    int rule_no = spd.rule_root;
    spd.rule_root = rule_list.raw[rule_no];
    rule_list.raw[rule_no] = NIL;
  }
  // Clear out all the sets in sp_rules and using the new revised
  // list of SP rules mark the new set of right-hand side symbols.
  // Note this code is important for consistency in case we are
  // removing single productions in an automaton containing
  // conflicts. If an automaton does not contain any conflict, the
  // new set of SP rules is always the same as the initial set.
  for (int i = symbol_root; i != NIL; i = symbol_list.raw[i]) {
    sp_rules.raw[i] = NIL;
  }
  spd.top = 0;
  for (int rule_no = spd.rule_root; rule_no != NIL; rule_no = rule_list.raw[rule_no]) {
    spd.top++;
    int i = rhs_sym.raw[rules[rule_no].rhs];
    sp_rules.raw[i] = OMEGA;
  }
  // Initialize the base of the hash table for the new SP states.
  // Initialize the root pointer for the list of new states.
  // Initialize max_sp_state to num_states. It will be incremented
  // each time a new state is constructed.
  // Initialize the update_action table.
  // Initialize the is_conflict_symbol array for non_terminals.
  // Since nonterminals are not used as lookahead symbols, they are
  // never involved in conflicts.
  // Initialize the set/list (symbol_root, symbol_list) to the
  // empty set/list.
  for (int i = 0; i < STATE_TABLE_SIZE; i++) {
    sp_table[i] = NULL;
  }
  struct StateMap state_map = {
    .sp_state_root = NULL,
    .max_sp_state = ls->num_states,
  };
  for ALL_STATES3(state_no, ls->num_states) {
    update_action[state_no] = NULL;
  }
  for ALL_NON_TERMINALS3(symbol) {
    is_conflict_symbol[symbol] = false;
  }
  symbol_root = NIL;
  for ALL_SYMBOLS3(symbol) {
    symbol_list.raw[symbol] = OMEGA;
  }
  // Traverse all regular states and process the relevant ones.
  for ALL_STATES3(state_no, ls->num_states) {
    new_action[state_no] = NULL;
    struct goto_header_type go_to = statset[state_no].go_to;
    struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
    // If the state has no goto actions, it is not considered, as
    // no single productions could have been introduced in it.
    // Otherwise, we initialize index_of to the empty map and
    // presume that symbol_list is initialized to the empty set.
    if (go_to.size > 0) {
      struct node *item_ptr;
      for ALL_SYMBOLS3(symbol) {
        index_of.raw[symbol] = OMEGA;
      }
      for ALL_TERMINALS3(symbol) {
        is_conflict_symbol[symbol] = false;
      }
      struct node *p;
      for (bool end_node = (p = conflict_symbols[state_no]) == NULL; !end_node; end_node = p == conflict_symbols[state_no]) {
        p = p->next;
        is_conflict_symbol[p->value] = true;
      }
      // First, use index_of to map each nonterminal symbol on
      // which there is a transition in state_no into its index
      // in the goto map of state_no.
      // Note that this initialization must be executed first
      // before we process the next loop, because index_of is
      // a global variable that is used in the routine
      // compute_sp_action.
      for (int i = 1; i <= go_to.size; i++) {
        index_of.raw[go_to.map[i].symbol] = i;
      }
      // Traverse first the goto map, then the shift map and
      // for each symbol that is the right-hand side of a single
      // production on which there is a transition, compute the
      // lookahead set that can follow this transition and add
      // the symbol to the set of candidates (in symbol_list).
      for (int i = 1; i <= go_to.size; i++) {
        short symbol = go_to.map[i].symbol;
        if (IS_SP_RHS(symbol, sp_rules)) {
          compute_sp_action(state_no, symbol, go_to.map[i].action, look_ahead, is_conflict_symbol, index_of, sp_action, sr, first, lai, conflict_symbols, la_set, adequate_item, in_stat, statset, rules, item_table);
          symbol_list.raw[symbol] = symbol_root;
          symbol_root = symbol;
        }
      }
      for (int i = 1; i <= sh.size; i++) {
        short symbol = sh.map[i].symbol;
        index_of.raw[symbol] = i;
        if (IS_SP_RHS(symbol, sp_rules)) {
          compute_sp_action(state_no, symbol, sh.map[i].action, look_ahead, is_conflict_symbol, index_of, sp_action, sr, first, lai, conflict_symbols, la_set, adequate_item, in_stat, statset, rules, item_table);
          symbol_list.raw[symbol] = symbol_root;
          symbol_root = symbol;
        }
      }
      // We now traverse the set of single productions in order
      // and for each rule that was introduced through closure
      // in the state (there is an action on both the left and
      // right-hand side)...
      for (int rule_no = spd.rule_root; rule_no != NIL; rule_no = rule_list.raw[rule_no]) {
        int symbol = rhs_sym.raw[rules[rule_no].rhs];
        if (symbol_list.raw[symbol] != OMEGA) {
          int lhs_symbol = rules[rule_no].lhs;
          if (index_of.raw[lhs_symbol] != OMEGA) {
            if (symbol_list.raw[lhs_symbol] == OMEGA) {
              compute_sp_action(state_no, lhs_symbol, go_to.map[index_of.raw[lhs_symbol]].action, look_ahead, is_conflict_symbol, index_of, sp_action, sr, first, lai, conflict_symbols, la_set, adequate_item, in_stat, statset, rules, item_table);
              symbol_list.raw[lhs_symbol] = symbol_root;
              symbol_root = lhs_symbol;
            }
            // Copy all reduce actions defined after the
            // transition on the left-hand side into the
            // corresponding action defined after the transition
            // on the right-hand side. If an action is defined
            // for the left-hand side -
            //
            //     sp_action[lhs_symbol][i] != OMEGA
            //
            // - but not for the right-hand side -
            //
            //     sp_action[symbol][i] == OMEGA
            //
            // it is an indication that after the transition on
            // symbol, the action on i is a lookahead shift. In
            // that case, no action is copied.
            for ALL_TERMINALS3(symbol) {
              if (sp_action[lhs_symbol].raw[symbol] != OMEGA && sp_action[symbol].raw[symbol] != OMEGA) {
                sp_action[symbol].raw[symbol] = sp_action[lhs_symbol].raw[symbol];
              }
            }
          }
        }
      }
      // For each symbol that is the right-hand side of some SP
      // for which a reduce map is defined, we either construct
      // a new state if the transition is into a final state,
      // or we update the relevant reduce action of the state
      // into which the transition is made, otherwise.
      //
      // When execution of this loop is terminated the set
      // symbol_root/symbol_list is reinitialize to the empty
      // set.
      for (int symbol = symbol_root; symbol != NIL; symbol_list.raw[symbol] = OMEGA, symbol = symbol_root) {
        symbol_root = symbol_list.raw[symbol];
        if (IS_SP_RHS(symbol, sp_rules)) {
          int action;
          if (IS_A_TERMINAL(symbol)) {
            action = sh.map[index_of.raw[symbol]].action;
          } else {
            action = go_to.map[index_of.raw[symbol]].action;
          }
          // If the transition is a lookahead shift, do nothing.
          // If the action is a goto- or shift-reduce, compute
          // the relevant rule and item involved.
          // Otherwise, the action is a shift or a goto. If the
          // transition is into a final state then it is
          // processed as the case of read-reduce above. If
          // not, we invoke compute_update_actions to update
          // the relevant actions.
          int rule_no;
          int item_no;
          if (action > ls->num_states) {
            // lookahead shift
            rule_no = OMEGA;
          } else if (action < 0) {
            // read-reduce action
            rule_no = -action;
            item_no = adequate_item[rule_no]->value;
          } else {
            // transition action
            item_ptr = statset[action].kernel_items;
            item_no = item_ptr->value;
            if (item_ptr->next == NULL && item_table[item_no].symbol == empty) {
              rule_no = item_table[item_no].rule_number;
            } else {
              compute_update_actions(state_no, action, symbol, rule_list, sp_action, update_action, srt, statset, rules);
              rule_no = OMEGA;
            }
          }
          // If we have a valid SP rule we first construct the
          // set of rules in the range of the reduce map of the
          // right-hand side of the rule. If that set contains
          // a single rule then the action on the right-hand
          // side is redefined as the same action on the left-
          // hand side of the rule in question. Otherwise, we
          // create a new state for the final item of the SP
          // rule consisting of the reduce map associated with
          // the right-hand side of the SP rule and the new
          // action on the right-hand side is a transition into
          // this new state.
          if (rule_no != OMEGA) {
            if (IS_SP_RULE(rule_no, rule_list)) {
              struct remsp_action_element *p_inner;
              int sp_rule_count = 0;
              int sp_action_count = 0;
              int rule_head = NIL;
              for ALL_RULES3(rule_no) {
                next_rule.raw[rule_no] = OMEGA;
              }
              for ALL_TERMINALS3(symbol) {
                rule_no = sp_action[symbol].raw[rule_no];
                if (rule_no != OMEGA) {
                  sp_action_count++;
                  if (next_rule.raw[rule_no] == OMEGA) {
                    sp_rule_count++;
                    next_rule.raw[rule_no] = rule_head;
                    rule_head = rule_no;
                  }
                }
              }
              if (sp_rule_count == 1 && IS_SP_RULE(rule_head, rule_list)) {
                int lhs_symbol = rules[rule_head].lhs;
                action = go_to.map[index_of.raw[lhs_symbol]].action;
              } else {
                action = sp_state_map(rule_head, item_no, sp_rule_count, sp_action_count, symbol, next_rule, sp_action, sp_table, &state_map, &pool);
              }
              p_inner = allocate_action_element(&pool);
              p_inner->symbol = symbol;
              p_inner->action = action;
              p_inner->next = new_action[state_no];
              new_action[state_no] = p_inner;
            }
          }
        }
      }
    }
  }
  // We are now ready to extend all global maps based on states and
  // permanently install the new states.
  realloc0p(&statset, state_map.max_sp_state + 1, struct statset_type);
  realloc0p(&srt->reduce, state_map.max_sp_state + 1, struct reduce_header_type);
  // see routine PRODUCE
  if (gd_index.raw != NULL) {
    realloc0p(&gd_index.raw, state_map.max_sp_state + 2, short);
    // Each element gd_index[i] points to the starting location
    // of a slice in another array. The last element of the slice
    // can be computed as (gd_index[i+1] - 1). After extending
    // gd_index, we set each new element to point to the same
    // index as its previous element, making it point to a null
    // slice.
    for (int state_no = ls->num_states + 2; state_no <= state_map.max_sp_state + 1; state_no++) {
      gd_index.raw[state_no] = gd_index.raw[state_no - 1];
    }
  }
  realloc0p(&in_stat, state_map.max_sp_state + 1, struct node*);
  for (int state_no = ls->num_states + 1; state_no <= state_map.max_sp_state; state_no++) {
    in_stat[state_no] = NULL;
  }
  // We now adjust all references to a lookahead state. The idea is
  // offset the number associated with each lookahead state by the
  // number of new SP states that were added.
  for (int j = 1; j <= num_shift_maps; j++) {
    struct shift_header_type sh = srt->shift[j];
    for (int i = 1; i <= sh.size; i++) {
      if (sh.map[i].action > ls->num_states) {
        sh.map[i].action += state_map.max_sp_state - ls->num_states;
      }
    }
  }
  for (int state_no = ls->num_states + 1; state_no <= ls->max_la_state; state_no++) {
    if (lastats[state_no].in_state > ls->num_states) {
      lastats[state_no].in_state += state_map.max_sp_state - ls->num_states;
    }
  }
  lastats -= state_map.max_sp_state - ls->num_states;
  ls->max_la_state += state_map.max_sp_state - ls->num_states;
  // We now permanently construct all the new SP states.
  for (struct sp_state_element *state = state_map.sp_state_root; state != NULL; state = state->next) {
    struct remsp_action_element *actionp;
    int default_rule;
    int reduce_size;
    int state_no = state->state_number;
    // These states are identified as special SP states since
    // they have no kernel items. They also have no goto and
    // shift actions.
    statset[state_no].kernel_items = NULL;
    statset[state_no].complete_items = state->complete_items;
    statset[state_no].go_to.size = 0;
    statset[state_no].go_to.map = NULL;
    statset[state_no].shift_number = 0;
    // Count the number of actions defined on each rule in the
    // range of the reduce map.
    for (struct node *p = state->rule_root; p != NULL; p = p->next) {
      rule_count.raw[p->value] = 0;
    }
    for (actionp = state->action_root; actionp != NULL; actionp = actionp->next) {
      rule_count.raw[actionp->action]++;
    }
    // Count the total number of reduce actions in the reduce map
    // and calculate the default.
    reduce_size = 0;
    int sp_rule_count = 0;
    struct node *rule_tail;
    for (struct node *p = state->rule_root; p != NULL; rule_tail = p, p = p->next) {
      reduce_size += rule_count.raw[p->value];
      if (rule_count.raw[p->value] > sp_rule_count) {
        sp_rule_count = rule_count.raw[p->value];
        default_rule = p->value;
      }
    }
    free_nodes(state->rule_root, rule_tail);
    // Construct a permanent reduce map for this SP state.
    num_reductions += reduce_size;
    struct reduce_header_type red = Allocate_reduce_map(reduce_size);
    srt->reduce[state_no] = red;
    red.map[0].symbol = DEFAULT_SYMBOL;
    red.map[0].rule_number = default_rule;
    for (actionp = state->action_root; actionp != NULL; actionp = actionp->next) {
      red.map[reduce_size].symbol = actionp->symbol;
      red.map[reduce_size].rule_number = actionp->action;
      reduce_size--;
    }
  }
  // We are now ready to update some old actions and add new ones.
  // This may require that we create new shift maps.  We
  // initialize top to 0 so we can use it as an index to allocate
  // elements from new_shift. We also initialize all the elements
  // of shift_table to NIL. Shift_table will be used as the base of
  // a hash table for the new shift maps.
  spd.top = 0;
  short shift_table[SHIFT_TABLE_SIZE];
  for (int i = 0; i <= SHIFT_TABLE_UBOUND; i++) {
    shift_table[i] = NIL;
  }
  // At most, the shift array contains 1..num_states elements. As,
  // each of these elements might be (theoretically) replaced by a
  // new one, we need to double its size.
  realloc0p(&srt->shift, 2 * (ls->num_states + 1), struct shift_header_type);
  // For each state with updates or new actions, take appropriate
  // actions.
  for ALL_STATES3(state_no, ls->num_states) {
    // Update reduce actions for final items of single productions
    // that are in non-final states.
    if (update_action[state_no] != NULL) {
      struct update_action_element *p_inner;
      struct reduce_header_type red = srt->reduce[state_no];
      for (int i = 1; i <= red.size; i++) {
        index_of.raw[red.map[i].symbol] = i;
      }
      for (p_inner = update_action[state_no]; p_inner != NULL; p_inner = p_inner->next) {
        red.map[index_of.raw[p_inner->symbol]].rule_number = p_inner->action;
      }
    }
    // Update initial automaton with transitions into new SP states.
    if (new_action[state_no] != NULL) {
      bool any_shift_action;
      struct remsp_action_element *p_inner;
      // Mark the index of each symbol on which there is a
      // transition and copy the shift map into the vector
      // shift_transition.
      struct goto_header_type go_to = statset[state_no].go_to;
      for (int i = 1; i <= go_to.size; i++) {
        index_of.raw[go_to.map[i].symbol] = i;
      }
      struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
      for (int i = 1; i <= sh.size; i++) {
        index_of.raw[sh.map[i].symbol] = i;
        shift_transition.raw[sh.map[i].symbol] = sh.map[i].action;
      }
      // Iterate over the new action and update the goto map
      // directly for goto actions but update shift_transition
      // for shift actions. Also, keep track as to whether or
      // not there were any shift transitions at all...
      any_shift_action = false;
      for (p_inner = new_action[state_no]; p_inner != NULL; p_inner = p_inner->next) {
        if (IS_A_NON_TERMINAL(p_inner->symbol)) {
          if (go_to.map[index_of.raw[p_inner->symbol]].action < 0 && p_inner->action > 0) {
            num_goto_reduces--;
            num_gotos++;
          }
          go_to.map[index_of.raw[p_inner->symbol]].action = p_inner->action;
        } else {
          if (sh.map[index_of.raw[p_inner->symbol]].action < 0 && p_inner->action > 0) {
            num_shift_reduces--;
            num_shifts++;
          }
          shift_transition.raw[p_inner->symbol] = p_inner->action;
          any_shift_action = true;
        }
      }
      // If there were any shift actions, a new shift map may
      // have been created. Hash shift_transition into the
      // shift hash table.
      if (any_shift_action) {
        struct shift_header_type sh2;
        unsigned long hash_address;
        hash_address = sh.size;
        for (int i = 1; i <= sh.size; i++) {
          // Compute Hash location
          hash_address += sh.map[i].symbol;
        }
        hash_address %= SHIFT_TABLE_SIZE;
        // Search HASH_ADDRESS location for shift map that matches
        // the shift map in shift_transition.  If a match is found
        // we leave the loop prematurely, the search index j is not
        // NIL, and it identifies the shift map in the hash table
        // that matched the shift_transition.
        int jj;
        for (jj = shift_table[hash_address]; jj != NIL; jj = new_shift[jj].link) {
          sh2 = srt->shift[new_shift[jj].shift_number];
          if (sh.size == sh2.size) {
            int ii;
            for (ii = 1; ii <= sh.size; ii++) {
              if (sh2.map[ii].action != shift_transition.raw[sh2.map[ii].symbol]) {
                break;
              }
            }
            if (ii > sh.size) {
              break; /* for (j = shift_table[ ... */
            }
          }
        }
        // If j == NIL, the map at hand had not yet being inserted in
        // the table, it is inserted.  Otherwise, we have a match,
        // and STATE_NO is reset to share the shift map previously
        // inserted that matches its shift map.
        if (jj == NIL) {
          sh2 = Allocate_shift_map(sh.size);
          for (int i = 1; i <= sh.size; i++) {
            int symbol = sh.map[i].symbol;
            sh2.map[i].symbol = symbol;
            sh2.map[i].action = shift_transition.raw[symbol];
          }
          num_shift_maps++;
          srt->shift[num_shift_maps] = sh2;
          statset[state_no].shift_number = num_shift_maps;
          spd.top++;
          new_shift[spd.top].shift_number = num_shift_maps;
          new_shift[spd.top].link = shift_table[hash_address];
          shift_table[hash_address] = spd.top;
        } else {
          statset[state_no].shift_number = new_shift[jj].shift_number;
        }
      }
    }
  }
  // Free all nodes used in the construction of the conflict_symbols
  // map as this map is no longer useful and its size is based on
  // the base value of num_states.
  for ALL_STATES3(state_no, ls->num_states) {
    if (conflict_symbols[state_no] != NULL) {
      struct node *p = conflict_symbols[state_no]->next;
      free_nodes(p, conflict_symbols[state_no]);
    }
  }
  // All updates have now been made, adjust the number of regular
  // states to include the new SP states.
  ls->num_states = state_map.max_sp_state;
  // Free all temporary space allocated earlier.
  ffree(sp_rules.raw);
  ffree(stack.raw);
  ffree(index_of.raw);
  ffree(next_rule.raw);
  ffree(rule_list.raw);
  ffree(symbol_list.raw);
  ffree(shift_transition.raw);
  ffree(rule_count.raw);
  ffree(new_shift);
  ffree(look_ahead.raw);
  for ALL_SYMBOLS3(symbol) {
    if (sp_action[symbol].raw != NULL) {
      ffree(sp_action[symbol].raw);
    }
  }
  ffree(sp_action);
  ffree(is_conflict_symbol);
  ffree(sp_table);
  ffree(new_action);
  ffree(update_action);
}
