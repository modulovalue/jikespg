#include <stdlib.h>
#include "lpgparse.h"
#include "common.h"

long la_top = 0;

/// The structure STATE_ELEMENT is used to construct lookahead states.
/// LA_STATE_ROOT point to a list of lookahead states using the LINK
/// field. The field NEXT_SHIFT is used to hash the new shift maps
/// associated with lookahead states. The field IN_STATE identifies the
/// state that shifted into the lookahead state in question. The field
/// SYMBOL identifies the symbol on shift the transition was made into
/// the lookahead state in question.  The remaining fields are
/// self-explanatory.
struct state_element {
  struct state_element *link;
  struct state_element *next_shift;
  struct reduce_header_type reduce;
  struct shift_header_type shift;
  short in_state;
  short symbol;
  short state_number;
  short shift_number;
};

/// The structures SR_CONFLICT_ELEMENT and RR_CONFLICT_ELEMENT are used
/// th store conflict information. CONFLICT_ELEMENT_POOL is used to
/// keep track of a pool conflict element structures (SR or RR) that
/// are available for allocation.
/// See routines ALLOCATE_CONFLICT_ELEMENT and FREE_CONFLICT_ELEMENTS.
struct sr_conflict_element {
  struct sr_conflict_element *next;
  short state_number;
  short item;
  short symbol;
};

struct rr_conflict_element {
  struct rr_conflict_element *next;
  short symbol;
  short item1;
  short item2;
};

/// VISITED is a structure used to mark state-symbol pairs that have
/// been visited in the process of computing follow-sources for a
/// given action in conflict.
/// The field MAP is an array indexable by the states 1..NUM_STATES;
/// each element of which points to a set (list) of symbols. Thus, for
/// a given state S, and for all symbol X in the list MAP[S], [S,X]
/// has been visited. For efficiency, the fields LIST and ROOT are used
/// to store the set (list) of indexed elements of MAP that are not
/// NULL.
/// See routines MARK_VISITED, WAS_VISITED, CLEAR_VISITED,
///              INIT_LALRK_PROCESS, EXIT_PROCESS
struct visited_element {
  struct node **map;
  short *list;
  short root;
};

struct ConflictPool {
  void *conflict_element_pool;
};

struct StackPool {
  struct stack_element *stack_pool;
  struct stack_element *dangling_stacks;
};

struct STRS {
  struct state_element *la_state_root;
  /// The variable HIGHEST_LEVEL is used to indicate the highest number of
  /// lookahead that was necessary to resolve all conflicts for a given
  /// grammar. If we can detect that the grammar is not LALR(k), we set
  /// HIGHEST_LEVEL to INFINITY.
  int highest_level;
};

/// Given a STATE_NO and an ITEM_NO, ACCESS computes the set of states where
/// the rule from which ITEM_NO is derived was introduced through closure.
struct node *lpgaccess(const int state_no, const int item_no, struct node **in_stat, struct itemtab *item_table) {
  // Build a list pointed to by ACCESS_ROOT originally consisting
  // only of STATE_NO.
  struct node *access_root = Allocate_node();
  access_root->value = state_no;
  access_root->next = NULL;
  for (int i = item_table[item_no].dot; i > 0; i--) /*distance to travel is DOT */
  {
    struct node *head = access_root; /* Save old ACCESS_ROOT */
    access_root = NULL; /* Initialize ACCESS_ROOT for new list */
    struct node *tail;
    for (struct node *p = head; p != NULL; tail = p, p = p->next) {
      // Compute set of states with transition into p->value.
      struct node *s;
      for (bool end_node = (s = in_stat[p->value]) == NULL; !end_node; end_node = s == in_stat[p->value]) {
        s = s->next;
        struct node *q = Allocate_node();
        q->value = s->value;
        q->next = access_root;
        access_root = q;
      }
    }
    free_nodes(head, tail); /* free previous list */
  }
  return access_root;
}

/// This function allocates a conflict_element (sr or rr) structure
/// & returns a pointer to it. If there are nodes in the free pool,
/// one of them is returned. Otherwise, a new node is allocated
/// from the temporary storage pool.
static void *allocate_conflict_element(struct ConflictPool* cp) {
  void *p = cp->conflict_element_pool;
  if (p != NULL) {
    cp->conflict_element_pool = ((struct sr_conflict_element *) p)->next;
  } else {
    talloc0_raw(p, void, MAX(sizeof(struct sr_conflict_element), sizeof(struct rr_conflict_element)));
  }
  return p;
}

/// This routine returns a list of conflict_element (sr/rr)structures
/// to the free pool.
static void free_conflict_elements(void *head, void *tail, struct ConflictPool* cp) {
  ((struct sr_conflict_element *) tail)->next = (struct sr_conflict_element *) cp->conflict_element_pool;
  cp->conflict_element_pool = head;
}

/// This function allocates a stack_element structure and returns a
/// pointer to it. If there are nodes in the free pool, one of them
/// is returned. Otherwise, a new node is allocated from the
/// temporary storage pool.
static struct stack_element *allocate_stack_element(struct StackPool* sp) {
  struct stack_element *p = sp->stack_pool;
  if (p != NULL) {
    sp->stack_pool = p->next;
  } else {
    talloc0p(&p, struct stack_element);
  }
  return p;
}

/// This routine returns a list of stack_element structures to the
/// free pool.
static void free_stack_elements(struct stack_element *head, struct stack_element *tail, struct StackPool* sp) {
  tail->next = sp->stack_pool;
  sp->stack_pool = head;
}

/// When an allocated stack_element structure is not directly associated
/// with an action, it is added to a circular list of dangling stack_element
/// nodes so that its space can be reclaimed.
static void add_dangling_stack_element(struct stack_element *s, struct StackPool* sp) {
  if (sp->dangling_stacks == NULL) {
    s->next = s;
  } else {
    s->next = sp->dangling_stacks->next;
    sp->dangling_stacks->next = s;
  }
  sp->dangling_stacks = s;
}

/// This function is invoked to free up all dangling stack_element nodes
/// and reset the dangling stack list.
/// Recall that the dangling stack list is circular.
static void free_dangling_stack_elements(struct StackPool* sp) {
  if (sp->dangling_stacks != NULL) {
    struct stack_element *tail = sp->dangling_stacks;
    free_stack_elements(sp->dangling_stacks->next, tail, sp);
    sp->dangling_stacks = NULL;
  }
}

/// This function allocates and initializes a SOURCE_ELEMENT map.
/// See definition of SOURCE_ELEMENT above.
static struct sources_element allocate_sources(void) {
  struct sources_element sources;
  calloc0(sources.configs, num_rules + num_rules + num_states + 1, struct stack_element *);
  sources.configs += num_rules;
  calloc0(sources.stack_seen, STATE_TABLE_SIZE, struct stack_element *);
  sources.list = Allocate_short_array(num_rules + num_rules + num_states + 1);
  sources.list += num_rules;
  sources.root = NIL;
  return sources;
}

/// This function takes as argument a SOURCES_ELEMENT structure which it
/// resets to the empty map.
/// See definition of SOURCE_ELEMENT above.
static struct sources_element clear_sources(struct sources_element sources, struct StackPool* sp) {
  struct stack_element *tail;
  for (int act = sources.root; act != NIL; act = sources.list[act]) {
    for (struct stack_element *p = sources.configs[act]; p != NULL; tail = p, p = p->next) {
    }
    free_stack_elements(sources.configs[act], tail, sp);
    sources.configs[act] = NULL;
  }
  sources.root = NIL;
  return sources;
}

/// This function takes as argument a SOURCES_ELEMENT structure. First, it
/// clears it to reclaim all space that was used by STACK_ELEMENTs and then
/// it frees the array space used as a base to construct the map.
static void free_sources(struct sources_element sources, struct StackPool* sp) {
  sources = clear_sources(sources, sp);
  sources.configs -= num_rules;
  ffree(sources.configs);
  ffree(sources.stack_seen);
  sources.list -= num_rules;
  ffree(sources.list);
}

/// This function takes as argument two pointers to sorted lists of stacks.
/// It merges the lists in the proper order and returns the resulting list.
static struct stack_element *union_config_sets(struct stack_element *root1, struct stack_element *root2, struct StackPool* sp) {
  struct stack_element *root = NULL;
  // This loop iterates over both lists until one (or both) has been
  // completely processed. Each time around the loop, a stack is
  // removed from one of the lists and possibly added to the new
  // list. The new list is initially kept as a circular list to
  // preserve the sorted ordering in which elements are added to it.
  while (root1 != NULL && root2 != NULL) {
    // Compare the two stacks in front of the lists for equality.
    // We exit this loop when we encounter the end of one (or both)
    // of the stacks or two elements in them that are not the same.
    struct stack_element *p1;
    struct stack_element *p2;
    for (p1 = root1, p2 = root2;
         p1 != NULL && p2 != NULL;
         p1 = p1->previous, p2 = p2->previous) {
      if (p1->state_number != p2->state_number)
        break;
    }
    // We now have 3 cases to consider:
    //    1. The two stacks are equal? Discard one!
    //    2. List 1 stack is prefix of list 2 stack (p1 == NULL)?
    //       or list 1 stack is less than list 2 stack?
    //       Remove list 1 stack and add it to new list.
    //    3. List 2 stack is either a prefix of list 1 stack, or
    //       it is smaller!
    //       Remove list 2 stack and add it to new list.
    if (p1 == p2) {
      // are both p1 and p2 NULL?
      p2 = root2;
      root2 = root2->next;
      add_dangling_stack_element(p2, sp);
    } else if (p1 == NULL || (p2 != NULL && p1->state_number < p2->state_number)) {
      p1 = root1;
      root1 = root1->next;
      if (root == NULL) {
        p1->next = p1;
      } else {
        p1->next = root->next;
        root->next = p1;
      }
      root = p1;
    } else {
      p2 = root2;
      root2 = root2->next;
      if (root == NULL) {
        p2->next = p2;
      } else {
        p2->next = root->next;
        root->next = p2;
      }
      root = p2;
    }
  }
  // At this stage, at least one (or both) list has been expended
  // (or was empty to start with).
  // If the new list is not empty, turn it into a linear list and
  // append the unexpended list to it, if any.
  // Otherwise, set the new list to the nonempty list if any!
  if (root != NULL) {
    struct stack_element *tail = root;
    root = root->next;
    tail->next = root1 == NULL ? root2 : root1;
  } else {
    root = root1 == NULL ? root2 : root1;
  }
  return root;
}

/// This function takes as argument a SOURCES_ELEMENT map, an ACTION and a
/// set (sorted list) of configurations. It adds the set of configurations
/// to the previous set of configurations associated with the ACTION in the
/// SOURCES_ELEMENT map.
static struct sources_element add_configs(struct sources_element sources, const int action, struct stack_element *config_root, struct StackPool* sp) {
  if (config_root != NULL) {
    if (sources.configs[action] == NULL) {
      // The previous was empty?
      sources.list[action] = sources.root;
      sources.root = action;
    }
    sources.configs[action] = union_config_sets(sources.configs[action], config_root, sp);
  }
  return sources;
}

/// This function clears out all external space used by the VISITED set and
/// resets VISITED to the empty set.
static void clear_visited(struct visited_element* visited) {
  for (int state_no = visited->root; state_no != NIL; state_no = visited->list[state_no]) {
    struct node *tail;
    for (struct node *p = visited->map[state_no]; p != NULL; tail = p, p = p->next) {
    }
    free_nodes(visited->map[state_no], tail);
    visited->map[state_no] = NULL;
  }
  visited->root = NIL;
}

/// This boolean function checks whether a given pair [state, symbol]
/// was already inserted in the VISITED set.
static bool was_visited(const int state_no, const int symbol, struct visited_element* visited) {
  struct node *p;
  for (p = visited->map[state_no]; p != NULL; p = p->next) {
    if (p->value == symbol) {
      break;
    }
  }
  return p != NULL;
}

/// This function inserts a given pair [state, symbol] into the VISITED set.
static void mark_visited(const int state_no, const int symbol, struct visited_element* visited) {
  if (visited->map[state_no] == NULL) {
    // 1st time we see state_no?
    visited->list[state_no] = visited->root;
    visited->root = state_no;
  }
  struct node *p = Allocate_node();
  p->value = symbol;
  p->next = visited->map[state_no];
  visited->map[state_no] = p;
}

struct CyclicTop {
  int top;
};

/// This procedure is a modified instantiation of the digraph algorithm
/// to compute the CYCLIC set of states.
static void compute_cyclic(const short state_no, short *stack, short *index_of, bool *cyclic, struct CyclicTop* topp, bool *null_nt, struct statset_type *statset) {
  stack[++topp->top] = state_no;
  const int indx = topp->top;
  cyclic[state_no] = false;
  index_of[state_no] = indx;
  const struct goto_header_type go_to = statset[state_no].go_to;
  for (int i = 1; i <= go_to.size; i++) {
    const int symbol = go_to.map[i].symbol;
    int act = go_to.map[i].action;
    if (act > 0 && null_nt[symbol]) {
      // We have a transition on a nullable nonterminal?
      if (index_of[act] == OMEGA) {
        compute_cyclic(act, stack, index_of, cyclic, topp, null_nt, statset);
      } else if (index_of[act] != INFINITY) {
        cyclic[state_no] = true;
      }
      cyclic[state_no] = cyclic[state_no] || cyclic[act];
      index_of[state_no] = MIN(index_of[state_no], index_of[act]);
    }
  }
  if (index_of[state_no] == indx) {
    int act;
    do {
      act = stack[topp->top--];
      index_of[act] = INFINITY;
    } while (act != state_no);
  }
}

///    In tracing an error, we will be moving backward in the state
/// automaton looking for items with the conflict symbol as look-ahead.
/// In the case of SLR, we may have to analoguously look at an
/// arbitrary set of items involved.  In moving around these graphs, it
/// is possible to encounter a cycle, in which case, we simply want to
/// back out of the cycle and try another path. We therefore need to
/// keep track of which nodes have already been visited.  For LALR
/// conflicts, we use the LA_PTR field of the GOTO_ELEMENTs as an index
/// to a BOOLEAN array LALR_VISITED.  For SLR conflicts, a boolean
/// array, SLR_VISITED, indexable by non-terminals, is used.  For
/// trace-backs to the root item, the boolean array SYMBOL_SEEN, also
/// indexable by non-terminals, is used.
static bool trace_root(const int lhs_symbol, struct CLIOptions* cli_options, bool *symbol_seen, short *item_list, short *nt_items, struct ruletab_type *rules, struct itemtab *item_table, short *rhs_sym) {
  if (lhs_symbol == accept_image) {
    return true;
  }
  if (symbol_seen[lhs_symbol]) {
    return false;
  }
  symbol_seen[lhs_symbol] = true;
  for (int item = nt_items[lhs_symbol]; item != NIL; item = item_list[item]) {
    if (trace_root(rules[item_table[item].rule_number].lhs, cli_options, symbol_seen, item_list, nt_items, rules, item_table, rhs_sym)) {
      print_item(item, cli_options, rules, item_table, rhs_sym);
      return true;
    }
  }
  return false;
}

/// The procedure below is invoked to retrace a path from the initial
/// item to a given item (ITEM_NO) passed to it as argument.
static void print_root_path(const int item_no, struct CLIOptions* cli_options, short *item_list, short *nt_items, struct ruletab_type *rules, struct itemtab *item_table, short *rhs_sym) {
  bool *symbol_seen = Allocate_boolean_array(num_non_terminals);
  symbol_seen -= num_terminals + 1;
  if (trace_root(rules[item_table[item_no].rule_number].lhs, cli_options, symbol_seen, item_list, nt_items, rules, item_table, rhs_sym)) {
    printf("\n"); /* Leave one blank line after root trace. */
  }
  symbol_seen += num_terminals + 1;
  ffree(symbol_seen);
}

/// This procedure takes as argument, a state number, STATE_NO, an
/// index into the goto map of state_no, GOTO_INDX, which identifies a
/// starting point for a search for the CONFLICT_SYMBOL. It attempts to
/// find a path in the automaton (from the starting point) that leads
/// to a state where the conflict symbol can be read. If a path is
/// found, all items along the path are printed and SUCCESS is returned.
///  Otherwise, FAILURE is returned.
static bool lalr_path_retraced(const int state_no, const int goto_indx, const int conflict_symbol, struct CLIOptions *cli_options, bool *lalr_visited, short *item_list, short *nt_items, JBitset first, struct node **adequate_item, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat, struct statset_type *statset, short *rhs_sym) {
  struct goto_header_type go_to = statset[state_no].go_to;
  lalr_visited[go_to.map[goto_indx].laptr] = true;
  bool found = false;
  const int state = go_to.map[goto_indx].action;
  int item;
  for (const struct node *p = state > 0 ? statset[state].kernel_items : adequate_item[-state]; p != NULL && !found; p = p->next) {
    item = p->value - 1;
    if (IS_IN_SET(first, item_table[item].suffix_index, conflict_symbol)) {
      // Conflict_symbol can be read in state?
      if (cli_options->trace_opt.value == TRACE_FULL.value) {
        print_root_path(item, cli_options, item_list, nt_items, rules, item_table, rhs_sym);
      }
      found = true;
    } else if (IS_IN_SET(first, item_table[item].suffix_index, empty)) {
      const long symbol = rules[item_table[item].rule_number].lhs;
      struct node *w = lpgaccess(state_no, item, in_stat, item_table);
      struct node *q;
      struct node *tail;
      for (q = w; q != NULL; tail = q, q = q->next) {
        go_to = statset[q->value].go_to;
        int ii;
        for (ii = 1; go_to.map[ii].symbol != symbol; ii++) {
        }
        if (!lalr_visited[go_to.map[ii].laptr]) {
          if (lalr_path_retraced(q->value, ii, conflict_symbol, cli_options, lalr_visited, item_list, nt_items, first, adequate_item, rules, item_table, in_stat, statset, rhs_sym)) {
            found = true;
            break;
          }
        }
      }
      for (; q != NULL; tail = q, q = q->next) {
      }
      free_nodes(w, tail);
    }
  }
  if (found) {
    print_item(item, cli_options, rules, item_table, rhs_sym);
  }
  return found;
}

/// In this procedure, we attempt to retrace an LALR conflict path
/// (there may be more than one) of CONFLICT_SYMBOL in the state
/// automaton that led to ITEM_NO in state STATE_NO.
static void print_relevant_lalr_items(const int state_no, const int item_no, const int conflict_symbol, struct CLIOptions *cli_options, short *item_list, short *nt_items, JBitset first, struct node **adequate_item, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat, struct statset_type *statset, short *rhs_sym) {
  const int lhs_symbol = rules[item_table[item_no].rule_number].lhs;
  if (lhs_symbol == accept_image) {
    // Do nothing.
  } else {
    /// LALR_VISITED is used to keep track of (state, nonterminal) pairs
    /// that are visited in tracing the path of a lalr conflict.SLR_VISITED
    /// is similarly used to keep track of nonterminal symbols that are
    /// visited in tracing the path of an slr conflict. SYMBOL_SEEN is used
    /// to keep track of nonterminal symbols that are visited in tracing a
    /// path to the start state (root).
    ///
    /// CYCLIC is a boolean vector used to identify states that can enter
    /// a cycle of transitions on nullable nonterminals.
    /// As the computation of CYCLIC requires a modified version of the
    /// digraph algorithm, the variables STACK, INDEX_OF and TOP are used
    /// for that algorithm.
    ///
    /// RMPSELF is a boolean vector that indicates whether a given
    /// non-terminal can right-most produce itself. It is only constructed
    /// when LALR_LEVEL > 1.
    bool *lalr_visited = Allocate_boolean_array(la_top + 1);
    struct node *v = lpgaccess(state_no, item_no, in_stat, item_table);
    struct node *p;
    struct node *tail;
    for (p = v; p != NULL; tail = p, p = p->next) {
      const struct goto_header_type go_to = statset[p->value].go_to;
      int ii;
      for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {
      }
      if (lalr_path_retraced(p->value, ii, conflict_symbol, cli_options, lalr_visited, item_list, nt_items, first, adequate_item, rules, item_table, in_stat, statset, rhs_sym)) {
        break;
      }
    }
    for (; p != NULL; tail = p, p = p->next) {
    }
    free_nodes(v, tail);
    ffree(lalr_visited);
  }
}

/// Add SYMBOL to the set of symbols CONFLICT_SYMBOLS[STATE_NO].
static void add_conflict_symbol(const int state_no, const int symbol, struct node **conflict_symbols) {
  struct node *p = Allocate_node();
  p->value = symbol;
  if (conflict_symbols[state_no] == NULL) {
    p->next = p;
  } else {
    p->next = conflict_symbols[state_no]->next;
    conflict_symbols[state_no]->next = p;
  }
  conflict_symbols[state_no] = p;
}

/// This function takes as argument a configuration STACK, a SYMBOL on
/// which a transition can be made in the configuration and a terminal
/// lookahead symbol, LA_SYMBOL. It executes the transition on SYMBOL
/// and simulates all paths taken in the automaton after that transition
/// until new state(s) are reached where a transition is possible on
/// the lookahead symbol. It then returns the new set of configurations
/// found on which a transition on LA_SYMBOL is possible.
static struct stack_element *follow_sources(struct stack_element *stack, int symbol, const int la_symbol, bool *cyclic, struct StackPool* sp, struct visited_element* visited, struct StackRoot* sr, bool *rmpself, JBitset first, struct LAIndex* lai, struct node **adequate_item, struct SRTable* srt, struct statset_type *statset, bool *null_nt, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat) {
  struct stack_element *configs = NULL; /* Initialize the output set of configurations */
  // If the starting configuration consists of a single state and
  // the initial [state, symbol] pair has already been visited,
  // return the null set. Otherwise, mark the pair visited and ...
  const int state_no = stack->state_number;
  if (stack->size == 1) {
    if (was_visited(state_no, symbol, visited) || (state_no == 1 && symbol == accept_image)) {
      return configs;
    }
    mark_visited(state_no, symbol, visited);
  }
  // Find the transition defined on the symbol...
  // If the SYMBOL is a nonterminal and we can determine that the
  // lookahead symbol (LA_SYMBOL) cannot possibly follow the
  // nonterminal in question in this context, we simply abandon the
  // search and return the NULL set.
  int act;
  if (IS_A_NON_TERMINAL(symbol)) {
    struct goto_header_type go_to = statset[state_no].go_to;
    int ii;
    for (ii = 1; go_to.map[ii].symbol != symbol; ii++) {
    }
    if (lai->la_index[go_to.map[ii].laptr] == OMEGA) {
      int stack_top = 0;
      la_traverse(state_no, ii, &stack_top, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
    }
    if (!IS_IN_SET(lai->la_set, go_to.map[ii].laptr, la_symbol)) {
      return configs;
    }
    act = go_to.map[ii].action;
  } else {
    struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
    int ii;
    for (ii = 1; sh.map[ii].symbol != symbol; ii++) {
    }
    act = sh.map[ii].action;
  }
  // If the ACTion on the symbol is a shift or a goto, ...
  if (act > 0) {
    // We check to see if the new state contains an action on the
    // lookahead symbol. If that's the case then we create a new
    // configuration by appending ACT to the starting configuration
    // and add this newly formed configuration to the set(list) of
    // configurations...
    struct shift_header_type sh = srt->shift[statset[act].shift_number];
    int ii;
    for (ii = 1; ii <= sh.size; ii++) {
      if (sh.map[ii].symbol == la_symbol) {
        break;
      }
    }
    if (ii <= sh.size) /* there is a transition on la_symbol in act */
    {
      struct stack_element *q = allocate_stack_element(sp);
      q->state_number = act;
      q->size = stack->size + 1;
      q->previous = stack;
      q->next = NULL;
      configs = q;
    }
    // If the new state cannot get into a cycle of null
    // transitions, we check to see if it contains any transition
    // on a nullable nonterminal. For each such transition, we
    // append the new state to the stack and recursively invoke
    // FOLLOW_SOURCES to check if a transition on LA_SYMBOL cannot
    // follow such a null transition.
    if (!cyclic[act]) {
      struct goto_header_type go_to = statset[act].go_to;
      for (ii = 1; ii <= go_to.size; ii++) {
        symbol = go_to.map[ii].symbol;
        if (null_nt[symbol]) {
          struct stack_element *q = allocate_stack_element(sp);
          q->state_number = act;
          q->size = stack->size + 1;
          q->previous = stack;
          q->next = NULL;
          struct stack_element *new_configs = follow_sources(q, symbol, la_symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat);
          if (new_configs == NULL) {
            free_stack_elements(q, q, sp);
          } else {
            add_dangling_stack_element(q, sp);
            configs = union_config_sets(configs, new_configs, sp);
          }
        }
      }
    }
  }
  // We now iterate over the kernel set of items associated with the
  // ACTion defined on SYMBOL...
  for (const struct node *item_ptr = act > 0 ? statset[act].kernel_items : adequate_item[-act]; item_ptr != NULL; item_ptr = item_ptr->next) {
    int item_no = item_ptr->value;
    // For each item that is a final item whose left-hand side
    // is neither the starting symbol nor a symbol that can
    // right-most produce itself...
    if (item_table[item_no].symbol == empty) {
      const int rule_no = item_table[item_no].rule_number;
      const int lhs_symbol = rules[rule_no].lhs;
      if (lhs_symbol != accept_image && !rmpself[lhs_symbol]) {
        // If the length of the prefix of the item preceeding
        // the dot is shorter that the length of the stack, we
        // retrace the item's path within the stack and
        // invoke FOLLOW_SOURCES with the prefix of the stack
        // where the item was introduced through closure, the
        // left-hand side of the item and the lookahead symbol.
        if (item_table[item_no].dot < stack->size) {
          struct stack_element *q = stack;
          for (int i = 1; i < item_table[item_no].dot; i++) {
            q = q->previous;
          }
          q = follow_sources(q, lhs_symbol, la_symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat);
          configs = union_config_sets(configs, q, sp);
        } else {
          struct node *tail;
          // Compute the item in the root state of the stack,
          // and find the root state...
          item_no -= stack->size;
          struct stack_element *q;
          for (q = stack; q->size != 1; q = q->previous) {
          }
          // We are now back in the main automaton, find all
          // sources where the item was introduced through
          // closure start a new configuration and invoke
          // FOLLOW_SOURCES with the appropriate arguments to
          // calculate the set of configurations associated
          // with these sources.
          struct node *v = lpgaccess(q->state_number, item_no, in_stat, item_table);
          for (struct node *p = v; p != NULL; tail = p, p = p->next) {
            q = allocate_stack_element(sp);
            q->state_number = p->value;
            q->size = 1;
            q->previous = NULL;
            q->next = NULL;
            struct stack_element *new_configs = follow_sources(q, lhs_symbol, la_symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat);
            if (new_configs == NULL) {
              free_stack_elements(q, q, sp);
            } else {
              add_dangling_stack_element(q, sp);
              configs = union_config_sets(configs, new_configs, sp);
            }
          }
          free_nodes(v, tail);
        }
      }
    }
  }
  return configs;
}

/// This function has a similar structure as FOLLOW_SOURCES.  But,
/// instead of computing configurations that can be reached, it
/// computes lookahead symbols that can be reached.  It takes as
/// argument a configuration STACK, a SYMBOL on which a transition can
/// be made in the configuration and a set variable, LOOK_AHEAD, where
/// the result is to be stored.  When NEXT_LA is invoked from the
/// outside, LOOK_AHEAD is assumed to be initialized to the empty set.
/// NEXT_LA first executes the transition on SYMBOL and thereafter, all
/// terminal symbols that can be read are added to LOOKAHEAD.
static void next_la(struct stack_element *stack, const int symbol, const JBitset look_ahead, struct StackRoot* sr, bool *rmpself, JBitset first, JBitset read_set, struct LAIndex* lai, struct node **adequate_item, struct SRTable* srt, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat, struct statset_type *statset) {
  // The only symbol that can follow the end-of-file symbol is the
  // end-of-file symbol.
  if (symbol == eoft_image) {
    SET_BIT_IN(look_ahead, 0, eoft_image);
    return;
  }
  const int state_no = stack->state_number;
  int act;
  // Find the transition defined on the symbol...
  if (IS_A_NON_TERMINAL(symbol)) {
    struct goto_header_type go_to = statset[state_no].go_to;
    int ii;
    for (ii = 1; go_to.map[ii].symbol != symbol; ii++) {
    }
    act = go_to.map[ii].action;
  } else {
    const struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
    int ii;
    for (ii = 1; sh.map[ii].symbol != symbol; ii++) {
    }
    act = sh.map[ii].action;
  }
  // If the ACTion on the symbol is a shift or a goto, then all
  // terminal symbols that can be read in ACT are added to
  // LOOK_AHEAD.
  if (act > 0) {
    SET_UNION(look_ahead, 0, read_set, act);
  }
  // We now iterate over the kernel set of items associated with the
  // ACTion defined on SYMBOL...
  // Recall that the READ_SET of ACT is but the union of the FIRST
  // map defined on the suffixes of the items in the kernel of ACT.
  for (const struct node *item_ptr = act > 0 ? statset[act].kernel_items : adequate_item[-act]; item_ptr != NULL; item_ptr = item_ptr->next) {
    int item_no = item_ptr->value;
    // For each item that is a final item whose left-hand side
    // is neither the starting symbol nor a symbol that can
    // right-most produce itself...
    if (IS_IN_SET(first, item_table[item_no - 1].suffix_index, empty)) {
      const int rule_no = item_table[item_no].rule_number;
      const int lhs_symbol = rules[rule_no].lhs;
      if (lhs_symbol != accept_image && !rmpself[lhs_symbol]) {
        // If the length of the prefix of the item preceeding
        // the dot is shorter that the length of the stack, we
        // retrace the item's path within the stack and
        // invoke NEXT_LA with the prefix of the stack
        // where the item was introduced through closure, the
        // left-hand side of the item and LOOK_AHEAD.
        if (item_table[item_no].dot < stack->size) {
          struct stack_element *q = stack;
          for (int i = 1; i < item_table[item_no].dot; i++) {
            q = q->previous;
          }
          next_la(q, lhs_symbol, look_ahead, sr, rmpself, first, read_set, lai, adequate_item, srt, rules, item_table, in_stat, statset);
        } else {
          struct node *tail;
          // Compute the item in the root state of the stack,
          // and find the root state...
          item_no -= stack->size;
          struct stack_element *q;
          for (q = stack; q->size != 1; q = q->previous) {
          }
          // We are now back in the main automaton, find all
          // sources where the item was introduced through
          // closure and add all terminal symbols in the
          // follow set of the left-hand side symbol in each
          // source to LOOK_AHEAD.
          struct node *v = lpgaccess(q->state_number, item_no, in_stat, item_table);
          for (struct node *p = v; p != NULL; tail = p, p = p->next) {
            struct goto_header_type go_to = statset[p->value].go_to;
            int ii;
            for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {
            }
            // If look-ahead after left hand side is not
            // yet computed,call LA_TRAVERSE to compute it.
            if (lai->la_index[go_to.map[ii].laptr] == OMEGA) {
              int stack_top = 0;
              la_traverse(p->value, ii, &stack_top, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
            }
            SET_UNION(look_ahead, 0, lai->la_set, go_to.map[ii].laptr);
          }
          free_nodes(v, tail);
        }
      }
    }
  }
}

/// This function takes as argument an array, STACK_SEEN, with
/// STATE_TABLE_SIZE elements (indexable in the range
/// 0..STATE_TABLE_SIZE-1) which is the base of a hash table and a
/// STACK. It searches the hash table to see if it already contained
/// the stack in question. If yes, it returns TRUE. Otherwise, it
/// inserts the stack into the table and returns FALSE.
static bool stack_was_seen(struct stack_element **stack_seen, struct stack_element *stack) {
  unsigned long hash_address = stack->size; /* Initialize hash address */
  for (struct stack_element *p = stack; p != NULL; p = p->previous) {
    hash_address += p->state_number;
  }
  hash_address %= STATE_TABLE_SIZE;
  for (struct stack_element *p = stack_seen[hash_address]; p != NULL; p = p->link) {
    if (stack->size == p->size) {
      struct stack_element *q;
      struct stack_element *r;
      for (q = stack, r = p; q != NULL; q = q->previous, r = r->previous) {
        if (q->state_number != r->state_number) {
          break;
        }
      }
      if (q == NULL) {
        return true;
      }
    }
  }
  stack->link = stack_seen[hash_address];
  stack_seen[hash_address] = stack;
  return false;
}

/// STATE_TO_RESOLVE_CONFLICTS is a function that attempts to resolve
/// conflicts by doing more look-ahead.  If the conflict resolution
/// is successful, then a new state is created and returned; otherwise,
/// the NULL pointer is returned.
static struct state_element *state_to_resolve_conflicts(struct sources_element sources, int la_symbol, int level, struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct state_element **shift_table, bool *cyclic, struct StackPool* sp, struct visited_element* visited, struct STRS* strs, struct StackRoot* sr, bool *rmpself, JBitset first, JBitset read_set, struct LAIndex* lai, struct node **adequate_item, struct SRTable* srt, bool *null_nt, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat, struct statset_type *statset) {
  struct sources_element new_sources = allocate_sources();
  struct node **action;
  calloc0(action, num_terminals + 1, struct node *);
  short *symbol_list = Allocate_short_array(num_terminals + 1);
  short *action_list = Allocate_short_array(num_terminals + 1);
  short *rule_count = Allocate_short_array(num_rules + 1);
  JBitset look_ahead;
  calloc0_set(look_ahead, 1, dss->term_set_size);
  struct state_element **la_shift_state;
  calloc0(la_shift_state, num_terminals + 1, struct state_element *);
  // Initialize new lookahead state. Initialize counters. Check and
  // adjust HIGHEST_LEVEL reached so far, if necessary.
  struct state_element *state = NULL;
  int num_shift_actions = 0;
  int num_reduce_actions = 0;
  short shift_root = NIL;
  short reduce_root = NIL;
  if (level > strs->highest_level) {
    strs->highest_level = level;
  }
  // One of the parameters received is a SOURCES map whose domain is
  // a set of actions and each of these actions is mapped into a set
  // of configurations that can be reached after that action is
  // executed (in the state where the conflicts were detected).
  // In this loop, we compute an ACTION map which maps each each
  // terminal symbol into 0 or more actions in the domain of SOURCES.
  //
  // NOTE in a sources map, if a configuration is associated with
  // more than one action then the grammar is not LALR(k) for any k.
  // We check for that condition below. However, this check is there
  // for purely cosmetic reason. It is not necessary for the
  // algorithm to work correctly and its removal will speed up this
  // loop somewhat (for conflict-less input).
  // The first loop below initializes the hash table used for
  // lookups ...
  for (int i = 0; i < STATE_TABLE_SIZE; i++) {
    sources.stack_seen[i] = NULL;
  }
  short symbol_root = NIL;
  for (int act = sources.root; act != NIL; act = sources.list[act]) {
    // For each action we iterate over its associated set of
    // configurations and invoke NEXT_LA to compute the lookahead
    // set for that configuration. These lookahead sets are in
    // turn unioned together to form a lookahead set for the
    // action in question.
    INIT_SET(look_ahead);
    for (struct stack_element *stack = sources.configs[act]; stack != NULL; stack = stack->next) {
      if (stack_was_seen(sources.stack_seen, stack)) {
        // This is the superfluous code mentioned above!
        strs->highest_level = INFINITY;
        goto clean_up_and_return;
      }
      next_la(stack, la_symbol, look_ahead, sr, rmpself, first, read_set, lai, adequate_item, srt, rules, item_table, in_stat, statset);
    }
    RESET_BIT(look_ahead, empty); /* EMPTY never in LA set */
    // For each lookahead symbol computed for this action, add an
    // action to the ACTION map and keep track of the symbols on
    // which any action is defined.
    // If new conflicts are detected and we are already at the
    // lookahead level requested, we terminate the computation...
    int count = 0;
    for ALL_TERMINALS3(symbol) {
      if (IS_ELEMENT(look_ahead, symbol)) {
        count++;
        if (action[symbol] == NULL) {
          symbol_list[symbol] = symbol_root;
          symbol_root = symbol;
        } else if (level == cli_options->lalr_level) {
          goto clean_up_and_return;
        }
        struct node *p = Allocate_node();
        p->value = act;
        p->next = action[symbol];
        action[symbol] = p;
      }
    }
    // If the action in question is a reduction then we keep track
    // of how many times it was used.
    if (act >= 0 && act <= num_rules) {
      rule_count[act] = count;
    }
  }
  // We now iterate over the symbols on which actions are defined.
  // If we detect conflicts on any symbol, we compute new sources
  // and try to recover by computing more lookahead. Otherwise, we
  // update the counts and create two lists: a list of symbols on
  // which shift actions are defined and a list of symbols on which
  // reduce actions are defined.
  for (int symbol = symbol_root; symbol != NIL; symbol = symbol_list[symbol]) {
    // We have four cases to consider:
    //    1. There are conflicts on SYMBOL
    //    2. The action on SYMBOL is a shift-reduce
    //    3. The action on SYMBOL is a shift
    //    4. The action on SYMBOL is a reduce
    if (action[symbol]->next != NULL) {
      new_sources = clear_sources(new_sources, sp);
      struct node *tail;
      for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
        int act = p->value;
        if (act >= 0 && act <= num_rules) {
          rule_count[act]--;
        }
        clear_visited(visited);
        for (struct stack_element *stack = sources.configs[act]; stack != NULL; stack = stack->next) {
          struct stack_element *new_configs;
          new_configs = follow_sources(stack, la_symbol, symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat);
          new_sources = add_configs(new_sources, act, new_configs, sp);
        }
      }
      free_nodes(action[symbol], tail);
      action[symbol] = NULL;
      state = state_to_resolve_conflicts(new_sources, symbol, level + 1, cli_options, dss, shift_table, cyclic, sp, visited, strs, sr, rmpself, first, read_set, lai, adequate_item, srt, null_nt, rules, item_table, in_stat, statset);
      if (state == NULL) {
        goto clean_up_and_return;
      }
      la_shift_state[symbol] = state;
      struct node *p = Allocate_node();
      p->value = state->state_number;
      p->next = NULL;
      action[symbol] = p;
      num_shift_actions++;
      action_list[symbol] = shift_root;
      shift_root = symbol;
    } else if (action[symbol]->value < 0) {
      num_shift_actions++;
      action_list[symbol] = shift_root;
      shift_root = symbol;
    } else if (action[symbol]->value > num_rules) {
      num_shift_actions++;
      action[symbol]->value -= num_rules;
      action_list[symbol] = shift_root;
      shift_root = symbol;
    } else {
      num_reduce_actions++;
      action_list[symbol] = reduce_root;
      reduce_root = symbol;
    }
  }
  // We now iterate over the reduce actions in the domain of sources
  // and compute a default action.
  int default_rule = OMEGA;
  int count = 0;
  for (int act = sources.root; act != NIL; act = sources.list[act]) {
    if (act >= 0 && act <= num_rules) {
      if (rule_count[act] > count) {
        count = rule_count[act];
        default_rule = act;
      }
    }
  }
  // By now, we are ready to create a new look-ahead state. The
  // actions for the state are in the ACTION vector, and the
  // constants: NUM_SHIFT_ACTIONS and NUM_REDUCE_ACTIONS indicate
  // the number of shift and reduce actions in the ACTION vector.
  // Note that the IN_STATE field of each look-ahead state created
  // is initially set to the number associated with that state. If
  // all the conflicts detected in the state, S, that requested the
  // creation of a look-ahead state are resolved, then this field
  // is updated with S.
  // Otherwise, this field indicates that this look-ahead state is
  // dangling - no other state point to it.
  talloc0p(&state, struct state_element);
  state->link = strs->la_state_root;
  strs->la_state_root = state;
  max_la_state++;
  state->symbol = la_symbol;
  state->state_number = max_la_state;
  state->in_state = max_la_state; /* Initialize it to something! */
  // If there are any shift-actions in this state, we create a shift
  // map for them if one does not yet exist, otherwise, we reuse the
  // old existing one.
  if (num_shift_actions > 0) {
    unsigned long hash_address;
    struct shift_header_type sh;
    struct state_element *p_inner;
    // In this loop, we compute the hash address as the number of
    // shift actions, plus the sum of all the symbols on which a
    // shift action is defined.  As a side effect, we also take
    // care of some other issues. Shift actions which were encoded
    // to distinguish them from reduces action are decoded.
    // The counters for shift and shift-reduce actions are updated.
    // For all Shift actions to look-ahead states, the IN_STATE
    // field of these look-ahead target states are updated.
    hash_address = num_shift_actions; /* Initialize hash address */
    for (int symbol = shift_root; symbol != NIL; symbol = action_list[symbol]) {
      hash_address += symbol;
      if (action[symbol]->value < 0) {
        num_shift_reduces++;
      } else if (action[symbol]->value <= num_states) {
        num_shifts++;
      } else {
        // lookahead-shift
        la_shift_state[symbol]->in_state = max_la_state;
      }
    }
    hash_address %= SHIFT_TABLE_SIZE;
    // Search list associated with HASH_ADDRESS, and if the shift
    // map in question is found, update the SHIFT, and SHIFT_NUMBER
    // fields of the new Look-Ahead State.
    for (p_inner = shift_table[hash_address]; p_inner != NULL; p_inner = p_inner->next_shift) {
      // Search hash table for shift map
      sh = p_inner->shift;
      if (sh.size == num_shift_actions) {
        int ii;
        for (ii = 1; ii <= num_shift_actions; ii++) {
          // compare shift maps
          if (sh.map[ii].action != action[sh.map[ii].symbol]->value) {
            break;
          }
        }
        if (ii > num_shift_actions) /* are they equal ? */
        {
          state->shift = sh;
          state->shift_number = p_inner->shift_number;
          break;
        }
      }
    }
    // Shift map was not found.  We have to create a new one and
    // insert it into the table.
    if (p_inner == NULL) {
      num_shift_maps++;
      sh = Allocate_shift_map(num_shift_actions);
      state->shift = sh;
      state->shift_number = num_shift_maps;
      state->next_shift = shift_table[hash_address];
      shift_table[hash_address] = state;
      for (int symbol = shift_root, i = 1; symbol != NIL; symbol = action_list[symbol], i++) {
        sh.map[i].symbol = symbol;
        sh.map[i].action = action[symbol]->value;
      }
    }
  } else {
    state->shift.size = 0;
    state->shift_number = 0;
  }
  // Construct Reduce map.
  // When SPACE or TIME tables are requested, no default actions are
  // taken.
Build_reduce_map: {
    struct reduce_header_type red;
    if (default_rule != OMEGA &&
        cli_options->table_opt.value != OPTIMIZE_TIME.value &&
        cli_options->table_opt.value != OPTIMIZE_SPACE.value &&
        cli_options->default_opt.value != OPT_0.value)
      num_reduce_actions -= rule_count[default_rule];
    num_reductions += num_reduce_actions;
    red = Allocate_reduce_map(num_reduce_actions);
    state->reduce = red;
    for (int symbol = reduce_root, i_inner = 1; symbol != NIL; symbol = action_list[symbol]) {
      if (cli_options->default_opt.value == OPT_0.value ||
          action[symbol]->value != default_rule ||
          cli_options->table_opt.value == OPTIMIZE_TIME.value ||
          cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
        red.map[i_inner].symbol = symbol;
        red.map[i_inner].rule_number = action[symbol]->value;
        i_inner++;
      }
    }
    red.map[0].symbol = DEFAULT_SYMBOL;
    if (cli_options->default_opt.value > OPT_0.value) {
      red.map[0].rule_number = default_rule;
    } else {
      red.map[0].rule_number = OMEGA;
    }
  }
  // Release all space allocated to process this lookahead state and
  // return.
clean_up_and_return:
  free_sources(new_sources, sp);
  for (int symbol = symbol_root; symbol != NIL; symbol = symbol_list[symbol]) {
    struct node *tail;
    for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
    }
    if (action[symbol] != NULL) {
      free_nodes(action[symbol], tail);
    }
  }
  ffree(action);
  ffree(symbol_list);
  ffree(action_list);
  ffree(rule_count);
  ffree(look_ahead.raw);
  ffree(la_shift_state);
  return state;
}

/// This procedure is invoked when LALR_LEVEL > 1 to construct the
/// RMPSELF set which identifies the nonterminals that can right-most
/// produce themselves. It takes as argumen the map PRODUCES which
/// identifies for each nonterminal the set of nonterminals that it can
/// right-most produce.
bool* init_rmpself(const JBitset produces) {
  bool *rmpself = Allocate_boolean_array(num_non_terminals);
  rmpself -= num_terminals + 1;
  // Note that each element of the map produces is a boolean vector
  // that is indexable in the range 1..num_non_terminals. Since each
  // nonterminal is offset by the value num_terminals (to distinguish
  // it from the terminals),it must therefore be adjusted accordingly
  // when dereferencing an element in the range of the produces map.
  for ALL_NON_TERMINALS3(nt) {
    rmpself[nt] = IS_IN_SET(produces, nt, nt - num_terminals);
  }
  return rmpself;
}

/// Free all support structures that were allocated to help compute
/// additional lookahead.
void exit_lalrk_process(const struct CLIOptions *cli_options, struct state_element **shift_table, bool *cyclic, struct StackPool* sp, struct visited_element* visited, struct SourcesElementSources* ses, bool *rmpself) {
  if (cli_options->lalr_level > 1) {
    rmpself += num_terminals + 1;
    ffree(rmpself);
    ffree(shift_table);
    ffree(cyclic);
    free_sources(ses->sources, sp);
    clear_visited(visited);
    ffree(visited->map);
    ffree(visited->list);
  }
}

/// If we had to report conflicts, free the SLR support structures.
void free_conflict_space(short *item_list, short *nt_items) {
  if (nt_items != NULL) {
    nt_items += num_terminals + 1;
    ffree(nt_items);
    ffree(item_list);
  }
}

/// If conflicts were detected and LALR(k) processing was requested,
/// where k > 1, then we attempt to resolve the conflicts by computing
/// more lookaheads. Shift-Reduce conflicts are processed first,
/// followed by Reduce-Reduce conflicts.
struct ConflictCounter resolve_conflicts(const int state_no, struct node **action, const short *symbol_list, const int reduce_root, struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct state_element **shift_table, bool *cyclic, short *item_list, short *nt_items, struct StackPool* sp, struct ConflictPool* cp, struct visited_element* visited, struct SourcesElementSources* ses, struct STRS* strs, struct StackRoot* sr, bool *rmpself, JBitset first, JBitset read_set, struct LAIndex* lai, struct node **conflict_symbols, struct node **adequate_item, struct SRTable* srt, struct lastats_type *lastats, bool *null_nt, struct node **in_stat, struct ruletab_type *rules, struct itemtab *item_table, struct statset_type *statset, short *rhs_sym) {
  long num_sr_conflicts = 0;
  long num_rr_conflicts = 0;
  // Note that a shift action to a state "S" is encoded with the
  // value (S+NUM_RULES) to help distinguish it from reduce actions.
  // Reduce actions lie in the range [0..NUM_RULES]. Shift-reduce
  // actions lie in the range [-NUM_RULES..-1].
  struct sr_conflict_element *sr_conflict_root = NULL;
  const struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
  for (int i = 1; i <= sh.size; i++) {
    int symbol = sh.map[i].symbol;
    if (cli_options->single_productions_bit && action[symbol] != NULL) {
      add_conflict_symbol(state_no, symbol, conflict_symbols);
    }
    if (cli_options->lalr_level > 1 && action[symbol] != NULL) {
      ses->sources = clear_sources(ses->sources, sp);
      struct stack_element *q = allocate_stack_element(sp);
      q->state_number = state_no;
      q->size = 1;
      q->previous = NULL;
      q->next = NULL;
      int act = sh.map[i].action;
      if (act > 0) {
        ses->sources = add_configs(ses->sources, act + num_rules, q, sp);
      } else {
        ses->sources = add_configs(ses->sources, act, q, sp);
      }
      struct node *tail;
      for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
        int item_no = p->value;
        act = item_table[item_no].rule_number;
        int lhs_symbol = rules[act].lhs;
        clear_visited(visited);
        struct node *v = lpgaccess(state_no, item_no, in_stat, item_table);
        for (struct node *s = v; s != NULL; tail = s, s = s->next) {
          q = allocate_stack_element(sp);
          q->state_number = s->value;
          q->size = 1;
          q->previous = NULL;
          q->next = NULL;
          struct stack_element *new_configs = follow_sources(q, lhs_symbol, symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat);
          if (new_configs == NULL) {
            free_stack_elements(q, q, sp);
          } else {
            add_dangling_stack_element(q, sp);
            ses->sources = add_configs(ses->sources, act, new_configs, sp);
          }
        }
        free_nodes(v, tail);
      }
      // The function STATE_TO_RESOLVE_CONFLICTS returns a pointer
      // value to a STATE_ELEMENT which has been constructed to
      // resolve the conflicts in question. If the value returned by
      // that function is NULL, then it was not possible to resolve
      // the conflicts.  In any case, STATE_TO_RESOLVE_CONFLICTS
      // frees the space that is used by the action map headed by
      // ACTION_ROOT.
      struct state_element *state = state_to_resolve_conflicts(ses->sources, symbol, 2, cli_options, dss, shift_table, cyclic, sp, visited, strs, sr, rmpself, first, read_set, lai, adequate_item, srt, null_nt, rules, item_table, in_stat, statset);
      if (state != NULL) {
        state->in_state = state_no;
        free_nodes(action[symbol], tail);
        action[symbol] = NULL;
      }
    }
    // If unresolved shift-reduce conflicts are detected on symbol,
    //  add them to the list of conflicts so they can be reported
    // (if the CONFLICT option is on) and count them.
    if (action[symbol] != NULL) {
      int act = sh.map[i].action;
      struct node *tail;
      for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
        if (cli_options->conflicts_bit) {
          struct sr_conflict_element *q_inner = allocate_conflict_element(cp);
          q_inner->state_number = act;
          q_inner->item = p->value;
          q_inner->symbol = symbol;
          q_inner->next = sr_conflict_root;
          sr_conflict_root = q_inner;
        }
        num_sr_conflicts++;
      }
      // Remove reduce actions defined on symbol so as to give
      // precedence to the shift.
      free_nodes(action[symbol], tail);
      action[symbol] = NULL;
    }
  }
  // We construct a map from each action to a list of states as we
  // did for the Shift-reduce conflicts. A boolean vector ITEM_SEEN
  // is used to prevent duplication of actions. This problem does
  // not occur with Shift-Reduce conflicts.
  struct rr_conflict_element *rr_conflict_root = NULL;
  for (int symbol = reduce_root; symbol != NIL; symbol = symbol_list[symbol]) {
    if (action[symbol] != NULL) {
      if (cli_options->single_productions_bit && action[symbol]->next != NULL) {
        add_conflict_symbol(state_no, symbol, conflict_symbols);
      }
      if (cli_options->lalr_level > 1 && action[symbol]->next != NULL) {
        ses->sources = clear_sources(ses->sources, sp);
        struct node *tail;
        for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
          int item_no = p->value;
          int act = item_table[item_no].rule_number;
          int lhs_symbol = rules[act].lhs;
          clear_visited(visited);
          struct node *v = lpgaccess(state_no, item_no, in_stat, item_table);
          for (struct node *s = v; s != NULL; tail = s, s = s->next) {
            struct stack_element *q = allocate_stack_element(sp);
            q->state_number = s->value;
            q->size = 1;
            q->previous = NULL;
            q->next = NULL;
            struct stack_element *new_configs = follow_sources(q, lhs_symbol, symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat);
            if (new_configs == NULL) {
              free_stack_elements(q, q, sp);
            } else {
              add_dangling_stack_element(q, sp);
              ses->sources = add_configs(ses->sources, act, new_configs, sp);
            }
          }
          free_nodes(v, tail);
        }
        //     STATE_TO_RESOLVE_CONFLICTS will return a pointer to a
        // STATE_ELEMENT if the conflicts were resolvable with more
        // lookaheads, otherwise, it returns NULL.
        struct state_element *state = state_to_resolve_conflicts(ses->sources, symbol, 2, cli_options, dss, shift_table, cyclic, sp, visited, strs, sr, rmpself, first, read_set, lai, adequate_item, srt, null_nt, rules, item_table, in_stat, statset);
        if (state != NULL) {
          state->in_state = state_no;
          free_nodes(action[symbol], tail);
          action[symbol] = NULL;
        }
      }
      // If unresolved reduce-reduce conflicts are detected on
      // symbol, add them to the list of conflicts so they can be
      // reported (if the CONFLICT option is on) and count them.
      if (action[symbol] != NULL) {
        int act = action[symbol]->value;
        struct node *tail;
        for (struct node *p = action[symbol]->next; p != NULL; tail = p, p = p->next) {
          if (cli_options->conflicts_bit) {
            struct rr_conflict_element *q_inner = allocate_conflict_element(cp);
            q_inner->symbol = symbol;
            q_inner->item1 = act;
            q_inner->item2 = p->value;
            q_inner->next = rr_conflict_root;
            rr_conflict_root = q_inner;
          }
          num_rr_conflicts++;
        }
        // Remove all reduce actions that are defined on symbol
        // except the first one. That rule is the one with the
        // longest right-hand side that was associated with symbol.
        // See code in MKRED.C.
        if (action[symbol]->next != NULL) {
          free_nodes(action[symbol]->next, tail);
          action[symbol]->next = NULL;
        }
      }
    }
  }
  // If any unresolved conflicts were detected, process them.
  if (sr_conflict_root != NULL || rr_conflict_root != NULL) {
    // If conflicts are detected, they are placed in two lists headed by
    // SR_CONFLICT_ROOT and RR_CONFLICT_ROOT.  We scan these lists, and
    // report the conflicts.
    int symbol;
    int rule_no;
    char temp[SYMBOL_SIZE + 1];
    if (nt_items == NULL) {
      // Conflicts Initialization
      //
      /// This routine is invoked when a grammar contains conflicts, and the
      /// first conflict is detected.
      ///
      // NT_ITEMS and ITEM_LIST are used in reporting SLR conflicts, and
      // in recreating paths from the Start item. See the routines
      // PRINT_RELEVANT_SLR_ITEMS and PRINT_ROOT_PATH.
      nt_items = Allocate_short_array(num_non_terminals);
      nt_items -= num_terminals + 1;
      item_list = Allocate_short_array(num_items + 1);
      fill_in(msg_line, (PRINT_LINE_SIZE - 11) / 2 - 1, '-');
      printf("\n%s CONFLICTS %s\n", msg_line, msg_line);
      //   SLR conflicts may be caused by a symbol in the FOLLOW set of a
      // left hand side, which is not actually in the LALR look-ahead set in
      // that context.  Therefore, there may not exist a path in the state
      // automaton from the state where the conflict was detected to another
      // state where it was introduced.  In such a case, we try to retrace a
      // path that contributed the conflict-symbol to the FOLLOW set via a
      // sequence of productions.
      //
      // To assist in this task, we build below a map from each non-terminal
      // A to the set of items of which A is the dot SYMBOL. I.e., all items
      // of the form [x .A y] where x and y are arbitrary strings, and A is
      // a non-terminal. This map is also used in retracing a path from the
      // Start item to any other item.
      for ALL_NON_TERMINALS3(symbol) {
        nt_items[symbol] = NIL;
      }
      for ALL_ITEMS3(item_no) {
        if (IS_A_NON_TERMINAL(item_table[item_no].symbol)) {
          item_list[item_no] = nt_items[item_table[item_no].symbol];
          nt_items[item_table[item_no].symbol] = item_no;
        }
      }
    }
    print_state(state_no, cli_options, adequate_item, srt, lastats, statset, in_stat, rules, item_table, rhs_sym); /* Print state containing conflicts */
    // Process shift-reduce conflicts.
    if (sr_conflict_root != NULL) {
      struct sr_conflict_element *tail;
      for (struct sr_conflict_element *p = sr_conflict_root; p != NULL; tail = p, p = p->next) {
        symbol = p->symbol;
        rule_no = item_table[p->item].rule_number;
        restore_symbol(temp, RETRIEVE_STRING(symbol), cli_options->ormark, cli_options->escape);
        printf("*** Shift/reduce conflict on \"%s\" with rule %d\n", temp, rule_no);
        if (cli_options->trace_opt.value != NOTRACE.value) {
          print_relevant_lalr_items(state_no, p->item, symbol, cli_options, item_list, nt_items, first, adequate_item, rules, item_table, in_stat, statset, rhs_sym);
          print_item(p->item, cli_options, rules, item_table, rhs_sym);
        }
      }
      free_conflict_elements(sr_conflict_root, tail, cp);
    }
    // Process reduce-reduce conflicts.
    if (rr_conflict_root != NULL) {
      struct rr_conflict_element *tail;
      for (struct rr_conflict_element *p = rr_conflict_root; p != NULL; tail = p, p = p->next) {
        symbol = p->symbol;
        const int n = item_table[p->item1].rule_number;
        rule_no = item_table[p->item2].rule_number;
        restore_symbol(temp, RETRIEVE_STRING(symbol), cli_options->ormark, cli_options->escape);
        printf("*** Reduce/reduce conflict on \"%s\" between rule %d and %d\n", temp, n, rule_no);
        if (cli_options->trace_opt.value != NOTRACE.value) {
          print_relevant_lalr_items(state_no, p->item1, symbol, cli_options, item_list, nt_items, first, adequate_item, rules, item_table, in_stat, statset, rhs_sym);
          print_item(p->item1, cli_options, rules, item_table, rhs_sym);
          fill_in(msg_line, PRINT_LINE_SIZE - 3, '-');
          printf("\n%s", msg_line);
          print_relevant_lalr_items(state_no, p->item2, symbol, cli_options, item_list, nt_items, first, adequate_item, rules, item_table, in_stat, statset, rhs_sym);
          print_item(p->item2, cli_options, rules, item_table, rhs_sym);
        }
      }
      free_conflict_elements(rr_conflict_root, tail, cp);
    }
  }
  free_dangling_stack_elements(sp);
  return (struct ConflictCounter) {
    .num_rr_conflicts = 0,
    .num_sr_conflicts = 0,
  };
}

/// Transfer the look-ahead states to their permanent destination, the
/// array LASTATS and update the original automaton with the relevant
/// transitions into the lookahead states.
void create_lastats(struct STRS* strs, struct SRTable* srt, struct statset_type *statset) {
  // Allocate LASTATS structure to permanently construct lookahead
  // states and reallocate SHIFT map as we may have to construct
  // new shift maps.
  calloc0(lastats, max_la_state - num_states, struct lastats_type);
  lastats -= num_states + 1;
  realloc0(srt->shift, max_la_state + 1, struct shift_header_type);
  // Allocate temporary space used to construct final lookahead
  // states.
  struct state_element **new_shift_actions;
  calloc0(new_shift_actions, num_states + 1, struct state_element *);
  short *shift_action = Allocate_short_array(num_terminals + 1);
  short *shift_list = Allocate_short_array(num_terminals + 1);
  short *shift_count = Allocate_short_array(max_la_state + 1);
  short *state_list = Allocate_short_array(max_la_state + 1);
  // The array shift_action will be used to construct a shift map
  // for a given state. It is initialized here to the empty map.
  // The array shift_count is used to count how many references
  // there are to each shift map.
  for ALL_TERMINALS3(symbol) {
    shift_action[symbol] = OMEGA;
  }
  for (int i = 0; i <= max_la_state; i++) {
    shift_count[i] = 0;
  }
  for ALL_STATES3(state_no) {
    shift_count[statset[state_no].shift_number]++;
  }
  // Traverse the list of lookahead states and initialize the
  // final lastat element appropriately. Also, construct a mapping
  // from each relevant initial state into the list of lookahead
  // states into which it can shift. We also keep track of these
  // initial states in a list headed by state_root.
  int state_root = NIL;
  for (struct state_element *p = strs->la_state_root; p != NULL; p = p->link) {
    lastats[p->state_number].in_state = p->in_state;
    lastats[p->state_number].shift_number = p->shift_number;
    lastats[p->state_number].reduce = p->reduce;
    if (p->shift.size != 0) {
      srt->shift[p->shift_number] = p->shift;
    }
    const int state_no = p->in_state;
    if (state_no <= num_states) {
      if (new_shift_actions[state_no] == NULL) {
        state_list[state_no] = state_root;
        state_root = state_no;
      }
      p->next_shift = new_shift_actions[state_no];
      new_shift_actions[state_no] = p;
    }
  }
  // We now traverse the list of initial states that can shift into
  // lookahead states and update their shift map appropriately.
  for (int state_no = state_root; state_no != NIL; state_no = state_list[state_no]) {
    // Copy the shift map associated with STATE_NO into the direct
    // access map SHIFT_ACTION.
    const int shift_no = statset[state_no].shift_number;
    struct shift_header_type sh = srt->shift[shift_no];
    int shift_root = NIL;
    for (int i = 1; i <= sh.size; i++) {
      const int symbol = sh.map[i].symbol;
      shift_action[symbol] = sh.map[i].action;
      shift_list[symbol] = shift_root;
      shift_root = symbol;
    }
    // Add the lookahead shift transitions to the initial shift
    // map.
    int shift_size = sh.size;
    for (struct state_element *p = new_shift_actions[state_no]; p != NULL; p = p->next_shift) {
      if (shift_action[p->symbol] == OMEGA) {
        shift_size++;
        shift_list[p->symbol] = shift_root;
        shift_root = p->symbol;
      } else if (shift_action[p->symbol] < 0) {
        num_shift_reduces--;
      } else {
        num_shifts--;
      }
      shift_action[p->symbol] = p->state_number;
    }
    // There are two conditions under which we have to construct
    // a new shift map:
    //     1. The initial shift map was shared with other states.
    //     2. The updated shift map contains more elements than
    //        the initial one.
    if (shift_count[shift_no] > 1) {
      shift_count[shift_no]--;
      num_shift_maps++;
      sh = Allocate_shift_map(shift_size);
      srt->shift[num_shift_maps] = sh;
      statset[state_no].shift_number = num_shift_maps;
    } else if (shift_size > sh.size) {
      sh = Allocate_shift_map(shift_size);
      srt->shift[shift_no] = sh;
    }
    // Reconstruct the relevant shift map.
    for (int symbol = shift_root, i = 1; symbol != NIL; shift_action[symbol] = OMEGA, symbol = shift_list[symbol], i++) {
      sh.map[i].symbol = symbol;
      sh.map[i].action = shift_action[symbol];
    }
  }
  // Free all local temporary structures and return.
  ffree(new_shift_actions);
  ffree(shift_action);
  ffree(shift_list);
  ffree(shift_count);
  ffree(state_list);
}















/// Given an item of the form: [x .A y], where x and y are arbitrary strings,
/// and A is a non-terminal, we pretrace the path(s) in the automaton  that
/// will be followed in computing the look-ahead set for that item in
/// STATE_NO.  A number is assigned to all pairs (S, B), where S is a state,
/// and B is a non-terminal, involved in the paths. GOTO_INDX points to the
/// GOTO_ELEMENT of (STATE_NO, A).
void trace_lalr_path(const int state_no, const int goto_indx, struct CLIOptions *cli_options, int *la_base, JBitset first, struct node **adequate_item, struct ruletab_type *rules, struct node **in_stat, struct statset_type *statset, struct itemtab *item_table) {
  //  If STATE is a state number we first check to see if its base
  // look-ahead set is a special one that does not contain EMPTY and
  // has already been assigned a slot that can be reused.
  // ((LA_BASE[STATE] != OMEGA) signals this condition.)
  // NOTE that look-ahead follow sets are shared only when the maximum
  // look-ahead level allowed is 1 and single productions will not be
  // removed. If either (or both) of these conditions is true, we need
  // to have a unique slot assigned to each pair [S, A] (where S is a
  // state, and A is a non-terminal) in the automaton.
  const struct goto_header_type go_to = statset[state_no].go_to;
  const int state = go_to.map[goto_indx].action;
  struct node *r;
  if (state > 0) {
    if (la_base[state] != OMEGA && cli_options->lalr_level == 1 && !cli_options->single_productions_bit) {
      go_to.map[goto_indx].laptr = la_base[state];
      return;
    }
    r = statset[state].kernel_items;
  } else {
    r = adequate_item[-state];
  }
  // At this point, R points to a list of items which are the successors
  // of the items needed to initialize the Look-ahead follow sets.  If
  // anyone of these items contains EMPTY, we trace the Digraph for other
  // look-ahead follow sets that may be needed, and signal this fact
  // using the variable CONTAINS_EMPTY.
  la_top++; /* allocate new slot */
  go_to.map[goto_indx].laptr = la_top;
  bool contains_empty = false;
  for (; r != NULL; r = r->next) {
    const int item = r->value - 1;
    if (IS_IN_SET(first, item_table[item].suffix_index, empty)) {
      contains_empty = true;
      const int symbol = rules[item_table[item].rule_number].lhs;
      struct node *w = lpgaccess(state_no, item, in_stat, item_table);
      struct node *p;
      for (struct node *t = w; t != NULL; p = t, t = t->next) {
        const struct goto_header_type go_to_inner = statset[t->value].go_to;
        int ii;
        for (ii = 1; go_to_inner.map[ii].symbol != symbol; ii++) {
        }
        if (go_to_inner.map[ii].laptr == OMEGA) {
          trace_lalr_path(t->value, ii, cli_options, la_base, first, adequate_item, rules, in_stat, statset, item_table);
        }
      }
      free_nodes(w, p);
    }
  }
  // If the look-ahead follow set involved does not contain EMPTY, we
  // mark the state involved (STATE) so that other look-ahead follow
  // sets which may need this set may reuse the same one.
  // NOTE that if CONTAINS_EMPTY is false, then STATE has to denote a
  // state number (positive value) and not a rule number (negative).
  if (!contains_empty) {
    la_base[state] = go_to.map[goto_indx].laptr;
  }
}

/// COMPUTE_READ computes the number of intermediate look-ahead sets that
/// will be needed (in LA_TOP), allocates space for the sets(LA_SET), and
/// initializes them.
///  By intermediate look-ahead set, we mean the set of terminals that may
/// follow a non-terminal in a given state.
///  These sets are initialized to the set of terminals that can immediately
/// follow the non-terminal in the state to which it can shift (READ set).
void compute_read(struct CLIOptions *cli_options, const struct DetectedSetSizes* dss, bool *single_complete_item, JBitset first, JBitset read_set, struct LAIndex* lai, struct node **adequate_item, struct SRTable* srt, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat) {
  //  We traverse all the states and for all complete items that requires
  // a look-ahead set, we retrace the state digraph (with the help of the
  // routine TRACE_LALR_PATH) and assign a unique number to all look-ahead
  // follow sets that it needs. A look-ahead follow set is a set of
  // terminal symbols associated with a pair [S, A], where S is a state,
  // and A is a non-terminal:
  //
  // [S, A] --> Follow-set
  // Follow-set = {t | t is a terminal that can be shifted on after
  //                      execution of a goto action on A in state S}.
  //
  // Each follow set is initialized with the set of terminals that can be
  // shifted on in state S2, where GOTO(S, A) = S2. After initialization
  // a follow set F that does not contain the special terminal symbol
  // EMPTY is marked with the help of the array LA_BASE, and if the
  // highest level of look-ahead allowed is 1, then only one such set is
  // allocated, and shared for all pairs (S, B) whose follow set is F.
  la_top = 0;
  int *la_base;
  calloc0(la_base, num_states + 1, int);
  for ALL_STATES3(state_no) {
    la_base[state_no] = OMEGA;
  }
  for ALL_STATES3(state_no) {
    for (const struct node *p = cli_options->lalr_level <= 1 && single_complete_item[state_no] ? NULL : statset[state_no].complete_items; p != NULL; p = p->next) {
      int item_no = p->value;
      int rule_no = item_table[item_no].rule_number;
      int lhs_symbol = rules[rule_no].lhs;
      if (lhs_symbol != accept_image) {
        struct node *v = lpgaccess(state_no, item_no, in_stat, item_table);
        struct node *q;
        for (struct node *s = v; s != NULL; q = s, s = s->next) {
          const struct goto_header_type go_to = statset[s->value].go_to;
          int ii;
          for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {
          }
          if (go_to.map[ii].laptr == OMEGA) {
            trace_lalr_path(s->value, ii, cli_options, la_base, first, adequate_item, rules, in_stat, statset, item_table);
          }
        }
        free_nodes(v, q);
      }
    }
    //  If the look-ahead level is greater than 1 or single productions
    // actions are to be removed when possible, then we have to compute
    // a Follow-set for all pairs [S, A] in the state automaton. Therefore,
    // we also have to consider Shift-reduce actions as reductions, and
    // trace back to their roots as well.
    // Note that this is not necessary for Goto-reduce actions. Since
    // they terminate with a non-terminal, and that non-terminal is
    // followed by the empty string, and we know that it must produce a
    // rule that either ends up in a reduction, a shift-reduce, or another
    // goto-reduce. It will therefore be taken care of automatically by
    // transitive closure.
    if (cli_options->lalr_level > 1 || cli_options->single_productions_bit) {
      const struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
      for (int j = 1; j <= sh.size; j++) {
        if (sh.map[j].action < 0) {
          int rule_no = -sh.map[j].action;
          int lhs_symbol = rules[rule_no].lhs;
          int item_no = adequate_item[rule_no]->value - 1;
          struct node *v = lpgaccess(state_no, item_no, in_stat, item_table);
          struct node *q;
          for (struct node *s = v; s != NULL; q = s, s = s->next) {
            const struct goto_header_type go_to = statset[s->value].go_to;
            int ii;
            for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {
            }
            if (go_to.map[ii].laptr == OMEGA) {
              trace_lalr_path(s->value, ii, cli_options, la_base, first, adequate_item, rules, in_stat, statset, item_table);
            }
          }
          free_nodes(v, q);
        }
      }
      // We also need to compute the set of terminal symbols that can be
      // read in a state entered via a terminal transition.
      if (cli_options->lalr_level > 1 && state_no != 1) {
        struct node *q = statset[state_no].kernel_items;
        int item_no = q->value - 1;
        if (IS_A_TERMINAL(item_table[item_no].symbol)) {
          ASSIGN_SET(read_set, state_no, first, item_table[item_no].suffix_index);
          for (q = q->next; q != NULL; q = q->next) {
            item_no = q->value - 1;
            SET_UNION(read_set, state_no, first, item_table[item_no].suffix_index);
          }
        }
      }
    }
  }
  //   We now allocate space for LA_INDEX and LA_SET, and initialize
  // all its elements as indicated in reduce.h. The array LA_BASE is
  // used to keep track of Follow sets that have been initialized. If
  // another set needs to be initialized with a value that has been
  // already computed, LA_BASE is used to retrieve the value.
  for ALL_STATES3(state_no) {
    la_base[state_no] = OMEGA;
  }
  calloc0_set(lai->la_set, la_top + 1, dss->term_set_size);
  for ALL_STATES3(state_no) {
    const struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      const int la_ptr = go_to.map[i].laptr;
      if (la_ptr != OMEGA) /* Follow Look-ahead needed */
      {
        const int state = go_to.map[i].action;
        struct node *q;
        if (state > 0) {
          /* already computed */
          if (la_base[state] != OMEGA) {
            lai->la_index[la_ptr] = lai->la_index[la_base[state]];
            ASSIGN_SET(lai->la_set, la_ptr, lai->la_set, la_base[state]);
            q = NULL;
          } else {
            la_base[state] = la_ptr;
            q = statset[state].kernel_items;
          }
        } else {
          q = adequate_item[-state];
        }
        if (q != NULL) {
          int item_no = q->value - 1;
          // initialize with first item
          ASSIGN_SET(lai->la_set, la_ptr, first, item_table[item_no].suffix_index);
          for (q = q->next; q != NULL; q = q->next) {
            item_no = q->value - 1;
            SET_UNION(lai->la_set, la_ptr, first, item_table[item_no].suffix_index);
          }
          if (IS_IN_SET(lai->la_set, la_ptr, empty)) {
            lai->la_index[la_ptr] = OMEGA;
          } else {
            lai->la_index[la_ptr] = INFINITY;
          }
          if (cli_options->lalr_level > 1 || cli_options->single_productions_bit) {
            if (state > 0) {
              ASSIGN_SET(read_set, state, lai->la_set, la_ptr);
            }
          }
        }
      }
    }
  }
  ffree(la_base);
}

/// COMPUTE_LA takes as argument a state number (STATE_NO), an item number
/// (ITEM_NO), and a set (LOOK_AHEAD).  It computes the look-ahead set of
/// terminals for the given item in the given state and places the answer in
/// the set LOOK_AHEAD.
void compute_la(const int state_no, const int item_no, const JBitset look_ahead, struct StackRoot* sr, JBitset first, struct LAIndex* lai, struct node **adequate_item, struct ruletab_type *rules, struct node **in_stat, struct statset_type *statset, struct itemtab *item_table) {
  sr->stack_root = NULL;
  const int lhs_symbol = rules[item_table[item_no].rule_number].lhs;
  if (lhs_symbol == accept_image) {
    ASSIGN_SET(look_ahead, 0, first, item_table[item_no - 1].suffix_index);
    return;
  }
  INIT_SET(look_ahead); /* initialize set */
  struct node *v = lpgaccess(state_no, item_no, in_stat, item_table);
  struct node *r;
  for (struct node *s = v; s != NULL; r = s, s = s->next) {
    // Search for GOTO action in Access-State after reducing rule to
    // its left hand side(LHS_SYMBOL). Q points to the state.
    const struct goto_header_type go_to = statset[s->value].go_to;
    int ii;
    for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {
    }
    // If look-ahead after left hand side is not yet computed,
    // LA_TRAVERSE the graph to compute it.
    if (lai->la_index[go_to.map[ii].laptr] == OMEGA) {
      int stack_top = 0;
      la_traverse(s->value, ii, &stack_top, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
    }
    SET_UNION(look_ahead, 0, lai->la_set, go_to.map[ii].laptr);
  }
  RESET_BIT(look_ahead, empty); /* empty not valid look-ahead */
  free_nodes(v, r);
}

/// We construct the IN_STAT map which is the inverse of the transition
/// map formed by GOTO and SHIFT maps.
/// This map is implemented as a table of pointers that can be indexed
/// by the states to a circular list of integers representing other
/// states that contain transitions to the state in question.
void build_in_stat(struct SRTable* srt, struct statset_type *statset, struct node **in_stat) {
  for ALL_STATES3(state_no) {
    int n = statset[state_no].shift_number;
    const struct shift_header_type sh = srt->shift[n];
    for (int i = 1; i <= sh.size; ++i) {
      n = sh.map[i].action;
      if (n > 0 && n <= num_states) {
        /* A shift action? */
        struct node *q = Allocate_node();
        q->value = state_no;
        if (in_stat[n] == NULL) {
          q->next = q;
        } else {
          q->next = in_stat[n]->next;
          in_stat[n]->next = q;
        }
        in_stat[n] = q;
      }
    }
    const struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      n = go_to.map[i].action;
      // A goto action
      if (n > 0) {
        struct node *q = Allocate_node();
        q->value = state_no;
        if (in_stat[n] == NULL) {
          q->next = q;
        } else {
          q->next = in_stat[n]->next;
          in_stat[n]->next = q;
        }
        in_stat[n] = q;
      }
    }
  }
}

/// LA_TRAVERSE takes two major arguments: STATE_NO, and an index (GOTO_INDX)
/// that points to the GOTO_ELEMENT array in STATE_NO for the non-terminal
/// left hand side of an item for which look-ahead is to be computed. The
/// look-ahead of an item of the form [x. A y] in state STATE_NO is the set
/// of terminals that can appear immediately after A in the context summarized
/// by STATE_NO. When a look-ahead set is computed, the result is placed in
/// an allocation of LA_ELEMENT pointed to by the LA_PTR field of the
/// GOTO_ELEMENT array.
///
/// The same digraph algorithm used in MKFIRST is used for this computation.
void la_traverse(const int state_no, const int goto_indx, int *stack_top, struct StackRoot* sr, JBitset first, struct LAIndex* lai, struct node **adequate_item, struct node **in_stat, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table) {
  const struct goto_header_type go_to = statset[state_no].go_to;
  const int la_ptr = go_to.map[goto_indx].laptr;
  struct node *s = Allocate_node(); /* Push LA_PTR down the stack */
  s->value = la_ptr;
  s->next = sr->stack_root;
  sr->stack_root = s;
  const int indx = ++*stack_top; /* one element was pushed into the stack */
  lai->la_index[la_ptr] = indx;
  // Compute STATE, action to perform on Goto symbol in question. If
  // STATE is positive, it denotes a state to which to shift. If it is
  // negative, it is a rule on which to perform a Goto-Reduce.
  const int state = go_to.map[goto_indx].action;
  struct node *r;
  /* Not a Goto-Reduce action */
  if (state > 0) {
    r = statset[state].kernel_items;
  } else {
    r = adequate_item[-state];
  }
  for (; r != NULL; r = r->next) {
    // loop over items [A -> x LHS_SYMBOL . y]
    const int item = r->value - 1;
    if (IS_IN_SET(first, item_table[item].suffix_index, empty)) {
      const int symbol = rules[item_table[item].rule_number].lhs;
      struct node *w = lpgaccess(state_no, item, in_stat, item_table); /* states where RULE was  */
      // introduced through closure
      for (struct node *t = w; t != NULL; s = t, t = t->next) {
        // Search for GOTO action in access-state after reducing
        // RULE to its left hand side (SYMBOL). Q points to the
        // GOTO_ELEMENT in question.
        const struct goto_header_type go_to_inner = statset[t->value].go_to;
        int ii;
        for (ii = 1; go_to_inner.map[ii].symbol != symbol; ii++) {
        }
        if (lai->la_index[go_to_inner.map[ii].laptr] == OMEGA) {
          la_traverse(t->value, ii, stack_top, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
        }
        SET_UNION(lai->la_set, la_ptr, lai->la_set, go_to_inner.map[ii].laptr);
        lai->la_index[la_ptr] = MIN(lai->la_index[la_ptr], lai->la_index[go_to_inner.map[ii].laptr]);
      }
      free_nodes(w, s);
    }
  }
  /* Top of a SCC */
  if (lai->la_index[la_ptr] == indx) {
    s = sr->stack_root;
    for (int ii = sr->stack_root->value; ii != la_ptr; sr->stack_root = sr->stack_root->next, ii = sr->stack_root->value) {
      ASSIGN_SET(lai->la_set, ii, lai->la_set, la_ptr);
      lai->la_index[ii] = INFINITY;
      (*stack_top)--; /* one element was popped from the stack; */
    }
    lai->la_index[la_ptr] = INFINITY;
    r = sr->stack_root; /* mark last element that is popped and ... */
    sr->stack_root = sr->stack_root->next; /* ... pop it! */
    (*stack_top)--; /* one element was popped from the stack; */
    free_nodes(s, r);
  }
}

/// Build Reduce map, and detect conflicts if any
/// MKRDCTS constructs the REDUCE map and detects conflicts in the grammar.
/// When constructing an LALR parser, the subroutine COMPUTE_LA is invoked to
/// compute the lalr look-ahead sets. For an SLR parser, the FOLLOW map
/// computed earlier in the procedure MKFIRST is used.
///
/// For a complete description of the lookahead algorithm used in this
/// program, see Charles, PhD thesis, NYU 1991.
struct ConflictCounter mkrdcts(struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct SourcesElementSources* ses, bool *rmpself, JBitset first, struct node **adequate_item, struct SRTable* srt, struct lastats_type *lastats, bool *null_nt, short *gd_index, struct ruletab_type *rules, struct statset_type *statset, struct itemtab *item_table, short *rhs_sym) {
  struct STRS strs = (struct STRS) {
    .highest_level = 0,
    .la_state_root = NULL,
  };
  struct StackRoot sr = (struct StackRoot) {
    .stack_root = NULL,
  };
  // Set up a pool of temporary space. If LALR(k), k > 1 is requested,
  // INIT_LALRK_PROCESS sets up the necessary environment for the
  // computation of multiple lookahead.
  reset_temporary_space();

  /// The boolean variable NOT_LRK is used to mark whether a grammar
  /// is not LR(k) for any k. NOT_LRK is marked true when either:
  ///    1. The grammar contains a nonterminal A such that A =>+rm A
  ///    2. The automaton contains a cycle with each of its edges labeled
  ///       with a nullable nonterminal.
  ///
  /// If LALR(k), k > 1, is requested, we may have to create more shift
  /// maps. Initialize SHIFT_TABLE. Note that each element of SHIFT_TABLE
  /// is automatically initialized to NULL by CALLOC.
  /// (See STATE_TO_RESOLVE_CONFLICTS)
  /// Second, we check whether the grammar is not LR(k) for any k
  /// because there exist a nonterminal A such that
  ///
  ///                     A =>+rm A
  ///
  /// Finally, allocate and compute the CYCLIC vector which identifies
  /// states that can enter a cycle via transitions on nullable
  /// nonterminals. If such a cyle exists, the grammar can also be
  /// claimed to be not LR(k) for any k.
  bool not_lrk = false;
  struct state_element **shift_table;
  bool *cyclic = Allocate_boolean_array(num_states + 1);
  /// NT_ITEMS and ITEM_LIST are used to construct a mapping from each
  /// nonterminal into the set of items of which the nonterminal in
  /// question is the dot symbol. See CONFLICTS_INITIALIZATION.
  short *item_list;
  short *nt_items = NULL;
  struct visited_element visited;
  if (cli_options->lalr_level > 1) {
    calloc0(shift_table, SHIFT_TABLE_SIZE, struct state_element *);
    for ALL_NON_TERMINALS3(symbol) {
      not_lrk = not_lrk || rmpself[symbol];
    }
    short *index_of = Allocate_short_array(num_states + 1);
    short *stack = Allocate_short_array(num_states + 1);
    for ALL_STATES3(state_no) {
      index_of[state_no] = OMEGA;
    }
    struct CyclicTop top = {.top = 0};
    for ALL_STATES3(state_no) {
      if (index_of[state_no] == OMEGA) {
        compute_cyclic(state_no, stack, index_of, cyclic, &top, null_nt, statset);
      }
      not_lrk = not_lrk || cyclic[state_no];
    }
    ffree(stack);
    ffree(index_of);
    ses->sources = allocate_sources();
    calloc0(visited.map, num_states + 1, struct node *);
    visited.list = Allocate_short_array(num_states + 1);
    visited.root = NIL;
  }
  // IN_STAT is a mapping from each state to the set of states that have
  // a transition into the state in question.
  // IN_STAT is used to construct a reverse transition map. See
  // BUILD_IN_STAT for more detail.
  struct node **in_stat = NULL;
  calloc0(in_stat, num_states + 1, struct node *);
  // RULE_COUNT is an array used to count the number of reductions on
  // particular rules within a given state.
  short *rule_count = Allocate_short_array(num_rules + 1);
  // NO_SHIFT_ON_ERROR_SYM is a vector used to identify states that
  // contain shift actions on the %ERROR symbol.  Such states are marked
  // only when DEFAULT_OPT is 5.
  bool *no_shift_on_error_sym = Allocate_boolean_array(num_states + 1);
  // SYMBOL_LIST is used to construct temporary lists of terminals on
  // which reductions are defined.
  short *symbol_list = Allocate_short_array(num_terminals + 1);
  // When default actions are requested, the vector SINGLE_COMPLETE_ITEM
  // is used to identify states that contain exactly one final item.
  // NOTE that when the READ_REDUCE options is turned on, the LR(0)
  // automaton constructed contains no such state.
  bool *single_complete_item = Allocate_boolean_array(num_states + 1);
  struct node **action;
  // ACTION is an array that is used as the base for a mapping from
  // each terminal symbol into a list of actions that can be executed
  // on that symbol in a given state.
  calloc0(action, num_terminals + 1, struct node *);
  // LOOK_AHEAD is used to compute lookahead sets.
  JBitset look_ahead;
  calloc0_set(look_ahead, 1, dss->term_set_size);
  // If we will be removing single productions, we need to keep
  // track of all (state, symbol) pairs on which a conflict is
  // detected. The structure conflict_symbols is used as a base
  // to construct that map. See ADD_CONFLICT_SYMBOL in resolve.c.
  // NOTE that this allocation automatically initialized all
  // elements of the conflict_symbols array to NULL.
  /// CONFLICT_SYMBOLS is a mapping from each state into a set of terminal
  /// symbols on which an LALR(1) conflict was detected in the state in
  /// question.
  struct node **conflict_symbols = NULL;
  if (cli_options->single_productions_bit) {
    calloc0(conflict_symbols, num_states + 1, struct node *);
  }
  // First, construct the IN_STAT map. Next, iterate over the states to
  // construct two boolean vectors.  One indicates whether there is a
  // shift action on the ERROR symbol when the DEFAULT_OPT is 5.  The
  // other indicates whether it is all right to take default action in
  // states containing exactly one final item.
  //
  // We also check whether the grammar is LR(0). I.e., whether it needs
  // any look-ahead at all.
  build_in_stat(srt, statset, in_stat);
  for ALL_STATES3(state_no) {
    no_shift_on_error_sym[state_no] = true;
    if (cli_options->default_opt.value == OPT_5.value) {
      int n = statset[state_no].shift_number;
      const struct shift_header_type sh = srt->shift[n];
      for (int i = 1; i <= sh.size; ++i) {
        if (sh.map[i].symbol == error_image) {
          no_shift_on_error_sym[state_no] = false;
        }
      }
    }
    //   Compute whether this state is a final state.  I.e., a state that
    // contains only a single complete item. If so, mark it as a default
    // state. Note that if the READ-REDUCE option is used, the automaton
    // will not contain such states. Also, states are marked only when
    // default actions are requested.
    struct node *item_ptr = statset[state_no].kernel_items;
    int item_no = item_ptr->value;
    single_complete_item[state_no] = !cli_options->read_reduce_bit && !cli_options->single_productions_bit && cli_options->table_opt.value != OPTIMIZE_TIME.value && cli_options->table_opt.value != OPTIMIZE_SPACE.value && cli_options->default_opt.value > OPT_0.value && item_ptr->next == NULL && item_table[item_no].symbol == empty;
    // If a state has a complete item, and more than one kernel item
    // which is different from the complete item, then this state
    // requires look-ahead for the complete item.
    if (strs.highest_level == 0) {
      const struct node *r = statset[state_no].complete_items;
      if (r != NULL) {
        if (item_ptr->next != NULL || item_ptr->value != r->value) {
          strs.highest_level = 1;
        }
      }
    }
  }
  JBitset read_set = {.raw = NULL};
  if (cli_options->lalr_level > 1 || cli_options->single_productions_bit) {
    calloc0_set(read_set, num_states + 1, dss->term_set_size);
  }
  struct LAIndex lai = (struct LAIndex) {
    .la_index = Allocate_short_array(la_top + 1),
  };
  // We call COMPUTE_READ to perform the following tasks:
  // 1) Count how many elements are needed in LA_ELEMENT: LA_TOP
  // 2) Allocate space for and initialize LA_SET and LA_INDEX
  compute_read(cli_options, dss, single_complete_item, first, read_set, &lai, adequate_item, srt, statset, rules, item_table, in_stat);
  // Allocate space for REDUCE which will be used to map each
  // into its reduce map. We also initialize RULE_COUNT which
  // will be used to count the number of reduce actions on each
  // rule with in a given state.
  calloc0(srt->reduce, num_states + 1, struct reduce_header_type);
  for ALL_RULES3(i) {
    rule_count[i] = 0;
  }
  // We are now ready to construct the reduce map. First, we
  // initialize MAX_LA_STATE to NUM_STATES. If no lookahead
  // state is added (the grammar is LALR(1)) this value will not
  // change. Otherwise, MAX_LA_STATE is incremented by 1 for each
  // lookahead state added.
  max_la_state = num_states;
  // We iterate over the states, compute the lookahead sets,
  // resolve conflicts (if multiple lookahead is requested) and/or
  // report the conflicts if requested...
  struct StackPool sp = (struct StackPool) {
    .stack_pool = NULL,
    .dangling_stacks = NULL,
  };
  struct ConflictPool cp = (struct ConflictPool) {
    .conflict_element_pool = NULL,
  };
  long num_rr_conflicts = 0;
  long num_sr_conflicts = 0;
  for ALL_STATES3(state_no) {
    int default_rule = OMEGA;
    int symbol_root = NIL;
    struct node *item_ptr = statset[state_no].complete_items;
    if (item_ptr != NULL) {
      // Check if it is possible to take default reduction. The DEFAULT_OPT
      // parameter indicates what kind of default options are requested.
      // The various values it can have are:
      //
      //    a)   0 => no default reduction.
      //    b)   1 => default reduction only on adequate states. I.e.,
      //              states with only one complete item in their kernel.
      //    c)   2 => Default on all states that contain exactly one
      //              complete item not derived from an empty rule.
      //    d)   3 => Default on all states that contain exactly one
      //              complete item including items from empty rules.
      //    e)   4 => Default reduction on all states that contain exactly
      //              one item. If a state contains more than one item we
      //              take Default on the item that generated the most
      //              reductions. If there is a tie, one is selected at
      //              random.
      //    f)   5 => Same as 4 except that no default actions are computed
      //              for states that contain a shift action on the ERROR
      //              symbol.
      //
      //  In the code below, we first check for category 3.  If it is not
      // satisfied, then we check for the others. Note that in any case,
      // default reductions are never taken on the ACCEPT rule.
      int item_no = item_ptr->value;
      int rule_no = item_table[item_no].rule_number;
      int symbol = rules[rule_no].lhs;
      if (single_complete_item[state_no] && symbol != accept_image) {
        default_rule = rule_no;
        item_ptr = NULL; /* No need to check for conflicts */
      }
      // Iterate over all complete items in the state, build action
      // map, and check for conflicts.
      for (; item_ptr != NULL; item_ptr = item_ptr->next) {
        // for all complete items
        item_no = item_ptr->value;
        rule_no = item_table[item_no].rule_number;
        compute_la(state_no, item_no, look_ahead, &sr, first, &lai, adequate_item, rules, in_stat, statset, item_table);
        for ALL_TERMINALS3(symbol) {
          // for all symbols in la set
          if (IS_ELEMENT(look_ahead, symbol)) {
            struct node *p = Allocate_node();
            p->value = item_no;
            if (action[symbol] == NULL) {
              symbol_list[symbol] = symbol_root;
              symbol_root = symbol;
            } else {
              // Always place the rule with the largest
              // right-hand side first in the list.
              int n = item_table[action[symbol]->value].rule_number;
              if (RHS_SIZE(n, rules) >= RHS_SIZE(rule_no, rules)) {
                p->value = action[symbol]->value;
                action[symbol]->value = item_no;
              }
            }
            p->next = action[symbol];
            action[symbol] = p;
          }
        }
      }
      // At this stage, we have constructed the ACTION map for STATE_NO.
      // ACTION is a map from each symbol into a set of final items.
      // The rules associated with these items are the rules by which
      // to reduce when the lookahead is the symbol in question.
      // SYMBOL_LIST/SYMBOL_ROOT is a list of the non-empty elements of
      // ACTION. If the number of elements in a set ACTION(t), for some
      // terminal t, is greater than one or it is not empty and STATE_NO
      // contains a shift action on t then STATE_NO has one or more
      // conflict(s). The procedure RESOLVE_CONFLICTS takes care of
      // resolving the conflicts appropriately and returns an ACTION
      // map where each element has either 0 (if the conflicts were
      // shift-reduce conflicts, the shift is given precedence) or 1
      // element (if the conflicts were reduce-reduce conflicts, only
      // the first element in the ACTION(t) list is returned).
      if (symbol_root != NIL) {
        struct ConflictCounter cc_ = resolve_conflicts(state_no, action, symbol_list, symbol_root, cli_options, dss, shift_table, cyclic, item_list, nt_items, &sp, &cp, &visited, ses, &strs, &sr, rmpself, first, read_set, &lai, conflict_symbols, adequate_item, srt, lastats, null_nt, in_stat, rules, item_table, statset, rhs_sym);
        num_rr_conflicts += cc_.num_rr_conflicts;
        num_sr_conflicts += cc_.num_sr_conflicts;
        for (symbol = symbol_root; symbol != NIL; symbol = symbol_list[symbol]) {
          if (action[symbol] != NULL) {
            item_no = action[symbol]->value;
            rule_count[item_table[item_no].rule_number]++;
          }
        }
      }
    }
    // We are now ready to compute the size of the reduce map for
    // STATE_NO (reduce_size) and the default rule.
    // If the state being processed contains only a single complete item
    // then the DEFAULT_RULE was previously computed and the list of
    // symbols is empty.
    // NOTE: a REDUCE_ELEMENT will be allocated for all states, even
    // those that have no reductions at all. This will facilitate the
    // Table Compression routines, for they can assume that such an
    // object exists, and can be used for Default values.
    int reduce_size = 0;
    if (symbol_root != NIL) {
      // Compute REDUCE_SIZE, the number of reductions in the state and
      // DEFAULT_RULE: the rule with the highest number of reductions
      // to it.
      int n = 0;
      for (struct node *q = statset[state_no].complete_items; q != NULL; q = q->next) {
        const int item_no = q->value;
        const int rule_no = item_table[item_no].rule_number;
        const int symbol = rules[rule_no].lhs;
        reduce_size += rule_count[rule_no];
        if (rule_count[rule_no] > n &&
            no_shift_on_error_sym[state_no] &&
            symbol != accept_image) {
          n = rule_count[rule_no];
          default_rule = rule_no;
        }
      }
      //   If the removal of single productions is requested
      // and/or parsing tables will not be output, figure out
      // if the level of the default option requested permits
      // default actions, and compute how many reduce actions
      // can be eliminated as a result.
      if (cli_options->default_opt.value == OPT_0.value) {
        default_rule = OMEGA;
      } else if (cli_options->table_opt.value != OPTIMIZE_TIME.value && cli_options->table_opt.value != OPTIMIZE_SPACE.value && !cli_options->single_productions_bit) {
        struct node *q = statset[state_no].complete_items;
        if (q->next == NULL) {
          const int item_no = q->value;
          const int rule_no = item_table[item_no].rule_number;
          if (cli_options->default_opt.value > OPT_2.value || /* No empty rule defined */
              (cli_options->default_opt.value == OPT_2.value && RHS_SIZE(rule_no, rules) != 0)) {
            reduce_size -= n;
          } else {
            default_rule = OMEGA;
          }
        } else if (cli_options->default_opt.value > OPT_3.value)
          reduce_size -= n;
      }
      num_reductions += reduce_size;
    }
    //   NOTE that the default fields are set for all states,
    // whether DEFAULT actions are requested. This is
    // all right since one can always check whether (DEFAULT > 0)
    // before using these fields.
    const struct reduce_header_type red = Allocate_reduce_map(reduce_size);
    srt->reduce[state_no] = red;
    red.map[0].symbol = DEFAULT_SYMBOL;
    red.map[0].rule_number = default_rule;
    for (int symbol = symbol_root; symbol != NIL; symbol = symbol_list[symbol]) {
      if (action[symbol] != NULL) {
        const int rule_no = item_table[action[symbol]->value].rule_number;
        if (rule_no != default_rule ||
            cli_options->table_opt.value == OPTIMIZE_SPACE.value ||
            cli_options->table_opt.value == OPTIMIZE_TIME.value ||
            cli_options->single_productions_bit) {
          red.map[reduce_size].symbol = symbol;
          red.map[reduce_size].rule_number = rule_no;
          reduce_size--;
        }
        free_nodes(action[symbol], action[symbol]);
        action[symbol] = NULL;
      }
    }
    // Reset RULE_COUNT elements used in this state.
    for (struct node *q = statset[state_no].complete_items; q != NULL; q = q->next) {
      const int rule_no = item_table[q->value].rule_number;
      rule_count[rule_no] = 0;
    }
  }
  printf("\n");
  // If the automaton required multiple lookahead, construct the
  // permanent lookahead states.
  if (max_la_state > num_states) {
    create_lastats(&strs, srt, statset);
  }
  // We are now finished with the LALR(k) construction of the
  // automaton. Clear all temporary space that was used in that
  // process and calculate the maximum lookahead level that was
  // needed.
  exit_lalrk_process(cli_options, shift_table, cyclic, &sp, &visited, ses, rmpself);
  free_conflict_space(item_list, nt_items);
  cli_options->lalr_level = strs.highest_level;
  // If the removal of single productions is requested, do that.
  if (cli_options->single_productions_bit) {
    remove_single_productions(dss, &sr, first, &lai, conflict_symbols, lai.la_set, adequate_item, srt, statset, lastats, gd_index, in_stat, rules, item_table, rhs_sym);
  }
  // If either more than one lookahead was needed or the removal
  // of single productions was requested, the automaton was
  // transformed with the addition of new states and new
  // transitions. In such a case, we reconstruct the IN_STAT map.
  if (cli_options->lalr_level > 1 || cli_options->single_productions_bit) {
    for ALL_STATES3(state_no) {
      // First, clear out the previous map
      if (in_stat[state_no] != NULL) {
        struct node *q = in_stat[state_no]->next; /* point to root */
        free_nodes(q, in_stat[state_no]);
        in_stat[state_no] = NULL;
      }
    }
    build_in_stat(srt, statset, in_stat); /* rebuild in_stat map */
  }
  // Print informational messages and free all temporary space that
  // was used to compute lookahead information.
  if (not_lrk) {
    printf("This grammar is not LR(K).\n\n");
  } else {
    if (num_rr_conflicts > 0 || num_sr_conflicts > 0) {
      if (strs.highest_level != INFINITY) {
        printf("This grammar is not LALR(%d).\n\n", strs.highest_level);
      } else {
        printf("This grammar is not LALR(K).\n\n");
      }
    } else {
      if (strs.highest_level == 0) {
        printf("This grammar is LR(0).\n\n");
      } else {
        printf("This grammar is LALR(%d).\n\n", strs.highest_level);
      }
    }
  }
  ffree(rule_count);
  ffree(no_shift_on_error_sym);
  ffree(symbol_list);
  ffree(single_complete_item);
  ffree(action);
  ffree(look_ahead.raw);
  if (conflict_symbols != NULL) {
    ffree(conflict_symbols);
  }
  if (read_set.raw != NULL) {
    ffree(read_set.raw);
  }
  if (lai.la_index != NULL) {
    ffree(lai.la_index);
  }
  if (lai.la_set.raw != NULL) {
    ffree(lai.la_set.raw);
  }
  return (struct ConflictCounter) {
    .num_rr_conflicts = num_rr_conflicts,
    .num_sr_conflicts = num_sr_conflicts,
    .in_stat = in_stat,
  };
}
