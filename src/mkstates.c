#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "lpgparse.h"

/// STATE_ELEMENT is used to represent states. Each state is mapped into a
/// unique number. The components QUEUE and LINK are auxiliary:
struct state_element {
  // LINK is used to resolve collisions in hashing the states.
  struct state_element *link;
  // QUEUE is used to form a sequential linked-list of the states ordered
  struct state_element *queue;
  // NEXT_SHIFT is used to resolve collisions in hashing SHIFT maps.
  struct state_element *next_shift;
  struct node *kernel_items;
  struct node *complete_items;
  struct shift_header_type lr0_shift;
  struct goto_header_type lr0_goto;
  short shift_number;
  // STATE_NUMBER and identified by the variable STATE_ROOT.
  short state_number;
};

struct StateContainer {
  struct state_element *state_root;
  struct state_element *state_tail;
};

/// LR0_STATE_MAP takes as an argument a pointer to a kernel set of items. If
/// no state based on that kernel set already exists, then a new one is
/// created and added to STATE_TABLE. In any case, a pointer to the STATE of
/// the KERNEL is returned.
struct state_element *lr0_state_map(struct node *kernel, struct state_element **state_table, struct StateContainer* sc) {
  unsigned long hash_address = 0;
  // Compute the hash address.
  for (const struct node *p = kernel; p != NULL; p = p->next) {
    hash_address += p->value;
  }
  hash_address %= STATE_TABLE_SIZE;
  // Check whether a state is already defined by the KERNEL set.
  for (struct state_element *state_ptr = state_table[hash_address];state_ptr != NULL; state_ptr = state_ptr->link) {
    struct node *q;
    struct node *r;
    struct node *p;
    for (p = state_ptr->kernel_items, q = kernel; p != NULL && q != NULL; p = p->next, r = q, q = q->next) {
      if (p->value != q->value)
        break;
    }
    // Both P and Q are NULL?
    if (p == q) {
      free_nodes(kernel, r);
      return state_ptr;
    }
  }
  // Add a new state based on the KERNEL set.
  num_states++;
  struct state_element *ptr;
  talloc0p(&ptr, struct state_element);
  ptr->queue = NULL;
  ptr->kernel_items = kernel;
  ptr->complete_items = NULL;
  ptr->state_number = num_states;
  ptr->link = state_table[hash_address];
  state_table[hash_address] = ptr;
  if (sc->state_root == NULL) {
    sc->state_root = ptr;
  } else {
    sc->state_tail->queue = ptr;
  }
  sc->state_tail = ptr;
  return ptr;
}

/// This procedure constructs an LR(0) automaton.
void mklr0(struct CLIOptions *cli_options, struct shift_header_type* no_shifts_ptr, struct goto_header_type* no_gotos_ptr, struct node **clitems, struct node **closure, struct SRTable* srt, struct ruletab_type *rules, struct itemtab *item_table) {
  // STATE_TABLE is the array used to hash the states. States are
  // identified by their Kernel set of items. Hash locations are
  // computed for the states. As states are inserted in the table,
  // they are threaded together via the QUEUE component of
  // STATE_ELEMENT. The variable STATE_ROOT points to the root of
  // the thread, and the variable STATE_TAIL points to the tail.
  //
  //   After constructing a state, Shift and Goto actions are
  // defined on that state based on a partition of the set of items
  // in that state. The partitioning is based on the symbols
  // following the dot in the items. The array PARTITION is used
  // for that partitioning. LIST and ROOT are used to construct
  // temporary lists of symbols in a state on which Shift or Goto
  // actions are defined.
  // NT_LIST and NT_ROOT are used to build temporary lists of non-terminals.
  // Set up a pool of temporary space.
  reset_temporary_space();
  short *list = Allocate_short_array(num_symbols + 1);
  short *shift_action = Allocate_short_array(num_terminals + 1);
  short *shift_list = Allocate_short_array(num_terminals + 1);
  short *nt_list = Allocate_short_array(num_non_terminals);
  nt_list -= num_terminals + 1;
  struct node **partition;
  calloc0(partition, num_symbols + 1, struct node *);
  struct state_element **state_table;
  calloc0(state_table, STATE_TABLE_SIZE, struct state_element *);
  struct state_element **shift_table;
  calloc0(shift_table, SHIFT_TABLE_SIZE, struct state_element *);
  // INITIALIZATION -----------------------------------------------------------
  int goto_size = 0;
  int shift_size = 0;
  struct StateContainer sc = (struct StateContainer) {
    .state_root = NULL,
    .state_tail = NULL,
  };
  for (int i = 0; i <= num_terminals; i++) {
    shift_action[i] = OMEGA;
  }
  int nt_root = NIL;
  for ALL_NON_TERMINALS3(i) {
    nt_list[i] = OMEGA;
  }
  // PARTITION, STATE_TABLE and SHIFT_TABLE are initialized by calloc
  //
  // END OF INITIALIZATION ----------------------------------------------------
  //
  // Kernel of the first state consists of the first items in each
  // rule produced by Accept non-terminal.
  struct node *q = NULL;
  struct node *p;
  for (bool end_node = (p = clitems[accept_image]) == NULL; !end_node; /* Loop over circular list */ end_node = p == clitems[accept_image]) {
    p = p->next;
    struct node *new_item = Allocate_node();
    new_item->value = p->value;
    new_item->next = q;
    q = new_item;
  }
  // Insert first state in STATE_TABLE and keep constructing states
  // until we no longer can.
  for (struct state_element *state = lr0_state_map(q, state_table, &sc); /* insert initial state */ state != NULL; /* and process next state until no more */ state = state->queue) {
    // Now we construct a list of all non-terminals that can be
    // introduced in this state through closure.  The CLOSURE of each
    // non-terminal has been previously computed in MKFIRST.
    for (q = state->kernel_items; q != NULL; /* iterate over kernel set of items */ q = q->next) {
      int item_no = q->value;
      int symbol = item_table[item_no].symbol; /* symbol after dot */
      if (IS_A_NON_TERMINAL(symbol)) /* Dot symbol */
      {
        if (nt_list[symbol] == OMEGA) /* not yet seen */
        {
          nt_list[symbol] = nt_root;
          nt_root = symbol;
          for (bool end_node = (p = closure[symbol]) == NULL; !end_node; /* Loop over circular list */ end_node = p == closure[symbol]) {
            // add its closure to list
            p = p->next;
            if (nt_list[p->value] == OMEGA) {
              nt_list[p->value] = nt_root;
              nt_root = p->value;
            }
          }
        }
      }
    }
    // We now construct lists of all start items that the closure
    // non-terminals produce. A map from each non-terminal to its set
    // start items has previously been computed in MKFIRST. (CLITEMS)
    // Empty items are placed directly in the state, whereas non_empty
    // items are placed in a temporary list rooted at CLOSURE_ROOT.
    struct node *closure_root = NULL; /* used to construct list of closure items */
    struct node *closure_tail;
    for (int symbol = nt_root; symbol != NIL; nt_list[symbol] = OMEGA, symbol = nt_root) {
      nt_root = nt_list[symbol];
      for (bool end_node = (p = clitems[symbol]) == NULL; !end_node; /* Loop over circular list */ end_node = p == clitems[symbol]) {
        p = p->next;
        int item_no = p->value;
        struct node *new_item = Allocate_node();
        new_item->value = item_no;
        if (item_table[item_no].symbol == empty) /* complete item */
        {
          // Add to COMPLETE_ITEMS set in state
          new_item->next = state->complete_items;
          state->complete_items = new_item;
        } else {
          // closure item, add to closure list
          if (closure_root == NULL) {
            closure_root = new_item;
          } else {
            closure_tail->next = new_item;
          }
          closure_tail = new_item;
        }
      }
    }
    struct node *item_ptr;
    /* any non-complete closure items? */
    if (closure_root != NULL) {
      // construct list of them and kernel items
      closure_tail->next = state->kernel_items;
      item_ptr = closure_root;
    } else {
      /* else just consider kernel items */
      item_ptr = state->kernel_items;
    }
    // In this loop, the PARTITION map is constructed. At this point,
    // ITEM_PTR points to all the non_complete items in the closure of
    // the state, plus all the kernel items.  We note that the kernel
    // items may still contain complete-items, and if any is found, the
    // COMPLETE_ITEMS list is updated.
    int root = NIL;
    for (; item_ptr != NULL; item_ptr = item_ptr->next) {
      int item_no = item_ptr->value;
      int symbol = item_table[item_no].symbol;
      /* incomplete item */
      if (symbol != empty) {
        int next_item_no = item_no + 1;
        if (partition[symbol] == NULL) {
          // PARTITION not defined on symbol
          list[symbol] = root; /* add to list */
          root = symbol;
          if (IS_A_TERMINAL(symbol)) /* Update transition count */
          {
            shift_size++;
          } else {
            goto_size++;
          }
        }
        struct node *tail;
        for (p = partition[symbol]; p != NULL; tail = p, p = p->next) {
          if (p->value > next_item_no) {
            break;
          }
        }
        struct node *r = Allocate_node();
        r->value = next_item_no;
        r->next = p;
        if (p == partition[symbol]) /* Insert at beginning */
        {
          partition[symbol] = r;
        } else {
          tail->next = r;
        }
      } else {
        /* Update complete item set with item from kernel */
        p = Allocate_node();
        p->value = item_no;
        p->next = state->complete_items;
        state->complete_items = p;
      }
    }
    if (closure_root != NULL) {
      free_nodes(closure_root, closure_tail);
    }
    // We now iterate over the set of partitions and update the state
    // automaton and the transition maps: SHIFT and GOTO. Each
    // partition represents the kernel of a state.
    struct goto_header_type go_to;
    if (goto_size > 0) {
      go_to = Allocate_goto_map(goto_size);
      state->lr0_goto = go_to;
    } else {
      state->lr0_goto = *no_gotos_ptr;
    }
    int shift_root = NIL;
    for (int symbol = root; symbol != NIL; /* symbols on which transition is defined */ symbol = list[symbol]) {
      short action = OMEGA;
      // If the partition contains only one item, and it is adequate
      // (i.e. the dot immediately follows the last symbol), and
      // READ-REDUCE is requested, a new state is not created, and the
      // action is marked as a Shift-reduce or a Goto-reduce. Otherwise
      // if a state with that kernel set does not yet exist, we create
      // it.
      q = partition[symbol]; /* kernel of a new state */
      if (cli_options->read_reduce_bit && q->next == NULL) {
        int item_no = q->value;
        if (item_table[item_no].symbol == empty) {
          int rule_no = item_table[item_no].rule_number;
          if (rules[rule_no].lhs != accept_image) {
            action = -rule_no;
            free_nodes(q, q);
          }
        }
      }
      /* Not a Read-Reduce action */
      if (action == OMEGA) {
        struct state_element *new_state = lr0_state_map(q, state_table, &sc);
        action = new_state->state_number;
      }
      // At this stage, the partition list has been freed (for an old
      // state or an ADEQUATE item), or used (for a new state).  The
      // PARTITION field involved should be reset.
      partition[symbol] = NULL; /* To be reused */
      // At this point, ACTION contains the value of the state to Shift
      // to, or rule to Read-Reduce on. If the symbol involved is a
      // terminal, we update the Shift map; else, it is a non-terminal
      // and we update the Goto map.
      // Shift maps are constructed temporarily in SHIFT_ACTION.
      // Later, they are inserted into a map of unique Shift maps, and
      // shared by states that contain identical shifts.
      // Since the lookahead set computation is based on the GOTO maps,
      // all these maps and their element maps should be kept as
      // separate entities.
      if (IS_A_TERMINAL(symbol)) {
        /* terminal? add to SHIFT map */
        shift_action[symbol] = action;
        shift_list[symbol] = shift_root;
        shift_root = symbol;
        if (action > 0) {
          num_shifts++;
        } else {
          num_shift_reduces++;
        }
      }
      // NOTE that for Goto's we update the field LA_PTR of GOTO. This
      // field will be used later in the routine MKRDCTS to point to a
      // look-ahead set.
      else {
        go_to.map[goto_size].symbol = symbol; /* symbol field */
        go_to.map[goto_size].action = action; /* state field  */
        go_to.map[goto_size].laptr = OMEGA; /* la_ptr field */
        goto_size--;
        if (action > 0) {
          num_gotos++;
        } else {
          num_goto_reduces++;
        }
      }
    }
    // We are now going to update the set of Shift-maps. Ths idea is
    // to do a look-up in a hash table based on SHIFT_TABLE to see if
    // the Shift map associated with the current state has already been
    // computed. If it has, we simply update the SHIFT_NUMBER and the
    // SHIFT field of the current state. Otherwise, we allocate and
    // construct a SHIFT_ELEMENT map, update the current state, and
    // add it to the set of Shift maps in the hash table.
    //   Note that the SHIFT_NUMBER field in the STATE_ELEMENTs could
    // have been factored out and associated instead with the
    // SHIFT_ELEMENTs. That would have saved some space, but only in
    // the short run. This field was purposely stored in the
    // STATE_ELEMENTs, because once the states have been constructed,
    // they are not kept, whereas the SHIFT_ELEMENTs are kept.
    //    One could have also threaded through the states that contain
    // original shift maps to avoid duplicate assignments in
    // creating the SHIFT map later. However, this would have
    // increased the storage requirement, and would probably have saved
    // (at most) a totally insignificant amount of time.
  update_shift_maps: {
      unsigned long hash_address;
      hash_address = shift_size;
      for (int symbol = shift_root; symbol != NIL; symbol = shift_list[symbol]) {
        hash_address += ABS(shift_action[symbol]);
      }
      struct shift_header_type sh;
      struct state_element *p_inner;
      hash_address %= SHIFT_TABLE_SIZE;
      for (p_inner = shift_table[hash_address];
           p_inner != NULL; /* Search has table for shift map */
           p_inner = p_inner->next_shift) {
        sh = p_inner->lr0_shift;
        if (sh.size == shift_size) {
          int ii;
          // Compare shift maps
          for (ii = 1; ii <= shift_size; ii++) {
            if (sh.map[ii].action != shift_action[sh.map[ii].symbol]) {
              break;
            }
          }
          // Are they equal ?
          if (ii > shift_size) {
            state->lr0_shift = sh;
            state->shift_number = p_inner->shift_number;
            for (int symbol = shift_root; symbol != NIL; symbol = shift_list[symbol]) {
              // Clear SHIFT_ACTION
              shift_action[symbol] = OMEGA;
            }
            shift_size = 0; /* Reset for next round */
            goto leave_update_shift_maps;
          }
        }
      }
      if (shift_size > 0) {
        sh = Allocate_shift_map(shift_size);
        num_shift_maps++;
        state->shift_number = num_shift_maps;
      } else {
        state->shift_number = 0;
        sh = *no_shifts_ptr;
      }
      state->lr0_shift = sh;
      state->next_shift = shift_table[hash_address];
      shift_table[hash_address] = state;
      for (int symbol = shift_root; symbol != NIL; symbol = shift_list[symbol]) {
        sh.map[shift_size].symbol = symbol;
        sh.map[shift_size].action = shift_action[symbol];
        shift_action[symbol] = OMEGA;
        shift_size--;
      }
    } /*end update_shift_maps */
  leave_update_shift_maps:;
  }
  // Construct STATSET, a "compact" and final representation of
  // State table, and SHIFT which is a set of all shift maps needed.
  // NOTE that assignments to elements of SHIFT may occur more than
  // once, but that's ok. It is probably faster to do that than to
  // set up an elaborate scheme to avoid the multiple assignment which
  // may in fact cost more. Look at it this way: it is only a pointer
  // assignment, namely a Load and a Store.
  // Release all NODEs used by the maps CLITEMS and CLOSURE.
  {
    int state_no;
    struct state_element *p_inner;
    // If the grammar is LALR(k), k > 1, more states may be added and
    // the size of the shift map increased.
    calloc0(srt->shift, num_states + 1, struct shift_header_type);
    srt->shift[0] = *no_shifts_ptr; /* MUST be initialized for LALR(k) */
    calloc0(statset, num_states + 1, struct statset_type);
    for (p_inner = sc.state_root; p_inner != NULL; p_inner = p_inner->queue) {
      state_no = p_inner->state_number;
      statset[state_no].kernel_items = p_inner->kernel_items;
      statset[state_no].complete_items = p_inner->complete_items;
      srt->shift[p_inner->shift_number] = p_inner->lr0_shift;
      statset[state_no].shift_number = p_inner->shift_number;
      statset[state_no].go_to = p_inner->lr0_goto;
    }
  }
  ffree(list);
  ffree(shift_action);
  ffree(shift_list);
  nt_list += num_terminals + 1;
  ffree(nt_list);
  ffree(partition);
  ffree(state_table);
  ffree(shift_table);
}



// === Produce Start ===

struct scope_elmt {
  short link;
  short item;
  short index;
};

struct ScopeTop {
  long top;
};

struct Produced {
  JBitset produces;
  JBitset right_produces;
  JBitset left_produces;
};

///                             SCOPE_CHECK:
/// Given a nonterminal LHS_SYMBOL and a nonterminal TARGET where,
///
///                     LHS_SYMBOL ::= TARGET x
///
/// find out if whenever LHS_SYMBOL is introduced through closure, it
/// is introduced by a nonterminal SOURCE such that
///
///                     SOURCE ->rm* LHS_SYMBOL
///
///                               and
///
///                     SOURCE ->rm+ TARGET
bool scope_check(const int lhs_symbol, const int target, const int source, bool* symbol_seen, struct Produced* produced, short *item_of, short *next_item, struct ruletab_type *rules, struct itemtab *item_table) {
  symbol_seen[source] = true;
  if (IS_IN_SET(produced->right_produces, target, source - num_terminals) &&
      IS_IN_SET(produced->right_produces, lhs_symbol, source - num_terminals)) {
    return false;
  }
  for (int item_no = item_of[source];
       item_no != NIL; item_no = next_item[item_no]) {
    if (item_table[item_no].dot != 0) {
      return true;
    }
    const int rule_no = item_table[item_no].rule_number;
    const int symbol = rules[rule_no].lhs;
    if (!symbol_seen[symbol]) {
      // not yet processed
      if (scope_check(lhs_symbol, target, symbol, symbol_seen, produced, item_of, next_item, rules, item_table)) {
        return 1;
      }
    }
  }
  return false;
}

/// This procedure checks whether an item of the form:
/// [A  ->  w B x  where  B ->* y A z  is a valid scope.
///
/// Such an item is a valid scope if the following conditions hold:
///
/// 1) it is not the case that x =>* %empty
/// 2) either it is not the case that w =>* %empty, or it is not the
///    case that B =>lm* A.
/// 3) it is not the case that whenever A is introduced through
///    closure, it is introduced by a nonterminal C where C =>rm* A
///    and C =>rm+ B.
bool is_scope(const int item_no, bool *symbol_seen, struct Produced* produced, short *item_of, short *next_item, bool *null_nt, struct ruletab_type *rules, struct itemtab *item_table) {
  for (int i = item_no - item_table[item_no].dot; i < item_no; i++) {
    const int symbol = item_table[i].symbol;
    if (IS_A_TERMINAL(symbol)) {
      return true;
    }
    if (!null_nt[symbol]) {
      return true;
    }
  }
  const int lhs_symbol = rules[item_table[item_no].rule_number].lhs;
  const int target = item_table[item_no].symbol;
  if (IS_IN_SET(produced->left_produces, target, lhs_symbol - num_terminals)) {
    return false;
  }
  if (item_table[item_no].dot > 0) {
    return true;
  }
  for ALL_NON_TERMINALS3(nt) {
    symbol_seen[nt] = false;
  }
  return scope_check(lhs_symbol, target, lhs_symbol, symbol_seen, produced, item_of, next_item, rules, item_table);
}

/// This boolean function takes two items as arguments and checks
/// whether they have the same prefix.
bool is_prefix_equal(const int item_no, const int item_no2, struct ruletab_type *rules, struct itemtab *item_table, short *rhs_sym) {
  // a suffix
  if (item_no > 0) {
    return false;
  }
  const int item_no1 = -item_no;
  if (item_table[item_no1].dot != item_table[item_no2].dot) {
    return false;
  }
  int j = rules[item_table[item_no1].rule_number].rhs;
  const int start = rules[item_table[item_no2].rule_number].rhs;
  const int dot = start + item_table[item_no2].dot - 1;
  for (int i = start; i <= dot; i++) {
    // symbols before dot
    if (rhs_sym[i] != rhs_sym[j]) {
      return false;
    }
    j++;
  }
  return true;
}

/// This procedure takes as argument an item and inserts the string
/// prefix of the item preceeding the "dot" into the scope table, if
/// that string is not already there.  In any case, the index  number
/// associated with the prefix in question is returned.
/// NOTE that since both prefixes and suffixes are entered in the
/// table, the prefix of a given item, ITEM_NO, is encoded as
/// -ITEM_NO, whereas the suffix of that item is encoded as +ITEM_NO.
int insert_prefix(const int item_no, struct scope_elmt *scope_element, short *scope_table, struct ScopeTop* st, struct ruletab_type *rules, struct itemtab *item_table, short *rhs_sym) {
  unsigned long hash_address = 0;
  const int rule_no = item_table[item_no].rule_number;
  int ii;
  for (ii = rules[rule_no].rhs; /* symbols before dot */
       ii < rules[rule_no].rhs + item_table[item_no].dot; ii++)
    hash_address += rhs_sym[ii];
  ii = hash_address % SCOPE_SIZE;
  for (int j = scope_table[ii]; j != NIL; j = scope_element[j].link) {
    if (is_prefix_equal(scope_element[j].item, item_no, rules, item_table, rhs_sym)) {
      return scope_element[j].index;
    }
  }
  st->top++;
  scope_element[st->top].item = -item_no;
  scope_element[st->top].index = scope_rhs_size + 1;
  scope_element[st->top].link = scope_table[ii];
  scope_table[ii] = st->top;
  scope_rhs_size += item_table[item_no].dot + 1;
  return scope_element[st->top].index;
}

/// This boolean function takes two items as arguments and checks
/// whether they have the same suffix.
bool is_suffix_equal(const int item_no1, const int item_no2, bool *null_nt, struct ruletab_type *rules, struct itemtab *item_table, short *rhs_sym) {
  if (item_no1 < 0) {
    // a prefix
    return false;
  }
  int rule_no = item_table[item_no1].rule_number;
  int i = rules[rule_no].rhs + item_table[item_no1].dot;
  const int dot1 = rules[rule_no + 1].rhs - 1;
  rule_no = item_table[item_no2].rule_number;
  int j = rules[rule_no].rhs + item_table[item_no2].dot;
  const int dot2 = rules[rule_no + 1].rhs - 1;
  while (i <= dot1 && j <= dot2) {
    // non-nullable syms before dot
    if (IS_A_NON_TERMINAL(rhs_sym[i])) {
      if (null_nt[rhs_sym[i]]) {
        i++;
        continue;
      }
    } else if (rhs_sym[i] == error_image) {
      i++;
      continue;
    }
    if (IS_A_NON_TERMINAL(rhs_sym[j])) {
      if (null_nt[rhs_sym[j]]) {
        j++;
        continue;
      }
    } else if (rhs_sym[j] == error_image) {
      j++;
      continue;
    }
    if (rhs_sym[i] != rhs_sym[j]) {
      return false;
    }
    j++;
    i++;
  }
  for (; i <= dot1; i++) {
    if (IS_A_NON_TERMINAL(rhs_sym[i])) {
      if (!null_nt[rhs_sym[i]]) {
        return false;
      }
    } else if (rhs_sym[i] != error_image) {
      return false;
    }
  }
  for (; j <= dot2; j++) {
    if (IS_A_NON_TERMINAL(rhs_sym[j])) {
      if (!null_nt[rhs_sym[j]]) {
        return false;
      }
    } else if (rhs_sym[j] != error_image) {
      return false;
    }
  }
  return true;
}

/// This procedure is analoguous to INSERT_PREFIX.  It takes as
/// argument an item, and inserts the suffix string following the dot
/// in the item into the scope table, if it is not already there.
/// In any case, it returns the index associated with the suffix.
/// When inserting a suffix into the table, all nullable nonterminals
/// in the suffix are disregarded.
int insert_suffix(const int item_no, struct scope_elmt *scope_element, short *scope_table, struct ScopeTop* st, bool *null_nt, struct ruletab_type *rules, struct itemtab *item_table, short *rhs_sym) {
  int num_elements = 0;
  unsigned long hash_address = 0;
  const int rule_no = item_table[item_no].rule_number;
  int ii;
  for (ii = rules[rule_no].rhs + item_table[item_no].dot;
       ii < rules[rule_no + 1].rhs; /* symbols after dot */
       ii++) {
    if (IS_A_NON_TERMINAL(rhs_sym[ii])) {
      if (!null_nt[rhs_sym[ii]]) {
        hash_address += rhs_sym[ii];
        num_elements++;
      }
    } else if (rhs_sym[ii] != error_image) {
      hash_address += rhs_sym[ii];
      num_elements++;
    }
  }
  ii = hash_address % SCOPE_SIZE;
  for (int j = scope_table[ii]; j != NIL; j = scope_element[j].link) {
    if (is_suffix_equal(scope_element[j].item, item_no, null_nt, rules, item_table, rhs_sym)) {
      return scope_element[j].index;
    }
  }
  st->top++;
  scope_element[st->top].item = item_no;
  scope_element[st->top].index = scope_rhs_size + 1;
  scope_element[st->top].link = scope_table[ii];
  scope_table[ii] = st->top;
  scope_rhs_size += num_elements + 1;
  return scope_element[st->top].index;
}

/// This procedure takes as parameter a nonterminal, LHS_SYMBOL, and
/// determines whether there is a terminal symbol t such that
/// LHS_SYMBOL can rightmost produce a string tX.  If so, t is
/// returned, otherwise EMPTY is returned.
int get_shift_symbol(const int lhs_symbol, bool *symbol_seen, struct node **clitems, struct ruletab_type *rules, struct itemtab *item_table, short *rhs_sym) {
  if (!symbol_seen[lhs_symbol]) {
    struct node *p;
    symbol_seen[lhs_symbol] = true;
    for (bool end_node = (p = clitems[lhs_symbol]) == NULL; !end_node; end_node = p == clitems[lhs_symbol]) {
      p = p->next;
      const int item_no = p->value;
      const int rule_no = item_table[item_no].rule_number;
      if (RHS_SIZE(rule_no, rules) > 0) {
        int symbol = rhs_sym[rules[rule_no].rhs];
        if (IS_A_TERMINAL(symbol)) {
          return symbol;
        } else {
          symbol = get_shift_symbol(symbol, symbol_seen, clitems, rules, item_table, rhs_sym);
          if (symbol != empty) {
            return symbol;
          }
        }
      }
    }
  }
  return empty;
}

/// This procedure computes for each state the set of non-terminal symbols
/// that are required as candidates for secondary error recovery.  If the
/// option NAMES=OPTIMIZED is requested, the NAME map is optimized and SYMNO
/// is updated accordingly.
void produce(struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct Produced* produced, struct ScopeTop* st, JBitset first, struct scope_type *scope, struct node **clitems, bool *null_nt, long *scope_right_side, struct ruletab_type *rules, short **scope_state, struct statset_type *statset, struct itemtab *item_table, short *rhs_sym, short **gd_range, short **gd_index) {
  // TOP, STACK, and INDEX are used for the digraph algorithm
  // in the routines COMPUTE_PRODUCES.
  //
  // The array PRODUCES is used to construct two maps:
  //
  // 1) PRODUCES, a mapping from each non-terminal A to the set of
  // non-terminals C such that:
  //
  //                   A  =>*  x C w
  //
  // 2) RIGHT_MOST_PRODUCES, a mapping from each non-terminal A to
  // the set of non-terminals C such that:
  //
  //                   C =>+ A x   and   x =>* %empty.
  //
  // NOTE: This is really a reverse right-most produces mapping,
  //       since given the above rule, we say that
  //       C right-most produces A.
  short *stack = Allocate_short_array(num_symbols + 1);
  short *index_of = Allocate_short_array(num_symbols + 1);
  short *names_map = Allocate_short_array(num_names + 1);
  bool *name_used = Allocate_boolean_array(num_names + 1);
  short *item_list = Allocate_short_array(num_items + 1);
  short *nt_list = Allocate_short_array(num_non_terminals + 1);
  nt_list -= num_terminals + 1;
  JBitset set;
  calloc0_set(set, 1, dss->non_term_set_size);
  JBitset produces;
  calloc0_set(produces, num_non_terminals, dss->non_term_set_size);
  produces.raw -= (num_terminals + 1) * dss->non_term_set_size;
  struct node **goto_domain;
  calloc0(goto_domain, num_states + 1, struct node *);
  struct node **direct_produces;
  calloc0(direct_produces, num_non_terminals, struct node *);
  direct_produces -= num_terminals + 1;
  // Note that the space allocated for PRODUCES and DIRECT_PRODUCES
  // is automatically initialized to 0 by calloc. Logically, this sets
  // all the sets in the PRODUCES map to the empty set and all the
  // pointers in DIRECT_PRODUCES are set to NULL.
  //
  // Next, PRODUCES is initialized to compute RIGHT_MOST_PRODUCES.
  // Also, we count the number of error rules and verify that they are
  // in the right format.
  int item_root = NIL;
  for ALL_NON_TERMINALS3(sym_a) {
    struct node *p;
    for (bool end_node = (p = clitems[sym_a]) == NULL; !end_node; end_node = p == clitems[sym_a]) {
      p = p->next;
      int item_no = p->value;
      int sym_b = item_table[item_no].symbol;
      if (IS_A_NON_TERMINAL(sym_b)) {
        const int i = item_table[item_no].suffix_index;
        if (IS_IN_SET(first, i, empty) && !IS_IN_SET(produces, sym_b, sym_a - num_terminals)) {
          SET_BIT_IN(produces, sym_b, sym_a - num_terminals);
          struct node *q = Allocate_node();
          q->value = sym_a;
          q->next = direct_produces[sym_b];
          direct_produces[sym_b] = q;
        }
      }
      int rule_no = item_table[item_no].rule_number;
      int ii;
      for (ii = 0; ii < RHS_SIZE(rule_no, rules); ii++) {
        if (item_table[item_no + ii].symbol == error_image) {
          break;
        }
      }
      item_no += ii;
      sym_b = item_table[item_no].symbol;
      if (sym_b == error_image) {
        if (IS_A_NON_TERMINAL(item_table[item_no + 1].symbol) && ii > 0) {
          sym_b = item_table[item_no + 2].symbol;
          if (sym_b == empty) {
            num_error_rules++;
          }
        }
        if (sym_b != empty) {
          item_list[item_no] = item_root;
          item_root = item_no;
        }
      }
      sym_b = eoft_image;
    }
  }
  // If some error rules are in the wrong format and report them.
  if (item_root != NIL) {
    if (item_list[item_root] == NIL) {
      printf("*** This error rule is not in manual format:\n\n");
    } else {
      printf("*** These error rules are not in manual format:\n\n");
    }
    for (int item_no = item_root; item_no != NIL; item_no = item_list[item_no]) {
      print_item(item_no, cli_options, rules, item_table, rhs_sym);
    }
  }
  // Complete the construction of the RIGHT_MOST_PRODUCES map for
  // non-terminals using the digraph algorithm.
  // We make sure that each non-terminal A is not present in its own
  // PRODUCES set since we are interested in the non-reflexive
  // (positive) transitive closure.
  for ALL_SYMBOLS3(symbol) {
    index_of[symbol] = OMEGA;
  }
  struct ProduceTop top = {.top = 0};
  for ALL_NON_TERMINALS3(nt) {
    if (index_of[nt] == OMEGA) {
      compute_produces(nt, direct_produces, stack, index_of, produced->produces, &top);
    }
    RESET_BIT_IN(produces, nt, nt - num_terminals);
  }
  // Construct the minimum subset of the domain of the GOTO map
  // needed for automatic secondary level error recovery.   For each
  // state, we start out with the set of all nonterminals on which
  // there is a transition in that state, and pare it down to a
  // subset S, by removing all nonterminals B in S such that there
  // is a goto-reduce action on B by a single production.  If the
  // READ-REDUCE option is not turned on, then, we check whether or
  // not the goto action on B is to an LR(0) reduce state.Once we have
  // our subset S, we further reduce its size as follows.  For each
  // nonterminal A in S such that there exists another nonterminal
  // B in S, where B ^= A,  A ->+ Bx  and  x =>* %empty, we remove A
  // from S.
  // At the end of this process, the nonterminal elements whose
  // NT_LIST values are still OMEGA are precisely the nonterminal
  // symbols that are never used as candidates.
  for ALL_NON_TERMINALS3(i) {
    nt_list[i] = OMEGA;
  }
  nt_list[accept_image] = NIL;
  for ALL_STATES3(state_no) {
    const struct goto_header_type go_to = statset[state_no].go_to;
    int nt_root = NIL;
    INIT_SET(set);
    for (int i = 1; i <= go_to.size; i++) {
      const int symbol = go_to.map[i].symbol;
      const int state = go_to.map[i].action;
      int rule_no;
      if (state < 0) {
        rule_no = -state;
      } else {
        struct node *q = statset[state].kernel_items;
        int item_no = q->value;
        if (q->next != NULL) {
          rule_no = 0;
        } else {
          rule_no = item_table[item_no].rule_number;
        }
      }
      if (rule_no == 0 || RHS_SIZE(rule_no, rules) != 1) {
        nt_list[symbol] = nt_root;
        nt_root = symbol;
        SET_UNION(set, 0, produces, symbol);
      }
    }
    goto_domain[state_no] = NULL;
    for (int symbol = nt_root; symbol != NIL; symbol = nt_list[symbol]) {
      if (!IS_ELEMENT(set, symbol - num_terminals)) {
        struct node *q = Allocate_node();
        q->value = symbol;
        q->next = goto_domain[state_no];
        goto_domain[state_no] = q;
        gotodom_size++;
      }
    }
  }
  // Allocate and construct the permanent goto domain structure:
  //   GD_INDEX and GD_RANGE.
  int n = 0;
  *gd_index = Allocate_short_array(num_states + 2);
  *gd_range = Allocate_short_array(gotodom_size + 1);
  for ALL_STATES3(state_no) {
    (*gd_index)[state_no] = n + 1;
    struct node *p;
    struct node *q;
    for (p = goto_domain[state_no]; p != NULL; q = p, p = p->next) {
      (*gd_range)[++n] = p->value;
    }
    if (goto_domain[state_no] != NULL) {
      free_nodes(goto_domain[state_no], q);
    }
  }
  (*gd_index)[num_states + 1] = n + 1;
  // Remove names assigned to nonterminals that are never used as
  // error candidates.
  if (cli_options->names_opt.value == OPTIMIZE_PHRASES.value) {
    // In addition to nonterminals that are never used as candidates,
    // if a nullable nonterminal was assigned a name by default
    // (nonterminals that were "named" by default are identified
    // with negative indices), that name is also removed.
    for ALL_NON_TERMINALS3(symbol) {
      if (nt_list[symbol] == OMEGA) {
        symno[symbol].name_index = symno[accept_image].name_index;
      } else if (symno[symbol].name_index < 0) {
        if (null_nt[symbol]) {
          symno[symbol].name_index = symno[accept_image].name_index;
        } else {
          symno[symbol].name_index = -symno[symbol].name_index;
        }
      }
    }
    // Adjust name map to remove unused elements and update SYMNO map.
    for (int i = 1; i <= num_names; i++) {
      name_used[i] = false;
    }
    for ALL_SYMBOLS3(symbol) {
      name_used[symno[symbol].name_index] = true;
    }
    n = 0;
    for (int i = 1; i <= num_names; i++) {
      if (name_used[i]) {
        name[++n] = name[i];
        names_map[i] = n;
      }
    }
    num_names = n;
    for ALL_SYMBOLS3(symbol) {
      symno[symbol].name_index = names_map[symno[symbol].name_index];
    }
  }
  if (cli_options->scopes_bit) {
    // Process scopes.
    {
      // Compute set of "scopes" and use it to construct SCOPE map.
      short *prefix_index;
      short *suffix_index;
      short *state_index;
      struct node **states_of;
      int num_state_sets = 0;
      int n;
      int max_prefix_length = 0;
      int dot_symbol;
      int item_root;
      int rule_no;
      int nt_root;
      bool end_node;
      struct node *p;
      struct node *q;
      prefix_index = Allocate_short_array(num_items + 1);
      suffix_index = Allocate_short_array(num_items + 1);
      short *item_of = Allocate_short_array(num_non_terminals);
      item_of -= num_terminals + 1;
      short *next_item = Allocate_short_array(num_items + 1);
      bool* symbol_seen = Allocate_boolean_array(num_non_terminals);
      symbol_seen -= num_terminals + 1;
      calloc0(states_of, num_non_terminals, struct node *);
      states_of -= num_terminals + 1;
      state_index = Allocate_short_array(num_non_terminals);
      state_index -= num_terminals + 1;
      struct scope_elmt *scope_element;
      calloc0(scope_element, num_items + 1, struct scope_elmt);
      // Initially, PRODUCES was used to compute the right-most-produces
      // map.  We save that map map and make it reflexive.  Recall that
      // RIGHT_PRODUCES is a mapping from each nonterminal B into the set
      // of nonterminals A such that:
      //
      //    A =>rm* B
      //
      // Next, reallocate PRODUCES and initialize it in order to
      // construct the LEFT_PRODUCES map. Initially, CALLOC sets PRODUCES
      // to the empty map.
      // LEFT_PRODUCES is a mapping  from each nonterminal A into the set
      // of nonterminals B such that:
      //
      //    A =>lm* B x
      //
      // for some arbitrary string x.
      //
      // Since A ->* A for all A,  we insert A in PRODUCES(A)  (but not
      // in the linked list).
      produced->right_produces = produces;
      calloc0_set(produces, num_non_terminals, dss->non_term_set_size);
      produces.raw -= (num_terminals + 1) * dss->non_term_set_size;
      for ALL_NON_TERMINALS3(nt) {
        SET_BIT_IN(produced->right_produces, nt, nt - num_terminals);
        SET_BIT_IN(produces, nt, nt - num_terminals);
        direct_produces[nt] = NULL;
        for (end_node = (p = clitems[nt]) == NULL;
             !end_node; end_node = p == clitems[nt]) {
          p = p->next;
          for (int item_no = p->value;
               IS_A_NON_TERMINAL(item_table[item_no].symbol);
               item_no++) {
            int symbol = item_table[item_no].symbol;
            if (!IS_IN_SET(produces, nt, symbol - num_terminals)) {
              SET_BIT_IN(produces, nt, symbol - num_terminals);
              q = Allocate_node();
              q->value = symbol;
              q->next = direct_produces[nt];
              direct_produces[nt] = q;
            }
            if (!null_nt[symbol]) {
              break;
            }
          }
        }
      }
      // Complete the construction of the LEFT_produces map for
      // non_terminals using the digraph algorithm.
      for ALL_NON_TERMINALS3(nt) {
        index_of[nt] = OMEGA;
      }
      top.top = 0;
      for ALL_NON_TERMINALS3(nt) {
        if (index_of[nt] == OMEGA) {
          compute_produces(nt, direct_produces, stack, index_of, produced->produces, &top);
        }
      }
      produced->left_produces = produces;
      // Allocate and initialize the PRODUCES array to construct the
      // PRODUCES map.  After allocation, CALLOC sets all sets to empty.
      // Since A ->* A for all A,  we insert A in PRODUCES(A)  (but not
      // in the linked list).
      calloc0_set(produces, num_non_terminals, dss->non_term_set_size);
      produces.raw -= (num_terminals + 1) * dss->non_term_set_size;
      for ALL_NON_TERMINALS3(nt) {
        SET_BIT_IN(produces, nt, nt - num_terminals);
        direct_produces[nt] = NULL;
        for (end_node = (p = clitems[nt]) == NULL;
             !end_node; end_node = p == clitems[nt]) {
          p = p->next;
          for (int item_no = p->value;
               item_table[item_no].symbol != empty; item_no++) {
            int symbol = item_table[item_no].symbol;
            if (IS_A_NON_TERMINAL(symbol)) {
              if (!IS_IN_SET(produces, nt, symbol - num_terminals)) {
                SET_BIT_IN(produces, nt, symbol - num_terminals);
                q = Allocate_node();
                q->value = symbol;
                q->next = direct_produces[nt];
                direct_produces[nt] = q;
              }
            }
          }
        }
      }
      // Complete the construction of the PRODUCES map for
      // non_terminals using the digraph algorithm.
      //
      // Since $ACC =>* x A y for all nonterminal A in the grammar, a
      // single call to COMPUTE_PRODUCES does the trick.
      for ALL_NON_TERMINALS3(nt) {
        index_of[nt] = OMEGA;
      }
      top.top = 0;
      compute_produces(accept_image, direct_produces, stack, index_of, produced->produces, &top);
      // Construct a mapping from each non_terminal A into the set of
      // items of the form [B  ->  x . A y].
      for ALL_NON_TERMINALS3(nt) {
        item_of[nt] = NIL;
      }
      for ALL_ITEMS3(item_no) {
        dot_symbol = item_table[item_no].symbol;
        if (IS_A_NON_TERMINAL(dot_symbol)) {
          next_item[item_no] = item_of[dot_symbol];
          item_of[dot_symbol] = item_no;
        }
      }
      // Construct a list of scoped items in ITEM_LIST.
      // Scoped items are derived from rules of the form  A -> x B y such
      // that B =>* w A z, %empty not in FIRST(y), and it is not the case
      // that x = %empty and B ->* A v.
      // Scoped items may also be identified by the user, using %error
      // productions.
      // As scoped items are added to the list, we keep track of the
      // longest prefix encountered.  This is subsequently used to
      // bucket sort the scoped items in descending order of the length
      // of their prefixes.
      for ALL_ITEMS3(item_no) {
        item_list[item_no] = OMEGA;
      }
      item_root = NIL;
      for ALL_ITEMS3(item_no) {
        dot_symbol = item_table[item_no].symbol;
        if (dot_symbol == error_image) {
          if (item_table[item_no].dot != 0 &&
              !IS_IN_SET(first, item_table[item_no].suffix_index, empty)) {
            if (item_list[item_no] == OMEGA) {
              item_list[item_no] = item_root;
              item_root = item_no;
              max_prefix_length = MAX(max_prefix_length, item_table[item_no].dot);
            }
          }
        } else if (IS_A_NON_TERMINAL(dot_symbol)) {
          int symbol = rules[item_table[item_no].rule_number].lhs;
          if (!IS_IN_SET(first, item_table[item_no].suffix_index, empty) && IS_IN_SET(produces, dot_symbol, symbol - num_terminals)) {
            if (is_scope(item_no, symbol_seen, produced, item_of, next_item, null_nt, rules, item_table)) {
              int ii;
              for (ii = item_no + 1; ; ii++) {
                symbol = item_table[ii].symbol;
                if (IS_A_TERMINAL(symbol)) {
                  break;
                }
                if (!null_nt[symbol]) {
                  break;
                }
              }
              if (IS_A_NON_TERMINAL(symbol)) {
                for ALL_NON_TERMINALS3(nt) {
                  symbol_seen[nt] = false;
                }
                symbol = get_shift_symbol(symbol, symbol_seen, clitems, rules, item_table, rhs_sym);
              }
              if (symbol != empty && item_list[ii] == OMEGA) {
                item_list[ii] = item_root;
                item_root = ii;
                max_prefix_length = MAX(max_prefix_length, item_table[ii].dot);
              }
            }
          }
        }
      }
      // In this loop, the prefix and suffix string for each scope in
      // entered into a table.  We also use the SYMBOL_SEEN array to
      // identify the set of left-hand side symbols associated with the
      // scopes.
      short *scope_table = Allocate_short_array(SCOPE_SIZE);
      for (int i = 0; i < SCOPE_SIZE; i++) {
        scope_table[i] = NIL;
      }
      for ALL_NON_TERMINALS3(nt) {
        symbol_seen[nt] = false;
      }
      for (int item_no = item_root; item_no != NIL; item_no = item_list[item_no]) {
        rule_no = item_table[item_no].rule_number;
        int symbol = rules[rule_no].lhs;
        num_scopes = num_scopes + 1;
        symbol_seen[symbol] = true;
        prefix_index[item_no] = insert_prefix(item_no, scope_element, scope_table, st, rules, item_table, rhs_sym);
        suffix_index[item_no] = insert_suffix(item_no, scope_element, scope_table, st, null_nt, rules, item_table, rhs_sym);
      }
      ffree(scope_table);
      // We now construct a mapping from each nonterminal symbol that is
      // the left-hand side of a rule containing scopes into the set of
      // states that has a transition on the nonterminal in question.
      nt_root = NIL;
      for ALL_NON_TERMINALS3(nt) {
        states_of[nt] = NULL;
      }
      for ALL_STATES3(state_no) {
        struct goto_header_type go_to;
        go_to = statset[state_no].go_to;
        for (int i = 1; i <= go_to.size; i++) {
          int symbol = go_to.map[i].symbol;
          if (symbol_seen[symbol]) {
            if (states_of[symbol] == NULL) {
              nt_list[symbol] = nt_root;
              nt_root = symbol;
              num_state_sets = num_state_sets + 1;
            }
            q = Allocate_node();
            q->value = state_no;
            q->next = states_of[symbol];
            states_of[symbol] = q;
          }
        }
      }
      produced->right_produces.raw += (num_terminals + 1) * dss->non_term_set_size;
      ffree(produced->right_produces.raw);
      produced->left_produces.raw += (num_terminals + 1) * dss->non_term_set_size;
      ffree(produced->left_produces.raw);
      // Next, we used the optimal partition procedure to compress the
      // space used by the sets of states, allocate the SCOPE structure
      // and store the compressed sets of states in it.
      // We also sort the list of items by the length of their prefixes in
      // descending order.  This is done primarily as an optimization.
      // If a longer prefix matches prior to a shorter one, the parsing
      // will terminate quicker.
    process_scope_states: {
        JBitset collection;
        long *element_size;
        long *list;
        long *start;
        long *stack;
        short *ordered_symbol;
        short *state_list;
        short *bucket;
        int state_root;
        int state_no_inner;
        long state_set_size = num_states / SIZEOF_BC + (num_states % SIZEOF_BC ? 1 : 0);
        calloc0_set(collection, num_state_sets + 1, state_set_size);
        element_size = Allocate_long_array(num_state_sets + 1);
        start = Allocate_long_array(num_state_sets + 2);
        stack = Allocate_long_array(num_state_sets + 1);
        ordered_symbol = Allocate_short_array(num_state_sets + 1);
        list = Allocate_long_array(num_state_sets + 1);
        state_list = Allocate_short_array(num_states + 1);
        bucket = Allocate_short_array(max_prefix_length + 1);
        for (int symbol = nt_root, i = 1; symbol != NIL; symbol = nt_list[symbol], i++) {
          list[i] = i;
          ordered_symbol[i] = symbol;
          INIT_BITSET(collection, i);
          element_size[i] = 0;
          for (p = states_of[symbol]; p != NULL; p = p->next) {
            element_size[i]++;
            SET_BIT_IN(collection, i, p->value);
          }
        }
        partset(collection, element_size, list, start, stack, num_state_sets, true);
        for (int i = 1; i <= num_state_sets; i++) {
          int symbol = ordered_symbol[i];
          state_index[symbol] = ABS(start[i]);
        }
        scope_state_size = start[num_state_sets + 1] - 1;
        calloc0(scope, num_scopes + 1, struct scope_type);
        scope_right_side = Allocate_long_array(scope_rhs_size + 1);
        *scope_state = Allocate_short_array(scope_state_size + 1);
        int k = 0;
        for (int i = 0; i <= num_states; i++) {
          state_list[i] = OMEGA;
        }
        for (int i = 1; i <= num_state_sets; i++) {
          if (start[i] > 0) {
            state_root = 0;
            state_list[state_root] = NIL;
            int j;
            for (end_node = (j = i) == NIL; !end_node; end_node = j == i) {
              j = stack[j];
              int symbol = ordered_symbol[j];
              for (p = states_of[symbol]; p != NULL; p = p->next) {
                state_no_inner = p->value;
                if (state_list[state_no_inner] == OMEGA) {
                  state_list[state_no_inner] = state_root;
                  state_root = state_no_inner;
                }
              }
            }
            for (state_no_inner = state_root; state_no_inner != NIL; state_no_inner = state_root) {
              state_root = state_list[state_no_inner];
              state_list[state_no_inner] = OMEGA;
              k++;
              *scope_state[k] = state_no_inner;
            }
          }
        }
        for (int symbol = nt_root; symbol != NIL; symbol = nt_list[symbol]) {
          for (p = states_of[symbol]; p != NULL; q = p, p = p->next) {
          }
          free_nodes(states_of[symbol], q);
        }
        // Use the BUCKET array as a base to partition the scoped items
        // based on the length of their prefixes.  The list of items in each
        // bucket is kept in the NEXT_ITEM array sorted in descending order
        // of the length of the right-hand side of the item.
        // Items are kept sorted in that fashion because when two items have
        // the same prefix, we want the one with the shortest suffix to be
        // chosen. In other words, if we have two scoped items, say:
        //
        //    A ::= x . y       and      B ::= x . z     where |y| < |z|
        //
        // and both of them are applicable in a given context with similar
        // result, then we always want A ::= x . y to be used.
        for (int i = 1; i <= max_prefix_length; i++) {
          bucket[i] = NIL;
        }
        for (int item_no = item_root; item_no != NIL; item_no = item_list[item_no]) {
          int tail;
          k = item_table[item_no].dot;
          int ii;
          for (ii = bucket[k]; ii != NIL; tail = ii, ii = next_item[ii]) {
            if (RHS_SIZE(item_table[item_no].rule_number, rules) >=
                RHS_SIZE(item_table[ii].rule_number, rules)) {
              break;
            }
          }
          next_item[item_no] = ii;
          if (ii == bucket[k]) {
            bucket[k] = item_no; /* insert at the beginning */
          } else {
            next_item[tail] = item_no; /* insert in middle or end */
          }
        }
        // Reconstruct list of scoped items in sorted order. Since we want
        // the items in descending order, we start with the smallest bucket
        // proceeding to the largest one and insert the items from each
        // bucket in LIFO order in ITEM_LIST.
        item_root = NIL;
        for (int k = 1; k <= max_prefix_length; k++) {
          for (int item_no = bucket[k]; item_no != NIL; item_no = next_item[item_no]) {
            item_list[item_no] = item_root;
            item_root = item_no;
          }
        }
        ffree(collection.raw);
        ffree(element_size);
        ffree(start);
        ffree(stack);
        ffree(ordered_symbol);
        ffree(state_list);
        ffree(list);
        ffree(bucket);
      } /* End PROCESS_SCOPE_STATES */
      // Next, we initialize the remaining fields of the SCOPE structure.
      int item_no = item_root;
      for (int i = 1; item_no != NIL; i++) {
        scope[i].prefix = prefix_index[item_no];
        scope[i].suffix = suffix_index[item_no];
        rule_no = item_table[item_no].rule_number;
        scope[i].lhs_symbol = rules[rule_no].lhs;
        int symbol = rhs_sym[rules[rule_no].rhs + item_table[item_no].dot];
        if (IS_A_TERMINAL(symbol)) {
          scope[i].look_ahead = symbol;
        } else {
          for ALL_NON_TERMINALS3(j) {
            symbol_seen[j] = false;
          }
          scope[i].look_ahead = get_shift_symbol(symbol, symbol_seen, clitems, rules, item_table, rhs_sym);
        }
        scope[i].state_set = state_index[scope[i].lhs_symbol];
        item_no = item_list[item_no];
      }
      for (int j = 1; j <= st->top; j++) {
        if (scope_element[j].item < 0) {
          item_no = -scope_element[j].item;
          rule_no = item_table[item_no].rule_number;
          n = scope_element[j].index;
          for (int k = rules[rule_no].rhs + item_table[item_no].dot - 1;
               k >= rules[rule_no].rhs; /* symbols before dot*/
               k--)
            scope_right_side[n++] = rhs_sym[k];
        } else {
          item_no = scope_element[j].item;
          rule_no = item_table[item_no].rule_number;
          n = scope_element[j].index;
          for (int k = rules[rule_no].rhs + item_table[item_no].dot;
               k < rules[rule_no + 1].rhs; /* symbols after dot */
               k++) {
            int symbol = rhs_sym[k];
            if (IS_A_NON_TERMINAL(symbol)) {
              if (!null_nt[symbol]) {
                scope_right_side[n++] = rhs_sym[k];
              }
            } else if (symbol != error_image) {
              scope_right_side[n++] = rhs_sym[k];
            }
          }
        }
        scope_right_side[n] = 0;
      }
      ffree(prefix_index);
      ffree(suffix_index);
      item_of += num_terminals + 1;
      ffree(item_of);
      ffree(next_item);
      symbol_seen += num_terminals + 1;
      ffree(symbol_seen);
      states_of += num_terminals + 1;
      ffree(states_of);
      state_index += num_terminals + 1;
      ffree(state_index);
      ffree(scope_element);
    }
  }
  ffree(stack);
  ffree(index_of);
  ffree(names_map);
  ffree(name_used);
  nt_list += num_terminals + 1;
  ffree(nt_list);
  ffree(set.raw);
  produces.raw += (num_terminals + 1) * dss->non_term_set_size;
  ffree(produces.raw);
  direct_produces += num_terminals + 1;
  ffree(direct_produces);
  ffree(goto_domain);
}

/// For a given symbol, complete the computation of
/// PRODUCES[symbol].
///
/// This procedure is used to compute the transitive closure of
/// the PRODUCES, LEFT_PRODUCES and RIGHT_MOST_PRODUCES maps.
void compute_produces(const int symbol, struct node **direct_produces, short *stack, short *index_of, JBitset produces, struct ProduceTop* top_value) {
  stack[++top_value->top] = symbol;
  const int indx = top_value->top;
  index_of[symbol] = indx;
  struct node *q;
  for (struct node *p = direct_produces[symbol]; p != NULL; q = p, p = p->next) {
    int new_symbol = p->value;
    /* first time seen? */
    if (index_of[new_symbol] == OMEGA) {
      compute_produces(new_symbol, direct_produces, stack, index_of, produces, top_value);
    }
    index_of[symbol] = MIN(index_of[symbol], index_of[new_symbol]);
    SET_UNION(produces, symbol, produces, new_symbol);
  }
  if (direct_produces[symbol] != NULL) {
    free_nodes(direct_produces[symbol], q);
  }
  /* symbol is SCC root */
  if (index_of[symbol] == indx) {
    for (int new_symbol = stack[top_value->top]; new_symbol != symbol; new_symbol = stack[--top_value->top]) {
      ASSIGN_SET(produces, new_symbol, produces, symbol);
      index_of[new_symbol] = INFINITY;
    }
    index_of[symbol] = INFINITY;
    top_value->top--;
  }
}
// === Produce End ===





/// In this procedure, we first construct the LR(0) automaton.
void mkstats(struct CLIOptions *cli_options, struct DetectedSetSizes* dss, JBitset first,  struct scope_type *scope, struct node **clitems, struct node **closure, struct SRTable* srt, long *scope_right_side, bool *null_nt, short **scope_state, struct itemtab *item_table, struct ruletab_type *rules, short *rhs_sym, short **gd_range, short **gd_index) {
  struct ScopeTop st = (struct ScopeTop) {
    .top = 0
  };
  struct goto_header_type no_gotos_ptr = (struct goto_header_type) {
    /* For states with no GOTOs */
    .size = 0,
    .map = NULL,
  };
  struct shift_header_type no_shifts_ptr = (struct shift_header_type) {
    /* For states with no SHIFTs */
    .size = 0,
    .map = NULL,
  };
  mklr0(cli_options, &no_shifts_ptr, &no_gotos_ptr, clitems, closure, srt, rules, item_table);
  struct Produced produced = {};
  if (error_maps_bit && (cli_options->table_opt.value == OPTIMIZE_TIME.value || cli_options->table_opt.value == OPTIMIZE_SPACE.value)) {
    produce(cli_options, dss, &produced, &st, first, scope, clitems, null_nt, scope_right_side, rules, scope_state, statset, item_table, rhs_sym, gd_range, gd_index);
  }
  // Free space trapped by the CLOSURE and CLITEMS maps.
  for ALL_NON_TERMINALS3(j) {
    struct node *p;
    struct node *q = clitems[j];
    if (q != NULL) {
      p = q->next;
      free_nodes(p, q);
    }
    q = closure[j];
    if (q != NULL) {
      p = q->next;
      free_nodes(p, q);
    }
  }
  closure += num_terminals + 1;
  ffree(closure);
  clitems += num_terminals + 1;
  ffree(clitems);
}
