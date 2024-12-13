#include <stdlib.h>

#include "lpgparse.h"
static char hostfile[] = __FILE__;

#include <string.h>
#include "common.h"

JBitset nt_first = {.raw = NULL};
JBitset first = {.raw = NULL};
JBitset follow = {.raw = NULL};

int const LEN = PRINT_LINE_SIZE - 4;

long NEXT_RULE_SIZE() {
  return num_rules + 1;
}

long LAST_RHS_INDEX(const int rule_no) {
  return rules[rule_no + 1].rhs - 1;
}

struct f_element_type {
  short suffix_root;
  short suffix_tail;
  short link;
} *first_element;

/// TOP, STACK, and INDEX_OF are used for the linear graph algorithm in
/// constructing the FIRST, FOLLOW and CLOSURE maps.
///
/// LHS_RULE and NEXT_RULE are used in constructing a map from non-terminals
/// to the set of rules produced by the non-terminals.
///
/// FIRSTis an array used as a hash table to construct
/// the mapping from sequence of symbols to their FIRST terminal
/// set.  A sequence is hashed into a location depending on the
/// first symbol in that sequence.
///
/// FIRST_ITEM_OF is a map from each rule into the first item
/// that it generates.
///
/// The following pointers are used to construct a mapping from each symbol
/// in the grammar into the set of items denoted by that symbol. I.e.,
///
///     f(t) := { [A -> x .t y] | A -> x t y is a rule in the grammar }
///
/// Since these sets are simply partitions of the set of items, they are kept
/// all in a sequential list in the array NEXT_ITEM.  The roots of the lists
/// are placed in the arrays T_ITEMS and NT_ITEMS.
short *stack;
short *index_of;
short *lhs_rule;
short *next_rule;
short *first_table;
short *first_item_of;
short *next_item;
short *nt_items;
short *nt_list;

int top;

///   This procedure tries to advance the RHS_START pointer.  If the current
/// symbol identified by the RHS_START element is a bad non-terminal it
/// returns FALSE.  Otherwise, the whole right-hand side is traversed, and it
/// returns the value TRUE.
bool is_terminal_rhs(short *rhs_start, const bool *produces_terminals, const int rule_no) {
  for (rhs_start[rule_no] = rhs_start[rule_no]; rhs_start[rule_no] <= rules[rule_no + 1].rhs - 1; rhs_start[rule_no]++) {
    const int symbol = rhs_sym[rhs_start[rule_no]];
    if (IS_A_NON_TERMINAL(symbol)) {
      if (!produces_terminals[symbol])
        return false;
    }
  }
  return true;
}

/// This procedure checks whether any non-terminal symbols can fail to
/// generate a string of terminals.
///
/// A non-terminal "A" can generate a terminal string if the grammar in
/// question contains a rule of the form:
///
///         A ::= X1 X2 ... Xn           n >= 0,  1 <= i <= n
///
/// and Xi, for all i, is a terminal or a non-terminal that can generate a
/// string of terminals.
/// This routine is structurally identical to COMPUTE_NULLABLES.
void check_non_terminals(struct CLIOptions* cli_options) {
  bool changed = true;
  short *rhs_start = Allocate_short_array(NEXT_RULE_SIZE());
  bool *produces_terminals = Allocate_boolean_array(num_non_terminals);
  produces_terminals -= num_terminals + 1;
  // First, mark all non-terminals as not producing terminals. Then
  // initialize RHS_START. RHS_START is a mapping from each rule in
  // the grammar into the next symbol in its right-hand side that
  // has not yet proven to be a symbol that generates terminals.
  for ALL_NON_TERMINALS3(nt)
    produces_terminals[nt] = false;
  produces_terminals[accept_image] = true;
  for ALL_RULES3(rule_no) {
    rhs_start[rule_no] = rules[rule_no].rhs;
  }
  // We now iterate over the rules and try to advance the RHS_START
  // pointer to each right-hand side as far as we can.  If one or
  // more non-terminals are found to be "all right", they are
  // marked as such and the process is repeated.
  //
  // If we go through all the rules and no new non-terminal is
  // found to be "all right" then we stop and return.
  //
  // Note that on each iteration, only rules associated with
  // non-terminals that are not "all right" are considered. Further,
  // as soon as a non-terminal is found to be "all right", the
  // remaining rules associated with it are not considered. I.e.,
  // we quit the inner loop.
  while (changed) {
    changed = false;
    for ALL_NON_TERMINALS3(nt) {
      int rule_no;
      for (bool end_node = (rule_no = lhs_rule[nt]) == NIL;
           !produces_terminals[nt] && !end_node;
           end_node = rule_no == lhs_rule[nt]) {
        rule_no = next_rule[rule_no];
        if (is_terminal_rhs(rhs_start, produces_terminals, rule_no)) {
          changed = true;
          produces_terminals[nt] = true;
        }
      }
    }
  }
  // Construct a list of all non-terminals that do not generate
  // terminal strings.
  int nt_root = NIL;
  int nt_last;
  for ALL_NON_TERMINALS3(nt) {
    if (!produces_terminals[nt]) {
      if (nt_root == NIL)
        nt_root = nt;
      else
        nt_list[nt_last] = nt;
      nt_last = nt;
    }
  }
  // If there are non-terminal symbols that do not generate
  // terminal strings, print them out and stop the program.
  if (nt_root != NIL) {
    char line[PRINT_LINE_SIZE + 1];
    nt_list[nt_last] = NIL; /* mark end of list */
    strcpy(line, "*** ERROR: The following Non-terminal");
    if (nt_list[nt_root] == NIL) {
      strcat(line, " does not generate any terminal strings: ");
    } else {
      strcat(line, "s do not generate any terminal strings: ");
      PRNT(line);
      strcpy(line, "        "); /* 8 spaces */
    }
    for (int symbol = nt_root; symbol != NIL; symbol = nt_list[symbol]) {
      char tok[SYMBOL_SIZE + 1];
      restore_symbol(tok, RETRIEVE_STRING(symbol), cli_options->ormark, cli_options->escape);
      if (strlen(line) + strlen(tok) > PRINT_LINE_SIZE - 1) {
        PRNT(line);
        print_large_token(line, tok, "    ", LEN);
      } else
        strcat(line, tok);
      strcat(line, " ");
    }
    PRNT(line);
    exit(12);
  }
  produces_terminals += num_terminals + 1;
  ffree(produces_terminals);
  ffree(rhs_start);
}

void no_rules_produced(struct CLIOptions* cli_options) {
  // Build a list of all non-terminals that do not produce any rules.
  int nt_root = NIL;
  int nt_last;
  for ALL_NON_TERMINALS3(symbol) {
    if (lhs_rule[symbol] == NIL) {
      if (nt_root == NIL) {
        nt_root = symbol;
      } else {
        nt_list[nt_last] = symbol;
      }
      nt_last = symbol;
    }
  }
  // If the list of non-terminals that do not produce any rules
  // is not empty, signal error and stop.
  if (nt_root != NIL) {
    char line[PRINT_LINE_SIZE + 1];
    nt_list[nt_last] = NIL;
    if (nt_list[nt_root] == NIL) {
      PRNTERR("The following Non-terminal does not produce any rules: ");
    } else {
      PRNTERR("The following Non-terminals do not produce any rules: ");
    }
    strcpy(line, "        ");
    for (int symbol = nt_root; symbol != NIL; symbol = nt_list[symbol]) {
      char tok[SYMBOL_SIZE + 1];
      restore_symbol(tok, RETRIEVE_STRING(symbol), cli_options->ormark, cli_options->escape);
      if (strlen(line) + strlen(tok) > PRINT_LINE_SIZE) {
        PRNT(line);
        print_large_token(line, tok, "    ", LEN);
      } else {
        strcat(line, tok);
      }
      strcat(line, " ");
    }
    PRNT(line);
    exit(12);
  }
}

///  This function computes the closure of a non-terminal LHS_SYMBOL passed
/// to it as an argument using the digraph algorithm.
///  The closure of a non-terminal A is the set of all non-terminals Bi that
/// can directly or indirectly start a string generated by A.
/// I.e., A *::= Bi X where X is an arbitrary string.
void compute_closure(const int lhs_symbol) {
  short *nont_list = Allocate_short_array(num_non_terminals);
  nont_list -= num_terminals + 1; /* Temporary direct        */
  // access set for closure.
  stack[++top] = lhs_symbol;
  const int indx = top;
  index_of[lhs_symbol] = indx;
  for ALL_NON_TERMINALS3(i) {
    nont_list[i] = OMEGA;
  }
  nont_list[lhs_symbol] = NIL;
  int nt_root = lhs_symbol;
  closure[lhs_symbol] = NULL; /* Permanent closure set. Linked list */
  int rule_no;
  for (bool end_node = (rule_no = lhs_rule[lhs_symbol]) == NIL; !end_node; /* Iterate over all rules of LHS_SYMBOL */ end_node = rule_no == lhs_rule[lhs_symbol]) {
    rule_no = next_rule[rule_no];
    int symbol = RHS_SIZE(rule_no) == 0 ? empty : rhs_sym[rules[rule_no].rhs];
    if (IS_A_NON_TERMINAL(symbol)) {
      if (nont_list[symbol] == OMEGA) {
        if (index_of[symbol] == OMEGA) /* if first time seen */
        {
          compute_closure(symbol);
        }
        index_of[lhs_symbol] = MIN(index_of[lhs_symbol], index_of[symbol]);
        nont_list[symbol] = nt_root;
        nt_root = symbol;
        // add closure[symbol] to closure of LHS_SYMBOL.
        struct node *q;
        for (end_node = (q = closure[symbol]) == NULL;
             !end_node;
             end_node = q == closure[symbol]) {
          q = q->next;
          if (nont_list[q->value] == OMEGA) {
            nont_list[q->value] = nt_root;
            nt_root = q->value;
          }
        }
      }
    }
  }
  for (; nt_root != lhs_symbol; nt_root = nont_list[nt_root]) {
    struct node *p = Allocate_node();
    p->value = nt_root;
    if (closure[lhs_symbol] == NULL)
      p->next = p;
    else {
      p->next = closure[lhs_symbol]->next;
      closure[lhs_symbol]->next = p;
    }
    closure[lhs_symbol] = p;
  }
  if (index_of[lhs_symbol] == indx) {
    for (int symbol = stack[top]; symbol != lhs_symbol; symbol = stack[--top]) {
      struct node *q = closure[symbol];
      if (q != NULL) {
        struct node *p = q->next;
        free_nodes(p, q); /* free nodes used by CLOSURE[SYMBOL] */
        closure[symbol] = NULL;
      }
      struct node *p = Allocate_node();
      p->value = lhs_symbol;
      p->next = p;
      closure[symbol] = p;
      for (bool end_node = (q = closure[lhs_symbol]) == NULL; !end_node; end_node = q == closure[lhs_symbol]) {
        q = q->next;
        if (q->value != symbol) {
          p = Allocate_node();
          p->value = q->value;
          p->next = closure[symbol]->next;
          closure[symbol]->next = p;
          closure[symbol] = p;
        }
      }
      index_of[symbol] = INFINITY;
    }
    index_of[lhs_symbol] = INFINITY;
    top--;
  }
  nont_list += num_terminals + 1;
  ffree(nont_list);
}

/// This procedure tries to advance the RHS_START pointer.  If the current
/// symbol identified by the RHS_START element is a terminal it returns FALSE
/// to indicate that it cannot go any further.  If it encounters a  non-null-
/// label non-terminal, it also returns FALSE. Otherwise, the whole right-hand
/// side is consumed, and it returns the value TRUE.
bool is_nullable_rhs(short *rhs_start, const int rule_no) {
  for (rhs_start[rule_no] = rhs_start[rule_no]; rhs_start[rule_no] <= rules[rule_no + 1].rhs - 1; rhs_start[rule_no]++) {
    const int symbol = rhs_sym[rhs_start[rule_no]];
    if (IS_A_TERMINAL(symbol)) {
      return false;
    } else if (!null_nt[symbol]){
      /* when symbol is a non-terminal */
      return false;
    }
  }
  return true;
}

/// This subroutine computes FIRST(NT) for some non-terminal NT using the
/// digraph algorithm.
/// FIRST(NT) is the set of all terminals Ti that may start a string generated
/// by NT. That is, NT *::= Ti X where X is an arbitrary string.
void compute_first(const int nt, struct DetectedSetSizes* dss) {
  JBitset temp_set;
  calloc0_set(temp_set, 1, dss->term_set_size)
  stack[++top] = nt;
  const int indx = top;
  index_of[nt] = indx;
  // Iterate over all rules generated by non-terminal NT...
  // In this application of the transitive closure algorithm,
  //
  //  G(A) := { t | A ::= t X for a terminal t and a string X }
  //
  // The relation R is defined as follows:
  //
  //    R(A, B) iff A ::= B1 B2 ... Bk B X
  //
  // where Bi is nullable for 1 <= i <= k
  int rule_no;
  for (bool end_node = (rule_no = lhs_rule[nt]) == NIL; !end_node; /* Iterate over all rules produced by NT */ end_node = rule_no == lhs_rule[nt]) {
    rule_no = next_rule[rule_no];
    bool blocked = false;
    for ENTIRE_RHS3(i, rule_no) {
      int symbol = rhs_sym[i];
      if (IS_A_NON_TERMINAL(symbol)) {
        if (index_of[symbol] == OMEGA) {
          compute_first(symbol, dss);
        }
        index_of[nt] = MIN(index_of[nt], index_of[symbol]);
        ASSIGN_SET(temp_set, 0, nt_first, symbol);
        RESET_BIT(temp_set, empty);
        SET_UNION(nt_first, nt, temp_set, 0);
        blocked = !null_nt[symbol];
      } else {
        SET_BIT_IN(nt_first, nt, symbol);
        blocked = true;
      }
      if (blocked)
        break;
    }
    if (!blocked) {
      SET_BIT_IN(nt_first, nt, empty);
    }
  }
  if (index_of[nt] == indx) {
    for (int symbol = stack[top]; symbol != nt; symbol = stack[--top]) {
      ASSIGN_SET(nt_first, symbol, nt_first, nt);
      index_of[symbol] = INFINITY;
    }
    index_of[nt] = INFINITY;
    top--;
  }
  ffree(temp_set.raw);
}

///  FIRST_MAP takes as arguments two pointers, ROOT and TAIL, to a sequence
/// of symbols in RHS which it inserts in FIRST_TABLE.  The vector FIRST_TABLE
/// is used as the base for a hashed table where collisions are resolved by
/// links.  Elements added to this hash table are allocated from the vector
/// FIRST_ELEMENT, with the variable TOP always indicating the position of the
/// last element in FIRST_ELEMENT that was allocated.
/// NOTE: The suffix indentified by ROOT and TAIL is presumed not to be empty.
///       That is, ROOT <= TAIL !!!
short first_map(const int root, const int tail) {
  for (int i = first_table[rhs_sym[root]]; i != NIL; i = first_element[i].link) {
    int k;
    int jj;
    for (jj = root + 1, k = first_element[i].suffix_root + 1; jj <= tail && k <= first_element[i].suffix_tail; jj++, k++) {
      if (rhs_sym[jj] != rhs_sym[k])
        break;
    }
    if (jj > tail && k > first_element[i].suffix_tail) {
      return i;
    }
  }
  top++;
  first_element[top].suffix_root = root;
  first_element[top].suffix_tail = tail;
  first_element[top].link = first_table[rhs_sym[root]];
  first_table[rhs_sym[root]] = top;
  return top;
}

/// COMPUTE_FOLLOW computes FOLLOW[nt] for some non-terminal NT using the
/// digraph algorithm.  FOLLOW[NT] is the set of all terminals Ti that
/// may immediately follow a string X generated by NT. I.e., if NT *::= X
/// then X Ti is a valid substring of a class of strings that may be
/// recognized by the language.
void compute_follow(const int nt, struct DetectedSetSizes* dss) {
  JBitset temp_set;
  calloc0_set(temp_set, 1, dss->term_set_size);
  // FOLLOW[NT] was initialized to 0 for all non-terminals.
  stack[++top] = nt;
  const int indx = top;
  index_of[nt] = indx;
  for (int item_no = nt_items[nt]; item_no != NIL; item_no = next_item[item_no]) {
    // iterate over all items of NT
    ASSIGN_SET(temp_set, 0, first, item_table[item_no].suffix_index);
    if (IS_ELEMENT(temp_set, empty)) {
      RESET_BIT(temp_set, empty);
      const int rule_no = item_table[item_no].rule_number;
      int lhs_symbol = rules[rule_no].lhs;
      if (index_of[lhs_symbol] == OMEGA) {
        compute_follow(lhs_symbol, dss);
      }
      SET_UNION(follow, nt, follow, lhs_symbol);
      index_of[nt] = MIN(index_of[nt], index_of[lhs_symbol]);
    }
    SET_UNION(follow, nt, temp_set, 0);
  }
  if (index_of[nt] == indx) {
    for (int lhs_symbol = stack[top]; lhs_symbol != nt; lhs_symbol = stack[--top]) {
      ASSIGN_SET(follow, lhs_symbol, follow, nt);
      index_of[lhs_symbol] = INFINITY;
    }
    index_of[nt] = INFINITY;
    top--;
  }
  ffree(temp_set.raw);
}

/// MKFIRST constructs the FIRST and FOLLOW maps, the CLOSURE map,
/// ADEQUATE_ITEM and ITEM_TABLE maps and all other basic maps.
struct DetectedSetSizes mkbasic(struct CLIOptions* cli_options) {
  struct DetectedSetSizes dss = {
    .non_term_set_size = num_non_terminals / SIZEOF_BC + (num_non_terminals % SIZEOF_BC ? 1 : 0),
    .term_set_size = num_terminals / SIZEOF_BC + (num_terminals % SIZEOF_BC ? 1 : 0),
  };
  // allocate various arrays
  lhs_rule = Allocate_short_array(num_non_terminals);
  lhs_rule -= num_terminals + 1;
  next_rule = Allocate_short_array(NEXT_RULE_SIZE());
  first_item_of = Allocate_short_array(NEXT_RULE_SIZE());
  stack = Allocate_short_array(num_non_terminals + 1);
  index_of = Allocate_short_array(num_non_terminals);
  index_of -= num_terminals + 1;
  // NT_FIRST is used to construct a mapping from non-terminals to the
  // set of terminals that may appear first in a string derived from
  // the non-terminal.
  calloc0_set(nt_first, num_non_terminals, dss.term_set_size);
  nt_first.raw -= (num_terminals + 1) * dss.term_set_size;
  next_item = Allocate_short_array(num_items + 1);
  nt_items = Allocate_short_array(num_non_terminals);
  nt_items -= num_terminals + 1;
  nt_list = Allocate_short_array(num_non_terminals);
  nt_list -= num_terminals + 1;
  calloc0(first_element, num_items + 1, struct f_element_type);
  calloc0(item_table, num_items + 1, struct itemtab);
  for ALL_NON_TERMINALS3(symbol) {
    lhs_rule[symbol] = NIL;
  }
  // In this loop, we construct the LHS_RULE map which maps
  // each non-terminal symbol into the set of rules it produces
  for ALL_RULES3(rule_no) {
    int symbol = rules[rule_no].lhs;
    if (lhs_rule[symbol] == NIL) {
      next_rule[rule_no] = rule_no;
    } else {
      next_rule[rule_no] = next_rule[lhs_rule[symbol]];
      next_rule[lhs_rule[symbol]] = rule_no;
    }
    lhs_rule[symbol] = rule_no;
  }
  // Check if there are any non-terminals that do not produce
  // any rules.
  no_rules_produced(cli_options);
  // Construct the CLOSURE map of non-terminals.
  calloc0(closure, num_non_terminals, struct node *);
  closure -= num_terminals + 1;
  for ALL_NON_TERMINALS3(symbol) {
    index_of[symbol] = OMEGA;
  }
  top = 0;
  for ALL_NON_TERMINALS3(nt) {
    if (index_of[nt] == OMEGA) {
      compute_closure(nt);
    }
  }
  // Construct the NULL_NT map for non-terminals.
  // A non-terminal B is said to be nullable if either:
  //    B -> %empty  or  B -> B1 B2 B3 ... Bk  where Bi is
  //                         nullable for 1 <= i <= k
  null_nt = Allocate_boolean_array(num_non_terminals);
  null_nt -= num_terminals + 1;
  // Calculate nullables
  {
    ///   This procedure computes the set of non-terminal symbols that can
    /// generate the empty string.  Such non-terminals are said to be nullable.
    ///
    /// A non-terminal "A" can generate empty if the grammar in question contains
    /// a rule:
    ///          A ::= B1 B2 ... Bn     n >= 0,  1 <= i <= n
    /// and Bi, for all i, is a nullable non-terminal.
    bool changed = true;
    short *rhs_start = Allocate_short_array(NEXT_RULE_SIZE());
    // First, mark all non-terminals as non-nullable.  Then initialize
    // RHS_START. RHS_START is a mapping from each rule in the grammar
    // into the next symbol in its right-hand side that has not yet
    // proven to be nullable.
    for ALL_NON_TERMINALS3(nt) {
      null_nt[nt] = false;
    }
    for ALL_RULES3(rule_no) {
      rhs_start[rule_no] = rules[rule_no].rhs;
    }
    // We now iterate over the rules and try to advance the RHS_START
    // pointer thru each right-hand side as far as we can.  If one or
    // more non-terminals are found to be nullable, they are marked
    // as such and the process is repeated.
    //
    // If we go through all the rules and no new non-terminal is found
    // to be nullable then we stop and return.
    //
    // Note that for each iteration, only rules associated with
    // non-terminals that are non-nullable are considered.  Further,
    // as soon as a non-terminal is found to be nullable, the
    // remaining rules associated with it are not considered.  I.e.,
    // we quit the inner loop.
    while (changed) {
      changed = false;
      for ALL_NON_TERMINALS3(nt) {
        int rule_no;
        for (bool end_node = (rule_no = lhs_rule[nt]) == NIL; !null_nt[nt] && !end_node; end_node = rule_no == lhs_rule[nt]) {
          rule_no = next_rule[rule_no];
          if (is_nullable_rhs(rhs_start, rule_no)) {
            changed = true;
            null_nt[nt] = true;
          }
        }
      }
    }
    ffree(rhs_start);
  }
  // Construct the FIRST map for non-terminals and also a list
  // of non-terminals whose first set is empty.
  for ALL_NON_TERMINALS3(symbol) {
    index_of[symbol] = OMEGA;
  }
  top = 0;
  for ALL_NON_TERMINALS3(nt) {
    if (index_of[nt] == OMEGA) {
      compute_first(nt, &dss);
    }
  }
  //  Since every input source will be followed by the EOFT
  //  symbol, FIRST[accept_image] cannot contain empty but
  //  instead must contain the EOFT symbol.
  if (null_nt[accept_image]) {
    null_nt[accept_image] = false;
    RESET_BIT_IN(nt_first, accept_image, empty);
    SET_BIT_IN(nt_first, accept_image, eoft_image);
  }
  // Check whether there are any non-terminals that do not
  // generate any terminal strings. If so, signal error and stop.
  check_non_terminals(cli_options);
  // Construct the ITEM_TABLE, FIRST_ITEM_OF, and NT_ITEMS maps.
  first_table = Allocate_short_array(num_symbols + 1);
  /* Initialize FIRST_TABLE to NIL */
  for ALL_SYMBOLS3(symbol) {
    first_table[symbol] = NIL;
  }
  top = 1;
  const int first_of_empty = top;
  first_element[first_of_empty].suffix_root = 1;
  first_element[first_of_empty].suffix_tail = 0;
  for ALL_NON_TERMINALS3(symbol) {
    nt_items[symbol] = NIL;
  }
  int item_no = 0;
  item_table[item_no].rule_number = 0;
  item_table[item_no].symbol = empty;
  item_table[item_no].dot = 0;
  item_table[item_no].suffix_index = NIL;
  for ALL_RULES3(rule_no) {
    first_item_of[rule_no] = item_no + 1;
    int j = 0;
    const int k = LAST_RHS_INDEX(rule_no);
    for ENTIRE_RHS3(i, rule_no) {
      item_no++;
      int symbol = rhs_sym[i];
      item_table[item_no].rule_number = rule_no;
      item_table[item_no].symbol = symbol;
      item_table[item_no].dot = j;
      if (cli_options->lalr_level > 1 ||
          IS_A_NON_TERMINAL(symbol) ||
          symbol == error_image) {
        if (i == k) {
          item_table[item_no].suffix_index = first_of_empty;
        } else {
          item_table[item_no].suffix_index = first_map(i + 1, k);
        }
      } else {
        item_table[item_no].suffix_index = NIL;
      }
      if (IS_A_NON_TERMINAL(symbol)) {
        next_item[item_no] = nt_items[symbol];
        nt_items[symbol] = item_no;
      }
      j++;
    }
    item_table[++item_no].rule_number = rule_no;
    item_table[item_no].symbol = empty;
    item_table[item_no].dot = j;
    item_table[item_no].suffix_index = NIL;
  }
  // We now compute the first set for all suffixes that were
  // inserted in the FIRST_TABLE map. There are TOP such suffixes
  // Extra space is also allocated to compute the first set for
  // suffixes whose left-hand side is the ACCEPT non-terminal.
  // The first set for these suffixes are the sets needed to
  // construct the FOLLOW map and compute look-ahead sets.  They
  // are placed in the FIRST table in the range 1..NUM_FIRST_SETS
  // The first element in the FIRST table contains the first sets
  // for the empty sequence.
  num_first_sets = top;
  int rule_no;
  for (bool end_node = (rule_no = lhs_rule[accept_image]) == NIL; !end_node; end_node = rule_no == lhs_rule[accept_image]) {
    rule_no = next_rule[rule_no];
    num_first_sets++;
  }
  calloc0_set(first, num_first_sets + 1, dss.term_set_size)
  for (int i = 1; i <= top; i++) {
    int root = first_element[i].suffix_root;
    int tail = first_element[i].suffix_tail;
    int index = i;
    // S_FIRST takes as argument, two pointers: ROOT and TAIL to a sequence of
    // symbols in the vector RHS, and INDEX which is the index of a first set.
    // It computes the set of all terminals that can appear as the first symbol
    // in the sequence and places the result in the FIRST set indexable by INDEX.
    int symbol = root > tail ? empty : rhs_sym[root];
    if (IS_A_TERMINAL(symbol)) {
      INIT_BITSET(first, index);
      SET_BIT_IN(first, index, symbol); /* add it to set */
    } else {
      ASSIGN_SET(first, index, nt_first, symbol);
    }
    for (int i = root + 1; i <= tail && IS_IN_SET(first, index, empty); i++) {
      symbol = rhs_sym[i];
      RESET_BIT_IN(first, index, empty); /* remove EMPTY */
      if (IS_A_TERMINAL(symbol)) {
        SET_BIT_IN(first, index, symbol); /* add it to set */
      } else {
        SET_UNION(first, index, nt_first, symbol);
      }
    }
  }
  rule_no = lhs_rule[accept_image];
  for (int i = top + 1; i <= num_first_sets; i++) {
    rule_no = next_rule[rule_no];
    item_no = first_item_of[rule_no];
    item_table[item_no].suffix_index = i;
    INIT_BITSET(first, i);
    SET_BIT_IN(first, i, eoft_image);
  }
  // If the READ/REDUCE option is on, we precalculate the kernel
  // of the final states which simply consists of the last item
  // in  the corresponding rule.  Rules with the ACCEPT
  // non-terminal as their left-hand side are not considered
  // to let the Accept action remain as a Reduce action
  // instead of a Goto/Reduce action.
  calloc0(adequate_item, num_rules + 1, struct node *);
  if (cli_options->read_reduce_bit) {
    for ALL_RULES3(rule_no) {
      const int j = RHS_SIZE(rule_no);
      if (rules[rule_no].lhs != accept_image && j > 0) {
        item_no = first_item_of[rule_no] + j;
        struct node *p = Allocate_node();
        p->value = item_no;
        p->next = NULL;
        adequate_item[rule_no] = p;
      } else
        adequate_item[rule_no] = NULL;
    }
  }
  // Construct the CLITEMS map. Each element of CLITEMS points
  // to a circular linked list of items.
  calloc0(clitems, num_non_terminals, struct node *);
  clitems -= num_terminals + 1;
  for ALL_NON_TERMINALS3(nt) {
    clitems[nt] = NULL;
    for (bool end_node = (rule_no = lhs_rule[nt]) == NIL; !end_node; end_node = rule_no == lhs_rule[nt]) {
      rule_no = next_rule[rule_no];
      struct node *p = Allocate_node();
      p->value = first_item_of[rule_no];
      if (clitems[nt] == NULL) {
        p->next = p;
      } else {
        p->next = clitems[nt]->next;
        clitems[nt]->next = p;
      }
      clitems[nt] = p;
    }
  }

  // If LALR_LEVEL > 1, we need to calculate RMPSELF, a set that
  // identifies the nonterminals that can right-most produce
  // themselves. In order to compute RMPSELF, the map PRODUCES
  // must be constructed which identifies for each nonterminal
  // the set of nonterminals that it can right-most produce.
  if (cli_options->lalr_level > 1) {
    calloc0_set(produces, num_non_terminals, dss.non_term_set_size);
    produces.raw -= (num_terminals + 1) * dss.non_term_set_size;
    calloc0(direct_produces, num_non_terminals, struct node *);
    direct_produces -= num_terminals + 1;
    for ALL_NON_TERMINALS3(nt) {
      struct node *p;
      for (bool end_node = (p = clitems[nt]) == NULL; !end_node; end_node = p == clitems[nt]) {
        p = p->next;
        item_no = p->value;
        int symbol = item_table[item_no].symbol;
        if (IS_A_NON_TERMINAL(symbol)) {
          const int i = item_table[item_no].suffix_index;
          if (IS_IN_SET(first, i, empty) && !IS_IN_SET(produces, nt, symbol - num_terminals)) {
            SET_BIT_IN(produces, nt, symbol - num_terminals);
            struct node *q = Allocate_node();
            q->value = symbol;
            q->next = direct_produces[nt];
            direct_produces[nt] = q;
          }
        }
      }
    }
    // Complete the construction of the RIGHT_MOST_PRODUCES map
    // for non-terminals using the digraph algorithm.
    for ALL_NON_TERMINALS3(nt) {
      index_of[nt] = OMEGA;
    }
    top = 0;
    for ALL_NON_TERMINALS3(nt) {
      if (index_of[nt] == OMEGA) {
        compute_produces(nt);
      }
    }
    init_rmpself(produces);
    produces.raw += (num_terminals + 1) * dss.non_term_set_size;
    ffree(produces.raw);
    direct_produces += num_terminals + 1;
    ffree(direct_produces);
  }

  // Construct the FOLLOW map if
  //   - If we have to print the FOLLOW map
  //   - Error-maps are requested
  //   - There are more than one starting symbol.
  if (cli_options->follow_bit || error_maps_bit || next_rule[lhs_rule[accept_image]] != lhs_rule[accept_image]) {
    calloc0_set(follow, num_non_terminals, dss.term_set_size);
    follow.raw -= (num_terminals + 1) * dss.term_set_size;
    SET_BIT_IN(follow, accept_image, eoft_image);
    for ALL_NON_TERMINALS3(symbol) {
      index_of[symbol] = OMEGA;
    }
    index_of[accept_image] = INFINITY; /* mark computed */
    top = 0;
    for ALL_NON_TERMINALS3(nt) {
      if (index_of[nt] == OMEGA) {
        // not yet computed ?
        compute_follow(nt, &dss);
      }
    }
    //  Initialize FIRST for suffixes that can follow each starting
    // non-terminal ( except the main symbol) with the FOLLOW set
    // of the non-terminal in question.
    rule_no = lhs_rule[accept_image];
    if (next_rule[rule_no] != rule_no) {
      rule_no = next_rule[rule_no]; /* first rule */
      top = item_table[first_item_of[rule_no]].suffix_index;
      for (int i = top + 1; i <= num_first_sets; i++) {
        rule_no = next_rule[rule_no];
        item_no = first_item_of[rule_no];
        int symbol = item_table[item_no].symbol;
        if (IS_A_NON_TERMINAL(symbol)) {
          ASSIGN_SET(first, i, follow, symbol);
        }
      }
    }
  }
  // The unreachable symbols in the grammar are printed.

  // Print unreachables.
  {
    char line[PRINT_LINE_SIZE + 1];
    char tok[SYMBOL_SIZE + 1];
    // SYMBOL_LIST is used for two purposes:
    //  1) to mark symbols that are reachable from the Accepting
    //        non-terminal.
    //  2) to construct lists of symbols that are not reachable.
    long *symbol_list = Allocate_long_array(num_symbols + 1);
    for ALL_SYMBOLS3(symbol) {
      symbol_list[symbol] = OMEGA;
    }
    symbol_list[eoft_image] = NIL;
    symbol_list[empty] = NIL;
    if (error_maps_bit) {
      symbol_list[error_image] = NIL;
    }
    // Initialize a list consisting only of the Accept non-terminal.
    // This list is a work pile of non-terminals to process as follows:
    // Each non-terminal in the front of the list is removed in turn and
    // 1) All terminal symbols in one of its right-hand sides are
    //     marked reachable.
    // 2) All non-terminals in one of its right-hand sides are placed
    //     in the work pile of it had not been processed previously
    int nt_root = accept_image;
    symbol_list[nt_root] = NIL;
    for (int nt = nt_root; nt != NIL; nt = nt_root) {
      nt_root = symbol_list[nt];
      int rule_no;
      for (bool end_node = (rule_no = lhs_rule[nt]) == NIL; !end_node; end_node = rule_no == lhs_rule[nt]) {
        rule_no = next_rule[rule_no];
        for ENTIRE_RHS3(i, rule_no) {
          const int symbol = rhs_sym[i];
          if (IS_A_TERMINAL(symbol)) {
            symbol_list[symbol] = NIL;
          } else if (symbol_list[symbol] == OMEGA) {
            symbol_list[symbol] = nt_root;
            nt_root = symbol;
          }
        }
      }
    }
    // We now iterate (backwards to keep things in order) over the
    // terminal symbols, and place each unreachable terminal in a
    // list. If the list is not empty, we signal that these symbols
    // are unused.
    int t_root = NIL;
    for ALL_TERMINALS_BACKWARDS3(symbol) {
      if (symbol_list[symbol] == OMEGA) {
        symbol_list[symbol] = t_root;
        t_root = symbol;
      }
    }
    if (t_root != NIL) {
      if (symbol_list[t_root] != NIL) {
        PRNT("*** The following Terminals are useless: ");
        fprintf(syslis, "\n\n");
        strcpy(line, "        "); /* 8 spaces */
      } else {
        strcpy(line, "*** The following Terminal is useless: ");
      }
      for (int symbol = t_root; symbol != NIL; symbol = symbol_list[symbol]) {
        restore_symbol(tok, RETRIEVE_STRING(symbol), cli_options->ormark, cli_options->escape);
        if (strlen(line) + strlen(tok) > PRINT_LINE_SIZE) {
          PRNT(line);
          print_large_token(line, tok, "    ", LEN);
        } else {
          strcat(line, tok);
          strcat(line, " ");
        }
        strcat(line, " ");
      }
      PRNT(line);
    }
    // We now iterate (backward to keep things in order) over the
    // non-terminals, and place each unreachable non-terminal in a
    // list.  If the list is not empty, we signal that these
    // symbols are unused.
    nt_root = NIL;
    for ALL_NON_TERMINALS_BACKWARDS3(symbol) {
      if (symbol_list[symbol] == OMEGA) {
        symbol_list[symbol] = nt_root;
        nt_root = symbol;
      }
    }
    if (nt_root != NIL) {
      if (symbol_list[nt_root] != NIL) {
        PRNT("*** The following Non-Terminals are useless: ");
        fprintf(syslis, "\n\n");
        strcpy(line, "        "); /* 8 spaces */
      } else {
        strcpy(line, "*** The following Non-Terminal is useless: ");
      }
      for (int symbol = nt_root; symbol != NIL; symbol = symbol_list[symbol]) {
        restore_symbol(tok, RETRIEVE_STRING(symbol), cli_options->ormark, cli_options->escape);
        if (strlen(line) + strlen(tok) > PRINT_LINE_SIZE) {
          PRNT(line);
          print_large_token(line, tok, "    ", LEN);
        } else {
          strcat(line, tok);
        }
        strcat(line, " ");
      }
      PRNT(line);
    }
    ffree(symbol_list);
  }
  // If a listing of the FIRST map is requested, it is generated here.
  if (cli_options->first_bit) {
    // Print first sets.
    {
      fprintf(syslis, "\nFirst map for non-terminals:\n\n");
      for ALL_NON_TERMINALS3(nt) {
        char tok[SYMBOL_SIZE + 1];
        char line[PRINT_LINE_SIZE + 1];
        restore_symbol(tok, RETRIEVE_STRING(nt), cli_options->ormark, cli_options->escape);
        print_large_token(line, tok, "", PRINT_LINE_SIZE - 7);
        strcat(line, "  ==>> ");
        for ALL_TERMINALS3(t) {
          if (IS_IN_SET(nt_first, nt, t)) {
            restore_symbol(tok, RETRIEVE_STRING(t), cli_options->ormark, cli_options->escape);
            if (strlen(line) + strlen(tok) > PRINT_LINE_SIZE - 1) {
              fprintf(syslis, "\n%s", line);
              print_large_token(line, tok, "    ", LEN);
            } else {
              strcat(line, tok);
            }
            strcat(line, " ");
          }
        }
        fprintf(syslis, "\n%s\n", line);
      }
    }
  }

  // If a listing of the FOLLOW map is requested, it is generated here.
  if (cli_options->follow_bit) {
    // Print follow sets.
    {
      fprintf(syslis, "\nFollow Map:\n\n");
      for ALL_NON_TERMINALS3(nt) {
        char tok[SYMBOL_SIZE + 1];
        char line[PRINT_LINE_SIZE + 1];
        restore_symbol(tok, RETRIEVE_STRING(nt), cli_options->ormark, cli_options->escape);
        print_large_token(line, tok, "", PRINT_LINE_SIZE - 7);
        strcat(line, "  ==>> ");
        for ALL_TERMINALS3(t) {
          if (IS_IN_SET(follow, nt, t)) {
            restore_symbol(tok, RETRIEVE_STRING(t), cli_options->ormark, cli_options->escape);
            if (strlen(line) + strlen(tok) > PRINT_LINE_SIZE - 2) {
              fprintf(syslis, "\n%s", line);
              print_large_token(line, tok, "    ", LEN);
            } else {
              strcat(line, tok);
            }
            strcat(line, " ");
          }
        }
        fprintf(syslis, "\n%s\n", line);
      }
    }
  }

  // Free allocated arrays.
  nt_first.raw += (num_terminals + 1) * dss.term_set_size;
  ffree(nt_first.raw);
  nt_list += num_terminals + 1;
  ffree(nt_list);
  ffree(first_table);
  ffree(first_element);
  nt_items += num_terminals + 1;
  ffree(nt_items);
  ffree(next_item);
  ffree(stack);
  index_of += num_terminals + 1;
  ffree(index_of);
  lhs_rule += num_terminals + 1;
  ffree(lhs_rule);
  ffree(next_rule);
  ffree(first_item_of);
  return dss;
}
