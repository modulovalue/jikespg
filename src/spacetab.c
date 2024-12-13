static char hostfile[] = __FILE__;

#include <stdlib.h>
#include "lpgparse.h"
#include <string.h>
#include "common.h"

struct node **new_state_element_reduce_nodes;

long total_bytes;
long num_table_entries;

int top;
int empty_root;
int single_root;
int multi_root;

long *row_size;
long *frequency_symbol;
long *frequency_count;

bool *shift_on_error_symbol;

///  REMAP_NON_TERMINALS remaps the non-terminal symbols and states based on
/// frequency of entries.
void remap_non_terminals(const struct CLIOptions *cli_options) {
  //   The variable FREQUENCY_SYMBOL is used to hold the non-terminals
  // in the grammar, and  FREQUENCY_COUNT is used correspondingly to
  // hold the number of actions defined on each non-terminal.
  // ORDERED_STATE and ROW_SIZE are used in a similar fashion for states
  long *frequency_symbol = Allocate_long_array(num_non_terminals);
  frequency_symbol -= num_terminals + 1;
  long *frequency_count = Allocate_long_array(num_non_terminals);
  frequency_count -= num_terminals + 1;
  long *row_size = Allocate_long_array(num_states + 1);
  for ALL_NON_TERMINALS3(i) {
    frequency_symbol[i] = i;
    frequency_count[i] = 0;
  }
  for ALL_STATES3(state_no) {
    ordered_state[state_no] = state_no;
    row_size[state_no] = 0;
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      row_size[state_no]++;
      const int symbol = go_to.map[i].symbol;
      frequency_count[symbol]++;
    }
  }
  // The non-terminals are sorted in descending order based on the
  // number of actions defined on then, and they are remapped based on
  // the new arrangement obtained by the sorting.
  sortdes(frequency_symbol, frequency_count, num_terminals + 1, num_symbols, num_states);
  for ALL_NON_TERMINALS3(i) {
    symbol_map[frequency_symbol[i]] = i;
  }
  //    All non-terminal entries in the state automaton are updated
  // accordingly.  We further subtract NUM_TERMINALS from each
  // non-terminal to make them fall in the range [1..NUM_NON_TERMINLS]
  // instead of [NUM_TERMINALS+1..NUM_SYMBOLS].
  for ALL_STATES3(state_no) {
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      go_to.map[i].symbol = symbol_map[go_to.map[i].symbol] - num_terminals;
    }
  }
  // If Goto-Default was requested, we find out how many non-terminals
  // were eliminated as a result, and adjust the GOTO-DEFAULT map,
  // based on the new mapping of the non-terminals.
  if (cli_options->goto_default_bit) {
    long *temp_goto_default = Allocate_long_array(num_non_terminals);
    temp_goto_default -= num_terminals + 1;
    for (last_symbol = num_symbols; last_symbol > num_terminals; last_symbol--) {
      if (frequency_count[last_symbol] != 0) {
        break;
      }
    }
    last_symbol -= num_terminals;
    PRNT3("Number of non-terminals eliminated: %ld", num_non_terminals - last_symbol);
    // Remap the GOTO-DEFAULT map.
    // to hold the original map.
    for ALL_NON_TERMINALS3(symbol) {
      temp_goto_default[symbol_map[symbol]] = gotodef[symbol];
    }
    gotodef += num_terminals + 1;
    ffree(gotodef);
    gotodef = temp_goto_default;
  } else {
    last_symbol = num_non_terminals;
  }
  // The states are sorted in descending order based on the number of
  // actions defined on them, and they are remapped based on the new
  // arrangement obtained by the sorting.
  sortdes(ordered_state, row_size, 1, num_states, last_symbol);
  frequency_symbol += num_terminals + 1;
  ffree(frequency_symbol);
  frequency_count += num_terminals + 1;
  ffree(frequency_count);
  ffree(row_size);
}

/// We now overlap the non-terminal table, or more precisely, we compute the
/// starting position in a vector where each of its rows may be placed
/// without clobbering elements in another row.  The starting positions are
/// stored in the vector STATE_INDEX.
void overlap_nt_rows(struct CLIOptions *cli_options) {
  num_table_entries = num_gotos + num_goto_reduces + num_states;
  increment_size = MAX(num_table_entries / 100 * increment, last_symbol + 1);
  table_size = MIN(num_table_entries + increment_size, MAX_TABLE_SIZE);
  // Allocate space for table, and initlaize the AVAIL_POOL list.  The
  // variable FIRST_INDEX keeps track of the first element in the doubly-
  // linked list, and LAST_ELEMENT keeps track of the last element in the
  // list.
  //   The variable MAX_INDX is used to keep track of the maximum starting
  // position for a row that has been used.
  next = Allocate_long_array(table_size + 1);
  previous = Allocate_long_array(table_size + 1);
  first_index = 1;
  previous[first_index] = NIL;
  next[first_index] = first_index + 1;
  for (long indx = 2; indx < (int) table_size; indx++) {
    next[indx] = indx + 1;
    previous[indx] = indx - 1;
  }
  last_index = table_size;
  previous[last_index] = last_index - 1;
  next[last_index] = NIL;
  int max_indx = first_index;
  // We now iterate over all the states in their new sorted order as
  // indicated by the variable STATE_NO, and determine an "overlap"
  // position for them.
  for ALL_STATES3(state_no) {
    const int state_no__ = ordered_state[state_no];
    // INDX is set to the beginning of the list of available slots
    // and we try to determine if it might be a valid starting
    // position.  If not, INDX is moved to the next element, and we
    // repeat the process until a valid position is found.
    const struct goto_header_type go_to = statset[state_no__].go_to;
    long indx = first_index;
  look_for_match_in_base_table:
    if (indx == NIL) {
      indx = table_size + 1;
    }
    if (indx + last_symbol > table_size) {
      reallocate(cli_options);
    }
    for (int i = 1; i <= go_to.size; i++) {
      if (next[indx + go_to.map[i].symbol] == OMEGA) {
        indx = next[indx];
        goto look_for_match_in_base_table;
      }
    }
    // At this stage, a position(INDX), was found to overlay the row
    // in question.  Remove elements associated with all positions
    // that will be taken by row from the doubly-linked list.
    // NOTE tha since SYMBOLs start at 1, the first index can never
    // be a candidate (==> I = INDX + SYMBOL) in this loop.
    for (int j = 1; j <= go_to.size; j++) {
      const int symbol = go_to.map[j].symbol;
      int i = indx + symbol;
      if (i == last_index) {
        last_index = previous[last_index];
        next[last_index] = NIL;
      } else {
        next[previous[i]] = next[i];
        previous[next[i]] = previous[i];
      }
      next[i] = OMEGA;
    }
    if (first_index == last_index) {
      first_index = NIL;
    } else if (indx == first_index) {
      first_index = next[first_index];
      previous[first_index] = NIL;
    } else if (indx == last_index) {
      last_index = previous[last_index];
      next[last_index] = NIL;
    } else {
      next[previous[indx]] = next[indx];
      previous[next[indx]] = previous[indx];
    }
    next[indx] = OMEGA;
    if (indx > max_indx) {
      max_indx = indx;
    }
    state_index[state_no__] = indx;
  }
  if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
    check_size = max_indx + num_non_terminals;
  } else {
    check_size = 0;
  }
  for (action_size = max_indx + last_symbol;
       action_size >= max_indx; action_size--) {
    if (next[action_size] == OMEGA) {
      break;
    }
  }
  accept_act = max_indx + num_rules + 1;
  error_act = accept_act + 1;
  printf("\n");
  if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
    PRNT3("Length of base Check Table: %d", check_size);
  }
  PRNT3("Length of base Action Table: %ld", action_size);
  PRNT3("Number of entries in base Action Table: %ld", num_table_entries);
  const int percentage = (action_size - num_table_entries) * 1000 / num_table_entries;
  PRNT3("Percentage of increase: %d.%d%%", percentage / 10, percentage % 10);
  long num_bytes;
  if (cli_options->byte_bit) {
    num_bytes = 2 * action_size + check_size;
    if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
      if (last_symbol > 255) {
        num_bytes += check_size;
      }
    }
  } else {
    num_bytes = 2 * (action_size + check_size);
  }
  if (cli_options->goto_default_bit) {
    num_bytes += (long) 2 * num_non_terminals;
  }
  total_bytes = num_bytes;
  const int k_bytes = num_bytes / 1024 + 1;
  PRNT3("Storage required for base Tables: %ld Bytes, %dK", num_bytes, k_bytes);
  num_bytes = (long) 4 * num_rules;
  if (cli_options->byte_bit) {
    num_bytes -= num_rules;
    if (num_non_terminals < 256)
      num_bytes -= num_rules;
  }
  PRNT3("Storage required for Rules: %ld Bytes", num_bytes);
}

/// We now try to merge states in the terminal table that are similar.
/// Two states S1 and S2 are said to be similar if they contain the
/// same shift actions, and they reduce to the same set of rules.  In
/// addition,  there must not exist a terminal symbol "t" such that:
/// REDUCE(S1, t) and REDUCE(S2, t) are defined, and
/// REDUCE(S1, t) ^= REDUCE(S2, t)
void merge_similar_t_rows(const struct CLIOptions *cli_options) {
  short *table = Allocate_short_array(num_shift_maps + 1);
  empty_root = NIL;
  single_root = NIL;
  multi_root = NIL;
  top = 0;
  for (int i = 1; i <= max_la_state; i++) {
    shift_on_error_symbol[i] = false;
  }
  for (int i = 0; i <= num_shift_maps; i++) {
    table[i] = NIL;
  }
  // We now hash all the states into TABLE, based on their shift map
  // number.
  // The rules in the range of the REDUCE MAP are placed in sorted
  // order in a linear linked list headed by REDUCE_ROOT.
  for (int state_no = 1; state_no <= max_la_state; state_no++) {
    struct node *reduce_root = NULL;
    struct reduce_header_type red;
    if (state_no > num_states) {
      red = lastats[state_no].reduce;
    } else {
      red = reduce[state_no];
    }
    for (int i = 1; i <= red.size; i++) {
      const int rule_no = red.map[i].rule_number;
      struct node *q;
      struct node *tail;
      for (q = reduce_root; q != NULL; tail = q, q = q->next) {
        // Is it or not in REDUCE_ROOT list?
        if (q->value == rule_no)
          goto continue_traverse_reduce_map;
        if (q->value > rule_no)
          break;
      }
      struct node *r = Allocate_node(); /* Add element to REDUCE_ROOT list */
      r->value = rule_no;
      if (q == reduce_root) {
        r->next = reduce_root;
        reduce_root = r;
      } else {
        r->next = q;
        tail->next = r;
      }
    continue_traverse_reduce_map:;
    }
    //   We compute the HASH_ADDRESS,  mark if the state has a shift
    // action on the ERROR symbol, and search the hash TABLE to see if a
    // state matching the description is already in there.
    unsigned long hash_address;
    if (state_no > num_states) {
      hash_address = lastats[state_no].shift_number;
    } else {
      if (cli_options->default_opt == 5) {
        const struct shift_header_type sh = shift[statset[state_no].shift_number];
        for (int j = 1; j <= sh.size && !shift_on_error_symbol[state_no]; j++)
          shift_on_error_symbol[state_no] = sh.map[j].symbol == error_image;
      }
      hash_address = statset[state_no].shift_number;
    }
    int ii;
    for (ii = table[hash_address]; ii != NIL; ii = new_state_element[ii].link) {
      struct node *q;
      struct node *r;
      for (r = reduce_root, q = new_state_element_reduce_nodes[ii];
           r != NULL && q != NULL;
           r = r->next, q = q->next) {
        if (r->value != q->value) {
          break;
        }
      }
      if (r == q) {
        break;
      }
    }
    // If the state is a new state to be inserted in the table, we now
    // do so,  and place it in the proper category based on its reduces,
    // In any case, the IMAGE field is updated, and so is the relevant
    // STATE_LIST element.
    //
    // If the state contains a shift action on the error symbol and also
    // contains reduce actions,  we allocate a new element for it and
    // place it in the list headed by MULTI_ROOT.  Such states are not
    // merged, because we do not take default reductions in them.
    if (shift_on_error_symbol[state_no] && reduce_root != NULL) {
      top++;
      if (ii == NIL) {
        new_state_element[top].link = table[hash_address];
        table[hash_address] = top;
      }
      new_state_element[top].thread = multi_root;
      multi_root = top;
      new_state_element[top].shift_number = hash_address;
      new_state_element_reduce_nodes[top] = reduce_root;
      state_list[state_no] = NIL;
      new_state_element[top].image = state_no;
    } else if (ii == NIL) {
      top++;
      new_state_element[top].link = table[hash_address];
      table[hash_address] = top;
      if (reduce_root == NULL) {
        new_state_element[top].thread = empty_root;
        empty_root = top;
      } else if (reduce_root->next == NULL) {
        new_state_element[top].thread = single_root;
        single_root = top;
      } else {
        new_state_element[top].thread = multi_root;
        multi_root = top;
      }
      new_state_element[top].shift_number = hash_address;
      new_state_element_reduce_nodes[top] = reduce_root;
      state_list[state_no] = NIL;
      new_state_element[top].image = state_no;
    } else {
      state_list[state_no] = new_state_element[ii].image;
      new_state_element[ii].image = state_no;
      struct node *tail;
      for (struct node *r = reduce_root; r != NULL; tail = r, r = r->next) {
      }
      if (reduce_root != NULL) {
        free_nodes(reduce_root, tail);
      }
    }
  }
  ffree(table);
}

///    If shift-default actions are requested, the shift actions
/// associated with each state are factored out of the Action matrix
/// and all identical rows are merged.  This merged matrix is used to
/// create a boolean vector that may be used to confirm whether
/// there is a shift action in a given state S on a given symbol t.
/// If we can determine that there is a shift action on a pair (S, t)
/// we can apply shift default to the Shift actions just like we did
/// for the Goto actions.
void merge_shift_domains(struct CLIOptions *cli_options) {
  // Some of the rows in the shift action map have already been merged
  // by the merging of compatible states... We simply need to increase
  // the size of the granularity by merging these new terminal states
  // based only on their shift actions.
  //
  // The array SHIFT_DOMAIN_TABLE is used as the base for a hash table.
  // Each submap represented by a row of the shift action map is hashed
  // into this table by summing the terminal symbols in its domain.
  // The submap is then entered in the hash table and assigned a unique
  // number if such a map was not already placed in the hash table.
  // Otherwise, the number assigned to the previous submap is also
  // associated with the new submap.
  //
  // The vector SHIFT_IMAGE is used to keep track of the unique number
  // associated with each unique shift submap.
  // The vector REAL_SHIFT_NUMBER is the inverse of SHIFT_IMAGE. It is
  // used to associate each unique number to its shift submap.
  // The integer NUM_TABLE_ENTRIES is used to count the number of
  // elements in the new merged shift map.
  //
  // The arrays ORDERED_SHIFT and ROW_SIZE are also initialized here.
  // They are used to sort the rows of the shift actions map later...
  short *shift_domain_link = Allocate_short_array(num_terminal_states + 1);
  long *ordered_shift = Allocate_long_array(num_shift_maps + 1);
  short *terminal_list = Allocate_short_array(num_terminals + 1);
  shift_image = Allocate_short_array(max_la_state + 1);
  real_shift_number = Allocate_short_array(num_shift_maps + 1);
  bool *shift_symbols = Allocate_boolean_array(num_terminals + 1);
  shift_check_index = Allocate_long_array(num_shift_maps + 1);
  short shift_domain_table[SHIFT_TABLE_SIZE];
  for (int i = 0; i <= SHIFT_TABLE_UBOUND; i++) {
    shift_domain_table[i] = NIL;
  }
  num_table_entries = 0;
  shift_domain_count = 0;
  for (int state_no = 1; state_no <= num_terminal_states; state_no++) {
    int shift_no = new_state_element[state_no].shift_number;
    for (int i = 1; i <= num_terminals; i++) {
      shift_symbols[i] = false;
    }
    struct shift_header_type sh = shift[shift_no];
    int shift_size = sh.size;
    unsigned long hash_address = shift_size;
    for (int i = 1; i <= shift_size; i++) {
      int symbol = sh.map[i].symbol;
      hash_address += symbol;
      shift_symbols[symbol] = true;
    }
    hash_address %= SHIFT_TABLE_SIZE;
    for (int i = shift_domain_table[hash_address]; i != NIL; i = shift_domain_link[i]) {
      sh = shift[new_state_element[i].shift_number];
      if (sh.size == shift_size) {
        int jj;
        for (jj = 1; jj <= shift_size; jj++) {
          if (!shift_symbols[sh.map[jj].symbol]) {
            break;
          }
        }
        if (jj > shift_size) {
          shift_image[state_no] = shift_image[i];
          goto continu;
        }
      }
    }
    shift_domain_link[state_no] = shift_domain_table[hash_address];
    shift_domain_table[hash_address] = state_no;
    shift_domain_count++;
    shift_image[state_no] = shift_domain_count;
    real_shift_number[shift_domain_count] = shift_no;
    ordered_shift[shift_domain_count] = shift_domain_count;
    row_size[shift_domain_count] = shift_size;
    num_table_entries += shift_size;
  continu:;
  }
  //   Compute the frequencies, and remap the terminal symbols
  // accordingly.
  for ALL_TERMINALS3(symbol) {
    frequency_symbol[symbol] = symbol;
    frequency_count[symbol] = 0;
  }
  for (int i = 1; i <= shift_domain_count; i++) {
    int shift_no = real_shift_number[i];
    struct shift_header_type sh = shift[shift_no];
    for (int j = 1; j <= sh.size; j++) {
      int symbol = sh.map[j].symbol;
      frequency_count[symbol]++;
    }
  }
  sortdes(frequency_symbol, frequency_count, 1, num_terminals, shift_domain_count);
  for ALL_TERMINALS3(symbol) {
    symbol_map[frequency_symbol[symbol]] = symbol;
  }
  symbol_map[DEFAULT_SYMBOL] = DEFAULT_SYMBOL;
  eoft_image = symbol_map[eoft_image];
  if (error_maps_bit) {
    error_image = symbol_map[error_image];
    eolt_image = symbol_map[eolt_image];
  }
  for (int i = 1; i <= num_shift_maps; i++) {
    struct shift_header_type sh = shift[i];
    for (int j = 1; j <= sh.size; j++) {
      sh.map[j].symbol = symbol_map[sh.map[j].symbol];
    }
  }
  for (int state_no = 1; state_no <= num_terminal_states; state_no++) {
    struct reduce_header_type red = new_state_element[state_no].reduce;
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol = symbol_map[red.map[i].symbol];
    }
  }
  // If ERROR_MAPS are requested, we also have to remap the original
  // REDUCE maps.
  if (error_maps_bit) {
    for ALL_STATES3(state_no) {
      struct reduce_header_type red = reduce[state_no];
      for (int i = 1; i <= red.size; i++) {
        red.map[i].symbol = symbol_map[red.map[i].symbol];
      }
    }
  }
  // Remap the SHIFT_DEFAULT map.
  short *temp_shift_default = Allocate_short_array(num_terminals + 1);
  for ALL_TERMINALS3(symbol) {
    temp_shift_default[symbol_map[symbol]] = shiftdf[symbol];
  }
  ffree(shiftdf);
  shiftdf = temp_shift_default;
  // We now compute the starting position for each Shift check row
  // as we did for the terminal states.  The starting positions are
  // stored in the vector SHIFT_CHECK_INDEX.
  sortdes(ordered_shift, row_size, 1, shift_domain_count, num_terminals);
  increment_size = MAX(num_table_entries / 100 * increment, num_terminals + 1);
  int old_table_size = table_size;
  table_size = MIN(num_table_entries + increment_size, MAX_TABLE_SIZE);
  if ((int) table_size > old_table_size) {
    ffree(previous);
    ffree(next);
    previous = Allocate_long_array(table_size + 1);
    next = Allocate_long_array(table_size + 1);
  } else {
    table_size = old_table_size;
  }
  first_index = 1;
  previous[first_index] = NIL;
  next[first_index] = first_index + 1;
  for (int indx = 2; indx < (int) table_size; indx++) {
    next[indx] = indx + 1;
    previous[indx] = indx - 1;
  }
  last_index = table_size;
  previous[last_index] = last_index - 1;
  next[last_index] = NIL;
  int max_indx = first_index;
  // Look for a suitable index where to overlay the shift check row.
  for (int k = 1; k <= shift_domain_count; k++) {
    int shift_no = ordered_shift[k];
    struct shift_header_type sh = shift[real_shift_number[shift_no]];
    int indx = first_index;
  look_for_match_in_sh_chk_tab:
    if (indx == NIL) {
      indx = table_size + 1;
    }
    if (indx + num_terminals > (int) table_size) {
      reallocate(cli_options);
    }
    for (int i = 1; i <= sh.size; i++) {
      int symbol = sh.map[i].symbol;
      if (next[indx + symbol] == OMEGA) {
        indx = next[indx];
        goto look_for_match_in_sh_chk_tab;
      }
    }
    // INDX marks the starting position for the row, remove all the
    // positions that are claimed by that shift check row.
    // If a position has the value 0,   then it is the starting position
    // of a Shift row that was previously processed, and that element
    // has already been removed from the list of available positions.
    for (int j = 1; j <= sh.size; j++) {
      int symbol = sh.map[j].symbol;
      int i = indx + symbol;
      if (next[i] != 0) {
        if (i == last_index) {
          last_index = previous[last_index];
          next[last_index] = NIL;
        } else {
          next[previous[i]] = next[i];
          previous[next[i]] = previous[i];
        }
      }
      next[i] = OMEGA;
    }
    // We now remove the starting position itself from the list without
    // marking it as taken, since it can still be used for a shift check.
    // MAX_INDX is updated if required.
    // SHIFT_CHECK_INDEX(SHIFT_NO) is properly set to INDX as the
    // starting position of STATE_NO.
    if (first_index == last_index)
      first_index = NIL;
    else if (indx == first_index) {
      first_index = next[first_index];
      previous[first_index] = NIL;
    } else if (indx == last_index) {
      last_index = previous[last_index];
      next[last_index] = NIL;
    } else {
      next[previous[indx]] = next[indx];
      previous[next[indx]] = previous[indx];
    }
    next[indx] = 0;
    if (indx > max_indx) {
      max_indx = indx;
    }
    shift_check_index[shift_no] = indx;
  }
  // Update all counts, and report statistics.
  shift_check_size = max_indx + num_terminals;
  printf("\n");
  PRNT3("Length of Shift Check Table: %d", shift_check_size);
  PRNT3("Number of entries in Shift Check Table: %ld", num_table_entries);
  int kk;
  for (kk = shift_check_size; kk >= max_indx; kk--) {
    if (next[kk] == OMEGA) {
      break;
    }
  }
  long percentage = (kk - num_table_entries) * 1000 / num_table_entries;
  PRNT3("Percentage of increase: %ld.%ld%%", percentage/10, percentage % 10);
  int num_bytes;
  if (cli_options->byte_bit) {
    num_bytes = shift_check_size;
    if (num_terminals > 255) {
      num_bytes += shift_check_size;
    }
  } else {
    num_bytes = 2 * shift_check_size;
  }
  num_bytes += 2 * (num_terminal_states + num_terminals);
  int k_bytes = num_bytes / 1024 + 1;
  PRNT3("Storage required for Shift Check Table: %d Bytes, %dK", num_bytes, k_bytes);
  total_bytes += num_bytes;
  ffree(ordered_shift);
  ffree(terminal_list);
  ffree(shift_symbols);
  ffree(shift_domain_link);
}

/// By now, similar states have been grouped together, and placed in
/// one of three linear linked lists headed by the root pointers:
/// MULTI_ROOT, SINGLE_ROOT, and EMPTY_ROOT.
/// We iterate over each of these lists and construct new states out
/// of these groups of similar states when they are compatible. Then,
/// we remap the terminal symbols.
void overlay_sim_t_rows(struct CLIOptions *cli_options) {
  int num_shifts_saved = 0;
  int num_reductions_saved = 0;
  int default_saves = 0;
  short *rule_count = Allocate_short_array(num_rules + 1);
  short *reduce_action = Allocate_short_array(num_terminals + 1);
  //     We first iterate over the groups of similar states in the
  // MULTI_ROOT list.  These states have been grouped together,
  // because they have the same Shift map, and reduce to the same
  // rules, but we must check that they are fully compatible by making
  // sure that no two states contain reduction to a different rule on
  // the same symbol.
  //     The idea is to take a state out of the group, and merge with
  // it as many other compatible states from the group as possible.
  // remaining states from the group that caused clashes are thrown
  // back into the MULTI_ROOT list as a new group of states.
  for (int i = multi_root; i != NIL; i = new_state_element[i].thread) {
    for (struct node *q = new_state_element_reduce_nodes[i]; q != NULL; q = q->next) {
      rule_count[q->value] = 0;
    }
    // REDUCE_ACTION is used to keep track of reductions that are to be
    // applied on terminal symbols as the states are merged.  We pick
    // out the first state (STATE_NO) from the group of states involved,
    // initialize REDUCE_ACTION with its reduce map, and count the number
    // of reductions associated with each rule in that state.
    int state_no = new_state_element[i].image;
    struct reduce_header_type red;
    if (state_no > num_states) {
      red = lastats[state_no].reduce;
    } else {
      red = reduce[state_no];
    }
    for ALL_TERMINALS3(j) {
      reduce_action[j] = OMEGA;
    }
    for (int j = 1; j <= red.size; j++) {
      int rule_no = red.map[j].rule_number;
      reduce_action[red.map[j].symbol] = rule_no;
      rule_count[rule_no]++;
    }
    // STATE_SET_ROOT is used to traverse the rest of the list that form
    // the group of states being processed.  STATE_SUBSET_ROOT is used
    // to construct the new list that will consist of all states in the
    // group that are compatible starting with the initial state.
    // STATE_ROOT is used to construct a list of all states in the group
    // that are not compatible with the initial state.
    int state_set_root = state_list[state_no];
    int state_subset_root = state_no;
    state_list[state_subset_root] = NIL;
    int state_root = NIL;
    for (int state_no = state_set_root; state_no != NIL; state_no = state_set_root) {
      state_set_root = state_list[state_set_root];
      // We traverse the reduce map of the state taken out from the group
      // and check to see if it is compatible with the subset being
      // constructed so far.
      if (state_no > num_states) {
        red = lastats[state_no].reduce;
      } else {
        red = reduce[state_no];
      }
      int jj;
      for (jj = 1; jj <= red.size; jj++) {
        int symbol = red.map[jj].symbol;
        if (reduce_action[symbol] != OMEGA && reduce_action[symbol] != red.map[jj].rule_number) {
          break;
        }
      }

      // If J > Q->REDUCE_ELEMENT.N then we traversed the whole reduce map,
      // and all the reductions involved are compatible with the new state
      // being constructed.  The state involved is added to the subset, the
      // rule counts are updated, and the REDUCE_ACTIONS map is updated.
      //     Otherwise, we add the state involved to the STATE_ROOT list
      // which will be thrown back in the MULTI_ROOT list.
      if (jj > red.size) {
        state_list[state_no] = state_subset_root;
        state_subset_root = state_no;
        for (jj = 1; jj <= red.size; jj++) {
          int symbol = red.map[jj].symbol;
          if (reduce_action[symbol] == OMEGA) {
            int rule_no = red.map[jj].rule_number;
            if (rules[rule_no].lhs == accept_image) {
              rule_no = 0;
            }
            reduce_action[symbol] = rule_no;
            rule_count[rule_no]++;
          } else {
            num_reductions_saved++;
          }
        }
      } else {
        state_list[state_no] = state_root;
        state_root = state_no;
      }
    }

    // Figure out the best default rule candidate, and update
    // DEFAULT_SAVES.
    // Recall that all accept actions were changed into reduce actions
    // by rule 0.
    int k = 0;
    int reduce_size = 0;
    int default_rule = error_act;
    struct node *tail;
    for (struct node *q = new_state_element_reduce_nodes[i]; q != NULL; tail = q, q = q->next) {
      int rule_no = q->value;
      reduce_size += rule_count[rule_no];
      if (rule_count[rule_no] > k && rule_no != 0
          && !shift_on_error_symbol[state_subset_root]) {
        k = rule_count[rule_no];
        default_rule = rule_no;
      }
    }
    default_saves += k;
    reduce_size -= k;

    // If STATE_ROOT is not NIL then there are states in the group that
    // did not meet the compatibility test.  Throw those states back in
    // front of MULTI_ROOT as a group.
    if (state_root != NIL) {
      top++;
      new_state_element[top].thread = new_state_element[i].thread;
      new_state_element[i].thread = top;
      if (state_root > num_states) {
        new_state_element[top].shift_number = lastats[state_root].shift_number;
      } else {
        new_state_element[top].shift_number = statset[state_root].shift_number;
      }
      new_state_element_reduce_nodes[top] = new_state_element_reduce_nodes[i];
      new_state_element[top].image = state_root;
    } else
      free_nodes(new_state_element_reduce_nodes[i], tail);

    // Create Reduce map for the newly created terminal state.
    // We may assume that SYMBOL field of defaults is already set to
    // the DEFAULT_SYMBOL value.
    struct reduce_header_type new_red = Allocate_reduce_map(reduce_size);
    new_red.map[0].symbol = DEFAULT_SYMBOL;
    new_red.map[0].rule_number = default_rule;
    for ALL_TERMINALS3(symbol) {
      if (reduce_action[symbol] != OMEGA) {
        if (reduce_action[symbol] != default_rule) {
          new_red.map[reduce_size].symbol = symbol;
          if (reduce_action[symbol] == 0) {
            new_red.map[reduce_size].rule_number = accept_act;
          } else {
            new_red.map[reduce_size].rule_number =
                reduce_action[symbol];
          }
          reduce_size--;
        }
      }
    }
    new_state_element[i].reduce = new_red;
    new_state_element[i].image = state_subset_root;
  }
  // We now process groups of states that have reductions to a single
  // rule.  Those states are fully compatible, and the default is the
  // rule in question.
  // Any of the REDUCE_ELEMENT maps that belongs to a state in the
  // group of states being processed may be reused for the new  merged
  // state.
  for (int i = single_root; i != NIL; i = new_state_element[i].thread) {
    int state_no = new_state_element[i].image;
    struct node *q = new_state_element_reduce_nodes[i];
    int rule_no = q->value;
    free_nodes(q, q);
    struct reduce_header_type new_red;
    struct reduce_header_type red;
    if (rules[rule_no].lhs == accept_image) {
      red = reduce[state_no];
      int reduce_size = red.size;
      new_red = Allocate_reduce_map(reduce_size);
      new_red.map[0].symbol = DEFAULT_SYMBOL;
      new_red.map[0].rule_number = error_act;
      for (int j = 1; j <= reduce_size; j++) {
        new_red.map[j].symbol = red.map[j].symbol;
        new_red.map[j].rule_number = accept_act;
      }
    } else {
      for ALL_TERMINALS3(j) {
        reduce_action[j] = OMEGA;
      }
      for (; state_no != NIL; state_no = state_list[state_no]) {
        if (state_no > num_states) {
          red = lastats[state_no].reduce;
        } else {
          red = reduce[state_no];
        }
        for (int j = 1; j <= red.size; j++) {
          int symbol = red.map[j].symbol;
          if (reduce_action[symbol] == OMEGA) {
            reduce_action[symbol] = rule_no;
            default_saves++;
          } else
            num_reductions_saved++;
        }
      }
      new_red = Allocate_reduce_map(0);
      new_red.map[0].symbol = DEFAULT_SYMBOL;
      new_red.map[0].rule_number = rule_no;
    }
    new_state_element[i].reduce = new_red;
  }

  // Groups of states that have no reductions are also compatible.
  // Their default is ERROR_ACTION.
  for (int i = empty_root; i != NIL; i = new_state_element[i].thread) {
    int state_no = new_state_element[i].image;
    struct reduce_header_type red;
    if (state_no > num_states) {
      red = lastats[state_no].reduce;
    } else {
      red = reduce[state_no];
    }
    red.map[0].symbol = DEFAULT_SYMBOL;
    red.map[0].rule_number = error_act;
    new_state_element[i].reduce = red;
  }
  num_terminal_states = top;
  frequency_symbol = Allocate_long_array(num_terminals + 1);
  frequency_count = Allocate_long_array(num_terminals + 1);
  row_size = Allocate_long_array(max_la_state + 1);
  if (cli_options->shift_default_bit) {
    merge_shift_domains(cli_options);
  }
  // We now reorder the terminal states based on the number of actions
  // in them, and remap the terminal symbols if they were not already
  // remapped in the previous block for the SHIFT_CHECK vector.
  for ALL_TERMINALS3(symbol) {
    frequency_symbol[symbol] = symbol;
    frequency_count[symbol] = 0;
  }
  for (int i = 1; i <= num_terminal_states; i++) {
    ordered_state[i] = i;
    row_size[i] = 0;
    struct shift_header_type sh = shift[new_state_element[i].shift_number];
    for (int j = 1; j <= sh.size; j++) {
      int symbol = sh.map[j].symbol;
      if (!cli_options->shift_default_bit || sh.map[j].action != shiftdf[symbol]) {
        row_size[i]++;
        frequency_count[symbol]++;
      }
    }
    for (int state_no = state_list[new_state_element[i].image];
         state_no != NIL; state_no = state_list[state_no]) {
      num_shifts_saved += row_size[i];
    }
    struct reduce_header_type red;
    // Note that the Default action is skipped !!!
    red = new_state_element[i].reduce;
    for (int j = 1; j <= red.size; j++) {
      int symbol = red.map[j].symbol;
      row_size[i]++;
      frequency_count[symbol]++;
    }
  }
  PRNT3("Number of unique terminal states: %d", num_terminal_states);

  PRNT3("Number of Shift actions saved by merging: %d", num_shifts_saved);

  PRNT3("Number of Reduce actions saved by merging: %d", num_reductions_saved);

  PRNT3("Number of Reduce saved by default: %d", default_saves);

  sortdes(ordered_state, row_size, 1, num_terminal_states, num_terminals);

  if (!cli_options->shift_default_bit) {
    sortdes(frequency_symbol, frequency_count, 1, num_terminals, num_terminal_states);
    for ALL_TERMINALS3(symbol) {
      symbol_map[frequency_symbol[symbol]] = symbol;
    }
    symbol_map[DEFAULT_SYMBOL] = DEFAULT_SYMBOL;
    eoft_image = symbol_map[eoft_image];
    if (error_maps_bit) {
      error_image = symbol_map[error_image];
      eolt_image = symbol_map[eolt_image];
    }
    for (int i = 1; i <= num_shift_maps; i++) {
      struct shift_header_type sh = shift[i];
      for (int j = 1; j <= sh.size; j++) {
        sh.map[j].symbol = symbol_map[sh.map[j].symbol];
      }
    }

    for (int state_no = 1; state_no <= num_terminal_states; state_no++) {
      struct reduce_header_type red = new_state_element[state_no].reduce;
      for (int i = 1; i <= red.size; i++) {
        red.map[i].symbol = symbol_map[red.map[i].symbol];
      }
    }
    // If ERROR_MAPS are requested, we also have to remap the original
    // REDUCE maps.
    if (error_maps_bit) {
      for ALL_STATES3(state_no) {
        struct reduce_header_type red = reduce[state_no];
        for (int i = 1; i <= red.size; i++) {
          red.map[i].symbol = symbol_map[red.map[i].symbol];
        }
      }
    }
  }
  num_table_entries = num_shifts + num_shift_reduces + num_reductions
                      - num_shifts_saved - num_reductions_saved
                      - default_saves + num_terminal_states;
  ffree(rule_count);
  ffree(reduce_action);
  ffree(row_size);
  ffree(frequency_count);
  ffree(frequency_symbol);
  ffree(shift_on_error_symbol);
  ffree(new_state_element_reduce_nodes);
}

/// We now compute the starting position for each terminal state just
/// as we did for the non-terminal states.
/// The starting positions are stored in the vector TERM_STATE_INDEX.
void overlap_t_rows(struct CLIOptions *cli_options) {
  short *terminal_list = Allocate_short_array(num_terminals + 1);
  term_state_index = Allocate_long_array(max_la_state + 1);
  increment_size = MAX(num_table_entries * increment / 100, num_terminals + 1);
  const long old_size = table_size;
  table_size = MIN(num_table_entries + increment_size, MAX_TABLE_SIZE);
  if ((int) table_size > old_size) {
    ffree(previous);
    ffree(next);
    next = Allocate_long_array(table_size + 1);
    previous = Allocate_long_array(table_size + 1);
  } else {
    table_size = old_size;
  }
  first_index = 1;
  previous[first_index] = NIL;
  next[first_index] = first_index + 1;
  for (int indx = 2; indx < (int) table_size; indx++) {
    next[indx] = indx + 1;
    previous[indx] = indx - 1;
  }
  last_index = table_size;
  previous[last_index] = last_index - 1;
  next[last_index] = NIL;
  int max_indx = first_index;
  for (int k = 1; k <= num_terminal_states; k++) {
    const int state_no = ordered_state[k];
    // For the terminal table, we are dealing with two lists, the SHIFT
    // list, and the REDUCE list. Those lists are merged together first
    // in TERMINAL_LIST.  Since we have to iterate over the list twice,
    // this merging makes things easy.
    int root_symbol = NIL;
    const struct shift_header_type sh = shift[new_state_element[state_no].shift_number];
    for (int i = 1; i <= sh.size; i++) {
      int symbol = sh.map[i].symbol;
      if (!cli_options->shift_default_bit ||
          sh.map[i].action != shiftdf[symbol]) {
        terminal_list[symbol] = root_symbol;
        root_symbol = symbol;
      }
    }
    const struct reduce_header_type red = new_state_element[state_no].reduce;
    for (int i = 1; i <= red.size; i++) {
      terminal_list[red.map[i].symbol] = root_symbol;
      root_symbol = red.map[i].symbol;
    }
    // Look for a suitable index where to overlay the state.
    int indx = first_index;
  look_for_match_in_term_table:
    if (indx == NIL) {
      indx = table_size + 1;
    }
    if (indx + num_terminals > (int) table_size) {
      reallocate(cli_options);
    }
    for (int symbol = root_symbol; symbol != NIL; symbol = terminal_list[symbol]) {
      if (next[indx + symbol] == OMEGA) {
        indx = next[indx];
        goto look_for_match_in_term_table;
      }
    }
    // INDX marks the starting position for the state, remove all the
    // positions that are claimed by terminal actions in the state.
    for (int symbol = root_symbol; symbol != NIL; symbol = terminal_list[symbol]) {
      const int i = indx + symbol;
      if (i == last_index) {
        last_index = previous[last_index];
        next[last_index] = NIL;
      } else {
        next[previous[i]] = next[i];
        previous[next[i]] = previous[i];
      }
      next[i] = OMEGA;
    }
    // We now remove the starting position itself from the list, and
    // mark it as taken(CHECK(INDX) = OMEGA)
    // MAX_INDX is updated if required.
    // TERM_STATE_INDEX(STATE_NO) is properly set to INDX as the starting
    // position of STATE_NO.
    if (first_index == last_index) {
      first_index = NIL;
    } else if (indx == first_index) {
      first_index = next[first_index];
      previous[first_index] = NIL;
    } else if (indx == last_index) {
      last_index = previous[last_index];
      next[last_index] = NIL;
    } else {
      next[previous[indx]] = next[indx];
      previous[next[indx]] = previous[indx];
    }
    next[indx] = OMEGA;
    if (indx > max_indx) {
      max_indx = indx;
    }
    term_state_index[state_no] = indx;
  }
  // Update all counts, and report statistics.
  term_check_size = max_indx + num_terminals;
  for (term_action_size = max_indx + num_terminals;
       term_action_size >= max_indx; term_action_size--) {
    if (next[term_action_size] == OMEGA) {
      break;
    }
  }
  printf("\n");
  PRNT3("Length of Terminal Check Table: %d", term_check_size);
  PRNT3("Length of Terminal Action Table: %d", term_action_size);
  PRNT3("Number of entries in Terminal Action Table: %ld", num_table_entries);
  const long percentage = (term_action_size - num_table_entries) * 1000 / num_table_entries;
  PRNT3("Percentage of increase: %ld.%ld%%", percentage / 10, percentage % 10);
  long num_bytes;
  if (cli_options->byte_bit) {
    num_bytes = 2 * term_action_size + term_check_size;
    if (num_terminals > 255) {
      num_bytes += term_check_size;
    }
  } else {
    num_bytes = 2 * (term_action_size + term_check_size);
  }
  if (cli_options->shift_default_bit) {
    num_bytes += 2 * num_terminal_states;
  }
  long k_bytes = num_bytes / 1024 + 1;
  PRNT3("Storage required for Terminal Tables: %ld Bytes, %ldK", num_bytes, k_bytes);
  total_bytes += num_bytes;
  // Report total number of storage used.
  k_bytes = total_bytes / 1024 + 1;
  PRNT3("Total storage required for Tables: %ld Bytes, %ldK", total_bytes, k_bytes);
  // We now write out the tables to the SYSTAB file.
  table_size = MAX(check_size, term_check_size);
  table_size = MAX(table_size, shift_check_size);
  table_size = MAX(table_size, action_size);
  table_size = MAX(table_size, term_action_size);
  ffree(terminal_list);
  ffree(next);
  ffree(previous);
}

/// We now write out the tables to the SYSTAB file.
void print_tables_space(struct CLIOptions *cli_options, FILE *systab) {
  int default_count = 0;
  int goto_count = 0;
  int goto_reduce_count = 0;
  int reduce_count = 0;
  int la_shift_count = 0;
  int shift_count = 0;
  int shift_reduce_count = 0;
  long *check = Allocate_long_array(table_size + 1);
  long *action = Allocate_long_array(table_size + 1);
  // Prepare header card with proper information, and write it out.
  long offset = error_act;
  if (cli_options->read_reduce_bit) {
    offset += num_rules;
  }
  int la_state_offset = offset;
  if (offset > MAX_TABLE_SIZE + 1) {
    PRNTERR2("Table contains entries that are > %ld; Processing stopped.", MAX_TABLE_SIZE + 1);
    exit(12);
  }
  output_buffer[0] = 'S';
  output_buffer[1] = cli_options->goto_default_bit ? '1' : '0';
  output_buffer[2] = cli_options->nt_check_bit ? '1' : '0';
  output_buffer[3] = cli_options->read_reduce_bit ? '1' : '0';
  output_buffer[4] = cli_options->single_productions_bit ? '1' : '0';
  output_buffer[5] = cli_options->shift_default_bit ? '1' : '0';
  output_buffer[6] = rules[1].lhs == accept_image ? '1' : '0';
  // are there more than 1 start symbols?
  output_buffer[7] = error_maps_bit ? '1' : '0';
  output_buffer[8] = cli_options->byte_bit && last_symbol <= 255 ? '1' : '0';
  output_buffer[9] = escape;
  output_ptr = output_buffer + 10;
  field(num_terminals, 5);
  field(num_non_terminals, 5);
  field(num_rules, 5);
  field(num_states, 5);
  field(check_size, 5);
  field(action_size, 5);
  field(term_check_size, 5);
  field(term_action_size, 5);
  field(state_index[1] + num_rules, 5);
  field(eoft_image, 5);
  field(accept_act, 5);
  field(error_act, 5);
  field(la_state_offset, 5);
  field(cli_options->lalr_level, 5);
  *output_ptr++ = '\n';
  // We write the terminal symbols map.
  for ALL_TERMINALS3(symbol) {
    char *tok = RETRIEVE_STRING(symbol);
    if (tok[0] == '\n') /* we're dealing with special symbol?  */
      tok[0] = escape; /* replace initial marker with escape. */
    int len = strlen(tok);
    field(symbol_map[symbol], 4);
    field(len, 4);
    if (len <= 64)
      strcpy(output_ptr, tok);
    else {
      memcpy(output_ptr, tok, 64);
      output_ptr += 64;
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
      tok += 64;

      for (len = strlen(tok); len > 72; len = strlen(tok)) {
        memcpy(output_ptr, tok, 72);
        output_ptr += 72;
        *output_ptr++ = '\n';
        BUFFER_CHECK(systab);
        tok += 72;
      }
      memcpy(output_ptr, tok, len);
    }
    output_ptr += len;
    *output_ptr++ = '\n';
    BUFFER_CHECK(systab);
  }
  // We write the non-terminal symbols map.
  for ALL_NON_TERMINALS3(symbol) {
    int len;
    char *tok = RETRIEVE_STRING(symbol);
    if (tok[0] == '\n') /* we're dealing with special symbol?  */
      tok[0] = escape; /* replace initial marker with escape. */
    len = strlen(tok);
    field(symbol_map[symbol] - num_terminals, 4);
    field(len, 4);
    if (len <= 64)
      strcpy(output_ptr, tok);
    else {
      memcpy(output_ptr, tok, 64);
      output_ptr += 64;
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
      tok += 64;

      for (len = strlen(tok); len > 72; len = strlen(tok)) {
        memcpy(output_ptr, tok, 72);
        output_ptr += 72;
        *output_ptr++ = '\n';
        BUFFER_CHECK(systab);
        tok += 72;
      }
      memcpy(output_ptr, tok, len);
    }
    output_ptr += len;
    *output_ptr++ = '\n';
    BUFFER_CHECK(systab);
  }
  // Initialize TABLES with default actions.
  for (int i = 1; i <= check_size; i++) {
    check[i] = DEFAULT_SYMBOL;
  }
  for (int i = 1; i <= (int) action_size; i++) {
    action[i] = error_act;
  }
  //    Update the default non-terminal action of each state with the
  // appropriate corresponding terminal state starting index.
  for (int i = 1; i <= num_terminal_states; i++) {
    int indx = term_state_index[i];
    int state_no = new_state_element[i].image;
    //   Update the action link between the non-terminal and terminal
    // tables.  If error-maps are requested, an indirect linking is made
    // as follows:
    //  Each non-terminal row identifies its original state number, and
    // a new vector START_TERMINAL_STATE indexable by state numbers
    // identifies the starting point of each state in the terminal table.
    if (state_no <= num_states) {
      for (; state_no != NIL; state_no = state_list[state_no]) {
        action[state_index[state_no]] = indx;
      }
    } else {
      for (; state_no != NIL; state_no = state_list[state_no]) {
        int act = la_state_offset + indx;
        state_index[state_no] = act;
      }
    }
  }
  //  Now update the non-terminal tables with the non-terminal actions.
  for ALL_STATES3(state_no) {
    struct goto_header_type go_to;
    int indx = state_index[state_no];
    go_to = statset[state_no].go_to;
    for (int j = 1; j <= go_to.size; j++) {
      int symbol = go_to.map[j].symbol;
      int i = indx + symbol;
      if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
        check[i] = symbol;
      }
      int act = go_to.map[j].action;
      if (act > 0) {
        action[i] = state_index[act] + num_rules;
        goto_count++;
      } else {
        action[i] = -act;
        goto_reduce_count++;
      }
    }
  }
  // Write size of right hand side of rules followed by CHECK table.
  int k = 0;
  for (int i = 1; i <= num_rules; i++) {
    field(RHS_SIZE(i), 4);
    k++;
    if (k == 18) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
      k = 0;
    }
  }
  for (int i = 1; i <= check_size; i++) {
    field(check[i], 4);
    k++;
    if (k == 18) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
      k = 0;
    }
  }
  if (k != 0) {
    *output_ptr++ = '\n';
    BUFFER_CHECK(systab);
  }
  // Write left hand side symbol of rules followed by ACTION table.
  k = 0;
  for (int i = 1; i <= num_rules; i++) {
    field(symbol_map[rules[i].lhs] - num_terminals, 6);
    k++;
    if (k == 12) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
      k = 0;
    }
  }
  for (int i = 1; i <= (int) action_size; i++) {
    field(action[i], 6);
    k++;
    if (k == 12) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
      k = 0;
    }
  }
  if (k != 0) {
    *output_ptr++ = '\n';
    BUFFER_CHECK(systab);
  }
  // Initialize the terminal tables,and update with terminal actions.
  for (int i = 1; i <= term_check_size; i++) {
    check[i] = DEFAULT_SYMBOL;
  }
  for (int i = 1; i <= term_action_size; i++) {
    action[i] = error_act;
  }
  for (int state_no = 1; state_no <= num_terminal_states; state_no++) {
    struct shift_header_type sh;
    struct reduce_header_type red;
    int indx = term_state_index[state_no];
    sh = shift[new_state_element[state_no].shift_number];
    for (int j = 1; j <= sh.size; j++) {
      int symbol = sh.map[j].symbol;
      int act = sh.map[j].action;
      if (!cli_options->shift_default_bit || act != shiftdf[symbol]) {
        int i = indx + symbol;
        check[i] = symbol;
        long result_act;
        if (act > num_states) {
          result_act = state_index[act];
          la_shift_count++;
        } else if (act > 0) {
          result_act = state_index[act] + num_rules;
          shift_count++;
        } else {
          result_act = -act + error_act;
          shift_reduce_count++;
        }
        if (result_act > MAX_TABLE_SIZE + 1) {
          PRNTERR2("Table contains look-ahead shift entry that is >%ld; Processing stopped.", MAX_TABLE_SIZE + 1);
          return;
        }
        action[i] = result_act;
      }
    }

    red = new_state_element[state_no].reduce;
    for (int j = 1; j <= red.size; j++) {
      int symbol = red.map[j].symbol;
      int rule_no = red.map[j].rule_number;
      int i = indx + symbol;
      check[i] = symbol;
      action[i] = rule_no;
      reduce_count++;
    }
    int rule_no = red.map[0].rule_number;
    if (rule_no != error_act) {
      default_count++;
    }
    check[indx] = DEFAULT_SYMBOL;
    if (cli_options->shift_default_bit) {
      action[indx] = state_no;
    } else {
      action[indx] = rule_no;
    }
  }
  PRNT("\n\nActions in Compressed Tables:");
  PRNT3("     Number of Shifts: %d", shift_count);
  PRNT3("     Number of Shift/Reduces: %d", shift_reduce_count);
  if (max_la_state > num_states) {
    PRNT3("     Number of Look-Ahead Shifts: %d", la_shift_count);
  }
  PRNT3("     Number of Gotos: %d", goto_count);
  PRNT3("     Number of Goto/Reduces: %d", goto_reduce_count);
  PRNT3("     Number of Reduces: %d", reduce_count);
  PRNT3("     Number of Defaults: %d", default_count);
  // Write Terminal Check Table.
  k = 0;
  for (int i = 1; i <= term_check_size; i++) {
    field(check[i], 4);
    k++;
    if (k == 18) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
      k = 0;
    }
  }
  if (k != 0) {
    *output_ptr++ = '\n';
    BUFFER_CHECK(systab);
  }
  // Write Terminal Action Table.
  k = 0;
  for (int i = 1; i <= term_action_size; i++) {
    field(action[i], 6);
    k++;
    if (k == 12) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
      k = 0;
    }
  }
  if (k != 0) {
    *output_ptr++ = '\n';
    BUFFER_CHECK(systab);
  }
  // If GOTO_DEFAULT is requested, we print out the GOTODEF vector.
  if (cli_options->goto_default_bit) {
    k = 0;
    for ALL_NON_TERMINALS3(symbol) {
      int act = gotodef[symbol];
      long result_act;
      if (act < 0) {
        result_act = -act;
      } else if (act == 0) {
        result_act = error_act;
      } else {
        result_act = state_index[act] + num_rules;
      }
      field(result_act, 6);
      k++;
      if (k == 12) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(systab);
        k = 0;
      }
    }

    if (k != 0) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
    }
  }
  // If SHIFT_DEFAULT is requested, we print out the Default Reduce
  // map, the Shift State map, the Shift Check vector, and the SHIFTDF
  // vector.
  if (cli_options->shift_default_bit) {
    // Print out header
    field(num_terminal_states, 5);
    field(shift_check_size, 5);
    *output_ptr++ = '\n';

    k = 0;
    for (int state_no = 1; state_no <= num_terminal_states; state_no++) {
      struct reduce_header_type red;
      red = new_state_element[state_no].reduce;
      field(red.map[0].rule_number, 4);
      k++;
      if (k == 18) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(systab);
        k = 0;
      }
    }
    if (k != 0) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
    }

    // First, check whether the  maximum value in SHIFT_STATE
    // table exceeds 9999. If so, stop. Otherwise, write out
    // SHIFT_STATE table.
    if (shift_check_size - num_terminals > 9999) {
      PRNTERR("SHIFT_STATE map contains > 9999 elements");
      return;
    }

    k = 0;
    for (int state_no = 1; state_no <= num_terminal_states; state_no++) {
      field(shift_check_index[shift_image[state_no]], 4);
      k++;
      if (k == 18) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(systab);
        k = 0;
      }
    }
    if (k != 0) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
    }
    // Set the Check vector with the symbols in the domain of the shift
    // maps.
    for (int i = 1; i <= shift_check_size; i++) {
      check[i] = DEFAULT_SYMBOL;
    }
    for (int i = 1; i <= shift_domain_count; i++) {
      struct shift_header_type sh;
      int indx = shift_check_index[i];
      sh = shift[real_shift_number[i]];
      for (int j = 1; j <= sh.size; j++) {
        int symbol = sh.map[j].symbol;
        check[indx + symbol] = symbol;
      }
    }

    k = 0;
    for (int i = 1; i <= shift_check_size; i++) {
      field(check[i], 4);
      k++;
      if (k == 18) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(systab);
        k = 0;
      }
    }
    if (k != 0) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
    }

    k = 0;
    for ALL_TERMINALS3(symbol) {
      int act = shiftdf[symbol];
      long result_act;
      if (act < 0) {
        result_act = -act + error_act;
      } else if (act == 0) {
        result_act = error_act;
      } else if (act > num_states) {
        result_act = state_index[act];
      } else {
        result_act = state_index[act] + num_rules;
      }
      if (result_act > MAX_TABLE_SIZE + 1) {
        PRNTERR2("Table contains look-ahead shift entry that is >%ld; Processing stopped.", MAX_TABLE_SIZE + 1);
        return;
      }
      field(result_act, 6);
      k++;
      if (k == 12) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(systab);
        k = 0;
      }
    }
    if (k != 0) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
    }
  }
  // We first sort the new state numbers.  A bucket sort technique
  // is used using the ACTION vector as a base to simulate the
  // buckets.  NOTE: the iteration over the buckets is done backward
  // because we also construct a list of the original state numbers
  // that reflects the permutation of the new state numbers.
  // During the backward iteration,  we construct the list as a stack.
  if (error_maps_bit || cli_options->states_bit) {
    int max_indx;
    max_indx = accept_act - num_rules - 1;
    for (int i = 1; i <= max_indx; i++) {
      action[i] = OMEGA;
    }
    for ALL_STATES3(state_no) {
      action[state_index[state_no]] = state_no;
    }
    int j = num_states + 1;
    for (int i = max_indx; i >= 1; i--) {
      int state_no = action[i];
      if (state_no != OMEGA) {
        j--;
        ordered_state[j] = i + num_rules;
        state_list[j] = state_no;
      }
    }
  }
  ffree(check);
  ffree(action);
  if (error_maps_bit) {
    process_error_maps(cli_options, systab);
  }
  fwrite(output_buffer, sizeof(char), output_ptr - &output_buffer[0], systab);
}

void cmprspa(struct OutputFiles *output_files, struct CLIOptions *cli_options, FILE *systab) {
  state_index = Allocate_long_array(max_la_state + 1);
  ordered_state = Allocate_long_array(max_la_state + 1);
  symbol_map = Allocate_long_array(num_symbols + 1);
  state_list = Allocate_long_array(max_la_state + 1);
  shift_on_error_symbol = Allocate_boolean_array(max_la_state + 1);
  calloc0(new_state_element, max_la_state + 1, struct new_state_type);
  calloc0(new_state_element_reduce_nodes, max_la_state + 1, struct node *);
  remap_non_terminals(cli_options);
  overlap_nt_rows(cli_options);
  merge_similar_t_rows(cli_options);
  overlay_sim_t_rows(cli_options);
  overlap_t_rows(cli_options);
  if (cli_options->c_bit || cli_options->cpp_bit || cli_options->java_bit) {
    init_parser_files(output_files, cli_options);
    print_space_parser(cli_options);
  } else {
    print_tables_space(cli_options, systab);
  }
}
