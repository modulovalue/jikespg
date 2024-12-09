#include <stdlib.h>

#include "lpgparse.h"
static char hostfile[] = __FILE__;

#include <string.h>
#include "common.h"

int default_saves = 0;
short default_rule;

bool *is_terminal;

/// We now remap the symbols in the unified Table based on frequency.
/// We also remap the states based on frequency.
void remap_symbols(void) {
  ordered_state = Allocate_long_array(max_la_state + 1);
  symbol_map = Allocate_long_array(num_symbols + 1);
  is_terminal = Allocate_boolean_array(num_symbols + 1);
  long *frequency_symbol = Allocate_long_array(num_symbols + 1);
  long *frequency_count = Allocate_long_array(num_symbols + 1);
  long *row_size = Allocate_long_array(max_la_state + 1);
  fprintf(syslis, "\n");
  //     The variable FREQUENCY_SYMBOL is used to hold the symbols
  // in the grammar,  and the variable FREQUENCY_COUNT is used
  // correspondingly to hold the number of actions defined on each
  // symbol.
  // ORDERED_STATE and ROW_SIZE are used in a similar fashion for
  // states.
  for (int i = 1; i <= num_symbols; i++) {
    frequency_symbol[i] = i;
    frequency_count[i] = 0;
  }
  for ALL_STATES3(state_no) {
    ordered_state[state_no] = state_no;
    row_size[state_no] = 0;
    struct shift_header_type sh = shift[statset[state_no].shift_number];
    for (int i = 1; i <= sh.size; i++) {
      row_size[state_no]++;
      long symbol = sh.map[i].symbol;
      frequency_count[symbol]++;
    }
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      row_size[state_no]++;
      long symbol = go_to.map[i].symbol;
      frequency_count[symbol]++;
    }
    struct reduce_header_type red = reduce[state_no];
    default_rule = red.map[0].rule_number;
    for (int i = 1; i <= red.size; i++) {
      if (red.map[i].rule_number != default_rule) {
        row_size[state_no]++;
        long symbol = red.map[i].symbol;
        frequency_count[symbol]++;
      } else {
        default_saves++;
      }
    }
  }
  PRNT3("Number of Reductions saved by default: %d", default_saves);
  for ALL_LA_STATES3(state_no) {
    ordered_state[state_no] = state_no;
    row_size[state_no] = 0;
    struct shift_header_type sh = shift[lastats[state_no].shift_number];
    for (int i = 1; i <= sh.size; i++) {
      row_size[state_no]++;
      long symbol = sh.map[i].symbol;
      frequency_count[symbol]++;
    }
    struct reduce_header_type red = lastats[state_no].reduce;
    default_rule = red.map[0].rule_number;
    for (int i = 1; i <= red.size; i++) {
      if (red.map[i].rule_number != default_rule) {
        row_size[state_no]++;
        long symbol = red.map[i].symbol;
        frequency_count[symbol]++;
      } else {
        default_saves++;
      }
    }
  }
  //     The non-terminals are sorted in descending order based on the
  // number of actions defined on them.
  //     The terminals are sorted in descending order based on the
  // number of actions defined on them.
  sortdes(frequency_symbol, frequency_count, 1, num_terminals, max_la_state);
  sortdes(frequency_symbol, frequency_count, num_terminals + 1, num_symbols, max_la_state);
  for (last_symbol = num_symbols; last_symbol > num_terminals; last_symbol--) {
    if (frequency_count[last_symbol] != 0) {
      break;
    }
  }
  // We now merge the two sorted arrays of symbols giving precedence to
  // the terminals.  Note that we can guarantee that the terminal array
  // will be depleted first since it has precedence, and we know that
  // there exists at least one non-terminal: the accept non-terminal,
  // on which no action is defined.
  // As we merge the symbols, we keep track of which ones are terminals
  // and which ones are non-terminals.  We also keep track of the new
  // mapping for the symbols in SYMBOL_MAP.
  int j = num_terminals + 1;
  int k = 0;
  for (int i = 1; i <= num_terminals;) {
    k++;
    long symbol;
    if (frequency_count[i] >= frequency_count[j]) {
      symbol = frequency_symbol[i];
      is_terminal[k] = true;
      i++;
    } else {
      symbol = frequency_symbol[j];
      is_terminal[k] = false;
      j++;
    }
    symbol_map[symbol] = k;
  }
  symbol_map[DEFAULT_SYMBOL] = DEFAULT_SYMBOL;
  // Process the remaining non-terminal and useless terminal symbols.
  for (; j <= num_symbols; j++) {
    k++;
    long symbol = frequency_symbol[j];
    is_terminal[k] = false;
    symbol_map[symbol] = k;
  }
  eoft_image = symbol_map[eoft_image];
  if (error_maps_bit) {
    error_image = symbol_map[error_image];
    eolt_image = symbol_map[eolt_image];
  }
  //    All symbol entries in the state automaton are updated based on
  // the new mapping of the symbols.
  // The states are sorted in descending order based on the number of
  // actions defined on them.
  for ALL_STATES3(state_no) {
    struct goto_header_type go_to = statset[state_no].go_to;
    // Remap Goto map
    for (int i = 1; i <= go_to.size; i++) {
      go_to.map[i].symbol = symbol_map[go_to.map[i].symbol];
    }
    struct reduce_header_type red = reduce[state_no];
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol = symbol_map[red.map[i].symbol];
    }
  }
  for ALL_LA_STATES3(state_no) {
    struct reduce_header_type red = lastats[state_no].reduce;
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol = symbol_map[red.map[i].symbol];
    }
  }
  for (int i = 1; i <= num_shift_maps; i++) {
    struct shift_header_type sh = shift[i];
    for (int j = 1; j <= sh.size; j++) {
      sh.map[j].symbol = symbol_map[sh.map[j].symbol];
    }
  }
  sortdes(ordered_state, row_size, 1, max_la_state, num_symbols);
  ffree(frequency_symbol);
  ffree(frequency_count);
  ffree(row_size);
}

/// We now overlap the State automaton table, or more precisely,  we
/// compute the starting position in a vector where each of its rows
/// may be placed without clobbering elements in another row.
/// The starting positions are stored in the vector STATE_INDEX.
void overlap_tables(struct CLIOptions *cli_options) {
  state_index = Allocate_long_array(max_la_state + 1);
  long *symbol_list = Allocate_long_array(num_symbols + 1);
  num_entries -= default_saves;
  increment_size = MAX(num_entries * increment / 100, num_symbols + 1);
  table_size = MIN(num_entries + increment_size, MAX_TABLE_SIZE);
  // Allocate space for table, and initialize the AVAIL_POOL list.
  // The variable FIRST_INDEX keeps track of the first element in the
  // doubly-linked list, and LAST_ELEMENT keeps track of the last
  // element in the list.
  // The variable MAX_INDX is used to keep track of the maximum
  // starting position for a row that has been used.
  next = Allocate_long_array(table_size + 1);
  previous = Allocate_long_array(table_size + 1);
  first_index = 1;
  next[first_index] = first_index + 1; /* Should be constant-folded */
  previous[first_index] = NIL;
  for (long indx = 2; indx < (int) table_size; indx++) {
    next[indx] = indx + 1;
    previous[indx] = indx - 1;
  }
  last_index = table_size;
  previous[last_index] = last_index - 1;
  next[last_index] = NIL;
  long max_indx = first_index;
  // We now iterate over all the states in their new sorted order as
  // indicated by the variable STATE_NO, and determine an "overlap"
  // position for them.
  for (int k = 1; k <= max_la_state; k++) {
    const long state_no = ordered_state[k];
    // First, we iterate over all actions defined in STATE_NO, and
    // create a set with all the symbols involved.
    int root_symbol = NIL;
    struct shift_header_type sh;
    struct reduce_header_type red;
    if (state_no > num_states) {
      sh = shift[lastats[state_no].shift_number];
      red = lastats[state_no].reduce;
    } else {
      const struct goto_header_type go_to = statset[state_no].go_to;
      for (int i = 1; i <= go_to.size; i++) {
        int symbol = go_to.map[i].symbol;
        symbol_list[symbol] = root_symbol;
        root_symbol = symbol;
      }
      sh = shift[statset[state_no].shift_number];
      red = reduce[state_no];
    }
    for (int i = 1; i <= sh.size; i++) {
      int symbol = sh.map[i].symbol;
      symbol_list[symbol] = root_symbol;
      root_symbol = symbol;
    }
    symbol_list[0] = root_symbol;
    root_symbol = 0;
    default_rule = red.map[0].rule_number;
    for (int i = 1; i <= red.size; i++) {
      if (red.map[i].rule_number != default_rule) {
        int symbol = red.map[i].symbol;
        symbol_list[symbol] = root_symbol;
        root_symbol = symbol;
      }
    }
    // INDX is set to the beginning of the list of available slots and
    // we try to determine if it might be a valid starting position. If
    // not, INDX is moved to the next element, and we repeat the process
    // until a valid position is found.
    long indx = first_index;
  look_for_match_in_table:
    if (indx == NIL) {
      indx = table_size + 1;
    }
    if (indx + num_symbols > (int) table_size) {
      reallocate(cli_options);
    }
    for (int symbol = root_symbol; symbol != NIL; symbol = symbol_list[symbol]) {
      if (next[indx + symbol] == OMEGA) {
        indx = next[indx];
        goto look_for_match_in_table;
      }
    }
    // At this stage, a position(INDX), was found to overlay the row in
    // question.  Remove elements associated with all positions that
    // will be taken by row from the doubly-linked list.
    // NOTE that since SYMBOLs start at 1, the first index can never be
    // a candidate (==> I = INDX + SYMBOL) in this loop.
    if (indx > max_indx) {
      max_indx = indx;
    }
    state_index[state_no] = indx;
    for (int symbol = root_symbol; symbol != NIL; symbol = symbol_list[symbol]) {
      const long i = indx + symbol;
      if (first_index == last_index)
        first_index = NIL;
      else if (i == first_index) {
        first_index = next[first_index];
        previous[first_index] = NIL;
      } else if (i == last_index) {
        last_index = previous[last_index];
        next[last_index] = NIL;
      } else {
        next[previous[i]] = next[i];
        previous[next[i]] = previous[i];
      }
      next[i] = OMEGA;
    }
  }
  // Update all global counters, and compute ACCEPT_ACTION and
  // ERROR_ACTION.
  table_size = max_indx + num_symbols;
  accept_act = max_indx + num_rules + 1;
  error_act = accept_act + 1;
  for (action_size = table_size; action_size >= max_indx; action_size--) {
    if (next[action_size] == OMEGA) {
      break;
    }
  }
  printf("\n");
  PRNT3("Length of Check table: %ld", table_size);
  PRNT3("Length of Action table: %ld", action_size);
  PRNT3("Number of entries in Action Table: %ld", num_entries);
  const long percentage = (action_size - num_entries) * 1000 / num_entries;
  PRNT3("Percentage of increase: %ld.%ld%%", percentage / 10, percentage % 10);
  long num_bytes;
  if (cli_options->byte_bit) {
    num_bytes = 2 * action_size + table_size;
    if (!cli_options->goto_default_bit && !cli_options->nt_check_bit) {
      for (; last_symbol >= 1 && !is_terminal[last_symbol]; last_symbol--) {
      }
    }
    PRNT3("Highest symbol in Check Table: %ld", last_symbol);
    if (last_symbol > 255) {
      num_bytes += table_size;
    }
  } else {
    num_bytes = 2 * (action_size + table_size);
  }
  if (cli_options->goto_default_bit) {
    num_bytes += (long) 2 * num_symbols;
  }
  const long k_bytes = num_bytes / 1024 + 1;
  PRNT3("Storage Required for Tables: %ld Bytes, %ldK", num_bytes, k_bytes);
  num_bytes = (long) 4 * num_rules;
  if (cli_options->byte_bit) {
    num_bytes -= num_rules;
    if (num_symbols < 256) {
      num_bytes -= num_rules;
    }
  }
  PRNT3("Storage Required for Rules: %ld Bytes", num_bytes);
}

/// We now write out the tables to the SYSTAB file.
void print_tables_time(struct CLIOptions *cli_options, FILE *systab) {
  int la_shift_count = 0;
  int shift_count = 0;
  int goto_count = 0;
  int default_count = 0;
  int reduce_count = 0;
  int shift_reduce_count = 0;
  int goto_reduce_count = 0;
  state_list = Allocate_long_array(max_la_state + 1);
  long *check = next;
  long *action = previous;
  long offset = error_act;
  if (cli_options->read_reduce_bit) {
    offset += num_rules;
  }
  long la_state_offset = offset;
  if (offset > MAX_TABLE_SIZE + 1) {
    PRNTERR2("Table contains entries that are > %ld; Processing stopped.", MAX_TABLE_SIZE + 1);
    exit(12);
  }
  // Initialize all unfilled slots with default values.
  int indx = first_index;
  for (long i = indx; i != NIL && i <= (int) action_size; i = indx) {
    indx = next[i];
    check[i] = DEFAULT_SYMBOL;
    action[i] = error_act;
  }
  for (int i = (int) action_size + 1; i <= (int) table_size; i++) {
    check[i] = DEFAULT_SYMBOL;
  }
  // We set the rest of the table with the proper table entries.
  for (long state_no = 1; state_no <= max_la_state; state_no++) {
    indx = state_index[state_no];
    struct shift_header_type sh;
    struct reduce_header_type red;
    if (state_no > num_states) {
      sh = shift[lastats[state_no].shift_number];
      red = lastats[state_no].reduce;
    } else {
      struct goto_header_type go_to = statset[state_no].go_to;
      for (int j = 1; j <= go_to.size; j++) {
        int symbol = go_to.map[j].symbol;
        long i = indx + symbol;
        if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
          check[i] = symbol;
        } else {
          check[i] = DEFAULT_SYMBOL;
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
      sh = shift[statset[state_no].shift_number];
      red = reduce[state_no];
    }
    for (int j = 1; j <= sh.size; j++) {
      int symbol = sh.map[j].symbol;
      long i = indx + symbol;
      check[i] = symbol;
      int act = sh.map[j].action;
      long result_act;
      if (act > num_states) {
        result_act = la_state_offset + state_index[act];
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
    //   We now initialize the elements reserved for reduce actions in
    // the current state.
    default_rule = red.map[0].rule_number;
    for (int j = 1; j <= red.size; j++) {
      if (red.map[j].rule_number != default_rule) {
        int symbol = red.map[j].symbol;
        long i = indx + symbol;
        check[i] = symbol;
        int act = red.map[j].rule_number;
        if (rules[act].lhs == accept_image) {
          action[i] = accept_act;
        } else {
          action[i] = act;
        }
        reduce_count++;
      }
    }
    //   We now initialize the element reserved for the DEFAULT reduce
    // action of the current state.  If error maps are requested,  the
    // default slot is initialized to the original state number, and the
    // corresponding element of the DEFAULT_REDUCE array is initialized.
    // Otherwise it is initialized to the rule number in question.
    long i = indx + DEFAULT_SYMBOL;
    check[i] = DEFAULT_SYMBOL;
    int act = red.map[0].rule_number;
    if (act == OMEGA) {
      action[i] = error_act;
    } else {
      action[i] = act;
      default_count++;
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
  // Prepare Header with proper information, and write it out.
  output_buffer[0] = 'T';
  output_buffer[1] = cli_options->goto_default_bit ? '1' : '0';
  output_buffer[2] = cli_options->nt_check_bit ? '1' : '0';
  output_buffer[3] = cli_options->read_reduce_bit ? '1' : '0';
  output_buffer[4] = cli_options->single_productions_bit ? '1' : '0';
  if (cli_options->default_opt == 0) {
    output_buffer[5] = '0';
  } else if (cli_options->default_opt == 1) {
    output_buffer[5] = '1';
  } else if (cli_options->default_opt == 2) {
    output_buffer[5] = '2';
  } else if (cli_options->default_opt == 3) {
    output_buffer[5] = '3';
  } else if (cli_options->default_opt == 4) {
    output_buffer[5] = '4';
  } else {
    output_buffer[5] = '5';
  }
  output_buffer[6] = rules[1].lhs == accept_image ? '1' : '0';
  output_buffer[7] = error_maps_bit ? '1' : '0';
  output_buffer[8] = cli_options->byte_bit && last_symbol <= 255 ? '1' : '0';
  output_buffer[9] = escape;
  output_ptr = &output_buffer[0] + 10;
  field(num_terminals, 5);
  field(num_symbols, 5);
  field(num_rules, 5);
  field(num_states, 5);
  field(table_size, 5);
  field(action_size, 5);
  field(state_index[1] + num_rules, 5);
  field(eoft_image, 5);
  field(accept_act, 5);
  field(error_act, 5);
  field(la_state_offset, 5);
  field(cli_options->lalr_level, 5);
  *output_ptr++ = '\n';
  // We write the terminal symbols map.
  for (int symbol = 1; symbol <= num_symbols; symbol++) {
    if (is_terminal[symbol_map[symbol]]) {
      if (last_terminal < symbol_map[symbol]) {
        last_terminal = symbol_map[symbol];
      }
      char *tok = RETRIEVE_STRING(symbol);
      // We're dealing with special symbol?
      if (tok[0] == '\n') {
        tok[0] = escape; /* replace initial marker with escape. */
      }
      unsigned long len = strlen(tok);
      field(symbol_map[symbol], 4);
      field(len, 4);
      if (len <= 64) {
        strcpy(output_ptr, tok);
      } else {
        memcpy(output_ptr, tok, 64);
        output_ptr += 64;
        *output_ptr++ = '\n';
        *output_ptr = '\0';
        BUFFER_CHECK(systab);
        tok += 64;
        for (unsigned long len1 = strlen(tok); len1 > 72; len1 = strlen(tok)) {
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
  }
  // We write the non-terminal symbols map.
  for (int symbol = 1; symbol <= num_symbols; symbol++) {
    if (!is_terminal[symbol_map[symbol]]) {
      if (last_non_terminal < symbol_map[symbol]) {
        last_non_terminal = symbol_map[symbol];
      }
      char *tok = RETRIEVE_STRING(symbol);
      // we're dealing with special symbol?
      if (tok[0] == '\n') {
        tok[0] = escape; /* replace initial marker with escape. */
      }
      unsigned long len = strlen(tok);
      field(symbol_map[symbol], 4);
      field(len, 4);
      if (len <= 64) {
        strcpy(output_ptr, tok);
      } else {
        memcpy(output_ptr, tok, 64);
        output_ptr += 64;
        *output_ptr++ = '\n';
        BUFFER_CHECK(systab);
        tok += 64;
        for (unsigned long len1 = strlen(tok); len1 > 72; len1 = strlen(tok)) {
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
  for (int i = 1; i <= (int) table_size; i++) {
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
    field(symbol_map[rules[i].lhs], 6);
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
  // If GOTO_DEFAULT is requested, we print out the GOTODEF vector
  // after rearranging its elements based on the new ordering of the
  // symbols.  The array TEMP is used to hold the GOTODEF values.
  if (cli_options->goto_default_bit) {
    long *default_map = Allocate_long_array(num_symbols + 1);
    for (int i = 0; i <= num_symbols; i++) {
      default_map[i] = error_act;
    }
    for ALL_NON_TERMINALS3(symbol) {
      int act = gotodef[symbol];
      long result_act;
      if (act < 0) {
        result_act = -act;
      } else if (act > 0) {
        result_act = state_index[act] + num_rules;
      } else {
        result_act = error_act;
      }
      default_map[symbol_map[symbol]] = result_act;
    }
    k = 0;
    for (int symbol = 1; symbol <= num_symbols; symbol++) {
      k++;
      field(default_map[symbol], 6);
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
  // We first sort the new state numbers. A bucket sort technique
  // is using the ACTION vector as a base to simulate the
  // buckets.  NOTE: the iteration over the buckets is done backward
  // because we also construct a list of the original state numbers
  // that reflects the permutation of the new state numbers.
  // During the backward iteration,  we construct the list as a stack.
  if (error_maps_bit || cli_options->states_bit) {
    long max_indx = accept_act - num_rules - 1;
    for (int i = 1; i <= max_indx; i++) {
      action[i] = OMEGA;
    }
    for ALL_STATES3(state_no) {
      action[state_index[state_no]] = state_no;
    }
    long j = num_states + 1;
    for (long i = max_indx; i >= 1; i--) {
      long state_no = action[i];
      if (state_no != OMEGA) {
        j--;
        ordered_state[j] = i + num_rules;
        state_list[j] = state_no;
      }
    }
  }
  ffree(next);
  ffree(previous);
  // If ERROR_MAPS are requested, we print them out in the following
  // order:
  //
  //    1) The FOLLOW map (NEWFOLL)
  //    2) The SORTED_STATE vector
  //    3) The ORIGINAL_STATE vector
  //    4) The map from states into valid symbols on which actions are
  //       defined within the state in question: ACTION_SYMBOLS
  //    5) The map from each symbol into the set of states that can
  //       possibly be reached after a transition on the symbol in
  //       question: TRANSITION_STATES
  //
  if (error_maps_bit) {
    process_error_maps(cli_options, systab);
  }
  fwrite(output_buffer, sizeof(char), output_ptr - &output_buffer[0], systab);
}

/// In this routine we compress the State tables and write them out
/// to a file.  The emphasis here is in generating tables that allow
/// fast access. The terminal and non-terminal tables are compressed
/// together, to achieve maximum speed efficiency.
/// Otherwise, the compression technique used in this table is
/// analogous to the technique used in the routine CMPRSPA.
void cmprtim(struct OutputFiles *output_files, struct CLIOptions *cli_options, FILE *systab) {
  remap_symbols();
  overlap_tables(cli_options);
  if (cli_options->c_bit || cli_options->cpp_bit || cli_options->java_bit) {
    init_parser_files(output_files, cli_options);
    print_time_parser(cli_options);
  } else {
    print_tables_time(cli_options, systab);
  }
}
