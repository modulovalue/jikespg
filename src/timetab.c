#include <stdlib.h>
#include <string.h>
#include "common.h"

/// We now remap the symbols in the unified Table based on frequency.
/// We also remap the states based on frequency.
struct DefaultSaves {
  int default_saves;
  int last_symbol;
} remap_symbols(struct TableOutput* toutput, bool* is_terminal, struct SRTable* srt, struct lastats_type *lastats, struct statset_type *statset) {
  int default_saves = 0;
  long *frequency_symbol = Allocate_long_array(num_symbols + 1);
  long *frequency_count = Allocate_long_array(num_symbols + 1);
  long *row_size = Allocate_long_array(max_la_state + 1);
  printf("\n");
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
    toutput->ordered_state[state_no] = state_no;
    row_size[state_no] = 0;
    struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
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
    struct reduce_header_type red = srt->reduce[state_no];
    short default_rule = red.map[0].rule_number;
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
    toutput->ordered_state[state_no] = state_no;
    row_size[state_no] = 0;
    struct shift_header_type sh = srt->shift[lastats[state_no].shift_number];
    for (int i = 1; i <= sh.size; i++) {
      row_size[state_no]++;
      long symbol = sh.map[i].symbol;
      frequency_count[symbol]++;
    }
    struct reduce_header_type red = lastats[state_no].reduce;
    short default_rule = red.map[0].rule_number;
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
  long last_symbol;
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
  long j = num_terminals + 1;
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
    toutput->symbol_map[symbol] = k;
  }
  toutput->symbol_map[DEFAULT_SYMBOL] = DEFAULT_SYMBOL;
  // Process the remaining non-terminal and useless terminal symbols.
  for (; j <= num_symbols; j++) {
    k++;
    long symbol = frequency_symbol[j];
    is_terminal[k] = false;
    toutput->symbol_map[symbol] = k;
  }
  eoft_image = toutput->symbol_map[eoft_image];
  if (error_maps_bit) {
    error_image = toutput->symbol_map[error_image];
    eolt_image = toutput->symbol_map[eolt_image];
  }
  //    All symbol entries in the state automaton are updated based on
  // the new mapping of the symbols.
  // The states are sorted in descending order based on the number of
  // actions defined on them.
  for ALL_STATES3(state_no) {
    struct goto_header_type go_to = statset[state_no].go_to;
    // Remap Goto map
    for (int i = 1; i <= go_to.size; i++) {
      go_to.map[i].symbol = toutput->symbol_map[go_to.map[i].symbol];
    }
    struct reduce_header_type red = srt->reduce[state_no];
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol = toutput->symbol_map[red.map[i].symbol];
    }
  }
  for ALL_LA_STATES3(state_no) {
    struct reduce_header_type red = lastats[state_no].reduce;
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol = toutput->symbol_map[red.map[i].symbol];
    }
  }
  for (int i = 1; i <= num_shift_maps; i++) {
    struct shift_header_type sh = srt->shift[i];
    for (int j = 1; j <= sh.size; j++) {
      sh.map[j].symbol = toutput->symbol_map[sh.map[j].symbol];
    }
  }
  sortdes(toutput->ordered_state, row_size, 1, max_la_state, num_symbols);
  ffree(frequency_symbol);
  ffree(frequency_count);
  ffree(row_size);
  return (struct DefaultSaves) {
    .default_saves = default_saves,
    .last_symbol = last_symbol,
  };
}

/// We now overlap the State automaton table, or more precisely,  we
/// compute the starting position in a vector where each of its rows
/// may be placed without clobbering elements in another row.
/// The starting positions are stored in the vector STATE_INDEX.
static void overlap_tables(struct CLIOptions *cli_options, struct TableOutput* toutput, bool* is_terminal, struct DefaultSaves default_saves, struct CTabsProps* ctp, long last_symbol, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, struct lastats_type *lastats, struct statset_type *statset) {
  long *symbol_list = Allocate_long_array(num_symbols + 1);
  num_entries -= default_saves.default_saves;
  ctp->increment_size = MAX(num_entries * increment / 100, num_symbols + 1);
  ctp->table_size = MIN(num_entries + ctp->increment_size, MAX_TABLE_SIZE);
  // Allocate space for table, and initialize the AVAIL_POOL list.
  // The variable FIRST_INDEX keeps track of the first element in the
  // doubly-linked list, and LAST_ELEMENT keeps track of the last
  // element in the list.
  // The variable MAX_INDX is used to keep track of the maximum
  // starting position for a row that has been used.
  np->next = Allocate_long_array(ctp->table_size + 1);
  np->previous = Allocate_long_array(ctp->table_size + 1);
  ia->first_index = 1;
  np->next[ia->first_index] = ia->first_index + 1; /* Should be constant-folded */
  np->previous[ia->first_index] = NIL;
  for (long indx = 2; indx < (int) ctp->table_size; indx++) {
    np->next[indx] = indx + 1;
    np->previous[indx] = indx - 1;
  }
  ia->last_index = ctp->table_size;
  np->previous[ia->last_index] = ia->last_index - 1;
  np->next[ia->last_index] = NIL;
  long max_indx = ia->first_index;
  // We now iterate over all the states in their new sorted order as
  // indicated by the variable STATE_NO, and determine an "overlap"
  // position for them.
  for (int k = 1; k <= max_la_state; k++) {
    const long state_no = toutput->ordered_state[k];
    // First, we iterate over all actions defined in STATE_NO, and
    // create a set with all the symbols involved.
    int root_symbol = NIL;
    struct shift_header_type sh;
    struct reduce_header_type red;
    if (state_no > num_states) {
      sh = srt->shift[lastats[state_no].shift_number];
      red = lastats[state_no].reduce;
    } else {
      const struct goto_header_type go_to = statset[state_no].go_to;
      for (int i = 1; i <= go_to.size; i++) {
        int symbol = go_to.map[i].symbol;
        symbol_list[symbol] = root_symbol;
        root_symbol = symbol;
      }
      sh = srt->shift[statset[state_no].shift_number];
      red = srt->reduce[state_no];
    }
    for (int i = 1; i <= sh.size; i++) {
      int symbol = sh.map[i].symbol;
      symbol_list[symbol] = root_symbol;
      root_symbol = symbol;
    }
    symbol_list[0] = root_symbol;
    root_symbol = 0;
    short default_rule = red.map[0].rule_number;
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
    long indx = ia->first_index;
  look_for_match_in_table:
    if (indx == NIL) {
      indx = ctp->table_size + 1;
    }
    if (indx + num_symbols > (int) ctp->table_size) {
      reallocate(cli_options, ctp, np, ia);
    }
    for (int symbol = root_symbol; symbol != NIL; symbol = symbol_list[symbol]) {
      if (np->next[indx + symbol] == OMEGA) {
        indx = np->next[indx];
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
    toutput->state_index[state_no] = indx;
    for (int symbol = root_symbol; symbol != NIL; symbol = symbol_list[symbol]) {
      const long i = indx + symbol;
      if (ia->first_index == ia->last_index) {
        ia->first_index = NIL;
      } else if (i == ia->first_index) {
        ia->first_index = np->next[ia->first_index];
        np->previous[ia->first_index] = NIL;
      } else if (i == ia->last_index) {
        ia->last_index = np->previous[ia->last_index];
        np->next[ia->last_index] = NIL;
      } else {
        np->next[np->previous[i]] = np->next[i];
        np->previous[np->next[i]] = np->previous[i];
      }
      np->next[i] = OMEGA;
    }
  }
  // Update all global counters, and compute ACCEPT_ACTION and
  // ERROR_ACTION.
  ctp->table_size = max_indx + num_symbols;
  ia->accept_act = max_indx + num_rules + 1;
  ia->error_act = ia->accept_act + 1;
  for (ctp->action_size = ctp->table_size; ctp->action_size >= max_indx; ctp->action_size--) {
    if (np->next[ctp->action_size] == OMEGA) {
      break;
    }
  }
  printf("\n");
  PRNT3("Length of Check table: %ld", ctp->table_size);
  PRNT3("Length of Action table: %ld", ctp->action_size);
  PRNT3("Number of entries in Action Table: %ld", num_entries);
  const long percentage = (ctp->action_size - num_entries) * 1000 / num_entries;
  PRNT3("Percentage of increase: %ld.%ld%%", percentage / 10, percentage % 10);
  long num_bytes;
  if (cli_options->byte_bit) {
    num_bytes = 2 * ctp->action_size + ctp->table_size;
    if (!cli_options->goto_default_bit && !cli_options->nt_check_bit) {
      for (; last_symbol >= 1 && !is_terminal[last_symbol]; last_symbol--) {
      }
    }
    PRNT3("Highest symbol in Check Table: %ld", last_symbol);
    if (last_symbol > 255) {
      num_bytes += ctp->table_size;
    }
  } else {
    num_bytes = 2 * (ctp->action_size + ctp->table_size);
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

/// In this routine we compress the State tables and write them out
/// to a file.  The emphasis here is in generating tables that allow
/// fast access. The terminal and non-terminal tables are compressed
/// together, to achieve maximum speed efficiency.
/// Otherwise, the compression technique used in this table is
/// analogous to the technique used in the routine CMPRSPA.
void cmprtim(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, long *scope_right_side, struct lastats_type *lastats, long *gotodef, short *gd_index, short *gd_range, short *scope_state, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table) {
  bool *is_terminal = Allocate_boolean_array(num_symbols + 1);
  struct DefaultSaves default_saves = remap_symbols(toutput, is_terminal, srt, lastats, statset);
  overlap_tables(cli_options, toutput, is_terminal, default_saves, ctp, default_saves.last_symbol, np, ia, srt, lastats, statset);
  print_time_parser(cli_options, toutput, dss, ctp, of, np, scope, ia, srt, scope_right_side, lastats, gotodef, gd_index, gd_range, rules, scope_state, statset, item_table);
}
