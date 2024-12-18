#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include "lpgparse.h"
#include "common.h"

// region ctabs
const int increment = 30;

struct OutputPtr {
  char **output_ptr;
  char **output_buffer;
};

/// The following macro definitions are used only in processing the output.
static void BUFFER_CHECK(FILE *file, struct OutputPtr output_ptr2) {
  if (IOBUFFER_SIZE - ((*output_ptr2.output_ptr) - &(*output_ptr2.output_buffer)[0]) < 73) {
    fwrite((*output_ptr2.output_buffer), sizeof(char), (*output_ptr2.output_ptr) - &(*output_ptr2.output_buffer)[0], file);
    (*output_ptr2.output_ptr) = &(*output_ptr2.output_buffer)[0];
  }
}

const long MAX_TABLE_SIZE = (USHRT_MAX < INT_MAX ? USHRT_MAX : INT_MAX) - 1;

struct ByteTerminalRange {
  bool value;
};

/// ITOC takes as arguments an integer NUM. NUM is an integer containing at
/// most 11 digits which is converted into a character string and placed in
/// the iobuffer. Leading zeros are eliminated and if the number is
/// negative, a leading "-" is added.
static void itoc(const int num, struct OutputPtr output_ptr2) {
  char tmp[12];
  long val = ABS(num);
  tmp[11] = '\0';
  char *p = &tmp[11];
  do {
    p--;
    *p = "0123456789"[val % 10];
    val /= 10;
  } while (val > 0);
  if (num < 0) {
    p--;
    *p = '-';
  }
  while (*p != '\0') {
    *(*output_ptr2.output_ptr)++ = *p++;
  }
}

static void padline(struct OutputPtr output_ptr2) {
  for (int i = 0; i < 12; i++) {
    *(*output_ptr2.output_ptr)++ = ' ';
  }
}

static void mystrcpy(const char *str, struct OutputFiles* of, struct OutputPtr output_ptr2) {
  while (*str != '\0') {
    *(*output_ptr2.output_ptr)++ = *str++;
  }
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  BUFFER_CHECK(of->syssym, output_ptr2);
}

static void prnt_longs(const char *title, const int init, const int bound, const int perline, ArrayLong array, struct CLIOptions *cli_options, struct OutputFiles* of, struct OutputPtr output_ptr2) {
  mystrcpy(title, of, output_ptr2);
  padline(output_ptr2);
  int k = 0;
  for (int i = init; i <= bound; i++) {
    itoc(array.raw[i], output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
    k++;
    if (k == perline && i != bound) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 0;
    }
  }
  if (k != 0) {
    *((*output_ptr2.output_ptr) - 1) = '\n';
    BUFFER_CHECK(of->sysdcl, output_ptr2);
  }
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of, output_ptr2);
  } else {
    mystrcpy("                 };\n", of, output_ptr2);
  }
}

/// This procedure computes the range of the ACTION_SYMBOLS map after
/// Optimal Partitioning has been used to compress that map.  Its
/// first argument is an array, STATE_START, that indicates the
/// starting location in the compressed vector for each state.  When
/// a value of STATE_START is negative it indicates that the state in
/// question shares its elements with another state.  Its second
/// argument, STATE_STACK, is an array that contains the elements of
/// the partition created by PARTSET.  Each element of the partition
/// is organized as a circular list where the smallest sets appear
/// first in the list.
static void compute_action_symbols_range(const ArrayLong state_start, const ArrayLong state_stack, const ArrayLong state_list, ArrayLong action_symbols_range, struct SRTable* srt, struct statset_type *statset) {
  ArrayShort symbol_list = Allocate_short_array2(num_symbols + 1);
  // We now write out the range elements of the ACTION_SYMBOLS map.
  // Recall that if STATE_START has a negative value, then the set in
  // question is sharing elements and does not need to be processed.
  int k = 0;
  for ALL_SYMBOLS3(j) {
    symbol_list.raw[j] = OMEGA; /* Initialize all links to OMEGA */
  }
  for ALL_STATES3(state_no) {
    const int state_no__ = state_list.raw[state_no];
    if (state_start.raw[state_no__] > 0) {
      int symbol_root = 0; /* Add "fence" element: 0 to list */
      symbol_list.raw[symbol_root] = NIL;
      // Pop a state from the stack,  and add each of its elements
      // that has not yet been processed into the list.
      // Continue until stack is empty...
      // Recall that the stack is represented by a circular queue.
      int state;
      for (bool end_node = (state = state_no__) == NIL; !end_node; end_node = state == state_no__) {
        state = state_stack.raw[state];
        const struct shift_header_type sh = srt->shift[statset[state].shift_number];
        for (int j = 1; j <= sh.size; j++) {
          int symbol = sh.map[j].symbol;
          if (symbol_list.raw[symbol] == OMEGA) {
            symbol_list.raw[symbol] = symbol_root;
            symbol_root = symbol;
          }
        }
        const struct reduce_header_type red = srt->reduce[state];
        for (int j = 1; j <= red.size; j++) {
          int symbol = red.map[j].symbol;
          if (symbol_list.raw[symbol] == OMEGA) {
            symbol_list.raw[symbol] = symbol_root;
            symbol_root = symbol;
          }
        }
      }
      // Write the list out.
      for (int symbol = symbol_root; symbol != NIL; symbol = symbol_root) {
        symbol_root = symbol_list.raw[symbol];
        symbol_list.raw[symbol] = OMEGA;
        action_symbols_range.raw[k++] = symbol;
      }
    }
  }
  ffree(symbol_list.raw);
}

/// This procedure computes the range of the NACTION_SYMBOLS map. It
/// organization is analoguous to COMPUTE_ACTION_SYMBOLS_RANGE.
static void compute_naction_symbols_range(const ArrayLong state_start, const ArrayLong state_stack, const ArrayLong state_list, ArrayLong naction_symbols_range, ArrayShort gd_index, ArrayShort gd_range) {
  ArrayShort symbol_list = Allocate_short_array2(num_symbols + 1);
  // We now write out the range elements of the NACTION_SYMBOLS map.
  // Recall that if STATE_START has a negative value, then the set in
  // question is sharing elements and does not need to be processed.
  int k = 0;
  for ALL_SYMBOLS3(j) {
    symbol_list.raw[j] = OMEGA; /* Initialize all links to OMEGA */
  }
  for ALL_STATES3(state_no) {
    const int state_no__ = state_list.raw[state_no];
    if (state_start.raw[state_no__] > 0) {
      int symbol_root = 0; /* Add "fence" element: 0 to list */
      symbol_list.raw[symbol_root] = NIL;
      // Pop a state from the stack,  and add each of its elements
      // that has not yet been processed into the list.
      // Continue until stack is empty...
      // Recall that the stack is represented by a circular queue.
      int state;
      for (bool end_node = (state = state_no__) == NIL; !end_node; end_node = state == state_no__) {
        state = state_stack.raw[state];
        for (int j = gd_index.raw[state]; j <= gd_index.raw[state + 1] - 1; j++) {
          int symbol = gd_range.raw[j];
          if (symbol_list.raw[symbol] == OMEGA) {
            symbol_list.raw[symbol] = symbol_root;
            symbol_root = symbol;
          }
        }
      }
      // Write the list out.
      for (int symbol = symbol_root; symbol != NIL; symbol = symbol_root) {
        symbol_root = symbol_list.raw[symbol];
        symbol_list.raw[symbol] = OMEGA;
        naction_symbols_range.raw[k++] = symbol;
      }
    }
  }
  ffree(symbol_list.raw);
}

static void exit_file(FILE **file, char *file_tag, struct CLIOptions *cli_options) {
  if (cli_options->c_bit || cli_options->cpp_bit) {
    fprintf(*file, "\n#endif /* %s_INCLUDED */\n", file_tag);
  }
}

static void print_error_maps(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct ByteTerminalRange* btr, struct scope_type *scope, struct SRTable* srt, ArrayLong scope_right_side, struct statset_type *statset, ArrayShort gd_index, ArrayShort gd_range, ArrayShort scope_state, struct itemtab *item_table, struct symno_type *symno, bool error_maps_bit, struct ScopeCounter* sc, struct OutputPtr output_ptr2) {
  ArrayLong state_start = Allocate_long_array2(num_states + 2);
  ArrayLong state_stack = Allocate_long_array2(num_states + 1);
  PRNT("\nError maps storage:");
  // We now construct a bit map for the set of terminal symbols that
  // may appear in each state. Then, we invoke PARTSET to apply the
  // Partition Heuristic and print it.
  ArrayLong as_size = Allocate_long_array2(num_states + 1);
  ArrayLong original;
  if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    original = Allocate_long_array2(num_symbols + 1);
    // In a compressed TIME table, the terminal and non-terminal
    // symbols are mixed together when they are remapped.
    // We shall now recover the original number associated with
    // each terminal symbol since it lies very nicely in the
    // range 1..NUM_TERMINALS.  This will save a considerable
    // amount of space in the bit_string representation of sets
    // as well as time when operations are performed on those
    // bit-strings.
    for ALL_TERMINALS3(symbol) {
      original.raw[toutput->symbol_map.raw[symbol]] = symbol;
    }
  }
  JBitset action_symbols;
  if (error_maps_bit) {
    calloc0_set(action_symbols, num_states + 1, dss->term_set_size);
  }
  for ALL_STATES3(state_no) {
    struct shift_header_type sh;
    struct reduce_header_type red;
    sh = srt->shift[statset[state_no].shift_number];
    as_size.raw[state_no] = sh.size;
    for (int i = 1; i <= sh.size; i++) {
      int symbol;
      if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
        symbol = original.raw[sh.map[i].symbol];
      } else {
        symbol = sh.map[i].symbol;
      }
      SET_BIT_IN(action_symbols, state_no, symbol);
    }
    red = srt->reduce[state_no];
    as_size.raw[state_no] += red.size;
    for (int i = 1; i <= red.size; i++) {
      int symbol;
      if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
        symbol = original.raw[red.map[i].symbol];
      } else {
        symbol = red.map[i].symbol;
      }
      SET_BIT_IN(action_symbols, state_no, symbol);
    }
  }
  partset(action_symbols, as_size, toutput->state_list, state_start, state_stack, num_terminals, false);
  ffree(action_symbols.raw);
  // Compute and write out the base of the ACTION_SYMBOLS map.
  ArrayLong action_symbols_base = Allocate_long_array2(num_states + 1);
  for ALL_STATES3(state_no) {
    action_symbols_base.raw[toutput->state_list.raw[state_no]] = ABS(state_start.raw[toutput->state_list.raw[state_no]]);
  }
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char asb[] = {0,\n", 1, num_states, 10, action_symbols_base, cli_options, of, output_ptr2);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER asb[] = {0,\n", 1, num_states, 10, action_symbols_base, cli_options, of, output_ptr2);
  }
  ffree(action_symbols_base.raw);
  // Compute and write out the range of the ACTION_SYMBOLS map.
  int offset = state_start.raw[num_states + 1];
  ArrayLong action_symbols_range = Allocate_long_array2(offset);
  compute_action_symbols_range(state_start, state_stack, toutput->state_list, action_symbols_range, srt, statset);
  for (int i = 0; i < offset - 1; i++) {
    if (action_symbols_range.raw[i] > (cli_options->java_bit ? 127 : 255)) {
      btr->value = 0;
      break;
    }
  }
  if (btr->value) {
    if (cli_options->java_bit) {
      prnt_longs("\n    public final static byte asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range, cli_options, of, output_ptr2);
    } else {
      prnt_longs("\nconst unsigned char  CLASS_HEADER asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range, cli_options, of, output_ptr2);
    }
  } else {
    if (cli_options->java_bit) {
      prnt_longs("\n    public final static char asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range, cli_options, of, output_ptr2);
    } else {
      prnt_longs("\nconst unsigned short CLASS_HEADER asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range, cli_options, of, output_ptr2);
    }
  }
  long num_bytes = 2 * num_states;
  PRNT3("    Storage required for ACTION_SYMBOLS_BASE map: %ld Bytes", num_bytes);
  if (cli_options->table_opt.value == OPTIMIZE_TIME.value && ctp->last_terminal <= (cli_options->java_bit ? 127 : 255)) {
    num_bytes = offset - 1;
  } else if (cli_options->table_opt.value != OPTIMIZE_TIME.value && num_terminals <= (cli_options->java_bit ? 127 : 255)) {
    num_bytes = offset - 1;
  } else {
    num_bytes = 2 * (offset - 1);
  }
  PRNT3("    Storage required for ACTION_SYMBOLS_RANGE map: %ld Bytes", num_bytes);
  ffree(action_symbols_range.raw);
  JBitset naction_symbols;
  if (error_maps_bit) {
    calloc0_set(naction_symbols, num_states + 1, dss->non_term_set_size);
  }
  // We now repeat the same process for the domain of the GOTO table.
  for ALL_STATES3(state_no) {
    as_size.raw[state_no] = gd_index.raw[state_no + 1] - gd_index.raw[state_no];
    for (int i = gd_index.raw[state_no]; i <= gd_index.raw[state_no + 1] - 1; i++) {
      int symbol = gd_range.raw[i] - num_terminals;
      SET_BIT_IN(naction_symbols, state_no, symbol);
    }
  }
  partset(naction_symbols, as_size, toutput->state_list, state_start, state_stack, num_non_terminals, false);
  ffree(as_size.raw);
  ffree(naction_symbols.raw);
  // Remap non-terminals
  for (int i = 1; i <= gotodom_size; i++) {
    if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
      gd_range.raw[i] = toutput->symbol_map.raw[gd_range.raw[i]] - num_terminals;
    } else {
      gd_range.raw[i] = toutput->symbol_map.raw[gd_range.raw[i]];
    }
  }
  // Compute and write out the base of the NACTION_SYMBOLS map.
  ArrayLong naction_symbols_base = Allocate_long_array2(num_states + 1);
  for ALL_STATES3(state_no) {
    naction_symbols_base.raw[toutput->state_list.raw[state_no]] = ABS(state_start.raw[toutput->state_list.raw[state_no]]);
  }
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char nasb[] = {0,\n", 1, num_states, 10, naction_symbols_base, cli_options, of, output_ptr2);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER nasb[] = {0,\n", 1, num_states, 10, naction_symbols_base, cli_options, of, output_ptr2);
  }
  ffree(naction_symbols_base.raw);
  // Compute and write out the range of the NACTION_SYMBOLS map.
  offset = state_start.raw[num_states + 1];
  ArrayLong naction_symbols_range = Allocate_long_array2(offset);
  compute_naction_symbols_range(state_start, state_stack, toutput->state_list, naction_symbols_range, gd_index, gd_range);
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char nasr[] = {0,\n", 0, offset - 2, 10, naction_symbols_range, cli_options, of, output_ptr2);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER nasr[] = {0,\n", 0, offset - 2, 10, naction_symbols_range, cli_options, of, output_ptr2);
  }
  PRNT3("    Storage required for NACTION_SYMBOLS_BASE map: %ld Bytes", 2 * num_states);
  PRNT3("    Storage required for NACTION_SYMBOLS_RANGE map: %d Bytes", 2 * (offset - 1));
  ffree(naction_symbols_range.raw);
  // We write the name_index of each terminal symbol.  The array TEMP
  // is used to remap the NAME_INDEX values based on the new symbol
  // numberings. If time tables are requested, the terminals and non-
  // terminals are mixed together.
  ArrayLong temp = Allocate_long_array2(num_symbols + 1);
  if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
    for ALL_TERMINALS3(symbol) {
      temp.raw[toutput->symbol_map.raw[symbol]] = symno[symbol].name_index;
    }
    if (num_names <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte terminal_index[] = {0,\n", 1, num_terminals, 10, temp, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER terminal_index[] = {0,\n", 1, num_terminals, 10, temp, cli_options, of, output_ptr2);
      }
      num_bytes = num_terminals;
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char terminal_index[] = {0,\n", 1, num_terminals, 10, temp, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER terminal_index[] = {0,\n", 1, num_terminals, 10, temp, cli_options, of, output_ptr2);
      }
      num_bytes = 2 * num_terminals;
    }
    // Compute and list space required for TERMINAL_INDEX map.
    PRNT3("    Storage required for TERMINAL_INDEX map: %ld Bytes", num_bytes);
    // We write the name_index of each non_terminal symbol. The array
    // TEMP is used to remap the NAME_INDEX values based on the new
    // symbol numberings.
    for ALL_NON_TERMINALS3(symbol) {
      temp.raw[toutput->symbol_map.raw[symbol]] = symno[symbol].name_index;
    }
    if (num_names <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
      }
      num_bytes = num_non_terminals;
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
      }
      num_bytes = 2 * num_non_terminals;
    }
    // Compute and list space required for NON_TERMINAL_INDEX map.
    PRNT3("    Storage required for NON_TERMINAL_INDEX map: %ld Bytes", num_bytes);
  } else {
    for ALL_SYMBOLS3(symbol) {
      temp.raw[toutput->symbol_map.raw[symbol]] = symno[symbol].name_index;
    }
    if (num_names <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte symbol_index[] = {0,\n", 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
        mystrcpy("    public final static byte terminal_index[] = symbol_index;\n", of, output_ptr2);
        mystrcpy("    public final static byte non_terminal_index[] = symbol_index;\n", of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER symbol_index[] = {0,\n", 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
        mystrcpy("const unsigned char  *CLASS_HEADER terminal_index[] = &(symbol_index[0]);\n", of, output_ptr2);
        mystrcpy("const unsigned char  *CLASS_HEADER non_terminal_index[] = &(symbol_index[0]);\n", of, output_ptr2);
      }
      num_bytes = num_symbols;
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char symbol_index[] = {0,\n", 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
        mystrcpy("    public final static char terminal_index[] = symbol_index[0];\n", of, output_ptr2);
        mystrcpy("    public final static char non_terminal_index[] = symbol_index;\n", of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER symbol_index[] = {0,\n", 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
        mystrcpy("const unsigned short *CLASS_HEADER terminal_index[] = &(symbol_index[0]);\n", of, output_ptr2);
        mystrcpy("const unsigned short *CLASS_HEADER non_terminal_index[] = &(symbol_index[0]);\n", of, output_ptr2);
      }
      num_bytes = 2 * num_symbols;
    }
    // Compute and list space required for SYMBOL_INDEX map.
    PRNT3("    Storage required for SYMBOL_INDEX map: %ld Bytes", num_bytes);
  }
  if (sc->num_scopes > 0) {
    short root = 0;
    ArrayShort list = Allocate_short_array2(sc->scope_rhs_size + 1);
    for (int i = 1; i <= sc->scope_rhs_size; i++) {
      if (scope_right_side.raw[i] != 0) {
        scope_right_side.raw[i] = toutput->symbol_map.raw[scope_right_side.raw[i]];
      }
    }
    for (int i = 1; i <= sc->num_scopes; i++) {
      scope[i].look_ahead = toutput->symbol_map.raw[scope[i].look_ahead];
      if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
        scope[i].lhs_symbol = toutput->symbol_map.raw[scope[i].lhs_symbol] - num_terminals;
      } else {
        scope[i].lhs_symbol = toutput->symbol_map.raw[scope[i].lhs_symbol];
      }
    }
    // Mark all elements of prefix strings.
    for (int i = 1; i <= sc->scope_rhs_size; i++) {
      list.raw[i] = -1;
    }
    for (int i = 1; i <= sc->num_scopes; i++) {
      if (list.raw[scope[i].suffix] < 0) {
        list.raw[scope[i].suffix] = root;
        root = scope[i].suffix;
      }
    }
    for (; root != 0; root = list.raw[root]) {
      for (int j = root; scope_right_side.raw[j] != 0; j++) {
        int k = scope_right_side.raw[j];
        scope_right_side.raw[j] = temp.raw[k];
      }
    }
    ffree(list.raw);
  }
  if (cli_options->java_bit) {
    // Print java names.
    long num_bytes = 0;
    long max_name_length = 0;
    mystrcpy("\n    public final static String name[] = { null,\n", of, output_ptr2);
    for (int i = 1; i <= num_names; i++) {
      char tok[SYMBOL_SIZE + 1];
      strcpy(tok, RETRIEVE_NAME(i));
      const int len = strlen(tok);
      num_bytes += len * 2;
      if (max_name_length < len) {
        max_name_length = len;
      }
      padline(output_ptr2);
      *(*output_ptr2.output_ptr)++ = '\"';
      int k = 0;
      for (int j = 0; j < len; j++) {
        if (tok[j] == '\"' || tok[j] == '\\') {
          *(*output_ptr2.output_ptr)++ = '\\';
        }
        if (tok[j] == '\n') {
          *(*output_ptr2.output_ptr)++ = cli_options->escape;
        } else {
          *(*output_ptr2.output_ptr)++ = tok[j];
        }
        k++;
        if (k == 30 && j != len - 1) {
          k = 0;
          *(*output_ptr2.output_ptr)++ = '\"';
          *(*output_ptr2.output_ptr)++ = ' ';
          *(*output_ptr2.output_ptr)++ = '+';
          *(*output_ptr2.output_ptr)++ = '\n';
          BUFFER_CHECK(of->sysdcl, output_ptr2);
          padline(output_ptr2);
          *(*output_ptr2.output_ptr)++ = '\"';
        }
      }
      *(*output_ptr2.output_ptr)++ = '\"';
      if (i < num_names) {
        *(*output_ptr2.output_ptr)++ = ',';
      }
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    // Compute and list space required for STRING_BUFFER map.
    PRNT3("    Storage required for STRING_BUFFER map: %ld Bytes", num_bytes);
  } else {
    // Print C names.
    ArrayLong name_len = Allocate_long_array2(num_names + 1);
    long num_bytes = 0;
    long max_name_length = 0;
    mystrcpy("\nconst char  CLASS_HEADER string_buffer[] = {0,\n", of, output_ptr2);
    int n = 0;
    padline(output_ptr2);
    for (int i = 1; i <= num_names; i++) {
      char tok[SYMBOL_SIZE + 1];
      strcpy(tok, RETRIEVE_NAME(i));
      name_len.raw[i] = strlen(tok);
      num_bytes += name_len.raw[i];
      if (max_name_length < name_len.raw[i]) {
        max_name_length = name_len.raw[i];
      }
      int k = 0;
      for (int j = 0; j < name_len.raw[i]; j++) {
        *(*output_ptr2.output_ptr)++ = '\'';
        if (tok[k] == '\'' || tok[k] == '\\') {
          *(*output_ptr2.output_ptr)++ = '\\';
        }
        if (tok[k] == '\n') {
          *(*output_ptr2.output_ptr)++ = cli_options->escape;
        } else {
          *(*output_ptr2.output_ptr)++ = tok[k];
        }
        k++;
        *(*output_ptr2.output_ptr)++ = '\'';
        *(*output_ptr2.output_ptr)++ = ',';
        n++;
        if (n == 10 && !(i == num_names && j == name_len.raw[i] - 1)) {
          n = 0;
          *(*output_ptr2.output_ptr)++ = '\n';
          BUFFER_CHECK(of->sysdcl, output_ptr2);
          padline(output_ptr2);
        }
      }
    }
    *((*output_ptr2.output_ptr) - 1) = '\n'; /*overwrite last comma*/
    BUFFER_CHECK(of->sysdcl, output_ptr2);
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    // Compute and list space required for STRING_BUFFER map.
    PRNT3("    Storage required for STRING_BUFFER map: %ld Bytes", num_bytes);
    // Write out NAME_START array
    mystrcpy("\nconst unsigned short CLASS_HEADER name_start[] = {0,\n", of, output_ptr2);
    padline(output_ptr2);
    int j = 1;
    int k = 0;
    for (int i = 1; i <= num_names; i++) {
      itoc(j, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      j += name_len.raw[i];
      k++;
      if (k == 10 && i != num_names) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    // Compute and list space required for NAME_START map.
    PRNT3("    Storage required for NAME_START map: %ld Bytes", 2 * num_names);
    // Write out NAME_LENGTH array
    prnt_longs("\nconst unsigned char  CLASS_HEADER name_length[] = {0,\n", 1, num_names, 10, name_len, cli_options, of, output_ptr2);
    // Compute and list space required for NAME_LENGTH map.
    PRNT3("    Storage required for NAME_LENGTH map: %ld Bytes", num_names);
    ffree(name_len.raw);
  }
  if (sc->num_scopes > 0) {
    if (sc->scope_rhs_size <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_prefix[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_prefix[] = {\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_prefix[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_prefix[] = {\n", of, output_ptr2);
      }
    }
    padline(output_ptr2);
    int k = 0;
    for (int i = 1; i <= sc->num_scopes; i++) {
      itoc(scope[i].prefix, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != sc->num_scopes) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    if (sc->scope_rhs_size <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_suffix[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_suffix[] = {\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_suffix[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_suffix[] = {\n", of, output_ptr2);
      }
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= sc->num_scopes; i++) {
      itoc(scope[i].suffix, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != sc->num_scopes) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    if (num_symbols <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_lhs[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_lhs[] = {\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_lhs[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_lhs[] = {\n", of, output_ptr2);
      }
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= sc->num_scopes; i++) {
      itoc(scope[i].lhs_symbol, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != sc->num_scopes) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    if (num_terminals <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_la[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_la[] = {\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_la[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_la[] = {\n", of, output_ptr2);
      }
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= sc->num_scopes; i++) {
      itoc(scope[i].look_ahead, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != sc->num_scopes) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    if (sc->scope_state_size <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_state_set[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_state_set[] = {\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_state_set[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_state_set[] = {\n", of, output_ptr2);
      }
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= sc->num_scopes; i++) {
      itoc(scope[i].state_set, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != sc->num_scopes) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    if (num_symbols <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte scope_rhs[] = {0,\n", 1, sc->scope_rhs_size, 10, scope_right_side, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER scope_rhs[] = {0,\n", 1, sc->scope_rhs_size, 10, scope_right_side, cli_options, of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char scope_rhs[] = {0,\n", 1, sc->scope_rhs_size, 10, scope_right_side, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER scope_rhs[] = {0,\n", 1, sc->scope_rhs_size, 10, scope_right_side, cli_options, of, output_ptr2);
      }
    }
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char scope_state[] = {0,\n", of, output_ptr2);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER scope_state[] = {0,\n", of, output_ptr2);
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= sc->scope_state_size; i++) {
      if (scope_state.raw[i] == 0) {
        itoc(0, output_ptr2);
      } else {
        itoc(toutput->state_index.raw[scope_state.raw[i]] + num_rules, output_ptr2);
      }
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != sc->scope_state_size) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    if (num_symbols <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte in_symb[] = {0,\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER in_symb[] = {0,\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char in_symb[] = {0,\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER in_symb[] = {0,\n", of, output_ptr2);
      }
    }
    // Transition symbol
    padline(output_ptr2);
    *(*output_ptr2.output_ptr)++ = '0';
    *(*output_ptr2.output_ptr)++ = ',';
    k = 1;
    for (int state_no = 2; state_no <= num_states; state_no++) {
      struct node *q;
      q = statset[state_no].kernel_items;
      int i;
      if (q != NULL) {
        int item_no;
        item_no = q->value - 1;
        i = item_table[item_no].symbol;
      } else {
        i = 0;
      }
      itoc(toutput->symbol_map.raw[i], output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && state_no != num_states) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
  }
}

static void common(const bool byte_check_bit, struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, ArrayLong scope_right_side, struct statset_type *statset, ArrayShort gd_index, ArrayShort gd_range, ArrayShort scope_state, struct itemtab *item_table, struct ScopeCounter* sc, char *output_buffer, struct OutputPtr output_ptr2) {
  struct ByteTerminalRange btr = (struct ByteTerminalRange) {
    .value = true
  };
  // Write table common.
  {
    if (cli_options->error_maps_bit) {
      print_error_maps(cli_options, toutput, dss, ctp, of, &btr, scope, srt, scope_right_side, statset, gd_index, gd_range, scope_state, item_table, symno, cli_options->error_maps_bit, sc, output_ptr2);
    }
    if (byte_check_bit) {
      // Do nothing.
    } else {
      if (cli_options->java_bit) {
        PRNT("\n***Warning: Base Check vector contains value > 127. 16-bit words used.");
      } else {
        PRNT("\n***Warning: Base Check vector contains value > 255. 16-bit words used.");
      }
    }
    if (btr.value) {
      // Do nothing.
    } else {
      if (cli_options->java_bit) {
        PRNT("***Warning: Terminal symbol > 127. 16-bit words used.");
      } else {
        PRNT("***Warning: Terminal symbol > 255. 16-bit words used.");
      }
    }
    if (cli_options->java_bit) {
      mystrcpy("}\n", of, output_ptr2);
    }
    fwrite(output_buffer, sizeof(char), (*output_ptr2.output_ptr) - &output_buffer[0], of->sysdcl);
  }

  // Print symbols.
  {
    int line_size = SYMBOL_SIZE + /* max length of a token symbol  */
              2 * MAX_PARM_SIZE + /* max length of prefix + suffix */
              64;
    char line[line_size]; /* +64 for error messages lines  */
    // or other fillers(blank, =,...)
    if (cli_options->java_bit) {
      strcpy(line, "interface ");
      strcat(line, of->sym_tag);
      strcat(line, "\n{\n    public final static int\n");
    } else {
      strcpy(line, "enum {\n");
    }
    // We write the terminal symbols map.
    for ALL_TERMINALS3(symbol) {
      char *tok = RETRIEVE_STRING(symbol);
      fprintf(of->syssym, "%s", line);
      if (tok[0] == '\n' || tok[0] == cli_options->escape) {
        tok[0] = cli_options->escape;
        PRNT4(line, line_size, "Escaped symbol %s is an invalid C variable.\n", tok);
      } else if (strpbrk(tok, "!%^&*()-+={}[];:\"`~|\\,.<>/?\'") != NULL) {
        PRNT4(line, line_size, "%s may be an invalid variable name.\n", tok);
      }
      snprintf(line, sizeof(line), "      %s%s%s = %li,\n", cli_options->prefix, tok, cli_options->suffix, toutput->symbol_map.raw[symbol]);
      if (cli_options->c_bit || cli_options->cpp_bit) {
        while (strlen(line) > PARSER_LINE_SIZE) {
          fwrite(line, sizeof(char), PARSER_LINE_SIZE - 2, of->syssym);
          fprintf(of->syssym, "\\\n");
          memmove(line, &line[PARSER_LINE_SIZE - 2], strlen(&line[PARSER_LINE_SIZE - 2]) + 1);
        }
      }
    }
    line[strlen(line) - 2] = '\0'; /* remove the string ",\n" from last line */
    fprintf(of->syssym, "%s%s", line, cli_options->java_bit ? ";\n}\n" : "\n     };\n");
  }

  // Print definitions.
  {
    if (cli_options->java_bit) {
      fprintf(of->sysdef, "interface %s\n{\n    public final static int\n\n", of->def_tag);
    } else {
      fprintf(of->sysdef, "enum {\n");
    }
    if (cli_options->error_maps_bit) {
      if (cli_options->java_bit) {
        fprintf(of->sysdef,
                "      ERROR_SYMBOL      = %d,\n"
                "      NUM_STATES        = %ld,\n\n",
                error_image,
                num_states);
      } else {
        fprintf(of->sysdef,
                "      ERROR_SYMBOL      = %d,\n"
                "      MAX_DISTANCE      = %d,\n"
                "      MIN_DISTANCE      = %d,\n"
                "      NUM_STATES        = %ld,\n\n",
                error_image,
                cli_options->maximum_distance,
                cli_options->minimum_distance,
                num_states);
      }
    }
    if (cli_options->java_bit) {
      fprintf(of->sysdef,
              "      NT_OFFSET         = %ld,\n"
              "      SCOPE_UBOUND      = %ld,\n"
              "      SCOPE_SIZE        = %ld,\n"
              "      LA_STATE_OFFSET   = %ld,\n"
              "      MAX_LA            = %d,\n"
              "      NUM_RULES         = %ld,\n"
              "      NUM_TERMINALS     = %ld,\n"
              "      NUM_NON_TERMINALS = %ld,\n"
              "      NUM_SYMBOLS       = %ld,\n"
              "      START_STATE       = %ld,\n"
              "      EOFT_SYMBOL       = %d,\n"
              "      EOLT_SYMBOL       = %d,\n"
              "      ACCEPT_ACTION     = %ld,\n"
              "      ERROR_ACTION      = %ld;\n"
              "};\n\n",
              cli_options->table_opt.value == OPTIMIZE_SPACE.value ? num_terminals : num_symbols,
              sc->num_scopes - 1,
              sc->num_scopes,
              cli_options->read_reduce_bit && cli_options->lalr_level > 1
                ? ia->error_act + num_rules
                : ia->error_act,
              cli_options->lalr_level,
              num_rules,
              num_terminals,
              num_non_terminals,
              num_symbols,
              toutput->state_index.raw[1] + num_rules,
              eoft_image,
              eolt_image,
              ia->accept_act,
              ia->error_act);
    } else {
      fprintf(of->sysdef,
              "      NT_OFFSET         = %ld,\n"
              "      BUFF_UBOUND       = %d,\n"
              "      BUFF_SIZE         = %d,\n"
              "      STACK_UBOUND      = %d,\n"
              "      STACK_SIZE        = %d,\n"
              "      SCOPE_UBOUND      = %ld,\n"
              "      SCOPE_SIZE        = %ld,\n"
              "      LA_STATE_OFFSET   = %ld,\n"
              "      MAX_LA            = %d,\n"
              "      NUM_RULES         = %ld,\n"
              "      NUM_TERMINALS     = %ld,\n"
              "      NUM_NON_TERMINALS = %ld,\n"
              "      NUM_SYMBOLS       = %ld,\n"
              "      START_STATE       = %ld,\n"
              "      EOFT_SYMBOL       = %d,\n"
              "      EOLT_SYMBOL       = %d,\n"
              "      ACCEPT_ACTION     = %ld,\n"
              "      ERROR_ACTION      = %ld\n"
              "     };\n\n",
              cli_options->table_opt.value == OPTIMIZE_SPACE.value ? num_terminals : num_symbols,
              cli_options->maximum_distance + cli_options->lalr_level - 1,
              cli_options->maximum_distance + cli_options->lalr_level,
              cli_options->stack_size - 1,
              cli_options->stack_size,
              sc->num_scopes - 1,
              sc->num_scopes,
              cli_options->read_reduce_bit && cli_options->lalr_level > 1
                ? ia->error_act + num_rules
                : ia->error_act,
              cli_options->lalr_level,
              num_rules,
              num_terminals,
              num_non_terminals,
              num_symbols,
              toutput->state_index.raw[1] + num_rules,
              eoft_image,
              eolt_image,
              ia->accept_act,
              ia->error_act);
    }
  }

  // Print externs.
  {
    if (cli_options->c_bit || cli_options->cpp_bit) {
      fprintf(of->sysprs,
              "%s SCOPE_REPAIR\n"
              "%s FULL_DIAGNOSIS\n"
              "%s SPACE_TABLES\n\n",
              sc->num_scopes > 0 ? "#define" : "#undef ",
              cli_options->error_maps_bit ? "#define" : "#undef ",
              cli_options->table_opt.value == OPTIMIZE_SPACE.value ? "#define" : "#undef ");
    }
    if (cli_options->c_bit) {
      fprintf(of->sysprs,
              "#define original_state(state) (-%s[state])\n"
              "#define asi(state)            asb[original_state(state)]\n"
              "#define nasi(state)           nasb[original_state(state)]\n"
              "#define in_symbol(state)      in_symb[original_state(state)]\n\n",
              cli_options->table_opt.value == OPTIMIZE_TIME.value ? "check" : "base_check");
    } else if (cli_options->cpp_bit) {
      fprintf(of->sysprs,
              "class LexStream;\n\n"
              "class %s_table\n"
              "{\n"
              "public:\n", of->prs_tag);
      if (cli_options->error_maps_bit) {
        fprintf(of->sysprs, "    static int original_state(int state) { return -%s[state]; }\n", cli_options->table_opt.value == OPTIMIZE_TIME.value ? "check" : "base_check");
      }
      if (cli_options->error_maps_bit) {
        fprintf(of->sysprs,
                "    static int asi(int state) "
                "{ return asb[original_state(state)]; }\n"
                "    static int nasi(int state) "
                "{ return nasb[original_state(state)]; }\n");
        if (sc->num_scopes > 0) {
          fprintf(of->sysprs,
                  "    static int in_symbol(int state) "
                  "{ return in_symb[original_state(state)]; }\n");
        }
      }
      fprintf(of->sysprs, "\n");
    } else if (cli_options->java_bit) {
      fprintf(of->sysprs, "abstract class %s extends %s implements %s\n{\n", of->prs_tag, of->dcl_tag, of->def_tag);
      if (cli_options->error_maps_bit) {
        fprintf(of->sysprs, "    public final static int original_state(int state) { return -%s(state); }\n", cli_options->table_opt.value == OPTIMIZE_TIME.value ? "check" : "base_check");
        if (cli_options->error_maps_bit) {
          fprintf(of->sysprs, "    public final static int asi(int state) { return asb[original_state(state)]; }\n");
          fprintf(of->sysprs, "    static int nasi(int state) { return nasb[original_state(state)]; }\n");
          if (sc->num_scopes > 0)
            fprintf(of->sysprs, "    public final static int in_symbol(int state) { return in_symb[original_state(state)]; }\n");
        }
        fprintf(of->sysprs, "\n");
      }
    }
    if (cli_options->c_bit || cli_options->cpp_bit) {
      fprintf(of->sysprs, "%s const unsigned char  rhs[];\n", cli_options->c_bit ? "extern" : "    static");
      if (ctp->check_size > 0 || cli_options->table_opt.value == OPTIMIZE_TIME.value) {
        const bool small = byte_check_bit && !cli_options->error_maps_bit;
        fprintf(of->sysprs, "%s const %s check_table[];\n"
                "%s const %s *%s;\n",
                cli_options->c_bit ? "extern" : "    static",
                small ? "unsigned char " : "  signed short",
                cli_options->c_bit ? "extern" : "    static",
                small ? "unsigned char " : "  signed short",
                cli_options->table_opt.value == OPTIMIZE_TIME.value ? "check" : "base_check");
      }
      fprintf(of->sysprs, "%s const unsigned short lhs[];\n"
              "%s const unsigned short *%s;\n",
              cli_options->c_bit ? "extern" : "    static",
              cli_options->c_bit ? "extern" : "    static",
              cli_options->table_opt.value == OPTIMIZE_TIME.value ? "action" : "base_action");
      if (cli_options->goto_default_bit) {
        fprintf(of->sysprs, "%s const unsigned short default_goto[];\n", cli_options->c_bit ? "extern" : "    static");
      }
      if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
        fprintf(of->sysprs, "%s const unsigned %s term_check[];\n", cli_options->c_bit ? "extern" : "    static", num_terminals <= (cli_options->java_bit ? 127 : 255) ? "char " : "short");
        fprintf(of->sysprs, "%s const unsigned short term_action[];\n", cli_options->c_bit ? "extern" : "    static");
        if (cli_options->shift_default_bit) {
          fprintf(of->sysprs, "%s const unsigned short default_reduce[];\n", cli_options->c_bit ? "extern" : "    static");
          fprintf(of->sysprs, "%s const unsigned short shift_state[];\n", cli_options->c_bit ? "extern" : "    static");
          fprintf(of->sysprs, "%s const unsigned %s shift_check[];\n", cli_options->c_bit ? "extern" : "    static", num_terminals <= (cli_options->java_bit ? 127 : 255) ? "char " : "short");
          fprintf(of->sysprs, "%s const unsigned short default_shift[];\n", cli_options->c_bit ? "extern" : "    static");
        }
      }
      if (cli_options->error_maps_bit) {
        fprintf(of->sysprs,
                "\n"
                "%s const unsigned short asb[];\n"
                "%s const unsigned %s asr[];\n"
                "%s const unsigned short nasb[];\n"
                "%s const unsigned short nasr[];\n"
                "%s const unsigned short name_start[];\n"
                "%s const unsigned char  name_length[];\n"
                "%s const          char  string_buffer[];\n",
                cli_options->c_bit ? "extern" : "    static",
                cli_options->c_bit ? "extern" : "    static",
                btr.value <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                cli_options->c_bit ? "extern" : "    static",
                cli_options->c_bit ? "extern" : "    static",
                cli_options->c_bit ? "extern" : "    static",
                cli_options->c_bit ? "extern" : "    static",
                cli_options->c_bit ? "extern" : "    static");
        if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
          fprintf(of->sysprs,
                  "%s const unsigned %s terminal_index[];\n"
                  "%s const unsigned %s non_terminal_index[];\n",
                  cli_options->c_bit ? "extern" : "    static",
                  num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short");
        } else {
          fprintf(of->sysprs, "%s const unsigned %s symbol_index[];\n"
                  "%s const unsigned %s *terminal_index;\n"
                  "%s const unsigned %s *non_terminal_index;\n",
                  cli_options->c_bit ? "extern" : "    static",
                  num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short");
        }
        if (sc->num_scopes > 0) {
          fprintf(of->sysprs, "%s const unsigned %s scope_prefix[];\n"
                  "%s const unsigned %s scope_suffix[];\n"
                  "%s const unsigned %s scope_lhs[];\n"
                  "%s const unsigned %s scope_la[];\n"
                  "%s const unsigned %s scope_state_set[];\n"
                  "%s const unsigned %s scope_rhs[];\n"
                  "%s const unsigned short scope_state[];\n"
                  "%s const unsigned %s in_symb[];\n",
                  cli_options->c_bit ? "extern" : "    static",
                  sc->scope_rhs_size <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  sc->scope_rhs_size <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  num_symbols <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  num_terminals <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  sc->scope_state_size <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  num_symbols <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  cli_options->c_bit ? "extern" : "    static",
                  num_symbols <= (cli_options->java_bit ? 127 : 255) ? "char " : "short");
        }
      }
      fprintf(of->sysprs, "\n");
    }
    if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
      if (cli_options->goto_default_bit) {
        // non_terminal_space_action
        {
          if (cli_options->c_bit) {
            fprintf(of->sysprs,
                    "#define nt_action(state, sym) \\\n"
                    "           ((base_check[state + sym] == sym) ? \\\n"
                    "               base_action[state + sym] : "
                    "default_goto[sym])\n\n");
          } else if (cli_options->cpp_bit) {
            fprintf(of->sysprs,
                    "    static int nt_action(int state, int sym)\n"
                    "    {\n"
                    "        return (base_check[state + sym] == sym)\n"
                    "                             ? base_action[state + sym]\n"
                    "                             : default_goto[sym];\n"
                    "    }\n\n");
          } else if (cli_options->java_bit) {
            fprintf(of->sysprs,
                    "    public final static int nt_action(int state, int sym)\n"
                    "    {\n"
                    "        return (base_check(state + sym) == sym)\n"
                    "                             ? base_action[state + sym]\n"
                    "                             : default_goto[sym];\n"
                    "    }\n\n");
          }
        }
      } else {
        // non_terminal_no_goto_default_space_action
        {
          if (cli_options->c_bit) {
            fprintf(of->sysprs,
                    "#define nt_action(state, sym) "
                    "base_action[state + sym]\n\n");
          } else if (cli_options->cpp_bit) {
            fprintf(of->sysprs,
                    "    static int nt_action(int state, int sym)\n"
                    "    {\n        return base_action[state + sym];\n    }\n\n");
          } else if (cli_options->java_bit) {
            fprintf(of->sysprs,
                    "    public final static int nt_action(int state, int sym)\n"
                    "    {\n        return base_action[state + sym];\n    }\n\n");
          }
        }
      }
      if (cli_options->lalr_level > 1) {
        if (cli_options->shift_default_bit) {
          // terminal_shift_default_space_lalr_k
          {
            if (cli_options->c_bit) {
              fprintf(of->sysprs,
                      "static int t_action(int state, int sym, TokenObject next_tok)\n"
                      "{\n"
                      "    int act = base_action[state],\n"
                      "          i = act + sym;\n\n"
                      "    if (sym == 0)\n"
                      "        act = ERROR_ACTION;\n"
                      "    else if (term_check[i] == sym)\n"
                      "        act = term_action[i];\n"
                      "    else\n"
                      "    {\n"
                      "        act = term_action[act];\n"
                      "        i = shift_state[act] + sym;\n"
                      "        act = (shift_check[i] == sym ? default_shift[sym]\n"
                      "                                     : default_reduce[act]);\n"
                      "    }\n\n"
                      "    while (act > LA_STATE_OFFSET)\n"
                      "    {\n"
                      "         act -= LA_STATE_OFFSET;\n"
                      "         sym = Class(next_tok);\n"
                      "         i = act + sym;\n"
                      "         if (term_check[i] == sym)\n"
                      "             act = term_action[i];\n"
                      "         else\n"
                      "         {\n"
                      "             act = term_action[act];\n"
                      "             i = shift_state[act] + sym;\n"
                      "             act = (shift_check[i] == sym\n"
                      "                                    ? default_shift[sym]\n"
                      "                                    : default_reduce[act]);\n"
                      "         }\n"
                      "         if (act <= LA_STATE_OFFSET)\n"
                      "             break;\n"
                      "         next_tok = Next(next_tok);\n"
                      "    }\n\n"
                      "    return act;\n"
                      "}\n\n");
            } else if (cli_options->cpp_bit) {
              fprintf(of->sysprs,
                      "    static int t_action(int act, int sym, LexStream *stream)\n"
                      "    {\n"
                      "        act = base_action[act];\n"
                      "        int i = act + sym;\n\n"
                      "        if (sym == 0)\n"
                      "            act = ERROR_ACTION;\n"
                      "        else if (term_check[i] == sym)\n"
                      "            act = term_action[i];\n"
                      "        else\n"
                      "        {\n"
                      "            act = term_action[act];\n"
                      "            i = shift_state[act] + sym;\n"
                      "            act = (shift_check[i] == sym ? default_shift[sym]\n"
                      "                                         : default_reduce[act]);\n"
                      "        }\n\n"
                      "        if (act > LA_STATE_OFFSET)\n"
                      "        {\n"
                      "            for (TokenObject tok = stream -> Peek();\n"
                      "                 ;\n"
                      "                 tok = stream -> Next(tok))\n"
                      "            {\n"
                      "                 act -= LA_STATE_OFFSET;\n"
                      "                 sym = stream -> Kind(tok);\n"
                      "                 i = act + sym;\n"
                      "                 if (term_check[i] == sym)\n"
                      "                     act = term_action[i];\n"
                      "                 else\n"
                      "                 {\n"
                      "                     act = term_action[act];\n"
                      "                     i = shift_state[act] + sym;\n"
                      "                     act = (shift_check[i] == sym\n"
                      "                                            ? default_shift[sym]\n"
                      "                                            : default_reduce[act]);\n"
                      "                 }\n"
                      "                 if (act <= LA_STATE_OFFSET)\n"
                      "                     break;\n"
                      "            }\n"
                      "        }\n\n"
                      "        return act;\n"
                      "    }\n");
            } else if (cli_options->java_bit) {
              fprintf(of->sysprs,
                      "    public final static int t_action(int act, int sym, LexStream stream)\n"
                      "    {\n"
                      "        act = base_action[act];\n"
                      "        int i = act + sym;\n\n"
                      "        if (sym == 0)\n"
                      "            act = ERROR_ACTION;\n"
                      "        else if (term_check[i] == sym)\n"
                      "            act = term_action[i];\n"
                      "        else\n"
                      "        {\n"
                      "            act = term_action[act];\n"
                      "            i = shift_state[act] + sym;\n"
                      "            act = (shift_check[i] == sym ? default_shift[sym]\n"
                      "                                         : default_reduce[act]);\n"
                      "        }\n\n"
                      "        if (act > LA_STATE_OFFSET)\n"
                      "        {\n"
                      "            for (int tok = stream.Peek();\n"
                      "                 ;\n"
                      "                 tok = stream.Next(tok))\n"
                      "            {\n"
                      "                 act -= LA_STATE_OFFSET;\n"
                      "                 sym = stream.Kind(tok);\n"
                      "                 i = act + sym;\n"
                      "                 if (term_check[i] == sym)\n"
                      "                     act = term_action[i];\n"
                      "                 else\n"
                      "                 {\n"
                      "                     act = term_action[act];\n"
                      "                     i = shift_state[act] + sym;\n"
                      "                     act = (shift_check[i] == sym\n"
                      "                                            ? default_shift[sym]\n"
                      "                                            : default_reduce[act]);\n"
                      "                 }\n"
                      "                 if (act <= LA_STATE_OFFSET)\n"
                      "                     break;\n"
                      "            }\n"
                      "        }\n\n"
                      "        return act;\n"
                      "    }\n");
            }
          }
        } else {
          // terminal_space_lalr_k
          {
            if (cli_options->c_bit) {
              fprintf(of->sysprs,
                      "static int t_action(int state, int sym, TokenObject next_tok)\n"
                      "{\n"
                      "    int act = base_action[state],\n"
                      "          i = act + sym;\n\n"
                      "    act = term_action[term_check[i] == sym ? i : act];\n\n"
                      "    if (act > LA_STATE_OFFSET)\n"
                      "    {\n"
                      "        for (;;)\n"
                      "        {\n"
                      "           act -= LA_STATE_OFFSET;\n"
                      "           sym = Class(next_tok);\n"
                      "           i = act + sym;\n"
                      "           act = term_action[term_check[i] == sym ? i : act];\n"
                      "           if (act <= LA_STATE_OFFSET)\n"
                      "               break;\n"
                      "           next_tok = Next(next_tok);\n"
                      "        }\n"
                      "    }\n\n"
                      "    return act;\n"
                      "}\n\n");
            } else if (cli_options->cpp_bit) {
              fprintf(of->sysprs,
                      "    static int t_action(int act, int sym, LexStream *stream)\n"
                      "    {\n"
                      "        act = base_action[act];\n"
                      "        int i = act + sym;\n\n"
                      "        act = term_action[term_check[i] == sym ? i : act];\n\n"
                      "        if (act > LA_STATE_OFFSET)\n"
                      "        {\n"
                      "            for (TokenObject tok = stream -> Peek();\n"
                      "                 ;\n"
                      "                 tok = stream -> Next(tok))\n"
                      "            {\n"
                      "               act -= LA_STATE_OFFSET;\n"
                      "               sym = stream -> Kind(tok);\n"
                      "               i = act + sym;\n"
                      "               act = term_action[term_check[i] == sym ? i : act];\n"
                      "               if (act <= LA_STATE_OFFSET)\n"
                      "                   break;\n"
                      "            } \n"
                      "        }\n\n"
                      "        return act;\n"
                      "    }\n");
            } else if (cli_options->java_bit) {
              fprintf(of->sysprs,
                      "    public final static int t_action(int act, int sym, LexStream stream)\n"
                      "    {\n"
                      "        act = base_action[act];\n"
                      "        int i = act + sym;\n\n"
                      "        act = term_action[term_check[i] == sym ? i : act];\n\n"
                      "        if (act > LA_STATE_OFFSET)\n"
                      "        {\n"
                      "            for (int tok = stream.Peek();\n"
                      "                 ;\n"
                      "                 tok = stream.Next(tok))\n"
                      "            {\n"
                      "               act -= LA_STATE_OFFSET;\n"
                      "               sym = stream.Kind(tok);\n"
                      "               i = act + sym;\n"
                      "               act = term_action[term_check[i] == sym ? i : act];\n"
                      "               if (act <= LA_STATE_OFFSET)\n"
                      "                   break;\n"
                      "            } \n"
                      "        }\n\n"
                      "        return act;\n"
                      "    }\n");
            }
          }
        }
      } else {
        if (cli_options->shift_default_bit) {
          // terminal_shift_default_space_action
          {
            if (cli_options->c_bit) {
              fprintf(of->sysprs,
                      "static int t_action(int state, int sym, TokenObject next_tok)\n"
                      "{\n"
                      "    int i;\n\n"
                      "    if (sym == 0)\n"
                      "        return ERROR_ACTION;\n"
                      "    i = base_action[state];\n"
                      "    if (term_check[i + sym] == sym)\n"
                      "        return term_action[i + sym];\n"
                      "    i = term_action[i];\n"
                      "    return ((shift_check[shift_state[i] + sym] == sym) ?\n"
                      "                 default_shift[sym] : default_reduce[i]);\n"
                      "}\n\n");
            } else if (cli_options->cpp_bit) {
              fprintf(of->sysprs,
                      "    static int t_action(int state, int sym, LexStream *stream)\n"
                      "    {\n"
                      "        if (sym == 0)\n"
                      "            return ERROR_ACTION;\n"
                      "        int i = base_action[state];\n"
                      "        if (term_check[i + sym] == sym)\n"
                      "            return term_action[i + sym];\n"
                      "        i = term_action[i];\n"
                      "        return ((shift_check[shift_state[i] + sym] == sym) ?\n"
                      "                      default_shift[sym] : default_reduce[i]);\n"
                      "    }\n");
            } else if (cli_options->java_bit) {
              fprintf(of->sysprs,
                      "    public final static int t_action(int state, int sym, LexStream stream)\n"
                      "    {\n"
                      "        if (sym == 0)\n"
                      "            return ERROR_ACTION;\n"
                      "        int i = base_action[state];\n"
                      "        if (term_check[i + sym] == sym)\n"
                      "            return term_action[i + sym];\n"
                      "        i = term_action[i];\n"
                      "        return ((shift_check[shift_state[i] + sym] == sym) ?\n"
                      "                      default_shift[sym] : default_reduce[i]);\n"
                      "    }\n");
            }
          }
        } else {
          // terminal_space_action
          {
            if (cli_options->c_bit) {
              fprintf(of->sysprs,
                      "#define t_action(state, sym, next_tok) \\\n"
                      "  term_action[term_check[base_action[state]+sym] == sym ? \\\n"
                      "          base_action[state] + sym : base_action[state]]\n\n");
            } else if (cli_options->cpp_bit) {
              fprintf(of->sysprs,
                      "    static int t_action(int state, int sym, LexStream *stream)\n"
                      "    {\n"
                      "        return term_action[term_check[base_action[state]"
                      "+sym] == sym\n"
                      "                               ? base_action[state] + sym\n"
                      "                               : base_action[state]];\n"
                      "    }\n");
            } else if (cli_options->java_bit) {
              fprintf(of->sysprs,
                      "    public final static int t_action(int state, int sym, LexStream stream)\n"
                      "    {\n"
                      "        return term_action[term_check[base_action[state]"
                      "+sym] == sym\n"
                      "                               ? base_action[state] + sym\n"
                      "                               : base_action[state]];\n"
                      "    }\n");
            }
          }
        }
      }
    } else {
      if (cli_options->goto_default_bit) {
        // non_terminal_time_action
        {
          if (cli_options->c_bit) {
            fprintf(of->sysprs,
                    "#define nt_action(state, sym) \\\n"
                    "           ((check[state+sym] == sym) ? \\\n"
                    "                   action[state + sym] : "
                    "default_goto[sym])\n\n");
          } else if (cli_options->cpp_bit) {
            fprintf(of->sysprs,
                    "    static int nt_action(int state, int sym)\n"
                    "    {\n"
                    "        return (check[state + sym] == sym)\n"
                    "                                    ? action[state + sym]\n"
                    "                                    : default_goto[sym];\n"
                    "    }\n\n");
          } else if (cli_options->java_bit) {
            fprintf(of->sysprs,
                    "    public final static int nt_action(int state, int sym)\n"
                    "    {\n"
                    "        return (check(state + sym) == sym)\n"
                    "                                    ? action[state + sym]\n"
                    "                                    : default_goto[sym];\n"
                    "    }\n\n");
          }
        }
      } else {
        // non_terminal_no_goto_default_time_action
        {
          if (cli_options->c_bit) {
            fprintf(of->sysprs,
                    "#define nt_action(state, sym) action[state + sym]\n\n");
          } else if (cli_options->cpp_bit) {
            fprintf(of->sysprs,
                    "    static int nt_action(int state, int sym)\n"
                    "    {\n        return action[state + sym];\n    }\n\n");
          } else if (cli_options->java_bit) {
            fprintf(of->sysprs,
                    "    public final static int nt_action(int state, int sym)\n"
                    "    {\n        return action[state + sym];\n    }\n\n");
          }
        }
      }
      if (cli_options->lalr_level > 1) {
        // terminal_time_lalr_k
        {
          if (cli_options->c_bit) {
            fprintf(of->sysprs,
                    "static int t_action(int act, int sym, TokenObject next_tok)\n"
                    "{\n"
                    "    int i = act + sym;\n\n"
                    "    act = action[check[i] == sym ? i : act];\n\n"
                    "    if (act > LA_STATE_OFFSET)\n"
                    "    {\n"
                    "        for (;;)\n"
                    "        {\n"
                    "            act -= ERROR_ACTION;\n"
                    "            sym = Class(next_tok);\n"
                    "            i = act + sym;\n"
                    "            act = action[check[i] == sym ? i : act];\n"
                    "            if (act <= LA_STATE_OFFSET)\n"
                    "                break;\n"
                    "            next_tok = Next(next_tok);\n"
                    "        }\n"
                    "    }\n\n"
                    "    return act;\n"
                    "}\n\n");
          } else if (cli_options->cpp_bit) {
            fprintf(of->sysprs,
                    "    static int t_action(int act, int sym, LexStream *stream)\n"
                    "    {\n"
                    "        int i = act + sym;\n\n"
                    "        act = action[check[i] == sym ? i : act];\n\n"
                    "        if (act > LA_STATE_OFFSET)\n"
                    "        {\n"
                    "            for (TokenObject tok = stream -> Peek();\n"
                    "                 ;\n"
                    "                 tok = stream -> Next(tok))\n"
                    "            {\n"
                    "                act -= ERROR_ACTION;\n"
                    "                sym = stream -> Kind(tok);\n"
                    "                i = act + sym;\n"
                    "                act = action[check[i] == sym ? i : act];\n"
                    "                if (act <= LA_STATE_OFFSET)\n"
                    "                    break;\n"
                    "            }\n"
                    "        }\n\n"
                    "        return act;\n"
                    "    }\n\n");
          } else if (cli_options->java_bit) {
            fprintf(of->sysprs,
                    "    public final static int t_action(int act, int sym, LexStream stream)\n"
                    "    {\n"
                    "        int i = act + sym;\n\n"
                    "        act = action[check(i) == sym ? i : act];\n\n"
                    "        if (act > LA_STATE_OFFSET)\n"
                    "        {\n"
                    "            for (int tok = stream.Peek();\n"
                    "                 ;\n"
                    "                 tok = stream.Next(tok))\n"
                    "            {\n"
                    "                act -= ERROR_ACTION;\n"
                    "                sym = stream.Kind(tok);\n"
                    "                i = act + sym;\n"
                    "                act = action[check(i) == sym ? i : act];\n"
                    "                if (act <= LA_STATE_OFFSET)\n"
                    "                    break;\n"
                    "            }\n"
                    "        }\n\n"
                    "        return act;\n"
                    "    }\n\n");
          }
        }
      } else {
        // terminal_time_action
        {
          if (cli_options->c_bit) {
            fprintf(of->sysprs,
                    "#define t_action(state, sym, next_tok) \\\n"
                    "   action[check[state + sym] == sym ? state + sym : state]\n\n");
          } else if (cli_options->cpp_bit) {
            fprintf(of->sysprs,
                    "    static int t_action(int state, int sym, LexStream *stream)\n"
                    "    {\n"
                    "        return action[check[state + sym] == sym"
                    " ? state + sym : state];\n"
                    "    }\n");
          } else if (cli_options->java_bit) {
            fprintf(of->sysprs,
                    "    public final static int t_action(int state, int sym, LexStream stream)\n"
                    "    {\n"
                    "        return action[check(state + sym) == sym"
                    " ? state + sym : state];\n"
                    "    }\n");
          }
        }
      }
    }
    if (cli_options->cpp_bit) {
      fprintf(of->sysprs, "};\n");
    } else if (cli_options->java_bit) {
      fprintf(of->sysprs, "}\n");
    }
  }

  // Exit parser files.
  {
    exit_file(&of->sysdcl, of->dcl_tag, cli_options);
    exit_file(&of->syssym, of->sym_tag, cli_options);
    exit_file(&of->sysdef, of->def_tag, cli_options);
    exit_file(&of->sysprs, of->prs_tag, cli_options);
    fclose(of->sysdcl);
    fclose(of->syssym);
    fclose(of->sysdef);
    fclose(of->sysprs);
  }
}

/// SORTDES sorts the elements of ARRAY and COUNT in the range LOW..HIGH
/// based on the values of the elements of COUNT. Knowing that the maximum
/// value of the elements of count cannot exceed MAX and cannot be lower
/// than zero, we can use a bucket sort technique.
void sortdes(ArrayLong array, ArrayLong count, const long low, const long high, const long max) {
  // BUCKET is used to hold the roots of lists that contain the
  // elements of each bucket.  LIST is used to hold these lists.
  ArrayLong bucket = Allocate_long_array2(max + 1);
  ArrayLong list = Allocate_long_array2(high - low + 1);
  for (int i = 0; i <= max; i++) {
    bucket.raw[i] = NIL;
  }
  // We now partition the elements to be sorted and place them in their
  // respective buckets.  We iterate backward over ARRAY and COUNT to
  // keep the sorting stable since elements are inserted in the buckets
  // in stack-fashion.
  //
  //   NOTE that it is known that the values of the elements of ARRAY
  // also lie in the range LOW..HIGH.
  for (long i = high; i >= low; i--) {
    const long k = count.raw[i];
    const long element = array.raw[i];
    list.raw[element - low] = bucket.raw[k];
    bucket.raw[k] = element;
  }
  // Iterate over each bucket, and place elements in ARRAY and COUNT
  // in sorted order.  The iteration is done backward because we want
  // the arrays sorted in descending order.
  long k = low;
  for (long i = max; i >= 0; i--) {
    for (long element = bucket.raw[i]; element != NIL; element = list.raw[element - low], k++) {
      array.raw[k] = element;
      count.raw[k] = i;
    }
  }
  ffree(bucket.raw);
  ffree(list.raw);
}

/// This procedure is invoked when the TABLE being used is not large
/// enough. A new table is allocated, the information from the old table
/// is copied, and the old space is released.
void reallocate(struct CLIOptions *cli_options, struct CTabsProps* ctp, struct NextPrevious* np, struct ImportantAspects* ia) {
  if (ctp->table_size == MAX_TABLE_SIZE) {
    PRNTERR2("Table has exceeded maximum limit of %ld", MAX_TABLE_SIZE);
    exit(12);
  }
  const int old_size = ctp->table_size;
  ctp->table_size = MIN(ctp->table_size + ctp->increment_size, MAX_TABLE_SIZE);
  if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    PRNT3("Reallocating storage for TIME table, adding %ld entries", ctp->table_size - old_size);
  } else {
    PRNT3("Reallocating storage for SPACE table, adding %ld entries", ctp->table_size - old_size);
  }
  ArrayLong n = Allocate_long_array2(ctp->table_size + 1);
  ArrayLong p = Allocate_long_array2(ctp->table_size + 1);
  // Copy old information
  for (int i = 1; i <= old_size; i++) {
    n.raw[i] = np->next.raw[i];
    p.raw[i] = np->previous.raw[i];
  }
  ffree(np->next.raw);
  ffree(np->previous.raw);
  np->next = n;
  np->previous = p;
  if (ia->first_index == NIL) {
    ia->first_index = old_size + 1;
    np->previous.raw[ia->first_index] = NIL;
  } else {
    np->next.raw[ia->last_index] = old_size + 1;
    np->previous.raw[old_size + 1] = ia->last_index;
  }
  np->next.raw[old_size + 1] = old_size + 2;
  for (int i = old_size + 2; i < (int) ctp->table_size; i++) {
    np->next.raw[i] = i + 1;
    np->previous.raw[i] = i - 1;
  }
  ia->last_index = ctp->table_size;
  np->next.raw[ia->last_index] = NIL;
  np->previous.raw[ia->last_index] = ia->last_index - 1;
}

void populate_start_file(FILE **file, char *file_tag, struct CLIOptions *cli_options) {
  if (cli_options->c_bit || cli_options->cpp_bit) {
    fprintf(*file, "#ifndef %s_INCLUDED\n", file_tag);
    fprintf(*file, "#define %s_INCLUDED\n\n", file_tag);
  }
}

void print_space_parser(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, ArrayLong term_state_index, ArrayLong shift_check_index, struct CTabsProps* ctp, struct new_state_type *new_state_element, ArrayShort shift_image, ArrayShort real_shift_number, struct OutputFiles* of, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, ArrayLong scope_right_side, ArrayShort shiftdf, ArrayLong gotodef, ArrayShort gd_index, ArrayShort gd_range, struct ruletab_type *rules, ArrayShort scope_state, struct statset_type *statset, struct itemtab *item_table, struct ScopeCounter* sc, char *output_buffer, struct OutputPtr output_ptr2)  {
  bool byte_check_bit = true;
  populate_start_file(&of->sysdcl, of->dcl_tag, cli_options);
  populate_start_file(&of->syssym, of->sym_tag, cli_options);
  populate_start_file(&of->sysdef, of->def_tag, cli_options);
  populate_start_file(&of->sysprs, of->prs_tag, cli_options);
  int default_count = 0;
  int goto_count = 0;
  int goto_reduce_count = 0;
  int reduce_count = 0;
  int la_shift_count = 0;
  int shift_count = 0;
  int shift_reduce_count = 0;
  ArrayLong check = Allocate_long_array2(ctp->table_size + 1);
  ArrayLong action = Allocate_long_array2(ctp->table_size + 1);
  (*output_ptr2.output_ptr) = &output_buffer[0];
  // Prepare header card with proper information, and write it out.
  long offset = ia->error_act;
  long la_state_offset;
  if (cli_options->lalr_level > 1) {
    if (cli_options->read_reduce_bit) {
      offset += num_rules;
    }
    la_state_offset = offset;
  } else {
    la_state_offset = ia->error_act;
  }
  if (offset > MAX_TABLE_SIZE + 1) {
    PRNTERR2("Table contains entries that are > %ld; Processing stopped.", MAX_TABLE_SIZE + 1);
    exit(12);
  }
  for (int i = 1; i <= ctp->check_size; i++) {
    check.raw[i] = DEFAULT_SYMBOL;
  }
  for (int i = 1; i <= (int) ctp->action_size; i++) {
    action.raw[i] = ia->error_act;
  }
  //    Update the default non-terminal action of each state with the
  // appropriate corresponding terminal state starting index.
  for (int i = 1; i <= ctp->num_terminal_states; i++) {
    int indx = term_state_index.raw[i];
    int state_no = new_state_element[i].image;
    // Update the action link between the non-terminal and terminal
    // tables. If error-maps are requested, an indirect linking is made
    // as follows:
    //  Each non-terminal row identifies its original state number, and
    // a new vector START_TERMINAL_STATE indexable by state numbers
    // identifies the starting point of each state in the terminal table.
    if (state_no <= num_states) {
      for (; state_no != NIL; state_no = toutput->state_list.raw[state_no]) {
        action.raw[toutput->state_index.raw[state_no]] = indx;
      }
    } else {
      for (; state_no != NIL; state_no = toutput->state_list.raw[state_no]) {
        int act = la_state_offset + indx;
        toutput->state_index.raw[state_no] = act;
      }
    }
  }
  //  Now update the non-terminal tables with the non-terminal actions.
  for ALL_STATES3(state_no) {
    struct goto_header_type go_to;
    int indx = toutput->state_index.raw[state_no];
    go_to = statset[state_no].go_to;
    for (int j = 1; j <= go_to.size; j++) {
      int symbol = go_to.map[j].symbol;
      int i = indx + symbol;
      if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
        check.raw[i] = symbol;
      }
      int act = go_to.map[j].action;
      if (act > 0) {
        action.raw[i] = toutput->state_index.raw[act] + num_rules;
        goto_count++;
      } else {
        action.raw[i] = -act;
        goto_reduce_count++;
      }
    }
  }
  if (cli_options->error_maps_bit) {
    if (ctp->check_size == 0) {
      ctp->check_size = ctp->action_size;
      for (int i = 0; i <= ctp->check_size; i++) {
        check.raw[i] = 0;
      }
    }
    for ALL_STATES3(state_no) {
      check.raw[toutput->state_index.raw[state_no]] = -state_no;
    }
  }
  for (int i = 1; i <= ctp->check_size; i++) {
    if (check.raw[i] < 0 || check.raw[i] > (cli_options->java_bit ? 127 : 255)) {
      byte_check_bit = false;
    }
  }
  if (cli_options->c_bit) {
    mystrcpy("\n#define CLASS_HEADER\n\n", of, output_ptr2);
  } else if (cli_options->cpp_bit) {
    mystrcpy("\n#define CLASS_HEADER ", of, output_ptr2);
    mystrcpy(of->prs_tag, of, output_ptr2);
    mystrcpy("_table::\n\n", of, output_ptr2);
  } else {
    mystrcpy("abstract class ", of, output_ptr2);
    mystrcpy(of->dcl_tag, of, output_ptr2);
    mystrcpy(" implements ", of, output_ptr2);
    mystrcpy(of->def_tag, of, output_ptr2);
    mystrcpy("\n{\n", of, output_ptr2);
  }
  // Write size of right hand side of rules followed by CHECK table.
  if (cli_options->java_bit) {
    mystrcpy("    public final static byte rhs[] = {0,\n", of, output_ptr2);
  } else {
    mystrcpy("const unsigned char  CLASS_HEADER rhs[] = {0,\n", of, output_ptr2);
  }
  padline(output_ptr2);
  int k = 0;
  for (int i = 1; i <= num_rules; i++) {
    k++;
    if (k > 15) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 1;
    }
    itoc(RHS_SIZE(i, rules), output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
  }
  *((*output_ptr2.output_ptr) - 1) = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of, output_ptr2);
  } else {
    mystrcpy("                 };\n", of, output_ptr2);
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  if (ctp->check_size > 0) {
    if (byte_check_bit && !cli_options->error_maps_bit) {
      if (cli_options->java_bit) {
        mystrcpy("    public final static byte check_table[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("const unsigned char  CLASS_HEADER check_table[] = {\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("    public final static short check_table[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("const   signed short CLASS_HEADER check_table[] = {\n", of, output_ptr2);
      }
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= ctp->check_size; i++) {
      k++;
      if (k > 10) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 1;
      }
      itoc(check.raw[i], output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
    }
    *((*output_ptr2.output_ptr) - 1) = '\n';
    BUFFER_CHECK(of->sysdcl, output_ptr2);
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                 };\n", of, output_ptr2);
    }
    *(*output_ptr2.output_ptr)++ = '\n';
    if (byte_check_bit && !cli_options->error_maps_bit) {
      if (cli_options->java_bit) {
        mystrcpy("    public final static byte base_check(int i)\n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n", of, output_ptr2);
      } else {
        mystrcpy("const unsigned char  *CLASS_HEADER base_check = &(check_table[0]) - (NUM_RULES + 1);\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("    public final static short base_check(int i) \n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n", of, output_ptr2);
      } else {
        mystrcpy("const   signed short *CLASS_HEADER base_check = &(check_table[0]) - (NUM_RULES + 1);\n", of, output_ptr2);
      }
    }
    *(*output_ptr2.output_ptr)++ = '\n';
  }
  // Write left hand side symbol of rules followed by ACTION table.
  if (cli_options->java_bit) {
    mystrcpy("    public final static char lhs[] = {0,\n", of, output_ptr2);
  } else {
    mystrcpy("const unsigned short CLASS_HEADER lhs[] = {0,\n", of, output_ptr2);
  }
  padline(output_ptr2);
  k = 0;
  for (int i = 1; i <= num_rules; i++) {
    itoc(toutput->symbol_map.raw[rules[i].lhs] - num_terminals, output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
    k++;
    if (k == 15) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 0;
    }
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  *(*output_ptr2.output_ptr)++ = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  padline(output_ptr2);
  k = 0;
  if (cli_options->error_maps_bit) {
    int max_indx;
    max_indx = ia->accept_act - num_rules - 1;
    for (int i = 1; i <= max_indx; i++) {
      check.raw[i] = OMEGA;
    }
    for ALL_STATES3(state_no) {
      check.raw[toutput->state_index.raw[state_no]] = state_no;
    }
    int j = num_states + 1;
    for (int i = max_indx; i >= 1; i--) {
      int state_no = check.raw[i];
      if (state_no != OMEGA) {
        j--;
        toutput->ordered_state.raw[j] = i + num_rules;
        toutput->state_list.raw[j] = state_no;
      }
    }
  }
  for (int i = 1; i <= (int) ctp->action_size; i++) {
    itoc(action.raw[i], output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
    k++;
    if (k == 10 && i != (int) ctp->action_size) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 0;
    }
  }
  if (k != 0) {
    *((*output_ptr2.output_ptr) - 1) = '\n';
    BUFFER_CHECK(of->sysdcl, output_ptr2);
  }
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of, output_ptr2);
  } else {
    mystrcpy("                 };\n", of, output_ptr2);
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  if (cli_options->java_bit) {
    mystrcpy("    public final static char base_action[] = lhs;\n", of, output_ptr2);
  } else {
    mystrcpy("const unsigned short *CLASS_HEADER base_action = lhs;\n", of, output_ptr2);
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  // Initialize the terminal tables,and update with terminal actions.
  for (int i = 1; i <= ctp->term_check_size; i++) {
    check.raw[i] = DEFAULT_SYMBOL;
  }
  for (int i = 1; i <= ctp->term_action_size; i++) {
    action.raw[i] = ia->error_act;
  }
  for (int state_no = 1; state_no <= ctp->num_terminal_states; state_no++) {
    struct shift_header_type sh;
    struct reduce_header_type red;
    int indx = term_state_index.raw[state_no];
    sh = srt->shift[new_state_element[state_no].shift_number];
    for (int j = 1; j <= sh.size; j++) {
      int symbol = sh.map[j].symbol;
      int act = sh.map[j].action;
      if (!cli_options->shift_default_bit || act != shiftdf.raw[symbol]) {
        int i = indx + symbol;
        check.raw[i] = symbol;
        long result_act;
        if (act > num_states) {
          result_act = toutput->state_index.raw[act];
          la_shift_count++;
        } else if (act > 0) {
          result_act = toutput->state_index.raw[act] + num_rules;
          shift_count++;
        } else {
          result_act = -act + ia->error_act;
          shift_reduce_count++;
        }
        if (result_act > MAX_TABLE_SIZE + 1) {
          PRNTERR2("Table contains look-ahead shift entry that is >%ld; Processing stopped.", MAX_TABLE_SIZE + 1);
          return;
        }
        action.raw[i] = result_act;
      }
    }
    red = new_state_element[state_no].reduce;
    for (int j = 1; j <= red.size; j++) {
      int symbol = red.map[j].symbol;
      int rule_no = red.map[j].rule_number;
      int i = indx + symbol;
      check.raw[i] = symbol;
      action.raw[i] = rule_no;
      reduce_count++;
    }
    int rule_no = red.map[0].rule_number;
    if (rule_no != ia->error_act) {
      default_count++;
    }
    check.raw[indx] = DEFAULT_SYMBOL;
    if (cli_options->shift_default_bit) {
      action.raw[indx] = state_no;
    } else {
      action.raw[indx] = rule_no;
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
  if (num_terminals <= (cli_options->java_bit ? 127 : 255)) {
    if (cli_options->java_bit) {
      prnt_longs("\n    public final static byte term_check[] = {0,\n", 1, ctp->term_check_size, 15, check, cli_options, of, output_ptr2);
    } else {
      prnt_longs("\nconst unsigned char  CLASS_HEADER term_check[] = {0,\n", 1, ctp->term_check_size, 15, check, cli_options, of, output_ptr2);
    }
  } else {
    if (cli_options->java_bit) {
      prnt_longs("\n    public final static char term_check[] = {0,\n", 1, ctp->term_check_size, 15, check, cli_options, of, output_ptr2);
    } else {
      prnt_longs("\nconst unsigned short CLASS_HEADER term_check[] = {0,\n", 1, ctp->term_check_size, 15, check, cli_options, of, output_ptr2);
    }
  }
  // Write Terminal Action Table.
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char term_action[] = {0,\n", 1, ctp->term_action_size, 10, action, cli_options, of, output_ptr2);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER term_action[] = {0,\n", 1, ctp->term_action_size, 10, action, cli_options, of, output_ptr2);
  }
  // If GOTO_DEFAULT is requested, we print out the GOTODEF vector.
  if (cli_options->goto_default_bit) {
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char default_goto[] = {0,\n", of, output_ptr2);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER default_goto[] = {0,\n", of, output_ptr2);
    }
    padline(output_ptr2);
    k = 0;
    for ALL_NON_TERMINALS3(symbol) {
      int act = gotodef.raw[symbol];
      long result_act;
      if (act < 0) {
        result_act = -act;
      } else if (act == 0) {
        result_act = ia->error_act;
      } else {
        result_act = toutput->state_index.raw[act] + num_rules;
      }
      itoc(result_act, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && symbol != num_symbols) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                 };\n", of, output_ptr2);
    }
  }
  if (cli_options->shift_default_bit) {
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char default_reduce[] = {0,\n", of, output_ptr2);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER default_reduce[] = {0,\n", of, output_ptr2);
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= ctp->num_terminal_states; i++) {
      struct reduce_header_type red;
      red = new_state_element[i].reduce;
      itoc(red.map[0].rule_number, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != ctp->num_terminal_states) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                 };\n", of, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char shift_state[] = {0,\n", of, output_ptr2);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER shift_state[] = {0,\n", of, output_ptr2);
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= ctp->num_terminal_states; i++) {
      itoc(shift_check_index.raw[shift_image.raw[i]], output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != ctp->num_terminal_states) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                 };\n", of, output_ptr2);
    }
    for (int i = 1; i <= ctp->shift_check_size; i++) {
      check.raw[i] = DEFAULT_SYMBOL;
    }
    for (int i = 1; i <= ctp->shift_domain_count; i++) {
      struct shift_header_type sh;
      int indx = shift_check_index.raw[i];
      sh = srt->shift[real_shift_number.raw[i]];
      for (int j = 1; j <= sh.size; j++) {
        int symbol = sh.map[j].symbol;
        check.raw[indx + symbol] = symbol;
      }
    }
    if (num_terminals <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte shift_check[] = {0,\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER shift_check[] = {0,\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char shift_check[] = {0,\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER shift_check[] = {0,\n", of, output_ptr2);
      }
    }
    padline(output_ptr2);
    k = 0;
    int ii;
    for (ii = 1; ii <= ctp->shift_check_size; ii++) {
      itoc(check.raw[ii], output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && ii != ctp->shift_check_size) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                 };\n", of, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char default_shift[] = {0,\n", of, output_ptr2);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER default_shift[] = {0,\n", of, output_ptr2);
    }
    padline(output_ptr2);
    k = 0;
    for ALL_TERMINALS3(symbol) {
      int act = shiftdf.raw[symbol];
      long result_act;
      if (act < 0) {
        result_act = -act + ia->error_act;
      } else if (act == 0) {
        result_act = ia->error_act;
      } else if (act > num_states) {
        result_act = toutput->state_index.raw[act];
      } else {
        result_act = toutput->state_index.raw[act] + num_rules;
      }
      if (result_act > MAX_TABLE_SIZE + 1) {
        PRNTERR2("Table contains look-ahead shift entry that is >%ld; Processing stopped.", MAX_TABLE_SIZE + 1);
        return;
      }
      itoc(result_act, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && ii != num_terminals) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                 };\n", of, output_ptr2);
    }
  }
  ffree(check.raw);
  ffree(action.raw);
  common(byte_check_bit, cli_options, toutput, dss, ctp, of, scope, ia, srt, scope_right_side, statset, gd_index, gd_range, scope_state, item_table, sc, output_buffer, output_ptr2);
}

void print_time_parser(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, ArrayLong scope_right_side, struct lastats_type *lastats, ArrayLong gotodef, ArrayShort gd_index, ArrayShort gd_range, struct ruletab_type *rules, ArrayShort scope_state, struct statset_type *statset, struct itemtab *item_table, struct ScopeCounter* sc, char *output_buffer, struct OutputPtr output_ptr2) {
  bool byte_check_bit = true;
  populate_start_file(&of->sysdcl, of->dcl_tag, cli_options);
  populate_start_file(&of->syssym, of->sym_tag, cli_options);
  populate_start_file(&of->sysdef, of->def_tag, cli_options);
  populate_start_file(&of->sysprs, of->prs_tag, cli_options);
  int la_shift_count = 0;
  int shift_count = 0;
  int goto_count = 0;
  int default_count = 0;
  int reduce_count = 0;
  int shift_reduce_count = 0;
  int goto_reduce_count = 0;
  (*output_ptr2.output_ptr) = &output_buffer[0];
  ArrayLong check = np->next;
  ArrayLong action = np->previous;
  long offset = ia->error_act;
  int la_state_offset;
  if (cli_options->lalr_level > 1) {
    if (cli_options->read_reduce_bit) {
      offset += num_rules;
    }
    la_state_offset = offset;
  } else {
    la_state_offset = ia->error_act;
  }
  if (offset > MAX_TABLE_SIZE + 1) {
    PRNTERR2(msg_line, "Table contains entries that are > %ld; Processing stopped.", MAX_TABLE_SIZE + 1);
    exit(12);
  }
  // Initialize all unfilled slots with default values.
  // RECALL that the vector "check" is aliased to the vector "next".
  long indx;
  indx = ia->first_index;
  for (long i = indx; i != NIL && i <= ctp->action_size; i = indx) {
    indx = np->next.raw[i];
    check.raw[i] = DEFAULT_SYMBOL;
    action.raw[i] = ia->error_act;
  }
  for (long i = ctp->action_size + 1; i <= ctp->table_size; i++) {
    check.raw[i] = DEFAULT_SYMBOL;
  }
  // We set the rest of the table with the proper table entries.
  for (int state_no = 1; state_no <= max_la_state; state_no++) {
    struct shift_header_type sh;
    struct reduce_header_type red;
    indx = toutput->state_index.raw[state_no];
    if (state_no > num_states) {
      sh = srt->shift[lastats[state_no].shift_number];
      red = lastats[state_no].reduce;
    } else {
      struct goto_header_type go_to = statset[state_no].go_to;
      for (int j = 1; j <= go_to.size; j++) {
        int symbol = go_to.map[j].symbol;
        long i = indx + symbol;
        if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
          check.raw[i] = symbol;
        } else {
          check.raw[i] = DEFAULT_SYMBOL;
        }
        int act = go_to.map[j].action;
        if (act > 0) {
          action.raw[i] = toutput->state_index.raw[act] + num_rules;
          goto_count++;
        } else {
          action.raw[i] = -act;
          goto_reduce_count++;
        }
      }
      sh = srt->shift[statset[state_no].shift_number];
      red = srt->reduce[state_no];
    }
    for (int j = 1; j <= sh.size; j++) {
      int symbol = sh.map[j].symbol;
      long i = indx + symbol;
      check.raw[i] = symbol;
      int act = sh.map[j].action;
      long result_act;
      if (act > num_states) {
        result_act = la_state_offset + toutput->state_index.raw[act];
        la_shift_count++;
      } else if (act > 0) {
        result_act = toutput->state_index.raw[act] + num_rules;
        shift_count++;
      } else {
        result_act = -act + ia->error_act;
        shift_reduce_count++;
      }
      if (result_act > MAX_TABLE_SIZE + 1) {
        PRNTERR2("Table contains look-ahead shift entry that is >%ld; Processing stopped.", MAX_TABLE_SIZE + 1);
        return;
      }
      action.raw[i] = result_act;
    }
    //   We now initialize the elements reserved for reduce actions in
    // the current state.
    short default_rule = red.map[0].rule_number;
    for (int j = 1; j <= red.size; j++) {
      if (red.map[j].rule_number != default_rule) {
        int symbol = red.map[j].symbol;
        long i = indx + symbol;
        check.raw[i] = symbol;
        int act = red.map[j].rule_number;
        if (rules[act].lhs == accept_image) {
          action.raw[i] = ia->accept_act;
        } else {
          action.raw[i] = act;
        }
        reduce_count++;
      }
    }
    //   We now initialize the element reserved for the DEFAULT reduce
    // action of the current state.  If error maps are requested,  the
    // default slot is initialized to the original state number, and the
    // corresponding element of the DEFAULT_REDUCE array is initialized.
    // Otherwise it is initialized to the rule number in question.
    int i = indx + DEFAULT_SYMBOL;
    check.raw[i] = DEFAULT_SYMBOL;
    int act = red.map[0].rule_number;
    if (act == OMEGA) {
      action.raw[i] = ia->error_act;
    } else {
      action.raw[i] = act;
      default_count++;
    }
  }
  PRNT("\n\nActions in Compressed Tables:");
  PRNT3("     Number of Shifts: %d", shift_count);
  PRNT3("     Number of Shift/Reduces: %d", shift_reduce_count);
  if (max_la_state > num_states) {
    snprintf(msg_line, sizeof(msg_line), "     Number of Look-Ahead Shifts: %d", la_shift_count);
    PRNT(msg_line);
  }
  PRNT3("     Number of Gotos: %d", goto_count);
  PRNT3("     Number of Goto/Reduces: %d", goto_reduce_count);
  PRNT3("     Number of Reduces: %d", reduce_count);
  PRNT3("     Number of Defaults: %d", default_count);
  if (cli_options->error_maps_bit) {
    for ALL_STATES3(state_no) {
      check.raw[toutput->state_index.raw[state_no]] = -state_no;
    }
  }
  for (int i = 1; i <= (int) ctp->table_size; i++) {
    if (check.raw[i] < 0 || check.raw[i] > (cli_options->java_bit ? 127 : 255)) {
      byte_check_bit = 0;
    }
  }
  if (cli_options->c_bit) {
    mystrcpy("\n#define CLASS_HEADER\n\n", of, output_ptr2);
  } else if (cli_options->cpp_bit) {
    mystrcpy("\n#define CLASS_HEADER ", of, output_ptr2);
    mystrcpy(of->prs_tag, of, output_ptr2);
    mystrcpy("_table::\n\n", of, output_ptr2);
  } else if (cli_options->java_bit) {
    mystrcpy("abstract class ", of, output_ptr2);
    mystrcpy(of->dcl_tag, of, output_ptr2);
    mystrcpy(" implements ", of, output_ptr2);
    mystrcpy(of->def_tag, of, output_ptr2);
    mystrcpy("\n{\n", of, output_ptr2);
  }
  // Write size of right hand side of rules followed by CHECK table.
  if (cli_options->java_bit) {
    mystrcpy("    public final static byte rhs[] = {0,\n", of, output_ptr2);
  } else {
    mystrcpy("const unsigned char  CLASS_HEADER rhs[] = {0,\n", of, output_ptr2);
  }
  padline(output_ptr2);
  int k = 0;
  for (int i = 1; i <= num_rules; i++) {
    k++;
    if (k > 15) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 1;
    }
    itoc(RHS_SIZE(i, rules), output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
  }
  *((*output_ptr2.output_ptr) - 1) = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of, output_ptr2);
  } else {
    mystrcpy("                 };\n", of, output_ptr2);
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  // Write CHECK table.
  if (byte_check_bit && !cli_options->error_maps_bit) {
    if (cli_options->java_bit) {
      mystrcpy("    public final static byte check_table[] = {\n", of, output_ptr2);
    } else {
      mystrcpy("const unsigned char  CLASS_HEADER check_table[] = {\n", of, output_ptr2);
    }
  } else {
    if (cli_options->java_bit) {
      mystrcpy("     public final static short check_table[] = {\n", of, output_ptr2);
    } else {
      mystrcpy("const   signed short CLASS_HEADER check_table[] = {\n", of, output_ptr2);
    }
  }
  padline(output_ptr2);
  k = 0;
  for (int i = 1; i <= (int) ctp->table_size; i++) {
    k++;
    if (k > 10) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 1;
    }
    itoc(check.raw[i], output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
  }
  *((*output_ptr2.output_ptr) - 1) = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of, output_ptr2);
  } else {
    mystrcpy("                 };\n", of, output_ptr2);
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  if (byte_check_bit && !cli_options->error_maps_bit) {
    if (cli_options->java_bit) {
      mystrcpy("    public final static byte check(int i) \n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n", of, output_ptr2);
    } else {
      mystrcpy("const unsigned char  *CLASS_HEADER check = &(check_table[0]) - (NUM_RULES + 1);\n", of, output_ptr2);
    }
  } else {
    if (cli_options->java_bit) {
      mystrcpy("    public final static short check(int i) \n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n", of, output_ptr2);
    } else {
      mystrcpy("const   signed short *CLASS_HEADER check = &(check_table[0]) - (NUM_RULES + 1);\n", of, output_ptr2);
    }
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  // Write left hand side symbol of rules followed by ACTION table.
  if (cli_options->java_bit) {
    mystrcpy("    public final static char lhs[] = {0,\n", of, output_ptr2);
  } else {
    mystrcpy("const unsigned short CLASS_HEADER lhs[] = {0,\n", of, output_ptr2);
  }
  padline(output_ptr2);
  k = 0;
  for (int i = 1; i <= num_rules; i++) {
    itoc(toutput->symbol_map.raw[rules[i].lhs], output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
    k++;
    if (k == 15) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 0;
    }
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  *(*output_ptr2.output_ptr)++ = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  padline(output_ptr2);
  k = 0;
  if (cli_options->error_maps_bit) {
    long max_indx;
    // Construct a map from new state numbers into original
    //   state numbers using the array check[]
    max_indx = ia->accept_act - num_rules - 1;
    for (int i = 1; i <= max_indx; i++) {
      check.raw[i] = OMEGA;
    }
    for ALL_STATES3(state_no) {
      check.raw[toutput->state_index.raw[state_no]] = state_no;
    }
    int j = num_states + 1;
    for (int i = max_indx; i >= 1; i--) {
      int state_no = check.raw[i];
      if (state_no != OMEGA) {
        toutput->ordered_state.raw[--j] = i + num_rules;
        toutput->state_list.raw[j] = state_no;
      }
    }
  }
  for (int i = 1; i <= (int) ctp->action_size; i++) {
    itoc(action.raw[i], output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
    k++;
    if (k == 10 && i != (int) ctp->action_size) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 0;
    }
  }
  if (k != 0) {
    *((*output_ptr2.output_ptr) - 1) = '\n';
    BUFFER_CHECK(of->sysdcl, output_ptr2);
  }
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of, output_ptr2);
  } else {
    mystrcpy("                 };\n", of, output_ptr2);
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  if (cli_options->java_bit) {
    mystrcpy("    public final static char action[] = lhs;\n", of, output_ptr2);
  } else {
    mystrcpy("const unsigned short *CLASS_HEADER action = lhs;\n", of, output_ptr2);
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  // If GOTO_DEFAULT is requested, we print out the GOTODEF vector.
  if (cli_options->goto_default_bit) {
    ArrayShort default_map = Allocate_short_array2(num_symbols + 1);
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char default_goto[] = {0,\n", of, output_ptr2);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER default_goto[] = {0,\n", of, output_ptr2);
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 0; i <= num_symbols; i++) {
      default_map.raw[i] = ia->error_act;
    }
    for ALL_NON_TERMINALS3(symbol) {
      int act = gotodef.raw[symbol];
      int result_act;
      if (act < 0) {
        result_act = -act;
      } else if (act > 0) {
        result_act = toutput->state_index.raw[act] + num_rules;
      } else {
        result_act = ia->error_act;
      }
      default_map.raw[toutput->symbol_map.raw[symbol]] = result_act;
    }
    for (int symbol = 1; symbol <= num_symbols; symbol++) {
      itoc(default_map.raw[symbol], output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && symbol != num_symbols) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                 };\n", of, output_ptr2);
    }
  }
  ffree(np->next.raw);
  ffree(np->previous.raw);
  common(byte_check_bit, cli_options, toutput, dss, ctp, of, scope, ia, srt, scope_right_side, statset, gd_index, gd_range, scope_state, item_table, sc, output_buffer, output_ptr2);
}

// endregion

// region spacetab

struct NumTableEntries {
  long value;
};

struct TResult {
  long single_root;
  long multi_root;
  long empty_root;
  long top;
};

///  REMAP_NON_TERMINALS remaps the non-terminal symbols and states based on
/// frequency of entries.
static long remap_non_terminals(const struct CLIOptions *cli_options, struct TableOutput *toutput, ArrayLong gotodef, struct statset_type *statset) {
  // The variable FREQUENCY_SYMBOL is used to hold the non-terminals
  // in the grammar, and  FREQUENCY_COUNT is used correspondingly to
  // hold the number of actions defined on each non-terminal.
  // ORDERED_STATE and ROW_SIZE are used in a similar fashion for states
  ArrayLong frequency_symbol = Allocate_long_array2(num_non_terminals);
  // TODO • The size in the arraylong will be wrong?
  frequency_symbol.raw -= num_terminals + 1;
  ArrayLong frequency_count = Allocate_long_array2(num_non_terminals);
  // TODO • The size in the arraylong will be wrong?
  frequency_count.raw -= num_terminals + 1;
  ArrayLong row_size = Allocate_long_array2(num_states + 1);
  for ALL_NON_TERMINALS3(i) {
    frequency_symbol.raw[i] = i;
    frequency_count.raw[i] = 0;
  }
  for ALL_STATES3(state_no) {
    toutput->ordered_state.raw[state_no] = state_no;
    row_size.raw[state_no] = 0;
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      row_size.raw[state_no]++;
      const int symbol = go_to.map[i].symbol;
      frequency_count.raw[symbol]++;
    }
  }
  // The non-terminals are sorted in descending order based on the
  // number of actions defined on then, and they are remapped based on
  // the new arrangement obtained by the sorting.
  sortdes(frequency_symbol, frequency_count, num_terminals + 1, num_symbols, num_states);
  for ALL_NON_TERMINALS3(i) {
    toutput->symbol_map.raw[frequency_symbol.raw[i]] = i;
  }
  //    All non-terminal entries in the state automaton are updated
  // accordingly.  We further subtract NUM_TERMINALS from each
  // non-terminal to make them fall in the range [1..NUM_NON_TERMINLS]
  // instead of [NUM_TERMINALS+1..NUM_SYMBOLS].
  for ALL_STATES3(state_no) {
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      go_to.map[i].symbol = toutput->symbol_map.raw[go_to.map[i].symbol] - num_terminals;
    }
  }
  // If Goto-Default was requested, we find out how many non-terminals
  // were eliminated as a result, and adjust the GOTO-DEFAULT map,
  // based on the new mapping of the non-terminals.
  long last_symbol;
  if (cli_options->goto_default_bit) {
    ArrayLong temp_goto_default = Allocate_long_array2(num_non_terminals);
    temp_goto_default.raw -= num_terminals + 1;
    for (last_symbol = num_symbols; last_symbol > num_terminals; last_symbol--) {
      if (frequency_count.raw[last_symbol] != 0) {
        break;
      }
    }
    last_symbol -= num_terminals;
    PRNT3("Number of non-terminals eliminated: %ld", num_non_terminals - last_symbol);
    // Remap the GOTO-DEFAULT map.
    // to hold the original map.
    for ALL_NON_TERMINALS3(symbol) {
      temp_goto_default.raw[toutput->symbol_map.raw[symbol]] = gotodef.raw[symbol];
    }
    gotodef.raw += num_terminals + 1;
    ffree(gotodef.raw);
    gotodef.raw = temp_goto_default.raw;
  } else {
    last_symbol = num_non_terminals;
  }
  // The states are sorted in descending order based on the number of
  // actions defined on them, and they are remapped based on the new
  // arrangement obtained by the sorting.
  sortdes(toutput->ordered_state, row_size, 1, num_states, last_symbol);
  frequency_symbol.raw += num_terminals + 1;
  ffree(frequency_symbol.raw);
  frequency_count.raw += num_terminals + 1;
  ffree(frequency_count.raw);
  ffree(row_size.raw);
  return last_symbol;
}

/// We now overlap the non-terminal table, or more precisely, we compute the
/// starting position in a vector where each of its rows may be placed
/// without clobbering elements in another row.  The starting positions are
/// stored in the vector STATE_INDEX.
static void overlap_nt_rows(struct CLIOptions *cli_options, struct TableOutput *toutput, struct NumTableEntries *nte, struct CTabsProps *ctp, long last_symbol, struct NextPrevious* np, struct ImportantAspects* ia, struct statset_type *statset) {
  nte->value = num_gotos + num_goto_reduces + num_states;
  ctp->increment_size = MAX(nte->value / 100 * increment, last_symbol + 1);
  ctp->table_size = MIN(nte->value + ctp->increment_size, MAX_TABLE_SIZE);
  // Allocate space for table, and initlaize the AVAIL_POOL list.  The
  // variable FIRST_INDEX keeps track of the first element in the doubly-
  // linked list, and LAST_ELEMENT keeps track of the last element in the
  // list.
  //   The variable MAX_INDX is used to keep track of the maximum starting
  // position for a row that has been used.
  np->next = Allocate_long_array2(ctp->table_size + 1);
  np->previous = Allocate_long_array2(ctp->table_size + 1);
  ia->first_index = 1;
  np->previous.raw[ia->first_index] = NIL;
  np->next.raw[ia->first_index] = ia->first_index + 1;
  for (long indx = 2; indx < (int) ctp->table_size; indx++) {
    np->next.raw[indx] = indx + 1;
    np->previous.raw[indx] = indx - 1;
  }
  ia->last_index = ctp->table_size;
  np->previous.raw[ia->last_index] = ia->last_index - 1;
  np->next.raw[ia->last_index] = NIL;
  int max_indx = ia->first_index;
  // We now iterate over all the states in their new sorted order as
  // indicated by the variable STATE_NO, and determine an "overlap"
  // position for them.
  for ALL_STATES3(state_no) {
    const int state_no__ = toutput->ordered_state.raw[state_no];
    // INDX is set to the beginning of the list of available slots
    // and we try to determine if it might be a valid starting
    // position.  If not, INDX is moved to the next element, and we
    // repeat the process until a valid position is found.
    const struct goto_header_type go_to = statset[state_no__].go_to;
    long indx = ia->first_index;
  look_for_match_in_base_table:
    if (indx == NIL) {
      indx = ctp->table_size + 1;
    }
    if (indx + last_symbol > ctp->table_size) {
      reallocate(cli_options, ctp, np, ia);
    }
    for (int i = 1; i <= go_to.size; i++) {
      if (np->next.raw[indx + go_to.map[i].symbol] == OMEGA) {
        indx = np->next.raw[indx];
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
      if (i == ia->last_index) {
        ia->last_index = np->previous.raw[ia->last_index];
        np->next.raw[ia->last_index] = NIL;
      } else {
        np->next.raw[np->previous.raw[i]] = np->next.raw[i];
        np->previous.raw[np->next.raw[i]] = np->previous.raw[i];
      }
      np->next.raw[i] = OMEGA;
    }
    if (ia->first_index == ia->last_index) {
      ia->first_index = NIL;
    } else if (indx == ia->first_index) {
      ia->first_index = np->next.raw[ia->first_index];
      np->previous.raw[ia->first_index] = NIL;
    } else if (indx == ia->last_index) {
      ia->last_index = np->previous.raw[ia->last_index];
      np->next.raw[ia->last_index] = NIL;
    } else {
      np->next.raw[np->previous.raw[indx]] = np->next.raw[indx];
      np->previous.raw[np->next.raw[indx]] = np->previous.raw[indx];
    }
    np->next.raw[indx] = OMEGA;
    if (indx > max_indx) {
      max_indx = indx;
    }
    toutput->state_index.raw[state_no__] = indx;
  }
  if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
    ctp->check_size = max_indx + num_non_terminals;
  } else {
    ctp->check_size = 0;
  }
  for (ctp->action_size = max_indx + last_symbol; ctp->action_size >= max_indx; ctp->action_size--) {
    if (np->next.raw[ctp->action_size] == OMEGA) {
      break;
    }
  }
  ia->accept_act = max_indx + num_rules + 1;
  ia->error_act = ia->accept_act + 1;
  printf("\n");
  if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
    PRNT3("Length of base Check Table: %d", ctp->check_size);
  }
  PRNT3("Length of base Action Table: %ld", ctp->action_size);
  PRNT3("Number of entries in base Action Table: %ld", nte->value);
  const int percentage = (ctp->action_size - nte->value) * 1000 / nte->value;
  PRNT3("Percentage of increase: %d.%d%%", percentage / 10, percentage % 10);
}

/// We now try to merge states in the terminal table that are similar.
/// Two states S1 and S2 are said to be similar if they contain the
/// same shift actions, and they reduce to the same set of rules.  In
/// addition,  there must not exist a terminal symbol "t" such that:
/// REDUCE(S1, t) and REDUCE(S2, t) are defined, and
/// REDUCE(S1, t) ^= REDUCE(S2, t)
static void merge_similar_t_rows(const struct CLIOptions *cli_options, struct TableOutput *toutput, ArrayBool shift_on_error_symbol, struct node **new_state_element_reduce_nodes, struct TResult *tresult, struct new_state_type *new_state_element, struct SRTable* srt, struct lastats_type *lastats, struct statset_type *statset) {
  ArrayShort table = Allocate_short_array2(num_shift_maps + 1);
  tresult->top = 0;
  for (int i = 1; i <= max_la_state; i++) {
    shift_on_error_symbol.raw[i] = false;
  }
  for (int i = 0; i <= num_shift_maps; i++) {
    table.raw[i] = NIL;
  }
  // We now hash all the states into TABLE, based on their shift map
  // number.
  // The rules in the range of the REDUCE MAP are placed in sorted
  // order in a linear linked list headed by REDUCE_ROOT.
  for (long state_no = 1; state_no <= max_la_state; state_no++) {
    struct node *reduce_root = NULL;
    struct reduce_header_type red;
    if (state_no > num_states) {
      red = lastats[state_no].reduce;
    } else {
      red = srt->reduce[state_no];
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
      if (cli_options->default_opt.value == OPT_5.value) {
        const struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
        for (int j = 1; j <= sh.size && !shift_on_error_symbol.raw[state_no]; j++)
          shift_on_error_symbol.raw[state_no] = sh.map[j].symbol == error_image;
      }
      hash_address = statset[state_no].shift_number;
    }
    int ii;
    for (ii = table.raw[hash_address]; ii != NIL; ii = new_state_element[ii].link) {
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
    if (shift_on_error_symbol.raw[state_no] && reduce_root != NULL) {
      tresult->top++;
      if (ii == NIL) {
        new_state_element[tresult->top].link = table.raw[hash_address];
        table.raw[hash_address] = tresult->top;
      }
      new_state_element[tresult->top].thread = tresult->multi_root;
      tresult->multi_root = tresult->top;
      new_state_element[tresult->top].shift_number = hash_address;
      new_state_element_reduce_nodes[tresult->top] = reduce_root;
      toutput->state_list.raw[state_no] = NIL;
      new_state_element[tresult->top].image = state_no;
    } else if (ii == NIL) {
      tresult->top++;
      new_state_element[tresult->top].link = table.raw[hash_address];
      table.raw[hash_address] = tresult->top;
      if (reduce_root == NULL) {
        new_state_element[tresult->top].thread = tresult->empty_root;
        tresult->empty_root = tresult->top;
      } else if (reduce_root->next == NULL) {
        new_state_element[tresult->top].thread = tresult->single_root;
        tresult->single_root = tresult->top;
      } else {
        new_state_element[tresult->top].thread = tresult->multi_root;
        tresult->multi_root = tresult->top;
      }
      new_state_element[tresult->top].shift_number = hash_address;
      new_state_element_reduce_nodes[tresult->top] = reduce_root;
      toutput->state_list.raw[state_no] = NIL;
      new_state_element[tresult->top].image = state_no;
    } else {
      toutput->state_list.raw[state_no] = new_state_element[ii].image;
      new_state_element[ii].image = state_no;
      struct node *tail;
      for (struct node *r = reduce_root; r != NULL; tail = r, r = r->next) {
      }
      if (reduce_root != NULL) {
        free_nodes(reduce_root, tail);
      }
    }
  }
  ffree(table.raw);
}

///    If shift-default actions are requested, the shift actions
/// associated with each state are factored out of the Action matrix
/// and all identical rows are merged.  This merged matrix is used to
/// create a boolean vector that may be used to confirm whether
/// there is a shift action in a given state S on a given symbol t.
/// If we can determine that there is a shift action on a pair (S, t)
/// we can apply shift default to the Shift actions just like we did
/// for the Goto actions.
static void merge_shift_domains(struct CLIOptions *cli_options, struct TableOutput *toutput, ArrayLong row_size, ArrayLong frequency_symbol, ArrayLong frequency_count, struct NumTableEntries *nte, ArrayLong shift_check_index, struct CTabsProps *ctp, struct new_state_type *new_state_element, ArrayShort shift_image, ArrayShort real_shift_number, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, ArrayShort shiftdf) {
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
  ArrayShort shift_domain_link = Allocate_short_array2(ctp->num_terminal_states + 1);
  ArrayLong ordered_shift = Allocate_long_array2(num_shift_maps + 1);
  ArrayBool shift_symbols = Allocate_bool_array2(num_terminals + 1);
  short shift_domain_table[SHIFT_TABLE_SIZE];
  for (int i = 0; i <= SHIFT_TABLE_UBOUND; i++) {
    shift_domain_table[i] = NIL;
  }
  nte->value = 0;
  ctp->shift_domain_count = 0;
  for (int state_no = 1; state_no <= ctp->num_terminal_states; state_no++) {
    int shift_no = new_state_element[state_no].shift_number;
    for (int i = 1; i <= num_terminals; i++) {
      shift_symbols.raw[i] = false;
    }
    struct shift_header_type sh = srt->shift[shift_no];
    int shift_size = sh.size;
    unsigned long hash_address = shift_size;
    for (int i = 1; i <= shift_size; i++) {
      int symbol = sh.map[i].symbol;
      hash_address += symbol;
      shift_symbols.raw[symbol] = true;
    }
    hash_address %= SHIFT_TABLE_SIZE;
    for (int i = shift_domain_table[hash_address]; i != NIL; i = shift_domain_link.raw[i]) {
      sh = srt->shift[new_state_element[i].shift_number];
      if (sh.size == shift_size) {
        int jj;
        for (jj = 1; jj <= shift_size; jj++) {
          if (!shift_symbols.raw[sh.map[jj].symbol]) {
            break;
          }
        }
        if (jj > shift_size) {
          shift_image.raw[state_no] = shift_image.raw[i];
          goto continu;
        }
      }
    }
    shift_domain_link.raw[state_no] = shift_domain_table[hash_address];
    shift_domain_table[hash_address] = state_no;
    ctp->shift_domain_count++;
    shift_image.raw[state_no] = ctp->shift_domain_count;
    real_shift_number.raw[ctp->shift_domain_count] = shift_no;
    ordered_shift.raw[ctp->shift_domain_count] = ctp->shift_domain_count;
    row_size.raw[ctp->shift_domain_count] = shift_size;
    nte->value = nte->value + shift_size;
  continu:;
  }
  //   Compute the frequencies, and remap the terminal symbols
  // accordingly.
  for ALL_TERMINALS3(symbol) {
    frequency_symbol.raw[symbol] = symbol;
    frequency_count.raw[symbol] = 0;
  }
  for (int i = 1; i <= ctp->shift_domain_count; i++) {
    int shift_no = real_shift_number.raw[i];
    struct shift_header_type sh = srt->shift[shift_no];
    for (int j = 1; j <= sh.size; j++) {
      int symbol = sh.map[j].symbol;
      frequency_count.raw[symbol]++;
    }
  }
  sortdes(frequency_symbol, frequency_count, 1, num_terminals, ctp->shift_domain_count);
  for ALL_TERMINALS3(symbol) {
    toutput->symbol_map.raw[frequency_symbol.raw[symbol]] = symbol;
  }
  toutput->symbol_map.raw[DEFAULT_SYMBOL] = DEFAULT_SYMBOL;
  eoft_image = toutput->symbol_map.raw[eoft_image];
  if (cli_options->error_maps_bit) {
    error_image = toutput->symbol_map.raw[error_image];
    eolt_image = toutput->symbol_map.raw[eolt_image];
  }
  for (int i = 1; i <= num_shift_maps; i++) {
    struct shift_header_type sh = srt->shift[i];
    for (int j = 1; j <= sh.size; j++) {
      sh.map[j].symbol = toutput->symbol_map.raw[sh.map[j].symbol];
    }
  }
  for (int state_no = 1; state_no <= ctp->num_terminal_states; state_no++) {
    struct reduce_header_type red = new_state_element[state_no].reduce;
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
    }
  }
  // If ERROR_MAPS are requested, we also have to remap the original
  // REDUCE maps.
  if (cli_options->error_maps_bit) {
    for ALL_STATES3(state_no) {
      struct reduce_header_type red = srt->reduce[state_no];
      for (int i = 1; i <= red.size; i++) {
        red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
      }
    }
  }
  // Remap the SHIFT_DEFAULT map.
  ArrayShort temp_shift_default = Allocate_short_array2(num_terminals + 1);
  for ALL_TERMINALS3(symbol) {
    temp_shift_default.raw[toutput->symbol_map.raw[symbol]] = shiftdf.raw[symbol];
  }
  ffree(shiftdf.raw);
  shiftdf.raw = temp_shift_default.raw;
  // We now compute the starting position for each Shift check row
  // as we did for the terminal states.  The starting positions are
  // stored in the vector SHIFT_CHECK_INDEX.
  sortdes(ordered_shift, row_size, 1, ctp->shift_domain_count, num_terminals);
  ctp->increment_size = MAX(nte->value / 100 * increment, num_terminals + 1);
  int old_table_size = ctp->table_size;
  ctp->table_size = MIN(nte->value + ctp->increment_size, MAX_TABLE_SIZE);
  if ((int) ctp->table_size > old_table_size) {
    ffree(np->previous.raw);
    ffree(np->next.raw);
    np->previous = Allocate_long_array2(ctp->table_size + 1);
    np->next = Allocate_long_array2(ctp->table_size + 1);
  } else {
    ctp->table_size = old_table_size;
  }
  ia->first_index = 1;
  np->previous.raw[ia->first_index] = NIL;
  np->next.raw[ia->first_index] = ia->first_index + 1;
  for (int indx = 2; indx < (int) ctp->table_size; indx++) {
    np->next.raw[indx] = indx + 1;
    np->previous.raw[indx] = indx - 1;
  }
  ia->last_index = ctp->table_size;
  np->previous.raw[ia->last_index] = ia->last_index - 1;
  np->next.raw[ia->last_index] = NIL;
  int max_indx = ia->first_index;
  // Look for a suitable index where to overlay the shift check row.
  for (int k = 1; k <= ctp->shift_domain_count; k++) {
    int shift_no = ordered_shift.raw[k];
    struct shift_header_type sh = srt->shift[real_shift_number.raw[shift_no]];
    int indx = ia->first_index;
  look_for_match_in_sh_chk_tab:
    if (indx == NIL) {
      indx = ctp->table_size + 1;
    }
    if (indx + num_terminals > (int) ctp->table_size) {
      reallocate(cli_options, ctp, np, ia);
    }
    for (int i = 1; i <= sh.size; i++) {
      int symbol = sh.map[i].symbol;
      if (np->next.raw[indx + symbol] == OMEGA) {
        indx = np->next.raw[indx];
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
      if (np->next.raw[i] != 0) {
        if (i == ia->last_index) {
          ia->last_index = np->previous.raw[ia->last_index];
          np->next.raw[ia->last_index] = NIL;
        } else {
          np->next.raw[np->previous.raw[i]] = np->next.raw[i];
          np->previous.raw[np->next.raw[i]] = np->previous.raw[i];
        }
      }
      np->next.raw[i] = OMEGA;
    }
    // We now remove the starting position itself from the list without
    // marking it as taken, since it can still be used for a shift check.
    // MAX_INDX is updated if required.
    // SHIFT_CHECK_INDEX(SHIFT_NO) is properly set to INDX as the
    // starting position of STATE_NO.
    if (ia->first_index == ia->last_index) {
      ia->first_index = NIL;
    } else if (indx == ia->first_index) {
      ia->first_index = np->next.raw[ia->first_index];
      np->previous.raw[ia->first_index] = NIL;
    } else if (indx == ia->last_index) {
      ia->last_index = np->previous.raw[ia->last_index];
      np->next.raw[ia->last_index] = NIL;
    } else {
      np->next.raw[np->previous.raw[indx]] = np->next.raw[indx];
      np->previous.raw[np->next.raw[indx]] = np->previous.raw[indx];
    }
    np->next.raw[indx] = 0;
    if (indx > max_indx) {
      max_indx = indx;
    }
    shift_check_index.raw[shift_no] = indx;
  }
  // Update all counts, and report statistics.
  ctp->shift_check_size = max_indx + num_terminals;
  printf("\n");
  PRNT3("Length of Shift Check Table: %d", ctp->shift_check_size);
  PRNT3("Number of entries in Shift Check Table: %ld", nte->value);
  int kk;
  for (kk = ctp->shift_check_size; kk >= max_indx; kk--) {
    if (np->next.raw[kk] == OMEGA) {
      break;
    }
  }
  long percentage = (kk - nte->value) * 1000 / nte->value;
  PRNT3("Percentage of increase: %ld.%ld%%", percentage/10, percentage % 10);
  ffree(ordered_shift.raw);
  ffree(shift_symbols.raw);
  ffree(shift_domain_link.raw);
}

/// By now, similar states have been grouped together, and placed in
/// one of three linear linked lists headed by the root pointers:
/// MULTI_ROOT, SINGLE_ROOT, and EMPTY_ROOT.
/// We iterate over each of these lists and construct new states out
/// of these groups of similar states when they are compatible. Then,
/// we remap the terminal symbols.
static void overlay_sim_t_rows(struct CLIOptions *cli_options, struct TableOutput *toutput, ArrayBool shift_on_error_symbol, struct node **new_state_element_reduce_nodes, struct TResult *tresult, struct NumTableEntries *nte, ArrayLong shift_check_index, struct CTabsProps *ctp, struct new_state_type *new_state_element, ArrayShort shift_image, ArrayShort real_shift_number, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, struct ruletab_type *rules, struct lastats_type *lastats, ArrayShort shiftdf, struct statset_type *statset) {
  int num_shifts_saved = 0;
  int num_reductions_saved = 0;
  int default_saves = 0;
  ArrayShort rule_count = Allocate_short_array2(num_rules + 1);
  ArrayShort reduce_action = Allocate_short_array2(num_terminals + 1);
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
  for (int i = tresult->multi_root; i != NIL; i = new_state_element[i].thread) {
    for (struct node *q = new_state_element_reduce_nodes[i]; q != NULL; q = q->next) {
      rule_count.raw[q->value] = 0;
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
      red = srt->reduce[state_no];
    }
    for ALL_TERMINALS3(j) {
      reduce_action.raw[j] = OMEGA;
    }
    for (int j = 1; j <= red.size; j++) {
      int rule_no = red.map[j].rule_number;
      reduce_action.raw[red.map[j].symbol] = rule_no;
      rule_count.raw[rule_no]++;
    }
    // STATE_SET_ROOT is used to traverse the rest of the list that form
    // the group of states being processed.  STATE_SUBSET_ROOT is used
    // to construct the new list that will consist of all states in the
    // group that are compatible starting with the initial state.
    // STATE_ROOT is used to construct a list of all states in the group
    // that are not compatible with the initial state.
    int state_set_root = toutput->state_list.raw[state_no];
    int state_subset_root = state_no;
    toutput->state_list.raw[state_subset_root] = NIL;
    int state_root = NIL;
    for (int state_no = state_set_root; state_no != NIL; state_no = state_set_root) {
      state_set_root = toutput->state_list.raw[state_set_root];
      // We traverse the reduce map of the state taken out from the group
      // and check to see if it is compatible with the subset being
      // constructed so far.
      if (state_no > num_states) {
        red = lastats[state_no].reduce;
      } else {
        red = srt->reduce[state_no];
      }
      int jj;
      for (jj = 1; jj <= red.size; jj++) {
        int symbol = red.map[jj].symbol;
        if (reduce_action.raw[symbol] != OMEGA && reduce_action.raw[symbol] != red.map[jj].rule_number) {
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
        toutput->state_list.raw[state_no] = state_subset_root;
        state_subset_root = state_no;
        for (jj = 1; jj <= red.size; jj++) {
          int symbol = red.map[jj].symbol;
          if (reduce_action.raw[symbol] == OMEGA) {
            int rule_no = red.map[jj].rule_number;
            if (rules[rule_no].lhs == accept_image) {
              rule_no = 0;
            }
            reduce_action.raw[symbol] = rule_no;
            rule_count.raw[rule_no]++;
          } else {
            num_reductions_saved++;
          }
        }
      } else {
        toutput->state_list.raw[state_no] = state_root;
        state_root = state_no;
      }
    }

    // Figure out the best default rule candidate, and update
    // DEFAULT_SAVES.
    // Recall that all accept actions were changed into reduce actions
    // by rule 0.
    int k = 0;
    int reduce_size = 0;
    int default_rule = ia->error_act;
    struct node *tail;
    for (struct node *q = new_state_element_reduce_nodes[i]; q != NULL; tail = q, q = q->next) {
      int rule_no = q->value;
      reduce_size += rule_count.raw[rule_no];
      if (rule_count.raw[rule_no] > k && rule_no != 0 && !shift_on_error_symbol.raw[state_subset_root]) {
        k = rule_count.raw[rule_no];
        default_rule = rule_no;
      }
    }
    default_saves += k;
    reduce_size -= k;

    // If STATE_ROOT is not NIL then there are states in the group that
    // did not meet the compatibility test.  Throw those states back in
    // front of MULTI_ROOT as a group.
    if (state_root != NIL) {
      tresult->top++;
      new_state_element[tresult->top].thread = new_state_element[i].thread;
      new_state_element[i].thread = tresult->top;
      if (state_root > num_states) {
        new_state_element[tresult->top].shift_number = lastats[state_root].shift_number;
      } else {
        new_state_element[tresult->top].shift_number = statset[state_root].shift_number;
      }
      new_state_element_reduce_nodes[tresult->top] = new_state_element_reduce_nodes[i];
      new_state_element[tresult->top].image = state_root;
    } else
      free_nodes(new_state_element_reduce_nodes[i], tail);

    // Create Reduce map for the newly created terminal state.
    // We may assume that SYMBOL field of defaults is already set to
    // the DEFAULT_SYMBOL value.
    struct reduce_header_type new_red = Allocate_reduce_map(reduce_size);
    new_red.map[0].symbol = DEFAULT_SYMBOL;
    new_red.map[0].rule_number = default_rule;
    for ALL_TERMINALS3(symbol) {
      if (reduce_action.raw[symbol] != OMEGA) {
        if (reduce_action.raw[symbol] != default_rule) {
          new_red.map[reduce_size].symbol = symbol;
          if (reduce_action.raw[symbol] == 0) {
            new_red.map[reduce_size].rule_number = ia->accept_act;
          } else {
            new_red.map[reduce_size].rule_number =
                reduce_action.raw[symbol];
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
  for (int i = tresult->single_root; i != NIL; i = new_state_element[i].thread) {
    int state_no = new_state_element[i].image;
    struct node *q = new_state_element_reduce_nodes[i];
    int rule_no = q->value;
    free_nodes(q, q);
    struct reduce_header_type new_red;
    struct reduce_header_type red;
    if (rules[rule_no].lhs == accept_image) {
      red = srt->reduce[state_no];
      int reduce_size = red.size;
      new_red = Allocate_reduce_map(reduce_size);
      new_red.map[0].symbol = DEFAULT_SYMBOL;
      new_red.map[0].rule_number = ia->error_act;
      for (int j = 1; j <= reduce_size; j++) {
        new_red.map[j].symbol = red.map[j].symbol;
        new_red.map[j].rule_number = ia->accept_act;
      }
    } else {
      for ALL_TERMINALS3(j) {
        reduce_action.raw[j] = OMEGA;
      }
      for (; state_no != NIL; state_no = toutput->state_list.raw[state_no]) {
        if (state_no > num_states) {
          red = lastats[state_no].reduce;
        } else {
          red = srt->reduce[state_no];
        }
        for (int j = 1; j <= red.size; j++) {
          int symbol = red.map[j].symbol;
          if (reduce_action.raw[symbol] == OMEGA) {
            reduce_action.raw[symbol] = rule_no;
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
  for (int i = tresult->empty_root; i != NIL; i = new_state_element[i].thread) {
    int state_no = new_state_element[i].image;
    struct reduce_header_type red;
    if (state_no > num_states) {
      red = lastats[state_no].reduce;
    } else {
      red = srt->reduce[state_no];
    }
    red.map[0].symbol = DEFAULT_SYMBOL;
    red.map[0].rule_number = ia->error_act;
    new_state_element[i].reduce = red;
  }
  ctp->num_terminal_states = tresult->top;
  ArrayLong frequency_symbol = Allocate_long_array2(num_terminals + 1);
  ArrayLong frequency_count = Allocate_long_array2(num_terminals + 1);
  ArrayLong row_size = Allocate_long_array2(max_la_state + 1);
  if (cli_options->shift_default_bit) {
    merge_shift_domains(cli_options, toutput, row_size, frequency_symbol, frequency_count, nte, shift_check_index, ctp, new_state_element, shift_image, real_shift_number, np, ia, srt, shiftdf);
  }
  // We now reorder the terminal states based on the number of actions
  // in them, and remap the terminal symbols if they were not already
  // remapped in the previous block for the SHIFT_CHECK vector.
  for ALL_TERMINALS3(symbol) {
    frequency_symbol.raw[symbol] = symbol;
    frequency_count.raw[symbol] = 0;
  }
  for (int i = 1; i <= ctp->num_terminal_states; i++) {
    toutput->ordered_state.raw[i] = i;
    row_size.raw[i] = 0;
    struct shift_header_type sh = srt->shift[new_state_element[i].shift_number];
    for (int j = 1; j <= sh.size; j++) {
      int symbol = sh.map[j].symbol;
      if (!cli_options->shift_default_bit || sh.map[j].action != shiftdf.raw[symbol]) {
        row_size.raw[i]++;
        frequency_count.raw[symbol]++;
      }
    }
    for (int state_no = toutput->state_list.raw[new_state_element[i].image]; state_no != NIL; state_no = toutput->state_list.raw[state_no]) {
      num_shifts_saved += row_size.raw[i];
    }
    struct reduce_header_type red;
    // Note that the Default action is skipped !!!
    red = new_state_element[i].reduce;
    for (int j = 1; j <= red.size; j++) {
      int symbol = red.map[j].symbol;
      row_size.raw[i]++;
      frequency_count.raw[symbol]++;
    }
  }
  PRNT3("Number of unique terminal states: %d", ctp->num_terminal_states);

  PRNT3("Number of Shift actions saved by merging: %d", num_shifts_saved);

  PRNT3("Number of Reduce actions saved by merging: %d", num_reductions_saved);

  PRNT3("Number of Reduce saved by default: %d", default_saves);

  sortdes(toutput->ordered_state, row_size, 1, ctp->num_terminal_states, num_terminals);

  if (!cli_options->shift_default_bit) {
    sortdes(frequency_symbol, frequency_count, 1, num_terminals, ctp->num_terminal_states);
    for ALL_TERMINALS3(symbol) {
      toutput->symbol_map.raw[frequency_symbol.raw[symbol]] = symbol;
    }
    toutput->symbol_map.raw[DEFAULT_SYMBOL] = DEFAULT_SYMBOL;
    eoft_image = toutput->symbol_map.raw[eoft_image];
    if (cli_options->error_maps_bit) {
      error_image = toutput->symbol_map.raw[error_image];
      eolt_image = toutput->symbol_map.raw[eolt_image];
    }
    for (int i = 1; i <= num_shift_maps; i++) {
      struct shift_header_type sh = srt->shift[i];
      for (int j = 1; j <= sh.size; j++) {
        sh.map[j].symbol = toutput->symbol_map.raw[sh.map[j].symbol];
      }
    }

    for (int state_no = 1; state_no <= ctp->num_terminal_states; state_no++) {
      struct reduce_header_type red = new_state_element[state_no].reduce;
      for (int i = 1; i <= red.size; i++) {
        red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
      }
    }
    // If ERROR_MAPS are requested, we also have to remap the original
    // REDUCE maps.
    if (cli_options->error_maps_bit) {
      for ALL_STATES3(state_no) {
        struct reduce_header_type red = srt->reduce[state_no];
        for (int i = 1; i <= red.size; i++) {
          red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
        }
      }
    }
  }
  nte->value = num_shifts + num_shift_reduces + num_reductions - num_shifts_saved - num_reductions_saved - default_saves + ctp->num_terminal_states;
  ffree(rule_count.raw);
  ffree(reduce_action.raw);
  ffree(row_size.raw);
  ffree(frequency_count.raw);
  ffree(frequency_symbol.raw);
  ffree(shift_on_error_symbol.raw);
  ffree(new_state_element_reduce_nodes);
}

/// We now compute the starting position for each terminal state just
/// as we did for the non-terminal states.
/// The starting positions are stored in the vector TERM_STATE_INDEX.
static void overlap_t_rows(struct CLIOptions *cli_options, struct TableOutput *toutput, struct NumTableEntries *nte, ArrayLong term_state_index, struct CTabsProps *ctp, struct new_state_type *new_state_element, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, ArrayShort shiftdf) {
  ArrayShort terminal_list = Allocate_short_array2(num_terminals + 1);
  ctp->increment_size = MAX(nte->value * increment / 100, num_terminals + 1);
  const long old_size = ctp->table_size;
  ctp->table_size = MIN(nte->value + ctp->increment_size, MAX_TABLE_SIZE);
  if ((int) ctp->table_size > old_size) {
    ffree(np->previous.raw);
    ffree(np->next.raw);
    np->next = Allocate_long_array2(ctp->table_size + 1);
    np->previous = Allocate_long_array2(ctp->table_size + 1);
  } else {
    ctp->table_size = old_size;
  }
  ia->first_index = 1;
  np->previous.raw[ia->first_index] = NIL;
  np->next.raw[ia->first_index] = ia->first_index + 1;
  for (int indx = 2; indx < (int) ctp->table_size; indx++) {
    np->next.raw[indx] = indx + 1;
    np->previous.raw[indx] = indx - 1;
  }
  ia->last_index = ctp->table_size;
  np->previous.raw[ia->last_index] = ia->last_index - 1;
  np->next.raw[ia->last_index] = NIL;
  int max_indx = ia->first_index;
  for (int k = 1; k <= ctp->num_terminal_states; k++) {
    const int state_no = toutput->ordered_state.raw[k];
    // For the terminal table, we are dealing with two lists, the SHIFT
    // list, and the REDUCE list. Those lists are merged together first
    // in TERMINAL_LIST.  Since we have to iterate over the list twice,
    // this merging makes things easy.
    int root_symbol = NIL;
    const struct shift_header_type sh = srt->shift[new_state_element[state_no].shift_number];
    for (int i = 1; i <= sh.size; i++) {
      int symbol = sh.map[i].symbol;
      if (!cli_options->shift_default_bit || sh.map[i].action != shiftdf.raw[symbol]) {
        terminal_list.raw[symbol] = root_symbol;
        root_symbol = symbol;
      }
    }
    const struct reduce_header_type red = new_state_element[state_no].reduce;
    for (int i = 1; i <= red.size; i++) {
      terminal_list.raw[red.map[i].symbol] = root_symbol;
      root_symbol = red.map[i].symbol;
    }
    // Look for a suitable index where to overlay the state.
    int indx = ia->first_index;
  look_for_match_in_term_table:
    if (indx == NIL) {
      indx = ctp->table_size + 1;
    }
    if (indx + num_terminals > (int) ctp->table_size) {
      reallocate(cli_options, ctp, np, ia);
    }
    for (int symbol = root_symbol; symbol != NIL; symbol = terminal_list.raw[symbol]) {
      if (np->next.raw[indx + symbol] == OMEGA) {
        indx = np->next.raw[indx];
        goto look_for_match_in_term_table;
      }
    }
    // INDX marks the starting position for the state, remove all the
    // positions that are claimed by terminal actions in the state.
    for (int symbol = root_symbol; symbol != NIL; symbol = terminal_list.raw[symbol]) {
      const int i = indx + symbol;
      if (i == ia->last_index) {
        ia->last_index = np->previous.raw[ia->last_index];
        np->next.raw[ia->last_index] = NIL;
      } else {
        np->next.raw[np->previous.raw[i]] = np->next.raw[i];
        np->previous.raw[np->next.raw[i]] = np->previous.raw[i];
      }
      np->next.raw[i] = OMEGA;
    }
    // We now remove the starting position itself from the list, and
    // mark it as taken(CHECK(INDX) = OMEGA)
    // MAX_INDX is updated if required.
    // TERM_STATE_INDEX(STATE_NO) is properly set to INDX as the starting
    // position of STATE_NO.
    if (ia->first_index == ia->last_index) {
      ia->first_index = NIL;
    } else if (indx == ia->first_index) {
      ia->first_index = np->next.raw[ia->first_index];
      np->previous.raw[ia->first_index] = NIL;
    } else if (indx == ia->last_index) {
      ia->last_index = np->previous.raw[ia->last_index];
      np->next.raw[ia->last_index] = NIL;
    } else {
      np->next.raw[np->previous.raw[indx]] = np->next.raw[indx];
      np->previous.raw[np->next.raw[indx]] = np->previous.raw[indx];
    }
    np->next.raw[indx] = OMEGA;
    if (indx > max_indx) {
      max_indx = indx;
    }
    term_state_index.raw[state_no] = indx;
  }
  // Update all counts, and report statistics.
  ctp->term_check_size = max_indx + num_terminals;
  for (ctp->term_action_size = max_indx + num_terminals; ctp->term_action_size >= max_indx; ctp->term_action_size--) {
    if (np->next.raw[ctp->term_action_size] == OMEGA) {
      break;
    }
  }
  printf("\n");
  PRNT3("Length of Terminal Check Table: %d", ctp->term_check_size);
  PRNT3("Length of Terminal Action Table: %d", ctp->term_action_size);
  PRNT3("Number of entries in Terminal Action Table: %ld", nte->value);
  const long percentage = (ctp->term_action_size - nte->value) * 1000 / nte->value;
  PRNT3("Percentage of increase: %ld.%ld%%", percentage / 10, percentage % 10);
  // We now write out the tables to the SYSTAB file.
  ctp->table_size = MAX(
    MAX(
      MAX(
        MAX(
          ctp->check_size,
          ctp->term_check_size
        ), ctp->shift_check_size), ctp->action_size),
    ctp->term_action_size
  );
  ffree(terminal_list.raw);
  ffree(np->next.raw);
  ffree(np->previous.raw);
}

void cmprspa(struct CLIOptions *cli_options, struct TableOutput *toutput, struct DetectedSetSizes *dss, struct CTabsProps *ctp, struct OutputFiles* of, struct NextPrevious* np, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, ArrayLong scope_right_side, struct lastats_type *lastats, ArrayShort shiftdf, ArrayLong gotodef, ArrayShort gd_index, ArrayShort gd_range, ArrayShort scope_state, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table, struct ScopeCounter* sc, char *output_buffer, struct OutputPtr output_ptr2) {
  ArrayBool shift_on_error_symbol = Allocate_bool_array2(max_la_state + 1);
  struct new_state_type *new_state_element;
  calloc0p(&new_state_element, max_la_state + 1, struct new_state_type);
  struct node **new_state_element_reduce_nodes;
  calloc0p(&new_state_element_reduce_nodes, max_la_state + 1, struct node *);
  long last_symbol = remap_non_terminals(cli_options, toutput, gotodef, statset);
  struct NumTableEntries nte = (struct NumTableEntries){
    .value = 0,
  };
  overlap_nt_rows(cli_options, toutput, &nte, ctp, last_symbol, np, ia, statset);
  struct TResult tresult = (struct TResult){
    .single_root = NIL,
    .multi_root = NIL,
    .empty_root = NIL,
  };
  merge_similar_t_rows(cli_options, toutput, shift_on_error_symbol, new_state_element_reduce_nodes, &tresult, new_state_element, srt, lastats, statset);
  ArrayLong shift_check_index = Allocate_long_array2(num_shift_maps + 1);
  ArrayShort shift_image = Allocate_short_array2(max_la_state + 1);
  ArrayShort real_shift_number = Allocate_short_array2(num_shift_maps + 1);
  overlay_sim_t_rows(cli_options, toutput, shift_on_error_symbol, new_state_element_reduce_nodes, &tresult, &nte, shift_check_index, ctp, new_state_element, shift_image, real_shift_number, np, ia, srt, rules, lastats, shiftdf, statset);
  ArrayLong term_state_index = Allocate_long_array2(max_la_state + 1);
  overlap_t_rows(cli_options, toutput, &nte, term_state_index, ctp, new_state_element, np, ia, srt, shiftdf);
  print_space_parser(cli_options, toutput, dss, term_state_index, shift_check_index, ctp, new_state_element, shift_image, real_shift_number, of, scope, ia, srt, scope_right_side, shiftdf, gotodef, gd_index, gd_range, rules, scope_state, statset, item_table, sc, output_buffer, output_ptr2);
}

// endregion

// region timetab

/// We now remap the symbols in the unified Table based on frequency.
/// We also remap the states based on frequency.
struct DefaultSaves {
  int default_saves;
  int last_symbol;
} remap_symbols(struct TableOutput* toutput, ArrayBool is_terminal, struct SRTable* srt, struct lastats_type *lastats, struct statset_type *statset, struct CLIOptions* cli_options) {
  int default_saves = 0;
  ArrayLong frequency_symbol = Allocate_long_array2(num_symbols + 1);
  ArrayLong frequency_count = Allocate_long_array2(num_symbols + 1);
  ArrayLong row_size = Allocate_long_array2(max_la_state + 1);
  printf("\n");
  //     The variable FREQUENCY_SYMBOL is used to hold the symbols
  // in the grammar,  and the variable FREQUENCY_COUNT is used
  // correspondingly to hold the number of actions defined on each
  // symbol.
  // ORDERED_STATE and ROW_SIZE are used in a similar fashion for
  // states.
  for (int i = 1; i <= num_symbols; i++) {
    frequency_symbol.raw[i] = i;
    frequency_count.raw[i] = 0;
  }
  for ALL_STATES3(state_no) {
    toutput->ordered_state.raw[state_no] = state_no;
    row_size.raw[state_no] = 0;
    struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
    for (int i = 1; i <= sh.size; i++) {
      row_size.raw[state_no]++;
      long symbol = sh.map[i].symbol;
      frequency_count.raw[symbol]++;
    }
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      row_size.raw[state_no]++;
      long symbol = go_to.map[i].symbol;
      frequency_count.raw[symbol]++;
    }
    struct reduce_header_type red = srt->reduce[state_no];
    short default_rule = red.map[0].rule_number;
    for (int i = 1; i <= red.size; i++) {
      if (red.map[i].rule_number != default_rule) {
        row_size.raw[state_no]++;
        long symbol = red.map[i].symbol;
        frequency_count.raw[symbol]++;
      } else {
        default_saves++;
      }
    }
  }
  PRNT3("Number of Reductions saved by default: %d", default_saves);
  for ALL_LA_STATES3(state_no) {
    toutput->ordered_state.raw[state_no] = state_no;
    row_size.raw[state_no] = 0;
    struct shift_header_type sh = srt->shift[lastats[state_no].shift_number];
    for (int i = 1; i <= sh.size; i++) {
      row_size.raw[state_no]++;
      long symbol = sh.map[i].symbol;
      frequency_count.raw[symbol]++;
    }
    struct reduce_header_type red = lastats[state_no].reduce;
    short default_rule = red.map[0].rule_number;
    for (int i = 1; i <= red.size; i++) {
      if (red.map[i].rule_number != default_rule) {
        row_size.raw[state_no]++;
        long symbol = red.map[i].symbol;
        frequency_count.raw[symbol]++;
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
    if (frequency_count.raw[last_symbol] != 0) {
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
    if (frequency_count.raw[i] >= frequency_count.raw[j]) {
      symbol = frequency_symbol.raw[i];
      is_terminal.raw[k] = true;
      i++;
    } else {
      symbol = frequency_symbol.raw[j];
      is_terminal.raw[k] = false;
      j++;
    }
    toutput->symbol_map.raw[symbol] = k;
  }
  toutput->symbol_map.raw[DEFAULT_SYMBOL] = DEFAULT_SYMBOL;
  // Process the remaining non-terminal and useless terminal symbols.
  for (; j <= num_symbols; j++) {
    k++;
    long symbol = frequency_symbol.raw[j];
    is_terminal.raw[k] = false;
    toutput->symbol_map.raw[symbol] = k;
  }
  eoft_image = toutput->symbol_map.raw[eoft_image];
  if (cli_options->error_maps_bit) {
    error_image = toutput->symbol_map.raw[error_image];
    eolt_image = toutput->symbol_map.raw[eolt_image];
  }
  //    All symbol entries in the state automaton are updated based on
  // the new mapping of the symbols.
  // The states are sorted in descending order based on the number of
  // actions defined on them.
  for ALL_STATES3(state_no) {
    struct goto_header_type go_to = statset[state_no].go_to;
    // Remap Goto map
    for (int i = 1; i <= go_to.size; i++) {
      go_to.map[i].symbol = toutput->symbol_map.raw[go_to.map[i].symbol];
    }
    struct reduce_header_type red = srt->reduce[state_no];
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
    }
  }
  for ALL_LA_STATES3(state_no) {
    struct reduce_header_type red = lastats[state_no].reduce;
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
    }
  }
  for (int i = 1; i <= num_shift_maps; i++) {
    struct shift_header_type sh = srt->shift[i];
    for (int j = 1; j <= sh.size; j++) {
      sh.map[j].symbol = toutput->symbol_map.raw[sh.map[j].symbol];
    }
  }
  sortdes(toutput->ordered_state, row_size, 1, max_la_state, num_symbols);
  ffree(frequency_symbol.raw);
  ffree(frequency_count.raw);
  ffree(row_size.raw);
  return (struct DefaultSaves) {
    .default_saves = default_saves,
    .last_symbol = last_symbol,
  };
}

/// We now overlap the State automaton table, or more precisely,  we
/// compute the starting position in a vector where each of its rows
/// may be placed without clobbering elements in another row.
/// The starting positions are stored in the vector STATE_INDEX.
static void overlap_tables(struct CLIOptions *cli_options, struct TableOutput* toutput, ArrayBool is_terminal, struct DefaultSaves default_saves, struct CTabsProps* ctp, long last_symbol, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, struct lastats_type *lastats, struct statset_type *statset) {
  ArrayLong symbol_list = Allocate_long_array2(num_symbols + 1);
  num_entries -= default_saves.default_saves;
  ctp->increment_size = MAX(num_entries * increment / 100, num_symbols + 1);
  ctp->table_size = MIN(num_entries + ctp->increment_size, MAX_TABLE_SIZE);
  // Allocate space for table, and initialize the AVAIL_POOL list.
  // The variable FIRST_INDEX keeps track of the first element in the
  // doubly-linked list, and LAST_ELEMENT keeps track of the last
  // element in the list.
  // The variable MAX_INDX is used to keep track of the maximum
  // starting position for a row that has been used.
  np->next = Allocate_long_array2(ctp->table_size + 1);
  np->previous = Allocate_long_array2(ctp->table_size + 1);
  ia->first_index = 1;
  np->next.raw[ia->first_index] = ia->first_index + 1; /* Should be constant-folded */
  np->previous.raw[ia->first_index] = NIL;
  for (long indx = 2; indx < (int) ctp->table_size; indx++) {
    np->next.raw[indx] = indx + 1;
    np->previous.raw[indx] = indx - 1;
  }
  ia->last_index = ctp->table_size;
  np->previous.raw[ia->last_index] = ia->last_index - 1;
  np->next.raw[ia->last_index] = NIL;
  long max_indx = ia->first_index;
  // We now iterate over all the states in their new sorted order as
  // indicated by the variable STATE_NO, and determine an "overlap"
  // position for them.
  for (int k = 1; k <= max_la_state; k++) {
    const long state_no = toutput->ordered_state.raw[k];
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
        symbol_list.raw[symbol] = root_symbol;
        root_symbol = symbol;
      }
      sh = srt->shift[statset[state_no].shift_number];
      red = srt->reduce[state_no];
    }
    for (int i = 1; i <= sh.size; i++) {
      int symbol = sh.map[i].symbol;
      symbol_list.raw[symbol] = root_symbol;
      root_symbol = symbol;
    }
    symbol_list.raw[0] = root_symbol;
    root_symbol = 0;
    short default_rule = red.map[0].rule_number;
    for (int i = 1; i <= red.size; i++) {
      if (red.map[i].rule_number != default_rule) {
        int symbol = red.map[i].symbol;
        symbol_list.raw[symbol] = root_symbol;
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
    for (int symbol = root_symbol; symbol != NIL; symbol = symbol_list.raw[symbol]) {
      if (np->next.raw[indx + symbol] == OMEGA) {
        indx = np->next.raw[indx];
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
    toutput->state_index.raw[state_no] = indx;
    for (int symbol = root_symbol; symbol != NIL; symbol = symbol_list.raw[symbol]) {
      const long i = indx + symbol;
      if (ia->first_index == ia->last_index) {
        ia->first_index = NIL;
      } else if (i == ia->first_index) {
        ia->first_index = np->next.raw[ia->first_index];
        np->previous.raw[ia->first_index] = NIL;
      } else if (i == ia->last_index) {
        ia->last_index = np->previous.raw[ia->last_index];
        np->next.raw[ia->last_index] = NIL;
      } else {
        np->next.raw[np->previous.raw[i]] = np->next.raw[i];
        np->previous.raw[np->next.raw[i]] = np->previous.raw[i];
      }
      np->next.raw[i] = OMEGA;
    }
  }
  // Update all global counters, and compute ACCEPT_ACTION and
  // ERROR_ACTION.
  ctp->table_size = max_indx + num_symbols;
  ia->accept_act = max_indx + num_rules + 1;
  ia->error_act = ia->accept_act + 1;
  for (ctp->action_size = ctp->table_size; ctp->action_size >= max_indx; ctp->action_size--) {
    if (np->next.raw[ctp->action_size] == OMEGA) {
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
      for (; last_symbol >= 1 && !is_terminal.raw[last_symbol]; last_symbol--) {
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
void cmprtim(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, ArrayLong scope_right_side, struct lastats_type *lastats, ArrayLong gotodef, ArrayShort gd_index, ArrayShort gd_range, ArrayShort scope_state, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table, struct ScopeCounter* sc, char *output_buffer, struct OutputPtr output_ptr2) {
  ArrayBool is_terminal = Allocate_bool_array2(num_symbols + 1);
  struct DefaultSaves default_saves = remap_symbols(toutput, is_terminal, srt, lastats, statset, cli_options);
  overlap_tables(cli_options, toutput, is_terminal, default_saves, ctp, default_saves.last_symbol, np, ia, srt, lastats, statset);
  print_time_parser(cli_options, toutput, dss, ctp, of, np, scope, ia, srt, scope_right_side, lastats, gotodef, gd_index, gd_range, rules, scope_state, statset, item_table, sc, output_buffer, output_ptr2);
}

// endregion

// region ptables

struct ptables_action_element {
  struct ptables_action_element *next;
  short count;
  short action;
};

/// The array ACTION_COUNT is used to construct a map from each terminal
/// into the set (list) of actions defined on that terminal. A count of the
/// number of occurrences of each action in the automaton is kept.
/// This procedure is invoked with a specific shift map which it processes
/// and updates the ACTION_COUNT map accordingly.
static void process_shift_actions(struct ptables_action_element **action_count, const int shift_no, struct SRTable* srt) {
  const struct shift_header_type sh = srt->shift[shift_no];
  for (int i = 1; i <= sh.size; i++) {
    const int symbol = sh.map[i].symbol;
    const short act = sh.map[i].action;
    struct ptables_action_element *q;
    for (q = action_count[symbol]; q != NULL; q = q->next) {
      if (q->action == act)
        break;
    }
    if (q == NULL) /* new action not yet seen */
    {
      talloc0p(&q, struct ptables_action_element);
      q->action = act;
      q->count = 1;
      q->next = action_count[symbol];
      action_count[symbol] = q;
    } else q->count++;
  }
}

/// This procedure updates the vector SHIFTDF, indexable by the terminals in
/// the grammar. Its task is to assign to each element of SHIFTDF, the action
/// most frequently defined on the symbol in question.
static void compute_shift_default(struct SRTable* srt, struct lastats_type *lastats, ArrayShort* shiftdf, struct statset_type *statset) {
  // Set up a pool of temporary space.
  reset_temporary_space();
  int shift_count = 0;
  int shift_reduce_count = 0;
  *shiftdf = Allocate_short_array2(num_terminals + 1);
  struct ptables_action_element **action_count;
  calloc0p(&action_count, num_terminals + 1, struct ptables_action_element *);
  // For each state, invoke PROCESS_SHIFT_ACTIONS to process the
  // shift map associated with that state.
  for ALL_STATES3(state_no) {
    process_shift_actions(action_count, statset[state_no].shift_number, srt);
  }
  for ALL_LA_STATES3(state_no) {
    process_shift_actions(action_count, lastats[state_no].shift_number, srt);
  }
  // We now iterate over the ACTION_COUNT mapping, and for each
  // terminal t, initialize SHIFTDF[t] to the action that is most
  // frequently defined on t.
  for ALL_TERMINALS3(symbol) {
    int max_count = 0;
    short default_action = 0;
    for (const struct ptables_action_element *q = action_count[symbol]; q != NULL; q = q->next) {
      if (q->count > max_count) {
        max_count = q->count;
        default_action = q->action;
      }
    }
    shiftdf->raw[symbol] = default_action;
    // A state number ?
    if (default_action > 0) {
      shift_count += max_count;
    } else {
      shift_reduce_count += max_count;
    }
  }
  PRNT3("Number of Shift entries saved by default: %d", shift_count);
  PRNT3("Number of Shift/Reduce entries saved by default: %d", shift_reduce_count);
  num_shifts -= shift_count;
  num_shift_reduces -= shift_reduce_count;
  num_entries = num_entries - shift_count - shift_reduce_count;
  ffree(action_count);
}

/// COMPUTE_GOTO_DEFAULT constructs the vector GOTODEF, which is indexed by
/// the non-terminals in the grammar. Its task is to assign to each element
/// of the array the Action which is most frequently defined on the symbol in
/// question, and remove all such actions from the state automaton.
static void compute_goto_default(ArrayLong* gotodef, struct statset_type *statset) {
  // Set up a pool of temporary space.
  reset_temporary_space();
  int goto_count = 0;
  int goto_reduce_count = 0;
  *gotodef = Allocate_long_array2(num_non_terminals);
  gotodef->raw -= num_terminals + 1;
  struct ptables_action_element **action_count;
  calloc0p(&action_count, num_non_terminals, struct ptables_action_element *);
  action_count -= num_terminals + 1;
  if (action_count == NULL) {
    nospace();
  }
  // The array ACTION_COUNT is used to construct a map from each
  // non-terminal into the set (list) of actions defined on that
  // non-terminal. A count of how many occurences of each action
  // is also kept.
  // This loop is analoguous to the loop in PROCESS_SHIFT_ACTIONS.
  for ALL_STATES3(state_no) {
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      const int symbol = go_to.map[i].symbol;
      const short act = go_to.map[i].action;
      struct ptables_action_element *q;
      for (q = action_count[symbol]; q != NULL; q = q->next) {
        if (q->action == act) {
          break;
        }
      }
      if (q == NULL) {
        /* new action not yet seen */
        talloc0p(&q, struct ptables_action_element);
        q->action = act;
        q->count = 1;
        q->next = action_count[symbol];
        action_count[symbol] = q;
      } else {
        q->count++;
      }
    }
  }
  // We now iterate over the mapping created above and for each
  // non-terminal A, initialize GOTODEF(A) to the action that is
  // most frequently defined on A.
  for ALL_NON_TERMINALS3(symbol) {
    int max_count = 0;
    int default_action = 0;
    struct ptables_action_element *q;
    for (q = action_count[symbol]; q != NULL; q = q->next) {
      if (q->count > max_count) {
        max_count = q->count;
        default_action = q->action;
      }
    }
    (*gotodef).raw[symbol] = default_action;
    if (default_action > 0) {
      /* A state number? */
      goto_count += max_count;
    } else {
      goto_reduce_count += max_count;
    }
  }
  //   We now iterate over the automaton and eliminate all GOTO actions
  // for which there is a DEFAULT.
  for ALL_STATES3(state_no) {
    int k = 0;
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      if ((*gotodef).raw[go_to.map[i].symbol] != go_to.map[i].action) {
        k++;
        go_to.map[k].symbol = go_to.map[i].symbol;
        go_to.map[k].action = go_to.map[i].action;
      }
    }
    statset[state_no].go_to.size = k; /* Readjust size */
  }
  PRNT3("Number of Goto entries saved by default: %d", goto_count);
  PRNT3("Number of Goto/Reduce entries saved by default: %d", goto_reduce_count);
  num_gotos -= goto_count;
  num_goto_reduces -= goto_reduce_count;
  num_entries = num_entries - goto_count - goto_reduce_count;
  action_count += num_terminals + 1;
  ffree(action_count);
}

static void init_file(FILE **file, char *file_name, char *file_tag) {
  const char *p = strrchr(file_name, '.');
  if ((*file = fopen(file_name, "w")) == NULL) {
    fprintf(stderr, "***ERROR: Symbol file \"%s\" cannot be opened\n", file_name);
    exit(12);
  } else {
    memcpy(file_tag, file_name, p - file_name);
    file_tag[p - file_name] = '\0';
  }
}

/// Remap symbols, apply transition default actions  and call
/// appropriate table compression routine.
void process_tables(char *tab_file, struct OutputFiles *output_files, struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, struct scope_type *scope, ArrayShort gd_range, struct SRTable* srt, ArrayLong scope_right_side, struct lastats_type *lastats, ArrayShort* shiftdf, ArrayLong* gotodef, ArrayShort gd_index, struct statset_type *statset, ArrayShort scope_state, struct ruletab_type *rules, struct itemtab *item_table, struct symno_type *symno, struct ScopeCounter* sc) {
  // First, we decrease by 1 the constants NUM_SYMBOLS
  // and NUM_TERMINALS, remove the EMPTY symbol(1) and remap the
  // other symbols beginning at 1.  If default reduction is
  // requested, we assume a special DEFAULT_SYMBOL with number zero.
  eoft_image--;
  accept_image--;
  if (cli_options->error_maps_bit) {
    error_image--;
    eolt_image--;
  }
  num_terminals--;
  num_symbols--;
  // Remap all the symbols used in GOTO and REDUCE actions.
  // Remap all the symbols used in GD_RANGE.
  // Remap all the symbols used in the range of SCOPE.
  // Release space trapped by the maps IN_STAT and FIRST.
  for ALL_STATES3(state_no) {
    const struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      go_to.map[i].symbol--;
    }
    struct reduce_header_type red = srt->reduce[state_no];
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol--;
    }
  }
  for ALL_LA_STATES3(state_no) {
    struct reduce_header_type red = lastats[state_no].reduce;
    for (int i = 1; i <= red.size; i++)
      red.map[i].symbol--;
  }
  for (int i = 1; i <= gotodom_size; i++) {
    gd_range.raw[i]--;
  }
  for (int i = 1; i <= sc->num_scopes; i++) {
    scope[i].lhs_symbol--;
    scope[i].look_ahead--;
  }
  for (int i = 1; i <= sc->scope_rhs_size; i++) {
    if (scope_right_side.raw[i] != 0) {
      scope_right_side.raw[i]--;
    }
  }
  // Remap all symbols in the domain of the Shift maps.
  for (int i = 1; i <= num_shift_maps; i++) {
    const struct shift_header_type sh = srt->shift[i];
    for (int j = 1; j <= sh.size; j++) {
      sh.map[j].symbol--;
    }
  }
  // Remap the left-hand side of all the rules.
  for ALL_RULES3(rule_no) {
    rules[rule_no].lhs--;
  }
  // Remap the dot symbols in ITEM_TABLE.
  if (cli_options->error_maps_bit) {
    for ALL_ITEMS3(item_no) {
      item_table[item_no].symbol--;
    }
  }
  // We update the SYMNO map.
  for ALL_SYMBOLS3(symbol) {
    symno[symbol] = symno[symbol + 1];
  }
  // If Goto Default and/or Shift Default were requested, process
  // appropriately.
  if (cli_options->shift_default_bit) {
    compute_shift_default(srt, lastats, shiftdf, statset);
  }
  if (cli_options->goto_default_bit) {
    compute_goto_default(gotodef, statset);
  }
  // Release the pool of temporary space.
  free_temporary_space();
  char *_output_ptr = NULL;
  char *_output_buffer = NULL;
  struct OutputPtr op = (struct OutputPtr) {
    .output_ptr = &_output_ptr,
    .output_buffer = &_output_buffer,
  };
  calloc0p(op.output_buffer, IOBUFFER_SIZE, char);
  FILE *systab;
  if (!cli_options->c_bit && !cli_options->cpp_bit && !cli_options->java_bit) {
    if ((systab = fopen(tab_file, "w")) == NULL) {
      fprintf(stderr, "***ERROR: Table file \"%s\" cannot be opened\n", tab_file);
      exit(12);
    }
  }
  struct TableOutput toutput = init_table_output();
  init_file(&of->sysdcl, output_files->dcl_file, of->dcl_tag);
  init_file(&of->syssym, output_files->sym_file, of->sym_tag);
  init_file(&of->sysdef, output_files->def_file, of->def_tag);
  init_file(&of->sysprs, output_files->prs_file, of->prs_tag);
  struct ImportantAspects ia = (struct ImportantAspects) {};
  if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
    cmprspa(cli_options, &toutput, dss, ctp, of, np, scope, &ia, srt, scope_right_side, lastats, *shiftdf, *gotodef, gd_index, gd_range, scope_state, statset, rules, item_table, sc, (*op.output_buffer), op);
  } else if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    cmprtim(cli_options, &toutput, dss, ctp, of, np, scope, &ia, srt, scope_right_side, lastats, *gotodef, gd_index, gd_range, scope_state, statset, rules, item_table, sc, (*op.output_buffer), op);
  } else {
    exit(999);
  }
  if (!cli_options->c_bit && !cli_options->cpp_bit && !cli_options->java_bit) {
    fclose(systab);
  }
}

// endregion