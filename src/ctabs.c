#include <stdlib.h>
#include "lpgparse.h"
#include <string.h>
#include "common.h"
#include <stdlib.h>

char msg_line[MAX_MSG_SIZE];

const long MAX_TABLE_SIZE = (USHRT_MAX < INT_MAX ? USHRT_MAX : INT_MAX) - 1;

struct scope_type *scope = NULL;

long *next = NULL;
long *previous = NULL;

long accept_act;
long error_act;
long first_index;
long last_index;
long max_name_length = 0;

struct ByteTerminalRange {
  bool value;
};

const char digits[] = "0123456789";

/// ITOC takes as arguments an integer NUM. NUM is an integer containing at
/// most 11 digits which is converted into a character string and placed in
/// the iobuffer. Leading zeros are eliminated and if the number is
/// negative, a leading "-" is added.
static void itoc(const int num) {
  char tmp[12];
  register long val = ABS(num);
  tmp[11] = '\0';
  register char *p = &tmp[11];
  do {
    p--;
    *p = digits[val % 10];
    val /= 10;
  } while (val > 0);
  if (num < 0) {
    p--;
    *p = '-';
  }
  while (*p != '\0') {
    *output_ptr++ = *p++;
  }
}

static void padline(void) {
  for (register int i = 0; i < 12; i++) {
    *output_ptr++ = ' ';
  }
}

static void mystrcpy(const char *str, struct OutputFiles* of) {
  while (*str != '\0') {
    *output_ptr++ = *str++;
  }
  BUFFER_CHECK(of->sysdcl);
  BUFFER_CHECK(of->syssym);
}

static void prnt_longs(const char *title, const int init, const int bound, const int perline, const long *array, struct CLIOptions *cli_options, struct OutputFiles* of) {
  mystrcpy(title, of);
  padline();
  int k = 0;
  for (int i = init; i <= bound; i++) {
    itoc(array[i]);
    *output_ptr++ = ',';
    k++;
    if (k == perline && i != bound) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(of->sysdcl);
      padline();
      k = 0;
    }
  }
  if (k != 0) {
    *(output_ptr - 1) = '\n';
    BUFFER_CHECK(of->sysdcl);
  }
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of);
  } else {
    mystrcpy("                 };\n", of);
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
static void compute_action_symbols_range(const long *state_start, const long *state_stack, const long *state_list, long *action_symbols_range) {
  short *symbol_list = Allocate_short_array(num_symbols + 1);
  // We now write out the range elements of the ACTION_SYMBOLS map.
  // Recall that if STATE_START has a negative value, then the set in
  // question is sharing elements and does not need to be processed.
  int k = 0;
  for ALL_SYMBOLS3(j) {
    symbol_list[j] = OMEGA; /* Initialize all links to OMEGA */
  }
  for ALL_STATES3(state_no) {
    const int state_no__ = state_list[state_no];
    if (state_start[state_no__] > 0) {
      int symbol_root = 0; /* Add "fence" element: 0 to list */
      symbol_list[symbol_root] = NIL;
      // Pop a state from the stack,  and add each of its elements
      // that has not yet been processed into the list.
      // Continue until stack is empty...
      // Recall that the stack is represented by a circular queue.
      int state;
      for (bool end_node = (state = state_no__) == NIL; !end_node; end_node = state == state_no__) {
        state = state_stack[state];
        const struct shift_header_type sh = shift[statset[state].shift_number];
        for (int j = 1; j <= sh.size; j++) {
          int symbol = sh.map[j].symbol;
          if (symbol_list[symbol] == OMEGA) {
            symbol_list[symbol] = symbol_root;
            symbol_root = symbol;
          }
        }
        const struct reduce_header_type red = reduce[state];
        for (int j = 1; j <= red.size; j++) {
          int symbol = red.map[j].symbol;
          if (symbol_list[symbol] == OMEGA) {
            symbol_list[symbol] = symbol_root;
            symbol_root = symbol;
          }
        }
      }
      // Write the list out.
      for (int symbol = symbol_root; symbol != NIL; symbol = symbol_root) {
        symbol_root = symbol_list[symbol];
        symbol_list[symbol] = OMEGA;
        action_symbols_range[k++] = symbol;
      }
    }
  }
  ffree(symbol_list);
}

/// This procedure computes the range of the NACTION_SYMBOLS map. It
/// organization is analoguous to COMPUTE_ACTION_SYMBOLS_RANGE.
static void compute_naction_symbols_range(const long *state_start, const long *state_stack, const long *state_list, long *naction_symbols_range) {
  short *symbol_list = Allocate_short_array(num_symbols + 1);
  // We now write out the range elements of the NACTION_SYMBOLS map.
  // Recall that if STATE_START has a negative value, then the set in
  // question is sharing elements and does not need to be processed.
  int k = 0;
  for ALL_SYMBOLS3(j) {
    symbol_list[j] = OMEGA; /* Initialize all links to OMEGA */
  }
  for ALL_STATES3(state_no) {
    const int state_no__ = state_list[state_no];
    if (state_start[state_no__] > 0) {
      int symbol_root = 0; /* Add "fence" element: 0 to list */
      symbol_list[symbol_root] = NIL;
      // Pop a state from the stack,  and add each of its elements
      // that has not yet been processed into the list.
      // Continue until stack is empty...
      // Recall that the stack is represented by a circular queue.
      int state;
      for (bool end_node = (state = state_no__) == NIL; !end_node; end_node = state == state_no__) {
        state = state_stack[state];
        for (int j = gd_index[state]; j <= gd_index[state + 1] - 1; j++) {
          int symbol = gd_range[j];
          if (symbol_list[symbol] == OMEGA) {
            symbol_list[symbol] = symbol_root;
            symbol_root = symbol;
          }
        }
      }
      // Write the list out.
      for (int symbol = symbol_root; symbol != NIL; symbol = symbol_root) {
        symbol_root = symbol_list[symbol];
        symbol_list[symbol] = OMEGA;
        naction_symbols_range[k++] = symbol;
      }
    }
  }
  ffree(symbol_list);
}

static void exit_file(FILE **file, char *file_tag, struct CLIOptions *cli_options) {
  if (cli_options->c_bit || cli_options->cpp_bit) {
    fprintf(*file, "\n#endif /* %s_INCLUDED */\n", file_tag);
  }
}

static void print_error_maps(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct ByteTerminalRange* btr) {
  long *state_start = Allocate_long_array(num_states + 2);
  long *state_stack = Allocate_long_array(num_states + 1);
  PRNT("\nError maps storage:");
  // We now construct a bit map for the set of terminal symbols that
  // may appear in each state. Then, we invoke PARTSET to apply the
  // Partition Heuristic and print it.
  long *as_size = Allocate_long_array(num_states + 1);
  long *original;
  if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    original = Allocate_long_array(num_symbols + 1);
    // In a compressed TIME table, the terminal and non-terminal
    // symbols are mixed together when they are remapped.
    // We shall now recover the original number associated with
    // each terminal symbol since it lies very nicely in the
    // range 1..NUM_TERMINALS.  This will save a considerable
    // amount of space in the bit_string representation of sets
    // as well as time when operations are performed on those
    // bit-strings.
    for ALL_TERMINALS3(symbol) {
      original[toutput->symbol_map[symbol]] = symbol;
    }
  }
  JBitset action_symbols;
  if (error_maps_bit) {
    calloc0_set(action_symbols, num_states + 1, dss->term_set_size);
  }
  for ALL_STATES3(state_no) {
    struct shift_header_type sh;
    struct reduce_header_type red;
    sh = shift[statset[state_no].shift_number];
    as_size[state_no] = sh.size;
    for (int i = 1; i <= sh.size; i++) {
      int symbol;
      if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
        symbol = original[sh.map[i].symbol];
      } else {
        symbol = sh.map[i].symbol;
      }
      SET_BIT_IN(action_symbols, state_no, symbol);
    }
    red = reduce[state_no];
    as_size[state_no] += red.size;
    for (int i = 1; i <= red.size; i++) {
      int symbol;
      if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
        symbol = original[red.map[i].symbol];
      } else {
        symbol = red.map[i].symbol;
      }
      SET_BIT_IN(action_symbols, state_no, symbol);
    }
  }
  partset(action_symbols, as_size, toutput->state_list, state_start, state_stack, num_terminals, false);
  ffree(action_symbols.raw);
  // Compute and write out the base of the ACTION_SYMBOLS map.
  long *action_symbols_base = Allocate_long_array(num_states + 1);
  for ALL_STATES3(state_no) {
    action_symbols_base[toutput->state_list[state_no]] = ABS(state_start[toutput->state_list[state_no]]);
  }
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char asb[] = {0,\n", 1, num_states, 10, action_symbols_base, cli_options, of);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER asb[] = {0,\n", 1, num_states, 10, action_symbols_base, cli_options, of);
  }
  ffree(action_symbols_base);
  // Compute and write out the range of the ACTION_SYMBOLS map.
  int offset = state_start[num_states + 1];
  long *action_symbols_range = Allocate_long_array(offset);
  compute_action_symbols_range(state_start, state_stack, toutput->state_list, action_symbols_range);
  for (int i = 0; i < offset - 1; i++) {
    if (action_symbols_range[i] > (cli_options->java_bit ? 127 : 255)) {
      btr->value = 0;
      break;
    }
  }
  if (btr->value) {
    if (cli_options->java_bit) {
      prnt_longs("\n    public final static byte asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range, cli_options, of);
    } else {
      prnt_longs("\nconst unsigned char  CLASS_HEADER asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range, cli_options, of);
    }
  } else {
    if (cli_options->java_bit) {
      prnt_longs("\n    public final static char asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range, cli_options, of);
    } else {
      prnt_longs("\nconst unsigned short CLASS_HEADER asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range, cli_options, of);
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
  ffree(action_symbols_range);
  JBitset naction_symbols;
  if (error_maps_bit) {
    calloc0_set(naction_symbols, num_states + 1, dss->non_term_set_size);
  }
  // We now repeat the same process for the domain of the GOTO table.
  for ALL_STATES3(state_no) {
    as_size[state_no] = gd_index[state_no + 1] - gd_index[state_no];
    for (int i = gd_index[state_no]; i <= gd_index[state_no + 1] - 1; i++) {
      int symbol = gd_range[i] - num_terminals;
      SET_BIT_IN(naction_symbols, state_no, symbol);
    }
  }
  partset(naction_symbols, as_size, toutput->state_list, state_start, state_stack, num_non_terminals, false);
  ffree(as_size);
  ffree(naction_symbols.raw);
  // Remap non-terminals
  for (int i = 1; i <= gotodom_size; i++) {
    if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
      gd_range[i] = toutput->symbol_map[gd_range[i]] - num_terminals;
    } else {
      gd_range[i] = toutput->symbol_map[gd_range[i]];
    }
  }
  // Compute and write out the base of the NACTION_SYMBOLS map.
  long *naction_symbols_base = Allocate_long_array(num_states + 1);
  for ALL_STATES3(state_no) {
    naction_symbols_base[toutput->state_list[state_no]] = ABS(state_start[toutput->state_list[state_no]]);
  }
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char nasb[] = {0,\n", 1, num_states, 10, naction_symbols_base, cli_options, of);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER nasb[] = {0,\n", 1, num_states, 10, naction_symbols_base, cli_options, of);
  }
  ffree(naction_symbols_base);
  // Compute and write out the range of the NACTION_SYMBOLS map.
  offset = state_start[num_states + 1];
  long *naction_symbols_range = Allocate_long_array(offset);
  compute_naction_symbols_range(state_start, state_stack, toutput->state_list, naction_symbols_range);
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char nasr[] = {0,\n", 0, offset - 2, 10, naction_symbols_range, cli_options, of);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER nasr[] = {0,\n", 0, offset - 2, 10, naction_symbols_range, cli_options, of );
  }
  PRNT3("    Storage required for NACTION_SYMBOLS_BASE map: %ld Bytes", 2 * num_states);
  PRNT3("    Storage required for NACTION_SYMBOLS_RANGE map: %d Bytes", 2 * (offset - 1));
  ffree(naction_symbols_range);
  // We write the name_index of each terminal symbol.  The array TEMP
  // is used to remap the NAME_INDEX values based on the new symbol
  // numberings. If time tables are requested, the terminals and non-
  // terminals are mixed together.
  long *temp = Allocate_long_array(num_symbols + 1);
  if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
    for ALL_TERMINALS3(symbol) {
      temp[toutput->symbol_map[symbol]] = symno[symbol].name_index;
    }
    if (num_names <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte terminal_index[] = {0,\n", 1, num_terminals, 10, temp, cli_options, of);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER terminal_index[] = {0,\n", 1, num_terminals, 10, temp, cli_options, of);
      }
      num_bytes = num_terminals;
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char terminal_index[] = {0,\n", 1, num_terminals, 10, temp, cli_options, of);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER terminal_index[] = {0,\n", 1, num_terminals, 10, temp, cli_options, of);
      }
      num_bytes = 2 * num_terminals;
    }
    // Compute and list space required for TERMINAL_INDEX map.
    PRNT3("    Storage required for TERMINAL_INDEX map: %ld Bytes", num_bytes);
    // We write the name_index of each non_terminal symbol. The array
    // TEMP is used to remap the NAME_INDEX values based on the new
    // symbol numberings.
    for ALL_NON_TERMINALS3(symbol) {
      temp[toutput->symbol_map[symbol]] = symno[symbol].name_index;
    }
    if (num_names <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp, cli_options, of);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp, cli_options, of);
      }
      num_bytes = num_non_terminals;
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp, cli_options, of);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp, cli_options, of);
      }
      num_bytes = 2 * num_non_terminals;
    }
    // Compute and list space required for NON_TERMINAL_INDEX map.
    PRNT3("    Storage required for NON_TERMINAL_INDEX map: %ld Bytes", num_bytes);
  } else {
    for ALL_SYMBOLS3(symbol) {
      temp[toutput->symbol_map[symbol]] = symno[symbol].name_index;
    }
    if (num_names <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte symbol_index[] = {0,\n", 1, num_symbols, 10, temp, cli_options, of);
        mystrcpy("    public final static byte terminal_index[] = symbol_index;\n", of);
        mystrcpy("    public final static byte non_terminal_index[] = symbol_index;\n", of);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER symbol_index[] = {0,\n", 1, num_symbols, 10, temp, cli_options, of);
        mystrcpy("const unsigned char  *CLASS_HEADER terminal_index[] = &(symbol_index[0]);\n", of);
        mystrcpy("const unsigned char  *CLASS_HEADER non_terminal_index[] = &(symbol_index[0]);\n", of);
      }
      num_bytes = num_symbols;
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char symbol_index[] = {0,\n", 1, num_symbols, 10, temp, cli_options, of);
        mystrcpy("    public final static char terminal_index[] = symbol_index[0];\n", of);
        mystrcpy("    public final static char non_terminal_index[] = symbol_index;\n", of);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER symbol_index[] = {0,\n", 1, num_symbols, 10, temp, cli_options, of);
        mystrcpy("const unsigned short *CLASS_HEADER terminal_index[] = &(symbol_index[0]);\n", of);
        mystrcpy("const unsigned short *CLASS_HEADER non_terminal_index[] = &(symbol_index[0]);\n", of);
      }
      num_bytes = 2 * num_symbols;
    }
    // Compute and list space required for SYMBOL_INDEX map.
    PRNT3("    Storage required for SYMBOL_INDEX map: %ld Bytes", num_bytes);
  }
  if (num_scopes > 0) {
    short root = 0;
    short *list;
    list = Allocate_short_array(scope_rhs_size + 1);
    for (int i = 1; i <= scope_rhs_size; i++) {
      if (scope_right_side[i] != 0) {
        scope_right_side[i] = toutput->symbol_map[scope_right_side[i]];
      }
    }
    for (int i = 1; i <= num_scopes; i++) {
      scope[i].look_ahead = toutput->symbol_map[scope[i].look_ahead];
      if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
        scope[i].lhs_symbol = toutput->symbol_map[scope[i].lhs_symbol] - num_terminals;
      } else {
        scope[i].lhs_symbol = toutput->symbol_map[scope[i].lhs_symbol];
      }
    }
    // Mark all elements of prefix strings.
    for (int i = 1; i <= scope_rhs_size; i++) {
      list[i] = -1;
    }
    for (int i = 1; i <= num_scopes; i++) {
      if (list[scope[i].suffix] < 0) {
        list[scope[i].suffix] = root;
        root = scope[i].suffix;
      }
    }
    for (; root != 0; root = list[root]) {
      for (int j = root; scope_right_side[j] != 0; j++) {
        int k = scope_right_side[j];
        scope_right_side[j] = temp[k];
      }
    }
    ffree(list);
  }
  if (cli_options->java_bit) {
    // Print java names.
    long num_bytes = 0;
    max_name_length = 0;
    mystrcpy("\n    public final static String name[] = { null,\n", of);
    for (int i = 1; i <= num_names; i++) {
      char tok[SYMBOL_SIZE + 1];
      strcpy(tok, RETRIEVE_NAME(i));
      const int len = strlen(tok);
      num_bytes += len * 2;
      if (max_name_length < len) {
        max_name_length = len;
      }
      padline();
      *output_ptr++ = '\"';
      int k = 0;
      for (int j = 0; j < len; j++) {
        if (tok[j] == '\"' || tok[j] == '\\') {
          *output_ptr++ = '\\';
        }
        if (tok[j] == '\n') {
          *output_ptr++ = cli_options->escape;
        } else {
          *output_ptr++ = tok[j];
        }
        k++;
        if (k == 30 && j != len - 1) {
          k = 0;
          *output_ptr++ = '\"';
          *output_ptr++ = ' ';
          *output_ptr++ = '+';
          *output_ptr++ = '\n';
          BUFFER_CHECK(of->sysdcl);
          padline();
          *output_ptr++ = '\"';
        }
      }
      *output_ptr++ = '\"';
      if (i < num_names) {
        *output_ptr++ = ',';
      }
      *output_ptr++ = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                          };\n", of);
    }
    // Compute and list space required for STRING_BUFFER map.
    PRNT3("    Storage required for STRING_BUFFER map: %ld Bytes", num_bytes);
  } else {
    // Print C names.
    long *name_len = Allocate_long_array(num_names + 1);
    long num_bytes = 0;
    max_name_length = 0;
    mystrcpy("\nconst char  CLASS_HEADER string_buffer[] = {0,\n", of);
    int n = 0;
    padline();
    for (int i = 1; i <= num_names; i++) {
      char tok[SYMBOL_SIZE + 1];
      strcpy(tok, RETRIEVE_NAME(i));
      name_len[i] = strlen(tok);
      num_bytes += name_len[i];
      if (max_name_length < name_len[i]) {
        max_name_length = name_len[i];
      }
      int k = 0;
      for (int j = 0; j < name_len[i]; j++) {
        *output_ptr++ = '\'';
        if (tok[k] == '\'' || tok[k] == '\\') {
          *output_ptr++ = '\\';
        }
        if (tok[k] == '\n') {
          *output_ptr++ = cli_options->escape;
        } else {
          *output_ptr++ = tok[k];
        }
        k++;
        *output_ptr++ = '\'';
        *output_ptr++ = ',';
        n++;
        if (n == 10 && !(i == num_names && j == name_len[i] - 1)) {
          n = 0;
          *output_ptr++ = '\n';
          BUFFER_CHECK(of->sysdcl);
          padline();
        }
      }
    }
    *(output_ptr - 1) = '\n'; /*overwrite last comma*/
    BUFFER_CHECK(of->sysdcl);
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                          };\n", of);
    }
    // Compute and list space required for STRING_BUFFER map.
    PRNT3("    Storage required for STRING_BUFFER map: %ld Bytes", num_bytes);
    // Write out NAME_START array
    mystrcpy("\nconst unsigned short CLASS_HEADER name_start[] = {0,\n", of);
    padline();
    int j = 1;
    int k = 0;
    for (int i = 1; i <= num_names; i++) {
      itoc(j);
      *output_ptr++ = ',';
      j += name_len[i];
      k++;
      if (k == 10 && i != num_names) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of );
    } else {
      mystrcpy("                          };\n", of);
    }
    // Compute and list space required for NAME_START map.
    PRNT3("    Storage required for NAME_START map: %ld Bytes", 2 * num_names);
    // Write out NAME_LENGTH array
    prnt_longs("\nconst unsigned char  CLASS_HEADER name_length[] = {0,\n", 1, num_names, 10, name_len, cli_options, of);
    // Compute and list space required for NAME_LENGTH map.
    PRNT3("    Storage required for NAME_LENGTH map: %ld Bytes", num_names);
    ffree(name_len);
  }
  if (num_scopes > 0) {
    if (scope_rhs_size <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_prefix[] = {\n", of);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_prefix[] = {\n", of);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_prefix[] = {\n", of);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_prefix[] = {\n", of);
      }
    }
    padline();
    int k = 0;
    for (int i = 1; i <= num_scopes; i++) {
      itoc(scope[i].prefix);
      *output_ptr++ = ',';
      k++;
      if (k == 10 && i != num_scopes) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                          };\n", of);
    }
    if (scope_rhs_size <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_suffix[] = {\n", of);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_suffix[] = {\n", of);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_suffix[] = {\n", of);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_suffix[] = {\n", of);
      }
    }
    padline();
    k = 0;
    for (int i = 1; i <= num_scopes; i++) {
      itoc(scope[i].suffix);
      *output_ptr++ = ',';
      k++;
      if (k == 10 && i != num_scopes) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                          };\n", of);
    }
    if (num_symbols <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_lhs[] = {\n", of);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_lhs[] = {\n", of);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_lhs[] = {\n", of);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_lhs[] = {\n", of);
      }
    }
    padline();
    k = 0;
    for (int i = 1; i <= num_scopes; i++) {
      itoc(scope[i].lhs_symbol);
      *output_ptr++ = ',';
      k++;
      if (k == 10 && i != num_scopes) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                          };\n", of);
    }
    if (num_terminals <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_la[] = {\n", of);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_la[] = {\n", of);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_la[] = {\n", of);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_la[] = {\n", of);
      }
    }
    padline();
    k = 0;
    for (int i = 1; i <= num_scopes; i++) {
      itoc(scope[i].look_ahead);
      *output_ptr++ = ',';
      k++;
      if (k == 10 && i != num_scopes) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                          };\n", of);
    }
    if (scope_state_size <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_state_set[] = {\n", of);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_state_set[] = {\n", of);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_state_set[] = {\n", of);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_state_set[] = {\n", of);
      }
    }
    padline();
    k = 0;
    for (int i = 1; i <= num_scopes; i++) {
      itoc(scope[i].state_set);
      *output_ptr++ = ',';
      k++;
      if (k == 10 && i != num_scopes) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                          };\n", of);
    }
    if (num_symbols <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte scope_rhs[] = {0,\n", 1, scope_rhs_size, 10, scope_right_side, cli_options, of);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER scope_rhs[] = {0,\n", 1, scope_rhs_size, 10, scope_right_side, cli_options, of);
      }
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char scope_rhs[] = {0,\n", 1, scope_rhs_size, 10, scope_right_side, cli_options, of);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER scope_rhs[] = {0,\n", 1, scope_rhs_size, 10, scope_right_side, cli_options, of);
      }
    }
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char scope_state[] = {0,\n", of);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER scope_state[] = {0,\n", of);
    }
    padline();
    k = 0;
    for (int i = 1; i <= scope_state_size; i++) {
      if (scope_state[i] == 0) {
        itoc(0);
      } else {
        itoc(toutput->state_index[scope_state[i]] + num_rules);
      }
      *output_ptr++ = ',';
      k++;
      if (k == 10 && i != scope_state_size) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                          };\n", of);
    }
    if (num_symbols <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte in_symb[] = {0,\n", of);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER in_symb[] = {0,\n", of);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char in_symb[] = {0,\n", of);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER in_symb[] = {0,\n", of);
      }
    }
    // Transition symbol
    padline();
    *output_ptr++ = '0';
    *output_ptr++ = ',';
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
      itoc(toutput->symbol_map[i]);
      *output_ptr++ = ',';
      k++;
      if (k == 10 && state_no != num_states) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                          };\n", of);
    }
  }
}

static void common(const bool byte_check_bit, struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of) {
  struct ByteTerminalRange btr = (struct ByteTerminalRange) {
    .value = true
  };

  // Write table common.
  {
    if (error_maps_bit) {
      print_error_maps(cli_options, toutput, dss, ctp, of, &btr);
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
      mystrcpy("}\n", of);
    }
    fwrite(output_buffer, sizeof(char), output_ptr - &output_buffer[0], of->sysdcl);
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
      snprintf(line, sizeof(line), "      %s%s%s = %li,\n", cli_options->prefix, tok, cli_options->suffix, toutput->symbol_map[symbol]);
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
    if (error_maps_bit) {
      if (cli_options->java_bit) {
        fprintf(of->sysdef,
                "      ERROR_SYMBOL      = %d,\n"
                "      MAX_NAME_LENGTH   = %ld,\n"
                "      NUM_STATES        = %ld,\n\n",
                error_image,
                max_name_length,
                num_states);
      } else {
        fprintf(of->sysdef,
                "      ERROR_SYMBOL      = %d,\n"
                "      MAX_DISTANCE      = %d,\n"
                "      MIN_DISTANCE      = %d,\n"
                "      MAX_NAME_LENGTH   = %ld,\n"
                "      MAX_TERM_LENGTH   = %ld,\n"
                "      NUM_STATES        = %ld,\n\n",
                error_image,
                cli_options->maximum_distance,
                cli_options->minimum_distance,
                max_name_length,
                max_name_length,
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
              num_scopes - 1,
              num_scopes,
              cli_options->read_reduce_bit && cli_options->lalr_level > 1
                ? error_act + num_rules
                : error_act,
              cli_options->lalr_level,
              num_rules,
              num_terminals,
              num_non_terminals,
              num_symbols,
              toutput->state_index[1] + num_rules,
              eoft_image,
              eolt_image,
              accept_act,
              error_act);
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
              num_scopes - 1,
              num_scopes,
              cli_options->read_reduce_bit && cli_options->lalr_level > 1
                ? error_act + num_rules
                : error_act,
              cli_options->lalr_level,
              num_rules,
              num_terminals,
              num_non_terminals,
              num_symbols,
              toutput->state_index[1] + num_rules,
              eoft_image,
              eolt_image,
              accept_act,
              error_act);
    }
  }

  // Print externs.
  {
    if (cli_options->c_bit || cli_options->cpp_bit) {
      fprintf(of->sysprs,
              "%s SCOPE_REPAIR\n"
              "%s FULL_DIAGNOSIS\n"
              "%s SPACE_TABLES\n\n",
              num_scopes > 0 ? "#define" : "#undef ",
              error_maps_bit ? "#define" : "#undef ",
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
      if (error_maps_bit) {
        fprintf(of->sysprs, "    static int original_state(int state) { return -%s[state]; }\n", cli_options->table_opt.value == OPTIMIZE_TIME.value ? "check" : "base_check");
      }
      if (error_maps_bit) {
        fprintf(of->sysprs,
                "    static int asi(int state) "
                "{ return asb[original_state(state)]; }\n"
                "    static int nasi(int state) "
                "{ return nasb[original_state(state)]; }\n");
        if (num_scopes > 0) {
          fprintf(of->sysprs,
                  "    static int in_symbol(int state) "
                  "{ return in_symb[original_state(state)]; }\n");
        }
      }
      fprintf(of->sysprs, "\n");
    } else if (cli_options->java_bit) {
      fprintf(of->sysprs, "abstract class %s extends %s implements %s\n{\n", of->prs_tag, of->dcl_tag, of->def_tag);
      if (error_maps_bit) {
        fprintf(of->sysprs, "    public final static int original_state(int state) { return -%s(state); }\n", cli_options->table_opt.value == OPTIMIZE_TIME.value ? "check" : "base_check");
        if (error_maps_bit) {
          fprintf(of->sysprs, "    public final static int asi(int state) { return asb[original_state(state)]; }\n");
          fprintf(of->sysprs, "    static int nasi(int state) { return nasb[original_state(state)]; }\n");
          if (num_scopes > 0)
            fprintf(of->sysprs, "    public final static int in_symbol(int state) { return in_symb[original_state(state)]; }\n");
        }
        fprintf(of->sysprs, "\n");
      }
    }
    if (cli_options->c_bit || cli_options->cpp_bit) {
      fprintf(of->sysprs, "%s const unsigned char  rhs[];\n", cli_options->c_bit ? "extern" : "    static");
      if (ctp->check_size > 0 || cli_options->table_opt.value == OPTIMIZE_TIME.value) {
        const bool small = byte_check_bit && !error_maps_bit;
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
      if (error_maps_bit) {
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
        if (num_scopes > 0) {
          fprintf(of->sysprs, "%s const unsigned %s scope_prefix[];\n"
                  "%s const unsigned %s scope_suffix[];\n"
                  "%s const unsigned %s scope_lhs[];\n"
                  "%s const unsigned %s scope_la[];\n"
                  "%s const unsigned %s scope_state_set[];\n"
                  "%s const unsigned %s scope_rhs[];\n"
                  "%s const unsigned short scope_state[];\n"
                  "%s const unsigned %s in_symb[];\n",
                  cli_options->c_bit ? "extern" : "    static",
                  scope_rhs_size <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  scope_rhs_size <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  num_symbols <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  num_terminals <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  scope_state_size <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
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

/// FIELD takes as arguments two integers: NUM and LEN.  NUM is an integer
/// containing at most LEN digits which is converted into a character
/// string and placed in the iobuffer.
/// Leading zeros are replaced by blanks and if the number is negative,  a
/// leading "-" is added.
void field(const long num, const int len) {
  register int val = ABS(num);
  register char *p = output_ptr + len;
  do {
    p--;
    *p = digits[val % 10];
    val /= 10;
  } while (val > 0 && p > output_ptr);
  if (num < 0 && p > output_ptr) {
    p--;
    *p = '-';
  }
  while (p > output_ptr) {
    p--;
    *p = ' ';
  }
  output_ptr += len;
}

///  SORTDES sorts the elements of ARRAY and COUNT in the range LOW..HIGH
/// based on the values of the elements of COUNT. Knowing that the maximum
/// value of the elements of count cannot exceed MAX and cannot be lower
/// than zero, we can use a bucket sort technique.
void sortdes(long array[], long count[], const long low, const long high, const long max) {
  // BUCKET is used to hold the roots of lists that contain the
  // elements of each bucket.  LIST is used to hold these lists.
  long *bucket = Allocate_long_array(max + 1);
  long *list = Allocate_long_array(high - low + 1);
  for (register int i = 0; i <= max; i++) {
    bucket[i] = NIL;
  }
  // We now partition the elements to be sorted and place them in their
  // respective buckets.  We iterate backward over ARRAY and COUNT to
  // keep the sorting stable since elements are inserted in the buckets
  // in stack-fashion.
  //
  //   NOTE that it is known that the values of the elements of ARRAY
  // also lie in the range LOW..HIGH.
  for (register long i = high; i >= low; i--) {
    long k = count[i];
    long element = array[i];
    list[element - low] = bucket[k];
    bucket[k] = element;
  }
  // Iterate over each bucket, and place elements in ARRAY and COUNT
  // in sorted order.  The iteration is done backward because we want
  // the arrays sorted in descending order.
  long k = low;
  for (register long i = max; i >= 0; i--) {
    for (long element = bucket[i]; element != NIL; element = list[element - low], k++) {
      array[k] = element;
      count[k] = i;
    }
  }
  ffree(bucket);
  ffree(list);
}

/// This procedure is invoked when the TABLE being used is not large
/// enough.  A new table is allocated, the information from the old table
/// is copied, and the old space is released.
void reallocate(struct CLIOptions *cli_options, struct CTabsProps* ctp) {
  if (ctp->table_size == MAX_TABLE_SIZE) {
    PRNTERR2("Table has exceeded maximum limit of %ld", MAX_TABLE_SIZE);
    exit(12);
  }
  const register int old_size = ctp->table_size;
  ctp->table_size = MIN(ctp->table_size + ctp->increment_size, MAX_TABLE_SIZE);
  if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    PRNT3("Reallocating storage for TIME table, adding %ld entries", ctp->table_size - old_size);
  } else {
    PRNT3("Reallocating storage for SPACE table, adding %ld entries", ctp->table_size - old_size);
  }
  long *n = Allocate_long_array(ctp->table_size + 1);
  long *p = Allocate_long_array(ctp->table_size + 1);
  // Copy old information
  for (register int i = 1; i <= old_size; i++) {
    n[i] = next[i];
    p[i] = previous[i];
  }
  ffree(next);
  ffree(previous);
  next = n;
  previous = p;
  if (first_index == NIL) {
    first_index = old_size + 1;
    previous[first_index] = NIL;
  } else {
    next[last_index] = old_size + 1;
    previous[old_size + 1] = last_index;
  }
  next[old_size + 1] = old_size + 2;
  for (register int i = old_size + 2; i < (int) ctp->table_size; i++) {
    next[i] = i + 1;
    previous[i] = i - 1;
  }
  last_index = ctp->table_size;
  next[last_index] = NIL;
  previous[last_index] = last_index - 1;
}

void print_space_parser(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, long *term_state_index, long *shift_check_index, struct CTabsProps* ctp, struct new_state_type *new_state_element, short *shift_image, short *real_shift_number, struct OutputFiles* of)  {
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
  long *check = Allocate_long_array(ctp->table_size + 1);
  long *action = Allocate_long_array(ctp->table_size + 1);
  output_ptr = &output_buffer[0];
  // Prepare header card with proper information, and write it out.
  long offset = error_act;
  long la_state_offset;
  if (cli_options->lalr_level > 1) {
    if (cli_options->read_reduce_bit) {
      offset += num_rules;
    }
    la_state_offset = offset;
  } else {
    la_state_offset = error_act;
  }
  if (offset > MAX_TABLE_SIZE + 1) {
    PRNTERR2("Table contains entries that are > %ld; Processing stopped.", MAX_TABLE_SIZE + 1);
    exit(12);
  }
  for (int i = 1; i <= ctp->check_size; i++) {
    check[i] = DEFAULT_SYMBOL;
  }
  for (int i = 1; i <= (int) ctp->action_size; i++) {
    action[i] = error_act;
  }
  //    Update the default non-terminal action of each state with the
  // appropriate corresponding terminal state starting index.
  for (int i = 1; i <= ctp->num_terminal_states; i++) {
    int indx = term_state_index[i];
    int state_no = new_state_element[i].image;
    // Update the action link between the non-terminal and terminal
    // tables. If error-maps are requested, an indirect linking is made
    // as follows:
    //  Each non-terminal row identifies its original state number, and
    // a new vector START_TERMINAL_STATE indexable by state numbers
    // identifies the starting point of each state in the terminal table.
    if (state_no <= num_states) {
      for (; state_no != NIL; state_no = toutput->state_list[state_no]) {
        action[toutput->state_index[state_no]] = indx;
      }
    } else {
      for (; state_no != NIL; state_no = toutput->state_list[state_no]) {
        int act = la_state_offset + indx;
        toutput->state_index[state_no] = act;
      }
    }
  }
  //  Now update the non-terminal tables with the non-terminal actions.
  for ALL_STATES3(state_no) {
    struct goto_header_type go_to;
    int indx = toutput->state_index[state_no];
    go_to = statset[state_no].go_to;
    for (int j = 1; j <= go_to.size; j++) {
      int symbol = go_to.map[j].symbol;
      int i = indx + symbol;
      if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
        check[i] = symbol;
      }
      int act = go_to.map[j].action;
      if (act > 0) {
        action[i] = toutput->state_index[act] + num_rules;
        goto_count++;
      } else {
        action[i] = -act;
        goto_reduce_count++;
      }
    }
  }
  if (error_maps_bit) {
    if (ctp->check_size == 0) {
      ctp->check_size = ctp->action_size;
      for (int i = 0; i <= ctp->check_size; i++) {
        check[i] = 0;
      }
    }
    for ALL_STATES3(state_no) {
      check[toutput->state_index[state_no]] = -state_no;
    }
  }
  for (int i = 1; i <= ctp->check_size; i++) {
    if (check[i] < 0 || check[i] > (cli_options->java_bit ? 127 : 255)) {
      byte_check_bit = false;
    }
  }
  if (cli_options->c_bit) {
    mystrcpy("\n#define CLASS_HEADER\n\n", of);
  } else if (cli_options->cpp_bit) {
    mystrcpy("\n#define CLASS_HEADER ", of);
    mystrcpy(of->prs_tag, of);
    mystrcpy("_table::\n\n", of);
  } else {
    mystrcpy("abstract class ", of);
    mystrcpy(of->dcl_tag, of);
    mystrcpy(" implements ", of);
    mystrcpy(of->def_tag, of);
    mystrcpy("\n{\n", of);
  }
  // Write size of right hand side of rules followed by CHECK table.
  if (cli_options->java_bit) {
    mystrcpy("    public final static byte rhs[] = {0,\n", of);
  } else {
    mystrcpy("const unsigned char  CLASS_HEADER rhs[] = {0,\n", of);
  }
  padline();
  int k = 0;
  for (int i = 1; i <= num_rules; i++) {
    k++;
    if (k > 15) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(of->sysdcl);
      padline();
      k = 1;
    }
    itoc(RHS_SIZE(i));
    *output_ptr++ = ',';
  }
  *(output_ptr - 1) = '\n';
  BUFFER_CHECK(of->sysdcl);
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of);
  } else {
    mystrcpy("                 };\n", of);
  }
  *output_ptr++ = '\n';
  if (ctp->check_size > 0) {
    if (byte_check_bit && !error_maps_bit) {
      if (cli_options->java_bit) {
        mystrcpy("    public final static byte check_table[] = {\n", of);
      } else {
        mystrcpy("const unsigned char  CLASS_HEADER check_table[] = {\n", of);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("    public final static short check_table[] = {\n", of);
      } else {
        mystrcpy("const   signed short CLASS_HEADER check_table[] = {\n", of);
      }
    }
    padline();
    k = 0;
    for (int i = 1; i <= ctp->check_size; i++) {
      k++;
      if (k > 10) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 1;
      }
      itoc(check[i]);
      *output_ptr++ = ',';
    }
    *(output_ptr - 1) = '\n';
    BUFFER_CHECK(of->sysdcl);
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                 };\n", of);
    }
    *output_ptr++ = '\n';
    if (byte_check_bit && !error_maps_bit) {
      if (cli_options->java_bit) {
        mystrcpy("    public final static byte base_check(int i)\n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n", of);
      } else {
        mystrcpy("const unsigned char  *CLASS_HEADER base_check = &(check_table[0]) - (NUM_RULES + 1);\n", of);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("    public final static short base_check(int i) \n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n", of);
      } else {
        mystrcpy("const   signed short *CLASS_HEADER base_check = &(check_table[0]) - (NUM_RULES + 1);\n", of);
      }
    }
    *output_ptr++ = '\n';
  }
  // Write left hand side symbol of rules followed by ACTION table.
  if (cli_options->java_bit) {
    mystrcpy("    public final static char lhs[] = {0,\n", of);
  } else {
    mystrcpy("const unsigned short CLASS_HEADER lhs[] = {0,\n", of);
  }
  padline();
  k = 0;
  for (int i = 1; i <= num_rules; i++) {
    itoc(toutput->symbol_map[rules[i].lhs] - num_terminals);
    *output_ptr++ = ',';
    k++;
    if (k == 15) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(of->sysdcl);
      padline();
      k = 0;
    }
  }
  *output_ptr++ = '\n';
  *output_ptr++ = '\n';
  BUFFER_CHECK(of->sysdcl);
  padline();
  k = 0;
  if (error_maps_bit) {
    int max_indx;
    max_indx = accept_act - num_rules - 1;
    for (int i = 1; i <= max_indx; i++) {
      check[i] = OMEGA;
    }
    for ALL_STATES3(state_no) {
      check[toutput->state_index[state_no]] = state_no;
    }
    int j = num_states + 1;
    for (int i = max_indx; i >= 1; i--) {
      int state_no = check[i];
      if (state_no != OMEGA) {
        j--;
        toutput->ordered_state[j] = i + num_rules;
        toutput->state_list[j] = state_no;
      }
    }
  }
  for (int i = 1; i <= (int) ctp->action_size; i++) {
    itoc(action[i]);
    *output_ptr++ = ',';
    k++;
    if (k == 10 && i != (int) ctp->action_size) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(of->sysdcl);
      padline();
      k = 0;
    }
  }
  if (k != 0) {
    *(output_ptr - 1) = '\n';
    BUFFER_CHECK(of->sysdcl);
  }
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of);
  } else {
    mystrcpy("                 };\n", of);
  }
  *output_ptr++ = '\n';
  BUFFER_CHECK(of->sysdcl);
  if (cli_options->java_bit) {
    mystrcpy("    public final static char base_action[] = lhs;\n", of);
  } else {
    mystrcpy("const unsigned short *CLASS_HEADER base_action = lhs;\n", of);
  }
  *output_ptr++ = '\n';
  // Initialize the terminal tables,and update with terminal actions.
  for (int i = 1; i <= ctp->term_check_size; i++) {
    check[i] = DEFAULT_SYMBOL;
  }
  for (int i = 1; i <= ctp->term_action_size; i++) {
    action[i] = error_act;
  }
  for (int state_no = 1; state_no <= ctp->num_terminal_states; state_no++) {
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
          result_act = toutput->state_index[act];
          la_shift_count++;
        } else if (act > 0) {
          result_act = toutput->state_index[act] + num_rules;
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
  if (num_terminals <= (cli_options->java_bit ? 127 : 255)) {
    if (cli_options->java_bit) {
      prnt_longs("\n    public final static byte term_check[] = {0,\n", 1, ctp->term_check_size, 15, check, cli_options, of);
    } else {
      prnt_longs("\nconst unsigned char  CLASS_HEADER term_check[] = {0,\n", 1, ctp->term_check_size, 15, check, cli_options, of);
    }
  } else {
    if (cli_options->java_bit) {
      prnt_longs("\n    public final static char term_check[] = {0,\n", 1, ctp->term_check_size, 15, check, cli_options, of);
    } else {
      prnt_longs("\nconst unsigned short CLASS_HEADER term_check[] = {0,\n", 1, ctp->term_check_size, 15, check, cli_options, of);
    }
  }
  // Write Terminal Action Table.
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char term_action[] = {0,\n", 1, ctp->term_action_size, 10, action, cli_options, of);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER term_action[] = {0,\n", 1, ctp->term_action_size, 10, action, cli_options, of);
  }
  // If GOTO_DEFAULT is requested, we print out the GOTODEF vector.
  if (cli_options->goto_default_bit) {
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char default_goto[] = {0,\n", of);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER default_goto[] = {0,\n", of);
    }
    padline();
    k = 0;
    for ALL_NON_TERMINALS3(symbol) {
      int act = gotodef[symbol];
      long result_act;
      if (act < 0) {
        result_act = -act;
      } else if (act == 0) {
        result_act = error_act;
      } else {
        result_act = toutput->state_index[act] + num_rules;
      }
      itoc(result_act);
      *output_ptr++ = ',';
      k++;
      if (k == 10 && symbol != num_symbols) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                 };\n", of);
    }
  }
  if (cli_options->shift_default_bit) {
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char default_reduce[] = {0,\n", of);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER default_reduce[] = {0,\n", of);
    }
    padline();
    k = 0;
    for (int i = 1; i <= ctp->num_terminal_states; i++) {
      struct reduce_header_type red;
      red = new_state_element[i].reduce;
      itoc(red.map[0].rule_number);
      *output_ptr++ = ',';
      k++;
      if (k == 10 && i != ctp->num_terminal_states) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                 };\n", of);
    }
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char shift_state[] = {0,\n", of);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER shift_state[] = {0,\n", of);
    }
    padline();
    k = 0;
    for (int i = 1; i <= ctp->num_terminal_states; i++) {
      itoc(shift_check_index[shift_image[i]]);
      *output_ptr++ = ',';
      k++;
      if (k == 10 && i != ctp->num_terminal_states) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                 };\n", of);
    }
    for (int i = 1; i <= ctp->shift_check_size; i++) {
      check[i] = DEFAULT_SYMBOL;
    }
    for (int i = 1; i <= ctp->shift_domain_count; i++) {
      struct shift_header_type sh;
      int indx = shift_check_index[i];
      sh = shift[real_shift_number[i]];
      for (int j = 1; j <= sh.size; j++) {
        int symbol = sh.map[j].symbol;
        check[indx + symbol] = symbol;
      }
    }
    if (num_terminals <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte shift_check[] = {0,\n", of);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER shift_check[] = {0,\n", of);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char shift_check[] = {0,\n", of);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER shift_check[] = {0,\n", of);
      }
    }
    padline();
    k = 0;
    int ii;
    for (ii = 1; ii <= ctp->shift_check_size; ii++) {
      itoc(check[ii]);
      *output_ptr++ = ',';
      k++;
      if (k == 10 && ii != ctp->shift_check_size) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                 };\n", of);
    }
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char default_shift[] = {0,\n", of);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER default_shift[] = {0,\n", of);
    }
    padline();
    k = 0;
    for ALL_TERMINALS3(symbol) {
      int act = shiftdf[symbol];
      long result_act;
      if (act < 0) {
        result_act = -act + error_act;
      } else if (act == 0) {
        result_act = error_act;
      } else if (act > num_states) {
        result_act = toutput->state_index[act];
      } else {
        result_act = toutput->state_index[act] + num_rules;
      }
      if (result_act > MAX_TABLE_SIZE + 1) {
        PRNTERR2("Table contains look-ahead shift entry that is >%ld; Processing stopped.", MAX_TABLE_SIZE + 1);
        return;
      }
      itoc(result_act);
      *output_ptr++ = ',';
      k++;
      if (k == 10 && ii != num_terminals) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                 };\n", of);
    }
  }
  ffree(check);
  ffree(action);
  common(byte_check_bit, cli_options, toutput, dss, ctp, of);
}

void print_time_parser(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of) {
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
  output_ptr = &output_buffer[0];
  long *check = next;
  long *action = previous;
  long offset = error_act;
  int la_state_offset;
  if (cli_options->lalr_level > 1) {
    if (cli_options->read_reduce_bit) {
      offset += num_rules;
    }
    la_state_offset = offset;
  } else {
    la_state_offset = error_act;
  }
  if (offset > MAX_TABLE_SIZE + 1) {
    PRNTERR2(msg_line, "Table contains entries that are > %ld; Processing stopped.", MAX_TABLE_SIZE + 1);
    exit(12);
  }
  // Initialize all unfilled slots with default values.
  // RECALL that the vector "check" is aliased to the vector "next".
  long indx;
  indx = first_index;
  for (long i = indx; i != NIL && i <= ctp->action_size; i = indx) {
    indx = next[i];
    check[i] = DEFAULT_SYMBOL;
    action[i] = error_act;
  }
  for (long i = ctp->action_size + 1; i <= ctp->table_size; i++) {
    check[i] = DEFAULT_SYMBOL;
  }
  // We set the rest of the table with the proper table entries.
  for (int state_no = 1; state_no <= max_la_state; state_no++) {
    struct shift_header_type sh;
    struct reduce_header_type red;
    indx = toutput->state_index[state_no];
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
          action[i] = toutput->state_index[act] + num_rules;
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
        result_act = la_state_offset + toutput->state_index[act];
        la_shift_count++;
      } else if (act > 0) {
        result_act = toutput->state_index[act] + num_rules;
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
    short default_rule = red.map[0].rule_number;
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
    int i = indx + DEFAULT_SYMBOL;
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
    snprintf(msg_line, sizeof(msg_line), "     Number of Look-Ahead Shifts: %d", la_shift_count);
    PRNT(msg_line);
  }
  PRNT3("     Number of Gotos: %d", goto_count);
  PRNT3("     Number of Goto/Reduces: %d", goto_reduce_count);
  PRNT3("     Number of Reduces: %d", reduce_count);
  PRNT3("     Number of Defaults: %d", default_count);
  if (error_maps_bit) {
    for ALL_STATES3(state_no) {
      check[toutput->state_index[state_no]] = -state_no;
    }
  }
  for (int i = 1; i <= (int) ctp->table_size; i++) {
    if (check[i] < 0 || check[i] > (cli_options->java_bit ? 127 : 255)) {
      byte_check_bit = 0;
    }
  }
  if (cli_options->c_bit) {
    mystrcpy("\n#define CLASS_HEADER\n\n", of);
  } else if (cli_options->cpp_bit) {
    mystrcpy("\n#define CLASS_HEADER ", of);
    mystrcpy(of->prs_tag, of);
    mystrcpy("_table::\n\n", of);
  } else if (cli_options->java_bit) {
    mystrcpy("abstract class ", of);
    mystrcpy(of->dcl_tag, of);
    mystrcpy(" implements ", of);
    mystrcpy(of->def_tag, of);
    mystrcpy("\n{\n", of);
  }
  // Write size of right hand side of rules followed by CHECK table.
  if (cli_options->java_bit) {
    mystrcpy("    public final static byte rhs[] = {0,\n", of);
  } else {
    mystrcpy("const unsigned char  CLASS_HEADER rhs[] = {0,\n", of);
  }
  padline();
  int k = 0;
  for (int i = 1; i <= num_rules; i++) {
    k++;
    if (k > 15) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(of->sysdcl);
      padline();
      k = 1;
    }
    itoc(RHS_SIZE(i));
    *output_ptr++ = ',';
  }
  *(output_ptr - 1) = '\n';
  BUFFER_CHECK(of->sysdcl);
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of);
  } else {
    mystrcpy("                 };\n", of);
  }
  *output_ptr++ = '\n';
  // Write CHECK table.
  if (byte_check_bit && !error_maps_bit) {
    if (cli_options->java_bit) {
      mystrcpy("    public final static byte check_table[] = {\n", of);
    } else {
      mystrcpy("const unsigned char  CLASS_HEADER check_table[] = {\n", of);
    }
  } else {
    if (cli_options->java_bit) {
      mystrcpy("     public final static short check_table[] = {\n", of);
    } else {
      mystrcpy("const   signed short CLASS_HEADER check_table[] = {\n", of);
    }
  }
  padline();
  k = 0;
  for (int i = 1; i <= (int) ctp->table_size; i++) {
    k++;
    if (k > 10) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(of->sysdcl);
      padline();
      k = 1;
    }
    itoc(check[i]);
    *output_ptr++ = ',';
  }
  *(output_ptr - 1) = '\n';
  BUFFER_CHECK(of->sysdcl);
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of);
  } else {
    mystrcpy("                 };\n", of);
  }
  *output_ptr++ = '\n';
  BUFFER_CHECK(of->sysdcl);
  if (byte_check_bit && !error_maps_bit) {
    if (cli_options->java_bit) {
      mystrcpy("    public final static byte check(int i) \n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n", of);
    } else {
      mystrcpy("const unsigned char  *CLASS_HEADER check = &(check_table[0]) - (NUM_RULES + 1);\n", of);
    }
  } else {
    if (cli_options->java_bit) {
      mystrcpy("    public final static short check(int i) \n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n", of);
    } else {
      mystrcpy("const   signed short *CLASS_HEADER check = &(check_table[0]) - (NUM_RULES + 1);\n", of);
    }
  }
  *output_ptr++ = '\n';
  // Write left hand side symbol of rules followed by ACTION table.
  if (cli_options->java_bit) {
    mystrcpy("    public final static char lhs[] = {0,\n", of);
  } else {
    mystrcpy("const unsigned short CLASS_HEADER lhs[] = {0,\n", of);
  }
  padline();
  k = 0;
  for (int i = 1; i <= num_rules; i++) {
    itoc(toutput->symbol_map[rules[i].lhs]);
    *output_ptr++ = ',';
    k++;
    if (k == 15) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(of->sysdcl);
      padline();
      k = 0;
    }
  }
  *output_ptr++ = '\n';
  *output_ptr++ = '\n';
  BUFFER_CHECK(of->sysdcl);
  padline();
  k = 0;
  if (error_maps_bit) {
    long max_indx;
    // Construct a map from new state numbers into original
    //   state numbers using the array check[]
    max_indx = accept_act - num_rules - 1;
    for (int i = 1; i <= max_indx; i++) {
      check[i] = OMEGA;
    }
    for ALL_STATES3(state_no) {
      check[toutput->state_index[state_no]] = state_no;
    }
    int j = num_states + 1;
    for (int i = max_indx; i >= 1; i--) {
      int state_no = check[i];
      if (state_no != OMEGA) {
        toutput->ordered_state[--j] = i + num_rules;
        toutput->state_list[j] = state_no;
      }
    }
  }
  for (int i = 1; i <= (int) ctp->action_size; i++) {
    itoc(action[i]);
    *output_ptr++ = ',';
    k++;
    if (k == 10 && i != (int) ctp->action_size) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(of->sysdcl);
      padline();
      k = 0;
    }
  }
  if (k != 0) {
    *(output_ptr - 1) = '\n';
    BUFFER_CHECK(of->sysdcl);
  }
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of);
  } else {
    mystrcpy("                 };\n", of);
  }
  *output_ptr++ = '\n';
  BUFFER_CHECK(of->sysdcl);
  if (cli_options->java_bit) {
    mystrcpy("    public final static char action[] = lhs;\n", of);
  } else {
    mystrcpy("const unsigned short *CLASS_HEADER action = lhs;\n", of);
  }
  *output_ptr++ = '\n';
  // If GOTO_DEFAULT is requested, we print out the GOTODEF vector.
  if (cli_options->goto_default_bit) {
    short *default_map = Allocate_short_array(num_symbols + 1);
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char default_goto[] = {0,\n", of);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER default_goto[] = {0,\n", of);
    }
    padline();
    k = 0;
    for (int i = 0; i <= num_symbols; i++) {
      default_map[i] = error_act;
    }
    for ALL_NON_TERMINALS3(symbol) {
      int act = gotodef[symbol];
      int result_act;
      if (act < 0) {
        result_act = -act;
      } else if (act > 0) {
        result_act = toutput->state_index[act] + num_rules;
      } else {
        result_act = error_act;
      }
      default_map[toutput->symbol_map[symbol]] = result_act;
    }
    for (int symbol = 1; symbol <= num_symbols; symbol++) {
      itoc(default_map[symbol]);
      *output_ptr++ = ',';
      k++;
      if (k == 10 && symbol != num_symbols) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(of->sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(of->sysdcl);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of);
    } else {
      mystrcpy("                 };\n", of);
    }
  }
  ffree(next);
  ffree(previous);
  common(byte_check_bit, cli_options, toutput, dss, ctp, of);
}

void populate_start_file(FILE **file, char *file_tag, struct CLIOptions *cli_options) {
  if (cli_options->c_bit || cli_options->cpp_bit) {
    fprintf(*file, "#ifndef %s_INCLUDED\n", file_tag);
    fprintf(*file, "#define %s_INCLUDED\n\n", file_tag);
  }
}
