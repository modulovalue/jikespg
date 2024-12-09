static char hostfile[] = __FILE__;

#include <stdlib.h>
#include "lpgparse.h"
#include <string.h>
#include "common.h"
#include <stdlib.h>

char prefix[MAX_PARM_SIZE] = "";
char suffix[MAX_PARM_SIZE] = "";
char msg_line[MAX_MSG_SIZE];

FILE *syslis;
FILE *systab;
FILE *syssym;
FILE *sysdcl;

const long MAX_TABLE_SIZE = (USHRT_MAX < INT_MAX ? USHRT_MAX : INT_MAX) - 1;

struct scope_type *scope = NULL;

int shift_domain_count;
int num_terminal_states;
int check_size;
int term_check_size;
int term_action_size;
int shift_check_size;

struct new_state_type *new_state_element;

short *shift_image = NULL;
short *real_shift_number = NULL;

int *term_state_index = NULL;
int *shift_check_index = NULL;

bool byte_terminal_range = true;

int *symbol_map = NULL;
long *ordered_state = NULL;
long *state_list = NULL;

long *next = NULL;
long *previous = NULL;
long *state_index = NULL;

long table_size;
long action_size;
long increment_size;

long last_non_terminal = 0;
long last_terminal = 0;

long accept_act;
long error_act;
long first_index;
long last_index;
long last_symbol;
long max_name_length = 0;

SET_PTR naction_symbols = NULL;
SET_PTR action_symbols = NULL;

const char digits[] = "0123456789";

/* ITOC takes as arguments an integer NUM. NUM is an integer containing at */
/* most 11 digits which is converted into a character string and placed in  */
/* the iobuffer.  Leading zeros are eliminated and if the number is        */
/* negative, a leading "-" is added.                                       */
void itoc(const int num) {
  char tmp[12];
  register int val = ABS(num);
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

void padline(void) {
  for (register int i = 0; i < 12; i++) {
    *output_ptr++ = ' ';
  }
}

void mystrcpy(const char *str) {
  while (*str != '\0') {
    *output_ptr++ = *str++;
  }
  BUFFER_CHECK(sysdcl);
  BUFFER_CHECK(syssym);
}

void prnt_shorts(const char *title, const int init, const int bound, const int perline, const long *array) {
  mystrcpy(title);
  padline();
  int k = 0;
  for (int i = init; i <= bound; i++) {
    itoc(array[i]);
    *output_ptr++ = COMMA;
    k++;
    if (k == perline && i != bound) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(sysdcl);
      padline();
      k = 0;
    }
  }
  if (k != 0) {
    *(output_ptr - 1) = '\n';
    BUFFER_CHECK(sysdcl);
  }
  if (java_bit) {
    mystrcpy("    };\n");
  } else {
    mystrcpy("                 };\n");
  }
}

void prnt_ints(const char *title, const int init, const int bound, const int perline, const int *array) {
  mystrcpy(title);
  padline();
  int k = 0;
  for (int i = init; i <= bound; i++) {
    itoc(array[i]);
    *output_ptr++ = COMMA;
    k++;
    if (k == perline && i != bound) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(sysdcl);
      padline();
      k = 0;
    }
  }
  if (k != 0) {
    *(output_ptr - 1) = '\n';
    BUFFER_CHECK(sysdcl);
  }
  if (java_bit) {
    mystrcpy("    };\n");
  } else {
    mystrcpy("                 };\n");
  }
}

/* FIELD takes as arguments two integers: NUM and LEN.  NUM is an integer  */
/* containing at most LEN digits which is converted into a character       */
/* string and placed in the iobuffer.                                      */
/* Leading zeros are replaced by blanks and if the number is negative,  a  */
/* leading "-" is added.                                                   */
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

/*  SORTDES sorts the elements of ARRAY and COUNT in the range LOW..HIGH   */
/* based on the values of the elements of COUNT. Knowing that the maximum  */
/* value of the elements of count cannot exceed MAX and cannot be lower    */
/* than zero, we can use a bucket sort technique.                          */
void sortdes(long array[], long count[], const long low, const long high, const long max) {
  register long element;
  register long k;
  /* BUCKET is used to hold the roots of lists that contain the    */
  /* elements of each bucket.  LIST is used to hold these lists.   */
  long *bucket = Allocate_long_array(max + 1);
  long *list = Allocate_long_array(high - low + 1);
  for (register int i = 0; i <= max; i++) {
    bucket[i] = NIL;
  }
  /* We now partition the elements to be sorted and place them in their*/
  /* respective buckets.  We iterate backward over ARRAY and COUNT to  */
  /* keep the sorting stable since elements are inserted in the buckets*/
  /* in stack-fashion.                                                 */
  /*                                                                   */
  /*   NOTE that it is known that the values of the elements of ARRAY  */
  /* also lie in the range LOW..HIGH.                                  */
  for (register long i = high; i >= low; i--) {
    k = count[i];
    element = array[i];
    list[element - low] = bucket[k];
    bucket[k] = element;
  }
  /* Iterate over each bucket, and place elements in ARRAY and COUNT   */
  /* in sorted order.  The iteration is done backward because we want  */
  /* the arrays sorted in descending order.                            */
  k = low;
  for (register long i = max; i >= 0; i--) {
    for (element = bucket[i]; element != NIL; element = list[element - low], k++) {
      array[k] = element;
      count[k] = i;
    }
  }
  ffree(bucket);
  ffree(list);
}

/*   This procedure is invoked when the TABLE being used is not large      */
/* enough.  A new table is allocated, the information from the old table   */
/* is copied, and the old space is released.                               */
void reallocate(void) {
  if (table_size == MAX_TABLE_SIZE) {
    PRNTERR2(msg_line, "Table has exceeded maximum limit of %ld", MAX_TABLE_SIZE);
    exit(12);
  }
  const register int old_size = table_size;
  table_size = MIN(table_size + increment_size, MAX_TABLE_SIZE);
  if (verbose_bit) {
    if (table_opt == OPTIMIZE_TIME) {
      PRNT2(msg_line, "Reallocating storage for TIME table, adding %ld entries", table_size - old_size);
    } else {
      PRNT2(msg_line, "Reallocating storage for SPACE table, adding %ld entries", table_size - old_size);
    }
  }
  long *n = Allocate_long_array(table_size + 1);
  long *p = Allocate_long_array(table_size + 1);
  /* Copy old information */
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
  for (register int i = old_size + 2; i < (int) table_size; i++) {
    next[i] = i + 1;
    previous[i] = i - 1;
  }
  last_index = table_size;
  next[last_index] = NIL;
  previous[last_index] = last_index - 1;
}

/* This procedure computes the range of the ACTION_SYMBOLS map after */
/* Optimal Partitioning has been used to compress that map.  Its     */
/* first argument is an array, STATE_START, that indicates the       */
/* starting location in the compressed vector for each state.  When  */
/* a value of STATE_START is negative it indicates that the state in */
/* question shares its elements with another state.  Its second      */
/* argument, STATE_STACK, is an array that contains the elements of  */
/* the partition created by PARTSET.  Each element of the partition  */
/* is organized as a circular list where the smallest sets appear    */
/* first in the list.                                                */
void compute_action_symbols_range(const long *state_start, const long *state_stack, const long *state_list, long *action_symbols_range) {
  int state;
  int symbol;
  short *symbol_list = Allocate_short_array(num_symbols + 1);
  /* We now write out the range elements of the ACTION_SYMBOLS map.    */
  /* Recall that if STATE_START has a negative value, then the set in  */
  /* question is sharing elements and does not need to be processed.   */
  int k = 0;
  for ALL_SYMBOLS3(j) {
    symbol_list[j] = OMEGA; /* Initialize all links to OMEGA */
  }
  for ALL_STATES3(state_no) {
    const int state_no__ = state_list[state_no];
    if (state_start[state_no__] > 0) {
      int symbol_root = 0; /* Add "fence" element: 0 to list */
      symbol_list[symbol_root] = NIL;
      /* Pop a state from the stack,  and add each of its elements */
      /* that has not yet been processed into the list.            */
      /* Continue until stack is empty...                          */
      /* Recall that the stack is represented by a circular queue. */
      for (bool end_node = (state = state_no__) == NIL; !end_node; end_node = state == state_no__) {
        state = state_stack[state];
        const struct shift_header_type sh = shift[statset[state].shift_number];
        for (int j = 1; j <= sh.size; j++) {
          symbol = sh.map[j].symbol;
          if (symbol_list[symbol] == OMEGA) {
            symbol_list[symbol] = symbol_root;
            symbol_root = symbol;
          }
        }
        const struct reduce_header_type red = reduce[state];
        for (int j = 1; j <= red.size; j++) {
          symbol = red.map[j].symbol;
          if (symbol_list[symbol] == OMEGA) {
            symbol_list[symbol] = symbol_root;
            symbol_root = symbol;
          }
        }
      }
      /* Write the list out.                                       */
      for (symbol = symbol_root; symbol != NIL; symbol = symbol_root) {
        symbol_root = symbol_list[symbol];
        symbol_list[symbol] = OMEGA;
        action_symbols_range[k++] = symbol;
      }
    }
  }
  ffree(symbol_list);
}

/* This procedure computes the range of the NACTION_SYMBOLS map. It  */
/* organization is analoguous to COMPUTE_ACTION_SYMBOLS_RANGE.       */
void compute_naction_symbols_range(const long *state_start, const long *state_stack, const long *state_list, long *naction_symbols_range) {
  int state;
  int symbol;
  short *symbol_list = Allocate_short_array(num_symbols + 1);
  /* We now write out the range elements of the NACTION_SYMBOLS map.   */
  /* Recall that if STATE_START has a negative value, then the set in  */
  /* question is sharing elements and does not need to be processed.   */
  int k = 0;
  for ALL_SYMBOLS3(j) {
    symbol_list[j] = OMEGA; /* Initialize all links to OMEGA */
  }
  for ALL_STATES3(state_no) {
    const int state_no__ = state_list[state_no];
    if (state_start[state_no__] > 0) {
      int symbol_root = 0; /* Add "fence" element: 0 to list */
      symbol_list[symbol_root] = NIL;
      /* Pop a state from the stack,  and add each of its elements */
      /* that has not yet been processed into the list.            */
      /* Continue until stack is empty...                          */
      /* Recall that the stack is represented by a circular queue. */
      for (bool end_node = (state = state_no__) == NIL;
           !end_node; end_node = state == state_no__) {
        state = state_stack[state];
        for (int j = gd_index[state];
             j <= gd_index[state + 1] - 1; j++) {
          symbol = gd_range[j];
          if (symbol_list[symbol] == OMEGA) {
            symbol_list[symbol] = symbol_root;
            symbol_root = symbol;
          }
        }
      }
      /* Write the list out.                                    */
      for (symbol = symbol_root; symbol != NIL; symbol = symbol_root) {
        symbol_root = symbol_list[symbol];
        symbol_list[symbol] = OMEGA;
        naction_symbols_range[k++] = symbol;
      }
    }
  }
  ffree(symbol_list);
}

FILE *sysprs;
FILE *sysdef;

char dcl_tag[SYMBOL_SIZE];
char sym_tag[SYMBOL_SIZE];
char def_tag[SYMBOL_SIZE];
char prs_tag[SYMBOL_SIZE];

void init_file(FILE **file, char *file_name, char *file_tag) {
  const char *p = strrchr(file_name, '.');
  if ((*file = fopen(file_name, "w")) == NULL) {
    fprintf(stderr, "***ERROR: Symbol file \"%s\" cannot be opened\n", file_name);
    exit(12);
  } else {
    memcpy(file_tag, file_name, p - file_name);
    file_tag[p - file_name] = '\0';
    if (c_bit || cpp_bit) {
      fprintf(*file, "#ifndef %s_INCLUDED\n", file_tag);
      fprintf(*file, "#define %s_INCLUDED\n\n", file_tag);
    }
  }
}

void exit_file(FILE **file, char *file_tag) {
  if (c_bit || cpp_bit) {
    fprintf(*file, "\n#endif /* %s_INCLUDED */\n", file_tag);
  }
  fclose(*file);
}

void print_error_maps(void) {
  long *state_start;
  long *state_stack;
  long *temp;
  long *original;
  long *as_size;
  long *action_symbols_base;
  long *action_symbols_range;
  long *naction_symbols_base;
  long *naction_symbols_range;
  int k;
  int offset;
  long num_bytes;
  state_start = Allocate_long_array(num_states + 2);
  state_stack = Allocate_long_array(num_states + 1);
  PRNT("\nError maps storage:");
  /* We now construct a bit map for the set of terminal symbols that  */
  /* may appear in each state. Then, we invoke PARTSET to apply the   */
  /* Partition Heuristic and print it.                                */
  as_size = Allocate_long_array(num_states + 1);
  if (table_opt == OPTIMIZE_TIME) {
    original = Allocate_long_array(num_symbols + 1);
    /* In a compressed TIME table, the terminal and non-terminal */
    /* symbols are mixed together when they are remapped.        */
    /* We shall now recover the original number associated with  */
    /* each terminal symbol since it lies very nicely in the     */
    /* range 1..NUM_TERMINALS.  This will save a considerable    */
    /* amount of space in the bit_string representation of sets  */
    /* as well as time when operations are performed on those    */
    /* bit-strings.                                              */
    for ALL_TERMINALS3(symbol) {
      original[symbol_map[symbol]] = symbol;
    }
  }
  for ALL_STATES3(state_no) {
    struct shift_header_type sh;
    struct reduce_header_type red;
    sh = shift[statset[state_no].shift_number];
    as_size[state_no] = sh.size;
    for (int i = 1; i <= sh.size; i++) {
      int symbol;
      if (table_opt == OPTIMIZE_TIME) {
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
      if (table_opt == OPTIMIZE_TIME) {
        symbol = original[red.map[i].symbol];
      } else {
        symbol = red.map[i].symbol;
      }
      SET_BIT_IN(action_symbols, state_no, symbol);
    }
  }
  partset(action_symbols, as_size, state_list, state_start, state_stack, num_terminals, 0);
  ffree(action_symbols);
  /* Compute and write out the base of the ACTION_SYMBOLS map. */
  action_symbols_base = Allocate_long_array(num_states + 1);
  for ALL_STATES3(state_no) {
    action_symbols_base[state_list[state_no]] = ABS(state_start[state_list[state_no]]);
  }
  if (java_bit) {
    prnt_shorts("\n    public final static char asb[] = {0,\n", 1, num_states, 10, action_symbols_base);
  } else {
    prnt_shorts("\nconst unsigned short CLASS_HEADER asb[] = {0,\n", 1, num_states, 10, action_symbols_base);
  }
  ffree(action_symbols_base);
  /* Compute and write out the range of the ACTION_SYMBOLS map. */
  offset = state_start[num_states + 1];
  action_symbols_range = Allocate_long_array(offset);
  compute_action_symbols_range(state_start, state_stack, state_list, action_symbols_range);
  for (int i = 0; i < offset - 1; i++) {
    if (action_symbols_range[i] > (java_bit ? 127 : 255)) {
      byte_terminal_range = 0;
      break;
    }
  }
  if (byte_terminal_range) {
    if (java_bit) {
      prnt_shorts("\n    public final static byte asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range);
    } else {
      prnt_shorts("\nconst unsigned char  CLASS_HEADER asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range);
    }
  } else {
    if (java_bit) {
      prnt_shorts("\n    public final static char asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range);
    } else {
      prnt_shorts("\nconst unsigned short CLASS_HEADER asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range);
    }
  }
  num_bytes = 2 * num_states;
  PRNT2(msg_line, "    Storage required for ACTION_SYMBOLS_BASE map: %ld Bytes", num_bytes);
  if (table_opt == OPTIMIZE_TIME && last_terminal <= (java_bit ? 127 : 255)) {
    num_bytes = offset - 1;
  } else if (table_opt != OPTIMIZE_TIME && num_terminals <= (java_bit ? 127 : 255)) {
    num_bytes = offset - 1;
  } else {
    num_bytes = 2 * (offset - 1);
  }
  PRNT2(msg_line, "    Storage required for ACTION_SYMBOLS_RANGE map: %ld Bytes", num_bytes);
  ffree(action_symbols_range);
  /* We now repeat the same process for the domain of the GOTO table.    */
  for ALL_STATES3(state_no) {
    as_size[state_no] = gd_index[state_no + 1] - gd_index[state_no];
    for (int i = gd_index[state_no]; i <= gd_index[state_no + 1] - 1; i++) {
      int symbol = gd_range[i] - num_terminals;
      NTSET_BIT_IN(naction_symbols, state_no, symbol);
    }
  }
  partset(naction_symbols, as_size, state_list, state_start, state_stack, num_non_terminals, 0);
  ffree(as_size);
  ffree(naction_symbols);
  /* Remap non-terminals */
  for (int i = 1; i <= gotodom_size; i++) {
    if (table_opt == OPTIMIZE_SPACE) {
      gd_range[i] = symbol_map[gd_range[i]] - num_terminals;
    } else {
      gd_range[i] = symbol_map[gd_range[i]];
    }
  }
  /* Compute and write out the base of the NACTION_SYMBOLS map.*/
  naction_symbols_base = Allocate_long_array(num_states + 1);
  for ALL_STATES3(state_no) {
    naction_symbols_base[state_list[state_no]] = ABS(state_start[state_list[state_no]]);
  }
  if (java_bit) {
    prnt_shorts("\n    public final static char nasb[] = {0,\n", 1, num_states, 10, naction_symbols_base);
  } else {
    prnt_shorts("\nconst unsigned short CLASS_HEADER nasb[] = {0,\n", 1, num_states, 10, naction_symbols_base);
  }
  ffree(naction_symbols_base);
  /* Compute and write out the range of the NACTION_SYMBOLS map.*/
  offset = state_start[num_states + 1];
  naction_symbols_range = Allocate_long_array(offset);
  compute_naction_symbols_range(state_start, state_stack, state_list, naction_symbols_range);
  if (java_bit) {
    prnt_shorts("\n    public final static char nasr[] = {0,\n", 0, offset - 2, 10, naction_symbols_range);
  } else {
    prnt_shorts("\nconst unsigned short CLASS_HEADER nasr[] = {0,\n", 0, offset - 2, 10, naction_symbols_range);
  }
  PRNT2(msg_line, "    Storage required for NACTION_SYMBOLS_BASE map: %ld Bytes", 2 * num_states);
  PRNT2(msg_line, "    Storage required for NACTION_SYMBOLS_RANGE map: %d Bytes", 2 * (offset - 1));
  ffree(naction_symbols_range);
  /* We write the name_index of each terminal symbol.  The array TEMP  */
  /* is used to remap the NAME_INDEX values based on the new symbol    */
  /* numberings. If time tables are requested, the terminals and non-  */
  /* terminals are mixed together.                                     */
  temp = Allocate_long_array(num_symbols + 1);
  if (table_opt == OPTIMIZE_SPACE) {
    for ALL_TERMINALS3(symbol) {
      temp[symbol_map[symbol]] = symno[symbol].name_index;
    }
    if (num_names <= (java_bit ? 127 : 255)) {
      if (java_bit) {
        prnt_shorts("\n    public final static byte terminal_index[] = {0,\n", 1, num_terminals, 10, temp);
      } else {
        prnt_shorts("\nconst unsigned char  CLASS_HEADER terminal_index[] = {0,\n", 1, num_terminals, 10, temp);
      }
      num_bytes = num_terminals;
    } else {
      if (java_bit) {
        prnt_shorts("\n    public final static char terminal_index[] = {0,\n", 1, num_terminals, 10, temp);
      } else {
        prnt_shorts("\nconst unsigned short CLASS_HEADER terminal_index[] = {0,\n", 1, num_terminals, 10, temp);
      }
      num_bytes = 2 * num_terminals;
    }
    /* Compute and list space required for TERMINAL_INDEX map.       */
    PRNT2(msg_line, "    Storage required for TERMINAL_INDEX map: %ld Bytes", num_bytes);
    /* We write the name_index of each non_terminal symbol. The array */
    /* TEMP is used to remap the NAME_INDEX values based on the new   */
    /* symbol numberings.                                             */
    for ALL_NON_TERMINALS3(symbol) {
      temp[symbol_map[symbol]] = symno[symbol].name_index;
    }
    if (num_names <= (java_bit ? 127 : 255)) {
      if (java_bit) {
        prnt_shorts("\n    public final static byte non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp);
      } else {
        prnt_shorts("\nconst unsigned char  CLASS_HEADER non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp);
      }
      num_bytes = num_non_terminals;
    } else {
      if (java_bit) {
        prnt_shorts("\n    public final static char non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp);
      } else {
        prnt_shorts("\nconst unsigned short CLASS_HEADER non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp);
      }
      num_bytes = 2 * num_non_terminals;
    }
    /* Compute and list space required for NON_TERMINAL_INDEX map.   */
    PRNT2(msg_line, "    Storage required for NON_TERMINAL_INDEX map: %ld Bytes", num_bytes);
  } else {
    for ALL_SYMBOLS3(symbol) {
      temp[symbol_map[symbol]] = symno[symbol].name_index;
    }
    if (num_names <= (java_bit ? 127 : 255)) {
      if (java_bit) {
        prnt_shorts("\n    public final static byte symbol_index[] = {0,\n", 1, num_symbols, 10, temp);
        mystrcpy("    public final static byte terminal_index[] = symbol_index;\n");
        mystrcpy("    public final static byte non_terminal_index[] = symbol_index;\n");
      } else {
        prnt_shorts("\nconst unsigned char  CLASS_HEADER symbol_index[] = {0,\n", 1, num_symbols, 10, temp);
        mystrcpy("const unsigned char  *CLASS_HEADER terminal_index[] = &(symbol_index[0]);\n");
        mystrcpy("const unsigned char  *CLASS_HEADER non_terminal_index[] = &(symbol_index[0]);\n");
      }
      num_bytes = num_symbols;
    } else {
      if (java_bit) {
        prnt_shorts("\n    public final static char symbol_index[] = {0,\n", 1, num_symbols, 10, temp);
        mystrcpy("    public final static char terminal_index[] = symbol_index[0];\n");
        mystrcpy("    public final static char non_terminal_index[] = symbol_index;\n");
      } else {
        prnt_shorts("\nconst unsigned short CLASS_HEADER symbol_index[] = {0,\n", 1, num_symbols, 10, temp);
        mystrcpy("const unsigned short *CLASS_HEADER terminal_index[] = &(symbol_index[0]);\n");
        mystrcpy("const unsigned short *CLASS_HEADER non_terminal_index[] = &(symbol_index[0]);\n");
      }
      num_bytes = 2 * num_symbols;
    }
    /* Compute and list space required for SYMBOL_INDEX map.         */
    PRNT2(msg_line, "    Storage required for SYMBOL_INDEX map: %ld Bytes", num_bytes);
  }
  if (num_scopes > 0) {
    short root = 0;
    short *list;
    list = Allocate_short_array(scope_rhs_size + 1);
    for (int i = 1; i <= scope_rhs_size; i++) {
      if (scope_right_side[i] != 0) {
        scope_right_side[i] = symbol_map[scope_right_side[i]];
      }
    }
    for (int i = 1; i <= num_scopes; i++) {
      scope[i].look_ahead = symbol_map[scope[i].look_ahead];
      if (table_opt == OPTIMIZE_SPACE) {
        scope[i].lhs_symbol = symbol_map[scope[i].lhs_symbol] - num_terminals;
      } else {
        scope[i].lhs_symbol = symbol_map[scope[i].lhs_symbol];
      }
    }
    /* Mark all elements of prefix strings. */
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
        k = scope_right_side[j];
        scope_right_side[j] = temp[k];
      }
    }
    ffree(list);
  }
  if (java_bit) {
    // Print java names.
    long num_bytes = 0;
    max_name_length = 0;
    mystrcpy("\n    public final static String name[] = { null,\n");
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
          *output_ptr++ = escape;
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
          BUFFER_CHECK(sysdcl);
          padline();
          *output_ptr++ = '\"';
        }
      }
      *output_ptr++ = '\"';
      if (i < num_names) {
        *output_ptr++ = ',';
      }
      *output_ptr++ = '\n';
      BUFFER_CHECK(sysdcl);
    }
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                          };\n");
    }
    /* Compute and list space required for STRING_BUFFER map.        */
    PRNT2(msg_line, "    Storage required for STRING_BUFFER map: %ld Bytes", num_bytes);
  } else {
    // Print C names.
    long *name_len = Allocate_long_array(num_names + 1);
    long num_bytes = 0;
    max_name_length = 0;
    mystrcpy("\nconst char  CLASS_HEADER string_buffer[] = {0,\n");
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
          *output_ptr++ = escape;
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
          BUFFER_CHECK(sysdcl);
          padline();
        }
      }
    }
    *(output_ptr - 1) = '\n'; /*overwrite last comma*/
    BUFFER_CHECK(sysdcl);
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                          };\n");
    }
    /* Compute and list space required for STRING_BUFFER map.        */
    PRNT2(msg_line, "    Storage required for STRING_BUFFER map: %ld Bytes", num_bytes);
    /* Write out NAME_START array */
    mystrcpy("\nconst unsigned short CLASS_HEADER name_start[] = {0,\n");
    padline();
    int j = 1;
    int k = 0;
    for (int i = 1; i <= num_names; i++) {
      itoc(j);
      *output_ptr++ = COMMA;
      j += name_len[i];
      k++;
      if (k == 10 && i != num_names) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(sysdcl);
    }
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                          };\n");
    }
    /* Compute and list space required for NAME_START map.           */
    PRNT2(msg_line, "    Storage required for NAME_START map: %ld Bytes", 2 * num_names);
    /* Write out NAME_LENGTH array */
    prnt_shorts("\nconst unsigned char  CLASS_HEADER name_length[] = {0,\n", 1, num_names, 10, name_len);
    /* Compute and list space required for NAME_LENGTH map.          */
    PRNT2(msg_line, "    Storage required for NAME_LENGTH map: %ld Bytes", num_names);
    ffree(name_len);
  }
  if (num_scopes > 0) {
    if (scope_rhs_size <= (java_bit ? 127 : 255)) {
      if (java_bit) {
        mystrcpy("\n    public final static byte scope_prefix[] = {\n");
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_prefix[] = {\n");
      }
    } else {
      if (java_bit) {
        mystrcpy("\n    public final static char scope_prefix[] = {\n");
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_prefix[] = {\n");
      }
    }
    padline();
    k = 0;
    for (int i = 1; i <= num_scopes; i++) {
      itoc(scope[i].prefix);
      *output_ptr++ = COMMA;
      k++;
      if (k == 10 && i != num_scopes) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(sysdcl);
    }
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                          };\n");
    }
    if (scope_rhs_size <= (java_bit ? 127 : 255)) {
      if (java_bit) {
        mystrcpy("\n    public final static byte scope_suffix[] = {\n");
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_suffix[] = {\n");
      }
    } else {
      if (java_bit) {
        mystrcpy("\n    public final static char scope_suffix[] = {\n");
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_suffix[] = {\n");
      }
    }
    padline();
    k = 0;
    for (int i = 1; i <= num_scopes; i++) {
      itoc(scope[i].suffix);
      *output_ptr++ = COMMA;
      k++;
      if (k == 10 && i != num_scopes) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(sysdcl);
    }
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                          };\n");
    }
    if (num_symbols <= (java_bit ? 127 : 255)) {
      if (java_bit) {
        mystrcpy("\n    public final static byte scope_lhs[] = {\n");
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_lhs[] = {\n");
      }
    } else {
      if (java_bit) {
        mystrcpy("\n    public final static char scope_lhs[] = {\n");
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_lhs[] = {\n");
      }
    }
    padline();
    k = 0;
    for (int i = 1; i <= num_scopes; i++) {
      itoc(scope[i].lhs_symbol);
      *output_ptr++ = COMMA;
      k++;
      if (k == 10 && i != num_scopes) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(sysdcl);
    }
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                          };\n");
    }
    if (num_terminals <= (java_bit ? 127 : 255)) {
      if (java_bit) {
        mystrcpy("\n    public final static byte scope_la[] = {\n");
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_la[] = {\n");
      }
    } else {
      if (java_bit) {
        mystrcpy("\n    public final static char scope_la[] = {\n");
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_la[] = {\n");
      }
    }
    padline();
    k = 0;
    for (int i = 1; i <= num_scopes; i++) {
      itoc(scope[i].look_ahead);
      *output_ptr++ = COMMA;
      k++;
      if (k == 10 && i != num_scopes) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(sysdcl);
    }
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                          };\n");
    }
    if (scope_state_size <= (java_bit ? 127 : 255)) {
      if (java_bit) {
        mystrcpy("\n    public final static byte scope_state_set[] = {\n");
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_state_set[] = {\n");
      }
    } else {
      if (java_bit) {
        mystrcpy("\n    public final static char scope_state_set[] = {\n");
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_state_set[] = {\n");
      }
    }
    padline();
    k = 0;
    for (int i = 1; i <= num_scopes; i++) {
      itoc(scope[i].state_set);
      *output_ptr++ = COMMA;
      k++;
      if (k == 10 && i != num_scopes) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(sysdcl);
    }
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                          };\n");
    }
    if (num_symbols <= (java_bit ? 127 : 255)) {
      if (java_bit) {
        prnt_shorts("\n    public final static byte scope_rhs[] = {0,\n", 1, scope_rhs_size, 10, scope_right_side);
      } else {
        prnt_shorts("\nconst unsigned char  CLASS_HEADER scope_rhs[] = {0,\n", 1, scope_rhs_size, 10, scope_right_side);
      }
    } else {
      if (java_bit) {
        prnt_shorts("\n    public final static char scope_rhs[] = {0,\n", 1, scope_rhs_size, 10, scope_right_side);
      } else {
        prnt_shorts("\nconst unsigned short CLASS_HEADER scope_rhs[] = {0,\n", 1, scope_rhs_size, 10, scope_right_side);
      }
    }
    if (java_bit) {
      mystrcpy("\n    public final static char scope_state[] = {0,\n");
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER scope_state[] = {0,\n");
    }
    padline();
    k = 0;
    for (int i = 1; i <= scope_state_size; i++) {
      if (scope_state[i] == 0) {
        itoc(0);
      } else {
        itoc(state_index[scope_state[i]] + num_rules);
      }
      *output_ptr++ = COMMA;
      k++;
      if (k == 10 && i != scope_state_size) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(sysdcl);
    }
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                          };\n");
    }
    if (num_symbols <= (java_bit ? 127 : 255)) {
      if (java_bit) {
        mystrcpy("\n    public final static byte in_symb[] = {0,\n");
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER in_symb[] = {0,\n");
      }
    } else {
      if (java_bit) {
        mystrcpy("\n    public final static char in_symb[] = {0,\n");
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER in_symb[] = {0,\n");
      }
    }
    /* Transition symbol */
    padline();
    *output_ptr++ = '0';
    *output_ptr++ = COMMA;
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
      itoc(symbol_map[i]);
      *output_ptr++ = COMMA;
      k++;
      if (k == 10 && state_no != num_states) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(sysdcl);
    }
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                          };\n");
    }
  }
}

void common(const bool byte_check_bit) {
  // Write table common.
  {
    if (error_maps_bit) {
      print_error_maps();
    }
    if (!byte_check_bit) {
      if (java_bit) {
        PRNT("\n***Warning: Base Check vector contains value > 127. 16-bit words used.");
      } else {
        PRNT("\n***Warning: Base Check vector contains value > 255. 16-bit words used.");
      }
    }
    if (!byte_terminal_range) {
      if (java_bit) {
        PRNT("***Warning: Terminal symbol > 127. 16-bit words used.");
      } else {
        PRNT("***Warning: Terminal symbol > 255. 16-bit words used.");
      }
    }
    if (java_bit) {
      mystrcpy("}\n");
    }
    fwrite(output_buffer, sizeof(char), output_ptr - &output_buffer[0], sysdcl);
  }

  // print symbols
  {
    char line[SYMBOL_SIZE + /* max length of a token symbol  */
              2 * MAX_PARM_SIZE + /* max length of prefix + suffix */
              64]; /* +64 for error messages lines  */
    /* or other fillers(blank, =,...)*/
    if (java_bit) {
      strcpy(line, "interface ");
      strcat(line, sym_tag);
      strcat(line, "\n{\n    public final static int\n");
    } else {
      strcpy(line, "enum {\n");
    }
    /* We write the terminal symbols map.                    */
    for ALL_TERMINALS3(symbol) {
      char *tok = RETRIEVE_STRING(symbol);
      fprintf(syssym, "%s", line);
      if (tok[0] == '\n' || tok[0] == escape) {
        tok[0] = escape;
        PRNT2(line, "Escaped symbol %s is an invalid C variable.\n", tok);
      } else if (strpbrk(tok, "!%^&*()-+={}[];:\"`~|\\,.<>/?\'") != NULL) {
        PRNT2(line, "%s may be an invalid variable name.\n", tok);
      }
      sprintf(line, "      %s%s%s = %i,\n", prefix, tok, suffix, symbol_map[symbol]);
      if (c_bit || cpp_bit) {
        while (strlen(line) > PARSER_LINE_SIZE) {
          fwrite(line, sizeof(char), PARSER_LINE_SIZE - 2, syssym);
          fprintf(syssym, "\\\n");
          memmove(line, &line[PARSER_LINE_SIZE - 2], strlen(&line[PARSER_LINE_SIZE - 2]) + 1);
        }
      }
    }
    line[strlen(line) - 2] = '\0'; /* remove the string ",\n" from last line */
    fprintf(syssym, "%s%s", line, java_bit ? ";\n}\n" : "\n     };\n");
  }

  // print definitions
  {
    if (java_bit) {
      fprintf(sysdef, "interface %s\n{\n    public final static int\n\n", def_tag);
    } else {
      fprintf(sysdef, "enum {\n");
    }
    if (error_maps_bit) {
      if (java_bit) {
        fprintf(sysdef,
                "      ERROR_SYMBOL      = %d,\n"
                "      MAX_NAME_LENGTH   = %ld,\n"
                "      NUM_STATES        = %ld,\n\n",
                error_image,
                max_name_length,
                num_states);
      } else {
        fprintf(sysdef,
                "      ERROR_SYMBOL      = %d,\n"
                "      MAX_DISTANCE      = %d,\n"
                "      MIN_DISTANCE      = %d,\n"
                "      MAX_NAME_LENGTH   = %ld,\n"
                "      MAX_TERM_LENGTH   = %ld,\n"
                "      NUM_STATES        = %ld,\n\n",

                error_image,
                maximum_distance,
                minimum_distance,
                max_name_length,
                max_name_length,
                num_states);
      }
    }
    if (java_bit) {
      fprintf(sysdef,
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
              table_opt == OPTIMIZE_SPACE ? num_terminals : num_symbols,
              num_scopes - 1,
              num_scopes,
              read_reduce_bit && lalr_level > 1
                ? error_act + num_rules
                : error_act,
              lalr_level,
              num_rules,
              num_terminals,
              num_non_terminals,
              num_symbols,
              state_index[1] + num_rules,
              eoft_image,
              eolt_image,
              accept_act,
              error_act);
    } else {
      fprintf(sysdef,
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
              table_opt == OPTIMIZE_SPACE ? num_terminals : num_symbols,
              maximum_distance + lalr_level - 1,
              maximum_distance + lalr_level,
              stack_size - 1,
              stack_size,
              num_scopes - 1,
              num_scopes,
              read_reduce_bit && lalr_level > 1
                ? error_act + num_rules
                : error_act,
              lalr_level,
              num_rules,
              num_terminals,
              num_non_terminals,
              num_symbols,
              state_index[1] + num_rules,
              eoft_image,
              eolt_image,
              accept_act,
              error_act);
    }
  }

  // print externs
  {
    if (c_bit || cpp_bit) {
      fprintf(sysprs,
              "%s SCOPE_REPAIR\n"
              "%s DEFERRED_RECOVERY\n"
              "%s FULL_DIAGNOSIS\n"
              "%s SPACE_TABLES\n\n",
              num_scopes > 0 ? "#define" : "#undef ",
              deferred_bit ? "#define" : "#undef ",
              error_maps_bit ? "#define" : "#undef ",
              table_opt == OPTIMIZE_SPACE ? "#define" : "#undef ");
    }
    if (c_bit) {
      fprintf(sysprs,
              "#define original_state(state) (-%s[state])\n"
              "#define asi(state)            asb[original_state(state)]\n"
              "#define nasi(state)           nasb[original_state(state)]\n"
              "#define in_symbol(state)      in_symb[original_state(state)]\n\n",
              table_opt == OPTIMIZE_TIME ? "check" : "base_check");
    } else if (cpp_bit) {
      fprintf(sysprs,
              "class LexStream;\n\n"
              "class %s_table\n"
              "{\n"
              "public:\n", prs_tag);
      if (error_maps_bit || debug_bit) {
        fprintf(sysprs, "    static int original_state(int state) { return -%s[state]; }\n", table_opt == OPTIMIZE_TIME ? "check" : "base_check");
      }
      if (error_maps_bit) {
        fprintf(sysprs,
                "    static int asi(int state) "
                "{ return asb[original_state(state)]; }\n"
                "    static int nasi(int state) "
                "{ return nasb[original_state(state)]; }\n");
        if (num_scopes > 0) {
          fprintf(sysprs,
                  "    static int in_symbol(int state) "
                  "{ return in_symb[original_state(state)]; }\n");
        }
      }
      fprintf(sysprs, "\n");
    } else if (java_bit) {
      fprintf(sysprs, "abstract class %s extends %s implements %s\n{\n", prs_tag, dcl_tag, def_tag);
      if (error_maps_bit || debug_bit) {
        fprintf(sysprs, "    public final static int original_state(int state) { return -%s(state); }\n", table_opt == OPTIMIZE_TIME ? "check" : "base_check");
        if (error_maps_bit) {
          fprintf(sysprs, "    public final static int asi(int state) { return asb[original_state(state)]; }\n");
          fprintf(sysprs, "    static int nasi(int state) { return nasb[original_state(state)]; }\n");
          if (num_scopes > 0)
            fprintf(sysprs, "    public final static int in_symbol(int state) { return in_symb[original_state(state)]; }\n");
        }
        fprintf(sysprs, "\n");
      }
    }
    if (c_bit || cpp_bit) {
      fprintf(sysprs, "%s const unsigned char  rhs[];\n", c_bit ? "extern" : "    static");
      if (check_size > 0 || table_opt == OPTIMIZE_TIME) {
        const bool small = byte_check_bit && !error_maps_bit;
        fprintf(sysprs, "%s const %s check_table[];\n"
                "%s const %s *%s;\n",
                c_bit ? "extern" : "    static",
                small ? "unsigned char " : "  signed short",
                c_bit ? "extern" : "    static",
                small ? "unsigned char " : "  signed short",
                table_opt == OPTIMIZE_TIME ? "check" : "base_check");
      }
      fprintf(sysprs, "%s const unsigned short lhs[];\n"
              "%s const unsigned short *%s;\n",
              c_bit ? "extern" : "    static",
              c_bit ? "extern" : "    static",
              table_opt == OPTIMIZE_TIME ? "action" : "base_action");
      if (goto_default_bit) {
        fprintf(sysprs, "%s const unsigned short default_goto[];\n", c_bit ? "extern" : "    static");
      }
      if (table_opt == OPTIMIZE_SPACE) {
        fprintf(sysprs, "%s const unsigned %s term_check[];\n", c_bit ? "extern" : "    static", num_terminals <= (java_bit ? 127 : 255) ? "char " : "short");
        fprintf(sysprs, "%s const unsigned short term_action[];\n", c_bit ? "extern" : "    static");
        if (shift_default_bit) {
          fprintf(sysprs, "%s const unsigned short default_reduce[];\n", c_bit ? "extern" : "    static");
          fprintf(sysprs, "%s const unsigned short shift_state[];\n", c_bit ? "extern" : "    static");
          fprintf(sysprs, "%s const unsigned %s shift_check[];\n", c_bit ? "extern" : "    static", num_terminals <= (java_bit ? 127 : 255) ? "char " : "short");
          fprintf(sysprs, "%s const unsigned short default_shift[];\n", c_bit ? "extern" : "    static");
        }
      }
      if (error_maps_bit) {
        fprintf(sysprs,
                "\n"
                "%s const unsigned short asb[];\n"
                "%s const unsigned %s asr[];\n"
                "%s const unsigned short nasb[];\n"
                "%s const unsigned short nasr[];\n"
                "%s const unsigned short name_start[];\n"
                "%s const unsigned char  name_length[];\n"
                "%s const          char  string_buffer[];\n",
                c_bit ? "extern" : "    static",
                c_bit ? "extern" : "    static",
                byte_terminal_range <= (java_bit ? 127 : 255) ? "char " : "short",
                c_bit ? "extern" : "    static",
                c_bit ? "extern" : "    static",
                c_bit ? "extern" : "    static",
                c_bit ? "extern" : "    static",
                c_bit ? "extern" : "    static");
        if (table_opt == OPTIMIZE_SPACE) {
          fprintf(sysprs,
                  "%s const unsigned %s terminal_index[];\n"
                  "%s const unsigned %s non_terminal_index[];\n",
                  c_bit ? "extern" : "    static",
                  num_names <= (java_bit ? 127 : 255) ? "char " : "short",
                  c_bit ? "extern" : "    static",
                  num_names <= (java_bit ? 127 : 255) ? "char " : "short");
        } else {
          fprintf(sysprs, "%s const unsigned %s symbol_index[];\n"
                  "%s const unsigned %s *terminal_index;\n"
                  "%s const unsigned %s *non_terminal_index;\n",
                  c_bit ? "extern" : "    static",
                  num_names <= (java_bit ? 127 : 255) ? "char " : "short",
                  c_bit ? "extern" : "    static",
                  num_names <= (java_bit ? 127 : 255) ? "char " : "short",
                  c_bit ? "extern" : "    static",
                  num_names <= (java_bit ? 127 : 255) ? "char " : "short");
        }
        if (num_scopes > 0) {
          fprintf(sysprs, "%s const unsigned %s scope_prefix[];\n"
                  "%s const unsigned %s scope_suffix[];\n"
                  "%s const unsigned %s scope_lhs[];\n"
                  "%s const unsigned %s scope_la[];\n"
                  "%s const unsigned %s scope_state_set[];\n"
                  "%s const unsigned %s scope_rhs[];\n"
                  "%s const unsigned short scope_state[];\n"
                  "%s const unsigned %s in_symb[];\n",
                  c_bit ? "extern" : "    static",
                  scope_rhs_size <= (java_bit ? 127 : 255) ? "char " : "short",
                  c_bit ? "extern" : "    static",
                  scope_rhs_size <= (java_bit ? 127 : 255) ? "char " : "short",
                  c_bit ? "extern" : "    static",
                  num_symbols <= (java_bit ? 127 : 255) ? "char " : "short",
                  c_bit ? "extern" : "    static",
                  num_terminals <= (java_bit ? 127 : 255) ? "char " : "short",
                  c_bit ? "extern" : "    static",
                  scope_state_size <= (java_bit ? 127 : 255) ? "char " : "short",
                  c_bit ? "extern" : "    static",
                  num_symbols <= (java_bit ? 127 : 255) ? "char " : "short",
                  c_bit ? "extern" : "    static",
                  c_bit ? "extern" : "    static",
                  num_symbols <= (java_bit ? 127 : 255) ? "char " : "short");
        }
      }
      fprintf(sysprs, "\n");
    }
    if (table_opt == OPTIMIZE_SPACE) {
      if (goto_default_bit) {
        // non_terminal_space_action
        {
          if (c_bit) {
            fprintf(sysprs,
                    "#define nt_action(state, sym) \\\n"
                    "           ((base_check[state + sym] == sym) ? \\\n"
                    "               base_action[state + sym] : "
                    "default_goto[sym])\n\n");
          } else if (cpp_bit) {
            fprintf(sysprs,
                    "    static int nt_action(int state, int sym)\n"
                    "    {\n"
                    "        return (base_check[state + sym] == sym)\n"
                    "                             ? base_action[state + sym]\n"
                    "                             : default_goto[sym];\n"
                    "    }\n\n");
          } else if (java_bit) {
            fprintf(sysprs,
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
          if (c_bit) {
            fprintf(sysprs,
                    "#define nt_action(state, sym) "
                    "base_action[state + sym]\n\n");
          } else if (cpp_bit) {
            fprintf(sysprs,
                    "    static int nt_action(int state, int sym)\n"
                    "    {\n        return base_action[state + sym];\n    }\n\n");
          } else if (java_bit) {
            fprintf(sysprs,
                    "    public final static int nt_action(int state, int sym)\n"
                    "    {\n        return base_action[state + sym];\n    }\n\n");
          }
        }
      }
      if (lalr_level > 1) {
        if (shift_default_bit) {
          // terminal_shift_default_space_lalr_k
          {
            if (c_bit) {
              fprintf(sysprs,
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
            } else if (cpp_bit) {
              fprintf(sysprs,
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
            } else if (java_bit) {
              fprintf(sysprs,
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
            if (c_bit) {
              fprintf(sysprs,
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
            } else if (cpp_bit) {
              fprintf(sysprs,
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
            } else if (java_bit) {
              fprintf(sysprs,
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
        if (shift_default_bit) {
          // terminal_shift_default_space_action
          {
            if (c_bit) {
              fprintf(sysprs,
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
            } else if (cpp_bit) {
              fprintf(sysprs,
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
            } else if (java_bit) {
              fprintf(sysprs,
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
            if (c_bit) {
              fprintf(sysprs,
                      "#define t_action(state, sym, next_tok) \\\n"
                      "  term_action[term_check[base_action[state]+sym] == sym ? \\\n"
                      "          base_action[state] + sym : base_action[state]]\n\n");
            } else if (cpp_bit) {
              fprintf(sysprs,
                      "    static int t_action(int state, int sym, LexStream *stream)\n"
                      "    {\n"
                      "        return term_action[term_check[base_action[state]"
                      "+sym] == sym\n"
                      "                               ? base_action[state] + sym\n"
                      "                               : base_action[state]];\n"
                      "    }\n");
            } else if (java_bit) {
              fprintf(sysprs,
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
      if (goto_default_bit) {
        // non_terminal_time_action
        {
          if (c_bit) {
            fprintf(sysprs,
                    "#define nt_action(state, sym) \\\n"
                    "           ((check[state+sym] == sym) ? \\\n"
                    "                   action[state + sym] : "
                    "default_goto[sym])\n\n");
          } else if (cpp_bit) {
            fprintf(sysprs,
                    "    static int nt_action(int state, int sym)\n"
                    "    {\n"
                    "        return (check[state + sym] == sym)\n"
                    "                                    ? action[state + sym]\n"
                    "                                    : default_goto[sym];\n"
                    "    }\n\n");
          } else if (java_bit) {
            fprintf(sysprs,
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
          if (c_bit) {
            fprintf(sysprs,
                    "#define nt_action(state, sym) action[state + sym]\n\n");
          } else if (cpp_bit) {
            fprintf(sysprs,
                    "    static int nt_action(int state, int sym)\n"
                    "    {\n        return action[state + sym];\n    }\n\n");
          } else if (java_bit) {
            fprintf(sysprs,
                    "    public final static int nt_action(int state, int sym)\n"
                    "    {\n        return action[state + sym];\n    }\n\n");
          }
        }
      }
      if (lalr_level > 1) {
        // terminal_time_lalr_k
        {
          if (c_bit) {
            fprintf(sysprs,
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
          } else if (cpp_bit) {
            fprintf(sysprs,
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
          } else if (java_bit) {
            fprintf(sysprs,
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
          if (c_bit) {
            fprintf(sysprs,
                    "#define t_action(state, sym, next_tok) \\\n"
                    "   action[check[state + sym] == sym ? state + sym : state]\n\n");
          } else if (cpp_bit) {
            fprintf(sysprs,
                    "    static int t_action(int state, int sym, LexStream *stream)\n"
                    "    {\n"
                    "        return action[check[state + sym] == sym"
                    " ? state + sym : state];\n"
                    "    }\n");
          } else if (java_bit) {
            fprintf(sysprs,
                    "    public final static int t_action(int state, int sym, LexStream stream)\n"
                    "    {\n"
                    "        return action[check(state + sym) == sym"
                    " ? state + sym : state];\n"
                    "    }\n");
          }
        }
      }
    }
    if (cpp_bit) {
      fprintf(sysprs, "};\n");
    } else if (java_bit) {
      fprintf(sysprs, "}\n");
    }
  }

  // exit parser files
  {
    exit_file(&sysdcl, dcl_tag);
    exit_file(&syssym, sym_tag);
    exit_file(&sysdef, def_tag);
    exit_file(&sysprs, prs_tag);
  }
}

/* if ERROR_MAPS are requested, we print them out in the following order:    */
/*                                                                           */
/*   1) The FOLLOW map (NEWFOLL)                                             */
/*   2) The SORTED_STATE vector                                              */
/*   3) The ORIGINAL_STATE vector                                            */
/*   4) The map from states into valid symbols on which actions are          */
/*      defined within the state in question: ACTION_SYMBOLS                 */
/*   5) The map from each symbol into the set of staes that can              */
/*      possibly be reached after a transition on the symbol in              */
/*      question: TRANSITION_STATES                                          */
void process_error_maps(void) {
  long *state_start;
  long *state_stack;
  int *temp;
  int *original = NULL;
  int *symbol_root;
  int *symbol_count;
  int *term_list;
  long *as_size;
  long *action_symbols_range;
  long *naction_symbols_range;
  int offset;
  int item_no;
  int lhs_symbol;
  int max_len;
  int k;
  int terminal_ubound;
  int non_terminal_ubound;
  long num_bytes;
  char tok[SYMBOL_SIZE + 1];
  terminal_ubound = table_opt == OPTIMIZE_TIME
                      ? num_symbols
                      : num_terminals;
  non_terminal_ubound = table_opt == OPTIMIZE_TIME
                          ? num_symbols
                          : num_non_terminals;
  symbol_root = Allocate_int_array(num_symbols + 1);
  symbol_count = Allocate_int_array(num_symbols + 1);
  state_start = Allocate_long_array(num_states + 2);
  state_stack = Allocate_long_array(num_states + 1);
  term_list = Allocate_int_array(num_symbols + 1);
  PRNT("\nError maps storage:");
  /* The FOLLOW map is written out as two vectors where the first      */
  /* vector indexed by a Symbol gives the starting location in the     */
  /* second vector where the elements of the follow set of that symbol */
  /* starts.  Note that since the terminal and non-terminal symbols    */
  /* have been intermixed, the FOLLOW map is written out with the      */
  /* complete set of symbols as its domain even though it is only      */
  /* defined on non-terminals.                                         */
  /*                                                                   */
  /* We now compute and write the starting location for each symbol.   */
  /* The offset for the first symbol is 1,  and hence does not         */
  /* have to be computed.  However,  we compute an extra offset to     */
  /* indicate the extent of the last symbol.                           */
  for (int symbol = 1; symbol <= non_terminal_ubound; symbol++) {
    symbol_count[symbol] = 0;
    symbol_root[symbol] = OMEGA;
  }
  for ALL_NON_TERMINALS3(lhs_symbol) {
    int symbol;
    if (table_opt == OPTIMIZE_TIME) {
      symbol = symbol_map[lhs_symbol];
    } else {
      symbol = symbol_map[lhs_symbol] - num_terminals;
    }
    symbol_root[symbol] = lhs_symbol;
    for ALL_TERMINALS3(symbol) {
      if (IS_IN_SET(follow, lhs_symbol + 1, symbol + 1)) {
        symbol_count[symbol]++;
      }
    }
  }
  offset = 1;
  k = 1;
  field(offset, 6); /* Offset of the first state */
  for (int symbol = 1; symbol <= non_terminal_ubound; symbol++) {
    offset += symbol_count[symbol];
    field(offset, 6);
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
  /*  We now write the elements in the range of the FOLLOW map.  */
  k = 0;
  for (int symbol = 1; symbol <= non_terminal_ubound; symbol++) {
    lhs_symbol = symbol_root[symbol];
    if (lhs_symbol != OMEGA) {
      for ALL_TERMINALS3(symbol) {
        if (IS_IN_SET(follow, lhs_symbol + 1, symbol + 1)) {
          field(symbol_map[symbol], 4);
          k++;
          if (k == 18) {
            *output_ptr++ = '\n';
            BUFFER_CHECK(systab);
            k = 0;
          }
        }
      }
    }
  }
  if (k != 0) {
    *output_ptr++ = '\n';
    BUFFER_CHECK(systab);
  }
  /* Compute and list amount of space required for the Follow map. */
  if (table_opt == OPTIMIZE_TIME) {
    num_bytes = 2 * (num_symbols + offset);
    if (byte_bit && last_non_terminal <= 255) {
      num_bytes = num_bytes - offset + 1;
    }
  } else {
    num_bytes = 2 * (num_non_terminals + offset);
    if (byte_bit && num_terminals <= 255) {
      num_bytes = num_bytes - offset + 1;
    }
  }
  PRNT2(msg_line, "    Storage required for FOLLOW map: %ld Bytes", num_bytes);
  /* We now write out the states in sorted order: SORTED_STATE. */
  k = 0;
  for ALL_STATES3(state_no) {
    field(ordered_state[state_no], 6);
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
  /* Compute and list space required for SORTED_STATE map.         */
  num_bytes = 2 * num_states;
  PRNT2(msg_line, "    Storage required for SORTED_STATE map: %ld Bytes", num_bytes);
  /* We now write a vector parallel to SORTED_STATE that gives us the */
  /* original number associated with the state: ORIGINAL_STATE.       */
  k = 0;
  for (int state_no = 1; state_no <= num_states; state_no++) {
    field(state_list[state_no], 6);
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
  /* Compute and list space required for ORIGINAL_STATE map.       */
  num_bytes = 2 * num_states;
  PRNT2(msg_line, "    Storage required for ORIGINAL_STATE map: %ld Bytes", num_bytes);
  /* We now construct a bit map for the set of terminal symbols that  */
  /* may appear in each state. Then, we invoke PARTSET to apply the   */
  /* Partition Heuristic and print it.                                */
  as_size = Allocate_long_array(num_states + 1);
  if (table_opt == OPTIMIZE_TIME) {
    original = Allocate_int_array(num_symbols + 1);
    /* In a compressed TIME table, the terminal and non-terminal */
    /* symbols are mixed together when they are remapped.        */
    /* We shall now recover the original number associated with  */
    /* each terminal symbol since it lies very nicely in the     */
    /* range 1..NUM_TERMINALS.  This will save a considerable    */
    /* amount of space in the bit_string representation of sets  */
    /* as well as time when operations are performed on those    */
    /* bit-strings.                                              */
    for ALL_TERMINALS3(symbol) {
      original[symbol_map[symbol]] = symbol;
    }
  }
  /* NOTE that the arrays ACTION_SYMBOLS and NACTION_SYMBOLS are global  */
  /* variables that are allocated in the procedure PROCESS_TABLES by     */
  /* calloc which automatically initializes them to 0.                   */
  for ALL_STATES3(state_no) {
    struct shift_header_type sh;
    struct reduce_header_type red;
    sh = shift[statset[state_no].shift_number];
    as_size[state_no] = sh.size;
    for (state_no = 1; state_no <= sh.size; state_no++) {
      int symbol;
      if (table_opt == OPTIMIZE_TIME) {
        symbol = original[sh.map[state_no].symbol];
      } else {
        symbol = sh.map[state_no].symbol;
      }
      SET_BIT_IN(action_symbols, state_no, symbol);
    }
    red = reduce[state_no];
    as_size[state_no] += red.size;
    for (state_no = 1; state_no <= red.size; state_no++) {
      int symbol;
      if (table_opt == OPTIMIZE_TIME) {
        symbol = original[red.map[state_no].symbol];
      } else {
        symbol = red.map[state_no].symbol;
      }
      SET_BIT_IN(action_symbols, state_no, symbol);
    }
  }
  partset(action_symbols, as_size, state_list, state_start,
          state_stack, num_terminals, 0);
  ffree(action_symbols);
  /* We now write the starting location for each state in the domain   */
  /* of the ACTION_SYMBOL map.                                         */
  /* The starting locations are contained in the STATE_START vector.   */
  offset = state_start[num_states + 1];
  k = 0;
  for ALL_STATES3(state_no) {
    field(ABS(state_start[state_list[state_no]]), 6);
    k++;
    if (k == 12) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
      k = 0;
    }
  }
  field(offset, 6);
  *output_ptr++ = '\n';
  BUFFER_CHECK(systab);
  /* Compute and write out the range of the ACTION_SYMBOLS map. */
  action_symbols_range = Allocate_long_array(offset);
  compute_action_symbols_range(state_start, state_stack, state_list, action_symbols_range);
  k = 0;
  for (int state_no = 0; state_no < offset - 1; state_no++) {
    field(action_symbols_range[state_no], 4);
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
  /* Compute and list space required for ACTION_SYMBOLS map.   */
  num_bytes = 2 * (num_states + offset);
  if (byte_bit) {
    if (offset <= 255) {
      num_bytes -= num_states + 1;
    }
    if ((table_opt == OPTIMIZE_TIME && last_terminal <= 255) ||
        (table_opt != OPTIMIZE_TIME && num_terminals <= 255)) {
      num_bytes -= offset - 1;
    }
  }
  PRNT2(msg_line, "    Storage required for ACTION_SYMBOLS map: %ld Bytes", num_bytes);
  ffree(action_symbols_range);
  /* We now repeat the same process for the domain of the GOTO table.    */
  for ALL_STATES3(state_no) {
    as_size[state_no] = gd_index[state_no + 1] - gd_index[state_no];
    for (state_no = gd_index[state_no]; state_no < gd_index[state_no + 1]; state_no++) {
      int symbol = gd_range[state_no] - num_terminals;
      NTSET_BIT_IN(naction_symbols, state_no, symbol);
    }
  }
  partset(naction_symbols, as_size, state_list, state_start,
          state_stack, num_non_terminals, 0);
  ffree(as_size);
  ffree(naction_symbols);
  for (int state_no = 1; state_no <= gotodom_size; state_no++) {
    /* Remap non-terminals */
    if (table_opt == OPTIMIZE_TIME) {
      gd_range[state_no] = symbol_map[gd_range[state_no]];
    } else {
      gd_range[state_no] = symbol_map[gd_range[state_no]] - num_terminals;
    }
  }
  /* We now write the starting location for each state in the      */
  /* domain of the NACTION_SYMBOLS map. The starting locations are */
  /* contained in the STATE_START vector.                          */
  offset = state_start[num_states + 1];
  k = 0;
  for ALL_STATES3(state_no) {
    field(ABS(state_start[state_list[state_no]]), 6);
    k++;
    if (k == 12) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
      k = 0;
    }
  }
  field(offset, 6);
  *output_ptr++ = '\n';
  BUFFER_CHECK(systab);
  /* Compute and write out the range of the NACTION_SYMBOLS map.*/
  naction_symbols_range = Allocate_long_array(offset);
  compute_naction_symbols_range(state_start, state_stack,
                                state_list, naction_symbols_range);
  k = 0;
  for (int state_no = 0; state_no < offset - 1; state_no++) {
    field(naction_symbols_range[state_no], 4);
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
  /* Compute and list space required for NACTION_SYMBOLS map.  */
  num_bytes = 2 * (num_states + offset);
  if (byte_bit) {
    if (offset <= 255)
      num_bytes -= num_states + 1;
    if ((table_opt == OPTIMIZE_TIME && last_non_terminal <= 255) ||
        (table_opt != OPTIMIZE_TIME && num_non_terminals <= 255))
      num_bytes -= offset - 1;
  }
  PRNT2(msg_line, "    Storage required for NACTION_SYMBOLS map: %ld Bytes", num_bytes);
  ffree(naction_symbols_range);
  /* Compute map from each symbol to state into which that symbol can    */
  /* cause a transition: TRANSITION_STATES                               */
  /* TRANSITION_STATES is also written as two vectors like the FOLLOW    */
  /* map and the ACTION_DOMAIN map.                                      */
  /* The first vector contains the starting location in the second       */
  /* vector for each symbol.                                             */
  /*   Construct the TRANSITION_STATES map using an array SYMBOL_ROOT    */
  /* (indexable by each symbol) whose elements are the root of a linked  */
  /* stack built in STATE_STACK. Another array SYMBOL_COUNT is used to   */
  /* keep count of the number of states associated with each symbol.     */
  /* For space tables, the TRANSITION_STATES map is written as two       */
  /* separate tables: SHIFT_STATES and GOTO_STATES.                      */
  for ALL_SYMBOLS3(symbol) {
    symbol_root[symbol] = NIL;
    symbol_count[symbol] = 0;
  }
  for (int state_no = 2; state_no <= num_states; state_no++) {
    struct node *q;
    q = statset[state_no].kernel_items;
    if (q == NULL) {
      /* is the state a single production state? */
      q = statset[state_no].complete_items; /* pick arbitrary item */
    }
    item_no = q->value - 1;
    state_no = item_table[item_no].symbol;
    int symbol = symbol_map[state_no];
    state_stack[state_no] = symbol_root[symbol];
    symbol_root[symbol] = state_no;
    symbol_count[symbol]++;
  }
  /* We now compute and write the starting location for each terminal symbol */
  offset = 1; /* Offset of the first state */
  field(offset, 6);
  k = 1;
  for (int symbol = 1; symbol <= terminal_ubound; symbol++) {
    offset += symbol_count[symbol];
    field(offset, 6);
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
  /* We now write out the range elements of SHIFT_STATES for space     */
  /* tables or TRANSITION_STATES for time tables.                      */
  k = 0;
  for (int symbol = 1; symbol <= terminal_ubound; symbol++) {
    for (int state_no = symbol_root[symbol]; state_no != NIL; state_no = state_stack[state_no]) {
      field(state_index[state_no] + num_rules, 6);
      k++;
      if (k == 12) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(systab);
        k = 0;
      }
    }
  }
  if (k != 0) {
    *output_ptr++ = '\n';
    BUFFER_CHECK(systab);
  }
  /* If space tables are requested we compute and list space required  */
  /* for SHIFT_STATES map. The base vector contains NUM_TERMINALS+ 1   */
  /* elements,  and the vector containing the range elements has size  */
  /* OFFSET - 1. If time tables are requested we compute and list space*/
  /* requirements for TRANSITION_STATES map.  The base vector has      */
  /* NUM_SYMBOLS + 1 elements, and the range elements vector contains  */
  /* OFFSET - 1 elements.                                              */
  if (table_opt == OPTIMIZE_TIME) {
    num_bytes = 2 * (num_symbols + offset);
    PRNT2(msg_line, "    Storage required for TRANSITION_STATES map: %ld Bytes", num_bytes);
  } else {
    num_bytes = 2 * (num_terminals + offset);
    PRNT2(msg_line, "    Storage required for SHIFT_STATES map: %ld Bytes", num_bytes);
    /* We now compute and write the starting location for each  */
    /* non-terminal symbol...                                   */
    offset = 1;
    field(offset, 6); /* Offset of the first state */
    k = 1;
    for ALL_NON_TERMINALS3(symbol) {
      offset += symbol_count[symbol];
      field(offset, 6);
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
    /* We now write out the range elements of GOTO_STATES whose domain   */
    /* is a non-terminal symbol.                                         */
    k = 0;
    for ALL_NON_TERMINALS3(symbol) {
      for (int state_no = symbol_root[symbol];
           state_no != NIL; state_no = state_stack[state_no]) {
        field(state_index[state_no] + num_rules, 6);
        k++;
        if (k == 12) {
          *output_ptr++ = '\n';
          BUFFER_CHECK(systab);
          k = 0;
        }
      }
    }
    if (k != 0) {
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
    }
    /* We compute and list space required for GOTO_STATES map. The       */
    /* base vector contains NUM_NON_TERMINALS+ 1 elements, and the vector*/
    /* containing the range elements has size OFFSET - 1                 */
    num_bytes = 2 * (num_non_terminals + offset);
    PRNT2(msg_line, "    Storage required for GOTO_STATES map: %ld Bytes", num_bytes);
  }
  /* Write the number associated with the ERROR symbol.                */
  field(error_image, 4);
  field(eolt_image, 4);
  field(num_names, 4);
  field(num_scopes, 4);
  field(scope_rhs_size, 4);
  field(scope_state_size, 4);
  if (table_opt == OPTIMIZE_SPACE) {
    field(num_error_rules, 4);
  }
  *output_ptr++ = '\n';
  BUFFER_CHECK(systab);
  /* We write out the names map.                                       */
  num_bytes = 0;
  max_len = 0;
  for (int state_no = 1; state_no <= num_names; state_no++) {
    int name_len;
    strcpy(tok, RETRIEVE_NAME(state_no));
    if (tok[0] == '\n') /* we're dealing with special symbol?  */
      tok[0] = escape; /* replace initial marker with escape. */
    name_len = strlen(tok);
    num_bytes += name_len;
    if (max_len < name_len) {
      max_len = name_len;
    }
    field(name_len, 4);
    if (name_len <= 68) {
      strcpy(output_ptr, tok);
    } else {
      memcpy(output_ptr, tok, 68);
      output_ptr += 68;
      *output_ptr++ = '\n';
      BUFFER_CHECK(systab);
      strcpy(tok, tok+68);
      for (name_len = strlen(tok); name_len > 72; name_len = strlen(tok)) {
        memcpy(output_ptr, tok, 72);
        output_ptr += 72;
        *output_ptr++ = '\n';
        BUFFER_CHECK(systab);
        strcpy(tok, tok+72);
      }
      memcpy(output_ptr, tok, name_len);
    }
    output_ptr += name_len;
    *output_ptr++ = '\n';
    BUFFER_CHECK(systab);
  }
  /* We write the name_index of each terminal symbol.  The array TEMP  */
  /* is used to remap the NAME_INDEX values based on the new symbol    */
  /* numberings. If time tables are requested, the terminals and non-  */
  /* terminals are mixed together.                                     */
  temp = Allocate_int_array(num_symbols + 1);
  if (table_opt == OPTIMIZE_TIME) {
    for ALL_SYMBOLS3(symbol) {
      temp[symbol_map[symbol]] = symno[symbol].name_index;
    }
    k = 0;
    for ALL_SYMBOLS3(symbol) {
      field(temp[symbol], 4);
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
  } else {
    for ALL_TERMINALS3(symbol) {
      temp[symbol_map[symbol]] = symno[symbol].name_index;
    }
    k = 0;
    for ALL_TERMINALS3(symbol) {
      field(temp[symbol], 4);
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
    /* We write the name_index of each non_terminal symbol. The     */
    /* array TEMP is used to remap the NAME_INDEX values based on   */
    /* the new symbol numberings.                                   */
    for ALL_NON_TERMINALS3(symbol) {
      temp[symbol_map[symbol]] = symno[symbol].name_index;
    }
    k = 0;
    for ALL_NON_TERMINALS3(symbol) {
      field(temp[symbol], 4);
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
  }
  /* Compute and list space requirements for NAME map. */
  if (max_len > 255) {
    offset = 2 * num_symbols;
  } else {
    offset = num_symbols;
  }
  if (num_bytes > 255) {
    offset += 2 * num_symbols;
  } else {
    offset += num_symbols;
  }
  PRNT2(msg_line, "    Storage required for direct NAME map: %ld Bytes", num_bytes + offset);
  if (max_len > 255) {
    offset = 2 * num_names;
  } else {
    offset = num_names;
  }
  if (num_bytes > 255) {
    offset += 2 * num_names;
  } else {
    offset += num_names;
  }
  if (num_names > 255) {
    offset += 2 * num_symbols;
  } else {
    offset += num_symbols;
  }
  PRNT2(msg_line, "    Storage required for indirect NAME map: %ld Bytes", num_bytes + offset);
  if (scopes_bit) {
    for (int i = 1; i <= scope_rhs_size; i++) {
      if (scope_right_side[i] != 0) {
        scope_right_side[i] = symbol_map[scope_right_side[i]];
      }
    }
    for (int i = 1; i <= num_scopes; i++) {
      scope[i].look_ahead = symbol_map[scope[i].look_ahead];
      if (table_opt == OPTIMIZE_TIME) {
        scope[i].lhs_symbol = symbol_map[scope[i].lhs_symbol];
      } else {
        scope[i].lhs_symbol = symbol_map[scope[i].lhs_symbol] - num_terminals;
      }
    }
    k = 0;
    for (int i = 1; i <= num_scopes; i++) {
      field(scope[i].prefix, 4);
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
    for (int i = 1; i <= num_scopes; i++) {
      field(scope[i].suffix, 4);
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
    for (int i = 1; i <= num_scopes; i++) {
      field(scope[i].lhs_symbol, 4);
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
    for (int i = 1; i <= num_scopes; i++) {
      field(scope[i].look_ahead, 4);
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
    for (int i = 1; i <= num_scopes; i++) {
      field(scope[i].state_set, 4);
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
    for (int i = 1; i <= scope_rhs_size; i++) {
      field(scope_right_side[i], 4);
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
    for (int i = 1; i <= scope_state_size; i++) {
      if (scope_state[i] == 0) {
        field(0, 6);
      } else {
        field(state_index[scope_state[i]] + num_rules, 6);
      }
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
    if (table_opt == OPTIMIZE_TIME) {
      num_bytes = 5 * num_scopes + scope_rhs_size + scope_state_size;
      if (num_symbols > 255) {
        num_bytes += 2 * num_scopes + scope_rhs_size;
      }
    } else {
      num_bytes = 5 * num_scopes + scope_rhs_size + 2 * scope_state_size;
      if (num_non_terminals > 255) {
        num_bytes += num_scopes;
      }
      if (num_terminals > 255) {
        num_bytes += num_scopes;
      }
      if (num_symbols > 255) {
        num_bytes += scope_rhs_size;
      }
    }
    if (scope_rhs_size > 255) {
      num_bytes += 2 * num_scopes;
    }
    if (scope_state_size > 255) {
      num_bytes += num_scopes;
    }
    PRNT2(msg_line, "    Storage required for SCOPE map: %ld Bytes", num_bytes);
  }
  if (original != NULL) {
    ffree(original);
  }
  ffree(symbol_root);
  ffree(symbol_count);
  ffree(temp);
  ffree(state_start);
  ffree(state_stack);
  ffree(term_list);
}

void print_space_parser() {
  bool byte_check_bit = true; {
    int *check;
    int *action;
    int la_state_offset;
    int k;
    int indx;
    long result_act;
    int default_count = 0;
    int goto_count = 0;
    int goto_reduce_count = 0;
    int reduce_count = 0;
    int la_shift_count = 0;
    int shift_count = 0;
    int shift_reduce_count = 0;
    int rule_no;
    long offset;
    check = Allocate_int_array(table_size + 1);
    action = Allocate_int_array(table_size + 1);
    output_ptr = &output_buffer[0];
    /* Prepare header card with proper information, and write it out. */
    offset = error_act;
    if (lalr_level > 1) {
      if (read_reduce_bit) {
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
    for (int i = 1; i <= check_size; i++) {
      check[i] = DEFAULT_SYMBOL;
    }
    for (int i = 1; i <= (int) action_size; i++) {
      action[i] = error_act;
    }
    /*    Update the default non-terminal action of each state with the */
    /* appropriate corresponding terminal state starting index.         */
    for (int i = 1; i <= num_terminal_states; i++) {
      indx = term_state_index[i];
      int state_no = new_state_element[i].image;
      /* Update the action link between the non-terminal and terminal      */
      /* tables. If error-maps are requested, an indirect linking is made  */
      /* as follows:                                                       */
      /*  Each non-terminal row identifies its original state number, and  */
      /* a new vector START_TERMINAL_STATE indexable by state numbers      */
      /* identifies the starting point of each state in the terminal table.*/
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
    /*  Now update the non-terminal tables with the non-terminal actions.*/
    for ALL_STATES3(state_no) {
      struct goto_header_type go_to;
      indx = state_index[state_no];
      go_to = statset[state_no].go_to;
      for (int j = 1; j <= go_to.size; j++) {
        int symbol = go_to.map[j].symbol;
        int i = indx + symbol;
        if (goto_default_bit || nt_check_bit) {
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
    if (error_maps_bit || debug_bit) {
      if (check_size == 0) {
        check_size = action_size;
        for (int i = 0; i <= check_size; i++) {
          check[i] = 0;
        }
      }
      for ALL_STATES3(state_no) {
        check[state_index[state_no]] = -state_no;
      }
    }
    for (int i = 1; i <= check_size; i++) {
      if (check[i] < 0 || check[i] > (java_bit ? 127 : 255)) {
        byte_check_bit = false;
      }
    }
    if (c_bit) {
      mystrcpy("\n#define CLASS_HEADER\n\n");
    } else if (cpp_bit) {
      mystrcpy("\n#define CLASS_HEADER ");
      mystrcpy(prs_tag);
      mystrcpy("_table::\n\n");
    } else {
      mystrcpy("abstract class ");
      mystrcpy(dcl_tag);
      mystrcpy(" implements ");
      mystrcpy(def_tag);
      mystrcpy("\n{\n");
    }
    /* Write size of right hand side of rules followed by CHECK table.   */
    if (java_bit) {
      mystrcpy("    public final static byte rhs[] = {0,\n");
    } else {
      mystrcpy("const unsigned char  CLASS_HEADER rhs[] = {0,\n");
    }
    padline();
    k = 0;
    for (int i = 1; i <= num_rules; i++) {
      k++;
      if (k > 15) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 1;
      }
      itoc(RHS_SIZE(i));
      *output_ptr++ = COMMA;
    }
    *(output_ptr - 1) = '\n';
    BUFFER_CHECK(sysdcl);
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                 };\n");
    }
    *output_ptr++ = '\n';
    if (check_size > 0) {
      if (byte_check_bit && !error_maps_bit) {
        if (java_bit) {
          mystrcpy("    public final static byte check_table[] = {\n");
        } else {
          mystrcpy("const unsigned char  CLASS_HEADER check_table[] = {\n");
        }
      } else {
        if (java_bit) {
          mystrcpy("    public final static short check_table[] = {\n");
        } else {
          mystrcpy("const   signed short CLASS_HEADER check_table[] = {\n");
        }
      }
      padline();
      k = 0;
      for (int i = 1; i <= check_size; i++) {
        k++;
        if (k > 10) {
          *output_ptr++ = '\n';
          BUFFER_CHECK(sysdcl);
          padline();
          k = 1;
        }
        itoc(check[i]);
        *output_ptr++ = COMMA;
      }
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(sysdcl);
      if (java_bit) {
        mystrcpy("    };\n");
      } else {
        mystrcpy("                 };\n");
      }
      *output_ptr++ = '\n';
      if (byte_check_bit && !error_maps_bit) {
        if (java_bit) {
          mystrcpy("    public final static byte base_check(int i)\n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n");
        } else {
          mystrcpy("const unsigned char  *CLASS_HEADER base_check = &(check_table[0]) - (NUM_RULES + 1);\n");
        }
      } else {
        if (java_bit) {
          mystrcpy("    public final static short base_check(int i) \n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n");
        } else {
          mystrcpy("const   signed short *CLASS_HEADER base_check = &(check_table[0]) - (NUM_RULES + 1);\n");
        }
      }
      *output_ptr++ = '\n';
    }
    /* Write left hand side symbol of rules followed by ACTION table.    */
    if (java_bit) {
      mystrcpy("    public final static char lhs[] = {0,\n");
    } else {
      mystrcpy("const unsigned short CLASS_HEADER lhs[] = {0,\n");
    }
    padline();
    k = 0;
    for (int i = 1; i <= num_rules; i++) {
      itoc(symbol_map[rules[i].lhs] - num_terminals);
      *output_ptr++ = COMMA;
      k++;
      if (k == 15) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 0;
      }
    }
    *output_ptr++ = '\n';
    *output_ptr++ = '\n';
    BUFFER_CHECK(sysdcl);
    padline();
    k = 0;
    if (error_maps_bit) {
      int max_indx;
      max_indx = accept_act - num_rules - 1;
      for (int i = 1; i <= max_indx; i++) {
        check[i] = OMEGA;
      }
      for ALL_STATES3(state_no) {
        check[state_index[state_no]] = state_no;
      }
      int j = num_states + 1;
      for (int i = max_indx; i >= 1; i--) {
        int state_no = check[i];
        if (state_no != OMEGA) {
          j--;
          ordered_state[j] = i + num_rules;
          state_list[j] = state_no;
        }
      }
    }
    for (int i = 1; i <= (int) action_size; i++) {
      itoc(action[i]);
      *output_ptr++ = COMMA;
      k++;
      if (k == 10 && i != (int) action_size) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(sysdcl);
    }
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                 };\n");
    }
    *output_ptr++ = '\n';
    BUFFER_CHECK(sysdcl);
    if (java_bit) {
      mystrcpy("    public final static char base_action[] = lhs;\n");
    } else {
      mystrcpy("const unsigned short *CLASS_HEADER base_action = lhs;\n");
    }
    *output_ptr++ = '\n';
    /* Initialize the terminal tables,and update with terminal actions. */
    for (int i = 1; i <= term_check_size; i++) {
      check[i] = DEFAULT_SYMBOL;
    }
    for (int i = 1; i <= term_action_size; i++) {
      action[i] = error_act;
    }
    for (int state_no = 1; state_no <= num_terminal_states; state_no++) {
      struct shift_header_type sh;
      struct reduce_header_type red;
      indx = term_state_index[state_no];
      sh = shift[new_state_element[state_no].shift_number];
      for (int j = 1; j <= sh.size; j++) {
        int symbol = sh.map[j].symbol;
        int act = sh.map[j].action;
        if (!shift_default_bit || act != shiftdf[symbol]) {
          int i = indx + symbol;
          check[i] = symbol;
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
            PRNTERR2(msg_line, "Table contains look-ahead shift entry that is >%ld; Processing stopped.", MAX_TABLE_SIZE + 1);
            return;
          }
          action[i] = result_act;
        }
      }
      red = new_state_element[state_no].reduce;
      for (int j = 1; j <= red.size; j++) {
        int symbol = red.map[j].symbol;
        rule_no = red.map[j].rule_number;
        int i = indx + symbol;
        check[i] = symbol;
        action[i] = rule_no;
        reduce_count++;
      }
      rule_no = red.map[0].rule_number;
      if (rule_no != error_act) {
        default_count++;
      }
      check[indx] = DEFAULT_SYMBOL;
      if (shift_default_bit) {
        action[indx] = state_no;
      } else {
        action[indx] = rule_no;
      }
    }
    PRNT("\n\nActions in Compressed Tables:");
    PRNT2(msg_line, "     Number of Shifts: %d", shift_count);
    PRNT2(msg_line, "     Number of Shift/Reduces: %d", shift_reduce_count);
    if (max_la_state > num_states) {
      PRNT2(msg_line, "     Number of Look-Ahead Shifts: %d", la_shift_count);
    }
    PRNT2(msg_line, "     Number of Gotos: %d", goto_count);
    PRNT2(msg_line, "     Number of Goto/Reduces: %d", goto_reduce_count);
    PRNT2(msg_line, "     Number of Reduces: %d", reduce_count);
    PRNT2(msg_line, "     Number of Defaults: %d", default_count);
    /* Write Terminal Check Table.                                      */
    if (num_terminals <= (java_bit ? 127 : 255)) {
      if (java_bit) {
        prnt_ints("\n    public final static byte term_check[] = {0,\n", 1, term_check_size, 15, check);
      } else {
        prnt_ints("\nconst unsigned char  CLASS_HEADER term_check[] = {0,\n", 1, term_check_size, 15, check);
      }
    } else {
      if (java_bit) {
        prnt_ints("\n    public final static char term_check[] = {0,\n", 1, term_check_size, 15, check);
      } else {
        prnt_ints("\nconst unsigned short CLASS_HEADER term_check[] = {0,\n", 1, term_check_size, 15, check);
      }
    }
    /* Write Terminal Action Table.                                      */
    if (java_bit) {
      prnt_ints("\n    public final static char term_action[] = {0,\n", 1, term_action_size, 10, action);
    } else {
      prnt_ints("\nconst unsigned short CLASS_HEADER term_action[] = {0,\n", 1, term_action_size, 10, action);
    }
    /* If GOTO_DEFAULT is requested, we print out the GOTODEF vector.   */
    if (goto_default_bit) {
      if (java_bit) {
        mystrcpy("\n    public final static char default_goto[] = {0,\n");
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER default_goto[] = {0,\n");
      }
      padline();
      k = 0;
      for ALL_NON_TERMINALS3(symbol) {
        int act = gotodef[symbol];
        if (act < 0) {
          result_act = -act;
        } else if (act == 0) {
          result_act = error_act;
        } else {
          result_act = state_index[act] + num_rules;
        }
        itoc(result_act);
        *output_ptr++ = COMMA;
        k++;
        if (k == 10 && symbol != num_symbols) {
          *output_ptr++ = '\n';
          BUFFER_CHECK(sysdcl);
          padline();
          k = 0;
        }
      }
      if (k != 0) {
        *(output_ptr - 1) = '\n';
        BUFFER_CHECK(sysdcl);
      }
      if (java_bit) {
        mystrcpy("    };\n");
      } else {
        mystrcpy("                 };\n");
      }
    }
    if (shift_default_bit) {
      if (java_bit) {
        mystrcpy("\n    public final static char default_reduce[] = {0,\n");
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER default_reduce[] = {0,\n");
      }
      padline();
      k = 0;
      for (int i = 1; i <= num_terminal_states; i++) {
        struct reduce_header_type red;
        red = new_state_element[i].reduce;
        itoc(red.map[0].rule_number);
        *output_ptr++ = COMMA;
        k++;
        if (k == 10 && i != num_terminal_states) {
          *output_ptr++ = '\n';
          BUFFER_CHECK(sysdcl);
          padline();
          k = 0;
        }
      }
      if (k != 0) {
        *(output_ptr - 1) = '\n';
        BUFFER_CHECK(sysdcl);
      }
      if (java_bit) {
        mystrcpy("    };\n");
      } else {
        mystrcpy("                 };\n");
      }
      if (java_bit) {
        mystrcpy("\n    public final static char shift_state[] = {0,\n");
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER shift_state[] = {0,\n");
      }
      padline();
      k = 0;
      for (int i = 1; i <= num_terminal_states; i++) {
        itoc(shift_check_index[shift_image[i]]);
        *output_ptr++ = COMMA;
        k++;
        if (k == 10 && i != num_terminal_states) {
          *output_ptr++ = '\n';
          BUFFER_CHECK(sysdcl);
          padline();
          k = 0;
        }
      }
      if (k != 0) {
        *(output_ptr - 1) = '\n';
        BUFFER_CHECK(sysdcl);
      }
      if (java_bit) {
        mystrcpy("    };\n");
      } else {
        mystrcpy("                 };\n");
      }
      for (int i = 1; i <= shift_check_size; i++) {
        check[i] = DEFAULT_SYMBOL;
      }
      for (int i = 1; i <= shift_domain_count; i++) {
        struct shift_header_type sh;
        indx = shift_check_index[i];
        sh = shift[real_shift_number[i]];
        for (int j = 1; j <= sh.size; j++) {
          int symbol = sh.map[j].symbol;
          check[indx + symbol] = symbol;
        }
      }
      if (num_terminals <= (java_bit ? 127 : 255)) {
        if (java_bit) {
          mystrcpy("\n    public final static byte shift_check[] = {0,\n");
        } else {
          mystrcpy("\nconst unsigned char  CLASS_HEADER shift_check[] = {0,\n");
        }
      } else {
        if (java_bit) {
          mystrcpy("\n    public final static char shift_check[] = {0,\n");
        } else {
          mystrcpy("\nconst unsigned short CLASS_HEADER shift_check[] = {0,\n");
        }
      }
      padline();
      k = 0;
      int ii;
      for (ii = 1; ii <= shift_check_size; ii++) {
        itoc(check[ii]);
        *output_ptr++ = COMMA;
        k++;
        if (k == 10 && ii != shift_check_size) {
          *output_ptr++ = '\n';
          BUFFER_CHECK(sysdcl);
          padline();
          k = 0;
        }
      }
      if (k != 0) {
        *(output_ptr - 1) = '\n';
        BUFFER_CHECK(sysdcl);
      }
      if (java_bit) {
        mystrcpy("    };\n");
      } else {
        mystrcpy("                 };\n");
      }
      if (java_bit) {
        mystrcpy("\n    public final static char default_shift[] = {0,\n");
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER default_shift[] = {0,\n");
      }
      padline();
      k = 0;
      for ALL_TERMINALS3(symbol) {
        int act = shiftdf[symbol];
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
          PRNTERR2(msg_line, "Table contains look-ahead shift entry that is >%ld; Processing stopped.", MAX_TABLE_SIZE + 1);
          return;
        }
        itoc(result_act);
        *output_ptr++ = COMMA;
        k++;
        if (k == 10 && ii != num_terminals) {
          *output_ptr++ = '\n';
          BUFFER_CHECK(sysdcl);
          padline();
          k = 0;
        }
      }
      if (k != 0) {
        *(output_ptr - 1) = '\n';
        BUFFER_CHECK(sysdcl);
      }
      if (java_bit) {
        mystrcpy("    };\n");
      } else {
        mystrcpy("                 };\n");
      }
    }
    ffree(check);
    ffree(action);
  }
  common(byte_check_bit);
}

void print_time_parser() {
  bool byte_check_bit = true; {
    long *action;
    long *check;
    int la_shift_count = 0;
    int shift_count = 0;
    int goto_count = 0;
    int default_count = 0;
    int reduce_count = 0;
    int shift_reduce_count = 0;
    int goto_reduce_count = 0;
    state_list = Allocate_long_array(max_la_state + 1);
    output_ptr = &output_buffer[0];
    check = next;
    action = previous;
    long offset = error_act;
    int la_state_offset;
    if (lalr_level > 1) {
      if (read_reduce_bit) {
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
    /* Initialize all unfilled slots with default values.                */
    /* RECALL that the vector "check" is aliased to the vector "next".   */
    long indx;
    indx = first_index;
    for (long i = indx; i != NIL && i <= action_size; i = indx) {
      indx = next[i];
      check[i] = DEFAULT_SYMBOL;
      action[i] = error_act;
    }
    for (long i = action_size + 1; i <= table_size; i++) {
      check[i] = DEFAULT_SYMBOL;
    }
    /* We set the rest of the table with the proper table entries.       */
    for (int state_no = 1; state_no <= max_la_state; state_no++) {
      struct shift_header_type sh;
      struct reduce_header_type red;
      indx = state_index[state_no];
      if (state_no > num_states) {
        sh = shift[lastats[state_no].shift_number];
        red = lastats[state_no].reduce;
      } else {
        struct goto_header_type go_to = statset[state_no].go_to;
        for (int j = 1; j <= go_to.size; j++) {
          int symbol = go_to.map[j].symbol;
          long i = indx + symbol;
          if (goto_default_bit || nt_check_bit) {
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
          PRNTERR2(msg_line, "Table contains look-ahead shift entry that is >%ld; Processing stopped.", MAX_TABLE_SIZE + 1);
          return;
        }
        action[i] = result_act;
      }
      /*   We now initialize the elements reserved for reduce actions in   */
      /* the current state.                                                */
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
      /*   We now initialize the element reserved for the DEFAULT reduce   */
      /* action of the current state.  If error maps are requested,  the   */
      /* default slot is initialized to the original state number, and the */
      /* corresponding element of the DEFAULT_REDUCE array is initialized. */
      /* Otherwise it is initialized to the rule number in question.       */
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
    PRNT2(msg_line, "     Number of Shifts: %d", shift_count);
    PRNT2(msg_line, "     Number of Shift/Reduces: %d", shift_reduce_count);
    if (max_la_state > num_states) {
      sprintf(msg_line, "     Number of Look-Ahead Shifts: %d", la_shift_count);
      PRNT(msg_line);
    }
    PRNT2(msg_line, "     Number of Gotos: %d", goto_count);
    PRNT2(msg_line, "     Number of Goto/Reduces: %d", goto_reduce_count);
    PRNT2(msg_line, "     Number of Reduces: %d", reduce_count);
    PRNT2(msg_line, "     Number of Defaults: %d", default_count);
    if (error_maps_bit || debug_bit) {
      for ALL_STATES3(state_no) {
        check[state_index[state_no]] = -state_no;
      }
    }
    for (int i = 1; i <= (int) table_size; i++) {
      if (check[i] < 0 || check[i] > (java_bit ? 127 : 255)) {
        byte_check_bit = 0;
      }
    }
    if (c_bit) {
      mystrcpy("\n#define CLASS_HEADER\n\n");
    } else if (cpp_bit) {
      mystrcpy("\n#define CLASS_HEADER ");
      mystrcpy(prs_tag);
      mystrcpy("_table::\n\n");
    } else if (java_bit) {
      mystrcpy("abstract class ");
      mystrcpy(dcl_tag);
      mystrcpy(" implements ");
      mystrcpy(def_tag);
      mystrcpy("\n{\n");
    }
    /* Write size of right hand side of rules followed by CHECK table.   */
    if (java_bit) {
      mystrcpy("    public final static byte rhs[] = {0,\n");
    } else {
      mystrcpy("const unsigned char  CLASS_HEADER rhs[] = {0,\n");
    }
    padline();
    int k = 0;
    for (int i = 1; i <= num_rules; i++) {
      k++;
      if (k > 15) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 1;
      }
      itoc(RHS_SIZE(i));
      *output_ptr++ = COMMA;
    }
    *(output_ptr - 1) = '\n';
    BUFFER_CHECK(sysdcl);
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                 };\n");
    }
    *output_ptr++ = '\n';
    /* Write CHECK table.                                            */
    if (byte_check_bit && !error_maps_bit) {
      if (java_bit) {
        mystrcpy("    public final static byte check_table[] = {\n");
      } else {
        mystrcpy("const unsigned char  CLASS_HEADER check_table[] = {\n");
      }
    } else {
      if (java_bit) {
        mystrcpy("     public final static short check_table[] = {\n");
      } else {
        mystrcpy("const   signed short CLASS_HEADER check_table[] = {\n");
      }
    }
    padline();
    k = 0;
    for (int i = 1; i <= (int) table_size; i++) {
      k++;
      if (k > 10) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 1;
      }
      itoc(check[i]);
      *output_ptr++ = COMMA;
    }
    *(output_ptr - 1) = '\n';
    BUFFER_CHECK(sysdcl);
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                 };\n");
    }
    *output_ptr++ = '\n';
    BUFFER_CHECK(sysdcl);
    if (byte_check_bit && !error_maps_bit) {
      if (java_bit) {
        mystrcpy("    public final static byte check(int i) \n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n");
      } else {
        mystrcpy("const unsigned char  *CLASS_HEADER check = &(check_table[0]) - (NUM_RULES + 1);\n");
      }
    } else {
      if (java_bit) {
        mystrcpy("    public final static short check(int i) \n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n");
      } else {
        mystrcpy("const   signed short *CLASS_HEADER check = &(check_table[0]) - (NUM_RULES + 1);\n");
      }
    }
    *output_ptr++ = '\n';
    /* Write left hand side symbol of rules followed by ACTION table.    */
    if (java_bit) {
      mystrcpy("    public final static char lhs[] = {0,\n");
    } else {
      mystrcpy("const unsigned short CLASS_HEADER lhs[] = {0,\n");
    }
    padline();
    k = 0;
    for (int i = 1; i <= num_rules; i++) {
      itoc(symbol_map[rules[i].lhs]);
      *output_ptr++ = COMMA;
      k++;
      if (k == 15) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 0;
      }
    }
    *output_ptr++ = '\n';
    *output_ptr++ = '\n';
    BUFFER_CHECK(sysdcl);
    padline();
    k = 0;
    if (error_maps_bit) {
      int max_indx;
      /* Construct a map from new state numbers into original      */
      /*   state numbers using the array check[]                   */
      max_indx = accept_act - num_rules - 1;
      for (int i = 1; i <= max_indx; i++) {
        check[i] = OMEGA;
      }
      for ALL_STATES3(state_no) {
        check[state_index[state_no]] = state_no;
      }
      int j = num_states + 1;
      for (int i = max_indx; i >= 1; i--) {
        int state_no = check[i];
        if (state_no != OMEGA) {
          ordered_state[--j] = i + num_rules;
          state_list[j] = state_no;
        }
      }
    }
    for (int i = 1; i <= (int) action_size; i++) {
      itoc(action[i]);
      *output_ptr++ = COMMA;
      k++;
      if (k == 10 && i != (int) action_size) {
        *output_ptr++ = '\n';
        BUFFER_CHECK(sysdcl);
        padline();
        k = 0;
      }
    }
    if (k != 0) {
      *(output_ptr - 1) = '\n';
      BUFFER_CHECK(sysdcl);
    }
    if (java_bit) {
      mystrcpy("    };\n");
    } else {
      mystrcpy("                 };\n");
    }
    *output_ptr++ = '\n';
    BUFFER_CHECK(sysdcl);
    if (java_bit) {
      mystrcpy("    public final static char action[] = lhs;\n");
    } else {
      mystrcpy("const unsigned short *CLASS_HEADER action = lhs;\n");
    }
    *output_ptr++ = '\n';
    /* If GOTO_DEFAULT is requested, we print out the GOTODEF vector.   */
    if (goto_default_bit) {
      short *default_map = Allocate_short_array(num_symbols + 1);
      if (java_bit) {
        mystrcpy("\n    public final static char default_goto[] = {0,\n");
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER default_goto[] = {0,\n");
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
          result_act = state_index[act] + num_rules;
        } else {
          result_act = error_act;
        }
        default_map[symbol_map[symbol]] = result_act;
      }
      for (int symbol = 1; symbol <= num_symbols; symbol++) {
        itoc(default_map[symbol]);
        *output_ptr++ = COMMA;
        k++;
        if (k == 10 && symbol != num_symbols) {
          *output_ptr++ = '\n';
          BUFFER_CHECK(sysdcl);
          padline();
          k = 0;
        }
      }
      if (k != 0) {
        *(output_ptr - 1) = '\n';
        BUFFER_CHECK(sysdcl);
      }
      if (java_bit) {
        mystrcpy("    };\n");
      } else {
        mystrcpy("                 };\n");
      }
    }
    ffree(next);
    ffree(previous);
  }
  common(byte_check_bit);
}

void init_parser_files(struct OutputFiles output_files) {
  init_file(&sysdcl, output_files.dcl_file, dcl_tag);
  init_file(&syssym, output_files.sym_file, sym_tag);
  init_file(&sysdef, output_files.def_file, def_tag);
  init_file(&sysprs, output_files.prs_file, prs_tag);
}

/// PT_STATS prints all the states of the parser.
void ptstats(void) {
  int max_size;
  int symbol;
  int number;
  struct shift_header_type sh;
  struct reduce_header_type red;
  char temp[SYMBOL_SIZE + 1];
  char line[MAX_LINE_SIZE + 1];
  fprintf(syslis, "Shift STATES: ");
  /* iterate over the states */
  for ALL_STATES3(state_no) {
    print_state(state_no);
    max_size = 0;
    /* Compute the size of the largest symbol.  The MAX_SIZE cannot */
    /* be larger than PRINT_LINE_SIZE - 17 to allow for printing of */
    /* headers for actions to be taken on the symbols.              */
    sh = shift[statset[state_no].shift_number];
    for (int i = 1; i <= sh.size; i++) {
      symbol = sh.map[i].symbol;
      restore_symbol(temp, RETRIEVE_STRING(symbol));
      max_size = MAX(max_size, strlen(temp));
    }
    const struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      symbol = go_to.map[i].symbol;
      restore_symbol(temp, RETRIEVE_STRING(symbol));
      max_size = MAX(max_size, strlen(temp));
    }
    red = reduce[state_no];
    for (int i = 1; i <= red.size; i++) {
      symbol = red.map[i].symbol;
      restore_symbol(temp, RETRIEVE_STRING(symbol));
      max_size = MAX(max_size, strlen(temp));
    }
    max_size = MIN(max_size, PRINT_LINE_SIZE - 17);
    /* 1) Print all Shift actions.                                */
    /* 2) Print all Goto actions.                                 */
    /* 3) Print all reduce actions.                               */
    /* 4) If there is a default then print it.                    */
    if (sh.size > 0) {
      fprintf(syslis, "\n");
      for (int i = 1; i <= sh.size; i++) {
        symbol = sh.map[i].symbol;
        restore_symbol(temp, RETRIEVE_STRING(symbol));
        print_large_token(line, temp, "", max_size);
        number = ABS(sh.map[i].action);
        if (sh.map[i].action > (short) num_states) {
          fprintf(syslis, "\n%-*s    La/Sh  %d", max_size, line, number);
        } else if (sh.map[i].action > 0) {
          fprintf(syslis, "\n%-*s    Shift  %d", max_size, line, number);
        } else {
          fprintf(syslis, "\n%-*s    Sh/Rd  %d", max_size, line, number);
        }
      }
    }
    if (go_to.size > 0) {
      fprintf(syslis, "\n");
      for (int i = 1; i <= go_to.size; i++) {
        symbol = go_to.map[i].symbol;
        restore_symbol(temp, RETRIEVE_STRING(symbol));
        print_large_token(line, temp, "", max_size);
        number = ABS(go_to.map[i].action);
        if (go_to.map[i].action > 0) {
          fprintf(syslis, "\n%-*s    Goto   %d", max_size, line, number);
        } else {
          fprintf(syslis, "\n%-*s    Gt/Rd  %d", max_size, line, number);
        }
      }
    }
    if (red.size != 0) {
      fprintf(syslis, "\n");
      for (int i = 1; i <= red.size; i++) {
        symbol = red.map[i].symbol;
        restore_symbol(temp, RETRIEVE_STRING(symbol));
        print_large_token(line, temp, "", max_size);
        number = red.map[i].rule_number;
        if (rules[number].lhs != accept_image) {
          fprintf(syslis, "\n%-*s    Reduce %d", max_size, line, number);
        } else {
          fprintf(syslis, "\n%-*s    Accept", max_size, line);
        }
      }
    }
    if (default_opt > 0 && red.map[0].rule_number != OMEGA) {
      fprintf(syslis, "\n\nDefault reduction to rule  %d", red.map[0].rule_number);
    }
  }
  if (max_la_state > num_states) {
    fprintf(syslis, "Look-Ahead STATES:");
  }
  for ALL_LA_STATES3(state_no) {
    char buffer[PRINT_LINE_SIZE + 1];
    int ii = number_len(state_no) + 8; /* 8 = length of "STATE" */
    /* + 2 spaces + newline  */
    fill_in(buffer, PRINT_LINE_SIZE - ii, '-');
    fprintf(syslis, "\n\n\nSTATE %d %s", state_no, buffer);
    /* Print the set of states that have transitions to STATE_NO. */
    if (lastats[state_no].in_state == state_no) {
      fprintf(syslis, "\n(Unreachable State)\n");
    } else {
      fprintf(syslis, "\n(%d)\n", lastats[state_no].in_state);
      max_size = 0;
      /* Compute the size of the largest symbol.  The MAX_SIZE */
      /* cannot be larger than PRINT_LINE_SIZE - 17 to allow   */
      /* for printing of headers for actions to be taken on    */
      /* the symbols.                                          */
      sh = shift[lastats[state_no].shift_number];
      for (ii = 1; ii <= sh.size; ii++) {
        symbol = sh.map[ii].symbol;
        restore_symbol(temp, RETRIEVE_STRING(symbol));
        max_size = MAX(max_size, strlen(temp));
      }
      red = lastats[state_no].reduce;
      for (ii = 1; ii <= red.size; ii++) {
        symbol = red.map[ii].symbol;
        restore_symbol(temp, RETRIEVE_STRING(symbol));
        max_size = MAX(max_size, strlen(temp));
      }
      max_size = MIN(max_size, PRINT_LINE_SIZE - 17);
      /* 1) Print all Shift actions.                            */
      /* 2) Print all Goto actions.                             */
      /* 3) Print all reduce actions.                           */
      /* 4) If there is a default then print it.                */
      fprintf(syslis, "\n");
      for (ii = 1; ii <= sh.size; ii++) {
        symbol = sh.map[ii].symbol;
        restore_symbol(temp, RETRIEVE_STRING(symbol));
        print_large_token(line, temp, "", max_size);
        number = ABS(sh.map[ii].action);
        if (sh.map[ii].action > (short) num_states) {
          fprintf(syslis, "\n%-*s    La/Sh  %d", max_size, line, number);
        } else if (sh.map[ii].action > 0) {
          fprintf(syslis, "\n%-*s    Shift  %d", max_size, line, number);
        } else {
          fprintf(syslis, "\n%-*s    Sh/Rd  %d", max_size, line, number);
        }
      }
      fprintf(syslis, "\n");
      for (ii = 1; ii <= red.size; ii++) {
        symbol = red.map[ii].symbol;
        restore_symbol(temp, RETRIEVE_STRING(symbol));
        print_large_token(line, temp, "", max_size);
        number = red.map[ii].rule_number;
        fprintf(syslis, "\n%-*s    Reduce %d", max_size, line, number);
      }
      if (default_opt > 0 && red.map[0].rule_number != OMEGA) {
        fprintf(syslis, "\n\nDefault reduction to rule  %d", red.map[0].rule_number);
      }
    }
  }
  fprintf(syslis, "\n");
}