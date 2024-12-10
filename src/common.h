#pragma once
#include "bitset.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>
#include <stdio.h>

static char hostfile[];

static const int MAX_PARM_SIZE = 22;
static const int SYMBOL_SIZE = 256;
static const int MAX_MSG_SIZE = 256 + SYMBOL_SIZE;
static const int PRINT_LINE_SIZE = 80;
static const int PARSER_LINE_SIZE = 80;
static const int MAX_LINE_SIZE = 512;

static const int OPTIMIZE_NO_TABLE = 0;
static const int OPTIMIZE_TIME = 1;
static const int OPTIMIZE_SPACE = 2;
static const int MINIMUM_NAMES = 1;
static const int MAXIMUM_NAMES = 2;
static const int OPTIMIZE_PHRASES = 3;
static const int NOTRACE = 0;
static const int TRACE_CONFLICTS = 1;
static const int TRACE_FULL = 2;
static const int STATE_TABLE_UBOUND = 1020;
static const int STATE_TABLE_SIZE = STATE_TABLE_UBOUND + 1; /* 1021 is a prime */
static const int SHIFT_TABLE_UBOUND = 400;
static const int SHIFT_TABLE_SIZE = SHIFT_TABLE_UBOUND + 1; /* 401 is a prime */
static const int SCOPE_UBOUND = 100;
static const int SCOPE_SIZE = SCOPE_UBOUND + 1; /* 101 is prime */

static const char SPACE = ' ';
static const char COMMA = ',';
static const int INFINITY = (short) SHRT_MAX;
static const int OMEGA = (short) SHRT_MIN;
static const int NIL = (short) SHRT_MIN + 1;
static const int DEFAULT_SYMBOL = 0;

static const int HT_SIZE = 701; /* 701 is a prime */
static const int RULEHDR_INCREMENT = 1024;
static const int ACTELMT_INCREMENT = 1024;
static const int DEFELMT_INCREMENT = 16;

static const int IOBUFFER_SIZE = 655360;

#define ALL_TERMINALS3(x) (int x = 1; x <= num_terminals; x++)

#define ALL_TERMINALS_BACKWARDS3(x) (int x = num_terminals; x >= 1; x--)

#define ALL_NON_TERMINALS3(x) (int x = num_terminals + 1; x <= num_symbols; x++)

#define ALL_NON_TERMINALS_BACKWARDS3(x) (int x = num_symbols; x >= num_terminals + 1; x--)

#define ALL_SYMBOLS3(x) (int x = 1; x <= num_symbols; x++)

#define ALL_LA_STATES3(x) (int x = num_states + 1; x <= max_la_state; x++)

#define ALL_STATES3(x) (int x = 1; x <= num_states; x++)

#define ALL_ITEMS3(x) (int x = 1; x <= num_items; x++)

#define ALL_RULES3(x) (int x = 0; x <= num_rules; x++)

#define ALL_RULES_BACKWARDS3(x) (int x = num_rules; x >= 0; x--)

#define ENTIRE_RHS3(x, rule_no) (int x = rules[rule_no].rhs; x < rules[(rule_no) + 1].rhs; x++)

extern const long MAX_TABLE_SIZE;

struct node {
  struct node *next;
  int value;
};

/// RULES is the structure that contain the rules of the grammar.
/// Every rule of the grammar is mapped into an integer, and given
/// rule, and we have access to a value RHS which is the index
/// location in the vector RHS where the right-hand-side of the rule
/// begins. The right hand side of a certain rule represented by an
/// integer I starts at index location RULES[I].RHS in RHS, and
/// ends at index location RULES[I + 1].RHS - 1.  An extra
/// NUM_RULES + 1 element is used as a "fence" for the last rule.
/// The RHS vector as mentioned above is used to hold a complete
/// list of allthe right-hand-side symbols specified in the grammar.
struct ruletab_type {
  long lhs;
  long rhs;
  bool sp;
};

struct shift_type {
  int symbol;
  short action;
};

struct shift_header_type {
  struct shift_type *map;
  short size;
};

struct reduce_type {
  int symbol;
  short rule_number;
};

struct reduce_header_type {
  struct reduce_type *map;
  short size;
};

struct goto_type {
  int laptr;
  int symbol;
  short action;
};

struct goto_header_type {
  struct goto_type *map;
  short size;
};

struct lastats_type {
  struct reduce_header_type reduce;
  short shift_number;
  short in_state;
};

struct statset_type {
  struct node *kernel_items;
  struct node *complete_items;
  struct goto_header_type go_to;
  short shift_number;
};

struct OutputFiles {
  char prs_file[80];
  char sym_file[80];
  char def_file[80];
  char dcl_file[80];
};

struct CLIOptions {
  bool list_bit;
  bool edit_bit;
  bool first_bit;
  bool follow_bit;
  bool states_bit;
  bool xref_bit;
  bool nt_check_bit;
  bool conflicts_bit;
  // TODO • have a union for c/cpp/java/unknown.
  bool c_bit;
  bool cpp_bit;
  bool java_bit;
  bool scopes_bit;
  bool read_reduce_bit;
  bool goto_default_bit;
  bool shift_default_bit;
  bool byte_bit;
  bool warnings_bit;
  bool debug_bit;
  bool deferred_bit;
  bool single_productions_bit;
  int lalr_level;
  int default_opt;
  int trace_opt;
  int names_opt;
  int table_opt;
  int maximum_distance;
  int minimum_distance;
  int stack_size;
  char act_file[80];
  char hact_file[80];
};

static struct CLIOptions init_cli_options() {
  return (struct CLIOptions){
    .list_bit = false,
    .edit_bit = false,
    .first_bit = false,
    .follow_bit = false,
    .states_bit = false,
    .xref_bit = false,
    .nt_check_bit = false,
    .conflicts_bit = true,
    .c_bit = false,
    .cpp_bit = false,
    .java_bit = false,
    .scopes_bit = false,
    .read_reduce_bit = true,
    .goto_default_bit = true,
    .shift_default_bit = false,
    .byte_bit = true,
    .warnings_bit = true,
    .debug_bit = false,
    .deferred_bit = true,
    .single_productions_bit = false,
    .lalr_level = 1,
    .default_opt = 5,
    .trace_opt = TRACE_CONFLICTS,
    .names_opt = OPTIMIZE_PHRASES,
    .table_opt = 0,
    .maximum_distance = 30,
    .minimum_distance = 3,
    .stack_size = 128,
  };
}

void process_input(char *grm_file, char *lis_file, struct OutputFiles *output_files, int argc, char *argv[], char *file_prefix, struct CLIOptions *cli_options);

extern char prefix[];
extern char suffix[];
extern char msg_line[];

extern FILE *syslis;
extern FILE *syssym;
extern FILE *sysdcl;

///  The variables below are global counters.
extern long num_items;

/// NUM_STATES, the actual number of elements used in it is indicated
/// by the integer NUM_SHIFT_MAPS. NUM_STATES elements were allocated,
/// because if the user requests that certain single productions be
/// removed, a Shift map containing actions involving such productions
/// cannot be shared.
extern long num_states;

extern long max_la_state;

extern long num_symbols;
extern long num_names;
extern long num_terminals;
extern long num_non_terminals;
extern long num_rules;
extern long num_single_productions;
extern long gotodom_size;

static bool IS_A_TERMINAL(const int i) {
  return i <= num_terminals;
}

static bool IS_A_NON_TERMINAL(const int i) {
  return i > num_terminals;
}

extern bool error_maps_bit;

extern char escape;
extern char ormark;

// The variables below are used to hold information about special grammar symbols.
extern int accept_image;
extern int eoft_image;
extern int eolt_image;
extern int empty;
extern int error_image;

extern int num_first_sets;
extern int num_shift_maps;

extern long num_shifts;
extern long num_shift_reduces;
extern long num_gotos;
extern long num_goto_reduces;
extern long num_reductions;
extern long num_sr_conflicts;
extern long num_rr_conflicts;
extern long num_entries;

extern short *rhs_sym;

extern struct ruletab_type *rules;

static int RHS_SIZE(const int rule_no) {
  return rules[rule_no + 1].rhs - rules[rule_no].rhs;
}

/// CLOSURE is a mapping from non-terminal to a set (linked-list) of
/// non-terminals.  The set consists of non-terminals that are
/// automatically introduced via closure when the original non-terminal
/// is introduced.
extern struct node **closure;

/// CL_ITEMS is a mapping from each non-terminal to a set (linked list)
/// of items which are the first item of the rules generated by the
/// non-terminal in question.
extern struct node **clitems;

/// ADEQUATE_ITEM is a mapping from each rule
/// to the last (complete) item produced by that rule.
extern struct node **adequate_item;

/// ITEM_TABLE is used to map each item into a number. Given that
/// number one can retrieve the rule the item belongs to, the position
/// of the dot,  the symbol following the dot, and FIRST of the suffix
/// following the "dot symbol".
extern struct itemtab {
  short symbol;
  short rule_number;
  short suffix_index;
  short dot;
} *item_table;

/// NULL_NT is a boolean vector that indicates whether a given
/// non-terminal is nullable.
extern bool *null_nt;

extern SET_PTR first;
/// FOLLOW is a mapping from non-terminals to a set of terminals that
/// may appear immediately after the non-terminal.
extern SET_PTR follow;

/// SHIFT is an array used to hold the complete set of all shift maps
/// needed to construct the state automaton. Though its size is
extern struct shift_header_type *shift;

/// REDUCE is a mapping from each state to reduce actions in that state.
extern struct reduce_header_type *reduce;

extern short *gotodef;
extern short *shiftdf;
extern short *gd_index;
extern short *gd_range;

/// STATSET is a mapping from state number to state information.
extern struct statset_type *statset;

/// LASTATS is a similar mapping for look-ahead states.
extern struct lastats_type *lastats;

/// IN_STAT is a mapping from each state to the set of states that have
/// a transition into the state in question.
extern struct node **in_stat;

extern long num_scopes;
extern long scope_rhs_size;
extern long scope_state_size;
extern long num_error_rules;

extern struct scope_type {
  short prefix;
  short suffix;
  short lhs_symbol;
  short look_ahead;
  short state_set;
} *scope;

extern long *scope_right_side;
extern short *scope_state;

extern char *output_ptr;
extern char *output_buffer;

extern int *symbol_map;
extern long *ordered_state;
extern long *state_list;

extern long *next;
extern long *previous;
extern long *state_index;

extern long table_size;
extern long action_size;
extern long increment_size;

extern long last_non_terminal;
extern long last_terminal;

extern long accept_act;
extern long error_act;
extern long first_index;
extern long last_index;
extern long last_symbol;
extern long max_name_length;

extern SET_PTR naction_symbols;
extern SET_PTR action_symbols;

extern bool byte_terminal_range;

void reset_temporary_space(void);

void free_temporary_space(void);

void *talloc(long size);

struct node *allocate_node(char *file, long line);

bool *allocate_boolean_array(long n, char *file, long line);

int *allocate_int_array(long n, char *file, long line);

long *allocate_long_array(long n, char *file, long line);

short *allocate_short_array(long n, char *file, long line);

struct goto_header_type allocate_goto_map(int n, char *file, long line);

struct shift_header_type allocate_shift_map(int n, char *file, long line);

struct reduce_header_type allocate_reduce_map(int n, char *file, long line);

void cmprtim(struct OutputFiles *output_files, struct CLIOptions *cli_options, FILE *systab);

void cmprspa(struct OutputFiles *output_files, struct CLIOptions *cli_options, FILE *systab);

void compute_la(int state_no, int item_no, SET_PTR look_ahead);

void create_lastats(void);

void dump_tables(void);

void exit_lalrk_process(struct CLIOptions *cli_options);

void init_lalrk_process(struct CLIOptions *cli_options);

void init_rmpself(SET_PTR produces);

void field(long num, int len);

void fill_in(char string[], int amount, char character);

void free_conflict_space(void);

void free_nodes(struct node *head, struct node *tail);

struct node *lpgaccess(int state_no, int item_no);

void mkbasic(struct CLIOptions *cli_options);

void mkrdcts(struct CLIOptions *cli_options);

void la_traverse(int state_no, int goto_indx, int *stack_top);

void remove_single_productions();

void mkstats(struct CLIOptions *cli_options);

void nospace(char *, long);

int number_len(int state_no);

void partset(SET_PTR collection, const long *element_size, const long *list, long *start, long *stack, long set_size, long from_process_scopes);

void print_item(int item_no);

void print_large_token(char *line, char *token, const char *indent, int len);

void print_state(int state_no);

void produce(struct CLIOptions *cli_options);

void process_error_maps(struct CLIOptions *cli_options, FILE *systab);

void print_space_parser(struct CLIOptions *cli_options);

void print_time_parser(struct CLIOptions *cli_options);

void init_parser_files(struct OutputFiles *output_files, struct CLIOptions *cli_options);

void process_tables(char *tab_file, struct OutputFiles *output_files, struct CLIOptions *cli_options);

void ptstats(struct CLIOptions *cli_options);

void remvsp(void);

void sortdes(long array[], long count[], long low, long high, long max);

void reallocate(struct CLIOptions *cli_options);

void resolve_conflicts(int state_no, struct node **action, const short *symbol_list, int reduce_root, struct CLIOptions *cli_options);

void restore_symbol(char *out, const char *in);

/// This function allocates an array of size "size" of int integers.
static int *Allocate_int_array(const long n) {
  int *p;
  p = (int *) calloc(n, sizeof(int));
  if (p == (int *) NULL)
    nospace(hostfile, __LINE__);
  return &p[0];
}

/// This function allocates an array of size "size" of int integers.
static long *Allocate_long_array(const long n) {
  long *p;
  p = (long *) calloc(n, sizeof(long));
  if (p == (long *) NULL)
    nospace(hostfile, __LINE__);
  return &p[0];
}

/// This function allocates an array of size "size" of short integers.
static short *Allocate_short_array(const long n) {
  short *p;
  p = (short *) calloc(n, sizeof(short));
  if (p == (short *) NULL)
    nospace(hostfile, __LINE__);
  return &p[0];
}

/// This function allocates an array of size "size" of type boolean.
static bool *Allocate_boolean_array(const long n) {
  bool *p;
  p = (bool *) calloc(n, sizeof(bool));
  if (p == (bool *) 0)
    nospace(hostfile, __LINE__);
  return &p[0];
}

void *galloc(long size);

extern struct node *node_pool;

/// This function allocates a node structure and returns a pointer to it.
/// If there are nodes in the free pool, one of them is returned. Otherwise,
/// a new node is allocated from the global storage pool.
static struct node *Allocate_node() {
  struct node *p = node_pool;
  if (p != NULL) {
    // Is free list not empty?
    node_pool = p->next;
  } else {
    p = (struct node *) galloc(sizeof(struct node));
    if (p == NULL) {
      nospace(hostfile, __LINE__);
    }
  }
  return p;
}

/// This function allocates space for a goto map with "size" elements,
/// initializes and returns a goto header for that map. NOTE that after the
/// map is successfully allocated, it is offset by one element. This is
/// to allow the array in question to be indexed from 1..size instead of
/// 0..(size-1).
static struct goto_header_type Allocate_goto_map(const long n) {
  struct goto_header_type go_to;
  go_to.size = n;
  go_to.map = (struct goto_type *) galloc(n * sizeof(struct goto_type));
  if (go_to.map == NULL)
    nospace(hostfile, __LINE__);
  go_to.map--; /* map will be indexed in range 1..size */
  return go_to;
}

/// This function allocates space for a shift map with "size" elements,
/// initializes and returns a shift header for that map. NOTE that after the
/// map is successfully allocated, it is offset by one element. This is
/// to allow the array in question to be indexed from 1..size instead of
/// 0..(size-1).
static struct shift_header_type Allocate_shift_map(const long n) {
  struct shift_header_type sh;
  sh.size = n;
  sh.map = (struct shift_type *) galloc(n * sizeof(struct shift_type));
  if (sh.map == NULL)
    nospace(hostfile, __LINE__);
  sh.map--; /* map will be indexed in range 1..size */
  return sh;
}

/// This function allocates space for a REDUCE map with "size"+1 elements,
/// initializes and returns a REDUCE header for that map. The 0th element of
/// a reduce map is used for the default reduction.
static struct reduce_header_type Allocate_reduce_map(const long n) {
  struct reduce_header_type red;
  red.map = (struct reduce_type *) galloc((n + 1) * sizeof(struct reduce_type));
  if (red.map == NULL)
    nospace(hostfile, __LINE__);
  red.size = n;
  return red;
}

static void ffree(void *y) {
  return free(y); /* { free(x); x = (void *) ULONG_MAX; } */
}

static int TOUPPER(const int c) {
  return islower(c) ? toupper(c) : c;
}

static int MAX(const long a, const long b) {
  return a > b ? a : b;
}

static int MIN(const long a, const long b) {
  return a < b ? a : b;
}

static int ABS(const long x) {
  return x < 0 ? -x : x;
}

static void PRNT(char *msg) {
  printf("%s\n", msg);
  fprintf(syslis, "%s\n", msg);
}

#define PRNT3(...) \
  snprintf(msg_line, MAX_MSG_SIZE, __VA_ARGS__); \
  PRNT(msg_line);

#define PRNT4(msg, size, ...) \
  snprintf(msg, size, __VA_ARGS__); \
  PRNT(msg);

static void PRNTWNG(char *msg) {
  printf("***WARNING: %s\n", msg);
  fprintf(syslis, "***WARNING: %s\n", msg);
}

#define PRNTWNG2(...) \
  snprintf(msg_line, MAX_MSG_SIZE, __VA_ARGS__); \
  PRNTWNG(msg_line);

static void PRNTERR(char *msg) {
  printf("***ERROR: %s\n", msg);
  fprintf(syslis, "***ERROR: %s\n", msg);
}

#define PRNTERR2(...) \
  snprintf(msg_line, MAX_MSG_SIZE, __VA_ARGS__); \
  PRNTERR(msg_line);

/// The following two macros check whether the value of an
/// integer variable exceeds the maximum limit for a short or a long
/// integer, respectively. Note that the user should declare the
/// variable in question as a long integer. In the case of INT_CHECK,
/// this check is meaningful only if INT and SHORT are the same size.
/// Otherwise, if INT and LONG are of the same size, as is usually the
/// case on large systems, this check is meaningless - too late !!!
static void SHORT_CHECK(const long var) {
  if (var > SHRT_MAX) {
    PRNTERR("The limit of a short int value has been exceeded by");
    exit(12);
  }
}

static void INT_CHECK(const long var) {
  if (var > INT_MAX) {
    PRNTERR("The limit of an int value has been exceeded by var");
    exit(12);
  }
}

/// The following macro definitions are used only in processing the output.
static void BUFFER_CHECK(FILE *file) {
  if (IOBUFFER_SIZE - (output_ptr - &output_buffer[0]) < 73) {
    fwrite(output_buffer, sizeof(char), output_ptr - &output_buffer[0], file);
    output_ptr = &output_buffer[0];
  }
}

struct new_state_type {
  struct reduce_header_type reduce;
  short shift_number;
  short link;
  short thread;
  short image;
};

extern struct new_state_type *new_state_element;

extern short *shift_image;
extern short *real_shift_number;

extern int *term_state_index;
extern int *shift_check_index;

extern int shift_domain_count;
extern int num_terminal_states;
extern int check_size;
extern int term_check_size;
extern int term_action_size;
extern int shift_check_size;

/// CONFLICT_SYMBOLS is a mapping from each state into a set of terminal
/// symbols on which an LALR(1) conflict was detected in the state in
/// question.
///
/// LA_INDEX and LA_SET are temporary look-ahead sets, each of which will
/// be pointed to by a GOTO action, and the associated set will be
/// initialized to READ_SET(S), where S is the state identified by the GOTO
/// action in question. READ_SET(S) is a set of terminals on which an action
/// is defined in state S. See COMPUTE_READ for more details.
/// LA_TOP is used to compute the number of such sets needed.
///
/// The boolean variable NOT_LRK is used to mark whether a grammar
/// is not LR(k) for any k. NOT_LRK is marked true when either:
///    1. The grammar contains a nonterminal A such that A =>+rm A
///    2. The automaton contains a cycle with each of its edges labeled
///       with a nullable nonterminal.
/// (Note that these are not the only conditions under which a grammar is
///  not LR(k). In fact, it is an undecidable problem.)
/// The variable HIGHEST_LEVEL is used to indicate the highest number of
/// lookahead that was necessary to resolve all conflicts for a given
/// grammar. If we can detect that the grammar is not LALR(k), we set
/// HIGHEST_LEVEL to INFINITY.
extern struct node **conflict_symbols;
extern BOOLEAN_CELL *read_set;
extern BOOLEAN_CELL *la_set;
extern int highest_level;
extern long la_top;
extern short *la_index;
extern bool not_lrk;

extern int increment;
