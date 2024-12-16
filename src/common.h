#pragma once
#include "bitset.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>
#include <stdio.h>


#define galloc0p(into, x, times) \
  (*into) = (x *) galloc((times) * sizeof(x)); \
  if ((*into) == (x *) NULL) \
    nospace();

#define talloc0(into, x) \
  into = (x *) talloc(sizeof(x)); \
  if ((into) == (x *) NULL) \
    nospace();

#define talloc0p(into, x) \
  (*into) = (x *) talloc(sizeof(x)); \
  if ((*into) == (x *) NULL) \
    nospace();

#define realloc0(into, times, t) \
  into = (t *) realloc(into, (times) * sizeof(t)); \
  if ((into) == (t *) NULL) \
    nospace();

#define talloc0_raw(into, xyz, s) \
  into = (xyz *) talloc(s); \
  if ((into) == (xyz *) NULL) \
    nospace();

#define calloc0(into, size, x) \
  (into) = (x *) calloc((size), sizeof(x)); \
  if ((into) == (x *) NULL) \
    nospace();

static const int MAX_PARM_SIZE = 22;
static const int SYMBOL_SIZE = 256;
static const int MAX_MSG_SIZE = 256 + SYMBOL_SIZE;
static const int PRINT_LINE_SIZE = 80;
static const int PARSER_LINE_SIZE = 80;
static const int MAX_LINE_SIZE = 512;



typedef struct {
  int value;
} OptimizeMode;
static const OptimizeMode OPTIMIZE_NO_TABLE = { .value = 0 },
  OPTIMIZE_TIME = { .value = 1 },
  OPTIMIZE_SPACE = { .value = 2 };



typedef struct {
  int value;
} OptimizeNames;
static const OptimizeNames MINIMUM_NAMES = { .value = 1 },
  MAXIMUM_NAMES = { .value = 2 },
  OPTIMIZE_PHRASES = { .value = 3 };



typedef struct {
  int value;
} TraceMode;
static const TraceMode NOTRACE = { .value = 0 },
  TRACE_CONFLICTS = { .value = 1 },
  TRACE_FULL = { .value = 2 };



typedef struct {
  int value;
} DefaultOpt;
static const DefaultOpt
  OPT_0 = { .value = 0 },
  OPT_1 = { .value = 1 },
  OPT_2 = { .value = 2 },
  OPT_3 = { .value = 3 },
  OPT_4 = { .value = 4 },
  OPT_5 = { .value = 5 };



static const int STATE_TABLE_UBOUND = 1020;
static const int STATE_TABLE_SIZE = STATE_TABLE_UBOUND + 1; /* 1021 is a prime */
static const int SHIFT_TABLE_UBOUND = 400;
static const int SHIFT_TABLE_SIZE = SHIFT_TABLE_UBOUND + 1; /* 401 is a prime */
static const int SCOPE_UBOUND = 100;
static const int SCOPE_SIZE = SCOPE_UBOUND + 1; /* 101 is prime */

static const short INFINITY = SHRT_MAX;
static const short OMEGA = SHRT_MIN;
static const short NIL = SHRT_MIN + 1;
static const int DEFAULT_SYMBOL = 0;

static const int HT_SIZE = 701; /* 701 is a prime */
static const int RULEHDR_INCREMENT = 1024;
static const int ACTELMT_INCREMENT = 1024;
static const int DEFELMT_INCREMENT = 16;

static const int IOBUFFER_SIZE = 655360;

#define ALL_TERMINALS3(x) (int (x) = 1; (x) <= num_terminals; (x)++)

#define ALL_TERMINALS_BACKWARDS3(x) (int (x) = num_terminals; (x) >= 1; (x)--)

#define ALL_NON_TERMINALS3(x) (int (x) = num_terminals + 1; (x) <= num_symbols; (x)++)

#define ALL_NON_TERMINALS_BACKWARDS3(x) (int (x) = num_symbols; (x) >= num_terminals + 1; (x)--)

#define ALL_SYMBOLS3(x) (int (x) = 1; (x) <= num_symbols; (x)++)

#define ALL_LA_STATES3(x) (int (x) = num_states + 1; (x) <= max_la_state; (x)++)

#define ALL_STATES3(x) (int (x) = 1; (x) <= num_states; (x)++)

#define ALL_ITEMS3(x) (int (x) = 1; (x) <= num_items; (x)++)

#define ALL_RULES3(x) (int (x) = 0; (x) <= num_rules; (x)++)

#define ALL_RULES_BACKWARDS3(x) (int (x) = num_rules; (x) >= 0; (x)--)

#define ENTIRE_RHS3(x, rule_no, rules) (int (x) = (rules)[rule_no].rhs; (x) < rules[(rule_no) + 1].rhs; (x)++)

extern const long MAX_TABLE_SIZE;

struct node {
  struct node *next;
  int value;
};

struct stack_element {
  struct stack_element *previous;
  struct stack_element *next;
  struct stack_element *link;
  short state_number;
  short size;
};

/// Given a set of actions that are in conflict on a given symbol, the
/// structure SOURCES_ELEMENT is used to store a mapping from each
/// such action into a set of configurations that can be reached
/// following execution of the action in question up to the point where
/// the automaton is about to shift the conflict symbol.
/// The field CONFIGS is an array indexable by actions which are
/// encoded as follows:
///    1. shift-reduce  [-NUM_RULES..-1]
///    2. reduce        [0..NUM_RULES]
///    3. shift         [NUM_RULES+1..NUM_STATES+1].
/// Each element of CONFIGS points to a set (sorted list) of
/// configurations.  For efficiency, the fields LIST and ROOT are used
/// to store the set (list) of indexed elements of CONFIGS that are not
/// NULL.
/// See routines ALLOCATE_SOURCES, FREE_SOURCES, CLEAR_SOURCES,
///              ADD_CONFIGS, UNION_CONFIG_SETS.
/// See STATE_TO_RESOLVE_CONFLICTS for an explanation of STACK_SEEN.
///
/// A configuration is a stack of states that represents a certain path
/// in the automaton. The stack is implemented as a list of
/// STACK_ELEMENT nodes linked through the field PREVIOUS.
/// A set/list of configurations is linked through the field NEXT.
/// When attempting to resolve conflicts we try to make sure that the
/// set of configurations associated with each action is unique. This
/// is achieved by throwing these configurations into a set and making
/// sure that there are no duplicates. The field LINK is used for that
/// purpose (see routine STACK_WAS_SEEN). The field STATE_NUMBER is
/// obviously used to store the number of a state in the automaton. The
/// field SIZE holds index of the node within the stack. Thus, for the
/// first element of the stack this field represents the number of
/// elements in the stack; for the last element, this field holds the
/// value 1.
/// See routines ALLOCATE_STACK_ELEMENT, FREE_STACK_ELEMENT,
///              ADD_DANGLING_STACK, FREE_DANGLING_STACK.
struct sources_element {
  struct stack_element **configs;
  struct stack_element **stack_seen;
  short *list;
  short root;
};

struct SourcesElementSources {
  struct sources_element sources;
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
  int size;
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
  FILE *sysdcl;
  FILE *syssym;
  FILE *sysdef;
  FILE *sysprs;
  char dcl_tag[SYMBOL_SIZE];
  char sym_tag[SYMBOL_SIZE];
  char def_tag[SYMBOL_SIZE];
  char prs_tag[SYMBOL_SIZE];
};

struct CLIOptions {
  bool nt_check_bit;
  bool conflicts_bit;
  // TODO • have a union for c/cpp/java.
  bool c_bit;
  bool cpp_bit;
  bool java_bit;
  bool scopes_bit;
  bool read_reduce_bit;
  bool goto_default_bit;
  bool shift_default_bit;
  bool byte_bit;
  bool single_productions_bit;
  int lalr_level;
  DefaultOpt default_opt;
  TraceMode trace_opt;
  OptimizeNames names_opt;
  OptimizeMode table_opt;
  int maximum_distance;
  int minimum_distance;
  int stack_size;
  char act_file[80];
  char hact_file[80];
  char escape;
  char ormark;
  char prefix[MAX_PARM_SIZE];
  char suffix[MAX_PARM_SIZE];
  int blockb_len;
  int blocke_len;
  int hblockb_len;
  int hblocke_len;
  char blockb[MAX_PARM_SIZE];
  char blocke[MAX_PARM_SIZE];
  char hblockb[MAX_PARM_SIZE];
  char hblocke[MAX_PARM_SIZE];
};

static struct CLIOptions init_cli_options() {
  return (struct CLIOptions){
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
    .single_productions_bit = false,
    .lalr_level = 1,
    .default_opt = 5,
    .trace_opt = TRACE_CONFLICTS,
    .names_opt = OPTIMIZE_PHRASES,
    .table_opt = OPTIMIZE_NO_TABLE,
    .maximum_distance = 30,
    .minimum_distance = 3,
    .stack_size = 128,
    .escape = '%',
    .ormark = '|',
    .prefix = "",
    .suffix = "",
    .blockb_len = -1,
    .blocke_len = -1,
    .hblockb_len = -1,
    .hblocke_len = -1,
    .blockb = {'/', '.'},
    .blocke = {'.', '/'},
    .hblockb = {'/', ':'},
    .hblocke = {':', '/'},
  };
}

void process_input(char *grm_file, struct OutputFiles *output_files, int argc, char *argv[], char *file_prefix, struct CLIOptions *cli_options, short **rhs_sym);

static char msg_line[MAX_MSG_SIZE];

/// The variables below are global counters.
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

// The variables below are used to hold information about special grammar symbols.
extern int accept_image;
extern int eoft_image;
extern int eolt_image;
extern int empty;
extern int error_image;

extern long num_shift_maps;

struct ConflictCounter {
  long num_sr_conflicts;
  long num_rr_conflicts;
  struct node **in_stat;
};

extern long num_shifts;
extern long num_shift_reduces;
extern long num_gotos;
extern long num_goto_reduces;
extern long num_reductions;
extern long num_entries;

extern struct ruletab_type *rules;

static int RHS_SIZE(const int rule_no, struct ruletab_type *rules) {
  return rules[rule_no + 1].rhs - rules[rule_no].rhs;
}

struct FirstDeps {
  /// CLOSURE is a mapping from non-terminal to a set (linked-list) of
  /// non-terminals.  The set consists of non-terminals that are
  /// automatically introduced via closure when the original non-terminal
  /// is introduced.
  struct node **closure;
  struct node **clitems;
  /// ADEQUATE_ITEM is a mapping from each rule
  /// to the last (complete) item produced by that rule.
  struct node **adequate_item;
};

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

struct SRTable {
  /// SHIFT is an array used to hold the complete set of all shift maps
  /// needed to construct the state automaton. Though its size is
  struct shift_header_type *shift;
  /// REDUCE is a mapping from each state to reduce actions in that state.
  struct reduce_header_type *reduce;
};

extern short *gd_index;

/// STATSET is a mapping from state number to state information.
extern struct statset_type *statset;

/// LASTATS is a similar mapping for look-ahead states.
extern struct lastats_type *lastats;

extern long num_scopes;
extern long scope_rhs_size;
extern long scope_state_size;
extern long num_error_rules;

struct scope_type {
  short prefix;
  short suffix;
  short lhs_symbol;
  short look_ahead;
  short state_set;
};

struct CTabsProps {
  int shift_domain_count;
  int num_terminal_states;
  int check_size;
  int term_check_size;
  int term_action_size;
  int shift_check_size;
  long last_non_terminal;
  long last_terminal;
  long table_size;
  long action_size;
  long increment_size;
};

extern short *scope_state;

extern char *output_buffer;

struct NextPrevious {
  long *next;
  long *previous;
};

struct ImportantAspects {
  long accept_act;
  long error_act;
  long first_index;
  long last_index;
};

struct ProduceTop {
  int top;
};

void compute_produces(int symbol, struct node **direct_produces, short *stack, short *index_of, JBitset produces, struct ProduceTop* top_value);

void reset_temporary_space(void);

void free_temporary_space(void);

void *talloc(long size);

struct DetectedSetSizes {
  long term_set_size;
  long non_term_set_size;
} mkbasic(struct CLIOptions *cli_options, JBitset nt_first, bool* * rmpself, JBitset* first, struct FirstDeps* fd, struct ruletab_type *rules, short *rhs_sym);

extern char ormark;
extern char escape;

void restore_symbol(char *out, const char *in, char ormark, char escape);

void *galloc(long size);

typedef long cell;

struct GlobalSpace {
  /// The following are global variables and constants used to manage a
  /// pool of global space. Externally, the user invokes one of the
  /// functions:
  ///
  ///    ALLOCATE_NODE
  ///    ALLOCATE_GOTO_MAP
  ///    ALLOCATE_SHIFT_MAP
  ///    ALLOCATE_REDUCE_MAP
  ///
  /// These functions allocate space from the global pool in the same
  /// using the function "galloc" below.
  cell **global_base;
  long global_top;
  long global_size;
  long global_base_size;
  struct node *node_pool;
} gs;

struct TableOutput {
  long *ordered_state;
  long *symbol_map;
  long *state_index;
  long *state_list;
};

static void ffree(void *y) {
  return free(y); /* { free(x); x = (void *) ULONG_MAX; } */
}

static int TOUPPER(const int c) {
  return islower(c) ? toupper(c) : c;
}

static long MAX(const long a, const long b) {
  return a > b ? a : b;
}

static long MIN(const long a, const long b) {
  return a < b ? a : b;
}

static long ABS(const long x) {
  return x < 0 ? -x : x;
}

static void PRNT(char *msg) {
  printf("%s\n", msg);
}

#define PRNT3(...) \
  snprintf(msg_line, MAX_MSG_SIZE, __VA_ARGS__); \
  PRNT(msg_line);

#define PRNT4(msg, size, ...) \
  snprintf(msg, size, __VA_ARGS__); \
  PRNT(msg);

static void PRNTWNG(char *msg) {
  printf("***WARNING: %s\n", msg);
}

#define PRNTWNG2(...) \
  snprintf(msg_line, MAX_MSG_SIZE, __VA_ARGS__); \
  PRNTWNG(msg_line);

static void PRNTERR(char *msg) {
  printf("***ERROR: %s\n", msg);
}

#define PRNTERR2(...) \
  snprintf(msg_line, MAX_MSG_SIZE, __VA_ARGS__); \
  PRNTERR(msg_line);

struct new_state_type {
  struct reduce_header_type reduce;
  short shift_number;
  short link;
  short thread;
  short image;
};

/// STACK_ROOT is used in la_traverse to construct a stack of symbols.
/// The boolean vector SINGLE_COMPLETE_ITEM identifies states whose
/// kernel consists of a single final item and other conditions allows
/// us to compute default reductions for such states.
/// The vector LA_BASE is used in COMPUTE_READ and TRACE_LALR_PATH to
/// identify states whose read sets can be completely computed from
/// their kernel items.
struct StackRoot {
  struct node *stack_root;
};

extern int increment;

void cmprtim(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, long *scope_right_side, struct lastats_type *lastats, long *gotodef, short *gd_index, short *gd_range, short *scope_state, struct statset_type *statset,struct ruletab_type *rules, struct itemtab *item_table);

void cmprspa(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, long *scope_right_side, struct lastats_type *lastats, short *shiftdf, long *gotodef, short *gd_index, short *gd_range, short *scope_state, struct statset_type *statset,struct ruletab_type *rules, struct itemtab *item_table);

bool* init_rmpself(JBitset produces);

void fill_in(char string[], int amount, char character);

void free_nodes(struct node *head, struct node *tail);

struct ConflictCounter mkrdcts(struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct SourcesElementSources* ses, bool *rmpself, JBitset first, struct node **adequate_item, struct SRTable* srt, struct lastats_type *lastats, bool *null_nt, short *gd_index, struct ruletab_type *rules, struct statset_type *statset, struct itemtab *item_table, short *rhs_sym);

/// LA_INDEX and LA_SET are temporary look-ahead sets, each of which will
/// be pointed to by a GOTO action, and the associated set will be
/// initialized to READ_SET(S), where S is the state identified by the GOTO
/// action in question. READ_SET(S) is a set of terminals on which an action
/// is defined in state S. See COMPUTE_READ for more details.
/// LA_TOP is used to compute the number of such sets needed.
struct LAIndex {
  short *la_index;
  JBitset la_set;
};

void la_traverse(int state_no, int goto_indx, int *stack_top, struct StackRoot* sr, JBitset first, struct LAIndex* lai, struct node **adequate_item, struct node **in_stat, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table);

void remove_single_productions(struct DetectedSetSizes* dss, struct StackRoot* sr, JBitset first, struct LAIndex* lai, struct node **conflict_symbols, JBitset la_set, struct node **adequate_item, struct SRTable* srt, struct statset_type *statset, struct lastats_type *lastats, short *gd_index, struct node **in_stat, struct ruletab_type *rules, struct itemtab *item_table, short *rhs_sym);

void mkstats(struct CLIOptions *cli_options, struct DetectedSetSizes* dss, JBitset first, struct scope_type *scope, struct node **clitems, struct node **closure, struct SRTable* srt, long *scope_right_side, bool *null_nt, short **scope_state, struct itemtab *item_table, struct ruletab_type *rules, short *rhs_sym, short **gd_range, short **gd_index);

void nospace();

int number_len(int state_no);

void partset(JBitset collection, const long *element_size, const long *list, long *start, long *stack, long set_size, bool from_process_scopes);

void print_item(int item_no, struct CLIOptions* cli_options, struct ruletab_type *rules, struct itemtab *item_table, short *rhs_sym);

void print_large_token(char *line, char *token, const char *indent, int len);

void print_state(int state_no, struct CLIOptions* cli_options, struct node **adequate_item, struct SRTable* srt, struct lastats_type *lastats, struct statset_type *statset, struct node **in_stat, struct ruletab_type *rules, struct itemtab *item_table, short *rhs_sym);

void process_error_maps(struct CLIOptions *cli_options, FILE *systab, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp);

void print_space_parser(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, long *term_state_index, long *shift_check_index, struct CTabsProps* ctp, struct new_state_type *new_state_element, short *shift_image, short *real_shift_number, struct OutputFiles* of, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, long *scope_right_side, short *shiftdf, long *gotodef, short *gd_index, short *gd_range, struct ruletab_type *rules, short *scope_state, struct statset_type *statset, struct itemtab *item_table);

void print_time_parser(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, long *scope_right_side, struct lastats_type *lastats, long *gotodef, short *gd_index, short *gd_range, struct ruletab_type *rules, short *scope_state, struct statset_type *statset, struct itemtab *item_table);

void populate_start_file(FILE **file, char *file_tag, struct CLIOptions *cli_options);

void process_tables(char *tab_file, struct OutputFiles *output_files, struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, struct scope_type *scope, short *gd_range, struct SRTable* srt, long *scope_right_side, struct lastats_type *lastats, short *shiftdf, long *gotodef, short *gd_index, struct statset_type *statset, short *scope_state, struct ruletab_type *rules, struct itemtab *item_table);

void sortdes(long array[], long count[], long low, long high, long max);

void reallocate(struct CLIOptions *cli_options, struct CTabsProps* ctp, struct NextPrevious* np, struct ImportantAspects* ia);

struct long_array {
  long *raw;
  long size;
};

#define ArrayLong struct long_array

/// This function allocates an array of size "size" of int integers.
static long *Allocate_long_array(const long n) {
  long *p;
  calloc0(p, n, long);
  return &p[0];
}

/// This function allocates an array of size "size" of int integers.
static ArrayLong Allocate_long_array2(const long n) {
  long *p;
  calloc0(p, n, long);
  return (struct long_array) {
    .raw = &p[0],
    .size = n,
  };
}

/// This function allocates an array of size "size" of short integers.
static short *Allocate_short_array(const long n) {
  short *p;
  calloc0(p, n, short);
  return &p[0];
}

/// This function allocates an array of size "size" of type boolean.
static bool *Allocate_boolean_array(const long n) {
  bool *p;
  calloc0(p, n, bool);
  return &p[0];
}

/// This function allocates a node structure and returns a pointer to it.
/// If there are nodes in the free pool, one of them is returned. Otherwise,
/// a new node is allocated from the global storage pool.
static struct node *Allocate_node() {
  struct node *p = gs.node_pool;
  if (p != NULL) {
    // Is free list not empty?
    gs.node_pool = p->next;
  } else {
    galloc0p(&p, struct node, 1);
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
  galloc0p(&go_to.map, struct goto_type, n);
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
  galloc0p(&sh.map, struct shift_type, n)
  sh.map--; /* map will be indexed in range 1..size */
  return sh;
}

/// This function allocates space for a REDUCE map with "size"+1 elements,
/// initializes and returns a REDUCE header for that map. The 0th element of
/// a reduce map is used for the default reduction.
static struct reduce_header_type Allocate_reduce_map(const long n) {
  struct reduce_header_type red;
  galloc0p(&red.map, struct reduce_type, n + 1);
  red.size = n;
  return red;
}

static struct TableOutput init_table_output() {
  return (struct TableOutput) {
    .ordered_state = Allocate_long_array(max_la_state + 1),
    .state_index = Allocate_long_array(max_la_state + 1),
    .symbol_map = Allocate_long_array(num_symbols + 1),
    .state_list = Allocate_long_array(max_la_state + 1),
  };
}

static void calloc0_set_fn(JBitset* into, const int s, const int l) {
  *into = (JBitset) {.raw = calloc(s, l * sizeof(BOOLEAN_CELL)), .size = (l)};
  if (into->raw == NULL) {
    nospace();
  }
}
