#pragma once
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>
#include <stdio.h>

// region bitset

typedef unsigned int BOOLEAN_CELL;

struct jbitset {
  BOOLEAN_CELL *raw;
  long size;
};

#define JBitset struct jbitset

#define calloc0_set(into, s, l) \
  (into) = (JBitset) {.raw = calloc(s, (l) * sizeof(BOOLEAN_CELL)), .size = (l)}; \
  if ((into).raw == NULL) \
    nospace();

/// The following macros are used to define operations on sets that
/// are represented as bit-strings.  BOOLEAN_CELL is a type that is
/// used as the elemental unit used to construct the sets.  For
/// example, if BOOLEAN_CELL consists of four bytes and assumming
/// that each byte contains 8 bits then the constant SIZEOF_BC
/// represents the total number of bits that is contained in each
/// elemental unit.
///
/// In general, a parameter called "set" or "set"i, where i is an
/// integer, is a pointer to a set or array of sets; a parameter
/// called "i" or "j" represents an index in an array of sets; a
/// parameter called "b" represents a particular element (or bit)
/// within a set.
static const int SIZEOF_BC = sizeof(BOOLEAN_CELL) * CHAR_BIT;
static const int BC_OFFSET = SIZEOF_BC - 1;

/// This macro takes as argument an array of bit sets called "set",
/// an integer "nt" indicating the index of a particular set in the
/// array and an integer "t" indicating a particular element within
/// the set. IS_IN_SET check whether ot not the element "t" is in
/// the set "set(nt)".
///
/// The value (nt*term_set_size) is used to determine the starting
/// address of the set element in question.  The value
/// (??? / SIZEOF_BC) is used to determine the actual BOOLEAN_CELL
/// containing the bit in question.  Finally, the value
/// (SIZEOF_BC - (t % SIZEOF_BC)) identifies the actual bit in the
/// unit. The bit in question is pushed to the first position and
/// and-ed with the value 01. This operation yields the value TRUE
/// if the bit is on. Otherwise, the value FALSE is obtained.
/// Recall that in C, one cannot shift (left or right) by 0. This
/// is why the ? is used here.
static bool IS_IN_SET(const JBitset set, const int i, const int b) {
  // is b in set[i] ?
  return set.raw[i * set.size + (b - 1) / SIZEOF_BC] &
         ((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

/// The macro SET_UNION takes as argument two arrays of sets:
/// "set1" and "set2", and two integers "i" and "j" which are
/// indices to be used to access particular sets in "set1" and
/// "set2", respectively.  SET_UNION computes the union of the two
/// sets in question and places the result in the relevant set in
/// "set1".
///
/// The remaining macros are either analogous to IS_IN_SET or
/// SET_UNION.
///
/// Note that a macro with the variable "kji" declared in its body
/// should not be invoked with a parameter of the same name.
/// set[i] union set2[j]
static void SET_UNION(const JBitset set1, const int i, const JBitset set2, const int j) {
  if (set1.size != set2.size) {
    exit(666);
  }
  const long size = set1.size;
  for (register long kji = 0; kji < size; kji++) {
    set1.raw[i * size + kji] |= set2.raw[j * size + kji];
  }
}

/// set = {}
static void INIT_SET(const JBitset set) {
  for (register long kji = 0; kji < set.size; kji++) {
    set.raw[kji] = 0;
  }
}

/// set1[i] = set2[j]
static void ASSIGN_SET(const JBitset set1, const int i, const JBitset set2, const int j) {
  if (set1.size != set2.size) exit(666);
  const long size = set1.size;
  for (register long kji = 0; kji < size; kji++) {
    set1.raw[i * size + kji] = set2.raw[j * size + kji];
  }
}

/// set[i] = set[i] with b;
static void SET_BIT_IN(const JBitset set, const int i, const int b) {
  set.raw[i * set.size + (b - 1) / SIZEOF_BC] |=
      (b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1;
}

/// set[i] = set[i] less b;
static void RESET_BIT_IN(const JBitset set, const int i, const int b) {
  set.raw[i * set.size + (b - 1) / SIZEOF_BC] &=
      ~((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

static void INIT_BITSET(const JBitset collection, const int i) {
  for (register long j = 0; j < collection.size; j++) {
    collection.raw[i * collection.size + j] = 0;
  }
}

static void RESET_BIT(const JBitset set, const int b) {
  set.raw[(b - 1) / SIZEOF_BC] &=
      ~((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

/// is b in set ?
static bool IS_ELEMENT(const JBitset set, const int b) {
  return set.raw[(b - 1) / SIZEOF_BC] &
         ((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

static void B_ASSIGN_SET(const JBitset s1, const int dest, const JBitset s2, const int source, const int bound) {
  for (int j = 0; j < bound; j++) {
    s1.raw[dest * bound + j] = s2.raw[source * bound + j];
  }
}

static void B_SET_UNION(const JBitset s1, const int dest, const JBitset s2, const int source, const int bound) {
  for (int j = 0; j < bound; j++) {
    s1.raw[dest * bound + j] |= s2.raw[source * bound + j];
  }
}

/// EQUAL_SETS checks to see if two sets are equal and returns True or False
static bool equal_sets(const JBitset set1, const int indx1, const JBitset set2, const int indx2, const int bound) {
  for (register long i = 0; i < bound; i++) {
    if (set1.raw[indx1 * bound + i] != set2.raw[indx2 * bound + i]) {
      return false;
    }
  }
  return true;
}

// endregion






struct symno_type {
  long ptr;
  int name_index;
};

void nospace();

void *galloc(long size);

void *talloc(long size);

#define galloc0p(into, x, times) \
  (*into) = (x *) galloc((times) * sizeof(x)); \
  if ((*into) == (x *) NULL) \
    nospace();

#define talloc0p(into, x) \
  (*into) = (x *) talloc(sizeof(x)); \
  if ((*into) == (x *) NULL) \
    nospace();

#define talloc0p_raw(into, xyz, s) \
  (*into) = (xyz *) talloc(s); \
  if ((*into) == (xyz *) NULL) \
    nospace();

#define realloc0p(into, times, t) \
  (*into) = (t *) realloc((*into), (times) * sizeof(t)); \
  if ((*into) == (t *) NULL) \
    nospace();

#define calloc0p(into, size, x) \
  (*into) = (x *) calloc((size), sizeof(x)); \
  if ((*into) == (x *) NULL) \
    nospace();

static const int MAX_PARM_SIZE = 22;
static const int SYMBOL_SIZE = 256;
static const int MAX_MSG_SIZE = 256 + SYMBOL_SIZE;
static const int PRINT_LINE_SIZE = 80;
static const int PARSER_LINE_SIZE = 80;
static const int MAX_LINE_SIZE = 512;

struct LaStats {
  struct lastats_type *lastats;
};


typedef struct {
  int value;
} OptimizeMode;
static const OptimizeMode OPTIMIZE_NO_TABLE = { .value = 0 },
  OPTIMIZE_TIME = { .value = 1 },
  OPTIMIZE_SPACE = { .value = 2 };



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

#define for_each_t_fw(x, ls) (int (x) = 1; (x) <= (ls)->num_terminals; (x)++)

#define for_each_t_bw(x, ls) (int (x) = (ls)->num_terminals; (x) >= 1; (x)--)

#define for_each_nt_fw(x, ls) (int (x) = (ls)->num_terminals + 1; (x) <= (ls)->num_symbols; (x)++)

#define for_each_nt_bw(x, ls) (int (x) = (ls)->num_symbols; (x) >= (ls)->num_terminals + 1; (x)--)

#define for_each_symbol(x, ls) (int (x) = 1; (x) <= (ls)->num_symbols; (x)++)

#define for_each_la_state(x, ls) (int (x) = (ls)->num_states + 1; (x) <= (ls)->max_la_state; (x)++)

#define for_each_state(x, ls) (int (x) = 1; (x) <= (ls)->num_states; (x)++)

#define for_each_item(x, ls) (int (x) = 1; (x) <= (ls)->num_items; (x)++)

#define for_each_rule_fw(x, ls) (int (x) = 0; (x) <= (ls)->num_rules; (x)++)

#define for_each_rhs(x, rule_no, rules) (int (x) = (rules)[rule_no].rhs; (x) < rules[(rule_no) + 1].rhs; (x)++)

// region long array
struct long_array {
  long *raw;
  long size;
};

#define ArrayLong struct long_array

/// This function allocates an array of size "size" of int integers.
static ArrayLong Allocate_long_array2(const long n) {
  long *p;
  calloc0p(&p, n, long);
  return (struct long_array) {
    .raw = &p[0],
    .size = n,
  };
}
// endregion

// region short array
struct short_array {
  short *raw;
  long size;
};

#define ArrayShort struct short_array

static ArrayShort Allocate_short_array2(const long n) {
  short *p;
  calloc0p(&p, n, short);
  return (struct short_array) {
    .raw = &p[0],
    .size = n,
  };
}
// endregion

// region boolean array
struct bool_array {
  bool *raw;
  long size;
};

#define ArrayBool struct bool_array

static ArrayBool Allocate_bool_array2(const long n) {
  bool *p;
  calloc0p(&p, n, bool);
  return (struct bool_array) {
    .raw = &p[0],
    .size = n,
  };
}
// endregion

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
  ArrayShort list;
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
/// list of all the right-hand-side symbols specified in the grammar.
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
  bool error_maps_bit;
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
  OptimizeMode table_opt;
  int stack_size;
  char act_file[80];
  char hact_file[80];
  char escape;
  char ormark;
  char prefix[MAX_PARM_SIZE];
  char suffix[MAX_PARM_SIZE];
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
    .error_maps_bit = true,
    .lalr_level = 1,
    .default_opt = 5,
    .trace_opt = TRACE_CONFLICTS,
    .table_opt = OPTIMIZE_NO_TABLE,
    .stack_size = 128,
    .escape = '%',
    .ormark = '|',
    .prefix = "",
    .suffix = "",
  };
}

static char msg_line[MAX_MSG_SIZE];

struct LAState {
  /// The variables below are global counters.
  long num_items;
  /// NUM_STATES, the actual number of elements used in it is indicated
  /// by the integer NUM_SHIFT_MAPS. NUM_STATES elements were allocated,
  /// because if the user requests that certain single productions be
  /// removed, a Shift map containing actions involving such productions
  /// cannot be shared.
  long num_states;
  long max_la_state;
  long num_shifts;
  long num_single_productions;
  long gotodom_size;
  long num_names;
  long num_non_terminals;
  long num_shift_maps;
  long num_shift_reduces;
  long num_gotos;
  long num_goto_reduces;
  long num_reductions;
  long num_entries;
  long num_rules;
  long num_symbols;
  long num_terminals;
};

// The variables below are used to hold information about special grammar symbols.
extern int accept_image;
extern int eoft_image;
extern int eolt_image;
extern int empty;
extern int error_image;

struct ConflictCounter {
  long num_sr_conflicts;
  long num_rr_conflicts;
  struct node **in_stat;
};

struct ScopeCounter {
  long num_scopes;
  long scope_rhs_size;
  long scope_state_size;
};

static int RHS_SIZE(const int rule_no, const struct ruletab_type *rules) {
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
struct itemtab {
  short symbol;
  short rule_number;
  short suffix_index;
  short dot;
};

struct SRTable {
  /// SHIFT is an array used to hold the complete set of all shift maps
  /// needed to construct the state automaton. Though its size is ...
  struct shift_header_type *shift;
  /// REDUCE is a mapping from each state to reduce actions in that state.
  struct reduce_header_type *reduce;
};

struct StatSet {
  /// STATSET is a mapping from state number to state information.
  struct statset_type *statset;
};

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

struct ImportantAspects {
  long accept_act;
  long error_act;
  long first_index;
  long last_index;
};

struct ProduceTop {
  int top;
};

struct NextPrevious {
  ArrayLong next;
  ArrayLong previous;
};

struct DetectedSetSizes {
  long term_set_size;
  long non_term_set_size;
  /// NULL_NT is a boolean vector that indicates whether a given non-terminal is nullable.
  ArrayBool null_nt;
};

void restore_symbol(char *out, const char *in, char ormark, char escape);

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
  ArrayLong ordered_state;
  ArrayLong symbol_map;
  ArrayLong state_index;
  ArrayLong state_list;
};

static void ffree(void *y) {
  return free(y); /* { free(x); x = (void *) ULONG_MAX; } */
}

static char TOUPPER(const int c) {
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

#define PRNT2(...) \
  snprintf(msg_line, MAX_MSG_SIZE, __VA_ARGS__); \
  PRNT(msg_line);

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

void free_nodes(struct node *head, struct node *tail);

struct ConflictCounter mkrdcts(struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct SourcesElementSources* ses, ArrayBool rmpself, JBitset first, struct node **adequate_item, struct SRTable* srt, ArrayBool null_nt, ArrayShort gd_index, struct ruletab_type *rules, struct statset_type *statset, struct itemtab *item_table, ArrayShort rhs_sym, struct LaStats* las, long* la_top, char *string_table, struct symno_type *symno, struct LAState* ls);

/// LA_INDEX and LA_SET are temporary look-ahead sets, each of which will
/// be pointed to by a GOTO action, and the associated set will be
/// initialized to READ_SET(S), where S is the state identified by the GOTO
/// action in question. READ_SET(S) is a set of terminals on which an action
/// is defined in state S. See COMPUTE_READ for more details.
/// LA_TOP is used to compute the number of such sets needed.
struct LAIndex {
  ArrayShort la_index;
  JBitset la_set;
};

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

static struct TableOutput init_table_output(struct LAState* ls) {
  return (struct TableOutput) {
    .ordered_state = Allocate_long_array2(ls->max_la_state + 1),
    .state_index = Allocate_long_array2(ls->max_la_state + 1),
    .symbol_map = Allocate_long_array2(ls->num_symbols + 1),
    .state_list = Allocate_long_array2(ls->max_la_state + 1),
  };
}

static void calloc0_set_fn(JBitset* into, const int s, const int l) {
  *into = (JBitset) {.raw = calloc(s, l * sizeof(BOOLEAN_CELL)), .size = (l)};
  if (into->raw == NULL) {
    nospace();
  }
}

struct ScannerState {
  short ct;
  short ct_start_col;
  short ct_end_col;
  short ct_length;
  long ct_start_line;
  long ct_end_line;
  char *linestart;
  int line_no;
  char *ct_ptr;
  char *bufend;
  char *p1;
  char *p2;
  char *input_buffer;
};

/// structure to store rule in first pass
struct rulehdr_type {
  struct node *rhs_root;
  long lhs;
  bool sp;
};

/// structure to store location of macro def.
struct defelmt_type {
  short next;
  short length;
  short start_column;
  short end_column;
  char *macro;
  long start_line;
  long end_line;
  char name[SYMBOL_SIZE + 1];
};

/// structure to store location of action
struct actelmt_type {
  long start_line;
  long end_line;
  long rule_number;
  short start_column;
  short end_column;
  bool header_block;
};

/// structure used to hash grammar symbols
struct hash_type {
  struct hash_type *link;
  long number;
  short name_index;
  long st_ptr;
};

struct ParserState {
  struct hash_type **hash_table;
  struct node *start_symbol_root;
  struct terminal_type *terminal;
  struct rulehdr_type *rulehdr;
  struct defelmt_type *defelmt;
  struct actelmt_type *actelmt;
  char ormark;
  char escape;
  bool error_maps_bit;
  int num_acts;
  int num_defs;
  long defelmt_size;
  long actelmt_size;
  long rulehdr_size;
  long string_offset;
  int stack_top;
  char *string_table;
  struct symno_type *symno;
  long num_items;
  long num_names;
  long num_non_terminals;
  long num_rules;
  long num_symbols;
  long num_terminals;
  long num_single_productions;
};

/// structure used to hold token information
struct terminal_type {
  long start_line;
  long end_line;
  short start_column;
  short end_column;
  short length;
  short kind;
  char name[SYMBOL_SIZE + 1];
};

static bool IS_A_TERMINAL_P(const int i, struct ParserState* ps) {
  return i <= ps->num_terminals;
}

static bool IS_A_TERMINAL_L(const int i, struct LAState* ls) {
  return i <= ls->num_terminals;
}

static bool IS_A_NON_TERMINAL(const int i, struct LAState* ls) {
  return i > ls->num_terminals;
}

void la_traverse(int state_no, int goto_indx, int *stack_top, struct StackRoot* sr, JBitset first, struct LAIndex* lai, struct node **adequate_item, struct node **in_stat, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table);

int number_len(int state_no);

void process_input(char *grm_file, struct OutputFiles *output_files, int argc, char *argv[], char *file_prefix, struct CLIOptions *cli_options, ArrayShort *rhs_sym, struct ruletab_type **rulesp, struct symno_type **symno, struct ParserState* ps, int **name);

/// The following variables hold the names
/// of keywords and predefined macros.
static char kdefine[8] = " define";
static char kterminals[11] = " terminals";
static char kalias[7] = " alias";
static char kstart[7] = " start";
static char krules[7] = " rules";
static char knames[7] = " names";
static char kend[5] = " end";
static char krule_text[11] = " rule_text";
static char krule_number[13] = " rule_number";
static char knum_rules[11] = " num_rules";
static char krule_size[11] = " rule_size";
static char knum_terminals[15] = " num_terminals";
static char knum_non_terminals[19] = " num_non_terminals";
static char knum_symbols[13] = " num_symbols";
static char kinput_file[12] = " input_file";
static char kcurrent_line[14] = " current_line";
static char knext_line[11] = " next_line";
/// Note that the next four keywords start with \n instead of
/// the escape character.  This is to prevent the user from
/// declaring a grammar symbol with the same name.  The
/// end-of-line character was chosen since that character can
/// never appear in the input without being interpreted as
/// marking the end of an input line.  When printing such a
/// keyword, the \n is properly replaced by the escape character.
static char kempty[7] = "\nempty";
static char kerror[7] = "\nerror";
static char keoft[5] = "\neof";
static char kaccept[5] = "\nacc";
static char kstart_nt[7] = " start";
static char keolt[5] = " eol";

char *RETRIEVE_STRING(int indx, char *string_table, const struct symno_type *symno);

char *RETRIEVE_NAME(int indx, char *string_table, const int *name);
