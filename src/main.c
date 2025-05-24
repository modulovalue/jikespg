#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <limits.h>
#include <assert.h>
#include <stdio.h>





// region other
int accept_image;

int eoft_image;

int eolt_image;

int empty;

int error_image;

typedef long cell;

struct TemporarySpace {
  cell **temp_base;
  long temp_top;
  long temp_size;
  long temp_base_size;
};

struct TemporarySpace ts = (struct TemporarySpace) {
  .temp_base = NULL,
  .temp_top = 0,
  .temp_size = 0,
  .temp_base_size = 0,
};

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
};

struct GlobalSpace gs = (struct GlobalSpace) {
  .global_base = NULL,
  .global_top = 0,
  .global_size = 0,
  .global_base_size = 0,
  .node_pool = NULL,
};

static const int STRING_BUFFER_SIZE = 8192;

static const int MAX_PARM_SIZE = 22;

static const int PRINT_LINE_SIZE = 80;

static const int OUTPUT_PARM_SIZE = MAX_PARM_SIZE + 7;

static const int MAXIMUM_LA_LEVEL = 100;

const int LOG_BLKSIZE = 14;

const int BLKSIZE = 1 << LOG_BLKSIZE;

const int LEN = PRINT_LINE_SIZE - 4;

const int increment = 30;

const long MAX_TABLE_SIZE = (USHRT_MAX < INT_MAX ? USHRT_MAX : INT_MAX) - 1;
// endregion













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
static bool IS_IN_SET(const JBitset set, const long i, const int b) {
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
static void SET_UNION(const JBitset set1, const long i, const JBitset set2, const long j) {
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
static void ASSIGN_SET(const JBitset set1, const long i, const JBitset set2, const long j) {
  if (set1.size != set2.size) exit(666);
  const long size = set1.size;
  for (register long kji = 0; kji < size; kji++) {
    set1.raw[i * size + kji] = set2.raw[j * size + kji];
  }
}

/// set[i] = set[i] with b;
static void SET_BIT_IN(const JBitset set, const long i, const long b) {
  set.raw[i * set.size + (b - 1) / SIZEOF_BC] |=
      (b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1;
}

/// set[i] = set[i] less b;
static void RESET_BIT_IN(const JBitset set, const long i, const long b) {
  set.raw[i * set.size + (b - 1) / SIZEOF_BC] &=
      ~((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

static void INIT_BITSET(const JBitset collection, const long i) {
  for (register long j = 0; j < collection.size; j++) {
    collection.raw[i * collection.size + j] = 0;
  }
}

static void RESET_BIT(const JBitset set, const long b) {
  set.raw[(b - 1) / SIZEOF_BC] &=
      ~((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

/// is b in set ?
static bool IS_ELEMENT(const JBitset set, const long b) {
  return set.raw[(b - 1) / SIZEOF_BC] &
         ((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

static void B_ASSIGN_SET(const JBitset s1, const long dest, const JBitset s2, const long source, const long bound) {
  for (int j = 0; j < bound; j++) {
    s1.raw[dest * bound + j] = s2.raw[source * bound + j];
  }
}

static void B_SET_UNION(const JBitset s1, const long dest, const JBitset s2, const long source, const long bound) {
  for (int j = 0; j < bound; j++) {
    s1.raw[dest * bound + j] |= s2.raw[source * bound + j];
  }
}

/// EQUAL_SETS checks to see if two sets are equal and returns True or False
static bool equal_sets(const JBitset set1, const long indx1, const JBitset set2, const long indx2, const long bound) {
  for (register long i = 0; i < bound; i++) {
    if (set1.raw[indx1 * bound + i] != set2.raw[indx2 * bound + i]) {
      return false;
    }
  }
  return true;
}
// endregion








// region common
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

static const int SYMBOL_SIZE = 256;
static const int MAX_MSG_SIZE = 256 + SYMBOL_SIZE;
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

static const short INFINITY = SHRT_MAX;
static const short OMEGA = SHRT_MIN;
static const short NIL = SHRT_MIN + 1;
static const int DEFAULT_SYMBOL = 0;

static const int HT_SIZE = 701; /* 701 is a prime */
static const int RULEHDR_INCREMENT = 1024;
static const int ACTELMT_INCREMENT = 1024;
static const int DEFELMT_INCREMENT = 16;

static const int IOBUFFER_SIZE = 655360;

#define for_each_t_fw(x, ls) (long (x) = 1; (x) <= (ls)->num_terminals; (x)++)

#define for_each_t_bw(x, ls) (long (x) = (ls)->num_terminals; (x) >= 1; (x)--)

#define for_each_nt_fw(x, ls) (long (x) = (ls)->num_terminals + 1; (x) <= (ls)->num_symbols; (x)++)

#define for_each_nt_bw(x, ls) (long (x) = (ls)->num_symbols; (x) >= (ls)->num_terminals + 1; (x)--)

#define for_each_symbol(x, ls) (long (x) = 1; (x) <= (ls)->num_symbols; (x)++)

#define for_each_la_state(x, ls) (long (x) = (ls)->num_states + 1; (x) <= (ls)->max_la_state; (x)++)

#define for_each_state(x, ls) (long (x) = 1; (x) <= (ls)->num_states; (x)++)

#define for_each_item(x, ls) (long (x) = 1; (x) <= (ls)->num_items; (x)++)

#define for_each_rule_fw(x, ls) (long (x) = 0; (x) <= (ls)->num_rules; (x)++)

#define for_each_rhs(x, rule_no, rules) (long (x) = (rules)[rule_no].rhs; (x) < rules[(rule_no) + 1].rhs; (x)++)

struct long_array {
  long *raw;
  long size;
};

#define ListInt struct long_array

/// This function allocates an array of size "size" of int integers.
static ListInt allocateListInt(const long n) {
  long *p;
  calloc0p(&p, n, long);
  return (struct long_array) {
    .raw = &p[0],
    .size = n,
  };
}

struct bool_array {
  bool *raw;
  long size;
};

#define ListBool struct bool_array

static ListBool Allocate_bool_array2(const long n) {
  bool *p;
  calloc0p(&p, n, bool);
  return (struct bool_array) {
    .raw = &p[0],
    .size = n,
  };
}

struct node {
  struct node *next;
  int value;
};

struct stack_element {
  struct stack_element *previous;
  struct stack_element *next;
  struct stack_element *link;
  long state_number;
  long size;
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
  ListInt list;
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
  bool read_reduce_bit;
  bool goto_default_bit;
  bool shift_default_bit;
  bool byte_bit;
  int lalr_level;
  DefaultOpt default_opt;
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
    .read_reduce_bit = true,
    .goto_default_bit = true,
    .shift_default_bit = false,
    .byte_bit = true,
    .error_maps_bit = true,
    .lalr_level = 1,
    .default_opt = 5,
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

struct ConflictCounter {
  long num_sr_conflicts;
  long num_rr_conflicts;
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
  ListInt next;
  ListInt previous;
};

struct DetectedSetSizes {
  long term_set_size;
  long non_term_set_size;
  /// NULL_NT is a boolean vector that indicates whether a given non-terminal is nullable.
  ListBool null_nt;
};

void restore_symbol(char *out, const char *in, char ormark, char escape);

struct TableOutput {
  ListInt ordered_state;
  ListInt symbol_map;
  ListInt state_index;
  ListInt state_list;
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

/// LA_INDEX and LA_SET are temporary look-ahead sets, each of which will
/// be pointed to by a GOTO action, and the associated set will be
/// initialized to READ_SET(S), where S is the state identified by the GOTO
/// action in question. READ_SET(S) is a set of terminals on which an action
/// is defined in state S. See COMPUTE_READ for more details.
/// LA_TOP is used to compute the number of such sets needed.
struct LAIndex {
  ListInt la_index;
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
    .ordered_state = allocateListInt(ls->max_la_state + 1),
    .state_index = allocateListInt(ls->max_la_state + 1),
    .symbol_map = allocateListInt(ls->num_symbols + 1),
    .state_list = allocateListInt(ls->max_la_state + 1),
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
// endregion






















// region generated parser
/// HASH takes as argument a symbol and hashes it into a location in
/// HASH_TABLE.
static int hash(const char *symbl) {
  unsigned long hash_value = 0;
  for (; *symbl != '\0'; symbl++) {
    const unsigned short k = *symbl;
    symbl++;
    hash_value += (k << 7) + *symbl;
    if (*symbl == '\0') {
      break;
    }
  }
  return hash_value % HT_SIZE;
}

/// INSERT_STRING takes as an argument a pointer to a ht_elemt structure and
/// a character string. It inserts the string into the string table and sets
/// the value of node to the index into the string table.
static void insert_string(struct hash_type *q, const char *string, long* string_offset, char **string_table) {
  long string_size = 0;
  if (*string_offset + strlen(string) >= string_size) {
    string_size += STRING_BUFFER_SIZE;
    *string_table = (char *) (*string_table == (char *) NULL
       ? malloc(string_size * sizeof(char))
       : realloc(*string_table, string_size * sizeof(char)));
    if (*string_table == (char *) NULL) {
      nospace();
    }
  }
  q->st_ptr = *string_offset;
  // Copy until NULL is copied.
  while (((*string_table)[(*string_offset)++] = *string++)) {
  }
}

static bool EQUAL_STRING(const char *symb, const struct hash_type *p, const char *string_table) {
  return strcmp(symb, string_table + p->st_ptr) == 0;
}

/// PROCESS_SYMBOL takes as an argument a pointer to the most recent token
/// which would be either a symbol or a macro name and then processes it. If
/// the token is a macro name then a check is made to see if it is a pre-
/// defined macro. If it is then an error message is printed and the program
/// is halted. If not, or if the token is a symbol then it is hashed into the
/// hash_table and its string is copied into the string table.  A struct is
/// created for the token. The ST_PTR field contains the index into the string
/// table and the NUMBER field is set to zero. Later on if the token is a
/// symbol, the value of the NUMBER field is changed to the appropriate symbol
/// number. However, if the token is a macro name, its value will remain zero.
/// The NAME_INDEX field is set to OMEGA and will be assigned a value later.
/// ASSIGN_SYMBOL_NO takes as arguments a pointer to a node and an image
/// number and assigns a symbol number to the symbol pointed to by the node.
void assign_symbol_no(const char *string_ptr, const int image, struct ParserState* ps) {
  struct hash_type *p;
  const int i = hash(string_ptr);
  for (p = ps->hash_table[i]; p != NULL; p = p->link) {
    if (EQUAL_STRING(string_ptr, p, ps->string_table)) /* Are they the same */
      return;
  }
  talloc0p(&p, struct hash_type);
  if (image == OMEGA) {
    ps->num_symbols++;
    p->number = ps->num_symbols;
  } else {
    p->number = -image;
  }
  p->name_index = OMEGA;
  insert_string(p, string_ptr, &ps->string_offset, &ps->string_table);
  p->link = ps->hash_table[i];
  ps->hash_table[i] = p;
}

/// ALIAS_MAP takes as input a symbol and an image. It searches the hash
/// table for stringptr and if it finds it, it turns it into an alias of the
/// symbol whose number is IMAGE. Otherwise, it invokes PROCESS_SYMBOL and
/// ASSIGN SYMBOL_NO to enter stringptr into the table and then we alias it.
void alias_map(const char *stringptr, const int image, struct ParserState* ps) {
  for (struct hash_type *q = ps->hash_table[hash(stringptr)]; q != NULL; q = q->link) {
    if (EQUAL_STRING(stringptr, q, ps->string_table)) {
      q->number = -image; /* Mark alias of image */
      return;
    }
  }
  assign_symbol_no(stringptr, image, ps);
}

/// SYMBOL_IMAGE takes as argument a symbol. It searches for that symbol
/// in the HASH_TABLE, and if found, it returns its image; otherwise, it
/// returns OMEGA.
int symbol_image(const char *item, const struct ParserState* ps) {
  for (const struct hash_type *q = ps->hash_table[hash(item)]; q != NULL; q = q->link) {
    if (EQUAL_STRING(item, q, ps->string_table)) {
      return ABS(q->number);
    }
  }
  return OMEGA;
}

/// NAME_MAP takes as input a symbol and inserts it into the HASH_TABLE if it
/// is not yet in the table. If it was already in the table then it is
/// assigned a NAME_INDEX number if it did not yet have one.  The name index
/// assigned is returned.
int name_map(const char *symb, struct ParserState* ps) {
  struct hash_type *p;
  const int i = hash(symb);
  for (p = ps->hash_table[i]; p != NULL; p = p->link) {
    if (EQUAL_STRING(symb, p, ps->string_table)) {
      if (p->name_index != OMEGA) {
        return p->name_index;
      } else {
        ps->num_names++;
        p->name_index = ps->num_names;
        return ps->num_names;
      }
    }
  }
  talloc0p(&p, struct hash_type);
  p->number = 0;
  insert_string(p, symb, &ps->string_offset, &ps->string_table);
  p->link = ps->hash_table[i];
  ps->hash_table[i] = p;
  ps->num_names++;
  p->name_index = ps->num_names;
  return ps->num_names;
}

/// BUILD_SYMNO constructs the SYMNO table which is a mapping from each
/// symbol number into that symbol.
void build_symno(struct ParserState* ps) {
  const long symno_size = ps->num_symbols + 1;
  calloc0p(&ps->symno, symno_size, struct symno_type);
  // Go through entire hash table. For each non_empty bucket, go through
  // linked list in that bucket.
  for (int i = 0; i < HT_SIZE; ++i) {
    for (const struct hash_type *p = ps->hash_table[i]; p != NULL; p = p->link) {
      const long symbol = p->number;
      // Not an alias
      if (symbol >= 0) {
        ps->symno[symbol].name_index = OMEGA;
        ps->symno[symbol].ptr = p->st_ptr;
      }
    }
  }
}

char *RETRIEVE_STRING(const int indx, char *string_table, const struct symno_type *symno) {
  return &string_table[symno[indx].ptr];
}

#define SYM1 (ps->terminal[ps->stack_top + 1])
#define SYM2 (ps->terminal[ps->stack_top + 2])
#define SYM3 (ps->terminal[ps->stack_top + 3])

enum {
    NT_OFFSET = 19,
    BUFF_UBOUND = 30,
    BUFF_SIZE = 31,
    STACK_UBOUND = 20,
    STACK_SIZE = 21,
    LA_STATE_OFFSET = 392,
    MAX_LA = 1,
    NUM_RULES = 141,
    NUM_TERMINALS = 19,
    NUM_NON_TERMINALS = 38,
    NUM_SYMBOLS = 57,
    START_STATE = 144,
    EOFT_SYMBOL = 19,
    EOLT_SYMBOL = 20,
    ACCEPT_ACTION = 250,
    ERROR_ACTION = 251
  };

enum {
    DEFINE_KEY_TK = 5,
    TERMINALS_KEY_TK = 9,
    ALIAS_KEY_TK = 10,
    START_KEY_TK = 11,
    RULES_KEY_TK = 12,
    NAMES_KEY_TK = 16,
    END_KEY_TK = 18,
    EQUIVALENCE_TK = 1,
    ARROW_TK = 2,
    OR_TK = 6,
    EMPTY_SYMBOL_TK = 7,
    ERROR_SYMBOL_TK = 8,
    EOL_SYMBOL_TK = 13,
    EOF_SYMBOL_TK = 14,
    MACRO_NAME_TK = 15,
    SYMBOL_TK = 3,
    BLOCK_TK = 4,
    HBLOCK_TK = 17,
    EOF_TK = 19
  };

#define nt_action(state, sym) base_action[state + sym]

#define t_action(state, sym, next_tok) \
  term_action[term_check[base_action[state]+sym] == sym ? \
  base_action[state] + sym : base_action[state]]

const unsigned char rhs[] = {
  0,
  7, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  2, 1, 1, 1, 1, 1, 1, 2, 3, 1, 2, 3, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1,
  1, 3, 2, 3, 2, 2, 2, 2, 1, 1, 1, 1, 3, 3, 3,
  3, 1, 1, 1, 1, 1, 1, 1, 2, 3, 3, 3, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0,
  1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 2, 0, 2,
  0, 2, 0, 2, 0, 2
};

const unsigned short lhs[] = {
  0,
  9, 9, 17, 17, 17, 17, 17, 17, 17, 17, 18, 18, 19, 19, 5,
  5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6,
  20, 22, 22, 22, 22, 22, 22, 23, 25, 25, 25, 25, 26, 26, 26,
  26, 27, 27, 27, 27, 27, 27, 27, 3, 3, 3, 3, 28, 28, 28,
  28, 29, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 32, 32, 1,
  1, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33,
  33, 7, 7, 2, 2, 2, 2, 2, 35, 37, 37, 37, 4, 4, 4,
  4, 4, 4, 4, 8, 8, 8, 8, 8, 8, 8, 8, 10, 10, 11,
  11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 21, 21, 30, 30,
  24, 24, 36, 36, 34, 34,
  54, 60, 129, 58, 175, 1, 109, 69, 53, 249,
  42, 146, 188, 247, 171, 109, 145, 20, 101, 2,
  119, 172, 81, 251, 87, 137, 210, 251, 40, 251,
  35, 27, 29, 251, 39, 191, 13, 35, 27, 29,
  18, 109, 139, 14, 100, 199, 193, 170, 102, 161,
  20, 186, 34, 203, 179, 218, 221, 189, 195, 187,
  50, 99, 67, 49, 80, 204, 121, 202, 196, 149,
  123, 41, 97, 133, 125, 207, 111, 112, 231, 48,
  251, 1, 251, 235, 193, 127, 141, 251, 142, 94,
  91, 251, 135, 124, 89, 129, 138, 90, 141, 79,
  154, 88, 203, 94, 77, 94, 156, 94, 182, 251,
  251, 251, 251, 147
};

const unsigned short *base_action = lhs;


const unsigned char term_check[] = {
  0,
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
  15, 16, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
  13, 14, 15, 16, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  11, 12, 0, 0, 0, 3, 4, 18, 0, 1, 2, 3, 4, 5, 6,
  7, 8, 9, 10, 17, 16, 13, 14, 0, 1, 2, 3, 4, 5, 6,
  7, 8, 9, 10, 0, 0, 13, 14, 3, 4, 5, 6, 7, 8, 9,
  10, 11, 12, 0, 1, 2, 0, 17, 0, 1, 2, 3, 4, 5, 6,
  7, 8, 9, 10, 11, 0, 0, 0, 3, 4, 5, 6, 7, 8, 9,
  10, 11, 12, 0, 1, 2, 0, 17, 0, 1, 2, 3, 4, 5, 6,
  7, 8, 0, 1, 2, 0, 1, 2, 15, 0, 1, 2, 3, 4, 5,
  6, 7, 8, 0, 1, 2, 0, 0, 0, 15, 0, 1, 2, 3, 4,
  5, 6, 7, 8, 0, 0, 1, 2, 3, 0, 15, 6, 7, 8, 10,
  0, 0, 0, 13, 14, 0, 1, 2, 3, 4, 5, 6, 0, 19, 9,
  0, 1, 2, 0, 4, 5, 9, 0, 0, 9, 10, 0, 0, 0, 11,
  0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 18, 0
};

const unsigned short term_action[] = {
  0,
  99, 326, 327, 354, 367, 361, 359, 355, 356, 362,
  363, 364, 365, 357, 358, 368, 366, 251, 326, 327,
  354, 367, 361, 359, 355, 356, 362, 363, 364, 365,
  357, 358, 368, 366, 251, 326, 327, 279, 274, 345,
  275, 276, 277, 346, 347, 348, 349, 251, 132, 128,
  245, 343, 281, 38, 326, 327, 294, 308, 305, 312,
  310, 295, 306, 307, 344, 219, 296, 297, 251, 326,
  327, 298, 308, 305, 303, 302, 299, 306, 307, 136,
  73, 300, 301, 140, 140, 345, 335, 336, 337, 346,
  347, 348, 349, 251, 326, 327, 134, 140, 62, 326,
  327, 314, 319, 320, 315, 316, 317, 321, 322, 323,
  74, 138, 251, 240, 343, 345, 242, 238, 333, 346,
  347, 348, 349, 81, 326, 327, 251, 344, 118, 254,
  255, 260, 261, 158, 256, 257, 258, 80, 326, 327,
  83, 326, 327, 259, 11, 326, 327, 267, 272, 273,
  268, 269, 270, 78, 326, 327, 251, 251, 251, 266,
  12, 326, 327, 267, 272, 273, 268, 269, 270, 122,
  251, 326, 327, 354, 251, 266, 359, 355, 356, 206,
  251, 251, 251, 357, 358, 31, 326, 327, 283, 288,
  286, 284, 120, 250, 287, 251, 326, 327, 124, 308,
  305, 205, 126, 130, 306, 307, 251, 251, 251, 214,
  251, 251, 251, 251, 164, 251, 251, 251, 251, 251,
  251, 382
};

static void null_action(struct ParserState* ps)
{
}

static void add_macro_definition(const char *name, const struct terminal_type *term, struct ParserState* ps)
{
    if (ps->num_defs >= (int)ps->defelmt_size)
    {
        ps->defelmt_size += DEFELMT_INCREMENT;
        ps->defelmt = (struct defelmt_type *)
            (ps->defelmt == (struct defelmt_type *) NULL
             ? malloc(ps->defelmt_size * sizeof(struct defelmt_type))
             : realloc(ps->defelmt, ps->defelmt_size * sizeof(struct defelmt_type)));
        if (ps->defelmt == (struct defelmt_type *) NULL)
            nospace();
    }

    ps->defelmt[ps->num_defs].length       = term->length;
    ps->defelmt[ps->num_defs].start_line   = term->start_line;
    ps->defelmt[ps->num_defs].start_column = term->start_column;
    ps->defelmt[ps->num_defs].end_line     = term->end_line;
    ps->defelmt[ps->num_defs].end_column   = term->end_column;
    strcpy(ps->defelmt[ps->num_defs].name, name);
    ps->num_defs++;
}

static void add_block_definition(const struct terminal_type *term, struct ParserState* ps)
{
    if (ps->num_acts >= (int) ps->actelmt_size)
    {
        ps->actelmt_size += ACTELMT_INCREMENT;
        ps->actelmt = (struct actelmt_type *)
            (ps->actelmt == (struct actelmt_type *) NULL
             ? malloc(ps->actelmt_size * sizeof(struct actelmt_type))
             : realloc(ps->actelmt, ps->actelmt_size * sizeof(struct actelmt_type)));
        if (ps->actelmt == (struct actelmt_type *) NULL)
            nospace();
    }

    ps->actelmt[ps->num_acts].rule_number  = ps->num_rules;
    ps->actelmt[ps->num_acts].start_line   = term->start_line;
    ps->actelmt[ps->num_acts].start_column = term->start_column;
    ps->actelmt[ps->num_acts].end_line     = term->end_line;
    ps->actelmt[ps->num_acts].end_column   = term->end_column;
    ps->actelmt[ps->num_acts].header_block = term->kind == HBLOCK_TK;
    ps->num_acts++;
}

/// bad_symbol ::= EQUIVALENCE
static void bad_first_symbol(struct ParserState* ps)
{
    PRNTERR2("First symbol: \"%s\" found in file is illegal. Line %ld, column %d", SYM1.name, SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_symbol ::= BLOCK
static void act10(struct ParserState* ps)
{
    PRNTERR2("Action block cannot be first object in file. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_list ::= macro_name_symbol macro_block
static void act13(struct ParserState* ps)
{
    add_macro_definition(SYM1.name, &(SYM2), ps);
}

/// macro_list ::= macro_list macro_name_symbol macro_block
static void act14(struct ParserState* ps)
{
    add_macro_definition(SYM2.name, &(SYM3), ps);
}

/// macro_name_symbol ::= SYMBOL
static void act16(struct ParserState* ps)
{
    PRNTWNG2("Macro name \"%s\" does not start with the escape character. Line %ld, column %d", SYM1.name, SYM1.start_line, SYM1.start_column);
}

/// macro_name_symbol ::= OR
static void bad_macro_name(struct ParserState* ps)
{
    PRNTERR2("Reserved symbol cannot be used as macro name. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_name_symbol ::= BLOCK
static void act21(struct ParserState* ps)
{
    PRNTERR2("Macro name not supplied for macro definition. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_name_symbol ::= DEFINE_KEY
static void act22(struct ParserState* ps)
{
    PRNTERR2("Macro keyword misplaced. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_block ::= OR
static void definition_expected(struct ParserState* ps)
{
    PRNTERR2("Definition block expected where symbol found. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// terminal_symbol ::= SYMBOL
static void process_terminal(struct ParserState* ps)
{
    assign_symbol_no(SYM1.name, OMEGA, ps);
}

/// terminal_symbol ::= DEFINE_KEY
static void bad_terminal(struct ParserState* ps)
{
    PRNTERR2("Keyword  has been misplaced in Terminal section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// terminal_symbol ::= BLOCK
static void act37(struct ParserState* ps)
{
    PRNTERR2("Misplaced block found in TERMINALS section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// alias_definition ::= alias_lhs produces alias_rhs
static void act39(struct ParserState* ps)
{
    int image;
    char tok_string[SYMBOL_SIZE + 1];

    switch(SYM3.kind)
    {
        case EMPTY_SYMBOL_TK:
            image = empty;
            break;

        case SYMBOL_TK:
            assign_symbol_no(SYM3.name, OMEGA, ps);
            image = symbol_image(SYM3.name, ps);
            break;

        case ERROR_SYMBOL_TK:
            if (error_image > ps->num_terminals)
            {
                restore_symbol(tok_string, kerror, ps->ormark, ps->escape);
                PRNTERR2("Illegal aliasing to %s prior to its definition.  Line %ld, column %d", tok_string, SYM3.start_line, SYM3.start_column);
                exit(12);
            }
            image = error_image;
            break;

        case EOF_SYMBOL_TK:
            if (eoft_image > ps->num_terminals)
            {
                restore_symbol(tok_string, keoft, ps->ormark, ps->escape);
                PRNTERR2("Illegal aliasing to %s prior to its definition. Line %ld, column %d", tok_string, SYM3.start_line, SYM3.start_column);
                exit(12);
            }
            image = eoft_image;
            break;

        case EOL_SYMBOL_TK:
            if (eolt_image == OMEGA)
            {
                PRNTERR2("Illegal aliasing to EOL prior to its definition. Line %ld, column %d", SYM3.start_line, SYM3.start_column);
                exit(12);
            }
            image = eolt_image;
            break;

        default: /* if SYM3.kind == symbol */
            image = symbol_image(SYM3.name, ps);
            break;
    }

    switch(SYM1.kind)
    {
        case SYMBOL_TK:
            if (symbol_image(SYM1.name, ps) != OMEGA)
            {
                restore_symbol(tok_string, SYM1.name, ps->ormark, ps->escape);
                PRNTERR2("Symbol %s was previously defined. Line %ld, column %d", tok_string, SYM1.start_line, SYM1.start_column);
                exit(12);
            }
            assign_symbol_no(SYM1.name, image, ps);
            break;

        case ERROR_SYMBOL_TK:
            if (error_image > ps->num_terminals || ! ps->error_maps_bit)
            {
                if (image == empty      || image == eolt_image ||
                    image == eoft_image || image > ps->num_terminals)
                {
                    restore_symbol(tok_string, kerror, ps->ormark, ps->escape);
                    PRNTERR2("Illegal alias for symbol %s. Line %ld, column %d.", tok_string, SYM1.start_line, SYM1.start_column);
                    exit(12);
                }
                alias_map(kerror, image, ps);
                error_image = image;
            }
            else
            {
                restore_symbol(tok_string, kerror, ps->ormark, ps->escape);
                PRNTERR2("Symbol %s was previously defined. Line %ld, column %d", tok_string, SYM1.start_line, SYM1.start_column);
                exit(12);
            }
            break;

        case EOF_SYMBOL_TK:
            if (eoft_image > ps->num_terminals)
            {
                if (image == empty       || image == eolt_image  ||
                    image == error_image || image > ps->num_terminals)
                {
                    restore_symbol(tok_string, keoft, ps->ormark, ps->escape);
                    PRNTERR2("Illegal alias for symbol %s. Line %ld, column %d.", tok_string, SYM1.start_line, SYM1.start_column);
                    exit(12);
                }
                alias_map(keoft, image, ps);
                eoft_image = image;
            }
            else
            {
                restore_symbol(tok_string, keoft, ps->ormark, ps->escape);
                PRNTERR2("Symbol %s was previously defined.  %ld, column %d", tok_string, SYM1.start_line, SYM1.start_column);
                exit(12);
            }
            break;

        default: /* if SYM1.kind == EOL_SYMBOL */
            if (eolt_image == OMEGA)
            {
                if (image == empty ||
                    image == eoft_image ||
                    image == error_image ||
                    image > ps->num_terminals)
                {
                    PRNTERR2("Illegal alias for symbol EOL. Line %ld, column %d.", SYM1.start_line, SYM1.start_column);
                    exit(12);
                }
                eolt_image = image;
            }
            else
            {
                PRNTERR2("Symbol EOL was previously defined. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
                exit(12);
            }
            break;
    }
}

/// bad_alias_rhs ::= DEFINE_KEY
static void bad_alias_rhs(struct ParserState* ps)
{
    PRNTERR2("Misplaced keyword found in Alias section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_alias_rhs ::= BLOCK
static void act57(struct ParserState* ps)
{
    PRNTERR2("Misplaced block found in Alias section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_alias_lhs ::= EMPTY_SYMBOL
static void act59(struct ParserState* ps)
{
    PRNTERR2("Empty symbol cannot be aliased. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_alias_lhs ::= produces
static void missing_quote(struct ParserState* ps)
{
    PRNTERR2("Symbol must be quoted when used as a grammar symbol. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// start_symbol ::= SYMBOL
static void act63(struct ParserState* ps)
{
    assign_symbol_no(SYM1.name, OMEGA, ps);
    struct node *q = Allocate_node();
    q -> value = symbol_image(SYM1.name, ps);
    if (ps->start_symbol_root == NULL) {
      q -> next = q;
    } else {
        q -> next = ps->start_symbol_root -> next;
        ps->start_symbol_root -> next = q;
    }
    ps->start_symbol_root = q;
    ps->num_rules++;
    ps->num_items++;
}

/// start_symbol ::= OR
static void bad_start_symbol(struct ParserState* ps)
{
    PRNTERR2("Symbol cannot be used as Start symbol. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// start_symbol ::= BLOCK
static void act68(struct ParserState* ps)
{
    PRNTERR2("Misplaced block found in Start section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// start_symbol ::= DEFINE_KEY
static void misplaced_keyword_found_in_START_section(struct ParserState* ps)
{
    PRNTERR2("Misplaced keyword found in START section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// rules_block ::= RULES_KEY
static void act73(struct ParserState* ps)
{

    if (ps->start_symbol_root == NULL)
    {
        struct node *q = Allocate_node();
        q -> value = empty;
        q -> next = q;
        ps->start_symbol_root = q;
        ps->num_rules = 0;                 // One rule
        ps->num_items = 0;                 // 0 items
    }
    build_symno(ps);
}

/// rules_block ::= RULES_KEY rule_list
static void act74(struct ParserState* ps)
{
    build_symno(ps);
}

/// rule_list ::= {action_block} SYMBOL produces
static void act77(struct ParserState* ps)
{
    assign_symbol_no(SYM2.name, OMEGA, ps);
    if (ps->start_symbol_root == NULL)
    {
        struct node *q = Allocate_node();
        q -> value = symbol_image(SYM2.name, ps);
        q -> next = q;
        ps->start_symbol_root = q;
        ps->num_rules = 1;
        ps->num_items = 1;
    }

/// Since we don't know for sure how many start symbols we have, a
/// "while" loop is used to increment the size of rulehdr. However,
/// it is highly unlikely that this loop would ever execute more than
/// once if the size of RULE_INCREMENT is reasonable.
    while (ps->num_rules >= (int)ps->rulehdr_size)
    {
        ps->rulehdr_size += RULEHDR_INCREMENT;
        ps->rulehdr = (struct rulehdr_type *)
            (ps->rulehdr == (struct rulehdr_type *) NULL
             ? malloc(ps->rulehdr_size * sizeof(struct rulehdr_type))
             : realloc(ps->rulehdr, ps->rulehdr_size * sizeof(struct rulehdr_type)));
        if (ps->rulehdr == (struct rulehdr_type *) NULL)
            nospace();
    }
    ps->rulehdr[ps->num_rules].sp = ((SYM3.kind == ARROW_TK) ? true : false);
    ps->rulehdr[ps->num_rules].lhs = symbol_image(SYM2.name, ps);
    ps->rulehdr[ps->num_rules].rhs_root = NULL;
}

/// rule_list ::= rule_list OR
static void act78(struct ParserState* ps)
{
    ps->num_rules++;
    if (ps->num_rules >= (int)ps->rulehdr_size)
    {
        ps->rulehdr_size += RULEHDR_INCREMENT;
        ps->rulehdr = (struct rulehdr_type *)
            (ps->rulehdr == (struct rulehdr_type *) NULL
             ? malloc(ps->rulehdr_size * sizeof(struct rulehdr_type))
             : realloc(ps->rulehdr, ps->rulehdr_size * sizeof(struct rulehdr_type)));
        if (ps->rulehdr == (struct rulehdr_type *) NULL)
            nospace();
    }
    ps->rulehdr[ps->num_rules].sp = ps->rulehdr[ps->num_rules - 1].sp;
    ps->rulehdr[ps->num_rules].lhs = OMEGA;
    ps->rulehdr[ps->num_rules].rhs_root = NULL;
}

/// rule_list ::= rule_list SYMBOL produces
static void act79(struct ParserState* ps)
{
    ps->num_rules++;
    if (ps->num_rules >= (int)ps->rulehdr_size)
    {
        ps->rulehdr_size += RULEHDR_INCREMENT;
        ps->rulehdr = (struct rulehdr_type *)
            (ps->rulehdr == (struct rulehdr_type *) NULL
             ? malloc(ps->rulehdr_size * sizeof(struct rulehdr_type))
             : realloc(ps->rulehdr, ps->rulehdr_size * sizeof(struct rulehdr_type)));
        if (ps->rulehdr == (struct rulehdr_type *) NULL)
            nospace();
    }
    ps->rulehdr[ps->num_rules].sp = ((SYM3.kind == ARROW_TK) ? true : false);
    assign_symbol_no(SYM2.name, OMEGA, ps);
    ps->rulehdr[ps->num_rules].lhs = symbol_image(SYM2.name, ps);
    ps->rulehdr[ps->num_rules].rhs_root = NULL;
}

/// rule_list ::= rule_list ERROR_SYMBOL
static void act82(struct ParserState* ps)
{
    if (error_image == DEFAULT_SYMBOL)
    {
        char tok_string[SYMBOL_SIZE + 1];
        restore_symbol(tok_string, kerror, ps->ormark, ps->escape);
        PRNTERR2("%s not declared or aliased to terminal symbol. Line %ld, column %d", tok_string, SYM2.start_line, SYM2.start_column);
        exit(12);
    }
    struct node *q = Allocate_node();
    q -> value = error_image;
    ps->num_items++;
    if (ps->rulehdr[ps->num_rules].rhs_root == NULL)
        q -> next = q;
    else
    {
        q -> next = ps->rulehdr[ps->num_rules].rhs_root -> next;
         ps->rulehdr[ps->num_rules].rhs_root -> next = q;
    }
    ps->rulehdr[ps->num_rules].rhs_root = q;
}

/// rule_list ::= rule_list SYMBOL
static void act83(struct ParserState* ps)
{
    assign_symbol_no(SYM2.name, OMEGA, ps);
    int sym = symbol_image(SYM2.name, ps);
    if (sym != empty)
    {
        if (sym == eoft_image)
        {
            PRNTERR2("End-of-file symbol cannot be used in rule. Line %ld, column %d", SYM2.start_line, SYM2.start_column);
            exit(12);
        }
        struct node *q = Allocate_node();
        q -> value = sym;
        ps->num_items++;
        if (ps->rulehdr[ps->num_rules].rhs_root == NULL)
            q -> next = q;
        else
        {
            q -> next = ps->rulehdr[ps->num_rules].rhs_root -> next;
            ps->rulehdr[ps->num_rules].rhs_root -> next = q;
        }
        ps->rulehdr[ps->num_rules].rhs_root = q;
    }
}

/// rule_list ::= OR
static void bad_first_symbol_in_RULES_section(struct ParserState* ps)
{
    PRNTERR2("First symbol in Rules section is not a valid left-hand side.\n Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// rule_list ::= rule_list OR produces
static void rule_without_left_hand_side(struct ParserState* ps)
{
    PRNTERR2("Rule without left-hand-side.  Line %ld, column %d", SYM3.start_line, SYM3.start_column);
    exit(12);
}

/// rule_list ::= rule_list keyword produces
static void act91(struct ParserState* ps)
{
    PRNTWNG2("Misplaced keyword found in Rules section Line %ld, column %d",  SYM2.start_line, SYM2.start_column);
    exit(12);
}

/// action_block ::= BLOCK
static void act92(struct ParserState* ps)
{
    add_block_definition(&(SYM1), ps);
}

/// action_block ::= HBLOCK
static void act93(struct ParserState* ps)
{
    add_block_definition(&(SYM1), ps);
}

/// keyword ::= DEFINE_KEY
static void misplaced_keyword_found_in_RULES_section(struct ParserState* ps)
{
    PRNTWNG2("Misplaced keyword found in RULES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// names_definition ::= name produces name
static void act100(struct ParserState* ps)
{
    if (ps->error_maps_bit)
    {
        int symbol;

        switch(SYM1.kind)
        {
            case EMPTY_SYMBOL_TK:
                symbol = empty;
                break;

            case ERROR_SYMBOL_TK:
                symbol = error_image;
                break;

            case EOL_SYMBOL_TK:
                symbol = eolt_image;
                break;

            case EOF_SYMBOL_TK:
                symbol = eoft_image;
                break;

            default:
                symbol = symbol_image(SYM1.name, ps);
                break;
        }

        if (symbol == OMEGA)
        {
            PRNTERR2("Symbol %s is undefined. Line %ld, column %d", SYM1.name, SYM1.start_line, SYM1.start_column);
            exit(12);
        }

        if (ps->symno[symbol].name_index != OMEGA)
        {
            PRNTERR2("Symbol %s has been named more than once. Line %ld, column %d.", SYM1.name, SYM1.start_line, SYM1.start_column);
            exit(12);
        }
         ps->symno[symbol].name_index = name_map(SYM3.name, ps);
     }
}

/// bad_name ::= DEFINE_KEY
static void misplaced_keyword_found_in_NAMES_section(struct ParserState* ps)
{
    PRNTERR2("Keyword  has been misplaced in NAMES section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_name ::= BLOCK
static void act116(struct ParserState* ps)
{
    PRNTERR2("Misplaced action block found in NAMES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_name ::= MACRO_NAME
static void act117(struct ParserState* ps)
{
    PRNTERR2("Misplaced macro name found in NAMES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// [terminals_block] ::=
static void process_TERMINALS_section(struct ParserState* ps)
{
    ps->num_terminals = ps->num_symbols;
    assign_symbol_no(keoft, OMEGA, ps);
    eoft_image = symbol_image(keoft, ps);
    if (ps->error_maps_bit) {
        assign_symbol_no(kerror, OMEGA, ps);
        error_image = symbol_image(kerror, ps);
    } else {
      error_image = DEFAULT_SYMBOL;   // should be 0
    }
    assign_symbol_no(kaccept, OMEGA, ps);
    accept_image = symbol_image(kaccept, ps);
}

/// [alias_block] ::=
static void process_ALIAS_section(struct ParserState* ps)
{
    int k = 0;
    if (eoft_image <= ps->num_terminals) {
        k++;
    } else {
        ps->num_terminals++;
    }
    if (ps->error_maps_bit) {
        if (error_image <= ps->num_terminals) {
            k++;
        } else {
            ps->num_terminals++;
            if (k == 1) {
                error_image--;
            }
        }
    }

    if (k > 0) {
        for (int i = 0; i < HT_SIZE; i++) {
            struct hash_type* p = ps->hash_table[i];
            while(p != NULL)
            {
                if (p -> number > ps->num_terminals)
                    p -> number -= k;
                else if (p -> number < -ps->num_terminals)
                    p -> number += k;
                p = p -> link;
            }
        }
        ps->num_symbols -= k;
        accept_image -= k;
    }
    if (eolt_image == OMEGA)
        eolt_image = eoft_image;
    if (error_image == DEFAULT_SYMBOL)
        alias_map(kerror, DEFAULT_SYMBOL, ps);
}

/// {terminal_symbol} ::=
static void act132(struct ParserState* ps)
{
    assign_symbol_no(kempty, OMEGA, ps);
    empty = symbol_image(kempty, ps);
}

static void (*rule_action[]) (struct ParserState* ps) = {NULL,
     null_action, /* 1 */
     null_action, /* 2 */
     bad_first_symbol, /* 3 */
     bad_first_symbol, /* 4 */
     bad_first_symbol, /* 5 */
     bad_first_symbol, /* 6 */
     bad_first_symbol, /* 7 */
     bad_first_symbol, /* 8 */
     bad_first_symbol, /* 9 */
     act10, /* 10 */
     null_action, /* 11 */
     null_action, /* 12 */
     act13, /* 13 */
     act14, /* 14 */
     null_action, /* 15 */
     act16, /* 16 */
     bad_macro_name, /* 17 */
     bad_macro_name, /* 18 */
     bad_macro_name, /* 19 */
     bad_macro_name, /* 20 */
     act21, /* 21 */
     act22, /* 22 */
     null_action, /* 23 */
     definition_expected, /* 24 */
     definition_expected, /* 25 */
     definition_expected, /* 26 */
     definition_expected, /* 27 */
     definition_expected, /* 28 */
     definition_expected, /* 29 */
     definition_expected, /* 30 */
     null_action, /* 31 */
     process_terminal, /* 32 */
     process_terminal, /* 33 */
     process_terminal, /* 34 */
     bad_terminal, /* 35 */
     bad_terminal, /* 36 */
     act37, /* 37 */
     null_action, /* 38 */
     act39, /* 39 */
     null_action, /* 40 */
     null_action, /* 41 */
     null_action, /* 42 */
     null_action, /* 43 */
     null_action, /* 44 */
     null_action, /* 45 */
     null_action, /* 46 */
     null_action, /* 47 */
     null_action, /* 48 */
     null_action, /* 49 */
     null_action, /* 50 */
     null_action, /* 51 */
     null_action, /* 52 */
     null_action, /* 53 */
     bad_alias_rhs, /* 54 */
     bad_alias_rhs, /* 55 */
     bad_alias_rhs, /* 56 */
     act57, /* 57 */
     null_action, /* 58 */
     act59, /* 59 */
     missing_quote, /* 60 */
     missing_quote, /* 61 */
     null_action, /* 62 */
     act63, /* 63 */
     bad_start_symbol, /* 64 */
     bad_start_symbol, /* 65 */
     bad_start_symbol, /* 66 */
     bad_start_symbol, /* 67 */
     act68, /* 68 */
     misplaced_keyword_found_in_START_section, /* 69 */
     misplaced_keyword_found_in_START_section, /* 70 */
     misplaced_keyword_found_in_START_section, /* 71 */
     misplaced_keyword_found_in_START_section, /* 72 */
     act73, /* 73 */
     act74, /* 74 */
     null_action, /* 75 */
     null_action, /* 76 */
     act77, /* 77 */
     act78, /* 78 */
     act79, /* 79 */
     null_action, /* 80 */
     null_action, /* 81 */
     act82, /* 82 */
     act83, /* 83 */
     bad_first_symbol_in_RULES_section, /* 84 */
     bad_first_symbol_in_RULES_section, /* 85 */
     bad_first_symbol_in_RULES_section, /* 86 */
     bad_first_symbol_in_RULES_section, /* 87 */
     rule_without_left_hand_side, /* 88 */
     rule_without_left_hand_side, /* 89 */
     rule_without_left_hand_side, /* 90 */
     act91, /* 91 */
     act92, /* 92 */
     act93, /* 93 */
     misplaced_keyword_found_in_RULES_section, /* 94 */
     misplaced_keyword_found_in_RULES_section, /* 95 */
     misplaced_keyword_found_in_RULES_section, /* 96 */
     misplaced_keyword_found_in_RULES_section, /* 97 */
     misplaced_keyword_found_in_RULES_section, /* 98 */
     null_action, /* 99 */
     act100, /* 100 */
     null_action, /* 101 */
     null_action, /* 102 */
     null_action, /* 103 */
     null_action, /* 104 */
     null_action, /* 105 */
     null_action, /* 106 */
     null_action, /* 107 */
     null_action, /* 108 */
     null_action, /* 109 */
     misplaced_keyword_found_in_NAMES_section, /* 110 */
     misplaced_keyword_found_in_NAMES_section, /* 111 */
     misplaced_keyword_found_in_NAMES_section, /* 112 */
     misplaced_keyword_found_in_NAMES_section, /* 113 */
     misplaced_keyword_found_in_NAMES_section, /* 114 */
     misplaced_keyword_found_in_NAMES_section, /* 115 */
     act116, /* 116 */
     act117, /* 117 */
     null_action, /* 118 */
     null_action, /* 119 */
     process_TERMINALS_section, /* 120 */
     process_TERMINALS_section, /* 121 */
     process_ALIAS_section, /* 122 */
     process_ALIAS_section, /* 123 */
     null_action, /* 124 */
     null_action, /* 125 */
     null_action, /* 126 */
     null_action, /* 127 */
     null_action, /* 128 */
     null_action, /* 129 */
     null_action, /* 130 */
     null_action, /* 131 */
     act132, /* 132 */
     null_action, /* 133 */
     null_action, /* 134 */
     null_action, /* 135 */
     null_action, /* 136 */
     null_action, /* 137 */
     null_action, /* 138 */
     null_action, /* 139 */
     null_action, /* 140 */
     null_action, /* 141 */
     NULL};
// endregion















// region manual parser
struct line_elemt {
  struct line_elemt *link;
  char line[MAX_LINE_SIZE + 1];
};

struct LinePool {
  struct line_elemt *line_pool_root;
};

/// READ_INPUT fills the buffer from p1 to the end.
static void read_input(char *grm_file, FILE *sysgrm, struct ScannerState* ss) {
  unsigned long num_read = ss->input_buffer + IOBUFFER_SIZE - ss->bufend;
  if ((num_read = fread(ss->bufend, 1, num_read, sysgrm)) == 0) {
    if (ferror(sysgrm) != 0) {
      fprintf(stderr, "*** Error reading input file \"%s\".\n", grm_file);
      exit(12);
    }
  }
  ss->bufend += num_read;
  *ss->bufend = '\0';
}

/// VERIFY takes as argument a character string and checks whether each
/// character is a digit. If all are digits, then 1 is returned; if not, then
/// 0 is returned.
static bool verify_is_digit(const char *item) {
  while (isdigit(*item)) {
    item++;
  }
  return *item == '\0';
}

/// TRANSLATE takes as arguments a character array, which it folds to upper
/// to uppercase and returns.
static char *translate(char *str, const int len) {
  for (int i = 0; i < len; i++) {
    str[i] = TOUPPER(str[i]);
  }
  return str;
}

/// Compare two character strings s1 and s2 to check whether s2
/// is a substring of s1. The string s2 is assumed to be in lowercase
/// and NULL terminated. However, s1 does not have to (indeed, may not)
/// be NULL terminated.
///
/// The test below may look awkward. For example, why not use:
///                  if (tolower(s1[i]) != s2[i])  ?
/// because tolower(ch) is sometimes implemented as (ch-'A'+'a') which
/// does not work when "ch" is already a lower case character.
static bool strxeq(char *s1, char *s2) {
  for (; *s2 != '\0'; s1++, s2++) {
    if (*s1 != *s2 && *s1 != toupper(*s2)) {
      return false;
    }
  }
  return true;
}

/// OPTION handles the decoding of options passed by the user and resets
/// them appropriately. "options" may be called twice: when a parameter line
/// is passed to the main program and when the user codes an %OPTIONS line in
/// his grammar.
/// Basically, there are two kinds of options: switches which indicate a
/// certain setting just by their appearance, and valued options which are
/// followed by an equal sign and the value to be assigned to them.
static void options(char *file_prefix, struct CLIOptions *cli_options, char *parm) {
  char token[MAX_PARM_SIZE + 1];
  char temp[MAX_PARM_SIZE + 1];
  char *c;
  // If we scan the comment sign, we stop processing the rest of the
  // parameter string.
  for (c = parm; *c != '\0'; c++) {
    if (*c == '-' && *(c + 1) == '-') {
      break;
    }
  }
  *c = '\0';
  int i = 0;
  while (parm[i] != '\0' && /* Clean front of string */ (parm[i] == ',' || parm[i] == '/' || parm[i] == ' ')) {
    i++;
  }
  while (parm[i] != '\0') {
    // Repeat until parm line is exhausted
    // Remove garbage in front
    memmove(parm, parm + i, strlen (parm + i) + 1);
    i = 0;
    while (parm[i] != '\0' && /* Search for delimiter */ (parm[i] != ',' && parm[i] != '/' && parm[i] != '=' && parm[i] != ' ')) {
      i++;
    }
    for (int j = 0; j < i; j++) {
      // Fold actual parameter
      token[j] = TOUPPER(parm[j]);
      temp[j] = parm[j];
    }
    token[i] = '\0';
    temp[i] = '\0';
    // find first non-blank after parm
    while (parm[i] != '\0' && parm[i] == ' ') {
      i++;
    }
    char delim;
    if (parm[i] != '\0') {
      delim = parm[i]; /* not end of parameter line */
    } else {
      delim = ' ';
    }
    unsigned long token_len = strlen(token);
    if (token_len > MAX_PARM_SIZE) {
      token[MAX_PARM_SIZE] = '\0';
    }
    // We check whether we have a switch or a value parameter.
    // Each category is checked separately.  A match is made whenever
    // a minimum unambiguous prefix of the token in question matches an
    // option...
    //
    // At this stage, TEMP contains the value of the switch as specified
    // and TOKEN contains the upper-case folded value of TEMP.
    // if switch parameter then process
    if (delim != '=') {
      bool flag;
      if (memcmp(token, "NO", 2) == 0) {
        flag = false;
        token_len = token_len - 2;
        memmove(token, token + 2, strlen(token + 2) + 1); /* get rid of "NO" prefix */
      } else {
        flag = true;
      }
      if (memcmp("BYTE", token, token_len) == 0) {
        cli_options->byte_bit = flag;
      } else if (memcmp("CONFLICTS", token, token_len) == 0) {
        cli_options->conflicts_bit = flag;
      } else if (memcmp("GOTODEFAULT", token, token_len) == 0) {
        cli_options->goto_default_bit = flag;
      } else if (memcmp("HALFWORD", token, token_len) == 0) {
        cli_options->byte_bit = !flag;
      } else if (memcmp("NTCHECK", token, token_len) == 0) {
        cli_options->nt_check_bit = flag;
      } else if (memcmp("READREDUCE", token, token_len) == 0) {
        cli_options->read_reduce_bit = flag;
      } else if (memcmp("SHIFTDEFAULT", token, token_len) == 0) {
        cli_options->shift_default_bit = flag;
      } else {
        PRNTERR2("\"%s\" is an invalid option", temp);
      }
    } else {
      // We now process the valued-parameter. Pick value after "=" and process
      i++;
      if (isspace(parm[i]) || parm[i] == '\0') {
        // no value specified
        PRNTERR2("Null string or blank is invalid for parameter %s", token);
        continue;
      }
      int j = i;
      while (parm[i] != '\0' && /* find next delimeter */ (parm[i] != ',' && parm[i] != '/' && parm[i] != ' ')) {
        i++;
      }
      memcpy(temp, parm+j, i - j); /* copy into TEMP */
      temp[i - j] = '\0';
      if (memcmp(token, "ACTFILENAME", token_len) == 0) {
        strcpy(cli_options->act_file, temp);
      } else if (memcmp("DEFAULT", token, token_len) == 0) {
        if (verify_is_digit(temp)) {
          switch (atoi(temp)) {
            case 0: cli_options->default_opt = OPT_0;
            case 1: cli_options->default_opt = OPT_1;
            case 2: cli_options->default_opt = OPT_2;
            case 3: cli_options->default_opt = OPT_3;
            case 4: cli_options->default_opt = OPT_4;
            case 5: cli_options->default_opt = OPT_5;
            default:
              printf("\"%s\" is an invalid option", temp);
              exit(999);
          }
        } else {
          PRNTERR2("\"%s\" is an invalid value for %s", temp, token);
        }
      } else if (memcmp(token, "ESCAPE", token_len) == 0) {
        cli_options->escape = temp[0];
      } else if (memcmp(token, "FILEPREFIX", token_len) == 0) {
        memcpy(file_prefix, temp, 5);
        file_prefix[MIN(5, strlen(temp))] = '\0';
      } else if (memcmp("GENERATEPARSER", token, token_len) == 0) {
        if (strxeq(temp, "c")) {
          cli_options->c_bit = true;
          cli_options->cpp_bit = false;
          cli_options->java_bit = false;
        } else if (strxeq(temp, "cpp")) {
          cli_options->c_bit = false;
          cli_options->cpp_bit = true;
          cli_options->java_bit = false;
        } else if (strxeq(temp, "java")) {
          cli_options->c_bit = false;
          cli_options->cpp_bit = false;
          cli_options->java_bit = true;
        } else {
          PRNTERR2("\"%s\" is an invalid language for %s. Supported languages are C|CPP|JAVA.", temp, token);
          exit(999);
        }
      } else if (memcmp(token, "HACTFILENAME", token_len) == 0) {
        strcpy(cli_options->hact_file, temp);
      } else if (memcmp("LALR", token, token_len) == 0) {
        unsigned long token_len = strlen(temp);
        if (token_len > MAX_PARM_SIZE) {
          temp[MAX_PARM_SIZE - 1] = '\0';
        }
        if (verify_is_digit(temp)) {
          cli_options->lalr_level = atoi(temp);
          if (cli_options->lalr_level > MAXIMUM_LA_LEVEL) {
            PRNTWNG2("\"%s\" exceeds maximum value of %d allowed for %s", temp, MAXIMUM_LA_LEVEL, token);
            cli_options->lalr_level = MAXIMUM_LA_LEVEL;
          }
        } else if (memcmp(translate(temp, token_len), "MAXIMUM", token_len) != 0) {
          PRNTERR2("\"%s\" is an invalid value for %s", temp, token);
        } else if (memcmp("MAXIMUM", translate(temp, token_len), token_len) == 0) {
          cli_options->lalr_level = MAXIMUM_LA_LEVEL;
        }
      } else if (memcmp(token, "ORMARK", token_len) == 0) {
        cli_options->ormark = temp[0];
      } else if (memcmp(token, "PREFIX", token_len) == 0) {
        strcpy(cli_options->prefix, temp);
      } else if (memcmp(token, "STACKSIZE", token_len) == 0) {
        if (verify_is_digit(temp)) {
          cli_options->stack_size = atoi(temp);
        } else {
          PRNTERR2("\"%s\" is an invalid value for %s", temp, token);
        }
      } else if (memcmp(token, "SUFFIX", token_len) == 0) {
        strcpy(cli_options->suffix, temp);
      } else if (memcmp(token, "TABLE", token_len) == 0) {
        int token_len = strlen(temp);
        if (token_len > MAX_PARM_SIZE) {
          temp[MAX_PARM_SIZE - 1] = '\0';
        }
        if (memcmp("SPACE", translate(temp, token_len), token_len) == 0) {
          cli_options->table_opt = OPTIMIZE_SPACE;
        } else if (memcmp(translate(temp, token_len), "TIME", token_len) == 0) {
          cli_options->table_opt = OPTIMIZE_TIME;
        } else {
          PRNTERR2("\"%s\" is an invalid value for %s", temp, token);
        }
      } else {
        PRNTERR2("\"%s\" is an invalid option", token);
      }
    }
    while (parm[i] != '\0' && /* clean after parameter */ (parm[i] == ',' || parm[i] == '/' || parm[i] == ' ')) {
      i++;
    }
  }
}

/// In this function, we read the first line(s) of the input text to see
/// if they are (it is an) "options" line(s). If so, the options are
/// processed. Then, we process user-supplied options if there are any.  In
/// any case, the options in effect are printed.
static void process_options_lines(char *grm_file, struct OutputFiles *of, char *file_prefix, struct CLIOptions *cli_options, FILE *sysgrm, struct ScannerState* ss, char *parm) {
  char old_parm[MAX_LINE_SIZE + 1];
  char output_line[PRINT_LINE_SIZE + 1];
  char opt_string[60][OUTPUT_PARM_SIZE + 1];
  int top = 0;
  strcpy(old_parm, parm); /* Save new options passed to program */
  static char ooptions[9] = " OPTIONS";
  ooptions[0] = cli_options->escape; /* "ooptions" always uses default escape symbol */
  // Until end-of-file is reached, process
  while (ss->p1 != NULL) {
    // all comment and %options lines.
    while (isspace(*ss->p2)) {
      // skip all space symbols
      if (*ss->p2 == '\n') {
        ss->line_no++;
        ss->linestart = ss->p2;
        ss->p1 = ss->p2 + 1;
      }
      ss->p2++;
    }
    char *line_end = strchr(ss->p2, '\n'); /* find end-of-line */
    // First, check if line is a comment line. If so, skip it.  Next,
    // check if line is an options line. If so, process it. Otherwise,
    // break out of the loop.
    // Note that no length check is necessary before checking for "--"
    // or "%options" since the buffer is always extended by
    // MAX_LINE_SIZE elements past its required length. (see read_input)
    if (*ss->p2 == '-' && *(ss->p2 + 1) == '-') {
      // Skip comment line.
    } else if (memcmp(ooptions, translate(ss->p2, 8), 8) == 0) {
      *line_end = '\0';
      PRNT(ss->p2); /* Print options line */
      strcpy(parm, ss->p2 + strlen(ooptions));
      options(file_prefix, cli_options, parm); /* Process hard-coded options */
    } else {
      ss->p2 = ss->p1; /* make p2 point to first character */
      break;
    }
    // If the line was a comment or an option line, check the following
    // line.  If we are at the end of the buffer, read in more data...
    ss->p1 = line_end + 1;
    if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
      int i = ss->bufend - ss->p1;
      if (i < MAX_LINE_SIZE) {
        strcpy(ss->input_buffer, ss->p1);
        ss->bufend = &ss->input_buffer[i];
        read_input(grm_file, sysgrm, ss);
        ss->p1 = &ss->input_buffer[0];
      }
    }
    ss->line_no++;
    ss->linestart = ss->p1 - 1;
    ss->p2 = ss->p1;
  }
  printf("\n");
  strcpy(parm, old_parm);
  options(file_prefix, cli_options, parm); /* Process new options passed directly to program */
  if (cli_options->act_file[0] == '\0') {
    sprintf(cli_options->act_file, "%sact.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  }
  if (cli_options->hact_file[0] == '\0') {
    sprintf(cli_options->hact_file, "%shdr.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  }
  sprintf(of->sym_file, "%ssym.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  sprintf(of->def_file, "%sdef.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  sprintf(of->prs_file, "%sprs.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  sprintf(of->dcl_file, "%sdcl.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  //                          PRINT OPTIONS:
  // Here we print all options set by the user. As of now, only about 48
  // different options and related aliases are allowed. In case that number
  // goes up, the bound of the array, opt_string, should be changed.
  // BLOCKB, BLOCKE, HBLOCKB and HBLOCKE can generate the longest strings
  // since their value can be up to MAX_PARM_SIZE characters long.
  sprintf(opt_string[++top], "ACTFILENAME=%s", cli_options->act_file);
  strcpy(opt_string[++top], cli_options->byte_bit ? "BYTE" : "NOBYTE");
  strcpy(opt_string[++top], cli_options->conflicts_bit ? "CONFLICTS" : "NOCONFLICTS");
  if (cli_options->default_opt.value == OPT_0.value) strcpy(opt_string[++top], "NODEFAULT");
  else if (cli_options->default_opt.value == OPT_1.value) printf("DEFAULT=1");
  else if (cli_options->default_opt.value == OPT_2.value) printf("DEFAULT=2");
  else if (cli_options->default_opt.value == OPT_3.value) printf("DEFAULT=3");
  else if (cli_options->default_opt.value == OPT_4.value) printf("DEFAULT=4");
  else if (cli_options->default_opt.value == OPT_5.value) printf("DEFAULT=5");
  sprintf(opt_string[++top], "ESCAPE=%c", cli_options->escape);
  sprintf(opt_string[++top], "FILEPREFIX=%s", file_prefix);
  if (cli_options->c_bit) {
    sprintf(opt_string[++top], "GENERATEPARSER=C");
  } else if (cli_options->cpp_bit) {
    sprintf(opt_string[++top], "GENERATEPARSER=CPP");
  } else if (cli_options->java_bit) {
    sprintf(opt_string[++top], "GENERATEPARSER=JAVA");
  } else {
    strcpy(opt_string[++top], "NOGENERATEPARSER");
  }
  strcpy(opt_string[++top], cli_options->goto_default_bit ? "GOTODEFAULT" : "NOGOTODEFAULT");
  sprintf(opt_string[++top], "HACTFILENAME=%s", cli_options->hact_file);
  sprintf(opt_string[++top], "LALR=%d", cli_options->lalr_level);
  strcpy(opt_string[++top], cli_options->nt_check_bit ? "NTCHECK" : "NONTCHECK");
  sprintf(opt_string[++top], "ORMARK=%c", cli_options->ormark);
  sprintf(opt_string[++top], "PREFIX=%s", cli_options->prefix);
  strcpy(opt_string[++top], cli_options->read_reduce_bit ? "READREDUCE" : "NOREADREDUCE");
  strcpy(opt_string[++top], cli_options->shift_default_bit ? "SHIFTDEFAULT" : "NOSHIFT-DEFAULT");
  sprintf(opt_string[++top], "STACKSIZE=%d", cli_options->stack_size);
  sprintf(opt_string[++top], "SUFFIX=%s", cli_options->suffix);
  if (cli_options->table_opt.value == OPTIMIZE_NO_TABLE.value) {
    strcpy(opt_string[++top], "NOTABLE");
  } else if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
    strcpy(opt_string[++top], "TABLE=SPACE");
  } else if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    strcpy(opt_string[++top], "TABLE=TIME");
  } else {
    PRNT("Unsupported table optimization option.");
    exit(12);
  }
  PRNT("Options in effect:");
  strcpy(output_line, "    ");
  for (int i = 1; i <= top; i++) {
    if (strlen(output_line) + strlen(opt_string[i]) > PRINT_LINE_SIZE - 1) {
      PRNT(output_line);
      strcpy(output_line, "    ");
    }
    strcat(output_line, opt_string[i]);
    if (strlen(output_line) + 2 < PRINT_LINE_SIZE - 1) {
      strcat(output_line, "  ");
    }
  }
  PRNT(output_line);
  PRNT("");
  if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
    if (cli_options->default_opt.value < OPT_4.value) {
      PRNTWNG("DEFAULT_OPTION requested must be >= 4 if optimizing for space");
    }
  }
  if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    if (cli_options->shift_default_bit) {
      PRNTWNG("SHIFT-DEFAULT option is only valid for Space tables");
    }
  }
  // Check if there are any conflicts in the options.
  if (cli_options->ormark == cli_options->escape) {
    PRNTERR("The options ormark and escape cannot have the same value");
    PRNT2("Input process aborted at line %d ...", ss->line_no);
    exit(12);
  }
}

/// SCANNER scans the input stream and returns the next input token.
static void scanner(char *grm_file, FILE *sysgrm, const struct CLIOptions* cli_options, struct ScannerState* ss, const struct ParserState* ps) {
  char tok_string[SYMBOL_SIZE + 1];
  char blockb[3] = {'/', '.'};
  char blocke[3] = {'.', '/'};
  char hblockb[3] = {'/', ':'};
  char hblocke[3] = {':', '/'};
  long blockb_len = strlen(blockb);
  long blocke_len = strlen(blocke);
  long hblockb_len = strlen(hblockb);
  long hblocke_len = strlen(hblocke);
scan_token:
  // Skip "blank" spaces.
  ss->p1 = ss->p2;
  while (isspace(*ss->p1)) {
    if (*ss->p1++ == '\n') {
      if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
        int i = ss->bufend - ss->p1;
        if (i < MAX_LINE_SIZE) {
          strcpy(ss->input_buffer, ss->p1);
          ss->bufend = &ss->input_buffer[i];
          read_input(grm_file, sysgrm, ss);
          ss->p1 = &ss->input_buffer[0];
        }
      }
      ss->line_no++;
      ss->linestart = ss->p1 - 1;
    }
  }
  if (strncmp(ss->p1, hblockb, hblockb_len) == 0) /* check block opener */ {
    ss->p1 = ss->p1 + hblockb_len;
    ss->ct_length = 0;
    ss->ct_ptr = ss->p1;
    if (*ss->p1 == '\n') {
      ss->ct_ptr++;
      ss->ct_length--;
      ss->ct_start_line = ss->line_no + 1;
      ss->ct_start_col = 1;
    } else {
      ss->ct_start_line = ss->line_no;
      ss->ct_start_col = ss->p1 - ss->linestart;
    }
    while (strncmp(ss->p1, hblocke, hblocke_len) != 0) {
      if (*ss->p1 == '\0') {
        PRNTERR2("End of file encountered while scanning header action block in rule %ld", ps->num_rules);
        exit(12);
      }
      if (*ss->p1++ == '\n') {
        if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
          int i = ss->bufend - ss->p1;
          if (i < MAX_LINE_SIZE) {
            strcpy(ss->input_buffer, ss->p1);
            ss->bufend = &ss->input_buffer[i];
            read_input(grm_file, sysgrm, ss);
            ss->p1 = &ss->input_buffer[0];
          }
        }
        ss->line_no++;
        ss->linestart = ss->p1 - 1;
      }
      ss->ct_length++;
    }
    ss->ct = HBLOCK_TK;
    ss->ct_end_line = ss->line_no;
    ss->ct_end_col = ss->p1 - ss->linestart - 1;
    ss->p2 = ss->p1 + hblocke_len;
    return;
  } else if (strncmp(ss->p1, blockb, blockb_len) == 0) /* check block  */ {
    ss->p1 = ss->p1 + blockb_len;
    ss->ct_length = 0;
    ss->ct_ptr = ss->p1;
    if (*ss->p1 == '\n') {
      ss->ct_ptr++;
      ss->ct_length--;
      ss->ct_start_line = ss->line_no + 1;
      ss->ct_start_col = 1;
    } else {
      ss->ct_start_line = ss->line_no;
      ss->ct_start_col = ss->p1 - ss->linestart;
    }
    while (strncmp(ss->p1, blocke, blocke_len) != 0) {
      if (*ss->p1 == '\0') {
        PRNTERR2("End of file encountered while scanning action block in rule %ld", ps->num_rules);
        exit(12);
      }
      if (*ss->p1++ == '\n') {
        if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
          long i = ss->bufend - ss->p1;
          if (i < MAX_LINE_SIZE) {
            strcpy(ss->input_buffer, ss->p1);
            ss->bufend = &ss->input_buffer[i];
            read_input(grm_file, sysgrm, ss);
            ss->p1 = &ss->input_buffer[0];
          }
        }
        ss->line_no++;
        ss->linestart = ss->p1 - 1;
      }
      ss->ct_length++;
    }
    ss->ct = BLOCK_TK;
    ss->ct_end_line = ss->line_no;
    ss->ct_end_col = ss->p1 - ss->linestart - 1;
    ss->p2 = ss->p1 + blocke_len;
    return;
  }
  // Scan the next token.
  ss->ct_ptr = ss->p1;
  ss->ct_start_line = ss->line_no;
  ss->ct_start_col = ss->p1 - ss->linestart;
  ss->p2 = ss->p1 + 1;
  switch (*ss->p1) {
    case '<':
      if (isalpha(*ss->p2)) {
        ss->p2++;
        while (*ss->p2 != '\n') {
          if (*ss->p2++ == '>') {
            ss->ct = SYMBOL_TK;
            ss->ct_length = ss->p2 - ss->p1;
            goto check_symbol_length;
          }
        }
        int i = SYMBOL_SIZE < ss->p2 - ss->p1 ? SYMBOL_SIZE : ss->p2 - ss->p1;
        memcpy(tok_string, ss->p1, i);
        tok_string[i] = '\0';
        PRNTERR2("Symbol \"%s\" has been referenced in line %ld without the closing \">\"", tok_string, ss->ct_start_line);
        exit(12);
      }
      break;
    case '\'':
      ss->ct_ptr = ss->p2;
      ss->ct = SYMBOL_TK;
      while (*ss->p2 != '\n') {
        if (*ss->p2 == '\'') {
          ss->p2++;
          if (*ss->p2 != '\'') {
            ss->ct_length = ss->p2 - ss->p1 - 2;
            goto remove_quotes;
          }
        }
        ss->p2++;
      }
      ss->ct_length = ss->p2 - ss->p1 - 1;
      memcpy(tok_string, ss->p1, ss->ct_length);
      tok_string[ss->ct_length] = '\0';
      PRNTWNG2("Symbol \"%s\" referenced in line %ld requires a closing quote", tok_string, ss->ct_start_line);
    remove_quotes:
      if (ss->ct_length == 0) /* Empty symbol? disregard it */
        goto scan_token;
      int i = 0;
      ss->p1 = ss->ct_ptr;
      do {
        *ss->p1++ = ss->ct_ptr[i++];
        if (ss->ct_ptr[i] == '\'') {
          i++; /* skip next quote */
        }
      } while (i < ss->ct_length);
      ss->ct_length = ss->p1 - ss->ct_ptr;
      goto check_symbol_length;
    case '-': /* scan possible comment  */
      if (*ss->p2 == '-') {
        ss->p2++;
        while (*ss->p2 != '\n') {
          ss->p2++;
        }
        goto scan_token;
      } else if (*ss->p2 == '>' && isspace(*(ss->p2 + 1))) {
        ss->ct = ARROW_TK;
        ss->ct_length = 2;
        ss->p2++;
        return;
      }
      break;
    case ':':
      if (*ss->p2 == ':' && *(ss->p2 + 1) == '=' && isspace(*(ss->p2 + 2))) {
        ss->ct = EQUIVALENCE_TK;
        ss->ct_length = 3;
        ss->p2 = ss->p1 + 3;
        return;
      }
      break;
    case '\0':
    case '\x1a': /* CTRL-Z • END-OF-FILE? */
      ss->ct = EOF_TK;
      ss->ct_length = 0;
      ss->p2 = ss->p1;
      return;
    default:
      if (*ss->p1 == cli_options->ormark && isspace(*ss->p2)) {
        ss->ct = OR_TK;
        ss->ct_length = 1;
        return;
      } else if (*ss->p1 == cli_options->escape) /* escape character? */
      {
        char *p3 = ss->p2 + 1;
        switch (*ss->p2) {
          case 't':
          case 'T':
            if (strxeq(p3, "erminals") && isspace(*(ss->p1 + 10))) {
              ss->ct = TERMINALS_KEY_TK;
              ss->ct_length = 10;
              ss->p2 = ss->p1 + 10;
              return;
            }
            break;
          case 'd':
          case 'D':
            if (strxeq(p3, "efine") && isspace(*(ss->p1 + 7))) {
              ss->ct = DEFINE_KEY_TK;
              ss->ct_length = 7;
              ss->p2 = ss->p1 + 7;
              return;
            }
            break;
          case 'e':
          case 'E':
            if (strxeq(p3, "mpty") && isspace(*(ss->p1 + 6))) {
              ss->ct = EMPTY_SYMBOL_TK;
              ss->ct_length = 6;
              ss->p2 = ss->p1 + 6;
              return;
            }
            if (strxeq(p3, "rror") && isspace(*(ss->p1 + 6))) {
              ss->ct = ERROR_SYMBOL_TK;
              ss->ct_length = 6;
              ss->p2 = ss->p1 + 6;
              return;
            }
            if (strxeq(p3, "ol") && isspace(*(ss->p1 + 4))) {
              ss->ct = EOL_SYMBOL_TK;
              ss->ct_length = 4;
              ss->p2 = ss->p1 + 4;
              return;
            }
            if (strxeq(p3, "of") && isspace(*(ss->p1 + 4))) {
              ss->ct = EOF_SYMBOL_TK;
              ss->ct_length = 4;
              ss->p2 = ss->p1 + 4;
              return;
            }
            if (strxeq(p3, "nd") && isspace(*(ss->p1 + 4))) {
              ss->ct = END_KEY_TK;
              ss->ct_length = 4;
              ss->p2 = ss->p1 + 4;
              return;
            }
            break;
          case 'r':
          case 'R':
            if (strxeq(p3, "ules") && isspace(*(ss->p1 + 6))) {
              ss->ct = RULES_KEY_TK;
              ss->ct_length = 6;
              ss->p2 = ss->p1 + 6;
              return;
            }
            break;
          case 'a':
          case 'A':
            if (strxeq(p3, "lias") && isspace(*(ss->p1 + 6))) {
              ss->ct = ALIAS_KEY_TK;
              ss->ct_length = 6;
              ss->p2 = ss->p1 + 6;
              return;
            }
            break;
          case 's':
          case 'S':
            if (strxeq(p3, "tart") && isspace(*(ss->p1 + 6))) {
              ss->ct = START_KEY_TK;
              ss->ct_length = 6;
              ss->p2 = ss->p1 + 6;
              return;
            }
            break;
          case 'n':
          case 'N':
            if (strxeq(p3, "ames") && isspace(*(ss->p1 + 6))) {
              ss->ct = NAMES_KEY_TK;
              ss->ct_length = 6;
              ss->p2 = ss->p1 + 6;
              return;
            }
            break;
          default:
            break;
        }
        ss->ct = MACRO_NAME_TK;
        while (!isspace(*ss->p2)) {
          ss->p2++;
        }
        ss->ct_length = ss->p2 - ss->p1;
        goto check_symbol_length;
      }
  }
  ss->ct = SYMBOL_TK;
  while (!isspace(*ss->p2)) {
    ss->p2++;
  }
  ss->ct_length = ss->p2 - ss->p1;
check_symbol_length:
  if (ss->ct_length > SYMBOL_SIZE) {
    ss->ct_length = SYMBOL_SIZE;
    memcpy(tok_string, ss->p1, ss->ct_length);
    tok_string[ss->ct_length] = '\0';
    if (symbol_image(tok_string, ps) == OMEGA) {
      PRNTWNG2("Length of Symbol \"%s\" in line %d exceeds maximum of ", tok_string, ss->line_no);
    }
  }
}

/// This function allocates a line_elemt structure and returns a pointer to it.
static struct line_elemt *alloc_line(struct LinePool* lp) {
  struct line_elemt *p = lp->line_pool_root;
  if (p != NULL) {
    lp->line_pool_root = p->link;
  } else {
    talloc0p(&p, struct line_elemt);
  }
  return p;
}

/// This function frees a line_elemt structure which is returned to a free pool.
static void free_line(struct line_elemt *p, struct LinePool* lp) {
  p->link = lp->line_pool_root;
  lp->line_pool_root = p;
}

/// FIND_MACRO takes as argument a pointer to a macro name. It searches for
/// the macro name in the hash table based on MACRO_TABLE. If the macro name
/// is found then the macro definition associated with it is returned.
/// If the name is not found, then a message is printed, a new definition is
/// entered to avoid more messages and NULL is returned.
static struct line_elemt *find_macro(char *name, ListInt macro_table, struct LinePool* lp, struct ParserState* ps) {
  struct line_elemt *root = NULL;
  char macro_name[MAX_LINE_SIZE + 1];
  char *s = macro_name;
  for (char *ptr = name; *ptr != '\0'; ptr++) {
    *s++ = isupper(*ptr) ? tolower(*ptr) : *ptr;
  }
  *s = '\0';
  const int i = hash(macro_name);
  for (int j = macro_table.raw[i]; j != NIL; j = ps->defelmt[j].next) {
    if (strcmp(macro_name, ps->defelmt[j].name) == 0) {
      char *ptr = ps->defelmt[j].macro;
      /* undefined macro? */
      if (ptr) {
        while (*ptr != '\0') {
          struct line_elemt *q = alloc_line(lp);
          s = q->line;
          while (*ptr != '\n')
            *s++ = *ptr++;
          *s = '\0';
          ptr++; /* skip newline marker */
          if (root == NULL) {
            q->link = q; /* make circular */
          } else {
            q->link = root->link;
            root->link = q;
          }
          root = q;
        }
      }
      return root;
    }
  }
  // Make phony definition for macro to avoid future errors.
  if (ps->num_defs >= (int) ps->defelmt_size) {
    ps->defelmt_size += DEFELMT_INCREMENT;
    ps->defelmt = (struct defelmt_type *)
    (ps->defelmt == (struct defelmt_type *) NULL
       ? malloc(ps->defelmt_size * sizeof(struct defelmt_type))
       : realloc(ps->defelmt, ps->defelmt_size * sizeof(struct defelmt_type)));
    if (ps->defelmt == (struct defelmt_type *) NULL)
      nospace();
  }
  strcpy(ps->defelmt[ps->num_defs].name, macro_name);
  ps->defelmt[ps->num_defs].length = 0;
  ps->defelmt[ps->num_defs].macro = NULL;
  ps->defelmt[ps->num_defs].next = macro_table.raw[i];
  macro_table.raw[i] = ps->num_defs;
  ps->num_defs++;
  return NULL;
}

/// PROCESS_ACTION_LINE takes as arguments a line of text from an action
/// block and the rule number with which the block is associated.
/// It first scans the text for predefined macro names and then for
/// user defined macro names. If one is found, the macro definition is
/// substituted for the name. The modified action text is then printed out in
/// the action file.
static void process_action_line(FILE *sysout, char *text, const int line_no, const int rule_no, char *grm_file, const struct CLIOptions* cli_options, ListInt macro_table, struct LinePool* lp, const struct ruletab_type *rules, ListInt rhs_sym, struct ParserState* ps) {
  char temp1[MAX_LINE_SIZE + 1];
  char suffix[MAX_LINE_SIZE + 1];
  char symbol[SYMBOL_SIZE + 1];
  const int output_size = 5000;
  struct line_elemt *q;
  struct line_elemt *root = NULL;
  struct line_elemt *input_line_root = NULL;
next_line: {
  }
  int text_len = strlen(text);
  int k = 0; /* k is the cursor */
  while (k < text_len) {
    // all macro names begin with the ESCAPE
    if (text[k] == cli_options->escape) {
      // character
      // 12 is length of %rule_number and
      // %num_symbols.
      if (k + 12 <= text_len) {
        if (strxeq(text + k, krule_number)) {
          strcpy(temp1, text + k + 12);
          if (k + 12 != text_len)
            sprintf(text + k, "%d%s", rule_no, temp1);
          else
            sprintf(text + k, "%d", rule_no);
          goto proceed;
        }
        if (strxeq(text + k, knum_symbols)) {
          strcpy(temp1, text + k + 12);
          if (k + 12 != text_len) {
            sprintf(text + k, "%ld%s", ps->num_symbols, temp1);
          } else {
            sprintf(text + k, "%ld", ps->num_symbols);
          }
          goto proceed;
        }
      }
      // 11 is the length of %input_file
      if (k + 11 <= text_len) {
        if (strxeq(text + k, kinput_file)) {
          strcpy(temp1, text + k + 11);
          if (k + 11 != text_len) {
            sprintf(text + k, "%s%s", grm_file, temp1);
          } else {
            sprintf(text + k, "%s", grm_file);
          }
          goto proceed;
        }
      }
      if (k + 10 <= text_len) /* 10 is the length of %rule_size and */
      // %rule_text, %num_rules and %next_line
      {
        if (strxeq(text + k, krule_text)) {
          char temp2[MAX_LINE_SIZE + 1];
          int jj;
          if (k + 10 != text_len) {
            strcpy(temp1, text + k + 10);
            // Remove trailing blanks
            for (jj = strlen(temp1) - 1; jj >= 0 && temp1[jj] == ' '; jj--) {
            }
            // if not a string of blanks
            if (jj != 0) {
              temp1[++jj] = '\0';
            } else {
              temp1[0] = '\0';
            }
          } else {
            temp1[0] = '\0';
            jj = 0;
          }
          const int max_len = output_size - k - jj;
          restore_symbol(temp2, RETRIEVE_STRING(rules[rule_no].lhs, ps->string_table, ps->symno), cli_options->ormark, cli_options->escape);
          // if a single production
          strcat(temp2, " ::=");
          if (strlen(temp2) > max_len) {
            strcpy(temp2, " ... ");
          } else /* Copy right-hand-side symbols to temp2 */
          {
            for for_each_rhs(j, rule_no, rules) {
              restore_symbol(symbol, RETRIEVE_STRING(rhs_sym.raw[j], ps->string_table, ps->symno), cli_options->ormark, cli_options->escape);
              if (strlen(temp2) + strlen(symbol) + 1 < max_len) {
                strcat(temp2, " ");
                strcat(temp2, symbol);
              } else {
                if (strlen(temp2) + 3 < max_len) {
                  strcat(temp2, "...");
                }
                break;
              }
            }
          }
          text[k] = '\0';
          strcat(text, temp2);
          strcat(text, temp1);
          k = k - 1 + strlen(temp2); /* Adjust cursor */
          goto proceed;
        }
        if (strxeq(text + k, krule_size)) {
          strcpy(temp1, text + k + 10);
          if (k + 10 != text_len) {
            sprintf(text + k, "%d%s", RHS_SIZE(rule_no, rules), temp1);
          } else {
            sprintf(text + k, "%d", RHS_SIZE(rule_no, rules));
          }
          goto proceed;
        }
        if (strxeq(text + k, knext_line)) {
          strcpy(temp1, text + k + 10);
          if (k + 10 != text_len) {
            sprintf(text + k, "%d%s", line_no + 1, temp1);
          } else {
            sprintf(text + k, "%d", line_no + 1);
          }
          goto proceed;
        }
        if (strxeq(text + k, knum_rules)) {
          strcpy(temp1, text + k + 10);
          if (k + 10 != text_len) {
            sprintf(text + k, "%ld%s", ps->num_rules, temp1);
          } else {
            sprintf(text + k, "%ld", ps->num_rules);
          }
          goto proceed;
        }
      }
      if (k + 13 <= text_len) /* 13 is length of %current_line  */
      {
        if (strxeq(text + k, kcurrent_line)) {
          strcpy(temp1, text + k + 13);
          if (k + 13 != text_len)
            sprintf(text + k, "%d%s", line_no, temp1);
          else
            sprintf(text + k, "%d", line_no);
          goto proceed;
        }
      }
      if (k + 14 <= text_len) /* 14 is length of %num_terminals */
      {
        if (strxeq(text + k, knum_terminals)) {
          strcpy(temp1, text + k + 14);
          if (k + 14 != text_len)
            sprintf(text + k, "%ld%s", ps->num_terminals, temp1);
          else
            sprintf(text + k, "%ld", ps->num_terminals);
          goto proceed;
        }
      }
      if (k + 18 <= text_len) /* 18 is length of %num_non_terminals */
      {
        if (strxeq(text + k, knum_non_terminals)) {
          strcpy(temp1, text + k + 18);
          if (k + 18 != text_len) {
            sprintf(text + k, "%ld%s", ps->num_non_terminals, temp1);
          } else {
            sprintf(text + k, "%ld", ps->num_non_terminals);
          }
          goto proceed;
        }
      }
      // Macro in question is not one of the predefined macros. Try user-defined
      // macro list.
      // find next delimeter
      int jj;
      for (jj = k + 1; jj < text_len && !isspace(text[jj]); ++jj) {
      }
      memcpy(symbol, text + k, jj - k); /* copy macro name into symbol */
      symbol[jj - k] = '\0';
      // Is there any text after macro ?
      if (jj < text_len) {
        strcpy(suffix, text + jj); /* Copy rest of text into "suffix". */
      } else {
        suffix[0] = '\0';
      }
      text[k] = '\0'; /* prefix before macro */
      root = find_macro(symbol, macro_table, lp, ps); /* "root" points to a circular  */
      // linked list of line_elemt(s)
      // containing macro definition.
      if (root != NULL) /* if macro name was found */
      {
        struct line_elemt *tail;
        q = root;
        root = root->link;
        if (suffix[0] != '\0') {
          // If there is room to add the suffix to the
          // last macro line then do it. Or else
          // allocate a new line_elemt, copy the suffix
          // into it and add it to the list of lines to
          // be processed.
          if (strlen(q->line) + strlen(suffix) < output_size) {
            strcat(q -> line, suffix);
            tail = q;
          } else {
            tail = alloc_line(lp);
            strcpy(tail -> line, suffix);
            q->link = tail;
          }
        } else {
          tail = q;
        }
        tail->link = NULL; /* make circular list linear */
        // If there is space for the first macro line to be
        // added to the prefix then do it.
        if (strlen(text) + strlen(root->line) < output_size) {
          strcat(text, root -> line);
          q = root;
          root = root->link;
          free_line(q, lp);
        }
        // if there are more macro lines to process,
        // add list to list headed by INPUT_LINE_ROOT
        if (root != NULL) {
          tail->link = input_line_root;
          input_line_root = root;
        }
        k--;
      } else /* If macro name is not found then rebuild line.*/
      {
        strcat(text, symbol);
        if (suffix[0] != '\0')
          strcat(text, suffix);
        k = jj;
      }
    proceed:
      text_len = strlen(text);
    }
    ++k;
  }
  // If text is greater than output size,
  // print error message and truncate line.
  const unsigned long l = strlen(text);
  if (l > output_size) {
    for (int j = l - 1; j >= output_size; j--) {
      if (text[j] != ' ') {
        PRNTERR2("Size of output line \"%s\" is greater than OUTPUT_SIZE (%d), it was %lu", text, output_size, strlen(text));
        break;
      }
    }
    text[output_size] = '\0';
  }
  fprintf(sysout, "%s\n", text);
  // If there is another macro line copy it to TEXT and then process it.
  if (input_line_root != NULL) {
    strcpy(text, input_line_root -> line);
    q = input_line_root;
    input_line_root = input_line_root->link;
    free_line(q, lp);
    goto next_line;
  }
}

/// This procedure takes as argument a macro definition. If the name of the
/// macro is one of the predefined names, it issues an error.  Otherwise, it
/// inserts the macro definition into the table headed by MACRO_TABLE.
static void mapmacro(const int def_index, ListInt macro_table, const struct ParserState* ps) {
  if (strcmp(ps->defelmt[def_index].name, krule_text) == 0 ||
      strcmp(ps->defelmt[def_index].name, krule_number) == 0 ||
      strcmp(ps->defelmt[def_index].name, knum_rules) == 0 ||
      strcmp(ps->defelmt[def_index].name, krule_size) == 0 ||
      strcmp(ps->defelmt[def_index].name, knum_terminals) == 0 ||
      strcmp(ps->defelmt[def_index].name, knum_non_terminals) == 0 ||
      strcmp(ps->defelmt[def_index].name, knum_symbols) == 0 ||
      strcmp(ps->defelmt[def_index].name, kinput_file) == 0 ||
      strcmp(ps->defelmt[def_index].name, kcurrent_line) == 0 ||
      strcmp(ps->defelmt[def_index].name, knext_line) == 0) {
    PRNTWNG2("predefined macro \"%s\" cannot be redefined. Line %ld", ps->defelmt[def_index].name, ps->defelmt[def_index].start_line);
  } else {
    const int i = hash(ps->defelmt[def_index].name);
    for (int j = macro_table.raw[i]; j != NIL; j = ps->defelmt[j].next) {
      if (strcmp(ps->defelmt[j].name, ps->defelmt[def_index].name) == 0) {
        PRNTWNG2("Redefinition of macro \"%s\" in line %ld", ps->defelmt[def_index].name, ps->defelmt[def_index].start_line);
        break;
      }
    }
    ps->defelmt[def_index].next = macro_table.raw[i];
    macro_table.raw[i] = def_index;
  }
}

/// Process all semantic actions and generate action file.
static void process_actions(char *grm_file, struct CLIOptions *cli_options, struct ScannerState* ss, const struct ruletab_type *rules, ListInt rhs_sym, struct ParserState* ps) {
  struct LinePool lp = (struct LinePool) {
    .line_pool_root = NULL,
  };
  char *p;
  char line[MAX_LINE_SIZE + 1];
  FILE *sysact = fopen(cli_options->act_file, "w");
  FILE *syshact = fopen(cli_options->hact_file, "w");
  if (sysact == (FILE *) NULL) {
    fprintf(stderr, "***ERROR: Action file \"%s\" cannot be opened.\n", cli_options->act_file);
    exit(12);
  }
  if (syshact == (FILE *) NULL) {
    fprintf(stderr, "***ERROR: Header Action file \"%s\" cannot be opened.\n", cli_options->hact_file);
    exit(12);
  }
  // TODO • make this a local?
  FILE *sysgrm;
  if ((sysgrm = fopen(grm_file, "r")) == (FILE *) NULL) {
    fprintf(stderr, "***ERROR: Input file %s containing grammar is empty, undefined, or invalid\n", grm_file);
    exit(12);
  }
  ListInt macro_table = allocateListInt(HT_SIZE);
  for (int i = 0; i < HT_SIZE; i++) {
    macro_table.raw[i] = NIL;
  }
  ss->bufend = &ss->input_buffer[0];
  read_input(grm_file, sysgrm, ss);
  ss->p2 = &ss->input_buffer[0];
  ss->linestart = ss->p2 - 1;
  ss->p1 = ss->p2;
  ss->line_no = 1;
  // Read in all the macro definitions and insert them into macro_table.
  for (int i = 0; i < ps->num_defs; i++) {
    calloc0p(&ps->defelmt[i].macro, ps->defelmt[i].length + 2, char);
    for (; ss->line_no < ps->defelmt[i].start_line; ss->line_no++) {
      while (*ss->p1 != '\n') {
        ss->p1++;
      }
      ss->p1++;
      if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
        int k = ss->bufend - ss->p1;
        if (k < MAX_LINE_SIZE) {
          strcpy(ss->input_buffer, ss->p1);
          ss->bufend = &ss->input_buffer[k];
          read_input(grm_file, sysgrm, ss);
          ss->p1 = &ss->input_buffer[0];
        }
      }
      ss->linestart = ss->p1 - 1;
    }
    ss->p1 = ss->linestart + ps->defelmt[i].start_column;
    for (int j = 0; j < ps->defelmt[i].length; j++) {
      ps->defelmt[i].macro[j] = *ss->p1;
      if (*ss->p1++ == '\n') {
        if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
          int k = ss->bufend - ss->p1;
          if (k < MAX_LINE_SIZE) {
            strcpy(ss->input_buffer, ss->p1);
            ss->bufend = &ss->input_buffer[k];
            read_input(grm_file, sysgrm, ss);
            ss->p1 = &ss->input_buffer[0];
          }
        }
        ss->line_no++;
        ss->linestart = ss->p1 - 1;
      }
    }
    ps->defelmt[i].macro[ps->defelmt[i].length] = '\n';
    ps->defelmt[i].macro[ps->defelmt[i].length + 1] = '\0';
    for (p = ps->defelmt[i].name; *p != '\0'; p++) {
      *p = isupper(*p) ? tolower(*p) : *p;
    }
    mapmacro(i, macro_table, ps);
  }
  // Read in all the action blocks and process them.
  for (int i = 0; i < ps->num_acts; i++) {
    for (; ss->line_no < ps->actelmt[i].start_line; ss->line_no++) {
      while (*ss->p1 != '\n') {
        ss->p1++;
      }
      ss->p1++;
      if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
        int k = ss->bufend - ss->p1;
        if (k < MAX_LINE_SIZE) {
          strcpy(ss->input_buffer, ss->p1);
          ss->bufend = &ss->input_buffer[k];
          read_input(grm_file, sysgrm, ss);
          ss->p1 = &ss->input_buffer[0];
        }
      }
      ss->linestart = ss->p1 - 1;
    }
    if (ps->actelmt[i].start_line == ps->actelmt[i].end_line) {
      int len = ps->actelmt[i].end_column - ps->actelmt[i].start_column + 1;
      memcpy(line, ss->linestart + ps->actelmt[i].start_column, len);
      line[len] = '\0';
      while (*ss->p1 != '\n') {
        ss->p1++;
      }
    } else {
      p = line;
      ss->p1 = ss->linestart + ps->actelmt[i].start_column;
      while (*ss->p1 != '\n') {
        *p++ = *ss->p1++;
      }
      *p = '\0';
    }
    if (ps->actelmt[i].header_block) {
      process_action_line(syshact, line, ss->line_no, ps->actelmt[i].rule_number, grm_file, cli_options, macro_table, &lp, rules, rhs_sym, ps);
    } else {
      process_action_line(sysact, line, ss->line_no, ps->actelmt[i].rule_number, grm_file, cli_options, macro_table, &lp, rules, rhs_sym, ps);
    }
    if (ss->line_no != ps->actelmt[i].end_line) {
      while (ss->line_no < ps->actelmt[i].end_line) {
        ss->p1++;
        if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
          int k = ss->bufend - ss->p1;
          if (k < MAX_LINE_SIZE) {
            strcpy(ss->input_buffer, ss->p1);
            ss->bufend = &ss->input_buffer[k];
            read_input(grm_file, sysgrm, ss);
            ss->p1 = &ss->input_buffer[0];
          }
        }
        ss->line_no++;
        ss->linestart = ss->p1 - 1;
        if (ss->line_no < ps->actelmt[i].end_line) {
          p = line;
          while (*ss->p1 != '\n') {
            *p++ = *ss->p1++;
          }
          *p = '\0';
          if (ps->actelmt[i].header_block) {
            process_action_line(syshact, line, ss->line_no, ps->actelmt[i].rule_number, grm_file, cli_options, macro_table, &lp, rules, rhs_sym, ps);
          } else {
            process_action_line(sysact, line, ss->line_no, ps->actelmt[i].rule_number, grm_file, cli_options, macro_table, &lp, rules, rhs_sym, ps);
          }
        }
      }
      if (ps->actelmt[i].end_column != 0) {
        int len = ps->actelmt[i].end_column;
        memcpy(line, ss->p1, len);
        line[len] = '\0';
        if (ps->actelmt[i].header_block) {
          process_action_line(syshact, line, ss->line_no, ps->actelmt[i].rule_number, grm_file, cli_options, macro_table, &lp, rules, rhs_sym, ps);
        } else {
          process_action_line(sysact, line, ss->line_no, ps->actelmt[i].rule_number, grm_file, cli_options, macro_table, &lp, rules, rhs_sym, ps);
        }
      }
    }
  }
  for (int i = 0; i < ps->num_defs; i++) {
    ffree(ps->defelmt[i].macro);
  }
  ffree(ps->defelmt);
  ffree(ps->actelmt);
  fclose(sysgrm); /* Close grammar file and reopen it. */
  fclose(sysact);
  fclose(syshact);
}

/// Actions to be taken if grammar is successfully parsed.
static void accept_action(char *grm_file, struct CLIOptions *cli_options, FILE *sysgrm, struct ScannerState* ss, ListInt *rhs_sym, struct ruletab_type **rulesp, struct ParserState* ps, struct symno_type *symno, int **name) {
  if (ps->rulehdr == NULL) {
    printf("Informative: Empty grammar read in. Processing stopped.\n");
    fclose(sysgrm);
    exit(12);
  }
  ps->num_non_terminals = ps->num_symbols - ps->num_terminals;
  if (cli_options->error_maps_bit) {
    // make_names_map
    {
      // Construct the NAME map, and update the elements of SYMNO with their names.
      symno[accept_image].name_index = name_map("", ps);
      if (error_image == DEFAULT_SYMBOL) {
        symno[DEFAULT_SYMBOL].name_index = symno[accept_image].name_index;
      }
      for for_each_t_fw(symbol, ps) {
        if (symno[symbol].name_index == OMEGA) {
          symno[symbol].name_index = name_map(RETRIEVE_STRING(symbol, ps->string_table, symno), ps);
        }
      }
      for for_each_nt_fw(symbol, ps) {
        if (symno[symbol].name_index == OMEGA) {
          symno[symbol].name_index = name_map(RETRIEVE_STRING(symbol, ps->string_table, symno), ps);
        }
      }
      calloc0p(name, ps->num_names + 1, int);
      for (int i = 0; i < HT_SIZE; i++) {
        for (const struct hash_type *p = ps->hash_table[i]; p != NULL; p = p->link) {
          if (p->name_index != OMEGA) {
            (*name)[p->name_index] = p->st_ptr;
          }
        }
      }
    }
  }
  // Construct the rule table.  At this stage, NUM_ITEMS is equal to the sum
  // of the right-hand side lists of symbols.  It is used in the declaration of
  // RULE_TAB.  After RULE_TAB is allocated, we increase NUM_ITEMS to its
  // correct value.  Recall that the first rule is numbered 0; therefore we
  // increase the number of items by 1 to reflect this numbering.
  {
    struct node *ptr;
    int rhs_ct = 0;
    calloc0p(rulesp, ps->num_rules + 2, struct ruletab_type);
    *rhs_sym = allocateListInt(ps->num_items + 1);
    ps->num_items += ps->num_rules + 1;
    int ii = 0;
    struct ruletab_type *rules = *rulesp;
    // Put starting rules from start symbol linked list in rule and rhs table
    if (ps->start_symbol_root != NULL) {
      // Turn circular list into linear
      struct node *q = ps->start_symbol_root;
      ps->start_symbol_root = q->next;
      q->next = NULL;
      for (ptr = ps->start_symbol_root; ptr != NULL; ptr = ptr->next) {
        rules[ii].lhs = accept_image;
        rules[ii++].rhs = rhs_ct;
        if (ptr->value != empty) {
          rhs_sym->raw[rhs_ct++] = ptr->value;
        }
      }
      free_nodes(ps->start_symbol_root, q);
    }
    // In this loop, the grammar is placed in the rule table structure and the
    // right-hand sides are placed in the RHS table.  A check is made to prevent
    // terminals from being used as left hand sides.
    for (; ii <= ps->num_rules; ii++) {
      rules[ii].rhs = rhs_ct;
      ptr = ps->rulehdr[ii].rhs_root;
      if (ptr != NULL) {
        // not am empty right-hand side?
        do {
          ptr = ptr->next;
          rhs_sym->raw[rhs_ct++] = ptr->value;
        } while (ptr != ps->rulehdr[ii].rhs_root);
        ptr = ptr->next; /* point to 1st element */
        free_nodes(ptr, ps->rulehdr[ii].rhs_root);
      }
      if (ps->rulehdr[ii].lhs == OMEGA) {
        rules[ii].lhs = rules[ii - 1].lhs;
      } else if (IS_A_TERMINAL_P(ps->rulehdr[ii].lhs, ps)) {
        char temp[SYMBOL_SIZE + 1];
        restore_symbol(temp, RETRIEVE_STRING(ps->rulehdr[ii].lhs, ps->string_table, symno), cli_options->ormark, cli_options->escape);
        PRNTERR2("In rule %d: terminal \"%s\" used as left hand side", ii, temp);
        PRNTERR("Processing terminated due to input errors.");
        exit(12);
      } else {
        rules[ii].lhs = ps->rulehdr[ii].lhs;
      }
    }
    rules[ps->num_rules + 1].rhs = rhs_ct; /* Fence !! */
  }
  fclose(sysgrm); /* Close grammar input file. */
  process_actions(grm_file, cli_options, ss, *rulesp, *rhs_sym, ps);
}

/// This procedure opens all relevant files and processes the input grammar.
void process_input(char *grm_file, struct OutputFiles *output_files, const int argc, char *argv[], char *file_prefix, struct CLIOptions *cli_options, ListInt *rhs_sym, struct ruletab_type **rulesp, struct ParserState* ps, int **name) {
  char parm[256] = "";

  // Parse args.
  {
    // If options are passed to the program, copy them into "parm".
    if (argc > 2) {
      int j = 0;
      parm[0] = '\0';
      while (j < argc - 2) {
        if (*argv[++j] == '-') {
          strcat(parm, argv[j]+1);
        } else {
          strcat(parm, argv[j]);
          printf("***WARNING: Option \"%s\" is missing preceding '-'.\n", argv[j]);
        }
        strcat(parm, " ");
      }
    }
  }

  FILE *sysgrm;

  // Prepare.
  {
    // Open input grammar file. If the file cannot be opened and that file name
    // did not have an extension, then the extension ".g" is added to the file
    // name and we try again. If no file can be found an error message is
    // issued and the program halts.
    if ((sysgrm = fopen(grm_file, "r")) == (FILE *) NULL) {
      int ii;
      for (ii = strlen(grm_file); ii > 0 && grm_file[ii] != '.' && grm_file[ii] != '/' && /* Unix */ grm_file[ii] != '\\'; /* Dos  */ ii--) {
      }
      if (grm_file[ii] != '.') {
        strcat(grm_file, ".g");
        if ((sysgrm = fopen(grm_file, "r")) == (FILE *) NULL) {
          fprintf(stderr, "***ERROR: Input file %s containing grammar is empty, undefined, or invalid\n", grm_file);
          exit(12);
        }
      } else {
        fprintf(stderr, "***ERROR: Input file %s containing grammar is empty, undefined, or invalid\n", grm_file);
        exit(12);
      }
    } else {
      if (strrchr(grm_file, '.') == NULL) {
        PRNTWNG2("A file named \"%s\" with no extension is being opened", grm_file);
      }
    }
  }

  struct ScannerState ss = {
    .ct = 0,
    .ct_start_col = 0,
    .ct_end_col = 0,
    .ct_length = 0,
    .ct_start_line = 0,
    .ct_end_line = 0,
    .line_no = 0,
  };

  // Init grammar.
  {
    calloc0p(&ps->terminal, STACK_SIZE, struct terminal_type);
    calloc0p(&(ps->hash_table), HT_SIZE, struct hash_type *);
    // Allocate space for input buffer and read in initial data in input
    // file. Next, invoke PROCESS_OPTION_LINES to process all lines in
    // input file that are options line.
    calloc0p(&ss.input_buffer, IOBUFFER_SIZE + 1 + MAX_LINE_SIZE, char);
    ss.bufend = &ss.input_buffer[0];
    read_input(grm_file, sysgrm, &ss);
    ss.p2 = &ss.input_buffer[0];
    ss.linestart = ss.p2 - 1;
    ss.p1 = ss.p2;
    ss.line_no++;
    if (*ss.p2 == '\0') {
      fprintf(stderr, "Input file \"%s\" containing grammar is empty, undefined, or invalid\n", grm_file);
      exit(12);
    }
    process_options_lines(grm_file, output_files, file_prefix, cli_options, sysgrm, &ss, parm);
    eolt_image = OMEGA;
    // Keywords, Reserved symbols, and predefined macros
    kdefine[0] = cli_options->escape; /* Set empty first space to the default */
    kterminals[0] = cli_options->escape; /* escape symbol.                      */
    kalias[0] = cli_options->escape;
    kstart[0] = cli_options->escape;
    krules[0] = cli_options->escape;
    knames[0] = cli_options->escape;
    kend[0] = cli_options->escape;
    krule_number[0] = cli_options->escape;
    krule_text[0] = cli_options->escape;
    krule_size[0] = cli_options->escape;
    knum_rules[0] = cli_options->escape;
    knum_terminals[0] = cli_options->escape;
    knum_non_terminals[0] = cli_options->escape;
    knum_symbols[0] = cli_options->escape;
    kinput_file[0] = cli_options->escape;
    kcurrent_line[0] = cli_options->escape;
    knext_line[0] = cli_options->escape;
    kstart_nt[0] = cli_options->escape;
    keolt[0] = cli_options->escape;
  }

  // Process grammar.
  {
    // PROCESS_GRAMMAR is invoked to process the source input. It uses an
    // LALR(1) parser table generated by LPG to recognize the grammar which it
    // places in the rulehdr structure.
    short state_stack[STACK_SIZE];
    scanner(grm_file, sysgrm, cli_options, &ss, ps); /* Get first token */
    int act = START_STATE;
  process_terminal:
    // Note that this driver assumes that the tables are LPG SPACE tables with no GOTO-DEFAULTS.
    state_stack[++(ps->stack_top)] = act;
    act = t_action(act, ss.ct, ?);
    // Reduce
    if (act <= NUM_RULES) {
      ps->stack_top--;
    } else if (act > ERROR_ACTION || /* Shift_reduce */ act < ACCEPT_ACTION) /* Shift */
    {
      // token_action
      {
        {
          //    This function, TOKEN_ACTION, pushes the current token onto the
          // parse stack called TERMINAL. Note that in case of a BLOCK_, the name of
          // the token is not copied since blocks are processed separately on a
          // second pass.
          const int top = ps->stack_top + 1;
          ps->terminal[top].kind = ss.ct;
          ps->terminal[top].start_line = ss.ct_start_line;
          ps->terminal[top].start_column = ss.ct_start_col;
          ps->terminal[top].end_line = ss.ct_end_line;
          ps->terminal[top].end_column = ss.ct_end_col;
          ps->terminal[top].length = ss.ct_length;
          if (ss.ct != BLOCK_TK) {
            memcpy(ps->terminal[top].name, ss.ct_ptr, ss.ct_length);
            ps->terminal[top].name[ss.ct_length] = '\0';
          } else {
            ps->terminal[top].name[0] = '\0';
          }
        }
      }
      scanner(grm_file, sysgrm, cli_options, &ss, ps);
      if (act < ACCEPT_ACTION) {
        goto process_terminal;
      }
      act -= ERROR_ACTION;
    } else if (act == ACCEPT_ACTION) {
      accept_action(grm_file, cli_options, sysgrm, &ss, rhs_sym, rulesp, ps, ps->symno, name);
      goto end;
    } else {
      // error_action
      {
        // Error messages to be printed if an error is encountered during parsing.
        ss.ct_ptr[ss.ct_length] = '\0';
        if (ss.ct == EOF_TK) {
          PRNTERR2("End-of file reached prematurely");
        } else if (ss.ct == MACRO_NAME_TK) {
          PRNTERR2("Misplaced macro name \"%s\" found in line %d, column %d", ss.ct_ptr, ss.line_no, ss.ct_start_col);
        } else if (ss.ct == SYMBOL_TK) {
          char tok_string[SYMBOL_SIZE + 1];
          restore_symbol(tok_string, ss.ct_ptr, cli_options->ormark, cli_options->escape);
          PRNTERR2("Misplaced symbol \"%s\" found in line %d, column %d", tok_string, ss.line_no, ss.ct_start_col);
        } else {
          PRNTERR2("Misplaced keyword \"%s\" found in line %d, column %d", ss.ct_ptr, ss.line_no, ss.ct_start_col);
        }
        exit(12);
      }
    }
  process_non_terminal:
    do {
      const int lhs_sym = lhs[act]; /* to bypass IBMC12 bug */
      ps->stack_top -= rhs[act] - 1;
      rule_action[act](ps);
      act = nt_action(state_stack[ps->stack_top], lhs_sym);
    } while (act <= NUM_RULES);
    goto process_terminal;
  }

end: {
  }

  // Exit grammar.
  {
    // This routine is invoked to free all space used to process the input that
    // is no longer needed. Note that for the string_table, only the unused
    // space is released.
    if (ps->string_offset > 0) {
      ps->string_table = (char *)
      (ps->string_table == (char *) NULL
         ? malloc(ps->string_offset * sizeof(char))
         : realloc(ps->string_table, ps->string_offset * sizeof(char)));
      if (ps->string_table == (char *) NULL)
        nospace();
    }
    ffree(ps->terminal);
    ffree(ps->hash_table);
    ffree(ss.input_buffer);
    ffree(ps->rulehdr); /* allocated in action LPGACT when grammar is not empty */
  }
}
// endregion


















// region globals
char *RETRIEVE_NAME(const int indx, char *string_table, const int *name) {
  return &string_table[name[indx]];
}

/// This procedure obtains more TEMPORARY space.
bool allocate_more_space(cell ***base, long *size, long *base_size) {
  // The variable size always indicates the maximum number of cells
  // that has been allocated and reserved for the storage pool.
  // Initially, size should be set to 0 to indicate that no space has
  // yet been allocated. The pool of cells available is divided into
  // segments of size 2**LOG_BLKSIZE each and each segment is pointer
  // to by a slot in the array base.
  //
  // By dividing "size" by the size of the segment we obtain the
  // index for the next segment in base. If base is already full, it is
  // reallocated.
  //
  const long k = *size >> LOG_BLKSIZE; /* which segment? */
  if (k == *base_size) {
    const int BASE_INCREMENT = 64;
    // base overflow? reallocate
    *base_size += BASE_INCREMENT;
    *base = (cell **) (*base == NULL ? malloc(sizeof(cell *) * *base_size) : realloc(*base, sizeof(cell *) * *base_size));
    if (*base == (cell **) NULL) {
      return false;
    }
    for (long i = *base_size; i < *base_size; i++) {
      (*base)[i] = NULL;
    }
  }
  // If the Ast slot "k" does not already contain a segment, We try to
  // allocate one and place its address in (*base)[k].
  // If the allocation was not successful, we terminate;
  // otherwise, we adjust the address in (*base)[k] so as to allow us
  // to index the segment directly, instead of having to perform a
  // subtraction for each reference. Finally, we update size.
  //
  // Finally, we set the block to zeros.
  if ((*base)[k] == NULL) {
    (*base)[k] = (cell *) malloc(sizeof(cell) << LOG_BLKSIZE);
    if ((*base)[k] == (cell *) NULL) {
      return false;
    }
    (*base)[k] -= *size;
  }
  memset((void *)((*base)[k] + (*size)), 0, sizeof(cell) << LOG_BLKSIZE);
  *size += BLKSIZE;
  return true;
}

/// This procedure resets the temporary space already allocated so
/// that it can be reused before new blocks are allocated.
void reset_temporary_space(void) {
  ts.temp_top = 0; /* index of next usable elemt */
  ts.temp_size = 0;
}

/// This procedure frees all allocated temporary space.
void free_temporary_space(void) {
  for (int k = 0; k < ts.temp_base_size && ts.temp_base[k] != NULL; k++) {
    ts.temp_base[k] += k * BLKSIZE;
    ffree(ts.temp_base[k]);
  }
  if (ts.temp_base != NULL) {
    ffree(ts.temp_base);
    ts.temp_base = NULL;
  }
  ts.temp_base_size = 0;
  ts.temp_top = 0;
  ts.temp_size = 0;
}

/// talloc allocates an object of size "size" in temporary space and
/// returns a pointer to it.
void *talloc(const long size) {
  long i = ts.temp_top;
  ts.temp_top += (size + sizeof(cell) - 1) / sizeof(cell);
  if (ts.temp_top > ts.temp_size) {
    i = ts.temp_size;
    ts.temp_top = ts.temp_size + (size + sizeof(cell) - 1) / sizeof(cell);
    if (!allocate_more_space(&ts.temp_base, &ts.temp_size, &ts.temp_base_size)) {
      ts.temp_top = ts.temp_size;
      return NULL;
    }
  }
  return &ts.temp_base[i >> LOG_BLKSIZE][i];
}

/// galloc allocates an object of size "size" in global space and
/// returns a pointer to it. It is analogous to "talloc", but it
/// is a local (static) routine that is only invoked in this file by
/// other more specialized routines.
void *galloc(const long size) {
  long i = gs.global_top;
  gs.global_top += (size + sizeof(cell) - 1) / sizeof(cell);
  if (gs.global_top > gs.global_size) {
    {
      // This function is invoked when the space left in a segment is not
      // enough for GALLOC to allocate a requested object. Rather than
      // waste the space, as many NODE structures as possible are allocated
      // in that space and stacked up in the NODE_POOL list.
      int top = i;
      while (true) {
        const long i = top;
        top += (sizeof(struct node) + sizeof(cell) - 1) / sizeof(cell);
        if (top > gs.global_size) {
          break;
        }
        struct node *p = (struct node *) &gs.global_base[i >> LOG_BLKSIZE][i];
        p->next = gs.node_pool;
        gs.node_pool = p;
      }
    }
    i = gs.global_size;
    gs.global_top = gs.global_size + (size + sizeof(cell) - 1) / sizeof(cell);
    if (!allocate_more_space(&gs.global_base, &gs.global_size, &gs.global_base_size)) {
      gs.global_top = gs.global_size;
      return NULL;
    }
  }
  return &gs.global_base[i >> LOG_BLKSIZE][i];
}

/// This function frees a linked list of nodes by adding them to the free
/// list. Head points to head of linked list and tail to the end.
void free_nodes(struct node *head, struct node *tail) {
  tail->next = gs.node_pool;
  gs.node_pool = head;
}

/// FILL_IN is a subroutine that pads a buffer, STRING,  with CHARACTER a
/// certain AMOUNT of times.
void fill_in(char string[], const int amount, const char character) {
  int ii;
  for (ii = 0; ii <= amount; ii++) {
    string[ii] = character;
  }
  string[ii] = '\0';
}

/// QCKSRT is a quicksort algorithm that takes as arguments an array of
/// integers, two numbers L and H that indicate the lower and upper bound
/// positions in ARRAY to be sorted.
static void qcksrt(ListInt array, const int l, const int h) {
  int lostack[14];
  int histack[14];
  // 2 ** 15 - 1 elements
  int top = 1;
  lostack[top] = l;
  histack[top] = h;
  while (top != 0) {
    int lower = lostack[top];
    int upper = histack[top--];
    while (upper > lower) {
      int i = lower;
      const int pivot = array.raw[lower];
      for (int j = lower + 1; j <= upper; j++) {
        if (array.raw[j] < pivot) {
          array.raw[i] = array.raw[j];
          i++;
          array.raw[j] = array.raw[i];
        }
      }
      array.raw[i] = pivot;
      top++;
      if (i - lower < upper - i) {
        lostack[top] = i + 1;
        histack[top] = upper;
        upper = i - 1;
      } else {
        histack[top] = i - 1;
        lostack[top] = lower;
        lower = i + 1;
      }
    }
  }
}

/// NUMBER_LEN takes a state number and returns the number of digits in that
/// number.
int number_len(int state_no) {
  int num = 0;
  do {
    state_no /= 10;
    num++;
  } while (state_no != 0);
  return num;
}

/// This procedure takes two character strings as arguments: IN and OUT.
/// IN identifies a grammar symbol or name that is checked whether
/// it needs to be quoted. If so, the necessary quotes are added
/// as IN is copied into the space identified by OUT.
/// NOTE that it is assumed that IN and OUT do not overlap each other.
void restore_symbol(char *out, const char *in, char ormark, char escape) {
  const int len = strlen(in);
  if (len > 0) {
    if ((len == 1 && in[0] == ormark) || in[0] == escape || in[0] == '\'' || in[len - 1] == '\'' || (strchr(in, ' ') != NULL &&(in[0] != '<' || in[len - 1] != '>'))) {
      *out++ = '\'';
      while (*in != '\0') {
        if (*in == '\'') {
          *out++ = *in;
        }
        *out++ = *in++;
      }
      *out++ = '\'';
      *out = '\0';
      return;
    }
  }
  strcpy(out, in);
  if (out[0] == '\n') {
    // one of the special grammar symbols?
    out[0] = escape;
  }
}

/// PRINT_LARGE_TOKEN generates code to print a token that may exceed the
/// limit of its field.  The argument are LINE which is the symbol a varying
/// length character string, TOKEN which is the symbol to be printed, INDENT
/// which is a character string to be used as an initial prefix to indent the
/// output line, and LEN which indicates the maximum number of characters that
/// can be printed on a given line.  At the end of this process, LINE will
/// have the value of the remaining substring that can fit on the output line.
/// If a TOKEN is too large to be indented in a line, but not too large for
/// the whole line, we forget the indentation, and printed it. Otherwise, it
/// is "chapped up" and printed in pieces that are each indented.
void print_large_token(char *line, char *token, const char *indent, int len) {
  int toklen = strlen(token);
  if (toklen > len && toklen <= PRINT_LINE_SIZE - 1) {
    printf("\n%s", token);
    token = "";
    strcpy(line, indent);
  } else {
    char temp[SYMBOL_SIZE + 1];
    for (; toklen > len; toklen = strlen(temp)) {
      memcpy(temp, token, len);
      temp[len] = '\0';
      printf("\n%s", temp);
      strcpy(temp, token+len + 1);
      token = temp;
    }
    strcpy(line, indent);
    strcat(line, token);
  }
}

/// PRINT_ITEM takes as parameter an ITEM_NO which it prints.
void print_item(const int item_no, struct CLIOptions* cli_options, struct ruletab_type *rules, struct itemtab *item_table, ListInt rhs_sym, char *string_table, struct symno_type *symno) {
  char tempstr[PRINT_LINE_SIZE + 1];
  char line[PRINT_LINE_SIZE + 1];
  char tok[SYMBOL_SIZE + 1];
  // We first print the left hand side of the rule, leaving at least
  // 5 spaces in the output line to accommodate the equivalence symbol
  // "::=" surrounded by blanks on both sides.  Then, we print all the
  // terminal symbols on the right hand side up to but not including
  // the dot symbol.
  const int rule_no = item_table[item_no].rule_number;
  int symbol = rules[rule_no].lhs;
  restore_symbol(tok, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
  int len = PRINT_LINE_SIZE - 5;
  print_large_token(line, tok, "", len);
  strcat(line, " ::= ");
  const int offset = MIN(strlen(line) - 1, PRINT_LINE_SIZE / 2 - 1);
  len = PRINT_LINE_SIZE - (offset + 4);
  int sbd = rules[rule_no].rhs; /* symbols before dot */
  for (const int k = rules[rule_no].rhs + item_table[item_no].dot - 1; sbd <= k; sbd++) {
    symbol = rhs_sym.raw[sbd];
    restore_symbol(tok, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
    if (strlen(tok) + strlen(line) > PRINT_LINE_SIZE - 4) {
      printf("\n%s", line);
      fill_in(tempstr, offset, ' ');
      print_large_token(line, tok, tempstr, len);
    } else {
      strcat(line, tok);
    }
    strcat(line, " ");
  }
  // We now add a DOT "." to the output line and print the remaining
  // symbols on the right hand side.  If ITEM_NO is a complete item,
  // we also print the rule number.
  if (item_table[item_no].dot == 0 || item_table[item_no].symbol == empty) {
    strcpy(tok, ".");
  } else {
    strcpy(tok, " .");
  }
  strcat(line, tok);
  len = PRINT_LINE_SIZE - (offset + 1);
  for (int i = rules[rule_no].rhs + item_table[item_no].dot; /* symbols after dot*/ i <= rules[rule_no + 1].rhs - 1; i++) {
    symbol = rhs_sym.raw[i];
    restore_symbol(tok, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
    if (strlen(tok) + strlen(line) > PRINT_LINE_SIZE - 1) {
      printf("\n%s", line);
      fill_in(tempstr, offset, ' ');
      print_large_token(line, tok, tempstr, len);
    } else {
      strcat(line, tok);
    }
    strcat(line, " ");
  }
  if (item_table[item_no].symbol == empty) /* complete item */
  {
    snprintf(tok, sizeof(tok), " (%d)", rule_no);
    if (strlen(tok) + strlen(line) > PRINT_LINE_SIZE - 1) {
      printf("\n%s", line);
      fill_in(line, offset, ' ');
    }
    strcat(line, tok);
  }
  printf("\n%s", line);
}

/// PRINT_STATE prints all the items in a state.  NOTE that when single
/// productions are eliminated, certain items that were added in a state by
/// CLOSURE, will no longer show up in the output.  Example: If we have the
/// item [A ::= .B]  in a state, and the GOTO_REDUCE generated on B has been
/// replaced by say the GOTO or GOTO_REDUCE of A, the item above can no longer
/// be retrieved, since transitions in a given state are reconstructed from
/// the KERNEL and ADEQUATE items of the actions in the GOTO and SHIFT maps.
void print_state(const int state_no, struct CLIOptions* cli_options, struct node **adequate_item, struct SRTable* srt, struct lastats_type *lastats, struct statset_type *statset, struct node **in_stat, struct ruletab_type *rules, struct itemtab *item_table, ListInt rhs_sym, char *string_table, struct symno_type *symno, struct LAState* ls) {
  char buffer[PRINT_LINE_SIZE + 1];
  char line[PRINT_LINE_SIZE + 1];
  // ITEM_SEEN is used to construct sets of items, to help avoid
  // adding duplicates in a list.  Duplicates can occur because an
  // item from the kernel set will either be shifted on if it is not a
  // complete item, or it will be a member of the Complete_items set.
  // Duplicates can also occur because of the elimination of single
  // productions.
  ListBool state_seen = Allocate_bool_array2(ls->max_la_state + 1);
  ListBool item_seen = Allocate_bool_array2(ls->num_items + 1);
  ListInt item_list = allocateListInt(ls->num_items + 1);
  // INITIALIZATION -----------------------------------------------------------
  for for_each_state(state_no, ls) {
    state_seen.raw[state_no] = false;
  }
  for for_each_item(item_no, ls) {
    item_seen.raw[item_no] = false;
  }
  int kernel_size = 0;
  // END OF INITIALIZATION ----------------------------------------------------
  fill_in(buffer, PRINT_LINE_SIZE - (number_len(state_no) + 8 /* 8 = length("STATE") + 2 spaces + newline*/), '-');
  printf("\n\n\nSTATE %d %s", state_no, buffer);
  // Print the set of states that have transitions to STATE_NO.
  int n = 0;
  strcpy(line, "( ");
  struct node *q;
  for (bool end_node = (q = in_stat[state_no]) == NULL;
       !end_node;
       end_node = q == in_stat[state_no]) {
    // copy list of IN_STAT into array
    q = q->next;
    if (!state_seen.raw[q->value]) {
      state_seen.raw[q->value] = true;
      if (strlen(line) + number_len(q->value) > PRINT_LINE_SIZE - 2) {
        printf("\n%s", line);
        strcpy(line, "  ");
      }
      if (q->value != 0) {
        snprintf(buffer, sizeof(buffer), "%d ", q -> value);
        strcat(line, buffer);
      }
    }
  }
  strcat(line, ")");
  printf("\n%s\n", line);
  // Add the set of kernel items to the array ITEM_LIST, and mark all
  // items seen to avoid duplicates.
  for (q = statset[state_no].kernel_items; q != NULL; q = q->next) {
    kernel_size++;
    int item_no = q->value;
    item_list.raw[kernel_size] = item_no; /* add to array */
    item_seen.raw[item_no] = true; /* Mark as "seen" */
  }
  // Add the Complete Items to the array ITEM_LIST, and mark used.
  n = kernel_size;
  for (q = statset[state_no].complete_items; q != NULL; q = q->next) {
    int item_no = q->value;
    if (!item_seen.raw[item_no]) {
      item_seen.raw[item_no] = true; /* Mark as "seen" */
      item_list.raw[++n] = item_no;
    }
  }
  // Iterate over the shift map.  Shift-Reduce actions are identified
  // by a negative integer that indicates the rule in question , and
  // the associated item can be retrieved by indexing the array
  // ADEQUATE_ITEMS at the location of the rule.  For shift-actions, we
  // simply take the predecessor-items of all the items in the kernel
  // of the following state.
  // If the shift-action is a look-ahead shift, we check to see if the
  // look-ahead state contains shift actions, and retrieve the next
  // state from one of those shift actions.
  const struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
  for (int i = 1; i <= sh.size; i++) {
    int next_state = sh.map[i].action;
    while (next_state > ls->num_states) {
      const struct shift_header_type next_sh = srt->shift[lastats[next_state].shift_number];
      if (next_sh.size > 0) {
        next_state = next_sh.map[1].action;
      } else {
        next_state = 0;
      }
    }
    if (next_state == 0) {
      q = NULL;
    } else if (next_state < 0) {
      q = adequate_item[-next_state];
    } else {
      q = statset[next_state].kernel_items;
      if (q == NULL) /* single production state? */
        q = statset[next_state].complete_items;
    }
    for (; q != NULL; q = q->next) {
      int item_no = q->value - 1;
      if (!item_seen.raw[item_no]) {
        item_seen.raw[item_no] = true;
        item_list.raw[++n] = item_no;
      }
    }
  }
  // GOTOS and GOTO-REDUCES are analogous to SHIFTS and SHIFT-REDUCES.
  const struct goto_header_type go_to = statset[state_no].go_to;
  for (int i = 1; i <= go_to.size; i++) {
    if (go_to.map[i].action > 0) {
      q = statset[go_to.map[i].action].kernel_items;
      if (q == NULL) /* single production state? */
        q = statset[go_to.map[i].action].complete_items;
    } else {
      q = adequate_item[-go_to.map[i].action];
    }
    for (; q != NULL; q = q->next) {
      int item_no = q->value - 1;
      if (!item_seen.raw[item_no]) {
        item_seen.raw[item_no] = true;
        item_list.raw[++n] = item_no;
      }
    }
  }
  // Print the Kernel items.  If there are any closure items, skip a
  // line, sort then, then print them.  The kernel items are in sorted
  // order.
  for (int item_no = 1; item_no <= kernel_size; item_no++) {
    print_item(item_list.raw[item_no], cli_options, rules, item_table, rhs_sym, string_table, symno);
  }
  if (kernel_size < n) {
    printf("\n");
    qcksrt(item_list, kernel_size + 1, n);
    for (int item_no = kernel_size + 1; item_no <= n; item_no++) {
      print_item(item_list.raw[item_no], cli_options, rules, item_table, rhs_sym, string_table, symno);
    }
  }
  ffree(item_list.raw);
  ffree(item_seen.raw);
  ffree(state_seen.raw);
}

void nospace() {
  fprintf(stderr, "*** Cannot allocate space ***\n");
  exit(12);
}
// endregion


















// region partset
/// This procedure, PARTSET, is invoked to apply a heuristic of the
/// Optimal Partitioning algorithm to a COLLECTION of subsets. The
/// size of each subset in COLLECTION is passed in a parallel vector:
/// ELEMENT_SIZE. Let SET_SIZE be the length of the bit_strings used
/// to represent the subsets in COLLECTION, the universe of the
/// subsets is the set of integers: [1..SET_SIZE].
/// The third argument, LIST, is a vector identifying the order in
/// which the subsets in COLLECTION must be processed when they are
/// output.
/// The last two arguments, START and STACK are output parameters.
/// We recall that the output of Optimal Partitioning is two vectors:
/// a BASE vector and a RANGE vector... START is the base vector.
/// It is used to indicate the starting position in the range
/// vector for each subset.  When a subset shares elements with
/// another subset, this is indicated by in index in START being
/// negative. START also contains an extra "fence" element.  I.e.,
/// it has one more element than COLLECTION.
/// STACK is a vector used to construct a partition of the elements
/// of COLLECTION. That partition is used later (in ctabs or tabutil)
/// to output the final range vector...
///
///
/// We first merge all sets that are identical. A hash table is used
/// to keep track of subsets that have already been seen.
/// DOMAIN_TABLE is used as the base of the hash table. DOMAIN_LINK
/// is used to link subsets that collided.
///
/// The next step is to partition the unique subsets in the hash
/// table based on the number of elements they contain. The vector
/// PARTITION is used for that purpose.
///
/// Finally, we attempt to overlay as many subsets as possible by
/// performing the following steps:
///
/// 1) Choose a base set in the partition with the largest subsets
///    and push it into a stack. (using the vector STACK)
///
/// 2) Iterate over the remaining non_empty elements of the partitions
///    in decreasing order of the size of the subsets contained in the
///    element in question. If there exists a subset in the element
///    which is a subset of the subset on top of the stack, currently
///    being constructed, remove it from the partition, and push it
///    into the stack. Repeat step 2 until the partition is empty.
void partset(JBitset collection, ListInt element_size, ListInt list, ListInt start, ListInt stack, long set_size, const struct LAState* ls) {
  long collection_size;
  collection_size = ls->num_states;
  const long bctype = collection.size;
  ListInt size_list = allocateListInt(set_size + 1);
  ListInt partition = allocateListInt(set_size + 1);
  ListInt domain_link = allocateListInt(collection_size + 1);
  ListInt head = allocateListInt(collection_size + 1);
  ListInt next = allocateListInt(collection_size + 1);
  ListBool is_a_base = Allocate_bool_array2(collection_size + 1);
  JBitset temp_set;
  calloc0_set(temp_set, 1, bctype);
  // DOMAIN_TABLE is the base of a hash table used to compute the set
  // of unique subsets in COLLECTION. Collisions are resolved by links
  // which are implemented in DOMAIN_LINK.
  // HEAD is an array containing either the value OMEGA which
  // indicates that the corresponding subset in COLLECTION is
  // identical to another set, or it contains the "root" of a list of
  // subsets that are identical.  The elements of the list are placed
  // in the array NEXT.  When a state is at te root of a list, it is
  // used as a representative of that list.
  short domain_table[STATE_TABLE_SIZE];
  for (int i = 0; i <= STATE_TABLE_UBOUND; i++) {
    domain_table[i] = NIL;
  }
  // We now iterate over the states and attempt to insert each
  // domain set into the hash table...
  for (int index = 1; index <= collection_size; index++) {
    unsigned long hash_address = 0;
    for (int i = 0; i < bctype; i++) {
      hash_address += collection.raw[index * bctype + i];
    }
    hash_address %= STATE_TABLE_SIZE;
    //  Next, we search the hash table to see if the subset was
    // already inserted in it. If we find such a subset, we simply
    // add INDEX to a list associated with the subset found and
    // mark it as a duplicate by setting the head of its list to
    // OMEGA.  Otherwise, we have a new set...
    for (int i = domain_table[hash_address]; i != NIL; i = domain_link.raw[i]) {
      if (equal_sets(collection, index, collection, i, bctype)) {
        head.raw[index] = OMEGA;
        next.raw[index] = head.raw[i];
        head.raw[i] = index;
        goto continu;
      }
    }
    //  ...Subset indicated by INDEX not previously seen. Insert
    // it into the hash table, and initialize a new list with it.
    domain_link.raw[index] = domain_table[hash_address];
    domain_table[hash_address] = index;
    head.raw[index] = NIL; /* Start a new list */
  continu: ;
  }
  // We now partition all the unique sets in the hash table
  // based on the number of elements they contain...
  // NEXT is also used to construct these lists.  Recall that
  // the unique elements are roots of lists. Hence, their
  // corresponding HEAD elements are used, but their
  // corresponding NEXT field is still unused.
  for (int i = 0; i <= set_size; i++) {
    partition.raw[i] = NIL;
  }
  for (int index = 1; index <= collection_size; index++) {
    if (head.raw[index] != OMEGA) {
      /* Subset representative */
      int size = element_size.raw[index];
      next.raw[index] = partition.raw[size];
      partition.raw[size] = index;
    }
  }
  //     ...Construct a list of all the elements of PARTITION
  // that are not empty.  Only elements in this list will be
  // considered for subset merging later ...
  // Note that since the elements of PARTITION are added to
  // the list in ascending order and in stack-fashion, the
  // resulting list will be sorted in descending order.
  int size_root = NIL;
  for (int i = 0; i <= set_size; i++) {
    if (partition.raw[i] != NIL) {
      size_list.raw[i] = size_root;
      size_root = i;
    }
  }
  // Merge subsets that are mergeable using heuristic described
  // above.  The vector IS_A_BASE is used to mark subsets
  // chosen as bases.
  for (int i = 0; i <= collection_size; i++) {
    is_a_base.raw[i] = false;
  }
  for (int size = size_root; size != NIL; size = size_list.raw[size]) {
    // For biggest partition there is
    for (int base_set = partition.raw[size]; base_set != NIL; base_set = next.raw[base_set]) {
      // For each set in it...
      // Mark the state as a base state, and initialize
      // its stack.  The list representing the stack will
      // be circular...
      is_a_base.raw[base_set] = true;
      stack.raw[base_set] = base_set;
      // For remaining elements in partitions in decreasing order...
      for (int next_size = size_list.raw[size]; next_size != NIL; next_size = size_list.raw[next_size]) {
        int previous = NIL; /* mark head of list */
        // Iterate over subsets in the partition until we
        // find one that is a subset of the subset on top
        // of the stack.  If none is found, we go on to
        // the next element of the partition. Otherwise,
        // we push the new subset on top of the stack and
        // go on to the next element of the partition.
        // INDEX identifies the state currently on top
        // of the stack.
        for (int subset = partition.raw[next_size]; subset != NIL; previous = subset, subset = next.raw[subset]) {
          int index = stack.raw[base_set];
          B_ASSIGN_SET(temp_set, 0, collection, index, bctype);
          B_SET_UNION(temp_set, 0, collection, subset, bctype);
          // SUBSET is a subset of INDEX?
          if (equal_sets(temp_set, 0, collection, index, bctype)) {
            if (previous == NIL) {
              partition.raw[next_size] = next.raw[subset];
            } else {
              next.raw[previous] = next.raw[subset];
            }
            stack.raw[subset] = stack.raw[base_set];
            stack.raw[base_set] = subset;
            break; /* for (subset = partition[next_size]... */
          }
        }
      }
    }
  }
  // Iterate over the states in the order in which they are to
  // be output, and assign an offset location to each state.
  // Notice that an extra element is added to the size of each
  // base subset for the "fence" element.
  int offset = 1;
  for (int i = 1; i <= collection_size; i++) {
    int base_set = list.raw[i];
    if (is_a_base.raw[base_set]) {
      start.raw[base_set] = offset;
      // Assign the same offset to each subset that is
      // identical to the BASE_SET subset in question. Also,
      // mark the fact that this is a copy by using the negative
      // value of the OFFSET.
      for (int index = head.raw[base_set]; index != NIL; index = next.raw[index]) {
        start.raw[index] = -start.raw[base_set];
      }
      int size = element_size.raw[base_set] + 1;
      offset += size;
      // Now, assign offset values to each subset of the
      // BASE_SET. Once again, we mark them as sharing elements
      // by using the negative value of the OFFSET.
      // Recall that the stack is constructed as a circular
      // list.  Therefore, its end is reached when we go back
      // to the root... In this case, the root is already
      // processed, so we stop when we reach it.
      for (int index = stack.raw[base_set]; index != base_set; index = stack.raw[index]) {
        size = element_size.raw[index] + 1;
        start.raw[index] = -(offset - size);
        // INDEX identifies a subset of BASE_SET. Assign the
        // same offset as INDEX to each subset j that is
        // identical to the subset INDEX.
        for (int j = head.raw[index]; j != NIL; j = next.raw[j])
          start.raw[j] = start.raw[index];
      }
    }
  }
  start.raw[collection_size + 1] = offset;
  ffree(size_list.raw);
  ffree(partition.raw);
  ffree(domain_link.raw);
  ffree(head.raw);
  ffree(next.raw);
  ffree(is_a_base.raw);
  ffree(temp_set.raw);
}
// endregion










// region mkfirst
long NEXT_RULE_SIZE(struct LAState* ls) {
  return ls->num_rules + 1;
}

long LAST_RHS_INDEX(const int rule_no, struct ruletab_type *rules) {
  return rules[rule_no + 1].rhs - 1;
}

struct f_element_type {
  short suffix_root;
  short suffix_tail;
  short link;
};

///   This procedure tries to advance the RHS_START pointer.  If the current
/// symbol identified by the RHS_START element is a bad non-terminal it
/// returns FALSE.  Otherwise, the whole right-hand side is traversed, and it
/// returns the value TRUE.
bool is_terminal_rhs(ListInt rhs_start, const bool *produces_terminals, const int rule_no, const struct ruletab_type *rules, ListInt rhs_sym, struct LAState* ls) {
  for (; rhs_start.raw[rule_no] <= rules[rule_no + 1].rhs - 1; rhs_start.raw[rule_no]++) {
    const int symbol = rhs_sym.raw[rhs_start.raw[rule_no]];
    if (IS_A_NON_TERMINAL(symbol, ls)) {
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
void check_non_terminals(const struct CLIOptions *cli_options, ListInt lhs_rule, ListInt next_rule, ListInt nt_list, struct ruletab_type *rules, ListInt rhs_sym, char *string_table, struct symno_type *symno, struct LAState* ls) {
  bool changed = true;
  ListInt rhs_start = allocateListInt(NEXT_RULE_SIZE(ls));
  ListBool produces_terminals = Allocate_bool_array2(ls->num_non_terminals);
  produces_terminals.raw -= ls->num_terminals + 1;
  // First, mark all non-terminals as not producing terminals. Then
  // initialize RHS_START. RHS_START is a mapping from each rule in
  // the grammar into the next symbol in its right-hand side that
  // has not yet proven to be a symbol that generates terminals.
  for for_each_nt_fw(nt, ls) {
    produces_terminals.raw[nt] = false;
  }
  produces_terminals.raw[accept_image] = true;
  for for_each_rule_fw(rule_no, ls) {
    rhs_start.raw[rule_no] = rules[rule_no].rhs;
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
    for for_each_nt_fw(nt, ls) {
      int rule_no;
      for (bool end_node = (rule_no = lhs_rule.raw[nt]) == NIL; !produces_terminals.raw[nt] && !end_node; end_node = rule_no == lhs_rule.raw[nt]) {
        rule_no = next_rule.raw[rule_no];
        if (is_terminal_rhs(rhs_start, produces_terminals.raw, rule_no, rules, rhs_sym, ls)) {
          changed = true;
          produces_terminals.raw[nt] = true;
        }
      }
    }
  }
  // Construct a list of all non-terminals that do not generate
  // terminal strings.
  int nt_root = NIL;
  int nt_last;
  for for_each_nt_fw(nt, ls) {
    if (!produces_terminals.raw[nt]) {
      if (nt_root == NIL) {
        nt_root = nt;
      } else {
        nt_list.raw[nt_last] = nt;
      }
      nt_last = nt;
    }
  }
  // If there are non-terminal symbols that do not generate
  // terminal strings, print them out and stop the program.
  if (nt_root != NIL) {
    char line[PRINT_LINE_SIZE + 1];
    nt_list.raw[nt_last] = NIL; /* mark end of list */
    strcpy(line, "*** ERROR: The following Non-terminal");
    if (nt_list.raw[nt_root] == NIL) {
      strcat(line, " does not generate any terminal strings: ");
    } else {
      strcat(line, "s do not generate any terminal strings: ");
      PRNT(line);
      strcpy(line, "        "); /* 8 spaces */
    }
    for (int symbol = nt_root; symbol != NIL; symbol = nt_list.raw[symbol]) {
      char tok[SYMBOL_SIZE + 1];
      restore_symbol(tok, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
      if (strlen(line) + strlen(tok) > PRINT_LINE_SIZE - 1) {
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
  produces_terminals.raw += ls->num_terminals + 1;
  ffree(produces_terminals.raw);
  ffree(rhs_start.raw);
}

void no_rules_produced(const struct CLIOptions *cli_options, ListInt lhs_rule, ListInt nt_list, char *string_table, struct symno_type *symno, struct LAState* ls) {
  // Build a list of all non-terminals that do not produce any rules.
  int nt_root = NIL;
  int nt_last;
  for for_each_nt_fw(symbol, ls) {
    if (lhs_rule.raw[symbol] == NIL) {
      if (nt_root == NIL) {
        nt_root = symbol;
      } else {
        nt_list.raw[nt_last] = symbol;
      }
      nt_last = symbol;
    }
  }
  // If the list of non-terminals that do not produce any rules
  // is not empty, signal error and stop.
  if (nt_root != NIL) {
    char line[PRINT_LINE_SIZE + 1];
    nt_list.raw[nt_last] = NIL;
    if (nt_list.raw[nt_root] == NIL) {
      PRNTERR("The following Non-terminal does not produce any rules: ");
    } else {
      PRNTERR("The following Non-terminals do not produce any rules: ");
    }
    strcpy(line, "        ");
    for (int symbol = nt_root; symbol != NIL; symbol = nt_list.raw[symbol]) {
      char tok[SYMBOL_SIZE + 1];
      restore_symbol(tok, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
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

/// This function computes the closure of a non-terminal LHS_SYMBOL passed
/// to it as an argument using the digraph algorithm.
/// The closure of a non-terminal A is the set of all non-terminals Bi that
/// can directly or indirectly start a string generated by A.
/// I.e., A *::= Bi X where X is an arbitrary string.
void compute_closure_REC(const long lhs_symbol, ListInt stack, ListInt index_of, struct ProduceTop *topp, ListInt lhs_rule, ListInt next_rule, struct node **closure, struct ruletab_type *rules, ListInt rhs_sym, struct LAState* ls) {
  ListInt nont_list = allocateListInt(ls->num_non_terminals);
  nont_list.raw -= ls->num_terminals + 1; /* Temporary direct        */
  // access set for closure.
  stack.raw[++topp->top] = lhs_symbol;
  const int indx = topp->top;
  index_of.raw[lhs_symbol] = indx;
  for for_each_nt_fw(i, ls) {
    nont_list.raw[i] = OMEGA;
  }
  nont_list.raw[lhs_symbol] = NIL;
  long nt_root = lhs_symbol;
  closure[lhs_symbol] = NULL; /* Permanent closure set. Linked list */
  long rule_no;
  for (bool end_node = (rule_no = lhs_rule.raw[lhs_symbol]) == NIL; !end_node; /* Iterate over all rules of LHS_SYMBOL */ end_node = rule_no == lhs_rule.raw[lhs_symbol]) {
    rule_no = next_rule.raw[rule_no];
    int symbol = RHS_SIZE(rule_no, rules) == 0 ? empty : rhs_sym.raw[rules[rule_no].rhs];
    if (IS_A_NON_TERMINAL(symbol, ls)) {
      if (nont_list.raw[symbol] == OMEGA) {
        /* if first time seen */
        if (index_of.raw[symbol] == OMEGA) {
          compute_closure_REC(symbol, stack, index_of, topp, lhs_rule, next_rule, closure, rules, rhs_sym, ls);
        }
        index_of.raw[lhs_symbol] = MIN(index_of.raw[lhs_symbol], index_of.raw[symbol]);
        nont_list.raw[symbol] = nt_root;
        nt_root = symbol;
        // add closure[symbol] to closure of LHS_SYMBOL.
        struct node *q;
        for (end_node = (q = closure[symbol]) == NULL;
             !end_node;
             end_node = q == closure[symbol]) {
          q = q->next;
          if (nont_list.raw[q->value] == OMEGA) {
            nont_list.raw[q->value] = nt_root;
            nt_root = q->value;
          }
        }
      }
    }
  }
  for (; nt_root != lhs_symbol; nt_root = nont_list.raw[nt_root]) {
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
  if (index_of.raw[lhs_symbol] == indx) {
    for (int symbol = stack.raw[topp->top]; symbol != lhs_symbol; symbol = stack.raw[--topp->top]) {
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
      index_of.raw[symbol] = INFINITY;
    }
    index_of.raw[lhs_symbol] = INFINITY;
    topp->top--;
  }
  nont_list.raw += ls->num_terminals + 1;
  ffree(nont_list.raw);
}

/// This procedure tries to advance the RHS_START pointer.  If the current
/// symbol identified by the RHS_START element is a terminal it returns FALSE
/// to indicate that it cannot go any further.  If it encounters a  non-null-
/// label non-terminal, it also returns FALSE. Otherwise, the whole right-hand
/// side is consumed, and it returns the value TRUE.
bool is_nullable_rhs(ListInt rhs_start, const int rule_no, ListBool null_nt, struct ruletab_type *rules, ListInt rhs_sym, struct LAState* ls) {
  for (; rhs_start.raw[rule_no] <= rules[rule_no + 1].rhs - 1; rhs_start.raw[rule_no]++) {
    const int symbol = rhs_sym.raw[rhs_start.raw[rule_no]];
    if (IS_A_TERMINAL_L(symbol, ls) || !null_nt.raw[symbol]) {
      return false;
    }
  }
  return true;
}

/// This subroutine computes FIRST(NT) for some non-terminal NT using the
/// digraph algorithm.
/// FIRST(NT) is the set of all terminals Ti that may start a string generated
/// by NT. That is, NT *::= Ti X where X is an arbitrary string.
void compute_first_REC(const int nt, struct DetectedSetSizes *dss, ListInt stack, ListInt index_of, struct ProduceTop *topp, JBitset nt_first, ListInt lhs_rule, ListInt next_rule, ListBool null_nt, struct ruletab_type *rules, ListInt rhs_sym, struct LAState* ls) {
  JBitset temp_set;
  calloc0_set(temp_set, 1, dss->term_set_size)
  stack.raw[++topp->top] = nt;
  const int indx = topp->top;
  index_of.raw[nt] = indx;
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
  for (bool end_node = (rule_no = lhs_rule.raw[nt]) == NIL; !end_node; /* Iterate over all rules produced by NT */ end_node = rule_no == lhs_rule.raw[nt]) {
    rule_no = next_rule.raw[rule_no];
    bool blocked = false;
    for for_each_rhs(i, rule_no, rules) {
      int symbol = rhs_sym.raw[i];
      if (IS_A_NON_TERMINAL(symbol, ls)) {
        if (index_of.raw[symbol] == OMEGA) {
          compute_first_REC(symbol, dss, stack, index_of, topp, nt_first, lhs_rule, next_rule, null_nt, rules, rhs_sym, ls);
        }
        index_of.raw[nt] = MIN(index_of.raw[nt], index_of.raw[symbol]);
        ASSIGN_SET(temp_set, 0, nt_first, symbol);
        RESET_BIT(temp_set, empty);
        SET_UNION(nt_first, nt, temp_set, 0);
        blocked = !null_nt.raw[symbol];
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
  if (index_of.raw[nt] == indx) {
    for (int symbol = stack.raw[topp->top]; symbol != nt; symbol = stack.raw[--topp->top]) {
      ASSIGN_SET(nt_first, symbol, nt_first, nt);
      index_of.raw[symbol] = INFINITY;
    }
    index_of.raw[nt] = INFINITY;
    topp->top--;
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
short first_map(const int root, const int tail, struct ProduceTop *topp, struct f_element_type *first_element, ListInt first_table, ListInt rhs_sym) {
  for (int i = first_table.raw[rhs_sym.raw[root]]; i != NIL; i = first_element[i].link) {
    int k;
    int jj;
    for (jj = root + 1, k = first_element[i].suffix_root + 1; jj <= tail && k <= first_element[i].suffix_tail; jj++, k++) {
      if (rhs_sym.raw[jj] != rhs_sym.raw[k])
        break;
    }
    if (jj > tail && k > first_element[i].suffix_tail) {
      return i;
    }
  }
  topp->top++;
  first_element[topp->top].suffix_root = root;
  first_element[topp->top].suffix_tail = tail;
  first_element[topp->top].link = first_table.raw[rhs_sym.raw[root]];
  first_table.raw[rhs_sym.raw[root]] = topp->top;
  return topp->top;
}

/// COMPUTE_FOLLOW computes FOLLOW[nt] for some non-terminal NT using the
/// digraph algorithm.  FOLLOW[NT] is the set of all terminals Ti that
/// may immediately follow a string X generated by NT. I.e., if NT *::= X
/// then X Ti is a valid substring of a class of strings that may be
/// recognized by the language.
void compute_follow_REC(const int nt, struct DetectedSetSizes *dss, ListInt stack, ListInt index_of, struct ProduceTop *topp, JBitset follow, ListInt next_item, ListInt nt_items, JBitset first, struct ruletab_type *rules, struct itemtab *item_table) {
  JBitset temp_set;
  calloc0_set(temp_set, 1, dss->term_set_size);
  // FOLLOW[NT] was initialized to 0 for all non-terminals.
  stack.raw[++topp->top] = nt;
  const int indx = topp->top;
  index_of.raw[nt] = indx;
  for (int item_no = nt_items.raw[nt]; item_no != NIL; item_no = next_item.raw[item_no]) {
    // iterate over all items of NT
    ASSIGN_SET(temp_set, 0, first, item_table[item_no].suffix_index);
    if (IS_ELEMENT(temp_set, empty)) {
      RESET_BIT(temp_set, empty);
      const int rule_no = item_table[item_no].rule_number;
      int lhs_symbol = rules[rule_no].lhs;
      if (index_of.raw[lhs_symbol] == OMEGA) {
        compute_follow_REC(lhs_symbol, dss, stack, index_of, topp, follow, next_item, nt_items, first, rules, item_table);
      }
      SET_UNION(follow, nt, follow, lhs_symbol);
      index_of.raw[nt] = MIN(index_of.raw[nt], index_of.raw[lhs_symbol]);
    }
    SET_UNION(follow, nt, temp_set, 0);
  }
  if (index_of.raw[nt] == indx) {
    for (int lhs_symbol = stack.raw[topp->top]; lhs_symbol != nt; lhs_symbol = stack.raw[--topp->top]) {
      ASSIGN_SET(follow, lhs_symbol, follow, nt);
      index_of.raw[lhs_symbol] = INFINITY;
    }
    index_of.raw[nt] = INFINITY;
    topp->top--;
  }
  ffree(temp_set.raw);
}

/// MKFIRST constructs the FIRST and FOLLOW maps, the CLOSURE map,
/// ADEQUATE_ITEM and ITEM_TABLE maps and all other basic maps.
struct DetectedSetSizes mkbasic(struct CLIOptions *cli_options, JBitset nt_first, JBitset* first, struct FirstDeps* fd, struct ruletab_type *rules, ListInt rhs_sym, struct itemtab **item_tablep, char *string_table, struct symno_type *symno, struct LAState* ls) {
  struct DetectedSetSizes dss = {
    .non_term_set_size = ls->num_non_terminals / SIZEOF_BC + (ls->num_non_terminals % SIZEOF_BC ? 1 : 0),
    .term_set_size = ls->num_terminals / SIZEOF_BC + (ls->num_terminals % SIZEOF_BC ? 1 : 0),
    .null_nt = NULL,
  };
  // Allocate various arrays.
  ListInt lhs_rule = allocateListInt(ls->num_non_terminals);
  lhs_rule.raw -= ls->num_terminals + 1;
  ListInt next_rule = allocateListInt(NEXT_RULE_SIZE(ls));
  // NT_FIRST is used to construct a mapping from non-terminals to the
  // set of terminals that may appear first in a string derived from
  // the non-terminal.
  calloc0_set(nt_first, ls->num_non_terminals, dss.term_set_size);
  nt_first.raw -= (ls->num_terminals + 1) * dss.term_set_size;
  ListInt next_item = allocateListInt(ls->num_items + 1);
  ListInt nt_items = allocateListInt(ls->num_non_terminals);
  nt_items.raw -= ls->num_terminals + 1;
  ListInt nt_list = allocateListInt(ls->num_non_terminals);
  nt_list.raw -= ls->num_terminals + 1;
  struct f_element_type *first_element;
  calloc0p(&first_element, ls->num_items + 1, struct f_element_type);
  calloc0p(item_tablep, ls->num_items + 1, struct itemtab);
  for for_each_nt_fw(symbol, ls) {
    lhs_rule.raw[symbol] = NIL;
  }
  // In this loop, we construct the LHS_RULE map which maps
  // each non-terminal symbol into the set of rules it produces
  for for_each_rule_fw(rule_no, ls) {
    int symbol = rules[rule_no].lhs;
    if (lhs_rule.raw[symbol] == NIL) {
      next_rule.raw[rule_no] = rule_no;
    } else {
      next_rule.raw[rule_no] = next_rule.raw[lhs_rule.raw[symbol]];
      next_rule.raw[lhs_rule.raw[symbol]] = rule_no;
    }
    lhs_rule.raw[symbol] = rule_no;
  }
  // Check if there are any non-terminals that do not produce any rules.
  no_rules_produced(cli_options, lhs_rule, nt_list, string_table, symno, ls);
  // Construct the CLOSURE map of non-terminals.
  calloc0p(&(fd->closure), ls->num_non_terminals, struct node *);
  fd->closure -= ls->num_terminals + 1;
  {
    ListInt stack = allocateListInt(ls->num_non_terminals + 1);
    ListInt index_of = allocateListInt(ls->num_non_terminals);
    index_of.raw -= ls->num_terminals + 1;
    for for_each_nt_fw(symbol, ls) {
      index_of.raw[symbol] = OMEGA;
    }
    struct ProduceTop topp = {.top = 0};
    for for_each_nt_fw(nt, ls) {
      if (index_of.raw[nt] == OMEGA) {
        compute_closure_REC(nt, stack, index_of, &topp, lhs_rule, next_rule, fd->closure, rules, rhs_sym, ls);
      }
    }
    ffree(stack.raw);
    index_of.raw += ls->num_terminals + 1;
    ffree(index_of.raw);
  }
  // Construct the NULL_NT map for non-terminals.
  // A non-terminal B is said to be nullable if either:
  //    B -> %empty  or  B -> B1 B2 B3 ... Bk  where Bi is
  //                         nullable for 1 <= i <= k
  dss.null_nt = Allocate_bool_array2(ls->num_non_terminals);
  dss.null_nt.raw -= ls->num_terminals + 1;
  // Calculate nullables.
  {
    /// This procedure computes the set of non-terminal symbols that can
    /// generate the empty string. Such non-terminals are said to be nullable.
    ///
    /// A non-terminal "A" can generate empty if the grammar in question contains
    /// a rule:
    ///          A ::= B1 B2 ... Bn     n >= 0,  1 <= i <= n
    /// and Bi, for all i, is a nullable non-terminal.
    bool changed = true;
    ListInt rhs_start = allocateListInt(NEXT_RULE_SIZE(ls));
    // First, mark all non-terminals as non-nullable. Then initialize
    // RHS_START. RHS_START is a mapping from each rule in the grammar
    // into the next symbol in its right-hand side that has not yet
    // proven to be nullable.
    for for_each_nt_fw(nt, ls) {
      dss.null_nt.raw[nt] = false;
    }
    for for_each_rule_fw(rule_no, ls) {
      rhs_start.raw[rule_no] = rules[rule_no].rhs;
    }
    // We now iterate over the rules and try to advance the RHS_START
    // pointer through each right-hand side as far as we can. If one or
    // more non-terminals are found to be nullable, they are marked
    // as such and the process is repeated.
    //
    // If we go through all the rules and no new non-terminal is found
    // to be nullable then we stop and return.
    //
    // Note that for each iteration, only rules associated with
    // non-terminals that are non-nullable are considered. Further,
    // as soon as a non-terminal is found to be nullable, the
    // remaining rules associated with it are not considered. I.e.,
    // we quit the inner loop.
    while (changed) {
      changed = false;
      for for_each_nt_fw(nt, ls) {
        int rule_no;
        for (bool end_node = (rule_no = lhs_rule.raw[nt]) == NIL; !dss.null_nt.raw[nt] && !end_node; end_node = rule_no == lhs_rule.raw[nt]) {
          rule_no = next_rule.raw[rule_no];
          if (is_nullable_rhs(rhs_start, rule_no, dss.null_nt, rules, rhs_sym, ls)) {
            changed = true;
            dss.null_nt.raw[nt] = true;
          }
        }
      }
    }
    ffree(rhs_start.raw);
  }
  // Construct the FIRST map for non-terminals and also a list
  // of non-terminals whose first set is empty.
  {
    ListInt stack = allocateListInt(ls->num_non_terminals + 1);
    ListInt index_of = allocateListInt(ls->num_non_terminals);
    index_of.raw -= ls->num_terminals + 1;
    struct ProduceTop topp = {.top = 0};
    for for_each_nt_fw(symbol, ls) {
      index_of.raw[symbol] = OMEGA;
    }
    topp.top = 0;
    for for_each_nt_fw(nt, ls) {
      if (index_of.raw[nt] == OMEGA) {
        compute_first_REC(nt, &dss, stack, index_of, &topp, nt_first, lhs_rule, next_rule, dss.null_nt, rules, rhs_sym, ls);
      }
    }
    ffree(stack.raw);
    index_of.raw += ls->num_terminals + 1;
    ffree(index_of.raw);
  }
  // Since every input source will be followed by the EOFT
  // symbol, FIRST[accept_image] cannot contain empty but
  // instead must contain the EOFT symbol.
  if (dss.null_nt.raw[accept_image]) {
    dss.null_nt.raw[accept_image] = false;
    RESET_BIT_IN(nt_first, accept_image, empty);
    SET_BIT_IN(nt_first, accept_image, eoft_image);
  }






  // TODO === check nonterminals ===
  // Check whether there are any non-terminals that do not
  // generate any terminal strings. If so, signal error and stop.
  check_non_terminals(cli_options, lhs_rule, next_rule, nt_list, rules, rhs_sym, string_table, symno, ls);






  long num_first_sets;
  ListInt first_item_of = allocateListInt(NEXT_RULE_SIZE(ls));
  {
    ListInt stack = allocateListInt(ls->num_non_terminals + 1);
    ListInt index_of = allocateListInt(ls->num_non_terminals);
    index_of.raw -= ls->num_terminals + 1;
    struct ProduceTop topp = {.top = 0};
    // Construct the ITEM_TABLE, FIRST_ITEM_OF, and NT_ITEMS maps.
    ListInt first_table = allocateListInt(ls->num_symbols + 1);
    /* Initialize FIRST_TABLE to NIL */
    for for_each_symbol(symbol, ls) {
      first_table.raw[symbol] = NIL;
    }
    topp.top = 1;
    const int first_of_empty = topp.top;
    first_element[first_of_empty].suffix_root = 1;
    first_element[first_of_empty].suffix_tail = 0;
    for for_each_nt_fw(symbol, ls) {
      nt_items.raw[symbol] = NIL;
    }
    int item_no = 0;
    (*item_tablep)[item_no].rule_number = 0;
    (*item_tablep)[item_no].symbol = empty;
    (*item_tablep)[item_no].dot = 0;
    (*item_tablep)[item_no].suffix_index = NIL;
    for for_each_rule_fw(rule_no, ls) {
      first_item_of.raw[rule_no] = item_no + 1;
      int j = 0;
      const int k = LAST_RHS_INDEX(rule_no, rules);
      for for_each_rhs(i, rule_no, rules) {
        item_no++;
        int symbol = rhs_sym.raw[i];
        (*item_tablep)[item_no].rule_number = rule_no;
        (*item_tablep)[item_no].symbol = symbol;
        (*item_tablep)[item_no].dot = j;
        if (cli_options->lalr_level > 1 || IS_A_NON_TERMINAL(symbol, ls) || symbol == error_image) {
          if (i == k) {
            (*item_tablep)[item_no].suffix_index = first_of_empty;
          } else {
            (*item_tablep)[item_no].suffix_index = first_map(i + 1, k, &topp, first_element, first_table, rhs_sym);
          }
        } else {
          (*item_tablep)[item_no].suffix_index = NIL;
        }
        if (IS_A_NON_TERMINAL(symbol, ls)) {
          next_item.raw[item_no] = nt_items.raw[symbol];
          nt_items.raw[symbol] = item_no;
        }
        j++;
      }
      (*item_tablep)[++item_no].rule_number = rule_no;
      (*item_tablep)[item_no].symbol = empty;
      (*item_tablep)[item_no].dot = j;
      (*item_tablep)[item_no].suffix_index = NIL;
    }
    // We now compute the first set for all suffixes that were
    // inserted in the FIRST_TABLE map. There are TOP such suffixes
    // Extra space is also allocated to compute the first set for
    // suffixes whose left-hand side is the ACCEPT non-terminal.
    // The first set for these suffixes are the sets needed to
    // construct the FOLLOW map and compute look-ahead sets. They
    // are placed in the FIRST table in the range 1..NUM_FIRST_SETS
    // The first element in the FIRST table contains the first sets
    // for the empty sequence.
    num_first_sets = topp.top;
    int rule_no;
    for (bool end_node = (rule_no = lhs_rule.raw[accept_image]) == NIL; !end_node; end_node = rule_no == lhs_rule.raw[accept_image]) {
      rule_no = next_rule.raw[rule_no];
      num_first_sets++;
    }
    calloc0_set_fn(first, num_first_sets + 1, dss.term_set_size);
    for (int i = 1; i <= topp.top; i++) {
      int root = first_element[i].suffix_root;
      int tail = first_element[i].suffix_tail;
      int index = i;
      // S_FIRST takes as argument, two pointers: ROOT and TAIL to a sequence of
      // symbols in the vector RHS, and INDEX which is the index of a first set.
      // It computes the set of all terminals that can appear as the first symbol
      // in the sequence and places the result in the FIRST set indexable by INDEX.
      int symbol = root > tail ? empty : rhs_sym.raw[root];
      if (IS_A_TERMINAL_L(symbol, ls)) {
        INIT_BITSET(*first, index);
        SET_BIT_IN(*first, index, symbol); /* add it to set */
      } else {
        ASSIGN_SET(*first, index, nt_first, symbol);
      }
      for (int i = root + 1; i <= tail && IS_IN_SET(*first, index, empty); i++) {
        symbol = rhs_sym.raw[i];
        RESET_BIT_IN(*first, index, empty); /* remove EMPTY */
        if (IS_A_TERMINAL_L(symbol, ls)) {
          SET_BIT_IN(*first, index, symbol); /* add it to set */
        } else {
          SET_UNION(*first, index, nt_first, symbol);
        }
      }
    }
    rule_no = lhs_rule.raw[accept_image];
    for (int i = topp.top + 1; i <= num_first_sets; i++) {
      rule_no = next_rule.raw[rule_no];
      item_no = first_item_of.raw[rule_no];
      (*item_tablep)[item_no].suffix_index = i;
      INIT_BITSET(*first, i);
      SET_BIT_IN(*first, i, eoft_image);
    }
    // If the READ/REDUCE option is on, we precalculate the kernel
    // of the final states which simply consists of the last item
    // in  the corresponding rule.  Rules with the ACCEPT
    // non-terminal as their left-hand side are not considered
    // to let the Accept action remain as a Reduce action
    // instead of a Goto/Reduce action.
    calloc0p(&(fd->adequate_item), ls->num_rules + 1, struct node *);
    if (cli_options->read_reduce_bit) {
      for for_each_rule_fw(rule_no, ls) {
        const int j = RHS_SIZE(rule_no, rules);
        if (rules[rule_no].lhs != accept_image && j > 0) {
          item_no = first_item_of.raw[rule_no] + j;
          struct node *p = Allocate_node();
          p->value = item_no;
          p->next = NULL;
          fd->adequate_item[rule_no] = p;
        } else
          fd->adequate_item[rule_no] = NULL;
      }
    }
    // Construct the CLITEMS map. Each element of CLITEMS points
    // to a circular linked list of items.
    /// CL_ITEMS is a mapping from each non-terminal to a set (linked list)
    /// of items which are the first item of the rules generated by the
    /// non-terminal in question.
    calloc0p(&(fd->clitems), ls->num_non_terminals, struct node *);
    fd->clitems -= ls->num_terminals + 1;
    for for_each_nt_fw(nt, ls) {
      fd->clitems[nt] = NULL;
      for (bool end_node = (rule_no = lhs_rule.raw[nt]) == NIL; !end_node; end_node = rule_no == lhs_rule.raw[nt]) {
        rule_no = next_rule.raw[rule_no];
        struct node *p = Allocate_node();
        p->value = first_item_of.raw[rule_no];
        if (fd->clitems[nt] == NULL) {
          p->next = p;
        } else {
          p->next = fd->clitems[nt]->next;
          fd->clitems[nt]->next = p;
        }
        fd->clitems[nt] = p;
      }
    }
    ffree(first_table.raw);
    ffree(stack.raw);
    index_of.raw += ls->num_terminals + 1;
    ffree(index_of.raw);
  }
  // Construct the FOLLOW map if
  //   - Error-maps are requested
  //   - There are more than one starting symbol.
  if (cli_options->error_maps_bit || next_rule.raw[lhs_rule.raw[accept_image]] != lhs_rule.raw[accept_image]) {
    ListInt stack = allocateListInt(ls->num_non_terminals + 1);
    ListInt index_of = allocateListInt(ls->num_non_terminals);
    index_of.raw -= ls->num_terminals + 1;
    struct ProduceTop topp = {.top = 0};
    /// FOLLOW is a mapping from non-terminals to a set of terminals that
    /// may appear immediately after the non-terminal.
    JBitset follow;
    calloc0_set(follow, ls->num_non_terminals, dss.term_set_size);
    follow.raw -= (ls->num_terminals + 1) * dss.term_set_size;
    SET_BIT_IN(follow, accept_image, eoft_image);
    for for_each_nt_fw(symbol, ls) {
      index_of.raw[symbol] = OMEGA;
    }
    index_of.raw[accept_image] = INFINITY; /* mark computed */
    topp.top = 0;
    for for_each_nt_fw(nt, ls) {
      if (index_of.raw[nt] == OMEGA) {
        // not yet computed ?
        compute_follow_REC(nt, &dss, stack, index_of, &topp, follow, next_item, nt_items, *first, rules, *item_tablep);
      }
    }
    // Initialize FIRST for suffixes that can follow each starting
    // non-terminal ( except the main symbol) with the FOLLOW set
    // of the non-terminal in question.
    int rule_no = lhs_rule.raw[accept_image];
    if (next_rule.raw[rule_no] != rule_no) {
      rule_no = next_rule.raw[rule_no]; /* first rule */
      topp.top = (*item_tablep)[first_item_of.raw[rule_no]].suffix_index;
      for (int i = topp.top + 1; i <= num_first_sets; i++) {
        rule_no = next_rule.raw[rule_no];
        int item_no = first_item_of.raw[rule_no];
        int symbol = (*item_tablep)[item_no].symbol;
        if (IS_A_NON_TERMINAL(symbol, ls)) {
          ASSIGN_SET(*first, i, follow, symbol);
        }
      }
    }
    ffree(stack.raw);
    index_of.raw += ls->num_terminals + 1;
    ffree(index_of.raw);
  }
  // The unreachable symbols in the grammar are printed.
  {
    char line[PRINT_LINE_SIZE + 1];
    char tok[SYMBOL_SIZE + 1];
    // SYMBOL_LIST is used for two purposes:
    //  1) to mark symbols that are reachable from the Accepting
    //        non-terminal.
    //  2) to construct lists of symbols that are not reachable.
    ListInt symbol_list = allocateListInt(ls->num_symbols + 1);
    for for_each_symbol(symbol, ls) {
      symbol_list.raw[symbol] = OMEGA;
    }
    symbol_list.raw[eoft_image] = NIL;
    symbol_list.raw[empty] = NIL;
    if (cli_options->error_maps_bit) {
      symbol_list.raw[error_image] = NIL;
    }
    // Initialize a list consisting only of the Accept non-terminal.
    // This list is a work pile of non-terminals to process as follows:
    // Each non-terminal in the front of the list is removed in turn and
    // 1) All terminal symbols in one of its right-hand sides are
    //    marked reachable.
    // 2) All non-terminals in one of its right-hand sides are placed
    //    in the work pile of it had not been processed previously
    int nt_root = accept_image;
    symbol_list.raw[nt_root] = NIL;
    for (int nt = nt_root; nt != NIL; nt = nt_root) {
      nt_root = symbol_list.raw[nt];
      int rule_no;
      for (bool end_node = (rule_no = lhs_rule.raw[nt]) == NIL; !end_node; end_node = rule_no == lhs_rule.raw[nt]) {
        rule_no = next_rule.raw[rule_no];
        for for_each_rhs(i, rule_no, rules) {
          const int symbol = rhs_sym.raw[i];
          if (IS_A_TERMINAL_L(symbol, ls)) {
            symbol_list.raw[symbol] = NIL;
          } else if (symbol_list.raw[symbol] == OMEGA) {
            symbol_list.raw[symbol] = nt_root;
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
    for for_each_t_bw(symbol, ls) {
      if (symbol_list.raw[symbol] == OMEGA) {
        symbol_list.raw[symbol] = t_root;
        t_root = symbol;
      }
    }
    if (t_root != NIL) {
      if (symbol_list.raw[t_root] != NIL) {
        PRNT("*** The following Terminals are useless: ");
        printf("\n\n");
        strcpy(line, "        "); /* 8 spaces */
      } else {
        strcpy(line, "*** The following Terminal is useless: ");
      }
      for (int symbol = t_root; symbol != NIL; symbol = symbol_list.raw[symbol]) {
        restore_symbol(tok, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
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
    // list. If the list is not empty, we signal that these
    // symbols are unused.
    nt_root = NIL;
    for for_each_nt_bw(symbol, ls) {
      if (symbol_list.raw[symbol] == OMEGA) {
        symbol_list.raw[symbol] = nt_root;
        nt_root = symbol;
      }
    }
    if (nt_root != NIL) {
      if (symbol_list.raw[nt_root] != NIL) {
        PRNT("*** The following Non-Terminals are useless: ");
        printf("\n\n");
        strcpy(line, "        "); /* 8 spaces */
      } else {
        strcpy(line, "*** The following Non-Terminal is useless: ");
      }
      for (int symbol = nt_root; symbol != NIL; symbol = symbol_list.raw[symbol]) {
        restore_symbol(tok, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
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
    ffree(symbol_list.raw);
  }









  // Free allocated arrays.
  nt_first.raw += (ls->num_terminals + 1) * dss.term_set_size;
  ffree(nt_first.raw);
  nt_list.raw += ls->num_terminals + 1;
  ffree(nt_list.raw);
  ffree(first_element);
  nt_items.raw += ls->num_terminals + 1;
  ffree(nt_items.raw);
  ffree(next_item.raw);
  lhs_rule.raw += ls->num_terminals + 1;
  ffree(lhs_rule.raw);
  ffree(next_rule.raw);
  ffree(first_item_of.raw);
  return dss;
}
// endregion











// region produce
void compute_produces_REC(const long symbol, struct node **direct_produces, ListInt stack, ListInt index_of, JBitset produces, struct ProduceTop* top_value) {
  stack.raw[++top_value->top] = symbol;
  const int indx = top_value->top;
  index_of.raw[symbol] = indx;
  struct node *q;
  for (struct node *p = direct_produces[symbol]; p != NULL; q = p, p = p->next) {
    int new_symbol = p->value;
    /* first time seen? */
    if (index_of.raw[new_symbol] == OMEGA) {
      compute_produces_REC(new_symbol, direct_produces, stack, index_of, produces, top_value);
    }
    index_of.raw[symbol] = MIN(index_of.raw[symbol], index_of.raw[new_symbol]);
    SET_UNION(produces, symbol, produces, new_symbol);
  }
  if (direct_produces[symbol] != NULL) {
    free_nodes(direct_produces[symbol], q);
  }
  /* symbol is SCC root */
  if (index_of.raw[symbol] == indx) {
    for (int new_symbol = stack.raw[top_value->top]; new_symbol != symbol; new_symbol = stack.raw[--top_value->top]) {
      ASSIGN_SET(produces, new_symbol, produces, symbol);
      index_of.raw[new_symbol] = INFINITY;
    }
    index_of.raw[symbol] = INFINITY;
    top_value->top--;
  }
}
// endregion














// region mkstates
/// STATE_ELEMENT is used to represent states. Each state is mapped into a
/// unique number.
struct state_element {
  // LINK is used to resolve collisions in hashing the states.
  struct state_element *link;
  // QUEUE is used to form a sequential linked-list of the states ordered
  struct state_element *queue;
  // NEXT_SHIFT is used to resolve collisions in hashing SHIFT maps.
  struct state_element *next_shift;
  struct node *kernel_items;
  struct node *complete_items;
  struct shift_header_type lr0_shift;
  struct goto_header_type lr0_goto;
  short shift_number;
  // STATE_NUMBER and identified by the variable STATE_ROOT.
  short state_number;
};

struct StateContainer {
  struct state_element *state_root;
  struct state_element *state_tail;
};

/// LR0_STATE_MAP takes as an argument a pointer to a kernel set of items. If
/// no state based on that kernel set already exists, then a new one is
/// created and added to STATE_TABLE. In any case, a pointer to the STATE of
/// the KERNEL is returned.
struct state_element *lr0_state_map(struct node *kernel, struct state_element **state_table, struct StateContainer* sc, struct LAState* ls) {
  unsigned long hash_address = 0;
  // Compute the hash address.
  for (const struct node *p = kernel; p != NULL; p = p->next) {
    hash_address += p->value;
  }
  hash_address %= STATE_TABLE_SIZE;
  // Check whether a state is already defined by the KERNEL set.
  for (struct state_element *state_ptr = state_table[hash_address];state_ptr != NULL; state_ptr = state_ptr->link) {
    struct node *q;
    struct node *r;
    struct node *p;
    for (p = state_ptr->kernel_items, q = kernel; p != NULL && q != NULL; p = p->next, r = q, q = q->next) {
      if (p->value != q->value)
        break;
    }
    // Both P and Q are NULL?
    if (p == q) {
      free_nodes(kernel, r);
      return state_ptr;
    }
  }
  // Add a new state based on the KERNEL set.
  ls->num_states++;
  struct state_element *ptr;
  talloc0p(&ptr, struct state_element);
  ptr->queue = NULL;
  ptr->kernel_items = kernel;
  ptr->complete_items = NULL;
  ptr->state_number = ls->num_states;
  ptr->link = state_table[hash_address];
  state_table[hash_address] = ptr;
  if (sc->state_root == NULL) {
    sc->state_root = ptr;
  } else {
    sc->state_tail->queue = ptr;
  }
  sc->state_tail = ptr;
  return ptr;
}

/// This procedure constructs an LR(0) automaton.
void mklr0(struct CLIOptions *cli_options, struct shift_header_type* no_shifts_ptr, struct goto_header_type* no_gotos_ptr, struct node **clitems, struct node **closure, struct SRTable* srt, struct ruletab_type *rules, struct itemtab *item_table, struct StatSet* ss, struct LAState* ls) {
  // STATE_TABLE is the array used to hash the states. States are
  // identified by their Kernel set of items. Hash locations are
  // computed for the states. As states are inserted in the table,
  // they are threaded together via the QUEUE component of
  // STATE_ELEMENT. The variable STATE_ROOT points to the root of
  // the thread, and the variable STATE_TAIL points to the tail.
  //
  //   After constructing a state, Shift and Goto actions are
  // defined on that state based on a partition of the set of items
  // in that state. The partitioning is based on the symbols
  // following the dot in the items. The array PARTITION is used
  // for that partitioning. LIST and ROOT are used to construct
  // temporary lists of symbols in a state on which Shift or Goto
  // actions are defined.
  // NT_LIST and NT_ROOT are used to build temporary lists of non-terminals.
  // Set up a pool of temporary space.
  reset_temporary_space();
  ListInt list = allocateListInt(ls->num_symbols + 1);
  ListInt shift_action = allocateListInt(ls->num_terminals + 1);
  ListInt shift_list = allocateListInt(ls->num_terminals + 1);
  ListInt nt_list = allocateListInt(ls->num_non_terminals);
  nt_list.raw -= ls->num_terminals + 1;
  struct node **partition;
  calloc0p(&partition, ls->num_symbols + 1, struct node *);
  struct state_element **state_table;
  calloc0p(&state_table, STATE_TABLE_SIZE, struct state_element *);
  struct state_element **shift_table;
  calloc0p(&shift_table, SHIFT_TABLE_SIZE, struct state_element *);
  // INITIALIZATION -----------------------------------------------------------
  int goto_size = 0;
  int shift_size = 0;
  struct StateContainer sc = (struct StateContainer) {
    .state_root = NULL,
    .state_tail = NULL,
  };
  for (int i = 0; i <= ls->num_terminals; i++) {
    shift_action.raw[i] = OMEGA;
  }
  int nt_root = NIL;
  for for_each_nt_fw(i, ls) {
    nt_list.raw[i] = OMEGA;
  }
  // PARTITION, STATE_TABLE and SHIFT_TABLE are initialized by calloc
  //
  // END OF INITIALIZATION ----------------------------------------------------
  //
  // Kernel of the first state consists of the first items in each
  // rule produced by Accept non-terminal.
  struct node *q = NULL;
  struct node *p;
  for (bool end_node = (p = clitems[accept_image]) == NULL; !end_node; /* Loop over circular list */ end_node = p == clitems[accept_image]) {
    p = p->next;
    struct node *new_item = Allocate_node();
    new_item->value = p->value;
    new_item->next = q;
    q = new_item;
  }
  // Insert first state in STATE_TABLE and keep constructing states
  // until we no longer can.
  for (struct state_element *state = lr0_state_map(q, state_table, &sc, ls); /* insert initial state */ state != NULL; /* and process next state until no more */ state = state->queue) {
    // Now we construct a list of all non-terminals that can be
    // introduced in this state through closure.  The CLOSURE of each
    // non-terminal has been previously computed in MKFIRST.
    for (q = state->kernel_items; q != NULL; /* iterate over kernel set of items */ q = q->next) {
      int item_no = q->value;
      int symbol = item_table[item_no].symbol; /* symbol after dot */
      if (IS_A_NON_TERMINAL(symbol, ls)) /* Dot symbol */
      {
        if (nt_list.raw[symbol] == OMEGA) /* not yet seen */
        {
          nt_list.raw[symbol] = nt_root;
          nt_root = symbol;
          for (bool end_node = (p = closure[symbol]) == NULL; !end_node; /* Loop over circular list */ end_node = p == closure[symbol]) {
            // add its closure to list
            p = p->next;
            if (nt_list.raw[p->value] == OMEGA) {
              nt_list.raw[p->value] = nt_root;
              nt_root = p->value;
            }
          }
        }
      }
    }
    // We now construct lists of all start items that the closure
    // non-terminals produce. A map from each non-terminal to its set
    // start items has previously been computed in MKFIRST. (CLITEMS)
    // Empty items are placed directly in the state, whereas non_empty
    // items are placed in a temporary list rooted at CLOSURE_ROOT.
    struct node *closure_root = NULL; /* used to construct list of closure items */
    struct node *closure_tail;
    for (int symbol = nt_root; symbol != NIL; nt_list.raw[symbol] = OMEGA, symbol = nt_root) {
      nt_root = nt_list.raw[symbol];
      for (bool end_node = (p = clitems[symbol]) == NULL; !end_node; /* Loop over circular list */ end_node = p == clitems[symbol]) {
        p = p->next;
        int item_no = p->value;
        struct node *new_item = Allocate_node();
        new_item->value = item_no;
        if (item_table[item_no].symbol == empty) /* complete item */
        {
          // Add to COMPLETE_ITEMS set in state
          new_item->next = state->complete_items;
          state->complete_items = new_item;
        } else {
          // closure item, add to closure list
          if (closure_root == NULL) {
            closure_root = new_item;
          } else {
            closure_tail->next = new_item;
          }
          closure_tail = new_item;
        }
      }
    }
    struct node *item_ptr;
    /* any non-complete closure items? */
    if (closure_root != NULL) {
      // construct list of them and kernel items
      closure_tail->next = state->kernel_items;
      item_ptr = closure_root;
    } else {
      /* else just consider kernel items */
      item_ptr = state->kernel_items;
    }
    // In this loop, the PARTITION map is constructed. At this point,
    // ITEM_PTR points to all the non_complete items in the closure of
    // the state, plus all the kernel items.  We note that the kernel
    // items may still contain complete-items, and if any is found, the
    // COMPLETE_ITEMS list is updated.
    int root = NIL;
    for (; item_ptr != NULL; item_ptr = item_ptr->next) {
      int item_no = item_ptr->value;
      int symbol = item_table[item_no].symbol;
      /* incomplete item */
      if (symbol != empty) {
        int next_item_no = item_no + 1;
        if (partition[symbol] == NULL) {
          // PARTITION not defined on symbol
          list.raw[symbol] = root; /* add to list */
          root = symbol;
          if (IS_A_TERMINAL_L(symbol, ls)) /* Update transition count */
          {
            shift_size++;
          } else {
            goto_size++;
          }
        }
        struct node *tail;
        for (p = partition[symbol]; p != NULL; tail = p, p = p->next) {
          if (p->value > next_item_no) {
            break;
          }
        }
        struct node *r = Allocate_node();
        r->value = next_item_no;
        r->next = p;
        if (p == partition[symbol]) /* Insert at beginning */
        {
          partition[symbol] = r;
        } else {
          tail->next = r;
        }
      } else {
        /* Update complete item set with item from kernel */
        p = Allocate_node();
        p->value = item_no;
        p->next = state->complete_items;
        state->complete_items = p;
      }
    }
    if (closure_root != NULL) {
      free_nodes(closure_root, closure_tail);
    }
    // We now iterate over the set of partitions and update the state
    // automaton and the transition maps: SHIFT and GOTO. Each
    // partition represents the kernel of a state.
    struct goto_header_type go_to;
    if (goto_size > 0) {
      go_to = Allocate_goto_map(goto_size);
      state->lr0_goto = go_to;
    } else {
      state->lr0_goto = *no_gotos_ptr;
    }
    int shift_root = NIL;
    for (int symbol = root; symbol != NIL; /* symbols on which transition is defined */ symbol = list.raw[symbol]) {
      short action = OMEGA;
      // If the partition contains only one item, and it is adequate
      // (i.e. the dot immediately follows the last symbol), and
      // READ-REDUCE is requested, a new state is not created, and the
      // action is marked as a Shift-reduce or a Goto-reduce. Otherwise
      // if a state with that kernel set does not yet exist, we create
      // it.
      q = partition[symbol]; /* kernel of a new state */
      if (cli_options->read_reduce_bit && q->next == NULL) {
        int item_no = q->value;
        if (item_table[item_no].symbol == empty) {
          int rule_no = item_table[item_no].rule_number;
          if (rules[rule_no].lhs != accept_image) {
            action = -rule_no;
            free_nodes(q, q);
          }
        }
      }
      /* Not a Read-Reduce action */
      if (action == OMEGA) {
        struct state_element *new_state = lr0_state_map(q, state_table, &sc, ls);
        action = new_state->state_number;
      }
      // At this stage, the partition list has been freed (for an old
      // state or an ADEQUATE item), or used (for a new state).  The
      // PARTITION field involved should be reset.
      partition[symbol] = NULL; /* To be reused */
      // At this point, ACTION contains the value of the state to Shift
      // to, or rule to Read-Reduce on. If the symbol involved is a
      // terminal, we update the Shift map; else, it is a non-terminal
      // and we update the Goto map.
      // Shift maps are constructed temporarily in SHIFT_ACTION.
      // Later, they are inserted into a map of unique Shift maps, and
      // shared by states that contain identical shifts.
      // Since the lookahead set computation is based on the GOTO maps,
      // all these maps and their element maps should be kept as
      // separate entities.
      if (IS_A_TERMINAL_L(symbol, ls)) {
        /* terminal? add to SHIFT map */
        shift_action.raw[symbol] = action;
        shift_list.raw[symbol] = shift_root;
        shift_root = symbol;
        if (action > 0) {
          ls->num_shifts++;
        } else {
          ls->num_shift_reduces++;
        }
      }
      // NOTE that for Goto's we update the field LA_PTR of GOTO. This
      // field will be used later in the routine MKRDCTS to point to a
      // look-ahead set.
      else {
        go_to.map[goto_size].symbol = symbol; /* symbol field */
        go_to.map[goto_size].action = action; /* state field  */
        go_to.map[goto_size].laptr = OMEGA; /* la_ptr field */
        goto_size--;
        if (action > 0) {
          ls->num_gotos++;
        } else {
          ls->num_goto_reduces++;
        }
      }
    }
    // We are now going to update the set of Shift-maps. Ths idea is
    // to do a look-up in a hash table based on SHIFT_TABLE to see if
    // the Shift map associated with the current state has already been
    // computed. If it has, we simply update the SHIFT_NUMBER and the
    // SHIFT field of the current state. Otherwise, we allocate and
    // construct a SHIFT_ELEMENT map, update the current state, and
    // add it to the set of Shift maps in the hash table.
    //   Note that the SHIFT_NUMBER field in the STATE_ELEMENTs could
    // have been factored out and associated instead with the
    // SHIFT_ELEMENTs. That would have saved some space, but only in
    // the short run. This field was purposely stored in the
    // STATE_ELEMENTs, because once the states have been constructed,
    // they are not kept, whereas the SHIFT_ELEMENTs are kept.
    //    One could have also threaded through the states that contain
    // original shift maps to avoid duplicate assignments in
    // creating the SHIFT map later. However, this would have
    // increased the storage requirement, and would probably have saved
    // (at most) a totally insignificant amount of time.
  update_shift_maps: {
      unsigned long hash_address;
      hash_address = shift_size;
      for (int symbol = shift_root; symbol != NIL; symbol = shift_list.raw[symbol]) {
        hash_address += ABS(shift_action.raw[symbol]);
      }
      struct shift_header_type sh;
      struct state_element *p_inner;
      hash_address %= SHIFT_TABLE_SIZE;
      for (p_inner = shift_table[hash_address];
           p_inner != NULL; /* Search has table for shift map */
           p_inner = p_inner->next_shift) {
        sh = p_inner->lr0_shift;
        if (sh.size == shift_size) {
          int ii;
          // Compare shift maps
          for (ii = 1; ii <= shift_size; ii++) {
            if (sh.map[ii].action != shift_action.raw[sh.map[ii].symbol]) {
              break;
            }
          }
          // Are they equal ?
          if (ii > shift_size) {
            state->lr0_shift = sh;
            state->shift_number = p_inner->shift_number;
            for (int symbol = shift_root; symbol != NIL; symbol = shift_list.raw[symbol]) {
              // Clear SHIFT_ACTION
              shift_action.raw[symbol] = OMEGA;
            }
            shift_size = 0; /* Reset for next round */
            goto leave_update_shift_maps;
          }
        }
      }
      if (shift_size > 0) {
        sh = Allocate_shift_map(shift_size);
        ls->num_shift_maps++;
        state->shift_number = ls->num_shift_maps;
      } else {
        state->shift_number = 0;
        sh = *no_shifts_ptr;
      }
      state->lr0_shift = sh;
      state->next_shift = shift_table[hash_address];
      shift_table[hash_address] = state;
      for (int symbol = shift_root; symbol != NIL; symbol = shift_list.raw[symbol]) {
        sh.map[shift_size].symbol = symbol;
        sh.map[shift_size].action = shift_action.raw[symbol];
        shift_action.raw[symbol] = OMEGA;
        shift_size--;
      }
    } /*end update_shift_maps */
  leave_update_shift_maps:;
  }
  // Construct STATSET, a "compact" and final representation of
  // State table, and SHIFT which is a set of all shift maps needed.
  // NOTE that assignments to elements of SHIFT may occur more than
  // once, but that's ok. It is probably faster to do that than to
  // set up an elaborate scheme to avoid the multiple assignment which
  // may in fact cost more. Look at it this way: it is only a pointer
  // assignment, namely a Load and a Store.
  // Release all NODEs used by the maps CLITEMS and CLOSURE.
  {
    int state_no;
    struct state_element *p_inner;
    // If the grammar is LALR(k), k > 1, more states may be added and
    // the size of the shift map increased.
    calloc0p(&srt->shift, ls->num_states + 1, struct shift_header_type);
    srt->shift[0] = *no_shifts_ptr; /* MUST be initialized for LALR(k) */
    // Allocate space for REDUCE which will be used to map each
    // into its reduce map. We also initialize RULE_COUNT which
    // will be used to count the number of reduce actions on each
    // rule with in a given state.
    calloc0p(&srt->reduce, ls->num_states + 1, struct reduce_header_type);
    calloc0p(&ss->statset, ls->num_states + 1, struct statset_type);
    for (p_inner = sc.state_root; p_inner != NULL; p_inner = p_inner->queue) {
      state_no = p_inner->state_number;
      ss->statset[state_no].kernel_items = p_inner->kernel_items;
      ss->statset[state_no].complete_items = p_inner->complete_items;
      srt->shift[p_inner->shift_number] = p_inner->lr0_shift;
      ss->statset[state_no].shift_number = p_inner->shift_number;
      ss->statset[state_no].go_to = p_inner->lr0_goto;
    }
  }
  ffree(list.raw);
  ffree(shift_action.raw);
  ffree(shift_list.raw);
  nt_list.raw += ls->num_terminals + 1;
  ffree(nt_list.raw);
  ffree(partition);
  ffree(state_table);
  ffree(shift_table);
}

/// In this procedure, we first construct the LR(0) automaton.
void mkstats(struct CLIOptions *cli_options, struct node **clitems, struct node **closure, struct SRTable* srt, struct itemtab *item_table, struct ruletab_type *rules, ListInt* gd_range, ListInt*gd_index, struct StatSet* ss, struct LAState* ls) {
  struct goto_header_type no_gotos_ptr = (struct goto_header_type) {.size = 0, .map = NULL};
  struct shift_header_type no_shifts_ptr = (struct shift_header_type) {.size = 0, .map = NULL};
  mklr0(cli_options, &no_shifts_ptr, &no_gotos_ptr, clitems, closure, srt, rules, item_table, ss, ls);
  if (cli_options->error_maps_bit && (cli_options->table_opt.value == OPTIMIZE_TIME.value || cli_options->table_opt.value == OPTIMIZE_SPACE.value)) {
    *gd_index = allocateListInt(ls->num_states + 2);
    *gd_range = allocateListInt(ls->gotodom_size + 1);
  }
  // Free space trapped by the CLOSURE and CLITEMS maps.
  for for_each_nt_fw(j, ls) {
    struct node *p;
    struct node *q = clitems[j];
    if (q != NULL) {
      p = q->next;
      free_nodes(p, q);
    }
    q = closure[j];
    if (q != NULL) {
      p = q->next;
      free_nodes(p, q);
    }
  }
  closure += ls->num_terminals + 1;
  ffree(closure);
  clitems += ls->num_terminals + 1;
  ffree(clitems);
}
// endregion



















/// Given a STATE_NO and an ITEM_NO, ACCESS computes the set of states where
/// the rule from which ITEM_NO is derived was introduced through closure.
struct node *lpg_access(const int state_no, const int item_no, struct node **in_stat, struct itemtab *item_table) {
  // Build a list pointed to by ACCESS_ROOT originally consisting
  // only of STATE_NO.
  struct node *access_root = Allocate_node();
  access_root->value = state_no;
  access_root->next = NULL;
  // distance to travel is DOT
  for (int i = item_table[item_no].dot; i > 0; i--) {
    struct node *head = access_root; /* Save old ACCESS_ROOT */
    access_root = NULL; /* Initialize ACCESS_ROOT for new list */
    struct node *tail;
    for (struct node *p = head; p != NULL; tail = p, p = p->next) {
      // Compute set of states with transition into p->value.
      struct node *s;
      for (bool end_node = (s = in_stat[p->value]) == NULL; !end_node; end_node = s == in_stat[p->value]) {
        s = s->next;
        struct node *q = Allocate_node();
        q->value = s->value;
        q->next = access_root;
        access_root = q;
      }
    }
    free_nodes(head, tail); /* free previous list */
  }
  return access_root;
}

/// LA_TRAVERSE takes two major arguments: STATE_NO, and an index (GOTO_INDX)
/// that points to the GOTO_ELEMENT array in STATE_NO for the non-terminal
/// left hand side of an item for which look-ahead is to be computed. The
/// look-ahead of an item of the form [x. A y] in state STATE_NO is the set
/// of terminals that can appear immediately after A in the context summarized
/// by STATE_NO. When a look-ahead set is computed, the result is placed in
/// an allocation of LA_ELEMENT pointed to by the LA_PTR field of the
/// GOTO_ELEMENT array.
///
/// The same digraph algorithm used in MKFIRST is used for this computation.
void _la_traverse(const int state_no, const int goto_indx, int *stack_top, struct StackRoot* sr, JBitset first, struct LAIndex* lai, struct node **adequate_item, struct node **in_stat, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table) {
  const struct goto_header_type go_to = statset[state_no].go_to;
  const int la_ptr = go_to.map[goto_indx].laptr;
  struct node *s = Allocate_node(); /* Push LA_PTR down the stack */
  s->value = la_ptr;
  s->next = sr->stack_root;
  sr->stack_root = s;
  const int indx = ++*stack_top; /* one element was pushed into the stack */
  lai->la_index.raw[la_ptr] = indx;
  // Compute STATE, action to perform on Goto symbol in question. If
  // STATE is positive, it denotes a state to which to shift. If it is
  // negative, it is a rule on which to perform a Goto-Reduce.
  const int state = go_to.map[goto_indx].action;
  struct node *r;
  /* Not a Goto-Reduce action */
  if (state > 0) {
    r = statset[state].kernel_items;
  } else {
    r = adequate_item[-state];
  }
  for (; r != NULL; r = r->next) {
    // loop over items [A -> x LHS_SYMBOL . y]
    const int item = r->value - 1;
    if (IS_IN_SET(first, item_table[item].suffix_index, empty)) {
      const int symbol = rules[item_table[item].rule_number].lhs;
      struct node *w = lpg_access(state_no, item, in_stat, item_table); /* states where RULE was  */
      // introduced through closure
      for (struct node *t = w; t != NULL; s = t, t = t->next) {
        // Search for GOTO action in access-state after reducing
        // RULE to its left hand side (SYMBOL). Q points to the
        // GOTO_ELEMENT in question.
        const struct goto_header_type go_to_inner = statset[t->value].go_to;
        int ii;
        for (ii = 1; go_to_inner.map[ii].symbol != symbol; ii++) {}
        if (lai->la_index.raw[go_to_inner.map[ii].laptr] == OMEGA) {
          _la_traverse(t->value, ii, stack_top, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
        }
        SET_UNION(lai->la_set, la_ptr, lai->la_set, go_to_inner.map[ii].laptr);
        lai->la_index.raw[la_ptr] = MIN(lai->la_index.raw[la_ptr], lai->la_index.raw[go_to_inner.map[ii].laptr]);
      }
      free_nodes(w, s);
    }
  }
  /* Top of a SCC */
  if (lai->la_index.raw[la_ptr] == indx) {
    s = sr->stack_root;
    for (int ii = sr->stack_root->value; ii != la_ptr; sr->stack_root = sr->stack_root->next, ii = sr->stack_root->value) {
      ASSIGN_SET(lai->la_set, ii, lai->la_set, la_ptr);
      lai->la_index.raw[ii] = INFINITY;
      (*stack_top)--; /* one element was popped from the stack; */
    }
    lai->la_index.raw[la_ptr] = INFINITY;
    r = sr->stack_root; /* mark last element that is popped and ... */
    sr->stack_root = sr->stack_root->next; /* ... pop it! */
    (*stack_top)--; /* one element was popped from the stack; */
    free_nodes(s, r);
  }
}

struct goto_type* la_traverse(int symbol, const int state_no, struct StackRoot* sr, JBitset first, struct LAIndex* lai, struct node **adequate_item, struct node **in_stat, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table) {
  int stack_top = 0;
  struct goto_header_type go_to = statset[state_no].go_to;
  int ii;
  for (ii = 1; go_to.map[ii].symbol != symbol; ii++) {}
  if (lai->la_index.raw[go_to.map[ii].laptr] == OMEGA) {
    _la_traverse(state_no, ii, &stack_top, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
  }
  return &go_to.map[ii];
}























// region red
/// The structure STATE_ELEMENT is used to construct lookahead states.
/// LA_STATE_ROOT point to a list of lookahead states using the LINK
/// field. The field NEXT_SHIFT is used to hash the new shift maps
/// associated with lookahead states. The field IN_STATE identifies the
/// state that shifted into the lookahead state in question. The field
/// SYMBOL identifies the symbol on shift the transition was made into
/// the lookahead state in question.  The remaining fields are
/// self-explanatory.
struct red_state_element {
  struct red_state_element *link;
  struct red_state_element *next_shift;
  struct reduce_header_type reduce;
  struct shift_header_type shift;
  short in_state;
  short symbol;
  short state_number;
  short shift_number;
};

/// The structures SR_CONFLICT_ELEMENT and RR_CONFLICT_ELEMENT are used
/// th store conflict information. CONFLICT_ELEMENT_POOL is used to
/// keep track of a pool conflict element structures (SR or RR) that
/// are available for allocation.
/// See routines ALLOCATE_CONFLICT_ELEMENT and FREE_CONFLICT_ELEMENTS.
struct sr_conflict_element {
  struct sr_conflict_element *next;
  short state_number;
  short item;
  short symbol;
};

struct rr_conflict_element {
  struct rr_conflict_element *next;
  short symbol;
  short item1;
  short item2;
};

/// VISITED is a structure used to mark state-symbol pairs that have
/// been visited in the process of computing follow-sources for a
/// given action in conflict.
/// The field MAP is an array indexable by the states 1..NUM_STATES;
/// each element of which points to a set (list) of symbols. Thus, for
/// a given state S, and for all symbol X in the list MAP[S], [S,X]
/// has been visited. For efficiency, the fields LIST and ROOT are used
/// to store the set (list) of indexed elements of MAP that are not
/// NULL.
/// See routines MARK_VISITED, WAS_VISITED, CLEAR_VISITED,
///              INIT_LALRK_PROCESS, EXIT_PROCESS
struct visited_element {
  struct node **map;
  ListInt list;
  short root;
};

struct ConflictPool {
  void *conflict_element_pool;
};

struct StackPool {
  struct stack_element *stack_pool;
  struct stack_element *dangling_stacks;
};

struct STRS {
  struct red_state_element *la_state_root;
  /// The variable HIGHEST_LEVEL is used to indicate the highest number of
  /// lookahead that was necessary to resolve all conflicts for a given
  /// grammar. If we can detect that the grammar is not LALR(k), we set
  /// HIGHEST_LEVEL to INFINITY.
  int highest_level;
};

/// This function allocates a conflict_element (sr or rr) structure
/// & returns a pointer to it. If there are nodes in the free pool,
/// one of them is returned. Otherwise, a new node is allocated
/// from the temporary storage pool.
static void *allocate_conflict_element(struct ConflictPool* cp) {
  void *p = cp->conflict_element_pool;
  if (p != NULL) {
    cp->conflict_element_pool = ((struct sr_conflict_element *) p)->next;
  } else {
    talloc0p_raw(&p, void, MAX(sizeof(struct sr_conflict_element), sizeof(struct rr_conflict_element)));
  }
  return p;
}

/// This routine returns a list of conflict_element (sr/rr)structures
/// to the free pool.
static void free_conflict_elements(void *head, void *tail, struct ConflictPool* cp) {
  ((struct sr_conflict_element *) tail)->next = (struct sr_conflict_element *) cp->conflict_element_pool;
  cp->conflict_element_pool = head;
}

/// This function allocates a stack_element structure and returns a
/// pointer to it. If there are nodes in the free pool, one of them
/// is returned. Otherwise, a new node is allocated from the
/// temporary storage pool.
static struct stack_element *allocate_stack_element(struct StackPool* sp) {
  struct stack_element *p = sp->stack_pool;
  if (p != NULL) {
    sp->stack_pool = p->next;
  } else {
    talloc0p(&p, struct stack_element);
  }
  return p;
}

/// This routine returns a list of stack_element structures to the
/// free pool.
static void free_stack_elements(struct stack_element *head, struct stack_element *tail, struct StackPool* sp) {
  tail->next = sp->stack_pool;
  sp->stack_pool = head;
}

/// When an allocated stack_element structure is not directly associated
/// with an action, it is added to a circular list of dangling stack_element
/// nodes so that its space can be reclaimed.
static void add_dangling_stack_element(struct stack_element *s, struct StackPool* sp) {
  if (sp->dangling_stacks == NULL) {
    s->next = s;
  } else {
    s->next = sp->dangling_stacks->next;
    sp->dangling_stacks->next = s;
  }
  sp->dangling_stacks = s;
}

/// This function is invoked to free up all dangling stack_element nodes
/// and reset the dangling stack list.
/// Recall that the dangling stack list is circular.
static void free_dangling_stack_elements(struct StackPool* sp) {
  if (sp->dangling_stacks != NULL) {
    struct stack_element *tail = sp->dangling_stacks;
    free_stack_elements(sp->dangling_stacks->next, tail, sp);
    sp->dangling_stacks = NULL;
  }
}

/// This function allocates and initializes a SOURCE_ELEMENT map.
/// See definition of SOURCE_ELEMENT above.
static struct sources_element allocate_sources(const struct LAState* ls) {
  struct sources_element sources;
  calloc0p(&sources.configs, ls->num_rules + ls->num_rules + ls->num_states + 1, struct stack_element *);
  sources.configs += ls->num_rules;
  calloc0p(&sources.stack_seen, STATE_TABLE_SIZE, struct stack_element *);
  sources.list = allocateListInt(ls->num_rules + ls->num_rules + ls->num_states + 1);
  sources.list.raw += ls->num_rules;
  sources.root = NIL;
  return sources;
}

/// This function takes as argument a SOURCES_ELEMENT structure which it resets to the empty map.
/// See definition of SOURCE_ELEMENT above.
static struct sources_element clear_sources(struct sources_element sources, struct StackPool* sp) {
  struct stack_element *tail;
  for (int act = sources.root; act != NIL; act = sources.list.raw[act]) {
    for (struct stack_element *p = sources.configs[act]; p != NULL; tail = p, p = p->next) {
    }
    free_stack_elements(sources.configs[act], tail, sp);
    sources.configs[act] = NULL;
  }
  sources.root = NIL;
  return sources;
}

/// This function takes as argument a SOURCES_ELEMENT structure. First, it
/// clears it to reclaim all space that was used by STACK_ELEMENTs and then
/// it frees the array space used as a base to construct the map.
static void free_sources(struct sources_element sources, struct StackPool* sp, struct LAState* ls) {
  sources = clear_sources(sources, sp);
  sources.configs -= ls->num_rules;
  ffree(sources.configs);
  ffree(sources.stack_seen);
  sources.list.raw -= ls->num_rules;
  ffree(sources.list.raw);
}

/// This function takes as argument two pointers to sorted lists of stacks.
/// It merges the lists in the proper order and returns the resulting list.
static struct stack_element *union_config_sets(struct stack_element *root1, struct stack_element *root2, struct StackPool* sp) {
  struct stack_element *root = NULL;
  // This loop iterates over both lists until one (or both) has been
  // completely processed. Each time around the loop, a stack is
  // removed from one of the lists and possibly added to the new
  // list. The new list is initially kept as a circular list to
  // preserve the sorted ordering in which elements are added to it.
  while (root1 != NULL && root2 != NULL) {
    // Compare the two stacks in front of the lists for equality.
    // We exit this loop when we encounter the end of one (or both)
    // of the stacks or two elements in them that are not the same.
    struct stack_element *p1;
    struct stack_element *p2;
    for (p1 = root1, p2 = root2;
         p1 != NULL && p2 != NULL;
         p1 = p1->previous, p2 = p2->previous) {
      if (p1->state_number != p2->state_number)
        break;
    }
    // We now have 3 cases to consider:
    //    1. The two stacks are equal? Discard one!
    //    2. List 1 stack is prefix of list 2 stack (p1 == NULL)?
    //       or list 1 stack is less than list 2 stack?
    //       Remove list 1 stack and add it to new list.
    //    3. List 2 stack is either a prefix of list 1 stack, or
    //       it is smaller!
    //       Remove list 2 stack and add it to new list.
    if (p1 == p2) {
      // are both p1 and p2 NULL?
      p2 = root2;
      root2 = root2->next;
      add_dangling_stack_element(p2, sp);
    } else if (p1 == NULL || (p2 != NULL && p1->state_number < p2->state_number)) {
      p1 = root1;
      root1 = root1->next;
      if (root == NULL) {
        p1->next = p1;
      } else {
        p1->next = root->next;
        root->next = p1;
      }
      root = p1;
    } else {
      p2 = root2;
      root2 = root2->next;
      if (root == NULL) {
        p2->next = p2;
      } else {
        p2->next = root->next;
        root->next = p2;
      }
      root = p2;
    }
  }
  // At this stage, at least one (or both) list has been expended
  // (or was empty to start with).
  // If the new list is not empty, turn it into a linear list and
  // append the unexpended list to it, if any.
  // Otherwise, set the new list to the nonempty list if any!
  if (root != NULL) {
    struct stack_element *tail = root;
    root = root->next;
    tail->next = root1 == NULL ? root2 : root1;
  } else {
    root = root1 == NULL ? root2 : root1;
  }
  return root;
}

/// This function takes as argument a SOURCES_ELEMENT map, an ACTION and a
/// set (sorted list) of configurations. It adds the set of configurations
/// to the previous set of configurations associated with the ACTION in the
/// SOURCES_ELEMENT map.
static struct sources_element add_configs(struct sources_element sources, const int action, struct stack_element *config_root, struct StackPool* sp) {
  if (config_root != NULL) {
    if (sources.configs[action] == NULL) {
      // The previous was empty?
      sources.list.raw[action] = sources.root;
      sources.root = action;
    }
    sources.configs[action] = union_config_sets(sources.configs[action], config_root, sp);
  }
  return sources;
}

/// This function clears out all external space used by the VISITED set and
/// resets VISITED to the empty set.
static void clear_visited(struct visited_element* visited) {
  for (int state_no = visited->root; state_no != NIL; state_no = visited->list.raw[state_no]) {
    struct node *tail;
    for (struct node *p = visited->map[state_no]; p != NULL; tail = p, p = p->next) {
    }
    free_nodes(visited->map[state_no], tail);
    visited->map[state_no] = NULL;
  }
  visited->root = NIL;
}

/// This boolean function checks whether a given pair [state, symbol]
/// was already inserted in the VISITED set.
static bool was_visited(const int state_no, const int symbol, struct visited_element* visited) {
  struct node *p;
  for (p = visited->map[state_no]; p != NULL; p = p->next) {
    if (p->value == symbol) {
      break;
    }
  }
  return p != NULL;
}

/// This function inserts a given pair [state, symbol] into the VISITED set.
static void mark_visited(const int state_no, const int symbol, struct visited_element* visited) {
  if (visited->map[state_no] == NULL) {
    // 1st time we see state_no?
    visited->list.raw[state_no] = visited->root;
    visited->root = state_no;
  }
  struct node *p = Allocate_node();
  p->value = symbol;
  p->next = visited->map[state_no];
  visited->map[state_no] = p;
}

struct CyclicTop {
  int top;
};

/// This procedure is a modified instantiation of the digraph algorithm
/// to compute the CYCLIC set of states.
static void compute_cyclic_rec(const long state_no, ListInt stack, ListInt index_of, ListBool cyclic, struct CyclicTop* topp, ListBool null_nt, struct statset_type *statset) {
  stack.raw[++topp->top] = state_no;
  const int indx = topp->top;
  cyclic.raw[state_no] = false;
  index_of.raw[state_no] = indx;
  const struct goto_header_type go_to = statset[state_no].go_to;
  for (int i = 1; i <= go_to.size; i++) {
    const int symbol = go_to.map[i].symbol;
    int act = go_to.map[i].action;
    if (act > 0 && null_nt.raw[symbol]) {
      // We have a transition on a nullable nonterminal?
      if (index_of.raw[act] == OMEGA) {
        compute_cyclic_rec(act, stack, index_of, cyclic, topp, null_nt, statset);
      } else if (index_of.raw[act] != INFINITY) {
        cyclic.raw[state_no] = true;
      }
      cyclic.raw[state_no] = cyclic.raw[state_no] || cyclic.raw[act];
      index_of.raw[state_no] = MIN(index_of.raw[state_no], index_of.raw[act]);
    }
  }
  if (index_of.raw[state_no] == indx) {
    int act;
    do {
      act = stack.raw[topp->top--];
      index_of.raw[act] = INFINITY;
    } while (act != state_no);
  }
}

/// This function takes as argument a configuration STACK, a SYMBOL on
/// which a transition can be made in the configuration and a terminal
/// lookahead symbol, LA_SYMBOL. It executes the transition on SYMBOL
/// and simulates all paths taken in the automaton after that transition
/// until new state(s) are reached where a transition is possible on
/// the lookahead symbol. It then returns the new set of configurations
/// found on which a transition on LA_SYMBOL is possible.
static struct stack_element *follow_sources_rec(struct stack_element *stack, int symbol, const int la_symbol, ListBool cyclic, struct StackPool* sp, struct visited_element* visited, struct StackRoot* sr, ListBool rmpself, JBitset first, struct LAIndex* lai, struct node **adequate_item, struct SRTable* srt, struct statset_type *statset, ListBool null_nt, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat, struct LAState* ls) {
  // If the starting configuration consists of a single state and
  // the initial [state, symbol] pair has already been visited,
  // return the null set. Otherwise, mark the pair visited and ...
  const long state_no = stack->state_number;
  if (stack->size == 1 && was_visited(state_no, symbol, visited) || (state_no == 1 && symbol == accept_image)) {
    return NULL;
  }
  if (stack->size == 1) {
    mark_visited(state_no, symbol, visited);
  }
  // Find the transition defined on the symbol...
  // If the SYMBOL is a nonterminal and we can determine that the
  // lookahead symbol (LA_SYMBOL) cannot possibly follow the
  // nonterminal in question in this context, we simply abandon the
  // search and return the NULL set.
  int act;
  if (IS_A_NON_TERMINAL(symbol, ls)) {
    struct goto_type* gtm = la_traverse(symbol, state_no, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
    if (!IS_IN_SET(lai->la_set, gtm->laptr, la_symbol)) {
      return NULL;
    }
    act = gtm->action;
  } else {
    struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
    int ii;
    for (ii = 1; sh.map[ii].symbol != symbol; ii++) {}
    act = sh.map[ii].action;
  }
  struct stack_element *configs = NULL; /* Initialize the output set of configurations */
  // If the ACTion on the symbol is a shift or a goto, ...
  if (act > 0) {
    // We check to see if the new state contains an action on the
    // lookahead symbol. If that's the case then we create a new
    // configuration by appending ACT to the starting configuration
    // and add this newly formed configuration to the set(list) of
    // configurations...
    struct shift_header_type sh = srt->shift[statset[act].shift_number];
    int ii;
    for (ii = 1; ii <= sh.size; ii++) {
      if (sh.map[ii].symbol == la_symbol) {
        break;
      }
    }
    /* there is a transition on la_symbol in act */
    if (ii <= sh.size) {
      struct stack_element *q = allocate_stack_element(sp);
      q->state_number = act;
      q->size = stack->size + 1;
      q->previous = stack;
      q->next = NULL;
      configs = q;
    }
    // If the new state cannot get into a cycle of null transitions ...
    if (!cyclic.raw[act]) {
      // ... we check to see if it contains any transition
      // on a nullable nonterminal. ...
      struct goto_header_type go_to = statset[act].go_to;
      for (ii = 1; ii <= go_to.size; ii++) {
        int symbol = go_to.map[ii].symbol;
        if (null_nt.raw[symbol]) {
          // ... For each such transition, we
          // append the new state to the stack and recursively invoke
          // FOLLOW_SOURCES to check if a transition on LA_SYMBOL cannot
          // follow such a null transition.
          struct stack_element *q = allocate_stack_element(sp);
          q->state_number = act;
          q->size = stack->size + 1;
          q->previous = stack;
          q->next = NULL;
          struct stack_element *new_configs = follow_sources_rec(q, symbol, la_symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat, ls);
          if (new_configs == NULL) {
            free_stack_elements(q, q, sp);
          } else {
            add_dangling_stack_element(q, sp);
            configs = union_config_sets(configs, new_configs, sp);
          }
        }
      }
    }
  }
  // We now iterate over the kernel set of items associated with the
  // ACTion defined on SYMBOL...
  for (const struct node *item_ptr = act > 0 ? statset[act].kernel_items : adequate_item[-act]; item_ptr != NULL; item_ptr = item_ptr->next) {
    int item_no = item_ptr->value;
    // For each item that is a final item ...
    if (item_table[item_no].symbol == empty) {
      const int rule_no = item_table[item_no].rule_number;
      const int lhs_symbol = rules[rule_no].lhs;
      // ... whose left-hand side is neither the starting
      // symbol nor a symbol that can right-most produce itself ...
      if (lhs_symbol != accept_image && !rmpself.raw[lhs_symbol]) {
        if (item_table[item_no].dot < stack->size) {
          // ... if the length of the prefix of the item preceding
          // the dot is shorter that the length of the stack, we
          // retrace the item's path within the stack and
          // invoke FOLLOW_SOURCES with the prefix of the stack
          // where the item was introduced through closure, the
          // left-hand side of the item and the lookahead symbol.
          struct stack_element *q = stack;
          for (int i = 1; i < item_table[item_no].dot; i++) {
            q = q->previous;
          }
          q = follow_sources_rec(q, lhs_symbol, la_symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat, ls);
          configs = union_config_sets(configs, q, sp);
        } else {
          // Compute the item in the root state of the stack,
          // and find the root state...
          item_no -= stack->size;
          struct stack_element *q;
          for (q = stack; q->size != 1; q = q->previous) {}
          // We are now back in the main automaton, find all
          // sources where the item was introduced through
          // closure start a new configuration and invoke
          // FOLLOW_SOURCES with the appropriate arguments to
          // calculate the set of configurations associated
          // with these sources.
          struct node *v = lpg_access(q->state_number, item_no, in_stat, item_table);
          struct node *tail;
          for (struct node *p = v; p != NULL; tail = p, p = p->next) {
            q = allocate_stack_element(sp);q->state_number = p->value;q->size = 1;q->previous = NULL;q->next = NULL;
            struct stack_element *new_configs = follow_sources_rec(q, lhs_symbol, la_symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat, ls);
            if (new_configs == NULL) {
              free_stack_elements(q, q, sp);
            } else {
              add_dangling_stack_element(q, sp);
              configs = union_config_sets(configs, new_configs, sp);
            }
          }
          free_nodes(v, tail);
        }
      }
    }
  }
  return configs;
}

/// This function has a similar structure as FOLLOW_SOURCES. But,
/// instead of computing configurations that can be reached, it
/// computes lookahead symbols that can be reached. It takes as
/// argument a configuration STACK, a SYMBOL on which a transition can
/// be made in the configuration and a set variable, LOOK_AHEAD, where
/// the result is to be stored. When NEXT_LA is invoked from the
/// outside, LOOK_AHEAD is assumed to be initialized to the empty set.
/// NEXT_LA first executes the transition on SYMBOL and thereafter, all
/// terminal symbols that can be read are added to LOOKAHEAD.
static void next_la_rec(struct stack_element *stack, const int symbol, const JBitset look_ahead, struct StackRoot* sr, ListBool rmpself, JBitset first, JBitset read_set, struct LAIndex* lai, struct node **adequate_item, struct SRTable* srt, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat, struct statset_type *statset, struct LAState* ls) {
  // The only symbol that can follow the end-of-file symbol is the
  // end-of-file symbol.
  if (symbol == eoft_image) {
    SET_BIT_IN(look_ahead, 0, eoft_image);
    return;
  }
  const int state_no = stack->state_number;
  int act;
  // Find the transition defined on the symbol...
  if (IS_A_NON_TERMINAL(symbol, ls)) {
    struct goto_header_type go_to = statset[state_no].go_to;
    int ii;
    for (ii = 1; go_to.map[ii].symbol != symbol; ii++) {
    }
    act = go_to.map[ii].action;
  } else {
    const struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
    int ii;
    for (ii = 1; sh.map[ii].symbol != symbol; ii++) {
    }
    act = sh.map[ii].action;
  }
  // If the ACTion on the symbol is a shift or a goto, then all
  // terminal symbols that can be read in ACT are added to
  // LOOK_AHEAD.
  if (act > 0) {
    SET_UNION(look_ahead, 0, read_set, act);
  }
  // We now iterate over the kernel set of items associated with the
  // ACTion defined on SYMBOL...
  // Recall that the READ_SET of ACT is but the union of the FIRST
  // map defined on the suffixes of the items in the kernel of ACT.
  for (const struct node *item_ptr = act > 0 ? statset[act].kernel_items : adequate_item[-act]; item_ptr != NULL; item_ptr = item_ptr->next) {
    int item_no = item_ptr->value;
    // For each item that is a final item whose left-hand side
    // is neither the starting symbol nor a symbol that can
    // right-most produce itself...
    if (IS_IN_SET(first, item_table[item_no - 1].suffix_index, empty)) {
      const int rule_no = item_table[item_no].rule_number;
      const int lhs_symbol = rules[rule_no].lhs;
      if (lhs_symbol != accept_image && !rmpself.raw[lhs_symbol]) {
        // If the length of the prefix of the item preceeding
        // the dot is shorter that the length of the stack, we
        // retrace the item's path within the stack and
        // invoke NEXT_LA with the prefix of the stack
        // where the item was introduced through closure, the
        // left-hand side of the item and LOOK_AHEAD.
        if (item_table[item_no].dot < stack->size) {
          struct stack_element *q = stack;
          for (int i = 1; i < item_table[item_no].dot; i++) {
            q = q->previous;
          }
          next_la_rec(q, lhs_symbol, look_ahead, sr, rmpself, first, read_set, lai, adequate_item, srt, rules, item_table, in_stat, statset, ls);
        } else {
          struct node *tail;
          // Compute the item in the root state of the stack,
          // and find the root state...
          item_no -= stack->size;
          struct stack_element *q;
          for (q = stack; q->size != 1; q = q->previous) {
          }
          // We are now back in the main automaton, find all
          // sources where the item was introduced through
          // closure and add all terminal symbols in the
          // follow set of the left-hand side symbol in each
          // source to LOOK_AHEAD.
          struct node *v = lpg_access(q->state_number, item_no, in_stat, item_table);
          for (struct node *p = v; p != NULL; tail = p, p = p->next) {
            struct goto_type* gtm = la_traverse(lhs_symbol, p->value, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
            SET_UNION(look_ahead, 0, lai->la_set, gtm->laptr);
          }
          free_nodes(v, tail);
        }
      }
    }
  }
}

/// This function takes as argument an array, STACK_SEEN, with
/// STATE_TABLE_SIZE elements (indexable in the range
/// 0..STATE_TABLE_SIZE-1) which is the base of a hash table and a
/// STACK. It searches the hash table to see if it already contained
/// the stack in question. If yes, it returns TRUE. Otherwise, it
/// inserts the stack into the table and returns FALSE.
static bool stack_was_seen(struct stack_element **stack_seen, struct stack_element *stack) {
  unsigned long hash_address = stack->size; /* Initialize hash address */
  for (struct stack_element *p = stack; p != NULL; p = p->previous) {
    hash_address += p->state_number;
  }
  hash_address %= STATE_TABLE_SIZE;
  for (struct stack_element *p = stack_seen[hash_address]; p != NULL; p = p->link) {
    if (stack->size == p->size) {
      struct stack_element *q;
      struct stack_element *r;
      for (q = stack, r = p; q != NULL; q = q->previous, r = r->previous) {
        if (q->state_number != r->state_number) {
          break;
        }
      }
      if (q == NULL) {
        return true;
      }
    }
  }
  stack->link = stack_seen[hash_address];
  stack_seen[hash_address] = stack;
  return false;
}

/// STATE_TO_RESOLVE_CONFLICTS is a function that attempts to resolve
/// conflicts by doing more look-ahead. If the conflict resolution
/// is successful, then a new state is created and returned; otherwise,
/// the NULL pointer is returned.
static struct red_state_element *state_to_resolve_conflicts_rec(struct sources_element sources, int la_symbol, int level, struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct red_state_element **shift_table, ListBool cyclic, struct StackPool* sp, struct visited_element* visited, struct STRS* strs, struct StackRoot* sr, ListBool rmpself, JBitset first, JBitset read_set, struct LAIndex* lai, struct node **adequate_item, struct SRTable* srt, ListBool null_nt, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat, struct statset_type *statset, struct LAState* ls) {
  struct sources_element new_sources = allocate_sources(ls);
  struct node **action;
  calloc0p(&action, ls->num_terminals + 1, struct node *);
  ListInt symbol_list = allocateListInt(ls->num_terminals + 1);
  ListInt action_list = allocateListInt(ls->num_terminals + 1);
  ListInt rule_count = allocateListInt(ls->num_rules + 1);
  JBitset look_ahead;
  calloc0_set(look_ahead, 1, dss->term_set_size);
  struct red_state_element **la_shift_state;
  calloc0p(&la_shift_state, ls->num_terminals + 1, struct red_state_element *);
  // Initialize new lookahead state. Initialize counters. Check and
  // adjust HIGHEST_LEVEL reached so far, if necessary.
  struct red_state_element *state = NULL;
  int num_shift_actions = 0;
  int num_reduce_actions = 0;
  short shift_root = NIL;
  short reduce_root = NIL;
  if (level > strs->highest_level) {
    strs->highest_level = level;
  }
  // One of the parameters received is a SOURCES map whose domain is
  // a set of actions and each of these actions is mapped into a set
  // of configurations that can be reached after that action is
  // executed (in the state where the conflicts were detected).
  // In this loop, we compute an ACTION map which maps each each
  // terminal symbol into 0 or more actions in the domain of SOURCES.
  //
  // NOTE in a sources map, if a configuration is associated with
  // more than one action then the grammar is not LALR(k) for any k.
  // We check for that condition below. However, this check is there
  // for purely cosmetic reason. It is not necessary for the
  // algorithm to work correctly and its removal will speed up this
  // loop somewhat (for conflict-less input).
  // The first loop below initializes the hash table used for
  // lookups ...
  for (int i = 0; i < STATE_TABLE_SIZE; i++) {
    sources.stack_seen[i] = NULL;
  }
  short symbol_root = NIL;
  for (int act = sources.root; act != NIL; act = sources.list.raw[act]) {
    // For each action we iterate over its associated set of
    // configurations and invoke NEXT_LA to compute the lookahead
    // set for that configuration. These lookahead sets are in
    // turn unioned together to form a lookahead set for the
    // action in question.
    INIT_SET(look_ahead);
    for (struct stack_element *stack = sources.configs[act]; stack != NULL; stack = stack->next) {
      if (stack_was_seen(sources.stack_seen, stack)) {
        // This is the superfluous code mentioned above!
        strs->highest_level = INFINITY;
        goto clean_up_and_return;
      }
      next_la_rec(stack, la_symbol, look_ahead, sr, rmpself, first, read_set, lai, adequate_item, srt, rules, item_table, in_stat, statset, ls);
    }
    RESET_BIT(look_ahead, empty); /* EMPTY never in LA set */
    // For each lookahead symbol computed for this action, add an
    // action to the ACTION map and keep track of the symbols on
    // which any action is defined.
    // If new conflicts are detected and we are already at the
    // lookahead level requested, we terminate the computation...
    int count = 0;
    for for_each_t_fw(symbol, ls) {
      if (IS_ELEMENT(look_ahead, symbol)) {
        count++;
        if (action[symbol] == NULL) {
          symbol_list.raw[symbol] = symbol_root;
          symbol_root = symbol;
        } else if (level == cli_options->lalr_level) {
          goto clean_up_and_return;
        }
        struct node *p = Allocate_node();
        p->value = act;
        p->next = action[symbol];
        action[symbol] = p;
      }
    }
    // If the action in question is a reduction then we keep track
    // of how many times it was used.
    if (act >= 0 && act <= ls->num_rules) {
      rule_count.raw[act] = count;
    }
  }
  // We now iterate over the symbols on which actions are defined.
  // If we detect conflicts on any symbol, we compute new sources
  // and try to recover by computing more lookahead. Otherwise, we
  // update the counts and create two lists: a list of symbols on
  // which shift actions are defined and a list of symbols on which
  // reduce actions are defined.
  for (int symbol = symbol_root; symbol != NIL; symbol = symbol_list.raw[symbol]) {
    // We have four cases to consider:
    //    1. There are conflicts on SYMBOL
    //    2. The action on SYMBOL is a shift-reduce
    //    3. The action on SYMBOL is a shift
    //    4. The action on SYMBOL is a reduce
    if (action[symbol]->next != NULL) {
      new_sources = clear_sources(new_sources, sp);
      struct node *tail;
      for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
        int act = p->value;
        if (act >= 0 && act <= ls->num_rules) {
          rule_count.raw[act]--;
        }
        clear_visited(visited);
        for (struct stack_element *stack = sources.configs[act]; stack != NULL; stack = stack->next) {
          struct stack_element *new_configs;
          new_configs = follow_sources_rec(stack, la_symbol, symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat, ls);
          new_sources = add_configs(new_sources, act, new_configs, sp);
        }
      }
      free_nodes(action[symbol], tail);
      action[symbol] = NULL;
      state = state_to_resolve_conflicts_rec(new_sources, symbol, level + 1, cli_options, dss, shift_table, cyclic, sp, visited, strs, sr, rmpself, first, read_set, lai, adequate_item, srt, null_nt, rules, item_table, in_stat, statset, ls);
      if (state == NULL) {
        goto clean_up_and_return;
      }
      la_shift_state[symbol] = state;
      struct node *p = Allocate_node();
      p->value = state->state_number;
      p->next = NULL;
      action[symbol] = p;
      num_shift_actions++;
      action_list.raw[symbol] = shift_root;
      shift_root = symbol;
    } else if (action[symbol]->value < 0) {
      num_shift_actions++;
      action_list.raw[symbol] = shift_root;
      shift_root = symbol;
    } else if (action[symbol]->value > ls->num_rules) {
      num_shift_actions++;
      action[symbol]->value -= ls->num_rules;
      action_list.raw[symbol] = shift_root;
      shift_root = symbol;
    } else {
      num_reduce_actions++;
      action_list.raw[symbol] = reduce_root;
      reduce_root = symbol;
    }
  }
  // We now iterate over the reduce actions in the domain of sources
  // and compute a default action.
  int default_rule = OMEGA;
  int count = 0;
  for (int act = sources.root; act != NIL; act = sources.list.raw[act]) {
    if (act >= 0 && act <= ls->num_rules) {
      if (rule_count.raw[act] > count) {
        count = rule_count.raw[act];
        default_rule = act;
      }
    }
  }
  // By now, we are ready to create a new look-ahead state. The
  // actions for the state are in the ACTION vector, and the
  // constants: NUM_SHIFT_ACTIONS and NUM_REDUCE_ACTIONS indicate
  // the number of shift and reduce actions in the ACTION vector.
  // Note that the IN_STATE field of each look-ahead state created
  // is initially set to the number associated with that state. If
  // all the conflicts detected in the state, S, that requested the
  // creation of a look-ahead state are resolved, then this field
  // is updated with S.
  // Otherwise, this field indicates that this look-ahead state is
  // dangling - no other state point to it.
  talloc0p(&state, struct red_state_element);
  state->link = strs->la_state_root;
  strs->la_state_root = state;
  ls->max_la_state++;
  state->symbol = la_symbol;
  state->state_number = ls->max_la_state;
  state->in_state = ls->max_la_state; /* Initialize it to something! */
  // If there are any shift-actions in this state, we create a shift
  // map for them if one does not yet exist, otherwise, we reuse the
  // old existing one.
  if (num_shift_actions > 0) {
    unsigned long hash_address;
    struct shift_header_type sh;
    struct red_state_element *p_inner;
    // In this loop, we compute the hash address as the number of
    // shift actions, plus the sum of all the symbols on which a
    // shift action is defined.  As a side effect, we also take
    // care of some other issues. Shift actions which were encoded
    // to distinguish them from reduces action are decoded.
    // The counters for shift and shift-reduce actions are updated.
    // For all Shift actions to look-ahead states, the IN_STATE
    // field of these look-ahead target states are updated.
    hash_address = num_shift_actions; /* Initialize hash address */
    for (int symbol = shift_root; symbol != NIL; symbol = action_list.raw[symbol]) {
      hash_address += symbol;
      if (action[symbol]->value < 0) {
        ls->num_shift_reduces++;
      } else if (action[symbol]->value <= ls->num_states) {
        ls->num_shifts++;
      } else {
        // lookahead-shift
        la_shift_state[symbol]->in_state = ls->max_la_state;
      }
    }
    hash_address %= SHIFT_TABLE_SIZE;
    // Search list associated with HASH_ADDRESS, and if the shift
    // map in question is found, update the SHIFT, and SHIFT_NUMBER
    // fields of the new Look-Ahead State.
    for (p_inner = shift_table[hash_address]; p_inner != NULL; p_inner = p_inner->next_shift) {
      // Search hash table for shift map
      sh = p_inner->shift;
      if (sh.size == num_shift_actions) {
        int ii;
        for (ii = 1; ii <= num_shift_actions; ii++) {
          // compare shift maps
          if (sh.map[ii].action != action[sh.map[ii].symbol]->value) {
            break;
          }
        }
        if (ii > num_shift_actions) /* are they equal ? */
        {
          state->shift = sh;
          state->shift_number = p_inner->shift_number;
          break;
        }
      }
    }
    // Shift map was not found.  We have to create a new one and
    // insert it into the table.
    if (p_inner == NULL) {
      ls->num_shift_maps++;
      sh = Allocate_shift_map(num_shift_actions);
      state->shift = sh;
      state->shift_number = ls->num_shift_maps;
      state->next_shift = shift_table[hash_address];
      shift_table[hash_address] = state;
      for (int symbol = shift_root, i = 1; symbol != NIL; symbol = action_list.raw[symbol], i++) {
        sh.map[i].symbol = symbol;
        sh.map[i].action = action[symbol]->value;
      }
    }
  } else {
    state->shift.size = 0;
    state->shift_number = 0;
  }
  // Construct Reduce map.
  // When SPACE or TIME tables are requested, no default actions are
  // taken.
Build_reduce_map: {
    struct reduce_header_type red;
    if (default_rule != OMEGA &&
        cli_options->table_opt.value != OPTIMIZE_TIME.value &&
        cli_options->table_opt.value != OPTIMIZE_SPACE.value &&
        cli_options->default_opt.value != OPT_0.value)
      num_reduce_actions -= rule_count.raw[default_rule];
    ls->num_reductions += num_reduce_actions;
    red = Allocate_reduce_map(num_reduce_actions);
    state->reduce = red;
    for (int symbol = reduce_root, i_inner = 1; symbol != NIL; symbol = action_list.raw[symbol]) {
      if (cli_options->default_opt.value == OPT_0.value ||
          action[symbol]->value != default_rule ||
          cli_options->table_opt.value == OPTIMIZE_TIME.value ||
          cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
        red.map[i_inner].symbol = symbol;
        red.map[i_inner].rule_number = action[symbol]->value;
        i_inner++;
      }
    }
    red.map[0].symbol = DEFAULT_SYMBOL;
    if (cli_options->default_opt.value > OPT_0.value) {
      red.map[0].rule_number = default_rule;
    } else {
      red.map[0].rule_number = OMEGA;
    }
  }
  // Release all space allocated to process this lookahead state and
  // return.
clean_up_and_return:
  free_sources(new_sources, sp, ls);
  for (int symbol = symbol_root; symbol != NIL; symbol = symbol_list.raw[symbol]) {
    struct node *tail;
    for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
    }
    if (action[symbol] != NULL) {
      free_nodes(action[symbol], tail);
    }
  }
  ffree(action);
  ffree(symbol_list.raw);
  ffree(action_list.raw);
  ffree(rule_count.raw);
  ffree(look_ahead.raw);
  ffree(la_shift_state);
  return state;
}

/// If conflicts were detected and LALR(k) processing was requested,
/// where k > 1, then we attempt to resolve the conflicts by computing
/// more lookaheads.
struct ConflictCounter resolve_lalrk_conflicts(const long state_no, struct node **action, const ListInt symbol_list, const int reduce_root, struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct red_state_element **shift_table, ListBool cyclic, struct StackPool* sp, struct ConflictPool* cp, struct visited_element* visited, struct SourcesElementSources* ses, struct STRS* strs, struct StackRoot* sr, ListBool rmpself, JBitset first, JBitset read_set, struct LAIndex* lai, struct node **adequate_item, struct SRTable* srt, struct lastats_type *lastats, ListBool null_nt, struct node **in_stat, struct ruletab_type *rules, struct itemtab *item_table, struct statset_type *statset, ListInt rhs_sym, char *string_table, struct symno_type *symno, struct LAState* ls) {
  /// TODO • understand this.
  long result_num_sr_conflicts = 0;
  // Note that a shift action to a state "S" is encoded with the
  // value (S+NUM_RULES) to help distinguish it from reduce actions.
  // Reduce actions lie in the range [0..NUM_RULES]. Shift-reduce
  // actions lie in the range [-NUM_RULES..-1].
  struct sr_conflict_element *sr_conflict_root = NULL;
  const struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
  for (int i = 1; i <= sh.size; i++) {
    int symbol = sh.map[i].symbol;
    if (cli_options->lalr_level > 1 && action[symbol] != NULL) {
      ses->sources = clear_sources(ses->sources, sp);
      {
        struct stack_element *q = allocate_stack_element(sp);q->state_number = state_no;q->size = 1;q->previous = NULL;q->next = NULL;
        int act = sh.map[i].action;
        if (act > 0) {
          ses->sources = add_configs(ses->sources, act + ls->num_rules, q, sp);
        } else {
          ses->sources = add_configs(ses->sources, act, q, sp);
        }
      }
      struct node *tail;
      for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
        int item_no = p->value;
        int act = item_table[item_no].rule_number;
        long lhs_symbol = rules[act].lhs;
        clear_visited(visited);
        struct node *v = lpg_access(state_no, item_no, in_stat, item_table);
        for (struct node *s = v; s != NULL; tail = s, s = s->next) {
          struct stack_element *q = allocate_stack_element(sp);q->state_number = s->value;q->size = 1;q->previous = NULL;q->next = NULL;
          struct stack_element *new_configs = follow_sources_rec(q, lhs_symbol, symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat, ls);
          if (new_configs == NULL) {
            free_stack_elements(q, q, sp);
          } else {
            add_dangling_stack_element(q, sp);
            ses->sources = add_configs(ses->sources, act, new_configs, sp);
          }
        }
        free_nodes(v, tail);
      }
      // The function STATE_TO_RESOLVE_CONFLICTS returns a pointer
      // value to a STATE_ELEMENT which has been constructed to
      // resolve the conflicts in question. If the value returned by
      // that function is NULL, then it was not possible to resolve
      // the conflicts. In any case, STATE_TO_RESOLVE_CONFLICTS
      // frees the space that is used by the action map headed by
      // ACTION_ROOT.
      struct red_state_element *state = state_to_resolve_conflicts_rec(ses->sources, symbol, 2, cli_options, dss, shift_table, cyclic, sp, visited, strs, sr, rmpself, first, read_set, lai, adequate_item, srt, null_nt, rules, item_table, in_stat, statset, ls);
      if (state != NULL) {
        state->in_state = state_no;
        free_nodes(action[symbol], tail);
        action[symbol] = NULL;
      }
    }
    // If unresolved shift-reduce conflicts are detected on symbol,
    // add them to the list of conflicts so they can be reported
    // (if the CONFLICT option is on) and count them.
    if (action[symbol] != NULL) {
      int act = sh.map[i].action;
      struct node *tail;
      for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
        if (cli_options->conflicts_bit) {
          struct sr_conflict_element *q_inner = allocate_conflict_element(cp);
          q_inner->state_number = act;
          q_inner->item = p->value;
          q_inner->symbol = symbol;
          q_inner->next = sr_conflict_root;
          sr_conflict_root = q_inner;
        }
        result_num_sr_conflicts++;
      }
      // Remove reduce actions defined on symbol so as to give
      // precedence to the shift.
      free_nodes(action[symbol], tail);
      action[symbol] = NULL;
    }
  }
  long result_num_rr_conflicts = 0;

  /// TODO • understand this.
  // We construct a map from each action to a list of states as we
  // did for the Shift-reduce conflicts. A boolean vector ITEM_SEEN
  // is used to prevent duplication of actions. This problem does
  // not occur with Shift-Reduce conflicts.
  struct rr_conflict_element *rr_conflict_root = NULL;
  for (int symbol = reduce_root; symbol != NIL; symbol = symbol_list.raw[symbol]) {
    if (action[symbol] != NULL) {
      if (cli_options->lalr_level > 1 && action[symbol]->next != NULL) {
        ses->sources = clear_sources(ses->sources, sp);
        struct node *tail;
        for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
          int item_no = p->value;
          int act = item_table[item_no].rule_number;
          int lhs_symbol = rules[act].lhs;
          clear_visited(visited);
          struct node *v = lpg_access(state_no, item_no, in_stat, item_table);
          for (struct node *s = v; s != NULL; tail = s, s = s->next) {
            struct stack_element *q = allocate_stack_element(sp);
            q->state_number = s->value;
            q->size = 1;
            q->previous = NULL;
            q->next = NULL;
            struct stack_element *new_configs = follow_sources_rec(q, lhs_symbol, symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat, ls);
            if (new_configs == NULL) {
              free_stack_elements(q, q, sp);
            } else {
              add_dangling_stack_element(q, sp);
              ses->sources = add_configs(ses->sources, act, new_configs, sp);
            }
          }
          free_nodes(v, tail);
        }
        // STATE_TO_RESOLVE_CONFLICTS will return a pointer to a
        // STATE_ELEMENT if the conflicts were resolvable with more
        // lookaheads, otherwise, it returns NULL.
        struct red_state_element *state = state_to_resolve_conflicts_rec(ses->sources, symbol, 2, cli_options, dss, shift_table, cyclic, sp, visited, strs, sr, rmpself, first, read_set, lai, adequate_item, srt, null_nt, rules, item_table, in_stat, statset, ls);
        if (state != NULL) {
          state->in_state = state_no;
          free_nodes(action[symbol], tail);
          action[symbol] = NULL;
        }
      }
      // If unresolved reduce-reduce conflicts are detected on
      // symbol, add them to the list of conflicts so they can be
      // reported (if the CONFLICT option is on) and count them.
      if (action[symbol] != NULL) {
        int act = action[symbol]->value;
        struct node *tail;
        for (struct node *p = action[symbol]->next; p != NULL; tail = p, p = p->next) {
          if (cli_options->conflicts_bit) {
            struct rr_conflict_element *q_inner = allocate_conflict_element(cp);
            q_inner->symbol = symbol;
            q_inner->item1 = act;
            q_inner->item2 = p->value;
            q_inner->next = rr_conflict_root;
            rr_conflict_root = q_inner;
          }
          result_num_rr_conflicts++;
        }
        // Remove all reduce actions that are defined on symbol
        // except the first one. That rule is the one with the
        // longest right-hand side that was associated with symbol.
        // See code in MKRED.C.
        if (action[symbol]->next != NULL) {
          free_nodes(action[symbol]->next, tail);
          action[symbol]->next = NULL;
        }
      }
    }
  }

  // If any unresolved conflicts were detected, process them.
  if (sr_conflict_root != NULL || rr_conflict_root != NULL) {
    // If conflicts are detected, they are placed in two lists headed by
    // SR_CONFLICT_ROOT and RR_CONFLICT_ROOT. We scan these lists, and
    // report the conflicts.
    char temp[SYMBOL_SIZE + 1];
    /// This routine is invoked when a grammar contains conflicts, and the
    /// first conflict is detected.
    // NT_ITEMS and ITEM_LIST are used in reporting SLR conflicts, and
    // in recreating paths from the Start item. See the routines
    // PRINT_RELEVANT_SLR_ITEMS and PRINT_ROOT_PATH.
    ListInt nt_items = allocateListInt(ls->num_non_terminals);
    nt_items.raw -= ls->num_terminals + 1;
    ListInt item_list = allocateListInt(ls->num_items + 1);
    fill_in(msg_line, (PRINT_LINE_SIZE - 11) / 2 - 1, '-');
    printf("\n%s CONFLICTS %s\n", msg_line, msg_line);
    // SLR conflicts may be caused by a symbol in the FOLLOW set of a
    // left hand side, which is not actually in the LALR look-ahead set in
    // that context. Therefore, there may not exist a path in the state
    // automaton from the state where the conflict was detected to another
    // state where it was introduced. In such a case, we try to retrace a
    // path that contributed the conflict-symbol to the FOLLOW set via a
    // sequence of productions.
    //
    // To assist in this task, we build below a map from each non-terminal
    // A to the set of items of which A is the dot SYMBOL. I.e., all items
    // of the form [x .A y] where x and y are arbitrary strings, and A is
    // a non-terminal. This map is also used in retracing a path from the
    // Start item to any other item.
    for for_each_nt_fw(symbol, ls) {
      nt_items.raw[symbol] = NIL;
    }
    for for_each_item(item_no, ls) {
      if (IS_A_NON_TERMINAL(item_table[item_no].symbol, ls)) {
        item_list.raw[item_no] = nt_items.raw[item_table[item_no].symbol];
        nt_items.raw[item_table[item_no].symbol] = item_no;
      }
    }
    print_state(state_no, cli_options, adequate_item, srt, lastats, statset, in_stat, rules, item_table, rhs_sym, string_table, symno, ls); /* Print state containing conflicts */
    // Process shift-reduce conflicts.
    if (sr_conflict_root != NULL) {
      struct sr_conflict_element *tail;
      for (struct sr_conflict_element *p = sr_conflict_root; p != NULL; tail = p, p = p->next) {
        int symbol = p->symbol;
        int rule_no = item_table[p->item].rule_number;
        restore_symbol(temp, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
        printf("*** Shift/reduce conflict on \"%s\" with rule %d\n", temp, rule_no);
      }
      free_conflict_elements(sr_conflict_root, tail, cp);
    }
    // Process reduce-reduce conflicts.
    if (rr_conflict_root != NULL) {
      struct rr_conflict_element *tail;
      for (struct rr_conflict_element *p = rr_conflict_root; p != NULL; tail = p, p = p->next) {
        int symbol = p->symbol;
        const int n = item_table[p->item1].rule_number;
        int rule_no = item_table[p->item2].rule_number;
        restore_symbol(temp, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
        printf("*** Reduce/reduce conflict on \"%s\" between rule %d and %d\n", temp, n, rule_no);
      }
      free_conflict_elements(rr_conflict_root, tail, cp);
    }
  }
  free_dangling_stack_elements(sp);
  return (struct ConflictCounter) {
    .num_rr_conflicts = result_num_rr_conflicts,
    .num_sr_conflicts = result_num_sr_conflicts,
  };
}
// endregion
















// region mkrdcts
/// Given an item of the form: [x .A y], where x and y are arbitrary strings,
/// and A is a non-terminal, we pretrace the path(s) in the automaton that
/// will be followed in computing the look-ahead set for that item in
/// STATE_NO. A number is assigned to all pairs (S, B), where S is a state,
/// and B is a non-terminal, involved in the paths. GOTO_INDX points to the
/// GOTO_ELEMENT of (STATE_NO, A).
void trace_lalr_path(const int state_no, const int goto_indx, struct CLIOptions *cli_options, int *la_base, JBitset first, struct node **adequate_item, struct ruletab_type *rules, struct node **in_stat, struct statset_type *statset, struct itemtab *item_table, long* la_top) {
  // If STATE is a state number we first check to see if its base
  // look-ahead set is a special one that does not contain EMPTY and
  // has already been assigned a slot that can be reused.
  // ((LA_BASE[STATE] != OMEGA) signals this condition.)
  // NOTE that look-ahead follow sets are shared only when the maximum
  // look-ahead level allowed is 1 and single productions will not be
  // removed. If either (or both) of these conditions is true, we need
  // to have a unique slot assigned to each pair [S, A] (where S is a
  // state, and A is a non-terminal) in the automaton.
  const struct goto_header_type go_to = statset[state_no].go_to;
  const int state = go_to.map[goto_indx].action;
  struct node *r;
  if (state > 0) {
    if (la_base[state] != OMEGA && cli_options->lalr_level == 1) {
      go_to.map[goto_indx].laptr = la_base[state];
      return;
    }
    r = statset[state].kernel_items;
  } else {
    r = adequate_item[-state];
  }
  // At this point, R points to a list of items which are the successors
  // of the items needed to initialize the Look-ahead follow sets.  If
  // anyone of these items contains EMPTY, we trace the Digraph for other
  // look-ahead follow sets that may be needed, and signal this fact
  // using the variable CONTAINS_EMPTY.
  (*la_top)++; /* allocate new slot */
  go_to.map[goto_indx].laptr = *la_top;
  bool contains_empty = false;
  for (; r != NULL; r = r->next) {
    const int item = r->value - 1;
    if (IS_IN_SET(first, item_table[item].suffix_index, empty)) {
      contains_empty = true;
      const int symbol = rules[item_table[item].rule_number].lhs;
      struct node *w = lpg_access(state_no, item, in_stat, item_table);
      struct node *p;
      for (struct node *t = w; t != NULL; p = t, t = t->next) {
        const struct goto_header_type go_to_inner = statset[t->value].go_to;
        int ii;
        for (ii = 1; go_to_inner.map[ii].symbol != symbol; ii++) {}
        if (go_to_inner.map[ii].laptr == OMEGA) {
          trace_lalr_path(t->value, ii, cli_options, la_base, first, adequate_item, rules, in_stat, statset, item_table, la_top);
        }
      }
      free_nodes(w, p);
    }
  }
  // If the look-ahead follow set involved does not contain EMPTY, we
  // mark the state involved (STATE) so that other look-ahead follow
  // sets which may need this set may reuse the same one.
  // NOTE that if CONTAINS_EMPTY is false, then STATE has to denote a
  // state number (positive value) and not a rule number (negative).
  if (!contains_empty) {
    la_base[state] = go_to.map[goto_indx].laptr;
  }
}

/// COMPUTE_READ computes the number of intermediate look-ahead sets that
/// will be needed (in LA_TOP), allocates space for the sets(LA_SET), and
/// initializes them.
/// By intermediate look-ahead set, we mean the set of terminals that may
/// follow a non-terminal in a given state.
/// These sets are initialized to the set of terminals that can immediately
/// follow the non-terminal in the state to which it can shift (READ set).
void compute_read(
  struct CLIOptions *cli_options,
  const struct DetectedSetSizes* dss,
  ListBool single_complete_item,
  JBitset first,
  JBitset read_set,
  struct LAIndex* lai,
  struct node **adequate_item,
  struct SRTable* srt,
  struct statset_type *statset,
  struct ruletab_type *rules,
  struct itemtab *item_table,
  struct node **in_stat,
  struct LAState* ls
) {
  // We traverse all the states and for all complete items that requires
  // a look-ahead set, we retrace the state digraph (with the help of the
  // routine TRACE_LALR_PATH) and assign a unique number to all look-ahead
  // follow sets that it needs. A look-ahead follow set is a set of
  // terminal symbols associated with a pair [S, A], where S is a state,
  // and A is a non-terminal:
  //
  // [S, A] --> Follow-set
  // Follow-set = {t | t is a terminal that can be shifted on after
  //                   execution of a goto action on A in state S}.
  //
  // Each follow set is initialized with the set of terminals that can be
  // shifted on in state S2, where GOTO(S, A) = S2. After initialization
  // a follow set F that does not contain the special terminal symbol
  // EMPTY is marked with the help of the array LA_BASE, and if the
  // highest level of look-ahead allowed is 1, then only one such set is
  // allocated, and shared for all pairs (S, B) whose follow set is F.
  int *la_base;
  calloc0p(&la_base, ls->num_states + 1, int);
  for for_each_state(state_no, ls) {
    la_base[state_no] = OMEGA;
  }
  long la_top = 0;
  for for_each_state(state_no, ls) {
    for (const struct node *p = cli_options->lalr_level <= 1 && single_complete_item.raw[state_no]
        ? NULL
        : statset[state_no].complete_items
      ; p != NULL
      ; p = p->next) {
      int item_no = p->value;
      int rule_no = item_table[item_no].rule_number;
      int lhs_symbol = rules[rule_no].lhs;
      if (lhs_symbol != accept_image) {
        struct node *v = lpg_access(state_no, item_no, in_stat, item_table);
        struct node *q;
        for (struct node *s = v; s != NULL; q = s, s = s->next) {
          const struct goto_header_type go_to = statset[s->value].go_to;
          int ii;
          for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {}
          if (go_to.map[ii].laptr == OMEGA) {
            trace_lalr_path(s->value, ii, cli_options, la_base, first, adequate_item, rules, in_stat, statset, item_table, &la_top);
          }
        }
        free_nodes(v, q);
      }
    }
    // If the look-ahead level is greater than 1 or single productions
    // actions are to be removed when possible, then we have to compute
    // a Follow-set for all pairs [S, A] in the state automaton. Therefore,
    // we also have to consider Shift-reduce actions as reductions, and
    // trace back to their roots as well.
    // Note that this is not necessary for Goto-reduce actions. Since
    // they terminate with a non-terminal, and that non-terminal is
    // followed by the empty string, and we know that it must produce a
    // rule that either ends up in a reduction, a shift-reduce, or another
    // goto-reduce. It will therefore be taken care of automatically by
    // transitive closure.
    if (cli_options->lalr_level > 1) {
      const struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
      for (int j = 1; j <= sh.size; j++) {
        if (sh.map[j].action < 0) {
          int rule_no = -sh.map[j].action;
          int lhs_symbol = rules[rule_no].lhs;
          int item_no = adequate_item[rule_no]->value - 1;
          struct node *v = lpg_access(state_no, item_no, in_stat, item_table);
          struct node *q;
          for (struct node *s = v; s != NULL; q = s, s = s->next) {
            const struct goto_header_type go_to = statset[s->value].go_to;
            int ii;
            for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {}
            if (go_to.map[ii].laptr == OMEGA) {
              trace_lalr_path(s->value, ii, cli_options, la_base, first, adequate_item, rules, in_stat, statset, item_table, &la_top);
            }
          }
          free_nodes(v, q);
        }
      }
      // We also need to compute the set of terminal symbols that can be
      // read in a state entered via a terminal transition.
      if (state_no != 1) {
        struct node *q = statset[state_no].kernel_items;
        int item_no = q->value - 1;
        if (IS_A_TERMINAL_L(item_table[item_no].symbol, ls)) {
          ASSIGN_SET(read_set, state_no, first, item_table[item_no].suffix_index);
          for (q = q->next; q != NULL; q = q->next) {
            item_no = q->value - 1;
            SET_UNION(read_set, state_no, first, item_table[item_no].suffix_index);
          }
        }
      }
    }
  }
  // We now allocate space for LA_INDEX and LA_SET, and initialize
  // all its elements as indicated in reduce.h. The array LA_BASE is
  // used to keep track of Follow sets that have been initialized. If
  // another set needs to be initialized with a value that has been
  // already computed, LA_BASE is used to retrieve the value.
  for for_each_state(state_no, ls) {
    la_base[state_no] = OMEGA;
  }
  calloc0_set(lai->la_set, la_top + 1, dss->term_set_size);
  for for_each_state(state_no, ls) {
    const struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      const int la_ptr = go_to.map[i].laptr;
      /* Follow Look-ahead needed */
      if (la_ptr != OMEGA) {
        const int state = go_to.map[i].action;
        struct node *q;
        if (state > 0) {
          /* already computed */
          if (la_base[state] != OMEGA) {
            lai->la_index.raw[la_ptr] = lai->la_index.raw[la_base[state]];
            ASSIGN_SET(lai->la_set, la_ptr, lai->la_set, la_base[state]);
            q = NULL;
          } else {
            la_base[state] = la_ptr;
            q = statset[state].kernel_items;
          }
        } else {
          q = adequate_item[-state];
        }
        if (q != NULL) {
          int item_no = q->value - 1;
          // initialize with first item
          ASSIGN_SET(lai->la_set, la_ptr, first, item_table[item_no].suffix_index);
          for (q = q->next; q != NULL; q = q->next) {
            item_no = q->value - 1;
            SET_UNION(lai->la_set, la_ptr, first, item_table[item_no].suffix_index);
          }
          if (IS_IN_SET(lai->la_set, la_ptr, empty)) {
            lai->la_index.raw[la_ptr] = OMEGA;
          } else {
            lai->la_index.raw[la_ptr] = INFINITY;
          }
          if (cli_options->lalr_level > 1) {
            if (state > 0) {
              ASSIGN_SET(read_set, state, lai->la_set, la_ptr);
            }
          }
        }
      }
    }
  }
  ffree(la_base);
}














/// Build Reduce map, and detect conflicts if any
/// MKRDCTS constructs the REDUCE map and detects conflicts in the grammar.
/// When constructing an LALR parser, the subroutine COMPUTE_LA is invoked to
/// compute the lalr look-ahead sets.
///
/// For a complete description of the lookahead algorithm used in this
/// program, see Charles, PhD thesis, NYU 1991.
struct ConflictCounter mkrdcts(
  struct CLIOptions *cli_options,
  struct FirstDeps* fd,
  struct DetectedSetSizes* dss,
  JBitset first,
  struct node **adequate_item,
  struct SRTable* srt,
  struct ruletab_type *rules,
  struct statset_type *statset,
  struct itemtab *item_table,
  ListInt rhs_sym,
  struct LaStats* las,
  struct ParserState* ps,
  struct LAState* ls
) {
  struct SourcesElementSources ses = (struct SourcesElementSources) {
    .sources = NULL,
  };
  long requested_lalr_level = cli_options->lalr_level;
  reset_temporary_space();






  // TODO === • calculate production related metrics. ===
  ListBool rmpself;
  // RMPSELF is a set that identifies the nonterminals that can
  // right-most produce themselves. To compute RMPSELF, the map
  // PRODUCES must be constructed which identifies for each NT
  // the set of NTs that it can right-most produce.
  if (requested_lalr_level > 1) {
    JBitset produces; calloc0_set(produces, ls->num_non_terminals, dss->non_term_set_size); produces.raw -= (ls->num_terminals + 1) * dss->non_term_set_size;
    struct node **direct_produces; calloc0p(&direct_produces, ls->num_non_terminals, struct node *); direct_produces -= ls->num_terminals + 1;
    for for_each_nt_fw(sym_b, ls) {
      struct node *p;
      for (bool end_node = (p = fd->clitems[sym_b]) == NULL; !end_node; end_node = p == fd->clitems[sym_b]) {
        p = p->next;
        int item_no = p->value;
        int sym_a = item_table[item_no].symbol;
        if (IS_A_NON_TERMINAL(sym_a, ls)) {
          const int i = item_table[item_no].suffix_index;
          if (IS_IN_SET(first, i, empty) && !IS_IN_SET(produces, sym_b, sym_a - ls->num_terminals)) {
            SET_BIT_IN(produces, sym_b, sym_a - ls->num_terminals);
            struct node *q = Allocate_node();
            q->value = sym_a;
            q->next = direct_produces[sym_b];
            direct_produces[sym_b] = q;
          }
        }
      }
    }
    // Complete the construction of the RIGHT_MOST_PRODUCES map
    // for non-terminals using the digraph algorithm.
    ListInt index_of = allocateListInt(ls->num_non_terminals); index_of.raw -= ls->num_terminals + 1;
    for for_each_nt_fw(nt, ls) {
      index_of.raw[nt] = OMEGA;
    }
    struct ProduceTop topp = {.top = 0};
    ListInt stack = allocateListInt(ls->num_non_terminals + 1);
    for for_each_nt_fw(nt, ls) {
      if (index_of.raw[nt] == OMEGA) {
        compute_produces_REC(nt, direct_produces, stack, index_of, produces, &topp);
      }
    }
    /// This procedure is invoked when LALR_LEVEL > 1 to construct the
    /// RMPSELF set which identifies the nonterminals that can right-most
    /// produce themselves. It takes as argument the map PRODUCES which
    /// identifies for each nonterminal the set of nonterminals that it can
    /// right-most produce.
    rmpself = Allocate_bool_array2(ls->num_non_terminals);
    rmpself.raw -= ls->num_terminals + 1;
    // Note that each element of the map produces is a boolean vector
    // that is indexable in the range 1..num_non_terminals. Since each
    // nonterminal is offset by the value num_terminals (to distinguish
    // it from the terminals),it must therefore be adjusted accordingly
    // when dereferencing an element in the range of the produces map.
    for for_each_nt_fw(nt, ls) {
      rmpself.raw[nt] = IS_IN_SET(produces, nt, nt - ls->num_terminals);
    }
    produces.raw += (ls->num_terminals + 1) * dss->non_term_set_size;
    ffree(produces.raw);
    direct_produces += ls->num_terminals + 1;
    ffree(direct_produces);
    ffree(stack.raw);
    index_of.raw += ls->num_terminals + 1;
    ffree(index_of.raw);
  }
  /// The grammar contains a nonterminal A such that A =>+rm A
  bool not_lrk_due_to_rmpself = false;
  if (requested_lalr_level > 1) {
    for for_each_nt_fw(symbol, ls) {
      not_lrk_due_to_rmpself = not_lrk_due_to_rmpself || rmpself.raw[symbol];
    }
  }






  /// The automaton contains a cycle with each of its edges labeled
  /// with a nullable nonterminal, also known as an indirect read cycle.
  bool not_lrk_due_to_cyclic = false;
  ListBool part_of_nullable_read_cycle = Allocate_bool_array2(ls->num_states + 1);
  if (requested_lalr_level > 1) {
    ListInt index_of = allocateListInt(ls->num_states + 1);
    for for_each_state(state_no, ls) {
      index_of.raw[state_no] = OMEGA;
    }
    ListInt stack = allocateListInt(ls->num_states + 1);
    struct CyclicTop top = {.top = 0};
    for for_each_state(state_no, ls) {
      if (index_of.raw[state_no] == OMEGA) {
        compute_cyclic_rec(state_no, stack, index_of, part_of_nullable_read_cycle, &top, dss->null_nt, statset);
      }
      not_lrk_due_to_cyclic = not_lrk_due_to_cyclic || part_of_nullable_read_cycle.raw[state_no];
    }
    ffree(stack.raw);
    ffree(index_of.raw);
    ses.sources = allocate_sources(ls);
  }






  // IN_STAT is a mapping from each state to the set of states that have
  // a transition into the state in question. It is used to construct a
  // reverse transition map. See BUILD_IN_STAT for more detail.
  struct node **in_stat = NULL;
  calloc0p(&in_stat, ls->num_states + 1, struct node *);
  /// We construct the IN_STAT map which is the inverse of the transition
  /// map formed by GOTO and SHIFT maps.
  /// This map is implemented as a table of pointers that can be indexed
  /// by the states to a circular list of integers representing other
  /// states that contain transitions to the state in question.
  for for_each_state(state_no, ls) {
    int n = statset[state_no].shift_number;
    const struct shift_header_type sh = srt->shift[n];
    for (int i = 1; i <= sh.size; ++i) {
      n = sh.map[i].action;
      if (n > 0 && n <= ls->num_states) {
        /* A shift action? */
        struct node *q = Allocate_node();
        q->value = state_no;
        if (in_stat[n] == NULL) {
          q->next = q;
        } else {
          q->next = in_stat[n]->next;
          in_stat[n]->next = q;
        }
        in_stat[n] = q;
      }
    }
    const struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      n = go_to.map[i].action;
      // A goto action
      if (n > 0) {
        struct node *q = Allocate_node();
        q->value = state_no;
        if (in_stat[n] == NULL) {
          q->next = q;
        } else {
          q->next = in_stat[n]->next;
          in_stat[n]->next = q;
        }
        in_stat[n] = q;
      }
    }
  }
  // NO_SHIFT_ON_ERROR_SYM is a vector used to identify states that
  // contain shift actions on the %ERROR symbol. Such states are marked
  // only when DEFAULT_OPT is 5.
  ListBool no_shift_on_error_sym = Allocate_bool_array2(ls->num_states + 1);
  // When default actions are requested, the vector SINGLE_COMPLETE_ITEM
  // is used to identify states that contain exactly one final item.
  // Indicates whether it is all right to take default action in
  // states containing exactly one final item.
  // NOTE that when the READ_REDUCE options is turned on, the LR(0)
  // automaton constructed contains no such state.
  // We also check whether the grammar is LR(0). I.e., whether it needs
  // any look-ahead at all.
  ListBool single_complete_item = Allocate_bool_array2(ls->num_states + 1);
  for for_each_state(state_no, ls) {
    no_shift_on_error_sym.raw[state_no] = true;
    if (cli_options->default_opt.value == OPT_5.value) {
      int n = statset[state_no].shift_number;
      const struct shift_header_type sh = srt->shift[n];
      for (int i = 1; i <= sh.size; ++i) {
        if (sh.map[i].symbol == error_image) {
          no_shift_on_error_sym.raw[state_no] = false;
        }
      }
    }
    // Compute whether this state is a final state. I.e., a state that
    // contains only a single complete item. If so, mark it as a default
    // state. Note that if the READ-REDUCE option is used, the automaton
    // will not contain such states. Also, states are marked only when
    // default actions are requested.
    struct node *item_ptr = statset[state_no].kernel_items;
    single_complete_item.raw[state_no] = !cli_options->read_reduce_bit
      && cli_options->table_opt.value != OPTIMIZE_TIME.value
      && cli_options->table_opt.value != OPTIMIZE_SPACE.value
      && cli_options->default_opt.value > OPT_0.value
      && item_ptr->next == NULL
      && item_table[item_ptr->value].symbol == empty;
  }
  struct STRS strs = (struct STRS) {
    .highest_level = 0,
    .la_state_root = NULL,
  };
  for for_each_state(state_no, ls) {
    struct node *item_ptr = statset[state_no].kernel_items;
    // If a state has a complete item, and more than one kernel item
    // which is different from the complete item, then this state
    // requires look-ahead for the complete item.
    // AKA this has at least one s/r or r/r conflict.
    if (strs.highest_level == 0) {
      const struct node *r = statset[state_no].complete_items;
      if (r != NULL) {
        if (item_ptr->next != NULL || item_ptr->value != r->value) {
          strs.highest_level = 1;
        }
      }
    }
  }






  // TODO === • calculate read ===
  // LOOK_AHEAD is used to compute lookahead sets.
  JBitset look_ahead; calloc0_set(look_ahead, 1, dss->term_set_size);
  // We call COMPUTE_READ to perform the following tasks:
  // 1) Count how many elements are needed in LA_ELEMENT: LA_TOP
  // 2) Allocate space for and initialize LA_SET and LA_INDEX
  JBitset read_set = {.raw = NULL}; calloc0_set(read_set, ls->num_states + 1, dss->term_set_size);
  struct LAIndex lai = (struct LAIndex) {
    .la_index = allocateListInt(1),
    .la_set = NULL,
  };
  compute_read(cli_options, dss, single_complete_item, first, read_set, &lai, adequate_item, srt, statset, rules, item_table, in_stat, ls);














  // TODO === what do I do per each state? ===
  // RULE_COUNT is an array used to count the number of reductions on
  // particular rules within a given state.
  ListInt rule_count = allocateListInt(ls->num_rules + 1);
  for for_each_rule_fw(i, ls) rule_count.raw[i] = 0;
  // If no lookahead state is added (the grammar is LALR(1)) this value will not
  // change. Otherwise, MAX_LA_STATE is incremented by 1 for each
  // lookahead state added.
  ls->max_la_state = ls->num_states;
  struct StackPool sp = (struct StackPool) {
    .stack_pool = NULL,
    .dangling_stacks = NULL,
  };
  struct ConflictPool cp = (struct ConflictPool) {
    .conflict_element_pool = NULL,
  };
  struct red_state_element **shift_table; calloc0p(&shift_table, SHIFT_TABLE_SIZE, struct red_state_element *);
  // SYMBOL_LIST is used to construct temporary lists of terminals on
  // which reductions are defined.
  ListInt symbol_list = allocateListInt(ls->num_terminals + 1);
  long num_rr_conflicts = 0;
  long num_sr_conflicts = 0;
  /// NT_ITEMS and ITEM_LIST are used to construct a mapping from each
  /// nonterminal into the set of items of which the nonterminal in
  /// question is the dot symbol. See CONFLICTS_INITIALIZATION.
  struct StackRoot sr = (struct StackRoot) {.stack_root = NULL};
  // ACTION is an array that is used as the base for a mapping from
  // each terminal symbol into a list of actions that can be executed
  // on that symbol in a given state.
  struct node **action; calloc0p(&action, ls->num_terminals + 1, struct node *);
  // We iterate over the states, compute the lookahead sets,
  // resolve conflicts (if multiple lookahead is requested) and/or
  // report the conflicts if requested...
  struct visited_element visited; calloc0p(&visited.map, ls->num_states + 1, struct node *); visited.list = allocateListInt(ls->num_states + 1); visited.root = NIL;
  for for_each_state(state_no, ls) {
    int default_rule = OMEGA;
    int symbol_root = NIL;
    struct node *item_ptr = statset[state_no].complete_items;
    if (item_ptr != NULL) {
      // Check if it is possible to take default reduction. The DEFAULT_OPT
      // parameter indicates what kind of default options are requested.
      // The various values it can have are:
      //
      //    a)   0 => no default reduction.
      //    b)   1 => default reduction only on adequate states. I.e.,
      //              states with only one complete item in their kernel.
      //    c)   2 => Default on all states that contain exactly one
      //              complete item not derived from an empty rule.
      //    d)   3 => Default on all states that contain exactly one
      //              complete item including items from empty rules.
      //    e)   4 => Default reduction on all states that contain exactly
      //              one item. If a state contains more than one item we
      //              take Default on the item that generated the most
      //              reductions. If there is a tie, one is selected at
      //              random.
      //    f)   5 => Same as 4 except that no default actions are computed
      //              for states that contain a shift action on the ERROR
      //              symbol.
      //
      // In the code below, we first check for category 3. If it is not
      // satisfied, then we check for the others. Note that in any case,
      // default reductions are never taken on the ACCEPT rule.
      int rule_no_ = item_table[item_ptr->value].rule_number;
      int symbol = rules[rule_no_].lhs;
      if (single_complete_item.raw[state_no] && symbol != accept_image) {
        default_rule = rule_no_;
        item_ptr = NULL; /* No need to check for conflicts */
      }
      // Iterate over all complete items in the state, build action
      // map, and check for conflicts.
      for (; item_ptr != NULL; item_ptr = item_ptr->next) {
        // for all complete items
        int item_no = item_ptr->value;
        int rule_no = item_table[item_no].rule_number;
        sr.stack_root = NULL;
        INIT_SET(look_ahead); /* initialize set */
        /// Take a state number (STATE_NO), an item number
        /// (ITEM_NO), and a set (LOOK_AHEAD) and compute the look-ahead set of
        /// terminals for the given item in the given state and places the answer in
        /// the set LOOK_AHEAD.
        const int lhs_symbol = rules[item_table[item_no].rule_number].lhs;
        if (lhs_symbol == accept_image) {
          ASSIGN_SET(look_ahead, 0, first, item_table[item_no - 1].suffix_index);
        } else {
          struct node *v = lpg_access(state_no, item_no, in_stat, item_table);
          struct node *r;
          for (struct node *cur = v; cur != NULL; r = cur, cur = cur->next) {
            // Search for GOTO action in Access-State after reducing rule to
            // its left hand side(LHS_SYMBOL). Q points to the state.
            struct goto_type* gtm = la_traverse(lhs_symbol, cur->value, &sr, first, &lai, adequate_item, in_stat, statset, rules, item_table);
            SET_UNION(look_ahead, 0, lai.la_set, gtm->laptr);
          }
          RESET_BIT(look_ahead, empty); /* empty not valid look-ahead */
          free_nodes(v, r);
        }
        for for_each_t_fw(symbol, ls) {
          // for all symbols in la set
          if (IS_ELEMENT(look_ahead, symbol)) {
            struct node *p = Allocate_node();
            p->value = item_no;
            if (action[symbol] == NULL) {
              symbol_list.raw[symbol] = symbol_root;
              symbol_root = symbol;
            } else {
              // Always place the rule with the largest
              // right-hand side first in the list.
              int n = item_table[action[symbol]->value].rule_number;
              if (RHS_SIZE(n, rules) >= RHS_SIZE(rule_no, rules)) {
                p->value = action[symbol]->value;
                action[symbol]->value = item_no;
              }
            }
            p->next = action[symbol];
            action[symbol] = p;
          }
        }
      }
      // At this stage, we have constructed the ACTION map for STATE_NO.
      // ACTION is a map from each symbol into a set of final items.
      // The rules associated with these items are the rules by which
      // to reduce when the lookahead is the symbol in question.
      // SYMBOL_LIST/SYMBOL_ROOT is a list of the non-empty elements of
      // ACTION. If the number of elements in a set ACTION(t), for some
      // terminal t, is greater than one or it is not empty and STATE_NO
      // contains a shift action on t then STATE_NO has one or more
      // conflict(s). The procedure RESOLVE_CONFLICTS takes care of
      // resolving the conflicts appropriately and returns an ACTION
      // map where each element has either 0 (if the conflicts were
      // shift-reduce conflicts, the shift is given precedence) or 1
      // element (if the conflicts were reduce-reduce conflicts, only
      // the first element in the ACTION(t) list is returned).
      if (symbol_root != NIL) {
        struct ConflictCounter cc_ = resolve_lalrk_conflicts(state_no, action, symbol_list, symbol_root, cli_options, dss, shift_table, part_of_nullable_read_cycle, &sp, &cp, &visited, &ses, &strs, &sr, rmpself, first, read_set, &lai, adequate_item, srt, las->lastats, dss->null_nt, in_stat, rules, item_table, statset, rhs_sym, ps->string_table, ps->symno, ls);
        num_rr_conflicts += cc_.num_rr_conflicts;
        num_sr_conflicts += cc_.num_sr_conflicts;
        for (symbol = symbol_root; symbol != NIL; symbol = symbol_list.raw[symbol]) {
          if (action[symbol] != NULL) {
            rule_count.raw[item_table[action[symbol]->value].rule_number]++;
          }
        }
      }
    }
    // We are now ready to compute the size of the reduce map for
    // STATE_NO (reduce_size) and the default rule.
    // If the state being processed contains only a single complete item
    // then the DEFAULT_RULE was previously computed and the list of
    // symbols is empty.
    // NOTE: a REDUCE_ELEMENT will be allocated for all states, even
    // those that have no reductions at all. This will facilitate the
    // Table Compression routines, for they can assume that such an
    // object exists, and can be used for Default values.
    int reduce_size = 0;
    if (symbol_root != NIL) {
      // Compute REDUCE_SIZE, the number of reductions in the state and
      // DEFAULT_RULE: the rule with the highest number of reductions
      // to it.
      int n = 0;
      for (struct node *q = statset[state_no].complete_items; q != NULL; q = q->next) {
        const int item_no = q->value;
        const int rule_no = item_table[item_no].rule_number;
        const int symbol = rules[rule_no].lhs;
        reduce_size += rule_count.raw[rule_no];
        if (rule_count.raw[rule_no] > n && no_shift_on_error_sym.raw[state_no] && symbol != accept_image) {
          n = rule_count.raw[rule_no];
          default_rule = rule_no;
        }
      }
      // If the removal of single productions is requested
      // and/or parsing tables will not be output, figure out
      // if the level of the default option requested permits
      // default actions, and compute how many reduce actions
      // can be eliminated as a result.
      if (cli_options->default_opt.value == OPT_0.value) {
        default_rule = OMEGA;
      } else if (cli_options->table_opt.value != OPTIMIZE_TIME.value && cli_options->table_opt.value != OPTIMIZE_SPACE.value) {
        struct node *q = statset[state_no].complete_items;
        if (q->next == NULL) {
          const int item_no = q->value;
          const int rule_no = item_table[item_no].rule_number;
          if (cli_options->default_opt.value > OPT_2.value || /* No empty rule defined */ (cli_options->default_opt.value == OPT_2.value && RHS_SIZE(rule_no, rules) != 0)) {
            reduce_size -= n;
          } else {
            default_rule = OMEGA;
          }
        } else if (cli_options->default_opt.value > OPT_3.value) {
          reduce_size -= n;
        }
      }
      ls->num_reductions += reduce_size;
    }
    // NOTE that the default fields are set for all states,
    // whether DEFAULT actions are requested. This is
    // all right since one can always check whether (DEFAULT > 0)
    // before using these fields.
    struct reduce_header_type red = Allocate_reduce_map(reduce_size);
    srt->reduce[state_no] = red;
    red.map[0].symbol = DEFAULT_SYMBOL;
    red.map[0].rule_number = default_rule;
    for (int symbol = symbol_root; symbol != NIL; symbol = symbol_list.raw[symbol]) {
      if (action[symbol] != NULL) {
        const int rule_no = item_table[action[symbol]->value].rule_number;
        if (rule_no != default_rule
          || cli_options->table_opt.value == OPTIMIZE_SPACE.value
          || cli_options->table_opt.value == OPTIMIZE_TIME.value) {
          red.map[reduce_size].symbol = symbol;
          red.map[reduce_size].rule_number = rule_no;
          reduce_size--;
        }
        free_nodes(action[symbol], action[symbol]);
        action[symbol] = NULL;
      }
    }
    // Reset RULE_COUNT elements used in this state.
    for (struct node *q = statset[state_no].complete_items; q != NULL; q = q->next) {
      rule_count.raw[item_table[q->value].rule_number] = 0;
    }
  }
  cli_options->lalr_level = strs.highest_level;
  clear_visited(&visited);
  ffree(visited.map);
  ffree(visited.list.raw);














  // If the automaton required multiple lookahead, construct the permanent lookahead states.
  if (ls->max_la_state > ls->num_states) {
    /// Transfer the look-ahead states to their permanent destination, the
    /// array LASTATS, and update the original automaton with the relevant
    /// transitions into the lookahead states.
    // Allocate LASTATS structure to permanently construct lookahead
    // states and reallocate SHIFT map as we may have to construct
    // new shift maps.
    calloc0p(&las->lastats, ls->max_la_state - ls->num_states, struct lastats_type);
    las->lastats -= ls->num_states + 1;
    realloc0p(&srt->shift, ls->max_la_state + 1, struct shift_header_type);
    ListInt shift_action = allocateListInt(ls->num_terminals + 1);
    // The array shift_action will be used to construct a shift map
    // for a given state. It is initialized here to the empty map.
    // The array shift_count is used to count how many references
    // there are to each shift map.
    for for_each_t_fw(symbol, ls) {
      shift_action.raw[symbol] = OMEGA;
    }
    ListInt shift_count = allocateListInt(ls->max_la_state + 1);
    for (int i = 0; i <= ls->max_la_state; i++) {
      shift_count.raw[i] = 0;
    }
    for for_each_state(state_no, ls) {
      shift_count.raw[statset[state_no].shift_number]++;
    }
    // Traverse the list of lookahead states and initialize the
    // final lastat element appropriately. Also, construct a mapping
    // from each relevant initial state into the list of lookahead
    // states into which it can shift. We also keep track of these
    // initial states in a list headed by state_root.
    int state_root = NIL;
    ListInt shift_list = allocateListInt(ls->num_terminals + 1);
    ListInt state_list = allocateListInt(ls->max_la_state + 1);
    struct red_state_element **new_shift_actions; calloc0p(&new_shift_actions, ls->num_states + 1, struct red_state_element *);
    for (struct red_state_element *p = strs.la_state_root; p != NULL; p = p->link) {
      las->lastats[p->state_number].in_state = p->in_state;
      las->lastats[p->state_number].shift_number = p->shift_number;
      las->lastats[p->state_number].reduce = p->reduce;
      if (p->shift.size != 0) {
        srt->shift[p->shift_number] = p->shift;
      }
      const int state_no = p->in_state;
      if (state_no <= ls->num_states) {
        if (new_shift_actions[state_no] == NULL) {
          state_list.raw[state_no] = state_root;
          state_root = state_no;
        }
        p->next_shift = new_shift_actions[state_no];
        new_shift_actions[state_no] = p;
      }
    }
    // We now traverse the list of initial states that can shift into
    // lookahead states and update their shift map appropriately.
    for (int state_no = state_root; state_no != NIL; state_no = state_list.raw[state_no]) {
      // Copy the shift map associated with STATE_NO into the direct
      // access map SHIFT_ACTION.
      const int shift_no = statset[state_no].shift_number;
      struct shift_header_type sh = srt->shift[shift_no];
      int shift_root = NIL;
      for (int i = 1; i <= sh.size; i++) {
        const int symbol = sh.map[i].symbol;
        shift_action.raw[symbol] = sh.map[i].action;
        shift_list.raw[symbol] = shift_root;
        shift_root = symbol;
      }
      // Add the lookahead shift transitions to the initial shift map.
      int shift_size = sh.size;
      for (struct red_state_element *p = new_shift_actions[state_no]; p != NULL; p = p->next_shift) {
        if (shift_action.raw[p->symbol] == OMEGA) {
          shift_size++;
          shift_list.raw[p->symbol] = shift_root;
          shift_root = p->symbol;
        } else if (shift_action.raw[p->symbol] < 0) {
          ls->num_shift_reduces--;
        } else {
          ls->num_shifts--;
        }
        shift_action.raw[p->symbol] = p->state_number;
      }
      // There are two conditions under which we have to construct
      // a new shift map:
      //     1. The initial shift map was shared with other states.
      //     2. The updated shift map contains more elements than
      //        the initial one.
      if (shift_count.raw[shift_no] > 1) {
        shift_count.raw[shift_no]--;
        ls->num_shift_maps++;
        sh = Allocate_shift_map(shift_size);
        srt->shift[ls->num_shift_maps] = sh;
        statset[state_no].shift_number = ls->num_shift_maps;
      } else if (shift_size > sh.size) {
        sh = Allocate_shift_map(shift_size);
        srt->shift[shift_no] = sh;
      }
      // Reconstruct the relevant shift map.
      for (int symbol = shift_root, i = 1; symbol != NIL; shift_action.raw[symbol] = OMEGA, symbol = shift_list.raw[symbol], i++) {
        sh.map[i].symbol = symbol;
        sh.map[i].action = shift_action.raw[symbol];
      }
    }
    // Free all local temporary structures and return.
    ffree(new_shift_actions);
    ffree(shift_action.raw);
    ffree(shift_list.raw);
    ffree(shift_count.raw);
    ffree(state_list.raw);
  }
  // Print informational messages and free all temporary space that
  // was used to compute lookahead information.
  printf("\n");
  if (not_lrk_due_to_cyclic || not_lrk_due_to_rmpself) {
    printf("This grammar is not LR(K).\n\n");
  } else {
    if (num_rr_conflicts > 0 || num_sr_conflicts > 0) {
      if (strs.highest_level != INFINITY) {
        printf("This grammar is not LALR(%d).\n\n", strs.highest_level);
      } else {
        printf("This grammar is not LALR(K).\n\n");
      }
    } else {
      if (strs.highest_level == 0) {
        printf("This grammar is LR(0).\n\n");
      } else {
        printf("This grammar is LALR(%d).\n\n", strs.highest_level);
      }
    }
  }
  // region free things
  if (requested_lalr_level > 1) {
    rmpself.raw += ls->num_terminals + 1;
    ffree(rmpself.raw);
    ffree(part_of_nullable_read_cycle.raw);
    free_sources(ses.sources, &sp, ls);
  }
  ffree(shift_table);
  ffree(rule_count.raw);
  ffree(no_shift_on_error_sym.raw);
  ffree(symbol_list.raw);
  ffree(single_complete_item.raw);
  ffree(action);
  ffree(look_ahead.raw);
  if (read_set.raw != NULL) ffree(read_set.raw);
  if (lai.la_index.raw != NULL) ffree(lai.la_index.raw);
  if (lai.la_set.raw != NULL) ffree(lai.la_set.raw);
  for for_each_state(state_no, ls) {
    struct node *head = in_stat[state_no];
    if (head != NULL) {
      head = head->next;
      free_nodes(head, in_stat[state_no]);
    }
  }
  ffree(in_stat);
  return (struct ConflictCounter) {
    .num_rr_conflicts = num_rr_conflicts,
    .num_sr_conflicts = num_sr_conflicts,
  };
  // endregion
}
// endregion































// region ctabs
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

static void mystrcpy(const char *str, const struct OutputFiles* of, struct OutputPtr output_ptr2) {
  while (*str != '\0') {
    *(*output_ptr2.output_ptr)++ = *str++;
  }
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  BUFFER_CHECK(of->syssym, output_ptr2);
}

static void prnt_longs(const char *title, const int init, const int bound, const int perline, ListInt array, const struct CLIOptions *cli_options, struct OutputFiles* of, struct OutputPtr output_ptr2) {
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
static void compute_action_symbols_range(const ListInt state_start, const ListInt state_stack, const ListInt state_list, ListInt action_symbols_range, struct SRTable* srt, const struct statset_type *statset, struct LAState* ls) {
  ListInt symbol_list = allocateListInt(ls->num_symbols + 1);
  // We now write out the range elements of the ACTION_SYMBOLS map.
  // Recall that if STATE_START has a negative value, then the set in
  // question is sharing elements and does not need to be processed.
  int k = 0;
  for for_each_symbol(j, ls) {
    symbol_list.raw[j] = OMEGA; /* Initialize all links to OMEGA */
  }
  for for_each_state(state_no, ls) {
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
static void compute_naction_symbols_range(const ListInt state_start, const ListInt state_stack, const ListInt state_list, ListInt naction_symbols_range, ListInt gd_index, ListInt gd_range, struct LAState* ls) {
  ListInt symbol_list = allocateListInt(ls->num_symbols + 1);
  // We now write out the range elements of the NACTION_SYMBOLS map.
  // Recall that if STATE_START has a negative value, then the set in
  // question is sharing elements and does not need to be processed.
  int k = 0;
  for for_each_symbol(j, ls) {
    symbol_list.raw[j] = OMEGA; /* Initialize all links to OMEGA */
  }
  for for_each_state(state_no, ls) {
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

static void print_error_maps(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct ByteTerminalRange* btr, struct SRTable* srt, struct statset_type *statset, ListInt gd_index, ListInt gd_range, struct itemtab *item_table, struct symno_type *symno, bool error_maps_bit, struct OutputPtr output_ptr2, char *string_table, int *name, struct LAState* ls) {
  ListInt state_start = allocateListInt(ls->num_states + 2);
  ListInt state_stack = allocateListInt(ls->num_states + 1);
  PRNT("\nError maps storage:");
  // We now construct a bit map for the set of terminal symbols that
  // may appear in each state. Then, we invoke PARTSET to apply the
  // Partition Heuristic and print it.
  ListInt as_size = allocateListInt(ls->num_states + 1);
  ListInt original;
  if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    original = allocateListInt(ls->num_symbols + 1);
    // In a compressed TIME table, the terminal and non-terminal
    // symbols are mixed together when they are remapped.
    // We shall now recover the original number associated with
    // each terminal symbol since it lies very nicely in the
    // range 1..NUM_TERMINALS.  This will save a considerable
    // amount of space in the bit_string representation of sets
    // as well as time when operations are performed on those
    // bit-strings.
    for for_each_t_fw(symbol, ls) {
      original.raw[toutput->symbol_map.raw[symbol]] = symbol;
    }
  }
  JBitset action_symbols;
  if (error_maps_bit) {
    calloc0_set(action_symbols, ls->num_states + 1, dss->term_set_size);
  }
  for for_each_state(state_no, ls) {
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
  partset(action_symbols, as_size, toutput->state_list, state_start, state_stack, ls->num_terminals, ls);
  ffree(action_symbols.raw);
  // Compute and write out the base of the ACTION_SYMBOLS map.
  ListInt action_symbols_base = allocateListInt(ls->num_states + 1);
  for for_each_state(state_no, ls) {
    action_symbols_base.raw[toutput->state_list.raw[state_no]] = ABS(state_start.raw[toutput->state_list.raw[state_no]]);
  }
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char asb[] = {0,\n", 1, ls->num_states, 10, action_symbols_base, cli_options, of, output_ptr2);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER asb[] = {0,\n", 1, ls->num_states, 10, action_symbols_base, cli_options, of, output_ptr2);
  }
  ffree(action_symbols_base.raw);
  // Compute and write out the range of the ACTION_SYMBOLS map.
  int offset = state_start.raw[ls->num_states + 1];
  ListInt action_symbols_range = allocateListInt(offset);
  compute_action_symbols_range(state_start, state_stack, toutput->state_list, action_symbols_range, srt, statset, ls);
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
  long num_bytes = 2 * ls->num_states;
  PRNT2("    Storage required for ACTION_SYMBOLS_BASE map: %ld Bytes", num_bytes);
  if (cli_options->table_opt.value == OPTIMIZE_TIME.value && ctp->last_terminal <= (cli_options->java_bit ? 127 : 255)) {
    num_bytes = offset - 1;
  } else if (cli_options->table_opt.value != OPTIMIZE_TIME.value && ls->num_terminals <= (cli_options->java_bit ? 127 : 255)) {
    num_bytes = offset - 1;
  } else {
    num_bytes = 2 * (offset - 1);
  }
  PRNT2("    Storage required for ACTION_SYMBOLS_RANGE map: %ld Bytes", num_bytes);
  ffree(action_symbols_range.raw);
  JBitset naction_symbols;
  if (error_maps_bit) {
    calloc0_set(naction_symbols, ls->num_states + 1, dss->non_term_set_size);
  }
  // We now repeat the same process for the domain of the GOTO table.
  for for_each_state(state_no, ls) {
    as_size.raw[state_no] = gd_index.raw[state_no + 1] - gd_index.raw[state_no];
    for (int i = gd_index.raw[state_no]; i <= gd_index.raw[state_no + 1] - 1; i++) {
      int symbol = gd_range.raw[i] - ls->num_terminals;
      SET_BIT_IN(naction_symbols, state_no, symbol);
    }
  }
  partset(naction_symbols, as_size, toutput->state_list, state_start, state_stack, ls->num_non_terminals, ls);
  ffree(as_size.raw);
  ffree(naction_symbols.raw);
  // Remap non-terminals
  for (int i = 1; i <= ls->gotodom_size; i++) {
    if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
      gd_range.raw[i] = toutput->symbol_map.raw[gd_range.raw[i]] - ls->num_terminals;
    } else {
      gd_range.raw[i] = toutput->symbol_map.raw[gd_range.raw[i]];
    }
  }
  // Compute and write out the base of the NACTION_SYMBOLS map.
  ListInt naction_symbols_base = allocateListInt(ls->num_states + 1);
  for for_each_state(state_no, ls) {
    naction_symbols_base.raw[toutput->state_list.raw[state_no]] = ABS(state_start.raw[toutput->state_list.raw[state_no]]);
  }
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char nasb[] = {0,\n", 1, ls->num_states, 10, naction_symbols_base, cli_options, of, output_ptr2);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER nasb[] = {0,\n", 1, ls->num_states, 10, naction_symbols_base, cli_options, of, output_ptr2);
  }
  ffree(naction_symbols_base.raw);
  // Compute and write out the range of the NACTION_SYMBOLS map.
  offset = state_start.raw[ls->num_states + 1];
  ListInt naction_symbols_range = allocateListInt(offset);
  compute_naction_symbols_range(state_start, state_stack, toutput->state_list, naction_symbols_range, gd_index, gd_range, ls);
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char nasr[] = {0,\n", 0, offset - 2, 10, naction_symbols_range, cli_options, of, output_ptr2);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER nasr[] = {0,\n", 0, offset - 2, 10, naction_symbols_range, cli_options, of, output_ptr2);
  }
  PRNT2("    Storage required for NACTION_SYMBOLS_BASE map: %ld Bytes", 2 * ls->num_states);
  PRNT2("    Storage required for NACTION_SYMBOLS_RANGE map: %d Bytes", 2 * (offset - 1));
  ffree(naction_symbols_range.raw);
  // We write the name_index of each terminal symbol.  The array TEMP
  // is used to remap the NAME_INDEX values based on the new symbol
  // numberings. If time tables are requested, the terminals and non-
  // terminals are mixed together.
  ListInt temp = allocateListInt(ls->num_symbols + 1);
  if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
    for for_each_t_fw(symbol, ls) {
      temp.raw[toutput->symbol_map.raw[symbol]] = symno[symbol].name_index;
    }
    if (ls->num_names <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte terminal_index[] = {0,\n", 1, ls->num_terminals, 10, temp, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER terminal_index[] = {0,\n", 1, ls->num_terminals, 10, temp, cli_options, of, output_ptr2);
      }
      num_bytes = ls->num_terminals;
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char terminal_index[] = {0,\n", 1, ls->num_terminals, 10, temp, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER terminal_index[] = {0,\n", 1, ls->num_terminals, 10, temp, cli_options, of, output_ptr2);
      }
      num_bytes = 2 * ls->num_terminals;
    }
    // Compute and list space required for TERMINAL_INDEX map.
    PRNT2("    Storage required for TERMINAL_INDEX map: %ld Bytes", num_bytes);
    // We write the name_index of each non_terminal symbol. The array
    // TEMP is used to remap the NAME_INDEX values based on the new
    // symbol numberings.
    for for_each_nt_fw(symbol, ls) {
      temp.raw[toutput->symbol_map.raw[symbol]] = symno[symbol].name_index;
    }
    if (ls->num_names <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte non_terminal_index[] = {0,\n", ls->num_terminals + 1, ls->num_symbols, 10, temp, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER non_terminal_index[] = {0,\n", ls->num_terminals + 1, ls->num_symbols, 10, temp, cli_options, of, output_ptr2);
      }
      num_bytes = ls->num_non_terminals;
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char non_terminal_index[] = {0,\n", ls->num_terminals + 1, ls->num_symbols, 10, temp, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER non_terminal_index[] = {0,\n", ls->num_terminals + 1, ls->num_symbols, 10, temp, cli_options, of, output_ptr2);
      }
      num_bytes = 2 * ls->num_non_terminals;
    }
    // Compute and list space required for NON_TERMINAL_INDEX map.
    PRNT2("    Storage required for NON_TERMINAL_INDEX map: %ld Bytes", num_bytes);
  } else {
    for for_each_symbol(symbol, ls) {
      temp.raw[toutput->symbol_map.raw[symbol]] = symno[symbol].name_index;
    }
    if (ls->num_names <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte symbol_index[] = {0,\n", 1, ls->num_symbols, 10, temp, cli_options, of, output_ptr2);
        mystrcpy("    public final static byte terminal_index[] = symbol_index;\n", of, output_ptr2);
        mystrcpy("    public final static byte non_terminal_index[] = symbol_index;\n", of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER symbol_index[] = {0,\n", 1, ls->num_symbols, 10, temp, cli_options, of, output_ptr2);
        mystrcpy("const unsigned char  *CLASS_HEADER terminal_index[] = &(symbol_index[0]);\n", of, output_ptr2);
        mystrcpy("const unsigned char  *CLASS_HEADER non_terminal_index[] = &(symbol_index[0]);\n", of, output_ptr2);
      }
      num_bytes = ls->num_symbols;
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char symbol_index[] = {0,\n", 1, ls->num_symbols, 10, temp, cli_options, of, output_ptr2);
        mystrcpy("    public final static char terminal_index[] = symbol_index[0];\n", of, output_ptr2);
        mystrcpy("    public final static char non_terminal_index[] = symbol_index;\n", of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER symbol_index[] = {0,\n", 1, ls->num_symbols, 10, temp, cli_options, of, output_ptr2);
        mystrcpy("const unsigned short *CLASS_HEADER terminal_index[] = &(symbol_index[0]);\n", of, output_ptr2);
        mystrcpy("const unsigned short *CLASS_HEADER non_terminal_index[] = &(symbol_index[0]);\n", of, output_ptr2);
      }
      num_bytes = 2 * ls->num_symbols;
    }
    // Compute and list space required for SYMBOL_INDEX map.
    PRNT2("    Storage required for SYMBOL_INDEX map: %ld Bytes", num_bytes);
  }
  if (cli_options->java_bit) {
    // Print java names.
    long num_bytes = 0;
    long max_name_length = 0;
    mystrcpy("\n    public final static String name[] = { null,\n", of, output_ptr2);
    for (int i = 1; i <= ls->num_names; i++) {
      char tok[SYMBOL_SIZE + 1];
      strcpy(tok, RETRIEVE_NAME(i, string_table, name));
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
      if (i < ls->num_names) {
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
    PRNT2("    Storage required for STRING_BUFFER map: %ld Bytes", num_bytes);
  } else {
    // Print C names.
    ListInt name_len = allocateListInt(ls->num_names + 1);
    long num_bytes = 0;
    long max_name_length = 0;
    mystrcpy("\nconst char  CLASS_HEADER string_buffer[] = {0,\n", of, output_ptr2);
    int n = 0;
    padline(output_ptr2);
    for (int i = 1; i <= ls->num_names; i++) {
      char tok[SYMBOL_SIZE + 1];
      strcpy(tok, RETRIEVE_NAME(i, string_table, name));
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
        if (n == 10 && !(i == ls->num_names && j == name_len.raw[i] - 1)) {
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
    PRNT2("    Storage required for STRING_BUFFER map: %ld Bytes", num_bytes);
    // Write out NAME_START array
    mystrcpy("\nconst unsigned short CLASS_HEADER name_start[] = {0,\n", of, output_ptr2);
    padline(output_ptr2);
    int j = 1;
    int k = 0;
    for (int i = 1; i <= ls->num_names; i++) {
      itoc(j, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      j += name_len.raw[i];
      k++;
      if (k == 10 && i != ls->num_names) {
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
    PRNT2("    Storage required for NAME_START map: %ld Bytes", 2 * ls->num_names);
    // Write out NAME_LENGTH array
    prnt_longs("\nconst unsigned char  CLASS_HEADER name_length[] = {0,\n", 1, ls->num_names, 10, name_len, cli_options, of, output_ptr2);
    // Compute and list space required for NAME_LENGTH map.
    PRNT2("    Storage required for NAME_LENGTH map: %ld Bytes", ls->num_names);
    ffree(name_len.raw);
  }
}

static void common(const bool byte_check_bit, struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct ImportantAspects* ia, struct SRTable* srt, struct statset_type *statset, ListInt gd_index, ListInt gd_range, struct itemtab *item_table, char *output_buffer, struct OutputPtr output_ptr2, struct symno_type *symno, char *string_table, int *name, struct LAState* ls) {
  struct ByteTerminalRange btr = (struct ByteTerminalRange) {
    .value = true
  };
  // Write table common.
  {
    if (cli_options->error_maps_bit) {
      print_error_maps(cli_options, toutput, dss, ctp, of, &btr, srt, statset, gd_index, gd_range, item_table, symno, cli_options->error_maps_bit, output_ptr2, string_table, name, ls);
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
    for for_each_t_fw(symbol, ls) {
      char *tok = RETRIEVE_STRING(symbol, string_table, symno);
      fprintf(of->syssym, "%s", line);
      if (tok[0] == '\n' || tok[0] == cli_options->escape) {
        tok[0] = cli_options->escape;
        PRNT2("Escaped symbol %s is an invalid C variable.\n", tok);
      } else if (strpbrk(tok, "!%^&*()-+={}[];:\"`~|\\,.<>/?\'") != NULL) {
        PRNT2("%s may be an invalid variable name.\n", tok);
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
                ls->num_states);
      } else {
        fprintf(of->sysdef,
                "      ERROR_SYMBOL      = %d,\n"
                "      NUM_STATES        = %ld,\n\n",
                error_image,
                ls->num_states);
      }
    }
    if (cli_options->java_bit) {
      fprintf(of->sysdef,
              "      NT_OFFSET         = %ld,\n"
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
              cli_options->table_opt.value == OPTIMIZE_SPACE.value ? ls->num_terminals : ls->num_symbols,
              cli_options->read_reduce_bit && cli_options->lalr_level > 1
                ? ia->error_act + ls->num_rules
                : ia->error_act,
              cli_options->lalr_level,
              ls->num_rules,
              ls->num_terminals,
              ls->num_non_terminals,
              ls->num_symbols,
              toutput->state_index.raw[1] + ls->num_rules,
              eoft_image,
              eolt_image,
              ia->accept_act,
              ia->error_act);
    } else {
      fprintf(of->sysdef,
              "      NT_OFFSET         = %ld,\n"
              "      STACK_UBOUND      = %d,\n"
              "      STACK_SIZE        = %d,\n"
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
              cli_options->table_opt.value == OPTIMIZE_SPACE.value ? ls->num_terminals : ls->num_symbols,
              cli_options->stack_size - 1,
              cli_options->stack_size,
              cli_options->read_reduce_bit && cli_options->lalr_level > 1
                ? ia->error_act + ls->num_rules
                : ia->error_act,
              cli_options->lalr_level,
              ls->num_rules,
              ls->num_terminals,
              ls->num_non_terminals,
              ls->num_symbols,
              toutput->state_index.raw[1] + ls->num_rules,
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
              "%s FULL_DIAGNOSIS\n"
              "%s SPACE_TABLES\n\n",
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
      }
      fprintf(of->sysprs, "\n");
    } else if (cli_options->java_bit) {
      fprintf(of->sysprs, "abstract class %s extends %s implements %s\n{\n", of->prs_tag, of->dcl_tag, of->def_tag);
      if (cli_options->error_maps_bit) {
        fprintf(of->sysprs, "    public final static int original_state(int state) { return -%s(state); }\n", cli_options->table_opt.value == OPTIMIZE_TIME.value ? "check" : "base_check");
        if (cli_options->error_maps_bit) {
          fprintf(of->sysprs, "    public final static int asi(int state) { return asb[original_state(state)]; }\n");
          fprintf(of->sysprs, "    static int nasi(int state) { return nasb[original_state(state)]; }\n");
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
        fprintf(of->sysprs, "%s const unsigned %s term_check[];\n", cli_options->c_bit ? "extern" : "    static", ls->num_terminals <= (cli_options->java_bit ? 127 : 255) ? "char " : "short");
        fprintf(of->sysprs, "%s const unsigned short term_action[];\n", cli_options->c_bit ? "extern" : "    static");
        if (cli_options->shift_default_bit) {
          fprintf(of->sysprs, "%s const unsigned short default_reduce[];\n", cli_options->c_bit ? "extern" : "    static");
          fprintf(of->sysprs, "%s const unsigned short shift_state[];\n", cli_options->c_bit ? "extern" : "    static");
          fprintf(of->sysprs, "%s const unsigned %s shift_check[];\n", cli_options->c_bit ? "extern" : "    static", ls->num_terminals <= (cli_options->java_bit ? 127 : 255) ? "char " : "short");
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
                  ls->num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  ls->num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short");
        } else {
          fprintf(of->sysprs, "%s const unsigned %s symbol_index[];\n"
                  "%s const unsigned %s *terminal_index;\n"
                  "%s const unsigned %s *non_terminal_index;\n",
                  cli_options->c_bit ? "extern" : "    static",
                  ls->num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  ls->num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  ls->num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short");
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
void sortdes(ListInt array, ListInt count, const long low, const long high, const long max) {
  // BUCKET is used to hold the roots of lists that contain the
  // elements of each bucket.  LIST is used to hold these lists.
  ListInt bucket = allocateListInt(max + 1);
  ListInt list = allocateListInt(high - low + 1);
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
    PRNT2("Reallocating storage for TIME table, adding %ld entries", ctp->table_size - old_size);
  } else {
    PRNT2("Reallocating storage for SPACE table, adding %ld entries", ctp->table_size - old_size);
  }
  ListInt n = allocateListInt(ctp->table_size + 1);
  ListInt p = allocateListInt(ctp->table_size + 1);
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

void print_space_parser(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, ListInt term_state_index, ListInt shift_check_index, struct CTabsProps* ctp, struct new_state_type *new_state_element, ListInt shift_image, ListInt real_shift_number, struct OutputFiles* of, struct ImportantAspects* ia, struct SRTable* srt, ListInt shiftdf, ListInt gotodef, ListInt gd_index, ListInt gd_range, struct ruletab_type *rules, struct statset_type *statset, struct itemtab *item_table, char *output_buffer, struct OutputPtr output_ptr2, struct symno_type *symno, char *string_table, int *name, struct LAState* ls)  {
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
  ListInt check = allocateListInt(ctp->table_size + 1);
  ListInt action = allocateListInt(ctp->table_size + 1);
  (*output_ptr2.output_ptr) = &output_buffer[0];
  // Prepare header card with proper information, and write it out.
  long offset = ia->error_act;
  long la_state_offset;
  if (cli_options->lalr_level > 1) {
    if (cli_options->read_reduce_bit) {
      offset += ls->num_rules;
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
    if (state_no <= ls->num_states) {
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
  for for_each_state(state_no, ls) {
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
        action.raw[i] = toutput->state_index.raw[act] + ls->num_rules;
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
    for for_each_state(state_no, ls) {
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
  for (int i = 1; i <= ls->num_rules; i++) {
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
  for (int i = 1; i <= ls->num_rules; i++) {
    itoc(toutput->symbol_map.raw[rules[i].lhs] - ls->num_terminals, output_ptr2);
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
    max_indx = ia->accept_act - ls->num_rules - 1;
    for (int i = 1; i <= max_indx; i++) {
      check.raw[i] = OMEGA;
    }
    for for_each_state(state_no, ls) {
      check.raw[toutput->state_index.raw[state_no]] = state_no;
    }
    int j = ls->num_states + 1;
    for (int i = max_indx; i >= 1; i--) {
      int state_no = check.raw[i];
      if (state_no != OMEGA) {
        j--;
        toutput->ordered_state.raw[j] = i + ls->num_rules;
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
        if (act > ls->num_states) {
          result_act = toutput->state_index.raw[act];
          la_shift_count++;
        } else if (act > 0) {
          result_act = toutput->state_index.raw[act] + ls->num_rules;
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
  PRNT2("     Number of Shifts: %d", shift_count);
  PRNT2("     Number of Shift/Reduces: %d", shift_reduce_count);
  if (ls->max_la_state > ls->num_states) {
    PRNT2("     Number of Look-Ahead Shifts: %d", la_shift_count);
  }
  PRNT2("     Number of Gotos: %d", goto_count);
  PRNT2("     Number of Goto/Reduces: %d", goto_reduce_count);
  PRNT2("     Number of Reduces: %d", reduce_count);
  PRNT2("     Number of Defaults: %d", default_count);
  // Write Terminal Check Table.
  if (ls->num_terminals <= (cli_options->java_bit ? 127 : 255)) {
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
    for for_each_nt_fw(symbol, ls) {
      int act = gotodef.raw[symbol];
      long result_act;
      if (act < 0) {
        result_act = -act;
      } else if (act == 0) {
        result_act = ia->error_act;
      } else {
        result_act = toutput->state_index.raw[act] + ls->num_rules;
      }
      itoc(result_act, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && symbol != ls->num_symbols) {
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
    if (ls->num_terminals <= (cli_options->java_bit ? 127 : 255)) {
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
    for for_each_t_fw(symbol, ls) {
      int act = shiftdf.raw[symbol];
      long result_act;
      if (act < 0) {
        result_act = -act + ia->error_act;
      } else if (act == 0) {
        result_act = ia->error_act;
      } else if (act > ls->num_states) {
        result_act = toutput->state_index.raw[act];
      } else {
        result_act = toutput->state_index.raw[act] + ls->num_rules;
      }
      if (result_act > MAX_TABLE_SIZE + 1) {
        PRNTERR2("Table contains look-ahead shift entry that is >%ld; Processing stopped.", MAX_TABLE_SIZE + 1);
        return;
      }
      itoc(result_act, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && ii != ls->num_terminals) {
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
  common(byte_check_bit, cli_options, toutput, dss, ctp, of, ia, srt, statset, gd_index, gd_range, item_table, output_buffer, output_ptr2, symno, string_table, name, ls);
}

void print_time_parser(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, struct lastats_type *lastats, ListInt gotodef, ListInt gd_index, ListInt gd_range, struct ruletab_type *rules, struct statset_type *statset, struct itemtab *item_table, char *output_buffer, struct OutputPtr output_ptr2, struct symno_type *symno, char *string_table, int *name, struct LAState* ls) {
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
  ListInt check = np->next;
  ListInt action = np->previous;
  long offset = ia->error_act;
  int la_state_offset;
  if (cli_options->lalr_level > 1) {
    if (cli_options->read_reduce_bit) {
      offset += ls->num_rules;
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
  for (int state_no = 1; state_no <= ls->max_la_state; state_no++) {
    struct shift_header_type sh;
    struct reduce_header_type red;
    indx = toutput->state_index.raw[state_no];
    if (state_no > ls->num_states) {
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
          action.raw[i] = toutput->state_index.raw[act] + ls->num_rules;
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
      if (act > ls->num_states) {
        result_act = la_state_offset + toutput->state_index.raw[act];
        la_shift_count++;
      } else if (act > 0) {
        result_act = toutput->state_index.raw[act] + ls->num_rules;
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
  PRNT2("     Number of Shifts: %d", shift_count);
  PRNT2("     Number of Shift/Reduces: %d", shift_reduce_count);
  if (ls->max_la_state > ls->num_states) {
    snprintf(msg_line, sizeof(msg_line), "     Number of Look-Ahead Shifts: %d", la_shift_count);
    PRNT(msg_line);
  }
  PRNT2("     Number of Gotos: %d", goto_count);
  PRNT2("     Number of Goto/Reduces: %d", goto_reduce_count);
  PRNT2("     Number of Reduces: %d", reduce_count);
  PRNT2("     Number of Defaults: %d", default_count);
  if (cli_options->error_maps_bit) {
    for for_each_state(state_no, ls) {
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
  for (int i = 1; i <= ls->num_rules; i++) {
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
  for (int i = 1; i <= ls->num_rules; i++) {
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
    max_indx = ia->accept_act - ls->num_rules - 1;
    for (int i = 1; i <= max_indx; i++) {
      check.raw[i] = OMEGA;
    }
    for for_each_state(state_no, ls) {
      check.raw[toutput->state_index.raw[state_no]] = state_no;
    }
    int j = ls->num_states + 1;
    for (int i = max_indx; i >= 1; i--) {
      int state_no = check.raw[i];
      if (state_no != OMEGA) {
        toutput->ordered_state.raw[--j] = i + ls->num_rules;
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
    ListInt default_map = allocateListInt(ls->num_symbols + 1);
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char default_goto[] = {0,\n", of, output_ptr2);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER default_goto[] = {0,\n", of, output_ptr2);
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 0; i <= ls->num_symbols; i++) {
      default_map.raw[i] = ia->error_act;
    }
    for for_each_nt_fw(symbol, ls) {
      int act = gotodef.raw[symbol];
      int result_act;
      if (act < 0) {
        result_act = -act;
      } else if (act > 0) {
        result_act = toutput->state_index.raw[act] + ls->num_rules;
      } else {
        result_act = ia->error_act;
      }
      default_map.raw[toutput->symbol_map.raw[symbol]] = result_act;
    }
    for (int symbol = 1; symbol <= ls->num_symbols; symbol++) {
      itoc(default_map.raw[symbol], output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && symbol != ls->num_symbols) {
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
  common(byte_check_bit, cli_options, toutput, dss, ctp, of, ia, srt, statset, gd_index, gd_range, item_table, output_buffer, output_ptr2, symno, string_table, name, ls);
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
static long remap_non_terminals(const struct CLIOptions *cli_options, struct TableOutput *toutput, ListInt gotodef, struct statset_type *statset, struct LAState* ls) {
  // The variable FREQUENCY_SYMBOL is used to hold the non-terminals
  // in the grammar, and  FREQUENCY_COUNT is used correspondingly to
  // hold the number of actions defined on each non-terminal.
  // ORDERED_STATE and ROW_SIZE are used in a similar fashion for states
  ListInt frequency_symbol = allocateListInt(ls->num_non_terminals);
  // TODO • The size in the arraylong will be wrong?
  frequency_symbol.raw -= ls->num_terminals + 1;
  ListInt frequency_count = allocateListInt(ls->num_non_terminals);
  // TODO • The size in the arraylong will be wrong?
  frequency_count.raw -= ls->num_terminals + 1;
  ListInt row_size = allocateListInt(ls->num_states + 1);
  for for_each_nt_fw(i, ls) {
    frequency_symbol.raw[i] = i;
    frequency_count.raw[i] = 0;
  }
  for for_each_state(state_no, ls) {
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
  sortdes(frequency_symbol, frequency_count, ls->num_terminals + 1, ls->num_symbols, ls->num_states);
  for for_each_nt_fw(i, ls) {
    toutput->symbol_map.raw[frequency_symbol.raw[i]] = i;
  }
  //    All non-terminal entries in the state automaton are updated
  // accordingly.  We further subtract NUM_TERMINALS from each
  // non-terminal to make them fall in the range [1..NUM_NON_TERMINLS]
  // instead of [NUM_TERMINALS+1..NUM_SYMBOLS].
  for for_each_state(state_no, ls) {
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      go_to.map[i].symbol = toutput->symbol_map.raw[go_to.map[i].symbol] - ls->num_terminals;
    }
  }
  // If Goto-Default was requested, we find out how many non-terminals
  // were eliminated as a result, and adjust the GOTO-DEFAULT map,
  // based on the new mapping of the non-terminals.
  long last_symbol;
  if (cli_options->goto_default_bit) {
    ListInt temp_goto_default = allocateListInt(ls->num_non_terminals);
    temp_goto_default.raw -= ls->num_terminals + 1;
    for (last_symbol = ls->num_symbols; last_symbol > ls->num_terminals; last_symbol--) {
      if (frequency_count.raw[last_symbol] != 0) {
        break;
      }
    }
    last_symbol -= ls->num_terminals;
    PRNT2("Number of non-terminals eliminated: %ld", ls->num_non_terminals - last_symbol);
    // Remap the GOTO-DEFAULT map.
    // to hold the original map.
    for for_each_nt_fw(symbol, ls) {
      temp_goto_default.raw[toutput->symbol_map.raw[symbol]] = gotodef.raw[symbol];
    }
    gotodef.raw += ls->num_terminals + 1;
    ffree(gotodef.raw);
    gotodef.raw = temp_goto_default.raw;
  } else {
    last_symbol = ls->num_non_terminals;
  }
  // The states are sorted in descending order based on the number of
  // actions defined on them, and they are remapped based on the new
  // arrangement obtained by the sorting.
  sortdes(toutput->ordered_state, row_size, 1, ls->num_states, last_symbol);
  frequency_symbol.raw += ls->num_terminals + 1;
  ffree(frequency_symbol.raw);
  frequency_count.raw += ls->num_terminals + 1;
  ffree(frequency_count.raw);
  ffree(row_size.raw);
  return last_symbol;
}

/// We now overlap the non-terminal table, or more precisely, we compute the
/// starting position in a vector where each of its rows may be placed
/// without clobbering elements in another row.  The starting positions are
/// stored in the vector STATE_INDEX.
static void overlap_nt_rows(struct CLIOptions *cli_options, struct TableOutput *toutput, struct NumTableEntries *nte, struct CTabsProps *ctp, long last_symbol, struct NextPrevious* np, struct ImportantAspects* ia, struct statset_type *statset, struct LAState* ls) {
  nte->value = ls->num_gotos + ls->num_goto_reduces + ls->num_states;
  ctp->increment_size = MAX(nte->value / 100 * increment, last_symbol + 1);
  ctp->table_size = MIN(nte->value + ctp->increment_size, MAX_TABLE_SIZE);
  // Allocate space for table, and initlaize the AVAIL_POOL list.  The
  // variable FIRST_INDEX keeps track of the first element in the doubly-
  // linked list, and LAST_ELEMENT keeps track of the last element in the
  // list.
  //   The variable MAX_INDX is used to keep track of the maximum starting
  // position for a row that has been used.
  np->next = allocateListInt(ctp->table_size + 1);
  np->previous = allocateListInt(ctp->table_size + 1);
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
  for for_each_state(state_no, ls) {
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
    ctp->check_size = max_indx + ls->num_non_terminals;
  } else {
    ctp->check_size = 0;
  }
  for (ctp->action_size = max_indx + last_symbol; ctp->action_size >= max_indx; ctp->action_size--) {
    if (np->next.raw[ctp->action_size] == OMEGA) {
      break;
    }
  }
  ia->accept_act = max_indx + ls->num_rules + 1;
  ia->error_act = ia->accept_act + 1;
  printf("\n");
  if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
    PRNT2("Length of base Check Table: %d", ctp->check_size);
  }
  PRNT2("Length of base Action Table: %ld", ctp->action_size);
  PRNT2("Number of entries in base Action Table: %ld", nte->value);
  const int percentage = (ctp->action_size - nte->value) * 1000 / nte->value;
  PRNT2("Percentage of increase: %d.%d%%", percentage / 10, percentage % 10);
}

/// We now try to merge states in the terminal table that are similar.
/// Two states S1 and S2 are said to be similar if they contain the
/// same shift actions, and they reduce to the same set of rules.  In
/// addition,  there must not exist a terminal symbol "t" such that:
/// REDUCE(S1, t) and REDUCE(S2, t) are defined, and
/// REDUCE(S1, t) ^= REDUCE(S2, t)
static void merge_similar_t_rows(const struct CLIOptions *cli_options, struct TableOutput *toutput, ListBool shift_on_error_symbol, struct node **new_state_element_reduce_nodes, struct TResult *tresult, struct new_state_type *new_state_element, struct SRTable* srt, struct lastats_type *lastats, struct statset_type *statset, struct LAState* ls) {
  ListInt table = allocateListInt(ls->num_shift_maps + 1);
  tresult->top = 0;
  for (int i = 1; i <= ls->max_la_state; i++) {
    shift_on_error_symbol.raw[i] = false;
  }
  for (int i = 0; i <= ls->num_shift_maps; i++) {
    table.raw[i] = NIL;
  }
  // We now hash all the states into TABLE, based on their shift map
  // number.
  // The rules in the range of the REDUCE MAP are placed in sorted
  // order in a linear linked list headed by REDUCE_ROOT.
  for (long state_no = 1; state_no <= ls->max_la_state; state_no++) {
    struct node *reduce_root = NULL;
    struct reduce_header_type red;
    if (state_no > ls->num_states) {
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
    if (state_no > ls->num_states) {
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
static void merge_shift_domains(struct CLIOptions *cli_options, struct TableOutput *toutput, ListInt row_size, ListInt frequency_symbol, ListInt frequency_count, struct NumTableEntries *nte, ListInt shift_check_index, struct CTabsProps *ctp, struct new_state_type *new_state_element, ListInt shift_image, ListInt real_shift_number, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, ListInt shiftdf, struct LAState* ls) {
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
  ListInt shift_domain_link = allocateListInt(ctp->num_terminal_states + 1);
  ListInt ordered_shift = allocateListInt(ls->num_shift_maps + 1);
  ListBool shift_symbols = Allocate_bool_array2(ls->num_terminals + 1);
  short shift_domain_table[SHIFT_TABLE_SIZE];
  for (int i = 0; i <= SHIFT_TABLE_UBOUND; i++) {
    shift_domain_table[i] = NIL;
  }
  nte->value = 0;
  ctp->shift_domain_count = 0;
  for (int state_no = 1; state_no <= ctp->num_terminal_states; state_no++) {
    int shift_no = new_state_element[state_no].shift_number;
    for (int i = 1; i <= ls->num_terminals; i++) {
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
  for for_each_t_fw(symbol, ls) {
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
  sortdes(frequency_symbol, frequency_count, 1, ls->num_terminals, ctp->shift_domain_count);
  for for_each_t_fw(symbol, ls) {
    toutput->symbol_map.raw[frequency_symbol.raw[symbol]] = symbol;
  }
  toutput->symbol_map.raw[DEFAULT_SYMBOL] = DEFAULT_SYMBOL;
  eoft_image = toutput->symbol_map.raw[eoft_image];
  if (cli_options->error_maps_bit) {
    error_image = toutput->symbol_map.raw[error_image];
    eolt_image = toutput->symbol_map.raw[eolt_image];
  }
  for (int i = 1; i <= ls->num_shift_maps; i++) {
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
    for for_each_state(state_no, ls) {
      struct reduce_header_type red = srt->reduce[state_no];
      for (int i = 1; i <= red.size; i++) {
        red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
      }
    }
  }
  // Remap the SHIFT_DEFAULT map.
  ListInt temp_shift_default = allocateListInt(ls->num_terminals + 1);
  for for_each_t_fw(symbol, ls) {
    temp_shift_default.raw[toutput->symbol_map.raw[symbol]] = shiftdf.raw[symbol];
  }
  ffree(shiftdf.raw);
  shiftdf.raw = temp_shift_default.raw;
  // We now compute the starting position for each Shift check row
  // as we did for the terminal states.  The starting positions are
  // stored in the vector SHIFT_CHECK_INDEX.
  sortdes(ordered_shift, row_size, 1, ctp->shift_domain_count, ls->num_terminals);
  ctp->increment_size = MAX(nte->value / 100 * increment, ls->num_terminals + 1);
  int old_table_size = ctp->table_size;
  ctp->table_size = MIN(nte->value + ctp->increment_size, MAX_TABLE_SIZE);
  if ((int) ctp->table_size > old_table_size) {
    ffree(np->previous.raw);
    ffree(np->next.raw);
    np->previous = allocateListInt(ctp->table_size + 1);
    np->next = allocateListInt(ctp->table_size + 1);
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
    if (indx + ls->num_terminals > (int) ctp->table_size) {
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
  ctp->shift_check_size = max_indx + ls->num_terminals;
  printf("\n");
  PRNT2("Length of Shift Check Table: %d", ctp->shift_check_size);
  PRNT2("Number of entries in Shift Check Table: %ld", nte->value);
  int kk;
  for (kk = ctp->shift_check_size; kk >= max_indx; kk--) {
    if (np->next.raw[kk] == OMEGA) {
      break;
    }
  }
  long percentage = (kk - nte->value) * 1000 / nte->value;
  PRNT2("Percentage of increase: %ld.%ld%%", percentage/10, percentage % 10);
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
static void overlay_sim_t_rows(struct CLIOptions *cli_options, struct TableOutput *toutput, ListBool shift_on_error_symbol, struct node **new_state_element_reduce_nodes, struct TResult *tresult, struct NumTableEntries *nte, ListInt shift_check_index, struct CTabsProps *ctp, struct new_state_type *new_state_element, ListInt shift_image, ListInt real_shift_number, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, struct ruletab_type *rules, struct lastats_type *lastats, ListInt shiftdf, struct statset_type *statset, struct LAState* ls) {
  int num_shifts_saved = 0;
  int num_reductions_saved = 0;
  int default_saves = 0;
  ListInt rule_count = allocateListInt(ls->num_rules + 1);
  ListInt reduce_action = allocateListInt(ls->num_terminals + 1);
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
    if (state_no > ls->num_states) {
      red = lastats[state_no].reduce;
    } else {
      red = srt->reduce[state_no];
    }
    for for_each_t_fw(j, ls) {
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
      if (state_no > ls->num_states) {
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
      if (state_root > ls->num_states) {
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
    for for_each_t_fw(symbol, ls) {
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
      for for_each_t_fw(j, ls) {
        reduce_action.raw[j] = OMEGA;
      }
      for (; state_no != NIL; state_no = toutput->state_list.raw[state_no]) {
        if (state_no > ls->num_states) {
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
    if (state_no > ls->num_states) {
      red = lastats[state_no].reduce;
    } else {
      red = srt->reduce[state_no];
    }
    red.map[0].symbol = DEFAULT_SYMBOL;
    red.map[0].rule_number = ia->error_act;
    new_state_element[i].reduce = red;
  }
  ctp->num_terminal_states = tresult->top;
  ListInt frequency_symbol = allocateListInt(ls->num_terminals + 1);
  ListInt frequency_count = allocateListInt(ls->num_terminals + 1);
  ListInt row_size = allocateListInt(ls->max_la_state + 1);
  if (cli_options->shift_default_bit) {
    merge_shift_domains(cli_options, toutput, row_size, frequency_symbol, frequency_count, nte, shift_check_index, ctp, new_state_element, shift_image, real_shift_number, np, ia, srt, shiftdf, ls);
  }
  // We now reorder the terminal states based on the number of actions
  // in them, and remap the terminal symbols if they were not already
  // remapped in the previous block for the SHIFT_CHECK vector.
  for for_each_t_fw(symbol, ls) {
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
  PRNT2("Number of unique terminal states: %d", ctp->num_terminal_states);

  PRNT2("Number of Shift actions saved by merging: %d", num_shifts_saved);

  PRNT2("Number of Reduce actions saved by merging: %d", num_reductions_saved);

  PRNT2("Number of Reduce saved by default: %d", default_saves);

  sortdes(toutput->ordered_state, row_size, 1, ctp->num_terminal_states, ls->num_terminals);

  if (!cli_options->shift_default_bit) {
    sortdes(frequency_symbol, frequency_count, 1, ls->num_terminals, ctp->num_terminal_states);
    for for_each_t_fw(symbol, ls) {
      toutput->symbol_map.raw[frequency_symbol.raw[symbol]] = symbol;
    }
    toutput->symbol_map.raw[DEFAULT_SYMBOL] = DEFAULT_SYMBOL;
    eoft_image = toutput->symbol_map.raw[eoft_image];
    if (cli_options->error_maps_bit) {
      error_image = toutput->symbol_map.raw[error_image];
      eolt_image = toutput->symbol_map.raw[eolt_image];
    }
    for (int i = 1; i <= ls->num_shift_maps; i++) {
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
      for for_each_state(state_no, ls) {
        struct reduce_header_type red = srt->reduce[state_no];
        for (int i = 1; i <= red.size; i++) {
          red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
        }
      }
    }
  }
  nte->value = ls->num_shifts + ls->num_shift_reduces + ls->num_reductions - num_shifts_saved - num_reductions_saved - default_saves + ctp->num_terminal_states;
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
static void overlap_t_rows(struct CLIOptions *cli_options, struct TableOutput *toutput, struct NumTableEntries *nte, ListInt term_state_index, struct CTabsProps *ctp, struct new_state_type *new_state_element, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, ListInt shiftdf, struct LAState* ls) {
  ListInt terminal_list = allocateListInt(ls->num_terminals + 1);
  ctp->increment_size = MAX(nte->value * increment / 100, ls->num_terminals + 1);
  const long old_size = ctp->table_size;
  ctp->table_size = MIN(nte->value + ctp->increment_size, MAX_TABLE_SIZE);
  if ((int) ctp->table_size > old_size) {
    ffree(np->previous.raw);
    ffree(np->next.raw);
    np->next = allocateListInt(ctp->table_size + 1);
    np->previous = allocateListInt(ctp->table_size + 1);
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
    if (indx + ls->num_terminals > (int) ctp->table_size) {
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
  ctp->term_check_size = max_indx + ls->num_terminals;
  for (ctp->term_action_size = max_indx + ls->num_terminals; ctp->term_action_size >= max_indx; ctp->term_action_size--) {
    if (np->next.raw[ctp->term_action_size] == OMEGA) {
      break;
    }
  }
  printf("\n");
  PRNT2("Length of Terminal Check Table: %d", ctp->term_check_size);
  PRNT2("Length of Terminal Action Table: %d", ctp->term_action_size);
  PRNT2("Number of entries in Terminal Action Table: %ld", nte->value);
  const long percentage = (ctp->term_action_size - nte->value) * 1000 / nte->value;
  PRNT2("Percentage of increase: %ld.%ld%%", percentage / 10, percentage % 10);
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

void cmprspa(struct CLIOptions *cli_options, struct TableOutput *toutput, struct DetectedSetSizes *dss, struct CTabsProps *ctp, struct OutputFiles* of, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, struct lastats_type *lastats, ListInt shiftdf, ListInt gotodef, ListInt gd_index, ListInt gd_range, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table, char *output_buffer, struct OutputPtr output_ptr2, struct symno_type *symno, char *string_table, int *name, struct LAState* ls) {
  ListBool shift_on_error_symbol = Allocate_bool_array2(ls->max_la_state + 1);
  struct new_state_type *new_state_element;
  calloc0p(&new_state_element, ls->max_la_state + 1, struct new_state_type);
  struct node **new_state_element_reduce_nodes;
  calloc0p(&new_state_element_reduce_nodes, ls->max_la_state + 1, struct node *);
  long last_symbol = remap_non_terminals(cli_options, toutput, gotodef, statset, ls);
  struct NumTableEntries nte = (struct NumTableEntries){
    .value = 0,
  };
  overlap_nt_rows(cli_options, toutput, &nte, ctp, last_symbol, np, ia, statset, ls);
  struct TResult tresult = (struct TResult){
    .single_root = NIL,
    .multi_root = NIL,
    .empty_root = NIL,
  };
  merge_similar_t_rows(cli_options, toutput, shift_on_error_symbol, new_state_element_reduce_nodes, &tresult, new_state_element, srt, lastats, statset, ls);
  ListInt shift_check_index = allocateListInt(ls->num_shift_maps + 1);
  ListInt shift_image = allocateListInt(ls->max_la_state + 1);
  ListInt real_shift_number = allocateListInt(ls->num_shift_maps + 1);
  overlay_sim_t_rows(cli_options, toutput, shift_on_error_symbol, new_state_element_reduce_nodes, &tresult, &nte, shift_check_index, ctp, new_state_element, shift_image, real_shift_number, np, ia, srt, rules, lastats, shiftdf, statset, ls);
  ListInt term_state_index = allocateListInt(ls->max_la_state + 1);
  overlap_t_rows(cli_options, toutput, &nte, term_state_index, ctp, new_state_element, np, ia, srt, shiftdf, ls);
  print_space_parser(cli_options, toutput, dss, term_state_index, shift_check_index, ctp, new_state_element, shift_image, real_shift_number, of, ia, srt, shiftdf, gotodef, gd_index, gd_range, rules, statset, item_table, output_buffer, output_ptr2, symno, string_table, name, ls);
}
// endregion

// region timetab
/// We now remap the symbols in the unified Table based on frequency.
/// We also remap the states based on frequency.
struct DefaultSaves {
  int default_saves;
  int last_symbol;
} remap_symbols(struct TableOutput* toutput, ListBool is_terminal, struct SRTable* srt, struct lastats_type *lastats, struct statset_type *statset, struct CLIOptions* cli_options, struct LAState* ls) {
  int default_saves = 0;
  ListInt frequency_symbol = allocateListInt(ls->num_symbols + 1);
  ListInt frequency_count = allocateListInt(ls->num_symbols + 1);
  ListInt row_size = allocateListInt(ls-> max_la_state + 1);
  printf("\n");
  //     The variable FREQUENCY_SYMBOL is used to hold the symbols
  // in the grammar,  and the variable FREQUENCY_COUNT is used
  // correspondingly to hold the number of actions defined on each
  // symbol.
  // ORDERED_STATE and ROW_SIZE are used in a similar fashion for
  // states.
  for (int i = 1; i <= ls->num_symbols; i++) {
    frequency_symbol.raw[i] = i;
    frequency_count.raw[i] = 0;
  }
  for for_each_state(state_no, ls) {
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
  PRNT2("Number of Reductions saved by default: %d", default_saves);
  for for_each_la_state(state_no, ls) {
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
  sortdes(frequency_symbol, frequency_count, 1, ls->num_terminals, ls->max_la_state);
  sortdes(frequency_symbol, frequency_count, ls->num_terminals + 1, ls->num_symbols, ls->max_la_state);
  long last_symbol;
  for (last_symbol = ls->num_symbols; last_symbol > ls->num_terminals; last_symbol--) {
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
  long j = ls->num_terminals + 1;
  int k = 0;
  for (int i = 1; i <= ls->num_terminals;) {
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
  for (; j <= ls->num_symbols; j++) {
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
  for for_each_state(state_no, ls) {
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
  for for_each_la_state(state_no, ls) {
    struct reduce_header_type red = lastats[state_no].reduce;
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
    }
  }
  for (int i = 1; i <= ls->num_shift_maps; i++) {
    struct shift_header_type sh = srt->shift[i];
    for (int j = 1; j <= sh.size; j++) {
      sh.map[j].symbol = toutput->symbol_map.raw[sh.map[j].symbol];
    }
  }
  sortdes(toutput->ordered_state, row_size, 1, ls->max_la_state, ls->num_symbols);
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
static void overlap_tables(struct CLIOptions *cli_options, struct TableOutput* toutput, ListBool is_terminal, struct DefaultSaves default_saves, struct CTabsProps* ctp, long last_symbol, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, struct lastats_type *lastats, struct statset_type *statset, struct LAState* ls) {
  ListInt symbol_list = allocateListInt(ls->num_symbols + 1);
  ls->num_entries -= default_saves.default_saves;
  ctp->increment_size = MAX(ls->num_entries * increment / 100, ls->num_symbols + 1);
  ctp->table_size = MIN(ls->num_entries + ctp->increment_size, MAX_TABLE_SIZE);
  // Allocate space for table, and initialize the AVAIL_POOL list.
  // The variable FIRST_INDEX keeps track of the first element in the
  // doubly-linked list, and LAST_ELEMENT keeps track of the last
  // element in the list.
  // The variable MAX_INDX is used to keep track of the maximum
  // starting position for a row that has been used.
  np->next = allocateListInt(ctp->table_size + 1);
  np->previous = allocateListInt(ctp->table_size + 1);
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
  for (int k = 1; k <= ls->max_la_state; k++) {
    const long state_no = toutput->ordered_state.raw[k];
    // First, we iterate over all actions defined in STATE_NO, and
    // create a set with all the symbols involved.
    int root_symbol = NIL;
    struct shift_header_type sh;
    struct reduce_header_type red;
    if (state_no > ls->num_states) {
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
    if (indx + ls->num_symbols > (int) ctp->table_size) {
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
  ctp->table_size = max_indx + ls->num_symbols;
  ia->accept_act = max_indx + ls->num_rules + 1;
  ia->error_act = ia->accept_act + 1;
  for (ctp->action_size = ctp->table_size; ctp->action_size >= max_indx; ctp->action_size--) {
    if (np->next.raw[ctp->action_size] == OMEGA) {
      break;
    }
  }
  printf("\n");
  PRNT2("Length of Check table: %ld", ctp->table_size);
  PRNT2("Length of Action table: %ld", ctp->action_size);
  PRNT2("Number of entries in Action Table: %ld", ls->num_entries);
  const long percentage = (ctp->action_size - ls->num_entries) * 1000 / ls->num_entries;
  PRNT2("Percentage of increase: %ld.%ld%%", percentage / 10, percentage % 10);
  long num_bytes;
  if (cli_options->byte_bit) {
    num_bytes = 2 * ctp->action_size + ctp->table_size;
    if (!cli_options->goto_default_bit && !cli_options->nt_check_bit) {
      for (; last_symbol >= 1 && !is_terminal.raw[last_symbol]; last_symbol--) {
      }
    }
    PRNT2("Highest symbol in Check Table: %ld", last_symbol);
    if (last_symbol > 255) {
      num_bytes += ctp->table_size;
    }
  } else {
    num_bytes = 2 * (ctp->action_size + ctp->table_size);
  }
  if (cli_options->goto_default_bit) {
    num_bytes += (long) 2 * ls->num_symbols;
  }
  const long k_bytes = num_bytes / 1024 + 1;
  PRNT2("Storage Required for Tables: %ld Bytes, %ldK", num_bytes, k_bytes);
  num_bytes = (long) 4 * ls->num_rules;
  if (cli_options->byte_bit) {
    num_bytes -= ls->num_rules;
    if (ls->num_symbols < 256) {
      num_bytes -= ls->num_rules;
    }
  }
  PRNT2("Storage Required for Rules: %ld Bytes", num_bytes);
}

/// In this routine we compress the State tables and write them out
/// to a file. The emphasis here is in generating tables that allow
/// fast access. The terminal and non-terminal tables are compressed
/// together, to achieve maximum speed efficiency.
/// Otherwise, the compression technique used in this table is
/// analogous to the technique used in the routine CMPRSPA.
void cmprtim(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, struct lastats_type *lastats, ListInt gotodef, ListInt gd_index, ListInt gd_range, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table, char *output_buffer, struct OutputPtr output_ptr2, struct symno_type *symno, char *string_table, int *name, struct LAState* ls) {
  ListBool is_terminal = Allocate_bool_array2(ls->num_symbols + 1);
  struct DefaultSaves default_saves = remap_symbols(toutput, is_terminal, srt, lastats, statset, cli_options, ls);
  overlap_tables(cli_options, toutput, is_terminal, default_saves, ctp, default_saves.last_symbol, np, ia, srt, lastats, statset, ls);
  print_time_parser(cli_options, toutput, dss, ctp, of, np, ia, srt, lastats, gotodef, gd_index, gd_range, rules, statset, item_table, output_buffer, output_ptr2, symno, string_table, name, ls);
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
static void compute_shift_default(struct SRTable* srt, struct lastats_type *lastats, ListInt* shiftdf, struct statset_type *statset, struct LAState* ls) {
  // Set up a pool of temporary space.
  reset_temporary_space();
  int shift_count = 0;
  int shift_reduce_count = 0;
  *shiftdf = allocateListInt(ls->num_terminals + 1);
  struct ptables_action_element **action_count;
  calloc0p(&action_count, ls->num_terminals + 1, struct ptables_action_element *);
  // For each state, invoke PROCESS_SHIFT_ACTIONS to process the
  // shift map associated with that state.
  for for_each_state(state_no, ls) {
    process_shift_actions(action_count, statset[state_no].shift_number, srt);
  }
  for for_each_la_state(state_no, ls) {
    process_shift_actions(action_count, lastats[state_no].shift_number, srt);
  }
  // We now iterate over the ACTION_COUNT mapping, and for each
  // terminal t, initialize SHIFTDF[t] to the action that is most
  // frequently defined on t.
  for for_each_t_fw(symbol, ls) {
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
  PRNT2("Number of Shift entries saved by default: %d", shift_count);
  PRNT2("Number of Shift/Reduce entries saved by default: %d", shift_reduce_count);
  ls->num_shifts -= shift_count;
  ls->num_shift_reduces -= shift_reduce_count;
  ls->num_entries = ls->num_entries - shift_count - shift_reduce_count;
  ffree(action_count);
}

/// COMPUTE_GOTO_DEFAULT constructs the vector GOTODEF, which is indexed by
/// the non-terminals in the grammar. Its task is to assign to each element
/// of the array the Action which is most frequently defined on the symbol in
/// question, and remove all such actions from the state automaton.
static void compute_goto_default(ListInt* gotodef, struct statset_type *statset, struct LAState* ls) {
  // Set up a pool of temporary space.
  reset_temporary_space();
  int goto_count = 0;
  int goto_reduce_count = 0;
  *gotodef = allocateListInt(ls->num_non_terminals);
  gotodef->raw -= ls->num_terminals + 1;
  struct ptables_action_element **action_count;
  calloc0p(&action_count, ls->num_non_terminals, struct ptables_action_element *);
  action_count -= ls->num_terminals + 1;
  if (action_count == NULL) {
    nospace();
  }
  // The array ACTION_COUNT is used to construct a map from each
  // non-terminal into the set (list) of actions defined on that
  // non-terminal. A count of how many occurences of each action
  // is also kept.
  // This loop is analoguous to the loop in PROCESS_SHIFT_ACTIONS.
  for for_each_state(state_no, ls) {
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
  for for_each_nt_fw(symbol, ls) {
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
  for for_each_state(state_no, ls) {
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
  PRNT2("Number of Goto entries saved by default: %d", goto_count);
  PRNT2("Number of Goto/Reduce entries saved by default: %d", goto_reduce_count);
  ls->num_gotos -= goto_count;
  ls->num_goto_reduces -= goto_reduce_count;
  ls->num_entries = ls->num_entries - goto_count - goto_reduce_count;
  action_count += ls->num_terminals + 1;
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
void process_tables(char *tab_file, struct OutputFiles *output_files, struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, ListInt gd_range, struct SRTable* srt, struct lastats_type *lastats, ListInt* shiftdf, ListInt* gotodef, ListInt gd_index, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table, struct symno_type *symno, char *string_table, int *name, struct LAState* ls) {
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
  ls->num_terminals--;
  ls->num_symbols--;
  // Remap all the symbols used in GOTO and REDUCE actions.
  // Remap all the symbols used in GD_RANGE.
  // Release space trapped by the maps IN_STAT and FIRST.
  for for_each_state(state_no, ls) {
    const struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      go_to.map[i].symbol--;
    }
    struct reduce_header_type red = srt->reduce[state_no];
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol--;
    }
  }
  for for_each_la_state(state_no, ls) {
    struct reduce_header_type red = lastats[state_no].reduce;
    for (int i = 1; i <= red.size; i++)
      red.map[i].symbol--;
  }
  for (int i = 1; i <= ls->gotodom_size; i++) {
    gd_range.raw[i]--;
  }
  // Remap all symbols in the domain of the Shift maps.
  for (int i = 1; i <= ls->num_shift_maps; i++) {
    const struct shift_header_type sh = srt->shift[i];
    for (int j = 1; j <= sh.size; j++) {
      sh.map[j].symbol--;
    }
  }
  // Remap the left-hand side of all the rules.
  for for_each_rule_fw(rule_no, ls) {
    rules[rule_no].lhs--;
  }
  // Remap the dot symbols in ITEM_TABLE.
  if (cli_options->error_maps_bit) {
    for for_each_item(item_no, ls) {
      item_table[item_no].symbol--;
    }
  }
  // We update the SYMNO map.
  for for_each_symbol(symbol, ls) {
    symno[symbol] = symno[symbol + 1];
  }
  // If Goto Default and/or Shift Default were requested, process
  // appropriately.
  if (cli_options->shift_default_bit) {
    compute_shift_default(srt, lastats, shiftdf, statset, ls);
  }
  if (cli_options->goto_default_bit) {
    compute_goto_default(gotodef, statset, ls);
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
  struct TableOutput toutput = init_table_output(ls);
  init_file(&of->sysdcl, output_files->dcl_file, of->dcl_tag);
  init_file(&of->syssym, output_files->sym_file, of->sym_tag);
  init_file(&of->sysdef, output_files->def_file, of->def_tag);
  init_file(&of->sysprs, output_files->prs_file, of->prs_tag);
  struct ImportantAspects ia = (struct ImportantAspects) {};
  if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
    cmprspa(cli_options, &toutput, dss, ctp, of, np, &ia, srt, lastats, *shiftdf, *gotodef, gd_index, gd_range, statset, rules, item_table, *op.output_buffer, op, symno, string_table, name, ls);
  } else if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    cmprtim(cli_options, &toutput, dss, ctp, of, np, &ia, srt, lastats, *gotodef, gd_index, gd_range, statset, rules, item_table, *op.output_buffer, op, symno, string_table, name, ls);
  } else {
    exit(999);
  }
  if (!cli_options->c_bit && !cli_options->cpp_bit && !cli_options->java_bit) {
    fclose(systab);
  }
}
// endregion

























/// This fork of jikespg was meant to clean up jikespg while maintaining its functionality.
/// Unfortunately, the lack of an expressive type system in C makes it very difficult
/// to do that. This fork is a reduced version of the original repo that contains its core
/// functionality with some bugs. Its purpose now is educational to learn about Philippe's
/// Dissertation.
int main(const int argc, char *argv[]) {
  if (argc == 1 || argv[1][0] == '?') {
    // We display the help screen if only "jikespg" or "jikespg ?*" is typed.
    printf(
      "Usage: jikespg [options] [filename[.extension]]\n"
      "AVAILABLE ACTIONS                              \n"
      "-fileprefix=string                             \n"
      "-actfilename=string                            \n"
      "-hactfilename=string                           \n"
      "-prefix=string                                 \n"
      "-suffix=string                                 \n"
      "-byte                                          \n"
      "-conflicts                                     \n"
      "-default=<0|1|2|3|4|5>                         \n"
      "-generateparser=string                         \n"
      "-gotodefault                                   \n"
      "-lalr=integer                                  \n"
      "-ntcheck                                       \n"
      "-escape=character                              \n"
      "-ormark=character                              \n"
      "-readreduce                                    \n"
      "-shift-default                                 \n"
      "-table=<space|time>                            \n"
      "-trace=<CONFLICTS|FULL|NO>                     \n"
      "-maxdistance=integer                           \n"
      "-mindistance=integer                           \n"
      "-stack-size=integer                            \n"
      "                                               \n"
      "Options must be separated by a space.          \n"
      "Options that are switches may be               \n"
      "negated by prefixing them with the             \n"
      "string \"no\". Default input file              \n"
      "extension is \".g\"                            \n"
    );
    return 4;
  } else {
    // Prepare
    struct CLIOptions cli_options = init_cli_options();
    struct OutputFiles of = {
      .prs_file = "",
      .sym_file = "",
      .def_file = "",
      .dcl_file = "",
    };
    char tab_file[80];
    ListInt rhs_sym;
    struct ruletab_type *rules;
    struct ParserState ps = (struct ParserState) {
      .hash_table = NULL,
      .error_maps_bit = cli_options.error_maps_bit,
      .num_acts = 0,
      .num_defs = 0,
      .defelmt_size = 0,
      .actelmt_size = 0,
      .rulehdr_size = 0,
      .string_offset = 0,
      .stack_top = -1,
      .string_table = NULL,
      .num_items = 0,
      .num_names = 0,
      .num_rules = 0,
      .num_symbols = 0,
      .num_terminals = 0,
    };
    /// NAME is an array containing names to be associated with symbols.
    int *name;
    char grm_file[80];
    // Create file names for output files
    strcpy(grm_file, argv[argc - 1]);
    const char *slash = strrchr(grm_file, '/');
    char tmpbuf[20];
    if (slash != NULL) {
      strcpy(tmpbuf, slash + 1);
    } else {
      strcpy(tmpbuf, grm_file);
    }
    const char *dot = strrchr(tmpbuf, '.');
    // if filename has no extension, copy it.
    char file_prefix[80] = "";
    if (dot == NULL) {
      strcpy(tab_file, tmpbuf);
      int ii;
      for (ii = 0; ii < 5; ii++) {
        file_prefix[ii] = tmpbuf[ii];
      }
      file_prefix[ii] = '\0';
    } else {
      int ii;
      // if file name contains an extension copy up to the dot
      for (ii = 0; ii < 5 && tmpbuf + ii != dot; ii++) {
        file_prefix[ii] = tmpbuf[ii];
      }
      file_prefix[ii] = '\0';
      memcpy(tab_file, tmpbuf, dot - tmpbuf);
      tab_file[dot - tmpbuf] = '\0';
    }
    strcat(tab_file, ".t"); /* add .t extension for table file */
    // This routine is invoked to allocate space for the global structures
    // needed to process the input grammar.
    //
    // Set up a pool of temporary space.
    reset_temporary_space();
    process_input(grm_file, &of, argc, argv, file_prefix, &cli_options, &rhs_sym, &rules, &ps, &name);
    struct LAState ls = (struct LAState) {
      .max_la_state = 0,
      .num_states = 0,
      .num_shifts = 0,
      .gotodom_size = 0,
      .num_shift_maps = 0,
      .num_shift_reduces = 0,
      .num_gotos = 0,
      .num_goto_reduces = 0,
      .num_reductions = 0,
      .num_entries = 0,
      .num_items = ps.num_items,
      .num_symbols = ps.num_symbols,
      .num_names = ps.num_names,
      .num_non_terminals = ps.num_non_terminals,
      .num_rules = ps.num_rules,
      .num_terminals = ps.num_terminals,
    };
    JBitset follow = {.raw = NULL};
    JBitset first;
    struct FirstDeps fd = (struct FirstDeps) {
      .adequate_item = NULL,
      .clitems = NULL,
      .closure = NULL,
    };
    struct itemtab *item_table = NULL;
    struct DetectedSetSizes dss = mkbasic(&cli_options, follow, &first, &fd, rules, rhs_sym, &item_table, ps.string_table, ps.symno, &ls);
    struct SRTable srt = (struct SRTable) {
      .reduce = NULL,
      .shift = NULL,
    };
    ListInt gd_range;
    ListInt gd_index;
    struct StatSet ss = (struct StatSet) {
      .statset = NULL,
    };
    mkstats(&cli_options, fd.clitems, fd.closure, &srt, item_table, rules, &gd_range, &gd_index, &ss, &ls);
    // Main routine.
    struct LaStats las = (struct LaStats) {
      .lastats = NULL,
    };
    struct ConflictCounter conflicts = mkrdcts(&cli_options, &fd, &dss, first, fd.adequate_item, &srt, rules, ss.statset, item_table, rhs_sym, &las, &ps, &ls);
    // Output more basic statistics.
    {
      PRNT2("Number of Terminals: %ld", ls.num_terminals - 1); /*-1 for %empty */
      PRNT2("Number of Nonterminals: %ld", ls.num_non_terminals - 1); /* -1 for %ACC */
      PRNT2("Number of Productions: %ld", ls.num_rules + 1);
      PRNT2("Number of Items: %ld", ls.num_items);
      PRNT2("Number of States: %ld", ls.num_states);
      if (ls.max_la_state > ls.num_states) {
        PRNT2("Number of look-ahead states: %ld", ls.max_la_state - ls.num_states);
      }
      PRNT2("Number of Shift actions: %ld", ls.num_shifts);
      PRNT2("Number of Goto actions: %ld", ls.num_gotos);
      if (cli_options.read_reduce_bit) {
        PRNT2("Number of Shift/Reduce actions: %ld", ls.num_shift_reduces);
        PRNT2("Number of Goto/Reduce actions: %ld", ls.num_goto_reduces);
      }
      PRNT2("Number of Reduce actions: %ld", ls.num_reductions);
      PRNT2("Number of Shift-Reduce conflicts: %ld", conflicts.num_sr_conflicts);
      PRNT2("Number of Reduce-Reduce conflicts: %ld", conflicts.num_rr_conflicts);
    }
    if (cli_options.table_opt.value != OPTIMIZE_NO_TABLE.value) {
      if (cli_options.goto_default_bit && cli_options.nt_check_bit) {
        PRNTERR("The options GOTO_DEFAULT and NT_CHECK are incompatible. Tables not generated");
      } else {
        // Prepare table processing.
        {
          ls.num_entries = ls.max_la_state + ls.num_shifts + ls.num_shift_reduces + ls.num_gotos + ls.num_goto_reduces + ls.num_reductions;
          // We release space used by RHS_SYM, the ADEQUATE_ITEM
          // map, ITEM_TABLE (if we don't have to dump error maps),
          // FIRST, NULL_NT and FOLLOW (if it's no longer
          // needed).
          ffree(rhs_sym.raw);
          if (fd.adequate_item != NULL) {
            for for_each_rule_fw(rule_no, &ls) {
              struct node *q = fd.adequate_item[rule_no];
              if (q != NULL) {
                free_nodes(q, q);
              }
            }
            ffree(fd.adequate_item);
          }
          if (!cli_options.error_maps_bit) {
            ffree(item_table);
          }
          ffree(first.raw);
          dss.null_nt.raw += ls.num_terminals + 1;
          ffree(dss.null_nt.raw);
          if (follow.raw != NULL) {
            if (!cli_options.error_maps_bit || cli_options.c_bit || cli_options.cpp_bit || cli_options.java_bit) {
              follow.raw += (ls.num_terminals + 1) * dss.term_set_size;
              ffree(follow.raw);
            }
          }
        }
        // Process tables.
        {
          struct CTabsProps ctp = (struct CTabsProps) {
            .last_non_terminal = 0,
            .last_terminal = 0,
          };
          struct NextPrevious np = (struct NextPrevious) {
            .previous = NULL,
            .next = NULL,
          };
          ListInt shiftdf;
          ListInt gotodef;
          process_tables(tab_file, &of, &cli_options, &dss, &ctp, &of, &np, gd_range, &srt, las.lastats, &shiftdf, &gotodef, gd_index, ss.statset, rules, item_table, ps.symno, ps.string_table, name, &ls);
        }
      }
    }
    return 0;
  }
}
