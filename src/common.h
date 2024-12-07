#pragma once
#include <ctype.h>

static char hostfile[];

#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>


// region bitsets
/* These variables hold the number of BOOLEAN_CELLS required to form a */
/* set of terminals, non-terminals and states, respectively.           */
extern int term_set_size;
extern int non_term_set_size;
extern int state_set_size;

/**                      GLOBAL DECLARATIONS                      **/
typedef unsigned int BOOLEAN_CELL;
/* Basic unit used to represent */
/* Bit sets                     */
typedef BOOLEAN_CELL *SET_PTR;

/**                         BIT SET MACROS                        **/
/* The following macros are used to define operations on sets that */
/* are represented as bit-strings.  BOOLEAN_CELL is a type that is */
/* used as the elemental unit used to construct the sets.  For     */
/* example, if BOOLEAN_CELL consists of four bytes and assumming   */
/* that each byte contains 8 bits then the constant SIZEOF_BC      */
/* represents the total number of bits that is contained in each   */
/* elemental unit.                                                 */
/*                                                                 */
/* In general, a parameter called "set" or "set"i, where i is an   */
/* integer, is a pointer to a set or array of sets; a parameter    */
/* called "i" or "j" represents an index in an array of sets; a    */
/* parameter called "b" represents a particular element (or bit)   */
/* within a set.                                                   */
static const int SIZEOF_BC = sizeof(BOOLEAN_CELL) * CHAR_BIT;
static const int BC_OFFSET = SIZEOF_BC - 1;

/* This macro takes as argument an array of bit sets called "set", */
/* an integer "nt" indicating the index of a particular set in the */
/* array and an integer "t" indicating a particular element within */
/* the set. IS_IN_SET check whether ot not the element "t" is in   */
/* the set "set(nt)".                                              */
/*                                                                 */
/* The value (nt*term_set_size) is used to determine the starting  */
/* address of the set element in question.  The value              */
/* (??? / SIZEOF_BC) is used to determine the actual BOOLEAN_CELL  */
/* containing the bit in question.  Finally, the value             */
/* (SIZEOF_BC - (t % SIZEOF_BC)) identifies the actual bit in the  */
/* unit. The bit in question is pushed to the first position and   */
/* and-ed with the value 01. This operation yields the value TRUE  */
/* if the bit is on. Otherwise, the value FALSE is obtained.       */
/* Recall that in C, one cannot shift (left or right) by 0. This   */
/* is why the ? is used here.                                      */
static bool IS_IN_SET(const SET_PTR set, const int i, const int b) {
  /* is b in set[i] ? */
  return set[i * term_set_size + (b - 1) / SIZEOF_BC] &
         ((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

/* The macro SET_UNION takes as argument two arrays of sets:       */
/* "set1" and "set2", and two integers "i" and "j" which are       */
/* indices to be used to access particular sets in "set1" and      */
/* "set2", respectively.  SET_UNION computes the union of the two  */
/* sets in question and places the result in the relevant set in   */
/* "set1".                                                         */
/*                                                                 */
/* The remaining macros are either analogous to IS_IN_SET or       */
/* SET_UNION.                                                      */
/*                                                                 */
/* Note that a macro with the variable "kji" declared in its body  */
/* should not be invoked with a parameter of the same name.        */
/* set[i] union set2[j] */
static void SET_UNION(const SET_PTR set1, const int i, const SET_PTR set2, const int j) {
  for (register int kji = 0; kji < term_set_size; kji++) {
    set1[i * term_set_size + kji] |= set2[j * term_set_size + kji];
  }
}

/* set = {} */
static void INIT_SET(const SET_PTR set) {
  for (register int kji = 0; kji < term_set_size; kji++) {
    set[kji] = 0;
  }
}

/* set1[i] = set2[j] */
static void ASSIGN_SET(const SET_PTR set1, const int i, const SET_PTR set2, const int j) {
  for (register int kji = 0; kji < term_set_size; kji++) {
    set1[i * term_set_size + kji] = set2[j * term_set_size + kji];
  }
}

/* set[i] = set[i] with b; */
static void SET_BIT_IN(const SET_PTR set, const int i, const int b) {
  set[i * term_set_size + (b - 1) / SIZEOF_BC] |=
      (b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1;
}

/* set[i] = set[i] less b; */
static void RESET_BIT_IN(const SET_PTR set, const int i, const int b) {
  set[i * term_set_size + (b - 1) / SIZEOF_BC] &=
      ~((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

/* The following macros are analogous to the ones above, except    */
/* that they deal with sets of non-terminals instead of sets of    */
/* terminals.                                                      */
/* is b in set[i] ? */
static bool IS_IN_NTSET(const SET_PTR set, const int i, const int b) {
  return set[i * non_term_set_size + (b - 1) / SIZEOF_BC] &
         ((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

/* set1[i] union set2[j] */
static void NTSET_UNION(const SET_PTR set1, const int i, const SET_PTR set2, const int j) {
  for (register int kji = 0; kji < non_term_set_size; kji++) {
    set1[i * non_term_set_size + kji] |= set2[j * non_term_set_size + kji];
  }
}

/* set = {} */
static void INIT_NTSET(const SET_PTR set) {
  for (register int kji = 0; kji < non_term_set_size; kji++) {
    set[kji] = 0;
  }
}

/* set1[i] = set2[j] */
static void ASSIGN_NTSET(const SET_PTR set1, const int i, const SET_PTR set2, const int j) {
  for (register int kji = 0; kji < non_term_set_size; kji++) {
    set1[i * non_term_set_size + kji] = set2[j * non_term_set_size + kji];
  }
}

/* set[i] = set[i] with b; */
static void NTSET_BIT_IN(const SET_PTR set, const int i, const int b) {
  set[i * non_term_set_size + (b - 1) / SIZEOF_BC] |=
      (b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1;
}

/* set[i] = set[i] less b; */
static void NTRESET_BIT_IN(const SET_PTR set, const int i, const int b) {
  set[i * non_term_set_size + (b - 1) / SIZEOF_BC] &=
      ~((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

/* The following macros are analogous to the ones above, except    */
/* that they deal with sets of states instead of sets of terminals */
/* or non-terminals.                                               */
static void SET_COLLECTION_BIT(const SET_PTR collection, const int i, const int b) {
  collection[i * state_set_size + (b - 1) / SIZEOF_BC] |=
      (b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1;
}

static void EMPTY_COLLECTION_SET(const SET_PTR collection, const int i) {
  for (register int kji = 0; kji < state_set_size; kji++) {
    collection[i * state_set_size + kji] = 0;
  }
}

/* The following macros can be used to check, set, or reset a bit  */
/* in a bit-string of any length.                                  */
static void SET_BIT(const SET_PTR set, const int b) {
  set[(b - 1) / SIZEOF_BC] |= (b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1;
}

static void RESET_BIT(const SET_PTR set, const int b) {
  set[(b - 1) / SIZEOF_BC] &=
      ~((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

/* is b in set ? */
static bool IS_ELEMENT(const SET_PTR set, const int b) {
  return set[(b - 1) / SIZEOF_BC] &
         ((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

static void B_ASSIGN_SET(const SET_PTR s1, const int dest, const SET_PTR s2, const int source, const int bound) {
  for (int j = 0; j < bound; j++) {
    s1[dest * bound + j] = s2[source * bound + j];
  }
}

static void B_SET_UNION(const SET_PTR s1, const int dest, const SET_PTR s2, const int source, const int bound) {
  for (int j = 0; j < bound; j++) {
    s1[dest * bound + j] |= s2[source * bound + j];
  }
}

/*                               EQUAL_SETS:                                */
/* EQUAL_SETS checks to see if two sets are equal and returns True or False */
static bool equal_sets(const SET_PTR set1, const int indx1, const SET_PTR set2, const int indx2, const int bound) {
  for (register int i = 0; i < bound; i++) {
    if (set1[indx1 * bound + i] != set2[indx2 * bound + i])
      return false;
  }
  return true;
}

// endregion


#ifndef COMMON_INCLUDED
#define COMMON_INCLUDED

/* One of the switches below may have to be set prior to building  */
/* JIKES PG. OS2 is for all C compilers running under OS2. DOS is  */
/* for all C compilers running under DOS. Note that to run under   */
/* DOS, the compiler used must support the Huge model (32-bit ptr  */
/* simulation). CW is for the Waterloo C compiler running under VM/CMS. */
/*                                                                 */
/* This system was built to run on a vanilla Unix or AIX system.   */
/* No switch need to be set for such an environment.  Set other    */
/* switch(es) as needed.                                           */

#include <assert.h>
#include <stdio.h>

static const int MAX_PARM_SIZE = 22;
static const int SYMBOL_SIZE = 256;
static const int MAX_MSG_SIZE = 256 + SYMBOL_SIZE;
static const int PRINT_LINE_SIZE = 80;
static const int PARSER_LINE_SIZE = 80;
static const int MAX_LINE_SIZE = 256;

static const int PAGE_SIZE = 55;
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

/**                                                               **/
/**                          PARSING MACROS                       **/
/**                                                               **/
/* The following macro definitions are used only in processing the */
/* input source.                                                   */

static const int HT_SIZE = 701; /* 701 is a prime */
static const int RULEHDR_INCREMENT = 1024;
static const int ACTELMT_INCREMENT = 1024;
static const int DEFELMT_INCREMENT = 16;

static const int IOBUFFER_SIZE = 655360;

/**                         ITERATION MACROS                      **/
/* The following macros (ALL_) are used to iterate over a sequence.*/
#define ALL_LA_STATES(indx) (indx = num_states + 1; indx <= max_la_state; indx++)

#define ALL_TERMINALS(indx) (indx = 1; indx <= num_terminals; indx++)

#define ALL_TERMINALS_BACKWARDS(indx) (indx = num_terminals; indx >= 1; indx--)

#define ALL_NON_TERMINALS(indx) (indx = num_terminals + 1; indx <= num_symbols; indx++)

#define ALL_NON_TERMINALS_BACKWARDS(indx) (indx = num_symbols; indx >= num_terminals + 1; indx--)

#define ALL_SYMBOLS(indx) (indx = 1; indx <= num_symbols; indx++)

#define ALL_ITEMS(indx) (indx = 1; indx <= num_items; indx++)

#define ALL_STATES(indx) (indx = 1; indx <= num_states; indx++)

#define ALL_RULES(indx) (indx = 0; indx <= num_rules; indx++)

#define ALL_RULES_BACKWARDS(indx) (indx = num_rules; indx >= 0; indx--)

#define ENTIRE_RHS(indx, rule_no) (indx = rules[rule_no].rhs; indx < rules[(rule_no) + 1].rhs; indx++)

extern const char HEADER_INFO[];
extern const char VERSION[];
extern const char BLANK[];
extern const long MAX_TABLE_SIZE;

struct node {
  struct node *next;
  int value;
};

/* RULES is the structure that contain the rules of the grammar.   */
/* Every rule of the grammar is mapped into an integer, and given  */
/* rule, and we have access to a value RHS which is the index      */
/* location in the vector RHS where the right-hand-side of the rule*/
/* begins.  The right hand side of a certain rule represented by an*/
/* integer I starts at index location RULES[I].RHS in RHS, and     */
/* ends at index location RULES[I + 1].RHS - 1.  An extra          */
/* NUM_RULES + 1 element is used as a "fence" for the last rule.   */
/* The RHS vector as mentioned above is used to hold a complete    */
/* list of allthe right-hand-side symbols specified in the grammar.*/
struct ruletab_type {
  short lhs;
  short rhs;
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

extern char *timeptr;

extern long output_line_no;

extern char grm_file[],
    lis_file[],
    act_file[],
    hact_file[],
    tab_file[],
    prs_file[],
    sym_file[],
    def_file[],
    dcl_file[],
    file_prefix[],
    prefix[],
    suffix[],
    parm[],
    msg_line[];

extern FILE *syslis,
    *sysgrm,
    *sysact,
    *syshact,
    *systab,
    *syssym,
    *sysprs,
    *sysdcl,
    *sysprs,
    *sysdef;

/*  The variables below are global counters.          */
extern long num_items;
extern int num_states;
extern int max_la_state;

extern int num_symbols,
    symno_size, /* NUM_SYMBOLS + 1 */
    num_names,
    num_terminals,
    num_non_terminals,
    num_rules,
    num_conflict_elements,
    num_single_productions,
    gotodom_size;

static bool IS_A_TERMINAL(const int i) {
  return i <= num_terminals;
}

static bool IS_A_NON_TERMINAL(const int i) {
  return i > num_terminals;
}

/*  The variables below are used for options setting. */
extern bool list_bit,
    slr_bit,
    verbose_bit,
    first_bit,
    follow_bit,
    action_bit,
    edit_bit,
    states_bit,
    xref_bit,
    nt_check_bit,
    conflicts_bit,
    read_reduce_bit,
    goto_default_bit,
    shift_default_bit,
    byte_bit,
    warnings_bit,
    single_productions_bit,
    error_maps_bit,
    debug_bit,
    deferred_bit,
    c_bit,
    cpp_bit,
    java_bit,
    jikes_bit, /* undocumented hack for special jikes behavior */
    scopes_bit;

extern int lalr_level,
    default_opt,
    trace_opt,
    table_opt,
    names_opt,
    increment,
    maximum_distance,
    minimum_distance,
    stack_size;

extern char escape,
    ormark,
    record_format;

extern char blockb[],
    blocke[],
    hblockb[],
    hblocke[],
    errmsg[],
    gettok[],
    smactn[],
    tkactn[];

/*   The variables below are used to hold information about special  */
/* grammar symbols.                                                  */
extern int accept_image;
extern int eoft_image;
extern int eolt_image;
extern int empty;
extern int error_image;

/* Miscellaneous counters. */

extern int num_first_sets,
    num_shift_maps,
    page_no;

extern long string_offset,
    string_size,
    num_shifts,
    num_shift_reduces,
    num_gotos,
    num_goto_reduces,
    num_reductions,
    num_sr_conflicts,
    num_rr_conflicts,
    num_entries;

extern short *rhs_sym;

extern struct ruletab_type *rules;

static int RHS_SIZE(const int rule_no) {
  return rules[rule_no + 1].rhs - rules[rule_no].rhs;
}

/* CLOSURE is a mapping from non-terminal to a set (linked-list) of    */
/* non-terminals.  The set consists of non-terminals that are          */
/* automatically introduced via closure when the original non-terminal */
/* is introduced.                                                      */
/* CL_ITEMS is a mapping from each non-terminal to a set (linked list) */
/* of items which are the first item of the rules generated by the     */
/* non-terminal in question. ADEQUATE_ITEM is a mapping from each rule */
/* to the last (complete) item produced by that rule.                  */
/* ITEM_TABLE is used to map each item into a number. Given that       */
/* number one can retrieve the rule the item belongs to, the position  */
/* of the dot,  the symbol following the dot, and FIRST of the suffix  */
/* following the "dot symbol".                                         */
extern struct node **closure,
    **clitems,
    **adequate_item;

extern struct itemtab {
  short symbol,
      rule_number,
      suffix_index,
      dot;
} *item_table;

/* SYMNO is an array that maps symbol numbers to actual symbols.       */
extern struct symno_type {
  int ptr,
      name_index;
} *symno;

/* NULL_NT is a boolean vector that indicates whether or not a given   */
/* non-terminal is nullable.                                           */
extern bool *null_nt;

/* FOLLOW is a mapping from non-terminals to a set of terminals that   */
/* may appear immediately after the non-terminal.                      */
extern SET_PTR nt_first,
    first,
    follow;

/* NAME is an array containing names to be associated with symbols.    */
/* REDUCE is a mapping from each state to reduce actions in that state.*/
/* SHIFT is an array used to hold the complete set of all shift maps   */
/* needed to construct the state automaton. Though its size is         */
/* NUM_STATES, the actual number of elements used in it is indicated   */
/* by the integer NUM_SHIFT_MAPS. NUM_STATES elements were allocated,  */
/* because if the user requests that certain single productions be     */
/* removed, a Shift map containing actions involving such productions  */
/* cannot be shared.                                                   */
extern struct shift_header_type *shift;

extern struct reduce_header_type *reduce;

extern short *gotodef,
    *shiftdf,
    *gd_index,
    *gd_range;

extern int *name;

/* STATSET is a mapping from state number to state information.        */
/* LASTATS is a similar mapping for look-ahead states.                 */
/* IN_STAT is a mapping from each state to the set of states that have */
/* a transition into the state in question.                            */
extern struct statset_type *statset;

extern struct lastats_type *lastats;
extern struct node **in_stat;

extern int num_scopes,
    scope_rhs_size,
    scope_state_size,
    num_error_rules;

extern struct scope_type {
  short prefix,
      suffix,
      lhs_symbol,
      look_ahead,
      state_set;
} *scope;

extern int *scope_right_side;
extern short *scope_state;

/**                                                               **/
/**                        OUTPUT DECLARATIONS                    **/
/**                                                               **/
/* The following external variables are used only in processing    */
/* output.                                                         */
extern char *output_ptr;
extern char *output_buffer;

extern int *symbol_map;
extern int *ordered_state;
extern int *state_list;

extern int *next,
    *previous,
    *state_index;

extern long table_size,
    action_size,
    increment_size;

extern int last_non_terminal,
    last_terminal;

extern int accept_act,
    error_act,
    first_index,
    last_index,
    last_symbol,
    max_name_length;

extern SET_PTR naction_symbols,
    action_symbols;

extern bool byte_terminal_range;

/**   The following declarations are specifications for all global    **/
/**   procedures and functions used in the program.                   **/
long temporary_space_allocated(void);

long temporary_space_used(void);

long global_space_allocated(void);

long global_space_used(void);

void reset_temporary_space(void);

void free_temporary_space(void);

void *talloc(long size);

struct node *allocate_node(char *file, long line);

bool *allocate_boolean_array(long size, char *file, long line);

int *allocate_int_array(long size, char *file, long line);

short *allocate_short_array(long size, char *file, long line);

struct goto_header_type allocate_goto_map(int size, char *file, long line);

struct shift_header_type allocate_shift_map(int size, char *file, long line);

struct reduce_header_type allocate_reduce_map(int size, char *file, long line);

void cmprspa(void);

void cmprtim(void);

void compute_la(int state_no, int item_no, SET_PTR look_ahead);

void create_lastats(void);

void dump_tables(void);

void exit_lalrk_process(void);

void init_lalrk_process(void);

void init_rmpself(SET_PTR produces);

void itoc(int num);

void field(int num, int len);

void fill_in(char string[], int amount, char character);

void free_conflict_space(void);

void free_nodes(struct node *head, struct node *tail);

struct node *lpgaccess(int state_no, int item_no);

void mkfirst(void);

void mkrdcts(void);

void la_traverse(int state_no, int goto_indx, int *stack_top);

void remove_single_productions(void);

void mkstats(void);

void mystrcpy(const char *str);

void padline(void);

void nospace(char *, long);

int number_len(int state_no);

void partset(SET_PTR collection, const int *element_size, const int *list,
             int *start, int *stack, int set_size, int from_process_scopes);

void print_item(int item_no);

void print_large_token(char *line, char *token, const char *indent, int len);

void print_state(int state_no);

void compute_action_symbols_range(const int *state_start,
                                  const int *state_stack,
                                  const int *state_list,
                                  int *action_symbols_range);

void compute_naction_symbols_range(const int *state_start,
                                   const int *state_stack,
                                   const int *state_list,
                                   int *naction_symbols_range);

void produce(void);

void process_error_maps(void);

void prnt_shorts(const char *title, int init, int bound, int perline, const int *array);

void prnt_ints(const char *title, int init, int bound, int perline, const int *array);

void print_space_parser(void);

void print_time_parser(void);

void process_tables(void);

void ptstats(void);

void remvsp(void);

void sortdes(int array[], int count[], long low, long high, long max);

void reallocate(void);

void resolve_conflicts(int state_no, struct node **action, const short *reduce_list, int reduce_root);

void restore_symbol(char *out, const char *in);

char *strlwr(char *string);

char *strupr(char *string);

/**                       ALLOCATE/FREE MACROS                    **/
/* The following macro definitions are used to preprocess calls to */
/* allocate routines that require locations. The FFREE macro is    */
/* normally an invocation to the FREE routine. It is encoded as    */
/* a macro here in case we need to do some debugging on dynamic    */
/* storage.                                                        */
static struct node *Allocate_node() {
  return allocate_node(hostfile, __LINE__);
}

static int *Allocate_int_array(const long n) {
  return allocate_int_array(n, hostfile, __LINE__);
}

static short *Allocate_short_array(const long n) {
  return allocate_short_array(n, hostfile, __LINE__);
}

static bool *Allocate_boolean_array(const long n) {
  return allocate_boolean_array(n, hostfile, __LINE__);
}

static struct goto_header_type Allocate_goto_map(const long n) {
  return allocate_goto_map(n, hostfile, __LINE__);
}

static struct shift_header_type Allocate_shift_map(const long n) {
  return allocate_shift_map(n, hostfile, __LINE__);
}

static struct reduce_header_type Allocate_reduce_map(const long n) {
  return allocate_reduce_map(n, hostfile, __LINE__);
}

static void PR_HEADING() {
  fprintf(syslis, "\f\n\n %-39s%s %-30.24s Page %d\n\n", HEADER_INFO, VERSION, timeptr, ++page_no);
  output_line_no = 4;
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

static int ABS(const int x) {
  return x < 0 ? -x : x;
}

static void ENDPAGE_CHECK() {
  if (++output_line_no >= PAGE_SIZE) PR_HEADING();
}

static void PRNT(char *msg) {
  printf("%s\n", msg);
  fprintf(syslis, "%s\n", msg);
  ENDPAGE_CHECK();
}

static void PRNTWNG(char *msg) {
  printf("***WARNING: %s\n", msg);
  fprintf(syslis, "***WARNING: %s\n", msg);
  ENDPAGE_CHECK();
}

static void PRNTERR(char *msg) {
  printf("***ERROR: %s\n", msg);
  fprintf(syslis, "***ERROR: %s\n", msg);
  ENDPAGE_CHECK();
}

/* The following two macros check whether the value of an             */
/* integer variable exceeds the maximum limit for a short or a long   */
/* integer, respectively. Note that the user should declare the       */
/* variable in question as a long integer. In the case of INT_CHECK,  */
/* this check is meaningful only if INT and SHORT are the same size.  */
/* Otherwise, if INT and LONG are of the same size, as is usually the */
/* case on large systems, this check is meaningless - too late !!!    */
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

/* The following macro definitions are used only in processing the output. */
static void BUFFER_CHECK(FILE *file) {
  if (IOBUFFER_SIZE - (output_ptr - &output_buffer[0]) < 73) {
    fwrite(output_buffer, sizeof(char), output_ptr - &output_buffer[0], file);
    output_ptr = &output_buffer[0];
  }
}
#endif /* COMMON_INCLUDED */
