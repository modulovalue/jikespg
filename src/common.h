#pragma once
#include "bitset.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>

static char hostfile[];

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

static const char HEADER_INFO[] = "IBM Research Jikes Parser Generator";
static const char VERSION[] = "1.2";
static const char BLANK[] = " ";
static const long MAX_TABLE_SIZE = (USHRT_MAX < INT_MAX ? USHRT_MAX : INT_MAX) - 1;

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

static char *timeptr;

static long output_line_no = 0;

static char grm_file[80];
static char lis_file[80];
static char act_file[80];
static char hact_file[80];
static char tab_file[80];
static char prs_file[80] = "";
static char sym_file[80] = "";
static char def_file[80] = "";
static char dcl_file[80] = "";
static char file_prefix[80] = "";
static char prefix[MAX_PARM_SIZE] = "";
static char suffix[MAX_PARM_SIZE] = "";
static char parm[256] = "";
static char msg_line[MAX_MSG_SIZE];

static FILE *syslis;
static FILE *sysgrm;
static FILE *sysact;
static FILE *syshact;
static FILE *systab;
static FILE *syssym;
static FILE *sysprs;
static FILE *sysdcl;
static FILE *sysprs;
static FILE *sysdef;

static long num_items = 0;
static int num_states = 0;
static int max_la_state;

static int num_symbols = 0;
static int symno_size;
static int num_names = 0;
static int num_terminals;
static int num_non_terminals;
static int num_rules = 0;
static int num_conflict_elements = 0;
static int num_single_productions = 0;
static int gotodom_size = 0;

static bool IS_A_TERMINAL(const int i) {
  return i <= num_terminals;
}

static bool IS_A_NON_TERMINAL(const int i) {
  return i > num_terminals;
}

/*  The variables below are used for options setting. */
static bool list_bit = false;
static bool slr_bit = false;
static bool verbose_bit = false;
static bool first_bit = false;
static bool follow_bit = false;
static bool action_bit = false;
static bool edit_bit = false;
static bool states_bit = false;
static bool xref_bit = false;
static bool nt_check_bit = false;
static bool conflicts_bit = true;
static bool read_reduce_bit = true;
static bool goto_default_bit = true;
static bool shift_default_bit = false;
static bool byte_bit = true;
static bool warnings_bit = true;
static bool single_productions_bit = false;
static bool error_maps_bit = false;
static bool debug_bit = false;
static bool deferred_bit = true;
static bool c_bit = false;
static bool cpp_bit = false;
static bool java_bit = false;
static bool scopes_bit = false;

static int lalr_level = 1;
static int default_opt = 5;
static int trace_opt = TRACE_CONFLICTS;
static int names_opt = OPTIMIZE_PHRASES;
static int table_opt = 0;
static int increment = 30;
static int maximum_distance = 30;
static int minimum_distance = 3;
static int stack_size = 128;

static char escape = '%';
static char ormark = '|';
static char record_format = 'V';

static char blockb[MAX_PARM_SIZE] = {'/', '.'};
static char blocke[MAX_PARM_SIZE] = {'.', '/'};
static char hblockb[MAX_PARM_SIZE] = {'/', ':'};
static char hblocke[MAX_PARM_SIZE] = {':', '/'};
static char errmsg[MAX_PARM_SIZE] = "errmsg";
static char gettok[MAX_PARM_SIZE] = "gettok";
static char smactn[MAX_PARM_SIZE] = "smactn";
static char tkactn[MAX_PARM_SIZE] = "tkactn";

/*   The variables below are used to hold information about special  */
/* grammar symbols.                                                  */
static int accept_image;
static int eoft_image;
static int eolt_image;
static int empty;
static int error_image;

/* Miscellaneous counters. */
static int num_first_sets;
static int num_shift_maps = 0;
static int page_no = 0;

static long string_offset = 0;
static long string_size = 0;
static long num_shifts = 0;
static long num_shift_reduces = 0;
static long num_gotos = 0;
static long num_goto_reduces = 0;
static long num_reductions = 0;
static long num_sr_conflicts = 0;
static long num_rr_conflicts = 0;
static long num_entries;

static short *rhs_sym = NULL;

static struct ruletab_type *rules = NULL;

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
static struct node **closure = NULL;
static struct node **clitems = NULL;
static struct node **adequate_item = NULL;

static  struct itemtab {
  short symbol,
      rule_number,
      suffix_index,
      dot;
} *item_table = NULL;

/* SYMNO is an array that maps symbol numbers to actual symbols.       */
static struct symno_type {
  int ptr;
  int name_index;
} *symno = NULL;

/* NULL_NT is a boolean vector that indicates whether or not a given   */
/* non-terminal is nullable.                                           */
static bool *null_nt = NULL;

/* FOLLOW is a mapping from non-terminals to a set of terminals that   */
/* may appear immediately after the non-terminal.                      */
static SET_PTR nt_first = NULL;
static SET_PTR first = NULL;
static SET_PTR follow = NULL;

/* NAME is an array containing names to be associated with symbols.    */
/* REDUCE is a mapping from each state to reduce actions in that state.*/
/* SHIFT is an array used to hold the complete set of all shift maps   */
/* needed to construct the state automaton. Though its size is         */
/* NUM_STATES, the actual number of elements used in it is indicated   */
/* by the integer NUM_SHIFT_MAPS. NUM_STATES elements were allocated,  */
/* because if the user requests that certain single productions be     */
/* removed, a Shift map containing actions involving such productions  */
/* cannot be shared.                                                   */
static struct shift_header_type *shift = NULL;

static struct reduce_header_type *reduce = NULL;

static short *shiftdf = NULL;
static short *gotodef = NULL;
static short *gd_index = NULL;
static short *gd_range = NULL;

static int *name;

/* STATSET is a mapping from state number to state information.        */
/* LASTATS is a similar mapping for look-ahead states.                 */
/* IN_STAT is a mapping from each state to the set of states that have */
/* a transition into the state in question.                            */
static struct statset_type *statset = NULL;

static struct lastats_type *lastats = NULL;

static struct node **in_stat = NULL;

static int num_scopes = 0;
static int scope_rhs_size = 0;
static int scope_state_size = 0;
static int num_error_rules = 0;

static struct scope_type {
  short prefix,
      suffix,
      lhs_symbol,
      look_ahead,
      state_set;
} *scope = NULL;

static int *scope_right_side = NULL;
static short *scope_state = NULL;

/**                                                               **/
/**                        OUTPUT DECLARATIONS                    **/
/**                                                               **/
/* The following external variables are used only in processing    */
/* output.                                                         */
static char *output_ptr = NULL;
static char *output_buffer = NULL;

static int *symbol_map = NULL;
static int *ordered_state = NULL;
static long *state_list = NULL;

static long *next = NULL;
static long *previous = NULL;
static long *state_index = NULL;

static long table_size;
static long action_size;
static long increment_size;


static int last_non_terminal = 0;
static int last_terminal = 0;

static long accept_act;
static long error_act;
static int first_index;
static long last_index;
static int last_symbol;
static int max_name_length = 0;

static SET_PTR naction_symbols = NULL;
static SET_PTR action_symbols = NULL;

static bool byte_terminal_range = true;

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

long *allocate_long_array(long size, char *file, long line);

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

void field(long num, int len);

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

void partset(SET_PTR collection, long *element_size, long *list,
             long *start, long *stack, long set_size, long from_process_scopes);

void print_item(int item_no);

void print_large_token(char *line, char *token, const char *indent, int len);

void print_state(int state_no);

void compute_action_symbols_range(const long *state_start,
                                  const long *state_stack,
                                  const long *state_list,
                                  int *action_symbols_range);

void compute_naction_symbols_range(const long *state_start,
                                   const long *state_stack,
                                   const long *state_list,
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

void resolve_conflicts(int state_no, struct node **action, const short *symbol_list, int reduce_root);

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

static long *Allocate_long_array(const long n) {
  return allocate_long_array(n, hostfile, __LINE__);
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

static int ABS(const long x) {
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
