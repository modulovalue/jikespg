#pragma once
#include <stdbool.h>
#include "common.h"

/* macro definition & action variables */
static int num_acts = 0;
static int num_defs = 0;

/* macro definition & action vars */
static long defelmt_size = 0;
static long actelmt_size = 0;
static long rulehdr_size = 0;

/* structure to store rule in first pass */
struct rulehdr_type {
  struct node *rhs_root;
  short lhs;
  bool sp;
};

/* structure to store location of macro def. */
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

/* structure to store location of action */
struct actelmt_type {
  long start_line;
  long end_line;
  short rule_number;
  short start_column;
  short end_column;
  bool header_block;
};

/* structure used to hash grammar symbols */
struct hash_type {
  struct hash_type *link;
  short number;
  short name_index;
  int st_ptr;
};

/* SYMNO is an array that maps symbol numbers to actual symbols.       */
extern struct symno_type {
  int ptr;
  int name_index;
} *symno;

/* NAME is an array containing names to be associated with symbols.    */
extern int *name;

char *RETRIEVE_STRING(int indx);

const char *RETRIEVE_NAME(int indx);

/* structure used to hold token information */
struct terminal_type {
  long start_line;
  long end_line;
  short start_column;
  short end_column;
  short length;
  short kind;
  char name[SYMBOL_SIZE + 1];
};

static struct rulehdr_type *rulehdr = NULL;
static struct defelmt_type *defelmt = NULL;
static struct actelmt_type *actelmt = NULL;

static struct node *start_symbol_root;
static struct hash_type **hash_table;
static struct terminal_type *terminal;

/* The following variables hold the names */
/* of keywords and predefined macros.     */
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
/* Note that the next four keywords start with \n instead of     */
/* the escape character.  This is to prevent the user from       */
/* declaring a grammar symbol with the same name.  The           */
/* end-of-line character was chosen since that character can     */
/* never appear in the input without being interpreted as        */
/* marking the end of an input line.  When printing such a       */
/* keyword, the \n is properly replaced by the escape character. */
/* See RESTORE_SYMBOL in the file LPGUTIL.C.                     */
static char kempty[7] = "\nempty";
static char kerror[7] = "\nerror";
static char keoft[5] = "\neof";
static char kaccept[5] = "\nacc";
static char kstart_nt[7] = " start";
static char keolt[5] = " eol";

static struct line_elemt {
  struct line_elemt *link;
  char line[MAX_LINE_SIZE + 1];
} *line_pool_root = NULL;

static int stack_top = -1;

void assign_symbol_no(const char *string_ptr, int image);

static void alias_map(const char *stringptr, int image);

static int symbol_image(const char *item);

static int name_map(const char *symb);

static void build_symno(void);

