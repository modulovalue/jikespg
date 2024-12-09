#pragma once
#include <stdbool.h>
#include "common.h"
#include <string.h>

/**                        OPTIONS DECLARATIONS                   **/
/* The following static variables are used only in processing the  */
/* options.                                                        */
static const int OUTPUT_PARM_SIZE = MAX_PARM_SIZE + 7;
static const int MAXIMUM_LA_LEVEL = 15;
static const int STRING_BUFFER_SIZE = 8192;

static const char *oaction = "ACTION";
static const char *oactfile_name = "ACTFILENAME";
static const char *oactfile_name2 = "ACTFILE-NAME";
static const char *oactfile_name3 = "ACTFILE_NAME";
static const char *oblockb = "BLOCKB";
static const char *oblocke = "BLOCKE";
static const char *obyte = "BYTE";
static const char *oconflicts = "CONFLICTS";
static const char *odebug = "DEBUG";
static const char *odefault = "DEFAULT";
static const char *odeferred = "DEFERRED";
static const char *oedit = "EDIT";
static const char *oerrormaps2 = "ERROR_MAPS";
static const char *oerrormaps3 = "ERROR-MAPS";
static const char *oerrormaps = "ERRORMAPS";
static const char *oescape = "ESCAPE";
static const char *ofile_prefix2 = "FILE_PREFIX";
static const char *ofile_prefix3 = "FILE-PREFIX";
static const char *ofile_prefix = "FILEPREFIX";
static const char *ofirst = "FIRST";
static const char *ofixed = "FIXED";
static const char *ofollow = "FOLLOW";
static const char *ofull = "FULL";
static const char *ogenprsr3 = "GENERATE-PARSER";
static const char *ogenprsr2 = "GENERATE_PARSER";
static const char *ogenprsr = "GENERATEPARSER";
static const char *ogotodefault2 = "GOTO_DEFAULT";
static const char *ogotodefault3 = "GOTO-DEFAULT";
static const char *ogotodefault = "GOTODEFAULT";
static const char *ohactfile_name = "HACTFILENAME";
static const char *ohactfile_name2 = "HACTFILE-NAME";
static const char *ohactfile_name3 = "HACTFILE_NAME";
static const char *ohalfword2 = "HALF_WORD";
static const char *ohalfword3 = "HALF-WORD";
static const char *ohalfword = "HALFWORD";
static const char *ohblockb = "HBLOCKB";
static const char *ohblocke = "HBLOCKE";
static const char *olalr = "LALR";
static const char *olist = "LIST";
static const char *omax = "MAXIMUM";
static const char *omaximum_distance2 = "MAX_DISTANCE";
static const char *omaximum_distance3 = "MAX-DISTANCE";
static const char *omaximum_distance = "MAXDISTANCE";
static const char *omin = "MINIMUM";
static const char *ominimum_distance2 = "MIN_DISTANCE";
static const char *ominimum_distance3 = "MIN-DISTANCE";
static const char *ominimum_distance = "MINDISTANCE";
static const char *onames = "NAMES";
static const char *ontcheck2 = "NT_CHECK";
static const char *ontcheck3 = "NT-CHECK";
static const char *ontcheck = "NTCHECK";
static const char *ooptimized = "OPTIMIZED";
static const char *oormark = "ORMARK";
static const char *oprefix = "PREFIX";
static const char *oreadreduce2 = "READ_REDUCE";
static const char *oreadreduce3 = "READ-REDUCE";
static const char *oreadreduce = "READREDUCE";
static const char *oscopes = "SCOPES";
static const char *oshiftdefault2 = "SHIFT-DEFAULT";
static const char *oshiftdefault3 = "SHIFT_DEFAULT";
static const char *oshiftdefault = "SHIFTDEFAULT";
static const char *osingleproductions2 = "SINGLE-PRODUCTIONS";
static const char *osingleproductions3 = "SINGLE_PRODUCTIONS";
static const char *osingleproductions = "SINGLEPRODUCTIONS";
static const char *oslr = "SLR";
static const char *ospace = "SPACE";
static const char *ostack_size2 = "STACK_SIZE";
static const char *ostack_size3 = "STACK-SIZE";
static const char *ostack_size = "STACKSIZE";
static const char *ostates = "STATES";
static const char *osuffix = "SUFFIX";
static const char *otable = "TABLE";
static const char *otime = "TIME";
static const char *otrace = "TRACE";
static const char *ovariable = "VARIABLE";
static const char *overbose = "VERBOSE";
static const char *owarnings = "WARNINGS";
static const char *oxref = "XREF";

/**                        PARSING DECLARATIONS                   **/
/* The following static variables are used only in processing the  */
/* the input source.                                               */
static const char CTL_Z = '\x1a';
#undef  min
#define min(x, y) ((x) < (y) ? (x) : (y))

/*                             IO variables                        */
/* The two character pointer variables, P1 and P2, are used in     */
/* processing the io buffer, INPUT_BUFFER.                         */
static char *p1;
static char *p2;
static char *input_buffer;

static char *linestart;
static char *bufend;
static char *ct_ptr;

/* current token & related variables */
static short ct = 0;
static short ct_start_col = 0;
static short ct_end_col = 0;
static short ct_length = 0;

static long ct_start_line = 0;
static long ct_end_line = 0;

/* macro definition & action variables */
static int num_acts = 0;
static int num_defs = 0;

/* macro definition & action vars */
static long defelmt_size = 0;
static long actelmt_size = 0;
static long rulehdr_size = 0;

struct rulehdr_type /* structure to store rule in first pass */
{
  struct node *rhs_root;
  short lhs;
  bool sp;
};

struct defelmt_type /* structure to store location of macro def. */
{
  short next;
  short length;
  short start_column;
  short end_column;
  char *macro;
  long start_line;
  long end_line;
  char name[SYMBOL_SIZE + 1];
};

struct actelmt_type /* structure to store location of action */
{
  long start_line;
  long end_line;
  short rule_number;
  short start_column;
  short end_column;
  bool header_block;
};

struct hash_type /* structure used to hash grammar symbols */
{
  struct hash_type *link;
  short number;
  short name_index;
  int st_ptr;
};

extern char *string_table;

static const char *EXTRACT_STRING(const int indx) {
  return &string_table[indx];
}

static char *RETRIEVE_STRING(const int indx) {
  return &string_table[symno[indx].ptr];
}

static const char *RETRIEVE_NAME(const int indx) {
  return &string_table[name[indx]];
}

static bool EQUAL_STRING(const char *symb, const struct hash_type *p) {
  return strcmp(symb, string_table + p->st_ptr) == 0;
}

struct terminal_type /* structure used to hold token information */
{
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

static int blockb_len;
static int blocke_len;
static int hblockb_len;
static int hblocke_len;

static int stack_top = -1;

static void exit_process(void);

static bool verify(const char *item);

static char *translate(char *str, int len);

static void options(void);

static void process_options_lines(char* grm_file, struct OutputFiles* output_files);

static int hash(const char *symbl);

static void insert_string(struct hash_type *q, const char *string);

static void assign_symbol_no(const char *string_ptr, int image);

static void alias_map(const char *stringptr, int image);

static int symbol_image(const char *item);

static int name_map(const char *symb);

static void scanner(char* grm_file);

static void accept_action(char* grm_file);

static void build_symno(void);

static struct hash_type *alias_root = NULL;

static short *macro_table;

static void process_actions(char* grm_file);

static void process_action_line(FILE *sysout, char *text, int line_no, int rule_no, char* grm_file);

static struct line_elemt *alloc_line(void);

static void free_line(struct line_elemt *p);

static void mapmacro(int def_index);

static struct line_elemt *find_macro(char *name);

static void display_input(void);
