static char hostfile[] = __FILE__;

#include "common.h"
#include "space.h"

/**    The following are global variables declared in COMMON.H    **/
const char HEADER_INFO[] = "IBM Research Jikes Parser Generator";
const char VERSION[] = "1.2";
const char BLANK[] = " ";
const long MAX_TABLE_SIZE = (USHRT_MAX < INT_MAX ? USHRT_MAX : INT_MAX) - 1;

long output_line_no = 0;

char grm_file[80];
char lis_file[80];
char act_file[80];
char hact_file[80];
char tab_file[80];
char prs_file[80] = "";
char sym_file[80] = "";
char def_file[80] = "";
char dcl_file[80] = "";
char file_prefix[80] = "";
char prefix[MAX_PARM_SIZE] = "";
char suffix[MAX_PARM_SIZE] = "";
char parm[256] = "";
char msg_line[MAX_MSG_SIZE];

FILE *sysgrm;
FILE *syslis;
FILE *sysact;
FILE *syshact;
FILE *systab;
FILE *syssym;
FILE *sysprs;
FILE *sysdcl;
FILE *sysdef;

/*  The variables below are global counters.          */
long num_items = 0;
int num_states = 0;
int max_la_state;

int num_symbols = 0;
int symno_size;
int num_names = 0;
int num_terminals;
int num_non_terminals;
int num_rules = 0;
int num_conflict_elements = 0;
int num_single_productions = 0;
int gotodom_size = 0;

/*   The variables below are used to hold information about special  */
/* grammar symbols.                                                  */
int accept_image;
int eoft_image;
int eolt_image;
int empty;
int error_image;

/* Miscellaneous counters. */

int num_first_sets;
int num_shift_maps = 0;
int page_no = 0;

long string_offset = 0;
long string_size = 0;
long num_shifts = 0;
long num_shift_reduces = 0;
long num_gotos = 0;
long num_goto_reduces = 0;
long num_reductions = 0;
long num_sr_conflicts = 0;
long num_rr_conflicts = 0;
long num_entries;

int num_scopes = 0;
int scope_rhs_size = 0;
int scope_state_size = 0;
int num_error_rules = 0;

bool list_bit = false;
bool slr_bit = false;
bool verbose_bit = false;
bool first_bit = false;
bool follow_bit = false;
bool action_bit = false;
bool edit_bit = false;
bool states_bit = false;
bool xref_bit = false;
bool nt_check_bit = false;
bool conflicts_bit = true;
bool read_reduce_bit = true;
bool goto_default_bit = true;
bool shift_default_bit = false;
bool byte_bit = true;
bool warnings_bit = true;
bool single_productions_bit = false;
bool error_maps_bit = false;
bool debug_bit = false;
bool deferred_bit = true;
bool c_bit = false;
bool cpp_bit = false;
bool java_bit = false;
bool scopes_bit = false;

int lalr_level = 1;
int default_opt = 5;
int trace_opt = TRACE_CONFLICTS;
int names_opt = OPTIMIZE_PHRASES;
int table_opt = 0;
int increment = 30;
int maximum_distance = 30;
int minimum_distance = 3;
int stack_size = 128;

char escape = '%';
char ormark = '|';
char record_format = 'V';

char blockb[MAX_PARM_SIZE] = {'/', '.'};
char blocke[MAX_PARM_SIZE] = {'.', '/'};
char hblockb[MAX_PARM_SIZE] = {'/', ':'};
char hblocke[MAX_PARM_SIZE] = {':', '/'};
char errmsg[MAX_PARM_SIZE] = "errmsg";
char gettok[MAX_PARM_SIZE] = "gettok";
char smactn[MAX_PARM_SIZE] = "smactn";
char tkactn[MAX_PARM_SIZE] = "tkactn";

char *string_table = NULL;

short *rhs_sym = NULL;

struct ruletab_type *rules = NULL;

struct node **closure = NULL;
struct node **clitems = NULL;
struct node **adequate_item = NULL;

struct itemtab *item_table = NULL;

struct symno_type *symno = NULL;

bool *null_nt = NULL;

int term_set_size;
int non_term_set_size;
int state_set_size;

SET_PTR nt_first = NULL;
SET_PTR first = NULL;
SET_PTR follow = NULL;

struct shift_header_type *shift = NULL;

struct reduce_header_type *reduce = NULL;

short *shiftdf = NULL;
short *gotodef = NULL;
short *gd_index = NULL;
short *gd_range = NULL;

int *name;

struct statset_type *statset = NULL;

struct lastats_type *lastats = NULL;

struct node **in_stat = NULL;

struct scope_type *scope = NULL;

int *scope_right_side = NULL;
short *scope_state = NULL;

char *output_ptr = NULL;
char *output_buffer = NULL;

int *symbol_map = NULL;
long *ordered_state = NULL;
long *state_list = NULL;

long *next = NULL;
long *previous = NULL;
long *state_index = NULL;

long table_size;
long action_size;
long increment_size;

int last_non_terminal = 0;
int last_terminal = 0;

long accept_act;
long error_act;
long first_index;
long last_index;
int last_symbol;
int max_name_length = 0;

SET_PTR naction_symbols = NULL;
SET_PTR action_symbols = NULL;

bool byte_terminal_range = true;

struct node **conflict_symbols = NULL;
SET_PTR la_set = NULL;
SET_PTR read_set = NULL;
int highest_level = 0;
long la_top = 0;
short *la_index = NULL;
bool not_lrk;

struct new_state_type *new_state_element;

short *shift_image = NULL;
short *real_shift_number = NULL;

int *term_state_index = NULL;
int *shift_check_index = NULL;

int shift_domain_count;
int num_terminal_states;
int check_size;
int term_check_size;
int term_action_size;
int shift_check_size;
