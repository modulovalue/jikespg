static char hostfile[] = __FILE__;

#include "common.h"
#include "space.h"

/**    The following are global variables declared in COMMON.H    **/
const char HEADER_INFO[] = "IBM Research Jikes Parser Generator";
const char VERSION[] = "1.2";
const char BLANK[] = " ";
const long MAX_TABLE_SIZE = MIN((long) USHRT_MAX, INT_MAX) - 1;

char *timeptr;

long output_line_no = 0;

char grm_file[80],
    lis_file[80],
    act_file[80],
    hact_file[80],
    tab_file[80],
    prs_file[80] = "",
    sym_file[80] = "",
    def_file[80] = "",
    dcl_file[80] = "",
    file_prefix[80] = "",
    prefix[MAX_PARM_SIZE] = "",
    suffix[MAX_PARM_SIZE] = "",
    parm[256] = "",
    msg_line[MAX_MSG_SIZE];

FILE *sysgrm,
    *syslis,
    *sysact,
    *syshact,
    *systab,
    *syssym,
    *sysprs,
    *sysdcl,
    *sysdef;

/*  The variables below are global counters.          */
long num_items = 0,
    num_states = 0,
    max_la_state;

int num_symbols = 0,
    symno_size,
    num_names = 0,
    num_terminals,
    num_non_terminals,
    num_rules = 0,
    num_conflict_elements = 0,
    num_single_productions = 0,
    gotodom_size = 0;

/*   The variables below are used to hold information about special  */
/* grammar symbols.                                                  */
short accept_image,
    eoft_image,
    eolt_image,
    empty,
    error_image;

/* Miscellaneous counters. */

int num_first_sets,
    num_shift_maps = 0,
    page_no = 0;

long string_offset = 0,
    string_size = 0,
    num_shifts = 0,
    num_shift_reduces = 0,
    num_gotos = 0,
    num_goto_reduces = 0,
    num_reductions = 0,
    num_sr_conflicts = 0,
    num_rr_conflicts = 0,
    num_entries;

int num_scopes = 0,
    scope_rhs_size = 0,
    scope_state_size = 0,
    num_error_rules = 0;

bool list_bit = false,
    slr_bit = false,
    verbose_bit = false,
    first_bit = false,
    follow_bit = false,
    action_bit = false,
    edit_bit = false,
    states_bit = false,
    xref_bit = false,
    nt_check_bit = false,
    conflicts_bit = true,
    read_reduce_bit = true,
    goto_default_bit = true,
    shift_default_bit = false,
    byte_bit = true,
    warnings_bit = true,
    single_productions_bit = false,
    error_maps_bit = false,
    debug_bit = false,
    deferred_bit = true,
    c_bit = false,
    cpp_bit = false,
    java_bit = false,
    jikes_bit = false,
    scopes_bit = false;

int lalr_level = 1,
    default_opt = 5,
    trace_opt = TRACE_CONFLICTS,
    names_opt = OPTIMIZE_PHRASES,
    table_opt = 0,
    increment = 30,
    maximum_distance = 30,
    minimum_distance = 3,
    stack_size = 128;

char escape = '%',
    ormark = '|',
    record_format = 'V';

char blockb[MAX_PARM_SIZE] = {'/', '.'},
    blocke[MAX_PARM_SIZE] = {'.', '/'},
    hblockb[MAX_PARM_SIZE] = {'/', ':'},
    hblocke[MAX_PARM_SIZE] = {':', '/'},
    errmsg[MAX_PARM_SIZE] = "errmsg",
    gettok[MAX_PARM_SIZE] = "gettok",
    smactn[MAX_PARM_SIZE] = "smactn",
    tkactn[MAX_PARM_SIZE] = "tkactn";

char *string_table = (char *) NULL;

short *rhs_sym = NULL;

struct ruletab_type *rules = NULL;

struct node **closure = NULL,
    **clitems = NULL,
    **adequate_item = NULL;

struct itemtab *item_table = NULL;

struct symno_type *symno = NULL;

bool *null_nt = NULL;

int term_set_size,
    non_term_set_size,
    state_set_size;

SET_PTR nt_first = NULL,
    first = NULL,
    follow = NULL;

struct shift_header_type *shift = NULL;

struct reduce_header_type *reduce = NULL;

short *shiftdf = NULL,
    *gotodef = NULL,
    *gd_index = NULL,
    *gd_range = NULL;

int *name;

struct statset_type *statset = NULL;

struct lastats_type *lastats = NULL;

struct node **in_stat = NULL;

struct scope_type *scope = NULL;

short *scope_right_side = NULL,
    *scope_state = NULL;

char *output_ptr = NULL,
    *output_buffer = NULL;

short *symbol_map = NULL,
    *ordered_state = NULL,
    *state_list = NULL;

int *next = NULL,
    *previous = NULL,
    *state_index = NULL;

long table_size,
    action_size,
    increment_size;

short last_non_terminal = 0,
    last_terminal = 0;

int accept_act,
    error_act,
    first_index,
    last_index,
    last_symbol,
    max_name_length = 0;

SET_PTR naction_symbols = NULL,
    action_symbols = NULL;

bool byte_terminal_range = true;

struct node **conflict_symbols = NULL;
SET_PTR la_set = NULL,
    read_set = NULL;
int highest_level = 0;
long la_top = 0;
short *la_index = NULL;
bool not_lrk;

struct new_state_type *new_state_element;

short *shift_image = NULL,
    *real_shift_number = NULL;

int *term_state_index = NULL,
    *shift_check_index = NULL;

int shift_domain_count,
    num_terminal_states,
    check_size,
    term_check_size,
    term_action_size,
    shift_check_size;
