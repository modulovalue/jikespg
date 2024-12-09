static char hostfile[] = __FILE__;

#include "common.h"

long num_items = 0;
long num_states = 0;
long max_la_state;

long num_symbols = 0;
long num_names = 0;
long num_terminals;
long num_non_terminals;
long num_rules = 0;
long num_single_productions = 0;
long gotodom_size = 0;

int accept_image;
int eoft_image;
int eolt_image;
int empty;
int error_image;

int num_first_sets;
int num_shift_maps = 0;

long num_shifts = 0;
long num_shift_reduces = 0;
long num_gotos = 0;
long num_goto_reduces = 0;
long num_reductions = 0;
long num_sr_conflicts = 0;
long num_rr_conflicts = 0;
long num_entries;

long num_scopes = 0;
long scope_rhs_size = 0;
long scope_state_size = 0;
long num_error_rules = 0;

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

short *rhs_sym = NULL;

struct ruletab_type *rules = NULL;

struct node **closure = NULL;
struct node **clitems = NULL;
struct node **adequate_item = NULL;

struct itemtab *item_table = NULL;

bool *null_nt = NULL;

int term_set_size;
int non_term_set_size;
int state_set_size;

struct shift_header_type *shift = NULL;

struct reduce_header_type *reduce = NULL;

short *shiftdf = NULL;
short *gotodef = NULL;
short *gd_index = NULL;
short *gd_range = NULL;

struct statset_type *statset = NULL;

struct lastats_type *lastats = NULL;

struct node **in_stat = NULL;

long *scope_right_side = NULL;
short *scope_state = NULL;

char *output_ptr = NULL;
char *output_buffer = NULL;

struct node **conflict_symbols = NULL;
SET_PTR la_set = NULL;
SET_PTR read_set = NULL;
int highest_level = 0;
long la_top = 0;
short *la_index = NULL;
bool not_lrk;
