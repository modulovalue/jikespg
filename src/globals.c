#include "bitset.h"

static char hostfile[] = __FILE__;

#include <stdbool.h>

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

long num_shift_maps = 0;

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

bool error_maps_bit = false;

int increment = 30;

// Used by the parser because C doesn't have classes.
char ormark;
// Used by the parser because C doesn't have classes.
char escape;

short *rhs_sym = NULL;

struct ruletab_type *rules = NULL;

struct node **closure = NULL;
struct node **clitems = NULL;
struct node **adequate_item = NULL;

struct itemtab *item_table = NULL;

bool *null_nt = NULL;

struct shift_header_type *shift = NULL;

struct reduce_header_type *reduce = NULL;

short *shiftdf = NULL;
long *gotodef = NULL;
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
JBitset la_set = {.raw = NULL};
JBitset read_set = {.raw = NULL};
long la_top = 0;
short *la_index = NULL;
