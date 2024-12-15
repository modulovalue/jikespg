#include "bitset.h"
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

struct itemtab *item_table = NULL;

// TODO wrap
bool *null_nt = NULL;

// TODO wrap
short *gd_index = NULL;

// TODO wrap
short *gd_range = NULL;

struct statset_type *statset = NULL;

// TODO wrap
struct lastats_type *lastats = NULL;

struct node **in_stat = NULL;

short *scope_state = NULL;

char *output_buffer = NULL;
