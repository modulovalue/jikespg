#include "bitset.h"
#include <stdbool.h>

// TODO propagate some global state struct through all the routines of the parser?
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

char *output_buffer = NULL;
