#include "bitset.h"
#include "common.h"

// TODO • propagate some global state struct through all the routines of the parser?
long num_items = 0;
long num_states = 0;
long max_la_state;

// TODO • move these into some struct?
long num_symbols = 0;
long num_names = 0;
long num_terminals;
long num_non_terminals;
long num_rules = 0;
long num_single_productions = 0;
long gotodom_size = 0;

struct PredefTerms {};

// TODO • move these into the PredefTerms struct or into the parser struct?
int accept_image;
int eoft_image;
int eolt_image;
int empty;
int error_image;

struct Counters {};

// TODO • move these into the counters struct.
long num_shift_maps = 0;
long num_shifts = 0;
long num_shift_reduces = 0;
long num_gotos = 0;
long num_goto_reduces = 0;
long num_reductions = 0;
long num_entries = 0;
long num_error_rules = 0;

// TODO • what to do with this?
int increment = 30;

// TODO • what to do with this?
char *output_buffer = NULL;
