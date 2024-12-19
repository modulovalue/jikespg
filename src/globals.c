#include "common.h"

// TODO • move these into some struct?
struct CounterState {
  long num_symbols;
  long num_names;
  long num_terminals;
  long num_non_terminals;
  long num_rules;
  long num_single_productions;
  long gotodom_size;
};

long num_symbols = 0;
long num_names = 0;
long num_terminals = 0;
long num_non_terminals = 0;
long num_rules = 0;
long num_single_productions = 0;
long gotodom_size = 0;




// TODO • move these into the PredefTerms struct or into the parser struct?
struct PredefTerms {
  int accept_image;
  int eoft_image;
  int eolt_image;
  int empty;
  int error_image;
};

int accept_image;
int eoft_image;
int eolt_image;
int empty;
int error_image;




// TODO • move these into the counters struct.
struct Counters {
  long num_shift_maps;
  long num_shifts;
  long num_shift_reduces;
  long num_gotos;
  long num_goto_reduces;
  long num_reductions;
  long num_entries;
  long num_error_rules;
};

long num_shift_maps = 0;
long num_shifts = 0;
long num_shift_reduces = 0;
long num_gotos = 0;
long num_goto_reduces = 0;
long num_reductions = 0;
long num_entries = 0;
long num_error_rules = 0;
