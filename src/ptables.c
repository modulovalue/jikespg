#include <stdlib.h>
static char hostfile[] = __FILE__;

#include "common.h"
#include "lpgparse.h"

struct action_element {
  struct action_element *next;
  short count;
  short action;
};

/* The array ACTION_COUNT is used to construct a map from each terminal     */
/* into the set (list) of actions defined on that terminal. A count of the  */
/* number of occurrences of each action in the automaton is kept.            */
/* This procedure is invoked with a specific shift map which it processes   */
/* and updates the ACTION_COUNT map accordingly.                            */
static void process_shift_actions(struct action_element **action_count, const int shift_no) {
  struct action_element *q;
  const struct shift_header_type sh = shift[shift_no];
  for (int i = 1; i <= sh.size; i++) {
    const int symbol = sh.map[i].symbol;
    const int act = sh.map[i].action;
    for (q = action_count[symbol]; q != NULL; q = q->next) {
      if (q->action == act)
        break;
    }
    if (q == NULL) /* new action not yet seen */
    {
      q = (struct action_element *) talloc(sizeof(struct action_element));
      if (q == NULL)
        nospace(__FILE__, __LINE__);
      q->action = act;
      q->count = 1;
      q->next = action_count[symbol];
      action_count[symbol] = q;
    } else q->count++;
  }
}

/* This procedure updates the vector SHIFTDF, indexable by the terminals in  */
/* the grammar. Its task is to assign to each element of SHIFTDF, the action */
/* most frequently defined on the symbol in question.                        */
static void compute_shift_default(void) {
  int shift_count = 0;
  int shift_reduce_count = 0;
  /* Set up a pool of temporary space.                            */
  reset_temporary_space();
  shiftdf = Allocate_short_array(num_terminals + 1);
  struct action_element **action_count = calloc(num_terminals + 1, sizeof(struct action_element *));
  if (action_count == NULL)
    nospace(__FILE__, __LINE__);
  /* For each state, invoke PROCESS_SHIFT_ACTIONS to process the     */
  /* shift map associated with that state.                           */
  for ALL_STATES3(state_no) {
    process_shift_actions(action_count, statset[state_no].shift_number);
  }
  for ALL_LA_STATES3(state_no) {
    process_shift_actions(action_count, lastats[state_no].shift_number);
  }
  /* We now iterate over the ACTION_COUNT mapping, and for each        */
  /* terminal t, initialize SHIFTDF[t] to the action that is most      */
  /* frequently defined on t.                                          */
  for ALL_TERMINALS3(symbol) {
    int max_count = 0;
    int default_action = 0;
    for (const struct action_element *q = action_count[symbol]; q != NULL; q = q->next) {
      if (q->count > max_count) {
        max_count = q->count;
        default_action = q->action;
      }
    }
    shiftdf[symbol] = default_action;
    /* A state number ? */
    if (default_action > 0) {
      shift_count += max_count;
    } else {
      shift_reduce_count += max_count;
    }
  }
  PRNT2(msg_line, "Number of Shift entries saved by default: %d", shift_count);
  PRNT2(msg_line, "Number of Shift/Reduce entries saved by default: %d", shift_reduce_count);
  num_shifts -= shift_count;
  num_shift_reduces -= shift_reduce_count;
  num_entries = num_entries - shift_count - shift_reduce_count;
  ffree(action_count);
}

/* COMPUTE_GOTO_DEFAULT constructs the vector GOTODEF, which is indexed by   */
/* the non-terminals in the grammar. Its task is to assign to each element   */
/* of the array the Action which is most frequently defined on the symbol in */
/* question, and remove all such actions from the state automaton.           */
static void compute_goto_default(void) {
  struct goto_header_type go_to;
  struct action_element *q;
  int goto_count = 0;
  int goto_reduce_count = 0;
  /* Set up a pool of temporary space.                            */
  reset_temporary_space();
  gotodef = Allocate_short_array(num_non_terminals);
  gotodef -= num_terminals + 1;
  struct action_element **action_count = calloc(num_non_terminals, sizeof(struct action_element *));
  action_count -= num_terminals + 1;
  if (action_count == NULL) {
    nospace(__FILE__, __LINE__);
  }
  /* The array ACTION_COUNT is used to construct a map from each     */
  /* non-terminal into the set (list) of actions defined on that     */
  /* non-terminal. A count of how many occurences of each action     */
  /* is also kept.                                                   */
  /* This loop is analoguous to the loop in PROCESS_SHIFT_ACTIONS.   */
  for ALL_STATES3(state_no) {
    go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      const int symbol = go_to.map[i].symbol;
      const int act = go_to.map[i].action;
      for (q = action_count[symbol]; q != NULL; q = q->next) {
        if (q->action == act)
          break;
      }
      if (q == NULL) /* new action not yet seen */
      {
        q = (struct action_element *)
            talloc(sizeof(struct action_element));
        if (q == NULL)
          nospace(__FILE__, __LINE__);
        q->action = act;
        q->count = 1;
        q->next = action_count[symbol];
        action_count[symbol] = q;
      } else q->count++;
    }
  }
  /* We now iterate over the mapping created above and for each      */
  /* non-terminal A, initialize GOTODEF(A) to the action that is     */
  /* most frequently defined on A.                                   */
  for ALL_NON_TERMINALS3(symbol) {
    int max_count = 0;
    int default_action = 0;
    for (q = action_count[symbol]; q != NULL; q = q->next) {
      if (q->count > max_count) {
        max_count = q->count;
        default_action = q->action;
      }
    }
    gotodef[symbol] = default_action;
    if (default_action > 0) /* A state number? */
    {
      goto_count += max_count;
    } else {
      goto_reduce_count += max_count;
    }
  }
  /*   We now iterate over the automaton and eliminate all GOTO actions  */
  /* for which there is a DEFAULT.                                       */
  for ALL_STATES3(state_no) {
    int k = 0;
    go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      if (gotodef[go_to.map[i].symbol] != go_to.map[i].action) {
        k++;
        go_to.map[k].symbol = go_to.map[i].symbol;
        go_to.map[k].action = go_to.map[i].action;
      }
    }
    statset[state_no].go_to.size = k; /* Readjust size */
  }
  PRNT2(msg_line, "Number of Goto entries saved by default: %d", goto_count);
  PRNT2(msg_line, "Number of Goto/Reduce entries saved by default: %d", goto_reduce_count);
  num_gotos -= goto_count;
  num_goto_reduces -= goto_reduce_count;
  num_entries = num_entries - goto_count - goto_reduce_count;
  action_count += num_terminals + 1;
  ffree(action_count);
}

/* Remap symbols, apply transition default actions  and call                 */
/* appropriate table compression routine.                                    */
void process_tables(char *tab_file, struct OutputFiles* output_files, struct CLIOptions* cli_options) {
  struct reduce_header_type red;
  /*        First, we decrease by 1 the constants NUM_SYMBOLS        */
  /* and NUM_TERMINALS, remove the EMPTY symbol(1) and remap the     */
  /* other symbols beginning at 1.  If default reduction is          */
  /* requested, we assume a special DEFAULT_SYMBOL with number zero. */
  eoft_image--;
  accept_image--;
  if (error_maps_bit) {
    error_image--;
    eolt_image--;
  }
  num_terminals--;
  num_symbols--;
  /* Remap all the symbols used in GOTO and REDUCE actions.      */
  /* Remap all the symbols used in GD_RANGE.                     */
  /* Remap all the symbols used in the range of SCOPE.           */
  /* Release space trapped by the maps IN_STAT and FIRST.        */
  for ALL_STATES3(state_no) {
    const struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      go_to.map[i].symbol--;
    }
    red = reduce[state_no];
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol--;
    }
  }
  for ALL_LA_STATES3(state_no) {
    red = lastats[state_no].reduce;
    for (int i = 1; i <= red.size; i++)
      red.map[i].symbol--;
  }
  for (int i = 1; i <= gotodom_size; i++) {
    gd_range[i]--;
  }
  for (int i = 1; i <= num_scopes; i++) {
    scope[i].lhs_symbol--;
    scope[i].look_ahead--;
  }
  for (int i = 1; i <= scope_rhs_size; i++) {
    if (scope_right_side[i] != 0) {
      scope_right_side[i]--;
    }
  }
  /* Remap all symbols in the domain of the Shift maps.              */
  for (int i = 1; i <= num_shift_maps; i++) {
    const struct shift_header_type sh = shift[i];
    for (int j = 1; j <= sh.size; j++) {
      sh.map[j].symbol--;
    }
  }
  /* Remap the left-hand side of all the rules.                      */
  for ALL_RULES3(rule_no) {
    rules[rule_no].lhs--;
  }
  /* Remap the dot symbols in ITEM_TABLE.                            */
  if (error_maps_bit) {
    for ALL_ITEMS3(item_no) {
      item_table[item_no].symbol--;
    }
  }
  /* We update the SYMNO map.                                        */
  for ALL_SYMBOLS3(symbol) {
    symno[symbol] = symno[symbol + 1];
  }
  /* If Goto Default and/or Shift Default were requested, process    */
  /* appropriately.                                                  */
  if (cli_options->shift_default_bit) {
    compute_shift_default();
  }
  if (cli_options->goto_default_bit) {
    compute_goto_default();
  }
  /* Release the pool of temporary space.                           */
  free_temporary_space();
  /* We allocate the necessary structures, open the appropriate    */
  /* output file and call the appropriate compression routine.     */
  if (error_maps_bit) {
    naction_symbols = (SET_PTR) calloc(num_states + 1, non_term_set_size * sizeof(BOOLEAN_CELL));
    if (naction_symbols == NULL) {
      nospace(__FILE__, __LINE__);
    }
    action_symbols = (SET_PTR) calloc(num_states + 1, term_set_size * sizeof(BOOLEAN_CELL));
    if (action_symbols == NULL) {
      nospace(__FILE__, __LINE__);
    }
  }
  output_buffer = (char *) calloc(IOBUFFER_SIZE, sizeof(char));
  if (output_buffer == NULL) {
    nospace(__FILE__, __LINE__);
  }
  FILE *systab;
  if (!cli_options->c_bit && !cli_options->cpp_bit && !cli_options->java_bit) {
    if ((systab = fopen(tab_file, "w")) == NULL) {
      fprintf(stderr, "***ERROR: Table file \"%s\" cannot be opened\n", tab_file);
      exit(12);
    }
  }
  if (cli_options->table_opt == OPTIMIZE_SPACE) {
    cmprspa(output_files, cli_options, systab);
  } else if (cli_options->table_opt == OPTIMIZE_TIME) {
    cmprtim(output_files, cli_options, systab);
  }
  if (!cli_options->c_bit && !cli_options->cpp_bit && !cli_options->java_bit) {
    fclose(systab);
  }
  /* If printing of the states was requested,  print the new mapping   */
  /* of the states.                                                    */
  if (cli_options->states_bit) {
    fprintf(syslis, "\nMapping of new state numbers into original numbers:\n");
    for ALL_STATES3(state_no) {
      fprintf(syslis, "\n%5ld  ==>>  %5ld", ordered_state[state_no], state_list[state_no]);
    }
    fprintf(syslis, "\n");
  }
}
