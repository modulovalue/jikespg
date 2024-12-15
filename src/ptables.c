#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "lpgparse.h"

struct ptables_action_element {
  struct ptables_action_element *next;
  short count;
  short action;
};

/// The array ACTION_COUNT is used to construct a map from each terminal
/// into the set (list) of actions defined on that terminal. A count of the
/// number of occurrences of each action in the automaton is kept.
/// This procedure is invoked with a specific shift map which it processes
/// and updates the ACTION_COUNT map accordingly.
static void process_shift_actions(struct ptables_action_element **action_count, const int shift_no, struct SRTable* srt) {
  const struct shift_header_type sh = srt->shift[shift_no];
  for (int i = 1; i <= sh.size; i++) {
    const int symbol = sh.map[i].symbol;
    const short act = sh.map[i].action;
    struct ptables_action_element *q;
    for (q = action_count[symbol]; q != NULL; q = q->next) {
      if (q->action == act)
        break;
    }
    if (q == NULL) /* new action not yet seen */
    {
      talloc0(q, struct ptables_action_element);
      q->action = act;
      q->count = 1;
      q->next = action_count[symbol];
      action_count[symbol] = q;
    } else q->count++;
  }
}

/// This procedure updates the vector SHIFTDF, indexable by the terminals in
/// the grammar. Its task is to assign to each element of SHIFTDF, the action
/// most frequently defined on the symbol in question.
static void compute_shift_default(struct SRTable* srt, struct lastats_type *lastats, short *shiftdf, struct statset_type *statset) {
  // Set up a pool of temporary space.
  reset_temporary_space();
  int shift_count = 0;
  int shift_reduce_count = 0;
  shiftdf = Allocate_short_array(num_terminals + 1);
  struct ptables_action_element **action_count;
  calloc0(action_count, num_terminals + 1, struct ptables_action_element *);
  // For each state, invoke PROCESS_SHIFT_ACTIONS to process the
  // shift map associated with that state.
  for ALL_STATES3(state_no) {
    process_shift_actions(action_count, statset[state_no].shift_number, srt);
  }
  for ALL_LA_STATES3(state_no) {
    process_shift_actions(action_count, lastats[state_no].shift_number, srt);
  }
  // We now iterate over the ACTION_COUNT mapping, and for each
  // terminal t, initialize SHIFTDF[t] to the action that is most
  // frequently defined on t.
  for ALL_TERMINALS3(symbol) {
    int max_count = 0;
    short default_action = 0;
    for (const struct ptables_action_element *q = action_count[symbol]; q != NULL; q = q->next) {
      if (q->count > max_count) {
        max_count = q->count;
        default_action = q->action;
      }
    }
    shiftdf[symbol] = default_action;
    // A state number ?
    if (default_action > 0) {
      shift_count += max_count;
    } else {
      shift_reduce_count += max_count;
    }
  }
  PRNT3("Number of Shift entries saved by default: %d", shift_count);
  PRNT3("Number of Shift/Reduce entries saved by default: %d", shift_reduce_count);
  num_shifts -= shift_count;
  num_shift_reduces -= shift_reduce_count;
  num_entries = num_entries - shift_count - shift_reduce_count;
  ffree(action_count);
}

/// COMPUTE_GOTO_DEFAULT constructs the vector GOTODEF, which is indexed by
/// the non-terminals in the grammar. Its task is to assign to each element
/// of the array the Action which is most frequently defined on the symbol in
/// question, and remove all such actions from the state automaton.
static void compute_goto_default(long *gotodef) {
  // Set up a pool of temporary space.
  reset_temporary_space();
  int goto_count = 0;
  int goto_reduce_count = 0;
  gotodef = Allocate_long_array(num_non_terminals);
  gotodef -= num_terminals + 1;
  struct ptables_action_element **action_count;
  calloc0(action_count, num_non_terminals, struct ptables_action_element *);
  action_count -= num_terminals + 1;
  if (action_count == NULL) {
    nospace();
  }
  // The array ACTION_COUNT is used to construct a map from each
  // non-terminal into the set (list) of actions defined on that
  // non-terminal. A count of how many occurences of each action
  // is also kept.
  // This loop is analoguous to the loop in PROCESS_SHIFT_ACTIONS.
  for ALL_STATES3(state_no) {
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      const int symbol = go_to.map[i].symbol;
      const short act = go_to.map[i].action;
      struct ptables_action_element *q;
      for (q = action_count[symbol]; q != NULL; q = q->next) {
        if (q->action == act) {
          break;
        }
      }
      if (q == NULL) {
        /* new action not yet seen */
        talloc0(q, struct ptables_action_element);
        q->action = act;
        q->count = 1;
        q->next = action_count[symbol];
        action_count[symbol] = q;
      } else {
        q->count++;
      }
    }
  }
  // We now iterate over the mapping created above and for each
  // non-terminal A, initialize GOTODEF(A) to the action that is
  // most frequently defined on A.
  for ALL_NON_TERMINALS3(symbol) {
    int max_count = 0;
    int default_action = 0;
    struct ptables_action_element *q;
    for (q = action_count[symbol]; q != NULL; q = q->next) {
      if (q->count > max_count) {
        max_count = q->count;
        default_action = q->action;
      }
    }
    gotodef[symbol] = default_action;
    if (default_action > 0) {
      /* A state number? */
      goto_count += max_count;
    } else {
      goto_reduce_count += max_count;
    }
  }
  //   We now iterate over the automaton and eliminate all GOTO actions
  // for which there is a DEFAULT.
  for ALL_STATES3(state_no) {
    int k = 0;
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      if (gotodef[go_to.map[i].symbol] != go_to.map[i].action) {
        k++;
        go_to.map[k].symbol = go_to.map[i].symbol;
        go_to.map[k].action = go_to.map[i].action;
      }
    }
    statset[state_no].go_to.size = k; /* Readjust size */
  }
  PRNT3("Number of Goto entries saved by default: %d", goto_count);
  PRNT3("Number of Goto/Reduce entries saved by default: %d", goto_reduce_count);
  num_gotos -= goto_count;
  num_goto_reduces -= goto_reduce_count;
  num_entries = num_entries - goto_count - goto_reduce_count;
  action_count += num_terminals + 1;
  ffree(action_count);
}

static void init_file(FILE **file, char *file_name, char *file_tag) {
  const char *p = strrchr(file_name, '.');
  if ((*file = fopen(file_name, "w")) == NULL) {
    fprintf(stderr, "***ERROR: Symbol file \"%s\" cannot be opened\n", file_name);
    exit(12);
  } else {
    memcpy(file_tag, file_name, p - file_name);
    file_tag[p - file_name] = '\0';
  }
}

/// Remap symbols, apply transition default actions  and call
/// appropriate table compression routine.
void process_tables(char *tab_file, struct OutputFiles *output_files, struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, struct scope_type *scope, short *gd_range, struct SRTable* srt, long *scope_right_side, struct lastats_type *lastats, short *shiftdf, long *gotodef, short *gd_index, struct statset_type *statset) {
  // First, we decrease by 1 the constants NUM_SYMBOLS
  // and NUM_TERMINALS, remove the EMPTY symbol(1) and remap the
  // other symbols beginning at 1.  If default reduction is
  // requested, we assume a special DEFAULT_SYMBOL with number zero.
  eoft_image--;
  accept_image--;
  if (error_maps_bit) {
    error_image--;
    eolt_image--;
  }
  num_terminals--;
  num_symbols--;
  // Remap all the symbols used in GOTO and REDUCE actions.
  // Remap all the symbols used in GD_RANGE.
  // Remap all the symbols used in the range of SCOPE.
  // Release space trapped by the maps IN_STAT and FIRST.
  for ALL_STATES3(state_no) {
    const struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      go_to.map[i].symbol--;
    }
    struct reduce_header_type red = srt->reduce[state_no];
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol--;
    }
  }
  for ALL_LA_STATES3(state_no) {
    struct reduce_header_type red = lastats[state_no].reduce;
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
  // Remap all symbols in the domain of the Shift maps.
  for (int i = 1; i <= num_shift_maps; i++) {
    const struct shift_header_type sh = srt->shift[i];
    for (int j = 1; j <= sh.size; j++) {
      sh.map[j].symbol--;
    }
  }
  // Remap the left-hand side of all the rules.
  for ALL_RULES3(rule_no) {
    rules[rule_no].lhs--;
  }
  // Remap the dot symbols in ITEM_TABLE.
  if (error_maps_bit) {
    for ALL_ITEMS3(item_no) {
      item_table[item_no].symbol--;
    }
  }
  // We update the SYMNO map.
  for ALL_SYMBOLS3(symbol) {
    symno[symbol] = symno[symbol + 1];
  }
  // If Goto Default and/or Shift Default were requested, process
  // appropriately.
  if (cli_options->shift_default_bit) {
    compute_shift_default(srt, lastats, shiftdf, statset);
  }
  if (cli_options->goto_default_bit) {
    compute_goto_default(gotodef);
  }
  // Release the pool of temporary space.
  free_temporary_space();
  calloc0(output_buffer, IOBUFFER_SIZE, char);
  FILE *systab;
  if (!cli_options->c_bit && !cli_options->cpp_bit && !cli_options->java_bit) {
    if ((systab = fopen(tab_file, "w")) == NULL) {
      fprintf(stderr, "***ERROR: Table file \"%s\" cannot be opened\n", tab_file);
      exit(12);
    }
  }
  struct TableOutput toutput = init_table_output();
  init_file(&of->sysdcl, output_files->dcl_file, of->dcl_tag);
  init_file(&of->syssym, output_files->sym_file, of->sym_tag);
  init_file(&of->sysdef, output_files->def_file, of->def_tag);
  init_file(&of->sysprs, output_files->prs_file, of->prs_tag);
  struct ImportantAspects ia = (struct ImportantAspects) {};
  if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
    cmprspa(cli_options, &toutput, dss, ctp, of, np, scope, &ia, srt, scope_right_side, lastats, shiftdf, gotodef, gd_index, gd_range);
  } else if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    cmprtim(cli_options, &toutput, dss, ctp, of, np, scope, &ia, srt, scope_right_side, lastats, gotodef, gd_index, gd_range);
  } else {
    exit(999);
  }
  if (!cli_options->c_bit && !cli_options->cpp_bit && !cli_options->java_bit) {
    fclose(systab);
  }
}
