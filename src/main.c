static char hostfile[] = __FILE__;

#include <stdlib.h>
#include <string.h>
#include "common.h"

/// This fork of jikespg was meant to clean up jikespg while maintaining its functionality.
/// Unfortunately, the lack of an expressive type system in C makes it very difficult
/// to do that. This fork is a reduced version of the original repo that contains its core
/// functionality with some bugs. Its purpose now is educational to learn about Philippe's
/// Dissertation.
int main(const int argc, char *argv[]) {
  if (argc == 1 || argv[1][0] == '?') {
    // We display the help screen if only "jikespg" or "jikespg ?*" is typed.
    printf(
      "Usage: jikespg [options] [filename[.extension]]\n"
      "AVAILABLE ACTIONS                              \n"
      "-actfilename=string                            \n"
      "-blockb=string                                 \n"
      "-blocke=string                                 \n"
      "-byte                                          \n"
      "-conflicts                                     \n"
      "-default=<0|1|2|3|4|5>                         \n"
      "-errormaps                                     \n"
      "-escape=character                              \n"
      "-first                                         \n"
      "-follow                                        \n"
      "-generateparser=string                         \n"
      "-gotodefault                                   \n"
      "-hactfilename=string                           \n"
      "-hblockb=string                                \n"
      "-hblocke=string                                \n"
      "-lalr=integer                                  \n"
      "-list                                          \n"
      "-names=<OPTIMIZED|MAXIMUM|MINIMUM>             \n"
      "-ntcheck                                       \n"
      "-ormark=character                              \n"
      "-readreduce                                    \n"
      "-scopes                                        \n"
      "-shift-default                                 \n"
      "-single-productions                            \n"
      "-states                                        \n"
      "-table=<space|time>                            \n"
      "-trace=<CONFLICTS|FULL|NO>                     \n"
      "                                               \n"
      "The following options are valid only if        \n"
      "GENERATEPARSER and TABLE are activated:        \n"
      "                                               \n"
      "-debug                                         \n"
      "-fileprefix=string                             \n"
      "-maxdistance=integer                           \n"
      "-mindistance=integer                           \n"
      "-prefix=string                                 \n"
      "-stack-size=integer                            \n"
      "-suffix=string                                 \n"
      "                                               \n"
      "Options must be separated by a space.          \n"
      "Options that are switches may be               \n"
      "negated by prefixing them with the             \n"
      "string \"no\". Default input file              \n"
      "extension is \".g\"                            \n"
    );
    return 4;
  } else {
    // Prepare
    struct CLIOptions cli_options = init_cli_options();
    struct OutputFiles of = {
      .prs_file = "",
      .sym_file = "",
      .def_file = "",
      .dcl_file = "",
    };
    char tab_file[80];

    // Process input.
    {
      char grm_file[80];
      // Create file names for output files
      strcpy(grm_file, argv[argc - 1]);
      const char *slash = strrchr(grm_file, '/');
      char tmpbuf[20];
      if (slash != NULL) {
        strcpy(tmpbuf, slash + 1);
      } else {
        strcpy(tmpbuf, grm_file);
      }
      const char *dot = strrchr(tmpbuf, '.');
      // if filename has no extension, copy it.
      char file_prefix[80] = "";
      if (dot == NULL) {
        strcpy(tab_file, tmpbuf);
        int ii;
        for (ii = 0; ii < 5; ii++) {
          file_prefix[ii] = tmpbuf[ii];
        }
        file_prefix[ii] = '\0';
      } else {
        int ii;
        // if file name contains an extension copy up to the dot
        for (ii = 0; ii < 5 && tmpbuf + ii != dot; ii++) {
          file_prefix[ii] = tmpbuf[ii];
        }
        file_prefix[ii] = '\0';
        memcpy(tab_file, tmpbuf, dot - tmpbuf);
        tab_file[dot - tmpbuf] = '\0';
      }
      strcat(tab_file, ".t"); /* add .t extension for table file */
      process_input(grm_file, &of, argc, argv, file_prefix, &cli_options);
    }

    /// FOLLOW is a mapping from non-terminals to a set of terminals that
    /// may appear immediately after the non-terminal.
    JBitset follow = {.raw = NULL};

    bool *rmpself;

    struct DetectedSetSizes dss = mkbasic(&cli_options, follow, &rmpself, &first);

    mkstats(&cli_options, &dss, first);

    struct SourcesElementSources ses = (struct SourcesElementSources) {
      .sources = NULL,
    };

    mkrdcts(&cli_options, &dss, &ses, rmpself, first);

    // Output more basic statistics.
    {
      PRNT3("Number of Terminals: %ld", num_terminals - 1); /*-1 for %empty */
      PRNT3("Number of Nonterminals: %ld", num_non_terminals - 1); /* -1 for %ACC */
      PRNT3("Number of Productions: %ld", num_rules + 1);
      if (cli_options.single_productions_bit) {
        PRNT3("Number of Single Productions: %ld", num_single_productions);
      }
      PRNT3("Number of Items: %ld", num_items);
      if (cli_options.scopes_bit) {
        PRNT3("Number of Scopes: %ld", num_scopes);
      }
      PRNT3("Number of States: %ld", num_states);
      if (max_la_state > num_states) {
        PRNT3("Number of look-ahead states: %ld", max_la_state - num_states);
      }
      PRNT3("Number of Shift actions: %ld", num_shifts);
      PRNT3("Number of Goto actions: %ld", num_gotos);
      if (cli_options.read_reduce_bit) {
        PRNT3("Number of Shift/Reduce actions: %ld", num_shift_reduces);
        PRNT3("Number of Goto/Reduce actions: %ld", num_goto_reduces);
      }
      PRNT3("Number of Reduce actions: %ld", num_reductions);
      PRNT3("Number of Shift-Reduce conflicts: %ld", num_sr_conflicts);
      PRNT3("Number of Reduce-Reduce conflicts: %ld", num_rr_conflicts);
    }

    if (cli_options.table_opt.value != OPTIMIZE_NO_TABLE.value) {
      if (cli_options.goto_default_bit && cli_options.nt_check_bit) {
        PRNTERR("The options GOTO_DEFAULT and NT_CHECK are incompatible. Tables not generated");
      } else {
        // Prepare table processing.
        {
          num_entries = max_la_state + num_shifts + num_shift_reduces + num_gotos + num_goto_reduces + num_reductions;
          // We release space used by RHS_SYM, the ADEQUATE_ITEM
          // map, ITEM_TABLE (if we don't have to dump error maps),
          // IN_STAT, FIRST, NULL_NT and FOLLOW (if it's no longer
          // needed).
          ffree(rhs_sym);
          if (adequate_item != NULL) {
            for ALL_RULES3(rule_no) {
              struct node *q = adequate_item[rule_no];
              if (q != NULL) {
                free_nodes(q, q);
              }
            }
            ffree(adequate_item);
          }
          if (!error_maps_bit) {
            ffree(item_table);
          }
          for ALL_STATES3(state_no) {
            struct node *head = in_stat[state_no];
            if (head != NULL) {
              head = head->next;
              free_nodes(head, in_stat[state_no]);
            }
          }
          ffree(in_stat);
          ffree(first.raw);
          null_nt += num_terminals + 1;
          ffree(null_nt);
          if (follow.raw != NULL) {
            if (!error_maps_bit || cli_options.c_bit || cli_options.cpp_bit || cli_options.java_bit) {
              follow.raw += (num_terminals + 1) * dss.term_set_size;
              ffree(follow.raw);
            }
          }
        }
        struct CTabsProps ctp = (struct CTabsProps) {
          .last_non_terminal = 0,
          .last_terminal = 0,
        };
        process_tables(tab_file, &of, &cli_options, &dss, &ctp, &of);
      }
    }

    return 0;
  }
}
