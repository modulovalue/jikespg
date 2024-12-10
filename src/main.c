static char hostfile[] = __FILE__;

#include <stdlib.h>
#include <string.h>
#include "common.h"

/// This is a parser generator that generates LALR(k) parsers.
/// It is organized as a main routine: MAIN, which
/// invokes five other major subroutines which are:
///
///    1) PROCESS_INPUT     - inputs and structures the grammar.
///    2) MKFIRST           - builds basic maps such as FIRST, FOLLOW, CLOSURE, CLITEMS, ITEM_TABLE, ADEQUATE_ITEMS.
///    3) MKSTATS           - constructs the LR(0) automaton.
///    4) MKRDCTS           - constructs reduction map.
///    5) One of the following three procedures:
///       a) CMPRSPA        - Space Table generation
///       b) CMPRTIM        - Time Table generation
///       c) BASETAB        - Write out Base Table
///
///   The following files are used:
///
///    1) SYSGRM           - Input file containing grammar
///    2) SYSLIS           - Output file used for listings, statistics and diagnostics.
///    3) SYSACT           - Output file used for semantic actions.
///    4) SYSTAB           - Output file used for Parsing tables.
int main(const int argc, char *argv[]) {
  struct OutputFiles output_files = {
    .prs_file = "",
    .sym_file = "",
    .def_file = "",
    .dcl_file = "",
  };
  if (argc == 1 || argv[1][0] == '?') {
    // We display the help screen if only "jikespg" or "jikespg ?*" is typed.
    printf(
      "Usage: jikespg [options] [filename[.extension]]\n"
      "AVAILABLE ACTIONS                         \n"
      "-action                                   \n"
      "-actfilename=string                       \n"
      "-blockb=string                            \n"
      "-blocke=string                            \n"
      "-byte                                     \n"
      "-conflicts                                \n"
      "-default=<0|1|2|3|4|5>                    \n"
      "-errormaps                                \n"
      "-escape=character                         \n"
      "-first                                    \n"
      "-follow                                   \n"
      "-generateparser=string                    \n"
      "-gotodefault                              \n"
      "-halfword                                 \n"
      "-hactfilename=string                      \n"
      "-hblockb=string                           \n"
      "-hblocke=string                           \n"
      "-lalr=integer                             \n"
      "-list                                     \n"
      "-names=<OPTIMIZED|MAXIMUM|MINIMUM>        \n"
      "-ntcheck                                  \n"
      "-ormark=character                         \n"
      "-readreduce                               \n"
      "-scopes                                   \n"
      "-shift-default                            \n"
      "-single-productions                       \n"
      "-states                                   \n"
      "-table=<space|time>                       \n"
      "-trace=<CONFLICTS|FULL|NO>                \n"
      "                                          \n"
      "The following options are valid only if   \n"
      "GENERATEPARSER and TABLE are activated:   \n"
      "                                          \n"
      "-debug                                    \n"
      "-fileprefix=string                        \n"
      "-maxdistance=integer                      \n"
      "-mindistance=integer                      \n"
      "-prefix=string                            \n"
      "-stack-size=integer                       \n"
      "-suffix=string                            \n"
      "                                          \n"
      "Options must be separated by a space.     \n"
      "Any non-ambiguous initial prefix of a     \n"
      "valid option may be used as an            \n"
      "abbreviation for that option. When an     \n"
      "option is composed of two separate words, \n"
      "an abbreviation may be formed by          \n"
      "concatenating the first character of each \n"
      "word. Options that are switches may       \n"
      "genegated by prefixing them with the      \n"
      "string \"no\". Default input file         \n"
      "extension is \".g\"                       \n"
    );
    return 4;
  } else {
    char tab_file[80];

    struct CLIOptions cli_options = init_cli_options();

    // Process input.
    {
      char file_prefix[80] = "";
      char grm_file[80];
      char lis_file[80];
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
      if (dot == NULL) {
        strcpy(lis_file, tmpbuf);
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
        memcpy(lis_file, tmpbuf, dot - tmpbuf);
        memcpy(tab_file, tmpbuf, dot - tmpbuf);
        lis_file[dot - tmpbuf] = '\0';
        tab_file[dot - tmpbuf] = '\0';
      }
      strcat(lis_file, ".l"); /* add .l extension for listing file */
      strcat(tab_file, ".t"); /* add .t extension for table file */
      process_input(grm_file, lis_file, &output_files, argc, argv, file_prefix, &cli_options);
    }

    // Process rest.
    {
      mkbasic(&cli_options);

      PRNT3("\nNumber of Terminals: %ld", num_terminals - 1); /*-1 for %empty */
      PRNT3("Number of Nonterminals: %ld", num_non_terminals - 1); /* -1 for %ACC */
      PRNT3("Number of Productions: %ld", num_rules + 1);
      if (cli_options.single_productions_bit) {
        PRNT3("Number of Single Productions: %ld", num_single_productions);
      }
      PRNT3("Number of Items: %ld", num_items);

      mkstats(&cli_options);

      mkrdcts(&cli_options);

      // Basic statistics.
      {
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

      if (cli_options.states_bit) {
        ptstats(&cli_options);
      }

      if (cli_options.table_opt != 0) {
        if (cli_options.goto_default_bit && cli_options.nt_check_bit) {
          PRNTERR("The options GOTO_DEFAULT and NT_CHECK are incompatible. Tables not generated");
        } else {
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
          ffree(first);
          null_nt += num_terminals + 1;
          ffree(null_nt);
          if (follow != NULL) {
            if (!error_maps_bit || cli_options.c_bit || cli_options.cpp_bit || cli_options.java_bit) {
              follow += (num_terminals + 1) * term_set_size;
              ffree(follow);
            }
          }
          process_tables(tab_file, &output_files, &cli_options);
        }

        fclose(syslis);
      }
    }

    return 0;
  }
}
