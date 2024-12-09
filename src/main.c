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
    // We display the help screen if only "jikespg" or "jikespg ?*" is typed
    {
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
        "-edit                                     \n"
        "-errormaps                                \n"
        // TODO • remove?
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
        // TODO • remove?
        "-slr                                      \n"
        "-states                                   \n"
        "-table=<space|time>                       \n"
        "-trace=<CONFLICTS|FULL|NO>                \n"
        "-verbose                                  \n"
        "-warnings                                 \n"
        "-xref                                     \n"
        "                                          \n"
        "The following options are valid only if   \n"
        "GENERATE-PARSER and TABLE are activated:  \n"
        "                                          \n"
        "-debug                                    \n"
        "-deferred                                 \n"
        "-fileprefix=string                        \n"
        "-maxdistance=integer                      \n"
        "-mindistance=integer                      \n"
        "-prefix=string                            \n"
        "-stack-size=integer                       \n"
        "-suffix=string                            \n"
        ""
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
    }
    return 4;
  } else {
    char tab_file[80];

    struct CLIOptions cli_options;

    // Process input.
    {
      char file_prefix[80] = "";
      char grm_file[80];
      char lis_file[80];
      /* Create file names for output files */
      strcpy(grm_file, argv[argc - 1]);
      char *slash = strrchr(grm_file, '/');
      char tmpbuf[20];
      if (slash != NULL) {
        strcpy(tmpbuf, slash + 1);
      } else {
        strcpy(tmpbuf, grm_file);
      }
      const char *dot = strrchr(tmpbuf, '.');
      /* if filename has no extension, copy it. */
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
        /* if file name contains an extension copy up to the dot */
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
      cli_options = process_input(grm_file, lis_file, &output_files, argc, argv, file_prefix);
    }

    // Process rest.
    {
      mkbasic(&cli_options);
      // If the user only wanted to edit his grammar, we quit the program.
      if (cli_options.edit_bit) {
        // Edit.
        {
          PRNT2(msg_line, "\nNumber of Terminals: %ld", num_terminals - 1); /*-1 for %empty */
          PRNT2(msg_line, "Number of Nonterminals: %ld", num_non_terminals - 1); /* -1 for %ACC */
          PRNT2(msg_line, "Number of Productions: %ld", num_rules + 1);
          if (single_productions_bit) {
            PRNT2(msg_line, "Number of Single Productions: %ld", num_single_productions);
          }
          PRNT2(msg_line, "Number of Items: %ld", num_items);
          fclose(syslis); /* close listing file */
          return 0;
        }
      } else {
        mkstats(&cli_options);

        mkrdcts(&cli_options);

        // Basic statistics.
        {
          PRNT2(msg_line, "\nNumber of Terminals: %ld", num_terminals - 1);
          PRNT2(msg_line, "Number of Nonterminals: %ld", num_non_terminals - 1);
          PRNT2(msg_line, "Number of Productions: %ld", num_rules + 1);
          if (single_productions_bit) {
            PRNT2(msg_line, "Number of Single Productions: %ld", num_single_productions);
          }
          PRNT2(msg_line, "Number of Items: %ld", num_items);
          if (scopes_bit) {
            PRNT2(msg_line, "Number of Scopes: %ld", num_scopes);
          }
          PRNT2(msg_line, "Number of States: %ld", num_states);
          if (max_la_state > num_states) {
            PRNT2(msg_line, "Number of look-ahead states: %ld", max_la_state - num_states);
          }
          PRNT2(msg_line, "Number of Shift actions: %ld", num_shifts);
          PRNT2(msg_line, "Number of Goto actions: %ld", num_gotos);
          if (read_reduce_bit) {
            PRNT2(msg_line, "Number of Shift/Reduce actions: %ld", num_shift_reduces);
            PRNT2(msg_line, "Number of Goto/Reduce actions: %ld", num_goto_reduces);
          }
          PRNT2(msg_line, "Number of Reduce actions: %ld", num_reductions);
          PRNT2(msg_line, "Number of Shift-Reduce conflicts: %ld", num_sr_conflicts);
          PRNT2(msg_line, "Number of Reduce-Reduce conflicts: %ld", num_rr_conflicts);
        }

        if (states_bit) {
          ptstats();
        }

        if (table_opt != 0) {
          if (goto_default_bit && nt_check_bit) {
            PRNTERR("The options GOTO_DEFAULT and NT_CHECK are incompatible. Tables not generated");
          } else {
            num_entries = max_la_state + num_shifts + num_shift_reduces + num_gotos + num_goto_reduces + num_reductions;
            /* We release space used by RHS_SYM, the ADEQUATE_ITEM     */
            /* map, ITEM_TABLE (if we don't have to dump error maps),  */
            /* IN_STAT, FIRST, NULL_NT and FOLLOW (if it's no longer   */
            /* needed).                                                */
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
              if (!error_maps_bit || c_bit || cpp_bit || java_bit) {
                follow += (num_terminals + 1) * term_set_size;
                ffree(follow);
              }
            }
            process_tables(tab_file, output_files, &cli_options);
          }
        }

        fclose(syslis);
      }
    }

    return 0;
  }
}
