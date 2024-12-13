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
      "-fileprefix=string                             \n"
      "-actfilename=string                            \n"
      "-hactfilename=string                           \n"
      "-prefix=string                                 \n"
      "-suffix=string                                 \n"
      "-byte                                          \n"
      "-conflicts                                     \n"
      "-default=<0|1|2|3|4|5>                         \n"
      "-generateparser=string                         \n"
      "-gotodefault                                   \n"
      "-lalr=integer                                  \n"
      "-ntcheck                                       \n"
      "-escape=character                              \n"
      "-ormark=character                              \n"
      "-readreduce                                    \n"
      "-scopes                                        \n"
      "-shift-default                                 \n"
      "-single-productions                            \n"
      "-table=<space|time>                            \n"
      "-trace=<CONFLICTS|FULL|NO>                     \n"
      "-maxdistance=integer                           \n"
      "-mindistance=integer                           \n"
      "-stack-size=integer                            \n"
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

    struct scope_type *scope;

    ArrayShort rhs_sym;

    struct ruletab_type *rules;

    struct ParserState ps = (struct ParserState) {
      .hash_table = NULL,
      .error_maps_bit = cli_options.error_maps_bit,
      .num_acts = 0,
      .num_defs = 0,
      .defelmt_size = 0,
      .actelmt_size = 0,
      .rulehdr_size = 0,
      .string_offset = 0,
      .stack_top = -1,
      .string_table = NULL,
      .num_items = 0,
    };
    struct LAState ls = (struct LAState) {
      .max_la_state = 0,
      .num_items = 0,
      .num_states = 0,
    };
    /// NAME is an array containing names to be associated with symbols.
    int *name;

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
      process_input(grm_file, &of, argc, argv, file_prefix, &cli_options, &rhs_sym, &rules, &ps.symno, &ps, &name);
      ls.num_items = ps.num_items;
    }

    /// FOLLOW is a mapping from non-terminals to a set of terminals that
    /// may appear immediately after the non-terminal.
    JBitset follow = {.raw = NULL};

    ArrayBool rmpself;
    JBitset first;

    struct FirstDeps fd = (struct FirstDeps) {
      .adequate_item = NULL,
      .clitems = NULL,
      .closure = NULL,
    };

    struct itemtab *item_table = NULL;

    struct DetectedSetSizes dss = mkbasic(&cli_options, follow, &rmpself, &first, &fd, rules, rhs_sym, &item_table, ps.string_table, ps.symno, &ls);

    struct SRTable srt = (struct SRTable) {
      .reduce = NULL,
      .shift = NULL,
    };

    ArrayLong scope_right_side;

    ArrayShort scope_state;

    ArrayShort gd_range;
    ArrayShort gd_index;

    struct StatSet ss = (struct StatSet) {
      .statset = NULL,
    };

    struct ScopeCounter sc = (struct ScopeCounter) {
      .num_scopes = 0,
      .scope_rhs_size = 0,
      .scope_state_size = 0,
    };

    mkstats(&cli_options, &dss, first, scope, fd.clitems, fd.closure, &srt, &scope_right_side, dss.null_nt, &scope_state, item_table, rules, rhs_sym, &gd_range, &gd_index, &ss, &sc, ps.symno, ps.string_table, name, &ls);

    struct SourcesElementSources ses = (struct SourcesElementSources) {
      .sources = NULL,
    };
    struct LaStats las = (struct LaStats) {
      .lastats = NULL,
    };
    long la_top = 0;
    struct ConflictCounter conflicts = mkrdcts(&cli_options, &dss, &ses, rmpself, first, fd.adequate_item, &srt, dss.null_nt, gd_index, rules, ss.statset, item_table, rhs_sym, &las, &la_top, ps.string_table, ps.symno, &ls);
    // Output more basic statistics.
    {
      PRNT3("Number of Terminals: %ld", num_terminals - 1); /*-1 for %empty */
      PRNT3("Number of Nonterminals: %ld", num_non_terminals - 1); /* -1 for %ACC */
      PRNT3("Number of Productions: %ld", num_rules + 1);
      PRNT3("Number of Error Rules: %ld", num_error_rules);
      if (cli_options.single_productions_bit) {
        PRNT3("Number of Single Productions: %ld", num_single_productions);
      }
      PRNT3("Number of Items: %ld", ls.num_items);
      if (cli_options.scopes_bit) {
        PRNT3("Number of Scopes: %ld", sc.num_scopes);
      }
      PRNT3("Number of States: %ld", ls.num_states);
      if (ls.max_la_state > ls.num_states) {
        PRNT3("Number of look-ahead states: %ld", ls.max_la_state - ls.num_states);
      }
      PRNT3("Number of Shift actions: %ld", num_shifts);
      PRNT3("Number of Goto actions: %ld", num_gotos);
      if (cli_options.read_reduce_bit) {
        PRNT3("Number of Shift/Reduce actions: %ld", num_shift_reduces);
        PRNT3("Number of Goto/Reduce actions: %ld", num_goto_reduces);
      }
      PRNT3("Number of Reduce actions: %ld", num_reductions);
      PRNT3("Number of Shift-Reduce conflicts: %ld", conflicts.num_sr_conflicts);
      PRNT3("Number of Reduce-Reduce conflicts: %ld", conflicts.num_rr_conflicts);
    }

    if (cli_options.table_opt.value != OPTIMIZE_NO_TABLE.value) {
      if (cli_options.goto_default_bit && cli_options.nt_check_bit) {
        PRNTERR("The options GOTO_DEFAULT and NT_CHECK are incompatible. Tables not generated");
      } else {
        // Prepare table processing.
        {
          num_entries = ls.max_la_state + num_shifts + num_shift_reduces + num_gotos + num_goto_reduces + num_reductions;
          // We release space used by RHS_SYM, the ADEQUATE_ITEM
          // map, ITEM_TABLE (if we don't have to dump error maps),
          // IN_STAT, FIRST, NULL_NT and FOLLOW (if it's no longer
          // needed).
          ffree(rhs_sym.raw);
          if (fd.adequate_item != NULL) {
            for ALL_RULES3(rule_no) {
              struct node *q = fd.adequate_item[rule_no];
              if (q != NULL) {
                free_nodes(q, q);
              }
            }
            ffree(fd.adequate_item);
          }
          if (!cli_options.error_maps_bit) {
            ffree(item_table);
          }
          for ALL_STATES3(state_no, ls.num_states) {
            struct node *head = conflicts.in_stat[state_no];
            if (head != NULL) {
              head = head->next;
              free_nodes(head, conflicts.in_stat[state_no]);
            }
          }
          ffree(conflicts.in_stat);
          ffree(first.raw);
          dss.null_nt.raw += num_terminals + 1;
          ffree(dss.null_nt.raw);
          if (follow.raw != NULL) {
            if (!cli_options.error_maps_bit || cli_options.c_bit || cli_options.cpp_bit || cli_options.java_bit) {
              follow.raw += (num_terminals + 1) * dss.term_set_size;
              ffree(follow.raw);
            }
          }
        }
        struct CTabsProps ctp = (struct CTabsProps) {
          .last_non_terminal = 0,
          .last_terminal = 0,
        };
        struct NextPrevious np = (struct NextPrevious) {
          .previous = NULL,
          .next = NULL,
        };

        ArrayShort shiftdf;
        ArrayLong gotodef;
        process_tables(tab_file, &of, &cli_options, &dss, &ctp, &of, &np, scope, gd_range, &srt, scope_right_side, las.lastats, &shiftdf, &gotodef, gd_index, ss.statset, scope_state, rules, item_table, ps.symno, &sc, ps.string_table, name, &ls);
      }
    }

    return 0;
  }
}
