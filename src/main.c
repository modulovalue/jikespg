#include <stdlib.h>
static char hostfile[] = __FILE__;

#include <string.h>
#include "common.h"

static void print_opts(void);

void process_input(void);

/* Jikes PG is a parser generator capable of generating LALR(k) and  */
/* SLR(1) tables.  It is organized as a main routine: MAIN, which    */
/* invokes five other major subroutines which are:                   */
/*                                                                   */
/*    1) PROCESS_INPUT     - inputs and structures the grammar.      */
/*    2) MKFIRST           - builds basic maps such as FIRST,        */
/*                           FOLLOW, CLOSURE, CLITEMS, ITEM_TABLE,   */
/*                           ADEQUATE_ITEMS.                         */
/*    3) MKSTATS           - constructs the LR(0) automaton.         */
/*    4) MKRDCTS           - constructs reduction map.               */
/*    5) One of the following three procedures:                      */
/*       a) CMPRSPA        - Space Table generation                  */
/*       b) CMPRTIM        - Time Table generation                   */
/*       c) BASETAB        - Write out Base Table                    */
/*                                                                   */
/*   The following files are used:                                   */
/*                                                                   */
/*    1) SYSGRM           - Input file containing grammar            */
/*    2) SYSLIS           - Output file used for listings, statistics*/
/*                          and diagnostics.                         */
/*    3) SYSACT           - Output file used for semantic actions.   */
/*    4) SYSTAB           - Output file used for Parsing tables.     */
/*                                                                   */

/*                          MAIN PROGRAM  MAIN:                             */
int main(int argc, char *argv[]) {
  int i;

  char *dot,
      *slash,
      tmpbuf[20];

  /* If only "jikespg" or "jikespg ?*" is typed, we display the help   */
  /* screen.                                                           */
  /*                                                                   */
  /* NOTE that because of some bug (feature?) in AIX, when two or more */
  /* consecutive "?" is passed as argument to a program, the behaviour */
  /* of the system becomes unpredictable. 1/4/94                       */
  /* You may test this by invoking the "echo" program with "???".      */
  if (argc == 1 || argv[1][0] == '?') {
    print_opts();
    return 4;
  }

  /*     If options are passed to the program, copy them into "parm".   */
  if (argc > 2) {
    int j = 0;
    parm[0] = '\0';
    while (j < argc - 2) {
      if (*(argv[++j]) == '-')
        strcat(parm, argv[j]+1);
      else {
        strcat(parm, argv[j]);
        printf("***WARNING: Option \"%s\" is missing preceding '-'.\n",
               argv[j]);
      }
      strcat(parm, BLANK);
    }
  }


  /*               Create file names for output files                         */
  strcpy(grm_file, argv[argc - 1]);

#if defined(DOS) || defined(OS2)
  slash = strrchr(grm_file, '\\');
#else
  slash = strrchr(grm_file, '/');
#endif
  if (slash != NULL)
    strcpy(tmpbuf, slash + 1);
  else
    strcpy(tmpbuf, grm_file);

  dot = strrchr(tmpbuf, '.');

  if (dot == NULL) /* if filename has no extension, copy it. */
  {
    strcpy(lis_file, tmpbuf);
    strcpy(tab_file, tmpbuf);
    for (i = 0; i < 5; i++)
      file_prefix[i] = tmpbuf[i];
    file_prefix[i] = '\0';
  } else /* if file name contains an extension copy up to the dot */
  {
    for (i = 0; i < 5 && tmpbuf + i != dot; i++)
      file_prefix[i] = tmpbuf[i];
    file_prefix[i] = '\0';
    memcpy(lis_file, tmpbuf, dot - tmpbuf);
    memcpy(tab_file, tmpbuf, dot - tmpbuf);
    lis_file[dot - tmpbuf] = '\0';
    tab_file[dot - tmpbuf] = '\0';
  }

  strcat(lis_file, ".l"); /* add .l extension for listing file */
  strcat(tab_file, ".t"); /* add .t extension for table file */

  process_input();

  /* If the user only wanted to edit his grammar, we quit the program.        */
  if (edit_bit) {
    if (first_bit || follow_bit || xref_bit)
      mkfirst();
    PR_HEADING();
    sprintf(msg_line, "\nNumber of Terminals: %d",
            num_terminals - 1); /*-1 for %empty */
    PRNT(msg_line);

    sprintf(msg_line, "Number of Nonterminals: %d",
            num_non_terminals - 1); /* -1 for %ACC */
    PRNT(msg_line);

    sprintf(msg_line, "Number of Productions: %d", num_rules + 1);
    PRNT(msg_line);

    if (single_productions_bit) {
      sprintf(msg_line, "Number of Single Productions: %d",
              num_single_productions);
      PRNT(msg_line);
    }

    sprintf(msg_line, "Number of Items: %ld", num_items);
    PRNT(msg_line);

    fclose(syslis); /* close listing file */
    return 0;
  }

  mkfirst(); /* Construct basic maps */

  mkstats(); /* Build State Automaton */

  mkrdcts(); /* Build Reduce map, and detect conflicts if any */

  /*                  Print more relevant statistics.                         */
  PR_HEADING();
  sprintf(msg_line, "\nNumber of Terminals: %d", num_terminals - 1);
  PRNT(msg_line);

  sprintf(msg_line, "Number of Nonterminals: %d", num_non_terminals - 1);
  PRNT(msg_line);

  sprintf(msg_line, "Number of Productions: %d", num_rules + 1);
  PRNT(msg_line);

  if (single_productions_bit) {
    sprintf(msg_line,
            "Number of Single Productions: %d",
            num_single_productions);
    PRNT(msg_line);
  }

  sprintf(msg_line, "Number of Items: %ld", num_items);
  PRNT(msg_line);
  if (scopes_bit) {
    sprintf(msg_line, "Number of Scopes: %d", num_scopes);
    PRNT(msg_line);
  }

  sprintf(msg_line, "Number of States: %ld", num_states);
  PRNT(msg_line);

  if (max_la_state > num_states) {
    sprintf(msg_line,
            "Number of look-ahead states: %ld",
            max_la_state - num_states);
    PRNT(msg_line);
  }

  sprintf(msg_line, "Number of Shift actions: %ld", num_shifts);
  PRNT(msg_line);

  sprintf(msg_line, "Number of Goto actions: %ld", num_gotos);
  PRNT(msg_line);

  if (read_reduce_bit) {
    sprintf(msg_line,
            "Number of Shift/Reduce actions: %ld", num_shift_reduces);
    PRNT(msg_line);

    sprintf(msg_line,
            "Number of Goto/Reduce actions: %ld", num_goto_reduces);
    PRNT(msg_line);
  }

  sprintf(msg_line, "Number of Reduce actions: %ld", num_reductions);
  PRNT(msg_line);

  sprintf(msg_line,
          "Number of Shift-Reduce conflicts: %ld", num_sr_conflicts);
  PRNT(msg_line);

  sprintf(msg_line,
          "Number of Reduce-Reduce conflicts: %ld", num_rr_conflicts);
  PRNT(msg_line);

  /* If the removal of single productions is requested, do  */
  /* so now.                                                */
  /* If STATE_BIT is on, we print the states.               */
  if (states_bit) {
    ptstats();
    if (table_opt != 0) {
      PR_HEADING();
    }
  }

  /* If the tables are requested, we process them.          */
  if (table_opt != 0) {
    if (goto_default_bit && nt_check_bit) {
      PRNTERR("The options GOTO_DEFAULT and NT_CHECK are "
        "incompatible.  Tables not generated");
    } else {
      int state_no;

      num_entries = max_la_state + num_shifts + num_shift_reduces
                    + num_gotos + num_goto_reduces
                    + num_reductions;

      /* We release space used by RHS_SYM, the ADEQUATE_ITEM     */
      /* map, ITEM_TABLE (if we don't have to dump error maps),  */
      /* IN_STAT, FIRST, NULL_NT and FOLLOW (if it's no longer   */
      /* needed).                                                */
      ffree(rhs_sym);
      if (adequate_item != NULL) {
        int rule_no;

        for ALL_RULES(rule_no) {
          struct node *q = adequate_item[rule_no];
          if (q != NULL)
            free_nodes(q, q);
        }
        ffree(adequate_item);
      }

      if (!error_maps_bit)
        ffree(item_table);

      for ALL_STATES(state_no) {
        struct node *head = in_stat[state_no];
        if (head != NULL) {
          head = head->next;
          free_nodes(head, in_stat[state_no]);
        }
      }
      ffree(in_stat);
      ffree(first);
      null_nt += (num_terminals + 1);
      ffree(null_nt);
      if (follow != NULL) {
        if ((!error_maps_bit) || c_bit || cpp_bit || java_bit) {
          follow += ((num_terminals + 1) * term_set_size);
          ffree(follow);
        }
      }

      process_tables();
    }
  }

  fclose(syslis); /* close listing file */
  return 0;
}


/*                              PRINT_OPTS:                                 */
static void print_opts(void) {
  printf("\n%s"
         "\n(C) Copyright IBM Corp. 1983, 1999.\n"
         "Licensed Materials - Program Property of IBM - All Rights Reserved.\n\n"
         "Usage: jikespg [options] [filename[.extension]]\n\n"
         "Options                   Options                   Options\n"
         "=======                   =======                   =======\n"

         "-action                   "
         "-actfile-name=string      "
         "-blockb=string\n"
         "-blocke=string            "
         "-byte                     "
         "-conflicts\n"
         "-default[=<0|1|2|3|4|5>]  "
         "-edit                     "
         "-error-maps\n"
         "-escape=character         "
         "-first                    "
         "-follow\n"
         "-generate-parser[=string] "
         "-goto-default             "
         "-half-word\n"
         "-hactfile-name=string     "
         "-hblockb=string           "
         "-hblocke=string\n"
         "-lalr[=integer]           "
         "-list                     "
         "-names=<OPTIMIZED|MAX|MIN>\n"
         "-nt-check                 "
         "-ormark=character         "
         "-output-size=integer\n"
         "-read-reduce              "
         "-scopes                   "
         "-shift-default\n"
         "-single-productions       "
         "-slr                      "
         "-states\n"
         "-table[=<space|time>]     "
         "-trace[=<conflicts|full>] "
         "-verbose\n"
         "-warnings                 "
         "-xref\n\n"

         "The following options are valid only if "
         "GENERATE-PARSER and TABLE are activated:\n"

         "-debug                    "
         "-deferred                 "
         "-file-prefix=string\n"
         "-max-distance=integer     "
         "-min-distance=integer     "
         "-prefix=string\n"
         "-stack-size=integer       "
         "-suffix=string\n\n"

         "Options must be separated by a space.  "
         "Any non-ambiguous initial prefix of a\n"
         "valid option may be used as an abbreviation "
         "for that option.  When an option is\n"
         "composed of two separate words, an "
         "abbreviation may be formed by concatenating\n"
         "the first character of each word.  "
         "Options that are switches may benegated by\n"
         "prefixing them with the string \"no\".  "
         "Default input file extension is \".g\"\n",

         HEADER_INFO);

  printf("\nVersion %s (27 Jan 98) by Philippe Charles, IBM Research."
         "\nAddress comments and questions to charles@watson.ibm.com.\n", VERSION);
}
