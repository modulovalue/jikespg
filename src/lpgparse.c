#include <stdlib.h>

static char hostfile[] = __FILE__;

#include <string.h>
#include <ctype.h>
#include "common.h"
#include "lpgsym.h"
#include "lpgdef.h"
#include "lpgdcl.h"
#include "lpgparse.h"
#include "lpgact.c"
#include "lpgact.h"
#include "lpgprs.h"

char *string_table = NULL;

/// SYMNO is an array that maps symbol numbers to actual symbols.
struct symno_type *symno = NULL;

/// NAME is an array containing names to be associated with symbols.
int *name;

char *RETRIEVE_STRING(const int indx) {
  return &string_table[symno[indx].ptr];
}

const char *RETRIEVE_NAME(const int indx) {
  return &string_table[name[indx]];
}

// TODO • make this a local?
char parm[256] = "";

long string_offset = 0;

char blockb[MAX_PARM_SIZE] = {'/', '.'};
char blocke[MAX_PARM_SIZE] = {'.', '/'};
char hblockb[MAX_PARM_SIZE] = {'/', ':'};
char hblocke[MAX_PARM_SIZE] = {':', '/'};

const int SPACE_CODE = 1;
const int DIGIT_CODE = 2;
const int ALPHA_CODE = 3;
char code[256] = {0};

bool IsSpace(const int c) {
  return code[c] == SPACE_CODE;
}

bool IsDigit(const int c) {
  return code[c] == DIGIT_CODE;
}

bool IsAlpha(const int c) {
  return code[c] == ALPHA_CODE;
}

int line_no = 0;

/// The two character pointer variables, P1 and P2, are used in
/// processing the io buffer, INPUT_BUFFER.
char *p1;
char *p2;
char *input_buffer;

char *linestart;
char *bufend;
char *ct_ptr;

/// current token & related variables
short ct = 0;
short ct_start_col = 0;
short ct_end_col = 0;
short ct_length = 0;

long ct_start_line = 0;
long ct_end_line = 0;

const int OUTPUT_PARM_SIZE = MAX_PARM_SIZE + 7;
const int MAXIMUM_LA_LEVEL = 100;
const int STRING_BUFFER_SIZE = 8192;

int blockb_len;
int blocke_len;
int hblockb_len;
int hblocke_len;

short *macro_table;

/// READ_INPUT fills the buffer from p1 to the end.
void read_input(char *grm_file, FILE *sysgrm) {
  long num_read = input_buffer + IOBUFFER_SIZE - bufend;
  if ((num_read = fread(bufend, 1, num_read, sysgrm)) == 0) {
    if (ferror(sysgrm) != 0) {
      fprintf(stderr, "*** Error reading input file \"%s\".\n", grm_file);
      exit(12);
    }
  }
  bufend += num_read;
  *bufend = '\0';
}

/// VERIFY takes as argument a character string and checks whether each
/// character is a digit. If all are digits, then 1 is returned; if not, then
/// 0 is returned.
bool verify_is_digit(const char *item) {
  while (IsDigit(*item)) {
    item++;
  }
  return *item == '\0';
}

/// TRANSLATE takes as arguments a character array, which it folds to upper
/// to uppercase and returns.
char *translate(char *str, const int len) {
  for (register int i = 0; i < len; i++) {
    str[i] = TOUPPER(str[i]);
  }
  return str;
}

/// Compare two character strings s1 and s2 to check whether s2
/// is a substring of s1. The string s2 is assumed to be in lowercase
/// and NULL terminated. However, s1 does not have to (indeed, may not)
/// be NULL terminated.
///
/// The test below may look awkward. For example, why not use:
///                  if (tolower(s1[i]) != s2[i])  ?
/// because tolower(ch) is sometimes implemented as (ch-'A'+'a') which
/// does not work when "ch" is already a lower case character.
bool strxeq(char *s1, char *s2) {
  for (; *s2 != '\0'; s1++, s2++) {
    if (*s1 != *s2 && *s1 != toupper(*s2))
      return false;
  }
  return true;
}

/// OPTION handles the decoding of options passed by the user and resets
/// them appropriately. "options" may be called twice: when a parameter line
/// is passed to the main program and when the user codes an %OPTIONS line in
/// his grammar.
/// Basically, there are two kinds of options: switches which indicate a
/// certain setting just by their appearance, and valued options which are
/// followed by an equal sign and the value to be assigned to them.
void options(char *file_prefix, struct CLIOptions *cli_options) {
  char token[MAX_PARM_SIZE + 1];
  char temp[MAX_PARM_SIZE + 1];
  char *c;
  // If we scan the comment sign, we stop processing the rest of the
  // parameter string.
  for (c = parm; *c != '\0'; c++) {
    if (*c == '-' && *(c + 1) == '-') {
      break;
    }
  }
  *c = '\0';
  register int i = 0;
  while (parm[i] != '\0' && /* Clean front of string */ (parm[i] == ',' || parm[i] == '/' || parm[i] == ' ')) {
    i++;
  }
  while (parm[i] != '\0') {
    // Repeat until parm line is exhausted
    // Remove garbage in front
    memmove(parm, parm + i, strlen (parm + i) + 1);
    i = 0;
    while (parm[i] != '\0' && /* Search for delimiter */ (parm[i] != ',' && parm[i] != '/' && parm[i] != '=' && parm[i] != ' ')) {
      i++;
    }
    for (int j = 0; j < i; j++) {
      // Fold actual parameter
      token[j] = TOUPPER(parm[j]);
      temp[j] = parm[j];
    }
    token[i] = '\0';
    temp[i] = '\0';
    // find first non-blank after parm
    while (parm[i] != '\0' && parm[i] == ' ') {
      i++;
    }
    char delim;
    if (parm[i] != '\0') {
      delim = parm[i]; /* not end of parameter line */
    } else {
      delim = ' ';
    }
    register int token_len = strlen(token);
    if (token_len > MAX_PARM_SIZE) {
      token[MAX_PARM_SIZE] = '\0';
    }
    // We check whether we have a switch or a value parameter.
    // Each category is checked separately.  A match is made whenever
    // a minimum unambiguous prefix of the token in question matches an
    // option...
    //
    // At this stage, TEMP contains the value of the switch as specified
    // and TOKEN contains the upper-case folded value of TEMP.
    // if switch parameter then process
    if (delim != '=') {
      bool flag;
      if (memcmp(token, "NO", 2) == 0) {
        flag = false;
        token_len = token_len - 2;
        memmove(token, token + 2, strlen(token + 2) + 1); /* get rid of "NO" prefix */
      } else {
        flag = true;
      }
      if (memcmp("BYTE", token, token_len) == 0) {
        cli_options->byte_bit = flag;
      } else if (memcmp("CONFLICTS", token, token_len) == 0) {
        cli_options->conflicts_bit = flag;
      } else if (memcmp("DEBUG", token, token_len) == 0) {
        cli_options->debug_bit = flag;
      } else if (memcmp("ERRORMAPS", token, token_len) == 0) {
        error_maps_bit = flag;
      } else if (memcmp("FIRST", token, token_len) == 0) {
        cli_options->first_bit = flag;
      } else if (memcmp("FOLLOW", token, token_len) == 0) {
        cli_options->follow_bit = flag;
      } else if (memcmp("GOTODEFAULT", token, token_len) == 0) {
        cli_options->goto_default_bit = flag;
      } else if (memcmp("HALFWORD", token, token_len) == 0) {
        cli_options->byte_bit = !flag;
      } else if (memcmp("LIST", token, token_len) == 0) {
        cli_options->list_bit = flag;
      } else if (memcmp("NTCHECK", token, token_len) == 0) {
        cli_options->nt_check_bit = flag;
      } else if (memcmp("READREDUCE", token, token_len) == 0) {
        cli_options->read_reduce_bit = flag;
      } else if (memcmp("SCOPES", token, token_len) == 0) {
        cli_options->scopes_bit = flag;
      } else if (memcmp("SHIFTDEFAULT", token, token_len) == 0) {
        cli_options->shift_default_bit = flag;
      } else if (memcmp("SINGLEPRODUCTIONS", token, token_len) == 0) {
        cli_options->single_productions_bit = flag;
      } else if (memcmp("STATES", token, token_len) == 0) {
        cli_options->states_bit = flag;
      } else {
        PRNTERR2("\"%s\" is an invalid option", temp);
      }
    } else {
      // We now process the valued-parameter. Pick value after "=" and process
      i++;
      if (IsSpace(parm[i]) || parm[i] == '\0') {
        // no value specified
        PRNTERR2("Null string or blank is invalid for parameter %s", token);
        continue;
      }
      int j = i;
      while (parm[i] != '\0' && /* find next delimeter */ (parm[i] != ',' && parm[i] != '/' && parm[i] != ' ')) {
        i++;
      }
      memcpy(temp, parm+j, i - j); /* copy into TEMP */
      temp[i - j] = '\0';
      if (memcmp(token, "ACTFILENAME", token_len) == 0) {
        strcpy(cli_options->act_file, temp);
      } else if (strcmp(token, "BLOCKB") == 0) {
        strcpy(blockb, temp);
      } else if (strcmp(token, "BLOCKE") == 0) {
        strcpy(blocke, temp);
      } else if (memcmp("DEFAULT", token, token_len) == 0) {
        if (verify_is_digit(temp)) {
          cli_options->default_opt = MIN(atoi(temp), 5);
        } else {
          PRNTERR2("\"%s\" is an invalid value for %s", temp, token);
        }
      } else if (memcmp(token, "ESCAPE", token_len) == 0) {
        escape = temp[0];
      } else if (memcmp(token, "FILEPREFIX", token_len) == 0) {
        memcpy(file_prefix, temp, 5);
        file_prefix[MIN(5, strlen(temp))] = '\0';
      } else if (memcmp("GENERATEPARSER", token, token_len) == 0) {
        if (strxeq(temp, "c")) {
          cli_options->c_bit = true;
          cli_options->cpp_bit = false;
          cli_options->java_bit = false;
        } else if (strxeq(temp, "cpp")) {
          cli_options->c_bit = false;
          cli_options->cpp_bit = true;
          cli_options->java_bit = false;
        } else if (strxeq(temp, "java")) {
          cli_options->c_bit = false;
          cli_options->cpp_bit = false;
          cli_options->java_bit = true;
        } else {
          PRNTERR2("\"%s\" is an invalid language for %s", temp, token);
        }
      } else if (memcmp(token, "HACTFILENAME", token_len) == 0) {
        strcpy(cli_options->hact_file, temp);
      } else if (strcmp(token, "HBLOCKB") == 0) {
        strcpy(hblockb, temp);
      } else if (strcmp(token, "HBLOCKE") == 0) {
        strcpy(hblocke, temp);
      } else if (memcmp("LALR", token, token_len) == 0) {
        register int token_len = strlen(temp);
        if (token_len > MAX_PARM_SIZE) {
          temp[MAX_PARM_SIZE - 1] = '\0';
        }
        if (verify_is_digit(temp)) {
          cli_options->lalr_level = atoi(temp);
          if (cli_options->lalr_level > MAXIMUM_LA_LEVEL) {
            PRNTWNG2("\"%s\" exceeds maximum value of %d allowed for %s", temp, MAXIMUM_LA_LEVEL, token);
            cli_options->lalr_level = MAXIMUM_LA_LEVEL;
          }
        } else if (memcmp(translate(temp, token_len), "MAXIMUM", token_len) != 0) {
          PRNTERR2("\"%s\" is an invalid value for %s", temp, token);
        } else if (memcmp("MAXIMUM", translate(temp, token_len), token_len) == 0) {
          cli_options->lalr_level = MAXIMUM_LA_LEVEL;
        }
      } else if (memcmp(token, "MAXDISTANCE", token_len) == 0) {
        if (verify_is_digit(temp)) {
          cli_options->maximum_distance = atoi(temp);
        } else {
          PRNTERR2("\"%s\" is an invalid value for %s", temp, token);
        }
      } else if (memcmp(token, "MINDISTANCE", token_len) == 0) {
        if (verify_is_digit(temp)) {
          cli_options->minimum_distance = atoi(temp);
        } else {
          PRNTERR2("\"%s\" is an invalid value for %s", temp, token);
        }
      } else if (memcmp("NAMES", token, token_len) == 0) {
        register int token_len = strlen(temp);
        if (memcmp("MAXIMUM", translate(temp, token_len), token_len) == 0) {
          cli_options->names_opt = MAXIMUM_NAMES;
        } else if (memcmp("MINIMUM", translate(temp, token_len), token_len) == 0) {
          cli_options->names_opt = MINIMUM_NAMES;
        } else if (memcmp(translate(temp, token_len), "OPTIMIZED", token_len) == 0) {
          cli_options->names_opt = OPTIMIZE_PHRASES;
        } else {
          PRNTERR2("\"%s\" is an invalid value for %s", temp, token);
        }
      } else if (memcmp(token, "ORMARK", token_len) == 0) {
        ormark = temp[0];
      } else if (memcmp(token, "PREFIX", token_len) == 0) {
        strcpy(prefix, temp);
      } else if (memcmp(token, "STACKSIZE", token_len) == 0) {
        if (verify_is_digit(temp)) {
          cli_options->stack_size = atoi(temp);
        } else {
          PRNTERR2("\"%s\" is an invalid value for %s", temp, token);
        }
      } else if (memcmp(token, "SUFFIX", token_len) == 0) {
        strcpy(suffix, temp);
      } else if (memcmp(token, "TABLE", token_len) == 0) {
        register int token_len = strlen(temp);
        if (token_len > MAX_PARM_SIZE) {
          temp[MAX_PARM_SIZE - 1] = '\0';
        }
        if (memcmp("SPACE", translate(temp, token_len), token_len) == 0) {
          cli_options->table_opt = OPTIMIZE_SPACE;
        } else if (memcmp(translate(temp, token_len), "TIME", token_len) == 0) {
          cli_options->table_opt = OPTIMIZE_TIME;
        } else {
          PRNTERR2("\"%s\" is an invalid value for %s", temp, token);
        }
      } else if (memcmp(token, "TRACE", token_len) == 0) {
        token_len = strlen(temp);
        if (token_len > MAX_PARM_SIZE) {
          temp[MAX_PARM_SIZE - 1] = '\0';
        }
        if (memcmp("CONFLICTS", translate(temp, token_len), token_len) == 0) {
          cli_options->trace_opt = TRACE_CONFLICTS;
        } else if (memcmp(translate(temp, token_len), "FULL", token_len) == 0) {
          cli_options->trace_opt = TRACE_FULL;
        } else if (memcmp(translate(temp, token_len), "NO", token_len) == 0) {
          cli_options->trace_opt = NOTRACE;
        } else {
          PRNTERR2("\"%s\" is an invalid value for %s", temp, token);
        }
      } else {
        PRNTERR2("\"%s\" is an invalid option", token);
      }
    }
    while (parm[i] != '\0' && /* clean after parameter */ (parm[i] == ',' || parm[i] == '/' || parm[i] == ' ')) {
      i++;
    }
  }
}

/// In this function, we read the first line(s) of the input text to see
/// if they are (it is an) "options" line(s).  If so, the options are
/// processed.  Then, we process user-supplied options if there are any.  In
/// any case, the options in effect are printed.
void process_options_lines(char *grm_file, struct OutputFiles *output_files, char *file_prefix, struct CLIOptions *cli_options, FILE *sysgrm) {
  char old_parm[MAX_LINE_SIZE + 1];
  char output_line[PRINT_LINE_SIZE + 1];
  char opt_string[60][OUTPUT_PARM_SIZE + 1];
  char *line_end;
  char temp[SYMBOL_SIZE + 1];
  int top = 0;
  strcpy(old_parm, parm); /* Save new options passed to program */
  static char ooptions[9] = " OPTIONS";
  ooptions[0] = escape; /* "ooptions" always uses default escape symbol */
  // Until end-of-file is reached, process
  while (p1 != NULL) {
    // all comment and %options lines.
    while (IsSpace(*p2)) {
      // skip all space symbols
      if (*p2 == '\n') {
        line_no++;
        linestart = p2;
        p1 = p2 + 1;
      }
      p2++;
    }
    line_end = strchr(p2, '\n'); /* find end-of-line */
    // First, check if line is a comment line. If so, skip it.  Next,
    // check if line is an options line. If so, process it. Otherwise,
    // break out of the loop.
    // Note that no length check is necessary before checking for "--"
    // or "%options" since the buffer is always extended by
    // MAX_LINE_SIZE elements past its required length. (see read_input)
    if (*p2 == '-' && *(p2 + 1) == '-') {
      // Skip comment line.
    } else if (memcmp(ooptions, translate(p2, 8), 8) == 0) {
      *line_end = '\0';
      PRNT(p2); /* Print options line */
      strcpy(parm, p2 + strlen(ooptions));
      options(file_prefix, cli_options); /* Process hard-coded options */
    } else {
      p2 = p1; /* make p2 point to first character */
      break;
    }
    // If the line was a comment or an option line, check the following
    // line.  If we are at the end of the buffer, read in more data...
    p1 = line_end + 1;
    if (bufend == input_buffer + IOBUFFER_SIZE) {
      int i = bufend - p1;
      if (i < MAX_LINE_SIZE) {
        strcpy(input_buffer, p1);
        bufend = &input_buffer[i];
        read_input(grm_file, sysgrm);
        p1 = &input_buffer[0];
      }
    }
    line_no++;
    linestart = p1 - 1;
    p2 = p1;
  }
  fprintf(syslis, "\n");
  strcpy(parm, old_parm);
  options(file_prefix, cli_options); /* Process new options passed directly to program */
  if (cli_options->act_file[0] == '\0') {
    sprintf(cli_options->act_file, "%sact.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  }
  if (cli_options->hact_file[0] == '\0') {
    sprintf(cli_options->hact_file, "%shdr.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  }
  sprintf(output_files->sym_file, "%ssym.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  sprintf(output_files->def_file, "%sdef.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  sprintf(output_files->prs_file, "%sprs.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  sprintf(output_files->dcl_file, "%sdcl.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  //                          PRINT OPTIONS:
  // Here we print all options set by the user. As of now, only about 48
  // different options and related aliases are allowed. In case that number
  // goes up, the bound of the array, opt_string, should be changed.
  // BLOCKB, BLOCKE, HBLOCKB and HBLOCKE can generate the longest strings
  // since their value can be up to MAX_PARM_SIZE characters long.
  sprintf(opt_string[++top], "ACTFILENAME=%s", cli_options->act_file);
  sprintf(opt_string[++top], "BLOCKB=%s", blockb);
  sprintf(opt_string[++top], "BLOCKE=%s", blocke);
  strcpy(opt_string[++top], cli_options->byte_bit ? "BYTE" : "NOBYTE");
  strcpy(opt_string[++top], cli_options->conflicts_bit ? "CONFLICTS" : "NOCONFLICTS");
  if (cli_options->default_opt == 0) {
    strcpy(opt_string[++top], "NODEFAULT");
  } else {
    sprintf(opt_string[++top], "DEFAULT=%d", cli_options->default_opt);
  }
  strcpy(opt_string[++top], cli_options->debug_bit ? "DEBUG" : "NODEBUG");
  strcpy(opt_string[++top], error_maps_bit ? "ERROR-MAPS" : "NOERROR-MAPS");
  sprintf(opt_string[++top], "ESCAPE=%c", escape);
  sprintf(opt_string[++top], "FILE-PREFIX=%s", file_prefix);
  strcpy(opt_string[++top], cli_options->first_bit ? "FIRST" : "NOFIRST");
  strcpy(opt_string[++top], cli_options->follow_bit ? "FOLLOW" : "NOFOLLOW");
  if (cli_options->c_bit) {
    sprintf(opt_string[++top], "GENERATE-PARSER=C");
  } else if (cli_options->cpp_bit) {
    sprintf(opt_string[++top], "GENERATE-PARSER=CPP");
  } else if (cli_options->java_bit) {
    sprintf(opt_string[++top], "GENERATE-PARSER=JAVA");
  } else {
    strcpy(opt_string[++top], "NOGENERATE-PARSER");
  }
  strcpy(opt_string[++top], cli_options->goto_default_bit ? "GOTODEFAULT" : "NOGOTODEFAULT");
  sprintf(opt_string[++top], "HACTFILENAME=%s", cli_options->hact_file);
  sprintf(opt_string[++top], "HBLOCKB=%s", hblockb);
  sprintf(opt_string[++top], "HBLOCKE=%s", hblocke);
  sprintf(opt_string[++top], "LALR=%d", cli_options->lalr_level);
  strcpy(opt_string[++top], cli_options->list_bit ? "LIST" : "NOLIST"); {
    sprintf(opt_string[++top], "MIN-DISTANCE=%d", cli_options->minimum_distance);
    if (cli_options->minimum_distance <= 1) {
      PRNT("MIN_DISTANCE must be > 1");
      exit(12);
    }
  } {
    sprintf(opt_string[++top], "MAX-DISTANCE=%d", cli_options->maximum_distance);
    if (cli_options->maximum_distance <= cli_options->minimum_distance + 1) {
      PRNT("MAX_DISTANCE must be > MIN_DISTANCE + 1");
      exit(12);
    }
  }
  if (cli_options->names_opt == MAXIMUM_NAMES) {
    strcpy(opt_string[++top], "NAMES=MAXIMUM");
  } else if (cli_options->names_opt == MINIMUM_NAMES) {
    strcpy(opt_string[++top], "NAMES=MINIMUM");
  } else {
    strcpy(opt_string[++top], "NAMES=OPTIMIZED");
  }
  strcpy(opt_string[++top], cli_options->nt_check_bit ? "NTCHECK" : "NONTCHECK");
  sprintf(opt_string[++top], "ORMARK=%c", ormark);
  sprintf(opt_string[++top], "PREFIX=%s", prefix);
  strcpy(opt_string[++top], cli_options->read_reduce_bit ? "READREDUCE" : "NOREADREDUCE");
  strcpy(opt_string[++top], cli_options->scopes_bit ? "SCOPES" : "NOSCOPES");
  strcpy(opt_string[++top], cli_options->shift_default_bit ? "SHIFT-DEFAULT" : "NOSHIFT-DEFAULT");
  strcpy(opt_string[++top], cli_options->single_productions_bit ? "SINGLE-PRODUCTIONS" : "NOSINGLE-PRODUCTIONS");
  sprintf(opt_string[++top], "STACK-SIZE=%d", cli_options->stack_size);
  strcpy(opt_string[++top], cli_options->states_bit ? "STATES" : "NOSTATES");
  sprintf(opt_string[++top], "SUFFIX=%s", suffix);
  if (cli_options->table_opt == OPTIMIZE_NO_TABLE) {
    strcpy(opt_string[++top], "NOTABLE");
  } else if (cli_options->table_opt == OPTIMIZE_SPACE) {
    strcpy(opt_string[++top], "TABLE=SPACE");
  } else if (cli_options->table_opt == OPTIMIZE_TIME) {
    strcpy(opt_string[++top], "TABLE=TIME");
  } else {
    PRNT("Unsupported table optimization option.");
    exit(12);
  }
  if (cli_options->trace_opt == NOTRACE) {
    strcpy(opt_string[++top], "NOTRACE");
  } else if (cli_options->trace_opt == TRACE_CONFLICTS) {
    strcpy(opt_string[++top], "TRACE=CONFLICTS");
  } else if (cli_options->trace_opt == TRACE_FULL) {
    strcpy(opt_string[++top], "TRACE=FULL");
  } else {
    PRNT("Unsupported trace option.");
    exit(12);
  }
  PRNT("Options in effect:");
  strcpy(output_line, "    ");
  for (int i = 1; i <= top; i++) {
    if (strlen(output_line) + strlen(opt_string[i]) > PRINT_LINE_SIZE - 1) {
      PRNT(output_line);
      strcpy(output_line, "    ");
    }
    strcat(output_line, opt_string[i]);
    if (strlen(output_line) + 2 < PRINT_LINE_SIZE - 1) {
      strcat(output_line, "  ");
    }
  }
  PRNT(output_line);
  PRNT("");
  if (cli_options->table_opt == OPTIMIZE_SPACE) {
    if (cli_options->default_opt < 4) {
      PRNTWNG("DEFAULT_OPTION requested must be >= 4 if optimizing for space");
    }
  } else if (cli_options->table_opt == OPTIMIZE_TIME) {
    if (cli_options->shift_default_bit) {
      PRNTWNG("SHIFT-DEFAULT option is only valid for Space tables");
    }
  }
  // Check if there are any conflicts in the options.
  temp[0] = '\0';
  if (strcmp(blockb, blocke) == 0) {
    strcpy(temp, "BLOCKB and BLOCKE");
  } else if (strlen(blockb) == 1 && blockb[0] == escape) {
    strcpy(temp, "BLOCKB and ESCAPE");
  } else if (strlen(blockb) == 1 && blockb[0] == ormark) {
    strcpy(temp, "BLOCKB and ORMARK");
  } else if (strlen(blocke) == 1 && blocke[0] == escape) {
    strcpy(temp, "ESCAPE and BLOCKE");
  } else if (strlen(blocke) == 1 && blocke[0] == ormark) {
    strcpy(temp, "ORMARK and BLOCKE");
  } else if (strcmp(hblockb, hblocke) == 0) {
    strcpy(temp, "HBLOCKB and HBLOCKE");
  } else if (strlen(hblockb) == 1 && hblockb[0] == escape) {
    strcpy(temp, "HBLOCKB and ESCAPE");
  } else if (strlen(hblockb) == 1 && hblockb[0] == ormark) {
    strcpy(temp, "HBLOCKB and ORMARK");
  } else if (strlen(hblocke) == 1 && hblocke[0] == escape) {
    strcpy(temp, "ESCAPE and HBLOCKE");
  } else if (strlen(hblocke) == 1 && hblocke[0] == ormark) {
    strcpy(temp, "ORMARK and HBLOCKE");
  } else if (ormark == escape) {
    strcpy(temp, "ORMARK and ESCAPE");
  }
  if (temp[0] != '\0') {
    PRNTERR2("The options %s cannot have the same value", temp);
    PRNT3("Input process aborted at line %d ...", line_no);
    exit(12);
  }
  if (strlen(hblockb) <= strlen(blockb) && memcmp(hblockb, blockb, strlen(hblockb)) == 0) {
    PRNTERR2("Hblockb value, %s, cannot be a suffix of blockb: %s", hblockb, blockb);
    PRNT3("Input process aborted at line %d ...", line_no);
    exit(12);
  }
}

/// HASH takes as argument a symbol and hashes it into a location in
/// HASH_TABLE.
int hash(const char *symbl) {
  register unsigned long hash_value = 0;
  for (; *symbl != '\0'; symbl++) {
    const register unsigned short k = *symbl;
    symbl++;
    hash_value += (k << 7) + *symbl;
    if (*symbl == '\0') {
      break;
    }
  }
  return hash_value % HT_SIZE;
}

/// INSERT_STRING takes as an argument a pointer to a ht_elemt structure and
/// a character string.  It inserts the string into the string table and sets
/// the value of node to the index into the string table.
void insert_string(struct hash_type *q, const char *string) {
  long string_size = 0;
  if (string_offset + strlen(string) >= string_size) {
    string_size += STRING_BUFFER_SIZE;
    INT_CHECK(string_size);
    string_table = (char *) (string_table == (char *) NULL
       ? malloc(string_size * sizeof(char))
       : realloc(string_table, string_size * sizeof(char)));
    if (string_table == (char *) NULL) {
      nospace(__FILE__, __LINE__);
    }
  }
  q->st_ptr = string_offset;
  // Copy until NULL is copied.
  while ((string_table[string_offset++] = *string++)) {
  }
}

bool EQUAL_STRING(const char *symb, const struct hash_type *p) {
  return strcmp(symb, string_table + p->st_ptr) == 0;
}

/// PROCESS_SYMBOL takes as an argument a pointer to the most recent token
/// which would be either a symbol or a macro name and then processes it. If
/// the token is a macro name then a check is made to see if it is a pre-
/// defined macro. If it is then an error message is printed and the program
/// is halted. If not, or if the token is a symbol then it is hashed into the
/// hash_table and its string is copied into the string table.  A struct is
/// created for the token. The ST_PTR field contains the index into the string
/// table and the NUMBER field is set to zero. Later on if the token is a
/// symbol, the value of the NUMBER field is changed to the appropriate symbol
/// number. However, if the token is a macro name, its value will remain zero.
/// The NAME_INDEX field is set to OMEGA and will be assigned a value later.
///   ASSIGN_SYMBOL_NO takes as arguments a pointer to a node and an image
/// number and assigns a symbol number to the symbol pointed to by the node.
void assign_symbol_no(const char *string_ptr, const int image) {
  register struct hash_type *p;
  const register int i = hash(string_ptr);
  for (p = hash_table[i]; p != NULL; p = p->link) {
    if (EQUAL_STRING(string_ptr, p)) /* Are they the same */
      return;
  }
  talloc0(p, struct hash_type);
  if (image == OMEGA) {
    num_symbols++;
    p->number = num_symbols;
  } else {
    p->number = -image;
  }
  p->name_index = OMEGA;
  insert_string(p, string_ptr);
  p->link = hash_table[i];
  hash_table[i] = p;
}

/// ALIAS_MAP takes as input a symbol and an image. It searcheds the hash
/// table for stringptr and if it finds it, it turns it into an alias of the
/// symbol whose number is IMAGE. Otherwise, it invokes PROCESS_SYMBOL and
/// ASSIGN SYMBOL_NO to enter stringptr into the table and then we alias it.
void alias_map(const char *stringptr, const int image) {
  for (register struct hash_type *q = hash_table[hash(stringptr)]; q != NULL; q = q->link) {
    if (EQUAL_STRING(stringptr, q)) {
      q->number = -image; /* Mark alias of image */
      return;
    }
  }
  assign_symbol_no(stringptr, image);
}

/// SYMBOL_IMAGE takes as argument a symbol.  It searches for that symbol
/// in the HASH_TABLE, and if found, it returns its image; otherwise, it
/// returns OMEGA.
static int symbol_image(const char *item) {
  for (const register struct hash_type *q = hash_table[hash(item)]; q != NULL; q = q->link) {
    if (EQUAL_STRING(item, q))
      return ABS(q->number);
  }
  return OMEGA;
}

/// NAME_MAP takes as input a symbol and inserts it into the HASH_TABLE if it
/// is not yet in the table. If it was already in the table then it is
/// assigned a NAME_INDEX number if it did not yet have one.  The name index
/// assigned is returned.
static int name_map(const char *symb) {
  register struct hash_type *p;
  const register int i = hash(symb);
  for (p = hash_table[i]; p != NULL; p = p->link) {
    if (EQUAL_STRING(symb, p)) {
      if (p->name_index != OMEGA) {
        return p->name_index;
      } else {
        num_names++;
        p->name_index = num_names;
        return num_names;
      }
    }
  }
  talloc0(p, struct hash_type);
  p->number = 0;
  insert_string(p, symb);
  p->link = hash_table[i];
  hash_table[i] = p;
  num_names++;
  p->name_index = num_names;
  return num_names;
}

/// SCANNER scans the input stream and returns the next input token.
void scanner(char *grm_file, FILE *sysgrm) {
  char tok_string[SYMBOL_SIZE + 1];
scan_token:
  // Skip "blank" spaces.
  p1 = p2;
  while (IsSpace(*p1)) {
    if (*p1++ == '\n') {
      if (bufend == input_buffer + IOBUFFER_SIZE) {
        int i = bufend - p1;
        if (i < MAX_LINE_SIZE) {
          strcpy(input_buffer, p1);
          bufend = &input_buffer[i];
          read_input(grm_file, sysgrm);
          p1 = &input_buffer[0];
        }
      }
      line_no++;
      linestart = p1 - 1;
    }
  }
  if (strncmp(p1, hblockb, hblockb_len) == 0) /* check block opener */
  {
    p1 = p1 + hblockb_len;
    ct_length = 0;
    ct_ptr = p1;
    if (*p1 == '\n') {
      ct_ptr++;
      ct_length--;
      ct_start_line = line_no + 1;
      ct_start_col = 1;
    } else {
      ct_start_line = line_no;
      ct_start_col = p1 - linestart;
    }

    while (strncmp(p1, hblocke, hblocke_len) != 0) {
      if (*p1 == '\0') {
        PRNTERR2("End of file encountered while scanning header action block in rule %ld", num_rules);
        exit(12);
      }
      if (*p1++ == '\n') {
        if (bufend == input_buffer + IOBUFFER_SIZE) {
          int i = bufend - p1;
          if (i < MAX_LINE_SIZE) {
            strcpy(input_buffer, p1);
            bufend = &input_buffer[i];
            read_input(grm_file, sysgrm);
            p1 = &input_buffer[0];
          }
        }
        line_no++;
        linestart = p1 - 1;
      }
      ct_length++;
    }
    ct = HBLOCK_TK;
    ct_end_line = line_no;
    ct_end_col = p1 - linestart - 1;
    p2 = p1 + hblocke_len;

    return;
  } else if (strncmp(p1, blockb, blockb_len) == 0) /* check block  */
  {
    p1 = p1 + blockb_len;
    ct_length = 0;
    ct_ptr = p1;
    if (*p1 == '\n') {
      ct_ptr++;
      ct_length--;
      ct_start_line = line_no + 1;
      ct_start_col = 1;
    } else {
      ct_start_line = line_no;
      ct_start_col = p1 - linestart;
    }

    while (strncmp(p1, blocke, blocke_len) != 0) {
      if (*p1 == '\0') {
        PRNTERR2("End of file encountered while scanning action block in rule %ld", num_rules);
        exit(12);
      }
      if (*p1++ == '\n') {
        if (bufend == input_buffer + IOBUFFER_SIZE) {
          int i = bufend - p1;
          if (i < MAX_LINE_SIZE) {
            strcpy(input_buffer, p1);
            bufend = &input_buffer[i];
            read_input(grm_file, sysgrm);
            p1 = &input_buffer[0];
          }
        }
        line_no++;
        linestart = p1 - 1;
      }
      ct_length++;
    }
    ct = BLOCK_TK;
    ct_end_line = line_no;
    ct_end_col = p1 - linestart - 1;
    p2 = p1 + blocke_len;

    return;
  }
  // Scan the next token.
  ct_ptr = p1;
  ct_start_line = line_no;
  ct_start_col = p1 - linestart;
  p2 = p1 + 1;
  switch (*p1) {
    case '<':
      if (IsAlpha(*p2)) {
        p2++;
        while (*p2 != '\n') {
          if (*p2++ == '>') {
            ct = SYMBOL_TK;
            ct_length = p2 - p1;
            goto check_symbol_length;
          }
        }
        int i = SYMBOL_SIZE < p2 - p1 ? SYMBOL_SIZE : p2 - p1;
        memcpy(tok_string, p1, i);
        tok_string[i] = '\0';
        PRNTERR2("Symbol \"%s\" has been referenced in line %ld without the closing \">\"", tok_string, ct_start_line);
        exit(12);
      }
      break;

    case '\'':
      ct_ptr = p2;
      ct = SYMBOL_TK;
      while (*p2 != '\n') {
        if (*p2 == '\'') {
          p2++;
          if (*p2 != '\'') {
            ct_length = p2 - p1 - 2;
            goto remove_quotes;
          }
        }
        p2++;
      }
      ct_length = p2 - p1 - 1;
      memcpy(tok_string, p1, ct_length);
      tok_string[ct_length] = '\0';
      PRNTWNG2("Symbol \"%s\" referenced in line %ld requires a closing quote", tok_string, ct_start_line);

    remove_quotes:
      if (ct_length == 0) /* Empty symbol? disregard it */
        goto scan_token;

      int i = 0;
      p1 = ct_ptr;
      do {
        *p1++ = ct_ptr[i++];
        if (ct_ptr[i] == '\'')
          i++; /* skip next quote */
      } while (i < ct_length);
      ct_length = p1 - ct_ptr;

      goto check_symbol_length;

    case '-': /* scan possible comment  */
      if (*p2 == '-') {
        p2++;
        while (*p2 != '\n')
          p2++;
        goto scan_token;
      } else if (*p2 == '>' && IsSpace(*(p2 + 1))) {
        ct = ARROW_TK;
        ct_length = 2;
        p2++;
        return;
      }
      break;

    case ':':
      if (*p2 == ':' && *(p2 + 1) == '=' && IsSpace(*(p2 + 2))) {
        ct = EQUIVALENCE_TK;
        ct_length = 3;
        p2 = p1 + 3;
        return;
      }
      break;

    case '\0':
    case '\x1a': /* CTRL-Z • END-OF-FILE? */
      ct = EOF_TK;
      ct_length = 0;
      p2 = p1;

      return;

    default:
      if (*p1 == ormark && IsSpace(*p2)) {
        ct = OR_TK;
        ct_length = 1;
        return;
      } else if (*p1 == escape) /* escape character? */
      {
        register char *p3 = p2 + 1;
        switch (*p2) {
          case 't':
          case 'T':
            if (strxeq(p3, "erminals") && IsSpace(*(p1 + 10))) {
              ct = TERMINALS_KEY_TK;
              ct_length = 10;
              p2 = p1 + 10;
              return;
            }
            break;

          case 'd':
          case 'D':
            if (strxeq(p3, "efine") && IsSpace(*(p1 + 7))) {
              ct = DEFINE_KEY_TK;
              ct_length = 7;
              p2 = p1 + 7;
              return;
            }
            break;

          case 'e':
          case 'E':
            if (strxeq(p3, "mpty") && IsSpace(*(p1 + 6))) {
              ct = EMPTY_SYMBOL_TK;
              ct_length = 6;
              p2 = p1 + 6;
              return;
            }
            if (strxeq(p3, "rror") && IsSpace(*(p1 + 6))) {
              ct = ERROR_SYMBOL_TK;
              ct_length = 6;
              p2 = p1 + 6;
              return;
            }
            if (strxeq(p3, "ol") && IsSpace(*(p1 + 4))) {
              ct = EOL_SYMBOL_TK;
              ct_length = 4;
              p2 = p1 + 4;
              return;
            }
            if (strxeq(p3, "of") && IsSpace(*(p1 + 4))) {
              ct = EOF_SYMBOL_TK;
              ct_length = 4;
              p2 = p1 + 4;
              return;
            }
            if (strxeq(p3, "nd") && IsSpace(*(p1 + 4))) {
              ct = END_KEY_TK;
              ct_length = 4;
              p2 = p1 + 4;
              return;
            }
            break;

          case 'r':
          case 'R':
            if (strxeq(p3, "ules") && IsSpace(*(p1 + 6))) {
              ct = RULES_KEY_TK;
              ct_length = 6;
              p2 = p1 + 6;
              return;
            }
            break;

          case 'a':
          case 'A':
            if (strxeq(p3, "lias") && IsSpace(*(p1 + 6))) {
              ct = ALIAS_KEY_TK;
              ct_length = 6;
              p2 = p1 + 6;
              return;
            }
            break;

          case 's':
          case 'S':
            if (strxeq(p3, "tart") && IsSpace(*(p1 + 6))) {
              ct = START_KEY_TK;
              ct_length = 6;
              p2 = p1 + 6;
              return;
            }
            break;

          case 'n':
          case 'N':
            if (strxeq(p3, "ames") && IsSpace(*(p1 + 6))) {
              ct = NAMES_KEY_TK;
              ct_length = 6;
              p2 = p1 + 6;
              return;
            }
            break;

          default:
            break;
        }

        ct = MACRO_NAME_TK;
        while (!IsSpace(*p2))
          p2++;
        ct_length = p2 - p1;
        goto check_symbol_length;
      }
  }
  ct = SYMBOL_TK;
  while (!IsSpace(*p2)) {
    p2++;
  }
  ct_length = p2 - p1;
check_symbol_length:
  if (ct_length > SYMBOL_SIZE) {
    ct_length = SYMBOL_SIZE;
    memcpy(tok_string, p1, ct_length);
    tok_string[ct_length] = '\0';
    if (symbol_image(tok_string) == OMEGA) {
      PRNTWNG2("Length of Symbol \"%s\" in line %d exceeds maximum of ", tok_string, line_no);
    }
  }
}

///  This function allocates a line_elemt structure and returns a pointer
/// to it.
struct line_elemt *alloc_line(void) {
  register struct line_elemt *p = line_pool_root;
  if (p != NULL) {
    line_pool_root = p->link;
  } else {
    talloc0(p, struct line_elemt);
  }
  return p;
}

///  This function frees a line_elemt structure which is returned to a free
/// pool.
void free_line(struct line_elemt *p) {
  p->link = line_pool_root;
  line_pool_root = p;
}

/// FIND_MACRO takes as argument a pointer to a macro name. It searches for
/// the macro name in the hash table based on MACRO_TABLE. If the macro name
/// is found then the macro definition associated with it is returned.
/// If the name is not found, then a message is printed, a new definition is
/// entered to avoid more messages and NULL is returned.
struct line_elemt *find_macro(char *name) {
  register struct line_elemt *root = NULL;
  char macro_name[MAX_LINE_SIZE + 1];
  register char *s = macro_name;
  for (register char *ptr = name; *ptr != '\0'; ptr++) {
    *s++ = isupper(*ptr) ? tolower(*ptr) : *ptr;
  }
  *s = '\0';
  const register int i = hash(macro_name);
  for (register int j = macro_table[i]; j != NIL; j = defelmt[j].next) {
    if (strcmp(macro_name, defelmt[j].name) == 0) {
      register char *ptr = defelmt[j].macro;
      if (ptr) /* undefined macro? */
      {
        while (*ptr != '\0') {
          register struct line_elemt *q = alloc_line();
          s = q->line;
          while (*ptr != '\n')
            *s++ = *ptr++;
          *s = '\0';
          ptr++; /* skip newline marker */
          if (root == NULL) {
            q->link = q; /* make circular */
          } else {
            q->link = root->link;
            root->link = q;
          }
          root = q;
        }
      }
      return root;
    }
  }
  // Make phony definition for macro so as to avoid future
  // errors.
  if (num_defs >= (int) defelmt_size) {
    defelmt_size += DEFELMT_INCREMENT;
    defelmt = (struct defelmt_type *)
    (defelmt == (struct defelmt_type *) NULL
       ? malloc(defelmt_size * sizeof(struct defelmt_type))
       : realloc(defelmt, defelmt_size * sizeof(struct defelmt_type)));
    if (defelmt == (struct defelmt_type *) NULL)
      nospace(__FILE__, __LINE__);
  }
  strcpy(defelmt[num_defs].name, macro_name);
  defelmt[num_defs].length = 0;
  defelmt[num_defs].macro = NULL;
  defelmt[num_defs].next = macro_table[i];
  macro_table[i] = num_defs;
  num_defs++;
  return NULL;
}

/// PROCESS_ACTION_LINE takes as arguments a line of text from an action
/// block and the rule number with which the block is associated.
/// It first scans the text for predefined macro names and then for
/// user defined macro names. If one is found, the macro definition is
/// substituted for the name. The modified action text is then printed out in
/// the action file.
void process_action_line(FILE *sysout, char *text, const int line_no, const int rule_no, char *grm_file) {
  char temp1[MAX_LINE_SIZE + 1];
  char suffix[MAX_LINE_SIZE + 1];
  char symbol[SYMBOL_SIZE + 1];
  const int output_size = 5000;
  struct line_elemt *q;
  struct line_elemt *root = NULL;
  struct line_elemt *input_line_root = NULL;
next_line: {
  }
  register int text_len = strlen(text);
  register int k = 0; /* k is the cursor */
  while (k < text_len) {
    // all macro names begin with the ESCAPE
    if (text[k] == escape) {
      // character
      // 12 is length of %rule_number and
      if (k + 12 <= text_len) {
        // %num_symbols.
        if (strxeq(text + k, krule_number)) {
          strcpy(temp1, text + k + 12);
          if (k + 12 != text_len)
            sprintf(text + k, "%d%s", rule_no, temp1);
          else
            sprintf(text + k, "%d", rule_no);
          goto proceed;
        }
        if (strxeq(text + k, knum_symbols)) {
          strcpy(temp1, text + k + 12);
          if (k + 12 != text_len) {
            sprintf(text + k, "%ld%s", num_symbols, temp1);
          } else {
            sprintf(text + k, "%ld", num_symbols);
          }
          goto proceed;
        }
      }
      // 11 is the length of %input_file
      if (k + 11 <= text_len) {
        if (strxeq(text + k, kinput_file)) {
          strcpy(temp1, text + k + 11);
          if (k + 11 != text_len) {
            sprintf(text + k, "%s%s", grm_file, temp1);
          } else {
            sprintf(text + k, "%s", grm_file);
          }
          goto proceed;
        }
      }
      if (k + 10 <= text_len) /* 10 is the length of %rule_size and */
      // %rule_text, %num_rules and %next_line
      {
        if (strxeq(text + k, krule_text)) {
          char temp2[MAX_LINE_SIZE + 1];
          int jj;
          if (k + 10 != text_len) {
            strcpy(temp1, text + k + 10);
            // Remove trailing blanks
            for (jj = strlen(temp1) - 1; jj >= 0 && temp1[jj] == ' '; jj--) {
            }
            // if not a string of blanks
            if (jj != 0) {
              temp1[++jj] = '\0';
            } else {
              temp1[0] = '\0';
            }
          } else {
            temp1[0] = '\0';
            jj = 0;
          }
          const register int max_len = output_size - k - jj;
          restore_symbol(temp2, RETRIEVE_STRING(rules[rule_no].lhs));
          // if a single production
          if (rules[rule_no].sp) {
            strcat(temp2, " ->");
          } else {
            strcat(temp2, " ::=");
          }
          if (strlen(temp2) > max_len) {
            strcpy(temp2, " ... ");
          } else /* Copy right-hand-side symbols to temp2 */
          {
            for ENTIRE_RHS3(j, rule_no) {
              restore_symbol(symbol, RETRIEVE_STRING(rhs_sym[j]));
              if (strlen(temp2) + strlen(symbol) + 1 < max_len) {
                strcat(temp2, " ");
                strcat(temp2, symbol);
              } else {
                if (strlen(temp2) + 3 < max_len) {
                  strcat(temp2, "...");
                }
                break;
              }
            }
          }
          text[k] = '\0';
          strcat(text, temp2);
          strcat(text, temp1);
          k = k - 1 + strlen(temp2); /* Adjust cursor */
          goto proceed;
        }
        if (strxeq(text + k, krule_size)) {
          strcpy(temp1, text + k + 10);
          if (k + 10 != text_len) {
            sprintf(text + k, "%d%s", RHS_SIZE(rule_no), temp1);
          } else {
            sprintf(text + k, "%d", RHS_SIZE(rule_no));
          }
          goto proceed;
        }
        if (strxeq(text + k, knext_line)) {
          strcpy(temp1, text + k + 10);
          if (k + 10 != text_len) {
            sprintf(text + k, "%d%s", line_no + 1, temp1);
          } else {
            sprintf(text + k, "%d", line_no + 1);
          }
          goto proceed;
        }
        if (strxeq(text + k, knum_rules)) {
          strcpy(temp1, text + k + 10);
          if (k + 10 != text_len) {
            sprintf(text + k, "%ld%s", num_rules, temp1);
          } else {
            sprintf(text + k, "%ld", num_rules);
          }
          goto proceed;
        }
      }
      if (k + 13 <= text_len) /* 13 is length of %current_line  */
      {
        if (strxeq(text + k, kcurrent_line)) {
          strcpy(temp1, text + k + 13);
          if (k + 13 != text_len)
            sprintf(text + k, "%d%s", line_no, temp1);
          else
            sprintf(text + k, "%d", line_no);
          goto proceed;
        }
      }
      if (k + 14 <= text_len) /* 14 is length of %num_terminals */
      {
        if (strxeq(text + k, knum_terminals)) {
          strcpy(temp1, text + k + 14);
          if (k + 14 != text_len)
            sprintf(text + k, "%ld%s", num_terminals, temp1);
          else
            sprintf(text + k, "%ld", num_terminals);
          goto proceed;
        }
      }
      if (k + 18 <= text_len) /* 18 is length of %num_non_terminals */
      {
        if (strxeq(text + k, knum_non_terminals)) {
          strcpy(temp1, text + k + 18);
          if (k + 18 != text_len) {
            sprintf(text + k, "%ld%s", num_non_terminals, temp1);
          } else {
            sprintf(text + k, "%ld", num_non_terminals);
          }
          goto proceed;
        }
      }
      // Macro in question is not one of the predefined macros. Try user-defined
      // macro list.
      // find next delimeter
      int jj;
      for (jj = k + 1; jj < text_len && !IsSpace(text[jj]); ++jj) {
      }
      memcpy(symbol, text + k, jj - k); /* copy macro name into symbol */
      symbol[jj - k] = '\0';
      // Is there any text after macro ?
      if (jj < text_len) {
        strcpy(suffix, text + jj); /* Copy rest of text into "suffix". */
      } else {
        suffix[0] = '\0';
      }
      text[k] = '\0'; /* prefix before macro */
      root = find_macro(symbol); /* "root" points to a circular  */
      // linked list of line_elemt(s)
      // containing macro definition.
      if (root != NULL) /* if macro name was found */
      {
        struct line_elemt *tail;
        q = root;
        root = root->link;
        if (suffix[0] != '\0') {
          // If there is room to add the suffix to the
          // last macro line then do it. Or else
          // allocate a new line_elemt, copy the suffix
          // into it and add it to the list of lines to
          // be processed.
          if (strlen(q->line) + strlen(suffix) < output_size) {
            strcat(q -> line, suffix);
            tail = q;
          } else {
            tail = alloc_line();
            strcpy(tail -> line, suffix);
            q->link = tail;
          }
        } else {
          tail = q;
        }
        tail->link = NULL; /* make circular list linear */
        // If there is space for the first macro line to be
        // added to the prefix then do it.
        if (strlen(text) + strlen(root->line) < output_size) {
          strcat(text, root -> line);
          q = root;
          root = root->link;
          free_line(q);
        }
        // if there are more macro lines to process,
        // add list to list headed by INPUT_LINE_ROOT
        if (root != NULL) {
          tail->link = input_line_root;
          input_line_root = root;
        }
        k--;
      } else /* If macro name is not found then rebuild line.*/
      {
        strcat(text, symbol);
        if (suffix[0] != '\0')
          strcat(text, suffix);
        k = jj;
      }
    proceed:
      text_len = strlen(text);
    }
    ++k;
  }
  // If text is greater than output size, print error message and truncate
  // line.
  const unsigned long l = strlen(text);
  if (l > output_size) {
    for (int j = l - 1; j >= output_size; j--) {
      if (text[j] != ' ') {
        PRNTERR2("Size of output line \"%s\" is greater than OUTPUT_SIZE (%d), it was %lu", text, output_size, strlen(text));
        break;
      }
    }
    text[output_size] = '\0';
  }
  fprintf(sysout, "%s\n", text);
  // If there is another macro line copy it to TEXT and then process it.
  if (input_line_root != NULL) {
    strcpy(text, input_line_root -> line);
    q = input_line_root;
    input_line_root = input_line_root->link;
    free_line(q);
    goto next_line;
  }
}

/// This procedure takes as argument a macro definition.  If the name of the
/// macro is one of the predefined names, it issues an error.  Otherwise, it
/// inserts the macro definition into the table headed by MACRO_TABLE.
void mapmacro(const int def_index) {
  if (strcmp(defelmt[def_index].name, krule_text) == 0 ||
      strcmp(defelmt[def_index].name, krule_number) == 0 ||
      strcmp(defelmt[def_index].name, knum_rules) == 0 ||
      strcmp(defelmt[def_index].name, krule_size) == 0 ||
      strcmp(defelmt[def_index].name, knum_terminals) == 0 ||
      strcmp(defelmt[def_index].name, knum_non_terminals) == 0 ||
      strcmp(defelmt[def_index].name, knum_symbols) == 0 ||
      strcmp(defelmt[def_index].name, kinput_file) == 0 ||
      strcmp(defelmt[def_index].name, kcurrent_line) == 0 ||
      strcmp(defelmt[def_index].name, knext_line) == 0) {
    PRNTWNG2("predefined macro \"%s\" cannot be redefined. Line %ld", defelmt[def_index].name, defelmt[def_index].start_line);
  } else {
    const register int i = hash(defelmt[def_index].name);
    for (register int j = macro_table[i]; j != NIL; j = defelmt[j].next) {
      if (strcmp(defelmt[j].name, defelmt[def_index].name) == 0) {
        PRNTWNG2("Redefinition of macro \"%s\" in line %ld", defelmt[def_index].name, defelmt[def_index].start_line);
        break;
      }
    }
    defelmt[def_index].next = macro_table[i];
    macro_table[i] = def_index;
  }
}

struct hash_type *alias_root = NULL;

const char *EXTRACT_STRING(const int indx) {
  return &string_table[indx];
}

/// If a listing is requested, this prints all the macros(if any), followed
/// by the aliases(if any), followed by the terminal symbols, followed by the
/// rules.
/// This grammar information is printed on lines no longer than
/// PRINT_LINE_SIZE characters long.  If all the symbols in a rule cannot fit
/// on one line, it is continued on a subsequent line beginning at the
/// position after the equivalence symbol (::= or ->) or the middle of the
/// print_line, whichever is smaller.  If a symbol cannot fit on a line
/// beginning at the proper offset, it is laid out on successive lines,
/// beginning at the proper offset.
void display_input(void) {
  char line[PRINT_LINE_SIZE + 1];
  char temp[SYMBOL_SIZE + 1];
  // Print the Macro definitions, if any.
  if (num_defs > 0) {
    fprintf(syslis, "\nDefined Symbols:\n\n");
    for (int j = 0; j < num_defs; j++) {
      fill_in(line, PRINT_LINE_SIZE - (strlen(blockb) + 1), '-');
      fprintf(syslis, "\n\n%s\n%s%s\n", defelmt[j].name, blockb, line);
      for (const char *ptr = defelmt[j].macro; *ptr != '\0'; ptr++) {
        for (; *ptr != '\n'; ptr++) {
          putc(*ptr, syslis);
        }
        putc(*ptr, syslis);
      }
      fill_in(line, PRINT_LINE_SIZE - (strlen(blocke) + 1), '-');
      fprintf(syslis, "%s%s\n", blocke, line);
    }
  }
  register int offset;
  //   Print the Aliases, if any.
  if (alias_root != NULL) {
    if (alias_root->link == NULL) {
      fprintf(syslis, "\nAlias:\n\n");
    } else {
      fprintf(syslis, "\nAliases:\n\n");
    }
    for (const struct hash_type *p = alias_root; p != NULL; p = p->link) {
      restore_symbol(temp, EXTRACT_STRING(p->st_ptr));
      int len = PRINT_LINE_SIZE - 5;
      print_large_token(line, temp, "", len);
      strcat(line, " ::= ");
      int symb = -p->number;
      restore_symbol(temp, RETRIEVE_STRING(symb));
      if (strlen(line) + strlen(temp) > PRINT_LINE_SIZE) {
        fprintf(syslis, "%s\n", line);
        len = PRINT_LINE_SIZE - 4;
        print_large_token(line, temp, "    ", len);
      } else {
        strcat(line, temp);
      }
      fprintf(syslis, "%s\n", line);
    }
  }
  //   Print the terminals.
  //   The first symbol (#1) represents the empty string.  The last terminal
  // declared by the user is followed by EOFT which may be followed by the
  // ERROR symbol.  See LPG GRAMMAR for more details.
  fprintf(syslis, "\nTerminals:\n\n");
  strcpy(line, "        "); /* 8 spaces */
  int len = PRINT_LINE_SIZE - 4;
  for (int symb = 2; symb <= num_terminals; symb++) {
    restore_symbol(temp, RETRIEVE_STRING(symb));
    if (strlen(line) + strlen(temp) > PRINT_LINE_SIZE) {
      fprintf(syslis, "\n%s", line);
      print_large_token(line, temp, "    ", len);
    } else {
      strcat(line, temp);
    }
    if (strlen(line) < PRINT_LINE_SIZE) {
      strcat(line, " ");
    }
  }
  fprintf(syslis, "\n%s", line);
  //    Print the Rules
  fprintf(syslis, "\nRules:\n\n");
  for (register int rule_no = 0; rule_no <= num_rules; rule_no++) {
    int symb = rules[rule_no].lhs;
    sprintf(line, "%-4d  ", rule_no);
    if (symb != OMEGA) {
      restore_symbol(temp, RETRIEVE_STRING(symb));
      if (strlen(temp) > PRINT_LINE_SIZE - 12) {
        strncat(line, temp, PRINT_LINE_SIZE - 12);
        fprintf(syslis, "\n%s", line);
        memmove(temp, temp + (PRINT_LINE_SIZE - 12), sizeof(temp) - (PRINT_LINE_SIZE - 12));
        print_large_token(line, temp, "       ", PRINT_LINE_SIZE - 12);
      } else {
        strcat(line, temp);
      }
      if (rules[rule_no].sp) {
        strcat(line, " -> ");
      } else {
        strcat(line, " ::= ");
      }
      offset = MIN(strlen(line) - 1, PRINT_LINE_SIZE / 2 + 1);
      len = PRINT_LINE_SIZE - offset - 1;
    } else {
      symb = rules[rule_no - 1].lhs;
      rules[rule_no].lhs = symb; /* update rules map */
      if (rules[rule_no].sp) {
        restore_symbol(temp, RETRIEVE_STRING(symb));
        if (strlen(temp) > PRINT_LINE_SIZE - 12) {
          strncat(line, temp, PRINT_LINE_SIZE - 12);
          fprintf(syslis, "\n%s", line);
          memmove(temp, temp + (PRINT_LINE_SIZE - 12), sizeof(temp) - (PRINT_LINE_SIZE - 12));
          print_large_token(line, temp, "       ", PRINT_LINE_SIZE - 12);
        } else {
          strcat(line, temp);
        }
        strcat(line, "  -> ");
      } else {
        for (int i = 1; i <= offset - 7; i++) {
          strcat(line, " ");
        }
        strcat(line, "| ");
      }
    }
    for ENTIRE_RHS3(i, rule_no) {
      restore_symbol(temp, RETRIEVE_STRING(rhs_sym[i]));
      if (strlen(temp) + strlen(line) > PRINT_LINE_SIZE - 1) {
        char tempbuffer1[SYMBOL_SIZE + 1];
        fprintf(syslis, "\n%s", line);
        strcpy(tempbuffer1, " ");
        for (int j = 1; j < offset + 1; j++) {
          strcat(tempbuffer1, " ");
        }
        print_large_token(line, temp, tempbuffer1, len);
      } else {
        strcat(line, temp);
      }
      strcat(line, " ");
    }
    fprintf(syslis, "\n%s", line);
  }
}

///     Process all semantic actions and generate action file.
void process_actions(char *grm_file, struct CLIOptions *cli_options) {
  register char *p;
  char line[MAX_LINE_SIZE + 1];
  FILE *sysact = fopen(cli_options->act_file, "w");
  FILE *syshact = fopen(cli_options->hact_file, "w");
  if (sysact == (FILE *) NULL) {
    fprintf(stderr, "***ERROR: Action file \"%s\" cannot be opened.\n", cli_options->act_file);
    exit(12);
  }
  if (syshact == (FILE *) NULL) {
    fprintf(stderr, "***ERROR: Header Action file \"%s\" cannot be opened.\n", cli_options->hact_file);
    exit(12);
  }
  // TODO • make this a local?
  FILE *sysgrm;
  if ((sysgrm = fopen(grm_file, "r")) == (FILE *) NULL) {
    fprintf(stderr, "***ERROR: Input file %s containing grammar is empty, undefined, or invalid\n", grm_file);
    exit(12);
  }
  macro_table = Allocate_short_array(HT_SIZE);
  for (int i = 0; i < HT_SIZE; i++) {
    macro_table[i] = NIL;
  }
  bufend = &input_buffer[0];
  read_input(grm_file, sysgrm);
  p2 = &input_buffer[0];
  linestart = p2 - 1;
  p1 = p2;
  line_no = 1;
  // Read in all the macro definitions and insert them into macro_table.
  for (int i = 0; i < num_defs; i++) {
    calloc0(defelmt[i].macro, defelmt[i].length + 2, char);
    for (; line_no < defelmt[i].start_line; line_no++) {
      while (*p1 != '\n') {
        p1++;
      }
      p1++;
      if (bufend == input_buffer + IOBUFFER_SIZE) {
        int k = bufend - p1;
        if (k < MAX_LINE_SIZE) {
          strcpy(input_buffer, p1);
          bufend = &input_buffer[k];
          read_input(grm_file, sysgrm);
          p1 = &input_buffer[0];
        }
      }
      linestart = p1 - 1;
    }
    p1 = linestart + defelmt[i].start_column;
    for (register int j = 0; j < defelmt[i].length; j++) {
      defelmt[i].macro[j] = *p1;
      if (*(p1++) == '\n') {
        if (bufend == input_buffer + IOBUFFER_SIZE) {
          int k = bufend - p1;
          if (k < MAX_LINE_SIZE) {
            strcpy(input_buffer, p1);
            bufend = &input_buffer[k];
            read_input(grm_file, sysgrm);
            p1 = &input_buffer[0];
          }
        }
        line_no++;
        linestart = p1 - 1;
      }
    }
    defelmt[i].macro[defelmt[i].length] = '\n';
    defelmt[i].macro[defelmt[i].length + 1] = '\0';
    for (p = defelmt[i].name; *p != '\0'; p++) {
      *p = isupper(*p) ? tolower(*p) : *p;
    }
    mapmacro(i);
  }
  // If LISTING was requested, invoke listing procedure.
  if (cli_options->list_bit) {
    display_input();
  }
  // Read in all the action blocks and process them.
  for (int i = 0; i < num_acts; i++) {
    for (; line_no < actelmt[i].start_line; line_no++) {
      while (*p1 != '\n') {
        p1++;
      }
      p1++;
      if (bufend == input_buffer + IOBUFFER_SIZE) {
        int k = bufend - p1;
        if (k < MAX_LINE_SIZE) {
          strcpy(input_buffer, p1);
          bufend = &input_buffer[k];
          read_input(grm_file, sysgrm);
          p1 = &input_buffer[0];
        }
      }
      linestart = p1 - 1;
    }
    if (actelmt[i].start_line == actelmt[i].end_line) {
      int len = actelmt[i].end_column - actelmt[i].start_column + 1;
      memcpy(line, linestart + actelmt[i].start_column, len);
      line[len] = '\0';
      while (*p1 != '\n') {
        p1++;
      }
    } else {
      p = line;
      p1 = linestart + actelmt[i].start_column;
      while (*p1 != '\n') {
        *(p++) = *(p1++);
      }
      *p = '\0';
    }
    if (actelmt[i].header_block) {
      process_action_line(syshact, line, line_no, actelmt[i].rule_number, grm_file);
    } else {
      process_action_line(sysact, line, line_no, actelmt[i].rule_number, grm_file);
    }
    if (line_no != actelmt[i].end_line) {
      while (line_no < actelmt[i].end_line) {
        p1++;
        if (bufend == input_buffer + IOBUFFER_SIZE) {
          int k = bufend - p1;
          if (k < MAX_LINE_SIZE) {
            strcpy(input_buffer, p1);
            bufend = &input_buffer[k];
            read_input(grm_file, sysgrm);
            p1 = &input_buffer[0];
          }
        }
        line_no++;
        linestart = p1 - 1;
        if (line_no < actelmt[i].end_line) {
          p = line;
          while (*p1 != '\n') {
            *(p++) = *(p1++);
          }
          *p = '\0';
          if (actelmt[i].header_block) {
            process_action_line(syshact, line, line_no, actelmt[i].rule_number, grm_file);
          } else {
            process_action_line(sysact, line, line_no, actelmt[i].rule_number, grm_file);
          }
        }
      }
      if (actelmt[i].end_column != 0) {
        int len = actelmt[i].end_column;
        memcpy(line, p1, len);
        line[len] = '\0';
        if (actelmt[i].header_block) {
          process_action_line(syshact, line, line_no, actelmt[i].rule_number, grm_file);
        } else {
          process_action_line(sysact, line, line_no, actelmt[i].rule_number, grm_file);
        }
      }
    }
  }
  for (int i = 0; i < num_defs; i++) {
    ffree(defelmt[i].macro);
  }
  ffree(defelmt);
  ffree(actelmt);
  fclose(sysgrm); /* Close grammar file and reopen it. */
  fclose(sysact);
  fclose(syshact);
}

///          Actions to be taken if grammar is successfully parsed.
void accept_action(char *grm_file, struct CLIOptions *cli_options, FILE *sysgrm) {
  if (rulehdr == NULL) {
    printf("Informative: Empty grammar read in. Processing stopped.\n");
    fprintf(syslis, "***Informative: Empty grammar read in. Processing stopped.\n");
    fclose(sysgrm);
    fclose(syslis);
    exit(12);
  }
  num_non_terminals = num_symbols - num_terminals;
  if (error_maps_bit) {
    // make_names_map
    {
      // Construct the NAME map, and update the elements of SYMNO with their names.
      symno[accept_image].name_index = name_map("");
      if (error_image == DEFAULT_SYMBOL) {
        symno[DEFAULT_SYMBOL].name_index = symno[accept_image].name_index;
      }
      for ALL_TERMINALS3(symbol) {
        if (symno[symbol].name_index == OMEGA) {
          symno[symbol].name_index = name_map(RETRIEVE_STRING(symbol));
        }
      }
      for ALL_NON_TERMINALS3(symbol) {
        if (symno[symbol].name_index == OMEGA) {
          if (cli_options->names_opt == MAXIMUM_NAMES) {
            symno[symbol].name_index = name_map(RETRIEVE_STRING(symbol));
          } else if (cli_options->names_opt == OPTIMIZE_PHRASES) {
            symno[symbol].name_index = -name_map(RETRIEVE_STRING(symbol));
          } else {
            symno[symbol].name_index = symno[error_image].name_index;
          }
        }
      }
      calloc0(name, num_names + 1, int);
      for (register int i = 0; i < HT_SIZE; i++) {
        for (const register struct hash_type *p = hash_table[i]; p != NULL; p = p->link) {
          if (p->name_index != OMEGA) {
            name[p->name_index] = p->st_ptr;
          }
        }
      }
    }
  }
  if (cli_options->list_bit) {
    // Aliases are placed in a separate linked list.  NOTE!! After execution
    // of this loop the hash_table is destroyed because the LINK field of
    // alias symbols is used to construct a list of the alias symbols.
    for (register int i = 0; i < HT_SIZE; i++) {
      register struct hash_type *tail = hash_table[i];
      for (register struct hash_type *p = tail; p != NULL; p = tail) {
        tail = p->link;
        if (p->number < 0) {
          p->link = alias_root;
          alias_root = p;
        }
      }
    }
  }
  // Construct the rule table.  At this stage, NUM_ITEMS is equal to the sum
  // of the right-hand side lists of symbols.  It is used in the declaration of
  // RULE_TAB.  After RULE_TAB is allocated, we increase NUM_ITEMS to its
  // correct value.  Recall that the first rule is numbered 0; therefore we
  // increase the number of items by 1 to reflect this numbering.
  {
    register struct node *ptr;
    register int rhs_ct = 0;
    calloc0(rules, num_rules + 2, struct ruletab_type);
    rhs_sym = Allocate_short_array(num_items + 1);
    num_items += num_rules + 1;
    SHORT_CHECK(num_items);
    register int ii = 0;
    // Put starting rules from start symbol linked list in rule and rhs table
    if (start_symbol_root != NULL) {
      // Turn circular list into linear
      register struct node *q = start_symbol_root;
      start_symbol_root = q->next;
      q->next = NULL;
      for (ptr = start_symbol_root; ptr != NULL; ptr = ptr->next) {
        rules[ii].lhs = accept_image;
        rules[ii].sp = 0;
        rules[ii++].rhs = rhs_ct;
        if (ptr->value != empty) {
          rhs_sym[rhs_ct++] = ptr->value;
        }
      }
      free_nodes(start_symbol_root, q);
    }
    //   In this loop, the grammar is placed in the rule table structure and the
    // right-hand sides are placed in the RHS table.  A check is made to prevent
    // terminals from being used as left hand sides.
    for (ii = ii; ii <= num_rules; ii++) {
      rules[ii].rhs = rhs_ct;
      ptr = rulehdr[ii].rhs_root;
      if (ptr != NULL) {
        // not am empty right-hand side?
        do {
          ptr = ptr->next;
          rhs_sym[rhs_ct++] = ptr->value;
        } while (ptr != rulehdr[ii].rhs_root);
        ptr = ptr->next; /* point to 1st element */
        rules[ii].sp = rulehdr[ii].sp && ptr == rulehdr[ii].rhs_root;
        if (rules[ii].sp)
          num_single_productions++;
        free_nodes(ptr, rulehdr[ii].rhs_root);
      } else {
        rules[ii].sp = false;
      }
      if (rulehdr[ii].lhs == OMEGA) {
        if (cli_options->list_bit) {
          // Proper LHS will be updated after printing
          rules[ii].lhs = OMEGA;
        } else {
          rules[ii].lhs = rules[ii - 1].lhs;
        }
      } else if (IS_A_TERMINAL(rulehdr[ii].lhs)) {
        char temp[SYMBOL_SIZE + 1];
        restore_symbol(temp, RETRIEVE_STRING(rulehdr[ii].lhs));
        PRNTERR2("In rule %d: terminal \"%s\" used as left hand side", ii, temp);
        PRNTERR("Processing terminated due to input errors.");
        exit(12);
      } else rules[ii].lhs = rulehdr[ii].lhs;
    }
    rules[num_rules + 1].rhs = rhs_ct; /* Fence !! */
  }
  fclose(sysgrm); /* Close grammar input file. */
  process_actions(grm_file, cli_options);
  if (cli_options->list_bit) {
    display_input();
  }
}

/// This procedure opens all relevant files and processes the input grammar.
void process_input(char *grm_file, char *lis_file, struct OutputFiles *output_files, const int argc, char *argv[], char *file_prefix, struct CLIOptions *cli_options) {
  // Parse args.
  {
    // If options are passed to the program, copy them into "parm".
    if (argc > 2) {
      int j = 0;
      parm[0] = '\0';
      while (j < argc - 2) {
        if (*argv[++j] == '-') {
          strcat(parm, argv[j]+1);
        } else {
          strcat(parm, argv[j]);
          printf("***WARNING: Option \"%s\" is missing preceding '-'.\n", argv[j]);
        }
        strcat(parm, " ");
      }
    }
  }

  FILE *sysgrm;
  // Prepare.
  {
    // Open input grammar file. If the file cannot be opened and that file name
    // did not have an extension, then the extension ".g" is added to the file
    // name and we try again. If no file can be found an error message is
    // issued and the program halts.
    if ((sysgrm = fopen(grm_file, "r")) == (FILE *) NULL) {
      register int ii;
      for (ii = strlen(grm_file); ii > 0 && grm_file[ii] != '.' && grm_file[ii] != '/' && /* Unix */ grm_file[ii] != '\\'; /* Dos  */ ii--) {
      }
      if (grm_file[ii] != '.') {
        strcat(grm_file, ".g");
        if ((sysgrm = fopen(grm_file, "r")) == (FILE *) NULL) {
          fprintf(stderr, "***ERROR: Input file %s containing grammar is empty, undefined, or invalid\n", grm_file);
          exit(12);
        }
      } else {
        fprintf(stderr, "***ERROR: Input file %s containing grammar is empty, undefined, or invalid\n", grm_file);
        exit(12);
      }
    } else {
      if (strrchr(grm_file, '.') == NULL) {
        PRNTWNG2("A file named \"%s\" with no extension is being opened", grm_file);
      }
    }
    //                Open listing file for output.
    syslis = fopen(lis_file, "w");
    if (syslis == (FILE *) NULL) {
      fprintf(stderr, "***ERROR: Listing file \"%s\" cannot be openned.\n", lis_file);
      exit(12);
    }
    // Complete the initialization of the code array used to replace the
    // builtin functions isalpha, isdigit and isspace.
    for (unsigned c = 'a'; c <= 'z'; c++) {
      if (isalpha(c)) {
        code[c] = ALPHA_CODE;
      }
    }
    for (unsigned c = 'A'; c <= 'Z'; c++) {
      if (isalpha(c)) {
        code[c] = ALPHA_CODE;
      }
    }
    for (unsigned c = '0'; c <= '9'; c++) {
      if (isdigit(c)) {
        code[c] = DIGIT_CODE;
      }
    }
    code[' '] = SPACE_CODE;
    code['\n'] = SPACE_CODE;
    code['\t'] = SPACE_CODE;
    code['\r'] = SPACE_CODE;
    code['\v'] = SPACE_CODE;
    code['\f'] = SPACE_CODE;
  }

  // Init grammar.
  {
    // This routine is invoked to allocate space for the global structures
    // needed to process the input grammar.
    //
    // Set up a pool of temporary space.
    reset_temporary_space();
    calloc0(terminal, STACK_SIZE, struct terminal_type);
    calloc0(hash_table, HT_SIZE, struct hash_type *);
    // Allocate space for input buffer and read in initial data in input
    // file. Next, invoke PROCESS_OPTION_LINES to process all lines in
    // input file that are options line.
    calloc0(input_buffer, IOBUFFER_SIZE + 1 + MAX_LINE_SIZE, char);
    bufend = &input_buffer[0];
    read_input(grm_file, sysgrm);
    p2 = &input_buffer[0];
    linestart = p2 - 1;
    p1 = p2;
    line_no++;
    if (*p2 == '\0') {
      fprintf(stderr, "Input file \"%s\" containing grammar is empty, undefined, or invalid\n", grm_file);
      exit(12);
    }
    process_options_lines(grm_file, output_files, file_prefix, cli_options, sysgrm);
    eolt_image = OMEGA;
    blockb_len = strlen(blockb);
    blocke_len = strlen(blocke);
    hblockb_len = strlen(hblockb);
    hblocke_len = strlen(hblocke);
    // Keywords, Reserved symbols, and predefined macros
    kdefine[0] = escape; /*Set empty first space to the default */
    kterminals[0] = escape; /* escape symbol.                      */
    kalias[0] = escape;
    kstart[0] = escape;
    krules[0] = escape;
    knames[0] = escape;
    kend[0] = escape;
    krule_number[0] = escape;
    krule_text[0] = escape;
    krule_size[0] = escape;
    knum_rules[0] = escape;
    knum_terminals[0] = escape;
    knum_non_terminals[0] = escape;
    knum_symbols[0] = escape;
    kinput_file[0] = escape;
    kcurrent_line[0] = escape;
    knext_line[0] = escape;
    kstart_nt[0] = escape;
    keolt[0] = escape;
  }

  // Process grammar.
  {
    //    PROCESS_GRAMMAR is invoked to process the source input. It uses an
    // LALR(1) parser table generated by LPG to recognize the grammar which it
    // places in the rulehdr structure.
    short state_stack[STACK_SIZE];
    scanner(grm_file, sysgrm); /* Get first token */
    register int act = START_STATE;
  process_terminal:
    // Note that this driver assumes that the tables are LPG SPACE
    // tables with no GOTO-DEFAULTS.
    state_stack[++stack_top] = act;
    act = t_action(act, ct, ?);
    // Reduce
    if (act <= NUM_RULES) {
      stack_top--;
    } else if (act > ERROR_ACTION || /* Shift_reduce */
               act < ACCEPT_ACTION) /* Shift */
    {
      // token_action
      {
        {
          //    This function, TOKEN_ACTION, pushes the current token onto the
          // parse stack called TERMINAL. Note that in case of a BLOCK_, the name of
          // the token is not copied since blocks are processed separately on a
          // second pass.
          const register int top = stack_top + 1;
          terminal[top].kind = ct;
          terminal[top].start_line = ct_start_line;
          terminal[top].start_column = ct_start_col;
          terminal[top].end_line = ct_end_line;
          terminal[top].end_column = ct_end_col;
          terminal[top].length = ct_length;
          if (ct != BLOCK_TK) {
            memcpy(terminal[top].name, ct_ptr, ct_length);
            terminal[top].name[ct_length] = '\0';
          } else {
            terminal[top].name[0] = '\0';
          }
        }
      }
      scanner(grm_file, sysgrm);
      if (act < ACCEPT_ACTION) {
        goto process_terminal;
      }
      act -= ERROR_ACTION;
    } else if (act == ACCEPT_ACTION) {
      accept_action(grm_file, cli_options, sysgrm);
      goto end;
    } else {
      // error_action
      {
        // Error messages to be printed if an error is encountered during parsing.
        ct_ptr[ct_length] = '\0';
        if (ct == EOF_TK) {
          PRNTERR2("End-of file reached prematurely");
        } else if (ct == MACRO_NAME_TK) {
          PRNTERR2("Misplaced macro name \"%s\" found in line %d, column %d", ct_ptr, line_no, ct_start_col);
        } else if (ct == SYMBOL_TK) {
          char tok_string[SYMBOL_SIZE + 1];
          restore_symbol(tok_string, ct_ptr);
          PRNTERR2("Misplaced symbol \"%s\" found in line %d, column %d", tok_string, line_no, ct_start_col);
        } else {
          PRNTERR2("Misplaced keyword \"%s\" found in line %d, column %d", ct_ptr, line_no, ct_start_col);
        }
        exit(12);
      }
    }
  process_non_terminal:
    do {
      const register int lhs_sym = lhs[act]; /* to bypass IBMC12 bug */
      stack_top -= rhs[act] - 1;
      rule_action[act]();
      act = nt_action(state_stack[stack_top], lhs_sym);
    } while (act <= NUM_RULES);
    goto process_terminal;
  }

end: {
  }

  // Exit grammar.
  {
    // This routine is invoked to free all space used to process the input that
    // is no longer needed. Note that for the string_table, only the unused
    // space is released.
    if (string_offset > 0) {
      string_table = (char *)
      (string_table == (char *) NULL
         ? malloc(string_offset * sizeof(char))
         : realloc(string_table, string_offset * sizeof(char)));
      if (string_table == (char *) NULL)
        nospace(__FILE__, __LINE__);
    }
    ffree(terminal);
    ffree(hash_table);
    ffree(input_buffer);
    ffree(rulehdr); /* allocated in action LPGACT when grammar is not empty */
  }
}
