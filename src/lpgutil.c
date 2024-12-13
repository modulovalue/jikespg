#include <stdlib.h>
#include <string.h>
#include "common.h"

struct TemporarySpace {
  cell **temp_base;
  long temp_top;
  long temp_size;
  long temp_base_size;
} ts = (struct TemporarySpace) {
  .temp_base = NULL,
  .temp_top = 0,
  .temp_size = 0,
  .temp_base_size = 0,
};

struct GlobalSpace gs = (struct GlobalSpace) {
  .global_base = NULL,
  .global_top = 0,
  .global_size = 0,
  .global_base_size = 0,
  .node_pool = NULL,
};

/// The following are global variables and constants used to manage a
/// pool of temporary space. Externally, the user invokes the function
/// "talloc" just as he would invoke "malloc".
const int LOG_BLKSIZE = 14;
const int BLKSIZE = 1 << LOG_BLKSIZE;

/// This procedure obtains more TEMPORARY space.
bool allocate_more_space(cell ***base, long *size, long *base_size) {
  // The variable size always indicates the maximum number of cells
  // that has been allocated and reserved for the storage pool.
  // Initially, size should be set to 0 to indicate that no space has
  // yet been allocated. The pool of cells available is divided into
  // segments of size 2**LOG_BLKSIZE each and each segment is pointer
  // to by a slot in the array base.
  //
  // By dividing "size" by the size of the segment we obtain the
  // index for the next segment in base. If base is already full, it is
  // reallocated.
  //
  const long k = *size >> LOG_BLKSIZE; /* which segment? */
  if (k == *base_size) {
    const int BASE_INCREMENT = 64;
    // base overflow? reallocate
    *base_size += BASE_INCREMENT;
    *base = (cell **) (*base == NULL ? malloc(sizeof(cell *) * *base_size) : realloc(*base, sizeof(cell *) * *base_size));
    if (*base == (cell **) NULL) {
      return false;
    }
    for (long i = *base_size; i < *base_size; i++) {
      (*base)[i] = NULL;
    }
  }
  // If the Ast slot "k" does not already contain a segment, We try to
  // allocate one and place its address in (*base)[k].
  // If the allocation was not successful, we terminate;
  // otherwise, we adjust the address in (*base)[k] so as to allow us
  // to index the segment directly, instead of having to perform a
  // subtraction for each reference. Finally, we update size.
  //
  // Finally, we set the block to zeros.
  if ((*base)[k] == NULL) {
    (*base)[k] = (cell *) malloc(sizeof(cell) << LOG_BLKSIZE);
    if ((*base)[k] == (cell *) NULL) {
      return false;
    }
    (*base)[k] -= *size;
  }
  memset((void *)((*base)[k] + (*size)), 0, sizeof(cell) << LOG_BLKSIZE);
  *size += BLKSIZE;
  return true;
}

/// This procedure resets the temporary space already allocated so
/// that it can be reused before new blocks are allocated.
void reset_temporary_space(void) {
  ts.temp_top = 0; /* index of next usable elemt */
  ts.temp_size = 0;
}

/// This procedure frees all allocated temporary space.
void free_temporary_space(void) {
  for (int k = 0; k < ts.temp_base_size && ts.temp_base[k] != NULL; k++) {
    ts.temp_base[k] += k * BLKSIZE;
    ffree(ts.temp_base[k]);
  }
  if (ts.temp_base != NULL) {
    ffree(ts.temp_base);
    ts.temp_base = NULL;
  }
  ts.temp_base_size = 0;
  ts.temp_top = 0;
  ts.temp_size = 0;
}

/// talloc allocates an object of size "size" in temporary space and
/// returns a pointer to it.
void *talloc(const long size) {
  long i = ts.temp_top;
  ts.temp_top += (size + sizeof(cell) - 1) / sizeof(cell);
  if (ts.temp_top > ts.temp_size) {
    i = ts.temp_size;
    ts.temp_top = ts.temp_size + (size + sizeof(cell) - 1) / sizeof(cell);
    if (!allocate_more_space(&ts.temp_base, &ts.temp_size, &ts.temp_base_size)) {
      ts.temp_top = ts.temp_size;
      return NULL;
    }
  }
  return &ts.temp_base[i >> LOG_BLKSIZE][i];
}

/// galloc allocates an object of size "size" in global space and
/// returns a pointer to it. It is analogous to "talloc", but it
/// is a local (static) routine that is only invoked in this file by
/// other more specialized routines.
void *galloc(const long size) {
  long i = gs.global_top;
  gs.global_top += (size + sizeof(cell) - 1) / sizeof(cell);
  if (gs.global_top > gs.global_size) {
    {
      // This function is invoked when the space left in a segment is not
      // enough for GALLOC to allocate a requested object. Rather than
      // waste the space, as many NODE structures as possible are allocated
      // in that space and stacked up in the NODE_POOL list.
      int top = i;
      while (true) {
        const long i = top;
        top += (sizeof(struct node) + sizeof(cell) - 1) / sizeof(cell);
        if (top > gs.global_size) {
          break;
        }
        struct node *p = (struct node *) &gs.global_base[i >> LOG_BLKSIZE][i];
        p->next = gs.node_pool;
        gs.node_pool = p;
      }
    }
    i = gs.global_size;
    gs.global_top = gs.global_size + (size + sizeof(cell) - 1) / sizeof(cell);
    if (!allocate_more_space(&gs.global_base, &gs.global_size, &gs.global_base_size)) {
      gs.global_top = gs.global_size;
      return NULL;
    }
  }
  return &gs.global_base[i >> LOG_BLKSIZE][i];
}

///  This function frees a linked list of nodes by adding them to the free
/// list.  Head points to head of linked list and tail to the end.
void free_nodes(struct node *head, struct node *tail) {
  tail->next = gs.node_pool;
  gs.node_pool = head;
}

/// FILL_IN is a subroutine that pads a buffer, STRING,  with CHARACTER a
/// certain AMOUNT of times.
void fill_in(char string[], const int amount, const char character) {
  int ii;
  for (ii = 0; ii <= amount; ii++) {
    string[ii] = character;
  }
  string[ii] = '\0';
}

/// QCKSRT is a quicksort algorithm that takes as arguments an array of
/// integers, two numbers L and H that indicate the lower and upper bound
/// positions in ARRAY to be sorted.
static void qcksrt(ArrayShort array, const int l, const int h) {
  int lostack[14];
  int histack[14];
  // 2 ** 15 - 1 elements
  int top = 1;
  lostack[top] = l;
  histack[top] = h;
  while (top != 0) {
    int lower = lostack[top];
    int upper = histack[top--];
    while (upper > lower) {
      int i = lower;
      const int pivot = array.raw[lower];
      for (int j = lower + 1; j <= upper; j++) {
        if (array.raw[j] < pivot) {
          array.raw[i] = array.raw[j];
          i++;
          array.raw[j] = array.raw[i];
        }
      }
      array.raw[i] = pivot;
      top++;
      if (i - lower < upper - i) {
        lostack[top] = i + 1;
        histack[top] = upper;
        upper = i - 1;
      } else {
        histack[top] = i - 1;
        lostack[top] = lower;
        lower = i + 1;
      }
    }
  }
}

/// NUMBER_LEN takes a state number and returns the number of digits in that
/// number.
int number_len(int state_no) {
  int num = 0;
  do {
    state_no /= 10;
    num++;
  } while (state_no != 0);
  return num;
}

/// This procedure takes two character strings as arguments: IN and OUT.
/// IN identifies a grammar symbol or name that is checked whether
/// it needs to be quoted. If so, the necessary quotes are added
/// as IN is copied into the space identified by OUT.
/// NOTE that it is assumed that IN and OUT do not overlap each other.
void restore_symbol(char *out, const char *in, char ormark, char escape) {
  const int len = strlen(in);
  if (len > 0) {
    if ((len == 1 && in[0] == ormark) || in[0] == escape || in[0] == '\'' || in[len - 1] == '\'' || (strchr(in, ' ') != NULL &&(in[0] != '<' || in[len - 1] != '>'))) {
      *out++ = '\'';
      while (*in != '\0') {
        if (*in == '\'') {
          *out++ = *in;
        }
        *out++ = *in++;
      }
      *out++ = '\'';
      *out = '\0';
      return;
    }
  }
  strcpy(out, in);
  if (out[0] == '\n') {
    // one of the special grammar symbols?
    out[0] = escape;
  }
}

/// PRINT_LARGE_TOKEN generates code to print a token that may exceed the
/// limit of its field.  The argument are LINE which is the symbol a varying
/// length character string, TOKEN which is the symbol to be printed, INDENT
/// which is a character string to be used as an initial prefix to indent the
/// output line, and LEN which indicates the maximum number of characters that
/// can be printed on a given line.  At the end of this process, LINE will
/// have the value of the remaining substring that can fit on the output line.
/// If a TOKEN is too large to be indented in a line, but not too large for
/// the whole line, we forget the indentation, and printed it. Otherwise, it
/// is "chapped up" and printed in pieces that are each indented.
void print_large_token(char *line, char *token, const char *indent, int len) {
  int toklen = strlen(token);
  if (toklen > len && toklen <= PRINT_LINE_SIZE - 1) {
    printf("\n%s", token);
    token = "";
    strcpy(line, indent);
  } else {
    char temp[SYMBOL_SIZE + 1];
    for (; toklen > len; toklen = strlen(temp)) {
      memcpy(temp, token, len);
      temp[len] = '\0';
      printf("\n%s", temp);
      strcpy(temp, token+len + 1);
      token = temp;
    }
    strcpy(line, indent);
    strcat(line, token);
  }
}

/// PRINT_ITEM takes as parameter an ITEM_NO which it prints.
void print_item(const int item_no, struct CLIOptions* cli_options, struct ruletab_type *rules, struct itemtab *item_table, ArrayShort rhs_sym, char *string_table, struct symno_type *symno) {
  char tempstr[PRINT_LINE_SIZE + 1];
  char line[PRINT_LINE_SIZE + 1];
  char tok[SYMBOL_SIZE + 1];
  // We first print the left hand side of the rule, leaving at least
  // 5 spaces in the output line to accommodate the equivalence symbol
  // "::=" surrounded by blanks on both sides.  Then, we print all the
  // terminal symbols on the right hand side up to but not including
  // the dot symbol.
  const int rule_no = item_table[item_no].rule_number;
  int symbol = rules[rule_no].lhs;
  restore_symbol(tok, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
  int len = PRINT_LINE_SIZE - 5;
  print_large_token(line, tok, "", len);
  strcat(line, " ::= ");
  const int offset = MIN(strlen(line) - 1, PRINT_LINE_SIZE / 2 - 1);
  len = PRINT_LINE_SIZE - (offset + 4);
  int sbd = rules[rule_no].rhs; /* symbols before dot */
  for (const int k = rules[rule_no].rhs + item_table[item_no].dot - 1; sbd <= k; sbd++) {
    symbol = rhs_sym.raw[sbd];
    restore_symbol(tok, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
    if (strlen(tok) + strlen(line) > PRINT_LINE_SIZE - 4) {
      printf("\n%s", line);
      fill_in(tempstr, offset, ' ');
      print_large_token(line, tok, tempstr, len);
    } else {
      strcat(line, tok);
    }
    strcat(line, " ");
  }
  // We now add a DOT "." to the output line and print the remaining
  // symbols on the right hand side.  If ITEM_NO is a complete item,
  // we also print the rule number.
  if (item_table[item_no].dot == 0 || item_table[item_no].symbol == empty) {
    strcpy(tok, ".");
  } else {
    strcpy(tok, " .");
  }
  strcat(line, tok);
  len = PRINT_LINE_SIZE - (offset + 1);
  for (int i = rules[rule_no].rhs + item_table[item_no].dot; /* symbols after dot*/ i <= rules[rule_no + 1].rhs - 1; i++) {
    symbol = rhs_sym.raw[i];
    restore_symbol(tok, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
    if (strlen(tok) + strlen(line) > PRINT_LINE_SIZE - 1) {
      printf("\n%s", line);
      fill_in(tempstr, offset, ' ');
      print_large_token(line, tok, tempstr, len);
    } else {
      strcat(line, tok);
    }
    strcat(line, " ");
  }
  if (item_table[item_no].symbol == empty) /* complete item */
  {
    snprintf(tok, sizeof(tok), " (%d)", rule_no);
    if (strlen(tok) + strlen(line) > PRINT_LINE_SIZE - 1) {
      printf("\n%s", line);
      fill_in(line, offset, ' ');
    }
    strcat(line, tok);
  }
  printf("\n%s", line);
}

/// PRINT_STATE prints all the items in a state.  NOTE that when single
/// productions are eliminated, certain items that were added in a state by
/// CLOSURE, will no longer show up in the output.  Example: If we have the
/// item [A ::= .B]  in a state, and the GOTO_REDUCE generated on B has been
/// replaced by say the GOTO or GOTO_REDUCE of A, the item above can no longer
/// be retrieved, since transitions in a given state are reconstructed from
/// the KERNEL and ADEQUATE items of the actions in the GOTO and SHIFT maps.
void print_state(const int state_no, struct CLIOptions* cli_options, struct node **adequate_item, struct SRTable* srt, struct lastats_type *lastats, struct statset_type *statset, struct node **in_stat, struct ruletab_type *rules, struct itemtab *item_table, ArrayShort rhs_sym, char *string_table, struct symno_type *symno, struct LAState* ls) {
  char buffer[PRINT_LINE_SIZE + 1];
  char line[PRINT_LINE_SIZE + 1];
  // ITEM_SEEN is used to construct sets of items, to help avoid
  // adding duplicates in a list.  Duplicates can occur because an
  // item from the kernel set will either be shifted on if it is not a
  // complete item, or it will be a member of the Complete_items set.
  // Duplicates can also occur because of the elimination of single
  // productions.
  ArrayBool state_seen = Allocate_bool_array2(ls->max_la_state + 1);
  ArrayBool item_seen = Allocate_bool_array2(ls->num_items + 1);
  ArrayShort item_list = Allocate_short_array2(ls->num_items + 1);
  // INITIALIZATION -----------------------------------------------------------
  for ALL_STATES3(state_no, ls->num_states) {
    state_seen.raw[state_no] = false;
  }
  for ALL_ITEMS3(item_no, ls->num_items) {
    item_seen.raw[item_no] = false;
  }
  int kernel_size = 0;
  // END OF INITIALIZATION ----------------------------------------------------
  fill_in(buffer, PRINT_LINE_SIZE - (number_len(state_no) + 8 /* 8 = length("STATE") + 2 spaces + newline*/), '-');
  printf("\n\n\nSTATE %d %s", state_no, buffer);
  // Print the set of states that have transitions to STATE_NO.
  int n = 0;
  strcpy(line, "( ");
  struct node *q;
  for (bool end_node = (q = in_stat[state_no]) == NULL;
       !end_node;
       end_node = q == in_stat[state_no]) {
    // copy list of IN_STAT into array
    q = q->next;
    if (!state_seen.raw[q->value]) {
      state_seen.raw[q->value] = true;
      if (strlen(line) + number_len(q->value) > PRINT_LINE_SIZE - 2) {
        printf("\n%s", line);
        strcpy(line, "  ");
      }
      if (q->value != 0) {
        snprintf(buffer, sizeof(buffer), "%d ", q -> value);
        strcat(line, buffer);
      }
    }
  }
  strcat(line, ")");
  printf("\n%s\n", line);
  // Add the set of kernel items to the array ITEM_LIST, and mark all
  // items seen to avoid duplicates.
  for (q = statset[state_no].kernel_items; q != NULL; q = q->next) {
    kernel_size++;
    int item_no = q->value;
    item_list.raw[kernel_size] = item_no; /* add to array */
    item_seen.raw[item_no] = true; /* Mark as "seen" */
  }
  // Add the Complete Items to the array ITEM_LIST, and mark used.
  n = kernel_size;
  for (q = statset[state_no].complete_items; q != NULL; q = q->next) {
    int item_no = q->value;
    if (!item_seen.raw[item_no]) {
      item_seen.raw[item_no] = true; /* Mark as "seen" */
      item_list.raw[++n] = item_no;
    }
  }
  // Iterate over the shift map.  Shift-Reduce actions are identified
  // by a negative integer that indicates the rule in question , and
  // the associated item can be retrieved by indexing the array
  // ADEQUATE_ITEMS at the location of the rule.  For shift-actions, we
  // simply take the predecessor-items of all the items in the kernel
  // of the following state.
  // If the shift-action is a look-ahead shift, we check to see if the
  // look-ahead state contains shift actions, and retrieve the next
  // state from one of those shift actions.
  const struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
  for (int i = 1; i <= sh.size; i++) {
    int next_state = sh.map[i].action;
    while (next_state > ls->num_states) {
      const struct shift_header_type next_sh = srt->shift[lastats[next_state].shift_number];
      if (next_sh.size > 0) {
        next_state = next_sh.map[1].action;
      } else {
        next_state = 0;
      }
    }
    if (next_state == 0) {
      q = NULL;
    } else if (next_state < 0) {
      q = adequate_item[-next_state];
    } else {
      q = statset[next_state].kernel_items;
      if (q == NULL) /* single production state? */
        q = statset[next_state].complete_items;
    }
    for (; q != NULL; q = q->next) {
      int item_no = q->value - 1;
      if (!item_seen.raw[item_no]) {
        item_seen.raw[item_no] = true;
        item_list.raw[++n] = item_no;
      }
    }
  }
  // GOTOS and GOTO-REDUCES are analogous to SHIFTS and SHIFT-REDUCES.
  const struct goto_header_type go_to = statset[state_no].go_to;
  for (int i = 1; i <= go_to.size; i++) {
    if (go_to.map[i].action > 0) {
      q = statset[go_to.map[i].action].kernel_items;
      if (q == NULL) /* single production state? */
        q = statset[go_to.map[i].action].complete_items;
    } else {
      q = adequate_item[-go_to.map[i].action];
    }
    for (; q != NULL; q = q->next) {
      int item_no = q->value - 1;
      if (!item_seen.raw[item_no]) {
        item_seen.raw[item_no] = true;
        item_list.raw[++n] = item_no;
      }
    }
  }
  // Print the Kernel items.  If there are any closure items, skip a
  // line, sort then, then print them.  The kernel items are in sorted
  // order.
  for (int item_no = 1; item_no <= kernel_size; item_no++) {
    print_item(item_list.raw[item_no], cli_options, rules, item_table, rhs_sym, string_table, symno);
  }
  if (kernel_size < n) {
    printf("\n");
    qcksrt(item_list, kernel_size + 1, n);
    for (int item_no = kernel_size + 1; item_no <= n; item_no++) {
      print_item(item_list.raw[item_no], cli_options, rules, item_table, rhs_sym, string_table, symno);
    }
  }
  ffree(item_list.raw);
  ffree(item_seen.raw);
  ffree(state_seen.raw);
}

/// This procedure is invoked when a call to MALLOC, CALLOC or REALLOC fails.
void nospace() {
  fprintf(stderr, "*** Cannot allocate space ***\n");
  exit(12);
}
