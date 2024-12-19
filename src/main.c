#include <stdlib.h>
#include <string.h>
#include "common.h"

// region globals
long num_symbols = 0;
long num_names = 0;
long num_terminals = 0;
long num_non_terminals = 0;
long num_rules = 0;
long num_single_productions = 0;
long gotodom_size = 0;

int accept_image;
int eoft_image;
int eolt_image;
int empty;
int error_image;

long num_shift_maps = 0;
long num_shifts = 0;
long num_shift_reduces = 0;
long num_gotos = 0;
long num_goto_reduces = 0;
long num_reductions = 0;
long num_entries = 0;
long num_error_rules = 0;

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
// endregion


















// region partset

/// This procedure, PARTSET, is invoked to apply a heuristic of the
/// Optimal Partitioning algorithm to a COLLECTION of subsets.  The
/// size of each subset in COLLECTION is passed in a parallel vector:
/// ELEMENT_SIZE. Let SET_SIZE be the length of the bit_strings used
/// to represent the subsets in COLLECTION, the universe of the
/// subsets is the set of integers: [1..SET_SIZE].
/// The third argument, LIST, is a vector identifying the order in
/// which the subsets in COLLECTION must be processed when they are
/// output.
/// The last two arguments, START and STACK are output parameters.
/// We recall that the output of Optimal Partitioning is two vectors:
/// a BASE vector and a RANGE vector...  START is the base vector.
/// It is used to indicate the starting position in the range
/// vector for each subset.  When a subset shares elements with
/// another subset, this is indicated by in index in START being
/// negative.  START also contains an extra "fence" element.  I.e.,
/// it has one more element than COLLECTION.
/// STACK is a vector used to construct a partition of the elements
/// of COLLECTION. That partition is used later (in ctabs or tabutil)
/// to output the final range vector...
///
///
/// We first merge all sets that are identical.  A hash table is used
/// to keep track of subsets that have already been seen.
/// DOMAIN_TABLE is used as the base of the hash table.  DOMAIN_LINK
/// is used to link subsets that collided.
///
/// The next step is to partition the unique subsets in the hash
/// table based on the number of elements they contain.  The vector
/// PARTITION is used for that purpose.
///
/// Finally, we attempt to overlay as many subsets as possible by
/// performing the following steps:
///
/// 1) Choose a base set in the partition with the largest subsets
///    and push it into a stack. (using the vector STACK)
///
/// 2) Iterate over the remaining non_empty elements of the partitions
///    in decreasing order of the size of the subsets contained in the
///    element in question. If there exists a subset in the element
///    which is a subset of the subset on top of the stack, currently
///    being constructed, remove it from the partition, and push it
///    into the stack. Repeat step 2 until the partition is empty.
void partset(JBitset collection, ArrayLong element_size, ArrayLong list, ArrayLong start, ArrayLong stack, long set_size, bool from_process_scopes, const struct LAState* ls) {
  long collection_size;
  // TODO • Remove this unnecessary indirection.
  if (from_process_scopes) {
    collection_size = set_size;
    set_size = ls->num_states;
  } else {
    collection_size = ls->num_states;
  }
  const long bctype = collection.size;
  ArrayShort size_list = Allocate_short_array2(set_size + 1);
  ArrayShort partition = Allocate_short_array2(set_size + 1);
  ArrayShort domain_link = Allocate_short_array2(collection_size + 1);
  ArrayShort head = Allocate_short_array2(collection_size + 1);
  ArrayShort next = Allocate_short_array2(collection_size + 1);
  ArrayBool is_a_base = Allocate_bool_array2(collection_size + 1);
  JBitset temp_set;
  calloc0_set(temp_set, 1, bctype);
  // DOMAIN_TABLE is the base of a hash table used to compute the set
  // of unique subsets in COLLECTION. Collisions are resolved by links
  // which are implemented in DOMAIN_LINK.
  // HEAD is an array containing either the value OMEGA which
  // indicates that the corresponding subset in COLLECTION is
  // identical to another set, or it contains the "root" of a list of
  // subsets that are identical.  The elements of the list are placed
  // in the array NEXT.  When a state is at te root of a list, it is
  // used as a representative of that list.
  short domain_table[STATE_TABLE_SIZE];
  for (int i = 0; i <= STATE_TABLE_UBOUND; i++) {
    domain_table[i] = NIL;
  }
  // We now iterate over the states and attempt to insert each
  // domain set into the hash table...
  for (int index = 1; index <= collection_size; index++) {
    unsigned long hash_address = 0;
    for (int i = 0; i < bctype; i++) {
      hash_address += collection.raw[index * bctype + i];
    }
    hash_address %= STATE_TABLE_SIZE;
    //  Next, we search the hash table to see if the subset was
    // already inserted in it. If we find such a subset, we simply
    // add INDEX to a list associated with the subset found and
    // mark it as a duplicate by setting the head of its list to
    // OMEGA.  Otherwise, we have a new set...
    for (int i = domain_table[hash_address]; i != NIL; i = domain_link.raw[i]) {
      if (equal_sets(collection, index, collection, i, bctype)) {
        head.raw[index] = OMEGA;
        next.raw[index] = head.raw[i];
        head.raw[i] = index;
        goto continu;
      }
    }
    //  ...Subset indicated by INDEX not previously seen. Insert
    // it into the hash table, and initialize a new list with it.
    domain_link.raw[index] = domain_table[hash_address];
    domain_table[hash_address] = index;
    head.raw[index] = NIL; /* Start a new list */
  continu: ;
  }
  // We now partition all the unique sets in the hash table
  // based on the number of elements they contain...
  // NEXT is also used to construct these lists.  Recall that
  // the unique elements are roots of lists. Hence, their
  // corresponding HEAD elements are used, but their
  // corresponding NEXT field is still unused.
  for (int i = 0; i <= set_size; i++) {
    partition.raw[i] = NIL;
  }
  for (int index = 1; index <= collection_size; index++) {
    if (head.raw[index] != OMEGA) {
      /* Subset representative */
      int size = element_size.raw[index];
      next.raw[index] = partition.raw[size];
      partition.raw[size] = index;
    }
  }
  //     ...Construct a list of all the elements of PARTITION
  // that are not empty.  Only elements in this list will be
  // considered for subset merging later ...
  // Note that since the elements of PARTITION are added to
  // the list in ascending order and in stack-fashion, the
  // resulting list will be sorted in descending order.
  int size_root = NIL;
  for (int i = 0; i <= set_size; i++) {
    if (partition.raw[i] != NIL) {
      size_list.raw[i] = size_root;
      size_root = i;
    }
  }
  // Merge subsets that are mergeable using heuristic described
  // above.  The vector IS_A_BASE is used to mark subsets
  // chosen as bases.
  for (int i = 0; i <= collection_size; i++) {
    is_a_base.raw[i] = false;
  }
  for (int size = size_root; size != NIL; size = size_list.raw[size]) {
    // For biggest partition there is
    for (int base_set = partition.raw[size]; base_set != NIL; base_set = next.raw[base_set]) {
      // For each set in it...
      // Mark the state as a base state, and initialize
      // its stack.  The list representing the stack will
      // be circular...
      is_a_base.raw[base_set] = true;
      stack.raw[base_set] = base_set;
      // For remaining elements in partitions in decreasing order...
      for (int next_size = size_list.raw[size]; next_size != NIL; next_size = size_list.raw[next_size]) {
        int previous = NIL; /* mark head of list */
        // Iterate over subsets in the partition until we
        // find one that is a subset of the subset on top
        // of the stack.  If none is found, we go on to
        // the next element of the partition. Otherwise,
        // we push the new subset on top of the stack and
        // go on to the next element of the partition.
        // INDEX identifies the state currently on top
        // of the stack.
        for (int subset = partition.raw[next_size]; subset != NIL; previous = subset, subset = next.raw[subset]) {
          int index = stack.raw[base_set];
          B_ASSIGN_SET(temp_set, 0, collection, index, bctype);
          B_SET_UNION(temp_set, 0, collection, subset, bctype);
          // SUBSET is a subset of INDEX?
          if (equal_sets(temp_set, 0, collection, index, bctype)) {
            if (previous == NIL) {
              partition.raw[next_size] = next.raw[subset];
            } else {
              next.raw[previous] = next.raw[subset];
            }
            stack.raw[subset] = stack.raw[base_set];
            stack.raw[base_set] = subset;
            break; /* for (subset = partition[next_size]... */
          }
        }
      }
    }
  }
  // Iterate over the states in the order in which they are to
  // be output, and assign an offset location to each state.
  // Notice that an extra element is added to the size of each
  // base subset for the "fence" element.
  int offset = 1;
  for (int i = 1; i <= collection_size; i++) {
    int base_set = list.raw[i];
    if (is_a_base.raw[base_set]) {
      start.raw[base_set] = offset;
      // Assign the same offset to each subset that is
      // identical to the BASE_SET subset in question. Also,
      // mark the fact that this is a copy by using the negative
      // value of the OFFSET.
      for (int index = head.raw[base_set]; index != NIL; index = next.raw[index]) {
        start.raw[index] = -start.raw[base_set];
      }
      int size = element_size.raw[base_set] + 1;
      offset += size;
      // Now, assign offset values to each subset of the
      // BASE_SET. Once again, we mark them as sharing elements
      // by using the negative value of the OFFSET.
      // Recall that the stack is constructed as a circular
      // list.  Therefore, its end is reached when we go back
      // to the root... In this case, the root is already
      // processed, so we stop when we reach it.
      for (int index = stack.raw[base_set]; index != base_set; index = stack.raw[index]) {
        size = element_size.raw[index] + 1;
        start.raw[index] = -(offset - size);
        // INDEX identifies a subset of BASE_SET. Assign the
        // same offset as INDEX to each subset j that is
        // identical to the subset INDEX.
        for (int j = head.raw[index]; j != NIL; j = next.raw[j])
          start.raw[j] = start.raw[index];
      }
    }
  }
  start.raw[collection_size + 1] = offset;
  ffree(size_list.raw);
  ffree(partition.raw);
  ffree(domain_link.raw);
  ffree(head.raw);
  ffree(next.raw);
  ffree(is_a_base.raw);
  ffree(temp_set.raw);
}

// endregion









// region mkfirst

const int LEN = PRINT_LINE_SIZE - 4;

long NEXT_RULE_SIZE() {
  return num_rules + 1;
}

long LAST_RHS_INDEX(const int rule_no, struct ruletab_type *rules) {
  return rules[rule_no + 1].rhs - 1;
}

struct f_element_type {
  short suffix_root;
  short suffix_tail;
  short link;
};

///   This procedure tries to advance the RHS_START pointer.  If the current
/// symbol identified by the RHS_START element is a bad non-terminal it
/// returns FALSE.  Otherwise, the whole right-hand side is traversed, and it
/// returns the value TRUE.
bool is_terminal_rhs(ArrayShort rhs_start, const bool *produces_terminals, const int rule_no, const struct ruletab_type *rules, ArrayShort rhs_sym) {
  for (; rhs_start.raw[rule_no] <= rules[rule_no + 1].rhs - 1; rhs_start.raw[rule_no]++) {
    const int symbol = rhs_sym.raw[rhs_start.raw[rule_no]];
    if (IS_A_NON_TERMINAL(symbol)) {
      if (!produces_terminals[symbol])
        return false;
    }
  }
  return true;
}

/// This procedure checks whether any non-terminal symbols can fail to
/// generate a string of terminals.
///
/// A non-terminal "A" can generate a terminal string if the grammar in
/// question contains a rule of the form:
///
///         A ::= X1 X2 ... Xn           n >= 0,  1 <= i <= n
///
/// and Xi, for all i, is a terminal or a non-terminal that can generate a
/// string of terminals.
/// This routine is structurally identical to COMPUTE_NULLABLES.
void check_non_terminals(const struct CLIOptions *cli_options, ArrayShort lhs_rule, ArrayShort next_rule, ArrayShort nt_list, struct ruletab_type *rules, ArrayShort rhs_sym, char *string_table, struct symno_type *symno) {
  bool changed = true;
  ArrayShort rhs_start = Allocate_short_array2(NEXT_RULE_SIZE());
  ArrayBool produces_terminals = Allocate_bool_array2(num_non_terminals);
  produces_terminals.raw -= num_terminals + 1;
  // First, mark all non-terminals as not producing terminals. Then
  // initialize RHS_START. RHS_START is a mapping from each rule in
  // the grammar into the next symbol in its right-hand side that
  // has not yet proven to be a symbol that generates terminals.
  for ALL_NON_TERMINALS3(nt) {
    produces_terminals.raw[nt] = false;
  }
  produces_terminals.raw[accept_image] = true;
  for ALL_RULES3(rule_no) {
    rhs_start.raw[rule_no] = rules[rule_no].rhs;
  }
  // We now iterate over the rules and try to advance the RHS_START
  // pointer to each right-hand side as far as we can.  If one or
  // more non-terminals are found to be "all right", they are
  // marked as such and the process is repeated.
  //
  // If we go through all the rules and no new non-terminal is
  // found to be "all right" then we stop and return.
  //
  // Note that on each iteration, only rules associated with
  // non-terminals that are not "all right" are considered. Further,
  // as soon as a non-terminal is found to be "all right", the
  // remaining rules associated with it are not considered. I.e.,
  // we quit the inner loop.
  while (changed) {
    changed = false;
    for ALL_NON_TERMINALS3(nt) {
      int rule_no;
      for (bool end_node = (rule_no = lhs_rule.raw[nt]) == NIL; !produces_terminals.raw[nt] && !end_node; end_node = rule_no == lhs_rule.raw[nt]) {
        rule_no = next_rule.raw[rule_no];
        if (is_terminal_rhs(rhs_start, produces_terminals.raw, rule_no, rules, rhs_sym)) {
          changed = true;
          produces_terminals.raw[nt] = true;
        }
      }
    }
  }
  // Construct a list of all non-terminals that do not generate
  // terminal strings.
  int nt_root = NIL;
  int nt_last;
  for ALL_NON_TERMINALS3(nt) {
    if (!produces_terminals.raw[nt]) {
      if (nt_root == NIL) {
        nt_root = nt;
      } else {
        nt_list.raw[nt_last] = nt;
      }
      nt_last = nt;
    }
  }
  // If there are non-terminal symbols that do not generate
  // terminal strings, print them out and stop the program.
  if (nt_root != NIL) {
    char line[PRINT_LINE_SIZE + 1];
    nt_list.raw[nt_last] = NIL; /* mark end of list */
    strcpy(line, "*** ERROR: The following Non-terminal");
    if (nt_list.raw[nt_root] == NIL) {
      strcat(line, " does not generate any terminal strings: ");
    } else {
      strcat(line, "s do not generate any terminal strings: ");
      PRNT(line);
      strcpy(line, "        "); /* 8 spaces */
    }
    for (int symbol = nt_root; symbol != NIL; symbol = nt_list.raw[symbol]) {
      char tok[SYMBOL_SIZE + 1];
      restore_symbol(tok, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
      if (strlen(line) + strlen(tok) > PRINT_LINE_SIZE - 1) {
        PRNT(line);
        print_large_token(line, tok, "    ", LEN);
      } else {
        strcat(line, tok);
      }
      strcat(line, " ");
    }
    PRNT(line);
    exit(12);
  }
  produces_terminals.raw += num_terminals + 1;
  ffree(produces_terminals.raw);
  ffree(rhs_start.raw);
}

void no_rules_produced(const struct CLIOptions *cli_options, ArrayShort lhs_rule, ArrayShort nt_list, char *string_table, struct symno_type *symno) {
  // Build a list of all non-terminals that do not produce any rules.
  int nt_root = NIL;
  int nt_last;
  for ALL_NON_TERMINALS3(symbol) {
    if (lhs_rule.raw[symbol] == NIL) {
      if (nt_root == NIL) {
        nt_root = symbol;
      } else {
        nt_list.raw[nt_last] = symbol;
      }
      nt_last = symbol;
    }
  }
  // If the list of non-terminals that do not produce any rules
  // is not empty, signal error and stop.
  if (nt_root != NIL) {
    char line[PRINT_LINE_SIZE + 1];
    nt_list.raw[nt_last] = NIL;
    if (nt_list.raw[nt_root] == NIL) {
      PRNTERR("The following Non-terminal does not produce any rules: ");
    } else {
      PRNTERR("The following Non-terminals do not produce any rules: ");
    }
    strcpy(line, "        ");
    for (int symbol = nt_root; symbol != NIL; symbol = nt_list.raw[symbol]) {
      char tok[SYMBOL_SIZE + 1];
      restore_symbol(tok, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
      if (strlen(line) + strlen(tok) > PRINT_LINE_SIZE) {
        PRNT(line);
        print_large_token(line, tok, "    ", LEN);
      } else {
        strcat(line, tok);
      }
      strcat(line, " ");
    }
    PRNT(line);
    exit(12);
  }
}

/// This function computes the closure of a non-terminal LHS_SYMBOL passed
/// to it as an argument using the digraph algorithm.
/// The closure of a non-terminal A is the set of all non-terminals Bi that
/// can directly or indirectly start a string generated by A.
/// I.e., A *::= Bi X where X is an arbitrary string.
void compute_closure(const int lhs_symbol, ArrayShort stack, ArrayShort index_of, struct ProduceTop *topp, ArrayShort lhs_rule, ArrayShort next_rule, struct node **closure, struct ruletab_type *rules, ArrayShort rhs_sym) {
  ArrayShort nont_list = Allocate_short_array2(num_non_terminals);
  nont_list.raw -= num_terminals + 1; /* Temporary direct        */
  // access set for closure.
  stack.raw[++topp->top] = lhs_symbol;
  const int indx = topp->top;
  index_of.raw[lhs_symbol] = indx;
  for ALL_NON_TERMINALS3(i) {
    nont_list.raw[i] = OMEGA;
  }
  nont_list.raw[lhs_symbol] = NIL;
  int nt_root = lhs_symbol;
  closure[lhs_symbol] = NULL; /* Permanent closure set. Linked list */
  int rule_no;
  for (bool end_node = (rule_no = lhs_rule.raw[lhs_symbol]) == NIL; !end_node; /* Iterate over all rules of LHS_SYMBOL */ end_node = rule_no == lhs_rule.raw[lhs_symbol]) {
    rule_no = next_rule.raw[rule_no];
    int symbol = RHS_SIZE(rule_no, rules) == 0 ? empty : rhs_sym.raw[rules[rule_no].rhs];
    if (IS_A_NON_TERMINAL(symbol)) {
      if (nont_list.raw[symbol] == OMEGA) {
        /* if first time seen */
        if (index_of.raw[symbol] == OMEGA) {
          compute_closure(symbol, stack, index_of, topp, lhs_rule, next_rule, closure, rules, rhs_sym);
        }
        index_of.raw[lhs_symbol] = MIN(index_of.raw[lhs_symbol], index_of.raw[symbol]);
        nont_list.raw[symbol] = nt_root;
        nt_root = symbol;
        // add closure[symbol] to closure of LHS_SYMBOL.
        struct node *q;
        for (end_node = (q = closure[symbol]) == NULL;
             !end_node;
             end_node = q == closure[symbol]) {
          q = q->next;
          if (nont_list.raw[q->value] == OMEGA) {
            nont_list.raw[q->value] = nt_root;
            nt_root = q->value;
          }
        }
      }
    }
  }
  for (; nt_root != lhs_symbol; nt_root = nont_list.raw[nt_root]) {
    struct node *p = Allocate_node();
    p->value = nt_root;
    if (closure[lhs_symbol] == NULL)
      p->next = p;
    else {
      p->next = closure[lhs_symbol]->next;
      closure[lhs_symbol]->next = p;
    }
    closure[lhs_symbol] = p;
  }
  if (index_of.raw[lhs_symbol] == indx) {
    for (int symbol = stack.raw[topp->top]; symbol != lhs_symbol; symbol = stack.raw[--topp->top]) {
      struct node *q = closure[symbol];
      if (q != NULL) {
        struct node *p = q->next;
        free_nodes(p, q); /* free nodes used by CLOSURE[SYMBOL] */
        closure[symbol] = NULL;
      }
      struct node *p = Allocate_node();
      p->value = lhs_symbol;
      p->next = p;
      closure[symbol] = p;
      for (bool end_node = (q = closure[lhs_symbol]) == NULL; !end_node; end_node = q == closure[lhs_symbol]) {
        q = q->next;
        if (q->value != symbol) {
          p = Allocate_node();
          p->value = q->value;
          p->next = closure[symbol]->next;
          closure[symbol]->next = p;
          closure[symbol] = p;
        }
      }
      index_of.raw[symbol] = INFINITY;
    }
    index_of.raw[lhs_symbol] = INFINITY;
    topp->top--;
  }
  nont_list.raw += num_terminals + 1;
  ffree(nont_list.raw);
}

/// This procedure tries to advance the RHS_START pointer.  If the current
/// symbol identified by the RHS_START element is a terminal it returns FALSE
/// to indicate that it cannot go any further.  If it encounters a  non-null-
/// label non-terminal, it also returns FALSE. Otherwise, the whole right-hand
/// side is consumed, and it returns the value TRUE.
bool is_nullable_rhs(ArrayShort rhs_start, const int rule_no, ArrayBool null_nt, struct ruletab_type *rules, ArrayShort rhs_sym) {
  for (; rhs_start.raw[rule_no] <= rules[rule_no + 1].rhs - 1; rhs_start.raw[rule_no]++) {
    const int symbol = rhs_sym.raw[rhs_start.raw[rule_no]];
    if (IS_A_TERMINAL(symbol)) {
      return false;
    } else if (!null_nt.raw[symbol]) {
      /* when symbol is a non-terminal */
      return false;
    }
  }
  return true;
}

/// This subroutine computes FIRST(NT) for some non-terminal NT using the
/// digraph algorithm.
/// FIRST(NT) is the set of all terminals Ti that may start a string generated
/// by NT. That is, NT *::= Ti X where X is an arbitrary string.
void compute_first(const int nt, struct DetectedSetSizes *dss, ArrayShort stack, ArrayShort index_of, struct ProduceTop *topp, JBitset nt_first, ArrayShort lhs_rule, ArrayShort next_rule, ArrayBool null_nt, struct ruletab_type *rules, ArrayShort rhs_sym) {
  JBitset temp_set;
  calloc0_set(temp_set, 1, dss->term_set_size)
  stack.raw[++topp->top] = nt;
  const int indx = topp->top;
  index_of.raw[nt] = indx;
  // Iterate over all rules generated by non-terminal NT...
  // In this application of the transitive closure algorithm,
  //
  //  G(A) := { t | A ::= t X for a terminal t and a string X }
  //
  // The relation R is defined as follows:
  //
  //    R(A, B) iff A ::= B1 B2 ... Bk B X
  //
  // where Bi is nullable for 1 <= i <= k
  int rule_no;
  for (bool end_node = (rule_no = lhs_rule.raw[nt]) == NIL; !end_node; /* Iterate over all rules produced by NT */ end_node = rule_no == lhs_rule.raw[nt]) {
    rule_no = next_rule.raw[rule_no];
    bool blocked = false;
    for ENTIRE_RHS3(i, rule_no, rules) {
      int symbol = rhs_sym.raw[i];
      if (IS_A_NON_TERMINAL(symbol)) {
        if (index_of.raw[symbol] == OMEGA) {
          compute_first(symbol, dss, stack, index_of, topp, nt_first, lhs_rule, next_rule, null_nt, rules, rhs_sym);
        }
        index_of.raw[nt] = MIN(index_of.raw[nt], index_of.raw[symbol]);
        ASSIGN_SET(temp_set, 0, nt_first, symbol);
        RESET_BIT(temp_set, empty);
        SET_UNION(nt_first, nt, temp_set, 0);
        blocked = !null_nt.raw[symbol];
      } else {
        SET_BIT_IN(nt_first, nt, symbol);
        blocked = true;
      }
      if (blocked)
        break;
    }
    if (!blocked) {
      SET_BIT_IN(nt_first, nt, empty);
    }
  }
  if (index_of.raw[nt] == indx) {
    for (int symbol = stack.raw[topp->top]; symbol != nt; symbol = stack.raw[--topp->top]) {
      ASSIGN_SET(nt_first, symbol, nt_first, nt);
      index_of.raw[symbol] = INFINITY;
    }
    index_of.raw[nt] = INFINITY;
    topp->top--;
  }
  ffree(temp_set.raw);
}

///  FIRST_MAP takes as arguments two pointers, ROOT and TAIL, to a sequence
/// of symbols in RHS which it inserts in FIRST_TABLE.  The vector FIRST_TABLE
/// is used as the base for a hashed table where collisions are resolved by
/// links.  Elements added to this hash table are allocated from the vector
/// FIRST_ELEMENT, with the variable TOP always indicating the position of the
/// last element in FIRST_ELEMENT that was allocated.
/// NOTE: The suffix indentified by ROOT and TAIL is presumed not to be empty.
///       That is, ROOT <= TAIL !!!
short first_map(const int root, const int tail, struct ProduceTop *topp, struct f_element_type *first_element, ArrayShort first_table, ArrayShort rhs_sym) {
  for (int i = first_table.raw[rhs_sym.raw[root]]; i != NIL; i = first_element[i].link) {
    int k;
    int jj;
    for (jj = root + 1, k = first_element[i].suffix_root + 1; jj <= tail && k <= first_element[i].suffix_tail; jj++, k++) {
      if (rhs_sym.raw[jj] != rhs_sym.raw[k])
        break;
    }
    if (jj > tail && k > first_element[i].suffix_tail) {
      return i;
    }
  }
  topp->top++;
  first_element[topp->top].suffix_root = root;
  first_element[topp->top].suffix_tail = tail;
  first_element[topp->top].link = first_table.raw[rhs_sym.raw[root]];
  first_table.raw[rhs_sym.raw[root]] = topp->top;
  return topp->top;
}

/// COMPUTE_FOLLOW computes FOLLOW[nt] for some non-terminal NT using the
/// digraph algorithm.  FOLLOW[NT] is the set of all terminals Ti that
/// may immediately follow a string X generated by NT. I.e., if NT *::= X
/// then X Ti is a valid substring of a class of strings that may be
/// recognized by the language.
void compute_follow(const int nt, struct DetectedSetSizes *dss, ArrayShort stack, ArrayShort index_of, struct ProduceTop *topp, JBitset follow, ArrayShort next_item, ArrayShort nt_items, JBitset first, struct ruletab_type *rules, struct itemtab *item_table) {
  JBitset temp_set;
  calloc0_set(temp_set, 1, dss->term_set_size);
  // FOLLOW[NT] was initialized to 0 for all non-terminals.
  stack.raw[++topp->top] = nt;
  const int indx = topp->top;
  index_of.raw[nt] = indx;
  for (int item_no = nt_items.raw[nt]; item_no != NIL; item_no = next_item.raw[item_no]) {
    // iterate over all items of NT
    ASSIGN_SET(temp_set, 0, first, item_table[item_no].suffix_index);
    if (IS_ELEMENT(temp_set, empty)) {
      RESET_BIT(temp_set, empty);
      const int rule_no = item_table[item_no].rule_number;
      int lhs_symbol = rules[rule_no].lhs;
      if (index_of.raw[lhs_symbol] == OMEGA) {
        compute_follow(lhs_symbol, dss, stack, index_of, topp, follow, next_item, nt_items, first, rules, item_table);
      }
      SET_UNION(follow, nt, follow, lhs_symbol);
      index_of.raw[nt] = MIN(index_of.raw[nt], index_of.raw[lhs_symbol]);
    }
    SET_UNION(follow, nt, temp_set, 0);
  }
  if (index_of.raw[nt] == indx) {
    for (int lhs_symbol = stack.raw[topp->top]; lhs_symbol != nt; lhs_symbol = stack.raw[--topp->top]) {
      ASSIGN_SET(follow, lhs_symbol, follow, nt);
      index_of.raw[lhs_symbol] = INFINITY;
    }
    index_of.raw[nt] = INFINITY;
    topp->top--;
  }
  ffree(temp_set.raw);
}

/// MKFIRST constructs the FIRST and FOLLOW maps, the CLOSURE map,
/// ADEQUATE_ITEM and ITEM_TABLE maps and all other basic maps.
struct DetectedSetSizes mkbasic(struct CLIOptions *cli_options, JBitset nt_first, ArrayBool *rmpself, JBitset* first, struct FirstDeps* fd, struct ruletab_type *rules, ArrayShort rhs_sym, struct itemtab **item_tablep, char *string_table, struct symno_type *symno, struct LAState* ls) {
  struct DetectedSetSizes dss = {
    .non_term_set_size = num_non_terminals / SIZEOF_BC + (num_non_terminals % SIZEOF_BC ? 1 : 0),
    .term_set_size = num_terminals / SIZEOF_BC + (num_terminals % SIZEOF_BC ? 1 : 0),
    .null_nt = NULL,
  };
  // allocate various arrays
  ArrayShort lhs_rule = Allocate_short_array2(num_non_terminals);
  lhs_rule.raw -= num_terminals + 1;
  ArrayShort next_rule = Allocate_short_array2(NEXT_RULE_SIZE());
  ArrayShort first_item_of = Allocate_short_array2(NEXT_RULE_SIZE());
  ArrayShort stack = Allocate_short_array2(num_non_terminals + 1);
  ArrayShort index_of = Allocate_short_array2(num_non_terminals);
  index_of.raw -= num_terminals + 1;
  // NT_FIRST is used to construct a mapping from non-terminals to the
  // set of terminals that may appear first in a string derived from
  // the non-terminal.
  calloc0_set(nt_first, num_non_terminals, dss.term_set_size);
  nt_first.raw -= (num_terminals + 1) * dss.term_set_size;
  ArrayShort next_item = Allocate_short_array2(ls->num_items + 1);
  ArrayShort nt_items = Allocate_short_array2(num_non_terminals);
  nt_items.raw -= num_terminals + 1;
  ArrayShort nt_list = Allocate_short_array2(num_non_terminals);
  nt_list.raw -= num_terminals + 1;
  struct f_element_type *first_element;
  calloc0p(&first_element, ls->num_items + 1, struct f_element_type);
  calloc0p(item_tablep, ls->num_items + 1, struct itemtab);
  struct itemtab *item_table = *item_tablep;
  for ALL_NON_TERMINALS3(symbol) {
    lhs_rule.raw[symbol] = NIL;
  }
  // In this loop, we construct the LHS_RULE map which maps
  // each non-terminal symbol into the set of rules it produces
  for ALL_RULES3(rule_no) {
    int symbol = rules[rule_no].lhs;
    if (lhs_rule.raw[symbol] == NIL) {
      next_rule.raw[rule_no] = rule_no;
    } else {
      next_rule.raw[rule_no] = next_rule.raw[lhs_rule.raw[symbol]];
      next_rule.raw[lhs_rule.raw[symbol]] = rule_no;
    }
    lhs_rule.raw[symbol] = rule_no;
  }
  // Check if there are any non-terminals that do not produce
  // any rules.
  no_rules_produced(cli_options, lhs_rule, nt_list, string_table, symno);
  // Construct the CLOSURE map of non-terminals.
  calloc0p(&(fd->closure), num_non_terminals, struct node *);
  fd->closure -= num_terminals + 1;
  for ALL_NON_TERMINALS3(symbol) {
    index_of.raw[symbol] = OMEGA;
  }
  struct ProduceTop topp = {.top = 0};
  for ALL_NON_TERMINALS3(nt) {
    if (index_of.raw[nt] == OMEGA) {
      compute_closure(nt, stack, index_of, &topp, lhs_rule, next_rule, fd->closure, rules, rhs_sym);
    }
  }
  // Construct the NULL_NT map for non-terminals.
  // A non-terminal B is said to be nullable if either:
  //    B -> %empty  or  B -> B1 B2 B3 ... Bk  where Bi is
  //                         nullable for 1 <= i <= k
  dss.null_nt = Allocate_bool_array2(num_non_terminals);
  dss.null_nt.raw -= num_terminals + 1;
  // Calculate nullables
  {
    ///   This procedure computes the set of non-terminal symbols that can
    /// generate the empty string.  Such non-terminals are said to be nullable.
    ///
    /// A non-terminal "A" can generate empty if the grammar in question contains
    /// a rule:
    ///          A ::= B1 B2 ... Bn     n >= 0,  1 <= i <= n
    /// and Bi, for all i, is a nullable non-terminal.
    bool changed = true;
    ArrayShort rhs_start = Allocate_short_array2(NEXT_RULE_SIZE());
    // First, mark all non-terminals as non-nullable.  Then initialize
    // RHS_START. RHS_START is a mapping from each rule in the grammar
    // into the next symbol in its right-hand side that has not yet
    // proven to be nullable.
    for ALL_NON_TERMINALS3(nt) {
      dss.null_nt.raw[nt] = false;
    }
    for ALL_RULES3(rule_no) {
      rhs_start.raw[rule_no] = rules[rule_no].rhs;
    }
    // We now iterate over the rules and try to advance the RHS_START
    // pointer thru each right-hand side as far as we can.  If one or
    // more non-terminals are found to be nullable, they are marked
    // as such and the process is repeated.
    //
    // If we go through all the rules and no new non-terminal is found
    // to be nullable then we stop and return.
    //
    // Note that for each iteration, only rules associated with
    // non-terminals that are non-nullable are considered.  Further,
    // as soon as a non-terminal is found to be nullable, the
    // remaining rules associated with it are not considered.  I.e.,
    // we quit the inner loop.
    while (changed) {
      changed = false;
      for ALL_NON_TERMINALS3(nt) {
        int rule_no;
        for (bool end_node = (rule_no = lhs_rule.raw[nt]) == NIL; !dss.null_nt.raw[nt] && !end_node; end_node = rule_no == lhs_rule.raw[nt]) {
          rule_no = next_rule.raw[rule_no];
          if (is_nullable_rhs(rhs_start, rule_no, dss.null_nt, rules, rhs_sym)) {
            changed = true;
            dss.null_nt.raw[nt] = true;
          }
        }
      }
    }
    ffree(rhs_start.raw);
  }
  // Construct the FIRST map for non-terminals and also a list
  // of non-terminals whose first set is empty.
  for ALL_NON_TERMINALS3(symbol) {
    index_of.raw[symbol] = OMEGA;
  }
  topp.top = 0;
  for ALL_NON_TERMINALS3(nt) {
    if (index_of.raw[nt] == OMEGA) {
      compute_first(nt, &dss, stack, index_of, &topp, nt_first, lhs_rule, next_rule, dss.null_nt, rules, rhs_sym);
    }
  }
  //  Since every input source will be followed by the EOFT
  //  symbol, FIRST[accept_image] cannot contain empty but
  //  instead must contain the EOFT symbol.
  if (dss.null_nt.raw[accept_image]) {
    dss.null_nt.raw[accept_image] = false;
    RESET_BIT_IN(nt_first, accept_image, empty);
    SET_BIT_IN(nt_first, accept_image, eoft_image);
  }
  // Check whether there are any non-terminals that do not
  // generate any terminal strings. If so, signal error and stop.
  check_non_terminals(cli_options, lhs_rule, next_rule, nt_list, rules, rhs_sym, string_table, symno);
  // Construct the ITEM_TABLE, FIRST_ITEM_OF, and NT_ITEMS maps.
  ArrayShort first_table = Allocate_short_array2(num_symbols + 1);
  /* Initialize FIRST_TABLE to NIL */
  for ALL_SYMBOLS3(symbol) {
    first_table.raw[symbol] = NIL;
  }
  topp.top = 1;
  const int first_of_empty = topp.top;
  first_element[first_of_empty].suffix_root = 1;
  first_element[first_of_empty].suffix_tail = 0;
  for ALL_NON_TERMINALS3(symbol) {
    nt_items.raw[symbol] = NIL;
  }
  int item_no = 0;
  item_table[item_no].rule_number = 0;
  item_table[item_no].symbol = empty;
  item_table[item_no].dot = 0;
  item_table[item_no].suffix_index = NIL;
  for ALL_RULES3(rule_no) {
    first_item_of.raw[rule_no] = item_no + 1;
    int j = 0;
    const int k = LAST_RHS_INDEX(rule_no, rules);
    for ENTIRE_RHS3(i, rule_no, rules) {
      item_no++;
      int symbol = rhs_sym.raw[i];
      item_table[item_no].rule_number = rule_no;
      item_table[item_no].symbol = symbol;
      item_table[item_no].dot = j;
      if (cli_options->lalr_level > 1 ||
          IS_A_NON_TERMINAL(symbol) ||
          symbol == error_image) {
        if (i == k) {
          item_table[item_no].suffix_index = first_of_empty;
        } else {
          item_table[item_no].suffix_index = first_map(i + 1, k, &topp, first_element, first_table, rhs_sym);
        }
      } else {
        item_table[item_no].suffix_index = NIL;
      }
      if (IS_A_NON_TERMINAL(symbol)) {
        next_item.raw[item_no] = nt_items.raw[symbol];
        nt_items.raw[symbol] = item_no;
      }
      j++;
    }
    item_table[++item_no].rule_number = rule_no;
    item_table[item_no].symbol = empty;
    item_table[item_no].dot = j;
    item_table[item_no].suffix_index = NIL;
  }
  // We now compute the first set for all suffixes that were
  // inserted in the FIRST_TABLE map. There are TOP such suffixes
  // Extra space is also allocated to compute the first set for
  // suffixes whose left-hand side is the ACCEPT non-terminal.
  // The first set for these suffixes are the sets needed to
  // construct the FOLLOW map and compute look-ahead sets.  They
  // are placed in the FIRST table in the range 1..NUM_FIRST_SETS
  // The first element in the FIRST table contains the first sets
  // for the empty sequence.
  long num_first_sets = topp.top;
  int rule_no;
  for (bool end_node = (rule_no = lhs_rule.raw[accept_image]) == NIL; !end_node; end_node = rule_no == lhs_rule.raw[accept_image]) {
    rule_no = next_rule.raw[rule_no];
    num_first_sets++;
  }
  calloc0_set_fn(first, num_first_sets + 1, dss.term_set_size);
  for (int i = 1; i <= topp.top; i++) {
    int root = first_element[i].suffix_root;
    int tail = first_element[i].suffix_tail;
    int index = i;
    // S_FIRST takes as argument, two pointers: ROOT and TAIL to a sequence of
    // symbols in the vector RHS, and INDEX which is the index of a first set.
    // It computes the set of all terminals that can appear as the first symbol
    // in the sequence and places the result in the FIRST set indexable by INDEX.
    int symbol = root > tail ? empty : rhs_sym.raw[root];
    if (IS_A_TERMINAL(symbol)) {
      INIT_BITSET(*first, index);
      SET_BIT_IN(*first, index, symbol); /* add it to set */
    } else {
      ASSIGN_SET(*first, index, nt_first, symbol);
    }
    for (int i = root + 1; i <= tail && IS_IN_SET(*first, index, empty); i++) {
      symbol = rhs_sym.raw[i];
      RESET_BIT_IN(*first, index, empty); /* remove EMPTY */
      if (IS_A_TERMINAL(symbol)) {
        SET_BIT_IN(*first, index, symbol); /* add it to set */
      } else {
        SET_UNION(*first, index, nt_first, symbol);
      }
    }
  }
  rule_no = lhs_rule.raw[accept_image];
  for (int i = topp.top + 1; i <= num_first_sets; i++) {
    rule_no = next_rule.raw[rule_no];
    item_no = first_item_of.raw[rule_no];
    item_table[item_no].suffix_index = i;
    INIT_BITSET(*first, i);
    SET_BIT_IN(*first, i, eoft_image);
  }
  // If the READ/REDUCE option is on, we precalculate the kernel
  // of the final states which simply consists of the last item
  // in  the corresponding rule.  Rules with the ACCEPT
  // non-terminal as their left-hand side are not considered
  // to let the Accept action remain as a Reduce action
  // instead of a Goto/Reduce action.
  calloc0p(&(fd->adequate_item), num_rules + 1, struct node *);
  if (cli_options->read_reduce_bit) {
    for ALL_RULES3(rule_no) {
      const int j = RHS_SIZE(rule_no, rules);
      if (rules[rule_no].lhs != accept_image && j > 0) {
        item_no = first_item_of.raw[rule_no] + j;
        struct node *p = Allocate_node();
        p->value = item_no;
        p->next = NULL;
        fd->adequate_item[rule_no] = p;
      } else
        fd->adequate_item[rule_no] = NULL;
    }
  }
  // Construct the CLITEMS map. Each element of CLITEMS points
  // to a circular linked list of items.
  /// CL_ITEMS is a mapping from each non-terminal to a set (linked list)
  /// of items which are the first item of the rules generated by the
  /// non-terminal in question.
  calloc0p(&(fd->clitems), num_non_terminals, struct node *);
  fd->clitems -= num_terminals + 1;
  for ALL_NON_TERMINALS3(nt) {
    fd->clitems[nt] = NULL;
    for (bool end_node = (rule_no = lhs_rule.raw[nt]) == NIL; !end_node; end_node = rule_no == lhs_rule.raw[nt]) {
      rule_no = next_rule.raw[rule_no];
      struct node *p = Allocate_node();
      p->value = first_item_of.raw[rule_no];
      if (fd->clitems[nt] == NULL) {
        p->next = p;
      } else {
        p->next = fd->clitems[nt]->next;
        fd->clitems[nt]->next = p;
      }
      fd->clitems[nt] = p;
    }
  }
  // If LALR_LEVEL > 1, we need to calculate RMPSELF, a set that
  // identifies the nonterminals that can right-most produce
  // themselves. In order to compute RMPSELF, the map PRODUCES
  // must be constructed which identifies for each nonterminal
  // the set of nonterminals that it can right-most produce.
  if (cli_options->lalr_level > 1) {
    JBitset produces;
    calloc0_set(produces, num_non_terminals, dss.non_term_set_size);
    produces.raw -= (num_terminals + 1) * dss.non_term_set_size;
    struct node **direct_produces;
    calloc0p(&direct_produces, num_non_terminals, struct node *);
    direct_produces -= num_terminals + 1;
    for ALL_NON_TERMINALS3(sym_b) {
      struct node *p;
      for (bool end_node = (p = fd->clitems[sym_b]) == NULL; !end_node; end_node = p == fd->clitems[sym_b]) {
        p = p->next;
        int item_no = p->value;
        int sym_a = item_table[item_no].symbol;
        if (IS_A_NON_TERMINAL(sym_a)) {
          const int i = item_table[item_no].suffix_index;
          if (IS_IN_SET(*first, i, empty) && !IS_IN_SET(produces, sym_b, sym_a - num_terminals)) {
            SET_BIT_IN(produces, sym_b, sym_a - num_terminals);
            struct node *q = Allocate_node();
            q->value = sym_a;
            q->next = direct_produces[sym_b];
            direct_produces[sym_b] = q;
          }
        }
      }
    }
    // Complete the construction of the RIGHT_MOST_PRODUCES map
    // for non-terminals using the digraph algorithm.
    for ALL_NON_TERMINALS3(nt) {
      index_of.raw[nt] = OMEGA;
    }
    topp.top = 0;
    for ALL_NON_TERMINALS3(nt) {
      if (index_of.raw[nt] == OMEGA) {
        compute_produces(nt, direct_produces, stack, index_of, produces, &topp);
      }
    }
    *rmpself = init_rmpself(produces);
    produces.raw += (num_terminals + 1) * dss.non_term_set_size;
    ffree(produces.raw);
    direct_produces += num_terminals + 1;
    ffree(direct_produces);
  }
  // Construct the FOLLOW map if
  //   - Error-maps are requested
  //   - There are more than one starting symbol.
  if (cli_options->error_maps_bit || next_rule.raw[lhs_rule.raw[accept_image]] != lhs_rule.raw[accept_image]) {
    /// FOLLOW is a mapping from non-terminals to a set of terminals that
    /// may appear immediately after the non-terminal.
    JBitset follow;
    calloc0_set(follow, num_non_terminals, dss.term_set_size);
    follow.raw -= (num_terminals + 1) * dss.term_set_size;
    SET_BIT_IN(follow, accept_image, eoft_image);
    for ALL_NON_TERMINALS3(symbol) {
      index_of.raw[symbol] = OMEGA;
    }
    index_of.raw[accept_image] = INFINITY; /* mark computed */
    topp.top = 0;
    for ALL_NON_TERMINALS3(nt) {
      if (index_of.raw[nt] == OMEGA) {
        // not yet computed ?
        compute_follow(nt, &dss, stack, index_of, &topp, follow, next_item, nt_items, *first, rules, item_table);
      }
    }
    //  Initialize FIRST for suffixes that can follow each starting
    // non-terminal ( except the main symbol) with the FOLLOW set
    // of the non-terminal in question.
    rule_no = lhs_rule.raw[accept_image];
    if (next_rule.raw[rule_no] != rule_no) {
      rule_no = next_rule.raw[rule_no]; /* first rule */
      topp.top = item_table[first_item_of.raw[rule_no]].suffix_index;
      for (int i = topp.top + 1; i <= num_first_sets; i++) {
        rule_no = next_rule.raw[rule_no];
        item_no = first_item_of.raw[rule_no];
        int symbol = item_table[item_no].symbol;
        if (IS_A_NON_TERMINAL(symbol)) {
          ASSIGN_SET(*first, i, follow, symbol);
        }
      }
    }
  }
  // The unreachable symbols in the grammar are printed.

  // Print unreachables.
  {
    char line[PRINT_LINE_SIZE + 1];
    char tok[SYMBOL_SIZE + 1];
    // SYMBOL_LIST is used for two purposes:
    //  1) to mark symbols that are reachable from the Accepting
    //        non-terminal.
    //  2) to construct lists of symbols that are not reachable.
    ArrayLong symbol_list = Allocate_long_array2(num_symbols + 1);
    for ALL_SYMBOLS3(symbol) {
      symbol_list.raw[symbol] = OMEGA;
    }
    symbol_list.raw[eoft_image] = NIL;
    symbol_list.raw[empty] = NIL;
    if (cli_options->error_maps_bit) {
      symbol_list.raw[error_image] = NIL;
    }
    // Initialize a list consisting only of the Accept non-terminal.
    // This list is a work pile of non-terminals to process as follows:
    // Each non-terminal in the front of the list is removed in turn and
    // 1) All terminal symbols in one of its right-hand sides are
    //     marked reachable.
    // 2) All non-terminals in one of its right-hand sides are placed
    //     in the work pile of it had not been processed previously
    int nt_root = accept_image;
    symbol_list.raw[nt_root] = NIL;
    for (int nt = nt_root; nt != NIL; nt = nt_root) {
      nt_root = symbol_list.raw[nt];
      int rule_no;
      for (bool end_node = (rule_no = lhs_rule.raw[nt]) == NIL; !end_node; end_node = rule_no == lhs_rule.raw[nt]) {
        rule_no = next_rule.raw[rule_no];
        for ENTIRE_RHS3(i, rule_no, rules) {
          const int symbol = rhs_sym.raw[i];
          if (IS_A_TERMINAL(symbol)) {
            symbol_list.raw[symbol] = NIL;
          } else if (symbol_list.raw[symbol] == OMEGA) {
            symbol_list.raw[symbol] = nt_root;
            nt_root = symbol;
          }
        }
      }
    }
    // We now iterate (backwards to keep things in order) over the
    // terminal symbols, and place each unreachable terminal in a
    // list. If the list is not empty, we signal that these symbols
    // are unused.
    int t_root = NIL;
    for ALL_TERMINALS_BACKWARDS3(symbol) {
      if (symbol_list.raw[symbol] == OMEGA) {
        symbol_list.raw[symbol] = t_root;
        t_root = symbol;
      }
    }
    if (t_root != NIL) {
      if (symbol_list.raw[t_root] != NIL) {
        PRNT("*** The following Terminals are useless: ");
        printf("\n\n");
        strcpy(line, "        "); /* 8 spaces */
      } else {
        strcpy(line, "*** The following Terminal is useless: ");
      }
      for (int symbol = t_root; symbol != NIL; symbol = symbol_list.raw[symbol]) {
        restore_symbol(tok, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
        if (strlen(line) + strlen(tok) > PRINT_LINE_SIZE) {
          PRNT(line);
          print_large_token(line, tok, "    ", LEN);
        } else {
          strcat(line, tok);
          strcat(line, " ");
        }
        strcat(line, " ");
      }
      PRNT(line);
    }
    // We now iterate (backward to keep things in order) over the
    // non-terminals, and place each unreachable non-terminal in a
    // list.  If the list is not empty, we signal that these
    // symbols are unused.
    nt_root = NIL;
    for ALL_NON_TERMINALS_BACKWARDS3(symbol) {
      if (symbol_list.raw[symbol] == OMEGA) {
        symbol_list.raw[symbol] = nt_root;
        nt_root = symbol;
      }
    }
    if (nt_root != NIL) {
      if (symbol_list.raw[nt_root] != NIL) {
        PRNT("*** The following Non-Terminals are useless: ");
        printf("\n\n");
        strcpy(line, "        "); /* 8 spaces */
      } else {
        strcpy(line, "*** The following Non-Terminal is useless: ");
      }
      for (int symbol = nt_root; symbol != NIL; symbol = symbol_list.raw[symbol]) {
        restore_symbol(tok, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
        if (strlen(line) + strlen(tok) > PRINT_LINE_SIZE) {
          PRNT(line);
          print_large_token(line, tok, "    ", LEN);
        } else {
          strcat(line, tok);
        }
        strcat(line, " ");
      }
      PRNT(line);
    }
    ffree(symbol_list.raw);
  }

  // Free allocated arrays.
  nt_first.raw += (num_terminals + 1) * dss.term_set_size;
  ffree(nt_first.raw);
  nt_list.raw += num_terminals + 1;
  ffree(nt_list.raw);
  ffree(first_table.raw);
  ffree(first_element);
  nt_items.raw += num_terminals + 1;
  ffree(nt_items.raw);
  ffree(next_item.raw);
  ffree(stack.raw);
  index_of.raw += num_terminals + 1;
  ffree(index_of.raw);
  lhs_rule.raw += num_terminals + 1;
  ffree(lhs_rule.raw);
  ffree(next_rule.raw);
  ffree(first_item_of.raw);
  return dss;
}

// endregion











// region produce

struct scope_elmt {
  short link;
  short item;
  short index;
};

struct ScopeTop {
  long top;
};

struct Produced {
  JBitset produces;
  JBitset right_produces;
  JBitset left_produces;
};

///                             SCOPE_CHECK:
/// Given a nonterminal LHS_SYMBOL and a nonterminal TARGET where,
///
///                     LHS_SYMBOL ::= TARGET x
///
/// find out if whenever LHS_SYMBOL is introduced through closure, it
/// is introduced by a nonterminal SOURCE such that
///
///                     SOURCE ->rm* LHS_SYMBOL
///
///                               and
///
///                     SOURCE ->rm+ TARGET
bool scope_check(const int lhs_symbol, const int target, const int source, ArrayBool symbol_seen, struct Produced* produced, ArrayShort item_of, ArrayShort next_item, struct ruletab_type *rules, struct itemtab *item_table) {
  symbol_seen.raw[source] = true;
  if (IS_IN_SET(produced->right_produces, target, source - num_terminals) && IS_IN_SET(produced->right_produces, lhs_symbol, source - num_terminals)) {
    return false;
  }
  for (int item_no = item_of.raw[source]; item_no != NIL; item_no = next_item.raw[item_no]) {
    if (item_table[item_no].dot != 0) {
      return true;
    }
    const int rule_no = item_table[item_no].rule_number;
    const int symbol = rules[rule_no].lhs;
    if (!symbol_seen.raw[symbol]) {
      // not yet processed
      if (scope_check(lhs_symbol, target, symbol, symbol_seen, produced, item_of, next_item, rules, item_table)) {
        return 1;
      }
    }
  }
  return false;
}

/// This procedure checks whether an item of the form:
/// [A  ->  w B x  where  B ->* y A z  is a valid scope.
///
/// Such an item is a valid scope if the following conditions hold:
///
/// 1) it is not the case that x =>* %empty
/// 2) either it is not the case that w =>* %empty, or it is not the
///    case that B =>lm* A.
/// 3) it is not the case that whenever A is introduced through
///    closure, it is introduced by a nonterminal C where C =>rm* A
///    and C =>rm+ B.
bool is_scope(const int item_no, ArrayBool symbol_seen, struct Produced* produced, ArrayShort item_of, ArrayShort next_item, ArrayBool null_nt, struct ruletab_type *rules, struct itemtab *item_table) {
  for (int i = item_no - item_table[item_no].dot; i < item_no; i++) {
    const int symbol = item_table[i].symbol;
    if (IS_A_TERMINAL(symbol)) {
      return true;
    }
    if (!null_nt.raw[symbol]) {
      return true;
    }
  }
  const int lhs_symbol = rules[item_table[item_no].rule_number].lhs;
  const int target = item_table[item_no].symbol;
  if (IS_IN_SET(produced->left_produces, target, lhs_symbol - num_terminals)) {
    return false;
  }
  if (item_table[item_no].dot > 0) {
    return true;
  }
  for ALL_NON_TERMINALS3(nt) {
    symbol_seen.raw[nt] = false;
  }
  return scope_check(lhs_symbol, target, lhs_symbol, symbol_seen, produced, item_of, next_item, rules, item_table);
}

/// This boolean function takes two items as arguments and checks
/// whether they have the same prefix.
bool is_prefix_equal(const int item_no, const int item_no2, struct ruletab_type *rules, const struct itemtab *item_table, ArrayShort rhs_sym) {
  // a suffix
  if (item_no > 0) {
    return false;
  }
  const int item_no1 = -item_no;
  if (item_table[item_no1].dot != item_table[item_no2].dot) {
    return false;
  }
  int j = rules[item_table[item_no1].rule_number].rhs;
  const int start = rules[item_table[item_no2].rule_number].rhs;
  const int dot = start + item_table[item_no2].dot - 1;
  for (int i = start; i <= dot; i++) {
    // symbols before dot
    if (rhs_sym.raw[i] != rhs_sym.raw[j]) {
      return false;
    }
    j++;
  }
  return true;
}

/// This procedure takes as argument an item and inserts the string
/// prefix of the item preceeding the "dot" into the scope table, if
/// that string is not already there.  In any case, the index  number
/// associated with the prefix in question is returned.
/// NOTE that since both prefixes and suffixes are entered in the
/// table, the prefix of a given item, ITEM_NO, is encoded as
/// -ITEM_NO, whereas the suffix of that item is encoded as +ITEM_NO.
int insert_prefix(const int item_no, struct scope_elmt *scope_element, ArrayShort scope_table, struct ScopeTop* st, struct ruletab_type *rules, struct itemtab *item_table, ArrayShort rhs_sym, struct ScopeCounter* sc) {
  unsigned long hash_address = 0;
  const int rule_no = item_table[item_no].rule_number;
  int ii;
  for (ii = rules[rule_no].rhs; /* symbols before dot */
       ii < rules[rule_no].rhs + item_table[item_no].dot; ii++)
    hash_address += rhs_sym.raw[ii];
  ii = hash_address % SCOPE_SIZE;
  for (int j = scope_table.raw[ii]; j != NIL; j = scope_element[j].link) {
    if (is_prefix_equal(scope_element[j].item, item_no, rules, item_table, rhs_sym)) {
      return scope_element[j].index;
    }
  }
  st->top++;
  scope_element[st->top].item = -item_no;
  scope_element[st->top].index = sc->scope_rhs_size + 1;
  scope_element[st->top].link = scope_table.raw[ii];
  scope_table.raw[ii] = st->top;
  sc->scope_rhs_size += item_table[item_no].dot + 1;
  return scope_element[st->top].index;
}

/// This boolean function takes two items as arguments and checks
/// whether they have the same suffix.
bool is_suffix_equal(const int item_no1, const int item_no2, ArrayBool null_nt, struct ruletab_type *rules, const struct itemtab *item_table, ArrayShort rhs_sym) {
  if (item_no1 < 0) {
    // a prefix
    return false;
  }
  int rule_no = item_table[item_no1].rule_number;
  int i = rules[rule_no].rhs + item_table[item_no1].dot;
  const int dot1 = rules[rule_no + 1].rhs - 1;
  rule_no = item_table[item_no2].rule_number;
  int j = rules[rule_no].rhs + item_table[item_no2].dot;
  const int dot2 = rules[rule_no + 1].rhs - 1;
  while (i <= dot1 && j <= dot2) {
    // non-nullable syms before dot
    if (IS_A_NON_TERMINAL(rhs_sym.raw[i])) {
      if (null_nt.raw[rhs_sym.raw[i]]) {
        i++;
        continue;
      }
    } else if (rhs_sym.raw[i] == error_image) {
      i++;
      continue;
    }
    if (IS_A_NON_TERMINAL(rhs_sym.raw[j])) {
      if (null_nt.raw[rhs_sym.raw[j]]) {
        j++;
        continue;
      }
    } else if (rhs_sym.raw[j] == error_image) {
      j++;
      continue;
    }
    if (rhs_sym.raw[i] != rhs_sym.raw[j]) {
      return false;
    }
    j++;
    i++;
  }
  for (; i <= dot1; i++) {
    if (IS_A_NON_TERMINAL(rhs_sym.raw[i])) {
      if (!null_nt.raw[rhs_sym.raw[i]]) {
        return false;
      }
    } else if (rhs_sym.raw[i] != error_image) {
      return false;
    }
  }
  for (; j <= dot2; j++) {
    if (IS_A_NON_TERMINAL(rhs_sym.raw[j])) {
      if (!null_nt.raw[rhs_sym.raw[j]]) {
        return false;
      }
    } else if (rhs_sym.raw[j] != error_image) {
      return false;
    }
  }
  return true;
}

/// This procedure is analoguous to INSERT_PREFIX.  It takes as
/// argument an item, and inserts the suffix string following the dot
/// in the item into the scope table, if it is not already there.
/// In any case, it returns the index associated with the suffix.
/// When inserting a suffix into the table, all nullable nonterminals
/// in the suffix are disregarded.
int insert_suffix(const int item_no, struct scope_elmt *scope_element, ArrayShort scope_table, struct ScopeTop* st, ArrayBool null_nt, struct ruletab_type *rules, struct itemtab *item_table, ArrayShort rhs_sym, struct ScopeCounter* sc) {
  int num_elements = 0;
  unsigned long hash_address = 0;
  const int rule_no = item_table[item_no].rule_number;
  int ii;
  for (ii = rules[rule_no].rhs + item_table[item_no].dot; ii < rules[rule_no + 1].rhs; /* symbols after dot */ ii++) {
    if (IS_A_NON_TERMINAL(rhs_sym.raw[ii])) {
      if (!null_nt.raw[rhs_sym.raw[ii]]) {
        hash_address += rhs_sym.raw[ii];
        num_elements++;
      }
    } else if (rhs_sym.raw[ii] != error_image) {
      hash_address += rhs_sym.raw[ii];
      num_elements++;
    }
  }
  ii = hash_address % SCOPE_SIZE;
  for (int j = scope_table.raw[ii]; j != NIL; j = scope_element[j].link) {
    if (is_suffix_equal(scope_element[j].item, item_no, null_nt, rules, item_table, rhs_sym)) {
      return scope_element[j].index;
    }
  }
  st->top++;
  scope_element[st->top].item = item_no;
  scope_element[st->top].index = sc->scope_rhs_size + 1;
  scope_element[st->top].link = scope_table.raw[ii];
  scope_table.raw[ii] = st->top;
  sc->scope_rhs_size += num_elements + 1;
  return scope_element[st->top].index;
}

/// This procedure takes as parameter a nonterminal, LHS_SYMBOL, and
/// determines whether there is a terminal symbol t such that
/// LHS_SYMBOL can rightmost produce a string tX.  If so, t is
/// returned, otherwise EMPTY is returned.
int get_shift_symbol(const int lhs_symbol, ArrayBool symbol_seen, struct node **clitems, struct ruletab_type *rules, struct itemtab *item_table, ArrayShort rhs_sym) {
  if (!symbol_seen.raw[lhs_symbol]) {
    struct node *p;
    symbol_seen.raw[lhs_symbol] = true;
    for (bool end_node = (p = clitems[lhs_symbol]) == NULL; !end_node; end_node = p == clitems[lhs_symbol]) {
      p = p->next;
      const int item_no = p->value;
      const int rule_no = item_table[item_no].rule_number;
      if (RHS_SIZE(rule_no, rules) > 0) {
        int symbol = rhs_sym.raw[rules[rule_no].rhs];
        if (IS_A_TERMINAL(symbol)) {
          return symbol;
        } else {
          symbol = get_shift_symbol(symbol, symbol_seen, clitems, rules, item_table, rhs_sym);
          if (symbol != empty) {
            return symbol;
          }
        }
      }
    }
  }
  return empty;
}

/// This procedure computes for each state the set of non-terminal symbols
/// that are required as candidates for secondary error recovery.  If the
/// option NAMES=OPTIMIZED is requested, the NAME map is optimized and SYMNO
/// is updated accordingly.
void produce(struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct Produced* produced, struct ScopeTop* st, JBitset first, struct scope_type *scope, struct node **clitems, ArrayBool null_nt, ArrayLong* scope_right_side, struct ruletab_type *rules, ArrayShort *scope_state, struct statset_type *statset, struct itemtab *item_table, ArrayShort rhs_sym, ArrayShort *gd_range, ArrayShort *gd_index, struct symno_type *symno, struct ScopeCounter* sc, char *string_table, int *name, struct LAState* ls) {
  // TOP, STACK, and INDEX are used for the digraph algorithm
  // in the routines COMPUTE_PRODUCES.
  //
  // The array PRODUCES is used to construct two maps:
  //
  // 1) PRODUCES, a mapping from each non-terminal A to the set of
  // non-terminals C such that:
  //
  //                   A  =>*  x C w
  //
  // 2) RIGHT_MOST_PRODUCES, a mapping from each non-terminal A to
  // the set of non-terminals C such that:
  //
  //                   C =>+ A x   and   x =>* %empty.
  //
  // NOTE: This is really a reverse right-most produces mapping,
  //       since given the above rule, we say that
  //       C right-most produces A.
  ArrayShort stack = Allocate_short_array2(num_symbols + 1);
  ArrayShort index_of = Allocate_short_array2(num_symbols + 1);
  ArrayShort names_map = Allocate_short_array2(num_names + 1);
  ArrayBool name_used = Allocate_bool_array2(num_names + 1);
  ArrayShort item_list = Allocate_short_array2(ls->num_items + 1);
  ArrayShort nt_list = Allocate_short_array2(num_non_terminals + 1);
  nt_list.raw -= num_terminals + 1;
  JBitset set;
  calloc0_set(set, 1, dss->non_term_set_size);
  JBitset produces;
  calloc0_set(produces, num_non_terminals, dss->non_term_set_size);
  produces.raw -= (num_terminals + 1) * dss->non_term_set_size;
  struct node **goto_domain;
  calloc0p(&goto_domain, ls->num_states + 1, struct node *);
  struct node **direct_produces;
  calloc0p(&direct_produces, num_non_terminals, struct node *);
  direct_produces -= num_terminals + 1;
  // Note that the space allocated for PRODUCES and DIRECT_PRODUCES
  // is automatically initialized to 0 by calloc. Logically, this sets
  // all the sets in the PRODUCES map to the empty set and all the
  // pointers in DIRECT_PRODUCES are set to NULL.
  //
  // Next, PRODUCES is initialized to compute RIGHT_MOST_PRODUCES.
  // Also, we count the number of error rules and verify that they are
  // in the right format.
  int item_root = NIL;
  for ALL_NON_TERMINALS3(sym_a) {
    struct node *p;
    for (bool end_node = (p = clitems[sym_a]) == NULL; !end_node; end_node = p == clitems[sym_a]) {
      p = p->next;
      int item_no = p->value;
      int sym_b = item_table[item_no].symbol;
      if (IS_A_NON_TERMINAL(sym_b)) {
        const int i = item_table[item_no].suffix_index;
        if (IS_IN_SET(first, i, empty) && !IS_IN_SET(produces, sym_b, sym_a - num_terminals)) {
          SET_BIT_IN(produces, sym_b, sym_a - num_terminals);
          struct node *q = Allocate_node();
          q->value = sym_a;
          q->next = direct_produces[sym_b];
          direct_produces[sym_b] = q;
        }
      }
      int rule_no = item_table[item_no].rule_number;
      int ii;
      for (ii = 0; ii < RHS_SIZE(rule_no, rules); ii++) {
        if (item_table[item_no + ii].symbol == error_image) {
          break;
        }
      }
      item_no += ii;
      sym_b = item_table[item_no].symbol;
      if (sym_b == error_image) {
        if (IS_A_NON_TERMINAL(item_table[item_no + 1].symbol) && ii > 0) {
          sym_b = item_table[item_no + 2].symbol;
          if (sym_b == empty) {
            num_error_rules++;
          }
        }
        if (sym_b != empty) {
          item_list.raw[item_no] = item_root;
          item_root = item_no;
        }
      }
      sym_b = eoft_image;
    }
  }
  // If some error rules are in the wrong format and report them.
  if (item_root != NIL) {
    if (item_list.raw[item_root] == NIL) {
      printf("*** This error rule is not in manual format:\n\n");
    } else {
      printf("*** These error rules are not in manual format:\n\n");
    }
    for (int item_no = item_root; item_no != NIL; item_no = item_list.raw[item_no]) {
      print_item(item_no, cli_options, rules, item_table, rhs_sym, string_table, symno);
    }
  }
  // Complete the construction of the RIGHT_MOST_PRODUCES map for
  // non-terminals using the digraph algorithm.
  // We make sure that each non-terminal A is not present in its own
  // PRODUCES set since we are interested in the non-reflexive
  // (positive) transitive closure.
  for ALL_SYMBOLS3(symbol) {
    index_of.raw[symbol] = OMEGA;
  }
  struct ProduceTop top = {.top = 0};
  for ALL_NON_TERMINALS3(nt) {
    if (index_of.raw[nt] == OMEGA) {
      compute_produces(nt, direct_produces, stack, index_of, produced->produces, &top);
    }
    RESET_BIT_IN(produces, nt, nt - num_terminals);
  }
  // Construct the minimum subset of the domain of the GOTO map
  // needed for automatic secondary level error recovery.   For each
  // state, we start out with the set of all nonterminals on which
  // there is a transition in that state, and pare it down to a
  // subset S, by removing all nonterminals B in S such that there
  // is a goto-reduce action on B by a single production.  If the
  // READ-REDUCE option is not turned on, then, we check whether or
  // not the goto action on B is to an LR(0) reduce state.Once we have
  // our subset S, we further reduce its size as follows.  For each
  // nonterminal A in S such that there exists another nonterminal
  // B in S, where B ^= A,  A ->+ Bx  and  x =>* %empty, we remove A
  // from S.
  // At the end of this process, the nonterminal elements whose
  // NT_LIST values are still OMEGA are precisely the nonterminal
  // symbols that are never used as candidates.
  for ALL_NON_TERMINALS3(i) {
    nt_list.raw[i] = OMEGA;
  }
  nt_list.raw[accept_image] = NIL;
  for ALL_STATES3(state_no, ls->num_states) {
    const struct goto_header_type go_to = statset[state_no].go_to;
    int nt_root = NIL;
    INIT_SET(set);
    for (int i = 1; i <= go_to.size; i++) {
      const int symbol = go_to.map[i].symbol;
      const int state = go_to.map[i].action;
      int rule_no;
      if (state < 0) {
        rule_no = -state;
      } else {
        struct node *q = statset[state].kernel_items;
        int item_no = q->value;
        if (q->next != NULL) {
          rule_no = 0;
        } else {
          rule_no = item_table[item_no].rule_number;
        }
      }
      if (rule_no == 0 || RHS_SIZE(rule_no, rules) != 1) {
        nt_list.raw[symbol] = nt_root;
        nt_root = symbol;
        SET_UNION(set, 0, produces, symbol);
      }
    }
    goto_domain[state_no] = NULL;
    for (int symbol = nt_root; symbol != NIL; symbol = nt_list.raw[symbol]) {
      if (!IS_ELEMENT(set, symbol - num_terminals)) {
        struct node *q = Allocate_node();
        q->value = symbol;
        q->next = goto_domain[state_no];
        goto_domain[state_no] = q;
        gotodom_size++;
      }
    }
  }
  // Allocate and construct the permanent goto domain structure:
  //   GD_INDEX and GD_RANGE.
  int n = 0;
  *gd_index = Allocate_short_array2(ls->num_states + 2);
  *gd_range = Allocate_short_array2(gotodom_size + 1);
  for ALL_STATES3(state_no, ls->num_states) {
    gd_index->raw[state_no] = n + 1;
    struct node *p;
    struct node *q;
    for (p = goto_domain[state_no]; p != NULL; q = p, p = p->next) {
      gd_range->raw[++n] = p->value;
    }
    if (goto_domain[state_no] != NULL) {
      free_nodes(goto_domain[state_no], q);
    }
  }
  gd_index->raw[ls->num_states + 1] = n + 1;
  if (cli_options->scopes_bit) {
      // Compute set of "scopes" and use it to construct SCOPE map.
      int num_state_sets = 0;
      int n;
      int max_prefix_length = 0;
      int item_root;
      ArrayShort prefix_index = Allocate_short_array2(ls->num_items + 1);
      ArrayShort suffix_index = Allocate_short_array2(ls->num_items + 1);
      ArrayShort item_of = Allocate_short_array2(num_non_terminals);
      item_of.raw -= num_terminals + 1;
      ArrayShort next_item = Allocate_short_array2(ls->num_items + 1);
      ArrayBool symbol_seen = Allocate_bool_array2(num_non_terminals);
      symbol_seen.raw -= num_terminals + 1;
      struct node **states_of;
      calloc0p(&states_of, num_non_terminals, struct node *);
      states_of -= num_terminals + 1;
      ArrayShort state_index = Allocate_short_array2(num_non_terminals);
      state_index.raw -= num_terminals + 1;
      struct scope_elmt *scope_element;
      calloc0p(&scope_element, ls->num_items + 1, struct scope_elmt);
      // Initially, PRODUCES was used to compute the right-most-produces
      // map.  We save that map map and make it reflexive.  Recall that
      // RIGHT_PRODUCES is a mapping from each nonterminal B into the set
      // of nonterminals A such that:
      //
      //    A =>rm* B
      //
      // Next, reallocate PRODUCES and initialize it in order to
      // construct the LEFT_PRODUCES map. Initially, CALLOC sets PRODUCES
      // to the empty map.
      // LEFT_PRODUCES is a mapping  from each nonterminal A into the set
      // of nonterminals B such that:
      //
      //    A =>lm* B x
      //
      // for some arbitrary string x.
      //
      // Since A ->* A for all A,  we insert A in PRODUCES(A)  (but not
      // in the linked list).
      produced->right_produces = produces;
      calloc0_set(produces, num_non_terminals, dss->non_term_set_size);
      produces.raw -= (num_terminals + 1) * dss->non_term_set_size;
      for ALL_NON_TERMINALS3(nt) {
        SET_BIT_IN(produced->right_produces, nt, nt - num_terminals);
        SET_BIT_IN(produces, nt, nt - num_terminals);
        direct_produces[nt] = NULL;
        struct node *p;
        for (bool end_node = (p = clitems[nt]) == NULL; !end_node; end_node = p == clitems[nt]) {
          p = p->next;
          for (int item_no = p->value; IS_A_NON_TERMINAL(item_table[item_no].symbol);item_no++) {
            int symbol = item_table[item_no].symbol;
            if (!IS_IN_SET(produces, nt, symbol - num_terminals)) {
              SET_BIT_IN(produces, nt, symbol - num_terminals);
               struct node *q = Allocate_node();
              q->value = symbol;
              q->next = direct_produces[nt];
              direct_produces[nt] = q;
            }
            if (!null_nt.raw[symbol]) {
              break;
            }
          }
        }
      }
      // Complete the construction of the LEFT_produces map for
      // non_terminals using the digraph algorithm.
      for ALL_NON_TERMINALS3(nt) {
        index_of.raw[nt] = OMEGA;
      }
      top.top = 0;
      for ALL_NON_TERMINALS3(nt) {
        if (index_of.raw[nt] == OMEGA) {
          compute_produces(nt, direct_produces, stack, index_of, produced->produces, &top);
        }
      }
      produced->left_produces = produces;
      // Allocate and initialize the PRODUCES array to construct the
      // PRODUCES map.  After allocation, CALLOC sets all sets to empty.
      // Since A ->* A for all A,  we insert A in PRODUCES(A)  (but not
      // in the linked list).
      calloc0_set(produces, num_non_terminals, dss->non_term_set_size);
      produces.raw -= (num_terminals + 1) * dss->non_term_set_size;
      for ALL_NON_TERMINALS3(nt) {
        SET_BIT_IN(produces, nt, nt - num_terminals);
        direct_produces[nt] = NULL;
        struct node *p;
        for (bool end_node = (p = clitems[nt]) == NULL; !end_node; end_node = p == clitems[nt]) {
          p = p->next;
          for (int item_no = p->value;
               item_table[item_no].symbol != empty; item_no++) {
            int symbol = item_table[item_no].symbol;
            if (IS_A_NON_TERMINAL(symbol)) {
              if (!IS_IN_SET(produces, nt, symbol - num_terminals)) {
                SET_BIT_IN(produces, nt, symbol - num_terminals);
                 struct node *q = Allocate_node();
                q->value = symbol;
                q->next = direct_produces[nt];
                direct_produces[nt] = q;
              }
            }
          }
        }
      }
      // Complete the construction of the PRODUCES map for
      // non_terminals using the digraph algorithm.
      //
      // Since $ACC =>* x A y for all nonterminal A in the grammar, a
      // single call to COMPUTE_PRODUCES does the trick.
      for ALL_NON_TERMINALS3(nt) {
        index_of.raw[nt] = OMEGA;
      }
      top.top = 0;
      compute_produces(accept_image, direct_produces, stack, index_of, produced->produces, &top);
      // Construct a mapping from each non_terminal A into the set of
      // items of the form [B  ->  x . A y].
      for ALL_NON_TERMINALS3(nt) {
        item_of.raw[nt] = NIL;
      }
      for ALL_ITEMS3(item_no, ls->num_items) {
        int dot_symbol = item_table[item_no].symbol;
        if (IS_A_NON_TERMINAL(dot_symbol)) {
          next_item.raw[item_no] = item_of.raw[dot_symbol];
          item_of.raw[dot_symbol] = item_no;
        }
      }
      // Construct a list of scoped items in ITEM_LIST.
      // Scoped items are derived from rules of the form  A -> x B y such
      // that B =>* w A z, %empty not in FIRST(y), and it is not the case
      // that x = %empty and B ->* A v.
      // Scoped items may also be identified by the user, using %error
      // productions.
      // As scoped items are added to the list, we keep track of the
      // longest prefix encountered.  This is subsequently used to
      // bucket sort the scoped items in descending order of the length
      // of their prefixes.
      for ALL_ITEMS3(item_no, ls->num_items) {
        item_list.raw[item_no] = OMEGA;
      }
      item_root = NIL;
      for ALL_ITEMS3(item_no, ls->num_items) {
        int dot_symbol = item_table[item_no].symbol;
        if (dot_symbol == error_image) {
          if (item_table[item_no].dot != 0 && !IS_IN_SET(first, item_table[item_no].suffix_index, empty)) {
            if (item_list.raw[item_no] == OMEGA) {
              item_list.raw[item_no] = item_root;
              item_root = item_no;
              max_prefix_length = MAX(max_prefix_length, item_table[item_no].dot);
            }
          }
        } else if (IS_A_NON_TERMINAL(dot_symbol)) {
          int symbol = rules[item_table[item_no].rule_number].lhs;
          if (!IS_IN_SET(first, item_table[item_no].suffix_index, empty) && IS_IN_SET(produces, dot_symbol, symbol - num_terminals)) {
            if (is_scope(item_no, symbol_seen, produced, item_of, next_item, null_nt, rules, item_table)) {
              int ii;
              for (ii = item_no + 1; ; ii++) {
                symbol = item_table[ii].symbol;
                if (IS_A_TERMINAL(symbol)) {
                  break;
                }
                if (!null_nt.raw[symbol]) {
                  break;
                }
              }
              if (IS_A_NON_TERMINAL(symbol)) {
                for ALL_NON_TERMINALS3(nt) {
                  symbol_seen.raw[nt] = false;
                }
                symbol = get_shift_symbol(symbol, symbol_seen, clitems, rules, item_table, rhs_sym);
              }
              if (symbol != empty && item_list.raw[ii] == OMEGA) {
                item_list.raw[ii] = item_root;
                item_root = ii;
                max_prefix_length = MAX(max_prefix_length, item_table[ii].dot);
              }
            }
          }
        }
      }
      // In this loop, the prefix and suffix string for each scope in
      // entered into a table.  We also use the SYMBOL_SEEN array to
      // identify the set of left-hand side symbols associated with the
      // scopes.
      ArrayShort scope_table = Allocate_short_array2(SCOPE_SIZE);
      for (int i = 0; i < SCOPE_SIZE; i++) {
        scope_table.raw[i] = NIL;
      }
      for ALL_NON_TERMINALS3(nt) {
        symbol_seen.raw[nt] = false;
      }
      for (int item_no = item_root; item_no != NIL; item_no = item_list.raw[item_no]) {
        int rule_no = item_table[item_no].rule_number;
        int symbol = rules[rule_no].lhs;
        sc->num_scopes = sc->num_scopes + 1;
        symbol_seen.raw[symbol] = true;
        prefix_index.raw[item_no] = insert_prefix(item_no, scope_element, scope_table, st, rules, item_table, rhs_sym, sc);
        suffix_index.raw[item_no] = insert_suffix(item_no, scope_element, scope_table, st, null_nt, rules, item_table, rhs_sym, sc);
      }
      ffree(scope_table.raw);
      // We now construct a mapping from each nonterminal symbol that is
      // the left-hand side of a rule containing scopes into the set of
      // states that has a transition on the nonterminal in question.
      int nt_root = NIL;
      for ALL_NON_TERMINALS3(nt) {
        states_of[nt] = NULL;
      }
      for ALL_STATES3(state_no, ls->num_states) {
        struct goto_header_type go_to;
        go_to = statset[state_no].go_to;
        for (int i = 1; i <= go_to.size; i++) {
          int symbol = go_to.map[i].symbol;
          if (symbol_seen.raw[symbol]) {
            if (states_of[symbol] == NULL) {
              nt_list.raw[symbol] = nt_root;
              nt_root = symbol;
              num_state_sets = num_state_sets + 1;
            }
             struct node *q = Allocate_node();
            q->value = state_no;
            q->next = states_of[symbol];
            states_of[symbol] = q;
          }
        }
      }
      produced->right_produces.raw += (num_terminals + 1) * dss->non_term_set_size;
      ffree(produced->right_produces.raw);
      produced->left_produces.raw += (num_terminals + 1) * dss->non_term_set_size;
      ffree(produced->left_produces.raw);
      // Next, we used the optimal partition procedure to compress the
      // space used by the sets of states, allocate the SCOPE structure
      // and store the compressed sets of states in it.
      // We also sort the list of items by the length of their prefixes in
      // descending order.  This is done primarily as an optimization.
      // If a longer prefix matches prior to a shorter one, the parsing
      // will terminate quicker.
    process_scope_states: {
        JBitset collection;
        int state_root;
        int state_no_inner;
        long state_set_size = ls->num_states / SIZEOF_BC + (ls->num_states % SIZEOF_BC ? 1 : 0);
        calloc0_set(collection, num_state_sets + 1, state_set_size);
        ArrayLong element_size = Allocate_long_array2(num_state_sets + 1);
        ArrayLong start = Allocate_long_array2(num_state_sets + 2);
        ArrayLong stack = Allocate_long_array2(num_state_sets + 1);
        ArrayShort ordered_symbol = Allocate_short_array2(num_state_sets + 1);
        ArrayLong list = Allocate_long_array2(num_state_sets + 1);
        ArrayShort state_list = Allocate_short_array2(ls->num_states + 1);
        ArrayShort bucket = Allocate_short_array2(max_prefix_length + 1);
        for (int symbol = nt_root, i = 1; symbol != NIL; symbol = nt_list.raw[symbol], i++) {
          list.raw[i] = i;
          ordered_symbol.raw[i] = symbol;
          INIT_BITSET(collection, i);
          element_size.raw[i] = 0;
          struct node *p;
          for (p = states_of[symbol]; p != NULL; p = p->next) {
            element_size.raw[i]++;
            SET_BIT_IN(collection, i, p->value);
          }
        }
        partset(collection, element_size, list, start, stack, num_state_sets, true, ls);
        for (int i = 1; i <= num_state_sets; i++) {
          int symbol = ordered_symbol.raw[i];
          state_index.raw[symbol] = ABS(start.raw[i]);
        }
        sc->scope_state_size = start.raw[num_state_sets + 1] - 1;
        calloc0p(&scope, sc->num_scopes + 1, struct scope_type);
        *scope_right_side = Allocate_long_array2(sc->scope_rhs_size + 1);
        *scope_state = Allocate_short_array2(sc->scope_state_size + 1);
        int k = 0;
        for (int i = 0; i <= ls->num_states; i++) {
          state_list.raw[i] = OMEGA;
        }
        for (int i = 1; i <= num_state_sets; i++) {
          if (start.raw[i] > 0) {
            state_root = 0;
            state_list.raw[state_root] = NIL;
            int j;
            for (bool end_node = (j = i) == NIL; !end_node; end_node = j == i) {
              j = stack.raw[j];
              int symbol = ordered_symbol.raw[j];
              struct node *p;
              for (p = states_of[symbol]; p != NULL; p = p->next) {
                state_no_inner = p->value;
                if (state_list.raw[state_no_inner] == OMEGA) {
                  state_list.raw[state_no_inner] = state_root;
                  state_root = state_no_inner;
                }
              }
            }
            for (state_no_inner = state_root; state_no_inner != NIL; state_no_inner = state_root) {
              state_root = state_list.raw[state_no_inner];
              state_list.raw[state_no_inner] = OMEGA;
              k++;
              scope_state->raw[k] = state_no_inner;
            }
          }
        }
        for (int symbol = nt_root; symbol != NIL; symbol = nt_list.raw[symbol]) {
          struct node *p;
          struct node *q;
          for (p = states_of[symbol]; p != NULL; q = p, p = p->next) {
          }
          free_nodes(states_of[symbol], q);
        }
        // Use the BUCKET array as a base to partition the scoped items
        // based on the length of their prefixes.  The list of items in each
        // bucket is kept in the NEXT_ITEM array sorted in descending order
        // of the length of the right-hand side of the item.
        // Items are kept sorted in that fashion because when two items have
        // the same prefix, we want the one with the shortest suffix to be
        // chosen. In other words, if we have two scoped items, say:
        //
        //    A ::= x . y       and      B ::= x . z     where |y| < |z|
        //
        // and both of them are applicable in a given context with similar
        // result, then we always want A ::= x . y to be used.
        for (int i = 1; i <= max_prefix_length; i++) {
          bucket.raw[i] = NIL;
        }
        for (int item_no = item_root; item_no != NIL; item_no = item_list.raw[item_no]) {
          int tail;
          k = item_table[item_no].dot;
          int ii;
          for (ii = bucket.raw[k]; ii != NIL; tail = ii, ii = next_item.raw[ii]) {
            if (RHS_SIZE(item_table[item_no].rule_number, rules) >=
                RHS_SIZE(item_table[ii].rule_number, rules)) {
              break;
            }
          }
          next_item.raw[item_no] = ii;
          if (ii == bucket.raw[k]) {
            bucket.raw[k] = item_no; /* insert at the beginning */
          } else {
            next_item.raw[tail] = item_no; /* insert in middle or end */
          }
        }
        // Reconstruct list of scoped items in sorted order. Since we want
        // the items in descending order, we start with the smallest bucket
        // proceeding to the largest one and insert the items from each
        // bucket in LIFO order in ITEM_LIST.
        item_root = NIL;
        for (int k = 1; k <= max_prefix_length; k++) {
          for (int item_no = bucket.raw[k]; item_no != NIL; item_no = next_item.raw[item_no]) {
            item_list.raw[item_no] = item_root;
            item_root = item_no;
          }
        }
        ffree(collection.raw);
        ffree(element_size.raw);
        ffree(start.raw);
        ffree(stack.raw);
        ffree(ordered_symbol.raw);
        ffree(state_list.raw);
        ffree(list.raw);
        ffree(bucket.raw);
      } /* End PROCESS_SCOPE_STATES */
      // Next, we initialize the remaining fields of the SCOPE structure.
      int item_no = item_root;
      for (int i = 1; item_no != NIL; i++) {
        scope[i].prefix = prefix_index.raw[item_no];
        scope[i].suffix = suffix_index.raw[item_no];
        int rule_no = item_table[item_no].rule_number;
        scope[i].lhs_symbol = rules[rule_no].lhs;
        int symbol = rhs_sym.raw[rules[rule_no].rhs + item_table[item_no].dot];
        if (IS_A_TERMINAL(symbol)) {
          scope[i].look_ahead = symbol;
        } else {
          for ALL_NON_TERMINALS3(j) {
            symbol_seen.raw[j] = false;
          }
          scope[i].look_ahead = get_shift_symbol(symbol, symbol_seen, clitems, rules, item_table, rhs_sym);
        }
        scope[i].state_set = state_index.raw[scope[i].lhs_symbol];
        item_no = item_list.raw[item_no];
      }
      for (int j = 1; j <= st->top; j++) {
        if (scope_element[j].item < 0) {
          item_no = -scope_element[j].item;
          int rule_no = item_table[item_no].rule_number;
          n = scope_element[j].index;
          for (int k = rules[rule_no].rhs + item_table[item_no].dot - 1; k >= rules[rule_no].rhs; /* symbols before dot*/ k--) {
            scope_right_side->raw[n++] = rhs_sym.raw[k];
          }
        } else {
          item_no = scope_element[j].item;
          int rule_no = item_table[item_no].rule_number;
          n = scope_element[j].index;
          for (int k = rules[rule_no].rhs + item_table[item_no].dot; k < rules[rule_no + 1].rhs; /* symbols after dot */ k++) {
            int symbol = rhs_sym.raw[k];
            if (IS_A_NON_TERMINAL(symbol)) {
              if (!null_nt.raw[symbol]) {
                scope_right_side->raw[n++] = rhs_sym.raw[k];
              }
            } else if (symbol != error_image) {
              scope_right_side->raw[n++] = rhs_sym.raw[k];
            }
          }
        }
        scope_right_side->raw[n] = 0;
      }
      ffree(prefix_index.raw);
      ffree(suffix_index.raw);
      item_of.raw += num_terminals + 1;
      ffree(item_of.raw);
      ffree(next_item.raw);
      symbol_seen.raw += num_terminals + 1;
      ffree(symbol_seen.raw);
      states_of += num_terminals + 1;
      ffree(states_of);
      state_index.raw += num_terminals + 1;
      ffree(state_index.raw);
      ffree(scope_element);
  }
  ffree(stack.raw);
  ffree(index_of.raw);
  ffree(names_map.raw);
  ffree(name_used.raw);
  nt_list.raw += num_terminals + 1;
  ffree(nt_list.raw);
  ffree(set.raw);
  produces.raw += (num_terminals + 1) * dss->non_term_set_size;
  ffree(produces.raw);
  direct_produces += num_terminals + 1;
  ffree(direct_produces);
  ffree(goto_domain);
}

/// For a given symbol, complete the computation of
/// PRODUCES[symbol].
///
/// This procedure is used to compute the transitive closure of
/// the PRODUCES, LEFT_PRODUCES and RIGHT_MOST_PRODUCES maps.
void compute_produces(const int symbol, struct node **direct_produces, ArrayShort stack, ArrayShort index_of, JBitset produces, struct ProduceTop* top_value) {
  stack.raw[++top_value->top] = symbol;
  const int indx = top_value->top;
  index_of.raw[symbol] = indx;
  struct node *q;
  for (struct node *p = direct_produces[symbol]; p != NULL; q = p, p = p->next) {
    int new_symbol = p->value;
    /* first time seen? */
    if (index_of.raw[new_symbol] == OMEGA) {
      compute_produces(new_symbol, direct_produces, stack, index_of, produces, top_value);
    }
    index_of.raw[symbol] = MIN(index_of.raw[symbol], index_of.raw[new_symbol]);
    SET_UNION(produces, symbol, produces, new_symbol);
  }
  if (direct_produces[symbol] != NULL) {
    free_nodes(direct_produces[symbol], q);
  }
  /* symbol is SCC root */
  if (index_of.raw[symbol] == indx) {
    for (int new_symbol = stack.raw[top_value->top]; new_symbol != symbol; new_symbol = stack.raw[--top_value->top]) {
      ASSIGN_SET(produces, new_symbol, produces, symbol);
      index_of.raw[new_symbol] = INFINITY;
    }
    index_of.raw[symbol] = INFINITY;
    top_value->top--;
  }
}

// endregion














// region mkstates

/// STATE_ELEMENT is used to represent states. Each state is mapped into a
/// unique number. The components QUEUE and LINK are auxiliary:
struct state_element {
  // LINK is used to resolve collisions in hashing the states.
  struct state_element *link;
  // QUEUE is used to form a sequential linked-list of the states ordered
  struct state_element *queue;
  // NEXT_SHIFT is used to resolve collisions in hashing SHIFT maps.
  struct state_element *next_shift;
  struct node *kernel_items;
  struct node *complete_items;
  struct shift_header_type lr0_shift;
  struct goto_header_type lr0_goto;
  short shift_number;
  // STATE_NUMBER and identified by the variable STATE_ROOT.
  short state_number;
};

struct StateContainer {
  struct state_element *state_root;
  struct state_element *state_tail;
};

/// LR0_STATE_MAP takes as an argument a pointer to a kernel set of items. If
/// no state based on that kernel set already exists, then a new one is
/// created and added to STATE_TABLE. In any case, a pointer to the STATE of
/// the KERNEL is returned.
struct state_element *lr0_state_map(struct node *kernel, struct state_element **state_table, struct StateContainer* sc, struct LAState* ls) {
  unsigned long hash_address = 0;
  // Compute the hash address.
  for (const struct node *p = kernel; p != NULL; p = p->next) {
    hash_address += p->value;
  }
  hash_address %= STATE_TABLE_SIZE;
  // Check whether a state is already defined by the KERNEL set.
  for (struct state_element *state_ptr = state_table[hash_address];state_ptr != NULL; state_ptr = state_ptr->link) {
    struct node *q;
    struct node *r;
    struct node *p;
    for (p = state_ptr->kernel_items, q = kernel; p != NULL && q != NULL; p = p->next, r = q, q = q->next) {
      if (p->value != q->value)
        break;
    }
    // Both P and Q are NULL?
    if (p == q) {
      free_nodes(kernel, r);
      return state_ptr;
    }
  }
  // Add a new state based on the KERNEL set.
  ls->num_states++;
  struct state_element *ptr;
  talloc0p(&ptr, struct state_element);
  ptr->queue = NULL;
  ptr->kernel_items = kernel;
  ptr->complete_items = NULL;
  ptr->state_number = ls->num_states;
  ptr->link = state_table[hash_address];
  state_table[hash_address] = ptr;
  if (sc->state_root == NULL) {
    sc->state_root = ptr;
  } else {
    sc->state_tail->queue = ptr;
  }
  sc->state_tail = ptr;
  return ptr;
}

/// This procedure constructs an LR(0) automaton.
void mklr0(struct CLIOptions *cli_options, struct shift_header_type* no_shifts_ptr, struct goto_header_type* no_gotos_ptr, struct node **clitems, struct node **closure, struct SRTable* srt, struct ruletab_type *rules, struct itemtab *item_table, struct StatSet* ss, struct LAState* ls) {
  // STATE_TABLE is the array used to hash the states. States are
  // identified by their Kernel set of items. Hash locations are
  // computed for the states. As states are inserted in the table,
  // they are threaded together via the QUEUE component of
  // STATE_ELEMENT. The variable STATE_ROOT points to the root of
  // the thread, and the variable STATE_TAIL points to the tail.
  //
  //   After constructing a state, Shift and Goto actions are
  // defined on that state based on a partition of the set of items
  // in that state. The partitioning is based on the symbols
  // following the dot in the items. The array PARTITION is used
  // for that partitioning. LIST and ROOT are used to construct
  // temporary lists of symbols in a state on which Shift or Goto
  // actions are defined.
  // NT_LIST and NT_ROOT are used to build temporary lists of non-terminals.
  // Set up a pool of temporary space.
  reset_temporary_space();
  ArrayShort list = Allocate_short_array2(num_symbols + 1);
  ArrayShort shift_action = Allocate_short_array2(num_terminals + 1);
  ArrayShort shift_list = Allocate_short_array2(num_terminals + 1);
  ArrayShort nt_list = Allocate_short_array2(num_non_terminals);
  nt_list.raw -= num_terminals + 1;
  struct node **partition;
  calloc0p(&partition, num_symbols + 1, struct node *);
  struct state_element **state_table;
  calloc0p(&state_table, STATE_TABLE_SIZE, struct state_element *);
  struct state_element **shift_table;
  calloc0p(&shift_table, SHIFT_TABLE_SIZE, struct state_element *);
  // INITIALIZATION -----------------------------------------------------------
  int goto_size = 0;
  int shift_size = 0;
  struct StateContainer sc = (struct StateContainer) {
    .state_root = NULL,
    .state_tail = NULL,
  };
  for (int i = 0; i <= num_terminals; i++) {
    shift_action.raw[i] = OMEGA;
  }
  int nt_root = NIL;
  for ALL_NON_TERMINALS3(i) {
    nt_list.raw[i] = OMEGA;
  }
  // PARTITION, STATE_TABLE and SHIFT_TABLE are initialized by calloc
  //
  // END OF INITIALIZATION ----------------------------------------------------
  //
  // Kernel of the first state consists of the first items in each
  // rule produced by Accept non-terminal.
  struct node *q = NULL;
  struct node *p;
  for (bool end_node = (p = clitems[accept_image]) == NULL; !end_node; /* Loop over circular list */ end_node = p == clitems[accept_image]) {
    p = p->next;
    struct node *new_item = Allocate_node();
    new_item->value = p->value;
    new_item->next = q;
    q = new_item;
  }
  // Insert first state in STATE_TABLE and keep constructing states
  // until we no longer can.
  for (struct state_element *state = lr0_state_map(q, state_table, &sc, ls); /* insert initial state */ state != NULL; /* and process next state until no more */ state = state->queue) {
    // Now we construct a list of all non-terminals that can be
    // introduced in this state through closure.  The CLOSURE of each
    // non-terminal has been previously computed in MKFIRST.
    for (q = state->kernel_items; q != NULL; /* iterate over kernel set of items */ q = q->next) {
      int item_no = q->value;
      int symbol = item_table[item_no].symbol; /* symbol after dot */
      if (IS_A_NON_TERMINAL(symbol)) /* Dot symbol */
      {
        if (nt_list.raw[symbol] == OMEGA) /* not yet seen */
        {
          nt_list.raw[symbol] = nt_root;
          nt_root = symbol;
          for (bool end_node = (p = closure[symbol]) == NULL; !end_node; /* Loop over circular list */ end_node = p == closure[symbol]) {
            // add its closure to list
            p = p->next;
            if (nt_list.raw[p->value] == OMEGA) {
              nt_list.raw[p->value] = nt_root;
              nt_root = p->value;
            }
          }
        }
      }
    }
    // We now construct lists of all start items that the closure
    // non-terminals produce. A map from each non-terminal to its set
    // start items has previously been computed in MKFIRST. (CLITEMS)
    // Empty items are placed directly in the state, whereas non_empty
    // items are placed in a temporary list rooted at CLOSURE_ROOT.
    struct node *closure_root = NULL; /* used to construct list of closure items */
    struct node *closure_tail;
    for (int symbol = nt_root; symbol != NIL; nt_list.raw[symbol] = OMEGA, symbol = nt_root) {
      nt_root = nt_list.raw[symbol];
      for (bool end_node = (p = clitems[symbol]) == NULL; !end_node; /* Loop over circular list */ end_node = p == clitems[symbol]) {
        p = p->next;
        int item_no = p->value;
        struct node *new_item = Allocate_node();
        new_item->value = item_no;
        if (item_table[item_no].symbol == empty) /* complete item */
        {
          // Add to COMPLETE_ITEMS set in state
          new_item->next = state->complete_items;
          state->complete_items = new_item;
        } else {
          // closure item, add to closure list
          if (closure_root == NULL) {
            closure_root = new_item;
          } else {
            closure_tail->next = new_item;
          }
          closure_tail = new_item;
        }
      }
    }
    struct node *item_ptr;
    /* any non-complete closure items? */
    if (closure_root != NULL) {
      // construct list of them and kernel items
      closure_tail->next = state->kernel_items;
      item_ptr = closure_root;
    } else {
      /* else just consider kernel items */
      item_ptr = state->kernel_items;
    }
    // In this loop, the PARTITION map is constructed. At this point,
    // ITEM_PTR points to all the non_complete items in the closure of
    // the state, plus all the kernel items.  We note that the kernel
    // items may still contain complete-items, and if any is found, the
    // COMPLETE_ITEMS list is updated.
    int root = NIL;
    for (; item_ptr != NULL; item_ptr = item_ptr->next) {
      int item_no = item_ptr->value;
      int symbol = item_table[item_no].symbol;
      /* incomplete item */
      if (symbol != empty) {
        int next_item_no = item_no + 1;
        if (partition[symbol] == NULL) {
          // PARTITION not defined on symbol
          list.raw[symbol] = root; /* add to list */
          root = symbol;
          if (IS_A_TERMINAL(symbol)) /* Update transition count */
          {
            shift_size++;
          } else {
            goto_size++;
          }
        }
        struct node *tail;
        for (p = partition[symbol]; p != NULL; tail = p, p = p->next) {
          if (p->value > next_item_no) {
            break;
          }
        }
        struct node *r = Allocate_node();
        r->value = next_item_no;
        r->next = p;
        if (p == partition[symbol]) /* Insert at beginning */
        {
          partition[symbol] = r;
        } else {
          tail->next = r;
        }
      } else {
        /* Update complete item set with item from kernel */
        p = Allocate_node();
        p->value = item_no;
        p->next = state->complete_items;
        state->complete_items = p;
      }
    }
    if (closure_root != NULL) {
      free_nodes(closure_root, closure_tail);
    }
    // We now iterate over the set of partitions and update the state
    // automaton and the transition maps: SHIFT and GOTO. Each
    // partition represents the kernel of a state.
    struct goto_header_type go_to;
    if (goto_size > 0) {
      go_to = Allocate_goto_map(goto_size);
      state->lr0_goto = go_to;
    } else {
      state->lr0_goto = *no_gotos_ptr;
    }
    int shift_root = NIL;
    for (int symbol = root; symbol != NIL; /* symbols on which transition is defined */ symbol = list.raw[symbol]) {
      short action = OMEGA;
      // If the partition contains only one item, and it is adequate
      // (i.e. the dot immediately follows the last symbol), and
      // READ-REDUCE is requested, a new state is not created, and the
      // action is marked as a Shift-reduce or a Goto-reduce. Otherwise
      // if a state with that kernel set does not yet exist, we create
      // it.
      q = partition[symbol]; /* kernel of a new state */
      if (cli_options->read_reduce_bit && q->next == NULL) {
        int item_no = q->value;
        if (item_table[item_no].symbol == empty) {
          int rule_no = item_table[item_no].rule_number;
          if (rules[rule_no].lhs != accept_image) {
            action = -rule_no;
            free_nodes(q, q);
          }
        }
      }
      /* Not a Read-Reduce action */
      if (action == OMEGA) {
        struct state_element *new_state = lr0_state_map(q, state_table, &sc, ls);
        action = new_state->state_number;
      }
      // At this stage, the partition list has been freed (for an old
      // state or an ADEQUATE item), or used (for a new state).  The
      // PARTITION field involved should be reset.
      partition[symbol] = NULL; /* To be reused */
      // At this point, ACTION contains the value of the state to Shift
      // to, or rule to Read-Reduce on. If the symbol involved is a
      // terminal, we update the Shift map; else, it is a non-terminal
      // and we update the Goto map.
      // Shift maps are constructed temporarily in SHIFT_ACTION.
      // Later, they are inserted into a map of unique Shift maps, and
      // shared by states that contain identical shifts.
      // Since the lookahead set computation is based on the GOTO maps,
      // all these maps and their element maps should be kept as
      // separate entities.
      if (IS_A_TERMINAL(symbol)) {
        /* terminal? add to SHIFT map */
        shift_action.raw[symbol] = action;
        shift_list.raw[symbol] = shift_root;
        shift_root = symbol;
        if (action > 0) {
          num_shifts++;
        } else {
          num_shift_reduces++;
        }
      }
      // NOTE that for Goto's we update the field LA_PTR of GOTO. This
      // field will be used later in the routine MKRDCTS to point to a
      // look-ahead set.
      else {
        go_to.map[goto_size].symbol = symbol; /* symbol field */
        go_to.map[goto_size].action = action; /* state field  */
        go_to.map[goto_size].laptr = OMEGA; /* la_ptr field */
        goto_size--;
        if (action > 0) {
          num_gotos++;
        } else {
          num_goto_reduces++;
        }
      }
    }
    // We are now going to update the set of Shift-maps. Ths idea is
    // to do a look-up in a hash table based on SHIFT_TABLE to see if
    // the Shift map associated with the current state has already been
    // computed. If it has, we simply update the SHIFT_NUMBER and the
    // SHIFT field of the current state. Otherwise, we allocate and
    // construct a SHIFT_ELEMENT map, update the current state, and
    // add it to the set of Shift maps in the hash table.
    //   Note that the SHIFT_NUMBER field in the STATE_ELEMENTs could
    // have been factored out and associated instead with the
    // SHIFT_ELEMENTs. That would have saved some space, but only in
    // the short run. This field was purposely stored in the
    // STATE_ELEMENTs, because once the states have been constructed,
    // they are not kept, whereas the SHIFT_ELEMENTs are kept.
    //    One could have also threaded through the states that contain
    // original shift maps to avoid duplicate assignments in
    // creating the SHIFT map later. However, this would have
    // increased the storage requirement, and would probably have saved
    // (at most) a totally insignificant amount of time.
  update_shift_maps: {
      unsigned long hash_address;
      hash_address = shift_size;
      for (int symbol = shift_root; symbol != NIL; symbol = shift_list.raw[symbol]) {
        hash_address += ABS(shift_action.raw[symbol]);
      }
      struct shift_header_type sh;
      struct state_element *p_inner;
      hash_address %= SHIFT_TABLE_SIZE;
      for (p_inner = shift_table[hash_address];
           p_inner != NULL; /* Search has table for shift map */
           p_inner = p_inner->next_shift) {
        sh = p_inner->lr0_shift;
        if (sh.size == shift_size) {
          int ii;
          // Compare shift maps
          for (ii = 1; ii <= shift_size; ii++) {
            if (sh.map[ii].action != shift_action.raw[sh.map[ii].symbol]) {
              break;
            }
          }
          // Are they equal ?
          if (ii > shift_size) {
            state->lr0_shift = sh;
            state->shift_number = p_inner->shift_number;
            for (int symbol = shift_root; symbol != NIL; symbol = shift_list.raw[symbol]) {
              // Clear SHIFT_ACTION
              shift_action.raw[symbol] = OMEGA;
            }
            shift_size = 0; /* Reset for next round */
            goto leave_update_shift_maps;
          }
        }
      }
      if (shift_size > 0) {
        sh = Allocate_shift_map(shift_size);
        num_shift_maps++;
        state->shift_number = num_shift_maps;
      } else {
        state->shift_number = 0;
        sh = *no_shifts_ptr;
      }
      state->lr0_shift = sh;
      state->next_shift = shift_table[hash_address];
      shift_table[hash_address] = state;
      for (int symbol = shift_root; symbol != NIL; symbol = shift_list.raw[symbol]) {
        sh.map[shift_size].symbol = symbol;
        sh.map[shift_size].action = shift_action.raw[symbol];
        shift_action.raw[symbol] = OMEGA;
        shift_size--;
      }
    } /*end update_shift_maps */
  leave_update_shift_maps:;
  }
  // Construct STATSET, a "compact" and final representation of
  // State table, and SHIFT which is a set of all shift maps needed.
  // NOTE that assignments to elements of SHIFT may occur more than
  // once, but that's ok. It is probably faster to do that than to
  // set up an elaborate scheme to avoid the multiple assignment which
  // may in fact cost more. Look at it this way: it is only a pointer
  // assignment, namely a Load and a Store.
  // Release all NODEs used by the maps CLITEMS and CLOSURE.
  {
    int state_no;
    struct state_element *p_inner;
    // If the grammar is LALR(k), k > 1, more states may be added and
    // the size of the shift map increased.
    calloc0p(&srt->shift, ls->num_states + 1, struct shift_header_type);
    srt->shift[0] = *no_shifts_ptr; /* MUST be initialized for LALR(k) */
    calloc0p(&ss->statset, ls->num_states + 1, struct statset_type);
    for (p_inner = sc.state_root; p_inner != NULL; p_inner = p_inner->queue) {
      state_no = p_inner->state_number;
      ss->statset[state_no].kernel_items = p_inner->kernel_items;
      ss->statset[state_no].complete_items = p_inner->complete_items;
      srt->shift[p_inner->shift_number] = p_inner->lr0_shift;
      ss->statset[state_no].shift_number = p_inner->shift_number;
      ss->statset[state_no].go_to = p_inner->lr0_goto;
    }
  }
  ffree(list.raw);
  ffree(shift_action.raw);
  ffree(shift_list.raw);
  nt_list.raw += num_terminals + 1;
  ffree(nt_list.raw);
  ffree(partition);
  ffree(state_table);
  ffree(shift_table);
}

/// In this procedure, we first construct the LR(0) automaton.
void mkstats(struct CLIOptions *cli_options, struct DetectedSetSizes* dss, JBitset first, struct scope_type *scope, struct node **clitems, struct node **closure, struct SRTable* srt, ArrayLong* scope_right_side, ArrayBool null_nt, ArrayShort *scope_state, struct itemtab *item_table, struct ruletab_type *rules, ArrayShort rhs_sym, ArrayShort* gd_range, ArrayShort*gd_index, struct StatSet* ss, struct ScopeCounter* sc, struct symno_type *symno, char *string_table, int *name, struct LAState* ls) {
  struct ScopeTop st = (struct ScopeTop) {
    .top = 0
  };
  struct goto_header_type no_gotos_ptr = (struct goto_header_type) {
    /* For states with no GOTOs */
    .size = 0,
    .map = NULL,
  };
  struct shift_header_type no_shifts_ptr = (struct shift_header_type) {
    /* For states with no SHIFTs */
    .size = 0,
    .map = NULL,
  };
  mklr0(cli_options, &no_shifts_ptr, &no_gotos_ptr, clitems, closure, srt, rules, item_table, ss, ls);
  struct Produced produced = {};
  if (cli_options->error_maps_bit && (cli_options->table_opt.value == OPTIMIZE_TIME.value || cli_options->table_opt.value == OPTIMIZE_SPACE.value)) {
    produce(cli_options, dss, &produced, &st, first, scope, clitems, null_nt, scope_right_side, rules, scope_state, ss->statset, item_table, rhs_sym, gd_range, gd_index, symno, sc, string_table, name, ls);
  }
  // Free space trapped by the CLOSURE and CLITEMS maps.
  for ALL_NON_TERMINALS3(j) {
    struct node *p;
    struct node *q = clitems[j];
    if (q != NULL) {
      p = q->next;
      free_nodes(p, q);
    }
    q = closure[j];
    if (q != NULL) {
      p = q->next;
      free_nodes(p, q);
    }
  }
  closure += num_terminals + 1;
  ffree(closure);
  clitems += num_terminals + 1;
  ffree(clitems);
}

// endregion

























// region mkred

/// The structure STATE_ELEMENT is used to construct lookahead states.
/// LA_STATE_ROOT point to a list of lookahead states using the LINK
/// field. The field NEXT_SHIFT is used to hash the new shift maps
/// associated with lookahead states. The field IN_STATE identifies the
/// state that shifted into the lookahead state in question. The field
/// SYMBOL identifies the symbol on shift the transition was made into
/// the lookahead state in question.  The remaining fields are
/// self-explanatory.
struct red_state_element {
  struct red_state_element *link;
  struct red_state_element *next_shift;
  struct reduce_header_type reduce;
  struct shift_header_type shift;
  short in_state;
  short symbol;
  short state_number;
  short shift_number;
};

/// The structures SR_CONFLICT_ELEMENT and RR_CONFLICT_ELEMENT are used
/// th store conflict information. CONFLICT_ELEMENT_POOL is used to
/// keep track of a pool conflict element structures (SR or RR) that
/// are available for allocation.
/// See routines ALLOCATE_CONFLICT_ELEMENT and FREE_CONFLICT_ELEMENTS.
struct sr_conflict_element {
  struct sr_conflict_element *next;
  short state_number;
  short item;
  short symbol;
};

struct rr_conflict_element {
  struct rr_conflict_element *next;
  short symbol;
  short item1;
  short item2;
};

/// VISITED is a structure used to mark state-symbol pairs that have
/// been visited in the process of computing follow-sources for a
/// given action in conflict.
/// The field MAP is an array indexable by the states 1..NUM_STATES;
/// each element of which points to a set (list) of symbols. Thus, for
/// a given state S, and for all symbol X in the list MAP[S], [S,X]
/// has been visited. For efficiency, the fields LIST and ROOT are used
/// to store the set (list) of indexed elements of MAP that are not
/// NULL.
/// See routines MARK_VISITED, WAS_VISITED, CLEAR_VISITED,
///              INIT_LALRK_PROCESS, EXIT_PROCESS
struct visited_element {
  struct node **map;
  ArrayShort list;
  short root;
};

struct ConflictPool {
  void *conflict_element_pool;
};

struct StackPool {
  struct stack_element *stack_pool;
  struct stack_element *dangling_stacks;
};

struct STRS {
  struct red_state_element *la_state_root;
  /// The variable HIGHEST_LEVEL is used to indicate the highest number of
  /// lookahead that was necessary to resolve all conflicts for a given
  /// grammar. If we can detect that the grammar is not LALR(k), we set
  /// HIGHEST_LEVEL to INFINITY.
  int highest_level;
};

/// Given a STATE_NO and an ITEM_NO, ACCESS computes the set of states where
/// the rule from which ITEM_NO is derived was introduced through closure.
struct node *lpgaccess(const int state_no, const int item_no, struct node **in_stat, struct itemtab *item_table) {
  // Build a list pointed to by ACCESS_ROOT originally consisting
  // only of STATE_NO.
  struct node *access_root = Allocate_node();
  access_root->value = state_no;
  access_root->next = NULL;
  for (int i = item_table[item_no].dot; i > 0; i--) /*distance to travel is DOT */
  {
    struct node *head = access_root; /* Save old ACCESS_ROOT */
    access_root = NULL; /* Initialize ACCESS_ROOT for new list */
    struct node *tail;
    for (struct node *p = head; p != NULL; tail = p, p = p->next) {
      // Compute set of states with transition into p->value.
      struct node *s;
      for (bool end_node = (s = in_stat[p->value]) == NULL; !end_node; end_node = s == in_stat[p->value]) {
        s = s->next;
        struct node *q = Allocate_node();
        q->value = s->value;
        q->next = access_root;
        access_root = q;
      }
    }
    free_nodes(head, tail); /* free previous list */
  }
  return access_root;
}

/// This function allocates a conflict_element (sr or rr) structure
/// & returns a pointer to it. If there are nodes in the free pool,
/// one of them is returned. Otherwise, a new node is allocated
/// from the temporary storage pool.
static void *allocate_conflict_element(struct ConflictPool* cp) {
  void *p = cp->conflict_element_pool;
  if (p != NULL) {
    cp->conflict_element_pool = ((struct sr_conflict_element *) p)->next;
  } else {
    talloc0p_raw(&p, void, MAX(sizeof(struct sr_conflict_element), sizeof(struct rr_conflict_element)));
  }
  return p;
}

/// This routine returns a list of conflict_element (sr/rr)structures
/// to the free pool.
static void free_conflict_elements(void *head, void *tail, struct ConflictPool* cp) {
  ((struct sr_conflict_element *) tail)->next = (struct sr_conflict_element *) cp->conflict_element_pool;
  cp->conflict_element_pool = head;
}

/// This function allocates a stack_element structure and returns a
/// pointer to it. If there are nodes in the free pool, one of them
/// is returned. Otherwise, a new node is allocated from the
/// temporary storage pool.
static struct stack_element *allocate_stack_element(struct StackPool* sp) {
  struct stack_element *p = sp->stack_pool;
  if (p != NULL) {
    sp->stack_pool = p->next;
  } else {
    talloc0p(&p, struct stack_element);
  }
  return p;
}

/// This routine returns a list of stack_element structures to the
/// free pool.
static void free_stack_elements(struct stack_element *head, struct stack_element *tail, struct StackPool* sp) {
  tail->next = sp->stack_pool;
  sp->stack_pool = head;
}

/// When an allocated stack_element structure is not directly associated
/// with an action, it is added to a circular list of dangling stack_element
/// nodes so that its space can be reclaimed.
static void add_dangling_stack_element(struct stack_element *s, struct StackPool* sp) {
  if (sp->dangling_stacks == NULL) {
    s->next = s;
  } else {
    s->next = sp->dangling_stacks->next;
    sp->dangling_stacks->next = s;
  }
  sp->dangling_stacks = s;
}

/// This function is invoked to free up all dangling stack_element nodes
/// and reset the dangling stack list.
/// Recall that the dangling stack list is circular.
static void free_dangling_stack_elements(struct StackPool* sp) {
  if (sp->dangling_stacks != NULL) {
    struct stack_element *tail = sp->dangling_stacks;
    free_stack_elements(sp->dangling_stacks->next, tail, sp);
    sp->dangling_stacks = NULL;
  }
}

/// This function allocates and initializes a SOURCE_ELEMENT map.
/// See definition of SOURCE_ELEMENT above.
static struct sources_element allocate_sources(const struct LAState* ls) {
  struct sources_element sources;
  calloc0p(&sources.configs, num_rules + num_rules + ls->num_states + 1, struct stack_element *);
  sources.configs += num_rules;
  calloc0p(&sources.stack_seen, STATE_TABLE_SIZE, struct stack_element *);
  sources.list = Allocate_short_array2(num_rules + num_rules + ls->num_states + 1);
  sources.list.raw += num_rules;
  sources.root = NIL;
  return sources;
}

/// This function takes as argument a SOURCES_ELEMENT structure which it
/// resets to the empty map.
/// See definition of SOURCE_ELEMENT above.
static struct sources_element clear_sources(struct sources_element sources, struct StackPool* sp) {
  struct stack_element *tail;
  for (int act = sources.root; act != NIL; act = sources.list.raw[act]) {
    for (struct stack_element *p = sources.configs[act]; p != NULL; tail = p, p = p->next) {
    }
    free_stack_elements(sources.configs[act], tail, sp);
    sources.configs[act] = NULL;
  }
  sources.root = NIL;
  return sources;
}

/// This function takes as argument a SOURCES_ELEMENT structure. First, it
/// clears it to reclaim all space that was used by STACK_ELEMENTs and then
/// it frees the array space used as a base to construct the map.
static void free_sources(struct sources_element sources, struct StackPool* sp) {
  sources = clear_sources(sources, sp);
  sources.configs -= num_rules;
  ffree(sources.configs);
  ffree(sources.stack_seen);
  sources.list.raw -= num_rules;
  ffree(sources.list.raw);
}

/// This function takes as argument two pointers to sorted lists of stacks.
/// It merges the lists in the proper order and returns the resulting list.
static struct stack_element *union_config_sets(struct stack_element *root1, struct stack_element *root2, struct StackPool* sp) {
  struct stack_element *root = NULL;
  // This loop iterates over both lists until one (or both) has been
  // completely processed. Each time around the loop, a stack is
  // removed from one of the lists and possibly added to the new
  // list. The new list is initially kept as a circular list to
  // preserve the sorted ordering in which elements are added to it.
  while (root1 != NULL && root2 != NULL) {
    // Compare the two stacks in front of the lists for equality.
    // We exit this loop when we encounter the end of one (or both)
    // of the stacks or two elements in them that are not the same.
    struct stack_element *p1;
    struct stack_element *p2;
    for (p1 = root1, p2 = root2;
         p1 != NULL && p2 != NULL;
         p1 = p1->previous, p2 = p2->previous) {
      if (p1->state_number != p2->state_number)
        break;
    }
    // We now have 3 cases to consider:
    //    1. The two stacks are equal? Discard one!
    //    2. List 1 stack is prefix of list 2 stack (p1 == NULL)?
    //       or list 1 stack is less than list 2 stack?
    //       Remove list 1 stack and add it to new list.
    //    3. List 2 stack is either a prefix of list 1 stack, or
    //       it is smaller!
    //       Remove list 2 stack and add it to new list.
    if (p1 == p2) {
      // are both p1 and p2 NULL?
      p2 = root2;
      root2 = root2->next;
      add_dangling_stack_element(p2, sp);
    } else if (p1 == NULL || (p2 != NULL && p1->state_number < p2->state_number)) {
      p1 = root1;
      root1 = root1->next;
      if (root == NULL) {
        p1->next = p1;
      } else {
        p1->next = root->next;
        root->next = p1;
      }
      root = p1;
    } else {
      p2 = root2;
      root2 = root2->next;
      if (root == NULL) {
        p2->next = p2;
      } else {
        p2->next = root->next;
        root->next = p2;
      }
      root = p2;
    }
  }
  // At this stage, at least one (or both) list has been expended
  // (or was empty to start with).
  // If the new list is not empty, turn it into a linear list and
  // append the unexpended list to it, if any.
  // Otherwise, set the new list to the nonempty list if any!
  if (root != NULL) {
    struct stack_element *tail = root;
    root = root->next;
    tail->next = root1 == NULL ? root2 : root1;
  } else {
    root = root1 == NULL ? root2 : root1;
  }
  return root;
}

/// This function takes as argument a SOURCES_ELEMENT map, an ACTION and a
/// set (sorted list) of configurations. It adds the set of configurations
/// to the previous set of configurations associated with the ACTION in the
/// SOURCES_ELEMENT map.
static struct sources_element add_configs(struct sources_element sources, const int action, struct stack_element *config_root, struct StackPool* sp) {
  if (config_root != NULL) {
    if (sources.configs[action] == NULL) {
      // The previous was empty?
      sources.list.raw[action] = sources.root;
      sources.root = action;
    }
    sources.configs[action] = union_config_sets(sources.configs[action], config_root, sp);
  }
  return sources;
}

/// This function clears out all external space used by the VISITED set and
/// resets VISITED to the empty set.
static void clear_visited(struct visited_element* visited) {
  for (int state_no = visited->root; state_no != NIL; state_no = visited->list.raw[state_no]) {
    struct node *tail;
    for (struct node *p = visited->map[state_no]; p != NULL; tail = p, p = p->next) {
    }
    free_nodes(visited->map[state_no], tail);
    visited->map[state_no] = NULL;
  }
  visited->root = NIL;
}

/// This boolean function checks whether a given pair [state, symbol]
/// was already inserted in the VISITED set.
static bool was_visited(const int state_no, const int symbol, struct visited_element* visited) {
  struct node *p;
  for (p = visited->map[state_no]; p != NULL; p = p->next) {
    if (p->value == symbol) {
      break;
    }
  }
  return p != NULL;
}

/// This function inserts a given pair [state, symbol] into the VISITED set.
static void mark_visited(const int state_no, const int symbol, struct visited_element* visited) {
  if (visited->map[state_no] == NULL) {
    // 1st time we see state_no?
    visited->list.raw[state_no] = visited->root;
    visited->root = state_no;
  }
  struct node *p = Allocate_node();
  p->value = symbol;
  p->next = visited->map[state_no];
  visited->map[state_no] = p;
}

struct CyclicTop {
  int top;
};

/// This procedure is a modified instantiation of the digraph algorithm
/// to compute the CYCLIC set of states.
static void compute_cyclic(const short state_no, ArrayShort stack, ArrayShort index_of, ArrayBool cyclic, struct CyclicTop* topp, ArrayBool null_nt, struct statset_type *statset) {
  stack.raw[++topp->top] = state_no;
  const int indx = topp->top;
  cyclic.raw[state_no] = false;
  index_of.raw[state_no] = indx;
  const struct goto_header_type go_to = statset[state_no].go_to;
  for (int i = 1; i <= go_to.size; i++) {
    const int symbol = go_to.map[i].symbol;
    int act = go_to.map[i].action;
    if (act > 0 && null_nt.raw[symbol]) {
      // We have a transition on a nullable nonterminal?
      if (index_of.raw[act] == OMEGA) {
        compute_cyclic(act, stack, index_of, cyclic, topp, null_nt, statset);
      } else if (index_of.raw[act] != INFINITY) {
        cyclic.raw[state_no] = true;
      }
      cyclic.raw[state_no] = cyclic.raw[state_no] || cyclic.raw[act];
      index_of.raw[state_no] = MIN(index_of.raw[state_no], index_of.raw[act]);
    }
  }
  if (index_of.raw[state_no] == indx) {
    int act;
    do {
      act = stack.raw[topp->top--];
      index_of.raw[act] = INFINITY;
    } while (act != state_no);
  }
}

///    In tracing an error, we will be moving backward in the state
/// automaton looking for items with the conflict symbol as look-ahead.
/// In the case of SLR, we may have to analoguously look at an
/// arbitrary set of items involved.  In moving around these graphs, it
/// is possible to encounter a cycle, in which case, we simply want to
/// back out of the cycle and try another path. We therefore need to
/// keep track of which nodes have already been visited.  For LALR
/// conflicts, we use the LA_PTR field of the GOTO_ELEMENTs as an index
/// to a BOOLEAN array LALR_VISITED.  For SLR conflicts, a boolean
/// array, SLR_VISITED, indexable by non-terminals, is used.  For
/// trace-backs to the root item, the boolean array SYMBOL_SEEN, also
/// indexable by non-terminals, is used.
static bool trace_root(const long lhs_symbol, struct CLIOptions* cli_options, ArrayBool symbol_seen, ArrayShort item_list, ArrayShort nt_items, struct ruletab_type *rules, struct itemtab *item_table, ArrayShort rhs_sym, char *string_table, struct symno_type *symno) {
  if (lhs_symbol == accept_image) {
    return true;
  }
  if (symbol_seen.raw[lhs_symbol]) {
    return false;
  }
  symbol_seen.raw[lhs_symbol] = true;
  for (long item = nt_items.raw[lhs_symbol]; item != NIL; item = item_list.raw[item]) {
    if (trace_root(rules[item_table[item].rule_number].lhs, cli_options, symbol_seen, item_list, nt_items, rules, item_table, rhs_sym, string_table, symno)) {
      print_item(item, cli_options, rules, item_table, rhs_sym, string_table, symno);
      return true;
    }
  }
  return false;
}

/// The procedure below is invoked to retrace a path from the initial
/// item to a given item (ITEM_NO) passed to it as argument.
static void print_root_path(const long item_no, struct CLIOptions* cli_options, ArrayShort item_list, ArrayShort nt_items, struct ruletab_type *rules, struct itemtab *item_table, ArrayShort rhs_sym, char *string_table, struct symno_type *symno) {
  ArrayBool symbol_seen = Allocate_bool_array2(num_non_terminals);
  symbol_seen.raw -= num_terminals + 1;
  if (trace_root(rules[item_table[item_no].rule_number].lhs, cli_options, symbol_seen, item_list, nt_items, rules, item_table, rhs_sym, string_table, symno)) {
    printf("\n"); /* Leave one blank line after root trace. */
  }
  symbol_seen.raw += num_terminals + 1;
  ffree(symbol_seen.raw);
}

/// This procedure takes as argument, a state number, STATE_NO, an
/// index into the goto map of state_no, GOTO_INDX, which identifies a
/// starting point for a search for the CONFLICT_SYMBOL. It attempts to
/// find a path in the automaton (from the starting point) that leads
/// to a state where the conflict symbol can be read. If a path is
/// found, all items along the path are printed and SUCCESS is returned.
///  Otherwise, FAILURE is returned.
static bool lalr_path_retraced(const int state_no, const int goto_indx, const int conflict_symbol, struct CLIOptions *cli_options, ArrayBool lalr_visited, ArrayShort item_list, ArrayShort nt_items, JBitset first, struct node **adequate_item, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat, struct statset_type *statset, ArrayShort rhs_sym, char *string_table, struct symno_type *symno) {
  struct goto_header_type go_to = statset[state_no].go_to;
  lalr_visited.raw[go_to.map[goto_indx].laptr] = true;
  bool found = false;
  const int state = go_to.map[goto_indx].action;
  int item;
  for (const struct node *p = state > 0 ? statset[state].kernel_items : adequate_item[-state]; p != NULL && !found; p = p->next) {
    item = p->value - 1;
    if (IS_IN_SET(first, item_table[item].suffix_index, conflict_symbol)) {
      // Conflict_symbol can be read in state?
      if (cli_options->trace_opt.value == TRACE_FULL.value) {
        print_root_path(item, cli_options, item_list, nt_items, rules, item_table, rhs_sym, string_table, symno);
      }
      found = true;
    } else if (IS_IN_SET(first, item_table[item].suffix_index, empty)) {
      const long symbol = rules[item_table[item].rule_number].lhs;
      struct node *w = lpgaccess(state_no, item, in_stat, item_table);
      struct node *q;
      struct node *tail;
      for (q = w; q != NULL; tail = q, q = q->next) {
        go_to = statset[q->value].go_to;
        int ii;
        for (ii = 1; go_to.map[ii].symbol != symbol; ii++) {
        }
        if (!lalr_visited.raw[go_to.map[ii].laptr]) {
          if (lalr_path_retraced(q->value, ii, conflict_symbol, cli_options, lalr_visited, item_list, nt_items, first, adequate_item, rules, item_table, in_stat, statset, rhs_sym, string_table, symno)) {
            found = true;
            break;
          }
        }
      }
      for (; q != NULL; tail = q, q = q->next) {
      }
      free_nodes(w, tail);
    }
  }
  if (found) {
    print_item(item, cli_options, rules, item_table, rhs_sym, string_table, symno);
  }
  return found;
}

/// In this procedure, we attempt to retrace an LALR conflict path
/// (there may be more than one) of CONFLICT_SYMBOL in the state
/// automaton that led to ITEM_NO in state STATE_NO.
static void print_relevant_lalr_items(const int state_no, const int item_no, const int conflict_symbol, struct CLIOptions *cli_options, ArrayShort item_list, ArrayShort nt_items, JBitset first, struct node **adequate_item, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat, struct statset_type *statset, ArrayShort rhs_sym, const long *la_top, char *string_table, struct symno_type *symno) {
  const int lhs_symbol = rules[item_table[item_no].rule_number].lhs;
  if (lhs_symbol == accept_image) {
    // Do nothing.
  } else {
    /// LALR_VISITED is used to keep track of (state, nonterminal) pairs
    /// that are visited in tracing the path of a lalr conflict.SLR_VISITED
    /// is similarly used to keep track of nonterminal symbols that are
    /// visited in tracing the path of an slr conflict. SYMBOL_SEEN is used
    /// to keep track of nonterminal symbols that are visited in tracing a
    /// path to the start state (root).
    ///
    /// CYCLIC is a boolean vector used to identify states that can enter
    /// a cycle of transitions on nullable nonterminals.
    /// As the computation of CYCLIC requires a modified version of the
    /// digraph algorithm, the variables STACK, INDEX_OF and TOP are used
    /// for that algorithm.
    ///
    /// RMPSELF is a boolean vector that indicates whether a given
    /// non-terminal can right-most produce itself. It is only constructed
    /// when LALR_LEVEL > 1.
    ArrayBool lalr_visited = Allocate_bool_array2(*la_top + 1);
    struct node *v = lpgaccess(state_no, item_no, in_stat, item_table);
    struct node *p;
    struct node *tail;
    for (p = v; p != NULL; tail = p, p = p->next) {
      const struct goto_header_type go_to = statset[p->value].go_to;
      int ii;
      for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {
      }
      if (lalr_path_retraced(p->value, ii, conflict_symbol, cli_options, lalr_visited, item_list, nt_items, first, adequate_item, rules, item_table, in_stat, statset, rhs_sym, string_table, symno)) {
        break;
      }
    }
    for (; p != NULL; tail = p, p = p->next) {
    }
    free_nodes(v, tail);
    ffree(lalr_visited.raw);
  }
}

/// Add SYMBOL to the set of symbols CONFLICT_SYMBOLS[STATE_NO].
static void add_conflict_symbol(const int state_no, const int symbol, struct node **conflict_symbols) {
  struct node *p = Allocate_node();
  p->value = symbol;
  if (conflict_symbols[state_no] == NULL) {
    p->next = p;
  } else {
    p->next = conflict_symbols[state_no]->next;
    conflict_symbols[state_no]->next = p;
  }
  conflict_symbols[state_no] = p;
}

/// This function takes as argument a configuration STACK, a SYMBOL on
/// which a transition can be made in the configuration and a terminal
/// lookahead symbol, LA_SYMBOL. It executes the transition on SYMBOL
/// and simulates all paths taken in the automaton after that transition
/// until new state(s) are reached where a transition is possible on
/// the lookahead symbol. It then returns the new set of configurations
/// found on which a transition on LA_SYMBOL is possible.
static struct stack_element *follow_sources(struct stack_element *stack, int symbol, const int la_symbol, ArrayBool cyclic, struct StackPool* sp, struct visited_element* visited, struct StackRoot* sr, ArrayBool rmpself, JBitset first, struct LAIndex* lai, struct node **adequate_item, struct SRTable* srt, struct statset_type *statset, ArrayBool null_nt, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat) {
  struct stack_element *configs = NULL; /* Initialize the output set of configurations */
  // If the starting configuration consists of a single state and
  // the initial [state, symbol] pair has already been visited,
  // return the null set. Otherwise, mark the pair visited and ...
  const int state_no = stack->state_number;
  if (stack->size == 1) {
    if (was_visited(state_no, symbol, visited) || (state_no == 1 && symbol == accept_image)) {
      return configs;
    }
    mark_visited(state_no, symbol, visited);
  }
  // Find the transition defined on the symbol...
  // If the SYMBOL is a nonterminal and we can determine that the
  // lookahead symbol (LA_SYMBOL) cannot possibly follow the
  // nonterminal in question in this context, we simply abandon the
  // search and return the NULL set.
  int act;
  if (IS_A_NON_TERMINAL(symbol)) {
    struct goto_header_type go_to = statset[state_no].go_to;
    int ii;
    for (ii = 1; go_to.map[ii].symbol != symbol; ii++) {
    }
    if (lai->la_index.raw[go_to.map[ii].laptr] == OMEGA) {
      int stack_top = 0;
      la_traverse(state_no, ii, &stack_top, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
    }
    if (!IS_IN_SET(lai->la_set, go_to.map[ii].laptr, la_symbol)) {
      return configs;
    }
    act = go_to.map[ii].action;
  } else {
    struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
    int ii;
    for (ii = 1; sh.map[ii].symbol != symbol; ii++) {
    }
    act = sh.map[ii].action;
  }
  // If the ACTion on the symbol is a shift or a goto, ...
  if (act > 0) {
    // We check to see if the new state contains an action on the
    // lookahead symbol. If that's the case then we create a new
    // configuration by appending ACT to the starting configuration
    // and add this newly formed configuration to the set(list) of
    // configurations...
    struct shift_header_type sh = srt->shift[statset[act].shift_number];
    int ii;
    for (ii = 1; ii <= sh.size; ii++) {
      if (sh.map[ii].symbol == la_symbol) {
        break;
      }
    }
    if (ii <= sh.size) /* there is a transition on la_symbol in act */
    {
      struct stack_element *q = allocate_stack_element(sp);
      q->state_number = act;
      q->size = stack->size + 1;
      q->previous = stack;
      q->next = NULL;
      configs = q;
    }
    // If the new state cannot get into a cycle of null
    // transitions, we check to see if it contains any transition
    // on a nullable nonterminal. For each such transition, we
    // append the new state to the stack and recursively invoke
    // FOLLOW_SOURCES to check if a transition on LA_SYMBOL cannot
    // follow such a null transition.
    if (!cyclic.raw[act]) {
      struct goto_header_type go_to = statset[act].go_to;
      for (ii = 1; ii <= go_to.size; ii++) {
        symbol = go_to.map[ii].symbol;
        if (null_nt.raw[symbol]) {
          struct stack_element *q = allocate_stack_element(sp);
          q->state_number = act;
          q->size = stack->size + 1;
          q->previous = stack;
          q->next = NULL;
          struct stack_element *new_configs = follow_sources(q, symbol, la_symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat);
          if (new_configs == NULL) {
            free_stack_elements(q, q, sp);
          } else {
            add_dangling_stack_element(q, sp);
            configs = union_config_sets(configs, new_configs, sp);
          }
        }
      }
    }
  }
  // We now iterate over the kernel set of items associated with the
  // ACTion defined on SYMBOL...
  for (const struct node *item_ptr = act > 0 ? statset[act].kernel_items : adequate_item[-act]; item_ptr != NULL; item_ptr = item_ptr->next) {
    int item_no = item_ptr->value;
    // For each item that is a final item whose left-hand side
    // is neither the starting symbol nor a symbol that can
    // right-most produce itself...
    if (item_table[item_no].symbol == empty) {
      const int rule_no = item_table[item_no].rule_number;
      const int lhs_symbol = rules[rule_no].lhs;
      if (lhs_symbol != accept_image && !rmpself.raw[lhs_symbol]) {
        // If the length of the prefix of the item preceeding
        // the dot is shorter that the length of the stack, we
        // retrace the item's path within the stack and
        // invoke FOLLOW_SOURCES with the prefix of the stack
        // where the item was introduced through closure, the
        // left-hand side of the item and the lookahead symbol.
        if (item_table[item_no].dot < stack->size) {
          struct stack_element *q = stack;
          for (int i = 1; i < item_table[item_no].dot; i++) {
            q = q->previous;
          }
          q = follow_sources(q, lhs_symbol, la_symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat);
          configs = union_config_sets(configs, q, sp);
        } else {
          struct node *tail;
          // Compute the item in the root state of the stack,
          // and find the root state...
          item_no -= stack->size;
          struct stack_element *q;
          for (q = stack; q->size != 1; q = q->previous) {
          }
          // We are now back in the main automaton, find all
          // sources where the item was introduced through
          // closure start a new configuration and invoke
          // FOLLOW_SOURCES with the appropriate arguments to
          // calculate the set of configurations associated
          // with these sources.
          struct node *v = lpgaccess(q->state_number, item_no, in_stat, item_table);
          for (struct node *p = v; p != NULL; tail = p, p = p->next) {
            q = allocate_stack_element(sp);
            q->state_number = p->value;
            q->size = 1;
            q->previous = NULL;
            q->next = NULL;
            struct stack_element *new_configs = follow_sources(q, lhs_symbol, la_symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat);
            if (new_configs == NULL) {
              free_stack_elements(q, q, sp);
            } else {
              add_dangling_stack_element(q, sp);
              configs = union_config_sets(configs, new_configs, sp);
            }
          }
          free_nodes(v, tail);
        }
      }
    }
  }
  return configs;
}

/// This function has a similar structure as FOLLOW_SOURCES.  But,
/// instead of computing configurations that can be reached, it
/// computes lookahead symbols that can be reached.  It takes as
/// argument a configuration STACK, a SYMBOL on which a transition can
/// be made in the configuration and a set variable, LOOK_AHEAD, where
/// the result is to be stored.  When NEXT_LA is invoked from the
/// outside, LOOK_AHEAD is assumed to be initialized to the empty set.
/// NEXT_LA first executes the transition on SYMBOL and thereafter, all
/// terminal symbols that can be read are added to LOOKAHEAD.
static void next_la(struct stack_element *stack, const int symbol, const JBitset look_ahead, struct StackRoot* sr, ArrayBool rmpself, JBitset first, JBitset read_set, struct LAIndex* lai, struct node **adequate_item, struct SRTable* srt, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat, struct statset_type *statset) {
  // The only symbol that can follow the end-of-file symbol is the
  // end-of-file symbol.
  if (symbol == eoft_image) {
    SET_BIT_IN(look_ahead, 0, eoft_image);
    return;
  }
  const int state_no = stack->state_number;
  int act;
  // Find the transition defined on the symbol...
  if (IS_A_NON_TERMINAL(symbol)) {
    struct goto_header_type go_to = statset[state_no].go_to;
    int ii;
    for (ii = 1; go_to.map[ii].symbol != symbol; ii++) {
    }
    act = go_to.map[ii].action;
  } else {
    const struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
    int ii;
    for (ii = 1; sh.map[ii].symbol != symbol; ii++) {
    }
    act = sh.map[ii].action;
  }
  // If the ACTion on the symbol is a shift or a goto, then all
  // terminal symbols that can be read in ACT are added to
  // LOOK_AHEAD.
  if (act > 0) {
    SET_UNION(look_ahead, 0, read_set, act);
  }
  // We now iterate over the kernel set of items associated with the
  // ACTion defined on SYMBOL...
  // Recall that the READ_SET of ACT is but the union of the FIRST
  // map defined on the suffixes of the items in the kernel of ACT.
  for (const struct node *item_ptr = act > 0 ? statset[act].kernel_items : adequate_item[-act]; item_ptr != NULL; item_ptr = item_ptr->next) {
    int item_no = item_ptr->value;
    // For each item that is a final item whose left-hand side
    // is neither the starting symbol nor a symbol that can
    // right-most produce itself...
    if (IS_IN_SET(first, item_table[item_no - 1].suffix_index, empty)) {
      const int rule_no = item_table[item_no].rule_number;
      const int lhs_symbol = rules[rule_no].lhs;
      if (lhs_symbol != accept_image && !rmpself.raw[lhs_symbol]) {
        // If the length of the prefix of the item preceeding
        // the dot is shorter that the length of the stack, we
        // retrace the item's path within the stack and
        // invoke NEXT_LA with the prefix of the stack
        // where the item was introduced through closure, the
        // left-hand side of the item and LOOK_AHEAD.
        if (item_table[item_no].dot < stack->size) {
          struct stack_element *q = stack;
          for (int i = 1; i < item_table[item_no].dot; i++) {
            q = q->previous;
          }
          next_la(q, lhs_symbol, look_ahead, sr, rmpself, first, read_set, lai, adequate_item, srt, rules, item_table, in_stat, statset);
        } else {
          struct node *tail;
          // Compute the item in the root state of the stack,
          // and find the root state...
          item_no -= stack->size;
          struct stack_element *q;
          for (q = stack; q->size != 1; q = q->previous) {
          }
          // We are now back in the main automaton, find all
          // sources where the item was introduced through
          // closure and add all terminal symbols in the
          // follow set of the left-hand side symbol in each
          // source to LOOK_AHEAD.
          struct node *v = lpgaccess(q->state_number, item_no, in_stat, item_table);
          for (struct node *p = v; p != NULL; tail = p, p = p->next) {
            struct goto_header_type go_to = statset[p->value].go_to;
            int ii;
            for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {
            }
            // If look-ahead after left hand side is not
            // yet computed,call LA_TRAVERSE to compute it.
            if (lai->la_index.raw[go_to.map[ii].laptr] == OMEGA) {
              int stack_top = 0;
              la_traverse(p->value, ii, &stack_top, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
            }
            SET_UNION(look_ahead, 0, lai->la_set, go_to.map[ii].laptr);
          }
          free_nodes(v, tail);
        }
      }
    }
  }
}

/// This function takes as argument an array, STACK_SEEN, with
/// STATE_TABLE_SIZE elements (indexable in the range
/// 0..STATE_TABLE_SIZE-1) which is the base of a hash table and a
/// STACK. It searches the hash table to see if it already contained
/// the stack in question. If yes, it returns TRUE. Otherwise, it
/// inserts the stack into the table and returns FALSE.
static bool stack_was_seen(struct stack_element **stack_seen, struct stack_element *stack) {
  unsigned long hash_address = stack->size; /* Initialize hash address */
  for (struct stack_element *p = stack; p != NULL; p = p->previous) {
    hash_address += p->state_number;
  }
  hash_address %= STATE_TABLE_SIZE;
  for (struct stack_element *p = stack_seen[hash_address]; p != NULL; p = p->link) {
    if (stack->size == p->size) {
      struct stack_element *q;
      struct stack_element *r;
      for (q = stack, r = p; q != NULL; q = q->previous, r = r->previous) {
        if (q->state_number != r->state_number) {
          break;
        }
      }
      if (q == NULL) {
        return true;
      }
    }
  }
  stack->link = stack_seen[hash_address];
  stack_seen[hash_address] = stack;
  return false;
}

/// STATE_TO_RESOLVE_CONFLICTS is a function that attempts to resolve
/// conflicts by doing more look-ahead.  If the conflict resolution
/// is successful, then a new state is created and returned; otherwise,
/// the NULL pointer is returned.
static struct red_state_element *state_to_resolve_conflicts(struct sources_element sources, int la_symbol, int level, struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct red_state_element **shift_table, ArrayBool cyclic, struct StackPool* sp, struct visited_element* visited, struct STRS* strs, struct StackRoot* sr, ArrayBool rmpself, JBitset first, JBitset read_set, struct LAIndex* lai, struct node **adequate_item, struct SRTable* srt, ArrayBool null_nt, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat, struct statset_type *statset, struct LAState* ls) {
  struct sources_element new_sources = allocate_sources(ls);
  struct node **action;
  calloc0p(&action, num_terminals + 1, struct node *);
  ArrayShort symbol_list = Allocate_short_array2(num_terminals + 1);
  ArrayShort action_list = Allocate_short_array2(num_terminals + 1);
  ArrayShort rule_count = Allocate_short_array2(num_rules + 1);
  JBitset look_ahead;
  calloc0_set(look_ahead, 1, dss->term_set_size);
  struct red_state_element **la_shift_state;
  calloc0p(&la_shift_state, num_terminals + 1, struct red_state_element *);
  // Initialize new lookahead state. Initialize counters. Check and
  // adjust HIGHEST_LEVEL reached so far, if necessary.
  struct red_state_element *state = NULL;
  int num_shift_actions = 0;
  int num_reduce_actions = 0;
  short shift_root = NIL;
  short reduce_root = NIL;
  if (level > strs->highest_level) {
    strs->highest_level = level;
  }
  // One of the parameters received is a SOURCES map whose domain is
  // a set of actions and each of these actions is mapped into a set
  // of configurations that can be reached after that action is
  // executed (in the state where the conflicts were detected).
  // In this loop, we compute an ACTION map which maps each each
  // terminal symbol into 0 or more actions in the domain of SOURCES.
  //
  // NOTE in a sources map, if a configuration is associated with
  // more than one action then the grammar is not LALR(k) for any k.
  // We check for that condition below. However, this check is there
  // for purely cosmetic reason. It is not necessary for the
  // algorithm to work correctly and its removal will speed up this
  // loop somewhat (for conflict-less input).
  // The first loop below initializes the hash table used for
  // lookups ...
  for (int i = 0; i < STATE_TABLE_SIZE; i++) {
    sources.stack_seen[i] = NULL;
  }
  short symbol_root = NIL;
  for (int act = sources.root; act != NIL; act = sources.list.raw[act]) {
    // For each action we iterate over its associated set of
    // configurations and invoke NEXT_LA to compute the lookahead
    // set for that configuration. These lookahead sets are in
    // turn unioned together to form a lookahead set for the
    // action in question.
    INIT_SET(look_ahead);
    for (struct stack_element *stack = sources.configs[act]; stack != NULL; stack = stack->next) {
      if (stack_was_seen(sources.stack_seen, stack)) {
        // This is the superfluous code mentioned above!
        strs->highest_level = INFINITY;
        goto clean_up_and_return;
      }
      next_la(stack, la_symbol, look_ahead, sr, rmpself, first, read_set, lai, adequate_item, srt, rules, item_table, in_stat, statset);
    }
    RESET_BIT(look_ahead, empty); /* EMPTY never in LA set */
    // For each lookahead symbol computed for this action, add an
    // action to the ACTION map and keep track of the symbols on
    // which any action is defined.
    // If new conflicts are detected and we are already at the
    // lookahead level requested, we terminate the computation...
    int count = 0;
    for ALL_TERMINALS3(symbol) {
      if (IS_ELEMENT(look_ahead, symbol)) {
        count++;
        if (action[symbol] == NULL) {
          symbol_list.raw[symbol] = symbol_root;
          symbol_root = symbol;
        } else if (level == cli_options->lalr_level) {
          goto clean_up_and_return;
        }
        struct node *p = Allocate_node();
        p->value = act;
        p->next = action[symbol];
        action[symbol] = p;
      }
    }
    // If the action in question is a reduction then we keep track
    // of how many times it was used.
    if (act >= 0 && act <= num_rules) {
      rule_count.raw[act] = count;
    }
  }
  // We now iterate over the symbols on which actions are defined.
  // If we detect conflicts on any symbol, we compute new sources
  // and try to recover by computing more lookahead. Otherwise, we
  // update the counts and create two lists: a list of symbols on
  // which shift actions are defined and a list of symbols on which
  // reduce actions are defined.
  for (int symbol = symbol_root; symbol != NIL; symbol = symbol_list.raw[symbol]) {
    // We have four cases to consider:
    //    1. There are conflicts on SYMBOL
    //    2. The action on SYMBOL is a shift-reduce
    //    3. The action on SYMBOL is a shift
    //    4. The action on SYMBOL is a reduce
    if (action[symbol]->next != NULL) {
      new_sources = clear_sources(new_sources, sp);
      struct node *tail;
      for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
        int act = p->value;
        if (act >= 0 && act <= num_rules) {
          rule_count.raw[act]--;
        }
        clear_visited(visited);
        for (struct stack_element *stack = sources.configs[act]; stack != NULL; stack = stack->next) {
          struct stack_element *new_configs;
          new_configs = follow_sources(stack, la_symbol, symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat);
          new_sources = add_configs(new_sources, act, new_configs, sp);
        }
      }
      free_nodes(action[symbol], tail);
      action[symbol] = NULL;
      state = state_to_resolve_conflicts(new_sources, symbol, level + 1, cli_options, dss, shift_table, cyclic, sp, visited, strs, sr, rmpself, first, read_set, lai, adequate_item, srt, null_nt, rules, item_table, in_stat, statset, ls);
      if (state == NULL) {
        goto clean_up_and_return;
      }
      la_shift_state[symbol] = state;
      struct node *p = Allocate_node();
      p->value = state->state_number;
      p->next = NULL;
      action[symbol] = p;
      num_shift_actions++;
      action_list.raw[symbol] = shift_root;
      shift_root = symbol;
    } else if (action[symbol]->value < 0) {
      num_shift_actions++;
      action_list.raw[symbol] = shift_root;
      shift_root = symbol;
    } else if (action[symbol]->value > num_rules) {
      num_shift_actions++;
      action[symbol]->value -= num_rules;
      action_list.raw[symbol] = shift_root;
      shift_root = symbol;
    } else {
      num_reduce_actions++;
      action_list.raw[symbol] = reduce_root;
      reduce_root = symbol;
    }
  }
  // We now iterate over the reduce actions in the domain of sources
  // and compute a default action.
  int default_rule = OMEGA;
  int count = 0;
  for (int act = sources.root; act != NIL; act = sources.list.raw[act]) {
    if (act >= 0 && act <= num_rules) {
      if (rule_count.raw[act] > count) {
        count = rule_count.raw[act];
        default_rule = act;
      }
    }
  }
  // By now, we are ready to create a new look-ahead state. The
  // actions for the state are in the ACTION vector, and the
  // constants: NUM_SHIFT_ACTIONS and NUM_REDUCE_ACTIONS indicate
  // the number of shift and reduce actions in the ACTION vector.
  // Note that the IN_STATE field of each look-ahead state created
  // is initially set to the number associated with that state. If
  // all the conflicts detected in the state, S, that requested the
  // creation of a look-ahead state are resolved, then this field
  // is updated with S.
  // Otherwise, this field indicates that this look-ahead state is
  // dangling - no other state point to it.
  talloc0p(&state, struct red_state_element);
  state->link = strs->la_state_root;
  strs->la_state_root = state;
  ls->max_la_state++;
  state->symbol = la_symbol;
  state->state_number = ls->max_la_state;
  state->in_state = ls->max_la_state; /* Initialize it to something! */
  // If there are any shift-actions in this state, we create a shift
  // map for them if one does not yet exist, otherwise, we reuse the
  // old existing one.
  if (num_shift_actions > 0) {
    unsigned long hash_address;
    struct shift_header_type sh;
    struct red_state_element *p_inner;
    // In this loop, we compute the hash address as the number of
    // shift actions, plus the sum of all the symbols on which a
    // shift action is defined.  As a side effect, we also take
    // care of some other issues. Shift actions which were encoded
    // to distinguish them from reduces action are decoded.
    // The counters for shift and shift-reduce actions are updated.
    // For all Shift actions to look-ahead states, the IN_STATE
    // field of these look-ahead target states are updated.
    hash_address = num_shift_actions; /* Initialize hash address */
    for (int symbol = shift_root; symbol != NIL; symbol = action_list.raw[symbol]) {
      hash_address += symbol;
      if (action[symbol]->value < 0) {
        num_shift_reduces++;
      } else if (action[symbol]->value <= ls->num_states) {
        num_shifts++;
      } else {
        // lookahead-shift
        la_shift_state[symbol]->in_state = ls->max_la_state;
      }
    }
    hash_address %= SHIFT_TABLE_SIZE;
    // Search list associated with HASH_ADDRESS, and if the shift
    // map in question is found, update the SHIFT, and SHIFT_NUMBER
    // fields of the new Look-Ahead State.
    for (p_inner = shift_table[hash_address]; p_inner != NULL; p_inner = p_inner->next_shift) {
      // Search hash table for shift map
      sh = p_inner->shift;
      if (sh.size == num_shift_actions) {
        int ii;
        for (ii = 1; ii <= num_shift_actions; ii++) {
          // compare shift maps
          if (sh.map[ii].action != action[sh.map[ii].symbol]->value) {
            break;
          }
        }
        if (ii > num_shift_actions) /* are they equal ? */
        {
          state->shift = sh;
          state->shift_number = p_inner->shift_number;
          break;
        }
      }
    }
    // Shift map was not found.  We have to create a new one and
    // insert it into the table.
    if (p_inner == NULL) {
      num_shift_maps++;
      sh = Allocate_shift_map(num_shift_actions);
      state->shift = sh;
      state->shift_number = num_shift_maps;
      state->next_shift = shift_table[hash_address];
      shift_table[hash_address] = state;
      for (int symbol = shift_root, i = 1; symbol != NIL; symbol = action_list.raw[symbol], i++) {
        sh.map[i].symbol = symbol;
        sh.map[i].action = action[symbol]->value;
      }
    }
  } else {
    state->shift.size = 0;
    state->shift_number = 0;
  }
  // Construct Reduce map.
  // When SPACE or TIME tables are requested, no default actions are
  // taken.
Build_reduce_map: {
    struct reduce_header_type red;
    if (default_rule != OMEGA &&
        cli_options->table_opt.value != OPTIMIZE_TIME.value &&
        cli_options->table_opt.value != OPTIMIZE_SPACE.value &&
        cli_options->default_opt.value != OPT_0.value)
      num_reduce_actions -= rule_count.raw[default_rule];
    num_reductions += num_reduce_actions;
    red = Allocate_reduce_map(num_reduce_actions);
    state->reduce = red;
    for (int symbol = reduce_root, i_inner = 1; symbol != NIL; symbol = action_list.raw[symbol]) {
      if (cli_options->default_opt.value == OPT_0.value ||
          action[symbol]->value != default_rule ||
          cli_options->table_opt.value == OPTIMIZE_TIME.value ||
          cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
        red.map[i_inner].symbol = symbol;
        red.map[i_inner].rule_number = action[symbol]->value;
        i_inner++;
      }
    }
    red.map[0].symbol = DEFAULT_SYMBOL;
    if (cli_options->default_opt.value > OPT_0.value) {
      red.map[0].rule_number = default_rule;
    } else {
      red.map[0].rule_number = OMEGA;
    }
  }
  // Release all space allocated to process this lookahead state and
  // return.
clean_up_and_return:
  free_sources(new_sources, sp);
  for (int symbol = symbol_root; symbol != NIL; symbol = symbol_list.raw[symbol]) {
    struct node *tail;
    for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
    }
    if (action[symbol] != NULL) {
      free_nodes(action[symbol], tail);
    }
  }
  ffree(action);
  ffree(symbol_list.raw);
  ffree(action_list.raw);
  ffree(rule_count.raw);
  ffree(look_ahead.raw);
  ffree(la_shift_state);
  return state;
}

/// This procedure is invoked when LALR_LEVEL > 1 to construct the
/// RMPSELF set which identifies the nonterminals that can right-most
/// produce themselves. It takes as argumen the map PRODUCES which
/// identifies for each nonterminal the set of nonterminals that it can
/// right-most produce.
ArrayBool init_rmpself(const JBitset produces) {
  ArrayBool rmpself = Allocate_bool_array2(num_non_terminals);
  rmpself.raw -= num_terminals + 1;
  // Note that each element of the map produces is a boolean vector
  // that is indexable in the range 1..num_non_terminals. Since each
  // nonterminal is offset by the value num_terminals (to distinguish
  // it from the terminals),it must therefore be adjusted accordingly
  // when dereferencing an element in the range of the produces map.
  for ALL_NON_TERMINALS3(nt) {
    rmpself.raw[nt] = IS_IN_SET(produces, nt, nt - num_terminals);
  }
  return rmpself;
}

/// Free all support structures that were allocated to help compute
/// additional lookahead.
void exit_lalrk_process(const struct CLIOptions *cli_options, struct red_state_element **shift_table, ArrayBool cyclic, struct StackPool* sp, struct visited_element* visited, struct SourcesElementSources* ses, ArrayBool rmpself) {
  if (cli_options->lalr_level > 1) {
    rmpself.raw += num_terminals + 1;
    ffree(rmpself.raw);
    ffree(shift_table);
    ffree(cyclic.raw);
    free_sources(ses->sources, sp);
    clear_visited(visited);
    ffree(visited->map);
    ffree(visited->list.raw);
  }
}

/// If conflicts were detected and LALR(k) processing was requested,
/// where k > 1, then we attempt to resolve the conflicts by computing
/// more lookaheads. Shift-Reduce conflicts are processed first,
/// followed by Reduce-Reduce conflicts.
struct ConflictCounter resolve_conflicts(const int state_no, struct node **action, const ArrayShort symbol_list, const int reduce_root, struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct red_state_element **shift_table, ArrayBool cyclic, ArrayShort* item_listp, struct StackPool* sp, struct ConflictPool* cp, struct visited_element* visited, struct SourcesElementSources* ses, struct STRS* strs, struct StackRoot* sr, ArrayBool rmpself, JBitset first, JBitset read_set, struct LAIndex* lai, struct node **conflict_symbols, struct node **adequate_item, struct SRTable* srt, struct lastats_type *lastats, ArrayBool null_nt, struct node **in_stat, struct ruletab_type *rules, struct itemtab *item_table, struct statset_type *statset, ArrayShort rhs_sym, long* la_top, char *string_table, struct symno_type *symno, struct LAState* ls) {
  long num_sr_conflicts = 0;
  long num_rr_conflicts = 0;
  // Note that a shift action to a state "S" is encoded with the
  // value (S+NUM_RULES) to help distinguish it from reduce actions.
  // Reduce actions lie in the range [0..NUM_RULES]. Shift-reduce
  // actions lie in the range [-NUM_RULES..-1].
  struct sr_conflict_element *sr_conflict_root = NULL;
  const struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
  for (int i = 1; i <= sh.size; i++) {
    int symbol = sh.map[i].symbol;
    if (cli_options->single_productions_bit && action[symbol] != NULL) {
      add_conflict_symbol(state_no, symbol, conflict_symbols);
    }
    if (cli_options->lalr_level > 1 && action[symbol] != NULL) {
      ses->sources = clear_sources(ses->sources, sp);
      struct stack_element *q = allocate_stack_element(sp);
      q->state_number = state_no;
      q->size = 1;
      q->previous = NULL;
      q->next = NULL;
      int act = sh.map[i].action;
      if (act > 0) {
        ses->sources = add_configs(ses->sources, act + num_rules, q, sp);
      } else {
        ses->sources = add_configs(ses->sources, act, q, sp);
      }
      struct node *tail;
      for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
        int item_no = p->value;
        act = item_table[item_no].rule_number;
        int lhs_symbol = rules[act].lhs;
        clear_visited(visited);
        struct node *v = lpgaccess(state_no, item_no, in_stat, item_table);
        for (struct node *s = v; s != NULL; tail = s, s = s->next) {
          q = allocate_stack_element(sp);
          q->state_number = s->value;
          q->size = 1;
          q->previous = NULL;
          q->next = NULL;
          struct stack_element *new_configs = follow_sources(q, lhs_symbol, symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat);
          if (new_configs == NULL) {
            free_stack_elements(q, q, sp);
          } else {
            add_dangling_stack_element(q, sp);
            ses->sources = add_configs(ses->sources, act, new_configs, sp);
          }
        }
        free_nodes(v, tail);
      }
      // The function STATE_TO_RESOLVE_CONFLICTS returns a pointer
      // value to a STATE_ELEMENT which has been constructed to
      // resolve the conflicts in question. If the value returned by
      // that function is NULL, then it was not possible to resolve
      // the conflicts.  In any case, STATE_TO_RESOLVE_CONFLICTS
      // frees the space that is used by the action map headed by
      // ACTION_ROOT.
      struct red_state_element *state = state_to_resolve_conflicts(ses->sources, symbol, 2, cli_options, dss, shift_table, cyclic, sp, visited, strs, sr, rmpself, first, read_set, lai, adequate_item, srt, null_nt, rules, item_table, in_stat, statset, ls);
      if (state != NULL) {
        state->in_state = state_no;
        free_nodes(action[symbol], tail);
        action[symbol] = NULL;
      }
    }
    // If unresolved shift-reduce conflicts are detected on symbol,
    //  add them to the list of conflicts so they can be reported
    // (if the CONFLICT option is on) and count them.
    if (action[symbol] != NULL) {
      int act = sh.map[i].action;
      struct node *tail;
      for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
        if (cli_options->conflicts_bit) {
          struct sr_conflict_element *q_inner = allocate_conflict_element(cp);
          q_inner->state_number = act;
          q_inner->item = p->value;
          q_inner->symbol = symbol;
          q_inner->next = sr_conflict_root;
          sr_conflict_root = q_inner;
        }
        num_sr_conflicts++;
      }
      // Remove reduce actions defined on symbol so as to give
      // precedence to the shift.
      free_nodes(action[symbol], tail);
      action[symbol] = NULL;
    }
  }
  // We construct a map from each action to a list of states as we
  // did for the Shift-reduce conflicts. A boolean vector ITEM_SEEN
  // is used to prevent duplication of actions. This problem does
  // not occur with Shift-Reduce conflicts.
  struct rr_conflict_element *rr_conflict_root = NULL;
  for (int symbol = reduce_root; symbol != NIL; symbol = symbol_list.raw[symbol]) {
    if (action[symbol] != NULL) {
      if (cli_options->single_productions_bit && action[symbol]->next != NULL) {
        add_conflict_symbol(state_no, symbol, conflict_symbols);
      }
      if (cli_options->lalr_level > 1 && action[symbol]->next != NULL) {
        ses->sources = clear_sources(ses->sources, sp);
        struct node *tail;
        for (struct node *p = action[symbol]; p != NULL; tail = p, p = p->next) {
          int item_no = p->value;
          int act = item_table[item_no].rule_number;
          int lhs_symbol = rules[act].lhs;
          clear_visited(visited);
          struct node *v = lpgaccess(state_no, item_no, in_stat, item_table);
          for (struct node *s = v; s != NULL; tail = s, s = s->next) {
            struct stack_element *q = allocate_stack_element(sp);
            q->state_number = s->value;
            q->size = 1;
            q->previous = NULL;
            q->next = NULL;
            struct stack_element *new_configs = follow_sources(q, lhs_symbol, symbol, cyclic, sp, visited, sr, rmpself, first, lai, adequate_item, srt, statset, null_nt, rules, item_table, in_stat);
            if (new_configs == NULL) {
              free_stack_elements(q, q, sp);
            } else {
              add_dangling_stack_element(q, sp);
              ses->sources = add_configs(ses->sources, act, new_configs, sp);
            }
          }
          free_nodes(v, tail);
        }
        //     STATE_TO_RESOLVE_CONFLICTS will return a pointer to a
        // STATE_ELEMENT if the conflicts were resolvable with more
        // lookaheads, otherwise, it returns NULL.
        struct red_state_element *state = state_to_resolve_conflicts(ses->sources, symbol, 2, cli_options, dss, shift_table, cyclic, sp, visited, strs, sr, rmpself, first, read_set, lai, adequate_item, srt, null_nt, rules, item_table, in_stat, statset, ls);
        if (state != NULL) {
          state->in_state = state_no;
          free_nodes(action[symbol], tail);
          action[symbol] = NULL;
        }
      }
      // If unresolved reduce-reduce conflicts are detected on
      // symbol, add them to the list of conflicts so they can be
      // reported (if the CONFLICT option is on) and count them.
      if (action[symbol] != NULL) {
        int act = action[symbol]->value;
        struct node *tail;
        for (struct node *p = action[symbol]->next; p != NULL; tail = p, p = p->next) {
          if (cli_options->conflicts_bit) {
            struct rr_conflict_element *q_inner = allocate_conflict_element(cp);
            q_inner->symbol = symbol;
            q_inner->item1 = act;
            q_inner->item2 = p->value;
            q_inner->next = rr_conflict_root;
            rr_conflict_root = q_inner;
          }
          num_rr_conflicts++;
        }
        // Remove all reduce actions that are defined on symbol
        // except the first one. That rule is the one with the
        // longest right-hand side that was associated with symbol.
        // See code in MKRED.C.
        if (action[symbol]->next != NULL) {
          free_nodes(action[symbol]->next, tail);
          action[symbol]->next = NULL;
        }
      }
    }
  }
  // If any unresolved conflicts were detected, process them.
  if (sr_conflict_root != NULL || rr_conflict_root != NULL) {
    // If conflicts are detected, they are placed in two lists headed by
    // SR_CONFLICT_ROOT and RR_CONFLICT_ROOT.  We scan these lists, and
    // report the conflicts.
    int symbol;
    int rule_no;
    char temp[SYMBOL_SIZE + 1];
    // Conflicts Initialization
    //
    /// This routine is invoked when a grammar contains conflicts, and the
    /// first conflict is detected.
    ///
    // NT_ITEMS and ITEM_LIST are used in reporting SLR conflicts, and
    // in recreating paths from the Start item. See the routines
    // PRINT_RELEVANT_SLR_ITEMS and PRINT_ROOT_PATH.
    ArrayShort nt_items = Allocate_short_array2(num_non_terminals);
    nt_items.raw -= num_terminals + 1;
    *item_listp = Allocate_short_array2(ls->num_items + 1);
    ArrayShort item_list = *item_listp;
    fill_in(msg_line, (PRINT_LINE_SIZE - 11) / 2 - 1, '-');
    printf("\n%s CONFLICTS %s\n", msg_line, msg_line);
    //   SLR conflicts may be caused by a symbol in the FOLLOW set of a
    // left hand side, which is not actually in the LALR look-ahead set in
    // that context.  Therefore, there may not exist a path in the state
    // automaton from the state where the conflict was detected to another
    // state where it was introduced.  In such a case, we try to retrace a
    // path that contributed the conflict-symbol to the FOLLOW set via a
    // sequence of productions.
    //
    // To assist in this task, we build below a map from each non-terminal
    // A to the set of items of which A is the dot SYMBOL. I.e., all items
    // of the form [x .A y] where x and y are arbitrary strings, and A is
    // a non-terminal. This map is also used in retracing a path from the
    // Start item to any other item.
    for ALL_NON_TERMINALS3(symbol) {
      nt_items.raw[symbol] = NIL;
    }
    for ALL_ITEMS3(item_no, ls->num_items) {
      if (IS_A_NON_TERMINAL(item_table[item_no].symbol)) {
        item_list.raw[item_no] = nt_items.raw[item_table[item_no].symbol];
        nt_items.raw[item_table[item_no].symbol] = item_no;
      }
    }
    print_state(state_no, cli_options, adequate_item, srt, lastats, statset, in_stat, rules, item_table, rhs_sym, string_table, symno, ls); /* Print state containing conflicts */
    // Process shift-reduce conflicts.
    if (sr_conflict_root != NULL) {
      struct sr_conflict_element *tail;
      for (struct sr_conflict_element *p = sr_conflict_root; p != NULL; tail = p, p = p->next) {
        symbol = p->symbol;
        rule_no = item_table[p->item].rule_number;
        restore_symbol(temp, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
        printf("*** Shift/reduce conflict on \"%s\" with rule %d\n", temp, rule_no);
        if (cli_options->trace_opt.value != NOTRACE.value) {
          print_relevant_lalr_items(state_no, p->item, symbol, cli_options, *item_listp, nt_items, first, adequate_item, rules, item_table, in_stat, statset, rhs_sym, la_top, string_table, symno);
          print_item(p->item, cli_options, rules, item_table, rhs_sym, string_table, symno);
        }
      }
      free_conflict_elements(sr_conflict_root, tail, cp);
    }
    // Process reduce-reduce conflicts.
    if (rr_conflict_root != NULL) {
      struct rr_conflict_element *tail;
      for (struct rr_conflict_element *p = rr_conflict_root; p != NULL; tail = p, p = p->next) {
        symbol = p->symbol;
        const int n = item_table[p->item1].rule_number;
        rule_no = item_table[p->item2].rule_number;
        restore_symbol(temp, RETRIEVE_STRING(symbol, string_table, symno), cli_options->ormark, cli_options->escape);
        printf("*** Reduce/reduce conflict on \"%s\" between rule %d and %d\n", temp, n, rule_no);
        if (cli_options->trace_opt.value != NOTRACE.value) {
          print_relevant_lalr_items(state_no, p->item1, symbol, cli_options, *item_listp, nt_items, first, adequate_item, rules, item_table, in_stat, statset, rhs_sym, la_top, string_table, symno);
          print_item(p->item1, cli_options, rules, item_table, rhs_sym, string_table, symno);
          fill_in(msg_line, PRINT_LINE_SIZE - 3, '-');
          printf("\n%s", msg_line);
          print_relevant_lalr_items(state_no, p->item2, symbol, cli_options, *item_listp, nt_items, first, adequate_item, rules, item_table, in_stat, statset, rhs_sym, la_top, string_table, symno);
          print_item(p->item2, cli_options, rules, item_table, rhs_sym, string_table, symno);
        }
      }
      free_conflict_elements(rr_conflict_root, tail, cp);
    }
  }
  free_dangling_stack_elements(sp);
  return (struct ConflictCounter) {
    .num_rr_conflicts = 0,
    .num_sr_conflicts = 0,
  };
}

/// Transfer the look-ahead states to their permanent destination, the
/// array LASTATS and update the original automaton with the relevant
/// transitions into the lookahead states.
void create_lastats(struct STRS* strs, struct SRTable* srt, struct statset_type *statset, struct LaStats* las, const struct LAState* ls) {
  // Allocate LASTATS structure to permanently construct lookahead
  // states and reallocate SHIFT map as we may have to construct
  // new shift maps.
  calloc0p(&las->lastats, ls->max_la_state - ls->num_states, struct lastats_type);
  las->lastats -= ls->num_states + 1;
  realloc0p(&srt->shift, ls->max_la_state + 1, struct shift_header_type);
  // Allocate temporary space used to construct final lookahead
  // states.
  struct red_state_element **new_shift_actions;
  calloc0p(&new_shift_actions, ls->num_states + 1, struct red_state_element *);
  ArrayShort shift_action = Allocate_short_array2(num_terminals + 1);
  ArrayShort shift_list = Allocate_short_array2(num_terminals + 1);
  ArrayShort shift_count = Allocate_short_array2(ls->max_la_state + 1);
  ArrayShort state_list = Allocate_short_array2(ls->max_la_state + 1);
  // The array shift_action will be used to construct a shift map
  // for a given state. It is initialized here to the empty map.
  // The array shift_count is used to count how many references
  // there are to each shift map.
  for ALL_TERMINALS3(symbol) {
    shift_action.raw[symbol] = OMEGA;
  }
  for (int i = 0; i <= ls->max_la_state; i++) {
    shift_count.raw[i] = 0;
  }
  for ALL_STATES3(state_no, ls->num_states) {
    shift_count.raw[statset[state_no].shift_number]++;
  }
  // Traverse the list of lookahead states and initialize the
  // final lastat element appropriately. Also, construct a mapping
  // from each relevant initial state into the list of lookahead
  // states into which it can shift. We also keep track of these
  // initial states in a list headed by state_root.
  int state_root = NIL;
  for (struct red_state_element *p = strs->la_state_root; p != NULL; p = p->link) {
    las->lastats[p->state_number].in_state = p->in_state;
    las->lastats[p->state_number].shift_number = p->shift_number;
    las->lastats[p->state_number].reduce = p->reduce;
    if (p->shift.size != 0) {
      srt->shift[p->shift_number] = p->shift;
    }
    const int state_no = p->in_state;
    if (state_no <= ls->num_states) {
      if (new_shift_actions[state_no] == NULL) {
        state_list.raw[state_no] = state_root;
        state_root = state_no;
      }
      p->next_shift = new_shift_actions[state_no];
      new_shift_actions[state_no] = p;
    }
  }
  // We now traverse the list of initial states that can shift into
  // lookahead states and update their shift map appropriately.
  for (int state_no = state_root; state_no != NIL; state_no = state_list.raw[state_no]) {
    // Copy the shift map associated with STATE_NO into the direct
    // access map SHIFT_ACTION.
    const int shift_no = statset[state_no].shift_number;
    struct shift_header_type sh = srt->shift[shift_no];
    int shift_root = NIL;
    for (int i = 1; i <= sh.size; i++) {
      const int symbol = sh.map[i].symbol;
      shift_action.raw[symbol] = sh.map[i].action;
      shift_list.raw[symbol] = shift_root;
      shift_root = symbol;
    }
    // Add the lookahead shift transitions to the initial shift
    // map.
    int shift_size = sh.size;
    for (struct red_state_element *p = new_shift_actions[state_no]; p != NULL; p = p->next_shift) {
      if (shift_action.raw[p->symbol] == OMEGA) {
        shift_size++;
        shift_list.raw[p->symbol] = shift_root;
        shift_root = p->symbol;
      } else if (shift_action.raw[p->symbol] < 0) {
        num_shift_reduces--;
      } else {
        num_shifts--;
      }
      shift_action.raw[p->symbol] = p->state_number;
    }
    // There are two conditions under which we have to construct
    // a new shift map:
    //     1. The initial shift map was shared with other states.
    //     2. The updated shift map contains more elements than
    //        the initial one.
    if (shift_count.raw[shift_no] > 1) {
      shift_count.raw[shift_no]--;
      num_shift_maps++;
      sh = Allocate_shift_map(shift_size);
      srt->shift[num_shift_maps] = sh;
      statset[state_no].shift_number = num_shift_maps;
    } else if (shift_size > sh.size) {
      sh = Allocate_shift_map(shift_size);
      srt->shift[shift_no] = sh;
    }
    // Reconstruct the relevant shift map.
    for (int symbol = shift_root, i = 1; symbol != NIL; shift_action.raw[symbol] = OMEGA, symbol = shift_list.raw[symbol], i++) {
      sh.map[i].symbol = symbol;
      sh.map[i].action = shift_action.raw[symbol];
    }
  }
  // Free all local temporary structures and return.
  ffree(new_shift_actions);
  ffree(shift_action.raw);
  ffree(shift_list.raw);
  ffree(shift_count.raw);
  ffree(state_list.raw);
}










// region remsp

struct SPData {
  int top;
  int rule_root;
};

struct remsp_action_element {
  struct remsp_action_element *next;
  int symbol;
  int action;
};

struct sp_state_element {
  struct sp_state_element *next;
  struct sp_state_element *link;
  struct remsp_action_element *action_root;
  struct node *rule_root;
  struct node *complete_items;
  short state_number;
  short rule_count;
  short action_count;
};

struct update_action_element {
  struct update_action_element *next;
  short symbol;
  int action;
  short state;
};

struct AEPool {
  struct remsp_action_element *action_element_pool;
};

struct StateMap {
  int max_sp_state;
  struct sp_state_element *sp_state_root;
};

static bool IS_SP_RHS(const long symbol, ArrayShort sp_rules) {
  return sp_rules.raw[symbol] != NIL;
}

static bool IS_SP_RULE(const long rule_no, ArrayShort rule_list) {
  return rule_list.raw[rule_no] != OMEGA;
}

/// This function first tries to recycle an action_element node from a
/// free list. If the list is empty a new node is allocated from
/// temporary storage.
static struct remsp_action_element *allocate_action_element(struct AEPool* pool) {
  struct remsp_action_element *p = pool->action_element_pool;
  if (p != NULL) {
    pool->action_element_pool = p->next;
  } else {
    talloc0p(&p, struct remsp_action_element);
  }
  return p;
}

/// This routine returns a list of action_element structures to the
/// free list.
static void free_action_elements(struct remsp_action_element *head, struct remsp_action_element *tail, struct AEPool* pool) {
  tail->next = pool->action_element_pool;
  pool->action_element_pool = head;
}

/// Compute_sp_map is an instantiation of the digraph algorithm. It
/// is invoked repeatedly by remove_single_productions to:
///
///   1) Partially order the right-hand side of all the single
///      productions (SP) into a list [A1, A2, A3, ..., An]
///      such that if Ai -> Aj then i < j.
///
///   2) As a side effect, it uses the ordering above to order all
///      the SP rules.
static void compute_sp_map(const int symbol, struct SPData *spd, ArrayShort sp_rules, ArrayShort rule_list, ArrayShort index_of, ArrayShort next_rule, ArrayShort stack, struct ruletab_type *rules) {
  stack.raw[++spd->top] = symbol;
  const int indx = spd->top;
  index_of.raw[symbol] = indx;
  // In this instantiation of the digraph algorithm, two symbols (A, B)
  // are related if  A -> B is an SP and A is the right-hand side of
  // some other SP rule.
  for (int rule_no = sp_rules.raw[symbol]; rule_no != NIL; rule_no = next_rule.raw[rule_no]) {
    const int lhs_symbol = rules[rule_no].lhs;
    if (IS_SP_RHS(lhs_symbol, sp_rules)) {
      if (index_of.raw[lhs_symbol] == OMEGA) {
        compute_sp_map(lhs_symbol, spd, sp_rules, rule_list, index_of, next_rule, stack, rules);
      }
      index_of.raw[symbol] = MIN(index_of.raw[symbol], index_of.raw[lhs_symbol]);
    }
  }
  // If the index of symbol is the same index it started with then
  // symbol if the root of a SCC...
  if (index_of.raw[symbol] == indx) {
    // If symbol is on top of the stack then it is the only
    // symbol in its SCC (thus it is not part of a cycle).
    // Note that since remove_single_productions is only invoked
    // when the input grammar is conflict-free, the graph of
    // the single productions will never contain any cycle.
    // Thus, this test will always succeed and all single
    // productions associated with the symbol being processed
    // are added to the list of SP rules here...
    if (stack.raw[spd->top] == symbol) {
      for (int rule_no = sp_rules.raw[symbol]; rule_no != NIL; rule_no = next_rule.raw[rule_no]) {
        if (spd->rule_root == NIL) {
          rule_list.raw[rule_no] = rule_no;
        } else {
          rule_list.raw[rule_no] = rule_list.raw[spd->rule_root];
          rule_list.raw[spd->rule_root] = rule_no;
        }
        spd->rule_root = rule_no;
      }
    }
    // As all SCC contains exactly one symbol (as explained above)
    // this loop will always execute exactly once.
    int i;
    do {
      i = stack.raw[spd->top--];
      index_of.raw[i] = INFINITY;
    } while (i != symbol);
  }
}

/// When the parser enters STATE_NO, and it is processing SYMBOL, its
/// next move is ACTION. Given these 3 parameters, compute_sp_action
/// computes the set of reduce actions that may be executed after
/// SYMBOL is shifted in STATE_NO.
///
/// NOTE that this algorithm works only for the LALR(1) case. When the
/// transition on SYMBOL is a lookahead-shift, indicating that the
/// parser requires extra lookahead on a particular symbol, the set of
/// reduce actions for that symbol is calculated as the empty set.
static void compute_sp_action(const int state_no, const short symbol, const short action, JBitset look_ahead, bool *is_conflict_symbol, ArrayShort index_of, ArrayShort* sp_action, struct StackRoot* sr, JBitset first, struct LAIndex* lai, struct node **conflict_symbols, JBitset la_set, struct node **adequate_item, struct node **in_stat, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table) {
  const struct goto_header_type go_to = statset[state_no].go_to;
  if (sp_action[symbol].raw == NULL) {
    sp_action[symbol] = Allocate_short_array2(num_terminals + 1);
  }
  for ALL_TERMINALS3(i) {
    // initialize sp_action to the empty map
    sp_action[symbol].raw[i] = OMEGA;
  }
  int i;
  // Note that before this routine is invoked, the global vector
  // index_of identifies the index of each symbol in the goto map of
  // state_no.
  if (is_conflict_symbol[symbol]) {
    // do nothing
  } else if (action > 0) {
    // transition action (shift or goto)
    struct node *p;
    for (const struct node *item_ptr = statset[action].complete_items; item_ptr != NULL; item_ptr = item_ptr->next) {
      const int item_no = item_ptr->value;
      int rule_no = item_table[item_no].rule_number;
      int lhs_symbol = rules[rule_no].lhs;
      if (RHS_SIZE(rule_no, rules) == 1 && lhs_symbol != accept_image) {
        i = index_of.raw[lhs_symbol];
        int k = go_to.map[i].laptr;
        if (lai->la_index.raw[k] == OMEGA) {
          int stack_top = 0;
          la_traverse(state_no, i, &stack_top, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
        }
        ASSIGN_SET(look_ahead, 0, la_set, k);
        RESET_BIT(look_ahead, empty); /* empty not valid look-ahead */
        for ALL_TERMINALS3(i) {
          if (IS_ELEMENT(look_ahead, i)) {
            sp_action[symbol].raw[i] = rule_no;
          }
        }
      }
    }
    // Remove all lookahead symbols on which conflicts were
    // detected from consideration.
    for (bool end_node = (p = conflict_symbols[action]) == NULL; !end_node; end_node = p == conflict_symbols[action]) {
      p = p->next;
      sp_action[symbol].raw[p->value] = OMEGA;
    }
  } else {
    // read-reduce action
    int rule_no = -action;
    if (RHS_SIZE(rule_no, rules) == 1) {
      int lhs_symbol = rules[rule_no].lhs;
      i = index_of.raw[lhs_symbol];
      int k = go_to.map[i].laptr;
      if (lai->la_index.raw[k] == OMEGA) {
        int stack_top = 0;
        la_traverse(state_no, i, &stack_top, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
      }
      ASSIGN_SET(look_ahead, 0, la_set, k);
      RESET_BIT(look_ahead, empty); /* empty not valid look-ahead */
      for ALL_TERMINALS3(i) {
        if (IS_ELEMENT(look_ahead, i)) {
          sp_action[symbol].raw[i] = rule_no;
        }
      }
    }
  }
}

/// Sp_default_action takes as parameter a state, state_no and a rule,
/// rule_no that may be reduced when the parser enters state_no.
/// Sp_default_action tries to determine the highest rule that may be
/// reached via a sequence of SP reductions.
static short sp_default_action(const short state_no, short rule_no, ArrayShort rule_list, struct SRTable* srt, const struct statset_type *statset, struct ruletab_type *rules) {
  const struct goto_header_type go_to = statset[state_no].go_to;
  // While the rule we have at hand is a single production, ...
  while (IS_SP_RULE(rule_no, rule_list)) {
    const int lhs_symbol = rules[rule_no].lhs;
    int ii;
    for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {
    }
    int action = go_to.map[ii].action;
    if (action < 0) {
      // goto-reduce action?
      action = -action;
      if (RHS_SIZE(action, rules) != 1) {
        break;
      }
      rule_no = action;
    } else {
      int best_rule = OMEGA;
      // Enter the state action and look for preferably a SP rule
      // or some rule with right-hand size 1.
      const struct reduce_header_type red = srt->reduce[action];
      for (ii = 1; ii <= red.size; ii++) {
        action = red.map[ii].rule_number;
        if (IS_SP_RULE(action, rule_list)) {
          best_rule = action;
          break;
        }
        if (RHS_SIZE(action, rules) == 1) {
          best_rule = action;
        }
      }
      if (best_rule == OMEGA) {
        break;
      }
      rule_no = best_rule;
    }
  }
  return rule_no;
}

/// This routine takes as parameter a state, state_no, a nonterminal,
/// lhs_symbol (that is the right-hand side of a SP or a rule with
/// right-hand size 1, but not identified as a SP) on which there is
/// a transition in state_no and a lookahead symbol la_symbol that may
/// be processed after taking the transition. It returns the reduce
/// action that follows the transition if an action on la_symbol is
/// found, otherwise it returns the most suitable default action.
static int sp_nt_action(const short state_no, const int lhs_symbol, const short la_symbol, ArrayShort rule_list, struct SRTable* srt, const struct statset_type *statset) {
  const struct goto_header_type go_to = statset[state_no].go_to;
  int ii;
  for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {
  }
  int action = go_to.map[ii].action;
  if (action < 0) {
    action = -action;
  } else {
    const struct reduce_header_type red = srt->reduce[action];
    action = OMEGA;
    for (ii = 1; ii <= red.size; ii++) {
      const int rule_no = red.map[ii].rule_number;
      if (red.map[ii].symbol == la_symbol) {
        action = rule_no;
        break;
      } else if (action == OMEGA && IS_SP_RULE(rule_no, rule_list)) {
        action = rule_no;
      }
    }
  }
  return action;
}

/// Let BASE_RULE be a rule  A -> X.  The item [A -> .X] is in STATE1
/// and STATE2.  After shifting on X (in STATE1 and STATE2), if the
/// lookahead is LA_SYMBOL then BASE_RULE is reduced. In STATE1, a
/// sequence of single-production reductions is executed ending with
/// a reduction of RULE1. In STATE2, a sequence of single-productions
/// is also executed ending with RULE2.
/// The goal of this function is to find the greatest ancestor of
/// BASE_RULE that is also a descendant of both RULE1 and RULE2.
static int greatest_common_ancestor(const short base_rule, const short la_symbol, const short state1, const short rule1, const short state2, const short rule2, ArrayShort rule_list, struct SRTable* srt, struct statset_type *statset, const struct ruletab_type *rules) {
  int act1 = base_rule;
  int act2 = base_rule;
  int rule_no;
  while (act1 == act2) {
    rule_no = act1;
    if (act1 == rule1 || act2 == rule2) {
      break;
    }
    const int lhs_symbol = rules[rule_no].lhs;
    act1 = sp_nt_action(state1, lhs_symbol, la_symbol, rule_list, srt, statset);
    act2 = sp_nt_action(state2, lhs_symbol, la_symbol, rule_list, srt, statset);
  }
  return rule_no;
}

/// In SOURCE_STATE there is a transition on SYMBOL into STATE_NO.
/// SYMBOL is the right-hand side of a SP rule and the global map
/// sp_action[SYMBOL] yields a set of update reduce actions that may
/// follow the transition on SYMBOL into STATE_NO.
static void compute_update_actions(const int source_state, const int state_no, const int symbol, ArrayShort rule_list, ArrayShort* sp_action, struct update_action_element **update_action, struct SRTable* srt, struct statset_type *statset, struct ruletab_type *rules) {
  const struct reduce_header_type red = srt->reduce[state_no];
  for (int i = 1; i <= red.size; i++) {
    if (IS_SP_RULE(red.map[i].rule_number, rule_list)) {
      int rule_no = sp_action[symbol].raw[red.map[i].symbol];
      if (rule_no == OMEGA) {
        rule_no = sp_default_action(source_state, red.map[i].rule_number, rule_list, srt, statset, rules);
      }
      // Lookup the update map to see if a previous update was made
      // in STATE_NO on SYMBOL...
      struct update_action_element *p;
      for (p = update_action[state_no]; p != NULL; p = p->next) {
        if (p->symbol == red.map[i].symbol) {
          break;
        }
      }
      // If no previous update action was defined on STATE_NO and
      // SYMBOL, simply add it. Otherwise, chose as the greatest
      // common ancestor of the initial reduce action and the two
      // contending updates as the update action.
      if (p == NULL) {
        talloc0p(&p, struct update_action_element);
        p->next = update_action[state_no];
        update_action[state_no] = p;
        p->symbol = red.map[i].symbol;
        p->action = rule_no;
        p->state = source_state;
      } else if (rule_no != p->action && p->action != red.map[i].rule_number) {
        p->action = greatest_common_ancestor(red.map[i].rule_number,
                                             red.map[i].symbol,
                                             source_state,
                                             rule_no,
                                             p->state,
                                             p->action, rule_list, srt, statset, rules);
      }
    }
  }
}

/// Sp_state_map is invoked to create a new state using the reduce map
/// sp_symbol[SYMBOL]. The new state will be entered via a transition
/// on SYMBOL which is the right-hand side of the SP rule of which
/// ITEM_NO is the final item.
///
/// RULE_HEAD is the root of a list of rules in the global vector
/// next_rule.  This list of rules identifies the range of the reduce
/// map sp_symbol[SYMBOL]. The value SP_RULE_COUNT is the number of
/// rules in the list. The value SP_ACTION_COUNT is the number of
/// actions in the map sp_symbol[SYMBOL].
static short sp_state_map(const int rule_head, const int item_no, const int sp_rule_count, const int sp_action_count, const int symbol, ArrayShort next_rule, ArrayShort* sp_action, struct sp_state_element **sp_table, struct StateMap* state_map, struct AEPool* pool) {
  // These new SP states are defined by their reduce maps. Hash the
  // reduce map based on the set of rules in its range - simply add
  // them up and reduce modulo STATE_TABLE_SIZE.
  unsigned long hash_address = 0;
  for (int rule_no = rule_head; rule_no != NIL; rule_no = next_rule.raw[rule_no]) {
    hash_address += rule_no;
  }
  hash_address %= STATE_TABLE_SIZE;
  // Search the hash table for a compatible state. Two states S1
  // and S2 are compatible if
  //     1) the set of rules in their reduce map is identical.
  //     2) for each terminal symbol t, either
  //            reduce[S1][t] == reduce[S2][t] or
  //            reduce[S1][t] == OMEGA         or
  //            reduce[S2][t] == OMEGA
  struct sp_state_element *state;
  for (state = sp_table[hash_address]; state != NULL; state = state->link) {
    if (state->rule_count == sp_rule_count) {
      // same # of rules?
      struct node *p;
      for (p = state->rule_root; p != NULL; p = p->next) {
        if (next_rule.raw[p->value] == OMEGA) {
          // not in list?
          break;
        }
      }
      // If the set of rules are identical, we proceed to compare the
      // actions for compatibility. The idea is to make sure that all
      // actions in the hash table do not clash in the actions in the
      // map sp_action[SYMBOL].
      if (p == NULL) {
        // all the rules match?
        struct remsp_action_element *actionp;
        for (actionp = state->action_root; actionp != NULL; actionp = actionp->next) {
          if (sp_action[symbol].raw[actionp->symbol] != OMEGA && sp_action[symbol].raw[actionp->symbol] != actionp->action) {
            break;
          }
        }
        // If the two states are compatible merge them into the map
        // sp_action[SYMBOL]. (Note that this effectively destroys
        // the original map.) Also, keep track of whether or not an
        // actual merging action was necessary with the boolean
        // variable no_overwrite.
        if (actionp == NULL) {
          // compatible states
          struct remsp_action_element *action_tail;
          bool no_overwrite = true;
          for (actionp = state->action_root; actionp != NULL; action_tail = actionp, actionp = actionp->next) {
            if (sp_action[symbol].raw[actionp->symbol] == OMEGA) {
              sp_action[symbol].raw[actionp->symbol] = actionp->action;
              no_overwrite = false;
            }
          }
          // If the item was not previously associated with this
          // state, add it.
          for (p = state->complete_items; p != NULL; p = p->next) {
            if (p->value == item_no) {
              break;
            }
          }
          if (p == NULL) {
            p = Allocate_node();
            p->value = item_no;
            p->next = state->complete_items;
            state->complete_items = p;
          }
          // If the two maps are identical (there was no merging),
          // return the state number otherwise, free the old map
          // and break out of the search loop.
          if (no_overwrite && state->action_count == sp_action_count) {
            return state->state_number;
          }
          free_action_elements(state->action_root, action_tail, pool);
          break; /* for (state = sp_table[hash_address]; ... */
        }
      }
    }
  }
  // If we did not find a compatible state, construct a new one.
  // Add it to the list of state and add it to the hash table.
  if (state == NULL) {
    talloc0p(&state, struct sp_state_element);
    state->next = state_map->sp_state_root;
    state_map->sp_state_root = state;
    state->link = sp_table[hash_address];
    sp_table[hash_address] = state;
    state_map->max_sp_state++;
    state->state_number = state_map->max_sp_state;
    state->rule_count = sp_rule_count;
    struct node *p = Allocate_node();
    p->value = item_no;
    p->next = NULL;
    state->complete_items = p;
    state->rule_root = NULL;
    for (int rule_no = rule_head; rule_no != NIL; rule_no = next_rule.raw[rule_no]) {
      p = Allocate_node();
      p->value = rule_no;
      p->next = state->rule_root;
      state->rule_root = p;
    }
  }
  // If the state is new or had its reduce map merged with another
  // map, we update the reduce map here.
  state->action_count = sp_action_count;
  state->action_root = NULL;
  for ALL_TERMINALS3(i) {
    if (sp_action[symbol].raw[i] != OMEGA) {
      struct remsp_action_element *actionp = allocate_action_element(pool);
      actionp->symbol = i;
      actionp->action = sp_action[symbol].raw[i];
      actionp->next = state->action_root;
      state->action_root = actionp;
    }
  }
  return state->state_number;
}

/// This program is invoked to remove as many single production
/// actions as possible for a conflict-free automaton.
void remove_single_productions(struct DetectedSetSizes *dss, struct StackRoot* sr, JBitset first, struct LAIndex* lai, struct node **conflict_symbols, JBitset la_set, struct node **adequate_item, struct SRTable* srt, struct statset_type *statset, struct lastats_type *lastats, ArrayShort gd_index, struct node **in_stat, struct ruletab_type *rules, struct itemtab *item_table, ArrayShort rhs_sym, struct LAState* ls) {
  struct AEPool pool = {
    .action_element_pool = NULL,
  };
  // Set up a pool of temporary space.
  reset_temporary_space();
  // Allocate all other necessary temporary objects.
  ArrayShort sp_rules = Allocate_short_array2(num_symbols + 1);
  ArrayShort stack = Allocate_short_array2(num_symbols + 1);
  ArrayShort index_of = Allocate_short_array2(num_symbols + 1);
  ArrayShort next_rule = Allocate_short_array2(num_rules + 1);
  ArrayShort rule_list = Allocate_short_array2(num_rules + 1);
  ArrayShort symbol_list = Allocate_short_array2(num_symbols + 1);
  ArrayShort shift_transition = Allocate_short_array2(num_symbols + 1);
  ArrayShort rule_count = Allocate_short_array2(num_rules + 1);
  struct new_shift_element {
    short link;
    short shift_number;
  } *new_shift;
  calloc0p(&new_shift, ls->max_la_state + 1, struct new_shift_element);
  JBitset look_ahead;
  calloc0_set(look_ahead, 1, dss->term_set_size);
  ArrayShort* sp_action;
  calloc0p(&sp_action, num_symbols + 1, ArrayShort);
  bool *is_conflict_symbol;
  calloc0p(&is_conflict_symbol, num_symbols + 1, bool);
  struct sp_state_element **sp_table;
  calloc0p(&sp_table, STATE_TABLE_SIZE, struct sp_state_element *);
  struct remsp_action_element **new_action;
  calloc0p(&new_action, ls->num_states + 1, struct remsp_action_element *);
  struct update_action_element **update_action;
  calloc0p(&update_action, ls->num_states + 1, struct update_action_element *);
  // Initialize all relevant sets and maps to the empty set.
  short symbol_root = NIL;
  for ALL_RULES3(rule_no) {
    rule_list.raw[rule_no] = OMEGA;
  }
  for ALL_SYMBOLS3(symbol) {
    symbol_list.raw[symbol] = OMEGA;
    sp_rules.raw[symbol] = NIL;
  }
  // Construct a set of all symbols used in the right-hand side of
  // single production in symbol_list. The variable symbol_root
  // points to the root of the list. Also, construct a mapping from
  // each symbol into the set of single productions of which it is
  // the right-hand side. sp_rules is the base of that map and the
  // relevant sets are stored in the vector next_rule.
  for ALL_RULES3(rule_no) {
    if (rules[rule_no].sp) {
      int i = rhs_sym.raw[rules[rule_no].rhs];
      next_rule.raw[rule_no] = sp_rules.raw[i];
      sp_rules.raw[i] = rule_no;
      if (symbol_list.raw[i] == OMEGA) {
        symbol_list.raw[i] = symbol_root;
        symbol_root = i;
      }
    }
  }
  // Initialize the index_of vector and clear the stack (top).
  // Next, iterate over the list of right-hand side symbols used in
  // single productions and invoke compute_sp_map to partially
  // order these symbols (based on the ::= (or ->) relationship) as
  // well as their associated rules. (See compute_sp_map for detail)
  // As the list of rules is constructed as a circular list to keep
  // it in proper order, it is turned into a linear list here.
  for (int i = symbol_root; i != NIL; i = symbol_list.raw[i]) {
    index_of.raw[i] = OMEGA;
  }
  struct SPData spd = {
    .rule_root = NIL,
    .top = 0,
  };
  for (int i = symbol_root; i != NIL; i = symbol_list.raw[i]) {
    if (index_of.raw[i] == OMEGA) {
      compute_sp_map(i, &spd, sp_rules, rule_list, index_of, next_rule, stack, rules);
    }
  }
  if (spd.rule_root != NIL) {
    // make rule_list non-circular
    int rule_no = spd.rule_root;
    spd.rule_root = rule_list.raw[rule_no];
    rule_list.raw[rule_no] = NIL;
  }
  // Clear out all the sets in sp_rules and using the new revised
  // list of SP rules mark the new set of right-hand side symbols.
  // Note this code is important for consistency in case we are
  // removing single productions in an automaton containing
  // conflicts. If an automaton does not contain any conflict, the
  // new set of SP rules is always the same as the initial set.
  for (int i = symbol_root; i != NIL; i = symbol_list.raw[i]) {
    sp_rules.raw[i] = NIL;
  }
  spd.top = 0;
  for (int rule_no = spd.rule_root; rule_no != NIL; rule_no = rule_list.raw[rule_no]) {
    spd.top++;
    int i = rhs_sym.raw[rules[rule_no].rhs];
    sp_rules.raw[i] = OMEGA;
  }
  // Initialize the base of the hash table for the new SP states.
  // Initialize the root pointer for the list of new states.
  // Initialize max_sp_state to num_states. It will be incremented
  // each time a new state is constructed.
  // Initialize the update_action table.
  // Initialize the is_conflict_symbol array for non_terminals.
  // Since nonterminals are not used as lookahead symbols, they are
  // never involved in conflicts.
  // Initialize the set/list (symbol_root, symbol_list) to the
  // empty set/list.
  for (int i = 0; i < STATE_TABLE_SIZE; i++) {
    sp_table[i] = NULL;
  }
  struct StateMap state_map = {
    .sp_state_root = NULL,
    .max_sp_state = ls->num_states,
  };
  for ALL_STATES3(state_no, ls->num_states) {
    update_action[state_no] = NULL;
  }
  for ALL_NON_TERMINALS3(symbol) {
    is_conflict_symbol[symbol] = false;
  }
  symbol_root = NIL;
  for ALL_SYMBOLS3(symbol) {
    symbol_list.raw[symbol] = OMEGA;
  }
  // Traverse all regular states and process the relevant ones.
  for ALL_STATES3(state_no, ls->num_states) {
    new_action[state_no] = NULL;
    struct goto_header_type go_to = statset[state_no].go_to;
    struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
    // If the state has no goto actions, it is not considered, as
    // no single productions could have been introduced in it.
    // Otherwise, we initialize index_of to the empty map and
    // presume that symbol_list is initialized to the empty set.
    if (go_to.size > 0) {
      struct node *item_ptr;
      for ALL_SYMBOLS3(symbol) {
        index_of.raw[symbol] = OMEGA;
      }
      for ALL_TERMINALS3(symbol) {
        is_conflict_symbol[symbol] = false;
      }
      struct node *p;
      for (bool end_node = (p = conflict_symbols[state_no]) == NULL; !end_node; end_node = p == conflict_symbols[state_no]) {
        p = p->next;
        is_conflict_symbol[p->value] = true;
      }
      // First, use index_of to map each nonterminal symbol on
      // which there is a transition in state_no into its index
      // in the goto map of state_no.
      // Note that this initialization must be executed first
      // before we process the next loop, because index_of is
      // a global variable that is used in the routine
      // compute_sp_action.
      for (int i = 1; i <= go_to.size; i++) {
        index_of.raw[go_to.map[i].symbol] = i;
      }
      // Traverse first the goto map, then the shift map and
      // for each symbol that is the right-hand side of a single
      // production on which there is a transition, compute the
      // lookahead set that can follow this transition and add
      // the symbol to the set of candidates (in symbol_list).
      for (int i = 1; i <= go_to.size; i++) {
        short symbol = go_to.map[i].symbol;
        if (IS_SP_RHS(symbol, sp_rules)) {
          compute_sp_action(state_no, symbol, go_to.map[i].action, look_ahead, is_conflict_symbol, index_of, sp_action, sr, first, lai, conflict_symbols, la_set, adequate_item, in_stat, statset, rules, item_table);
          symbol_list.raw[symbol] = symbol_root;
          symbol_root = symbol;
        }
      }
      for (int i = 1; i <= sh.size; i++) {
        short symbol = sh.map[i].symbol;
        index_of.raw[symbol] = i;
        if (IS_SP_RHS(symbol, sp_rules)) {
          compute_sp_action(state_no, symbol, sh.map[i].action, look_ahead, is_conflict_symbol, index_of, sp_action, sr, first, lai, conflict_symbols, la_set, adequate_item, in_stat, statset, rules, item_table);
          symbol_list.raw[symbol] = symbol_root;
          symbol_root = symbol;
        }
      }
      // We now traverse the set of single productions in order
      // and for each rule that was introduced through closure
      // in the state (there is an action on both the left and
      // right-hand side)...
      for (int rule_no = spd.rule_root; rule_no != NIL; rule_no = rule_list.raw[rule_no]) {
        int symbol = rhs_sym.raw[rules[rule_no].rhs];
        if (symbol_list.raw[symbol] != OMEGA) {
          int lhs_symbol = rules[rule_no].lhs;
          if (index_of.raw[lhs_symbol] != OMEGA) {
            if (symbol_list.raw[lhs_symbol] == OMEGA) {
              compute_sp_action(state_no, lhs_symbol, go_to.map[index_of.raw[lhs_symbol]].action, look_ahead, is_conflict_symbol, index_of, sp_action, sr, first, lai, conflict_symbols, la_set, adequate_item, in_stat, statset, rules, item_table);
              symbol_list.raw[lhs_symbol] = symbol_root;
              symbol_root = lhs_symbol;
            }
            // Copy all reduce actions defined after the
            // transition on the left-hand side into the
            // corresponding action defined after the transition
            // on the right-hand side. If an action is defined
            // for the left-hand side -
            //
            //     sp_action[lhs_symbol][i] != OMEGA
            //
            // - but not for the right-hand side -
            //
            //     sp_action[symbol][i] == OMEGA
            //
            // it is an indication that after the transition on
            // symbol, the action on i is a lookahead shift. In
            // that case, no action is copied.
            for ALL_TERMINALS3(symbol) {
              if (sp_action[lhs_symbol].raw[symbol] != OMEGA && sp_action[symbol].raw[symbol] != OMEGA) {
                sp_action[symbol].raw[symbol] = sp_action[lhs_symbol].raw[symbol];
              }
            }
          }
        }
      }
      // For each symbol that is the right-hand side of some SP
      // for which a reduce map is defined, we either construct
      // a new state if the transition is into a final state,
      // or we update the relevant reduce action of the state
      // into which the transition is made, otherwise.
      //
      // When execution of this loop is terminated the set
      // symbol_root/symbol_list is reinitialize to the empty
      // set.
      for (int symbol = symbol_root; symbol != NIL; symbol_list.raw[symbol] = OMEGA, symbol = symbol_root) {
        symbol_root = symbol_list.raw[symbol];
        if (IS_SP_RHS(symbol, sp_rules)) {
          int action;
          if (IS_A_TERMINAL(symbol)) {
            action = sh.map[index_of.raw[symbol]].action;
          } else {
            action = go_to.map[index_of.raw[symbol]].action;
          }
          // If the transition is a lookahead shift, do nothing.
          // If the action is a goto- or shift-reduce, compute
          // the relevant rule and item involved.
          // Otherwise, the action is a shift or a goto. If the
          // transition is into a final state then it is
          // processed as the case of read-reduce above. If
          // not, we invoke compute_update_actions to update
          // the relevant actions.
          int rule_no;
          int item_no;
          if (action > ls->num_states) {
            // lookahead shift
            rule_no = OMEGA;
          } else if (action < 0) {
            // read-reduce action
            rule_no = -action;
            item_no = adequate_item[rule_no]->value;
          } else {
            // transition action
            item_ptr = statset[action].kernel_items;
            item_no = item_ptr->value;
            if (item_ptr->next == NULL && item_table[item_no].symbol == empty) {
              rule_no = item_table[item_no].rule_number;
            } else {
              compute_update_actions(state_no, action, symbol, rule_list, sp_action, update_action, srt, statset, rules);
              rule_no = OMEGA;
            }
          }
          // If we have a valid SP rule we first construct the
          // set of rules in the range of the reduce map of the
          // right-hand side of the rule. If that set contains
          // a single rule then the action on the right-hand
          // side is redefined as the same action on the left-
          // hand side of the rule in question. Otherwise, we
          // create a new state for the final item of the SP
          // rule consisting of the reduce map associated with
          // the right-hand side of the SP rule and the new
          // action on the right-hand side is a transition into
          // this new state.
          if (rule_no != OMEGA) {
            if (IS_SP_RULE(rule_no, rule_list)) {
              struct remsp_action_element *p_inner;
              int sp_rule_count = 0;
              int sp_action_count = 0;
              int rule_head = NIL;
              for ALL_RULES3(rule_no) {
                next_rule.raw[rule_no] = OMEGA;
              }
              for ALL_TERMINALS3(symbol) {
                rule_no = sp_action[symbol].raw[rule_no];
                if (rule_no != OMEGA) {
                  sp_action_count++;
                  if (next_rule.raw[rule_no] == OMEGA) {
                    sp_rule_count++;
                    next_rule.raw[rule_no] = rule_head;
                    rule_head = rule_no;
                  }
                }
              }
              if (sp_rule_count == 1 && IS_SP_RULE(rule_head, rule_list)) {
                int lhs_symbol = rules[rule_head].lhs;
                action = go_to.map[index_of.raw[lhs_symbol]].action;
              } else {
                action = sp_state_map(rule_head, item_no, sp_rule_count, sp_action_count, symbol, next_rule, sp_action, sp_table, &state_map, &pool);
              }
              p_inner = allocate_action_element(&pool);
              p_inner->symbol = symbol;
              p_inner->action = action;
              p_inner->next = new_action[state_no];
              new_action[state_no] = p_inner;
            }
          }
        }
      }
    }
  }
  // We are now ready to extend all global maps based on states and
  // permanently install the new states.
  realloc0p(&statset, state_map.max_sp_state + 1, struct statset_type);
  realloc0p(&srt->reduce, state_map.max_sp_state + 1, struct reduce_header_type);
  // see routine PRODUCE
  if (gd_index.raw != NULL) {
    realloc0p(&gd_index.raw, state_map.max_sp_state + 2, short);
    // Each element gd_index[i] points to the starting location
    // of a slice in another array. The last element of the slice
    // can be computed as (gd_index[i+1] - 1). After extending
    // gd_index, we set each new element to point to the same
    // index as its previous element, making it point to a null
    // slice.
    for (int state_no = ls->num_states + 2; state_no <= state_map.max_sp_state + 1; state_no++) {
      gd_index.raw[state_no] = gd_index.raw[state_no - 1];
    }
  }
  realloc0p(&in_stat, state_map.max_sp_state + 1, struct node*);
  for (int state_no = ls->num_states + 1; state_no <= state_map.max_sp_state; state_no++) {
    in_stat[state_no] = NULL;
  }
  // We now adjust all references to a lookahead state. The idea is
  // offset the number associated with each lookahead state by the
  // number of new SP states that were added.
  for (int j = 1; j <= num_shift_maps; j++) {
    struct shift_header_type sh = srt->shift[j];
    for (int i = 1; i <= sh.size; i++) {
      if (sh.map[i].action > ls->num_states) {
        sh.map[i].action += state_map.max_sp_state - ls->num_states;
      }
    }
  }
  for (int state_no = ls->num_states + 1; state_no <= ls->max_la_state; state_no++) {
    if (lastats[state_no].in_state > ls->num_states) {
      lastats[state_no].in_state += state_map.max_sp_state - ls->num_states;
    }
  }
  lastats -= state_map.max_sp_state - ls->num_states;
  ls->max_la_state += state_map.max_sp_state - ls->num_states;
  // We now permanently construct all the new SP states.
  for (struct sp_state_element *state = state_map.sp_state_root; state != NULL; state = state->next) {
    struct remsp_action_element *actionp;
    int default_rule;
    int reduce_size;
    int state_no = state->state_number;
    // These states are identified as special SP states since
    // they have no kernel items. They also have no goto and
    // shift actions.
    statset[state_no].kernel_items = NULL;
    statset[state_no].complete_items = state->complete_items;
    statset[state_no].go_to.size = 0;
    statset[state_no].go_to.map = NULL;
    statset[state_no].shift_number = 0;
    // Count the number of actions defined on each rule in the
    // range of the reduce map.
    for (struct node *p = state->rule_root; p != NULL; p = p->next) {
      rule_count.raw[p->value] = 0;
    }
    for (actionp = state->action_root; actionp != NULL; actionp = actionp->next) {
      rule_count.raw[actionp->action]++;
    }
    // Count the total number of reduce actions in the reduce map
    // and calculate the default.
    reduce_size = 0;
    int sp_rule_count = 0;
    struct node *rule_tail;
    for (struct node *p = state->rule_root; p != NULL; rule_tail = p, p = p->next) {
      reduce_size += rule_count.raw[p->value];
      if (rule_count.raw[p->value] > sp_rule_count) {
        sp_rule_count = rule_count.raw[p->value];
        default_rule = p->value;
      }
    }
    free_nodes(state->rule_root, rule_tail);
    // Construct a permanent reduce map for this SP state.
    num_reductions += reduce_size;
    struct reduce_header_type red = Allocate_reduce_map(reduce_size);
    srt->reduce[state_no] = red;
    red.map[0].symbol = DEFAULT_SYMBOL;
    red.map[0].rule_number = default_rule;
    for (actionp = state->action_root; actionp != NULL; actionp = actionp->next) {
      red.map[reduce_size].symbol = actionp->symbol;
      red.map[reduce_size].rule_number = actionp->action;
      reduce_size--;
    }
  }
  // We are now ready to update some old actions and add new ones.
  // This may require that we create new shift maps.  We
  // initialize top to 0 so we can use it as an index to allocate
  // elements from new_shift. We also initialize all the elements
  // of shift_table to NIL. Shift_table will be used as the base of
  // a hash table for the new shift maps.
  spd.top = 0;
  short shift_table[SHIFT_TABLE_SIZE];
  for (int i = 0; i <= SHIFT_TABLE_UBOUND; i++) {
    shift_table[i] = NIL;
  }
  // At most, the shift array contains 1..num_states elements. As,
  // each of these elements might be (theoretically) replaced by a
  // new one, we need to double its size.
  realloc0p(&srt->shift, 2 * (ls->num_states + 1), struct shift_header_type);
  // For each state with updates or new actions, take appropriate
  // actions.
  for ALL_STATES3(state_no, ls->num_states) {
    // Update reduce actions for final items of single productions
    // that are in non-final states.
    if (update_action[state_no] != NULL) {
      struct update_action_element *p_inner;
      struct reduce_header_type red = srt->reduce[state_no];
      for (int i = 1; i <= red.size; i++) {
        index_of.raw[red.map[i].symbol] = i;
      }
      for (p_inner = update_action[state_no]; p_inner != NULL; p_inner = p_inner->next) {
        red.map[index_of.raw[p_inner->symbol]].rule_number = p_inner->action;
      }
    }
    // Update initial automaton with transitions into new SP states.
    if (new_action[state_no] != NULL) {
      bool any_shift_action;
      struct remsp_action_element *p_inner;
      // Mark the index of each symbol on which there is a
      // transition and copy the shift map into the vector
      // shift_transition.
      struct goto_header_type go_to = statset[state_no].go_to;
      for (int i = 1; i <= go_to.size; i++) {
        index_of.raw[go_to.map[i].symbol] = i;
      }
      struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
      for (int i = 1; i <= sh.size; i++) {
        index_of.raw[sh.map[i].symbol] = i;
        shift_transition.raw[sh.map[i].symbol] = sh.map[i].action;
      }
      // Iterate over the new action and update the goto map
      // directly for goto actions but update shift_transition
      // for shift actions. Also, keep track as to whether or
      // not there were any shift transitions at all...
      any_shift_action = false;
      for (p_inner = new_action[state_no]; p_inner != NULL; p_inner = p_inner->next) {
        if (IS_A_NON_TERMINAL(p_inner->symbol)) {
          if (go_to.map[index_of.raw[p_inner->symbol]].action < 0 && p_inner->action > 0) {
            num_goto_reduces--;
            num_gotos++;
          }
          go_to.map[index_of.raw[p_inner->symbol]].action = p_inner->action;
        } else {
          if (sh.map[index_of.raw[p_inner->symbol]].action < 0 && p_inner->action > 0) {
            num_shift_reduces--;
            num_shifts++;
          }
          shift_transition.raw[p_inner->symbol] = p_inner->action;
          any_shift_action = true;
        }
      }
      // If there were any shift actions, a new shift map may
      // have been created. Hash shift_transition into the
      // shift hash table.
      if (any_shift_action) {
        struct shift_header_type sh2;
        unsigned long hash_address;
        hash_address = sh.size;
        for (int i = 1; i <= sh.size; i++) {
          // Compute Hash location
          hash_address += sh.map[i].symbol;
        }
        hash_address %= SHIFT_TABLE_SIZE;
        // Search HASH_ADDRESS location for shift map that matches
        // the shift map in shift_transition.  If a match is found
        // we leave the loop prematurely, the search index j is not
        // NIL, and it identifies the shift map in the hash table
        // that matched the shift_transition.
        int jj;
        for (jj = shift_table[hash_address]; jj != NIL; jj = new_shift[jj].link) {
          sh2 = srt->shift[new_shift[jj].shift_number];
          if (sh.size == sh2.size) {
            int ii;
            for (ii = 1; ii <= sh.size; ii++) {
              if (sh2.map[ii].action != shift_transition.raw[sh2.map[ii].symbol]) {
                break;
              }
            }
            if (ii > sh.size) {
              break; /* for (j = shift_table[ ... */
            }
          }
        }
        // If j == NIL, the map at hand had not yet being inserted in
        // the table, it is inserted.  Otherwise, we have a match,
        // and STATE_NO is reset to share the shift map previously
        // inserted that matches its shift map.
        if (jj == NIL) {
          sh2 = Allocate_shift_map(sh.size);
          for (int i = 1; i <= sh.size; i++) {
            int symbol = sh.map[i].symbol;
            sh2.map[i].symbol = symbol;
            sh2.map[i].action = shift_transition.raw[symbol];
          }
          num_shift_maps++;
          srt->shift[num_shift_maps] = sh2;
          statset[state_no].shift_number = num_shift_maps;
          spd.top++;
          new_shift[spd.top].shift_number = num_shift_maps;
          new_shift[spd.top].link = shift_table[hash_address];
          shift_table[hash_address] = spd.top;
        } else {
          statset[state_no].shift_number = new_shift[jj].shift_number;
        }
      }
    }
  }
  // Free all nodes used in the construction of the conflict_symbols
  // map as this map is no longer useful and its size is based on
  // the base value of num_states.
  for ALL_STATES3(state_no, ls->num_states) {
    if (conflict_symbols[state_no] != NULL) {
      struct node *p = conflict_symbols[state_no]->next;
      free_nodes(p, conflict_symbols[state_no]);
    }
  }
  // All updates have now been made, adjust the number of regular
  // states to include the new SP states.
  ls->num_states = state_map.max_sp_state;
  // Free all temporary space allocated earlier.
  ffree(sp_rules.raw);
  ffree(stack.raw);
  ffree(index_of.raw);
  ffree(next_rule.raw);
  ffree(rule_list.raw);
  ffree(symbol_list.raw);
  ffree(shift_transition.raw);
  ffree(rule_count.raw);
  ffree(new_shift);
  ffree(look_ahead.raw);
  for ALL_SYMBOLS3(symbol) {
    if (sp_action[symbol].raw != NULL) {
      ffree(sp_action[symbol].raw);
    }
  }
  ffree(sp_action);
  ffree(is_conflict_symbol);
  ffree(sp_table);
  ffree(new_action);
  ffree(update_action);
}

// endregion














/// Given an item of the form: [x .A y], where x and y are arbitrary strings,
/// and A is a non-terminal, we pretrace the path(s) in the automaton  that
/// will be followed in computing the look-ahead set for that item in
/// STATE_NO.  A number is assigned to all pairs (S, B), where S is a state,
/// and B is a non-terminal, involved in the paths. GOTO_INDX points to the
/// GOTO_ELEMENT of (STATE_NO, A).
void trace_lalr_path(const int state_no, const int goto_indx, struct CLIOptions *cli_options, int *la_base, JBitset first, struct node **adequate_item, struct ruletab_type *rules, struct node **in_stat, struct statset_type *statset, struct itemtab *item_table, long* la_top) {
  //  If STATE is a state number we first check to see if its base
  // look-ahead set is a special one that does not contain EMPTY and
  // has already been assigned a slot that can be reused.
  // ((LA_BASE[STATE] != OMEGA) signals this condition.)
  // NOTE that look-ahead follow sets are shared only when the maximum
  // look-ahead level allowed is 1 and single productions will not be
  // removed. If either (or both) of these conditions is true, we need
  // to have a unique slot assigned to each pair [S, A] (where S is a
  // state, and A is a non-terminal) in the automaton.
  const struct goto_header_type go_to = statset[state_no].go_to;
  const int state = go_to.map[goto_indx].action;
  struct node *r;
  if (state > 0) {
    if (la_base[state] != OMEGA && cli_options->lalr_level == 1 && !cli_options->single_productions_bit) {
      go_to.map[goto_indx].laptr = la_base[state];
      return;
    }
    r = statset[state].kernel_items;
  } else {
    r = adequate_item[-state];
  }
  // At this point, R points to a list of items which are the successors
  // of the items needed to initialize the Look-ahead follow sets.  If
  // anyone of these items contains EMPTY, we trace the Digraph for other
  // look-ahead follow sets that may be needed, and signal this fact
  // using the variable CONTAINS_EMPTY.
  (*la_top)++; /* allocate new slot */
  go_to.map[goto_indx].laptr = *la_top;
  bool contains_empty = false;
  for (; r != NULL; r = r->next) {
    const int item = r->value - 1;
    if (IS_IN_SET(first, item_table[item].suffix_index, empty)) {
      contains_empty = true;
      const int symbol = rules[item_table[item].rule_number].lhs;
      struct node *w = lpgaccess(state_no, item, in_stat, item_table);
      struct node *p;
      for (struct node *t = w; t != NULL; p = t, t = t->next) {
        const struct goto_header_type go_to_inner = statset[t->value].go_to;
        int ii;
        for (ii = 1; go_to_inner.map[ii].symbol != symbol; ii++) {
        }
        if (go_to_inner.map[ii].laptr == OMEGA) {
          trace_lalr_path(t->value, ii, cli_options, la_base, first, adequate_item, rules, in_stat, statset, item_table, la_top);
        }
      }
      free_nodes(w, p);
    }
  }
  // If the look-ahead follow set involved does not contain EMPTY, we
  // mark the state involved (STATE) so that other look-ahead follow
  // sets which may need this set may reuse the same one.
  // NOTE that if CONTAINS_EMPTY is false, then STATE has to denote a
  // state number (positive value) and not a rule number (negative).
  if (!contains_empty) {
    la_base[state] = go_to.map[goto_indx].laptr;
  }
}

/// COMPUTE_READ computes the number of intermediate look-ahead sets that
/// will be needed (in LA_TOP), allocates space for the sets(LA_SET), and
/// initializes them.
///  By intermediate look-ahead set, we mean the set of terminals that may
/// follow a non-terminal in a given state.
///  These sets are initialized to the set of terminals that can immediately
/// follow the non-terminal in the state to which it can shift (READ set).
void compute_read(struct CLIOptions *cli_options, const struct DetectedSetSizes* dss, ArrayBool single_complete_item, JBitset first, JBitset read_set, struct LAIndex* lai, struct node **adequate_item, struct SRTable* srt, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table, struct node **in_stat, long* la_top, const struct LAState* ls) {
  //  We traverse all the states and for all complete items that requires
  // a look-ahead set, we retrace the state digraph (with the help of the
  // routine TRACE_LALR_PATH) and assign a unique number to all look-ahead
  // follow sets that it needs. A look-ahead follow set is a set of
  // terminal symbols associated with a pair [S, A], where S is a state,
  // and A is a non-terminal:
  //
  // [S, A] --> Follow-set
  // Follow-set = {t | t is a terminal that can be shifted on after
  //                      execution of a goto action on A in state S}.
  //
  // Each follow set is initialized with the set of terminals that can be
  // shifted on in state S2, where GOTO(S, A) = S2. After initialization
  // a follow set F that does not contain the special terminal symbol
  // EMPTY is marked with the help of the array LA_BASE, and if the
  // highest level of look-ahead allowed is 1, then only one such set is
  // allocated, and shared for all pairs (S, B) whose follow set is F.
  *la_top = 0;
  int *la_base;
  calloc0p(&la_base, ls->num_states + 1, int);
  for ALL_STATES3(state_no, ls->num_states) {
    la_base[state_no] = OMEGA;
  }
  for ALL_STATES3(state_no, ls->num_states) {
    for (const struct node *p = cli_options->lalr_level <= 1 && single_complete_item.raw[state_no] ? NULL : statset[state_no].complete_items; p != NULL; p = p->next) {
      int item_no = p->value;
      int rule_no = item_table[item_no].rule_number;
      int lhs_symbol = rules[rule_no].lhs;
      if (lhs_symbol != accept_image) {
        struct node *v = lpgaccess(state_no, item_no, in_stat, item_table);
        struct node *q;
        for (struct node *s = v; s != NULL; q = s, s = s->next) {
          const struct goto_header_type go_to = statset[s->value].go_to;
          int ii;
          for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {
          }
          if (go_to.map[ii].laptr == OMEGA) {
            trace_lalr_path(s->value, ii, cli_options, la_base, first, adequate_item, rules, in_stat, statset, item_table, la_top);
          }
        }
        free_nodes(v, q);
      }
    }
    //  If the look-ahead level is greater than 1 or single productions
    // actions are to be removed when possible, then we have to compute
    // a Follow-set for all pairs [S, A] in the state automaton. Therefore,
    // we also have to consider Shift-reduce actions as reductions, and
    // trace back to their roots as well.
    // Note that this is not necessary for Goto-reduce actions. Since
    // they terminate with a non-terminal, and that non-terminal is
    // followed by the empty string, and we know that it must produce a
    // rule that either ends up in a reduction, a shift-reduce, or another
    // goto-reduce. It will therefore be taken care of automatically by
    // transitive closure.
    if (cli_options->lalr_level > 1 || cli_options->single_productions_bit) {
      const struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
      for (int j = 1; j <= sh.size; j++) {
        if (sh.map[j].action < 0) {
          int rule_no = -sh.map[j].action;
          int lhs_symbol = rules[rule_no].lhs;
          int item_no = adequate_item[rule_no]->value - 1;
          struct node *v = lpgaccess(state_no, item_no, in_stat, item_table);
          struct node *q;
          for (struct node *s = v; s != NULL; q = s, s = s->next) {
            const struct goto_header_type go_to = statset[s->value].go_to;
            int ii;
            for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {
            }
            if (go_to.map[ii].laptr == OMEGA) {
              trace_lalr_path(s->value, ii, cli_options, la_base, first, adequate_item, rules, in_stat, statset, item_table, la_top);
            }
          }
          free_nodes(v, q);
        }
      }
      // We also need to compute the set of terminal symbols that can be
      // read in a state entered via a terminal transition.
      if (cli_options->lalr_level > 1 && state_no != 1) {
        struct node *q = statset[state_no].kernel_items;
        int item_no = q->value - 1;
        if (IS_A_TERMINAL(item_table[item_no].symbol)) {
          ASSIGN_SET(read_set, state_no, first, item_table[item_no].suffix_index);
          for (q = q->next; q != NULL; q = q->next) {
            item_no = q->value - 1;
            SET_UNION(read_set, state_no, first, item_table[item_no].suffix_index);
          }
        }
      }
    }
  }
  //   We now allocate space for LA_INDEX and LA_SET, and initialize
  // all its elements as indicated in reduce.h. The array LA_BASE is
  // used to keep track of Follow sets that have been initialized. If
  // another set needs to be initialized with a value that has been
  // already computed, LA_BASE is used to retrieve the value.
  for ALL_STATES3(state_no, ls->num_states) {
    la_base[state_no] = OMEGA;
  }
  calloc0_set(lai->la_set, (*la_top) + 1, dss->term_set_size);
  for ALL_STATES3(state_no, ls->num_states) {
    const struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      const int la_ptr = go_to.map[i].laptr;
      if (la_ptr != OMEGA) /* Follow Look-ahead needed */
      {
        const int state = go_to.map[i].action;
        struct node *q;
        if (state > 0) {
          /* already computed */
          if (la_base[state] != OMEGA) {
            lai->la_index.raw[la_ptr] = lai->la_index.raw[la_base[state]];
            ASSIGN_SET(lai->la_set, la_ptr, lai->la_set, la_base[state]);
            q = NULL;
          } else {
            la_base[state] = la_ptr;
            q = statset[state].kernel_items;
          }
        } else {
          q = adequate_item[-state];
        }
        if (q != NULL) {
          int item_no = q->value - 1;
          // initialize with first item
          ASSIGN_SET(lai->la_set, la_ptr, first, item_table[item_no].suffix_index);
          for (q = q->next; q != NULL; q = q->next) {
            item_no = q->value - 1;
            SET_UNION(lai->la_set, la_ptr, first, item_table[item_no].suffix_index);
          }
          if (IS_IN_SET(lai->la_set, la_ptr, empty)) {
            lai->la_index.raw[la_ptr] = OMEGA;
          } else {
            lai->la_index.raw[la_ptr] = INFINITY;
          }
          if (cli_options->lalr_level > 1 || cli_options->single_productions_bit) {
            if (state > 0) {
              ASSIGN_SET(read_set, state, lai->la_set, la_ptr);
            }
          }
        }
      }
    }
  }
  ffree(la_base);
}

/// COMPUTE_LA takes as argument a state number (STATE_NO), an item number
/// (ITEM_NO), and a set (LOOK_AHEAD).  It computes the look-ahead set of
/// terminals for the given item in the given state and places the answer in
/// the set LOOK_AHEAD.
void compute_la(const int state_no, const int item_no, const JBitset look_ahead, struct StackRoot* sr, JBitset first, struct LAIndex* lai, struct node **adequate_item, struct ruletab_type *rules, struct node **in_stat, struct statset_type *statset, struct itemtab *item_table) {
  sr->stack_root = NULL;
  const int lhs_symbol = rules[item_table[item_no].rule_number].lhs;
  if (lhs_symbol == accept_image) {
    ASSIGN_SET(look_ahead, 0, first, item_table[item_no - 1].suffix_index);
    return;
  }
  INIT_SET(look_ahead); /* initialize set */
  struct node *v = lpgaccess(state_no, item_no, in_stat, item_table);
  struct node *r;
  for (struct node *s = v; s != NULL; r = s, s = s->next) {
    // Search for GOTO action in Access-State after reducing rule to
    // its left hand side(LHS_SYMBOL). Q points to the state.
    const struct goto_header_type go_to = statset[s->value].go_to;
    int ii;
    for (ii = 1; go_to.map[ii].symbol != lhs_symbol; ii++) {
    }
    // If look-ahead after left hand side is not yet computed,
    // LA_TRAVERSE the graph to compute it.
    if (lai->la_index.raw[go_to.map[ii].laptr] == OMEGA) {
      int stack_top = 0;
      la_traverse(s->value, ii, &stack_top, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
    }
    SET_UNION(look_ahead, 0, lai->la_set, go_to.map[ii].laptr);
  }
  RESET_BIT(look_ahead, empty); /* empty not valid look-ahead */
  free_nodes(v, r);
}

/// We construct the IN_STAT map which is the inverse of the transition
/// map formed by GOTO and SHIFT maps.
/// This map is implemented as a table of pointers that can be indexed
/// by the states to a circular list of integers representing other
/// states that contain transitions to the state in question.
void build_in_stat(const struct SRTable* srt, const struct statset_type *statset, struct node **in_stat, struct LAState* ls) {
  for ALL_STATES3(state_no, ls->num_states) {
    int n = statset[state_no].shift_number;
    const struct shift_header_type sh = srt->shift[n];
    for (int i = 1; i <= sh.size; ++i) {
      n = sh.map[i].action;
      if (n > 0 && n <= ls->num_states) {
        /* A shift action? */
        struct node *q = Allocate_node();
        q->value = state_no;
        if (in_stat[n] == NULL) {
          q->next = q;
        } else {
          q->next = in_stat[n]->next;
          in_stat[n]->next = q;
        }
        in_stat[n] = q;
      }
    }
    const struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      n = go_to.map[i].action;
      // A goto action
      if (n > 0) {
        struct node *q = Allocate_node();
        q->value = state_no;
        if (in_stat[n] == NULL) {
          q->next = q;
        } else {
          q->next = in_stat[n]->next;
          in_stat[n]->next = q;
        }
        in_stat[n] = q;
      }
    }
  }
}

/// LA_TRAVERSE takes two major arguments: STATE_NO, and an index (GOTO_INDX)
/// that points to the GOTO_ELEMENT array in STATE_NO for the non-terminal
/// left hand side of an item for which look-ahead is to be computed. The
/// look-ahead of an item of the form [x. A y] in state STATE_NO is the set
/// of terminals that can appear immediately after A in the context summarized
/// by STATE_NO. When a look-ahead set is computed, the result is placed in
/// an allocation of LA_ELEMENT pointed to by the LA_PTR field of the
/// GOTO_ELEMENT array.
///
/// The same digraph algorithm used in MKFIRST is used for this computation.
void la_traverse(const int state_no, const int goto_indx, int *stack_top, struct StackRoot* sr, JBitset first, struct LAIndex* lai, struct node **adequate_item, struct node **in_stat, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table) {
  const struct goto_header_type go_to = statset[state_no].go_to;
  const int la_ptr = go_to.map[goto_indx].laptr;
  struct node *s = Allocate_node(); /* Push LA_PTR down the stack */
  s->value = la_ptr;
  s->next = sr->stack_root;
  sr->stack_root = s;
  const int indx = ++*stack_top; /* one element was pushed into the stack */
  lai->la_index.raw[la_ptr] = indx;
  // Compute STATE, action to perform on Goto symbol in question. If
  // STATE is positive, it denotes a state to which to shift. If it is
  // negative, it is a rule on which to perform a Goto-Reduce.
  const int state = go_to.map[goto_indx].action;
  struct node *r;
  /* Not a Goto-Reduce action */
  if (state > 0) {
    r = statset[state].kernel_items;
  } else {
    r = adequate_item[-state];
  }
  for (; r != NULL; r = r->next) {
    // loop over items [A -> x LHS_SYMBOL . y]
    const int item = r->value - 1;
    if (IS_IN_SET(first, item_table[item].suffix_index, empty)) {
      const int symbol = rules[item_table[item].rule_number].lhs;
      struct node *w = lpgaccess(state_no, item, in_stat, item_table); /* states where RULE was  */
      // introduced through closure
      for (struct node *t = w; t != NULL; s = t, t = t->next) {
        // Search for GOTO action in access-state after reducing
        // RULE to its left hand side (SYMBOL). Q points to the
        // GOTO_ELEMENT in question.
        const struct goto_header_type go_to_inner = statset[t->value].go_to;
        int ii;
        for (ii = 1; go_to_inner.map[ii].symbol != symbol; ii++) {
        }
        if (lai->la_index.raw[go_to_inner.map[ii].laptr] == OMEGA) {
          la_traverse(t->value, ii, stack_top, sr, first, lai, adequate_item, in_stat, statset, rules, item_table);
        }
        SET_UNION(lai->la_set, la_ptr, lai->la_set, go_to_inner.map[ii].laptr);
        lai->la_index.raw[la_ptr] = MIN(lai->la_index.raw[la_ptr], lai->la_index.raw[go_to_inner.map[ii].laptr]);
      }
      free_nodes(w, s);
    }
  }
  /* Top of a SCC */
  if (lai->la_index.raw[la_ptr] == indx) {
    s = sr->stack_root;
    for (int ii = sr->stack_root->value; ii != la_ptr; sr->stack_root = sr->stack_root->next, ii = sr->stack_root->value) {
      ASSIGN_SET(lai->la_set, ii, lai->la_set, la_ptr);
      lai->la_index.raw[ii] = INFINITY;
      (*stack_top)--; /* one element was popped from the stack; */
    }
    lai->la_index.raw[la_ptr] = INFINITY;
    r = sr->stack_root; /* mark last element that is popped and ... */
    sr->stack_root = sr->stack_root->next; /* ... pop it! */
    (*stack_top)--; /* one element was popped from the stack; */
    free_nodes(s, r);
  }
}

/// Build Reduce map, and detect conflicts if any
/// MKRDCTS constructs the REDUCE map and detects conflicts in the grammar.
/// When constructing an LALR parser, the subroutine COMPUTE_LA is invoked to
/// compute the lalr look-ahead sets. For an SLR parser, the FOLLOW map
/// computed earlier in the procedure MKFIRST is used.
///
/// For a complete description of the lookahead algorithm used in this
/// program, see Charles, PhD thesis, NYU 1991.
struct ConflictCounter mkrdcts(struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct SourcesElementSources* ses, ArrayBool rmpself, JBitset first, struct node **adequate_item, struct SRTable* srt, ArrayBool null_nt, ArrayShort gd_index, struct ruletab_type *rules, struct statset_type *statset, struct itemtab *item_table, ArrayShort rhs_sym, struct LaStats* las, long* la_top, char *string_table, struct symno_type *symno, struct LAState* ls) {
  struct STRS strs = (struct STRS) {
    .highest_level = 0,
    .la_state_root = NULL,
  };
  struct StackRoot sr = (struct StackRoot) {
    .stack_root = NULL,
  };
  // Set up a pool of temporary space. If LALR(k), k > 1 is requested,
  // INIT_LALRK_PROCESS sets up the necessary environment for the
  // computation of multiple lookahead.
  reset_temporary_space();

  /// The boolean variable NOT_LRK is used to mark whether a grammar
  /// is not LR(k) for any k. NOT_LRK is marked true when either:
  ///    1. The grammar contains a nonterminal A such that A =>+rm A
  ///    2. The automaton contains a cycle with each of its edges labeled
  ///       with a nullable nonterminal.
  ///
  /// If LALR(k), k > 1, is requested, we may have to create more shift
  /// maps. Initialize SHIFT_TABLE. Note that each element of SHIFT_TABLE
  /// is automatically initialized to NULL by CALLOC.
  /// (See STATE_TO_RESOLVE_CONFLICTS)
  /// Second, we check whether the grammar is not LR(k) for any k
  /// because there exist a nonterminal A such that
  ///
  ///                     A =>+rm A
  ///
  /// Finally, allocate and compute the CYCLIC vector which identifies
  /// states that can enter a cycle via transitions on nullable
  /// nonterminals. If such a cyle exists, the grammar can also be
  /// claimed to be not LR(k) for any k.
  bool not_lrk = false;
  struct red_state_element **shift_table;
  ArrayBool cyclic = Allocate_bool_array2(ls->num_states + 1);
  /// NT_ITEMS and ITEM_LIST are used to construct a mapping from each
  /// nonterminal into the set of items of which the nonterminal in
  /// question is the dot symbol. See CONFLICTS_INITIALIZATION.
  ArrayShort item_list;
  struct visited_element visited;
  if (cli_options->lalr_level > 1) {
    calloc0p(&shift_table, SHIFT_TABLE_SIZE, struct red_state_element *);
    for ALL_NON_TERMINALS3(symbol) {
      not_lrk = not_lrk || rmpself.raw[symbol];
    }
    ArrayShort index_of = Allocate_short_array2(ls->num_states + 1);
    ArrayShort stack = Allocate_short_array2(ls->num_states + 1);
    for ALL_STATES3(state_no, ls->num_states) {
      index_of.raw[state_no] = OMEGA;
    }
    struct CyclicTop top = {.top = 0};
    for ALL_STATES3(state_no, ls->num_states) {
      if (index_of.raw[state_no] == OMEGA) {
        compute_cyclic(state_no, stack, index_of, cyclic, &top, null_nt, statset);
      }
      not_lrk = not_lrk || cyclic.raw[state_no];
    }
    ffree(stack.raw);
    ffree(index_of.raw);
    ses->sources = allocate_sources(ls);
    calloc0p(&visited.map, ls->num_states + 1, struct node *);
    visited.list = Allocate_short_array2(ls->num_states + 1);
    visited.root = NIL;
  }
  // IN_STAT is a mapping from each state to the set of states that have
  // a transition into the state in question.
  // IN_STAT is used to construct a reverse transition map. See
  // BUILD_IN_STAT for more detail.
  struct node **in_stat = NULL;
  calloc0p(&in_stat, ls->num_states + 1, struct node *);
  // RULE_COUNT is an array used to count the number of reductions on
  // particular rules within a given state.
  ArrayShort rule_count = Allocate_short_array2(num_rules + 1);
  // NO_SHIFT_ON_ERROR_SYM is a vector used to identify states that
  // contain shift actions on the %ERROR symbol.  Such states are marked
  // only when DEFAULT_OPT is 5.
  ArrayBool no_shift_on_error_sym = Allocate_bool_array2(ls->num_states + 1);
  // SYMBOL_LIST is used to construct temporary lists of terminals on
  // which reductions are defined.
  ArrayShort symbol_list = Allocate_short_array2(num_terminals + 1);
  // When default actions are requested, the vector SINGLE_COMPLETE_ITEM
  // is used to identify states that contain exactly one final item.
  // NOTE that when the READ_REDUCE options is turned on, the LR(0)
  // automaton constructed contains no such state.
  ArrayBool single_complete_item = Allocate_bool_array2(ls->num_states + 1);
  struct node **action;
  // ACTION is an array that is used as the base for a mapping from
  // each terminal symbol into a list of actions that can be executed
  // on that symbol in a given state.
  calloc0p(&action, num_terminals + 1, struct node *);
  // LOOK_AHEAD is used to compute lookahead sets.
  JBitset look_ahead;
  calloc0_set(look_ahead, 1, dss->term_set_size);
  // If we will be removing single productions, we need to keep
  // track of all (state, symbol) pairs on which a conflict is
  // detected. The structure conflict_symbols is used as a base
  // to construct that map. See ADD_CONFLICT_SYMBOL in resolve.c.
  // NOTE that this allocation automatically initialized all
  // elements of the conflict_symbols array to NULL.
  /// CONFLICT_SYMBOLS is a mapping from each state into a set of terminal
  /// symbols on which an LALR(1) conflict was detected in the state in
  /// question.
  struct node **conflict_symbols = NULL;
  if (cli_options->single_productions_bit) {
    calloc0p(&conflict_symbols, ls->num_states + 1, struct node *);
  }
  // First, construct the IN_STAT map. Next, iterate over the states to
  // construct two boolean vectors.  One indicates whether there is a
  // shift action on the ERROR symbol when the DEFAULT_OPT is 5.  The
  // other indicates whether it is all right to take default action in
  // states containing exactly one final item.
  //
  // We also check whether the grammar is LR(0). I.e., whether it needs
  // any look-ahead at all.
  build_in_stat(srt, statset, in_stat, ls);
  for ALL_STATES3(state_no, ls->num_states) {
    no_shift_on_error_sym.raw[state_no] = true;
    if (cli_options->default_opt.value == OPT_5.value) {
      int n = statset[state_no].shift_number;
      const struct shift_header_type sh = srt->shift[n];
      for (int i = 1; i <= sh.size; ++i) {
        if (sh.map[i].symbol == error_image) {
          no_shift_on_error_sym.raw[state_no] = false;
        }
      }
    }
    //   Compute whether this state is a final state.  I.e., a state that
    // contains only a single complete item. If so, mark it as a default
    // state. Note that if the READ-REDUCE option is used, the automaton
    // will not contain such states. Also, states are marked only when
    // default actions are requested.
    struct node *item_ptr = statset[state_no].kernel_items;
    int item_no = item_ptr->value;
    single_complete_item.raw[state_no] = !cli_options->read_reduce_bit && !cli_options->single_productions_bit && cli_options->table_opt.value != OPTIMIZE_TIME.value && cli_options->table_opt.value != OPTIMIZE_SPACE.value && cli_options->default_opt.value > OPT_0.value && item_ptr->next == NULL && item_table[item_no].symbol == empty;
    // If a state has a complete item, and more than one kernel item
    // which is different from the complete item, then this state
    // requires look-ahead for the complete item.
    if (strs.highest_level == 0) {
      const struct node *r = statset[state_no].complete_items;
      if (r != NULL) {
        if (item_ptr->next != NULL || item_ptr->value != r->value) {
          strs.highest_level = 1;
        }
      }
    }
  }
  JBitset read_set = {.raw = NULL};
  if (cli_options->lalr_level > 1 || cli_options->single_productions_bit) {
    calloc0_set(read_set, ls->num_states + 1, dss->term_set_size);
  }
  struct LAIndex lai = (struct LAIndex) {
    .la_index = Allocate_short_array2(*la_top + 1),
  };
  // We call COMPUTE_READ to perform the following tasks:
  // 1) Count how many elements are needed in LA_ELEMENT: LA_TOP
  // 2) Allocate space for and initialize LA_SET and LA_INDEX
  compute_read(cli_options, dss, single_complete_item, first, read_set, &lai, adequate_item, srt, statset, rules, item_table, in_stat, la_top, ls);
  // Allocate space for REDUCE which will be used to map each
  // into its reduce map. We also initialize RULE_COUNT which
  // will be used to count the number of reduce actions on each
  // rule with in a given state.
  calloc0p(&srt->reduce, ls->num_states + 1, struct reduce_header_type);
  for ALL_RULES3(i) {
    rule_count.raw[i] = 0;
  }
  // We are now ready to construct the reduce map. First, we
  // initialize MAX_LA_STATE to NUM_STATES. If no lookahead
  // state is added (the grammar is LALR(1)) this value will not
  // change. Otherwise, MAX_LA_STATE is incremented by 1 for each
  // lookahead state added.
  ls->max_la_state = ls->num_states;
  // We iterate over the states, compute the lookahead sets,
  // resolve conflicts (if multiple lookahead is requested) and/or
  // report the conflicts if requested...
  struct StackPool sp = (struct StackPool) {
    .stack_pool = NULL,
    .dangling_stacks = NULL,
  };
  struct ConflictPool cp = (struct ConflictPool) {
    .conflict_element_pool = NULL,
  };
  long num_rr_conflicts = 0;
  long num_sr_conflicts = 0;
  for ALL_STATES3(state_no, ls->num_states) {
    int default_rule = OMEGA;
    int symbol_root = NIL;
    struct node *item_ptr = statset[state_no].complete_items;
    if (item_ptr != NULL) {
      // Check if it is possible to take default reduction. The DEFAULT_OPT
      // parameter indicates what kind of default options are requested.
      // The various values it can have are:
      //
      //    a)   0 => no default reduction.
      //    b)   1 => default reduction only on adequate states. I.e.,
      //              states with only one complete item in their kernel.
      //    c)   2 => Default on all states that contain exactly one
      //              complete item not derived from an empty rule.
      //    d)   3 => Default on all states that contain exactly one
      //              complete item including items from empty rules.
      //    e)   4 => Default reduction on all states that contain exactly
      //              one item. If a state contains more than one item we
      //              take Default on the item that generated the most
      //              reductions. If there is a tie, one is selected at
      //              random.
      //    f)   5 => Same as 4 except that no default actions are computed
      //              for states that contain a shift action on the ERROR
      //              symbol.
      //
      //  In the code below, we first check for category 3.  If it is not
      // satisfied, then we check for the others. Note that in any case,
      // default reductions are never taken on the ACCEPT rule.
      int item_no = item_ptr->value;
      int rule_no = item_table[item_no].rule_number;
      int symbol = rules[rule_no].lhs;
      if (single_complete_item.raw[state_no] && symbol != accept_image) {
        default_rule = rule_no;
        item_ptr = NULL; /* No need to check for conflicts */
      }
      // Iterate over all complete items in the state, build action
      // map, and check for conflicts.
      for (; item_ptr != NULL; item_ptr = item_ptr->next) {
        // for all complete items
        item_no = item_ptr->value;
        rule_no = item_table[item_no].rule_number;
        compute_la(state_no, item_no, look_ahead, &sr, first, &lai, adequate_item, rules, in_stat, statset, item_table);
        for ALL_TERMINALS3(symbol) {
          // for all symbols in la set
          if (IS_ELEMENT(look_ahead, symbol)) {
            struct node *p = Allocate_node();
            p->value = item_no;
            if (action[symbol] == NULL) {
              symbol_list.raw[symbol] = symbol_root;
              symbol_root = symbol;
            } else {
              // Always place the rule with the largest
              // right-hand side first in the list.
              int n = item_table[action[symbol]->value].rule_number;
              if (RHS_SIZE(n, rules) >= RHS_SIZE(rule_no, rules)) {
                p->value = action[symbol]->value;
                action[symbol]->value = item_no;
              }
            }
            p->next = action[symbol];
            action[symbol] = p;
          }
        }
      }
      // At this stage, we have constructed the ACTION map for STATE_NO.
      // ACTION is a map from each symbol into a set of final items.
      // The rules associated with these items are the rules by which
      // to reduce when the lookahead is the symbol in question.
      // SYMBOL_LIST/SYMBOL_ROOT is a list of the non-empty elements of
      // ACTION. If the number of elements in a set ACTION(t), for some
      // terminal t, is greater than one or it is not empty and STATE_NO
      // contains a shift action on t then STATE_NO has one or more
      // conflict(s). The procedure RESOLVE_CONFLICTS takes care of
      // resolving the conflicts appropriately and returns an ACTION
      // map where each element has either 0 (if the conflicts were
      // shift-reduce conflicts, the shift is given precedence) or 1
      // element (if the conflicts were reduce-reduce conflicts, only
      // the first element in the ACTION(t) list is returned).
      if (symbol_root != NIL) {
        struct ConflictCounter cc_ = resolve_conflicts(state_no, action, symbol_list, symbol_root, cli_options, dss, shift_table, cyclic, &item_list, &sp, &cp, &visited, ses, &strs, &sr, rmpself, first, read_set, &lai, conflict_symbols, adequate_item, srt, las->lastats, null_nt, in_stat, rules, item_table, statset, rhs_sym, la_top, string_table, symno, ls);
        num_rr_conflicts += cc_.num_rr_conflicts;
        num_sr_conflicts += cc_.num_sr_conflicts;
        for (symbol = symbol_root; symbol != NIL; symbol = symbol_list.raw[symbol]) {
          if (action[symbol] != NULL) {
            item_no = action[symbol]->value;
            rule_count.raw[item_table[item_no].rule_number]++;
          }
        }
      }
    }
    // We are now ready to compute the size of the reduce map for
    // STATE_NO (reduce_size) and the default rule.
    // If the state being processed contains only a single complete item
    // then the DEFAULT_RULE was previously computed and the list of
    // symbols is empty.
    // NOTE: a REDUCE_ELEMENT will be allocated for all states, even
    // those that have no reductions at all. This will facilitate the
    // Table Compression routines, for they can assume that such an
    // object exists, and can be used for Default values.
    int reduce_size = 0;
    if (symbol_root != NIL) {
      // Compute REDUCE_SIZE, the number of reductions in the state and
      // DEFAULT_RULE: the rule with the highest number of reductions
      // to it.
      int n = 0;
      for (struct node *q = statset[state_no].complete_items; q != NULL; q = q->next) {
        const int item_no = q->value;
        const int rule_no = item_table[item_no].rule_number;
        const int symbol = rules[rule_no].lhs;
        reduce_size += rule_count.raw[rule_no];
        if (rule_count.raw[rule_no] > n && no_shift_on_error_sym.raw[state_no] && symbol != accept_image) {
          n = rule_count.raw[rule_no];
          default_rule = rule_no;
        }
      }
      //   If the removal of single productions is requested
      // and/or parsing tables will not be output, figure out
      // if the level of the default option requested permits
      // default actions, and compute how many reduce actions
      // can be eliminated as a result.
      if (cli_options->default_opt.value == OPT_0.value) {
        default_rule = OMEGA;
      } else if (cli_options->table_opt.value != OPTIMIZE_TIME.value && cli_options->table_opt.value != OPTIMIZE_SPACE.value && !cli_options->single_productions_bit) {
        struct node *q = statset[state_no].complete_items;
        if (q->next == NULL) {
          const int item_no = q->value;
          const int rule_no = item_table[item_no].rule_number;
          if (cli_options->default_opt.value > OPT_2.value || /* No empty rule defined */
              (cli_options->default_opt.value == OPT_2.value && RHS_SIZE(rule_no, rules) != 0)) {
            reduce_size -= n;
          } else {
            default_rule = OMEGA;
          }
        } else if (cli_options->default_opt.value > OPT_3.value)
          reduce_size -= n;
      }
      num_reductions += reduce_size;
    }
    //   NOTE that the default fields are set for all states,
    // whether DEFAULT actions are requested. This is
    // all right since one can always check whether (DEFAULT > 0)
    // before using these fields.
    const struct reduce_header_type red = Allocate_reduce_map(reduce_size);
    srt->reduce[state_no] = red;
    red.map[0].symbol = DEFAULT_SYMBOL;
    red.map[0].rule_number = default_rule;
    for (int symbol = symbol_root; symbol != NIL; symbol = symbol_list.raw[symbol]) {
      if (action[symbol] != NULL) {
        const int rule_no = item_table[action[symbol]->value].rule_number;
        if (rule_no != default_rule ||
            cli_options->table_opt.value == OPTIMIZE_SPACE.value ||
            cli_options->table_opt.value == OPTIMIZE_TIME.value ||
            cli_options->single_productions_bit) {
          red.map[reduce_size].symbol = symbol;
          red.map[reduce_size].rule_number = rule_no;
          reduce_size--;
        }
        free_nodes(action[symbol], action[symbol]);
        action[symbol] = NULL;
      }
    }
    // Reset RULE_COUNT elements used in this state.
    for (struct node *q = statset[state_no].complete_items; q != NULL; q = q->next) {
      const int rule_no = item_table[q->value].rule_number;
      rule_count.raw[rule_no] = 0;
    }
  }
  printf("\n");
  // If the automaton required multiple lookahead, construct the
  // permanent lookahead states.
  if (ls->max_la_state > ls->num_states) {
    create_lastats(&strs, srt, statset, las, ls);
  }
  // We are now finished with the LALR(k) construction of the
  // automaton. Clear all temporary space that was used in that
  // process and calculate the maximum lookahead level that was
  // needed.
  exit_lalrk_process(cli_options, shift_table, cyclic, &sp, &visited, ses, rmpself);
  cli_options->lalr_level = strs.highest_level;
  // If the removal of single productions is requested, do that.
  if (cli_options->single_productions_bit) {
    remove_single_productions(dss, &sr, first, &lai, conflict_symbols, lai.la_set, adequate_item, srt, statset, las->lastats, gd_index, in_stat, rules, item_table, rhs_sym, ls);
  }
  // If either more than one lookahead was needed or the removal
  // of single productions was requested, the automaton was
  // transformed with the addition of new states and new
  // transitions. In such a case, we reconstruct the IN_STAT map.
  if (cli_options->lalr_level > 1 || cli_options->single_productions_bit) {
    for ALL_STATES3(state_no, ls->num_states) {
      // First, clear out the previous map
      if (in_stat[state_no] != NULL) {
        struct node *q = in_stat[state_no]->next; /* point to root */
        free_nodes(q, in_stat[state_no]);
        in_stat[state_no] = NULL;
      }
    }
    build_in_stat(srt, statset, in_stat, ls); /* rebuild in_stat map */
  }
  // Print informational messages and free all temporary space that
  // was used to compute lookahead information.
  if (not_lrk) {
    printf("This grammar is not LR(K).\n\n");
  } else {
    if (num_rr_conflicts > 0 || num_sr_conflicts > 0) {
      if (strs.highest_level != INFINITY) {
        printf("This grammar is not LALR(%d).\n\n", strs.highest_level);
      } else {
        printf("This grammar is not LALR(K).\n\n");
      }
    } else {
      if (strs.highest_level == 0) {
        printf("This grammar is LR(0).\n\n");
      } else {
        printf("This grammar is LALR(%d).\n\n", strs.highest_level);
      }
    }
  }
  ffree(rule_count.raw);
  ffree(no_shift_on_error_sym.raw);
  ffree(symbol_list.raw);
  ffree(single_complete_item.raw);
  ffree(action);
  ffree(look_ahead.raw);
  if (conflict_symbols != NULL) {
    ffree(conflict_symbols);
  }
  if (read_set.raw != NULL) {
    ffree(read_set.raw);
  }
  if (lai.la_index.raw != NULL) {
    ffree(lai.la_index.raw);
  }
  if (lai.la_set.raw != NULL) {
    ffree(lai.la_set.raw);
  }
  return (struct ConflictCounter) {
    .num_rr_conflicts = num_rr_conflicts,
    .num_sr_conflicts = num_sr_conflicts,
    .in_stat = in_stat,
  };
}

// endregion































// region ctabs

const int increment = 30;

struct OutputPtr {
  char **output_ptr;
  char **output_buffer;
};

/// The following macro definitions are used only in processing the output.
static void BUFFER_CHECK(FILE *file, struct OutputPtr output_ptr2) {
  if (IOBUFFER_SIZE - ((*output_ptr2.output_ptr) - &(*output_ptr2.output_buffer)[0]) < 73) {
    fwrite((*output_ptr2.output_buffer), sizeof(char), (*output_ptr2.output_ptr) - &(*output_ptr2.output_buffer)[0], file);
    (*output_ptr2.output_ptr) = &(*output_ptr2.output_buffer)[0];
  }
}

const long MAX_TABLE_SIZE = (USHRT_MAX < INT_MAX ? USHRT_MAX : INT_MAX) - 1;

struct ByteTerminalRange {
  bool value;
};

/// ITOC takes as arguments an integer NUM. NUM is an integer containing at
/// most 11 digits which is converted into a character string and placed in
/// the iobuffer. Leading zeros are eliminated and if the number is
/// negative, a leading "-" is added.
static void itoc(const int num, struct OutputPtr output_ptr2) {
  char tmp[12];
  long val = ABS(num);
  tmp[11] = '\0';
  char *p = &tmp[11];
  do {
    p--;
    *p = "0123456789"[val % 10];
    val /= 10;
  } while (val > 0);
  if (num < 0) {
    p--;
    *p = '-';
  }
  while (*p != '\0') {
    *(*output_ptr2.output_ptr)++ = *p++;
  }
}

static void padline(struct OutputPtr output_ptr2) {
  for (int i = 0; i < 12; i++) {
    *(*output_ptr2.output_ptr)++ = ' ';
  }
}

static void mystrcpy(const char *str, const struct OutputFiles* of, struct OutputPtr output_ptr2) {
  while (*str != '\0') {
    *(*output_ptr2.output_ptr)++ = *str++;
  }
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  BUFFER_CHECK(of->syssym, output_ptr2);
}

static void prnt_longs(const char *title, const int init, const int bound, const int perline, ArrayLong array, const struct CLIOptions *cli_options, struct OutputFiles* of, struct OutputPtr output_ptr2) {
  mystrcpy(title, of, output_ptr2);
  padline(output_ptr2);
  int k = 0;
  for (int i = init; i <= bound; i++) {
    itoc(array.raw[i], output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
    k++;
    if (k == perline && i != bound) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 0;
    }
  }
  if (k != 0) {
    *((*output_ptr2.output_ptr) - 1) = '\n';
    BUFFER_CHECK(of->sysdcl, output_ptr2);
  }
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of, output_ptr2);
  } else {
    mystrcpy("                 };\n", of, output_ptr2);
  }
}

/// This procedure computes the range of the ACTION_SYMBOLS map after
/// Optimal Partitioning has been used to compress that map.  Its
/// first argument is an array, STATE_START, that indicates the
/// starting location in the compressed vector for each state.  When
/// a value of STATE_START is negative it indicates that the state in
/// question shares its elements with another state.  Its second
/// argument, STATE_STACK, is an array that contains the elements of
/// the partition created by PARTSET.  Each element of the partition
/// is organized as a circular list where the smallest sets appear
/// first in the list.
static void compute_action_symbols_range(const ArrayLong state_start, const ArrayLong state_stack, const ArrayLong state_list, ArrayLong action_symbols_range, struct SRTable* srt, const struct statset_type *statset, struct LAState* ls) {
  ArrayShort symbol_list = Allocate_short_array2(num_symbols + 1);
  // We now write out the range elements of the ACTION_SYMBOLS map.
  // Recall that if STATE_START has a negative value, then the set in
  // question is sharing elements and does not need to be processed.
  int k = 0;
  for ALL_SYMBOLS3(j) {
    symbol_list.raw[j] = OMEGA; /* Initialize all links to OMEGA */
  }
  for ALL_STATES3(state_no, ls->num_states) {
    const int state_no__ = state_list.raw[state_no];
    if (state_start.raw[state_no__] > 0) {
      int symbol_root = 0; /* Add "fence" element: 0 to list */
      symbol_list.raw[symbol_root] = NIL;
      // Pop a state from the stack,  and add each of its elements
      // that has not yet been processed into the list.
      // Continue until stack is empty...
      // Recall that the stack is represented by a circular queue.
      int state;
      for (bool end_node = (state = state_no__) == NIL; !end_node; end_node = state == state_no__) {
        state = state_stack.raw[state];
        const struct shift_header_type sh = srt->shift[statset[state].shift_number];
        for (int j = 1; j <= sh.size; j++) {
          int symbol = sh.map[j].symbol;
          if (symbol_list.raw[symbol] == OMEGA) {
            symbol_list.raw[symbol] = symbol_root;
            symbol_root = symbol;
          }
        }
        const struct reduce_header_type red = srt->reduce[state];
        for (int j = 1; j <= red.size; j++) {
          int symbol = red.map[j].symbol;
          if (symbol_list.raw[symbol] == OMEGA) {
            symbol_list.raw[symbol] = symbol_root;
            symbol_root = symbol;
          }
        }
      }
      // Write the list out.
      for (int symbol = symbol_root; symbol != NIL; symbol = symbol_root) {
        symbol_root = symbol_list.raw[symbol];
        symbol_list.raw[symbol] = OMEGA;
        action_symbols_range.raw[k++] = symbol;
      }
    }
  }
  ffree(symbol_list.raw);
}

/// This procedure computes the range of the NACTION_SYMBOLS map. It
/// organization is analoguous to COMPUTE_ACTION_SYMBOLS_RANGE.
static void compute_naction_symbols_range(const ArrayLong state_start, const ArrayLong state_stack, const ArrayLong state_list, ArrayLong naction_symbols_range, ArrayShort gd_index, ArrayShort gd_range, struct LAState* ls) {
  ArrayShort symbol_list = Allocate_short_array2(num_symbols + 1);
  // We now write out the range elements of the NACTION_SYMBOLS map.
  // Recall that if STATE_START has a negative value, then the set in
  // question is sharing elements and does not need to be processed.
  int k = 0;
  for ALL_SYMBOLS3(j) {
    symbol_list.raw[j] = OMEGA; /* Initialize all links to OMEGA */
  }
  for ALL_STATES3(state_no, ls->num_states) {
    const int state_no__ = state_list.raw[state_no];
    if (state_start.raw[state_no__] > 0) {
      int symbol_root = 0; /* Add "fence" element: 0 to list */
      symbol_list.raw[symbol_root] = NIL;
      // Pop a state from the stack,  and add each of its elements
      // that has not yet been processed into the list.
      // Continue until stack is empty...
      // Recall that the stack is represented by a circular queue.
      int state;
      for (bool end_node = (state = state_no__) == NIL; !end_node; end_node = state == state_no__) {
        state = state_stack.raw[state];
        for (int j = gd_index.raw[state]; j <= gd_index.raw[state + 1] - 1; j++) {
          int symbol = gd_range.raw[j];
          if (symbol_list.raw[symbol] == OMEGA) {
            symbol_list.raw[symbol] = symbol_root;
            symbol_root = symbol;
          }
        }
      }
      // Write the list out.
      for (int symbol = symbol_root; symbol != NIL; symbol = symbol_root) {
        symbol_root = symbol_list.raw[symbol];
        symbol_list.raw[symbol] = OMEGA;
        naction_symbols_range.raw[k++] = symbol;
      }
    }
  }
  ffree(symbol_list.raw);
}

static void exit_file(FILE **file, char *file_tag, struct CLIOptions *cli_options) {
  if (cli_options->c_bit || cli_options->cpp_bit) {
    fprintf(*file, "\n#endif /* %s_INCLUDED */\n", file_tag);
  }
}

static void print_error_maps(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct ByteTerminalRange* btr, struct scope_type *scope, struct SRTable* srt, ArrayLong scope_right_side, struct statset_type *statset, ArrayShort gd_index, ArrayShort gd_range, ArrayShort scope_state, struct itemtab *item_table, struct symno_type *symno, bool error_maps_bit, struct ScopeCounter* sc, struct OutputPtr output_ptr2, char *string_table, int *name, struct LAState* ls) {
  ArrayLong state_start = Allocate_long_array2(ls->num_states + 2);
  ArrayLong state_stack = Allocate_long_array2(ls->num_states + 1);
  PRNT("\nError maps storage:");
  // We now construct a bit map for the set of terminal symbols that
  // may appear in each state. Then, we invoke PARTSET to apply the
  // Partition Heuristic and print it.
  ArrayLong as_size = Allocate_long_array2(ls->num_states + 1);
  ArrayLong original;
  if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    original = Allocate_long_array2(num_symbols + 1);
    // In a compressed TIME table, the terminal and non-terminal
    // symbols are mixed together when they are remapped.
    // We shall now recover the original number associated with
    // each terminal symbol since it lies very nicely in the
    // range 1..NUM_TERMINALS.  This will save a considerable
    // amount of space in the bit_string representation of sets
    // as well as time when operations are performed on those
    // bit-strings.
    for ALL_TERMINALS3(symbol) {
      original.raw[toutput->symbol_map.raw[symbol]] = symbol;
    }
  }
  JBitset action_symbols;
  if (error_maps_bit) {
    calloc0_set(action_symbols, ls->num_states + 1, dss->term_set_size);
  }
  for ALL_STATES3(state_no, ls->num_states) {
    struct shift_header_type sh;
    struct reduce_header_type red;
    sh = srt->shift[statset[state_no].shift_number];
    as_size.raw[state_no] = sh.size;
    for (int i = 1; i <= sh.size; i++) {
      int symbol;
      if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
        symbol = original.raw[sh.map[i].symbol];
      } else {
        symbol = sh.map[i].symbol;
      }
      SET_BIT_IN(action_symbols, state_no, symbol);
    }
    red = srt->reduce[state_no];
    as_size.raw[state_no] += red.size;
    for (int i = 1; i <= red.size; i++) {
      int symbol;
      if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
        symbol = original.raw[red.map[i].symbol];
      } else {
        symbol = red.map[i].symbol;
      }
      SET_BIT_IN(action_symbols, state_no, symbol);
    }
  }
  partset(action_symbols, as_size, toutput->state_list, state_start, state_stack, num_terminals, false, ls);
  ffree(action_symbols.raw);
  // Compute and write out the base of the ACTION_SYMBOLS map.
  ArrayLong action_symbols_base = Allocate_long_array2(ls->num_states + 1);
  for ALL_STATES3(state_no, ls->num_states) {
    action_symbols_base.raw[toutput->state_list.raw[state_no]] = ABS(state_start.raw[toutput->state_list.raw[state_no]]);
  }
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char asb[] = {0,\n", 1, ls->num_states, 10, action_symbols_base, cli_options, of, output_ptr2);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER asb[] = {0,\n", 1, ls->num_states, 10, action_symbols_base, cli_options, of, output_ptr2);
  }
  ffree(action_symbols_base.raw);
  // Compute and write out the range of the ACTION_SYMBOLS map.
  int offset = state_start.raw[ls->num_states + 1];
  ArrayLong action_symbols_range = Allocate_long_array2(offset);
  compute_action_symbols_range(state_start, state_stack, toutput->state_list, action_symbols_range, srt, statset, ls);
  for (int i = 0; i < offset - 1; i++) {
    if (action_symbols_range.raw[i] > (cli_options->java_bit ? 127 : 255)) {
      btr->value = 0;
      break;
    }
  }
  if (btr->value) {
    if (cli_options->java_bit) {
      prnt_longs("\n    public final static byte asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range, cli_options, of, output_ptr2);
    } else {
      prnt_longs("\nconst unsigned char  CLASS_HEADER asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range, cli_options, of, output_ptr2);
    }
  } else {
    if (cli_options->java_bit) {
      prnt_longs("\n    public final static char asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range, cli_options, of, output_ptr2);
    } else {
      prnt_longs("\nconst unsigned short CLASS_HEADER asr[] = {0,\n", 0, offset - 2, 10, action_symbols_range, cli_options, of, output_ptr2);
    }
  }
  long num_bytes = 2 * ls->num_states;
  PRNT3("    Storage required for ACTION_SYMBOLS_BASE map: %ld Bytes", num_bytes);
  if (cli_options->table_opt.value == OPTIMIZE_TIME.value && ctp->last_terminal <= (cli_options->java_bit ? 127 : 255)) {
    num_bytes = offset - 1;
  } else if (cli_options->table_opt.value != OPTIMIZE_TIME.value && num_terminals <= (cli_options->java_bit ? 127 : 255)) {
    num_bytes = offset - 1;
  } else {
    num_bytes = 2 * (offset - 1);
  }
  PRNT3("    Storage required for ACTION_SYMBOLS_RANGE map: %ld Bytes", num_bytes);
  ffree(action_symbols_range.raw);
  JBitset naction_symbols;
  if (error_maps_bit) {
    calloc0_set(naction_symbols, ls->num_states + 1, dss->non_term_set_size);
  }
  // We now repeat the same process for the domain of the GOTO table.
  for ALL_STATES3(state_no, ls->num_states) {
    as_size.raw[state_no] = gd_index.raw[state_no + 1] - gd_index.raw[state_no];
    for (int i = gd_index.raw[state_no]; i <= gd_index.raw[state_no + 1] - 1; i++) {
      int symbol = gd_range.raw[i] - num_terminals;
      SET_BIT_IN(naction_symbols, state_no, symbol);
    }
  }
  partset(naction_symbols, as_size, toutput->state_list, state_start, state_stack, num_non_terminals, false, ls);
  ffree(as_size.raw);
  ffree(naction_symbols.raw);
  // Remap non-terminals
  for (int i = 1; i <= gotodom_size; i++) {
    if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
      gd_range.raw[i] = toutput->symbol_map.raw[gd_range.raw[i]] - num_terminals;
    } else {
      gd_range.raw[i] = toutput->symbol_map.raw[gd_range.raw[i]];
    }
  }
  // Compute and write out the base of the NACTION_SYMBOLS map.
  ArrayLong naction_symbols_base = Allocate_long_array2(ls->num_states + 1);
  for ALL_STATES3(state_no, ls->num_states) {
    naction_symbols_base.raw[toutput->state_list.raw[state_no]] = ABS(state_start.raw[toutput->state_list.raw[state_no]]);
  }
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char nasb[] = {0,\n", 1, ls->num_states, 10, naction_symbols_base, cli_options, of, output_ptr2);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER nasb[] = {0,\n", 1, ls->num_states, 10, naction_symbols_base, cli_options, of, output_ptr2);
  }
  ffree(naction_symbols_base.raw);
  // Compute and write out the range of the NACTION_SYMBOLS map.
  offset = state_start.raw[ls->num_states + 1];
  ArrayLong naction_symbols_range = Allocate_long_array2(offset);
  compute_naction_symbols_range(state_start, state_stack, toutput->state_list, naction_symbols_range, gd_index, gd_range, ls);
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char nasr[] = {0,\n", 0, offset - 2, 10, naction_symbols_range, cli_options, of, output_ptr2);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER nasr[] = {0,\n", 0, offset - 2, 10, naction_symbols_range, cli_options, of, output_ptr2);
  }
  PRNT3("    Storage required for NACTION_SYMBOLS_BASE map: %ld Bytes", 2 * ls->num_states);
  PRNT3("    Storage required for NACTION_SYMBOLS_RANGE map: %d Bytes", 2 * (offset - 1));
  ffree(naction_symbols_range.raw);
  // We write the name_index of each terminal symbol.  The array TEMP
  // is used to remap the NAME_INDEX values based on the new symbol
  // numberings. If time tables are requested, the terminals and non-
  // terminals are mixed together.
  ArrayLong temp = Allocate_long_array2(num_symbols + 1);
  if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
    for ALL_TERMINALS3(symbol) {
      temp.raw[toutput->symbol_map.raw[symbol]] = symno[symbol].name_index;
    }
    if (num_names <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte terminal_index[] = {0,\n", 1, num_terminals, 10, temp, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER terminal_index[] = {0,\n", 1, num_terminals, 10, temp, cli_options, of, output_ptr2);
      }
      num_bytes = num_terminals;
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char terminal_index[] = {0,\n", 1, num_terminals, 10, temp, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER terminal_index[] = {0,\n", 1, num_terminals, 10, temp, cli_options, of, output_ptr2);
      }
      num_bytes = 2 * num_terminals;
    }
    // Compute and list space required for TERMINAL_INDEX map.
    PRNT3("    Storage required for TERMINAL_INDEX map: %ld Bytes", num_bytes);
    // We write the name_index of each non_terminal symbol. The array
    // TEMP is used to remap the NAME_INDEX values based on the new
    // symbol numberings.
    for ALL_NON_TERMINALS3(symbol) {
      temp.raw[toutput->symbol_map.raw[symbol]] = symno[symbol].name_index;
    }
    if (num_names <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
      }
      num_bytes = num_non_terminals;
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER non_terminal_index[] = {0,\n", num_terminals + 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
      }
      num_bytes = 2 * num_non_terminals;
    }
    // Compute and list space required for NON_TERMINAL_INDEX map.
    PRNT3("    Storage required for NON_TERMINAL_INDEX map: %ld Bytes", num_bytes);
  } else {
    for ALL_SYMBOLS3(symbol) {
      temp.raw[toutput->symbol_map.raw[symbol]] = symno[symbol].name_index;
    }
    if (num_names <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte symbol_index[] = {0,\n", 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
        mystrcpy("    public final static byte terminal_index[] = symbol_index;\n", of, output_ptr2);
        mystrcpy("    public final static byte non_terminal_index[] = symbol_index;\n", of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER symbol_index[] = {0,\n", 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
        mystrcpy("const unsigned char  *CLASS_HEADER terminal_index[] = &(symbol_index[0]);\n", of, output_ptr2);
        mystrcpy("const unsigned char  *CLASS_HEADER non_terminal_index[] = &(symbol_index[0]);\n", of, output_ptr2);
      }
      num_bytes = num_symbols;
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char symbol_index[] = {0,\n", 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
        mystrcpy("    public final static char terminal_index[] = symbol_index[0];\n", of, output_ptr2);
        mystrcpy("    public final static char non_terminal_index[] = symbol_index;\n", of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER symbol_index[] = {0,\n", 1, num_symbols, 10, temp, cli_options, of, output_ptr2);
        mystrcpy("const unsigned short *CLASS_HEADER terminal_index[] = &(symbol_index[0]);\n", of, output_ptr2);
        mystrcpy("const unsigned short *CLASS_HEADER non_terminal_index[] = &(symbol_index[0]);\n", of, output_ptr2);
      }
      num_bytes = 2 * num_symbols;
    }
    // Compute and list space required for SYMBOL_INDEX map.
    PRNT3("    Storage required for SYMBOL_INDEX map: %ld Bytes", num_bytes);
  }
  if (sc->num_scopes > 0) {
    short root = 0;
    ArrayShort list = Allocate_short_array2(sc->scope_rhs_size + 1);
    for (int i = 1; i <= sc->scope_rhs_size; i++) {
      if (scope_right_side.raw[i] != 0) {
        scope_right_side.raw[i] = toutput->symbol_map.raw[scope_right_side.raw[i]];
      }
    }
    for (int i = 1; i <= sc->num_scopes; i++) {
      scope[i].look_ahead = toutput->symbol_map.raw[scope[i].look_ahead];
      if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
        scope[i].lhs_symbol = toutput->symbol_map.raw[scope[i].lhs_symbol] - num_terminals;
      } else {
        scope[i].lhs_symbol = toutput->symbol_map.raw[scope[i].lhs_symbol];
      }
    }
    // Mark all elements of prefix strings.
    for (int i = 1; i <= sc->scope_rhs_size; i++) {
      list.raw[i] = -1;
    }
    for (int i = 1; i <= sc->num_scopes; i++) {
      if (list.raw[scope[i].suffix] < 0) {
        list.raw[scope[i].suffix] = root;
        root = scope[i].suffix;
      }
    }
    for (; root != 0; root = list.raw[root]) {
      for (int j = root; scope_right_side.raw[j] != 0; j++) {
        int k = scope_right_side.raw[j];
        scope_right_side.raw[j] = temp.raw[k];
      }
    }
    ffree(list.raw);
  }
  if (cli_options->java_bit) {
    // Print java names.
    long num_bytes = 0;
    long max_name_length = 0;
    mystrcpy("\n    public final static String name[] = { null,\n", of, output_ptr2);
    for (int i = 1; i <= num_names; i++) {
      char tok[SYMBOL_SIZE + 1];
      strcpy(tok, RETRIEVE_NAME(i, string_table, name));
      const int len = strlen(tok);
      num_bytes += len * 2;
      if (max_name_length < len) {
        max_name_length = len;
      }
      padline(output_ptr2);
      *(*output_ptr2.output_ptr)++ = '\"';
      int k = 0;
      for (int j = 0; j < len; j++) {
        if (tok[j] == '\"' || tok[j] == '\\') {
          *(*output_ptr2.output_ptr)++ = '\\';
        }
        if (tok[j] == '\n') {
          *(*output_ptr2.output_ptr)++ = cli_options->escape;
        } else {
          *(*output_ptr2.output_ptr)++ = tok[j];
        }
        k++;
        if (k == 30 && j != len - 1) {
          k = 0;
          *(*output_ptr2.output_ptr)++ = '\"';
          *(*output_ptr2.output_ptr)++ = ' ';
          *(*output_ptr2.output_ptr)++ = '+';
          *(*output_ptr2.output_ptr)++ = '\n';
          BUFFER_CHECK(of->sysdcl, output_ptr2);
          padline(output_ptr2);
          *(*output_ptr2.output_ptr)++ = '\"';
        }
      }
      *(*output_ptr2.output_ptr)++ = '\"';
      if (i < num_names) {
        *(*output_ptr2.output_ptr)++ = ',';
      }
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    // Compute and list space required for STRING_BUFFER map.
    PRNT3("    Storage required for STRING_BUFFER map: %ld Bytes", num_bytes);
  } else {
    // Print C names.
    ArrayLong name_len = Allocate_long_array2(num_names + 1);
    long num_bytes = 0;
    long max_name_length = 0;
    mystrcpy("\nconst char  CLASS_HEADER string_buffer[] = {0,\n", of, output_ptr2);
    int n = 0;
    padline(output_ptr2);
    for (int i = 1; i <= num_names; i++) {
      char tok[SYMBOL_SIZE + 1];
      strcpy(tok, RETRIEVE_NAME(i, string_table, name));
      name_len.raw[i] = strlen(tok);
      num_bytes += name_len.raw[i];
      if (max_name_length < name_len.raw[i]) {
        max_name_length = name_len.raw[i];
      }
      int k = 0;
      for (int j = 0; j < name_len.raw[i]; j++) {
        *(*output_ptr2.output_ptr)++ = '\'';
        if (tok[k] == '\'' || tok[k] == '\\') {
          *(*output_ptr2.output_ptr)++ = '\\';
        }
        if (tok[k] == '\n') {
          *(*output_ptr2.output_ptr)++ = cli_options->escape;
        } else {
          *(*output_ptr2.output_ptr)++ = tok[k];
        }
        k++;
        *(*output_ptr2.output_ptr)++ = '\'';
        *(*output_ptr2.output_ptr)++ = ',';
        n++;
        if (n == 10 && !(i == num_names && j == name_len.raw[i] - 1)) {
          n = 0;
          *(*output_ptr2.output_ptr)++ = '\n';
          BUFFER_CHECK(of->sysdcl, output_ptr2);
          padline(output_ptr2);
        }
      }
    }
    *((*output_ptr2.output_ptr) - 1) = '\n'; /*overwrite last comma*/
    BUFFER_CHECK(of->sysdcl, output_ptr2);
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    // Compute and list space required for STRING_BUFFER map.
    PRNT3("    Storage required for STRING_BUFFER map: %ld Bytes", num_bytes);
    // Write out NAME_START array
    mystrcpy("\nconst unsigned short CLASS_HEADER name_start[] = {0,\n", of, output_ptr2);
    padline(output_ptr2);
    int j = 1;
    int k = 0;
    for (int i = 1; i <= num_names; i++) {
      itoc(j, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      j += name_len.raw[i];
      k++;
      if (k == 10 && i != num_names) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    // Compute and list space required for NAME_START map.
    PRNT3("    Storage required for NAME_START map: %ld Bytes", 2 * num_names);
    // Write out NAME_LENGTH array
    prnt_longs("\nconst unsigned char  CLASS_HEADER name_length[] = {0,\n", 1, num_names, 10, name_len, cli_options, of, output_ptr2);
    // Compute and list space required for NAME_LENGTH map.
    PRNT3("    Storage required for NAME_LENGTH map: %ld Bytes", num_names);
    ffree(name_len.raw);
  }
  if (sc->num_scopes > 0) {
    if (sc->scope_rhs_size <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_prefix[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_prefix[] = {\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_prefix[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_prefix[] = {\n", of, output_ptr2);
      }
    }
    padline(output_ptr2);
    int k = 0;
    for (int i = 1; i <= sc->num_scopes; i++) {
      itoc(scope[i].prefix, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != sc->num_scopes) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    if (sc->scope_rhs_size <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_suffix[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_suffix[] = {\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_suffix[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_suffix[] = {\n", of, output_ptr2);
      }
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= sc->num_scopes; i++) {
      itoc(scope[i].suffix, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != sc->num_scopes) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    if (num_symbols <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_lhs[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_lhs[] = {\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_lhs[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_lhs[] = {\n", of, output_ptr2);
      }
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= sc->num_scopes; i++) {
      itoc(scope[i].lhs_symbol, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != sc->num_scopes) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    if (num_terminals <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_la[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_la[] = {\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_la[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_la[] = {\n", of, output_ptr2);
      }
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= sc->num_scopes; i++) {
      itoc(scope[i].look_ahead, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != sc->num_scopes) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    if (sc->scope_state_size <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte scope_state_set[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER scope_state_set[] = {\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char scope_state_set[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER scope_state_set[] = {\n", of, output_ptr2);
      }
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= sc->num_scopes; i++) {
      itoc(scope[i].state_set, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != sc->num_scopes) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    if (num_symbols <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static byte scope_rhs[] = {0,\n", 1, sc->scope_rhs_size, 10, scope_right_side, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned char  CLASS_HEADER scope_rhs[] = {0,\n", 1, sc->scope_rhs_size, 10, scope_right_side, cli_options, of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        prnt_longs("\n    public final static char scope_rhs[] = {0,\n", 1, sc->scope_rhs_size, 10, scope_right_side, cli_options, of, output_ptr2);
      } else {
        prnt_longs("\nconst unsigned short CLASS_HEADER scope_rhs[] = {0,\n", 1, sc->scope_rhs_size, 10, scope_right_side, cli_options, of, output_ptr2);
      }
    }
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char scope_state[] = {0,\n", of, output_ptr2);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER scope_state[] = {0,\n", of, output_ptr2);
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= sc->scope_state_size; i++) {
      if (scope_state.raw[i] == 0) {
        itoc(0, output_ptr2);
      } else {
        itoc(toutput->state_index.raw[scope_state.raw[i]] + num_rules, output_ptr2);
      }
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != sc->scope_state_size) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
    if (num_symbols <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte in_symb[] = {0,\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER in_symb[] = {0,\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char in_symb[] = {0,\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER in_symb[] = {0,\n", of, output_ptr2);
      }
    }
    // Transition symbol
    padline(output_ptr2);
    *(*output_ptr2.output_ptr)++ = '0';
    *(*output_ptr2.output_ptr)++ = ',';
    k = 1;
    for (int state_no = 2; state_no <= ls->num_states; state_no++) {
      struct node *q;
      q = statset[state_no].kernel_items;
      int i;
      if (q != NULL) {
        int item_no;
        item_no = q->value - 1;
        i = item_table[item_no].symbol;
      } else {
        i = 0;
      }
      itoc(toutput->symbol_map.raw[i], output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && state_no != ls->num_states) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                          };\n", of, output_ptr2);
    }
  }
}

static void common(const bool byte_check_bit, struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, ArrayLong scope_right_side, struct statset_type *statset, ArrayShort gd_index, ArrayShort gd_range, ArrayShort scope_state, struct itemtab *item_table, struct ScopeCounter* sc, char *output_buffer, struct OutputPtr output_ptr2, struct symno_type *symno, char *string_table, int *name, struct LAState* ls) {
  struct ByteTerminalRange btr = (struct ByteTerminalRange) {
    .value = true
  };
  // Write table common.
  {
    if (cli_options->error_maps_bit) {
      print_error_maps(cli_options, toutput, dss, ctp, of, &btr, scope, srt, scope_right_side, statset, gd_index, gd_range, scope_state, item_table, symno, cli_options->error_maps_bit, sc, output_ptr2, string_table, name, ls);
    }
    if (byte_check_bit) {
      // Do nothing.
    } else {
      if (cli_options->java_bit) {
        PRNT("\n***Warning: Base Check vector contains value > 127. 16-bit words used.");
      } else {
        PRNT("\n***Warning: Base Check vector contains value > 255. 16-bit words used.");
      }
    }
    if (btr.value) {
      // Do nothing.
    } else {
      if (cli_options->java_bit) {
        PRNT("***Warning: Terminal symbol > 127. 16-bit words used.");
      } else {
        PRNT("***Warning: Terminal symbol > 255. 16-bit words used.");
      }
    }
    if (cli_options->java_bit) {
      mystrcpy("}\n", of, output_ptr2);
    }
    fwrite(output_buffer, sizeof(char), (*output_ptr2.output_ptr) - &output_buffer[0], of->sysdcl);
  }

  // Print symbols.
  {
    int line_size = SYMBOL_SIZE + /* max length of a token symbol  */
              2 * MAX_PARM_SIZE + /* max length of prefix + suffix */
              64;
    char line[line_size]; /* +64 for error messages lines  */
    // or other fillers(blank, =,...)
    if (cli_options->java_bit) {
      strcpy(line, "interface ");
      strcat(line, of->sym_tag);
      strcat(line, "\n{\n    public final static int\n");
    } else {
      strcpy(line, "enum {\n");
    }
    // We write the terminal symbols map.
    for ALL_TERMINALS3(symbol) {
      char *tok = RETRIEVE_STRING(symbol, string_table, symno);
      fprintf(of->syssym, "%s", line);
      if (tok[0] == '\n' || tok[0] == cli_options->escape) {
        tok[0] = cli_options->escape;
        PRNT4(line, line_size, "Escaped symbol %s is an invalid C variable.\n", tok);
      } else if (strpbrk(tok, "!%^&*()-+={}[];:\"`~|\\,.<>/?\'") != NULL) {
        PRNT4(line, line_size, "%s may be an invalid variable name.\n", tok);
      }
      snprintf(line, sizeof(line), "      %s%s%s = %li,\n", cli_options->prefix, tok, cli_options->suffix, toutput->symbol_map.raw[symbol]);
      if (cli_options->c_bit || cli_options->cpp_bit) {
        while (strlen(line) > PARSER_LINE_SIZE) {
          fwrite(line, sizeof(char), PARSER_LINE_SIZE - 2, of->syssym);
          fprintf(of->syssym, "\\\n");
          memmove(line, &line[PARSER_LINE_SIZE - 2], strlen(&line[PARSER_LINE_SIZE - 2]) + 1);
        }
      }
    }
    line[strlen(line) - 2] = '\0'; /* remove the string ",\n" from last line */
    fprintf(of->syssym, "%s%s", line, cli_options->java_bit ? ";\n}\n" : "\n     };\n");
  }

  // Print definitions.
  {
    if (cli_options->java_bit) {
      fprintf(of->sysdef, "interface %s\n{\n    public final static int\n\n", of->def_tag);
    } else {
      fprintf(of->sysdef, "enum {\n");
    }
    if (cli_options->error_maps_bit) {
      if (cli_options->java_bit) {
        fprintf(of->sysdef,
                "      ERROR_SYMBOL      = %d,\n"
                "      NUM_STATES        = %ld,\n\n",
                error_image,
                ls->num_states);
      } else {
        fprintf(of->sysdef,
                "      ERROR_SYMBOL      = %d,\n"
                "      NUM_STATES        = %ld,\n\n",
                error_image,
                ls->num_states);
      }
    }
    if (cli_options->java_bit) {
      fprintf(of->sysdef,
              "      NT_OFFSET         = %ld,\n"
              "      SCOPE_UBOUND      = %ld,\n"
              "      SCOPE_SIZE        = %ld,\n"
              "      LA_STATE_OFFSET   = %ld,\n"
              "      MAX_LA            = %d,\n"
              "      NUM_RULES         = %ld,\n"
              "      NUM_TERMINALS     = %ld,\n"
              "      NUM_NON_TERMINALS = %ld,\n"
              "      NUM_SYMBOLS       = %ld,\n"
              "      START_STATE       = %ld,\n"
              "      EOFT_SYMBOL       = %d,\n"
              "      EOLT_SYMBOL       = %d,\n"
              "      ACCEPT_ACTION     = %ld,\n"
              "      ERROR_ACTION      = %ld;\n"
              "};\n\n",
              cli_options->table_opt.value == OPTIMIZE_SPACE.value ? num_terminals : num_symbols,
              sc->num_scopes - 1,
              sc->num_scopes,
              cli_options->read_reduce_bit && cli_options->lalr_level > 1
                ? ia->error_act + num_rules
                : ia->error_act,
              cli_options->lalr_level,
              num_rules,
              num_terminals,
              num_non_terminals,
              num_symbols,
              toutput->state_index.raw[1] + num_rules,
              eoft_image,
              eolt_image,
              ia->accept_act,
              ia->error_act);
    } else {
      fprintf(of->sysdef,
              "      NT_OFFSET         = %ld,\n"
              "      STACK_UBOUND      = %d,\n"
              "      STACK_SIZE        = %d,\n"
              "      SCOPE_UBOUND      = %ld,\n"
              "      SCOPE_SIZE        = %ld,\n"
              "      LA_STATE_OFFSET   = %ld,\n"
              "      MAX_LA            = %d,\n"
              "      NUM_RULES         = %ld,\n"
              "      NUM_TERMINALS     = %ld,\n"
              "      NUM_NON_TERMINALS = %ld,\n"
              "      NUM_SYMBOLS       = %ld,\n"
              "      START_STATE       = %ld,\n"
              "      EOFT_SYMBOL       = %d,\n"
              "      EOLT_SYMBOL       = %d,\n"
              "      ACCEPT_ACTION     = %ld,\n"
              "      ERROR_ACTION      = %ld\n"
              "     };\n\n",
              cli_options->table_opt.value == OPTIMIZE_SPACE.value ? num_terminals : num_symbols,
              cli_options->stack_size - 1,
              cli_options->stack_size,
              sc->num_scopes - 1,
              sc->num_scopes,
              cli_options->read_reduce_bit && cli_options->lalr_level > 1
                ? ia->error_act + num_rules
                : ia->error_act,
              cli_options->lalr_level,
              num_rules,
              num_terminals,
              num_non_terminals,
              num_symbols,
              toutput->state_index.raw[1] + num_rules,
              eoft_image,
              eolt_image,
              ia->accept_act,
              ia->error_act);
    }
  }

  // Print externs.
  {
    if (cli_options->c_bit || cli_options->cpp_bit) {
      fprintf(of->sysprs,
              "%s SCOPE_REPAIR\n"
              "%s FULL_DIAGNOSIS\n"
              "%s SPACE_TABLES\n\n",
              sc->num_scopes > 0 ? "#define" : "#undef ",
              cli_options->error_maps_bit ? "#define" : "#undef ",
              cli_options->table_opt.value == OPTIMIZE_SPACE.value ? "#define" : "#undef ");
    }
    if (cli_options->c_bit) {
      fprintf(of->sysprs,
              "#define original_state(state) (-%s[state])\n"
              "#define asi(state)            asb[original_state(state)]\n"
              "#define nasi(state)           nasb[original_state(state)]\n"
              "#define in_symbol(state)      in_symb[original_state(state)]\n\n",
              cli_options->table_opt.value == OPTIMIZE_TIME.value ? "check" : "base_check");
    } else if (cli_options->cpp_bit) {
      fprintf(of->sysprs,
              "class LexStream;\n\n"
              "class %s_table\n"
              "{\n"
              "public:\n", of->prs_tag);
      if (cli_options->error_maps_bit) {
        fprintf(of->sysprs, "    static int original_state(int state) { return -%s[state]; }\n", cli_options->table_opt.value == OPTIMIZE_TIME.value ? "check" : "base_check");
      }
      if (cli_options->error_maps_bit) {
        fprintf(of->sysprs,
                "    static int asi(int state) "
                "{ return asb[original_state(state)]; }\n"
                "    static int nasi(int state) "
                "{ return nasb[original_state(state)]; }\n");
        if (sc->num_scopes > 0) {
          fprintf(of->sysprs,
                  "    static int in_symbol(int state) "
                  "{ return in_symb[original_state(state)]; }\n");
        }
      }
      fprintf(of->sysprs, "\n");
    } else if (cli_options->java_bit) {
      fprintf(of->sysprs, "abstract class %s extends %s implements %s\n{\n", of->prs_tag, of->dcl_tag, of->def_tag);
      if (cli_options->error_maps_bit) {
        fprintf(of->sysprs, "    public final static int original_state(int state) { return -%s(state); }\n", cli_options->table_opt.value == OPTIMIZE_TIME.value ? "check" : "base_check");
        if (cli_options->error_maps_bit) {
          fprintf(of->sysprs, "    public final static int asi(int state) { return asb[original_state(state)]; }\n");
          fprintf(of->sysprs, "    static int nasi(int state) { return nasb[original_state(state)]; }\n");
          if (sc->num_scopes > 0)
            fprintf(of->sysprs, "    public final static int in_symbol(int state) { return in_symb[original_state(state)]; }\n");
        }
        fprintf(of->sysprs, "\n");
      }
    }
    if (cli_options->c_bit || cli_options->cpp_bit) {
      fprintf(of->sysprs, "%s const unsigned char  rhs[];\n", cli_options->c_bit ? "extern" : "    static");
      if (ctp->check_size > 0 || cli_options->table_opt.value == OPTIMIZE_TIME.value) {
        const bool small = byte_check_bit && !cli_options->error_maps_bit;
        fprintf(of->sysprs, "%s const %s check_table[];\n"
                "%s const %s *%s;\n",
                cli_options->c_bit ? "extern" : "    static",
                small ? "unsigned char " : "  signed short",
                cli_options->c_bit ? "extern" : "    static",
                small ? "unsigned char " : "  signed short",
                cli_options->table_opt.value == OPTIMIZE_TIME.value ? "check" : "base_check");
      }
      fprintf(of->sysprs, "%s const unsigned short lhs[];\n"
              "%s const unsigned short *%s;\n",
              cli_options->c_bit ? "extern" : "    static",
              cli_options->c_bit ? "extern" : "    static",
              cli_options->table_opt.value == OPTIMIZE_TIME.value ? "action" : "base_action");
      if (cli_options->goto_default_bit) {
        fprintf(of->sysprs, "%s const unsigned short default_goto[];\n", cli_options->c_bit ? "extern" : "    static");
      }
      if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
        fprintf(of->sysprs, "%s const unsigned %s term_check[];\n", cli_options->c_bit ? "extern" : "    static", num_terminals <= (cli_options->java_bit ? 127 : 255) ? "char " : "short");
        fprintf(of->sysprs, "%s const unsigned short term_action[];\n", cli_options->c_bit ? "extern" : "    static");
        if (cli_options->shift_default_bit) {
          fprintf(of->sysprs, "%s const unsigned short default_reduce[];\n", cli_options->c_bit ? "extern" : "    static");
          fprintf(of->sysprs, "%s const unsigned short shift_state[];\n", cli_options->c_bit ? "extern" : "    static");
          fprintf(of->sysprs, "%s const unsigned %s shift_check[];\n", cli_options->c_bit ? "extern" : "    static", num_terminals <= (cli_options->java_bit ? 127 : 255) ? "char " : "short");
          fprintf(of->sysprs, "%s const unsigned short default_shift[];\n", cli_options->c_bit ? "extern" : "    static");
        }
      }
      if (cli_options->error_maps_bit) {
        fprintf(of->sysprs,
                "\n"
                "%s const unsigned short asb[];\n"
                "%s const unsigned %s asr[];\n"
                "%s const unsigned short nasb[];\n"
                "%s const unsigned short nasr[];\n"
                "%s const unsigned short name_start[];\n"
                "%s const unsigned char  name_length[];\n"
                "%s const          char  string_buffer[];\n",
                cli_options->c_bit ? "extern" : "    static",
                cli_options->c_bit ? "extern" : "    static",
                btr.value <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                cli_options->c_bit ? "extern" : "    static",
                cli_options->c_bit ? "extern" : "    static",
                cli_options->c_bit ? "extern" : "    static",
                cli_options->c_bit ? "extern" : "    static",
                cli_options->c_bit ? "extern" : "    static");
        if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
          fprintf(of->sysprs,
                  "%s const unsigned %s terminal_index[];\n"
                  "%s const unsigned %s non_terminal_index[];\n",
                  cli_options->c_bit ? "extern" : "    static",
                  num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short");
        } else {
          fprintf(of->sysprs, "%s const unsigned %s symbol_index[];\n"
                  "%s const unsigned %s *terminal_index;\n"
                  "%s const unsigned %s *non_terminal_index;\n",
                  cli_options->c_bit ? "extern" : "    static",
                  num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  num_names <= (cli_options->java_bit ? 127 : 255) ? "char " : "short");
        }
        if (sc->num_scopes > 0) {
          fprintf(of->sysprs, "%s const unsigned %s scope_prefix[];\n"
                  "%s const unsigned %s scope_suffix[];\n"
                  "%s const unsigned %s scope_lhs[];\n"
                  "%s const unsigned %s scope_la[];\n"
                  "%s const unsigned %s scope_state_set[];\n"
                  "%s const unsigned %s scope_rhs[];\n"
                  "%s const unsigned short scope_state[];\n"
                  "%s const unsigned %s in_symb[];\n",
                  cli_options->c_bit ? "extern" : "    static",
                  sc->scope_rhs_size <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  sc->scope_rhs_size <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  num_symbols <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  num_terminals <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  sc->scope_state_size <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  num_symbols <= (cli_options->java_bit ? 127 : 255) ? "char " : "short",
                  cli_options->c_bit ? "extern" : "    static",
                  cli_options->c_bit ? "extern" : "    static",
                  num_symbols <= (cli_options->java_bit ? 127 : 255) ? "char " : "short");
        }
      }
      fprintf(of->sysprs, "\n");
    }
    if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
      if (cli_options->goto_default_bit) {
        // non_terminal_space_action
        {
          if (cli_options->c_bit) {
            fprintf(of->sysprs,
                    "#define nt_action(state, sym) \\\n"
                    "           ((base_check[state + sym] == sym) ? \\\n"
                    "               base_action[state + sym] : "
                    "default_goto[sym])\n\n");
          } else if (cli_options->cpp_bit) {
            fprintf(of->sysprs,
                    "    static int nt_action(int state, int sym)\n"
                    "    {\n"
                    "        return (base_check[state + sym] == sym)\n"
                    "                             ? base_action[state + sym]\n"
                    "                             : default_goto[sym];\n"
                    "    }\n\n");
          } else if (cli_options->java_bit) {
            fprintf(of->sysprs,
                    "    public final static int nt_action(int state, int sym)\n"
                    "    {\n"
                    "        return (base_check(state + sym) == sym)\n"
                    "                             ? base_action[state + sym]\n"
                    "                             : default_goto[sym];\n"
                    "    }\n\n");
          }
        }
      } else {
        // non_terminal_no_goto_default_space_action
        {
          if (cli_options->c_bit) {
            fprintf(of->sysprs,
                    "#define nt_action(state, sym) "
                    "base_action[state + sym]\n\n");
          } else if (cli_options->cpp_bit) {
            fprintf(of->sysprs,
                    "    static int nt_action(int state, int sym)\n"
                    "    {\n        return base_action[state + sym];\n    }\n\n");
          } else if (cli_options->java_bit) {
            fprintf(of->sysprs,
                    "    public final static int nt_action(int state, int sym)\n"
                    "    {\n        return base_action[state + sym];\n    }\n\n");
          }
        }
      }
      if (cli_options->lalr_level > 1) {
        if (cli_options->shift_default_bit) {
          // terminal_shift_default_space_lalr_k
          {
            if (cli_options->c_bit) {
              fprintf(of->sysprs,
                      "static int t_action(int state, int sym, TokenObject next_tok)\n"
                      "{\n"
                      "    int act = base_action[state],\n"
                      "          i = act + sym;\n\n"
                      "    if (sym == 0)\n"
                      "        act = ERROR_ACTION;\n"
                      "    else if (term_check[i] == sym)\n"
                      "        act = term_action[i];\n"
                      "    else\n"
                      "    {\n"
                      "        act = term_action[act];\n"
                      "        i = shift_state[act] + sym;\n"
                      "        act = (shift_check[i] == sym ? default_shift[sym]\n"
                      "                                     : default_reduce[act]);\n"
                      "    }\n\n"
                      "    while (act > LA_STATE_OFFSET)\n"
                      "    {\n"
                      "         act -= LA_STATE_OFFSET;\n"
                      "         sym = Class(next_tok);\n"
                      "         i = act + sym;\n"
                      "         if (term_check[i] == sym)\n"
                      "             act = term_action[i];\n"
                      "         else\n"
                      "         {\n"
                      "             act = term_action[act];\n"
                      "             i = shift_state[act] + sym;\n"
                      "             act = (shift_check[i] == sym\n"
                      "                                    ? default_shift[sym]\n"
                      "                                    : default_reduce[act]);\n"
                      "         }\n"
                      "         if (act <= LA_STATE_OFFSET)\n"
                      "             break;\n"
                      "         next_tok = Next(next_tok);\n"
                      "    }\n\n"
                      "    return act;\n"
                      "}\n\n");
            } else if (cli_options->cpp_bit) {
              fprintf(of->sysprs,
                      "    static int t_action(int act, int sym, LexStream *stream)\n"
                      "    {\n"
                      "        act = base_action[act];\n"
                      "        int i = act + sym;\n\n"
                      "        if (sym == 0)\n"
                      "            act = ERROR_ACTION;\n"
                      "        else if (term_check[i] == sym)\n"
                      "            act = term_action[i];\n"
                      "        else\n"
                      "        {\n"
                      "            act = term_action[act];\n"
                      "            i = shift_state[act] + sym;\n"
                      "            act = (shift_check[i] == sym ? default_shift[sym]\n"
                      "                                         : default_reduce[act]);\n"
                      "        }\n\n"
                      "        if (act > LA_STATE_OFFSET)\n"
                      "        {\n"
                      "            for (TokenObject tok = stream -> Peek();\n"
                      "                 ;\n"
                      "                 tok = stream -> Next(tok))\n"
                      "            {\n"
                      "                 act -= LA_STATE_OFFSET;\n"
                      "                 sym = stream -> Kind(tok);\n"
                      "                 i = act + sym;\n"
                      "                 if (term_check[i] == sym)\n"
                      "                     act = term_action[i];\n"
                      "                 else\n"
                      "                 {\n"
                      "                     act = term_action[act];\n"
                      "                     i = shift_state[act] + sym;\n"
                      "                     act = (shift_check[i] == sym\n"
                      "                                            ? default_shift[sym]\n"
                      "                                            : default_reduce[act]);\n"
                      "                 }\n"
                      "                 if (act <= LA_STATE_OFFSET)\n"
                      "                     break;\n"
                      "            }\n"
                      "        }\n\n"
                      "        return act;\n"
                      "    }\n");
            } else if (cli_options->java_bit) {
              fprintf(of->sysprs,
                      "    public final static int t_action(int act, int sym, LexStream stream)\n"
                      "    {\n"
                      "        act = base_action[act];\n"
                      "        int i = act + sym;\n\n"
                      "        if (sym == 0)\n"
                      "            act = ERROR_ACTION;\n"
                      "        else if (term_check[i] == sym)\n"
                      "            act = term_action[i];\n"
                      "        else\n"
                      "        {\n"
                      "            act = term_action[act];\n"
                      "            i = shift_state[act] + sym;\n"
                      "            act = (shift_check[i] == sym ? default_shift[sym]\n"
                      "                                         : default_reduce[act]);\n"
                      "        }\n\n"
                      "        if (act > LA_STATE_OFFSET)\n"
                      "        {\n"
                      "            for (int tok = stream.Peek();\n"
                      "                 ;\n"
                      "                 tok = stream.Next(tok))\n"
                      "            {\n"
                      "                 act -= LA_STATE_OFFSET;\n"
                      "                 sym = stream.Kind(tok);\n"
                      "                 i = act + sym;\n"
                      "                 if (term_check[i] == sym)\n"
                      "                     act = term_action[i];\n"
                      "                 else\n"
                      "                 {\n"
                      "                     act = term_action[act];\n"
                      "                     i = shift_state[act] + sym;\n"
                      "                     act = (shift_check[i] == sym\n"
                      "                                            ? default_shift[sym]\n"
                      "                                            : default_reduce[act]);\n"
                      "                 }\n"
                      "                 if (act <= LA_STATE_OFFSET)\n"
                      "                     break;\n"
                      "            }\n"
                      "        }\n\n"
                      "        return act;\n"
                      "    }\n");
            }
          }
        } else {
          // terminal_space_lalr_k
          {
            if (cli_options->c_bit) {
              fprintf(of->sysprs,
                      "static int t_action(int state, int sym, TokenObject next_tok)\n"
                      "{\n"
                      "    int act = base_action[state],\n"
                      "          i = act + sym;\n\n"
                      "    act = term_action[term_check[i] == sym ? i : act];\n\n"
                      "    if (act > LA_STATE_OFFSET)\n"
                      "    {\n"
                      "        for (;;)\n"
                      "        {\n"
                      "           act -= LA_STATE_OFFSET;\n"
                      "           sym = Class(next_tok);\n"
                      "           i = act + sym;\n"
                      "           act = term_action[term_check[i] == sym ? i : act];\n"
                      "           if (act <= LA_STATE_OFFSET)\n"
                      "               break;\n"
                      "           next_tok = Next(next_tok);\n"
                      "        }\n"
                      "    }\n\n"
                      "    return act;\n"
                      "}\n\n");
            } else if (cli_options->cpp_bit) {
              fprintf(of->sysprs,
                      "    static int t_action(int act, int sym, LexStream *stream)\n"
                      "    {\n"
                      "        act = base_action[act];\n"
                      "        int i = act + sym;\n\n"
                      "        act = term_action[term_check[i] == sym ? i : act];\n\n"
                      "        if (act > LA_STATE_OFFSET)\n"
                      "        {\n"
                      "            for (TokenObject tok = stream -> Peek();\n"
                      "                 ;\n"
                      "                 tok = stream -> Next(tok))\n"
                      "            {\n"
                      "               act -= LA_STATE_OFFSET;\n"
                      "               sym = stream -> Kind(tok);\n"
                      "               i = act + sym;\n"
                      "               act = term_action[term_check[i] == sym ? i : act];\n"
                      "               if (act <= LA_STATE_OFFSET)\n"
                      "                   break;\n"
                      "            } \n"
                      "        }\n\n"
                      "        return act;\n"
                      "    }\n");
            } else if (cli_options->java_bit) {
              fprintf(of->sysprs,
                      "    public final static int t_action(int act, int sym, LexStream stream)\n"
                      "    {\n"
                      "        act = base_action[act];\n"
                      "        int i = act + sym;\n\n"
                      "        act = term_action[term_check[i] == sym ? i : act];\n\n"
                      "        if (act > LA_STATE_OFFSET)\n"
                      "        {\n"
                      "            for (int tok = stream.Peek();\n"
                      "                 ;\n"
                      "                 tok = stream.Next(tok))\n"
                      "            {\n"
                      "               act -= LA_STATE_OFFSET;\n"
                      "               sym = stream.Kind(tok);\n"
                      "               i = act + sym;\n"
                      "               act = term_action[term_check[i] == sym ? i : act];\n"
                      "               if (act <= LA_STATE_OFFSET)\n"
                      "                   break;\n"
                      "            } \n"
                      "        }\n\n"
                      "        return act;\n"
                      "    }\n");
            }
          }
        }
      } else {
        if (cli_options->shift_default_bit) {
          // terminal_shift_default_space_action
          {
            if (cli_options->c_bit) {
              fprintf(of->sysprs,
                      "static int t_action(int state, int sym, TokenObject next_tok)\n"
                      "{\n"
                      "    int i;\n\n"
                      "    if (sym == 0)\n"
                      "        return ERROR_ACTION;\n"
                      "    i = base_action[state];\n"
                      "    if (term_check[i + sym] == sym)\n"
                      "        return term_action[i + sym];\n"
                      "    i = term_action[i];\n"
                      "    return ((shift_check[shift_state[i] + sym] == sym) ?\n"
                      "                 default_shift[sym] : default_reduce[i]);\n"
                      "}\n\n");
            } else if (cli_options->cpp_bit) {
              fprintf(of->sysprs,
                      "    static int t_action(int state, int sym, LexStream *stream)\n"
                      "    {\n"
                      "        if (sym == 0)\n"
                      "            return ERROR_ACTION;\n"
                      "        int i = base_action[state];\n"
                      "        if (term_check[i + sym] == sym)\n"
                      "            return term_action[i + sym];\n"
                      "        i = term_action[i];\n"
                      "        return ((shift_check[shift_state[i] + sym] == sym) ?\n"
                      "                      default_shift[sym] : default_reduce[i]);\n"
                      "    }\n");
            } else if (cli_options->java_bit) {
              fprintf(of->sysprs,
                      "    public final static int t_action(int state, int sym, LexStream stream)\n"
                      "    {\n"
                      "        if (sym == 0)\n"
                      "            return ERROR_ACTION;\n"
                      "        int i = base_action[state];\n"
                      "        if (term_check[i + sym] == sym)\n"
                      "            return term_action[i + sym];\n"
                      "        i = term_action[i];\n"
                      "        return ((shift_check[shift_state[i] + sym] == sym) ?\n"
                      "                      default_shift[sym] : default_reduce[i]);\n"
                      "    }\n");
            }
          }
        } else {
          // terminal_space_action
          {
            if (cli_options->c_bit) {
              fprintf(of->sysprs,
                      "#define t_action(state, sym, next_tok) \\\n"
                      "  term_action[term_check[base_action[state]+sym] == sym ? \\\n"
                      "          base_action[state] + sym : base_action[state]]\n\n");
            } else if (cli_options->cpp_bit) {
              fprintf(of->sysprs,
                      "    static int t_action(int state, int sym, LexStream *stream)\n"
                      "    {\n"
                      "        return term_action[term_check[base_action[state]"
                      "+sym] == sym\n"
                      "                               ? base_action[state] + sym\n"
                      "                               : base_action[state]];\n"
                      "    }\n");
            } else if (cli_options->java_bit) {
              fprintf(of->sysprs,
                      "    public final static int t_action(int state, int sym, LexStream stream)\n"
                      "    {\n"
                      "        return term_action[term_check[base_action[state]"
                      "+sym] == sym\n"
                      "                               ? base_action[state] + sym\n"
                      "                               : base_action[state]];\n"
                      "    }\n");
            }
          }
        }
      }
    } else {
      if (cli_options->goto_default_bit) {
        // non_terminal_time_action
        {
          if (cli_options->c_bit) {
            fprintf(of->sysprs,
                    "#define nt_action(state, sym) \\\n"
                    "           ((check[state+sym] == sym) ? \\\n"
                    "                   action[state + sym] : "
                    "default_goto[sym])\n\n");
          } else if (cli_options->cpp_bit) {
            fprintf(of->sysprs,
                    "    static int nt_action(int state, int sym)\n"
                    "    {\n"
                    "        return (check[state + sym] == sym)\n"
                    "                                    ? action[state + sym]\n"
                    "                                    : default_goto[sym];\n"
                    "    }\n\n");
          } else if (cli_options->java_bit) {
            fprintf(of->sysprs,
                    "    public final static int nt_action(int state, int sym)\n"
                    "    {\n"
                    "        return (check(state + sym) == sym)\n"
                    "                                    ? action[state + sym]\n"
                    "                                    : default_goto[sym];\n"
                    "    }\n\n");
          }
        }
      } else {
        // non_terminal_no_goto_default_time_action
        {
          if (cli_options->c_bit) {
            fprintf(of->sysprs,
                    "#define nt_action(state, sym) action[state + sym]\n\n");
          } else if (cli_options->cpp_bit) {
            fprintf(of->sysprs,
                    "    static int nt_action(int state, int sym)\n"
                    "    {\n        return action[state + sym];\n    }\n\n");
          } else if (cli_options->java_bit) {
            fprintf(of->sysprs,
                    "    public final static int nt_action(int state, int sym)\n"
                    "    {\n        return action[state + sym];\n    }\n\n");
          }
        }
      }
      if (cli_options->lalr_level > 1) {
        // terminal_time_lalr_k
        {
          if (cli_options->c_bit) {
            fprintf(of->sysprs,
                    "static int t_action(int act, int sym, TokenObject next_tok)\n"
                    "{\n"
                    "    int i = act + sym;\n\n"
                    "    act = action[check[i] == sym ? i : act];\n\n"
                    "    if (act > LA_STATE_OFFSET)\n"
                    "    {\n"
                    "        for (;;)\n"
                    "        {\n"
                    "            act -= ERROR_ACTION;\n"
                    "            sym = Class(next_tok);\n"
                    "            i = act + sym;\n"
                    "            act = action[check[i] == sym ? i : act];\n"
                    "            if (act <= LA_STATE_OFFSET)\n"
                    "                break;\n"
                    "            next_tok = Next(next_tok);\n"
                    "        }\n"
                    "    }\n\n"
                    "    return act;\n"
                    "}\n\n");
          } else if (cli_options->cpp_bit) {
            fprintf(of->sysprs,
                    "    static int t_action(int act, int sym, LexStream *stream)\n"
                    "    {\n"
                    "        int i = act + sym;\n\n"
                    "        act = action[check[i] == sym ? i : act];\n\n"
                    "        if (act > LA_STATE_OFFSET)\n"
                    "        {\n"
                    "            for (TokenObject tok = stream -> Peek();\n"
                    "                 ;\n"
                    "                 tok = stream -> Next(tok))\n"
                    "            {\n"
                    "                act -= ERROR_ACTION;\n"
                    "                sym = stream -> Kind(tok);\n"
                    "                i = act + sym;\n"
                    "                act = action[check[i] == sym ? i : act];\n"
                    "                if (act <= LA_STATE_OFFSET)\n"
                    "                    break;\n"
                    "            }\n"
                    "        }\n\n"
                    "        return act;\n"
                    "    }\n\n");
          } else if (cli_options->java_bit) {
            fprintf(of->sysprs,
                    "    public final static int t_action(int act, int sym, LexStream stream)\n"
                    "    {\n"
                    "        int i = act + sym;\n\n"
                    "        act = action[check(i) == sym ? i : act];\n\n"
                    "        if (act > LA_STATE_OFFSET)\n"
                    "        {\n"
                    "            for (int tok = stream.Peek();\n"
                    "                 ;\n"
                    "                 tok = stream.Next(tok))\n"
                    "            {\n"
                    "                act -= ERROR_ACTION;\n"
                    "                sym = stream.Kind(tok);\n"
                    "                i = act + sym;\n"
                    "                act = action[check(i) == sym ? i : act];\n"
                    "                if (act <= LA_STATE_OFFSET)\n"
                    "                    break;\n"
                    "            }\n"
                    "        }\n\n"
                    "        return act;\n"
                    "    }\n\n");
          }
        }
      } else {
        // terminal_time_action
        {
          if (cli_options->c_bit) {
            fprintf(of->sysprs,
                    "#define t_action(state, sym, next_tok) \\\n"
                    "   action[check[state + sym] == sym ? state + sym : state]\n\n");
          } else if (cli_options->cpp_bit) {
            fprintf(of->sysprs,
                    "    static int t_action(int state, int sym, LexStream *stream)\n"
                    "    {\n"
                    "        return action[check[state + sym] == sym"
                    " ? state + sym : state];\n"
                    "    }\n");
          } else if (cli_options->java_bit) {
            fprintf(of->sysprs,
                    "    public final static int t_action(int state, int sym, LexStream stream)\n"
                    "    {\n"
                    "        return action[check(state + sym) == sym"
                    " ? state + sym : state];\n"
                    "    }\n");
          }
        }
      }
    }
    if (cli_options->cpp_bit) {
      fprintf(of->sysprs, "};\n");
    } else if (cli_options->java_bit) {
      fprintf(of->sysprs, "}\n");
    }
  }

  // Exit parser files.
  {
    exit_file(&of->sysdcl, of->dcl_tag, cli_options);
    exit_file(&of->syssym, of->sym_tag, cli_options);
    exit_file(&of->sysdef, of->def_tag, cli_options);
    exit_file(&of->sysprs, of->prs_tag, cli_options);
    fclose(of->sysdcl);
    fclose(of->syssym);
    fclose(of->sysdef);
    fclose(of->sysprs);
  }
}

/// SORTDES sorts the elements of ARRAY and COUNT in the range LOW..HIGH
/// based on the values of the elements of COUNT. Knowing that the maximum
/// value of the elements of count cannot exceed MAX and cannot be lower
/// than zero, we can use a bucket sort technique.
void sortdes(ArrayLong array, ArrayLong count, const long low, const long high, const long max) {
  // BUCKET is used to hold the roots of lists that contain the
  // elements of each bucket.  LIST is used to hold these lists.
  ArrayLong bucket = Allocate_long_array2(max + 1);
  ArrayLong list = Allocate_long_array2(high - low + 1);
  for (int i = 0; i <= max; i++) {
    bucket.raw[i] = NIL;
  }
  // We now partition the elements to be sorted and place them in their
  // respective buckets.  We iterate backward over ARRAY and COUNT to
  // keep the sorting stable since elements are inserted in the buckets
  // in stack-fashion.
  //
  //   NOTE that it is known that the values of the elements of ARRAY
  // also lie in the range LOW..HIGH.
  for (long i = high; i >= low; i--) {
    const long k = count.raw[i];
    const long element = array.raw[i];
    list.raw[element - low] = bucket.raw[k];
    bucket.raw[k] = element;
  }
  // Iterate over each bucket, and place elements in ARRAY and COUNT
  // in sorted order.  The iteration is done backward because we want
  // the arrays sorted in descending order.
  long k = low;
  for (long i = max; i >= 0; i--) {
    for (long element = bucket.raw[i]; element != NIL; element = list.raw[element - low], k++) {
      array.raw[k] = element;
      count.raw[k] = i;
    }
  }
  ffree(bucket.raw);
  ffree(list.raw);
}

/// This procedure is invoked when the TABLE being used is not large
/// enough. A new table is allocated, the information from the old table
/// is copied, and the old space is released.
void reallocate(struct CLIOptions *cli_options, struct CTabsProps* ctp, struct NextPrevious* np, struct ImportantAspects* ia) {
  if (ctp->table_size == MAX_TABLE_SIZE) {
    PRNTERR2("Table has exceeded maximum limit of %ld", MAX_TABLE_SIZE);
    exit(12);
  }
  const int old_size = ctp->table_size;
  ctp->table_size = MIN(ctp->table_size + ctp->increment_size, MAX_TABLE_SIZE);
  if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    PRNT3("Reallocating storage for TIME table, adding %ld entries", ctp->table_size - old_size);
  } else {
    PRNT3("Reallocating storage for SPACE table, adding %ld entries", ctp->table_size - old_size);
  }
  ArrayLong n = Allocate_long_array2(ctp->table_size + 1);
  ArrayLong p = Allocate_long_array2(ctp->table_size + 1);
  // Copy old information
  for (int i = 1; i <= old_size; i++) {
    n.raw[i] = np->next.raw[i];
    p.raw[i] = np->previous.raw[i];
  }
  ffree(np->next.raw);
  ffree(np->previous.raw);
  np->next = n;
  np->previous = p;
  if (ia->first_index == NIL) {
    ia->first_index = old_size + 1;
    np->previous.raw[ia->first_index] = NIL;
  } else {
    np->next.raw[ia->last_index] = old_size + 1;
    np->previous.raw[old_size + 1] = ia->last_index;
  }
  np->next.raw[old_size + 1] = old_size + 2;
  for (int i = old_size + 2; i < (int) ctp->table_size; i++) {
    np->next.raw[i] = i + 1;
    np->previous.raw[i] = i - 1;
  }
  ia->last_index = ctp->table_size;
  np->next.raw[ia->last_index] = NIL;
  np->previous.raw[ia->last_index] = ia->last_index - 1;
}

void populate_start_file(FILE **file, char *file_tag, struct CLIOptions *cli_options) {
  if (cli_options->c_bit || cli_options->cpp_bit) {
    fprintf(*file, "#ifndef %s_INCLUDED\n", file_tag);
    fprintf(*file, "#define %s_INCLUDED\n\n", file_tag);
  }
}

void print_space_parser(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, ArrayLong term_state_index, ArrayLong shift_check_index, struct CTabsProps* ctp, struct new_state_type *new_state_element, ArrayShort shift_image, ArrayShort real_shift_number, struct OutputFiles* of, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, ArrayLong scope_right_side, ArrayShort shiftdf, ArrayLong gotodef, ArrayShort gd_index, ArrayShort gd_range, struct ruletab_type *rules, ArrayShort scope_state, struct statset_type *statset, struct itemtab *item_table, struct ScopeCounter* sc, char *output_buffer, struct OutputPtr output_ptr2, struct symno_type *symno, char *string_table, int *name, struct LAState* ls)  {
  bool byte_check_bit = true;
  populate_start_file(&of->sysdcl, of->dcl_tag, cli_options);
  populate_start_file(&of->syssym, of->sym_tag, cli_options);
  populate_start_file(&of->sysdef, of->def_tag, cli_options);
  populate_start_file(&of->sysprs, of->prs_tag, cli_options);
  int default_count = 0;
  int goto_count = 0;
  int goto_reduce_count = 0;
  int reduce_count = 0;
  int la_shift_count = 0;
  int shift_count = 0;
  int shift_reduce_count = 0;
  ArrayLong check = Allocate_long_array2(ctp->table_size + 1);
  ArrayLong action = Allocate_long_array2(ctp->table_size + 1);
  (*output_ptr2.output_ptr) = &output_buffer[0];
  // Prepare header card with proper information, and write it out.
  long offset = ia->error_act;
  long la_state_offset;
  if (cli_options->lalr_level > 1) {
    if (cli_options->read_reduce_bit) {
      offset += num_rules;
    }
    la_state_offset = offset;
  } else {
    la_state_offset = ia->error_act;
  }
  if (offset > MAX_TABLE_SIZE + 1) {
    PRNTERR2("Table contains entries that are > %ld; Processing stopped.", MAX_TABLE_SIZE + 1);
    exit(12);
  }
  for (int i = 1; i <= ctp->check_size; i++) {
    check.raw[i] = DEFAULT_SYMBOL;
  }
  for (int i = 1; i <= (int) ctp->action_size; i++) {
    action.raw[i] = ia->error_act;
  }
  //    Update the default non-terminal action of each state with the
  // appropriate corresponding terminal state starting index.
  for (int i = 1; i <= ctp->num_terminal_states; i++) {
    int indx = term_state_index.raw[i];
    int state_no = new_state_element[i].image;
    // Update the action link between the non-terminal and terminal
    // tables. If error-maps are requested, an indirect linking is made
    // as follows:
    //  Each non-terminal row identifies its original state number, and
    // a new vector START_TERMINAL_STATE indexable by state numbers
    // identifies the starting point of each state in the terminal table.
    if (state_no <= ls->num_states) {
      for (; state_no != NIL; state_no = toutput->state_list.raw[state_no]) {
        action.raw[toutput->state_index.raw[state_no]] = indx;
      }
    } else {
      for (; state_no != NIL; state_no = toutput->state_list.raw[state_no]) {
        int act = la_state_offset + indx;
        toutput->state_index.raw[state_no] = act;
      }
    }
  }
  //  Now update the non-terminal tables with the non-terminal actions.
  for ALL_STATES3(state_no, ls->num_states) {
    struct goto_header_type go_to;
    int indx = toutput->state_index.raw[state_no];
    go_to = statset[state_no].go_to;
    for (int j = 1; j <= go_to.size; j++) {
      int symbol = go_to.map[j].symbol;
      int i = indx + symbol;
      if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
        check.raw[i] = symbol;
      }
      int act = go_to.map[j].action;
      if (act > 0) {
        action.raw[i] = toutput->state_index.raw[act] + num_rules;
        goto_count++;
      } else {
        action.raw[i] = -act;
        goto_reduce_count++;
      }
    }
  }
  if (cli_options->error_maps_bit) {
    if (ctp->check_size == 0) {
      ctp->check_size = ctp->action_size;
      for (int i = 0; i <= ctp->check_size; i++) {
        check.raw[i] = 0;
      }
    }
    for ALL_STATES3(state_no, ls->num_states) {
      check.raw[toutput->state_index.raw[state_no]] = -state_no;
    }
  }
  for (int i = 1; i <= ctp->check_size; i++) {
    if (check.raw[i] < 0 || check.raw[i] > (cli_options->java_bit ? 127 : 255)) {
      byte_check_bit = false;
    }
  }
  if (cli_options->c_bit) {
    mystrcpy("\n#define CLASS_HEADER\n\n", of, output_ptr2);
  } else if (cli_options->cpp_bit) {
    mystrcpy("\n#define CLASS_HEADER ", of, output_ptr2);
    mystrcpy(of->prs_tag, of, output_ptr2);
    mystrcpy("_table::\n\n", of, output_ptr2);
  } else {
    mystrcpy("abstract class ", of, output_ptr2);
    mystrcpy(of->dcl_tag, of, output_ptr2);
    mystrcpy(" implements ", of, output_ptr2);
    mystrcpy(of->def_tag, of, output_ptr2);
    mystrcpy("\n{\n", of, output_ptr2);
  }
  // Write size of right hand side of rules followed by CHECK table.
  if (cli_options->java_bit) {
    mystrcpy("    public final static byte rhs[] = {0,\n", of, output_ptr2);
  } else {
    mystrcpy("const unsigned char  CLASS_HEADER rhs[] = {0,\n", of, output_ptr2);
  }
  padline(output_ptr2);
  int k = 0;
  for (int i = 1; i <= num_rules; i++) {
    k++;
    if (k > 15) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 1;
    }
    itoc(RHS_SIZE(i, rules), output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
  }
  *((*output_ptr2.output_ptr) - 1) = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of, output_ptr2);
  } else {
    mystrcpy("                 };\n", of, output_ptr2);
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  if (ctp->check_size > 0) {
    if (byte_check_bit && !cli_options->error_maps_bit) {
      if (cli_options->java_bit) {
        mystrcpy("    public final static byte check_table[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("const unsigned char  CLASS_HEADER check_table[] = {\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("    public final static short check_table[] = {\n", of, output_ptr2);
      } else {
        mystrcpy("const   signed short CLASS_HEADER check_table[] = {\n", of, output_ptr2);
      }
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= ctp->check_size; i++) {
      k++;
      if (k > 10) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 1;
      }
      itoc(check.raw[i], output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
    }
    *((*output_ptr2.output_ptr) - 1) = '\n';
    BUFFER_CHECK(of->sysdcl, output_ptr2);
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                 };\n", of, output_ptr2);
    }
    *(*output_ptr2.output_ptr)++ = '\n';
    if (byte_check_bit && !cli_options->error_maps_bit) {
      if (cli_options->java_bit) {
        mystrcpy("    public final static byte base_check(int i)\n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n", of, output_ptr2);
      } else {
        mystrcpy("const unsigned char  *CLASS_HEADER base_check = &(check_table[0]) - (NUM_RULES + 1);\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("    public final static short base_check(int i) \n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n", of, output_ptr2);
      } else {
        mystrcpy("const   signed short *CLASS_HEADER base_check = &(check_table[0]) - (NUM_RULES + 1);\n", of, output_ptr2);
      }
    }
    *(*output_ptr2.output_ptr)++ = '\n';
  }
  // Write left hand side symbol of rules followed by ACTION table.
  if (cli_options->java_bit) {
    mystrcpy("    public final static char lhs[] = {0,\n", of, output_ptr2);
  } else {
    mystrcpy("const unsigned short CLASS_HEADER lhs[] = {0,\n", of, output_ptr2);
  }
  padline(output_ptr2);
  k = 0;
  for (int i = 1; i <= num_rules; i++) {
    itoc(toutput->symbol_map.raw[rules[i].lhs] - num_terminals, output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
    k++;
    if (k == 15) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 0;
    }
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  *(*output_ptr2.output_ptr)++ = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  padline(output_ptr2);
  k = 0;
  if (cli_options->error_maps_bit) {
    int max_indx;
    max_indx = ia->accept_act - num_rules - 1;
    for (int i = 1; i <= max_indx; i++) {
      check.raw[i] = OMEGA;
    }
    for ALL_STATES3(state_no, ls->num_states) {
      check.raw[toutput->state_index.raw[state_no]] = state_no;
    }
    int j = ls->num_states + 1;
    for (int i = max_indx; i >= 1; i--) {
      int state_no = check.raw[i];
      if (state_no != OMEGA) {
        j--;
        toutput->ordered_state.raw[j] = i + num_rules;
        toutput->state_list.raw[j] = state_no;
      }
    }
  }
  for (int i = 1; i <= (int) ctp->action_size; i++) {
    itoc(action.raw[i], output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
    k++;
    if (k == 10 && i != (int) ctp->action_size) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 0;
    }
  }
  if (k != 0) {
    *((*output_ptr2.output_ptr) - 1) = '\n';
    BUFFER_CHECK(of->sysdcl, output_ptr2);
  }
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of, output_ptr2);
  } else {
    mystrcpy("                 };\n", of, output_ptr2);
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  if (cli_options->java_bit) {
    mystrcpy("    public final static char base_action[] = lhs;\n", of, output_ptr2);
  } else {
    mystrcpy("const unsigned short *CLASS_HEADER base_action = lhs;\n", of, output_ptr2);
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  // Initialize the terminal tables,and update with terminal actions.
  for (int i = 1; i <= ctp->term_check_size; i++) {
    check.raw[i] = DEFAULT_SYMBOL;
  }
  for (int i = 1; i <= ctp->term_action_size; i++) {
    action.raw[i] = ia->error_act;
  }
  for (int state_no = 1; state_no <= ctp->num_terminal_states; state_no++) {
    struct shift_header_type sh;
    struct reduce_header_type red;
    int indx = term_state_index.raw[state_no];
    sh = srt->shift[new_state_element[state_no].shift_number];
    for (int j = 1; j <= sh.size; j++) {
      int symbol = sh.map[j].symbol;
      int act = sh.map[j].action;
      if (!cli_options->shift_default_bit || act != shiftdf.raw[symbol]) {
        int i = indx + symbol;
        check.raw[i] = symbol;
        long result_act;
        if (act > ls->num_states) {
          result_act = toutput->state_index.raw[act];
          la_shift_count++;
        } else if (act > 0) {
          result_act = toutput->state_index.raw[act] + num_rules;
          shift_count++;
        } else {
          result_act = -act + ia->error_act;
          shift_reduce_count++;
        }
        if (result_act > MAX_TABLE_SIZE + 1) {
          PRNTERR2("Table contains look-ahead shift entry that is >%ld; Processing stopped.", MAX_TABLE_SIZE + 1);
          return;
        }
        action.raw[i] = result_act;
      }
    }
    red = new_state_element[state_no].reduce;
    for (int j = 1; j <= red.size; j++) {
      int symbol = red.map[j].symbol;
      int rule_no = red.map[j].rule_number;
      int i = indx + symbol;
      check.raw[i] = symbol;
      action.raw[i] = rule_no;
      reduce_count++;
    }
    int rule_no = red.map[0].rule_number;
    if (rule_no != ia->error_act) {
      default_count++;
    }
    check.raw[indx] = DEFAULT_SYMBOL;
    if (cli_options->shift_default_bit) {
      action.raw[indx] = state_no;
    } else {
      action.raw[indx] = rule_no;
    }
  }
  PRNT("\n\nActions in Compressed Tables:");
  PRNT3("     Number of Shifts: %d", shift_count);
  PRNT3("     Number of Shift/Reduces: %d", shift_reduce_count);
  if (ls->max_la_state > ls->num_states) {
    PRNT3("     Number of Look-Ahead Shifts: %d", la_shift_count);
  }
  PRNT3("     Number of Gotos: %d", goto_count);
  PRNT3("     Number of Goto/Reduces: %d", goto_reduce_count);
  PRNT3("     Number of Reduces: %d", reduce_count);
  PRNT3("     Number of Defaults: %d", default_count);
  // Write Terminal Check Table.
  if (num_terminals <= (cli_options->java_bit ? 127 : 255)) {
    if (cli_options->java_bit) {
      prnt_longs("\n    public final static byte term_check[] = {0,\n", 1, ctp->term_check_size, 15, check, cli_options, of, output_ptr2);
    } else {
      prnt_longs("\nconst unsigned char  CLASS_HEADER term_check[] = {0,\n", 1, ctp->term_check_size, 15, check, cli_options, of, output_ptr2);
    }
  } else {
    if (cli_options->java_bit) {
      prnt_longs("\n    public final static char term_check[] = {0,\n", 1, ctp->term_check_size, 15, check, cli_options, of, output_ptr2);
    } else {
      prnt_longs("\nconst unsigned short CLASS_HEADER term_check[] = {0,\n", 1, ctp->term_check_size, 15, check, cli_options, of, output_ptr2);
    }
  }
  // Write Terminal Action Table.
  if (cli_options->java_bit) {
    prnt_longs("\n    public final static char term_action[] = {0,\n", 1, ctp->term_action_size, 10, action, cli_options, of, output_ptr2);
  } else {
    prnt_longs("\nconst unsigned short CLASS_HEADER term_action[] = {0,\n", 1, ctp->term_action_size, 10, action, cli_options, of, output_ptr2);
  }
  // If GOTO_DEFAULT is requested, we print out the GOTODEF vector.
  if (cli_options->goto_default_bit) {
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char default_goto[] = {0,\n", of, output_ptr2);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER default_goto[] = {0,\n", of, output_ptr2);
    }
    padline(output_ptr2);
    k = 0;
    for ALL_NON_TERMINALS3(symbol) {
      int act = gotodef.raw[symbol];
      long result_act;
      if (act < 0) {
        result_act = -act;
      } else if (act == 0) {
        result_act = ia->error_act;
      } else {
        result_act = toutput->state_index.raw[act] + num_rules;
      }
      itoc(result_act, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && symbol != num_symbols) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                 };\n", of, output_ptr2);
    }
  }
  if (cli_options->shift_default_bit) {
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char default_reduce[] = {0,\n", of, output_ptr2);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER default_reduce[] = {0,\n", of, output_ptr2);
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= ctp->num_terminal_states; i++) {
      struct reduce_header_type red;
      red = new_state_element[i].reduce;
      itoc(red.map[0].rule_number, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != ctp->num_terminal_states) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                 };\n", of, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char shift_state[] = {0,\n", of, output_ptr2);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER shift_state[] = {0,\n", of, output_ptr2);
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 1; i <= ctp->num_terminal_states; i++) {
      itoc(shift_check_index.raw[shift_image.raw[i]], output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && i != ctp->num_terminal_states) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                 };\n", of, output_ptr2);
    }
    for (int i = 1; i <= ctp->shift_check_size; i++) {
      check.raw[i] = DEFAULT_SYMBOL;
    }
    for (int i = 1; i <= ctp->shift_domain_count; i++) {
      struct shift_header_type sh;
      int indx = shift_check_index.raw[i];
      sh = srt->shift[real_shift_number.raw[i]];
      for (int j = 1; j <= sh.size; j++) {
        int symbol = sh.map[j].symbol;
        check.raw[indx + symbol] = symbol;
      }
    }
    if (num_terminals <= (cli_options->java_bit ? 127 : 255)) {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static byte shift_check[] = {0,\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned char  CLASS_HEADER shift_check[] = {0,\n", of, output_ptr2);
      }
    } else {
      if (cli_options->java_bit) {
        mystrcpy("\n    public final static char shift_check[] = {0,\n", of, output_ptr2);
      } else {
        mystrcpy("\nconst unsigned short CLASS_HEADER shift_check[] = {0,\n", of, output_ptr2);
      }
    }
    padline(output_ptr2);
    k = 0;
    int ii;
    for (ii = 1; ii <= ctp->shift_check_size; ii++) {
      itoc(check.raw[ii], output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && ii != ctp->shift_check_size) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                 };\n", of, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char default_shift[] = {0,\n", of, output_ptr2);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER default_shift[] = {0,\n", of, output_ptr2);
    }
    padline(output_ptr2);
    k = 0;
    for ALL_TERMINALS3(symbol) {
      int act = shiftdf.raw[symbol];
      long result_act;
      if (act < 0) {
        result_act = -act + ia->error_act;
      } else if (act == 0) {
        result_act = ia->error_act;
      } else if (act > ls->num_states) {
        result_act = toutput->state_index.raw[act];
      } else {
        result_act = toutput->state_index.raw[act] + num_rules;
      }
      if (result_act > MAX_TABLE_SIZE + 1) {
        PRNTERR2("Table contains look-ahead shift entry that is >%ld; Processing stopped.", MAX_TABLE_SIZE + 1);
        return;
      }
      itoc(result_act, output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && ii != num_terminals) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                 };\n", of, output_ptr2);
    }
  }
  ffree(check.raw);
  ffree(action.raw);
  common(byte_check_bit, cli_options, toutput, dss, ctp, of, scope, ia, srt, scope_right_side, statset, gd_index, gd_range, scope_state, item_table, sc, output_buffer, output_ptr2, symno, string_table, name, ls);
}

void print_time_parser(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, ArrayLong scope_right_side, struct lastats_type *lastats, ArrayLong gotodef, ArrayShort gd_index, ArrayShort gd_range, struct ruletab_type *rules, ArrayShort scope_state, struct statset_type *statset, struct itemtab *item_table, struct ScopeCounter* sc, char *output_buffer, struct OutputPtr output_ptr2, struct symno_type *symno, char *string_table, int *name, struct LAState* ls) {
  bool byte_check_bit = true;
  populate_start_file(&of->sysdcl, of->dcl_tag, cli_options);
  populate_start_file(&of->syssym, of->sym_tag, cli_options);
  populate_start_file(&of->sysdef, of->def_tag, cli_options);
  populate_start_file(&of->sysprs, of->prs_tag, cli_options);
  int la_shift_count = 0;
  int shift_count = 0;
  int goto_count = 0;
  int default_count = 0;
  int reduce_count = 0;
  int shift_reduce_count = 0;
  int goto_reduce_count = 0;
  (*output_ptr2.output_ptr) = &output_buffer[0];
  ArrayLong check = np->next;
  ArrayLong action = np->previous;
  long offset = ia->error_act;
  int la_state_offset;
  if (cli_options->lalr_level > 1) {
    if (cli_options->read_reduce_bit) {
      offset += num_rules;
    }
    la_state_offset = offset;
  } else {
    la_state_offset = ia->error_act;
  }
  if (offset > MAX_TABLE_SIZE + 1) {
    PRNTERR2(msg_line, "Table contains entries that are > %ld; Processing stopped.", MAX_TABLE_SIZE + 1);
    exit(12);
  }
  // Initialize all unfilled slots with default values.
  // RECALL that the vector "check" is aliased to the vector "next".
  long indx;
  indx = ia->first_index;
  for (long i = indx; i != NIL && i <= ctp->action_size; i = indx) {
    indx = np->next.raw[i];
    check.raw[i] = DEFAULT_SYMBOL;
    action.raw[i] = ia->error_act;
  }
  for (long i = ctp->action_size + 1; i <= ctp->table_size; i++) {
    check.raw[i] = DEFAULT_SYMBOL;
  }
  // We set the rest of the table with the proper table entries.
  for (int state_no = 1; state_no <= ls->max_la_state; state_no++) {
    struct shift_header_type sh;
    struct reduce_header_type red;
    indx = toutput->state_index.raw[state_no];
    if (state_no > ls->num_states) {
      sh = srt->shift[lastats[state_no].shift_number];
      red = lastats[state_no].reduce;
    } else {
      struct goto_header_type go_to = statset[state_no].go_to;
      for (int j = 1; j <= go_to.size; j++) {
        int symbol = go_to.map[j].symbol;
        long i = indx + symbol;
        if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
          check.raw[i] = symbol;
        } else {
          check.raw[i] = DEFAULT_SYMBOL;
        }
        int act = go_to.map[j].action;
        if (act > 0) {
          action.raw[i] = toutput->state_index.raw[act] + num_rules;
          goto_count++;
        } else {
          action.raw[i] = -act;
          goto_reduce_count++;
        }
      }
      sh = srt->shift[statset[state_no].shift_number];
      red = srt->reduce[state_no];
    }
    for (int j = 1; j <= sh.size; j++) {
      int symbol = sh.map[j].symbol;
      long i = indx + symbol;
      check.raw[i] = symbol;
      int act = sh.map[j].action;
      long result_act;
      if (act > ls->num_states) {
        result_act = la_state_offset + toutput->state_index.raw[act];
        la_shift_count++;
      } else if (act > 0) {
        result_act = toutput->state_index.raw[act] + num_rules;
        shift_count++;
      } else {
        result_act = -act + ia->error_act;
        shift_reduce_count++;
      }
      if (result_act > MAX_TABLE_SIZE + 1) {
        PRNTERR2("Table contains look-ahead shift entry that is >%ld; Processing stopped.", MAX_TABLE_SIZE + 1);
        return;
      }
      action.raw[i] = result_act;
    }
    //   We now initialize the elements reserved for reduce actions in
    // the current state.
    short default_rule = red.map[0].rule_number;
    for (int j = 1; j <= red.size; j++) {
      if (red.map[j].rule_number != default_rule) {
        int symbol = red.map[j].symbol;
        long i = indx + symbol;
        check.raw[i] = symbol;
        int act = red.map[j].rule_number;
        if (rules[act].lhs == accept_image) {
          action.raw[i] = ia->accept_act;
        } else {
          action.raw[i] = act;
        }
        reduce_count++;
      }
    }
    //   We now initialize the element reserved for the DEFAULT reduce
    // action of the current state.  If error maps are requested,  the
    // default slot is initialized to the original state number, and the
    // corresponding element of the DEFAULT_REDUCE array is initialized.
    // Otherwise it is initialized to the rule number in question.
    int i = indx + DEFAULT_SYMBOL;
    check.raw[i] = DEFAULT_SYMBOL;
    int act = red.map[0].rule_number;
    if (act == OMEGA) {
      action.raw[i] = ia->error_act;
    } else {
      action.raw[i] = act;
      default_count++;
    }
  }
  PRNT("\n\nActions in Compressed Tables:");
  PRNT3("     Number of Shifts: %d", shift_count);
  PRNT3("     Number of Shift/Reduces: %d", shift_reduce_count);
  if (ls->max_la_state > ls->num_states) {
    snprintf(msg_line, sizeof(msg_line), "     Number of Look-Ahead Shifts: %d", la_shift_count);
    PRNT(msg_line);
  }
  PRNT3("     Number of Gotos: %d", goto_count);
  PRNT3("     Number of Goto/Reduces: %d", goto_reduce_count);
  PRNT3("     Number of Reduces: %d", reduce_count);
  PRNT3("     Number of Defaults: %d", default_count);
  if (cli_options->error_maps_bit) {
    for ALL_STATES3(state_no, ls->num_states) {
      check.raw[toutput->state_index.raw[state_no]] = -state_no;
    }
  }
  for (int i = 1; i <= (int) ctp->table_size; i++) {
    if (check.raw[i] < 0 || check.raw[i] > (cli_options->java_bit ? 127 : 255)) {
      byte_check_bit = 0;
    }
  }
  if (cli_options->c_bit) {
    mystrcpy("\n#define CLASS_HEADER\n\n", of, output_ptr2);
  } else if (cli_options->cpp_bit) {
    mystrcpy("\n#define CLASS_HEADER ", of, output_ptr2);
    mystrcpy(of->prs_tag, of, output_ptr2);
    mystrcpy("_table::\n\n", of, output_ptr2);
  } else if (cli_options->java_bit) {
    mystrcpy("abstract class ", of, output_ptr2);
    mystrcpy(of->dcl_tag, of, output_ptr2);
    mystrcpy(" implements ", of, output_ptr2);
    mystrcpy(of->def_tag, of, output_ptr2);
    mystrcpy("\n{\n", of, output_ptr2);
  }
  // Write size of right hand side of rules followed by CHECK table.
  if (cli_options->java_bit) {
    mystrcpy("    public final static byte rhs[] = {0,\n", of, output_ptr2);
  } else {
    mystrcpy("const unsigned char  CLASS_HEADER rhs[] = {0,\n", of, output_ptr2);
  }
  padline(output_ptr2);
  int k = 0;
  for (int i = 1; i <= num_rules; i++) {
    k++;
    if (k > 15) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 1;
    }
    itoc(RHS_SIZE(i, rules), output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
  }
  *((*output_ptr2.output_ptr) - 1) = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of, output_ptr2);
  } else {
    mystrcpy("                 };\n", of, output_ptr2);
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  // Write CHECK table.
  if (byte_check_bit && !cli_options->error_maps_bit) {
    if (cli_options->java_bit) {
      mystrcpy("    public final static byte check_table[] = {\n", of, output_ptr2);
    } else {
      mystrcpy("const unsigned char  CLASS_HEADER check_table[] = {\n", of, output_ptr2);
    }
  } else {
    if (cli_options->java_bit) {
      mystrcpy("     public final static short check_table[] = {\n", of, output_ptr2);
    } else {
      mystrcpy("const   signed short CLASS_HEADER check_table[] = {\n", of, output_ptr2);
    }
  }
  padline(output_ptr2);
  k = 0;
  for (int i = 1; i <= (int) ctp->table_size; i++) {
    k++;
    if (k > 10) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 1;
    }
    itoc(check.raw[i], output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
  }
  *((*output_ptr2.output_ptr) - 1) = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of, output_ptr2);
  } else {
    mystrcpy("                 };\n", of, output_ptr2);
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  if (byte_check_bit && !cli_options->error_maps_bit) {
    if (cli_options->java_bit) {
      mystrcpy("    public final static byte check(int i) \n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n", of, output_ptr2);
    } else {
      mystrcpy("const unsigned char  *CLASS_HEADER check = &(check_table[0]) - (NUM_RULES + 1);\n", of, output_ptr2);
    }
  } else {
    if (cli_options->java_bit) {
      mystrcpy("    public final static short check(int i) \n    {\n        return check_table[i - (NUM_RULES + 1)];\n    }\n", of, output_ptr2);
    } else {
      mystrcpy("const   signed short *CLASS_HEADER check = &(check_table[0]) - (NUM_RULES + 1);\n", of, output_ptr2);
    }
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  // Write left hand side symbol of rules followed by ACTION table.
  if (cli_options->java_bit) {
    mystrcpy("    public final static char lhs[] = {0,\n", of, output_ptr2);
  } else {
    mystrcpy("const unsigned short CLASS_HEADER lhs[] = {0,\n", of, output_ptr2);
  }
  padline(output_ptr2);
  k = 0;
  for (int i = 1; i <= num_rules; i++) {
    itoc(toutput->symbol_map.raw[rules[i].lhs], output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
    k++;
    if (k == 15) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 0;
    }
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  *(*output_ptr2.output_ptr)++ = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  padline(output_ptr2);
  k = 0;
  if (cli_options->error_maps_bit) {
    long max_indx;
    // Construct a map from new state numbers into original
    //   state numbers using the array check[]
    max_indx = ia->accept_act - num_rules - 1;
    for (int i = 1; i <= max_indx; i++) {
      check.raw[i] = OMEGA;
    }
    for ALL_STATES3(state_no, ls->num_states) {
      check.raw[toutput->state_index.raw[state_no]] = state_no;
    }
    int j = ls->num_states + 1;
    for (int i = max_indx; i >= 1; i--) {
      int state_no = check.raw[i];
      if (state_no != OMEGA) {
        toutput->ordered_state.raw[--j] = i + num_rules;
        toutput->state_list.raw[j] = state_no;
      }
    }
  }
  for (int i = 1; i <= (int) ctp->action_size; i++) {
    itoc(action.raw[i], output_ptr2);
    *(*output_ptr2.output_ptr)++ = ',';
    k++;
    if (k == 10 && i != (int) ctp->action_size) {
      *(*output_ptr2.output_ptr)++ = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
      padline(output_ptr2);
      k = 0;
    }
  }
  if (k != 0) {
    *((*output_ptr2.output_ptr) - 1) = '\n';
    BUFFER_CHECK(of->sysdcl, output_ptr2);
  }
  if (cli_options->java_bit) {
    mystrcpy("    };\n", of, output_ptr2);
  } else {
    mystrcpy("                 };\n", of, output_ptr2);
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  BUFFER_CHECK(of->sysdcl, output_ptr2);
  if (cli_options->java_bit) {
    mystrcpy("    public final static char action[] = lhs;\n", of, output_ptr2);
  } else {
    mystrcpy("const unsigned short *CLASS_HEADER action = lhs;\n", of, output_ptr2);
  }
  *(*output_ptr2.output_ptr)++ = '\n';
  // If GOTO_DEFAULT is requested, we print out the GOTODEF vector.
  if (cli_options->goto_default_bit) {
    ArrayShort default_map = Allocate_short_array2(num_symbols + 1);
    if (cli_options->java_bit) {
      mystrcpy("\n    public final static char default_goto[] = {0,\n", of, output_ptr2);
    } else {
      mystrcpy("\nconst unsigned short CLASS_HEADER default_goto[] = {0,\n", of, output_ptr2);
    }
    padline(output_ptr2);
    k = 0;
    for (int i = 0; i <= num_symbols; i++) {
      default_map.raw[i] = ia->error_act;
    }
    for ALL_NON_TERMINALS3(symbol) {
      int act = gotodef.raw[symbol];
      int result_act;
      if (act < 0) {
        result_act = -act;
      } else if (act > 0) {
        result_act = toutput->state_index.raw[act] + num_rules;
      } else {
        result_act = ia->error_act;
      }
      default_map.raw[toutput->symbol_map.raw[symbol]] = result_act;
    }
    for (int symbol = 1; symbol <= num_symbols; symbol++) {
      itoc(default_map.raw[symbol], output_ptr2);
      *(*output_ptr2.output_ptr)++ = ',';
      k++;
      if (k == 10 && symbol != num_symbols) {
        *(*output_ptr2.output_ptr)++ = '\n';
        BUFFER_CHECK(of->sysdcl, output_ptr2);
        padline(output_ptr2);
        k = 0;
      }
    }
    if (k != 0) {
      *((*output_ptr2.output_ptr) - 1) = '\n';
      BUFFER_CHECK(of->sysdcl, output_ptr2);
    }
    if (cli_options->java_bit) {
      mystrcpy("    };\n", of, output_ptr2);
    } else {
      mystrcpy("                 };\n", of, output_ptr2);
    }
  }
  ffree(np->next.raw);
  ffree(np->previous.raw);
  common(byte_check_bit, cli_options, toutput, dss, ctp, of, scope, ia, srt, scope_right_side, statset, gd_index, gd_range, scope_state, item_table, sc, output_buffer, output_ptr2, symno, string_table, name, ls);
}

// endregion

// region spacetab

struct NumTableEntries {
  long value;
};

struct TResult {
  long single_root;
  long multi_root;
  long empty_root;
  long top;
};

///  REMAP_NON_TERMINALS remaps the non-terminal symbols and states based on
/// frequency of entries.
static long remap_non_terminals(const struct CLIOptions *cli_options, struct TableOutput *toutput, ArrayLong gotodef, struct statset_type *statset, struct LAState* ls) {
  // The variable FREQUENCY_SYMBOL is used to hold the non-terminals
  // in the grammar, and  FREQUENCY_COUNT is used correspondingly to
  // hold the number of actions defined on each non-terminal.
  // ORDERED_STATE and ROW_SIZE are used in a similar fashion for states
  ArrayLong frequency_symbol = Allocate_long_array2(num_non_terminals);
  // TODO • The size in the arraylong will be wrong?
  frequency_symbol.raw -= num_terminals + 1;
  ArrayLong frequency_count = Allocate_long_array2(num_non_terminals);
  // TODO • The size in the arraylong will be wrong?
  frequency_count.raw -= num_terminals + 1;
  ArrayLong row_size = Allocate_long_array2(ls->num_states + 1);
  for ALL_NON_TERMINALS3(i) {
    frequency_symbol.raw[i] = i;
    frequency_count.raw[i] = 0;
  }
  for ALL_STATES3(state_no, ls->num_states) {
    toutput->ordered_state.raw[state_no] = state_no;
    row_size.raw[state_no] = 0;
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      row_size.raw[state_no]++;
      const int symbol = go_to.map[i].symbol;
      frequency_count.raw[symbol]++;
    }
  }
  // The non-terminals are sorted in descending order based on the
  // number of actions defined on then, and they are remapped based on
  // the new arrangement obtained by the sorting.
  sortdes(frequency_symbol, frequency_count, num_terminals + 1, num_symbols, ls->num_states);
  for ALL_NON_TERMINALS3(i) {
    toutput->symbol_map.raw[frequency_symbol.raw[i]] = i;
  }
  //    All non-terminal entries in the state automaton are updated
  // accordingly.  We further subtract NUM_TERMINALS from each
  // non-terminal to make them fall in the range [1..NUM_NON_TERMINLS]
  // instead of [NUM_TERMINALS+1..NUM_SYMBOLS].
  for ALL_STATES3(state_no, ls->num_states) {
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      go_to.map[i].symbol = toutput->symbol_map.raw[go_to.map[i].symbol] - num_terminals;
    }
  }
  // If Goto-Default was requested, we find out how many non-terminals
  // were eliminated as a result, and adjust the GOTO-DEFAULT map,
  // based on the new mapping of the non-terminals.
  long last_symbol;
  if (cli_options->goto_default_bit) {
    ArrayLong temp_goto_default = Allocate_long_array2(num_non_terminals);
    temp_goto_default.raw -= num_terminals + 1;
    for (last_symbol = num_symbols; last_symbol > num_terminals; last_symbol--) {
      if (frequency_count.raw[last_symbol] != 0) {
        break;
      }
    }
    last_symbol -= num_terminals;
    PRNT3("Number of non-terminals eliminated: %ld", num_non_terminals - last_symbol);
    // Remap the GOTO-DEFAULT map.
    // to hold the original map.
    for ALL_NON_TERMINALS3(symbol) {
      temp_goto_default.raw[toutput->symbol_map.raw[symbol]] = gotodef.raw[symbol];
    }
    gotodef.raw += num_terminals + 1;
    ffree(gotodef.raw);
    gotodef.raw = temp_goto_default.raw;
  } else {
    last_symbol = num_non_terminals;
  }
  // The states are sorted in descending order based on the number of
  // actions defined on them, and they are remapped based on the new
  // arrangement obtained by the sorting.
  sortdes(toutput->ordered_state, row_size, 1, ls->num_states, last_symbol);
  frequency_symbol.raw += num_terminals + 1;
  ffree(frequency_symbol.raw);
  frequency_count.raw += num_terminals + 1;
  ffree(frequency_count.raw);
  ffree(row_size.raw);
  return last_symbol;
}

/// We now overlap the non-terminal table, or more precisely, we compute the
/// starting position in a vector where each of its rows may be placed
/// without clobbering elements in another row.  The starting positions are
/// stored in the vector STATE_INDEX.
static void overlap_nt_rows(struct CLIOptions *cli_options, struct TableOutput *toutput, struct NumTableEntries *nte, struct CTabsProps *ctp, long last_symbol, struct NextPrevious* np, struct ImportantAspects* ia, struct statset_type *statset, struct LAState* ls) {
  nte->value = num_gotos + num_goto_reduces + ls->num_states;
  ctp->increment_size = MAX(nte->value / 100 * increment, last_symbol + 1);
  ctp->table_size = MIN(nte->value + ctp->increment_size, MAX_TABLE_SIZE);
  // Allocate space for table, and initlaize the AVAIL_POOL list.  The
  // variable FIRST_INDEX keeps track of the first element in the doubly-
  // linked list, and LAST_ELEMENT keeps track of the last element in the
  // list.
  //   The variable MAX_INDX is used to keep track of the maximum starting
  // position for a row that has been used.
  np->next = Allocate_long_array2(ctp->table_size + 1);
  np->previous = Allocate_long_array2(ctp->table_size + 1);
  ia->first_index = 1;
  np->previous.raw[ia->first_index] = NIL;
  np->next.raw[ia->first_index] = ia->first_index + 1;
  for (long indx = 2; indx < (int) ctp->table_size; indx++) {
    np->next.raw[indx] = indx + 1;
    np->previous.raw[indx] = indx - 1;
  }
  ia->last_index = ctp->table_size;
  np->previous.raw[ia->last_index] = ia->last_index - 1;
  np->next.raw[ia->last_index] = NIL;
  int max_indx = ia->first_index;
  // We now iterate over all the states in their new sorted order as
  // indicated by the variable STATE_NO, and determine an "overlap"
  // position for them.
  for ALL_STATES3(state_no, ls->num_states) {
    const int state_no__ = toutput->ordered_state.raw[state_no];
    // INDX is set to the beginning of the list of available slots
    // and we try to determine if it might be a valid starting
    // position.  If not, INDX is moved to the next element, and we
    // repeat the process until a valid position is found.
    const struct goto_header_type go_to = statset[state_no__].go_to;
    long indx = ia->first_index;
  look_for_match_in_base_table:
    if (indx == NIL) {
      indx = ctp->table_size + 1;
    }
    if (indx + last_symbol > ctp->table_size) {
      reallocate(cli_options, ctp, np, ia);
    }
    for (int i = 1; i <= go_to.size; i++) {
      if (np->next.raw[indx + go_to.map[i].symbol] == OMEGA) {
        indx = np->next.raw[indx];
        goto look_for_match_in_base_table;
      }
    }
    // At this stage, a position(INDX), was found to overlay the row
    // in question.  Remove elements associated with all positions
    // that will be taken by row from the doubly-linked list.
    // NOTE tha since SYMBOLs start at 1, the first index can never
    // be a candidate (==> I = INDX + SYMBOL) in this loop.
    for (int j = 1; j <= go_to.size; j++) {
      const int symbol = go_to.map[j].symbol;
      int i = indx + symbol;
      if (i == ia->last_index) {
        ia->last_index = np->previous.raw[ia->last_index];
        np->next.raw[ia->last_index] = NIL;
      } else {
        np->next.raw[np->previous.raw[i]] = np->next.raw[i];
        np->previous.raw[np->next.raw[i]] = np->previous.raw[i];
      }
      np->next.raw[i] = OMEGA;
    }
    if (ia->first_index == ia->last_index) {
      ia->first_index = NIL;
    } else if (indx == ia->first_index) {
      ia->first_index = np->next.raw[ia->first_index];
      np->previous.raw[ia->first_index] = NIL;
    } else if (indx == ia->last_index) {
      ia->last_index = np->previous.raw[ia->last_index];
      np->next.raw[ia->last_index] = NIL;
    } else {
      np->next.raw[np->previous.raw[indx]] = np->next.raw[indx];
      np->previous.raw[np->next.raw[indx]] = np->previous.raw[indx];
    }
    np->next.raw[indx] = OMEGA;
    if (indx > max_indx) {
      max_indx = indx;
    }
    toutput->state_index.raw[state_no__] = indx;
  }
  if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
    ctp->check_size = max_indx + num_non_terminals;
  } else {
    ctp->check_size = 0;
  }
  for (ctp->action_size = max_indx + last_symbol; ctp->action_size >= max_indx; ctp->action_size--) {
    if (np->next.raw[ctp->action_size] == OMEGA) {
      break;
    }
  }
  ia->accept_act = max_indx + num_rules + 1;
  ia->error_act = ia->accept_act + 1;
  printf("\n");
  if (cli_options->goto_default_bit || cli_options->nt_check_bit) {
    PRNT3("Length of base Check Table: %d", ctp->check_size);
  }
  PRNT3("Length of base Action Table: %ld", ctp->action_size);
  PRNT3("Number of entries in base Action Table: %ld", nte->value);
  const int percentage = (ctp->action_size - nte->value) * 1000 / nte->value;
  PRNT3("Percentage of increase: %d.%d%%", percentage / 10, percentage % 10);
}

/// We now try to merge states in the terminal table that are similar.
/// Two states S1 and S2 are said to be similar if they contain the
/// same shift actions, and they reduce to the same set of rules.  In
/// addition,  there must not exist a terminal symbol "t" such that:
/// REDUCE(S1, t) and REDUCE(S2, t) are defined, and
/// REDUCE(S1, t) ^= REDUCE(S2, t)
static void merge_similar_t_rows(const struct CLIOptions *cli_options, struct TableOutput *toutput, ArrayBool shift_on_error_symbol, struct node **new_state_element_reduce_nodes, struct TResult *tresult, struct new_state_type *new_state_element, struct SRTable* srt, struct lastats_type *lastats, struct statset_type *statset, struct LAState* ls) {
  ArrayShort table = Allocate_short_array2(num_shift_maps + 1);
  tresult->top = 0;
  for (int i = 1; i <= ls->max_la_state; i++) {
    shift_on_error_symbol.raw[i] = false;
  }
  for (int i = 0; i <= num_shift_maps; i++) {
    table.raw[i] = NIL;
  }
  // We now hash all the states into TABLE, based on their shift map
  // number.
  // The rules in the range of the REDUCE MAP are placed in sorted
  // order in a linear linked list headed by REDUCE_ROOT.
  for (long state_no = 1; state_no <= ls->max_la_state; state_no++) {
    struct node *reduce_root = NULL;
    struct reduce_header_type red;
    if (state_no > ls->num_states) {
      red = lastats[state_no].reduce;
    } else {
      red = srt->reduce[state_no];
    }
    for (int i = 1; i <= red.size; i++) {
      const int rule_no = red.map[i].rule_number;
      struct node *q;
      struct node *tail;
      for (q = reduce_root; q != NULL; tail = q, q = q->next) {
        // Is it or not in REDUCE_ROOT list?
        if (q->value == rule_no)
          goto continue_traverse_reduce_map;
        if (q->value > rule_no)
          break;
      }
      struct node *r = Allocate_node(); /* Add element to REDUCE_ROOT list */
      r->value = rule_no;
      if (q == reduce_root) {
        r->next = reduce_root;
        reduce_root = r;
      } else {
        r->next = q;
        tail->next = r;
      }
    continue_traverse_reduce_map:;
    }
    //   We compute the HASH_ADDRESS,  mark if the state has a shift
    // action on the ERROR symbol, and search the hash TABLE to see if a
    // state matching the description is already in there.
    unsigned long hash_address;
    if (state_no > ls->num_states) {
      hash_address = lastats[state_no].shift_number;
    } else {
      if (cli_options->default_opt.value == OPT_5.value) {
        const struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
        for (int j = 1; j <= sh.size && !shift_on_error_symbol.raw[state_no]; j++)
          shift_on_error_symbol.raw[state_no] = sh.map[j].symbol == error_image;
      }
      hash_address = statset[state_no].shift_number;
    }
    int ii;
    for (ii = table.raw[hash_address]; ii != NIL; ii = new_state_element[ii].link) {
      struct node *q;
      struct node *r;
      for (r = reduce_root, q = new_state_element_reduce_nodes[ii];
           r != NULL && q != NULL;
           r = r->next, q = q->next) {
        if (r->value != q->value) {
          break;
        }
      }
      if (r == q) {
        break;
      }
    }
    // If the state is a new state to be inserted in the table, we now
    // do so,  and place it in the proper category based on its reduces,
    // In any case, the IMAGE field is updated, and so is the relevant
    // STATE_LIST element.
    //
    // If the state contains a shift action on the error symbol and also
    // contains reduce actions,  we allocate a new element for it and
    // place it in the list headed by MULTI_ROOT.  Such states are not
    // merged, because we do not take default reductions in them.
    if (shift_on_error_symbol.raw[state_no] && reduce_root != NULL) {
      tresult->top++;
      if (ii == NIL) {
        new_state_element[tresult->top].link = table.raw[hash_address];
        table.raw[hash_address] = tresult->top;
      }
      new_state_element[tresult->top].thread = tresult->multi_root;
      tresult->multi_root = tresult->top;
      new_state_element[tresult->top].shift_number = hash_address;
      new_state_element_reduce_nodes[tresult->top] = reduce_root;
      toutput->state_list.raw[state_no] = NIL;
      new_state_element[tresult->top].image = state_no;
    } else if (ii == NIL) {
      tresult->top++;
      new_state_element[tresult->top].link = table.raw[hash_address];
      table.raw[hash_address] = tresult->top;
      if (reduce_root == NULL) {
        new_state_element[tresult->top].thread = tresult->empty_root;
        tresult->empty_root = tresult->top;
      } else if (reduce_root->next == NULL) {
        new_state_element[tresult->top].thread = tresult->single_root;
        tresult->single_root = tresult->top;
      } else {
        new_state_element[tresult->top].thread = tresult->multi_root;
        tresult->multi_root = tresult->top;
      }
      new_state_element[tresult->top].shift_number = hash_address;
      new_state_element_reduce_nodes[tresult->top] = reduce_root;
      toutput->state_list.raw[state_no] = NIL;
      new_state_element[tresult->top].image = state_no;
    } else {
      toutput->state_list.raw[state_no] = new_state_element[ii].image;
      new_state_element[ii].image = state_no;
      struct node *tail;
      for (struct node *r = reduce_root; r != NULL; tail = r, r = r->next) {
      }
      if (reduce_root != NULL) {
        free_nodes(reduce_root, tail);
      }
    }
  }
  ffree(table.raw);
}

///    If shift-default actions are requested, the shift actions
/// associated with each state are factored out of the Action matrix
/// and all identical rows are merged.  This merged matrix is used to
/// create a boolean vector that may be used to confirm whether
/// there is a shift action in a given state S on a given symbol t.
/// If we can determine that there is a shift action on a pair (S, t)
/// we can apply shift default to the Shift actions just like we did
/// for the Goto actions.
static void merge_shift_domains(struct CLIOptions *cli_options, struct TableOutput *toutput, ArrayLong row_size, ArrayLong frequency_symbol, ArrayLong frequency_count, struct NumTableEntries *nte, ArrayLong shift_check_index, struct CTabsProps *ctp, struct new_state_type *new_state_element, ArrayShort shift_image, ArrayShort real_shift_number, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, ArrayShort shiftdf, struct LAState* ls) {
  // Some of the rows in the shift action map have already been merged
  // by the merging of compatible states... We simply need to increase
  // the size of the granularity by merging these new terminal states
  // based only on their shift actions.
  //
  // The array SHIFT_DOMAIN_TABLE is used as the base for a hash table.
  // Each submap represented by a row of the shift action map is hashed
  // into this table by summing the terminal symbols in its domain.
  // The submap is then entered in the hash table and assigned a unique
  // number if such a map was not already placed in the hash table.
  // Otherwise, the number assigned to the previous submap is also
  // associated with the new submap.
  //
  // The vector SHIFT_IMAGE is used to keep track of the unique number
  // associated with each unique shift submap.
  // The vector REAL_SHIFT_NUMBER is the inverse of SHIFT_IMAGE. It is
  // used to associate each unique number to its shift submap.
  // The integer NUM_TABLE_ENTRIES is used to count the number of
  // elements in the new merged shift map.
  //
  // The arrays ORDERED_SHIFT and ROW_SIZE are also initialized here.
  // They are used to sort the rows of the shift actions map later...
  ArrayShort shift_domain_link = Allocate_short_array2(ctp->num_terminal_states + 1);
  ArrayLong ordered_shift = Allocate_long_array2(num_shift_maps + 1);
  ArrayBool shift_symbols = Allocate_bool_array2(num_terminals + 1);
  short shift_domain_table[SHIFT_TABLE_SIZE];
  for (int i = 0; i <= SHIFT_TABLE_UBOUND; i++) {
    shift_domain_table[i] = NIL;
  }
  nte->value = 0;
  ctp->shift_domain_count = 0;
  for (int state_no = 1; state_no <= ctp->num_terminal_states; state_no++) {
    int shift_no = new_state_element[state_no].shift_number;
    for (int i = 1; i <= num_terminals; i++) {
      shift_symbols.raw[i] = false;
    }
    struct shift_header_type sh = srt->shift[shift_no];
    int shift_size = sh.size;
    unsigned long hash_address = shift_size;
    for (int i = 1; i <= shift_size; i++) {
      int symbol = sh.map[i].symbol;
      hash_address += symbol;
      shift_symbols.raw[symbol] = true;
    }
    hash_address %= SHIFT_TABLE_SIZE;
    for (int i = shift_domain_table[hash_address]; i != NIL; i = shift_domain_link.raw[i]) {
      sh = srt->shift[new_state_element[i].shift_number];
      if (sh.size == shift_size) {
        int jj;
        for (jj = 1; jj <= shift_size; jj++) {
          if (!shift_symbols.raw[sh.map[jj].symbol]) {
            break;
          }
        }
        if (jj > shift_size) {
          shift_image.raw[state_no] = shift_image.raw[i];
          goto continu;
        }
      }
    }
    shift_domain_link.raw[state_no] = shift_domain_table[hash_address];
    shift_domain_table[hash_address] = state_no;
    ctp->shift_domain_count++;
    shift_image.raw[state_no] = ctp->shift_domain_count;
    real_shift_number.raw[ctp->shift_domain_count] = shift_no;
    ordered_shift.raw[ctp->shift_domain_count] = ctp->shift_domain_count;
    row_size.raw[ctp->shift_domain_count] = shift_size;
    nte->value = nte->value + shift_size;
  continu:;
  }
  //   Compute the frequencies, and remap the terminal symbols
  // accordingly.
  for ALL_TERMINALS3(symbol) {
    frequency_symbol.raw[symbol] = symbol;
    frequency_count.raw[symbol] = 0;
  }
  for (int i = 1; i <= ctp->shift_domain_count; i++) {
    int shift_no = real_shift_number.raw[i];
    struct shift_header_type sh = srt->shift[shift_no];
    for (int j = 1; j <= sh.size; j++) {
      int symbol = sh.map[j].symbol;
      frequency_count.raw[symbol]++;
    }
  }
  sortdes(frequency_symbol, frequency_count, 1, num_terminals, ctp->shift_domain_count);
  for ALL_TERMINALS3(symbol) {
    toutput->symbol_map.raw[frequency_symbol.raw[symbol]] = symbol;
  }
  toutput->symbol_map.raw[DEFAULT_SYMBOL] = DEFAULT_SYMBOL;
  eoft_image = toutput->symbol_map.raw[eoft_image];
  if (cli_options->error_maps_bit) {
    error_image = toutput->symbol_map.raw[error_image];
    eolt_image = toutput->symbol_map.raw[eolt_image];
  }
  for (int i = 1; i <= num_shift_maps; i++) {
    struct shift_header_type sh = srt->shift[i];
    for (int j = 1; j <= sh.size; j++) {
      sh.map[j].symbol = toutput->symbol_map.raw[sh.map[j].symbol];
    }
  }
  for (int state_no = 1; state_no <= ctp->num_terminal_states; state_no++) {
    struct reduce_header_type red = new_state_element[state_no].reduce;
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
    }
  }
  // If ERROR_MAPS are requested, we also have to remap the original
  // REDUCE maps.
  if (cli_options->error_maps_bit) {
    for ALL_STATES3(state_no, ls->num_states) {
      struct reduce_header_type red = srt->reduce[state_no];
      for (int i = 1; i <= red.size; i++) {
        red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
      }
    }
  }
  // Remap the SHIFT_DEFAULT map.
  ArrayShort temp_shift_default = Allocate_short_array2(num_terminals + 1);
  for ALL_TERMINALS3(symbol) {
    temp_shift_default.raw[toutput->symbol_map.raw[symbol]] = shiftdf.raw[symbol];
  }
  ffree(shiftdf.raw);
  shiftdf.raw = temp_shift_default.raw;
  // We now compute the starting position for each Shift check row
  // as we did for the terminal states.  The starting positions are
  // stored in the vector SHIFT_CHECK_INDEX.
  sortdes(ordered_shift, row_size, 1, ctp->shift_domain_count, num_terminals);
  ctp->increment_size = MAX(nte->value / 100 * increment, num_terminals + 1);
  int old_table_size = ctp->table_size;
  ctp->table_size = MIN(nte->value + ctp->increment_size, MAX_TABLE_SIZE);
  if ((int) ctp->table_size > old_table_size) {
    ffree(np->previous.raw);
    ffree(np->next.raw);
    np->previous = Allocate_long_array2(ctp->table_size + 1);
    np->next = Allocate_long_array2(ctp->table_size + 1);
  } else {
    ctp->table_size = old_table_size;
  }
  ia->first_index = 1;
  np->previous.raw[ia->first_index] = NIL;
  np->next.raw[ia->first_index] = ia->first_index + 1;
  for (int indx = 2; indx < (int) ctp->table_size; indx++) {
    np->next.raw[indx] = indx + 1;
    np->previous.raw[indx] = indx - 1;
  }
  ia->last_index = ctp->table_size;
  np->previous.raw[ia->last_index] = ia->last_index - 1;
  np->next.raw[ia->last_index] = NIL;
  int max_indx = ia->first_index;
  // Look for a suitable index where to overlay the shift check row.
  for (int k = 1; k <= ctp->shift_domain_count; k++) {
    int shift_no = ordered_shift.raw[k];
    struct shift_header_type sh = srt->shift[real_shift_number.raw[shift_no]];
    int indx = ia->first_index;
  look_for_match_in_sh_chk_tab:
    if (indx == NIL) {
      indx = ctp->table_size + 1;
    }
    if (indx + num_terminals > (int) ctp->table_size) {
      reallocate(cli_options, ctp, np, ia);
    }
    for (int i = 1; i <= sh.size; i++) {
      int symbol = sh.map[i].symbol;
      if (np->next.raw[indx + symbol] == OMEGA) {
        indx = np->next.raw[indx];
        goto look_for_match_in_sh_chk_tab;
      }
    }
    // INDX marks the starting position for the row, remove all the
    // positions that are claimed by that shift check row.
    // If a position has the value 0,   then it is the starting position
    // of a Shift row that was previously processed, and that element
    // has already been removed from the list of available positions.
    for (int j = 1; j <= sh.size; j++) {
      int symbol = sh.map[j].symbol;
      int i = indx + symbol;
      if (np->next.raw[i] != 0) {
        if (i == ia->last_index) {
          ia->last_index = np->previous.raw[ia->last_index];
          np->next.raw[ia->last_index] = NIL;
        } else {
          np->next.raw[np->previous.raw[i]] = np->next.raw[i];
          np->previous.raw[np->next.raw[i]] = np->previous.raw[i];
        }
      }
      np->next.raw[i] = OMEGA;
    }
    // We now remove the starting position itself from the list without
    // marking it as taken, since it can still be used for a shift check.
    // MAX_INDX is updated if required.
    // SHIFT_CHECK_INDEX(SHIFT_NO) is properly set to INDX as the
    // starting position of STATE_NO.
    if (ia->first_index == ia->last_index) {
      ia->first_index = NIL;
    } else if (indx == ia->first_index) {
      ia->first_index = np->next.raw[ia->first_index];
      np->previous.raw[ia->first_index] = NIL;
    } else if (indx == ia->last_index) {
      ia->last_index = np->previous.raw[ia->last_index];
      np->next.raw[ia->last_index] = NIL;
    } else {
      np->next.raw[np->previous.raw[indx]] = np->next.raw[indx];
      np->previous.raw[np->next.raw[indx]] = np->previous.raw[indx];
    }
    np->next.raw[indx] = 0;
    if (indx > max_indx) {
      max_indx = indx;
    }
    shift_check_index.raw[shift_no] = indx;
  }
  // Update all counts, and report statistics.
  ctp->shift_check_size = max_indx + num_terminals;
  printf("\n");
  PRNT3("Length of Shift Check Table: %d", ctp->shift_check_size);
  PRNT3("Number of entries in Shift Check Table: %ld", nte->value);
  int kk;
  for (kk = ctp->shift_check_size; kk >= max_indx; kk--) {
    if (np->next.raw[kk] == OMEGA) {
      break;
    }
  }
  long percentage = (kk - nte->value) * 1000 / nte->value;
  PRNT3("Percentage of increase: %ld.%ld%%", percentage/10, percentage % 10);
  ffree(ordered_shift.raw);
  ffree(shift_symbols.raw);
  ffree(shift_domain_link.raw);
}

/// By now, similar states have been grouped together, and placed in
/// one of three linear linked lists headed by the root pointers:
/// MULTI_ROOT, SINGLE_ROOT, and EMPTY_ROOT.
/// We iterate over each of these lists and construct new states out
/// of these groups of similar states when they are compatible. Then,
/// we remap the terminal symbols.
static void overlay_sim_t_rows(struct CLIOptions *cli_options, struct TableOutput *toutput, ArrayBool shift_on_error_symbol, struct node **new_state_element_reduce_nodes, struct TResult *tresult, struct NumTableEntries *nte, ArrayLong shift_check_index, struct CTabsProps *ctp, struct new_state_type *new_state_element, ArrayShort shift_image, ArrayShort real_shift_number, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, struct ruletab_type *rules, struct lastats_type *lastats, ArrayShort shiftdf, struct statset_type *statset, struct LAState* ls) {
  int num_shifts_saved = 0;
  int num_reductions_saved = 0;
  int default_saves = 0;
  ArrayShort rule_count = Allocate_short_array2(num_rules + 1);
  ArrayShort reduce_action = Allocate_short_array2(num_terminals + 1);
  //     We first iterate over the groups of similar states in the
  // MULTI_ROOT list.  These states have been grouped together,
  // because they have the same Shift map, and reduce to the same
  // rules, but we must check that they are fully compatible by making
  // sure that no two states contain reduction to a different rule on
  // the same symbol.
  //     The idea is to take a state out of the group, and merge with
  // it as many other compatible states from the group as possible.
  // remaining states from the group that caused clashes are thrown
  // back into the MULTI_ROOT list as a new group of states.
  for (int i = tresult->multi_root; i != NIL; i = new_state_element[i].thread) {
    for (struct node *q = new_state_element_reduce_nodes[i]; q != NULL; q = q->next) {
      rule_count.raw[q->value] = 0;
    }
    // REDUCE_ACTION is used to keep track of reductions that are to be
    // applied on terminal symbols as the states are merged.  We pick
    // out the first state (STATE_NO) from the group of states involved,
    // initialize REDUCE_ACTION with its reduce map, and count the number
    // of reductions associated with each rule in that state.
    int state_no = new_state_element[i].image;
    struct reduce_header_type red;
    if (state_no > ls->num_states) {
      red = lastats[state_no].reduce;
    } else {
      red = srt->reduce[state_no];
    }
    for ALL_TERMINALS3(j) {
      reduce_action.raw[j] = OMEGA;
    }
    for (int j = 1; j <= red.size; j++) {
      int rule_no = red.map[j].rule_number;
      reduce_action.raw[red.map[j].symbol] = rule_no;
      rule_count.raw[rule_no]++;
    }
    // STATE_SET_ROOT is used to traverse the rest of the list that form
    // the group of states being processed.  STATE_SUBSET_ROOT is used
    // to construct the new list that will consist of all states in the
    // group that are compatible starting with the initial state.
    // STATE_ROOT is used to construct a list of all states in the group
    // that are not compatible with the initial state.
    int state_set_root = toutput->state_list.raw[state_no];
    int state_subset_root = state_no;
    toutput->state_list.raw[state_subset_root] = NIL;
    int state_root = NIL;
    for (int state_no = state_set_root; state_no != NIL; state_no = state_set_root) {
      state_set_root = toutput->state_list.raw[state_set_root];
      // We traverse the reduce map of the state taken out from the group
      // and check to see if it is compatible with the subset being
      // constructed so far.
      if (state_no > ls->num_states) {
        red = lastats[state_no].reduce;
      } else {
        red = srt->reduce[state_no];
      }
      int jj;
      for (jj = 1; jj <= red.size; jj++) {
        int symbol = red.map[jj].symbol;
        if (reduce_action.raw[symbol] != OMEGA && reduce_action.raw[symbol] != red.map[jj].rule_number) {
          break;
        }
      }

      // If J > Q->REDUCE_ELEMENT.N then we traversed the whole reduce map,
      // and all the reductions involved are compatible with the new state
      // being constructed.  The state involved is added to the subset, the
      // rule counts are updated, and the REDUCE_ACTIONS map is updated.
      //     Otherwise, we add the state involved to the STATE_ROOT list
      // which will be thrown back in the MULTI_ROOT list.
      if (jj > red.size) {
        toutput->state_list.raw[state_no] = state_subset_root;
        state_subset_root = state_no;
        for (jj = 1; jj <= red.size; jj++) {
          int symbol = red.map[jj].symbol;
          if (reduce_action.raw[symbol] == OMEGA) {
            int rule_no = red.map[jj].rule_number;
            if (rules[rule_no].lhs == accept_image) {
              rule_no = 0;
            }
            reduce_action.raw[symbol] = rule_no;
            rule_count.raw[rule_no]++;
          } else {
            num_reductions_saved++;
          }
        }
      } else {
        toutput->state_list.raw[state_no] = state_root;
        state_root = state_no;
      }
    }

    // Figure out the best default rule candidate, and update
    // DEFAULT_SAVES.
    // Recall that all accept actions were changed into reduce actions
    // by rule 0.
    int k = 0;
    int reduce_size = 0;
    int default_rule = ia->error_act;
    struct node *tail;
    for (struct node *q = new_state_element_reduce_nodes[i]; q != NULL; tail = q, q = q->next) {
      int rule_no = q->value;
      reduce_size += rule_count.raw[rule_no];
      if (rule_count.raw[rule_no] > k && rule_no != 0 && !shift_on_error_symbol.raw[state_subset_root]) {
        k = rule_count.raw[rule_no];
        default_rule = rule_no;
      }
    }
    default_saves += k;
    reduce_size -= k;

    // If STATE_ROOT is not NIL then there are states in the group that
    // did not meet the compatibility test.  Throw those states back in
    // front of MULTI_ROOT as a group.
    if (state_root != NIL) {
      tresult->top++;
      new_state_element[tresult->top].thread = new_state_element[i].thread;
      new_state_element[i].thread = tresult->top;
      if (state_root > ls->num_states) {
        new_state_element[tresult->top].shift_number = lastats[state_root].shift_number;
      } else {
        new_state_element[tresult->top].shift_number = statset[state_root].shift_number;
      }
      new_state_element_reduce_nodes[tresult->top] = new_state_element_reduce_nodes[i];
      new_state_element[tresult->top].image = state_root;
    } else
      free_nodes(new_state_element_reduce_nodes[i], tail);

    // Create Reduce map for the newly created terminal state.
    // We may assume that SYMBOL field of defaults is already set to
    // the DEFAULT_SYMBOL value.
    struct reduce_header_type new_red = Allocate_reduce_map(reduce_size);
    new_red.map[0].symbol = DEFAULT_SYMBOL;
    new_red.map[0].rule_number = default_rule;
    for ALL_TERMINALS3(symbol) {
      if (reduce_action.raw[symbol] != OMEGA) {
        if (reduce_action.raw[symbol] != default_rule) {
          new_red.map[reduce_size].symbol = symbol;
          if (reduce_action.raw[symbol] == 0) {
            new_red.map[reduce_size].rule_number = ia->accept_act;
          } else {
            new_red.map[reduce_size].rule_number =
                reduce_action.raw[symbol];
          }
          reduce_size--;
        }
      }
    }
    new_state_element[i].reduce = new_red;
    new_state_element[i].image = state_subset_root;
  }
  // We now process groups of states that have reductions to a single
  // rule.  Those states are fully compatible, and the default is the
  // rule in question.
  // Any of the REDUCE_ELEMENT maps that belongs to a state in the
  // group of states being processed may be reused for the new  merged
  // state.
  for (int i = tresult->single_root; i != NIL; i = new_state_element[i].thread) {
    int state_no = new_state_element[i].image;
    struct node *q = new_state_element_reduce_nodes[i];
    int rule_no = q->value;
    free_nodes(q, q);
    struct reduce_header_type new_red;
    struct reduce_header_type red;
    if (rules[rule_no].lhs == accept_image) {
      red = srt->reduce[state_no];
      int reduce_size = red.size;
      new_red = Allocate_reduce_map(reduce_size);
      new_red.map[0].symbol = DEFAULT_SYMBOL;
      new_red.map[0].rule_number = ia->error_act;
      for (int j = 1; j <= reduce_size; j++) {
        new_red.map[j].symbol = red.map[j].symbol;
        new_red.map[j].rule_number = ia->accept_act;
      }
    } else {
      for ALL_TERMINALS3(j) {
        reduce_action.raw[j] = OMEGA;
      }
      for (; state_no != NIL; state_no = toutput->state_list.raw[state_no]) {
        if (state_no > ls->num_states) {
          red = lastats[state_no].reduce;
        } else {
          red = srt->reduce[state_no];
        }
        for (int j = 1; j <= red.size; j++) {
          int symbol = red.map[j].symbol;
          if (reduce_action.raw[symbol] == OMEGA) {
            reduce_action.raw[symbol] = rule_no;
            default_saves++;
          } else
            num_reductions_saved++;
        }
      }
      new_red = Allocate_reduce_map(0);
      new_red.map[0].symbol = DEFAULT_SYMBOL;
      new_red.map[0].rule_number = rule_no;
    }
    new_state_element[i].reduce = new_red;
  }

  // Groups of states that have no reductions are also compatible.
  // Their default is ERROR_ACTION.
  for (int i = tresult->empty_root; i != NIL; i = new_state_element[i].thread) {
    int state_no = new_state_element[i].image;
    struct reduce_header_type red;
    if (state_no > ls->num_states) {
      red = lastats[state_no].reduce;
    } else {
      red = srt->reduce[state_no];
    }
    red.map[0].symbol = DEFAULT_SYMBOL;
    red.map[0].rule_number = ia->error_act;
    new_state_element[i].reduce = red;
  }
  ctp->num_terminal_states = tresult->top;
  ArrayLong frequency_symbol = Allocate_long_array2(num_terminals + 1);
  ArrayLong frequency_count = Allocate_long_array2(num_terminals + 1);
  ArrayLong row_size = Allocate_long_array2(ls->max_la_state + 1);
  if (cli_options->shift_default_bit) {
    merge_shift_domains(cli_options, toutput, row_size, frequency_symbol, frequency_count, nte, shift_check_index, ctp, new_state_element, shift_image, real_shift_number, np, ia, srt, shiftdf, ls);
  }
  // We now reorder the terminal states based on the number of actions
  // in them, and remap the terminal symbols if they were not already
  // remapped in the previous block for the SHIFT_CHECK vector.
  for ALL_TERMINALS3(symbol) {
    frequency_symbol.raw[symbol] = symbol;
    frequency_count.raw[symbol] = 0;
  }
  for (int i = 1; i <= ctp->num_terminal_states; i++) {
    toutput->ordered_state.raw[i] = i;
    row_size.raw[i] = 0;
    struct shift_header_type sh = srt->shift[new_state_element[i].shift_number];
    for (int j = 1; j <= sh.size; j++) {
      int symbol = sh.map[j].symbol;
      if (!cli_options->shift_default_bit || sh.map[j].action != shiftdf.raw[symbol]) {
        row_size.raw[i]++;
        frequency_count.raw[symbol]++;
      }
    }
    for (int state_no = toutput->state_list.raw[new_state_element[i].image]; state_no != NIL; state_no = toutput->state_list.raw[state_no]) {
      num_shifts_saved += row_size.raw[i];
    }
    struct reduce_header_type red;
    // Note that the Default action is skipped !!!
    red = new_state_element[i].reduce;
    for (int j = 1; j <= red.size; j++) {
      int symbol = red.map[j].symbol;
      row_size.raw[i]++;
      frequency_count.raw[symbol]++;
    }
  }
  PRNT3("Number of unique terminal states: %d", ctp->num_terminal_states);

  PRNT3("Number of Shift actions saved by merging: %d", num_shifts_saved);

  PRNT3("Number of Reduce actions saved by merging: %d", num_reductions_saved);

  PRNT3("Number of Reduce saved by default: %d", default_saves);

  sortdes(toutput->ordered_state, row_size, 1, ctp->num_terminal_states, num_terminals);

  if (!cli_options->shift_default_bit) {
    sortdes(frequency_symbol, frequency_count, 1, num_terminals, ctp->num_terminal_states);
    for ALL_TERMINALS3(symbol) {
      toutput->symbol_map.raw[frequency_symbol.raw[symbol]] = symbol;
    }
    toutput->symbol_map.raw[DEFAULT_SYMBOL] = DEFAULT_SYMBOL;
    eoft_image = toutput->symbol_map.raw[eoft_image];
    if (cli_options->error_maps_bit) {
      error_image = toutput->symbol_map.raw[error_image];
      eolt_image = toutput->symbol_map.raw[eolt_image];
    }
    for (int i = 1; i <= num_shift_maps; i++) {
      struct shift_header_type sh = srt->shift[i];
      for (int j = 1; j <= sh.size; j++) {
        sh.map[j].symbol = toutput->symbol_map.raw[sh.map[j].symbol];
      }
    }

    for (int state_no = 1; state_no <= ctp->num_terminal_states; state_no++) {
      struct reduce_header_type red = new_state_element[state_no].reduce;
      for (int i = 1; i <= red.size; i++) {
        red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
      }
    }
    // If ERROR_MAPS are requested, we also have to remap the original
    // REDUCE maps.
    if (cli_options->error_maps_bit) {
      for ALL_STATES3(state_no, ls->num_states) {
        struct reduce_header_type red = srt->reduce[state_no];
        for (int i = 1; i <= red.size; i++) {
          red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
        }
      }
    }
  }
  nte->value = num_shifts + num_shift_reduces + num_reductions - num_shifts_saved - num_reductions_saved - default_saves + ctp->num_terminal_states;
  ffree(rule_count.raw);
  ffree(reduce_action.raw);
  ffree(row_size.raw);
  ffree(frequency_count.raw);
  ffree(frequency_symbol.raw);
  ffree(shift_on_error_symbol.raw);
  ffree(new_state_element_reduce_nodes);
}

/// We now compute the starting position for each terminal state just
/// as we did for the non-terminal states.
/// The starting positions are stored in the vector TERM_STATE_INDEX.
static void overlap_t_rows(struct CLIOptions *cli_options, struct TableOutput *toutput, struct NumTableEntries *nte, ArrayLong term_state_index, struct CTabsProps *ctp, struct new_state_type *new_state_element, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, ArrayShort shiftdf) {
  ArrayShort terminal_list = Allocate_short_array2(num_terminals + 1);
  ctp->increment_size = MAX(nte->value * increment / 100, num_terminals + 1);
  const long old_size = ctp->table_size;
  ctp->table_size = MIN(nte->value + ctp->increment_size, MAX_TABLE_SIZE);
  if ((int) ctp->table_size > old_size) {
    ffree(np->previous.raw);
    ffree(np->next.raw);
    np->next = Allocate_long_array2(ctp->table_size + 1);
    np->previous = Allocate_long_array2(ctp->table_size + 1);
  } else {
    ctp->table_size = old_size;
  }
  ia->first_index = 1;
  np->previous.raw[ia->first_index] = NIL;
  np->next.raw[ia->first_index] = ia->first_index + 1;
  for (int indx = 2; indx < (int) ctp->table_size; indx++) {
    np->next.raw[indx] = indx + 1;
    np->previous.raw[indx] = indx - 1;
  }
  ia->last_index = ctp->table_size;
  np->previous.raw[ia->last_index] = ia->last_index - 1;
  np->next.raw[ia->last_index] = NIL;
  int max_indx = ia->first_index;
  for (int k = 1; k <= ctp->num_terminal_states; k++) {
    const int state_no = toutput->ordered_state.raw[k];
    // For the terminal table, we are dealing with two lists, the SHIFT
    // list, and the REDUCE list. Those lists are merged together first
    // in TERMINAL_LIST.  Since we have to iterate over the list twice,
    // this merging makes things easy.
    int root_symbol = NIL;
    const struct shift_header_type sh = srt->shift[new_state_element[state_no].shift_number];
    for (int i = 1; i <= sh.size; i++) {
      int symbol = sh.map[i].symbol;
      if (!cli_options->shift_default_bit || sh.map[i].action != shiftdf.raw[symbol]) {
        terminal_list.raw[symbol] = root_symbol;
        root_symbol = symbol;
      }
    }
    const struct reduce_header_type red = new_state_element[state_no].reduce;
    for (int i = 1; i <= red.size; i++) {
      terminal_list.raw[red.map[i].symbol] = root_symbol;
      root_symbol = red.map[i].symbol;
    }
    // Look for a suitable index where to overlay the state.
    int indx = ia->first_index;
  look_for_match_in_term_table:
    if (indx == NIL) {
      indx = ctp->table_size + 1;
    }
    if (indx + num_terminals > (int) ctp->table_size) {
      reallocate(cli_options, ctp, np, ia);
    }
    for (int symbol = root_symbol; symbol != NIL; symbol = terminal_list.raw[symbol]) {
      if (np->next.raw[indx + symbol] == OMEGA) {
        indx = np->next.raw[indx];
        goto look_for_match_in_term_table;
      }
    }
    // INDX marks the starting position for the state, remove all the
    // positions that are claimed by terminal actions in the state.
    for (int symbol = root_symbol; symbol != NIL; symbol = terminal_list.raw[symbol]) {
      const int i = indx + symbol;
      if (i == ia->last_index) {
        ia->last_index = np->previous.raw[ia->last_index];
        np->next.raw[ia->last_index] = NIL;
      } else {
        np->next.raw[np->previous.raw[i]] = np->next.raw[i];
        np->previous.raw[np->next.raw[i]] = np->previous.raw[i];
      }
      np->next.raw[i] = OMEGA;
    }
    // We now remove the starting position itself from the list, and
    // mark it as taken(CHECK(INDX) = OMEGA)
    // MAX_INDX is updated if required.
    // TERM_STATE_INDEX(STATE_NO) is properly set to INDX as the starting
    // position of STATE_NO.
    if (ia->first_index == ia->last_index) {
      ia->first_index = NIL;
    } else if (indx == ia->first_index) {
      ia->first_index = np->next.raw[ia->first_index];
      np->previous.raw[ia->first_index] = NIL;
    } else if (indx == ia->last_index) {
      ia->last_index = np->previous.raw[ia->last_index];
      np->next.raw[ia->last_index] = NIL;
    } else {
      np->next.raw[np->previous.raw[indx]] = np->next.raw[indx];
      np->previous.raw[np->next.raw[indx]] = np->previous.raw[indx];
    }
    np->next.raw[indx] = OMEGA;
    if (indx > max_indx) {
      max_indx = indx;
    }
    term_state_index.raw[state_no] = indx;
  }
  // Update all counts, and report statistics.
  ctp->term_check_size = max_indx + num_terminals;
  for (ctp->term_action_size = max_indx + num_terminals; ctp->term_action_size >= max_indx; ctp->term_action_size--) {
    if (np->next.raw[ctp->term_action_size] == OMEGA) {
      break;
    }
  }
  printf("\n");
  PRNT3("Length of Terminal Check Table: %d", ctp->term_check_size);
  PRNT3("Length of Terminal Action Table: %d", ctp->term_action_size);
  PRNT3("Number of entries in Terminal Action Table: %ld", nte->value);
  const long percentage = (ctp->term_action_size - nte->value) * 1000 / nte->value;
  PRNT3("Percentage of increase: %ld.%ld%%", percentage / 10, percentage % 10);
  // We now write out the tables to the SYSTAB file.
  ctp->table_size = MAX(
    MAX(
      MAX(
        MAX(
          ctp->check_size,
          ctp->term_check_size
        ), ctp->shift_check_size), ctp->action_size),
    ctp->term_action_size
  );
  ffree(terminal_list.raw);
  ffree(np->next.raw);
  ffree(np->previous.raw);
}

void cmprspa(struct CLIOptions *cli_options, struct TableOutput *toutput, struct DetectedSetSizes *dss, struct CTabsProps *ctp, struct OutputFiles* of, struct NextPrevious* np, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, ArrayLong scope_right_side, struct lastats_type *lastats, ArrayShort shiftdf, ArrayLong gotodef, ArrayShort gd_index, ArrayShort gd_range, ArrayShort scope_state, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table, struct ScopeCounter* sc, char *output_buffer, struct OutputPtr output_ptr2, struct symno_type *symno, char *string_table, int *name, struct LAState* ls) {
  ArrayBool shift_on_error_symbol = Allocate_bool_array2(ls->max_la_state + 1);
  struct new_state_type *new_state_element;
  calloc0p(&new_state_element, ls->max_la_state + 1, struct new_state_type);
  struct node **new_state_element_reduce_nodes;
  calloc0p(&new_state_element_reduce_nodes, ls->max_la_state + 1, struct node *);
  long last_symbol = remap_non_terminals(cli_options, toutput, gotodef, statset, ls);
  struct NumTableEntries nte = (struct NumTableEntries){
    .value = 0,
  };
  overlap_nt_rows(cli_options, toutput, &nte, ctp, last_symbol, np, ia, statset, ls);
  struct TResult tresult = (struct TResult){
    .single_root = NIL,
    .multi_root = NIL,
    .empty_root = NIL,
  };
  merge_similar_t_rows(cli_options, toutput, shift_on_error_symbol, new_state_element_reduce_nodes, &tresult, new_state_element, srt, lastats, statset, ls);
  ArrayLong shift_check_index = Allocate_long_array2(num_shift_maps + 1);
  ArrayShort shift_image = Allocate_short_array2(ls->max_la_state + 1);
  ArrayShort real_shift_number = Allocate_short_array2(num_shift_maps + 1);
  overlay_sim_t_rows(cli_options, toutput, shift_on_error_symbol, new_state_element_reduce_nodes, &tresult, &nte, shift_check_index, ctp, new_state_element, shift_image, real_shift_number, np, ia, srt, rules, lastats, shiftdf, statset, ls);
  ArrayLong term_state_index = Allocate_long_array2(ls->max_la_state + 1);
  overlap_t_rows(cli_options, toutput, &nte, term_state_index, ctp, new_state_element, np, ia, srt, shiftdf);
  print_space_parser(cli_options, toutput, dss, term_state_index, shift_check_index, ctp, new_state_element, shift_image, real_shift_number, of, scope, ia, srt, scope_right_side, shiftdf, gotodef, gd_index, gd_range, rules, scope_state, statset, item_table, sc, output_buffer, output_ptr2, symno, string_table, name, ls);
}

// endregion

// region timetab

/// We now remap the symbols in the unified Table based on frequency.
/// We also remap the states based on frequency.
struct DefaultSaves {
  int default_saves;
  int last_symbol;
} remap_symbols(struct TableOutput* toutput, ArrayBool is_terminal, struct SRTable* srt, struct lastats_type *lastats, struct statset_type *statset, struct CLIOptions* cli_options, struct LAState* ls) {
  int default_saves = 0;
  ArrayLong frequency_symbol = Allocate_long_array2(num_symbols + 1);
  ArrayLong frequency_count = Allocate_long_array2(num_symbols + 1);
  ArrayLong row_size = Allocate_long_array2(ls-> max_la_state + 1);
  printf("\n");
  //     The variable FREQUENCY_SYMBOL is used to hold the symbols
  // in the grammar,  and the variable FREQUENCY_COUNT is used
  // correspondingly to hold the number of actions defined on each
  // symbol.
  // ORDERED_STATE and ROW_SIZE are used in a similar fashion for
  // states.
  for (int i = 1; i <= num_symbols; i++) {
    frequency_symbol.raw[i] = i;
    frequency_count.raw[i] = 0;
  }
  for ALL_STATES3(state_no, ls->num_states) {
    toutput->ordered_state.raw[state_no] = state_no;
    row_size.raw[state_no] = 0;
    struct shift_header_type sh = srt->shift[statset[state_no].shift_number];
    for (int i = 1; i <= sh.size; i++) {
      row_size.raw[state_no]++;
      long symbol = sh.map[i].symbol;
      frequency_count.raw[symbol]++;
    }
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      row_size.raw[state_no]++;
      long symbol = go_to.map[i].symbol;
      frequency_count.raw[symbol]++;
    }
    struct reduce_header_type red = srt->reduce[state_no];
    short default_rule = red.map[0].rule_number;
    for (int i = 1; i <= red.size; i++) {
      if (red.map[i].rule_number != default_rule) {
        row_size.raw[state_no]++;
        long symbol = red.map[i].symbol;
        frequency_count.raw[symbol]++;
      } else {
        default_saves++;
      }
    }
  }
  PRNT3("Number of Reductions saved by default: %d", default_saves);
  for ALL_LA_STATES3(state_no, ls) {
    toutput->ordered_state.raw[state_no] = state_no;
    row_size.raw[state_no] = 0;
    struct shift_header_type sh = srt->shift[lastats[state_no].shift_number];
    for (int i = 1; i <= sh.size; i++) {
      row_size.raw[state_no]++;
      long symbol = sh.map[i].symbol;
      frequency_count.raw[symbol]++;
    }
    struct reduce_header_type red = lastats[state_no].reduce;
    short default_rule = red.map[0].rule_number;
    for (int i = 1; i <= red.size; i++) {
      if (red.map[i].rule_number != default_rule) {
        row_size.raw[state_no]++;
        long symbol = red.map[i].symbol;
        frequency_count.raw[symbol]++;
      } else {
        default_saves++;
      }
    }
  }
  //     The non-terminals are sorted in descending order based on the
  // number of actions defined on them.
  //     The terminals are sorted in descending order based on the
  // number of actions defined on them.
  sortdes(frequency_symbol, frequency_count, 1, num_terminals, ls->max_la_state);
  sortdes(frequency_symbol, frequency_count, num_terminals + 1, num_symbols, ls->max_la_state);
  long last_symbol;
  for (last_symbol = num_symbols; last_symbol > num_terminals; last_symbol--) {
    if (frequency_count.raw[last_symbol] != 0) {
      break;
    }
  }
  // We now merge the two sorted arrays of symbols giving precedence to
  // the terminals.  Note that we can guarantee that the terminal array
  // will be depleted first since it has precedence, and we know that
  // there exists at least one non-terminal: the accept non-terminal,
  // on which no action is defined.
  // As we merge the symbols, we keep track of which ones are terminals
  // and which ones are non-terminals.  We also keep track of the new
  // mapping for the symbols in SYMBOL_MAP.
  long j = num_terminals + 1;
  int k = 0;
  for (int i = 1; i <= num_terminals;) {
    k++;
    long symbol;
    if (frequency_count.raw[i] >= frequency_count.raw[j]) {
      symbol = frequency_symbol.raw[i];
      is_terminal.raw[k] = true;
      i++;
    } else {
      symbol = frequency_symbol.raw[j];
      is_terminal.raw[k] = false;
      j++;
    }
    toutput->symbol_map.raw[symbol] = k;
  }
  toutput->symbol_map.raw[DEFAULT_SYMBOL] = DEFAULT_SYMBOL;
  // Process the remaining non-terminal and useless terminal symbols.
  for (; j <= num_symbols; j++) {
    k++;
    long symbol = frequency_symbol.raw[j];
    is_terminal.raw[k] = false;
    toutput->symbol_map.raw[symbol] = k;
  }
  eoft_image = toutput->symbol_map.raw[eoft_image];
  if (cli_options->error_maps_bit) {
    error_image = toutput->symbol_map.raw[error_image];
    eolt_image = toutput->symbol_map.raw[eolt_image];
  }
  //    All symbol entries in the state automaton are updated based on
  // the new mapping of the symbols.
  // The states are sorted in descending order based on the number of
  // actions defined on them.
  for ALL_STATES3(state_no, ls->num_states) {
    struct goto_header_type go_to = statset[state_no].go_to;
    // Remap Goto map
    for (int i = 1; i <= go_to.size; i++) {
      go_to.map[i].symbol = toutput->symbol_map.raw[go_to.map[i].symbol];
    }
    struct reduce_header_type red = srt->reduce[state_no];
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
    }
  }
  for ALL_LA_STATES3(state_no, ls) {
    struct reduce_header_type red = lastats[state_no].reduce;
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol = toutput->symbol_map.raw[red.map[i].symbol];
    }
  }
  for (int i = 1; i <= num_shift_maps; i++) {
    struct shift_header_type sh = srt->shift[i];
    for (int j = 1; j <= sh.size; j++) {
      sh.map[j].symbol = toutput->symbol_map.raw[sh.map[j].symbol];
    }
  }
  sortdes(toutput->ordered_state, row_size, 1, ls->max_la_state, num_symbols);
  ffree(frequency_symbol.raw);
  ffree(frequency_count.raw);
  ffree(row_size.raw);
  return (struct DefaultSaves) {
    .default_saves = default_saves,
    .last_symbol = last_symbol,
  };
}

/// We now overlap the State automaton table, or more precisely,  we
/// compute the starting position in a vector where each of its rows
/// may be placed without clobbering elements in another row.
/// The starting positions are stored in the vector STATE_INDEX.
static void overlap_tables(struct CLIOptions *cli_options, struct TableOutput* toutput, ArrayBool is_terminal, struct DefaultSaves default_saves, struct CTabsProps* ctp, long last_symbol, struct NextPrevious* np, struct ImportantAspects* ia, struct SRTable* srt, struct lastats_type *lastats, struct statset_type *statset, struct LAState* ls) {
  ArrayLong symbol_list = Allocate_long_array2(num_symbols + 1);
  num_entries -= default_saves.default_saves;
  ctp->increment_size = MAX(num_entries * increment / 100, num_symbols + 1);
  ctp->table_size = MIN(num_entries + ctp->increment_size, MAX_TABLE_SIZE);
  // Allocate space for table, and initialize the AVAIL_POOL list.
  // The variable FIRST_INDEX keeps track of the first element in the
  // doubly-linked list, and LAST_ELEMENT keeps track of the last
  // element in the list.
  // The variable MAX_INDX is used to keep track of the maximum
  // starting position for a row that has been used.
  np->next = Allocate_long_array2(ctp->table_size + 1);
  np->previous = Allocate_long_array2(ctp->table_size + 1);
  ia->first_index = 1;
  np->next.raw[ia->first_index] = ia->first_index + 1; /* Should be constant-folded */
  np->previous.raw[ia->first_index] = NIL;
  for (long indx = 2; indx < (int) ctp->table_size; indx++) {
    np->next.raw[indx] = indx + 1;
    np->previous.raw[indx] = indx - 1;
  }
  ia->last_index = ctp->table_size;
  np->previous.raw[ia->last_index] = ia->last_index - 1;
  np->next.raw[ia->last_index] = NIL;
  long max_indx = ia->first_index;
  // We now iterate over all the states in their new sorted order as
  // indicated by the variable STATE_NO, and determine an "overlap"
  // position for them.
  for (int k = 1; k <= ls->max_la_state; k++) {
    const long state_no = toutput->ordered_state.raw[k];
    // First, we iterate over all actions defined in STATE_NO, and
    // create a set with all the symbols involved.
    int root_symbol = NIL;
    struct shift_header_type sh;
    struct reduce_header_type red;
    if (state_no > ls->num_states) {
      sh = srt->shift[lastats[state_no].shift_number];
      red = lastats[state_no].reduce;
    } else {
      const struct goto_header_type go_to = statset[state_no].go_to;
      for (int i = 1; i <= go_to.size; i++) {
        int symbol = go_to.map[i].symbol;
        symbol_list.raw[symbol] = root_symbol;
        root_symbol = symbol;
      }
      sh = srt->shift[statset[state_no].shift_number];
      red = srt->reduce[state_no];
    }
    for (int i = 1; i <= sh.size; i++) {
      int symbol = sh.map[i].symbol;
      symbol_list.raw[symbol] = root_symbol;
      root_symbol = symbol;
    }
    symbol_list.raw[0] = root_symbol;
    root_symbol = 0;
    short default_rule = red.map[0].rule_number;
    for (int i = 1; i <= red.size; i++) {
      if (red.map[i].rule_number != default_rule) {
        int symbol = red.map[i].symbol;
        symbol_list.raw[symbol] = root_symbol;
        root_symbol = symbol;
      }
    }
    // INDX is set to the beginning of the list of available slots and
    // we try to determine if it might be a valid starting position. If
    // not, INDX is moved to the next element, and we repeat the process
    // until a valid position is found.
    long indx = ia->first_index;
  look_for_match_in_table:
    if (indx == NIL) {
      indx = ctp->table_size + 1;
    }
    if (indx + num_symbols > (int) ctp->table_size) {
      reallocate(cli_options, ctp, np, ia);
    }
    for (int symbol = root_symbol; symbol != NIL; symbol = symbol_list.raw[symbol]) {
      if (np->next.raw[indx + symbol] == OMEGA) {
        indx = np->next.raw[indx];
        goto look_for_match_in_table;
      }
    }
    // At this stage, a position(INDX), was found to overlay the row in
    // question.  Remove elements associated with all positions that
    // will be taken by row from the doubly-linked list.
    // NOTE that since SYMBOLs start at 1, the first index can never be
    // a candidate (==> I = INDX + SYMBOL) in this loop.
    if (indx > max_indx) {
      max_indx = indx;
    }
    toutput->state_index.raw[state_no] = indx;
    for (int symbol = root_symbol; symbol != NIL; symbol = symbol_list.raw[symbol]) {
      const long i = indx + symbol;
      if (ia->first_index == ia->last_index) {
        ia->first_index = NIL;
      } else if (i == ia->first_index) {
        ia->first_index = np->next.raw[ia->first_index];
        np->previous.raw[ia->first_index] = NIL;
      } else if (i == ia->last_index) {
        ia->last_index = np->previous.raw[ia->last_index];
        np->next.raw[ia->last_index] = NIL;
      } else {
        np->next.raw[np->previous.raw[i]] = np->next.raw[i];
        np->previous.raw[np->next.raw[i]] = np->previous.raw[i];
      }
      np->next.raw[i] = OMEGA;
    }
  }
  // Update all global counters, and compute ACCEPT_ACTION and
  // ERROR_ACTION.
  ctp->table_size = max_indx + num_symbols;
  ia->accept_act = max_indx + num_rules + 1;
  ia->error_act = ia->accept_act + 1;
  for (ctp->action_size = ctp->table_size; ctp->action_size >= max_indx; ctp->action_size--) {
    if (np->next.raw[ctp->action_size] == OMEGA) {
      break;
    }
  }
  printf("\n");
  PRNT3("Length of Check table: %ld", ctp->table_size);
  PRNT3("Length of Action table: %ld", ctp->action_size);
  PRNT3("Number of entries in Action Table: %ld", num_entries);
  const long percentage = (ctp->action_size - num_entries) * 1000 / num_entries;
  PRNT3("Percentage of increase: %ld.%ld%%", percentage / 10, percentage % 10);
  long num_bytes;
  if (cli_options->byte_bit) {
    num_bytes = 2 * ctp->action_size + ctp->table_size;
    if (!cli_options->goto_default_bit && !cli_options->nt_check_bit) {
      for (; last_symbol >= 1 && !is_terminal.raw[last_symbol]; last_symbol--) {
      }
    }
    PRNT3("Highest symbol in Check Table: %ld", last_symbol);
    if (last_symbol > 255) {
      num_bytes += ctp->table_size;
    }
  } else {
    num_bytes = 2 * (ctp->action_size + ctp->table_size);
  }
  if (cli_options->goto_default_bit) {
    num_bytes += (long) 2 * num_symbols;
  }
  const long k_bytes = num_bytes / 1024 + 1;
  PRNT3("Storage Required for Tables: %ld Bytes, %ldK", num_bytes, k_bytes);
  num_bytes = (long) 4 * num_rules;
  if (cli_options->byte_bit) {
    num_bytes -= num_rules;
    if (num_symbols < 256) {
      num_bytes -= num_rules;
    }
  }
  PRNT3("Storage Required for Rules: %ld Bytes", num_bytes);
}

/// In this routine we compress the State tables and write them out
/// to a file. The emphasis here is in generating tables that allow
/// fast access. The terminal and non-terminal tables are compressed
/// together, to achieve maximum speed efficiency.
/// Otherwise, the compression technique used in this table is
/// analogous to the technique used in the routine CMPRSPA.
void cmprtim(struct CLIOptions *cli_options, struct TableOutput* toutput, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, struct scope_type *scope, struct ImportantAspects* ia, struct SRTable* srt, ArrayLong scope_right_side, struct lastats_type *lastats, ArrayLong gotodef, ArrayShort gd_index, ArrayShort gd_range, ArrayShort scope_state, struct statset_type *statset, struct ruletab_type *rules, struct itemtab *item_table, struct ScopeCounter* sc, char *output_buffer, struct OutputPtr output_ptr2, struct symno_type *symno, char *string_table, int *name, struct LAState* ls) {
  ArrayBool is_terminal = Allocate_bool_array2(num_symbols + 1);
  struct DefaultSaves default_saves = remap_symbols(toutput, is_terminal, srt, lastats, statset, cli_options, ls);
  overlap_tables(cli_options, toutput, is_terminal, default_saves, ctp, default_saves.last_symbol, np, ia, srt, lastats, statset, ls);
  print_time_parser(cli_options, toutput, dss, ctp, of, np, scope, ia, srt, scope_right_side, lastats, gotodef, gd_index, gd_range, rules, scope_state, statset, item_table, sc, output_buffer, output_ptr2, symno, string_table, name, ls);
}

// endregion

// region ptables

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
      talloc0p(&q, struct ptables_action_element);
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
static void compute_shift_default(struct SRTable* srt, struct lastats_type *lastats, ArrayShort* shiftdf, struct statset_type *statset, struct LAState* ls) {
  // Set up a pool of temporary space.
  reset_temporary_space();
  int shift_count = 0;
  int shift_reduce_count = 0;
  *shiftdf = Allocate_short_array2(num_terminals + 1);
  struct ptables_action_element **action_count;
  calloc0p(&action_count, num_terminals + 1, struct ptables_action_element *);
  // For each state, invoke PROCESS_SHIFT_ACTIONS to process the
  // shift map associated with that state.
  for ALL_STATES3(state_no, ls->num_states) {
    process_shift_actions(action_count, statset[state_no].shift_number, srt);
  }
  for ALL_LA_STATES3(state_no, ls) {
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
    shiftdf->raw[symbol] = default_action;
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
static void compute_goto_default(ArrayLong* gotodef, struct statset_type *statset, struct LAState* ls) {
  // Set up a pool of temporary space.
  reset_temporary_space();
  int goto_count = 0;
  int goto_reduce_count = 0;
  *gotodef = Allocate_long_array2(num_non_terminals);
  gotodef->raw -= num_terminals + 1;
  struct ptables_action_element **action_count;
  calloc0p(&action_count, num_non_terminals, struct ptables_action_element *);
  action_count -= num_terminals + 1;
  if (action_count == NULL) {
    nospace();
  }
  // The array ACTION_COUNT is used to construct a map from each
  // non-terminal into the set (list) of actions defined on that
  // non-terminal. A count of how many occurences of each action
  // is also kept.
  // This loop is analoguous to the loop in PROCESS_SHIFT_ACTIONS.
  for ALL_STATES3(state_no, ls->num_states) {
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
        talloc0p(&q, struct ptables_action_element);
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
    (*gotodef).raw[symbol] = default_action;
    if (default_action > 0) {
      /* A state number? */
      goto_count += max_count;
    } else {
      goto_reduce_count += max_count;
    }
  }
  //   We now iterate over the automaton and eliminate all GOTO actions
  // for which there is a DEFAULT.
  for ALL_STATES3(state_no, ls->num_states) {
    int k = 0;
    struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      if ((*gotodef).raw[go_to.map[i].symbol] != go_to.map[i].action) {
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
void process_tables(char *tab_file, struct OutputFiles *output_files, struct CLIOptions *cli_options, struct DetectedSetSizes* dss, struct CTabsProps* ctp, struct OutputFiles* of, struct NextPrevious* np, struct scope_type *scope, ArrayShort gd_range, struct SRTable* srt, ArrayLong scope_right_side, struct lastats_type *lastats, ArrayShort* shiftdf, ArrayLong* gotodef, ArrayShort gd_index, struct statset_type *statset, ArrayShort scope_state, struct ruletab_type *rules, struct itemtab *item_table, struct symno_type *symno, struct ScopeCounter* sc, char *string_table, int *name, struct LAState* ls) {
  // First, we decrease by 1 the constants NUM_SYMBOLS
  // and NUM_TERMINALS, remove the EMPTY symbol(1) and remap the
  // other symbols beginning at 1.  If default reduction is
  // requested, we assume a special DEFAULT_SYMBOL with number zero.
  eoft_image--;
  accept_image--;
  if (cli_options->error_maps_bit) {
    error_image--;
    eolt_image--;
  }
  num_terminals--;
  num_symbols--;
  // Remap all the symbols used in GOTO and REDUCE actions.
  // Remap all the symbols used in GD_RANGE.
  // Remap all the symbols used in the range of SCOPE.
  // Release space trapped by the maps IN_STAT and FIRST.
  for ALL_STATES3(state_no, ls->num_states) {
    const struct goto_header_type go_to = statset[state_no].go_to;
    for (int i = 1; i <= go_to.size; i++) {
      go_to.map[i].symbol--;
    }
    struct reduce_header_type red = srt->reduce[state_no];
    for (int i = 1; i <= red.size; i++) {
      red.map[i].symbol--;
    }
  }
  for ALL_LA_STATES3(state_no, ls) {
    struct reduce_header_type red = lastats[state_no].reduce;
    for (int i = 1; i <= red.size; i++)
      red.map[i].symbol--;
  }
  for (int i = 1; i <= gotodom_size; i++) {
    gd_range.raw[i]--;
  }
  for (int i = 1; i <= sc->num_scopes; i++) {
    scope[i].lhs_symbol--;
    scope[i].look_ahead--;
  }
  for (int i = 1; i <= sc->scope_rhs_size; i++) {
    if (scope_right_side.raw[i] != 0) {
      scope_right_side.raw[i]--;
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
  if (cli_options->error_maps_bit) {
    for ALL_ITEMS3(item_no, ls->num_items) {
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
    compute_shift_default(srt, lastats, shiftdf, statset, ls);
  }
  if (cli_options->goto_default_bit) {
    compute_goto_default(gotodef, statset, ls);
  }
  // Release the pool of temporary space.
  free_temporary_space();
  char *_output_ptr = NULL;
  char *_output_buffer = NULL;
  struct OutputPtr op = (struct OutputPtr) {
    .output_ptr = &_output_ptr,
    .output_buffer = &_output_buffer,
  };
  calloc0p(op.output_buffer, IOBUFFER_SIZE, char);
  FILE *systab;
  if (!cli_options->c_bit && !cli_options->cpp_bit && !cli_options->java_bit) {
    if ((systab = fopen(tab_file, "w")) == NULL) {
      fprintf(stderr, "***ERROR: Table file \"%s\" cannot be opened\n", tab_file);
      exit(12);
    }
  }
  struct TableOutput toutput = init_table_output(ls);
  init_file(&of->sysdcl, output_files->dcl_file, of->dcl_tag);
  init_file(&of->syssym, output_files->sym_file, of->sym_tag);
  init_file(&of->sysdef, output_files->def_file, of->def_tag);
  init_file(&of->sysprs, output_files->prs_file, of->prs_tag);
  struct ImportantAspects ia = (struct ImportantAspects) {};
  if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
    cmprspa(cli_options, &toutput, dss, ctp, of, np, scope, &ia, srt, scope_right_side, lastats, *shiftdf, *gotodef, gd_index, gd_range, scope_state, statset, rules, item_table, sc, *op.output_buffer, op, symno, string_table, name, ls);
  } else if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    cmprtim(cli_options, &toutput, dss, ctp, of, np, scope, &ia, srt, scope_right_side, lastats, *gotodef, gd_index, gd_range, scope_state, statset, rules, item_table, sc, *op.output_buffer, op, symno, string_table, name, ls);
  } else {
    exit(999);
  }
  if (!cli_options->c_bit && !cli_options->cpp_bit && !cli_options->java_bit) {
    fclose(systab);
  }
}

// endregion

























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



