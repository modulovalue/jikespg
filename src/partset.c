#include <stdlib.h>
#include "common.h"

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
void partset(JBitset collection, ArrayLong element_size, ArrayLong list, ArrayLong start, ArrayLong stack, long set_size, bool from_process_scopes, struct LAState* ls) {
  int collection_size;
  // TODO • Remove this unnecessary indirection.
  if (from_process_scopes) {
    collection_size = set_size;
    set_size = ls->num_states;
  } else {
    collection_size = ls->num_states;
  }
  const int bctype = collection.size;
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
