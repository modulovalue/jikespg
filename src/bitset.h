#ifndef BITSET_H
#define BITSET_H
#include <stdlib.h>

#include "stdbool.h"
#include "limits.h"

typedef unsigned int BOOLEAN_CELL;

struct jbitset {
  BOOLEAN_CELL * raw;
  long size;
};

#define JBitset struct jbitset

#define calloc0_set(into, s, l) \
  into = (JBitset) {.raw = calloc(s, (l) * sizeof(BOOLEAN_CELL)), .size = l}; \
  if (into.raw == NULL) \
    nospace(hostfile, __LINE__);

/// The following macros are used to define operations on sets that
/// are represented as bit-strings.  BOOLEAN_CELL is a type that is
/// used as the elemental unit used to construct the sets.  For
/// example, if BOOLEAN_CELL consists of four bytes and assumming
/// that each byte contains 8 bits then the constant SIZEOF_BC
/// represents the total number of bits that is contained in each
/// elemental unit.
///
/// In general, a parameter called "set" or "set"i, where i is an
/// integer, is a pointer to a set or array of sets; a parameter
/// called "i" or "j" represents an index in an array of sets; a
/// parameter called "b" represents a particular element (or bit)
/// within a set.
static const int SIZEOF_BC = sizeof(BOOLEAN_CELL) * CHAR_BIT;
static const int BC_OFFSET = SIZEOF_BC - 1;

/// This macro takes as argument an array of bit sets called "set",
/// an integer "nt" indicating the index of a particular set in the
/// array and an integer "t" indicating a particular element within
/// the set. IS_IN_SET check whether ot not the element "t" is in
/// the set "set(nt)".
///
/// The value (nt*term_set_size) is used to determine the starting
/// address of the set element in question.  The value
/// (??? / SIZEOF_BC) is used to determine the actual BOOLEAN_CELL
/// containing the bit in question.  Finally, the value
/// (SIZEOF_BC - (t % SIZEOF_BC)) identifies the actual bit in the
/// unit. The bit in question is pushed to the first position and
/// and-ed with the value 01. This operation yields the value TRUE
/// if the bit is on. Otherwise, the value FALSE is obtained.
/// Recall that in C, one cannot shift (left or right) by 0. This
/// is why the ? is used here.
static bool IS_IN_SET(const JBitset set, const int i, const int b) {
  // is b in set[i] ?
  return set.raw[i * set.size + (b - 1) / SIZEOF_BC] &
         ((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

/// The macro SET_UNION takes as argument two arrays of sets:
/// "set1" and "set2", and two integers "i" and "j" which are
/// indices to be used to access particular sets in "set1" and
/// "set2", respectively.  SET_UNION computes the union of the two
/// sets in question and places the result in the relevant set in
/// "set1".
///
/// The remaining macros are either analogous to IS_IN_SET or
/// SET_UNION.
///
/// Note that a macro with the variable "kji" declared in its body
/// should not be invoked with a parameter of the same name.
/// set[i] union set2[j]
static void SET_UNION(const JBitset set1, const int i, const JBitset set2, const int j) {
  if (set1.size != set2.size) exit(666);
  int size = set1.size;
  for (register int kji = 0; kji < size; kji++) {
    set1.raw[i * size + kji] |= set2.raw[j * size + kji];
  }
}

/// set = {}
static void INIT_SET(const JBitset set) {
  for (register int kji = 0; kji < set.size; kji++) {
    set.raw[kji] = 0;
  }
}

/// set1[i] = set2[j]
static void ASSIGN_SET(const JBitset set1, const int i, const JBitset set2, const int j) {
  if (set1.size != set2.size) exit(666);
  int size = set1.size;
  for (register int kji = 0; kji < size; kji++) {
    set1.raw[i * size + kji] = set2.raw[j * size + kji];
  }
}

/// set[i] = set[i] with b;
static void SET_BIT_IN(const JBitset set, const int i, const int b) {
  set.raw[i * set.size + (b - 1) / SIZEOF_BC] |=
      (b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1;
}

/// set[i] = set[i] less b;
static void RESET_BIT_IN(const JBitset set, const int i, const int b) {
  set.raw[i * set.size + (b - 1) / SIZEOF_BC] &=
      ~((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

static void EMPTY_COLLECTION_SET(const JBitset collection, const int i) {
  for (register int kji = 0; kji < collection.size; kji++) {
    collection.raw[i * collection.size + kji] = 0;
  }
}

/// The following macros can be used to check, set, or reset a bit
/// in a bit-string of any length.
static void SET_BIT(const JBitset set, const int b) {
  set.raw[(b - 1) / SIZEOF_BC] |= (b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1;
}

static void RESET_BIT(const JBitset set, const int b) {
  set.raw[(b - 1) / SIZEOF_BC] &=
      ~((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

/// is b in set ?
static bool IS_ELEMENT(const JBitset set, const int b) {
  return set.raw[(b - 1) / SIZEOF_BC] &
         ((b + BC_OFFSET) % SIZEOF_BC ? (BOOLEAN_CELL) 1 << (b + BC_OFFSET) % SIZEOF_BC : (BOOLEAN_CELL) 1);
}

static void B_ASSIGN_SET(const JBitset s1, const int dest, const JBitset s2, const int source, const int bound) {
  for (int j = 0; j < bound; j++) {
    s1.raw[dest * bound + j] = s2.raw[source * bound + j];
  }
}

static void B_SET_UNION(const JBitset s1, const int dest, const JBitset s2, const int source, const int bound) {
  for (int j = 0; j < bound; j++) {
    s1.raw[dest * bound + j] |= s2.raw[source * bound + j];
  }
}

/// EQUAL_SETS checks to see if two sets are equal and returns True or False
static bool equal_sets(const JBitset set1, const int indx1, const JBitset set2, const int indx2, const int bound) {
  for (register int i = 0; i < bound; i++) {
    if (set1.raw[indx1 * bound + i] != set2.raw[indx2 * bound + i])
      return false;
  }
  return true;
}

#endif //BITSET_H
