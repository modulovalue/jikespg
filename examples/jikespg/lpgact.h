#pragma once

/// BUILD_SYMNO constructs the SYMNO table which is a mapping from each
/// symbol number into that symbol.
void build_symno(struct ParserState* ps) {
  const long symno_size = num_symbols + 1;
  calloc0p(&ps->symno, symno_size, struct symno_type);
  // Go through entire hash table. For each non_empty bucket, go through
  // linked list in that bucket.
  for (int i = 0; i < HT_SIZE; ++i) {
    for (const struct hash_type *p = ps->hash_table[i]; p != NULL; p = p->link) {
      const int symbol = p->number;
      // Not an alias
      if (symbol >= 0) {
        ps->symno[symbol].name_index = OMEGA;
        ps->symno[symbol].ptr = p->st_ptr;
      }
    }
  }
}

static void (*rule_action[]) (struct ParserState* ps) = {NULL,
     null_action, /* 1 */
     null_action, /* 2 */
     bad_first_symbol, /* 3 */
     bad_first_symbol, /* 4 */
     bad_first_symbol, /* 5 */
     bad_first_symbol, /* 6 */
     bad_first_symbol, /* 7 */
     bad_first_symbol, /* 8 */
     bad_first_symbol, /* 9 */
     act10, /* 10 */
     null_action, /* 11 */
     null_action, /* 12 */
     act13, /* 13 */
     act14, /* 14 */
     null_action, /* 15 */
     act16, /* 16 */
     bad_macro_name, /* 17 */
     bad_macro_name, /* 18 */
     bad_macro_name, /* 19 */
     bad_macro_name, /* 20 */
     act21, /* 21 */
     act22, /* 22 */
     null_action, /* 23 */
     definition_expected, /* 24 */
     definition_expected, /* 25 */
     definition_expected, /* 26 */
     definition_expected, /* 27 */
     definition_expected, /* 28 */
     definition_expected, /* 29 */
     definition_expected, /* 30 */
     null_action, /* 31 */
     process_terminal, /* 32 */
     process_terminal, /* 33 */
     process_terminal, /* 34 */
     bad_terminal, /* 35 */
     bad_terminal, /* 36 */
     act37, /* 37 */
     null_action, /* 38 */
     act39, /* 39 */
     null_action, /* 40 */
     null_action, /* 41 */
     null_action, /* 42 */
     null_action, /* 43 */
     null_action, /* 44 */
     null_action, /* 45 */
     null_action, /* 46 */
     null_action, /* 47 */
     null_action, /* 48 */
     null_action, /* 49 */
     null_action, /* 50 */
     null_action, /* 51 */
     null_action, /* 52 */
     null_action, /* 53 */
     bad_alias_rhs, /* 54 */
     bad_alias_rhs, /* 55 */
     bad_alias_rhs, /* 56 */
     act57, /* 57 */
     null_action, /* 58 */
     act59, /* 59 */
     missing_quote, /* 60 */
     missing_quote, /* 61 */
     null_action, /* 62 */
     act63, /* 63 */
     bad_start_symbol, /* 64 */
     bad_start_symbol, /* 65 */
     bad_start_symbol, /* 66 */
     bad_start_symbol, /* 67 */
     act68, /* 68 */
     misplaced_keyword_found_in_START_section, /* 69 */
     misplaced_keyword_found_in_START_section, /* 70 */
     misplaced_keyword_found_in_START_section, /* 71 */
     misplaced_keyword_found_in_START_section, /* 72 */
     act73, /* 73 */
     act74, /* 74 */
     null_action, /* 75 */
     null_action, /* 76 */
     act77, /* 77 */
     act78, /* 78 */
     act79, /* 79 */
     null_action, /* 80 */
     null_action, /* 81 */
     act82, /* 82 */
     act83, /* 83 */
     bad_first_symbol_in_RULES_section, /* 84 */
     bad_first_symbol_in_RULES_section, /* 85 */
     bad_first_symbol_in_RULES_section, /* 86 */
     bad_first_symbol_in_RULES_section, /* 87 */
     rule_without_left_hand_side, /* 88 */
     rule_without_left_hand_side, /* 89 */
     rule_without_left_hand_side, /* 90 */
     act91, /* 91 */
     act92, /* 92 */
     act93, /* 93 */
     misplaced_keyword_found_in_RULES_section, /* 94 */
     misplaced_keyword_found_in_RULES_section, /* 95 */
     misplaced_keyword_found_in_RULES_section, /* 96 */
     misplaced_keyword_found_in_RULES_section, /* 97 */
     misplaced_keyword_found_in_RULES_section, /* 98 */
     null_action, /* 99 */
     act100, /* 100 */
     null_action, /* 101 */
     null_action, /* 102 */
     null_action, /* 103 */
     null_action, /* 104 */
     null_action, /* 105 */
     null_action, /* 106 */
     null_action, /* 107 */
     null_action, /* 108 */
     null_action, /* 109 */
     misplaced_keyword_found_in_NAMES_section, /* 110 */
     misplaced_keyword_found_in_NAMES_section, /* 111 */
     misplaced_keyword_found_in_NAMES_section, /* 112 */
     misplaced_keyword_found_in_NAMES_section, /* 113 */
     misplaced_keyword_found_in_NAMES_section, /* 114 */
     misplaced_keyword_found_in_NAMES_section, /* 115 */
     act116, /* 116 */
     act117, /* 117 */
     null_action, /* 118 */
     null_action, /* 119 */
     process_TERMINALS_section, /* 120 */
     process_TERMINALS_section, /* 121 */
     process_ALIAS_section, /* 122 */
     process_ALIAS_section, /* 123 */
     null_action, /* 124 */
     null_action, /* 125 */
     null_action, /* 126 */
     null_action, /* 127 */
     null_action, /* 128 */
     null_action, /* 129 */
     null_action, /* 130 */
     null_action, /* 131 */
     act132, /* 132 */
     null_action, /* 133 */
     null_action, /* 134 */
     null_action, /* 135 */
     null_action, /* 136 */
     null_action, /* 137 */
     null_action, /* 138 */
     null_action, /* 139 */
     null_action, /* 140 */
     null_action, /* 141 */
     NULL};
