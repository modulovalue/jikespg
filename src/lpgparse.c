#include <stdlib.h>
#include <string.h>
#include "common.h"

// region generated parser

int name_map(const char *symb, struct ParserState* ps);

int symbol_image(const char *item, const struct ParserState* ps);

void alias_map(const char *stringptr, int image, struct ParserState* ps);

void build_symno(struct ParserState* ps);

void assign_symbol_no(const char *string_ptr, int image, struct ParserState* ps);

int accept_image;
int eoft_image;
int eolt_image;
int empty;
int error_image;

#define SYM1 (ps->terminal[ps->stack_top + 1])
#define SYM2 (ps->terminal[ps->stack_top + 2])
#define SYM3 (ps->terminal[ps->stack_top + 3])

enum {
    NT_OFFSET = 19,
    BUFF_UBOUND = 30,
    BUFF_SIZE = 31,
    STACK_UBOUND = 20,
    STACK_SIZE = 21,
    LA_STATE_OFFSET = 392,
    MAX_LA = 1,
    NUM_RULES = 141,
    NUM_TERMINALS = 19,
    NUM_NON_TERMINALS = 38,
    NUM_SYMBOLS = 57,
    START_STATE = 144,
    EOFT_SYMBOL = 19,
    EOLT_SYMBOL = 20,
    ACCEPT_ACTION = 250,
    ERROR_ACTION = 251
  };

enum {
    DEFINE_KEY_TK = 5,
    TERMINALS_KEY_TK = 9,
    ALIAS_KEY_TK = 10,
    START_KEY_TK = 11,
    RULES_KEY_TK = 12,
    NAMES_KEY_TK = 16,
    END_KEY_TK = 18,
    EQUIVALENCE_TK = 1,
    ARROW_TK = 2,
    OR_TK = 6,
    EMPTY_SYMBOL_TK = 7,
    ERROR_SYMBOL_TK = 8,
    EOL_SYMBOL_TK = 13,
    EOF_SYMBOL_TK = 14,
    MACRO_NAME_TK = 15,
    SYMBOL_TK = 3,
    BLOCK_TK = 4,
    HBLOCK_TK = 17,
    EOF_TK = 19
  };

#define nt_action(state, sym) base_action[state + sym]

#define t_action(state, sym, next_tok) \
  term_action[term_check[base_action[state]+sym] == sym ? \
  base_action[state] + sym : base_action[state]]

const unsigned char rhs[] = {
  0,
  7, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  2, 1, 1, 1, 1, 1, 1, 2, 3, 1, 2, 3, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1,
  1, 3, 2, 3, 2, 2, 2, 2, 1, 1, 1, 1, 3, 3, 3,
  3, 1, 1, 1, 1, 1, 1, 1, 2, 3, 3, 3, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0,
  1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 2, 0, 2,
  0, 2, 0, 2, 0, 2
};

const unsigned short lhs[] = {
  0,
  9, 9, 17, 17, 17, 17, 17, 17, 17, 17, 18, 18, 19, 19, 5,
  5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6,
  20, 22, 22, 22, 22, 22, 22, 23, 25, 25, 25, 25, 26, 26, 26,
  26, 27, 27, 27, 27, 27, 27, 27, 3, 3, 3, 3, 28, 28, 28,
  28, 29, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 32, 32, 1,
  1, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33,
  33, 7, 7, 2, 2, 2, 2, 2, 35, 37, 37, 37, 4, 4, 4,
  4, 4, 4, 4, 8, 8, 8, 8, 8, 8, 8, 8, 10, 10, 11,
  11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 21, 21, 30, 30,
  24, 24, 36, 36, 34, 34,
  54, 60, 129, 58, 175, 1, 109, 69, 53, 249,
  42, 146, 188, 247, 171, 109, 145, 20, 101, 2,
  119, 172, 81, 251, 87, 137, 210, 251, 40, 251,
  35, 27, 29, 251, 39, 191, 13, 35, 27, 29,
  18, 109, 139, 14, 100, 199, 193, 170, 102, 161,
  20, 186, 34, 203, 179, 218, 221, 189, 195, 187,
  50, 99, 67, 49, 80, 204, 121, 202, 196, 149,
  123, 41, 97, 133, 125, 207, 111, 112, 231, 48,
  251, 1, 251, 235, 193, 127, 141, 251, 142, 94,
  91, 251, 135, 124, 89, 129, 138, 90, 141, 79,
  154, 88, 203, 94, 77, 94, 156, 94, 182, 251,
  251, 251, 251, 147
};

const unsigned short *base_action = lhs;


const unsigned char term_check[] = {
  0,
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
  15, 16, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
  13, 14, 15, 16, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  11, 12, 0, 0, 0, 3, 4, 18, 0, 1, 2, 3, 4, 5, 6,
  7, 8, 9, 10, 17, 16, 13, 14, 0, 1, 2, 3, 4, 5, 6,
  7, 8, 9, 10, 0, 0, 13, 14, 3, 4, 5, 6, 7, 8, 9,
  10, 11, 12, 0, 1, 2, 0, 17, 0, 1, 2, 3, 4, 5, 6,
  7, 8, 9, 10, 11, 0, 0, 0, 3, 4, 5, 6, 7, 8, 9,
  10, 11, 12, 0, 1, 2, 0, 17, 0, 1, 2, 3, 4, 5, 6,
  7, 8, 0, 1, 2, 0, 1, 2, 15, 0, 1, 2, 3, 4, 5,
  6, 7, 8, 0, 1, 2, 0, 0, 0, 15, 0, 1, 2, 3, 4,
  5, 6, 7, 8, 0, 0, 1, 2, 3, 0, 15, 6, 7, 8, 10,
  0, 0, 0, 13, 14, 0, 1, 2, 3, 4, 5, 6, 0, 19, 9,
  0, 1, 2, 0, 4, 5, 9, 0, 0, 9, 10, 0, 0, 0, 11,
  0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 18, 0
};

const unsigned short term_action[] = {
  0,
  99, 326, 327, 354, 367, 361, 359, 355, 356, 362,
  363, 364, 365, 357, 358, 368, 366, 251, 326, 327,
  354, 367, 361, 359, 355, 356, 362, 363, 364, 365,
  357, 358, 368, 366, 251, 326, 327, 279, 274, 345,
  275, 276, 277, 346, 347, 348, 349, 251, 132, 128,
  245, 343, 281, 38, 326, 327, 294, 308, 305, 312,
  310, 295, 306, 307, 344, 219, 296, 297, 251, 326,
  327, 298, 308, 305, 303, 302, 299, 306, 307, 136,
  73, 300, 301, 140, 140, 345, 335, 336, 337, 346,
  347, 348, 349, 251, 326, 327, 134, 140, 62, 326,
  327, 314, 319, 320, 315, 316, 317, 321, 322, 323,
  74, 138, 251, 240, 343, 345, 242, 238, 333, 346,
  347, 348, 349, 81, 326, 327, 251, 344, 118, 254,
  255, 260, 261, 158, 256, 257, 258, 80, 326, 327,
  83, 326, 327, 259, 11, 326, 327, 267, 272, 273,
  268, 269, 270, 78, 326, 327, 251, 251, 251, 266,
  12, 326, 327, 267, 272, 273, 268, 269, 270, 122,
  251, 326, 327, 354, 251, 266, 359, 355, 356, 206,
  251, 251, 251, 357, 358, 31, 326, 327, 283, 288,
  286, 284, 120, 250, 287, 251, 326, 327, 124, 308,
  305, 205, 126, 130, 306, 307, 251, 251, 251, 214,
  251, 251, 251, 251, 164, 251, 251, 251, 251, 251,
  251, 382
};

static void null_action(struct ParserState* ps)
{
}

static void add_macro_definition(const char *name, const struct terminal_type *term, struct ParserState* ps)
{
    if (ps->num_defs >= (int)ps->defelmt_size)
    {
        ps->defelmt_size += DEFELMT_INCREMENT;
        ps->defelmt = (struct defelmt_type *)
            (ps->defelmt == (struct defelmt_type *) NULL
             ? malloc(ps->defelmt_size * sizeof(struct defelmt_type))
             : realloc(ps->defelmt, ps->defelmt_size * sizeof(struct defelmt_type)));
        if (ps->defelmt == (struct defelmt_type *) NULL)
            nospace();
    }

    ps->defelmt[ps->num_defs].length       = term->length;
    ps->defelmt[ps->num_defs].start_line   = term->start_line;
    ps->defelmt[ps->num_defs].start_column = term->start_column;
    ps->defelmt[ps->num_defs].end_line     = term->end_line;
    ps->defelmt[ps->num_defs].end_column   = term->end_column;
    strcpy(ps->defelmt[ps->num_defs].name, name);
    ps->num_defs++;
}

static void add_block_definition(const struct terminal_type *term, struct ParserState* ps)
{
    if (ps->num_acts >= (int) ps->actelmt_size)
    {
        ps->actelmt_size += ACTELMT_INCREMENT;
        ps->actelmt = (struct actelmt_type *)
            (ps->actelmt == (struct actelmt_type *) NULL
             ? malloc(ps->actelmt_size * sizeof(struct actelmt_type))
             : realloc(ps->actelmt, ps->actelmt_size * sizeof(struct actelmt_type)));
        if (ps->actelmt == (struct actelmt_type *) NULL)
            nospace();
    }

    ps->actelmt[ps->num_acts].rule_number  = ps->num_rules;
    ps->actelmt[ps->num_acts].start_line   = term->start_line;
    ps->actelmt[ps->num_acts].start_column = term->start_column;
    ps->actelmt[ps->num_acts].end_line     = term->end_line;
    ps->actelmt[ps->num_acts].end_column   = term->end_column;
    ps->actelmt[ps->num_acts].header_block = term->kind == HBLOCK_TK;
    ps->num_acts++;
}

/// bad_symbol ::= EQUIVALENCE
static void bad_first_symbol(struct ParserState* ps)
{
    PRNTERR2("First symbol: \"%s\" found in file is illegal. Line %ld, column %d", SYM1.name, SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_symbol ::= BLOCK
static void act10(struct ParserState* ps)
{
    PRNTERR2("Action block cannot be first object in file. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_list ::= macro_name_symbol macro_block
static void act13(struct ParserState* ps)
{
    add_macro_definition(SYM1.name, &(SYM2), ps);
}

/// macro_list ::= macro_list macro_name_symbol macro_block
static void act14(struct ParserState* ps)
{
    add_macro_definition(SYM2.name, &(SYM3), ps);
}

/// macro_name_symbol ::= SYMBOL
static void act16(struct ParserState* ps)
{
    PRNTWNG2("Macro name \"%s\" does not start with the escape character. Line %ld, column %d", SYM1.name, SYM1.start_line, SYM1.start_column);
}

/// macro_name_symbol ::= OR
static void bad_macro_name(struct ParserState* ps)
{
    PRNTERR2("Reserved symbol cannot be used as macro name. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_name_symbol ::= BLOCK
static void act21(struct ParserState* ps)
{
    PRNTERR2("Macro name not supplied for macro definition. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_name_symbol ::= DEFINE_KEY
static void act22(struct ParserState* ps)
{
    PRNTERR2("Macro keyword misplaced. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_block ::= OR
static void definition_expected(struct ParserState* ps)
{
    PRNTERR2("Definition block expected where symbol found. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// terminal_symbol ::= SYMBOL
static void process_terminal(struct ParserState* ps)
{
    assign_symbol_no(SYM1.name, OMEGA, ps);
}

/// terminal_symbol ::= DEFINE_KEY
static void bad_terminal(struct ParserState* ps)
{
    PRNTERR2("Keyword  has been misplaced in Terminal section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// terminal_symbol ::= BLOCK
static void act37(struct ParserState* ps)
{
    PRNTERR2("Misplaced block found in TERMINALS section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// alias_definition ::= alias_lhs produces alias_rhs
static void act39(struct ParserState* ps)
{
    int image;
    char tok_string[SYMBOL_SIZE + 1];

    switch(SYM3.kind)
    {
        case EMPTY_SYMBOL_TK:
            image = empty;
            break;

        case SYMBOL_TK:
            assign_symbol_no(SYM3.name, OMEGA, ps);
            image = symbol_image(SYM3.name, ps);
            break;

        case ERROR_SYMBOL_TK:
            if (error_image > ps->num_terminals)
            {
                restore_symbol(tok_string, kerror, ps->ormark, ps->escape);
                PRNTERR2("Illegal aliasing to %s prior to its definition.  Line %ld, column %d", tok_string, SYM3.start_line, SYM3.start_column);
                exit(12);
            }
            image = error_image;
            break;

        case EOF_SYMBOL_TK:
            if (eoft_image > ps->num_terminals)
            {
                restore_symbol(tok_string, keoft, ps->ormark, ps->escape);
                PRNTERR2("Illegal aliasing to %s prior to its definition. Line %ld, column %d", tok_string, SYM3.start_line, SYM3.start_column);
                exit(12);
            }
            image = eoft_image;
            break;

        case EOL_SYMBOL_TK:
            if (eolt_image == OMEGA)
            {
                PRNTERR2("Illegal aliasing to EOL prior to its definition. Line %ld, column %d", SYM3.start_line, SYM3.start_column);
                exit(12);
            }
            image = eolt_image;
            break;

        default: /* if SYM3.kind == symbol */
            image = symbol_image(SYM3.name, ps);
            break;
    }

    switch(SYM1.kind)
    {
        case SYMBOL_TK:
            if (symbol_image(SYM1.name, ps) != OMEGA)
            {
                restore_symbol(tok_string, SYM1.name, ps->ormark, ps->escape);
                PRNTERR2("Symbol %s was previously defined. Line %ld, column %d", tok_string, SYM1.start_line, SYM1.start_column);
                exit(12);
            }
            assign_symbol_no(SYM1.name, image, ps);
            break;

        case ERROR_SYMBOL_TK:
            if (error_image > ps->num_terminals || ! ps->error_maps_bit)
            {
                if (image == empty      || image == eolt_image ||
                    image == eoft_image || image > ps->num_terminals)
                {
                    restore_symbol(tok_string, kerror, ps->ormark, ps->escape);
                    PRNTERR2("Illegal alias for symbol %s. Line %ld, column %d.", tok_string, SYM1.start_line, SYM1.start_column);
                    exit(12);
                }
                alias_map(kerror, image, ps);
                error_image = image;
            }
            else
            {
                restore_symbol(tok_string, kerror, ps->ormark, ps->escape);
                PRNTERR2("Symbol %s was previously defined. Line %ld, column %d", tok_string, SYM1.start_line, SYM1.start_column);
                exit(12);
            }
            break;

        case EOF_SYMBOL_TK:
            if (eoft_image > ps->num_terminals)
            {
                if (image == empty       || image == eolt_image  ||
                    image == error_image || image > ps->num_terminals)
                {
                    restore_symbol(tok_string, keoft, ps->ormark, ps->escape);
                    PRNTERR2("Illegal alias for symbol %s. Line %ld, column %d.", tok_string, SYM1.start_line, SYM1.start_column);
                    exit(12);
                }
                alias_map(keoft, image, ps);
                eoft_image = image;
            }
            else
            {
                restore_symbol(tok_string, keoft, ps->ormark, ps->escape);
                PRNTERR2("Symbol %s was previously defined.  %ld, column %d", tok_string, SYM1.start_line, SYM1.start_column);
                exit(12);
            }
            break;

        default: /* if SYM1.kind == EOL_SYMBOL */
            if (eolt_image == OMEGA)
            {
                if (image == empty ||
                    image == eoft_image ||
                    image == error_image ||
                    image > ps->num_terminals)
                {
                    PRNTERR2("Illegal alias for symbol EOL. Line %ld, column %d.", SYM1.start_line, SYM1.start_column);
                    exit(12);
                }
                eolt_image = image;
            }
            else
            {
                PRNTERR2("Symbol EOL was previously defined. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
                exit(12);
            }
            break;
    }
}

/// bad_alias_rhs ::= DEFINE_KEY
static void bad_alias_rhs(struct ParserState* ps)
{
    PRNTERR2("Misplaced keyword found in Alias section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_alias_rhs ::= BLOCK
static void act57(struct ParserState* ps)
{
    PRNTERR2("Misplaced block found in Alias section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_alias_lhs ::= EMPTY_SYMBOL
static void act59(struct ParserState* ps)
{
    PRNTERR2("Empty symbol cannot be aliased. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_alias_lhs ::= produces
static void missing_quote(struct ParserState* ps)
{
    PRNTERR2("Symbol must be quoted when used as a grammar symbol. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// start_symbol ::= SYMBOL
static void act63(struct ParserState* ps)
{
    assign_symbol_no(SYM1.name, OMEGA, ps);
    struct node *q = Allocate_node();
    q -> value = symbol_image(SYM1.name, ps);
    if (ps->start_symbol_root == NULL) {
      q -> next = q;
    } else {
        q -> next = ps->start_symbol_root -> next;
        ps->start_symbol_root -> next = q;
    }
    ps->start_symbol_root = q;
    ps->num_rules++;
    ps->num_items++;
}

/// start_symbol ::= OR
static void bad_start_symbol(struct ParserState* ps)
{
    PRNTERR2("Symbol cannot be used as Start symbol. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// start_symbol ::= BLOCK
static void act68(struct ParserState* ps)
{
    PRNTERR2("Misplaced block found in Start section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// start_symbol ::= DEFINE_KEY
static void misplaced_keyword_found_in_START_section(struct ParserState* ps)
{
    PRNTERR2("Misplaced keyword found in START section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// rules_block ::= RULES_KEY
static void act73(struct ParserState* ps)
{

    if (ps->start_symbol_root == NULL)
    {
        struct node *q = Allocate_node();
        q -> value = empty;
        q -> next = q;
        ps->start_symbol_root = q;
        ps->num_rules = 0;                 // One rule
        ps->num_items = 0;                 // 0 items
    }
    build_symno(ps);
}

/// rules_block ::= RULES_KEY rule_list
static void act74(struct ParserState* ps)
{
    build_symno(ps);
}

/// rule_list ::= {action_block} SYMBOL produces
static void act77(struct ParserState* ps)
{
    assign_symbol_no(SYM2.name, OMEGA, ps);
    if (ps->start_symbol_root == NULL)
    {
        struct node *q = Allocate_node();
        q -> value = symbol_image(SYM2.name, ps);
        q -> next = q;
        ps->start_symbol_root = q;
        ps->num_rules = 1;
        ps->num_items = 1;
    }

/// Since we don't know for sure how many start symbols we have, a
/// "while" loop is used to increment the size of rulehdr. However,
/// it is highly unlikely that this loop would ever execute more than
/// once if the size of RULE_INCREMENT is reasonable.
    while (ps->num_rules >= (int)ps->rulehdr_size)
    {
        ps->rulehdr_size += RULEHDR_INCREMENT;
        ps->rulehdr = (struct rulehdr_type *)
            (ps->rulehdr == (struct rulehdr_type *) NULL
             ? malloc(ps->rulehdr_size * sizeof(struct rulehdr_type))
             : realloc(ps->rulehdr, ps->rulehdr_size * sizeof(struct rulehdr_type)));
        if (ps->rulehdr == (struct rulehdr_type *) NULL)
            nospace();
    }
    ps->rulehdr[ps->num_rules].sp = ((SYM3.kind == ARROW_TK) ? true : false);
    ps->rulehdr[ps->num_rules].lhs = symbol_image(SYM2.name, ps);
    ps->rulehdr[ps->num_rules].rhs_root = NULL;
}

/// rule_list ::= rule_list OR
static void act78(struct ParserState* ps)
{
    ps->num_rules++;
    if (ps->num_rules >= (int)ps->rulehdr_size)
    {
        ps->rulehdr_size += RULEHDR_INCREMENT;
        ps->rulehdr = (struct rulehdr_type *)
            (ps->rulehdr == (struct rulehdr_type *) NULL
             ? malloc(ps->rulehdr_size * sizeof(struct rulehdr_type))
             : realloc(ps->rulehdr, ps->rulehdr_size * sizeof(struct rulehdr_type)));
        if (ps->rulehdr == (struct rulehdr_type *) NULL)
            nospace();
    }
    ps->rulehdr[ps->num_rules].sp = ps->rulehdr[ps->num_rules - 1].sp;
    ps->rulehdr[ps->num_rules].lhs = OMEGA;
    ps->rulehdr[ps->num_rules].rhs_root = NULL;
}

/// rule_list ::= rule_list SYMBOL produces
static void act79(struct ParserState* ps)
{
    ps->num_rules++;
    if (ps->num_rules >= (int)ps->rulehdr_size)
    {
        ps->rulehdr_size += RULEHDR_INCREMENT;
        ps->rulehdr = (struct rulehdr_type *)
            (ps->rulehdr == (struct rulehdr_type *) NULL
             ? malloc(ps->rulehdr_size * sizeof(struct rulehdr_type))
             : realloc(ps->rulehdr, ps->rulehdr_size * sizeof(struct rulehdr_type)));
        if (ps->rulehdr == (struct rulehdr_type *) NULL)
            nospace();
    }
    ps->rulehdr[ps->num_rules].sp = ((SYM3.kind == ARROW_TK) ? true : false);
    assign_symbol_no(SYM2.name, OMEGA, ps);
    ps->rulehdr[ps->num_rules].lhs = symbol_image(SYM2.name, ps);
    ps->rulehdr[ps->num_rules].rhs_root = NULL;
}

/// rule_list ::= rule_list ERROR_SYMBOL
static void act82(struct ParserState* ps)
{
    if (error_image == DEFAULT_SYMBOL)
    {
        char tok_string[SYMBOL_SIZE + 1];
        restore_symbol(tok_string, kerror, ps->ormark, ps->escape);
        PRNTERR2("%s not declared or aliased to terminal symbol. Line %ld, column %d", tok_string, SYM2.start_line, SYM2.start_column);
        exit(12);
    }
    struct node *q = Allocate_node();
    q -> value = error_image;
    ps->num_items++;
    if (ps->rulehdr[ps->num_rules].rhs_root == NULL)
        q -> next = q;
    else
    {
        q -> next = ps->rulehdr[ps->num_rules].rhs_root -> next;
         ps->rulehdr[ps->num_rules].rhs_root -> next = q;
    }
    ps->rulehdr[ps->num_rules].rhs_root = q;
}

/// rule_list ::= rule_list SYMBOL
static void act83(struct ParserState* ps)
{
    assign_symbol_no(SYM2.name, OMEGA, ps);
    int sym = symbol_image(SYM2.name, ps);
    if (sym != empty)
    {
        if (sym == eoft_image)
        {
            PRNTERR2("End-of-file symbol cannot be used in rule. Line %ld, column %d", SYM2.start_line, SYM2.start_column);
            exit(12);
        }
        struct node *q = Allocate_node();
        q -> value = sym;
        ps->num_items++;
        if (ps->rulehdr[ps->num_rules].rhs_root == NULL)
            q -> next = q;
        else
        {
            q -> next = ps->rulehdr[ps->num_rules].rhs_root -> next;
            ps->rulehdr[ps->num_rules].rhs_root -> next = q;
        }
        ps->rulehdr[ps->num_rules].rhs_root = q;
    }
}

/// rule_list ::= OR
static void bad_first_symbol_in_RULES_section(struct ParserState* ps)
{
    PRNTERR2("First symbol in Rules section is not a valid left-hand side.\n Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// rule_list ::= rule_list OR produces
static void rule_without_left_hand_side(struct ParserState* ps)
{
    PRNTERR2("Rule without left-hand-side.  Line %ld, column %d", SYM3.start_line, SYM3.start_column);
    exit(12);
}

/// rule_list ::= rule_list keyword produces
static void act91(struct ParserState* ps)
{
    PRNTWNG2("Misplaced keyword found in Rules section Line %ld, column %d",  SYM2.start_line, SYM2.start_column);
    exit(12);
}

/// action_block ::= BLOCK
static void act92(struct ParserState* ps)
{
    add_block_definition(&(SYM1), ps);
}

/// action_block ::= HBLOCK
static void act93(struct ParserState* ps)
{
    add_block_definition(&(SYM1), ps);
}

/// keyword ::= DEFINE_KEY
static void misplaced_keyword_found_in_RULES_section(struct ParserState* ps)
{
    PRNTWNG2("Misplaced keyword found in RULES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// names_definition ::= name produces name
static void act100(struct ParserState* ps)
{
    if (ps->error_maps_bit)
    {
        int symbol;

        switch(SYM1.kind)
        {
            case EMPTY_SYMBOL_TK:
                symbol = empty;
                break;

            case ERROR_SYMBOL_TK:
                symbol = error_image;
                break;

            case EOL_SYMBOL_TK:
                symbol = eolt_image;
                break;

            case EOF_SYMBOL_TK:
                symbol = eoft_image;
                break;

            default:
                symbol = symbol_image(SYM1.name, ps);
                break;
        }

        if (symbol == OMEGA)
        {
            PRNTERR2("Symbol %s is undefined. Line %ld, column %d", SYM1.name, SYM1.start_line, SYM1.start_column);
            exit(12);
        }

        if (ps->symno[symbol].name_index != OMEGA)
        {
            PRNTERR2("Symbol %s has been named more than once. Line %ld, column %d.", SYM1.name, SYM1.start_line, SYM1.start_column);
            exit(12);
        }
         ps->symno[symbol].name_index = name_map(SYM3.name, ps);
     }
}

/// bad_name ::= DEFINE_KEY
static void misplaced_keyword_found_in_NAMES_section(struct ParserState* ps)
{
    PRNTERR2("Keyword  has been misplaced in NAMES section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_name ::= BLOCK
static void act116(struct ParserState* ps)
{
    PRNTERR2("Misplaced action block found in NAMES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_name ::= MACRO_NAME
static void act117(struct ParserState* ps)
{
    PRNTERR2("Misplaced macro name found in NAMES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// [terminals_block] ::=
static void process_TERMINALS_section(struct ParserState* ps)
{
    ps->num_terminals = ps->num_symbols;
    assign_symbol_no(keoft, OMEGA, ps);
    eoft_image = symbol_image(keoft, ps);
    if (ps->error_maps_bit) {
        assign_symbol_no(kerror, OMEGA, ps);
        error_image = symbol_image(kerror, ps);
    } else {
      error_image = DEFAULT_SYMBOL;   // should be 0
    }
    assign_symbol_no(kaccept, OMEGA, ps);
    accept_image = symbol_image(kaccept, ps);
}

/// [alias_block] ::=
static void process_ALIAS_section(struct ParserState* ps)
{
    int k = 0;
    if (eoft_image <= ps->num_terminals) {
        k++;
    } else {
        ps->num_terminals++;
    }
    if (ps->error_maps_bit) {
        if (error_image <= ps->num_terminals) {
            k++;
        } else {
            ps->num_terminals++;
            if (k == 1) {
                error_image--;
            }
        }
    }

    if (k > 0) {
        for (int i = 0; i < HT_SIZE; i++) {
            struct hash_type* p = ps->hash_table[i];
            while(p != NULL)
            {
                if (p -> number > ps->num_terminals)
                    p -> number -= k;
                else if (p -> number < -ps->num_terminals)
                    p -> number += k;
                p = p -> link;
            }
        }
        ps->num_symbols -= k;
        accept_image -= k;
    }
    if (eolt_image == OMEGA)
        eolt_image = eoft_image;
    if (error_image == DEFAULT_SYMBOL)
        alias_map(kerror, DEFAULT_SYMBOL, ps);
}

/// {terminal_symbol} ::=
static void act132(struct ParserState* ps)
{
    assign_symbol_no(kempty, OMEGA, ps);
    empty = symbol_image(kempty, ps);
}

/// BUILD_SYMNO constructs the SYMNO table which is a mapping from each
/// symbol number into that symbol.
void build_symno(struct ParserState* ps) {
  const long symno_size = ps->num_symbols + 1;
  calloc0p(&ps->symno, symno_size, struct symno_type);
  // Go through entire hash table. For each non_empty bucket, go through
  // linked list in that bucket.
  for (int i = 0; i < HT_SIZE; ++i) {
    for (const struct hash_type *p = ps->hash_table[i]; p != NULL; p = p->link) {
      const long symbol = p->number;
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
// endregion





// region manual parser
struct line_elemt {
  struct line_elemt *link;
  char line[MAX_LINE_SIZE + 1];
};

struct LinePool {
  struct line_elemt *line_pool_root;
};

char *RETRIEVE_STRING(const int indx, char *string_table, const struct symno_type *symno) {
  return &string_table[symno[indx].ptr];
}

char *RETRIEVE_NAME(const int indx, char *string_table, const int *name) {
  return &string_table[name[indx]];
}

static const int OUTPUT_PARM_SIZE = MAX_PARM_SIZE + 7;
static const int MAXIMUM_LA_LEVEL = 100;
static const int STRING_BUFFER_SIZE = 8192;

/// READ_INPUT fills the buffer from p1 to the end.
static void read_input(char *grm_file, FILE *sysgrm, struct ScannerState* ss) {
  unsigned long num_read = ss->input_buffer + IOBUFFER_SIZE - ss->bufend;
  if ((num_read = fread(ss->bufend, 1, num_read, sysgrm)) == 0) {
    if (ferror(sysgrm) != 0) {
      fprintf(stderr, "*** Error reading input file \"%s\".\n", grm_file);
      exit(12);
    }
  }
  ss->bufend += num_read;
  *ss->bufend = '\0';
}

/// VERIFY takes as argument a character string and checks whether each
/// character is a digit. If all are digits, then 1 is returned; if not, then
/// 0 is returned.
static bool verify_is_digit(const char *item) {
  while (isdigit(*item)) {
    item++;
  }
  return *item == '\0';
}

/// TRANSLATE takes as arguments a character array, which it folds to upper
/// to uppercase and returns.
static char *translate(char *str, const int len) {
  for (int i = 0; i < len; i++) {
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
static bool strxeq(char *s1, char *s2) {
  for (; *s2 != '\0'; s1++, s2++) {
    if (*s1 != *s2 && *s1 != toupper(*s2)) {
      return false;
    }
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
static void options(char *file_prefix, struct CLIOptions *cli_options, char *parm) {
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
  int i = 0;
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
    unsigned long token_len = strlen(token);
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
      } else if (memcmp("GOTODEFAULT", token, token_len) == 0) {
        cli_options->goto_default_bit = flag;
      } else if (memcmp("HALFWORD", token, token_len) == 0) {
        cli_options->byte_bit = !flag;
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
      } else {
        PRNTERR2("\"%s\" is an invalid option", temp);
      }
    } else {
      // We now process the valued-parameter. Pick value after "=" and process
      i++;
      if (isspace(parm[i]) || parm[i] == '\0') {
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
      } else if (memcmp("DEFAULT", token, token_len) == 0) {
        if (verify_is_digit(temp)) {
          switch (atoi(temp)) {
            case 0: cli_options->default_opt = OPT_0;
            case 1: cli_options->default_opt = OPT_1;
            case 2: cli_options->default_opt = OPT_2;
            case 3: cli_options->default_opt = OPT_3;
            case 4: cli_options->default_opt = OPT_4;
            case 5: cli_options->default_opt = OPT_5;
            default:
              printf("\"%s\" is an invalid option", temp);
              exit(999);
          }
        } else {
          PRNTERR2("\"%s\" is an invalid value for %s", temp, token);
        }
      } else if (memcmp(token, "ESCAPE", token_len) == 0) {
        cli_options->escape = temp[0];
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
          PRNTERR2("\"%s\" is an invalid language for %s. Supported languages are C|CPP|JAVA.", temp, token);
          exit(999);
        }
      } else if (memcmp(token, "HACTFILENAME", token_len) == 0) {
        strcpy(cli_options->hact_file, temp);
      } else if (memcmp("LALR", token, token_len) == 0) {
        unsigned long token_len = strlen(temp);
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
      } else if (memcmp(token, "ORMARK", token_len) == 0) {
        cli_options->ormark = temp[0];
      } else if (memcmp(token, "PREFIX", token_len) == 0) {
        strcpy(cli_options->prefix, temp);
      } else if (memcmp(token, "STACKSIZE", token_len) == 0) {
        if (verify_is_digit(temp)) {
          cli_options->stack_size = atoi(temp);
        } else {
          PRNTERR2("\"%s\" is an invalid value for %s", temp, token);
        }
      } else if (memcmp(token, "SUFFIX", token_len) == 0) {
        strcpy(cli_options->suffix, temp);
      } else if (memcmp(token, "TABLE", token_len) == 0) {
        int token_len = strlen(temp);
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
/// if they are (it is an) "options" line(s). If so, the options are
/// processed. Then, we process user-supplied options if there are any.  In
/// any case, the options in effect are printed.
static void process_options_lines(char *grm_file, struct OutputFiles *of, char *file_prefix, struct CLIOptions *cli_options, FILE *sysgrm, struct ScannerState* ss, char *parm) {
  char old_parm[MAX_LINE_SIZE + 1];
  char output_line[PRINT_LINE_SIZE + 1];
  char opt_string[60][OUTPUT_PARM_SIZE + 1];
  int top = 0;
  strcpy(old_parm, parm); /* Save new options passed to program */
  static char ooptions[9] = " OPTIONS";
  ooptions[0] = cli_options->escape; /* "ooptions" always uses default escape symbol */
  // Until end-of-file is reached, process
  while (ss->p1 != NULL) {
    // all comment and %options lines.
    while (isspace(*ss->p2)) {
      // skip all space symbols
      if (*ss->p2 == '\n') {
        ss->line_no++;
        ss->linestart = ss->p2;
        ss->p1 = ss->p2 + 1;
      }
      ss->p2++;
    }
    char *line_end = strchr(ss->p2, '\n'); /* find end-of-line */
    // First, check if line is a comment line. If so, skip it.  Next,
    // check if line is an options line. If so, process it. Otherwise,
    // break out of the loop.
    // Note that no length check is necessary before checking for "--"
    // or "%options" since the buffer is always extended by
    // MAX_LINE_SIZE elements past its required length. (see read_input)
    if (*ss->p2 == '-' && *(ss->p2 + 1) == '-') {
      // Skip comment line.
    } else if (memcmp(ooptions, translate(ss->p2, 8), 8) == 0) {
      *line_end = '\0';
      PRNT(ss->p2); /* Print options line */
      strcpy(parm, ss->p2 + strlen(ooptions));
      options(file_prefix, cli_options, parm); /* Process hard-coded options */
    } else {
      ss->p2 = ss->p1; /* make p2 point to first character */
      break;
    }
    // If the line was a comment or an option line, check the following
    // line.  If we are at the end of the buffer, read in more data...
    ss->p1 = line_end + 1;
    if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
      int i = ss->bufend - ss->p1;
      if (i < MAX_LINE_SIZE) {
        strcpy(ss->input_buffer, ss->p1);
        ss->bufend = &ss->input_buffer[i];
        read_input(grm_file, sysgrm, ss);
        ss->p1 = &ss->input_buffer[0];
      }
    }
    ss->line_no++;
    ss->linestart = ss->p1 - 1;
    ss->p2 = ss->p1;
  }
  printf("\n");
  strcpy(parm, old_parm);
  options(file_prefix, cli_options, parm); /* Process new options passed directly to program */
  if (cli_options->act_file[0] == '\0') {
    sprintf(cli_options->act_file, "%sact.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  }
  if (cli_options->hact_file[0] == '\0') {
    sprintf(cli_options->hact_file, "%shdr.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  }
  sprintf(of->sym_file, "%ssym.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  sprintf(of->def_file, "%sdef.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  sprintf(of->prs_file, "%sprs.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  sprintf(of->dcl_file, "%sdcl.%s", file_prefix, cli_options->java_bit ? "java" : "h");
  //                          PRINT OPTIONS:
  // Here we print all options set by the user. As of now, only about 48
  // different options and related aliases are allowed. In case that number
  // goes up, the bound of the array, opt_string, should be changed.
  // BLOCKB, BLOCKE, HBLOCKB and HBLOCKE can generate the longest strings
  // since their value can be up to MAX_PARM_SIZE characters long.
  sprintf(opt_string[++top], "ACTFILENAME=%s", cli_options->act_file);
  strcpy(opt_string[++top], cli_options->byte_bit ? "BYTE" : "NOBYTE");
  strcpy(opt_string[++top], cli_options->conflicts_bit ? "CONFLICTS" : "NOCONFLICTS");
  if (cli_options->default_opt.value == OPT_0.value) strcpy(opt_string[++top], "NODEFAULT");
  else if (cli_options->default_opt.value == OPT_1.value) printf("DEFAULT=1");
  else if (cli_options->default_opt.value == OPT_2.value) printf("DEFAULT=2");
  else if (cli_options->default_opt.value == OPT_3.value) printf("DEFAULT=3");
  else if (cli_options->default_opt.value == OPT_4.value) printf("DEFAULT=4");
  else if (cli_options->default_opt.value == OPT_5.value) printf("DEFAULT=5");
  sprintf(opt_string[++top], "ESCAPE=%c", cli_options->escape);
  sprintf(opt_string[++top], "FILEPREFIX=%s", file_prefix);
  if (cli_options->c_bit) {
    sprintf(opt_string[++top], "GENERATEPARSER=C");
  } else if (cli_options->cpp_bit) {
    sprintf(opt_string[++top], "GENERATEPARSER=CPP");
  } else if (cli_options->java_bit) {
    sprintf(opt_string[++top], "GENERATEPARSER=JAVA");
  } else {
    strcpy(opt_string[++top], "NOGENERATEPARSER");
  }
  strcpy(opt_string[++top], cli_options->goto_default_bit ? "GOTODEFAULT" : "NOGOTODEFAULT");
  sprintf(opt_string[++top], "HACTFILENAME=%s", cli_options->hact_file);
  sprintf(opt_string[++top], "LALR=%d", cli_options->lalr_level);
  strcpy(opt_string[++top], cli_options->nt_check_bit ? "NTCHECK" : "NONTCHECK");
  sprintf(opt_string[++top], "ORMARK=%c", cli_options->ormark);
  sprintf(opt_string[++top], "PREFIX=%s", cli_options->prefix);
  strcpy(opt_string[++top], cli_options->read_reduce_bit ? "READREDUCE" : "NOREADREDUCE");
  strcpy(opt_string[++top], cli_options->scopes_bit ? "SCOPES" : "NOSCOPES");
  strcpy(opt_string[++top], cli_options->shift_default_bit ? "SHIFTDEFAULT" : "NOSHIFT-DEFAULT");
  strcpy(opt_string[++top], cli_options->single_productions_bit ? "SINGLEPRODUCTIONS" : "NOSINGLE-PRODUCTIONS");
  sprintf(opt_string[++top], "STACKSIZE=%d", cli_options->stack_size);
  sprintf(opt_string[++top], "SUFFIX=%s", cli_options->suffix);
  if (cli_options->table_opt.value == OPTIMIZE_NO_TABLE.value) {
    strcpy(opt_string[++top], "NOTABLE");
  } else if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
    strcpy(opt_string[++top], "TABLE=SPACE");
  } else if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    strcpy(opt_string[++top], "TABLE=TIME");
  } else {
    PRNT("Unsupported table optimization option.");
    exit(12);
  }
  if (cli_options->trace_opt.value == NOTRACE.value) {
    strcpy(opt_string[++top], "NOTRACE");
  } else if (cli_options->trace_opt.value == TRACE_CONFLICTS.value) {
    strcpy(opt_string[++top], "TRACE=CONFLICTS");
  } else if (cli_options->trace_opt.value == TRACE_FULL.value) {
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
  if (cli_options->table_opt.value == OPTIMIZE_SPACE.value) {
    if (cli_options->default_opt.value < OPT_4.value) {
      PRNTWNG("DEFAULT_OPTION requested must be >= 4 if optimizing for space");
    }
  }
  if (cli_options->table_opt.value == OPTIMIZE_TIME.value) {
    if (cli_options->shift_default_bit) {
      PRNTWNG("SHIFT-DEFAULT option is only valid for Space tables");
    }
  }
  // Check if there are any conflicts in the options.
  if (cli_options->ormark == cli_options->escape) {
    PRNTERR("The options ormark and escape cannot have the same value");
    PRNT2("Input process aborted at line %d ...", ss->line_no);
    exit(12);
  }
}

/// HASH takes as argument a symbol and hashes it into a location in
/// HASH_TABLE.
static int hash(const char *symbl) {
  unsigned long hash_value = 0;
  for (; *symbl != '\0'; symbl++) {
    const unsigned short k = *symbl;
    symbl++;
    hash_value += (k << 7) + *symbl;
    if (*symbl == '\0') {
      break;
    }
  }
  return hash_value % HT_SIZE;
}

/// INSERT_STRING takes as an argument a pointer to a ht_elemt structure and
/// a character string. It inserts the string into the string table and sets
/// the value of node to the index into the string table.
static void insert_string(struct hash_type *q, const char *string, long* string_offset, char **string_table) {
  long string_size = 0;
  if (*string_offset + strlen(string) >= string_size) {
    string_size += STRING_BUFFER_SIZE;
    *string_table = (char *) (*string_table == (char *) NULL
       ? malloc(string_size * sizeof(char))
       : realloc(*string_table, string_size * sizeof(char)));
    if (*string_table == (char *) NULL) {
      nospace();
    }
  }
  q->st_ptr = *string_offset;
  // Copy until NULL is copied.
  while (((*string_table)[(*string_offset)++] = *string++)) {
  }
}

static bool EQUAL_STRING(const char *symb, const struct hash_type *p, const char *string_table) {
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
/// ASSIGN_SYMBOL_NO takes as arguments a pointer to a node and an image
/// number and assigns a symbol number to the symbol pointed to by the node.
void assign_symbol_no(const char *string_ptr, const int image, struct ParserState* ps) {
  struct hash_type *p;
  const int i = hash(string_ptr);
  for (p = ps->hash_table[i]; p != NULL; p = p->link) {
    if (EQUAL_STRING(string_ptr, p, ps->string_table)) /* Are they the same */
      return;
  }
  talloc0p(&p, struct hash_type);
  if (image == OMEGA) {
    ps->num_symbols++;
    p->number = ps->num_symbols;
  } else {
    p->number = -image;
  }
  p->name_index = OMEGA;
  insert_string(p, string_ptr, &ps->string_offset, &ps->string_table);
  p->link = ps->hash_table[i];
  ps->hash_table[i] = p;
}

/// ALIAS_MAP takes as input a symbol and an image. It searches the hash
/// table for stringptr and if it finds it, it turns it into an alias of the
/// symbol whose number is IMAGE. Otherwise, it invokes PROCESS_SYMBOL and
/// ASSIGN SYMBOL_NO to enter stringptr into the table and then we alias it.
void alias_map(const char *stringptr, const int image, struct ParserState* ps) {
  for (struct hash_type *q = ps->hash_table[hash(stringptr)]; q != NULL; q = q->link) {
    if (EQUAL_STRING(stringptr, q, ps->string_table)) {
      q->number = -image; /* Mark alias of image */
      return;
    }
  }
  assign_symbol_no(stringptr, image, ps);
}

/// SYMBOL_IMAGE takes as argument a symbol. It searches for that symbol
/// in the HASH_TABLE, and if found, it returns its image; otherwise, it
/// returns OMEGA.
int symbol_image(const char *item, const struct ParserState* ps) {
  for (const struct hash_type *q = ps->hash_table[hash(item)]; q != NULL; q = q->link) {
    if (EQUAL_STRING(item, q, ps->string_table)) {
      return ABS(q->number);
    }
  }
  return OMEGA;
}

/// NAME_MAP takes as input a symbol and inserts it into the HASH_TABLE if it
/// is not yet in the table. If it was already in the table then it is
/// assigned a NAME_INDEX number if it did not yet have one.  The name index
/// assigned is returned.
int name_map(const char *symb, struct ParserState* ps) {
  struct hash_type *p;
  const int i = hash(symb);
  for (p = ps->hash_table[i]; p != NULL; p = p->link) {
    if (EQUAL_STRING(symb, p, ps->string_table)) {
      if (p->name_index != OMEGA) {
        return p->name_index;
      } else {
        ps->num_names++;
        p->name_index = ps->num_names;
        return ps->num_names;
      }
    }
  }
  talloc0p(&p, struct hash_type);
  p->number = 0;
  insert_string(p, symb, &ps->string_offset, &ps->string_table);
  p->link = ps->hash_table[i];
  ps->hash_table[i] = p;
  ps->num_names++;
  p->name_index = ps->num_names;
  return ps->num_names;
}

/// SCANNER scans the input stream and returns the next input token.
static void scanner(char *grm_file, FILE *sysgrm, const struct CLIOptions* cli_options, struct ScannerState* ss, const struct ParserState* ps) {
  char tok_string[SYMBOL_SIZE + 1];
  char blockb[3] = {'/', '.'};
  char blocke[3] = {'.', '/'};
  char hblockb[3] = {'/', ':'};
  char hblocke[3] = {':', '/'};
  long blockb_len = strlen(blockb);
  long blocke_len = strlen(blocke);
  long hblockb_len = strlen(hblockb);
  long hblocke_len = strlen(hblocke);
scan_token:
  // Skip "blank" spaces.
  ss->p1 = ss->p2;
  while (isspace(*ss->p1)) {
    if (*ss->p1++ == '\n') {
      if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
        int i = ss->bufend - ss->p1;
        if (i < MAX_LINE_SIZE) {
          strcpy(ss->input_buffer, ss->p1);
          ss->bufend = &ss->input_buffer[i];
          read_input(grm_file, sysgrm, ss);
          ss->p1 = &ss->input_buffer[0];
        }
      }
      ss->line_no++;
      ss->linestart = ss->p1 - 1;
    }
  }
  if (strncmp(ss->p1, hblockb, hblockb_len) == 0) /* check block opener */ {
    ss->p1 = ss->p1 + hblockb_len;
    ss->ct_length = 0;
    ss->ct_ptr = ss->p1;
    if (*ss->p1 == '\n') {
      ss->ct_ptr++;
      ss->ct_length--;
      ss->ct_start_line = ss->line_no + 1;
      ss->ct_start_col = 1;
    } else {
      ss->ct_start_line = ss->line_no;
      ss->ct_start_col = ss->p1 - ss->linestart;
    }
    while (strncmp(ss->p1, hblocke, hblocke_len) != 0) {
      if (*ss->p1 == '\0') {
        PRNTERR2("End of file encountered while scanning header action block in rule %ld", ps->num_rules);
        exit(12);
      }
      if (*ss->p1++ == '\n') {
        if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
          int i = ss->bufend - ss->p1;
          if (i < MAX_LINE_SIZE) {
            strcpy(ss->input_buffer, ss->p1);
            ss->bufend = &ss->input_buffer[i];
            read_input(grm_file, sysgrm, ss);
            ss->p1 = &ss->input_buffer[0];
          }
        }
        ss->line_no++;
        ss->linestart = ss->p1 - 1;
      }
      ss->ct_length++;
    }
    ss->ct = HBLOCK_TK;
    ss->ct_end_line = ss->line_no;
    ss->ct_end_col = ss->p1 - ss->linestart - 1;
    ss->p2 = ss->p1 + hblocke_len;
    return;
  } else if (strncmp(ss->p1, blockb, blockb_len) == 0) /* check block  */ {
    ss->p1 = ss->p1 + blockb_len;
    ss->ct_length = 0;
    ss->ct_ptr = ss->p1;
    if (*ss->p1 == '\n') {
      ss->ct_ptr++;
      ss->ct_length--;
      ss->ct_start_line = ss->line_no + 1;
      ss->ct_start_col = 1;
    } else {
      ss->ct_start_line = ss->line_no;
      ss->ct_start_col = ss->p1 - ss->linestart;
    }
    while (strncmp(ss->p1, blocke, blocke_len) != 0) {
      if (*ss->p1 == '\0') {
        PRNTERR2("End of file encountered while scanning action block in rule %ld", ps->num_rules);
        exit(12);
      }
      if (*ss->p1++ == '\n') {
        if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
          long i = ss->bufend - ss->p1;
          if (i < MAX_LINE_SIZE) {
            strcpy(ss->input_buffer, ss->p1);
            ss->bufend = &ss->input_buffer[i];
            read_input(grm_file, sysgrm, ss);
            ss->p1 = &ss->input_buffer[0];
          }
        }
        ss->line_no++;
        ss->linestart = ss->p1 - 1;
      }
      ss->ct_length++;
    }
    ss->ct = BLOCK_TK;
    ss->ct_end_line = ss->line_no;
    ss->ct_end_col = ss->p1 - ss->linestart - 1;
    ss->p2 = ss->p1 + blocke_len;
    return;
  }
  // Scan the next token.
  ss->ct_ptr = ss->p1;
  ss->ct_start_line = ss->line_no;
  ss->ct_start_col = ss->p1 - ss->linestart;
  ss->p2 = ss->p1 + 1;
  switch (*ss->p1) {
    case '<':
      if (isalpha(*ss->p2)) {
        ss->p2++;
        while (*ss->p2 != '\n') {
          if (*ss->p2++ == '>') {
            ss->ct = SYMBOL_TK;
            ss->ct_length = ss->p2 - ss->p1;
            goto check_symbol_length;
          }
        }
        int i = SYMBOL_SIZE < ss->p2 - ss->p1 ? SYMBOL_SIZE : ss->p2 - ss->p1;
        memcpy(tok_string, ss->p1, i);
        tok_string[i] = '\0';
        PRNTERR2("Symbol \"%s\" has been referenced in line %ld without the closing \">\"", tok_string, ss->ct_start_line);
        exit(12);
      }
      break;
    case '\'':
      ss->ct_ptr = ss->p2;
      ss->ct = SYMBOL_TK;
      while (*ss->p2 != '\n') {
        if (*ss->p2 == '\'') {
          ss->p2++;
          if (*ss->p2 != '\'') {
            ss->ct_length = ss->p2 - ss->p1 - 2;
            goto remove_quotes;
          }
        }
        ss->p2++;
      }
      ss->ct_length = ss->p2 - ss->p1 - 1;
      memcpy(tok_string, ss->p1, ss->ct_length);
      tok_string[ss->ct_length] = '\0';
      PRNTWNG2("Symbol \"%s\" referenced in line %ld requires a closing quote", tok_string, ss->ct_start_line);
    remove_quotes:
      if (ss->ct_length == 0) /* Empty symbol? disregard it */
        goto scan_token;
      int i = 0;
      ss->p1 = ss->ct_ptr;
      do {
        *ss->p1++ = ss->ct_ptr[i++];
        if (ss->ct_ptr[i] == '\'') {
          i++; /* skip next quote */
        }
      } while (i < ss->ct_length);
      ss->ct_length = ss->p1 - ss->ct_ptr;
      goto check_symbol_length;
    case '-': /* scan possible comment  */
      if (*ss->p2 == '-') {
        ss->p2++;
        while (*ss->p2 != '\n') {
          ss->p2++;
        }
        goto scan_token;
      } else if (*ss->p2 == '>' && isspace(*(ss->p2 + 1))) {
        ss->ct = ARROW_TK;
        ss->ct_length = 2;
        ss->p2++;
        return;
      }
      break;
    case ':':
      if (*ss->p2 == ':' && *(ss->p2 + 1) == '=' && isspace(*(ss->p2 + 2))) {
        ss->ct = EQUIVALENCE_TK;
        ss->ct_length = 3;
        ss->p2 = ss->p1 + 3;
        return;
      }
      break;
    case '\0':
    case '\x1a': /* CTRL-Z • END-OF-FILE? */
      ss->ct = EOF_TK;
      ss->ct_length = 0;
      ss->p2 = ss->p1;
      return;
    default:
      if (*ss->p1 == cli_options->ormark && isspace(*ss->p2)) {
        ss->ct = OR_TK;
        ss->ct_length = 1;
        return;
      } else if (*ss->p1 == cli_options->escape) /* escape character? */
      {
        char *p3 = ss->p2 + 1;
        switch (*ss->p2) {
          case 't':
          case 'T':
            if (strxeq(p3, "erminals") && isspace(*(ss->p1 + 10))) {
              ss->ct = TERMINALS_KEY_TK;
              ss->ct_length = 10;
              ss->p2 = ss->p1 + 10;
              return;
            }
            break;
          case 'd':
          case 'D':
            if (strxeq(p3, "efine") && isspace(*(ss->p1 + 7))) {
              ss->ct = DEFINE_KEY_TK;
              ss->ct_length = 7;
              ss->p2 = ss->p1 + 7;
              return;
            }
            break;
          case 'e':
          case 'E':
            if (strxeq(p3, "mpty") && isspace(*(ss->p1 + 6))) {
              ss->ct = EMPTY_SYMBOL_TK;
              ss->ct_length = 6;
              ss->p2 = ss->p1 + 6;
              return;
            }
            if (strxeq(p3, "rror") && isspace(*(ss->p1 + 6))) {
              ss->ct = ERROR_SYMBOL_TK;
              ss->ct_length = 6;
              ss->p2 = ss->p1 + 6;
              return;
            }
            if (strxeq(p3, "ol") && isspace(*(ss->p1 + 4))) {
              ss->ct = EOL_SYMBOL_TK;
              ss->ct_length = 4;
              ss->p2 = ss->p1 + 4;
              return;
            }
            if (strxeq(p3, "of") && isspace(*(ss->p1 + 4))) {
              ss->ct = EOF_SYMBOL_TK;
              ss->ct_length = 4;
              ss->p2 = ss->p1 + 4;
              return;
            }
            if (strxeq(p3, "nd") && isspace(*(ss->p1 + 4))) {
              ss->ct = END_KEY_TK;
              ss->ct_length = 4;
              ss->p2 = ss->p1 + 4;
              return;
            }
            break;
          case 'r':
          case 'R':
            if (strxeq(p3, "ules") && isspace(*(ss->p1 + 6))) {
              ss->ct = RULES_KEY_TK;
              ss->ct_length = 6;
              ss->p2 = ss->p1 + 6;
              return;
            }
            break;
          case 'a':
          case 'A':
            if (strxeq(p3, "lias") && isspace(*(ss->p1 + 6))) {
              ss->ct = ALIAS_KEY_TK;
              ss->ct_length = 6;
              ss->p2 = ss->p1 + 6;
              return;
            }
            break;
          case 's':
          case 'S':
            if (strxeq(p3, "tart") && isspace(*(ss->p1 + 6))) {
              ss->ct = START_KEY_TK;
              ss->ct_length = 6;
              ss->p2 = ss->p1 + 6;
              return;
            }
            break;
          case 'n':
          case 'N':
            if (strxeq(p3, "ames") && isspace(*(ss->p1 + 6))) {
              ss->ct = NAMES_KEY_TK;
              ss->ct_length = 6;
              ss->p2 = ss->p1 + 6;
              return;
            }
            break;
          default:
            break;
        }
        ss->ct = MACRO_NAME_TK;
        while (!isspace(*ss->p2)) {
          ss->p2++;
        }
        ss->ct_length = ss->p2 - ss->p1;
        goto check_symbol_length;
      }
  }
  ss->ct = SYMBOL_TK;
  while (!isspace(*ss->p2)) {
    ss->p2++;
  }
  ss->ct_length = ss->p2 - ss->p1;
check_symbol_length:
  if (ss->ct_length > SYMBOL_SIZE) {
    ss->ct_length = SYMBOL_SIZE;
    memcpy(tok_string, ss->p1, ss->ct_length);
    tok_string[ss->ct_length] = '\0';
    if (symbol_image(tok_string, ps) == OMEGA) {
      PRNTWNG2("Length of Symbol \"%s\" in line %d exceeds maximum of ", tok_string, ss->line_no);
    }
  }
}

/// This function allocates a line_elemt structure and returns a pointer to it.
static struct line_elemt *alloc_line(struct LinePool* lp) {
  struct line_elemt *p = lp->line_pool_root;
  if (p != NULL) {
    lp->line_pool_root = p->link;
  } else {
    talloc0p(&p, struct line_elemt);
  }
  return p;
}

/// This function frees a line_elemt structure which is returned to a free pool.
static void free_line(struct line_elemt *p, struct LinePool* lp) {
  p->link = lp->line_pool_root;
  lp->line_pool_root = p;
}

/// FIND_MACRO takes as argument a pointer to a macro name. It searches for
/// the macro name in the hash table based on MACRO_TABLE. If the macro name
/// is found then the macro definition associated with it is returned.
/// If the name is not found, then a message is printed, a new definition is
/// entered to avoid more messages and NULL is returned.
static struct line_elemt *find_macro(char *name, ArrayShort macro_table, struct LinePool* lp, struct ParserState* ps) {
  struct line_elemt *root = NULL;
  char macro_name[MAX_LINE_SIZE + 1];
  char *s = macro_name;
  for (char *ptr = name; *ptr != '\0'; ptr++) {
    *s++ = isupper(*ptr) ? tolower(*ptr) : *ptr;
  }
  *s = '\0';
  const int i = hash(macro_name);
  for (int j = macro_table.raw[i]; j != NIL; j = ps->defelmt[j].next) {
    if (strcmp(macro_name, ps->defelmt[j].name) == 0) {
      char *ptr = ps->defelmt[j].macro;
      /* undefined macro? */
      if (ptr) {
        while (*ptr != '\0') {
          struct line_elemt *q = alloc_line(lp);
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
  // Make phony definition for macro to avoid future errors.
  if (ps->num_defs >= (int) ps->defelmt_size) {
    ps->defelmt_size += DEFELMT_INCREMENT;
    ps->defelmt = (struct defelmt_type *)
    (ps->defelmt == (struct defelmt_type *) NULL
       ? malloc(ps->defelmt_size * sizeof(struct defelmt_type))
       : realloc(ps->defelmt, ps->defelmt_size * sizeof(struct defelmt_type)));
    if (ps->defelmt == (struct defelmt_type *) NULL)
      nospace();
  }
  strcpy(ps->defelmt[ps->num_defs].name, macro_name);
  ps->defelmt[ps->num_defs].length = 0;
  ps->defelmt[ps->num_defs].macro = NULL;
  ps->defelmt[ps->num_defs].next = macro_table.raw[i];
  macro_table.raw[i] = ps->num_defs;
  ps->num_defs++;
  return NULL;
}

/// PROCESS_ACTION_LINE takes as arguments a line of text from an action
/// block and the rule number with which the block is associated.
/// It first scans the text for predefined macro names and then for
/// user defined macro names. If one is found, the macro definition is
/// substituted for the name. The modified action text is then printed out in
/// the action file.
static void process_action_line(FILE *sysout, char *text, const int line_no, const int rule_no, char *grm_file, const struct CLIOptions* cli_options, ArrayShort macro_table, struct LinePool* lp, const struct ruletab_type *rules, ArrayShort rhs_sym, struct ParserState* ps) {
  char temp1[MAX_LINE_SIZE + 1];
  char suffix[MAX_LINE_SIZE + 1];
  char symbol[SYMBOL_SIZE + 1];
  const int output_size = 5000;
  struct line_elemt *q;
  struct line_elemt *root = NULL;
  struct line_elemt *input_line_root = NULL;
next_line: {
  }
  int text_len = strlen(text);
  int k = 0; /* k is the cursor */
  while (k < text_len) {
    // all macro names begin with the ESCAPE
    if (text[k] == cli_options->escape) {
      // character
      // 12 is length of %rule_number and
      // %num_symbols.
      if (k + 12 <= text_len) {
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
            sprintf(text + k, "%ld%s", ps->num_symbols, temp1);
          } else {
            sprintf(text + k, "%ld", ps->num_symbols);
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
          const int max_len = output_size - k - jj;
          restore_symbol(temp2, RETRIEVE_STRING(rules[rule_no].lhs, ps->string_table, ps->symno), cli_options->ormark, cli_options->escape);
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
            for for_each_rhs(j, rule_no, rules) {
              restore_symbol(symbol, RETRIEVE_STRING(rhs_sym.raw[j], ps->string_table, ps->symno), cli_options->ormark, cli_options->escape);
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
            sprintf(text + k, "%d%s", RHS_SIZE(rule_no, rules), temp1);
          } else {
            sprintf(text + k, "%d", RHS_SIZE(rule_no, rules));
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
            sprintf(text + k, "%ld%s", ps->num_rules, temp1);
          } else {
            sprintf(text + k, "%ld", ps->num_rules);
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
            sprintf(text + k, "%ld%s", ps->num_terminals, temp1);
          else
            sprintf(text + k, "%ld", ps->num_terminals);
          goto proceed;
        }
      }
      if (k + 18 <= text_len) /* 18 is length of %num_non_terminals */
      {
        if (strxeq(text + k, knum_non_terminals)) {
          strcpy(temp1, text + k + 18);
          if (k + 18 != text_len) {
            sprintf(text + k, "%ld%s", ps->num_non_terminals, temp1);
          } else {
            sprintf(text + k, "%ld", ps->num_non_terminals);
          }
          goto proceed;
        }
      }
      // Macro in question is not one of the predefined macros. Try user-defined
      // macro list.
      // find next delimeter
      int jj;
      for (jj = k + 1; jj < text_len && !isspace(text[jj]); ++jj) {
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
      root = find_macro(symbol, macro_table, lp, ps); /* "root" points to a circular  */
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
            tail = alloc_line(lp);
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
          free_line(q, lp);
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
  // If text is greater than output size,
  // print error message and truncate line.
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
    free_line(q, lp);
    goto next_line;
  }
}

/// This procedure takes as argument a macro definition. If the name of the
/// macro is one of the predefined names, it issues an error.  Otherwise, it
/// inserts the macro definition into the table headed by MACRO_TABLE.
static void mapmacro(const int def_index, ArrayShort macro_table, const struct ParserState* ps) {
  if (strcmp(ps->defelmt[def_index].name, krule_text) == 0 ||
      strcmp(ps->defelmt[def_index].name, krule_number) == 0 ||
      strcmp(ps->defelmt[def_index].name, knum_rules) == 0 ||
      strcmp(ps->defelmt[def_index].name, krule_size) == 0 ||
      strcmp(ps->defelmt[def_index].name, knum_terminals) == 0 ||
      strcmp(ps->defelmt[def_index].name, knum_non_terminals) == 0 ||
      strcmp(ps->defelmt[def_index].name, knum_symbols) == 0 ||
      strcmp(ps->defelmt[def_index].name, kinput_file) == 0 ||
      strcmp(ps->defelmt[def_index].name, kcurrent_line) == 0 ||
      strcmp(ps->defelmt[def_index].name, knext_line) == 0) {
    PRNTWNG2("predefined macro \"%s\" cannot be redefined. Line %ld", ps->defelmt[def_index].name, ps->defelmt[def_index].start_line);
  } else {
    const int i = hash(ps->defelmt[def_index].name);
    for (int j = macro_table.raw[i]; j != NIL; j = ps->defelmt[j].next) {
      if (strcmp(ps->defelmt[j].name, ps->defelmt[def_index].name) == 0) {
        PRNTWNG2("Redefinition of macro \"%s\" in line %ld", ps->defelmt[def_index].name, ps->defelmt[def_index].start_line);
        break;
      }
    }
    ps->defelmt[def_index].next = macro_table.raw[i];
    macro_table.raw[i] = def_index;
  }
}

/// Process all semantic actions and generate action file.
static void process_actions(char *grm_file, struct CLIOptions *cli_options, struct ScannerState* ss, const struct ruletab_type *rules, ArrayShort rhs_sym, struct ParserState* ps) {
  struct LinePool lp = (struct LinePool) {
    .line_pool_root = NULL,
  };
  char *p;
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
  ArrayShort macro_table = Allocate_short_array2(HT_SIZE);
  for (int i = 0; i < HT_SIZE; i++) {
    macro_table.raw[i] = NIL;
  }
  ss->bufend = &ss->input_buffer[0];
  read_input(grm_file, sysgrm, ss);
  ss->p2 = &ss->input_buffer[0];
  ss->linestart = ss->p2 - 1;
  ss->p1 = ss->p2;
  ss->line_no = 1;
  // Read in all the macro definitions and insert them into macro_table.
  for (int i = 0; i < ps->num_defs; i++) {
    calloc0p(&ps->defelmt[i].macro, ps->defelmt[i].length + 2, char);
    for (; ss->line_no < ps->defelmt[i].start_line; ss->line_no++) {
      while (*ss->p1 != '\n') {
        ss->p1++;
      }
      ss->p1++;
      if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
        int k = ss->bufend - ss->p1;
        if (k < MAX_LINE_SIZE) {
          strcpy(ss->input_buffer, ss->p1);
          ss->bufend = &ss->input_buffer[k];
          read_input(grm_file, sysgrm, ss);
          ss->p1 = &ss->input_buffer[0];
        }
      }
      ss->linestart = ss->p1 - 1;
    }
    ss->p1 = ss->linestart + ps->defelmt[i].start_column;
    for (int j = 0; j < ps->defelmt[i].length; j++) {
      ps->defelmt[i].macro[j] = *ss->p1;
      if (*ss->p1++ == '\n') {
        if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
          int k = ss->bufend - ss->p1;
          if (k < MAX_LINE_SIZE) {
            strcpy(ss->input_buffer, ss->p1);
            ss->bufend = &ss->input_buffer[k];
            read_input(grm_file, sysgrm, ss);
            ss->p1 = &ss->input_buffer[0];
          }
        }
        ss->line_no++;
        ss->linestart = ss->p1 - 1;
      }
    }
    ps->defelmt[i].macro[ps->defelmt[i].length] = '\n';
    ps->defelmt[i].macro[ps->defelmt[i].length + 1] = '\0';
    for (p = ps->defelmt[i].name; *p != '\0'; p++) {
      *p = isupper(*p) ? tolower(*p) : *p;
    }
    mapmacro(i, macro_table, ps);
  }
  // Read in all the action blocks and process them.
  for (int i = 0; i < ps->num_acts; i++) {
    for (; ss->line_no < ps->actelmt[i].start_line; ss->line_no++) {
      while (*ss->p1 != '\n') {
        ss->p1++;
      }
      ss->p1++;
      if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
        int k = ss->bufend - ss->p1;
        if (k < MAX_LINE_SIZE) {
          strcpy(ss->input_buffer, ss->p1);
          ss->bufend = &ss->input_buffer[k];
          read_input(grm_file, sysgrm, ss);
          ss->p1 = &ss->input_buffer[0];
        }
      }
      ss->linestart = ss->p1 - 1;
    }
    if (ps->actelmt[i].start_line == ps->actelmt[i].end_line) {
      int len = ps->actelmt[i].end_column - ps->actelmt[i].start_column + 1;
      memcpy(line, ss->linestart + ps->actelmt[i].start_column, len);
      line[len] = '\0';
      while (*ss->p1 != '\n') {
        ss->p1++;
      }
    } else {
      p = line;
      ss->p1 = ss->linestart + ps->actelmt[i].start_column;
      while (*ss->p1 != '\n') {
        *p++ = *ss->p1++;
      }
      *p = '\0';
    }
    if (ps->actelmt[i].header_block) {
      process_action_line(syshact, line, ss->line_no, ps->actelmt[i].rule_number, grm_file, cli_options, macro_table, &lp, rules, rhs_sym, ps);
    } else {
      process_action_line(sysact, line, ss->line_no, ps->actelmt[i].rule_number, grm_file, cli_options, macro_table, &lp, rules, rhs_sym, ps);
    }
    if (ss->line_no != ps->actelmt[i].end_line) {
      while (ss->line_no < ps->actelmt[i].end_line) {
        ss->p1++;
        if (ss->bufend == ss->input_buffer + IOBUFFER_SIZE) {
          int k = ss->bufend - ss->p1;
          if (k < MAX_LINE_SIZE) {
            strcpy(ss->input_buffer, ss->p1);
            ss->bufend = &ss->input_buffer[k];
            read_input(grm_file, sysgrm, ss);
            ss->p1 = &ss->input_buffer[0];
          }
        }
        ss->line_no++;
        ss->linestart = ss->p1 - 1;
        if (ss->line_no < ps->actelmt[i].end_line) {
          p = line;
          while (*ss->p1 != '\n') {
            *p++ = *ss->p1++;
          }
          *p = '\0';
          if (ps->actelmt[i].header_block) {
            process_action_line(syshact, line, ss->line_no, ps->actelmt[i].rule_number, grm_file, cli_options, macro_table, &lp, rules, rhs_sym, ps);
          } else {
            process_action_line(sysact, line, ss->line_no, ps->actelmt[i].rule_number, grm_file, cli_options, macro_table, &lp, rules, rhs_sym, ps);
          }
        }
      }
      if (ps->actelmt[i].end_column != 0) {
        int len = ps->actelmt[i].end_column;
        memcpy(line, ss->p1, len);
        line[len] = '\0';
        if (ps->actelmt[i].header_block) {
          process_action_line(syshact, line, ss->line_no, ps->actelmt[i].rule_number, grm_file, cli_options, macro_table, &lp, rules, rhs_sym, ps);
        } else {
          process_action_line(sysact, line, ss->line_no, ps->actelmt[i].rule_number, grm_file, cli_options, macro_table, &lp, rules, rhs_sym, ps);
        }
      }
    }
  }
  for (int i = 0; i < ps->num_defs; i++) {
    ffree(ps->defelmt[i].macro);
  }
  ffree(ps->defelmt);
  ffree(ps->actelmt);
  fclose(sysgrm); /* Close grammar file and reopen it. */
  fclose(sysact);
  fclose(syshact);
}

/// Actions to be taken if grammar is successfully parsed.
static void accept_action(char *grm_file, struct CLIOptions *cli_options, FILE *sysgrm, struct ScannerState* ss, ArrayShort *rhs_sym, struct ruletab_type **rulesp, struct ParserState* ps, struct symno_type *symno, int **name) {
  if (ps->rulehdr == NULL) {
    printf("Informative: Empty grammar read in. Processing stopped.\n");
    fclose(sysgrm);
    exit(12);
  }
  ps->num_non_terminals = ps->num_symbols - ps->num_terminals;
  if (cli_options->error_maps_bit) {
    // make_names_map
    {
      // Construct the NAME map, and update the elements of SYMNO with their names.
      symno[accept_image].name_index = name_map("", ps);
      if (error_image == DEFAULT_SYMBOL) {
        symno[DEFAULT_SYMBOL].name_index = symno[accept_image].name_index;
      }
      for for_each_t_fw(symbol, ps) {
        if (symno[symbol].name_index == OMEGA) {
          symno[symbol].name_index = name_map(RETRIEVE_STRING(symbol, ps->string_table, symno), ps);
        }
      }
      for for_each_nt_fw(symbol, ps) {
        if (symno[symbol].name_index == OMEGA) {
          symno[symbol].name_index = name_map(RETRIEVE_STRING(symbol, ps->string_table, symno), ps);
        }
      }
      calloc0p(name, ps->num_names + 1, int);
      for (int i = 0; i < HT_SIZE; i++) {
        for (const struct hash_type *p = ps->hash_table[i]; p != NULL; p = p->link) {
          if (p->name_index != OMEGA) {
            (*name)[p->name_index] = p->st_ptr;
          }
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
    struct node *ptr;
    int rhs_ct = 0;
    calloc0p(rulesp, ps->num_rules + 2, struct ruletab_type);
    *rhs_sym = Allocate_short_array2(ps->num_items + 1);
    ps->num_items += ps->num_rules + 1;
    int ii = 0;
    struct ruletab_type *rules = *rulesp;
    // Put starting rules from start symbol linked list in rule and rhs table
    if (ps->start_symbol_root != NULL) {
      // Turn circular list into linear
      struct node *q = ps->start_symbol_root;
      ps->start_symbol_root = q->next;
      q->next = NULL;
      for (ptr = ps->start_symbol_root; ptr != NULL; ptr = ptr->next) {
        rules[ii].lhs = accept_image;
        rules[ii].sp = 0;
        rules[ii++].rhs = rhs_ct;
        if (ptr->value != empty) {
          rhs_sym->raw[rhs_ct++] = ptr->value;
        }
      }
      free_nodes(ps->start_symbol_root, q);
    }
    //   In this loop, the grammar is placed in the rule table structure and the
    // right-hand sides are placed in the RHS table.  A check is made to prevent
    // terminals from being used as left hand sides.
    for (; ii <= ps->num_rules; ii++) {
      rules[ii].rhs = rhs_ct;
      ptr = ps->rulehdr[ii].rhs_root;
      if (ptr != NULL) {
        // not am empty right-hand side?
        do {
          ptr = ptr->next;
          rhs_sym->raw[rhs_ct++] = ptr->value;
        } while (ptr != ps->rulehdr[ii].rhs_root);
        ptr = ptr->next; /* point to 1st element */
        rules[ii].sp = ps->rulehdr[ii].sp && ptr == ps->rulehdr[ii].rhs_root;
        if (rules[ii].sp) {
          ps->num_single_productions++;
        }
        free_nodes(ptr, ps->rulehdr[ii].rhs_root);
      } else {
        rules[ii].sp = false;
      }
      if (ps->rulehdr[ii].lhs == OMEGA) {
        rules[ii].lhs = rules[ii - 1].lhs;
      } else if (IS_A_TERMINAL_P(ps->rulehdr[ii].lhs, ps)) {
        char temp[SYMBOL_SIZE + 1];
        restore_symbol(temp, RETRIEVE_STRING(ps->rulehdr[ii].lhs, ps->string_table, symno), cli_options->ormark, cli_options->escape);
        PRNTERR2("In rule %d: terminal \"%s\" used as left hand side", ii, temp);
        PRNTERR("Processing terminated due to input errors.");
        exit(12);
      } else {
        rules[ii].lhs = ps->rulehdr[ii].lhs;
      }
    }
    rules[ps->num_rules + 1].rhs = rhs_ct; /* Fence !! */
  }
  fclose(sysgrm); /* Close grammar input file. */
  process_actions(grm_file, cli_options, ss, *rulesp, *rhs_sym, ps);
}

/// This procedure opens all relevant files and processes the input grammar.
void process_input(char *grm_file, struct OutputFiles *output_files, const int argc, char *argv[], char *file_prefix, struct CLIOptions *cli_options, ArrayShort *rhs_sym, struct ruletab_type **rulesp, struct symno_type **symno, struct ParserState* ps, int **name) {
  char parm[256] = "";

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
      int ii;
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
  }

  struct ScannerState ss = {
    .ct = 0,
    .ct_start_col = 0,
    .ct_end_col = 0,
    .ct_length = 0,
    .ct_start_line = 0,
    .ct_end_line = 0,
    .line_no = 0,
  };

  // Init grammar.
  {
    calloc0p(&ps->terminal, STACK_SIZE, struct terminal_type);
    calloc0p(&(ps->hash_table), HT_SIZE, struct hash_type *);
    // Allocate space for input buffer and read in initial data in input
    // file. Next, invoke PROCESS_OPTION_LINES to process all lines in
    // input file that are options line.
    calloc0p(&ss.input_buffer, IOBUFFER_SIZE + 1 + MAX_LINE_SIZE, char);
    ss.bufend = &ss.input_buffer[0];
    read_input(grm_file, sysgrm, &ss);
    ss.p2 = &ss.input_buffer[0];
    ss.linestart = ss.p2 - 1;
    ss.p1 = ss.p2;
    ss.line_no++;
    if (*ss.p2 == '\0') {
      fprintf(stderr, "Input file \"%s\" containing grammar is empty, undefined, or invalid\n", grm_file);
      exit(12);
    }
    process_options_lines(grm_file, output_files, file_prefix, cli_options, sysgrm, &ss, parm);
    eolt_image = OMEGA;
    // Keywords, Reserved symbols, and predefined macros
    kdefine[0] = cli_options->escape; /* Set empty first space to the default */
    kterminals[0] = cli_options->escape; /* escape symbol.                      */
    kalias[0] = cli_options->escape;
    kstart[0] = cli_options->escape;
    krules[0] = cli_options->escape;
    knames[0] = cli_options->escape;
    kend[0] = cli_options->escape;
    krule_number[0] = cli_options->escape;
    krule_text[0] = cli_options->escape;
    krule_size[0] = cli_options->escape;
    knum_rules[0] = cli_options->escape;
    knum_terminals[0] = cli_options->escape;
    knum_non_terminals[0] = cli_options->escape;
    knum_symbols[0] = cli_options->escape;
    kinput_file[0] = cli_options->escape;
    kcurrent_line[0] = cli_options->escape;
    knext_line[0] = cli_options->escape;
    kstart_nt[0] = cli_options->escape;
    keolt[0] = cli_options->escape;
  }

  // Process grammar.
  {
    // PROCESS_GRAMMAR is invoked to process the source input. It uses an
    // LALR(1) parser table generated by LPG to recognize the grammar which it
    // places in the rulehdr structure.
    short state_stack[STACK_SIZE];
    scanner(grm_file, sysgrm, cli_options, &ss, ps); /* Get first token */
    int act = START_STATE;
  process_terminal:
    // Note that this driver assumes that the tables are LPG SPACE tables with no GOTO-DEFAULTS.
    state_stack[++(ps->stack_top)] = act;
    act = t_action(act, ss.ct, ?);
    // Reduce
    if (act <= NUM_RULES) {
      ps->stack_top--;
    } else if (act > ERROR_ACTION || /* Shift_reduce */ act < ACCEPT_ACTION) /* Shift */
    {
      // token_action
      {
        {
          //    This function, TOKEN_ACTION, pushes the current token onto the
          // parse stack called TERMINAL. Note that in case of a BLOCK_, the name of
          // the token is not copied since blocks are processed separately on a
          // second pass.
          const int top = ps->stack_top + 1;
          ps->terminal[top].kind = ss.ct;
          ps->terminal[top].start_line = ss.ct_start_line;
          ps->terminal[top].start_column = ss.ct_start_col;
          ps->terminal[top].end_line = ss.ct_end_line;
          ps->terminal[top].end_column = ss.ct_end_col;
          ps->terminal[top].length = ss.ct_length;
          if (ss.ct != BLOCK_TK) {
            memcpy(ps->terminal[top].name, ss.ct_ptr, ss.ct_length);
            ps->terminal[top].name[ss.ct_length] = '\0';
          } else {
            ps->terminal[top].name[0] = '\0';
          }
        }
      }
      scanner(grm_file, sysgrm, cli_options, &ss, ps);
      if (act < ACCEPT_ACTION) {
        goto process_terminal;
      }
      act -= ERROR_ACTION;
    } else if (act == ACCEPT_ACTION) {
      accept_action(grm_file, cli_options, sysgrm, &ss, rhs_sym, rulesp, ps, *symno, name);
      goto end;
    } else {
      // error_action
      {
        // Error messages to be printed if an error is encountered during parsing.
        ss.ct_ptr[ss.ct_length] = '\0';
        if (ss.ct == EOF_TK) {
          PRNTERR2("End-of file reached prematurely");
        } else if (ss.ct == MACRO_NAME_TK) {
          PRNTERR2("Misplaced macro name \"%s\" found in line %d, column %d", ss.ct_ptr, ss.line_no, ss.ct_start_col);
        } else if (ss.ct == SYMBOL_TK) {
          char tok_string[SYMBOL_SIZE + 1];
          restore_symbol(tok_string, ss.ct_ptr, cli_options->ormark, cli_options->escape);
          PRNTERR2("Misplaced symbol \"%s\" found in line %d, column %d", tok_string, ss.line_no, ss.ct_start_col);
        } else {
          PRNTERR2("Misplaced keyword \"%s\" found in line %d, column %d", ss.ct_ptr, ss.line_no, ss.ct_start_col);
        }
        exit(12);
      }
    }
  process_non_terminal:
    do {
      const int lhs_sym = lhs[act]; /* to bypass IBMC12 bug */
      ps->stack_top -= rhs[act] - 1;
      rule_action[act](ps);
      act = nt_action(state_stack[ps->stack_top], lhs_sym);
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
    if (ps->string_offset > 0) {
      ps->string_table = (char *)
      (ps->string_table == (char *) NULL
         ? malloc(ps->string_offset * sizeof(char))
         : realloc(ps->string_table, ps->string_offset * sizeof(char)));
      if (ps->string_table == (char *) NULL)
        nospace();
    }
    ffree(ps->terminal);
    ffree(ps->hash_table);
    ffree(ss.input_buffer);
    ffree(ps->rulehdr); /* allocated in action LPGACT when grammar is not empty */
  }
}
// endregion