#include "common.h"

#line 87 "jikespg.g"

#define SYM1 (ps->terminal[ps->stack_top + 1])
#define SYM2 (ps->terminal[ps->stack_top + 2])
#define SYM3 (ps->terminal[ps->stack_top + 3])

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

    ps->actelmt[ps->num_acts].rule_number  = num_rules;
    ps->actelmt[ps->num_acts].start_line   = term->start_line;
    ps->actelmt[ps->num_acts].start_column = term->start_column;
    ps->actelmt[ps->num_acts].end_line     = term->end_line;
    ps->actelmt[ps->num_acts].end_column   = term->end_column;
    ps->actelmt[ps->num_acts].header_block = term->kind == HBLOCK_TK;
    ps->num_acts++;
}

/// bad_symbol ::= EQUIVALENCE
#line 154 "jikespg.g"
static void bad_first_symbol(struct ParserState* ps)
{
    PRNTERR2("First symbol: \"%s\" found in file is illegal. Line %ld, column %d", SYM1.name, SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_symbol ::= BLOCK
#line 175 "jikespg.g"
static void act10(struct ParserState* ps)
{
    PRNTERR2("Action block cannot be first object in file. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_list ::= macro_name_symbol macro_block
#line 190 "jikespg.g"
static void act13(struct ParserState* ps)
{
    add_macro_definition(SYM1.name, &(SYM2), ps);
}

/// macro_list ::= macro_list macro_name_symbol macro_block
#line 198 "jikespg.g"
static void act14(struct ParserState* ps)
{
    add_macro_definition(SYM2.name, &(SYM3), ps);
}

/// macro_name_symbol ::= SYMBOL
#line 209 "jikespg.g"
static void act16(struct ParserState* ps)
{
    PRNTWNG2("Macro name \"%s\" does not start with the escape character. Line %ld, column %d", SYM1.name, SYM1.start_line, SYM1.start_column);
}

/// macro_name_symbol ::= OR
#line 217 "jikespg.g"
static void bad_macro_name(struct ParserState* ps)
{
    PRNTERR2("Reserved symbol cannot be used as macro name. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_name_symbol ::= BLOCK
#line 232 "jikespg.g"
static void act21(struct ParserState* ps)
{
    PRNTERR2("Macro name not supplied for macro definition. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_name_symbol ::= DEFINE_KEY
#line 241 "jikespg.g"
static void act22(struct ParserState* ps)
{
    PRNTERR2("Macro keyword misplaced. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_block ::= OR
#line 253 "jikespg.g"
static void definition_expected(struct ParserState* ps)
{
    PRNTERR2("Definition block expected where symbol found. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// terminal_symbol ::= SYMBOL
#line 279 "jikespg.g"
static void process_terminal(struct ParserState* ps)
{
    assign_symbol_no(SYM1.name, OMEGA, ps->hash_table, &ps->string_offset);
}

/// terminal_symbol ::= DEFINE_KEY
#line 291 "jikespg.g"
static void bad_terminal(struct ParserState* ps)
{
    PRNTERR2("Keyword  has been misplaced in Terminal section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// terminal_symbol ::= BLOCK
#line 302 "jikespg.g"
static void act37(struct ParserState* ps)
{
    PRNTERR2("Misplaced block found in TERMINALS section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// alias_definition ::= alias_lhs produces alias_rhs
#line 315 "jikespg.g"
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
            assign_symbol_no(SYM3.name, OMEGA, ps->hash_table, &ps->string_offset);
            image = symbol_image(SYM3.name, ps);
            break;

        case ERROR_SYMBOL_TK:
            if (error_image > num_terminals)
            {
                restore_symbol(tok_string, kerror, ps->ormark, ps->escape);
                PRNTERR2("Illegal aliasing to %s prior to its definition.  Line %ld, column %d", tok_string, SYM3.start_line, SYM3.start_column);
                exit(12);
            }
            image = error_image;
            break;

        case EOF_SYMBOL_TK:
            if (eoft_image > num_terminals)
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
            assign_symbol_no(SYM1.name, image, ps->hash_table, &ps->string_offset);
            break;

        case ERROR_SYMBOL_TK:
            if (error_image > num_terminals || ! ps->error_maps_bit)
            {
                if (image == empty      || image == eolt_image ||
                    image == eoft_image || image > num_terminals)
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
            if (eoft_image > num_terminals)
            {
                if (image == empty       || image == eolt_image  ||
                    image == error_image || image > num_terminals)
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
                    image > num_terminals)
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
#line 475 "jikespg.g"
static void bad_alias_rhs(struct ParserState* ps)
{
    PRNTERR2("Misplaced keyword found in Alias section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_alias_rhs ::= BLOCK
#line 488 "jikespg.g"
static void act57(struct ParserState* ps)
{
    PRNTERR2("Misplaced block found in Alias section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_alias_lhs ::= EMPTY_SYMBOL
#line 501 "jikespg.g"
static void act59(struct ParserState* ps)
{
    PRNTERR2("Empty symbol cannot be aliased. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_alias_lhs ::= produces
#line 510 "jikespg.g"
static void missing_quote(struct ParserState* ps)
{
    PRNTERR2("Symbol must be quoted when used as a grammar symbol. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// start_symbol ::= SYMBOL
#line 526 "jikespg.g"
static void act63(struct ParserState* ps)
{
    assign_symbol_no(SYM1.name, OMEGA, ps->hash_table, &ps->string_offset);
    struct node *q = Allocate_node();
    q -> value = symbol_image(SYM1.name, ps);
    if (ps->start_symbol_root == NULL)
        q -> next = q;
    else
    {
        q -> next = ps->start_symbol_root -> next;
        ps->start_symbol_root -> next = q;
    }
    ps->start_symbol_root = q;
    num_rules++;
    num_items++;
}

/// start_symbol ::= OR
#line 546 "jikespg.g"
static void bad_start_symbol(struct ParserState* ps)
{
    PRNTERR2("Symbol cannot be used as Start symbol. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// start_symbol ::= BLOCK
#line 561 "jikespg.g"
static void act68(struct ParserState* ps)
{
    PRNTERR2("Misplaced block found in Start section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// start_symbol ::= DEFINE_KEY
#line 570 "jikespg.g"
static void misplaced_keyword_found_in_START_section(struct ParserState* ps)
{
    PRNTERR2("Misplaced keyword found in START section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// rules_block ::= RULES_KEY
#line 586 "jikespg.g"
static void act73(struct ParserState* ps)
{

    if (ps->start_symbol_root == NULL)
    {
        struct node *q = Allocate_node();
        q -> value = empty;
        q -> next = q;
        ps->start_symbol_root = q;
        num_rules = 0;                 // One rule
        num_items = 0;                 // 0 items
    }
    build_symno(ps);
}

/// rules_block ::= RULES_KEY rule_list
#line 604 "jikespg.g"
static void act74(struct ParserState* ps)
{
    build_symno(ps);
}

/// rule_list ::= {action_block} SYMBOL produces
#line 618 "jikespg.g"
static void act77(struct ParserState* ps)
{
    assign_symbol_no(SYM2.name, OMEGA, ps->hash_table, &ps->string_offset);
    if (ps->start_symbol_root == NULL)
    {
        struct node *q = Allocate_node();
        q -> value = symbol_image(SYM2.name, ps);
        q -> next = q;

        ps->start_symbol_root = q;

        num_rules = 1;
        num_items = 1;
    }

/// Since we don't know for sure how many start symbols we have, a
/// "while" loop is used to increment the size of rulehdr. However,
/// it is highly unlikely that this loop would ever execute more than
/// once if the size of RULE_INCREMENT is reasonable.
    while (num_rules >= (int)ps->rulehdr_size)
    {
        ps->rulehdr_size += RULEHDR_INCREMENT;
        ps->rulehdr = (struct rulehdr_type *)
            (ps->rulehdr == (struct rulehdr_type *) NULL
             ? malloc(ps->rulehdr_size * sizeof(struct rulehdr_type))
             : realloc(ps->rulehdr, ps->rulehdr_size * sizeof(struct rulehdr_type)));
        if (ps->rulehdr == (struct rulehdr_type *) NULL)
            nospace();
    }

    ps->rulehdr[num_rules].sp = ((SYM3.kind == ARROW_TK) ? true : false);
    ps->rulehdr[num_rules].lhs = symbol_image(SYM2.name, ps);
    ps->rulehdr[num_rules].rhs_root = NULL;
}

/// rule_list ::= rule_list OR
#line 657 "jikespg.g"
static void act78(struct ParserState* ps)
{
    num_rules++;
    if (num_rules >= (int)ps->rulehdr_size)
    {
        ps->rulehdr_size += RULEHDR_INCREMENT;
        ps->rulehdr = (struct rulehdr_type *)
            (ps->rulehdr == (struct rulehdr_type *) NULL
             ? malloc(ps->rulehdr_size * sizeof(struct rulehdr_type))
             : realloc(ps->rulehdr, ps->rulehdr_size * sizeof(struct rulehdr_type)));
        if (ps->rulehdr == (struct rulehdr_type *) NULL)
            nospace();
    }
    ps->rulehdr[num_rules].sp = ps->rulehdr[num_rules - 1].sp;
    ps->rulehdr[num_rules].lhs = OMEGA;
    ps->rulehdr[num_rules].rhs_root = NULL;
}

/// rule_list ::= rule_list SYMBOL produces
#line 678 "jikespg.g"
static void act79(struct ParserState* ps)
{
    num_rules++;
    if (num_rules >= (int)ps->rulehdr_size)
    {
        ps->rulehdr_size += RULEHDR_INCREMENT;
        ps->rulehdr = (struct rulehdr_type *)
            (ps->rulehdr == (struct rulehdr_type *) NULL
             ? malloc(ps->rulehdr_size * sizeof(struct rulehdr_type))
             : realloc(ps->rulehdr, ps->rulehdr_size * sizeof(struct rulehdr_type)));
        if (ps->rulehdr == (struct rulehdr_type *) NULL)
            nospace();
    }
    ps->rulehdr[num_rules].sp = ((SYM3.kind == ARROW_TK) ? true : false);
    assign_symbol_no(SYM2.name, OMEGA, ps->hash_table, &ps->string_offset);
    ps->rulehdr[num_rules].lhs = symbol_image(SYM2.name, ps);
    ps->rulehdr[num_rules].rhs_root = NULL;
}

/// rule_list ::= rule_list ERROR_SYMBOL
#line 705 "jikespg.g"
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
    num_items++;
    if (ps->rulehdr[num_rules].rhs_root == NULL)
        q -> next = q;
    else
    {
        q -> next = ps->rulehdr[num_rules].rhs_root -> next;
         ps->rulehdr[num_rules].rhs_root -> next = q;
    }
    ps->rulehdr[num_rules].rhs_root = q;
}

/// rule_list ::= rule_list SYMBOL
#line 730 "jikespg.g"
static void act83(struct ParserState* ps)
{
    assign_symbol_no(SYM2.name, OMEGA, ps->hash_table, &ps->string_offset);
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
        num_items++;
        if (ps->rulehdr[num_rules].rhs_root == NULL)
            q -> next = q;
        else
        {
            q -> next = ps->rulehdr[num_rules].rhs_root -> next;
            ps->rulehdr[num_rules].rhs_root -> next = q;
        }
        ps->rulehdr[num_rules].rhs_root = q;
    }
}

/// rule_list ::= OR
#line 758 "jikespg.g"
static void bad_first_symbol_in_RULES_section(struct ParserState* ps)
{
    PRNTERR2("First symbol in Rules section is not a valid left-hand side.\n Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// rule_list ::= rule_list OR produces
#line 773 "jikespg.g"
static void rule_without_left_hand_side(struct ParserState* ps)
{
    PRNTERR2("Rule without left-hand-side.  Line %ld, column %d", SYM3.start_line, SYM3.start_column);
    exit(12);
}

/// rule_list ::= rule_list keyword produces
#line 786 "jikespg.g"
static void act91(struct ParserState* ps)
{
    PRNTWNG2("Misplaced keyword found in Rules section Line %ld, column %d",  SYM2.start_line, SYM2.start_column);
    exit(12);
}

/// action_block ::= BLOCK
#line 796 "jikespg.g"
static void act92(struct ParserState* ps)
{
    add_block_definition(&(SYM1), ps);
}

/// action_block ::= HBLOCK
#line 804 "jikespg.g"
static void act93(struct ParserState* ps)
{
    add_block_definition(&(SYM1), ps);
}

/// keyword ::= DEFINE_KEY
#line 813 "jikespg.g"
static void misplaced_keyword_found_in_RULES_section(struct ParserState* ps)
{
    PRNTWNG2("Misplaced keyword found in RULES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// names_definition ::= name produces name
#line 834 "jikespg.g"
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

        if (symno[symbol].name_index != OMEGA)
        {
            PRNTERR2("Symbol %s has been named more than once. Line %ld, column %d.", SYM1.name, SYM1.start_line, SYM1.start_column);
            exit(12);
        }
         symno[symbol].name_index = name_map(SYM3.name, ps);
     }
}

/// bad_name ::= DEFINE_KEY
#line 901 "jikespg.g"
static void misplaced_keyword_found_in_NAMES_section(struct ParserState* ps)
{
    PRNTERR2("Keyword  has been misplaced in NAMES section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_name ::= BLOCK
#line 920 "jikespg.g"
static void act116(struct ParserState* ps)
{
    PRNTERR2("Misplaced action block found in NAMES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_name ::= MACRO_NAME
#line 929 "jikespg.g"
static void act117(struct ParserState* ps)
{
    PRNTERR2("Misplaced macro name found in NAMES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// [terminals_block] ::=
#line 946 "jikespg.g"
static void process_TERMINALS_section(struct ParserState* ps)
{
    num_terminals = num_symbols;
    assign_symbol_no(keoft, OMEGA, ps->hash_table, &ps->string_offset);
    eoft_image = symbol_image(keoft, ps);
    if (ps->error_maps_bit) {
        assign_symbol_no(kerror, OMEGA, ps->hash_table, &ps->string_offset);
        error_image = symbol_image(kerror, ps);
    } else {
      error_image = DEFAULT_SYMBOL;   // should be 0
    }
    assign_symbol_no(kaccept, OMEGA, ps->hash_table, &ps->string_offset);
    accept_image = symbol_image(kaccept, ps);
}

/// [alias_block] ::=
#line 967 "jikespg.g"
static void process_ALIAS_section(struct ParserState* ps)
{

    int k = 0;
    if (eoft_image <= num_terminals) {
        k++;
    } else {
        num_terminals++;
    }

    if (ps->error_maps_bit)
    {
        if (error_image <= num_terminals)
            k++;
        else
        {
            num_terminals++;
            if (k == 1)
                error_image--;
        }
    }

    if (k > 0)
    {
        for (int i = 0; i < HT_SIZE; i++)
        {
            struct hash_type* p = ps->hash_table[i];
            while(p != NULL)
            {
                if (p -> number > num_terminals)
                    p -> number -= k;
                else if (p -> number < -num_terminals)
                    p -> number += k;
                p = p -> link;
            }
        }
        num_symbols -= k;
        accept_image -= k;
    }
    if (eolt_image == OMEGA)
        eolt_image = eoft_image;
    if (error_image == DEFAULT_SYMBOL)
        alias_map(kerror, DEFAULT_SYMBOL, ps);
}

/// {terminal_symbol} ::=
#line 1040 "jikespg.g"
static void act132(struct ParserState* ps)
{
    assign_symbol_no(kempty, OMEGA, ps->hash_table, &ps->string_offset);
    empty = symbol_image(kempty, ps);
}
