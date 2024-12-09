#include "lpgparse.h"

#line 65 "jikespg.g"
#define SYM1 terminal[stack_top + 1]
#define SYM2 terminal[stack_top + 2]
#define SYM3 terminal[stack_top + 3]

static void null_action(void)
{
}

static void add_macro_definition(const char *name, const struct terminal_type *term)
{
    if (num_defs >= (int)defelmt_size)
    {
        defelmt_size += DEFELMT_INCREMENT;
        defelmt = (struct defelmt_type *)
            (defelmt == (struct defelmt_type *) NULL
             ? malloc(defelmt_size * sizeof(struct defelmt_type))
             : realloc(defelmt, defelmt_size * sizeof(struct defelmt_type)));
        if (defelmt == (struct defelmt_type *) NULL)
            nospace(__FILE__, __LINE__);
    }
    defelmt[num_defs].length       = term->length;
    defelmt[num_defs].start_line   = term->start_line;
    defelmt[num_defs].start_column = term->start_column;
    defelmt[num_defs].end_line     = term->end_line;
    defelmt[num_defs].end_column   = term->end_column;
    strcpy(defelmt[num_defs].name, name);
    num_defs++;
}

static void add_block_definition(const struct terminal_type *term)
{
    if (num_acts >= (int) actelmt_size)
    {
        actelmt_size += ACTELMT_INCREMENT;
        actelmt = (struct actelmt_type *)
            (actelmt == (struct actelmt_type *) NULL
             ? malloc(actelmt_size * sizeof(struct actelmt_type))
             : realloc(actelmt, actelmt_size * sizeof(struct actelmt_type)));
        if (actelmt == (struct actelmt_type *) NULL)
            nospace(__FILE__, __LINE__);
    }
    actelmt[num_acts].rule_number  = num_rules;
    actelmt[num_acts].start_line   = term->start_line;
    actelmt[num_acts].start_column = term->start_column;
    actelmt[num_acts].end_line     = term->end_line;
    actelmt[num_acts].end_column   = term->end_column;
    actelmt[num_acts].header_block = term->kind == HBLOCK_TK;
    num_acts++;
}

/* bad_symbol ::= EQUIVALENCE */
#line 131 "jikespg.g"
static void bad_first_symbol(void)
{
    PRNTERR2(msg_line, "First symbol: \"%s\" found in file is illegal. Line %ld, column %d", SYM1.name, SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* bad_symbol ::= BLOCK */
#line 152 "jikespg.g"
static void act10(void)
{
    PRNTERR2(msg_line, "Action block cannot be first object in file. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* macro_list ::= macro_name_symbol macro_block */
#line 167 "jikespg.g"
static void act13(void)
{
    if (action_bit) {
        add_macro_definition(SYM1.name, &(SYM2));
    }
}

/* macro_list ::= macro_list macro_name_symbol macro_block */
#line 176 "jikespg.g"
static void act14(void)
{
    if (action_bit) {
        add_macro_definition(SYM2.name, &(SYM3));
    }
}

/* macro_name_symbol ::= SYMBOL */
#line 188 "jikespg.g"
static void act16(void)
{
    PRNTWNG2(msg_line, "Macro name \"%s\" does not start with the escape character. Line %ld, column %d", SYM1.name, SYM1.start_line, SYM1.start_column);
}

/* macro_name_symbol ::= OR */
#line 196 "jikespg.g"
static void bad_macro_name(void)
{
    PRNTERR2(msg_line, "Reserved symbol cannot be used as macro name. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* macro_name_symbol ::= BLOCK */
#line 211 "jikespg.g"
static void act21(void)
{
    PRNTERR2(msg_line, "Macro name not supplied for macro definition. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* macro_name_symbol ::= DEFINE_KEY */
#line 220 "jikespg.g"
static void act22(void)
{
    PRNTERR2(msg_line, "Macro keyword misplaced. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* macro_block ::= OR */
#line 232 "jikespg.g"
static void definition_expected(void)
{
    PRNTERR2(msg_line, "Definition block expected where symbol found. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* terminal_symbol ::= SYMBOL */
#line 258 "jikespg.g"
static void process_terminal(void)
{
    assign_symbol_no(SYM1.name, OMEGA);
}

/* terminal_symbol ::= DEFINE_KEY */
#line 270 "jikespg.g"
static void bad_terminal(void)
{
    PRNTERR2(msg_line, "Keyword  has been misplaced in Terminal section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* terminal_symbol ::= BLOCK */
#line 281 "jikespg.g"
static void act37(void)
{
    PRNTERR2(msg_line, "Misplaced block found in TERMINALS section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* alias_definition ::= alias_lhs produces alias_rhs */
#line 294 "jikespg.g"
static void act39(void)
{
    register int image;
    char tok_string[SYMBOL_SIZE + 1];
    switch(SYM3.kind)
    {
        case EMPTY_SYMBOL_TK:
            image = empty;
            break;
        case SYMBOL_TK:
            assign_symbol_no(SYM3.name, OMEGA);
            image = symbol_image(SYM3.name);
            break;
        case ERROR_SYMBOL_TK:
            if (error_image > num_terminals)
            {
                restore_symbol(tok_string, kerror);
                PRNTERR2(msg_line, "Illegal aliasing to %s prior to its definition.  Line %ld, column %d", tok_string, SYM3.start_line, SYM3.start_column);
                exit(12);
            }
            image = error_image;
            break;
        case EOF_SYMBOL_TK:
            if (eoft_image > num_terminals)
            {
                restore_symbol(tok_string, keoft);
                PRNTERR2(msg_line, "Illegal aliasing to %s prior to its definition. Line %ld, column %d", tok_string, SYM3.start_line, SYM3.start_column);
                exit(12);
            }
            image = eoft_image;
            break;
        case EOL_SYMBOL_TK:
            if (eolt_image == OMEGA)
            {
                PRNTERR2(msg_line, "Illegal aliasing to EOL prior to its definition. Line %ld, column %d", SYM3.start_line, SYM3.start_column);
                exit(12);
            }
            image = eolt_image;
            break;
        default: /* if SYM3.kind == symbol */
            image = symbol_image(SYM3.name);
            break;
    }
    switch(SYM1.kind)
    {
        case SYMBOL_TK:
            if (symbol_image(SYM1.name) != OMEGA)
            {
                restore_symbol(tok_string, SYM1.name);
                PRNTERR2(msg_line, "Symbol %s was previously defined. Line %ld, column %d", tok_string, SYM1.start_line, SYM1.start_column);
                exit(12);
            }
            assign_symbol_no(SYM1.name, image);
            break;
        case ERROR_SYMBOL_TK:
            if (error_image > num_terminals || ! error_maps_bit)
            {
                if (image == empty      || image == eolt_image ||
                    image == eoft_image || image > num_terminals)
                {
                    restore_symbol(tok_string, kerror);
                    PRNTERR2(msg_line, "Illegal alias for symbol %s. Line %ld, column %d.", tok_string, SYM1.start_line, SYM1.start_column);
                    exit(12);
                }
                alias_map(kerror, image);
                error_image = image;
            }
            else
            {
                restore_symbol(tok_string, kerror);
                PRNTERR2(msg_line, "Symbol %s was previously defined. Line %ld, column %d", tok_string, SYM1.start_line, SYM1.start_column);
                exit(12);
            }
            break;
        case EOF_SYMBOL_TK:
            if (eoft_image > num_terminals)
            {
                if (image == empty       || image == eolt_image  ||
                    image == error_image || image > num_terminals)
                {
                    restore_symbol(tok_string, keoft);
                    PRNTERR2(msg_line, "Illegal alias for symbol %s. Line %ld, column %d.", tok_string, SYM1.start_line, SYM1.start_column);
                    exit(12);
                }
                alias_map(keoft, image);
                eoft_image = image;
            }
            else
            {
                restore_symbol(tok_string, keoft);
                PRNTERR2(msg_line, "Symbol %s was previously defined.  %ld, column %d", tok_string, SYM1.start_line, SYM1.start_column);
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
                    PRNTERR2(msg_line, "Illegal alias for symbol EOL. Line %ld, column %d.", SYM1.start_line, SYM1.start_column);
                    exit(12);
                }
                eolt_image = image;
            }
            else
            {
                PRNTERR2(msg_line, "Symbol EOL was previously defined. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
                exit(12);
            }
            break;
    }
}

/* bad_alias_rhs ::= DEFINE_KEY */
#line 454 "jikespg.g"
static void bad_alias_rhs(void)
{
    PRNTERR2(msg_line, "Misplaced keyword found in Alias section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* bad_alias_rhs ::= BLOCK */
#line 467 "jikespg.g"
static void act57(void)
{
    PRNTERR2(msg_line, "Misplaced block found in Alias section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* bad_alias_lhs ::= EMPTY_SYMBOL */
#line 480 "jikespg.g"
static void act59(void)
{
    PRNTERR2(msg_line, "Empty symbol cannot be aliased. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* bad_alias_lhs ::= produces */
#line 489 "jikespg.g"
static void missing_quote(void)
{
    PRNTERR2(msg_line, "Symbol must be quoted when used as a grammar symbol. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* start_symbol ::= SYMBOL */
#line 505 "jikespg.g"
static void act63(void)
{
    assign_symbol_no(SYM1.name, OMEGA);
    register struct node *q = Allocate_node();
    q -> value = symbol_image(SYM1.name);
    if (start_symbol_root == NULL) {
        q -> next = q;
    } else
    {
        q -> next = start_symbol_root -> next;
        start_symbol_root -> next = q;
    }
    start_symbol_root = q;
    num_rules++;
    num_items++;
    SHORT_CHECK(num_items);
}

/* start_symbol ::= OR */
#line 526 "jikespg.g"
static void bad_start_symbol(void)
{
    PRNTERR2(msg_line, "Symbol cannot be used as Start symbol. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* start_symbol ::= BLOCK */
#line 541 "jikespg.g"
static void act68(void)
{
    PRNTERR2(msg_line, "Misplaced block found in Start section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* start_symbol ::= DEFINE_KEY */
#line 550 "jikespg.g"
static void misplaced_keyword_found_in_START_section(void)
{
    PRNTERR2(msg_line, "Misplaced keyword found in START section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* rules_block ::= RULES_KEY */
#line 566 "jikespg.g"
static void act73(void)
{
    if (start_symbol_root == NULL)
    {
        register struct node *q = Allocate_node();
        q -> value = empty;
        q -> next = q;
        start_symbol_root = q;
        num_rules = 0;                 /* One rule */
        num_items = 0;                 /* 0 items */
    }
    build_symno();
}

/* rules_block ::= RULES_KEY rule_list */
#line 584 "jikespg.g"
static void act74(void)
{
    build_symno();
}

/* rule_list ::= {action_block} SYMBOL produces */
#line 598 "jikespg.g"
static void act77(void)
{
    assign_symbol_no(SYM2.name, OMEGA);
    if (start_symbol_root == NULL)
    {
        register struct node *q = Allocate_node();
        q -> value = symbol_image(SYM2.name);
        q -> next = q;
        start_symbol_root = q;
        num_rules = 1;
        num_items = 1;
    }
/* Since we don't know for sure how many start symbols we have, a    */
/* "while" loop is used to increment the size of rulehdr. However,   */
/* it is highly unlikely that this loop would ever execute more than */
/* once if the size of RULE_INCREMENT is reasonable.                 */
    while (num_rules >= (int)rulehdr_size)
    {
        rulehdr_size += RULEHDR_INCREMENT;
        rulehdr = (struct rulehdr_type *)
            (rulehdr == (struct rulehdr_type *) NULL
             ? malloc(rulehdr_size * sizeof(struct rulehdr_type))
             : realloc(rulehdr, rulehdr_size * sizeof(struct rulehdr_type)));
        if (rulehdr == (struct rulehdr_type *) NULL)
            nospace(__FILE__, __LINE__);
    }
    rulehdr[num_rules].sp = ((SYM3.kind == ARROW_TK) ? true : false);
    rulehdr[num_rules].lhs = symbol_image(SYM2.name);
    rulehdr[num_rules].rhs_root = NULL;
}

/* rule_list ::= rule_list OR */
#line 637 "jikespg.g"
static void act78(void)
{
    num_rules++;
    if (num_rules >= (int)rulehdr_size)
    {
        rulehdr_size += RULEHDR_INCREMENT;
        rulehdr = (struct rulehdr_type *)
            (rulehdr == (struct rulehdr_type *) NULL
             ? malloc(rulehdr_size * sizeof(struct rulehdr_type))
             : realloc(rulehdr, rulehdr_size * sizeof(struct rulehdr_type)));
        if (rulehdr == (struct rulehdr_type *) NULL)
            nospace(__FILE__, __LINE__);
    }
    rulehdr[num_rules].sp = rulehdr[num_rules - 1].sp;
    rulehdr[num_rules].lhs = OMEGA;
    rulehdr[num_rules].rhs_root = NULL;
}

/* rule_list ::= rule_list SYMBOL produces */
#line 658 "jikespg.g"
static void act79(void)
{
    num_rules++;
    if (num_rules >= (int)rulehdr_size)
    {
        rulehdr_size += RULEHDR_INCREMENT;
        rulehdr = (struct rulehdr_type *)
            (rulehdr == (struct rulehdr_type *) NULL
             ? malloc(rulehdr_size * sizeof(struct rulehdr_type))
             : realloc(rulehdr, rulehdr_size * sizeof(struct rulehdr_type)));
        if (rulehdr == (struct rulehdr_type *) NULL)
            nospace(__FILE__, __LINE__);
    }
    rulehdr[num_rules].sp = ((SYM3.kind == ARROW_TK) ? true : false);
    assign_symbol_no(SYM2.name, OMEGA);
    rulehdr[num_rules].lhs = symbol_image(SYM2.name);
    rulehdr[num_rules].rhs_root = NULL;
}

/* rule_list ::= rule_list ERROR_SYMBOL */
#line 685 "jikespg.g"
static void act82(void)
{
    if (error_image == DEFAULT_SYMBOL)
    {
        char tok_string[SYMBOL_SIZE + 1];
        restore_symbol(tok_string, kerror);
        PRNTERR2(msg_line, "%s not declared or aliased to terminal symbol. Line %ld, column %d", tok_string, SYM2.start_line, SYM2.start_column);
        exit(12);
    }
    register struct node *q = Allocate_node();
    q -> value = error_image;
    num_items++;
    SHORT_CHECK(num_items);
    if (rulehdr[num_rules].rhs_root == NULL) {
        q -> next = q;
    } else
    {
        q -> next = rulehdr[num_rules].rhs_root -> next;
         rulehdr[num_rules].rhs_root -> next = q;
    }
    rulehdr[num_rules].rhs_root = q;
}

/* rule_list ::= rule_list SYMBOL */
#line 711 "jikespg.g"
static void act83(void)
{
    assign_symbol_no(SYM2.name, OMEGA);
    register int sym = symbol_image(SYM2.name);
    if (sym != empty)
    {
        if (sym == eoft_image)
        {
            PRNTERR2(msg_line, "End-of-file symbol cannot be used in rule. Line %ld, column %d", SYM2.start_line, SYM2.start_column);
            exit(12);
        }
        register struct node *q = Allocate_node();
        q -> value = sym;
        num_items++;
        SHORT_CHECK(num_items);
        if (rulehdr[num_rules].rhs_root == NULL) {
            q -> next = q;
        } else
        {
            q -> next = rulehdr[num_rules].rhs_root -> next;
            rulehdr[num_rules].rhs_root -> next = q;
        }
        rulehdr[num_rules].rhs_root = q;
    }
}

/* rule_list ::= OR */
#line 740 "jikespg.g"
static void bad_first_symbol_in_RULES_section(void)
{
    PRNTERR2(msg_line, "First symbol in Rules section is not a valid left-hand side.\n Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* rule_list ::= rule_list OR produces */
#line 755 "jikespg.g"
static void rule_without_left_hand_side(void)
{
    PRNTERR2(msg_line, "Rule without left-hand-side.  Line %ld, column %d", SYM3.start_line, SYM3.start_column);
    exit(12);
}

/* rule_list ::= rule_list keyword produces */
#line 768 "jikespg.g"
static void act91(void)
{
    sprintf(msg_line, "Misplaced keyword found in Rules section Line %ld, column %d",  SYM2.start_line, SYM2.start_column);
    exit(12);
}

/* action_block ::= BLOCK */
#line 778 "jikespg.g"
static void act92(void)
{
    if (action_bit) {
        add_block_definition(&(SYM1));
    }
}

/* action_block ::= HBLOCK */
#line 787 "jikespg.g"
static void act93(void)
{
    if (action_bit) {
        add_block_definition(&(SYM1));
    }
}

/* keyword ::= DEFINE_KEY */
#line 797 "jikespg.g"
static void misplaced_keyword_found_in_RULES_section(void)
{
    sprintf(msg_line,"Misplaced keyword found in RULES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* names_definition ::= name produces name */
#line 818 "jikespg.g"
static void act100(void)
{
    if (error_maps_bit)
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
                symbol = symbol_image(SYM1.name);
                break;
        }
        if (symbol == OMEGA)
        {
            PRNTERR2(msg_line, "Symbol %s is undefined. Line %ld, column %d", SYM1.name, SYM1.start_line, SYM1.start_column);
            exit(12);
        }
        if (symno[symbol].name_index != OMEGA)
        {
            PRNTERR2(msg_line, "Symbol %s has been named more than once. Line %ld, column %d.", SYM1.name, SYM1.start_line, SYM1.start_column);
            exit(12);
        }
         symno[symbol].name_index = name_map(SYM3.name);
     }
}

/* bad_name ::= DEFINE_KEY */
#line 885 "jikespg.g"
static void misplaced_keyword_found_in_NAMES_section(void)
{
    PRNTERR2(msg_line, "Keyword  has been misplaced in NAMES section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* bad_name ::= BLOCK */
#line 904 "jikespg.g"
static void act116(void)
{
    PRNTERR2(msg_line, "Misplaced action block found in NAMES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* bad_name ::= MACRO_NAME */
#line 913 "jikespg.g"
static void act117(void)
{
    PRNTERR2(msg_line, "Misplaced macro name found in NAMES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/* [terminals_block] ::= */
#line 930 "jikespg.g"
static void process_TERMINALS_section(void)
{
    num_terminals = num_symbols;
    assign_symbol_no(keoft, OMEGA);
    eoft_image = symbol_image(keoft);
    if (error_maps_bit)
    {
        assign_symbol_no(kerror, OMEGA);
        error_image = symbol_image(kerror);
    }
    else error_image = DEFAULT_SYMBOL;   /* should be 0 */
    assign_symbol_no(kaccept, OMEGA);
    accept_image = symbol_image(kaccept);
}

/* [alias_block] ::= */
#line 953 "jikespg.g"
static void process_ALIAS_section(void)
{
    register int k = 0;
    if (eoft_image <= num_terminals) {
        k++;
    } else {
        num_terminals++;
    }
    if (error_maps_bit)
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
        for (register int i = 0; i < HT_SIZE; i++)
        {
            register struct hash_type* p = hash_table[i];
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
        alias_map(kerror, DEFAULT_SYMBOL);
}

/* {terminal_symbol} ::= */
#line 1025 "jikespg.g"
static void act132(void)
{
    assign_symbol_no(kempty, OMEGA);
    empty = symbol_image(kempty);
}
