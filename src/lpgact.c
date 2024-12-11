#line 85 "jikespg.g"
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

/// bad_symbol ::= EQUIVALENCE
#line 151 "jikespg.g"
static void bad_first_symbol(void)
{
    PRNTERR2("First symbol: \"%s\" found in file is illegal. Line %ld, column %d", SYM1.name, SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_symbol ::= BLOCK
#line 172 "jikespg.g"
static void act10(void)
{
    PRNTERR2("Action block cannot be first object in file. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_list ::= macro_name_symbol macro_block
#line 187 "jikespg.g"
static void act13(void)
{
    add_macro_definition(SYM1.name, &(SYM2));
}

/// macro_list ::= macro_list macro_name_symbol macro_block
#line 195 "jikespg.g"
static void act14(void)
{
    add_macro_definition(SYM2.name, &(SYM3));
}

/// macro_name_symbol ::= SYMBOL
#line 206 "jikespg.g"
static void act16(void)
{
    PRNTWNG2("Macro name \"%s\" does not start with the escape character. Line %ld, column %d", SYM1.name, SYM1.start_line, SYM1.start_column);
}

/// macro_name_symbol ::= OR
#line 214 "jikespg.g"
static void bad_macro_name(void)
{
    PRNTERR2("Reserved symbol cannot be used as macro name. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_name_symbol ::= BLOCK
#line 229 "jikespg.g"
static void act21(void)
{
    PRNTERR2("Macro name not supplied for macro definition. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_name_symbol ::= DEFINE_KEY
#line 238 "jikespg.g"
static void act22(void)
{
    PRNTERR2("Macro keyword misplaced. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// macro_block ::= OR
#line 250 "jikespg.g"
static void definition_expected(void)
{
    PRNTERR2("Definition block expected where symbol found. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// terminal_symbol ::= SYMBOL
#line 276 "jikespg.g"
static void process_terminal(void)
{
    assign_symbol_no(SYM1.name, OMEGA);
}

/// terminal_symbol ::= DEFINE_KEY
#line 288 "jikespg.g"
static void bad_terminal(void)
{
    PRNTERR2("Keyword  has been misplaced in Terminal section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// terminal_symbol ::= BLOCK
#line 299 "jikespg.g"
static void act37(void)
{
    PRNTERR2("Misplaced block found in TERMINALS section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// alias_definition ::= alias_lhs produces alias_rhs
#line 312 "jikespg.g"
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
                PRNTERR2("Illegal aliasing to %s prior to its definition.  Line %ld, column %d", tok_string, SYM3.start_line, SYM3.start_column);
                exit(12);
            }
            image = error_image;
            break;

        case EOF_SYMBOL_TK:
            if (eoft_image > num_terminals)
            {
                restore_symbol(tok_string, keoft);
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
            image = symbol_image(SYM3.name);
            break;
    }

    switch(SYM1.kind)
    {
        case SYMBOL_TK:
            if (symbol_image(SYM1.name) != OMEGA)
            {
                restore_symbol(tok_string, SYM1.name);
                PRNTERR2("Symbol %s was previously defined. Line %ld, column %d", tok_string, SYM1.start_line, SYM1.start_column);
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
                    PRNTERR2("Illegal alias for symbol %s. Line %ld, column %d.", tok_string, SYM1.start_line, SYM1.start_column);
                    exit(12);
                }
                alias_map(kerror, image);
                error_image = image;
            }
            else
            {
                restore_symbol(tok_string, kerror);
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
                    restore_symbol(tok_string, keoft);
                    PRNTERR2("Illegal alias for symbol %s. Line %ld, column %d.", tok_string, SYM1.start_line, SYM1.start_column);
                    exit(12);
                }
                alias_map(keoft, image);
                eoft_image = image;
            }
            else
            {
                restore_symbol(tok_string, keoft);
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
#line 472 "jikespg.g"
static void bad_alias_rhs(void)
{
    PRNTERR2("Misplaced keyword found in Alias section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_alias_rhs ::= BLOCK
#line 485 "jikespg.g"
static void act57(void)
{
    PRNTERR2("Misplaced block found in Alias section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_alias_lhs ::= EMPTY_SYMBOL
#line 498 "jikespg.g"
static void act59(void)
{
    PRNTERR2("Empty symbol cannot be aliased. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_alias_lhs ::= produces
#line 507 "jikespg.g"
static void missing_quote(void)
{
    PRNTERR2("Symbol must be quoted when used as a grammar symbol. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// start_symbol ::= SYMBOL
#line 523 "jikespg.g"
static void act63(void)
{
    assign_symbol_no(SYM1.name, OMEGA);
    register struct node *q = Allocate_node();
    q -> value = symbol_image(SYM1.name);
    if (start_symbol_root == NULL)
        q -> next = q;
    else
    {
        q -> next = start_symbol_root -> next;
        start_symbol_root -> next = q;
    }
    start_symbol_root = q;
    num_rules++;
    num_items++;
}

/// start_symbol ::= OR
#line 544 "jikespg.g"
static void bad_start_symbol(void)
{
    PRNTERR2("Symbol cannot be used as Start symbol. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// start_symbol ::= BLOCK
#line 559 "jikespg.g"
static void act68(void)
{
    PRNTERR2("Misplaced block found in Start section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// start_symbol ::= DEFINE_KEY
#line 568 "jikespg.g"
static void misplaced_keyword_found_in_START_section(void)
{
    PRNTERR2("Misplaced keyword found in START section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// rules_block ::= RULES_KEY
#line 584 "jikespg.g"
static void act73(void)
{

    if (start_symbol_root == NULL)
    {
        register struct node *q = Allocate_node();
        q -> value = empty;
        q -> next = q;
        start_symbol_root = q;
        num_rules = 0;                 // One rule
        num_items = 0;                 // 0 items
    }
    build_symno();
}

/// rules_block ::= RULES_KEY rule_list
#line 602 "jikespg.g"
static void act74(void)
{
    build_symno();
}

/// rule_list ::= {action_block} SYMBOL produces
#line 616 "jikespg.g"
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

/// Since we don't know for sure how many start symbols we have, a
/// "while" loop is used to increment the size of rulehdr. However,
/// it is highly unlikely that this loop would ever execute more than
/// once if the size of RULE_INCREMENT is reasonable.
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

/// rule_list ::= rule_list OR
#line 655 "jikespg.g"
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

/// rule_list ::= rule_list SYMBOL produces
#line 676 "jikespg.g"
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

/// rule_list ::= rule_list ERROR_SYMBOL
#line 703 "jikespg.g"
static void act82(void)
{
    if (error_image == DEFAULT_SYMBOL)
    {
        char tok_string[SYMBOL_SIZE + 1];
        restore_symbol(tok_string, kerror);
        PRNTERR2("%s not declared or aliased to terminal symbol. Line %ld, column %d", tok_string, SYM2.start_line, SYM2.start_column);
        exit(12);
    }
    register struct node *q = Allocate_node();
    q -> value = error_image;
    num_items++;
    if (rulehdr[num_rules].rhs_root == NULL)
        q -> next = q;
    else
    {
        q -> next = rulehdr[num_rules].rhs_root -> next;
         rulehdr[num_rules].rhs_root -> next = q;
    }
    rulehdr[num_rules].rhs_root = q;
}

/// rule_list ::= rule_list SYMBOL
#line 729 "jikespg.g"
static void act83(void)
{
    assign_symbol_no(SYM2.name, OMEGA);
    register int sym = symbol_image(SYM2.name);
    if (sym != empty)
    {
        if (sym == eoft_image)
        {
            PRNTERR2("End-of-file symbol cannot be used in rule. Line %ld, column %d", SYM2.start_line, SYM2.start_column);
            exit(12);
        }
        register struct node *q = Allocate_node();
        q -> value = sym;
        num_items++;
        if (rulehdr[num_rules].rhs_root == NULL)
            q -> next = q;
        else
        {
            q -> next = rulehdr[num_rules].rhs_root -> next;
            rulehdr[num_rules].rhs_root -> next = q;
        }
        rulehdr[num_rules].rhs_root = q;
    }
}

/// rule_list ::= OR
#line 758 "jikespg.g"
static void bad_first_symbol_in_RULES_section(void)
{
    PRNTERR2("First symbol in Rules section is not a valid left-hand side.\n Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// rule_list ::= rule_list OR produces
#line 773 "jikespg.g"
static void rule_without_left_hand_side(void)
{
    PRNTERR2("Rule without left-hand-side.  Line %ld, column %d", SYM3.start_line, SYM3.start_column);
    exit(12);
}

/// rule_list ::= rule_list keyword produces
#line 786 "jikespg.g"
static void act91(void)
{
    PRNTWNG2("Misplaced keyword found in Rules section Line %ld, column %d",  SYM2.start_line, SYM2.start_column);
    exit(12);
}

/// action_block ::= BLOCK
#line 796 "jikespg.g"
static void act92(void)
{
    add_block_definition(&(SYM1));
}

/// action_block ::= HBLOCK
#line 804 "jikespg.g"
static void act93(void)
{
    add_block_definition(&(SYM1));
}

/// keyword ::= DEFINE_KEY
#line 813 "jikespg.g"
static void misplaced_keyword_found_in_RULES_section(void)
{
    PRNTWNG2("Misplaced keyword found in RULES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// names_definition ::= name produces name
#line 834 "jikespg.g"
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
            PRNTERR2("Symbol %s is undefined. Line %ld, column %d", SYM1.name, SYM1.start_line, SYM1.start_column);
            exit(12);
        }

        if (symno[symbol].name_index != OMEGA)
        {
            PRNTERR2("Symbol %s has been named more than once. Line %ld, column %d.", SYM1.name, SYM1.start_line, SYM1.start_column);
            exit(12);
        }
         symno[symbol].name_index = name_map(SYM3.name);
     }
}

/// bad_name ::= DEFINE_KEY
#line 901 "jikespg.g"
static void misplaced_keyword_found_in_NAMES_section(void)
{
    PRNTERR2("Keyword  has been misplaced in NAMES section.  Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_name ::= BLOCK
#line 920 "jikespg.g"
static void act116(void)
{
    PRNTERR2("Misplaced action block found in NAMES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// bad_name ::= MACRO_NAME
#line 929 "jikespg.g"
static void act117(void)
{
    PRNTERR2("Misplaced macro name found in NAMES section. Line %ld, column %d", SYM1.start_line, SYM1.start_column);
    exit(12);
}

/// [terminals_block] ::=
#line 946 "jikespg.g"
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
    else error_image = DEFAULT_SYMBOL;   // should be 0

    assign_symbol_no(kaccept, OMEGA);
    accept_image = symbol_image(kaccept);
}

/// [alias_block] ::=
#line 969 "jikespg.g"
static void process_ALIAS_section(void)
{

    register int k = 0;
    if (eoft_image <= num_terminals)
        k++;
    else
        num_terminals++;

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

/// {terminal_symbol} ::=
#line 1041 "jikespg.g"
static void act132(void)
{
    assign_symbol_no(kempty, OMEGA);
    empty = symbol_image(kempty);
}
