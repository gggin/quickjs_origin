_false, label1);
            emit_op(s, OP_drop);
            
            if (js_parse_expr_binary(s, 8, parse_flags & ~PF_ARROW_FUNC))
                return -1;
            if (s->token.val != TOK_DOUBLE_QUESTION_MARK)
                break;
        }
        emit_label(s, label1);
    }
    return 0;
}

/* allowed parse_flags: PF_ARROW_FUNC, PF_IN_ACCEPTED */
static __exception int js_parse_cond_expr(JSParseState *s, int parse_flags)
{
    int label1, label2;

    if (js_parse_coalesce_expr(s, parse_flags))
        return -1;
    if (s->token.val == '?') {
        if (next_token(s))
            return -1;
        label1 = emit_goto(s, OP_if_false, -1);

        if (js_parse_assign_expr(s))
            return -1;
        if (js_parse_expect(s, ':'))
            return -1;

        label2 = emit_goto(s, OP_goto, -1);

        emit_label(s, label1);

        if (js_parse_assign_expr2(s, parse_flags & PF_IN_ACCEPTED))
            return -1;

        emit_label(s, label2);
    }
    return 0;
}

static void emit_return(JSParseState *s, BOOL hasval);

/* allowed parse_flags: PF_IN_ACCEPTED */
static __exception int js_parse_assign_expr2(JSParseState *s, int parse_flags)
{
    int opcode, op, scope;
    JSAtom name0 = JS_ATOM_NULL;
    JSAtom name;

    if (s->token.val == TOK_YIELD) {
        BOOL is_star = FALSE, is_async;
        
        if (!(s->cur_func->func_kind & JS_FUNC_GENERATOR))
            return js_parse_error(s, "unexpected 'yield' keyword");
        if (!s->cur_func->in_function_body)
            return js_parse_error(s, "yield in default expression");
        if (next_token(s))
            return -1;
        /* XXX: is there a better method to detect 'yield' without
           parameters ? */
        if (s->token.val != ';' && s->token.val != ')' &&
            s->token.val != ']' && s->token.val != '}' &&
            s->token.val != ',' && s->token.val != ':' && !s->got_lf) {
            if (s->token.val == '*') {
                is_star = TRUE;
                if (next_token(s))
                    return -1;
            }
            if (js_parse_assign_expr2(s, parse_flags))
                return -1;
        } else {
            emit_op(s, OP_undefined);
        }
        is_async = (s->cur_func->func_kind == JS_FUNC_ASYNC_GENERATOR);

        if (is_star) {
            int label_loop, label_return, label_next;
            int label_return1, label_yield, label_throw, label_throw1;
            int label_throw2;

            label_loop = new_label(s);
            label_yield = new_label(s);

            emit_op(s, is_async ? OP_for_await_of_start : OP_for_of_start);

            /* remove the catch offset (XXX: could avoid pushing back
               undefined) */
            emit_op(s, OP_drop);
            emit_op(s, OP_undefined);
            
            emit_op(s, OP_undefined); /* initial value */
            
            emit_label(s, label_loop);
            emit_op(s, OP_iterator_next);
            if (is_async)
                emit_op(s, OP_await);
            emit_op(s, OP_iterator_check_object);
            emit_op(s, OP_get_field2);
            emit_atom(s, JS_ATOM_done);
            label_next = emit_goto(s, OP_if_true, -1); /* end of loop */
            emit_label(s, label_yield);
            if (is_async) {
                /* OP_async_yield_star takes the value as parameter */
                emit_op(s, OP_get_field);
                emit_atom(s, JS_ATOM_value);
                emit_op(s, OP_await);
                emit_op(s, OP_async_yield_star);
            } else {
                /* OP_yield_star takes (value, done) as parameter */
                emit_op(s, OP_yield_star);
            }
            emit_op(s, OP_dup);
            label_return = emit_goto(s, OP_if_true, -1);
            emit_op(s, OP_drop);
            emit_goto(s, OP_goto, label_loop);
            
            emit_label(s, label_return);
            emit_op(s, OP_push_i32);
            emit_u32(s, 2);
            emit_op(s, OP_strict_eq);
            label_throw = emit_goto(s, OP_if_true, -1);
            
            /* return handling */
            if (is_async)
                emit_op(s, OP_await);
            emit_op(s, OP_iterator_call);
            emit_u8(s, 0);
            label_return1 = emit_goto(s, OP_if_true, -1);
            if (is_async)
                emit_op(s, OP_await);
            emit_op(s, OP_iterator_check_object);
            emit_op(s, OP_get_field2);
            emit_atom(s, JS_ATOM_done);
            emit_goto(s, OP_if_false, label_yield);

            emit_op(s, OP_get_field);
            emit_atom(s, JS_ATOM_value);
            
            emit_label(s, label_return1);
            emit_op(s, OP_nip);
            emit_op(s, OP_nip);
            emit_op(s, OP_nip);
            emit_return(s, TRUE);
            
            /* throw handling */
            emit_label(s, label_throw);
            emit_op(s, OP_iterator_call);
            emit_u8(s, 1);
            label_throw1 = emit_goto(s, OP_if_true, -1);
            if (is_async)
                emit_op(s, OP_await);
            emit_op(s, OP_iterator_check_object);
            emit_op(s, OP_get_field2);
            emit_atom(s, JS_ATOM_done);
            emit_goto(s, OP_if_false, label_yield);
            emit_goto(s, OP_goto, label_next);
            /* close the iterator and throw a type error exception */
            emit_label(s, label_throw1);
            emit_op(s, OP_iterator_call);
            emit_u8(s, 2);
            label_throw2 = emit_goto(s, OP_if_true, -1);
            if (is_async)
                emit_op(s, OP_await);
            emit_label(s, label_throw2);

            emit_op(s, OP_throw_error);
            emit_atom(s, JS_ATOM_NULL);
            emit_u8(s, JS_THROW_ERROR_ITERATOR_THROW);
            
            emit_label(s, label_next);
            emit_op(s, OP_get_field);
            emit_atom(s, JS_ATOM_value);
            emit_op(s, OP_nip); /* keep the value associated with
                                   done = true */
            emit_op(s, OP_nip);
            emit_op(s, OP_nip);
        } else {
            int label_next;
            
            if (is_async)
                emit_op(s, OP_await);
            emit_op(s, OP_yield);
            label_next = emit_goto(s, OP_if_false, -1);
            emit_return(s, TRUE);
            emit_label(s, label_next);
        }
        return 0;
    }
    if (s->token.val == TOK_IDENT) {
        /* name0 is used to check for OP_set_name pattern, not duplicated */
        name0 = s->token.u.ident.atom;
    }
    if (js_parse_cond_expr(s, parse_flags | PF_ARROW_FUNC))
        return -1;

    op = s->token.val;
    if (op == '=' || (op >= TOK_MUL_ASSIGN && op <= TOK_POW_ASSIGN)) {
        int label;
        if (next_token(s))
            return -1;
        if (get_lvalue(s, &opcode, &scope, &name, &label, NULL, (op != '='), op) < 0)
            return -1;

        if (js_parse_assign_expr2(s, parse_flags)) {
            JS_FreeAtom(s->ctx, name);
            return -1;
        }

        if (op == '=') {
            if (opcode == OP_get_ref_value && name == name0) {
                set_object_name(s, name);
            }
        } else {
            static const uint8_t assign_opcodes[] = {
                OP_mul, OP_div, OP_mod, OP_add, OP_sub,
                OP_shl, OP_sar, OP_shr, OP_and, OP_xor, OP_or,
#ifdef CONFIG_BIGNUM
                OP_pow,
#endif
                OP_pow,
            };
            op = assign_opcodes[op - TOK_MUL_ASSIGN];
#ifdef CONFIG_BIGNUM
            if (s->cur_func->js_mode & JS_MODE_MATH) {
                if (op == OP_mod)
                    op = OP_math_mod;
            }
#endif
            emit_op(s, op);
        }
        put_lvalue(s, opcode, scope, name, label, PUT_LVALUE_KEEP_TOP, FALSE);
    } else if (op >= TOK_LAND_ASSIGN && op <= TOK_DOUBLE_QUESTION_MARK_ASSIGN) {
        int label, label1, depth_lvalue, label2;
        
        if (next_token(s))
            return -1;
        if (get_lvalue(s, &opcode, &scope, &name, &label,
                       &depth_lvalue, TRUE, op) < 0)
            return -1;

        emit_op(s, OP_dup);
        if (op == TOK_DOUBLE_QUESTION_MARK_ASSIGN)
            emit_op(s, OP_is_undefined_or_null);
        label1 = emit_goto(s, op == TOK_LOR_ASSIGN ? OP_if_true : OP_if_false,
                           -1);
        emit_op(s, OP_drop);
        
        if (js_parse_assign_expr2(s, parse_flags)) {
            JS_FreeAtom(s->ctx, name);
            return -1;
        }

        if (opcode == OP_get_ref_value && name == name0) {
            set_object_name(s, name);
        }
        
        switch(depth_lvalue) {
        case 1:
            emit_op(s, OP_insert2);
            break;
        case 2:
            emit_op(s, OP_insert3);
            break;
        case 3:
            emit_op(s, OP_insert4);
            break;
        default:
            abort();
        }

        /* XXX: we disable the OP_put_ref_value optimization by not
           using put_lvalue() otherwise depth_lvalue is not correct */
        put_lvalue(s, opcode, scope, name, label, PUT_LVALUE_NOKEEP_DEPTH,
                   FALSE);
        label2 = emit_goto(s, OP_goto, -1);
        
        emit_label(s, label1);

        /* remove the lvalue stack entries */
        while (depth_lvalue != 0) {
            emit_op(s, OP_nip);
            depth_lvalue--;
        }

        emit_label(s, label2);
    }
    return 0;
}

static __exception int js_parse_assign_expr(JSParseState *s)
{
    return js_parse_assign_expr2(s, PF_IN_ACCEPTED);
}

/* allowed parse_flags: PF_IN_ACCEPTED */
static __exception int js_parse_expr2(JSParseState *s, int parse_flags)
{
    BOOL comma = FALSE;
    for(;;) {
        if (js_parse_assign_expr2(s, parse_flags))
            return -1;
        if (comma) {
            /* prevent get_lvalue from using the last expression
               as an lvalue. This also prevents the conversion of
               of get_var to get_ref for method lookup in function
               call inside `with` statement.
             */
            s->cur_func->last_opcode_pos = -1;
        }
        if (s->token.val != ',')
            break;
        comma = TRUE;
        if (next_token(s))
            return -1;
        emit_op(s, OP_drop);
    }
    return 0;
}

static __exception int js_parse_expr(JSParseState *s)
{
    return js_parse_expr2(s, PF_IN_ACCEPTED);
}

static void push_break_entry(JSFunctionDef *fd, BlockEnv *be,
                             JSAtom label_name,
                             int label_break, int label_cont,
                             int drop_count)
{
    be->prev = fd->top_break;
    fd->top_break = be;
    be->label_name = label_name;
    be->label_break = label_break;
    be->label_cont = label_cont;
    be->drop_count = drop_count;
    be->label_finally = -1;
    be->scope_level = fd->scope_level;
    be->has_iterator = FALSE;
}

static void pop_break_entry(JSFunctionDef *fd)
{
    BlockEnv *be;
    be = fd->top_break;
    fd->top_break = be->prev;
}

static __exception int emit_break(JSParseState *s, JSAtom name, int is_cont)
{
    BlockEnv *top;
    int i, scope_level;

    scope_level = s->cur_func->scope_level;
    top = s->cur_func->top_break;
    while (top != NULL) {
        close_scopes(s, scope_level, top->scope_level);
        scope_level = top->scope_level;
        if (is_cont &&
            top->label_cont != -1 &&
            (name == JS_ATOM_NULL || top->label_name == name)) {
            /* continue stays inside the same block */
            emit_goto(s, OP_goto, top->label_cont);
            return 0;
        }
        if (!is_cont &&
            top->label_break != -1 &&
            (name == JS_ATOM_NULL || top->label_name == name)) {
            emit_goto(s, OP_goto, top->label_break);
            return 0;
        }
        i = 0;
        if (top->has_iterator) {
            emit_op(s, OP_iterator_close);
            i += 3;
        }
        for(; i < top->drop_count; i++)
            emit_op(s, OP_drop);
        if (top->label_finally != -1) {
            /* must push dummy value to keep same stack depth */
            emit_op(s, OP_undefined);
            emit_goto(s, OP_gosub, top->label_finally);
            emit_op(s, OP_drop);
        }
        top = top->prev;
    }
    if (name == JS_ATOM_NULL) {
        if (is_cont)
            return js_parse_error(s, "continue must be inside loop");
        else
            return js_parse_error(s, "break must be inside loop or switch");
    } else {
        return js_parse_error(s, "break/continue label not found");
    }
}

/* execute the finally blocks before return */
static void emit_return(JSParseState *s, BOOL hasval)
{
    BlockEnv *top;
    int drop_count;

    drop_count = 0;
    top = s->cur_func->top_break;
    while (top != NULL) {
        /* XXX: emit the appropriate OP_leave_scope opcodes? Probably not
           required as all local variables will be closed upon returning
           from JS_CallInternal, but not in the same order. */
        if (top->has_iterator) {
            /* with 'yield', the exact number of OP_drop to emit is
               unknown, so we use a specific operation to look for
               the catch offset */
            if (!hasval) {
                emit_op(s, OP_undefined);
                hasval = TRUE;
            }
            emit_op(s, OP_iterator_close_return);
            if (s->cur_func->func_kind == JS_FUNC_ASYNC_GENERATOR) {
                int label_next, label_next2;

                emit_op(s, OP_drop); /* catch offset */
                emit_op(s, OP_drop); /* next */
                emit_op(s, OP_get_field2);
                emit_atom(s, JS_ATOM_return);
                /* stack: iter_obj return_func */
                emit_op(s, OP_dup);
                emit_op(s, OP_is_undefined_or_null);
                label_next = emit_goto(s, OP_if_true, -1);
                emit_op(s, OP_call_method);
                emit_u16(s, 0);
                emit_op(s, OP_iterator_check_object);
                emit_op(s, OP_await);
                label_next2 = emit_goto(s, OP_goto, -1);
                emit_label(s, label_next);
                emit_op(s, OP_drop);
                emit_label(s, label_next2);
                emit_op(s, OP_drop);
            } else {
                emit_op(s, OP_iterator_close);
            }
            drop_count = -3;
        }
        drop_count += top->drop_count;
        if (top->label_finally != -1) {
            while(drop_count) {
                /* must keep the stack top if hasval */
                emit_op(s, hasval ? OP_nip : OP_drop);
                drop_count--;
            }
            if (!hasval) {
                /* must push return value to keep same stack size */
                emit_op(s, OP_undefined);
                hasval = TRUE;
            }
            emit_goto(s, OP_gosub, top->label_finally);
        }
        top = top->prev;
    }
    if (s->cur_func->is_derived_class_constructor) {
        int label_return;

        /* 'this' can be uninitialized, so it may be accessed only if
           the derived class constructor does not return an object */
        if (hasval) {
            emit_op(s, OP_check_ctor_return);
            label_return = emit_goto(s, OP_if_false, -1);
            emit_op(s, OP_drop);
        } else {
            label_return = -1;
        }

        /* XXX: if this is not initialized, should throw the
           ReferenceError in the caller realm */
        emit_op(s, OP_scope_get_var);
        emit_atom(s, JS_ATOM_this);
        emit_u16(s, 0);

        emit_label(s, label_return);
        emit_op(s, OP_return);
    } else if (s->cur_func->func_kind != JS_FUNC_NORMAL) {
        if (!hasval) {
            emit_op(s, OP_undefined);
        } else if (s->cur_func->func_kind == JS_FUNC_ASYNC_GENERATOR) {
            emit_op(s, OP_await);
        }
        emit_op(s, OP_return_async);
    } else {
        emit_op(s, hasval ? OP_return : OP_return_undef);
    }
}

#define DECL_MASK_FUNC  (1 << 0) /* allow normal function declaration */
/* ored with DECL_MASK_FUNC if function declarations are allowed with a label */
#define DECL_MASK_FUNC_WITH_LABEL (1 << 1)
#define DECL_MASK_OTHER (1 << 2) /* all other declarations */
#define DECL_MASK_ALL   (DECL_MASK_FUNC | DECL_MASK_FUNC_WITH_LABEL | DECL_MASK_OTHER)

static __exception int js_parse_statement_or_decl(JSParseState *s,
                                                  int decl_mask);

static __exception int js_parse_statement(JSParseState *s)
{
    return js_parse_statement_or_decl(s, 0);
}

static __exception int js_parse_block(JSParseState *s)
{
    if (js_parse_expect(s, '{'))
        return -1;
    if (s->token.val != '}') {
        push_scope(s);
        for(;;) {
            if (js_parse_statement_or_decl(s, DECL_MASK_ALL))
                return -1;
            if (s->token.val == '}')
                break;
        }
        pop_scope(s);
    }
    if (next_token(s))
        return -1;
    return 0;
}

/* allowed parse_flags: PF_IN_ACCEPTED */
static __exception int js_parse_var(JSParseState *s, int parse_flags, int tok,
                                    BOOL export_flag)
{
    JSContext *ctx = s->ctx;
    JSFunctionDef *fd = s->cur_func;
    JSAtom name = JS_ATOM_NULL;

    for (;;) {
        if (s->token.val == TOK_IDENT) {
            if (s->token.u.ident.is_reserved) {
                return js_parse_error_reserved_identifier(s);
            }
            name = JS_DupAtom(ctx, s->token.u.ident.atom);
            if (name == JS_ATOM_let && (tok == TOK_LET || tok == TOK_CONST)) {
                js_parse_error(s, "'let' is not a valid lexical identifier");
                goto var_error;
            }
            if (next_token(s))
                goto var_error;
            if (js_define_var(s, name, tok))
                goto var_error;
            if (export_flag) {
                if (!add_export_entry(s, s->cur_func->module, name, name,
                                      JS_EXPORT_TYPE_LOCAL))
                    goto var_error;
            }

            if (s->token.val == '=') {
                if (next_token(s))
                    goto var_error;
                if (tok == TOK_VAR) {
                    /* Must make a reference for proper `with` semantics */
                    int opcode, scope, label;
                    JSAtom name1;

                    emit_op(s, OP_scope_get_var);
                    emit_atom(s, name);
                    emit_u16(s, fd->scope_level);
                    if (get_lvalue(s, &opcode, &scope, &name1, &label, NULL, FALSE, '=') < 0)
                        goto var_error;
                    if (js_parse_assign_expr2(s, parse_flags)) {
                        JS_FreeAtom(ctx, name1);
                        goto var_error;
                    }
                    set_object_name(s, name);
                    put_lvalue(s, opcode, scope, name1, label,
                               PUT_LVALUE_NOKEEP, FALSE);
                } else {
                    if (js_parse_assign_expr2(s, parse_flags))
                        goto var_error;
                    set_object_name(s, name);
                    emit_op(s, (tok == TOK_CONST || tok == TOK_LET) ?
                        OP_scope_put_var_init : OP_scope_put_var);
                    emit_atom(s, name);
                    emit_u16(s, fd->scope_level);
                }
            } else {
                if (tok == TOK_CONST) {
                    js_parse_error(s, "missing initializer for const variable");
                    goto var_error;
                }
                if (tok == TOK_LET) {
                    /* initialize lexical variable upon entering its scope */
                    emit_op(s, OP_undefined);
                    emit_op(s, OP_scope_put_var_init);
                    emit_atom(s, name);
                    emit_u16(s, fd->scope_level);
                }
            }
            JS_FreeAtom(ctx, name);
        } else {
            int skip_bits;
            if ((s->token.val == '[' || s->token.val == '{')
            &&  js_parse_skip_parens_token(s, &skip_bits, FALSE) == '=') {
                emit_op(s, OP_undefined);
                if (js_parse_destructuring_element(s, tok, 0, TRUE, skip_bits & SKIP_HAS_ELLIPSIS, TRUE) < 0)
                    return -1;
            } else {
                return js_parse_error(s, "variable name expected");
            }
        }
        if (s->token.val != ',')
            break;
        if (next_token(s))
            return -1;
    }
    return 0;

 var_error:
    JS_FreeAtom(ctx, name);
    return -1;
}

/* test if the current token is a label. Use simplistic look-ahead scanner */
static BOOL is_label(JSParseState *s)
{
    return (s->token.val == TOK_IDENT && !s->token.u.ident.is_reserved &&
            peek_token(s, FALSE) == ':');
}

/* test if the current token is a let keyword. Use simplistic look-ahead scanner */
static int is_let(JSParseState *s, int decl_mask)
{
    int res = FALSE;

    if (token_is_pseudo_keyword(s, JS_ATOM_let)) {
#if 1
        JSParsePos pos;
        js_parse_get_pos(s, &pos);
        for (;;) {
            if (next_token(s)) {
                res = -1;
                break;
            }
            if (s->token.val == '[') {
                /* let [ is a syntax restriction:
                   it never introduces an ExpressionStatement */
                res = TRUE;
                break;
            }
            if (s->token.val == '{' ||
                (s->token.val == TOK_IDENT && !s->token.u.ident.is_reserved) ||
                s->token.val == TOK_LET ||
                s->token.val == TOK_YIELD ||
                s->token.val == TOK_AWAIT) {
                /* Check for possible ASI if not scanning for Declaration */
                /* XXX: should also check that `{` introduces a BindingPattern,
                   but Firefox does not and rejects eval("let=1;let\n{if(1)2;}") */
                if (s->last_line_num == s->token.line_num || (decl_mask & DECL_MASK_OTHER)) {
                    res = TRUE;
                    break;
                }
                break;
            }
            break;
        }
        if (js_parse_seek_token(s, &pos)) {
            res = -1;
        }
#else
        int tok = peek_token(s, TRUE);
        if (tok == '{' || tok == TOK_IDENT || peek_token(s, FALSE) == '[') {
            res = TRUE;
        }
#endif
    }
    return res;
}

/* XXX: handle IteratorClose when exiting the loop before the
   enumeration is done */
static __exception int js_parse_for_in_of(JSParseState *s, int label_name,
                                          BOOL is_async)
{
    JSContext *ctx = s->ctx;
    JSFunctionDef *fd = s->cur_func;
    JSAtom var_name;
    BOOL has_initializer, is_for_of, has_destructuring;
    int tok, tok1, opcode, scope, block_scope_level;
    int label_next, label_expr, label_cont, label_body, label_break;
    int pos_next, pos_expr;
    BlockEnv break_entry;

    has_initializer = FALSE;
    has_destructuring = FALSE;
    is_for_of = FALSE;
    block_scope_level = fd->scope_level;
    label_cont = new_label(s);
    label_body = new_label(s);
    label_break = new_label(s);
    label_next = new_label(s);

    /* create scope for the lexical variables declared in the enumeration
       expressions. XXX: Not completely correct because of weird capturing
       semantics in `for (i of o) a.push(function(){return i})` */
    push_scope(s);

    /* local for_in scope starts here so individual elements
       can be closed in statement. */
    push_break_entry(s->cur_func, &break_entry,
                     label_name, label_break, label_cont, 1);
    break_entry.scope_level = block_scope_level;

    label_expr = emit_goto(s, OP_goto, -1);

    pos_next = s->cur_func->byte_code.size;
    emit_label(s, label_next);

    tok = s->token.val;
    switch (is_let(s, DECL_MASK_OTHER)) {
    case TRUE:
        tok = TOK_LET;
        break;
    case FALSE:
        break;
    default:
        return -1;
    }
    if (tok == TOK_VAR || tok == TOK_LET || tok == TOK_CONST) {
        if (next_token(s))
            return -1;

        if (!(s->token.val == TOK_IDENT && !s->token.u.ident.is_reserved)) {
            if (s->token.val == '[' || s->token.val == '{') {
                if (js_parse_destructuring_element(s, tok, 0, TRUE, -1, FALSE) < 0)
                    return -1;
                has_destructuring = TRUE;
            } else {
                return js_parse_error(s, "variable name expected");
            }
            var_name = JS_ATOM_NULL;
        } else {
            var_name = JS_DupAtom(ctx, s->token.u.ident.atom);
            if (next_token(s)) {
                JS_FreeAtom(s->ctx, var_name);
                return -1;
            }
            if (js_define_var(s, var_name, tok)) {
                JS_FreeAtom(s->ctx, var_name);
                return -1;
            }
            emit_op(s, (tok == TOK_CONST || tok == TOK_LET) ?
                    OP_scope_put_var_init : OP_scope_put_var);
            emit_atom(s, var_name);
            emit_u16(s, fd->scope_level);
        }
    } else {
        int skip_bits;
        if ((s->token.val == '[' || s->token.val == '{')
        &&  ((tok1 = js_parse_skip_parens_token(s, &skip_bits, FALSE)) == TOK_IN || tok1 == TOK_OF)) {
            if (js_parse_destructuring_element(s, 0, 0, TRUE, skip_bits & SKIP_HAS_ELLIPSIS, TRUE) < 0)
                return -1;
        } else {
            int lvalue_label;
            if (js_parse_left_hand_side_expr(s))
                return -1;
            if (get_lvalue(s, &opcode, &scope, &var_name, &lvalue_label,
                           NULL, FALSE, TOK_FOR))
                return -1;
            put_lvalue(s, opcode, scope, var_name, lvalue_label,
                       PUT_LVALUE_NOKEEP_BOTTOM, FALSE);
        }
        var_name = JS_ATOM_NULL;
    }
    emit_goto(s, OP_goto, label_body);

    pos_expr = s->cur_func->byte_code.size;
    emit_label(s, label_expr);
    if (s->token.val == '=') {
        /* XXX: potential scoping issue if inside `with` statement */
        has_initializer = TRUE;
        /* parse and evaluate initializer prior to evaluating the
           object (only used with "for in" with a non lexical variable
           in non strict mode */
        if (next_token(s) || js_parse_assign_expr2(s, 0)) {
            JS_FreeAtom(ctx, var_name);
            return -1;
        }
        if (var_name != JS_ATOM_NULL) {
            emit_op(s, OP_scope_put_var);
            emit_atom(s, var_name);
            emit_u16(s, fd->scope_level);
        }
    }
    JS_FreeAtom(ctx, var_name);

    if (token_is_pseudo_keyword(s, JS_ATOM_of)) {
        break_entry.has_iterator = is_for_of = TRUE;
        break_entry.drop_count += 2;
        if (has_initializer)
            goto initializer_error;
    } else if (s->token.val == TOK_IN) {
        if (is_async)
            return js_parse_error(s, "'for await' loop should be used with 'of'");
        if (has_initializer &&
            (tok != TOK_VAR || (fd->js_mode & JS_MODE_STRICT) ||
             has_destructuring)) {
        initializer_error:
            return js_parse_error(s, "a declaration in the head of a for-%s loop can't have an initializer",
                                  is_for_of ? "of" : "in");
        }
    } else {
        return js_parse_error(s, "expected 'of' or 'in' in for control expression");
    }
    if (next_token(s))
        return -1;
    if (is_for_of) {
        if (js_parse_assign_expr(s))
            return -1;
    } else {
        if (js_parse_expr(s))
            return -1;
    }
    /* close the scope after having evaluated the expression so that
       the TDZ values are in the closures */
    close_scopes(s, s->cur_func->scope_level, block_scope_level);
    if (is_for_of) {
        if (is_async)
            emit_op(s, OP_for_await_of_start);
        else
            emit_op(s, OP_for_of_start);
        /* on stack: enum_rec */
    } else {
        emit_op(s, OP_for_in_start);
        /* on stack: enum_obj */
    }
    emit_goto(s, OP_goto, label_cont);

    if (js_parse_expect(s, ')'))
        return -1;

    if (OPTIMIZE) {
        /* move the `next` code here */
        DynBuf *bc = &s->cur_func->byte_code;
        int chunk_size = pos_expr - pos_next;
        int offset = bc->size - pos_next;
        int i;
        dbuf_realloc(bc, bc->size + chunk_size);
        dbuf_put(bc, bc->buf + pos_next, chunk_size);
        memset(bc->buf + pos_next, OP_nop, chunk_size);
        /* `next` part ends with a goto */
        s->cur_func->last_opcode_pos = bc->size - 5;
        /* relocate labels */
        for (i = label_cont; i < s->cur_func->label_count; i++) {
            LabelSlot *ls = &s->cur_func->label_slots[i];
            if (ls->pos >= pos_next && ls->pos < pos_expr)
                ls->pos += offset;
        }
    }

    emit_label(s, label_body);
    if (js_parse_statement(s))
        return -1;

    close_scopes(s, s->cur_func->scope_level, block_scope_level);

    emit_label(s, label_cont);
    if (is_for_of) {
        if (is_async) {
            /* call the next method */
            /* stack: iter_obj next catch_offset */
            emit_op(s, OP_dup3);
            emit_op(s, OP_drop);
            emit_op(s, OP_call_method);
            emit_u16(s, 0);
            /* get the result of the promise */
            emit_op(s, OP_await);
            /* unwrap the value and done values */
            emit_op(s, OP_iterator_get_value_done);
        } else {
            emit_op(s, OP_for_of_next);
            emit_u8(s, 0);
        }
    } else {
        emit_op(s, OP_for_in_next);
    }
    /* on stack: enum_rec / enum_obj value bool */
    emit_goto(s, OP_if_false, label_next);
    /* drop the undefined value from for_xx_next */
    emit_op(s, OP_drop);

    emit_label(s, label_break);
    if (is_for_of) {
        /* close and drop enum_rec */
        emit_op(s, OP_iterator_close);
    } else {
        emit_op(s, OP_drop);
    }
    pop_break_entry(s->cur_func);
    pop_scope(s);
    return 0;
}

static void set_eval_ret_undefined(JSParseState *s)
{
    if (s->cur_func->eval_ret_idx >= 0) {
        emit_op(s, OP_undefined);
        emit_op(s, OP_put_loc);
        emit_u16(s, s->cur_func->eval_ret_idx);
    }
}

static __exception int js_parse_statement_or_decl(JSParseState *s,
                                                  int decl_mask)
{
    JSContext *ctx = s->ctx;
    JSAtom label_name;
    int tok;

    /* specific label handling */
    /* XXX: support multiple labels on loop statements */
    label_name = JS_ATOM_NULL;
    if (is_label(s)) {
        BlockEnv *be;

        label_name = JS_DupAtom(ctx, s->token.u.ident.atom);

        for (be = s->cur_func->top_break; be; be = be->prev) {
            if (be->label_name == label_name) {
                js_parse_error(s, "duplicate label name");
                goto fail;
            }
        }

        if (next_token(s))
            goto fail;
        if (js_parse_expect(s, ':'))
            goto fail;
        if (s->token.val != TOK_FOR
        &&  s->token.val != TOK_DO
        &&  s->token.val != TOK_WHILE) {
            /* labelled regular statement */
            int label_break, mask;
            BlockEnv break_entry;

            label_break = new_label(s);
            push_break_entry(s->cur_func, &break_entry,
                             label_name, label_break, -1, 0);
            if (!(s->cur_func->js_mode & JS_MODE_STRICT) &&
                (decl_mask & DECL_MASK_FUNC_WITH_LABEL)) {
                mask = DECL_MASK_FUNC | DECL_MASK_FUNC_WITH_LABEL;
            } else {
                mask = 0;
            }
            if (js_parse_statement_or_decl(s, mask))
                goto fail;
            emit_label(s, label_break);
            pop_break_entry(s->cur_func);
            goto done;
        }
    }

    switch(tok = s->token.val) {
    case '{':
        if (js_parse_block(s))
            goto fail;
        break;
    case TOK_RETURN:
        if (s->cur_func->is_eval) {
            js_parse_error(s, "return not in a function");
            goto fail;
        }
        if (next_token(s))
            goto fail;
        if (s->token.val != ';' && s->token.val != '}' && !s->got_lf) {
            if (js_parse_expr(s))
                goto fail;
            emit_return(s, TRUE);
        } else {
            emit_return(s, FALSE);
        }
        if (js_parse_expect_semi(s))
            goto fail;
        break;
    case TOK_THROW:
        if (next_token(s))
            goto fail;
        if (s->got_lf) {
            js_parse_error(s, "line terminator not allowed after throw");
            goto fail;
        }
        if (js_parse_expr(s))
            goto fail;
        emit_op(s, OP_throw);
        if (js_parse_expect_semi(s))
            goto fail;
        break;
    case TOK_LET:
    case TOK_CONST:
    haslet:
        if (!(decl_mask & DECL_MASK_OTHER)) {
            js_parse_error(s, "lexical declarations can't appear in single-statement context");
            goto fail;
        }
        /* fall thru */
    case TOK_VAR:
        if (next_token(s))
            goto fail;
        if (js_parse_var(s, TRUE, tok, FALSE))
            goto fail;
        if (js_parse_expect_semi(s))
            goto fail;
        break;
    case TOK_IF:
        {
            int label1, label2, mask;
            if (next_token(s))
                goto fail;
            /* create a new scope for `let f;if(1) function f(){}` */
            push_scope(s);
            set_eval_ret_undefined(s);
            if (js_parse_expr_paren(s))
                goto fail;
            label1 = emit_goto(s, OP_if_false, -1);
            if (s->cur_func->js_mode & JS_MODE_STRICT)
                mask = 0;
            else
                mask = DECL_MASK_FUNC; /* Annex B.3.4 */

            if (js_parse_statement_or_decl(s, mask))
                goto fail;

            if (s->token.val == TOK_ELSE) {
                label2 = emit_goto(s, OP_goto, -1);
                if (next_token(s))
                    goto fail;

                emit_label(s, label1);
                if (js_parse_statement_or_decl(s, mask))
                    goto fail;

                label1 = label2;
            }
            emit_label(s, label1);
            pop_scope(s);
        }
        break;
    case TOK_WHILE:
        {
            int label_cont, label_break;
            BlockEnv break_entry;

            label_cont = new_label(s);
            label_break = new_label(s);

            push_break_entry(s->cur_func, &break_entry,
                             label_name, label_break, label_cont, 0);

            if (next_token(s))
                goto fail;

            set_eval_ret_undefined(s);

            emit_label(s, label_cont);
            if (js_parse_expr_paren(s))
                goto fail;
            emit_goto(s, OP_if_false, label_break);

            if (js_parse_statement(s))
                goto fail;
            emit_goto(s, OP_goto, label_cont);

            emit_label(s, label_break);

            pop_break_entry(s->cur_func);
        }
        break;
    case TOK_DO:
        {
            int label_cont, label_break, label1;
            BlockEnv break_entry;

            label_cont = new_label(s);
            label_break = new_label(s);
            label1 = new_label(s);

            push_break_entry(s->cur_func, &break_entry,
                             label_name, label_break, label_cont, 0);

            if (next_token(s))
                goto fail;

            emit_label(s, label1);

            set_eval_ret_undefined(s);

            if (js_parse_statement(s))
                goto fail;

            emit_label(s, label_cont);
            if (js_parse_expect(s, TOK_WHILE))
                goto fail;
            if (js_parse_expr_paren(s))
                goto fail;
            /* Insert semicolon if missing */
            if (s->token.val == ';') {
                if (next_token(s))
                    goto fail;
            }
            emit_goto(s, OP_if_true, label1);

            emit_label(s, label_break);

            pop_break_entry(s->cur_func);
        }
        break;
    case TOK_FOR:
        {
            int label_cont, label_break, label_body, label_test;
            int pos_cont, pos_body, block_scope_level;
            BlockEnv break_entry;
            int tok, bits;
            BOOL is_async;

            if (next_token(s))
                goto fail;

            set_eval_ret_undefined(s);
            bits = 0;
            is_async = FALSE;
            if (s->token.val == '(') {
                js_parse_skip_parens_token(s, &bits, FALSE);
            } else if (s->token.val == TOK_AWAIT) {
                if (!(s->cur_func->func_kind & JS_FUNC_ASYNC)) {
                    js_parse_error(s, "for await is only valid in asynchronous functions");
                    goto fail;
                }
                is_async = TRUE;
                if (next_token(s))
                    goto fail;
            }
            if (js_parse_expect(s, '('))
                goto fail;

            if (!(bits & SKIP_HAS_SEMI)) {
                /* parse for/in or for/of */
                if (js_parse_for_in_of(s, label_name, is_async))
                    goto fail;
                break;
            }
            block_scope_level = s->cur_func->scope_level;

            /* create scope for the lexical variables declared in the initial,
               test and increment expressions */
            push_scope(s);
            /* initial expression */
            tok = s->token.val;
            if (tok != ';') {
                switch (is_let(s, DECL_MASK_OTHER)) {
                case TRUE:
                    tok = TOK_LET;
                    break;
                case FALSE:
                    break;
                default:
                    goto fail;
                }
                if (tok == TOK_VAR || tok == TOK_LET || tok == TOK_CONST) {
                    if (next_token(s))
                        goto fail;
                    if (js_parse_var(s, FALSE, tok, FALSE))
                        goto fail;
                } else {
                    if (js_parse_expr2(s, FALSE))
                        goto fail;
                    emit_op(s, OP_drop);
                }

                /* close the closures before the first iteration */
                close_scopes(s, s->cur_func->scope_level, block_scope_level);
            }
            if (js_parse_expect(s, ';'))
                goto fail;

            label_test = new_label(s);
            label_cont = new_label(s);
            label_body = new_label(s);
            label_break = new_label(s);

            push_break_entry(s->cur_func, &break_entry,
                             label_name, label_break, label_cont, 0);

            /* test expression */
            if (s->token.val == ';') {
                /* no test expression */
                label_test = label_body;
            } else {
                emit_label(s, label_test);
                if (js_parse_expr(s))
                    goto fail;
                emit_goto(s, OP_if_false, label_break);
            }
            if (js_parse_expect(s, ';'))
                goto fail;

            if (s->token.val == ')') {
                /* no end expression */
                break_entry.label_cont = label_cont = label_test;
                pos_cont = 0; /* avoid warning */
            } else {
                /* skip the end expression */
                emit_goto(s, OP_goto, label_body);

                pos_cont = s->cur_func->byte_code.size;
                emit_label(s, label_cont);
                if (js_parse_expr(s))
                    goto fail;
                emit_op(s, OP_drop);
                if (label_test != label_body)
                    emit_goto(s, OP_goto, label_test);
            }
            if (js_parse_expect(s, ')'))
                goto fail;

            pos_body = s->cur_func->byte_code.size;
            emit_label(s, label_body);
            if (js_parse_statement(s))
                goto fail;

            /* close the closures before the next iteration */
            /* XXX: check continue case */
            close_scopes(s, s->cur_func->scope_level, block_scope_level);

            if (OPTIMIZE && label_test != label_body && label_cont != label_test) {
                /* move the increment code here */
                DynBuf *bc = &s->cur_func->byte_code;
                int chunk_size = pos_body - pos_cont;
                int offset = bc->size - pos_cont;
                int i;
                dbuf_realloc(bc, bc->size + chunk_size);
                dbuf_put(bc, bc->buf + pos_cont, chunk_size);
                memset(bc->buf + pos_cont, OP_nop, chunk_size);
                /* increment part ends with a goto */
                s->cur_func->last_opcode_pos = bc->size - 5;
                /* relocate labels */
                for (i = label_cont; i < s->cur_func->label_count; i++) {
                    LabelSlot *ls = &s->cur_func->label_slots[i];
                    if (ls->pos >= pos_cont && ls->pos < pos_body)
                        ls->pos += offset;
                }
            } else {
                emit_goto(s, OP_goto, label_cont);
            }

            emit_label(s, label_break);

            pop_break_entry(s->cur_func);
            pop_scope(s);
        }
        break;
    case TOK_BREAK:
    case TOK_CONTINUE:
        {
            int is_cont = s->token.val - TOK_BREAK;
            int label;

            if (next_token(s))
                goto fail;
            if (!s->got_lf && s->token.val == TOK_IDENT && !s->token.u.ident.is_reserved)
                label = s->token.u.ident.atom;
            else
                label = JS_ATOM_NULL;
            if (emit_break(s, label, is_cont))
                goto fail;
            if (label != JS_ATOM_NULL) {
                if (next_token(s))
                    goto fail;
            }
            if (js_parse_expect_semi(s))
                goto fail;
        }
        break;
    case TOK_SWITCH:
        {
            int label_case, label_break, label1;
            int default_label_pos;
            BlockEnv break_entry;

            if (next_token(s))
                goto fail;

            set_eval_ret_undefined(s);
            if (js_parse_expr_paren(s))
                goto fail;

            push_scope(s);
            label_break = new_label(s);
            push_break_entry(s->cur_func, &break_entry,
                             label_name, label_break, -1, 1);

            if (js_parse_expect(s, '{'))
                goto fail;

            default_label_pos = -1;
            label_case = -1;
            while (s->token.val != '}') {
                if (s->token.val == TOK_CASE) {
                    label1 = -1;
                    if (label_case >= 0) {
                        /* skip the case if needed */
                        label1 = emit_goto(s, OP_goto, -1);
                    }
                    emit_label(s, label_case);
                    label_case = -1;
                    for (;;) {
                        /* parse a sequence of case clauses */
                        if (next_token(s))
                            goto fail;
                        emit_op(s, OP_dup);
                        if (js_parse_expr(s))
                            goto fail;
                        if (js_parse_expect(s, ':'))
                            goto fail;
                        emit_op(s, OP_strict_eq);
                        if (s->token.val == TOK_CASE) {
                            label1 = emit_goto(s, OP_if_true, label1);
                        } else {
                            label_case = emit_goto(s, OP_if_false, -1);
                            emit_label(s, label1);
                            break;
                        }
                    }
                } else if (s->token.val == TOK_DEFAULT) {
                    if (next_token(s))
                        goto fail;
                    if (js_parse_expect(s, ':'))
                        goto fail;
                    if (default_label_pos >= 0) {
                        js_parse_error(s, "duplicate default");
                        goto fail;
                    }
                    if (label_case < 0) {
                        /* falling thru direct from switch expression */
                        label_case = emit_goto(s, OP_goto, -1);
                    }
                    /* Emit a dummy label opcode. Label will be patched after
                       the end of the switch body. Do not use emit_label(s, 0)
                       because it would clobber label 0 address, preventing
                       proper optimizer operation.
                     */
                    emit_op(s, OP_label);
                    emit_u32(s, 0);
                    default_label_pos = s->cur_func->byte_code.size - 4;
                } else {
                    if (label_case < 0) {
                        /* falling thru direct from switch expression */
                        js_parse_error(s, "invalid switch statement");
                        goto fail;
                    }
                    if (js_parse_statement_or_decl(s, DECL_MASK_ALL))
                        goto fail;
                }
            }
            if (js_parse_expect(s, '}'))
                goto fail;
            if (default_label_pos >= 0) {
                /* Ugly patch for the the `default` label, shameful and risky */
                put_u32(s->cur_func->byte_code.buf + default_label_pos,
                        label_case);
                s->cur_func->label_slots[label_case].pos = default_label_pos + 4;
            } else {
                emit_label(s, label_case);
            }
            emit_label(s, label_break);
            emit_op(s, OP_drop); /* drop the switch expression */

            pop_break_entry(s->cur_func);
            pop_scope(s);
        }
        break;
    case TOK_TRY:
        {
            int label_catch, label_catch2, label_finally, label_end;
            JSAtom name;
            BlockEnv block_env;

            set_eval_ret_undefined(s);
            if (next_token(s))
                goto fail;
            label_catch = new_label(s);
            label_catch2 = new_label(s);
            label_finally = new_label(s);
            label_end = new_label(s);

            emit_goto(s, OP_catch, label_catch);

            push_break_entry(s->cur_func, &block_env,
                             JS_ATOM_NULL, -1, -1, 1);
            block_env.label_finally = label_finally;

            if (js_parse_block(s))
                goto fail;

            pop_break_entry(s->cur_func);

            if (js_is_live_code(s)) {
                /* drop the catch offset */
                emit_op(s, OP_drop);
                /* must push dummy value to keep same stack size */
                emit_op(s, OP_undefined);
                emit_goto(s, OP_gosub, label_finally);
                emit_op(s, OP_drop);

                emit_goto(s, OP_goto, label_end);
            }

            if (s->token.val == TOK_CATCH) {
                if (next_token(s))
                    goto fail;

                push_scope(s);  /* catch variable */
                emit_label(s, label_catch);

                if (s->token.val == '{') {
                    /* support optional-catch-binding feature */
                    emit_op(s, OP_drop);    /* pop the exception object */
                } else {
                    if (js_parse_expect(s, '('))
                        goto fail;
                    if (!(s->token.val == TOK_IDENT && !s->token.u.ident.is_reserved)) {
                        if (s->token.val == '[' || s->token.val == '{') {
                            /* XXX: TOK_LET is not completely correct */
                            if (js_parse_destructuring_element(s, TOK_LET, 0, TRUE, -1, TRUE) < 0)
                                goto fail;
                        } else {
                            js_parse_error(s, "identifier expected");
                            goto fail;
                        }
                    } else {
                        name = JS_DupAtom(ctx, s->token.u.ident.atom);
                        if (next_token(s)
                        ||  js_define_var(s, name, TOK_CATCH) < 0) {
                            JS_FreeAtom(ctx, name);
                            goto fail;
                        }
                        /* store the exception value in the catch variable */
                        emit_op(s, OP_scope_put_var);
                        emit_u32(s, name);
                        emit_u16(s, s->cur_func->scope_level);
                    }
                    if (js_parse_expect(s, ')'))
                        goto fail;
                }
                /* XXX: should keep the address to nop it out if there is no finally block */
                emit_goto(s, OP_catch, label_catch2);

                push_scope(s);  /* catch block */
                push_break_entry(s->cur_func, &block_env, JS_ATOM_NULL,
                                 -1, -1, 1);
                block_env.label_finally = label_finally;

                if (js_parse_block(s))
                    goto fail;

                pop_break_entry(s->cur_func);
                pop_scope(s);  /* catch block */
                pop_scope(s);  /* catch variable */

                if (js_is_live_code(s)) {
                    /* drop the catch2 offset */
                    emit_op(s, OP_drop);
                    /* XXX: should keep the address to nop it out if there is no finally block */
                    /* must push dummy value to keep same stack size */
                    emit_op(s, OP_undefined);
                    emit_goto(s, OP_gosub, label_finally);
                    emit_op(s, OP_drop);
                    emit_goto(s, OP_goto, label_end);
                }
                /* catch exceptions thrown in the catch block to execute the
                 * finally clause and rethrow the exception */
                emit_label(s, label_catch2);
                /* catch value is at TOS, no need to push undefined */
                emit_goto(s, OP_gosub, label_finally);
                emit_op(s, OP_throw);

            } else if (s->token.val == TOK_FINALLY) {
                /* finally without catch : execute the finally clause
                 * and rethrow the exception */
                emit_label(s, label_catch);
                /* catch value is at TOS, no need to push undefined */
                emit_goto(s, OP_gosub, label_finally);
                emit_op(s, OP_throw);
            } else {
                js_parse_error(s, "expecting catch or finally");
                goto fail;
            }
            emit_label(s, label_finally);
            if (s->token.val == TOK_FINALLY) {
                int saved_eval_ret_idx = 0; /* avoid warning */
                
                if (next_token(s))
                    goto fail;
                /* on the stack: ret_value gosub_ret_value */
                push_break_entry(s->cur_func, &block_env, JS_ATOM_NULL,
                                 -1, -1, 2);

                if (s->cur_func->eval_ret_idx >= 0) {
                    /* 'finally' updates eval_ret only if not a normal
                       termination */
                    saved_eval_ret_idx =
                        add_var(s->ctx, s->cur_func, JS_ATOM__ret_);
                    if (saved_eval_ret_idx < 0)
                        goto fail;
                    emit_op(s, OP_get_loc);
                    emit_u16(s, s->cur_func->eval_ret_idx);
                    emit_op(s, OP_put_loc);
                    emit_u16(s, saved_eval_ret_idx);
                    set_eval_ret_undefined(s);
                }
                
                if (js_parse_block(s))
                    goto fail;

                if (s->cur_func->eval_ret_idx >= 0) {
                    emit_op(s, OP_get_loc);
                    emit_u16(s, saved_eval_ret_idx);
                    emit_op(s, OP_put_loc);
                    emit_u16(s, s->cur_func->eval_ret_idx);
                }
                pop_break_entry(s->cur_func);
            }
            emit_op(s, OP_ret);
            emit_label(s, label_end);
        }
        break;
    case ';':
        /* empty statement */
        if (next_token(s))
            goto fail;
        break;
    case TOK_WITH:
        if (s->cur_func->js_mode & JS_MODE_STRICT) {
            js_parse_error(s, "invalid keyword: with");
            goto fail;
        } else {
            int with_idx;

            if (next_token(s))
                goto fail;

            if (js_parse_expr_paren(s))
                goto fail;

            push_scope(s);
            with_idx = define_var(s, s->cur_func, JS_ATOM__with_,
                                  JS_VAR_DEF_WITH);
            if (with_idx < 0)
                goto fail;
            emit_op(s, OP_to_object);
            emit_op(s, OP_put_loc);
            emit_u16(s, with_idx);

            set_eval_ret_undefined(s);
            if (js_parse_statement(s))
                goto fail;

            /* Popping scope drops lexical context for the with object variable */
            pop_scope(s);
        }
        break;
    case TOK_FUNCTION:
        /* ES6 Annex B.3.2 and B.3.3 semantics */
        if (!(decl_mask & DECL_MASK_FUNC))
            goto func_decl_error;
        if (!(decl_mask & DECL_MASK_OTHER) && peek_token(s, FALSE) == '*')
            goto func_decl_error;
        goto parse_func_var;
    case TOK_IDENT:
        if (s->token.u.ident.is_reserved) {
            js_parse_error_reserved_identifier(s);
            goto fail;
        }
        /* Determine if `let` introduces a Declaration or an ExpressionStatement */
        switch (is_let(s, decl_mask)) {
        case TRUE:
            tok = TOK_LET;
            goto haslet;
        case FALSE:
            break;
        default:
            goto fail;
        }
        if (token_is_pseudo_keyword(s, JS_ATOM_async) &&
            peek_token(s, TRUE) == TOK_FUNCTION) {
            if (!(decl_mask & DECL_MASK_OTHER)) {
            func_decl_error:
                js_parse_error(s, "function declarations can't appear in single-statement context");
                goto fail;
            }
        parse_func_var:
            if (js_parse_function_decl(s, JS_PARSE_FUNC_VAR,
                                       JS_FUNC_NORMAL, JS_ATOM_NULL,
                                       s->token.ptr, s->token.line_num))
                goto fail;
            break;
        }
        goto hasexpr;

    case TOK_CLASS:
        if (!(decl_mask & DECL_MASK_OTHER)) {
            js_parse_error(s, "class declarations can't appear in single-statement context");
            goto fail;
        }
        if (js_parse_class(s, FALSE, JS_PARSE_EXPORT_NONE))
            return -1;
        break;

    case TOK_DEBUGGER:
        /* currently no debugger, so just skip the keyword */
        if (next_token(s))
            goto fail;
        if (js_parse_expect_semi(s))
            goto fail;
        break;
        
    case TOK_ENUM:
    case TOK_EXPORT:
    case TOK_EXTENDS:
        js_unsupported_keyword(s, s->token.u.ident.atom);
        goto fail;

    default:
    hasexpr:
        if (js_parse_expr(s))
            goto fail;
        if (s->cur_func->eval_ret_idx >= 0) {
            /* store the expression value so that it can be returned
               by eval() */
            emit_op(s, OP_put_loc);
            emit_u16(s, s->cur_func->eval_ret_idx);
        } else {
            emit_op(s, OP_drop); /* drop the result */
        }
        if (js_parse_expect_semi(s))
            goto fail;
        break;
    }
done:
    JS_FreeAtom(ctx, label_name);
    return 0;
fail:
    JS_FreeAtom(ctx, label_name);
    return -1;
}

/* 'name' is freed */
static JSModuleDef *js_new_module_def(JSContext *ctx, JSAtom name)
{
    JSModuleDef *m;
    m = js_mallocz(ctx, sizeof(*m));
    if (!m) {
        JS_FreeAtom(ctx, name);
        return NULL;
    }
    m->header.ref_count = 1;
    m->module_name = name;
    m->module_ns = JS_UNDEFINED;
    m->func_obj = JS_UNDEFINED;
    m->eval_exception = JS_UNDEFINED;
    m->meta_obj = JS_UNDEFINED;
    list_add_tail(&m->link, &ctx->loaded_modules);
    return m;
}

static void js_mark_module_def(JSRuntime *rt, JSModuleDef *m,
                               JS_MarkFunc *mark_func)
{
    int i;

    for(i = 0; i < m->export_entries_count; i++) {
        JSExportEntry *me = &m->export_entries[i];
        if (me->export_type == JS_EXPORT_TYPE_LOCAL &&
            me->u.local.var_ref) {
            mark_func(rt, &me->u.local.var_ref->header);
        }
    }

    JS_MarkValue(rt, m->module_ns, mark_func);
    JS_MarkValue(rt, m->func_obj, mark_func);
    JS_MarkValue(rt, m->eval_exception, mark_func);
    JS_MarkValue(rt, m->meta_obj, mark_func);
}

static void js_free_module_def(JSContext *ctx, JSModuleDef *m)
{
    int i;

    JS_FreeAtom(ctx, m->module_name);

    for(i = 0; i < m->req_module_entries_count; i++) {
        JSReqModuleEntry *rme = &m->req_module_entries[i];
        JS_FreeAtom(ctx, rme->module_name);
    }
    js_free(ctx, m->req_module_entries);

    for(i = 0; i < m->export_entries_count; i++) {
        JSExportEntry *me = &m->export_entries[i];
        if (me->export_type == JS_EXPORT_TYPE_LOCAL)
            free_var_ref(ctx->rt, me->u.local.var_ref);
        JS_FreeAtom(ctx, me->export_name);
        JS_FreeAtom(ctx, me->local_name);
    }
    js_free(ctx, m->export_entries);

    js_free(ctx, m->star_export_entries);

    for(i = 0; i < m->import_entries_count; i++) {
        JSImportEntry *mi = &m->import_entries[i];
        JS_FreeAtom(ctx, mi->import_name);
    }
    js_free(ctx, m->import_entries);

    JS_FreeValue(ctx, m->module_ns);
    JS_FreeValue(ctx, m->func_obj);
    JS_FreeValue(ctx, m->eval_exception);
    JS_FreeValue(ctx, m->meta_obj);
    list_del(&m->link);
    js_free(ctx, m);
}

static int add_req_module_entry(JSContext *ctx, JSModuleDef *m,
                                JSAtom module_name)
{
    JSReqModuleEntry *rme;
    int i;

    /* no need to add the module request if it is already present */
    for(i = 0; i < m->req_module_entries_count; i++) {
        rme = &m->req_module_entries[i];
        if (rme->module_name == module_name)
            return i;
    }

    if (js_resize_array(ctx, (void **)&m->req_module_entries,
                        sizeof(JSReqModuleEntry),
                        &m->req_module_entries_size,
                        m->req_module_entries_count + 1))
        return -1;
    rme = &m->req_module_entries[m->req_module_entries_count++];
    rme->module_name = JS_DupAtom(ctx, module_name);
    rme->module = NULL;
    return i;
}

static JSExportEntry *find_export_entry(JSContext *ctx, JSModuleDef *m,
                                        JSAtom export_name)
{
    JSExportEntry *me;
    int i;
    for(i = 0; i < m->export_entries_count; i++) {
        me = &m->export_entries[i];
        if (me->export_name == export_name)
            return me;
    }
    return NULL;
}

static JSExportEntry *add_export_entry2(JSContext *ctx,
                                        JSParseState *s, JSModuleDef *m,
                                       JSAtom local_name, JSAtom export_name,
                                       JSExportTypeEnum export_type)
{
    JSExportEntry *me;

    if (find_export_entry(ctx, m, export_name)) {
        char buf1[ATOM_GET_STR_BUF_SIZE];
        if (s) {
            js_parse_error(s, "duplicate exported name '%s'",
                           JS_AtomGetStr(ctx, buf1, sizeof(buf1), export_name));
        } else {
            JS_ThrowSyntaxErrorAtom(ctx, "duplicate exported name '%s'", export_name);
        }
        return NULL;
    }

    if (js_resize_array(ctx, (void **)&m->export_entries,
                        sizeof(JSExportEntry),
                        &m->export_entries_size,
                        m->export_entries_count + 1))
        return NULL;
    me = &m->export_entries[m->export_entries_count++];
    memset(me, 0, sizeof(*me));
    me->local_name = JS_DupAtom(ctx, local_name);
    me->export_name = JS_DupAtom(ctx, export_name);
    me->export_type = export_type;
    return me;
}

static JSExportEntry *add_export_entry(JSParseState *s, JSModuleDef *m,
                                       JSAtom local_name, JSAtom export_name,
                                       JSExportTypeEnum export_type)
{
    return add_export_entry2(s->ctx, s, m, local_name, export_name,
                             export_type);
}

static int add_star_export_entry(JSContext *ctx, JSModuleDef *m,
                                 int req_module_idx)
{
    JSStarExportEntry *se;

    if (js_resize_array(ctx, (void **)&m->star_export_entries,
                        sizeof(JSStarExportEntry),
                        &m->star_export_entries_size,
                        m->star_export_entries_count + 1))
        return -1;
    se = &m->star_export_entries[m->star_export_entries_count++];
    se->req_module_idx = req_module_idx;
    return 0;
}

/* create a C module */
JSModuleDef *JS_NewCModule(JSContext *ctx, const char *name_str,
                           JSModuleInitFunc *func)
{
    JSModuleDef *m;
    JSAtom name;
    name = JS_NewAtom(ctx, name_str);
    if (name == JS_ATOM_NULL)
        return NULL;
    m = js_new_module_def(ctx, name);
    m->init_func = func;
    return m;
}

int JS_AddModuleExport(JSContext *ctx, JSModuleDef *m, const char *export_name)
{
    JSExportEntry *me;
    JSAtom name;
    name = JS_NewAtom(ctx, export_name);
    if (name == JS_ATOM_NULL)
        return -1;
    me = add_export_entry2(ctx, NULL, m, JS_ATOM_NULL, name,
                           JS_EXPORT_TYPE_LOCAL);
    JS_FreeAtom(ctx, name);
    if (!me)
        return -1;
    else
        return 0;
}

int JS_SetModuleExport(JSContext *ctx, JSModuleDef *m, const char *export_name,
                       JSValue val)
{
    JSExportEntry *me;
    JSAtom name;
    name = JS_NewAtom(ctx, export_name);
    if (name == JS_ATOM_NULL)
        goto fail;
    me = find_export_entry(ctx, m, name);
    JS_FreeAtom(ctx, name);
    if (!me)
        goto fail;
    set_value(ctx, me->u.local.var_ref->pvalue, val);
    return 0;
 fail:
    JS_FreeValue(ctx, val);
    return -1;
}

void JS_SetModuleLoaderFunc(JSRuntime *rt,
                            JSModuleNormalizeFunc *module_normalize,
                            JSModuleLoaderFunc *module_loader, void *opaque)
{
    rt->module_normalize_func = module_normalize;
    rt->module_loader_func = module_loader;
    rt->module_loader_opaque = opaque;
}

/* default module filename normalizer */
static char *js_default_module_normalize_name(JSContext *ctx,
                                              const char *base_name,
                                              const char *name)
{
    char *filename, *p;
    const char *r;
    int len;

    if (name[0] != '.') {
        /* if no initial dot, the module name is not modified */
        return js_strdup(ctx, name);
    }

    p = strrchr(base_name, '/');
    if (p)
        len = p - base_name;
    else
        len = 0;

    filename = js_malloc(ctx, len + strlen(name) + 1 + 1);
    if (!filename)
        return NULL;
    memcpy(filename, base_name, len);
    filename[len] = '\0';

    /* we only normalize the leading '..' or '.' */
    r = name;
    for(;;) {
        if (r[0] == '.' && r[1] == '/') {
            r += 2;
        } else if (r[0] == '.' && r[1] == '.' && r[2] == '/') {
            /* remove the last path element of filename, except if "."
               or ".." */
            if (filename[0] == '\0')
                break;
            p = strrchr(filename, '/');
            if (!p)
                p = filename;
            else
                p++;
            if (!strcmp(p, ".") || !strcmp(p, ".."))
                break;
            if (p > filename)
                p--;
            *p = '\0';
            r += 3;
        } else {
            break;
        }
    }
    if (filename[0] != '\0')
        strcat(filename, "/");
    strcat(filename, r);
    //    printf("normalize: %s %s -> %s\n", base_name, name, filename);
    return filename;
}

static JSModuleDef *js_find_loaded_module(JSContext *ctx, JSAtom name)
{
    struct list_head *el;
    JSModuleDef *m;
    
    /* first look at the loaded modules */
    list_for_each(el, &ctx->loaded_modules) {
        m = list_entry(el, JSModuleDef, link);
        if (m->module_name == name)
            return m;
    }
    return NULL;
}

/* return NULL in case of exception (e.g. module could not be loaded) */
static JSModuleDef *js_host_resolve_imported_module(JSContext *ctx,
                                                    const char *base_cname,
                                                    const char *cname1)
{
    JSRuntime *rt = ctx->rt;
    JSModuleDef *m;
    char *cname;
    JSAtom module_name;

    if (!rt->module_normalize_func) {
        cname = js_default_module_normalize_name(ctx, base_cname, cname1);
    } else {
        cname = rt->module_normalize_func(ctx, base_cname, cname1,
                                          rt->module_loader_opaque);
    }
    if (!cname)
        return NULL;

    module_name = JS_NewAtom(ctx, cname);
    if (module_name == JS_ATOM_NULL) {
        js_free(ctx, cname);
        return NULL;
    }

    /* first look at the loaded modules */
    m = js_find_loaded_module(ctx, module_name);
    if (m) {
        js_free(ctx, cname);
        JS_FreeAtom(ctx, module_name);
        return m;
    }

    JS_FreeAtom(ctx, module_name);

    /* load the module */
    if (!rt->module_loader_func) {
        /* XXX: use a syntax error ? */
        JS_ThrowReferenceError(ctx, "could not load module '%s'",
                               cname);
        js_free(ctx, cname);
        return NULL;
    }

    m = rt->module_loader_func(ctx, cname, rt->module_loader_opaque);
    js_free(ctx, cname);
    return m;
}

static JSModuleDef *js_host_resolve_imported_module_atom(JSContext *ctx,
                                                    JSAtom base_module_name,
                                                    JSAtom module_name1)
{
    const char *base_cname, *cname;
    JSModuleDef *m;
    
    base_cname = JS_AtomToCString(ctx, base_module_name);
    if (!base_cname)
        return NULL;
    cname = JS_AtomToCString(ctx, module_name1);
    if (!cname) {
        JS_FreeCString(ctx, base_cname);
        return NULL;
    }
    m = js_host_resolve_imported_module(ctx, base_cname, cname);
    JS_FreeCString(ctx, base_cname);
    JS_FreeCString(ctx, cname);
    return m;
}

typedef struct JSResolveEntry {
    JSModuleDef *module;
    JSAtom name;
} JSResolveEntry;

typedef struct JSResolveState {
    JSResolveEntry *array;
    int size;
    int count;
} JSResolveState;

static int find_resolve_entry(JSResolveState *s,
                              JSModuleDef *m, JSAtom name)
{
    int i;
    for(i = 0; i < s->count; i++) {
        JSResolveEntry *re = &s->array[i];
        if (re->module == m && re->name == name)
            return i;
    }
    return -1;
}

static int add_resolve_entry(JSContext *ctx, JSResolveState *s,
                             JSModuleDef *m, JSAtom name)
{
    JSResolveEntry *re;

    if (js_resize_array(ctx, (void **)&s->array,
                        sizeof(JSResolveEntry),
                        &s->size, s->count + 1))
        return -1;
    re = &s->array[s->count++];
    re->module = m;
    re->name = JS_DupAtom(ctx, name);
    return 0;
}

typedef enum JSResolveResultEnum {
    JS_RESOLVE_RES_EXCEPTION = -1, /* memory alloc error */
    JS_RESOLVE_RES_FOUND = 0,
    JS_RESOLVE_RES_NOT_FOUND,
    JS_RESOLVE_RES_CIRCULAR,
    JS_RESOLVE_RES_AMBIGUOUS,
} JSResolveResultEnum;

static JSResolveResultEnum js_resolve_export1(JSContext *ctx,
                                              JSModuleDef **pmodule,
                                              JSExportEntry **pme,
                                              JSModuleDef *m,
                                              JSAtom export_name,
                                              JSResolveState *s)
{
    JSExportEntry *me;

    *pmodule = NULL;
    *pme = NULL;
    if (find_resolve_entry(s, m, export_name) >= 0)
        return JS_RESOLVE_RES_CIRCULAR;
    if (add_resolve_entry(ctx, s, m, export_name) < 0)
        return JS_RESOLVE_RES_EXCEPTION;
    me = find_export_entry(ctx, m, export_name);
    if (me) {
        if (me->export_type == JS_EXPORT_TYPE_LOCAL) {
            /* local export */
            *pmodule = m;
            *pme = me;
            return JS_RESOLVE_RES_FOUND;
        } else {
            /* indirect export */
            JSModuleDef *m1;
            m1 = m->req_module_entries[me->u.req_module_idx].module;
            if (me->local_name == JS_ATOM__star_) {
                /* export ns from */
                *pmodule = m;
                *pme = me;
                return JS_RESOLVE_RES_FOUND;
            } else {
                return js_resolve_export1(ctx, pmodule, pme, m1,
                                          me->local_name, s);
            }
        }
    } else {
        if (export_name != JS_ATOM_default) {
            /* not found in direct or indirect exports: try star exports */
            int i;

            for(i = 0; i < m->star_export_entries_count; i++) {
                JSStarExportEntry *se = &m->star_export_entries[i];
                JSModuleDef *m1, *res_m;
                JSExportEntry *res_me;
                JSResolveResultEnum ret;

                m1 = m->req_module_entries[se->req_module_idx].module;
                ret = js_resolve_export1(ctx, &res_m, &res_me, m1,
                                         export_name, s);
                if (ret == JS_RESOLVE_RES_AMBIGUOUS ||
                    ret == JS_RESOLVE_RES_EXCEPTION) {
                    return ret;
                } else if (ret == JS_RESOLVE_RES_FOUND) {
                    if (*pme != NULL) {
                        if (*pmodule != res_m ||
                            res_me->local_name != (*pme)->local_name) {
                            *pmodule = NULL;
                            *pme = NULL;
                            return JS_RESOLVE_RES_AMBIGUOUS;
                        }
                    } else {
                        *pmodule = res_m;
                        *pme = res_me;
                    }
                }
            }
            if (*pme != NULL)
                return JS_RESOLVE_RES_FOUND;
        }
        return JS_RESOLVE_RES_NOT_FOUND;
    }
}

/* If the return value is JS_RESOLVE_RES_FOUND, return the module
  (*pmodule) and the corresponding local export entry
  (*pme). Otherwise return (NULL, NULL) */
static JSResolveResultEnum js_resolve_export(JSContext *ctx,
                                             JSModuleDef **pmodule,
                                             JSExportEntry **pme,
                                             JSModuleDef *m,
                                             JSAtom export_name)
{
    JSResolveState ss, *s = &ss;
    int i;
    JSResolveResultEnum ret;

    s->array = NULL;
    s->size = 0;
    s->count = 0;

    ret = js_resolve_export1(ctx, pmodule, pme, m, export_name, s);

    for(i = 0; i < s->count; i++)
        JS_FreeAtom(ctx, s->array[i].name);
    js_free(ctx, s->array);

    return ret;
}

static void js_resolve_export_throw_error(JSContext *ctx,
                                          JSResolveResultEnum res,
                                          JSModuleDef *m, JSAtom export_name)
{
    char buf1[ATOM_GET_STR_BUF_SIZE];
    char buf2[ATOM_GET_STR_BUF_SIZE];
    switch(res) {
    case JS_RESOLVE_RES_EXCEPTION:
        break;
    default:
    case JS_RESOLVE_RES_NOT_FOUND:
        JS_ThrowSyntaxError(ctx, "Could not find export '%s' in module '%s'",
                            JS_AtomGetStr(ctx, buf1, sizeof(buf1), export_name),
                            JS_AtomGetStr(ctx, buf2, sizeof(buf2), m->module_name));
        break;
    case JS_RESOLVE_RES_CIRCULAR:
        JS_ThrowSyntaxError(ctx, "circular reference when looking for export '%s' in module '%s'",
                            JS_AtomGetStr(ctx, buf1, sizeof(buf1), export_name),
                            JS_AtomGetStr(ctx, buf2, sizeof(buf2), m->module_name));
        break;
    case JS_RESOLVE_RES_AMBIGUOUS:
        JS_ThrowSyntaxError(ctx, "export '%s' in module '%s' is ambiguous",
                            JS_AtomGetStr(ctx, buf1, sizeof(buf1), export_name),
                            JS_AtomGetStr(ctx, buf2, sizeof(buf2), m->module_name));
        break;
    }
}


typedef enum {
    EXPORTED_NAME_AMBIGUOUS,
    EXPORTED_NAME_NORMAL,
    EXPORTED_NAME_NS,
} ExportedNameEntryEnum;

typedef struct ExportedNameEntry {
    JSAtom export_name;
    ExportedNameEntryEnum export_type;
    union {
        JSExportEntry *me; /* using when the list is built */
        JSVarRef *var_ref; /* EXPORTED_NAME_NORMAL */
        JSModuleDef *module; /* for EXPORTED_NAME_NS */
    } u;
} ExportedNameEntry;

typedef struct GetExportNamesState {
    JSModuleDef **modules;
    int modules_size;
    int modules_count;

    ExportedNameEntry *exported_names;
    int exported_names_size;
    int exported_names_count;
} GetExportNamesState;

static int find_exported_name(GetExportNamesState *s, JSAtom name)
{
    int i;
    for(i = 0; i < s->exported_names_count; i++) {
        if (s->exported_names[i].export_name == name)
            return i;
    }
    return -1;
}

static __exception int get_exported_names(JSContext *ctx,
                                          GetExportNamesState *s,
                                          JSModuleDef *m, BOOL from_star)
{
    ExportedNameEntry *en;
    int i, j;

    /* check circular reference */
    for(i = 0; i < s->modules_count; i++) {
        if (s->modules[i] == m)
            return 0;
    }
    if (js_resize_array(ctx, (void **)&s->modules, sizeof(s->modules[0]),
                        &s->modules_size, s->modules_count + 1))
        return -1;
    s->modules[s->modules_count++] = m;

    for(i = 0; i < m->export_entries_count; i++) {
        JSExportEntry *me = &m->export_entries[i];
        if (from_star && me->export_name == JS_ATOM_default)
            continue;
        j = find_exported_name(s, me->export_name);
        if (j < 0) {
            if (js_resize_array(ctx, (void **)&s->exported_names, sizeof(s->exported_names[0]),
                                &s->exported_names_size,
                                s->exported_names_count + 1))
                return -1;
            en = &s->exported_names[s->exported_names_count++];
            en->export_name = me->export_name;
            /* avoid a second lookup for simple module exports */
            if (from_star || me->export_type != JS_EXPORT_TYPE_LOCAL)
                en->u.me = NULL;
            else
                en->u.me = me;
        } else {
            en = &s->exported_names[j];
            en->u.me = NULL;
        }
    }
    for(i = 0; i < m->star_export_entries_count; i++) {
        JSStarExportEntry *se = &m->star_export_entries[i];
        JSModuleDef *m1;
        m1 = m->req_module_entries[se->req_module_idx].module;
        if (get_exported_names(ctx, s, m1, TRUE))
            return -1;
    }
    return 0;
}

/* Unfortunately, the spec gives a different behavior from GetOwnProperty ! */
static int js_module_ns_has(JSContext *ctx, JSValueConst obj, JSAtom atom)
{
    return (find_own_property1(JS_VALUE_GET_OBJ(obj), atom) != NULL);
}

static const JSClassExoticMethods js_module_ns_exotic_methods = {
    .has_property = js_module_ns_has,
};

static int exported_names_cmp(const void *p1, const void *p2, void *opaque)
{
    JSContext *ctx = opaque;
    const ExportedNameEntry *me1 = p1;
    const ExportedNameEntry *me2 = p2;
    JSValue str1, str2;
    int ret;

    /* XXX: should avoid allocation memory in atom comparison */
    str1 = JS_AtomToString(ctx, me1->export_name);
    str2 = JS_AtomToString(ctx, me2->export_name);
    if (JS_IsException(str1) || JS_IsException(str2)) {
        /* XXX: raise an error ? */
        ret = 0;
    } else {
        ret = js_string_compare(ctx, JS_VALUE_GET_STRING(str1),
                                JS_VALUE_GET_STRING(str2));
    }
    JS_FreeValue(ctx, str1);
    JS_FreeValue(ctx, str2);
    return ret;
}

static JSValue js_get_module_ns(JSContext *ctx, JSModuleDef *m);

static JSValue js_module_ns_autoinit(JSContext *ctx, JSObject *p, JSAtom atom,
                                     void *opaque)
{
    JSModuleDef *m = opaque;
    return js_get_module_ns(ctx, m);
}

static JSValue js_build_module_ns(JSContext *ctx, JSModuleDef *m)
{
    JSValue obj;
    JSObject *p;
    GetExportNamesState s_s, *s = &s_s;
    int i, ret;
    JSProperty *pr;

    obj = JS_NewObjectClass(ctx, JS_CLASS_MODULE_NS);
    if (JS_IsException(obj))
        return obj;
    p = JS_VALUE_GET_OBJ(obj);

    memset(s, 0, sizeof(*s));
    ret = get_exported_names(ctx, s, m, FALSE);
    js_free(ctx, s->modules);
    if (ret)
        goto fail;

    /* Resolve the exported names. The ambiguous exports are removed */
    for(i = 0; i < s->exported_names_count; i++) {
        ExportedNameEntry *en = &s->exported_names[i];
        JSResolveResultEnum res;
        JSExportEntry *res_me;
        JSModuleDef *res_m;

        if (en->u.me) {
            res_me = en->u.me; /* fast case: no resolution needed */
            res_m = m;
            res = JS_RESOLVE_RES_FOUND;
        } else {
            res = js_resolve_export(ctx, &res_m, &res_me, m,
                                    en->export_name);
        }
        if (res != JS_RESOLVE_RES_FOUND) {
            if (res != JS_RESOLVE_RES_AMBIGUOUS) {
                js_resolve_export_throw_error(ctx, res, m, en->export_name);
                goto fail;
            }
            en->export_type = EXPORTED_NAME_AMBIGUOUS;
        } else {
            if (res_me->local_name == JS_ATOM__star_) {
                en->export_type = EXPORTED_NAME_NS;
                en->u.module = res_m->req_module_entries[res_me->u.req_module_idx].module;
            } else {
                en->export_type = EXPORTED_NAME_NORMAL;
                if (res_me->u.local.var_ref) {
                    en->u.var_ref = res_me->u.local.var_ref;
                } else {
                    JSObject *p1 = JS_VALUE_GET_OBJ(res_m->func_obj);
                    p1 = JS_VALUE_GET_OBJ(res_m->func_obj);
                    en->u.var_ref = p1->u.func.var_refs[res_me->u.local.var_idx];
                }
            }
        }
    }

    /* sort the exported names */
    rqsort(s->exported_names, s->exported_names_count,
           sizeof(s->exported_names[0]), exported_names_cmp, ctx);

    for(i = 0; i < s->exported_names_count; i++) {
        ExportedNameEntry *en = &s->exported_names[i];
        switch(en->export_type) {
        case EXPORTED_NAME_NORMAL:
            {
                JSVarRef *var_ref = en->u.var_ref;
                pr = add_property(ctx, p, en->export_name,
                                  JS_PROP_ENUMERABLE | JS_PROP_WRITABLE |
                                  JS_PROP_VARREF);
                if (!pr)
                    goto fail;
                var_ref->header.ref_count++;
                pr->u.var_ref = var_ref;
            }
            break;
        case EXPORTED_NAME_NS:
            /* the exported namespace must be created on demand */
            if (JS_DefineAutoInitProperty(ctx, obj,
                                          en->export_name,
                                          JS_AUTOINIT_ID_MODULE_NS,
                                          en->u.module, JS_PROP_ENUMERABLE | JS_PROP_WRITABLE) < 0)
                goto fail;
            break;
        default:
            break;
        }
    }

    js_free(ctx, s->exported_names);

    JS_DefinePropertyValue(ctx, obj, JS_ATOM_Symbol_toStringTag,
                           JS_AtomToString(ctx, JS_ATOM_Module),
                           0);

    p->extensible = FALSE;
    return obj;
 fail:
    js_free(ctx, s->exported_names);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_get_module_ns(JSContext *ctx, JSModuleDef *m)
{
    if (JS_IsUndefined(m->module_ns)) {
        JSValue val;
        val = js_build_module_ns(ctx, m);
        if (JS_IsException(val))
            return JS_EXCEPTION;
        m->module_ns = val;
    }
    return JS_DupValue(ctx, m->module_ns);
}

/* Load all the required modules for module 'm' */
static int js_resolve_module(JSContext *ctx, JSModuleDef *m)
{
    int i;
    JSModuleDef *m1;

    if (m->resolved)
        return 0;
#ifdef DUMP_MODULE_RESOLVE
    {
        char buf1[ATOM_GET_STR_BUF_SIZE];
        printf("resolving module '%s':\n", JS_AtomGetStr(ctx, buf1, sizeof(buf1), m->module_name));
    }
#endif
    m->resolved = TRUE;
    /* resolve each requested module */
    for(i = 0; i < m->req_module_entries_count; i++) {
        JSReqModuleEntry *rme = &m->req_module_entries[i];
        m1 = js_host_resolve_imported_module_atom(ctx, m->module_name,
                                                  rme->module_name);
        if (!m1)
            return -1;
        rme->module = m1;
        /* already done in js_host_resolve_imported_module() except if
           the module was loaded with JS_EvalBinary() */
        if (js_resolve_module(ctx, m1) < 0)
            return -1;
    }
    return 0;
}

static JSVarRef *js_create_module_var(JSContext *ctx, BOOL is_lexical)
{
    JSVarRef *var_ref;
    var_ref = js_malloc(ctx, sizeof(JSVarRef));
    if (!var_ref)
        return NULL;
    var_ref->header.ref_count = 1;
    if (is_lexical)
        var_ref->value = JS_UNINITIALIZED;
    else
        var_ref->value = JS_UNDEFINED;
    var_ref->pvalue = &var_ref->value;
    var_ref->is_detached = TRUE;
    add_gc_object(ctx->rt, &var_ref->header, JS_GC_OBJ_TYPE_VAR_REF);
    return var_ref;
}

/* Create the <eval> function associated with the module */
static int js_create_module_bytecode_function(JSContext *ctx, JSModuleDef *m)
{
    JSFunctionBytecode *b;
    int i;
    JSVarRef **var_refs;
    JSValue func_obj, bfunc;
    JSObject *p;

    bfunc = m->func_obj;
    func_obj = JS_NewObjectProtoClass(ctx, ctx->function_proto,
                                      JS_CLASS_BYTECODE_FUNCTION);

    if (JS_IsException(func_obj))
        return -1;
    b = JS_VALUE_GET_PTR(bfunc);

    p = JS_VALUE_GET_OBJ(func_obj);
    p->u.func.function_bytecode = b;
    b->header.ref_count++;
    p->u.func.home_object = NULL;
    p->u.func.var_refs = NULL;
    if (b->closure_var_count) {
        var_refs = js_mallocz(ctx, sizeof(var_refs[0]) * b->closure_var_count);
        if (!var_refs)
            goto fail;
        p->u.func.var_refs = var_refs;

        /* create the global variables. The other variables are
           imported from other modules */
        for(i = 0; i < b->closure_var_count; i++) {
            JSClosureVar *cv = &b->closure_var[i];
            JSVarRef *var_ref;
            if (cv->is_local) {
                var_ref = js_create_module_var(ctx, cv->is_lexical);
                if (!var_ref)
                    goto fail;
#ifdef DUMP_MODULE_RESOLVE
                printf("local %d: %p\n", i, var_ref);
#endif
                var_refs[i] = var_ref;
            }
        }
    }
    m->func_obj = func_obj;
    JS_FreeValue(ctx, bfunc);
    return 0;
 fail:
    JS_FreeValue(ctx, func_obj);
    return -1;
}

/* must be done before js_link_module() because of cyclic references */
static int js_create_module_function(JSContext *ctx, JSModuleDef *m)
{
    BOOL is_c_module;
    int i;
    JSVarRef *var_ref;
    
    if (m->func_created)
        return 0;

    is_c_module = (m->init_func != NULL);

    if (is_c_module) {
        /* initialize the exported variables */
        for(i = 0; i < m->export_entries_count; i++) {
            JSExportEntry *me = &m->export_entries[i];
            if (me->export_type == JS_EXPORT_TYPE_LOCAL) {
                var_ref = js_create_module_var(ctx, FALSE);
                if (!var_ref)
                    return -1;
                me->u.local.var_ref = var_ref;
            }
        }
    } else {
        if (js_create_module_bytecode_function(ctx, m))
            return -1;
    }
    m->func_created = TRUE;

    /* do it on the dependencies */
    
    for(i = 0; i < m->req_module_entries_count; i++) {
        JSReqModuleEntry *rme = &m->req_module_entries[i];
        if (js_create_module_function(ctx, rme->module) < 0)
            return -1;
    }

    return 0;
}    

    
/* Prepare a module to be executed by resolving all the imported
   variables. */
static int js_link_module(JSContext *ctx, JSModuleDef *m)
{
    int i;
    JSImportEntry *mi;
    JSModuleDef *m1;
    JSVarRef **var_refs, *var_ref;
    JSObject *p;
    BOOL is_c_module;
    JSValue ret_val;
    
    if (m->instantiated)
        return 0;
    m->instantiated = TRUE;

#ifdef DUMP_MODULE_RESOLVE
    {
        char buf1[ATOM_GET_STR_BUF_SIZE];
        printf("start instantiating module '%s':\n", JS_AtomGetStr(ctx, buf1, sizeof(buf1), m->module_name));
    }
#endif

    for(i = 0; i < m->req_module_entries_count; i++) {
        JSReqModuleEntry *rme = &m->req_module_entries[i];
        if (js_link_module(ctx, rme->module) < 0)
            goto fail;
    }

#ifdef DUMP_MODULE_RESOLVE
    {
        char buf1[ATOM_GET_STR_BUF_SIZE];
        printf("instantiating module '%s':\n", JS_AtomGetStr(ctx, buf1, sizeof(buf1), m->module_name));
    }
#endif
    /* check the indirect exports */
    for(i = 0; i < m->export_entries_count; i++) {
        JSExportEntry *me = &m->export_entries[i];
        if (me->export_type == JS_EXPORT_TYPE_INDIRECT &&
            me->local_name != JS_ATOM__star_) {
            JSResolveResultEnum ret;
            JSExportEntry *res_me;
            JSModuleDef *res_m, *m1;
            m1 = m->req_module_entries[me->u.req_module_idx].module;
            ret = js_resolve_export(ctx, &res_m, &res_me, m1, me->local_name);
            if (ret != JS_RESOLVE_RES_FOUND) {
                js_resolve_export_throw_error(ctx, ret, m, me->export_name);
                goto fail;
            }
        }
    }

#ifdef DUMP_MODULE_RESOLVE
    {
        printf("exported bindings:\n");
        for(i = 0; i < m->export_entries_count; i++) {
            JSExportEntry *me = &m->export_entries[i];
            printf(" name="); print_atom(ctx, me->export_name);
            printf(" local="); print_atom(ctx, me->local_name);
            printf(" type=%d idx=%d\n", me->export_type, me->u.local.var_idx);
        }
    }
#endif

    is_c_module = (m->init_func != NULL);

    if (!is_c_module) {
        p = JS_VALUE_GET_OBJ(m->func_obj);
        var_refs = p->u.func.var_refs;

        for(i = 0; i < m->import_entries_count; i++) {
            mi = &m->import_entries[i];
#ifdef DUMP_MODULE_RESOLVE
            printf("import var_idx=%d name=", mi->var_idx);
            print_atom(ctx, mi->import_name);
            printf(": ");
#endif
            m1 = m->req_module_entries[mi->req_module_idx].module;
            if (mi->import_name == JS_ATOM__star_) {
                JSValue val;
                /* name space import */
                val = js_get_module_ns(ctx, m1);
                if (JS_IsException(val))
                    goto fail;
                set_value(ctx, &var_refs[mi->var_idx]->value, val);
#ifdef DUMP_MODULE_RESOLVE
                printf("namespace\n");
#endif
            } else {
                JSResolveResultEnum ret;
                JSExportEntry *res_me;
                JSModuleDef *res_m;
                JSObject *p1;

                ret = js_resolve_export(ctx, &res_m,
                                        &res_me, m1, mi->import_name);
                if (ret != JS_RESOLVE_RES_FOUND) {
                    js_resolve_export_throw_error(ctx, ret, m1, mi->import_name);
                    goto fail;
                }
                if (res_me->local_name == JS_ATOM__star_) {
                    JSValue val;
                    JSModuleDef *m2;
                    /* name space import from */
                    m2 = res_m->req_module_entries[res_me->u.req_module_idx].module;
                    val = js_get_module_ns(ctx, m2);
                    if (JS_IsException(val))
                        goto fail;
                    var_ref = js_create_module_var(ctx, TRUE);
                    if (!var_ref) {
                        JS_FreeValue(ctx, val);
                        goto fail;
                    }
                    set_value(ctx, &var_ref->value, val);
                    var_refs[mi->var_idx] = var_ref;
#ifdef DUMP_MODULE_RESOLVE
                    printf("namespace from\n");
#endif
                } else {
                    var_ref = res_me->u.local.var_ref;
                    if (!var_ref) {
                        p1 = JS_VALUE_GET_OBJ(res_m->func_obj);
                        var_ref = p1->u.func.var_refs[res_me->u.local.var_idx];
                    }
                    var_ref->header.ref_count++;
                    var_refs[mi->var_idx] = var_ref;
#ifdef DUMP_MODULE_RESOLVE
                    printf("local export (var_ref=%p)\n", var_ref);
#endif
                }
            }
        }

        /* keep the exported variables in the module export entries (they
           are used when the eval function is deleted and cannot be
           initialized before in case imports are exported) */
        for(i = 0; i < m->export_entries_count; i++) {
            JSExportEntry *me = &m->export_entries[i];
            if (me->export_type == JS_EXPORT_TYPE_LOCAL) {
                var_ref = var_refs[me->u.local.var_idx];
                var_ref->header.ref_count++;
                me->u.local.var_ref = var_ref;
            }
        }

        /* initialize the global variables */
        ret_val = JS_Call(ctx, m->func_obj, JS_TRUE, 0, NULL);
        if (JS_IsException(ret_val))
            goto fail;
        JS_FreeValue(ctx, ret_val);
    }

#ifdef DUMP_MODULE_RESOLVE
    printf("done instantiate\n");
#endif
    return 0;
 fail:
    return -1;
}

/* return JS_ATOM_NULL if the name cannot be found. Only works with
   not striped bytecode functions. */
JSAtom JS_GetScriptOrModuleName(JSContext *ctx, int n_stack_levels)
{
    JSStackFrame *sf;
    JSFunctionBytecode *b;
    JSObject *p;
    /* XXX: currently we just use the filename of the englobing
       function. It does not work for eval(). Need to add a
       ScriptOrModule info in JSFunctionBytecode */
    sf = ctx->rt->current_stack_frame;
    if (!sf)
        return JS_ATOM_NULL;
    while (n_stack_levels-- > 0) {
        sf = sf->prev_frame;
        if (!sf)
            return JS_ATOM_NULL;
    }
    if (JS_VALUE_GET_TAG(sf->cur_func) != JS_TAG_OBJECT)
        return JS_ATOM_NULL;
    p = JS_VALUE_GET_OBJ(sf->cur_func);
    if (!js_class_has_bytecode(p->class_id))
        return JS_ATOM_NULL;
    b = p->u.func.function_bytecode;
    if (!b->has_debug)
        return JS_ATOM_NULL;
    return JS_DupAtom(ctx, b->debug.filename);
}

JSAtom JS_GetModuleName(JSContext *ctx, JSModuleDef *m)
{
    return JS_DupAtom(ctx, m->module_name);
}

JSValue JS_GetImportMeta(JSContext *ctx, JSModuleDef *m)
{
    JSValue obj;
    /* allocate meta_obj only if requested to save memory */
    obj = m->meta_obj;
    if (JS_IsUndefined(obj)) {
        obj = JS_NewObjectProto(ctx, JS_NULL);
        if (JS_IsException(obj))
            return JS_EXCEPTION;
        m->meta_obj = obj;
    }
    return JS_DupValue(ctx, obj);
}

static JSValue js_import_meta(JSContext *ctx)
{
    JSAtom filename;
    JSModuleDef *m;
    
    filename = JS_GetScriptOrModuleName(ctx, 0);
    if (filename == JS_ATOM_NULL)
        goto fail;

    /* XXX: inefficient, need to add a module or script pointer in
       JSFunctionBytecode */
    m = js_find_loaded_module(ctx, filename);
    JS_FreeAtom(ctx, filename);
    if (!m) {
    fail:
        JS_ThrowTypeError(ctx, "import.meta not supported in this context");
        return JS_EXCEPTION;
    }
    return JS_GetImportMeta(ctx, m);
}

/* used by os.Worker() and import() */
JSModuleDef *JS_RunModule(JSContext *ctx, const char *basename,
                          const char *filename)
{
    JSModuleDef *m;
    JSValue ret, func_obj;
    
    m = js_host_resolve_imported_module(ctx, basename, filename);
    if (!m)
        return NULL;
    
    if (js_resolve_module(ctx, m) < 0) {
        js_free_modules(ctx, JS_FREE_MODULE_NOT_RESOLVED);
        return NULL;
    }

    /* Evaluate the module code */
    func_obj = JS_DupValue(ctx, JS_MKPTR(JS_TAG_MODULE, m));
    ret = JS_EvalFunction(ctx, func_obj);
    if (JS_IsException(ret))
        return NULL;
    JS_FreeValue(ctx, ret);
    return m;
}

static JSValue js_dynamic_import_job(JSContext *ctx,
                                     int argc, JSValueConst *argv)
{
    JSValueConst *resolving_funcs = argv;
    JSValueConst basename_val = argv[2];
    JSValueConst specifier = argv[3];
    JSModuleDef *m;
    const char *basename = NULL, *filename;
    JSValue ret, err, ns;

    if (!JS_IsString(basename_val)) {
        JS_ThrowTypeError(ctx, "no function filename for import()");
        goto exception;
    }
    basename = JS_ToCString(ctx, basename_val);
    if (!basename)
        goto exception;

    filename = JS_ToCString(ctx, specifier);
    if (!filename)
        goto exception;
                     
    m = JS_RunModule(ctx, basename, filename);
    JS_FreeCString(ctx, filename);
    if (!m)
        goto exception;

    /* return the module namespace */
    ns = js_get_module_ns(ctx, m);
    if (JS_IsException(ns))
        goto exception;

    ret = JS_Call(ctx, resolving_funcs[0], JS_UNDEFINED,
                   1, (JSValueConst *)&ns);
    JS_FreeValue(ctx, ret); /* XXX: what to do if exception ? */
    JS_FreeValue(ctx, ns);
    JS_FreeCString(ctx, basename);
    return JS_UNDEFINED;
 exception:

    err = JS_GetException(ctx);
    ret = JS_Call(ctx, resolving_funcs[1], JS_UNDEFINED,
                   1, (JSValueConst *)&err);
    JS_FreeValue(ctx, ret); /* XXX: what to do if exception ? */
    JS_FreeValue(ctx, err);
    JS_FreeCString(ctx, basename);
    return JS_UNDEFINED;
}

static JSValue js_dynamic_import(JSContext *ctx, JSValueConst specifier)
{
    JSAtom basename;
    JSValue promise, resolving_funcs[2], basename_val;
    JSValueConst args[4];

    basename = JS_GetScriptOrModuleName(ctx, 0);
    if (basename == JS_ATOM_NULL)
        basename_val = JS_NULL;
    else
        basename_val = JS_AtomToValue(ctx, basename);
    JS_FreeAtom(ctx, basename);
    if (JS_IsException(basename_val))
        return basename_val;
    
    promise = JS_NewPromiseCapability(ctx, resolving_funcs);
    if (JS_IsException(promise)) {
        JS_FreeValue(ctx, basename_val);
        return promise;
    }

    args[0] = resolving_funcs[0];
    args[1] = resolving_funcs[1];
    args[2] = basename_val;
    args[3] = specifier;
    
    JS_EnqueueJob(ctx, js_dynamic_import_job, 4, args);

    JS_FreeValue(ctx, basename_val);
    JS_FreeValue(ctx, resolving_funcs[0]);
    JS_FreeValue(ctx, resolving_funcs[1]);
    return promise;
}

/* Run the <eval> function of the module and of all its requested
   modules. */
static JSValue js_evaluate_module(JSContext *ctx, JSModuleDef *m)
{
    JSModuleDef *m1;
    int i;
    JSValue ret_val;

    if (m->eval_mark)
        return JS_UNDEFINED; /* avoid cycles */

    if (m->evaluated) {
        /* if the module was already evaluated, rethrow the exception
           it raised */
        if (m->eval_has_exception) {
            return JS_Throw(ctx, JS_DupValue(ctx, m->eval_exception));
        } else {
            return JS_UNDEFINED;
        }
    }

    m->eval_mark = TRUE;

    for(i = 0; i < m->req_module_entries_count; i++) {
        JSReqModuleEntry *rme = &m->req_module_entries[i];
        m1 = rme->module;
        if (!m1->eval_mark) {
            ret_val = js_evaluate_module(ctx, m1);
            if (JS_IsException(ret_val)) {
                m->eval_mark = FALSE;
                return ret_val;
            }
            JS_FreeValue(ctx, ret_val);
        }
    }

    if (m->init_func) {
        /* C module init */
        if (m->init_func(ctx, m) < 0)
            ret_val = JS_EXCEPTION;
        else
            ret_val = JS_UNDEFINED;
    } else {
        ret_val = JS_CallFree(ctx, m->func_obj, JS_UNDEFINED, 0, NULL);
        m->func_obj = JS_UNDEFINED;
    }
    if (JS_IsException(ret_val)) {
        /* save the thrown exception value */
        m->eval_has_exception = TRUE;
        m->eval_exception = JS_DupValue(ctx, ctx->rt->current_exception);
    }
    m->eval_mark = FALSE;
    m->evaluated = TRUE;
    return ret_val;
}

static __exception JSAtom js_parse_from_clause(JSParseState *s)
{
    JSAtom module_name;
    if (!token_is_pseudo_keyword(s, JS_ATOM_from)) {
        js_parse_error(s, "from clause expected");
        return JS_ATOM_NULL;
    }
    if (next_token(s))
        return JS_ATOM_NULL;
    if (s->token.val != TOK_STRING) {
        js_parse_error(s, "string expected");
        return JS_ATOM_NULL;
    }
    module_name = JS_ValueToAtom(s->ctx, s->token.u.str.str);
    if (module_name == JS_ATOM_NULL)
        return JS_ATOM_NULL;
    if (next_token(s)) {
        JS_FreeAtom(s->ctx, module_name);
        return JS_ATOM_NULL;
    }
    return module_name;
}

static __exception int js_parse_export(JSParseState *s)
{
    JSContext *ctx = s->ctx;
    JSModuleDef *m = s->cur_func->module;
    JSAtom local_name, export_name;
    int first_export, idx, i, tok;
    JSAtom module_name;
    JSExportEntry *me;

    if (next_token(s))
        return -1;

    tok = s->token.val;
    if (tok == TOK_CLASS) {
        return js_parse_class(s, FALSE, JS_PARSE_EXPORT_NAMED);
    } else if (tok == TOK_FUNCTION ||
               (token_is_pseudo_keyword(s, JS_ATOM_async) &&
                peek_token(s, TRUE) == TOK_FUNCTION)) {
        return js_parse_function_decl2(s, JS_PARSE_FUNC_STATEMENT,
                                       JS_FUNC_NORMAL, JS_ATOM_NULL,
                                       s->token.ptr, s->token.line_num,
                                       JS_PARSE_EXPORT_NAMED, NULL);
    }

    if (next_token(s))
        return -1;

    switch(tok) {
    case '{':
        first_export = m->export_entries_count;
        while (s->token.val != '}') {
            if (!token_is_ident(s->token.val)) {
                js_parse_error(s, "identifier expected");
                return -1;
            }
            local_name = JS_DupAtom(ctx, s->token.u.ident.atom);
            export_name = JS_ATOM_NULL;
            if (next_token(s))
                goto fail;
            if (token_is_pseudo_keyword(s, JS_ATOM_as)) {
                if (next_token(s))
                    goto fail;
                if (!token_is_ident(s->token.val)) {
                    js_parse_error(s, "identifier expected");
                    goto fail;
                }
                export_name = JS_DupAtom(ctx, s->token.u.ident.atom);
                if (next_token(s)) {
                fail:
                    JS_FreeAtom(ctx, local_name);
                fail1:
                    JS_FreeAtom(ctx, export_name);
                    return -1;
                }
            } else {
                export_name = JS_DupAtom(ctx, local_name);
            }
            me = add_export_entry(s, m, local_name, export_name,
                                  JS_EXPORT_TYPE_LOCAL);
            JS_FreeAtom(ctx, local_name);
            JS_FreeAtom(ctx, export_name);
            if (!me)
                return -1;
            if (s->token.val != ',')
                break;
            if (next_token(s))
                return -1;
        }
        if (js_parse_expect(s, '}'))
            return -1;
        if (token_is_pseudo_keyword(s, JS_ATOM_from)) {
            module_name = js_parse_from_clause(s);
            if (module_name == JS_ATOM_NULL)
                return -1;
            idx = add_req_module_entry(ctx, m, module_name);
            JS_FreeAtom(ctx, module_name);
            if (idx < 0)
                return -1;
            for(i = first_export; i < m->export_entries_count; i++) {
                me = &m->export_entries[i];
                me->export_type = JS_EXPORT_TYPE_INDIRECT;
                me->u.req_module_idx = idx;
            }
        }
        break;
    case '*':
        if (token_is_pseudo_keyword(s, JS_ATOM_as)) {
            /* export ns from */
            if (next_token(s))
                return -1;
            if (!token_is_ident(s->token.val)) {
                js_parse_error(s, "identifier expected");
                return -1;
            }
            export_name = JS_DupAtom(ctx, s->token.u.ident.atom);
            if (next_token(s))
                goto fail1;
            module_name = js_parse_from_clause(s);
            if (module_name == JS_ATOM_NULL)
                goto fail1;
            idx = add_req_module_entry(ctx, m, module_name);
            JS_FreeAtom(ctx, module_name);
            if (idx < 0)
                goto fail1;
            me = add_export_entry(s, m, JS_ATOM__star_, export_name,
                                  JS_EXPORT_TYPE_INDIRECT);
            JS_FreeAtom(ctx, export_name);
            if (!me)
                return -1;
            me->u.req_module_idx = idx;
        } else {
            module_name = js_parse_from_clause(s);
            if (module_name == JS_ATOM_NULL)
                return -1;
            idx = add_req_module_entry(ctx, m, module_name);
            JS_FreeAtom(ctx, module_name);
            if (idx < 0)
                return -1;
            if (add_star_export_entry(ctx, m, idx) < 0)
                return -1;
        }
        break;
    case TOK_DEFAULT:
        if (s->token.val == TOK_CLASS) {
            return js_parse_class(s, FALSE, JS_PARSE_EXPORT_DEFAULT);
        } else if (s->token.val == TOK_FUNCTION ||
                   (token_is_pseudo_keyword(s, JS_ATOM_async) &&
                    peek_token(s, TRUE) == TOK_FUNCTION)) {
            return js_parse_function_decl2(s, JS_PARSE_FUNC_STATEMENT,
                                           JS_FUNC_NORMAL, JS_ATOM_NULL,
                                           s->token.ptr, s->token.line_num,
                                           JS_PARSE_EXPORT_DEFAULT, NULL);
        } else {
            if (js_parse_assign_expr(s))
                return -1;
        }
        /* set the name of anonymous functions */
        set_object_name(s, JS_ATOM_default);

        /* store the value in the _default_ global variable and export
           it */
        local_name = JS_ATOM__default_;
        if (define_var(s, s->cur_func, local_name, JS_VAR_DEF_LET) < 0)
            return -1;
        emit_op(s, OP_scope_put_var_init);
        emit_atom(s, local_name);
        emit_u16(s, 0);

        if (!add_export_entry(s, m, local_name, JS_ATOM_default,
                              JS_EXPORT_TYPE_LOCAL))
            return -1;
        break;
    case TOK_VAR:
    case TOK_LET:
    case TOK_CONST:
        return js_parse_var(s, TRUE, tok, TRUE);
    default:
        return js_parse_error(s, "invalid export syntax");
    }
    return js_parse_expect_semi(s);
}

static int add_closure_var(JSContext *ctx, JSFunctionDef *s,
                           BOOL is_local, BOOL is_arg,
                           int var_idx, JSAtom var_name,
                           BOOL is_const, BOOL is_lexical,
                           JSVarKindEnum var_kind);

static int add_import(JSParseState *s, JSModuleDef *m,
                      JSAtom local_name, JSAtom import_name)
{
    JSContext *ctx = s->ctx;
    int i, var_idx;
    JSImportEntry *mi;
    BOOL is_local;

    if (local_name == JS_ATOM_arguments || local_name == JS_ATOM_eval)
        return js_parse_error(s, "invalid import binding");

    if (local_name != JS_ATOM_default) {
        for (i = 0; i < s->cur_func->closure_var_count; i++) {
            if (s->cur_func->closure_var[i].var_name == local_name)
                return js_parse_error(s, "duplicate import binding");
        }
    }

    is_local = (import_name == JS_ATOM__star_);
    var_idx = add_closure_var(ctx, s->cur_func, is_local, FALSE,
                              m->import_entries_count,
                              local_name, TRUE, TRUE, FALSE);
    if (var_idx < 0)
        return -1;
    if (js_resize_array(ctx, (void **)&m->import_entries,
                        sizeof(JSImportEntry),
                        &m->import_entries_size,
                        m->import_entries_count + 1))
        return -1;
    mi = &m->import_entries[m->import_entries_count++];
    mi->import_name = JS_DupAtom(ctx, import_name);
    mi->var_idx = var_idx;
    return 0;
}

static __exception int js_parse_import(JSParseState *s)
{
    JSContext *ctx = s->ctx;
    JSModuleDef *m = s->cur_func->module;
    JSAtom local_name, import_name, module_name;
    int first_import, i, idx;

    if (next_token(s))
        return -1;

    first_import = m->import_entries_count;
    if (s->token.val == TOK_STRING) {
        module_name = JS_ValueToAtom(ctx, s->token.u.str.str);
        if (module_name == JS_ATOM_NULL)
            return -1;
        if (next_token(s)) {
            JS_FreeAtom(ctx, module_name);
            return -1;
        }
    } else {
        if (s->token.val == TOK_IDENT) {
            if (s->token.u.ident.is_reserved) {
                return js_parse_error_reserved_identifier(s);
            }
            /* "default" import */
            local_name = JS_DupAtom(ctx, s->token.u.ident.atom);
            import_name = JS_ATOM_default;
            if (next_token(s))
                goto fail;
            if (add_import(s, m, local_name, import_name))
                goto fail;
            JS_FreeAtom(ctx, local_name);

            if (s->token.val != ',')
                goto end_import_clause;
            if (next_token(s))
                return -1;
        }

        if (s->token.val == '*') {
            /* name space import */
            if (next_token(s))
                return -1;
            if (!token_is_pseudo_keyword(s, JS_ATOM_as))
                return js_parse_error(s, "expecting 'as'");
            if (next_token(s))
                return -1;
            if (!token_is_ident(s->token.val)) {
                js_parse_error(s, "identifier expected");
                return -1;
            }
            local_name = JS_DupAtom(ctx, s->token.u.ident.atom);
            import_name = JS_ATOM__star_;
            if (next_token(s))
                goto fail;
            if (add_import(s, m, local_name, import_name))
                goto fail;
            JS_FreeAtom(ctx, local_name);
        } else if (s->token.val == '{') {
            if (next_token(s))
                return -1;

            while (s->token.val != '}') {
                if (!token_is_ident(s->token.val)) {
                    js_parse_error(s, "identifier expected");
                    return -1;
                }
                import_name = JS_DupAtom(ctx, s->token.u.ident.atom);
                local_name = JS_ATOM_NULL;
                if (next_token(s))
                    goto fail;
                if (token_is_pseudo_keyword(s, JS_ATOM_as)) {
                    if (next_token(s))
                        goto fail;
                    if (!token_is_ident(s->token.val)) {
                        js_parse_error(s, "identifier expected");
                        goto fail;
                    }
                    local_name = JS_DupAtom(ctx, s->token.u.ident.atom);
                    if (next_token(s)) {
                    fail:
                        JS_FreeAtom(ctx, local_name);
                        JS_FreeAtom(ctx, import_name);
                        return -1;
                    }
                } else {
                    local_name = JS_DupAtom(ctx, import_name);
                }
                if (add_import(s, m, local_name, import_name))
                    goto fail;
                JS_FreeAtom(ctx, local_name);
                JS_FreeAtom(ctx, import_name);
                if (s->token.val != ',')
                    break;
                if (next_token(s))
                    return -1;
            }
            if (js_parse_expect(s, '}'))
                return -1;
        }
    end_import_clause:
        module_name = js_parse_from_clause(s);
        if (module_name == JS_ATOM_NULL)
            return -1;
    }
    idx = add_req_module_entry(ctx, m, module_name);
    JS_FreeAtom(ctx, module_name);
    if (idx < 0)
        return -1;
    for(i = first_import; i < m->import_entries_count; i++)
        m->import_entries[i].req_module_idx = idx;

    return js_parse_expect_semi(s);
}

static __exception int js_parse_source_element(JSParseState *s)
{
    JSFunctionDef *fd = s->cur_func;
    int tok;
    
    if (s->token.val == TOK_FUNCTION ||
        (token_is_pseudo_keyword(s, JS_ATOM_async) &&
         peek_token(s, TRUE) == TOK_FUNCTION)) {
        if (js_parse_function_decl(s, JS_PARSE_FUNC_STATEMENT,
                                   JS_FUNC_NORMAL, JS_ATOM_NULL,
                                   s->token.ptr, s->token.line_num))
            return -1;
    } else if (s->token.val == TOK_EXPORT && fd->module) {
        if (js_parse_export(s))
            return -1;
    } else if (s->token.val == TOK_IMPORT && fd->module &&
               ((tok = peek_token(s, FALSE)) != '(' && tok != '.'))  {
        /* the peek_token is needed to avoid confusion with ImportCall
           (dynamic import) or import.meta */
        if (js_parse_import(s))
            return -1;
    } else {
        if (js_parse_statement_or_decl(s, DECL_MASK_ALL))
            return -1;
    }
    return 0;
}

static JSFunctionDef *js_new_function_def(JSContext *ctx,
                                          JSFunctionDef *parent,
                                          BOOL is_eval,
                                          BOOL is_func_expr,
                                          const char *filename, int line_num)
{
    JSFunctionDef *fd;

    fd = js_mallocz(ctx, sizeof(*fd));
    if (!fd)
        return NULL;

    fd->ctx = ctx;
    init_list_head(&fd->child_list);

    /* insert in parent list */
    fd->parent = parent;
    fd->parent_cpool_idx = -1;
    if (parent) {
        list_add_tail(&fd->link, &parent->child_list);
        fd->js_mode = parent->js_mode;
        fd->parent_scope_level = parent->scope_level;
    }

    fd->is_eval = is_eval;
    fd->is_func_expr = is_func_expr;
    js_dbuf_init(ctx, &fd->byte_code);
    fd->last_opcode_pos = -1;
    fd->func_name = JS_ATOM_NULL;
    fd->var_object_idx = -1;
    fd->arg_var_object_idx = -1;
    fd->arguments_var_idx = -1;
    fd->arguments_arg_idx = -1;
    fd->func_var_idx = -1;
    fd->eval_ret_idx = -1;
    fd->this_var_idx = -1;
    fd->new_target_var_idx = -1;
    fd->this_active_func_var_idx = -1;
    fd->home_object_var_idx = -1;

    /* XXX: should distinguish arg, var and var object and body scopes */
    fd->scopes = fd->def_scope_array;
    fd->scope_size = countof(fd->def_scope_array);
    fd->scope_count = 1;
    fd->scopes[0].first = -1;
    fd->scopes[0].parent = -1;
    fd->scope_level = 0;  /* 0: var/arg scope */
    fd->scope_first = -1;
    fd->body_scope = -1;

    fd->filename = JS_NewAtom(ctx, filename);
    fd->line_num = line_num;

    js_dbuf_init(ctx, &fd->pc2line);
    //fd->pc2line_last_line_num = line_num;
    //fd->pc2line_last_pc = 0;
    fd->last_opcode_line_num = line_num;

    return fd;
}

static void free_bytecode_atoms(JSRuntime *rt,
                                const uint8_t *bc_buf, int bc_len,
                                BOOL use_short_opcodes)
{
    int pos, len, op;
    JSAtom atom;
    const JSOpCode *oi;
    
    pos = 0;
    while (pos < bc_len) {
        op = bc_buf[pos];
        if (use_short_opcodes)
            oi = &short_opcode_info(op);
        else
            oi = &opcode_info[op];
            
        len = oi->size;
        switch(oi->fmt) {
        case OP_FMT_atom:
        case OP_FMT_atom_u8:
        case OP_FMT_atom_u16:
        case OP_FMT_atom_label_u8:
        case OP_FMT_atom_label_u16:
            atom = get_u32(bc_buf + pos + 1);
            JS_FreeAtomRT(rt, atom);
            break;
        default:
            break;
        }
        pos += len;
    }
}

static void js_free_function_def(JSContext *ctx, JSFunctionDef *fd)
{
    int i;
    struct list_head *el, *el1;

    /* free the child functions */
    list_for_each_safe(el, el1, &fd->child_list) {
        JSFunctionDef *fd1;
        fd1 = list_entry(el, JSFunctionDef, link);
        js_free_function_def(ctx, fd1);
    }

    free_bytecode_atoms(ctx->rt, fd->byte_code.buf, fd->byte_code.size,
                        fd->use_short_opcodes);
    dbuf_free(&fd->byte_code);
    js_free(ctx, fd->jump_slots);
    js_free(ctx, fd->label_slots);
    js_free(ctx, fd->line_number_slots);

    for(i = 0; i < fd->cpool_count; i++) {
        JS_FreeValue(ctx, fd->cpool[i]);
    }
    js_free(ctx, fd->cpool);

    JS_FreeAtom(ctx, fd->func_name);

    for(i = 0; i < fd->var_count; i++) {
        JS_FreeAtom(ctx, fd->vars[i].var_name);
    }
    js_free(ctx, fd->vars);
    for(i = 0; i < fd->arg_count; i++) {
        JS_FreeAtom(ctx, fd->args[i].var_name);
    }
    js_free(ctx, fd->args);

    for(i = 0; i < fd->global_var_count; i++) {
        JS_FreeAtom(ctx, fd->global_vars[i].var_name);
    }
    js_free(ctx, fd->global_vars);

    for(i = 0; i < fd->closure_var_count; i++) {
        JSClosureVar *cv = &fd->closure_var[i];
        JS_FreeAtom(ctx, cv->var_name);
    }
    js_free(ctx, fd->closure_var);

    if (fd->scopes != fd->def_scope_array)
        js_free(ctx, fd->scopes);

    JS_FreeAtom(ctx, fd->filename);
    dbuf_free(&fd->pc2line);

    js_free(ctx, fd->source);

    if (fd->parent) {
        /* remove in parent list */
        list_del(&fd->link);
    }
    js_free(ctx, fd);
}

#ifdef DUMP_BYTECODE
static const char *skip_lines(const char *p, int n) {
    while (n-- > 0 && *p) {
        while (*p && *p++ != '\n')
            continue;
    }
    return p;
}

static void print_lines(const char *source, int line, int line1) {
    const char *s = source;
    const char *p = skip_lines(s, line);
    if (*p) {
        while (line++ < line1) {
            p = skip_lines(s = p, 1);
            printf(";; %.*s", (int)(p - s), s);
            if (!*p) {
                if (p[-1] != '\n')
                    printf("\n");
                break;
            }
        }
    }
}

static void dump_byte_code(JSContext *ctx, int pass,
                           const uint8_t *tab, int len,
                           const JSVarDef *args, int arg_count,
                           const JSVarDef *vars, int var_count,
                           const JSClosureVar *closure_var, int closure_var_count,
                           const JSValue *cpool, uint32_t cpool_count,
                           const char *source, int line_num,
                           const LabelSlot *label_slots, JSFunctionBytecode *b)
{
    const JSOpCode *oi;
    int pos, pos_next, op, size, idx, addr, line, line1, in_source;
    uint8_t *bits = js_mallocz(ctx, len * sizeof(*bits));
    BOOL use_short_opcodes = (b != NULL);

    /* scan for jump targets */
    for (pos = 0; pos < len; pos = pos_next) {
        op = tab[pos];
        if (use_short_opcodes)
            oi = &short_opcode_info(op);
        else
            oi = &opcode_info[op];
        pos_next = pos + oi->size;
        if (op < OP_COUNT) {
            switch (oi->fmt) {
#if SHORT_OPCODES
            case OP_FMT_label8:
                pos++;
                addr = (int8_t)tab[pos];
                goto has_addr;
            case OP_FMT_label16:
                pos++;
                addr = (int16_t)get_u16(tab + pos);
                goto has_addr;
#endif
            case OP_FMT_atom_label_u8:
            case OP_FMT_atom_label_u16:
                pos += 4;
                /* fall thru */
            case OP_FMT_label:
            case OP_FMT_label_u16:
                pos++;
                addr = get_u32(tab + pos);
                goto has_addr;
            has_addr:
                if (pass == 1)
                    addr = label_slots[addr].pos;
                if (pass == 2)
                    addr = label_slots[addr].pos2;
                if (pass == 3)
                    addr += pos;
                if (addr >= 0 && addr < len)
                    bits[addr] |= 1;
                break;
            }
        }
    }
    in_source = 0;
    if (source) {
        /* Always print first line: needed if single line */
        print_lines(source, 0, 1);
        in_source = 1;
    }
    line1 = line = 1;
    pos = 0;
    while (pos < len) {
        op = tab[pos];
        if (source) {
            if (b) {
                line1 = find_line_num(ctx, b, pos) - line_num + 1;
            } else if (op == OP_line_num) {
                line1 = get_u32(tab + pos + 1) - line_num + 1;
            }
            if (line1 > line) {
                if (!in_source)
                    printf("\n");
                in_source = 1;
                print_lines(source, line, line1);
                line = line1;
                //bits[pos] |= 2;
            }
        }
        if (in_source)
            printf("\n");
        in_source = 0;
        if (op >= OP_COUNT) {
            printf("invalid opcode (0x%02x)\n", op);
            pos++;
            continue;
        }
        if (use_short_opcodes)
            oi = &short_opcode_info(op);
        else
            oi = &opcode_info[op];
        size = oi->size;
        if (pos + size > len) {
            printf("truncated opcode (0x%02x)\n", op);
            break;
        }
#if defined(DUMP_BYTECODE) && (DUMP_BYTECODE & 16)
        {
            int i, x, x0;
            x = x0 = printf("%5d ", pos);
            for (i = 0; i < size; i++) {
                if (i == 6) {
                    printf("\n%*s", x = x0, "");
                }
                x += printf(" %02X", tab[pos + i]);
            }
            printf("%*s", x0 + 20 - x, "");
        }
#endif
        if (bits[pos]) {
            printf("%5d:  ", pos);
        } else {
            printf("        ");
        }
        printf("%s", oi->name);
        pos++;
        switch(oi->fmt) {
        case OP_FMT_none_int:
            printf(" %d", op - OP_push_0);
            break;
        case OP_FMT_npopx:
            printf(" %d", op - OP_call0);
            break;
        case OP_FMT_u8:
            printf(" %u", get_u8(tab + pos));
            break;
        case OP_FMT_i8:
            printf(" %d", get_i8(tab + pos));
            break;
        case OP_FMT_u16:
        case OP_FMT_npop:
            printf(" %u", get_u16(tab + pos));
            break;
        case OP_FMT_npop_u16:
            printf(" %u,%u", get_u16(tab + pos), get_u16(tab + pos + 2));
            break;
        case OP_FMT_i16:
            printf(" %d", get_i16(tab + pos));
            break;
        case OP_FMT_i32:
            printf(" %d", get_i32(tab + pos));
            break;
        case OP_FMT_u32:
            printf(" %u", get_u32(tab + pos));
            break;
#if SHORT_OPCODES
        case OP_FMT_label8:
            addr = get_i8(tab + pos);
            goto has_addr1;
        case OP_FMT_label16:
            addr = get_i16(tab + pos);
            goto has_addr1;
#endif
        case OP_FMT_label:
            addr = get_u32(tab + pos);
            goto has_addr1;
        has_addr1:
            if (pass == 1)
                printf(" %u:%u", addr, label_slots[addr].pos);
            if (pass == 2)
                printf(" %u:%u", addr, label_slots[addr].pos2);
            if (pass == 3)
                printf(" %u", addr + pos);
            break;
        case OP_FMT_label_u16:
            addr = get_u32(tab + pos);
            if (pass == 1)
                printf(" %u:%u", addr, label_slots[addr].pos);
            if (pass == 2)
                printf(" %u:%u", addr, label_slots[addr].pos2);
            if (pass == 3)
                printf(" %u", addr + pos);
            printf(",%u", get_u16(tab + pos + 4));
            break;
#if SHORT_OPCODES
        case OP_FMT_const8:
            idx = get_u8(tab + pos);
            goto has_pool_idx;
#endif
        case OP_FMT_const:
            idx = get_u32(tab + pos);
            goto has_pool_idx;
        has_pool_idx:
            printf(" %u: ", idx);
            if (idx < cpool_count) {
                JS_DumpValue(ctx, cpool[idx]);
            }
            break;
        case OP_FMT_atom:
            printf(" ");
            print_atom(ctx, get_u32(tab + pos));
            break;
        case OP_FMT_atom_u8:
            printf(" ");
            print_atom(ctx, get_u32(tab + pos));
            printf(",%d", get_u8(tab + pos + 4));
            break;
        case OP_FMT_atom_u16:
            printf(" ");
            print_atom(ctx, get_u32(tab + pos));
            printf(",%d", get_u16(tab + pos + 4));
            break;
        case OP_FMT_atom_label_u8:
        case OP_FMT_atom_label_u16:
            printf(" ");
            print_atom(ctx, get_u32(tab + pos));
            addr = get_u32(tab + pos + 4);
            if (pass == 1)
                printf(",%u:%u", addr, label_slots[addr].pos);
            if (pass == 2)
                printf(",%u:%u", addr, label_slots[addr].pos2);
            if (pass == 3)
                printf(",%u", addr + pos + 4);
            if (oi->fmt == OP_FMT_atom_label_u8)
                printf(",%u", get_u8(tab + pos + 8));
            else
                printf(",%u", get_u16(tab + pos + 8));
            break;
        case OP_FMT_none_loc:
            idx = (op - OP_get_loc0) % 4;
            goto has_loc;
        case OP_FMT_loc8:
            idx = get_u8(tab + pos);
            goto has_loc;
        case OP_FMT_loc:
            idx = get_u16(tab + pos);
        has_loc:
            printf(" %d: ", idx);
            if (idx < var_count) {
                print_atom(ctx, vars[idx].var_name);
            }
            break;
        case OP_FMT_none_arg:
            idx = (op - OP_get_arg0) % 4;
            goto has_arg;
        case OP_FMT_arg:
            idx = get_u16(tab + pos);
        has_arg:
            printf(" %d: ", idx);
            if (idx < arg_count) {
                print_atom(ctx, args[idx].var_name);
            }
            break;
        case OP_FMT_none_var_ref:
            idx = (op - OP_get_var_ref0) % 4;
            goto has_var_ref;
        case OP_FMT_var_ref:
            idx = get_u16(tab + pos);
        has_var_ref:
            printf(" %d: ", idx);
            if (idx < closure_var_count) {
                print_atom(ctx, closure_var[idx].var_name);
            }
            break;
        default:
            break;
        }
        printf("\n");
        pos += oi->size - 1;
    }
    if (source) {
        if (!in_source)
            printf("\n");
        print_lines(source, line, INT32_MAX);
    }
    js_free(ctx, bits);
}

static __maybe_unused void dump_pc2line(JSContext *ctx, const uint8_t *buf, int len,
                                                 int line_num)
{
    const uint8_t *p_end, *p_next, *p;
    int pc, v;
    unsigned int op;

    if (len <= 0)
        return;

    printf("%5s %5s\n", "PC", "LINE");

    p = buf;
    p_end = buf + len;
    pc = 0;
    while (p < p_end) {
        op = *p++;
        if (op == 0) {
            v = unicode_from_utf8(p, p_end - p, &p_next);
            if (v < 0)
                goto fail;
            pc += v;
            p = p_next;
            v = unicode_from_utf8(p, p_end - p, &p_next);
            if (v < 0) {
            fail:
                printf("invalid pc2line encode pos=%d\n", (int)(p - buf));
                return;
            }
            if (!(v & 1)) {
                v = v >> 1;
            } else {
                v = -(v >> 1) - 1;
            }
            line_num += v;
            p = p_next;
        } else {
            op -= PC2LINE_OP_FIRST;
            pc += (op / PC2LINE_RANGE);
            line_num += (op % PC2LINE_RANGE) + PC2LINE_BASE;
        }
        printf("%5d %5d\n", pc, line_num);
    }
}

static __maybe_unused void js_dump_function_bytecode(JSContext *ctx, JSFunctionBytecode *b)
{
    int i;
    char atom_buf[ATOM_GET_STR_BUF_SIZE];
    const char *str;

    if (b->has_debug && b->debug.filename != JS_ATOM_NULL) {
        str = JS_AtomGetStr(ctx, atom_buf, sizeof(atom_buf), b->debug.filename);
        printf("%s:%d: ", str, b->debug.line_num);
    }

    str = JS_AtomGetStr(ctx, atom_buf, sizeof(atom_buf), b->func_name);
    printf("function: %s%s\n", &"*"[b->func_kind != JS_FUNC_GENERATOR], str);
    if (b->js_mode) {
        printf("  mode:");
        if (b->js_mode & JS_MODE_STRICT)
            printf(" strict");
#ifdef CONFIG_BIGNUM
        if (b->js_mode & JS_MODE_MATH)
            printf(" math");
#endif
        printf("\n");
    }
    if (b->arg_count && b->vardefs) {
        printf("  args:");
        for(i = 0; i < b->arg_count; i++) {
            printf(" %s", JS_AtomGetStr(ctx, atom_buf, sizeof(atom_buf),
                                        b->vardefs[i].var_name));
        }
        printf("\n");
    }
    if (b->var_count && b->vardefs) {
        printf("  locals:\n");
        for(i = 0; i < b->var_count; i++) {
            JSVarDef *vd = &b->vardefs[b->arg_count + i];
            printf("%5d: %s %s", i,
                   vd->var_kind == JS_VAR_CATCH ? "catch" :
                   (vd->var_kind == JS_VAR_FUNCTION_DECL ||
                    vd->var_kind == JS_VAR_NEW_FUNCTION_DECL) ? "function" :
                   vd->is_const ? "const" :
                   vd->is_lexical ? "let" : "var",
                   JS_AtomGetStr(ctx, atom_buf, sizeof(atom_buf), vd->var_name));
            if (vd->scope_level)
                printf(" [level:%d next:%d]", vd->scope_level, vd->scope_next);
            printf("\n");
        }
    }
    if (b->closure_var_count) {
        printf("  closure vars:\n");
        for(i = 0; i < b->closure_var_count; i++) {
            JSClosureVar *cv = &b->closure_var[i];
            printf("%5d: %s %s:%s%d %s\n", i,
                   JS_AtomGetStr(ctx, atom_buf, sizeof(atom_buf), cv->var_name),
                   cv->is_local ? "local" : "parent",
                   cv->is_arg ? "arg" : "loc", cv->var_idx,
                   cv->is_const ? "const" :
                   cv->is_lexical ? "let" : "var");
        }
    }
    printf("  stack_size: %d\n", b->stack_size);
    printf("  opcodes:\n");
    dump_byte_code(ctx, 3, b->byte_code_buf, b->byte_code_len,
                   b->vardefs, b->arg_count,
                   b->vardefs ? b->vardefs + b->arg_count : NULL, b->var_count,
                   b->closure_var, b->closure_var_count,
                   b->cpool, b->cpool_count,
                   b->has_debug ? b->debug.source : NULL,
                   b->has_debug ? b->debug.line_num : -1, NULL, b);
#if defined(DUMP_BYTECODE) && (DUMP_BYTECODE & 32)
    if (b->has_debug)
        dump_pc2line(ctx, b->debug.pc2line_buf, b->debug.pc2line_len, b->debug.line_num);
#endif
    printf("\n");
}
#endif

static int add_closure_var(JSContext *ctx, JSFunctionDef *s,
                           BOOL is_local, BOOL is_arg,
                           int var_idx, JSAtom var_name,
                           BOOL is_const, BOOL is_lexical,
                           JSVarKindEnum var_kind)
{
    JSClosureVar *cv;

    /* the closure variable indexes are currently stored on 16 bits */
    if (s->closure_var_count >= JS_MAX_LOCAL_VARS) {
        JS_ThrowInternalError(ctx, "too many closure variables");
        return -1;
    }

    if (js_resize_array(ctx, (void **)&s->closure_var,
                        sizeof(s->closure_var[0]),
                        &s->closure_var_size, s->closure_var_count + 1))
        return -1;
    cv = &s->closure_var[s->closure_var_count++];
    cv->is_local = is_local;
    cv->is_arg = is_arg;
    cv->is_const = is_const;
    cv->is_lexical = is_lexical;
    cv->var_kind = var_kind;
    cv->var_idx = var_idx;
    cv->var_name = JS_DupAtom(ctx, var_name);
    return s->closure_var_count - 1;
}

static int find_closure_var(JSContext *ctx, JSFunctionDef *s,
                            JSAtom var_name)
{
    int i;
    for(i = 0; i < s->closure_var_count; i++) {
        JSClosureVar *cv = &s->closure_var[i];
        if (cv->var_name == var_name)
            return i;
    }
    return -1;
}

/* 'fd' must be a parent of 's'. Create in 's' a closure referencing a
   local variable (is_local = TRUE) or a closure (is_local = FALSE) in
   'fd' */
static int get_closure_var2(JSContext *ctx, JSFunctionDef *s,
                            JSFunctionDef *fd, BOOL is_local,
                            BOOL is_arg, int var_idx, JSAtom var_name,
                            BOOL is_const, BOOL is_lexical,
                            JSVarKindEnum var_kind)
{
    int i;

    if (fd != s->parent) {
        var_idx = get_closure_var2(ctx, s->parent, fd, is_local,
                                   is_arg, var_idx, var_name,
                                   is_const, is_lexical, var_kind);
        if (var_idx < 0)
            return -1;
        is_local = FALSE;
    }
    for(i = 0; i < s->closure_var_count; i++) {
        JSClosureVar *cv = &s->closure_var[i];
        if (cv->var_idx == var_idx && cv->is_arg == is_arg &&
            cv->is_local == is_local)
            return i;
    }
    return add_closure_var(ctx, s, is_local, is_arg, var_idx, var_name,
                           is_const, is_lexical, var_kind);
}

static int get_closure_var(JSContext *ctx, JSFunctionDef *s,
                           JSFunctionDef *fd, BOOL is_arg,
                           int var_idx, JSAtom var_name,
                           BOOL is_const, BOOL is_lexical,
                           JSVarKindEnum var_kind)
{
    return get_closure_var2(ctx, s, fd, TRUE, is_arg,
                            var_idx, var_name, is_const, is_lexical,
                            var_kind);
}

static int get_with_scope_opcode(int op)
{
    if (op == OP_scope_get_var_undef)
        return OP_with_get_var;
    else
        return OP_with_get_var + (op - OP_scope_get_var);
}

static BOOL can_opt_put_ref_value(const uint8_t *bc_buf, int pos)
{
    int opcode = bc_buf[pos];
    return (bc_buf[pos + 1] == OP_put_ref_value &&
            (opcode == OP_insert3 ||
             opcode == OP_perm4 ||
             opcode == OP_nop ||
             opcode == OP_rot3l));
}

static BOOL can_opt_put_global_ref_value(const uint8_t *bc_buf, int pos)
{
    int opcode = bc_buf[pos];
    return (bc_buf[pos + 1] == OP_put_ref_value &&
            (opcode == OP_insert3 ||
             opcode == OP_perm4 ||
             opcode == OP_nop ||
             opcode == OP_rot3l));
}

static int optimize_scope_make_ref(JSContext *ctx, JSFunctionDef *s,
                                   DynBuf *bc, uint8_t *bc_buf,
                                   LabelSlot *ls, int pos_next,
                                   int get_op, int var_idx)
{
    int label_pos, end_pos, pos;

    /* XXX: should optimize `loc(a) += expr` as `expr add_loc(a)`
       but only if expr does not modify `a`.
       should scan the code between pos_next and label_pos
       for operations that can potentially change `a`:
       OP_scope_make_ref(a), function calls, jumps and gosub.
     */
    /* replace the reference get/put with normal variable
       accesses */
    if (bc_buf[pos_next] == OP_get_ref_value) {
        dbuf_putc(bc, get_op);
        dbuf_put_u16(bc, var_idx);
        pos_next++;
    }
    /* remove the OP_label to make room for replacement */
    /* label should have a refcount of 0 anyway */
    /* XXX: should avoid this patch by inserting nops in phase 1 */
    label_pos = ls->pos;
    pos = label_pos - 5;
    assert(bc_buf[pos] == OP_label);
    /* label points to an instruction pair:
       - insert3 / put_ref_value
       - perm4 / put_ref_value
       - rot3l / put_ref_value
       - nop / put_ref_value
     */
    end_pos = label_pos + 2;
    if (bc_buf[label_pos] == OP_insert3)
        bc_buf[pos++] = OP_dup;
    bc_buf[pos] = get_op + 1;
    put_u16(bc_buf + pos + 1, var_idx);
    pos += 3;
    /* pad with OP_nop */
    while (pos < end_pos)
        bc_buf[pos++] = OP_nop;
    return pos_next;
}

static int optimize_scope_make_global_ref(JSContext *ctx, JSFunctionDef *s,
                                          DynBuf *bc, uint8_t *bc_buf,
                                          LabelSlot *ls, int pos_next,
                                          JSAtom var_name)
{
    int label_pos, end_pos, pos, op;
    BOOL is_strict;
    is_strict = ((s->js_mode & JS_MODE_STRICT) != 0);

    /* replace the reference get/put with normal variable
       accesses */
    if (is_strict) {
        /* need to check if the variable exists before evaluating the right
           expression */
        /* XXX: need an extra OP_true if destructuring an array */
        dbuf_putc(bc, OP_check_var);
        dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
    } else {
        /* XXX: need 2 extra OP_true if destructuring an array */
    }
    if (bc_buf[pos_next] == OP_get_ref_value) {
        dbuf_putc(bc, OP_get_var);
        dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
        pos_next++;
    }
    /* remove the OP_label to make room for replacement */
    /* label should have a refcount of 0 anyway */
    /* XXX: should have emitted several OP_nop to avoid this kludge */
    label_pos = ls->pos;
    pos = label_pos - 5;
    assert(bc_buf[pos] == OP_label);
    end_pos = label_pos + 2;
    op = bc_buf[label_pos];
    if (is_strict) {
        if (op != OP_nop) {
            switch(op) {
            case OP_insert3:
                op = OP_insert2;
                break;
            case OP_perm4:
                op = OP_perm3;
                break;
            case OP_rot3l:
                op = OP_swap;
                break;
            default:
                abort();
            }
            bc_buf[pos++] = op;
        }
    } else {
        if (op == OP_insert3)
            bc_buf[pos++] = OP_dup;
    }
    if (is_strict) {
        bc_buf[pos] = OP_put_var_strict;
        /* XXX: need 1 extra OP_drop if destructuring an array */
    } else {
        bc_buf[pos] = OP_put_var;
        /* XXX: need 2 extra OP_drop if destructuring an array */
    }
    put_u32(bc_buf + pos + 1, JS_DupAtom(ctx, var_name));
    pos += 5;
    /* pad with OP_nop */
    while (pos < end_pos)
        bc_buf[pos++] = OP_nop;
    return pos_next;
}

static int add_var_this(JSContext *ctx, JSFunctionDef *fd)
{
    int idx;
    idx = add_var(ctx, fd, JS_ATOM_this);
    if (idx >= 0 && fd->is_derived_class_constructor) {
        JSVarDef *vd = &fd->vars[idx];
        /* XXX: should have is_this flag or var type */
        vd->is_lexical = 1; /* used to trigger 'uninitialized' checks
                               in a derived class constructor */
    }
    return idx;
}

static int resolve_pseudo_var(JSContext *ctx, JSFunctionDef *s,
                               JSAtom var_name)
{
    int var_idx;

    if (!s->has_this_binding)
        return -1;
    switch(var_name) {
    case JS_ATOM_home_object:
        /* 'home_object' pseudo variable */
        if (s->home_object_var_idx < 0)
            s->home_object_var_idx = add_var(ctx, s, var_name);
        var_idx = s->home_object_var_idx;
        break;
    case JS_ATOM_this_active_func:
        /* 'this.active_func' pseudo variable */
        if (s->this_active_func_var_idx < 0)
            s->this_active_func_var_idx = add_var(ctx, s, var_name);
        var_idx = s->this_active_func_var_idx;
        break;
    case JS_ATOM_new_target:
        /* 'new.target' pseudo variable */
        if (s->new_target_var_idx < 0)
            s->new_target_var_idx = add_var(ctx, s, var_name);
        var_idx = s->new_target_var_idx;
        break;
    case JS_ATOM_this:
        /* 'this' pseudo variable */
        if (s->this_var_idx < 0)
            s->this_var_idx = add_var_this(ctx, s);
        var_idx = s->this_var_idx;
        break;
    default:
        var_idx = -1;
        break;
    }
    return var_idx;
}

/* test if 'var_name' is in the variable object on the stack. If is it
   the case, handle it and jump to 'label_done' */
static void var_object_test(JSContext *ctx, JSFunctionDef *s,
                            JSAtom var_name, int op, DynBuf *bc,
                            int *plabel_done, BOOL is_with)
{
    dbuf_putc(bc, get_with_scope_opcode(op));
    dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
    *plabel_done = new_label_fd(s, *plabel_done);
    dbuf_put_u32(bc, *plabel_done);
    dbuf_putc(bc, is_with);
    update_label(s, *plabel_done, 1);
    s->jump_size++;
}
    
/* return the position of the next opcode */
static int resolve_scope_var(JSContext *ctx, JSFunctionDef *s,
                             JSAtom var_name, int scope_level, int op,
                             DynBuf *bc, uint8_t *bc_buf,
                             LabelSlot *ls, int pos_next)
{
    int idx, var_idx, is_put;
    int label_done;
    JSFunctionDef *fd;
    JSVarDef *vd;
    BOOL is_pseudo_var, is_arg_scope;

    label_done = -1;

    /* XXX: could be simpler to use a specific function to
       resolve the pseudo variables */
    is_pseudo_var = (var_name == JS_ATOM_home_object ||
                     var_name == JS_ATOM_this_active_func ||
                     var_name == JS_ATOM_new_target ||
                     var_name == JS_ATOM_this);

    /* resolve local scoped variables */
    var_idx = -1;
    for (idx = s->scopes[scope_level].first; idx >= 0;) {
        vd = &s->vars[idx];
        if (vd->var_name == var_name) {
            if (op == OP_scope_put_var || op == OP_scope_make_ref) {
                if (vd->is_const) {
                    dbuf_putc(bc, OP_throw_error);
                    dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
                    dbuf_putc(bc, JS_THROW_VAR_RO);
                    goto done;
                }
            }
            var_idx = idx;
            break;
        } else
        if (vd->var_name == JS_ATOM__with_ && !is_pseudo_var) {
            dbuf_putc(bc, OP_get_loc);
            dbuf_put_u16(bc, idx);
            var_object_test(ctx, s, var_name, op, bc, &label_done, 1);
        }
        idx = vd->scope_next;
    }
    is_arg_scope = (idx == ARG_SCOPE_END);
    if (var_idx < 0) {
        /* argument scope: variables are not visible but pseudo
           variables are visible */
        if (!is_arg_scope) {
            var_idx = find_var(ctx, s, var_name);
        }

        if (var_idx < 0 && is_pseudo_var)
            var_idx = resolve_pseudo_var(ctx, s, var_name);

        if (var_idx < 0 && var_name == JS_ATOM_arguments &&
            s->has_arguments_binding) {
            /* 'arguments' pseudo variable */
            var_idx = add_arguments_var(ctx, s);
        }
        if (var_idx < 0 && s->is_func_expr && var_name == s->func_name) {
            /* add a new variable with the function name */
            var_idx = add_func_var(ctx, s, var_name);
        }
    }
    if (var_idx >= 0) {
        if ((op == OP_scope_put_var || op == OP_scope_make_ref) &&
            !(var_idx & ARGUMENT_VAR_OFFSET) &&
            s->vars[var_idx].is_const) {
            /* only happens when assigning a function expression name
               in strict mode */
            dbuf_putc(bc, OP_throw_error);
            dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
            dbuf_putc(bc, JS_THROW_VAR_RO);
            goto done;
        }
        /* OP_scope_put_var_init is only used to initialize a
           lexical variable, so it is never used in a with or var object. It
           can be used with a closure (module global variable case). */
        switch (op) {
        case OP_scope_make_ref:
            if (!(var_idx & ARGUMENT_VAR_OFFSET) &&
                s->vars[var_idx].var_kind == JS_VAR_FUNCTION_NAME) {
                /* Create a dummy object reference for the func_var */
                dbuf_putc(bc, OP_object);
                dbuf_putc(bc, OP_get_loc);
                dbuf_put_u16(bc, var_idx);
                dbuf_putc(bc, OP_define_field);
                dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
                dbuf_putc(bc, OP_push_atom_value);
                dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
            } else
            if (label_done == -1 && can_opt_put_ref_value(bc_buf, ls->pos)) {
                int get_op;
                if (var_idx & ARGUMENT_VAR_OFFSET) {
                    get_op = OP_get_arg;
                    var_idx -= ARGUMENT_VAR_OFFSET;
                } else {
                    if (s->vars[var_idx].is_lexical)
                        get_op = OP_get_loc_check;
                    else
                        get_op = OP_get_loc;
                }
                pos_next = optimize_scope_make_ref(ctx, s, bc, bc_buf, ls,
                                                   pos_next, get_op, var_idx);
            } else {
                /* Create a dummy object with a named slot that is
                   a reference to the local variable */
                if (var_idx & ARGUMENT_VAR_OFFSET) {
                    dbuf_putc(bc, OP_make_arg_ref);
                    dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
                    dbuf_put_u16(bc, var_idx - ARGUMENT_VAR_OFFSET);
                } else {
                    dbuf_putc(bc, OP_make_loc_ref);
                    dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
                    dbuf_put_u16(bc, var_idx);
                }
            }
            break;
        case OP_scope_get_ref:
            dbuf_putc(bc, OP_undefined);
            /* fall thru */
        case OP_scope_get_var_undef:
        case OP_scope_get_var:
        case OP_scope_put_var:
        case OP_scope_put_var_init:
            is_put = (op == OP_scope_put_var || op == OP_scope_put_var_init);
            if (var_idx & ARGUMENT_VAR_OFFSET) {
                dbuf_putc(bc, OP_get_arg + is_put);
                dbuf_put_u16(bc, var_idx - ARGUMENT_VAR_OFFSET);
            } else {
                if (is_put) {
                    if (s->vars[var_idx].is_lexical) {
                        if (op == OP_scope_put_var_init) {
                            /* 'this' can only be initialized once */
                            if (var_name == JS_ATOM_this)
                                dbuf_putc(bc, OP_put_loc_check_init);
                            else
                                dbuf_putc(bc, OP_put_loc);
                        } else {
                            dbuf_putc(bc, OP_put_loc_check);
                        }
                    } else {
                        dbuf_putc(bc, OP_put_loc);
                    }
                } else {
                    if (s->vars[var_idx].is_lexical) {
                        dbuf_putc(bc, OP_get_loc_check);
                    } else {
                        dbuf_putc(bc, OP_get_loc);
                    }
                }
                dbuf_put_u16(bc, var_idx);
            }
            break;
        case OP_scope_delete_var:
            dbuf_putc(bc, OP_push_false);
            break;
        }
        goto done;
    }
    /* check eval object */
    if (!is_arg_scope && s->var_object_idx >= 0 && !is_pseudo_var) {
        dbuf_putc(bc, OP_get_loc);
        dbuf_put_u16(bc, s->var_object_idx);
        var_object_test(ctx, s, var_name, op, bc, &label_done, 0);
    }
    /* check eval object in argument scope */
    if (s->arg_var_object_idx >= 0 && !is_pseudo_var) {
        dbuf_putc(bc, OP_get_loc);
        dbuf_put_u16(bc, s->arg_var_object_idx);
        var_object_test(ctx, s, var_name, op, bc, &label_done, 0);
    }

    /* check parent scopes */
    for (fd = s; fd->parent;) {
        scope_level = fd->parent_scope_level;
        fd = fd->parent;
        for (idx = fd->scopes[scope_level].first; idx >= 0;) {
            vd = &fd->vars[idx];
            if (vd->var_name == var_name) {
                if (op == OP_scope_put_var || op == OP_scope_make_ref) {
                    if (vd->is_const) {
                        dbuf_putc(bc, OP_throw_error);
                        dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
                        dbuf_putc(bc, JS_THROW_VAR_RO);
                        goto done;
                    }
                }
                var_idx = idx;
                break;
            } else if (vd->var_name == JS_ATOM__with_ && !is_pseudo_var) {
                vd->is_captured = 1;
                idx = get_closure_var(ctx, s, fd, FALSE, idx, vd->var_name, FALSE, FALSE, JS_VAR_NORMAL);
                if (idx >= 0) {
                    dbuf_putc(bc, OP_get_var_ref);
                    dbuf_put_u16(bc, idx);
                    var_object_test(ctx, s, var_name, op, bc, &label_done, 1);
                }
            }
            idx = vd->scope_next;
        }
        is_arg_scope = (idx == ARG_SCOPE_END);
        if (var_idx >= 0)
            break;
        
        if (!is_arg_scope) {
            var_idx = find_var(ctx, fd, var_name);
            if (var_idx >= 0)
                break;
        }
        if (is_pseudo_var) {
            var_idx = resolve_pseudo_var(ctx, fd, var_name);
            if (var_idx >= 0)
                break;
        }
        if (var_name == JS_ATOM_arguments && fd->has_arguments_binding) {
            var_idx = add_arguments_var(ctx, fd);
            break;
        }
        if (fd->is_func_expr && fd->func_name == var_name) {
            /* add a new variable with the function name */
            var_idx = add_func_var(ctx, fd, var_name);
            break;
        }

        /* check eval object */
        if (!is_arg_scope && fd->var_object_idx >= 0 && !is_pseudo_var) {
            vd = &fd->vars[fd->var_object_idx];
            vd->is_captured = 1;
            idx = get_closure_var(ctx, s, fd, FALSE,
                                  fd->var_object_idx, vd->var_name,
                                  FALSE, FALSE, JS_VAR_NORMAL);
            dbuf_putc(bc, OP_get_var_ref);
            dbuf_put_u16(bc, idx);
            var_object_test(ctx, s, var_name, op, bc, &label_done, 0);
        }

        /* check eval object in argument scope */
        if (fd->arg_var_object_idx >= 0 && !is_pseudo_var) {
            vd = &fd->vars[fd->arg_var_object_idx];
            vd->is_captured = 1;
            idx = get_closure_var(ctx, s, fd, FALSE,
                                  fd->arg_var_object_idx, vd->var_name,
                                  FALSE, FALSE, JS_VAR_NORMAL);
            dbuf_putc(bc, OP_get_var_ref);
            dbuf_put_u16(bc, idx);
            var_object_test(ctx, s, var_name, op, bc, &label_done, 0);
        }
        
        if (fd->is_eval)
            break; /* it it necessarily the top level function */
    }

    /* check direct eval scope (in the closure of the eval function
       which is necessarily at the top level) */
    if (!fd)
        fd = s;
    if (var_idx < 0 && fd->is_eval) {
        int idx1;
        for (idx1 = 0; idx1 < fd->closure_var_count; idx1++) {
            JSClosureVar *cv = &fd->closure_var[idx1];
            if (var_name == cv->var_name) {
                if (fd != s) {
                    idx = get_closure_var2(ctx, s, fd,
                                           FALSE,
                                           cv->is_arg, idx1,
                                           cv->var_name, cv->is_const,
                                           cv->is_lexical, cv->var_kind);
                } else {
                    idx = idx1;
                }
                goto has_idx;
            } else if ((cv->var_name == JS_ATOM__var_ ||
                        cv->var_name == JS_ATOM__arg_var_ ||
                        cv->var_name == JS_ATOM__with_) && !is_pseudo_var) {
                int is_with = (cv->var_name == JS_ATOM__with_);
                if (fd != s) {
                    idx = get_closure_var2(ctx, s, fd,
                                           FALSE,
                                           cv->is_arg, idx1,
                                           cv->var_name, FALSE, FALSE,
                                           JS_VAR_NORMAL);
                } else {
                    idx = idx1;
                }
                dbuf_putc(bc, OP_get_var_ref);
                dbuf_put_u16(bc, idx);
                var_object_test(ctx, s, var_name, op, bc, &label_done, is_with);
            }
        }
    }

    if (var_idx >= 0) {
        /* find the corresponding closure variable */
        if (var_idx & ARGUMENT_VAR_OFFSET) {
            fd->args[var_idx - ARGUMENT_VAR_OFFSET].is_captured = 1;
            idx = get_closure_var(ctx, s, fd,
                                  TRUE, var_idx - ARGUMENT_VAR_OFFSET,
                                  var_name, FALSE, FALSE, JS_VAR_NORMAL);
        } else {
            fd->vars[var_idx].is_captured = 1;
            idx = get_closure_var(ctx, s, fd,
                                  FALSE, var_idx,
                                  var_name,
                                  fd->vars[var_idx].is_const,
                                  fd->vars[var_idx].is_lexical,
                                  fd->vars[var_idx].var_kind);
        }
        if (idx >= 0) {
        has_idx:
            if ((op == OP_scope_put_var || op == OP_scope_make_ref) &&
                s->closure_var[idx].is_const) {
                dbuf_putc(bc, OP_throw_error);
                dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
                dbuf_putc(bc, JS_THROW_VAR_RO);
                goto done;
            }
            switch (op) {
            case OP_scope_make_ref:
                if (s->closure_var[idx].var_kind == JS_VAR_FUNCTION_NAME) {
                    /* Create a dummy object reference for the func_var */
                    dbuf_putc(bc, OP_object);
                    dbuf_putc(bc, OP_get_var_ref);
                    dbuf_put_u16(bc, idx);
                    dbuf_putc(bc, OP_define_field);
                    dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
                    dbuf_putc(bc, OP_push_atom_value);
                    dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
                } else
                if (label_done == -1 &&
                    can_opt_put_ref_value(bc_buf, ls->pos)) {
                    int get_op;
                    if (s->closure_var[idx].is_lexical)
                        get_op = OP_get_var_ref_check;
                    else
                        get_op = OP_get_var_ref;
                    pos_next = optimize_scope_make_ref(ctx, s, bc, bc_buf, ls,
                                                       pos_next,
                                                       get_op, idx);
                } else {
                    /* Create a dummy object with a named slot that is
                       a reference to the closure variable */
                    dbuf_putc(bc, OP_make_var_ref_ref);
                    dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
                    dbuf_put_u16(bc, idx);
                }
                break;
            case OP_scope_get_ref:
                /* XXX: should create a dummy object with a named slot that is
                   a reference to the closure variable */
                dbuf_putc(bc, OP_undefined);
                /* fall thru */
            case OP_scope_get_var_undef:
            case OP_scope_get_var:
            case OP_scope_put_var:
            case OP_scope_put_var_init:
                is_put = (op == OP_scope_put_var ||
                          op == OP_scope_put_var_init);
                if (is_put) {
                    if (s->closure_var[idx].is_lexical) {
                        if (op == OP_scope_put_var_init) {
                            /* 'this' can only be initialized once */
                            if (var_name == JS_ATOM_this)
                                dbuf_putc(bc, OP_put_var_ref_check_init);
                            else
                                dbuf_putc(bc, OP_put_var_ref);
                        } else {
                            dbuf_putc(bc, OP_put_var_ref_check);
                        }
                    } else {
                        dbuf_putc(bc, OP_put_var_ref);
                    }
                } else {
                    if (s->closure_var[idx].is_lexical) {
                        dbuf_putc(bc, OP_get_var_ref_check);
                    } else {
                        dbuf_putc(bc, OP_get_var_ref);
                    }
                }
                dbuf_put_u16(bc, idx);
                break;
            case OP_scope_delete_var:
                dbuf_putc(bc, OP_push_false);
                break;
            }
            goto done;
        }
    }

    /* global variable access */

    switch (op) {
    case OP_scope_make_ref:
        if (label_done == -1 && can_opt_put_global_ref_value(bc_buf, ls->pos)) {
            pos_next = optimize_scope_make_global_ref(ctx, s, bc, bc_buf, ls,
                                                      pos_next, var_name);
        } else {
            dbuf_putc(bc, OP_make_var_ref);
            dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
        }
        break;
    case OP_scope_get_ref:
        /* XXX: should create a dummy object with a named slot that is
           a reference to the global variable */
        dbuf_putc(bc, OP_undefined);
        dbuf_putc(bc, OP_get_var);
        dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
        break;
    case OP_scope_get_var_undef:
    case OP_scope_get_var:
    case OP_scope_put_var:
        dbuf_putc(bc, OP_get_var_undef + (op - OP_scope_get_var_undef));
        dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
        break;
    case OP_scope_put_var_init:
        dbuf_putc(bc, OP_put_var_init);
        dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
        break;
    case OP_scope_delete_var:
        dbuf_putc(bc, OP_delete_var);
        dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
        break;
    }
done:
    if (label_done >= 0) {
        dbuf_putc(bc, OP_label);
        dbuf_put_u32(bc, label_done);
        s->label_slots[label_done].pos2 = bc->size;
    }
    return pos_next;
}

/* search in all scopes */
static int find_private_class_field_all(JSContext *ctx, JSFunctionDef *fd,
                                        JSAtom name, int scope_level)
{
    int idx;

    idx = fd->scopes[scope_level].first;
    while (idx >= 0) {
        if (fd->vars[idx].var_name == name)
            return idx;
        idx = fd->vars[idx].scope_next;
    }
    return -1;
}

static void get_loc_or_ref(DynBuf *bc, BOOL is_ref, int idx)
{
    /* if the field is not initialized, the error is catched when
       accessing it */
    if (is_ref) 
        dbuf_putc(bc, OP_get_var_ref);
    else
        dbuf_putc(bc, OP_get_loc);
    dbuf_put_u16(bc, idx);
}

static int resolve_scope_private_field1(JSContext *ctx,
                                        BOOL *pis_ref, int *pvar_kind,
                                        JSFunctionDef *s,
                                        JSAtom var_name, int scope_level)
{
    int idx, var_kind;
    JSFunctionDef *fd;
    BOOL is_ref;
    
    fd = s;
    is_ref = FALSE;
    for(;;) {
        idx = find_private_class_field_all(ctx, fd, var_name, scope_level);
        if (idx >= 0) {
            var_kind = fd->vars[idx].var_kind;
            if (is_ref) {
                idx = get_closure_var(ctx, s, fd, FALSE, idx, var_name,
                                      TRUE, TRUE, JS_VAR_NORMAL);
                if (idx < 0)
                    return -1;
            }
            break;
        }
        scope_level = fd->parent_scope_level;
        if (!fd->parent) {
            if (fd->is_eval) {
                /* closure of the eval function (top level) */
                for (idx = 0; idx < fd->closure_var_count; idx++) {
                    JSClosureVar *cv = &fd->closure_var[idx];
                    if (cv->var_name == var_name) {
                        var_kind = cv->var_kind;
                        is_ref = TRUE;
                        if (fd != s) {
                            idx = get_closure_var2(ctx, s, fd,
                                                   FALSE,
                                                   cv->is_arg, idx,
                                                   cv->var_name, cv->is_const,
                                                   cv->is_lexical,
                                                   cv->var_kind);
                            if (idx < 0)
                                return -1;
                        }
                        goto done;
                    }
                }
            }
            /* XXX: no line number info */
            JS_ThrowSyntaxErrorAtom(ctx, "undefined private field '%s'",
                                    var_name);
            return -1;
        } else {
            fd = fd->parent;
        }
        is_ref = TRUE;
    }
 done:
    *pis_ref = is_ref;
    *pvar_kind = var_kind;
    return idx;
}

/* return 0 if OK or -1 if the private field could not be resolved */
static int resolve_scope_private_field(JSContext *ctx, JSFunctionDef *s,
                                       JSAtom var_name, int scope_level, int op,
                                       DynBuf *bc)
{
    int idx, var_kind;
    BOOL is_ref;

    idx = resolve_scope_private_field1(ctx, &is_ref, &var_kind, s,
                                       var_name, scope_level);
    if (idx < 0)
        return -1;
    assert(var_kind != JS_VAR_NORMAL);
    switch (op) {
    case OP_scope_get_private_field:
    case OP_scope_get_private_field2:
        switch(var_kind) {
        case JS_VAR_PRIVATE_FIELD:
            if (op == OP_scope_get_private_field2)
                dbuf_putc(bc, OP_dup);
            get_loc_or_ref(bc, is_ref, idx);
            dbuf_putc(bc, OP_get_private_field);
            break;
        case JS_VAR_PRIVATE_METHOD:
            get_loc_or_ref(bc, is_ref, idx);
            dbuf_putc(bc, OP_check_brand);
            if (op != OP_scope_get_private_field2)
                dbuf_putc(bc, OP_nip);
            break;
        case JS_VAR_PRIVATE_GETTER:
        case JS_VAR_PRIVATE_GETTER_SETTER:
            if (op == OP_scope_get_private_field2)
                dbuf_putc(bc, OP_dup);
            get_loc_or_ref(bc, is_ref, idx);
            dbuf_putc(bc, OP_check_brand);
            dbuf_putc(bc, OP_call_method);
            dbuf_put_u16(bc, 0);
            break;
        case JS_VAR_PRIVATE_SETTER:
            /* XXX: add clearer error message */
            dbuf_putc(bc, OP_throw_error);
            dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
            dbuf_putc(bc, JS_THROW_VAR_RO);
            break;
        default:
            abort();
        }
        break;
    case OP_scope_put_private_field:
        switch(var_kind) {
        case JS_VAR_PRIVATE_FIELD:
            get_loc_or_ref(bc, is_ref, idx);
            dbuf_putc(bc, OP_put_private_field);
            break;
        case JS_VAR_PRIVATE_METHOD:
        case JS_VAR_PRIVATE_GETTER:
            /* XXX: add clearer error message */
            dbuf_putc(bc, OP_throw_error);
            dbuf_put_u32(bc, JS_DupAtom(ctx, var_name));
            dbuf_putc(bc, JS_THROW_VAR_RO);
            break;
        case JS_VAR_PRIVATE_SETTER:
        case JS_VAR_PRIVATE_GETTER_SETTER:
            {
                JSAtom setter_name = get_private_setter_name(ctx, var_name);
                if (setter_name == JS_ATOM_NULL)
                    return -1;
                idx = resolve_scope_private_field1(ctx, &is_ref,
                                                   &var_kind, s,
                                                   setter_name, scope_level);
                JS_FreeAtom(ctx, setter_name);
                if (idx < 0)
                    return -1;
                assert(var_kind == JS_VAR_PRIVATE_SETTER);
                get_loc_or_ref(bc, is_ref, idx);
                dbuf_putc(bc, OP_swap);
                /* obj func value */
                dbuf_putc(bc, OP_rot3r);
                /* value obj func */
                dbuf_putc(bc, OP_check_brand);
                dbuf_putc(bc, OP_rot3l);
                /* obj func value */
                dbuf_putc(bc, OP_call_method);
                dbuf_put_u16(bc, 1);
            }
            break;
        default:
            abort();
        }
        break;
    default:
        abort();
    }
    return 0;
}

static void mark_eval_captured_variables(JSContext *ctx, JSFunctionDef *s,
                                         int scope_level)
{
    int idx;
    JSVarDef *vd;

    for (idx = s->scopes[scope_level].first; idx >= 0;) {
        vd = &s->vars[idx];
        vd->is_captured = 1;
        idx = vd->scope_next;
    }
}

/* XXX: should handle the argument scope generically */
static BOOL is_var_in_arg_scope(const JSVarDef *vd)
{
    return (vd->var_name == JS_ATOM_home_object ||
            vd->var_name == JS_ATOM_this_active_func ||
            vd->var_name == JS_ATOM_new_target ||
            vd->var_name == JS_ATOM_this ||
            vd->var_name == JS_ATOM__arg_var_ ||
            vd->var_kind == JS_VAR_FUNCTION_NAME);
}

static void add_eval_variables(JSContext *ctx, JSFunctionDef *s)
{
    JSFunctionDef *fd;
    JSVarDef *vd;
    int i, scope_level, scope_idx;
    BOOL has_arguments_binding, has_this_binding, is_arg_scope;

    /* in non strict mode, variables are created in the caller's
       environment object */
    if (!s->is_eval && !(s->js_mode & JS_MODE_STRICT)) {
        s->var_object_idx = add_var(ctx, s, JS_ATOM__var_);
        if (s->has_parameter_expressions) {
            /* an additional variable object is needed for the
               argument scope */
            s->arg_var_object_idx = add_var(ctx, s, JS_ATOM__arg_var_);
        }
    }

    /* eval can potentially use 'arguments' so we must define it */
    has_this_binding = s->has_this_binding;
    if (has_this_binding) {
        if (s->this_var_idx < 0)
            s->this_var_idx = add_var_this(ctx, s);
        if (s->new_target_var_idx < 0)
            s->new_target_var_idx = add_var(ctx, s, JS_ATOM_new_target);
        if (s->is_derived_class_constructor && s->this_active_func_var_idx < 0)
            s->this_active_func_var_idx = add_var(ctx, s, JS_ATOM_this_active_func);
        if (s->has_home_object && s->home_object_var_idx < 0)
            s->home_object_var_idx = add_var(ctx, s, JS_ATOM_home_object);
    }
    has_arguments_binding = s->has_arguments_binding;
    if (has_arguments_binding) {
        add_arguments_var(ctx, s);
        /* also add an arguments binding in the argument scope to
           raise an error if a direct eval in the argument scope tries
           to redefine it */
        if (s->has_parameter_expressions && !(s->js_mode & JS_MODE_STRICT))
            add_arguments_arg(ctx, s);
    }
    if (s->is_func_expr && s->func_name != JS_ATOM_NULL)
        add_func_var(ctx, s, s->func_name);

    /* eval can use all the variables of the enclosing functions, so
       they must be all put in the closure. The closure variables are
       ordered by scope. It works only because no closure are created
       before. */
    assert(s->is_eval || s->closure_var_count == 0);

    /* XXX: inefficient, but eval performance is less critical */
    fd = s;
    for(;;) {
        scope_level = fd->parent_scope_level;
        fd = fd->parent;
        if (!fd)
            break;
        /* add 'this' if it was not previously added */
        if (!has_this_binding && fd->has_this_binding) {
            if (fd->this_var_idx < 0)
                fd->this_var_idx = add_var_this(ctx, fd);
            if (fd->new_target_var_idx < 0)
                fd->new_target_var_idx = add_var(ctx, fd, JS_ATOM_new_target);
            if (fd->is_derived_class_constructor && fd->this_active_func_var_idx < 0)
                fd->this_active_func_var_idx = add_var(ctx, fd, JS_ATOM_this_active_func);
            if (fd->has_home_object && fd->home_object_var_idx < 0)
                fd->home_object_var_idx = add_var(ctx, fd, JS_ATOM_home_object);
            has_this_binding = TRUE;
        }
        /* add 'arguments' if it was not previously added */
        if (!has_arguments_binding && fd->has_arguments_binding) {
            add_arguments_var(ctx, fd);
            has_arguments_binding = TRUE;
        }
        /* add function name */
        if (fd->is_func_expr && fd->func_name != JS_ATOM_NULL)
            add_func_var(ctx, fd, fd->func_name);

        /* add lexical variables */
        scope_idx = fd->scopes[scope_level].first;
        while (scope_idx >= 0) {
            vd = &fd->vars[scope_idx];
            vd->is_captured = 1;
            get_closure_var(ctx, s, fd, FALSE, scope_idx,
                            vd->var_name, vd->is_const, vd->is_lexical, vd->var_kind);
            scope_idx = vd->scope_next;
        }
        is_arg_scope = (scope_idx == ARG_SCOPE_END);
        if (!is_arg_scope) {
            /* add unscoped variables */
            for(i = 0; i < fd->arg_count; i++) {
                vd = &fd->args[i];
                if (vd->var_name != JS_ATOM_NULL) {
                    get_closure_var(ctx, s, fd,
                                    TRUE, i, vd->var_name, FALSE, FALSE,
                                    JS_VAR_NORMAL);
                }
            }
            for(i = 0; i < fd->var_count; i++) {
                vd = &fd->vars[i];
                /* do not close top level last result */
                if (vd->scope_level == 0 &&
                    vd->var_name != JS_ATOM__ret_ &&
                    vd->var_name != JS_ATOM_NULL) {
                    get_closure_var(ctx, s, fd,
                                    FALSE, i, vd->var_name, FALSE, FALSE,
                                    JS_VAR_NORMAL);
                }
            }
        } else {
            for(i = 0; i < fd->var_count; i++) {
                vd = &fd->vars[i];
                /* do not close top level last result */
                if (vd->scope_level == 0 && is_var_in_arg_scope(vd)) {
                    get_closure_var(ctx, s, fd,
                                    FALSE, i, vd->var_name, FALSE, FALSE,
                                    JS_VAR_NORMAL);
                }
            }
        }
        if (fd->is_eval) {
            int idx;
            /* add direct eval variables (we are necessarily at the
               top level) */
            for (idx = 0; idx < fd->closure_var_count; idx++) {
                JSClosureVar *cv = &fd->closure_var[idx];
                get_closure_var2(ctx, s, fd,
                                 FALSE, cv->is_arg,
                                 idx, cv->var_name, cv->is_const,
                                 cv->is_lexical, cv->var_kind);
            }
        }
    }
}

static void set_closure_from_var(JSContext *ctx, JSClosureVar *cv,
                                 JSVarDef *vd, int var_idx)
{
    cv->is_local = TRUE;
    cv->is_arg = FALSE;
    cv->is_const = vd->is_const;
    cv->is_lexical = vd->is_lexical;
    cv->var_kind = vd->var_kind;
    cv->var_idx = var_idx;
    cv->var_name = JS_DupAtom(ctx, vd->var_name);
}

/* for direct eval compilation: add references to the variables of the
   calling function */
static __exception int add_closure_variables(JSContext *ctx, JSFunctionDef *s,
                                             JSFunctionBytecode *b, int scope_idx)
{
    int i, count;
    JSVarDef *vd;
    BOOL is_arg_scope;
    
    count = b->arg_count + b->var_count + b->closure_var_count;
    s->closure_var = NULL;
    s->closure_var_count = 0;
    s->closure_var_size = count;
    if (count == 0)
        return 0;
    s->closure_var = js_malloc(ctx, sizeof(s->closure_var[0]) * count);
    if (!s->closure_var)
        return -1;
    /* Add lexical variables in scope at the point of evaluation */
    for (i = scope_idx; i >= 0;) {
        vd = &b->vardefs[b->arg_count + i];
        if (vd->scope_level > 0) {
            JSClosureVar *cv = &s->closure_var[s->closure_var_count++];
            set_closure_from_var(ctx, cv, vd, i);
        }
        i = vd->scope_next;
    }
    is_arg_scope = (i == ARG_SCOPE_END);
    if (!is_arg_scope) {
        /* Add argument variables */
        for(i = 0; i < b->arg_count; i++) {
            JSClosureVar *cv = &s->closure_var[s->closure_var_count++];
            vd = &b->vardefs[i];
            cv->is_local = TRUE;
            cv->is_arg = TRUE;
            cv->is_const = FALSE;
            cv->is_lexical = FALSE;
            cv->var_kind = JS_VAR_NORMAL;
            cv->var_idx = i;
            cv->var_name = JS_DupAtom(ctx, vd->var_name);
        }
        /* Add local non lexical variables */
        for(i = 0; i < b->var_count; i++) {
            vd = &b->vardefs[b->arg_count + i];
            if (vd->scope_level == 0 && vd->var_name != JS_ATOM__ret_) {
                JSClosureVar *cv = &s->closure_var[s->closure_var_count++];
                set_closure_from_var(ctx, cv, vd, i);
            }
        }
    } else {
        /* only add pseudo variables */
        for(i = 0; i < b->var_count; i++) {
            vd = &b->vardefs[b->arg_count + i];
            if (vd->scope_level == 0 && is_var_in_arg_scope(vd)) {
                JSClosureVar *cv = &s->closure_var[s->closure_var_count++];
                set_closure_from_var(ctx, cv, vd, i);
            }
        }
    }
    for(i = 0; i < b->closure_var_count; i++) {
        JSClosureVar *cv0 = &b->closure_var[i];
        JSClosureVar *cv = &s->closure_var[s->closure_var_count++];
        cv->is_local = FALSE;
        cv->is_arg = cv0->is_arg;
        cv->is_const = cv0->is_const;
        cv->is_lexical = cv0->is_lexical;
        cv->var_kind = cv0->var_kind;
        cv->var_idx = i;
        cv->var_name = JS_DupAtom(ctx, cv0->var_name);
    }
    return 0;
}

typedef struct CodeContext {
    const uint8_t *bc_buf; /* code buffer */
    int bc_len;   /* length of the code buffer */
    int pos;      /* position past the matched code pattern */
    int line_num; /* last visited OP_line_num parameter or -1 */
    int op;
    int idx;
    int label;
    int val;
    JSAtom atom;
} CodeContext;

#define M2(op1, op2)            ((op1) | ((op2) << 8))
#define M3(op1, op2, op3)       ((op1) | ((op2) << 8) | ((op3) << 16))
#define M4(op1, op2, op3, op4)  ((op1) | ((op2) << 8) | ((op3) << 16) | ((op4) << 24))

static BOOL code_match(CodeContext *s, int pos, ...)
{
    const uint8_t *tab = s->bc_buf;
    int op, len, op1, line_num, pos_next;
    va_list ap;
    BOOL ret = FALSE;

    line_num = -1;
    va_start(ap, pos);

    for(;;) {
        op1 = va_arg(ap, int);
        if (op1 == -1) {
            s->pos = pos;
            s->line_num = line_num;
            ret = TRUE;
            break;
        }
        for (;;) {
            if (pos >= s->bc_len)
                goto done;
            op = tab[pos];
            len = opcode_info[op].size;
            pos_next = pos + len;
            if (pos_next > s->bc_len)
                goto done;
            if (op == OP_line_num) {
                line_num = get_u32(tab + pos + 1);
                pos = pos_next;
            } else {
                break;
            }
        }
        if (op != op1) {
            if (op1 == (uint8_t)op1 || !op)
                break;
            if (op != (uint8_t)op1
            &&  op != (uint8_t)(op1 >> 8)
            &&  op != (uint8_t)(op1 >> 16)
            &&  op != (uint8_t)(op1 >> 24)) {
                break;
            }
            s->op = op;
        }

        pos++;
        switch(opcode_info[op].fmt) {
        case OP_FMT_loc8:
        case OP_FMT_u8:
            {
                int idx = tab[pos];
                int arg = va_arg(ap, int);
                if (arg == -1) {
                    s->idx = idx;
                } else {
                    if (arg != idx)
                        goto done;
                }
                break;
            }
        case OP_FMT_u16:
        case OP_FMT_npop:
        case OP_FMT_loc:
        case OP_FMT_arg:
        case OP_FMT_var_ref:
            {
                int idx = get_u16(tab + pos);
                int arg = va_arg(ap, int);
                if (arg == -1) {
                    s->idx = idx;
                } else {
                    if (arg != idx)
                        goto done;
                }
                break;
            }
        case OP_FMT_i32:
        case OP_FMT_u32:
        case OP_FMT_label:
        case OP_FMT_const:
            {
                s->label = get_u32(tab + pos);
                break;
            }
        case OP_FMT_label_u16:
            {
                s->label = get_u32(tab + pos);
                s->val = get_u16(tab + pos + 4);
                break;
            }
        case OP_FMT_atom:
            {
                s->atom = get_u32(tab + pos);
                break;
            }
        case OP_FMT_atom_u8:
            {
                s->atom = get_u32(tab + pos);
                s->val = get_u8(tab + pos + 4);
                break;
            }
        case OP_FMT_atom_u16:
            {
                s->atom = get_u32(tab + pos);
                s->val = get_u16(tab + pos + 4);
                break;
            }
        case OP_FMT_atom_label_u8:
            {
                s->atom = get_u32(tab + pos);
                s->label = get_u32(tab + pos + 4);
                s->val = get_u8(tab + pos + 8);
                break;
            }
        default:
            break;
        }
        pos = pos_next;
    }
 done:
    va_end(ap);
    return ret;
}

static void instantiate_hoisted_definitions(JSContext *ctx, JSFunctionDef *s, DynBuf *bc)
{
    int i, idx, label_next = -1;

    /* add the hoisted functions in arguments and local variables */
    for(i = 0; i < s->arg_count; i++) {
        JSVarDef *vd = &s->args[i];
        if (vd->func_pool_idx >= 0) {
            dbuf_putc(bc, OP_fclosure);
            dbuf_put_u32(bc, vd->func_pool_idx);
            dbuf_putc(bc, OP_put_arg);
            dbuf_put_u16(bc, i);
        }
    }
    for(i = 0; i < s->var_count; i++) {
        JSVarDef *vd = &s->vars[i];
        if (vd->scope_level == 0 && vd->func_pool_idx >= 0) {
            dbuf_putc(bc, OP_fclosure);
            dbuf_put_u32(bc, vd->func_pool_idx);
            dbuf_putc(bc, OP_put_loc);
            dbuf_put_u16(bc, i);
        }
    }

    /* the module global variables must be initialized before
       evaluating the module so that the exported functions are
       visible if there are cyclic module references */
    if (s->module) {
        label_next = new_label_fd(s, -1);
        
        /* if 'this' is true, initialize the global variables and return */
        dbuf_putc(bc, OP_push_this);
        dbuf_putc(bc, OP_if_false);
        dbuf_put_u32(bc, label_next);
        update_label(s, label_next, 1);
        s->jump_size++;
    }
    
    /* add the global variables (only happens if s->is_global_var is
       true) */
    for(i = 0; i < s->global_var_count; i++) {
        JSGlobalVar *hf = &s->global_vars[i];
        int has_closure = 0;
        BOOL force_init = hf->force_init;
        /* we are in an eval, so the closure contains all the
           enclosing variables */
        /* If the outer function has a variable environment,
           create a property for the variable there */
        for(idx = 0; idx < s->closure_var_count; idx++) {
            JSClosureVar *cv = &s->closure_var[idx];
            if (cv->var_name == hf->var_name) {
                has_closure = 2;
                force_init = FALSE;
                break;
            }
            if (cv->var_name == JS_ATOM__var_ ||
                cv->var_name == JS_ATOM__arg_var_) {
                dbuf_putc(bc, OP_get_var_ref);
                dbuf_put_u16(bc, idx);
                has_closure = 1;
                force_init = TRUE;
                break;
            }
        }
        if (!has_closure) {
            int flags;
            
            flags = 0;
            if (s->eval_type != JS_EVAL_TYPE_GLOBAL)
                flags |= JS_PROP_CONFIGURABLE;
            if (hf->cpool_idx >= 0 && !hf->is_lexical) {
                /* global function definitions need a specific handling */
                dbuf_putc(bc, OP_fclosure);
                dbuf_put_u32(bc, hf->cpool_idx);
                
                dbuf_putc(bc, OP_define_func);
                dbuf_put_u32(bc, JS_DupAtom(ctx, hf->var_name));
                dbuf_putc(bc, flags);
                
                goto done_global_var;
            } else {
                if (hf->is_lexical) {
                    flags |= DEFINE_GLOBAL_LEX_VAR;
                    if (!hf->is_const)
                        flags |= JS_PROP_WRITABLE;
                }
                dbuf_putc(bc, OP_define_var);
                dbuf_put_u32(bc, JS_DupAtom(ctx, hf->var_name));
                dbuf_putc(bc, flags);
            }
        }
        if (hf->cpool_idx >= 0 || force_init) {
            if (hf->cpool_idx >= 0) {
                dbuf_putc(bc, OP_fclosure);
                dbuf_put_u32(bc, hf->cpool_idx);
                if (hf->var_name == JS_ATOM__default_) {
                    /* set default export function name */
                    dbuf_putc(bc, OP_set_name);
                    dbuf_put_u32(bc, JS_DupAtom(ctx, JS_ATOM_default));
                }
            } else {
                dbuf_putc(bc, OP_undefined);
            }
            if (has_closure == 2) {
                dbuf_putc(bc, OP_put_var_ref);
                dbuf_put_u16(bc, idx);
            } else if (has_closure == 1) {
                dbuf_putc(bc, OP_define_field);
                dbuf_put_u32(bc, JS_DupAtom(ctx, hf->var_name));
                dbuf_putc(bc, OP_drop);
            } else {
                /* XXX: Check if variable is writable and enumerable */
                dbuf_putc(bc, OP_put_var);
                dbuf_put_u32(bc, JS_DupAtom(ctx, hf->var_name));
            }
        }
    done_global_var:
        JS_FreeAtom(ctx, hf->var_name);
    }

    if (s->module) {
        dbuf_putc(bc, OP_return_undef);
        
        dbuf_putc(bc, OP_label);
        dbuf_put_u32(bc, label_next);
        s->label_slots[label_next].pos2 = bc->size;
    }

    js_free(ctx, s->global_vars);
    s->global_vars = NULL;
    s->global_var_count = 0;
    s->global_var_size = 0;
}

static int skip_dead_code(JSFunctionDef *s, const uint8_t *bc_buf, int bc_len,
                          int pos, int *linep)
{
    int op, len, label;

    for (; pos < bc_len; pos += len) {
        op = bc_buf[pos];
        len = opcode_info[op].size;
        if (op == OP_line_num) {
            *linep = get_u32(bc_buf + pos + 1);
        } else
        if (op == OP_label) {
            label = get_u32(bc_buf + pos + 1);
            if (update_label(s, label, 0) > 0)
                break;
#if 0
            if (s->label_slots[label].first_reloc) {
                printf("line %d: unreferenced label %d:%d has relocations\n",
                       *linep, label, s->label_slots[label].pos2);
            }
#endif
            assert(s->label_slots[label].first_reloc == NULL);
        } else {
            /* XXX: output a warning for unreachable code? */
            JSAtom atom;
            switch(opcode_info[op].fmt) {
            case OP_FMT_label:
            case OP_FMT_label_u16:
                label = get_u32(bc_buf + pos + 1);
                update_label(s, label, -1);
                break;
            case OP_FMT_atom_label_u8:
            case OP_FMT_atom_label_u16:
                label = get_u32(bc_buf + pos + 5);
                update_label(s, label, -1);
                /* fall thru */
            case OP_FMT_atom:
            case OP_FMT_atom_u8:
            case OP_FMT_atom_u16:
                atom = get_u32(bc_buf + pos + 1);
                JS_FreeAtom(s->ctx, atom);
                break;
            default:
                break;
            }
        }
    }
    return pos;
}

static int get_label_pos(JSFunctionDef *s, int label)
{
    int i, pos;
    for (i = 0; i < 20; i++) {
        pos = s->label_slots[label].pos;
        for (;;) {
            switch (s->byte_code.buf[pos]) {
            case OP_line_num:
            case OP_label:
                pos += 5;
                continue;
            case OP_goto:
                label = get_u32(s->byte_code.buf + pos + 1);
                break;
            default:
                return pos;
            }
            break;
        }
    }
    return pos;
}

/* convert global variable accesses to local variables or closure
   variables when necessary */
static __exception int resolve_variables(JSContext *ctx, JSFunctionDef *s)
{
    int pos, pos_next, bc_len, op, len, i, idx, line_num;
    uint8_t *bc_buf;
    JSAtom var_name;
    DynBuf bc_out;
    CodeContext cc;
    int scope;

    cc.bc_buf = bc_buf = s->byte_code.buf;
    cc.bc_len = bc_len = s->byte_code.size;
    js_dbuf_init(ctx, &bc_out);

    /* first pass for runtime checks (must be done before the
       variables are created) */
    for(i = 0; i < s->global_var_count; i++) {
        JSGlobalVar *hf = &s->global_vars[i];
        int flags;
        
        /* check if global variable (XXX: simplify) */
        for(idx = 0; idx < s->closure_var_count; idx++) {
            JSClosureVar *cv = &s->closure_var[idx];
            if (cv->var_name == hf->var_name) {
                if (s->eval_type == JS_EVAL_TYPE_DIRECT &&
                    cv->is_lexical) {
                    /* Check if a lexical variable is
                       redefined as 'var'. XXX: Could abort
                       compilation here, but for consistency
                       with the other checks, we delay the
                       error generation. */
                    dbuf_putc(&bc_out, OP_throw_error);
                    dbuf_put_u32(&bc_out, JS_DupAtom(ctx, hf->var_name));
                    dbuf_putc(&bc_out, JS_THROW_VAR_REDECL);
                }
                goto next;
            }
            if (cv->var_name == JS_ATOM__var_ ||
                cv->var_name == JS_ATOM__arg_var_)
                goto next;
        }
        
        dbuf_putc(&bc_out, OP_check_define_var);
        dbuf_put_u32(&bc_out, JS_DupAtom(ctx, hf->var_name));
        flags = 0;
        if (hf->is_lexical)
            flags |= DEFINE_GLOBAL_LEX_VAR;
        if (hf->cpool_idx >= 0)
            flags |= DEFINE_GLOBAL_FUNC_VAR;
        dbuf_putc(&bc_out, flags);
    next: ;
    }

    line_num = 0; /* avoid warning */
    for (pos = 0; pos < bc_len; pos = pos_next) {
        op = bc_buf[pos];
        len = opcode_info[op].size;
        pos_next = pos + len;
        switch(op) {
        case OP_line_num:
            line_num = get_u32(bc_buf + pos + 1);
            s->line_number_size++;
            goto no_change;

        case OP_eval: /* convert scope index to adjusted variable index */
            {
                int call_argc = get_u16(bc_buf + pos + 1);
                scope = get_u16(bc_buf + pos + 1 + 2);
                mark_eval_captured_variables(ctx, s, scope);
                dbuf_putc(&bc_out, op);
                dbuf_put_u16(&bc_out, call_argc);
                dbuf_put_u16(&bc_out, s->scopes[scope].first + 1);
            }
            break;
        case OP_apply_eval: /* convert scope index to adjusted variable index */
            scope = get_u16(bc_buf + pos + 1);
            mark_eval_captured_variables(ctx, s, scope);
            dbuf_putc(&bc_out, op);
            dbuf_put_u16(&bc_out, s->scopes[scope].first + 1);
            break;
        case OP_scope_get_var_undef:
        case OP_scope_get_var:
        case OP_scope_put_var:
        case OP_scope_delete_var:
        case OP_scope_get_ref:
        case OP_scope_put_var_init:
            var_name = get_u32(bc_buf + pos + 1);
            scope = get_u16(bc_buf + pos + 5);
            pos_next = resolve_scope_var(ctx, s, var_name, scope, op, &bc_out,
                                         NULL, NULL, pos_next);
            JS_FreeAtom(ctx, var_name);
            break;
        case OP_scope_make_ref:
            {
                int label;
                LabelSlot *ls;
                var_name = get_u32(bc_buf + pos + 1);
                label = get_u32(bc_buf + pos + 5);
                scope = get_u16(bc_buf + pos + 9);
                ls = &s->label_slots[label];
                ls->ref_count--;  /* always remove label reference */
                pos_next = resolve_scope_var(ctx, s, var_name, scope, op, &bc_out,
                                             bc_buf, ls, pos_next);
                JS_FreeAtom(ctx, var_name);
            }
            break;
        case OP_scope_get_private_field:
        case OP_scope_get_private_field2:
        case OP_scope_put_private_field:
            {
                int ret;
                var_name = get_u32(bc_buf + pos + 1);
                scope = get_u16(bc_buf + pos + 5);
                ret = resolve_scope_private_field(ctx, s, var_name, scope, op, &bc_out);
                if (ret < 0)
                    goto fail;
                JS_FreeAtom(ctx, var_name);
            }
            break;
        case OP_gosub:
            s->jump_size++;
            if (OPTIMIZE) {
                /* remove calls to empty finalizers  */
                int label;
                LabelSlot *ls;

                label = get_u32(bc_buf + pos + 1);
                assert(label >= 0 && label < s->label_count);
                ls = &s->label_slots[label];
                if (code_match(&cc, ls->pos, OP_ret, -1)) {
                    ls->ref_count--;
                    break;
                }
            }
            goto no_change;
        case OP_drop:
            if (0) {
                /* remove drops before return_undef */
                /* do not perform this optimization in pass2 because
                   it breaks patterns recognised in resolve_labels */
                int pos1 = pos_next;
                int line1 = line_num;
                while (code_match(&cc, pos1, OP_drop, -1)) {
                    if (cc.line_num >= 0) line1 = cc.line_num;
                    pos1 = cc.pos;
                }
                if (code_match(&cc, pos1, OP_return_undef, -1)) {
                    pos_next = pos1;
                    if (line1 != -1 && line1 != line_num) {
                        line_num = line1;
                        s->line_number_size++;
                        dbuf_putc(&bc_out, OP_line_num);
                        dbuf_put_u32(&bc_out, line_num);
                    }
                    break;
                }
            }
            goto no_change;
        case OP_insert3:
            if (OPTIMIZE) {
                /* Transformation: insert3 put_array_el|put_ref_value drop -> put_array_el|put_ref_value */
                if (code_match(&cc, pos_next, M2(OP_put_array_el, OP_put_ref_value), OP_drop, -1)) {
                    dbuf_putc(&bc_out, cc.op);
                    pos_next = cc.pos;
                    if (cc.line_num != -1 && cc.line_num != line_num) {
                        line_num = cc.line_num;
                        s->line_number_size++;
                        dbuf_putc(&bc_out, OP_line_num);
                        dbuf_put_u32(&bc_out, line_num);
                    }
                    break;
                }
            }
            goto no_change;

        case OP_goto:
            s->jump_size++;
            /* fall thru */
        case OP_tail_call:
        case OP_tail_call_method:
        case OP_return:
        case OP_return_undef:
        case OP_throw:
        case OP_throw_error:
        case OP_ret:
            if (OPTIMIZE) {
                /* remove dead code */
                int line = -1;
                dbuf_put(&bc_out, bc_buf + pos, len);
                pos = skip_dead_code(s, bc_buf, bc_len, pos + len, &line);
                pos_next = pos;
                if (pos < bc_len && line >= 0 && line_num != line) {
                    line_num = line;
                    s->line_number_size++;
                    dbuf_putc(&bc_out, OP_line_num);
                    dbuf_put_u32(&bc_out, line_num);
                }
                break;
            }
            goto no_change;

        case OP_label:
            {
                int label;
                LabelSlot *ls;

                label = get_u32(bc_buf + pos + 1);
                assert(label >= 0 && label < s->label_count);
                ls = &s->label_slots[label];
                ls->pos2 = bc_out.size + opcode_info[op].size;
            }
            goto no_change;

        case OP_enter_scope:
            {
                int scope_idx, scope = get_u16(bc_buf + pos + 1);

                if (scope == s->body_scope) {
                    instantiate_hoisted_definitions(ctx, s, &bc_out);
                }

                for(scope_idx = s->scopes[scope].first; scope_idx >= 0;) {
                    JSVarDef *vd = &s->vars[scope_idx];
                    if (vd->scope_level == scope) {
                        if (scope_idx != s->arguments_arg_idx) {
                            if (vd->var_kind == JS_VAR_FUNCTION_DECL ||
                                vd->var_kind == JS_VAR_NEW_FUNCTION_DECL) {
                                /* Initialize lexical variable upon entering scope */
                                dbuf_putc(&bc_out, OP_fclosure);
                                dbuf_put_u32(&bc_out, vd->func_pool_idx);
                                dbuf_putc(&bc_out, OP_put_loc);
                                dbuf_put_u16(&bc_out, scope_idx);
                            } else {
                                /* XXX: should check if variable can be used
                                   before initialization */
                                dbuf_putc(&bc_out, OP_set_loc_uninitialized);
                                dbuf_put_u16(&bc_out, scope_idx);
                            }
                        }
                        scope_idx = vd->scope_next;
                    } else {
                        break;
                    }
                }
            }
            break;

        case OP_leave_scope:
            {
                int scope_idx, scope = get_u16(bc_buf + pos + 1);

                for(scope_idx = s->scopes[scope].first; scope_idx >= 0;) {
                    JSVarDef *vd = &s->vars[scope_idx];
                    if (vd->scope_level == scope) {
                        if (vd->is_captured) {
                            dbuf_putc(&bc_out, OP_close_loc);
                            dbuf_put_u16(&bc_out, scope_idx);
                        }
                        scope_idx = vd->scope_next;
                    } else {
                        break;
                    }
                }
            }
            break;

        case OP_set_name:
            {
                /* remove dummy set_name opcodes */
                JSAtom name = get_u32(bc_buf + pos + 1);
                if (name == JS_ATOM_NULL)
                    break;
            }
            goto no_change;

        case OP_if_false:
        case OP_if_true:
        case OP_catch:
            s->jump_size++;
            goto no_change;

        case OP_dup:
            if (OPTIMIZE) {
                /* Transformation: dup if_false(l1) drop, l1: if_false(l2) -> if_false(l2) */
                /* Transformation: dup if_true(l1) drop, l1: if_true(l2) -> if_true(l2) */
                if (code_match(&cc, pos_next, M2(OP_if_false, OP_if_true), OP_drop, -1)) {
                    int lab0, lab1, op1, pos1, line1, pos2;
                    lab0 = lab1 = cc.label;
                    assert(lab1 >= 0 && lab1 < s->label_count);
                    op1 = cc.op;
                    pos1 = cc.pos;
                    line1 = cc.line_num;
                    while (code_match(&cc, (pos2 = get_label_pos(s, lab1)), OP_dup, op1, OP_drop, -1)) {
                        lab1 = cc.label;
                    }
                    if (code_match(&cc, pos2, op1, -1)) {
                        s->jump_size++;
                        update_label(s, lab0, -1);
                        update_label(s, cc.label, +1);
                        dbuf_putc(&bc_out, op1);
                        dbuf_put_u32(&bc_out, cc.label);
                        pos_next = pos1;
                        if (line1 != -1 && line1 != line_num) {
                            line_num = line1;
                            s->line_number_size++;
                            dbuf_putc(&bc_out, OP_line_num);
                            dbuf_put_u32(&bc_out, line_num);
                        }
                        break;
                    }
                }
            }
            goto no_change;

        case OP_nop:
            /* remove erased code */
            break;
        case OP_set_class_name:
            /* only used during parsing */
            break;
            
        default:
        no_change:
            dbuf_put(&bc_out, bc_buf + pos, len);
            break;
        }
    }

    /* set the new byte code */
    dbuf_free(&s->byte_code);
    s->byte_code = bc_out;
    if (dbuf_error(&s->byte_code)) {
        JS_ThrowOutOfMemory(ctx);
        return -1;
    }
    return 0;
 fail:
    /* continue the copy to keep the atom refcounts consistent */
    /* XXX: find a better solution ? */
    for (; pos < bc_len; pos = pos_next) {
        op = bc_buf[pos];
        len = opcode_info[op].size;
        pos_next = pos + len;
        dbuf_put(&bc_out, bc_buf + pos, len);
    }
    dbuf_free(&s->byte_code);
    s->byte_code = bc_out;
    return -1;
}

/* the pc2line table gives a line number for each PC value */
static void add_pc2line_info(JSFunctionDef *s, uint32_t pc, int line_num)
{
    if (s->line_number_slots != NULL
    &&  s->line_number_count < s->line_number_size
    &&  pc >= s->line_number_last_pc
    &&  line_num != s->line_number_last) {
        s->line_number_slots[s->line_number_count].pc = pc;
        s->line_number_slots[s->line_number_count].line_num = line_num;
        s->line_number_count++;
        s->line_number_last_pc = pc;
        s->line_number_last = line_num;
    }
}

static void compute_pc2line_info(JSFunctionDef *s)
{
    if (!(s->js_mode & JS_MODE_STRIP) && s->line_number_slots) {
        int last_line_num = s->line_num;
        uint32_t last_pc = 0;
        int i;

        js_dbuf_init(s->ctx, &s->pc2line);
        for (i = 0; i < s->line_number_count; i++) {
            uint32_t pc = s->line_number_slots[i].pc;
            int line_num = s->line_number_slots[i].line_num;
            int diff_pc, diff_line;

            if (line_num < 0)
                continue;

            diff_pc = pc - last_pc;
            diff_line = line_num - last_line_num;
            if (diff_line == 0 || diff_pc < 0)
                continue;

            if (diff_line >= PC2LINE_BASE &&
                diff_line < PC2LINE_BASE + PC2LINE_RANGE &&
                diff_pc <= PC2LINE_DIFF_PC_MAX) {
                dbuf_putc(&s->pc2line, (diff_line - PC2LINE_BASE) +
                          diff_pc * PC2LINE_RANGE + PC2LINE_OP_FIRST);
            } else {
                /* longer encoding */
                dbuf_putc(&s->pc2line, 0);
                dbuf_put_leb128(&s->pc2line, diff_pc);
                dbuf_put_sleb128(&s->pc2line, diff_line);
            }
            last_pc = pc;
            last_line_num = line_num;
        }
    }
}

static RelocEntry *add_reloc(JSContext *ctx, LabelSlot *ls, uint32_t addr, int size)
{
    RelocEntry *re;
    re = js_malloc(ctx, sizeof(*re));
    if (!re)
        return NULL;
    re->addr = addr;
    re->size = size;
    re->next = ls->first_reloc;
    ls->first_reloc = re;
    return re;
}

static BOOL code_has_label(CodeContext *s, int pos, int label)
{
    while (pos < s->bc_len) {
        int op = s->bc_buf[pos];
        if (op == OP_line_num) {
            pos += 5;
            continue;
        }
        if (op == OP_label) {
            int lab = get_u32(s->bc_buf + pos + 1);
            if (lab == label)
                return TRUE;
            pos += 5;
            continue;
        }
        if (op == OP_goto) {
            int lab = get_u32(s->bc_buf + pos + 1);
            if (lab == label)
                return TRUE;
        }
        break;
    }
    return FALSE;
}

/* return the target label, following the OP_goto jumps
   the first opcode at destination is stored in *pop
 */
static int find_jump_target(JSFunctionDef *s, int label, int *pop, int *pline)
{
    int i, pos, op;

    update_label(s, label, -1);
    for (i = 0; i < 10; i++) {
        assert(label >= 0 && label < s->label_count);
        pos = s->label_slots[label].pos2;
        for (;;) {
            switch(op = s->byte_code.buf[pos]) {
            case OP_line_num:
                if (pline)
                    *pline = get_u32(s->byte_code.buf + pos + 1);
                /* fall thru */
            case OP_label:
                pos += opcode_info[op].size;
                continue;
            case OP_goto:
                label = get_u32(s->byte_code.buf + pos + 1);
                break;
            case OP_drop:
                /* ignore drop opcodes if followed by OP_return_undef */
                while (s->byte_code.buf[++pos] == OP_drop)
                    continue;
                if (s->byte_code.buf[pos] == OP_return_undef)
                    op = OP_return_undef;
                /* fall thru */
            default:
                goto done;
            }
            break;
        }
    }
    /* cycle detected, could issue a warning */
 done:
    *pop = op;
    update_label(s, label, +1);
    return label;
}

static void push_short_int(DynBuf *bc_out, int val)
{
#if SHORT_OPCODES
    if (val >= -1 && val <= 7) {
        dbuf_putc(bc_out, OP_push_0 + val);
        return;
    }
    if (val == (int8_t)val) {
        dbuf_putc(bc_out, OP_push_i8);
        dbuf_putc(bc_out, val);
        return;
    }
    if (val == (int16_t)val) {
        dbuf_putc(bc_out, OP_push_i16);
        dbuf_put_u16(bc_out, val);
        return;
    }
#endif
    dbuf_putc(bc_out, OP_push_i32);
    dbuf_put_u32(bc_out, val);
}

static void put_short_code(DynBuf *bc_out, int op, int idx)
{
#if SHORT_OPCODES
    if (idx < 4) {
        switch (op) {
        case OP_get_loc:
            dbuf_putc(bc_out, OP_get_loc0 + idx);
            return;
        case OP_put_loc:
            dbuf_putc(bc_out, OP_put_loc0 + idx);
            return;
        case OP_set_loc:
            dbuf_putc(bc_out, OP_set_loc0 + idx);
            return;
        case OP_get_arg:
            dbuf_putc(bc_out, OP_get_arg0 + idx);
            return;
        case OP_put_arg:
            dbuf_putc(bc_out, OP_put_arg0 + idx);
            return;
        case OP_set_arg:
            dbuf_putc(bc_out, OP_set_arg0 + idx);
            return;
        case OP_get_var_ref:
            dbuf_putc(bc_out, OP_get_var_ref0 + idx);
            return;
        case OP_put_var_ref:
            dbuf_putc(bc_out, OP_put_var_ref0 + idx);
            return;
        case OP_set_var_ref:
            dbuf_putc(bc_out, OP_set_var_ref0 + idx);
            return;
        case OP_call:
            dbuf_putc(bc_out, OP_call0 + idx);
            return;
        }
    }
    if (idx < 256) {
        switch (op) {
        case OP_get_loc:
            dbuf_putc(bc_out, OP_get_loc8);
            dbuf_putc(bc_out, idx);
            return;
        case OP_put_loc:
            dbuf_putc(bc_out, OP_put_loc8);
            dbuf_putc(bc_out, idx);
            return;
        case OP_set_loc:
            dbuf_putc(bc_out, OP_set_loc8);
            dbuf_putc(bc_out, idx);
            return;
        }
    }
#endif
    dbuf_putc(bc_out, op);
    dbuf_put_u16(bc_out, idx);
}

/* peephole optimizations and resolve goto/labels */
static __exception int resolve_labels(JSContext *ctx, JSFunctionDef *s)
{
    int pos, pos_next, bc_len, op, op1, len, i, line_num;
    const uint8_t *bc_buf;
    DynBuf bc_out;
    LabelSlot *label_slots, *ls;
    RelocEntry *re, *re_next;
    CodeContext cc;
    int label;
#if SHORT_OPCODES
    JumpSlot *jp;
#endif

    label_slots = s->label_slots;

    line_num = s->line_num;

    cc.bc_buf = bc_buf = s->byte_code.buf;
    cc.bc_len = bc_len = s->byte_code.size;
    js_dbuf_init(ctx, &bc_out);

#if SHORT_OPCODES
    if (s->jump_size) {
        s->jump_slots = js_mallocz(s->ctx, sizeof(*s->jump_slots) * s->jump_size);
        if (s->jump_slots == NULL)
            return -1;
    }
#endif
    /* XXX: Should skip this phase if not generating SHORT_OPCODES */
    if (s->line_number_size && !(s->js_mode & JS_MODE_STRIP)) {
        s->line_number_slots = js_mallocz(s->ctx, sizeof(*s->line_number_slots) * s->line_number_size);
        if (s->line_number_slots == NULL)
            return -1;
        s->line_number_last = s->line_num;
        s->line_number_last_pc = 0;
    }

    /* initialize the 'home_object' variable if needed */
    if (s->home_object_var_idx >= 0) {
        dbuf_putc(&bc_out, OP_special_object);
        dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_HOME_OBJECT);
        put_short_code(&bc_out, OP_put_loc, s->home_object_var_idx);
    }
    /* initialize the 'this.active_func' variable if needed */
    if (s->this_active_func_var_idx >= 0) {
        dbuf_putc(&bc_out, OP_special_object);
        dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_THIS_FUNC);
        put_short_code(&bc_out, OP_put_loc, s->this_active_func_var_idx);
    }
    /* initialize the 'new.target' variable if needed */
    if (s->new_target_var_idx >= 0) {
        dbuf_putc(&bc_out, OP_special_object);
        dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_NEW_TARGET);
        put_short_code(&bc_out, OP_put_loc, s->new_target_var_idx);
    }
    /* initialize the 'this' variable if needed. In a derived class
       constructor, this is initially uninitialized. */
    if (s->this_var_idx >= 0) {
        if (s->is_derived_class_constructor) {
            dbuf_putc(&bc_out, OP_set_loc_uninitialized);
            dbuf_put_u16(&bc_out, s->this_var_idx);
        } else {
            dbuf_putc(&bc_out, OP_push_this);
            put_short_code(&bc_out, OP_put_loc, s->this_var_idx);
        }
    }
    /* initialize the 'arguments' variable if needed */
    if (s->arguments_var_idx >= 0) {
        if ((s->js_mode & JS_MODE_STRICT) || !s->has_simple_parameter_list) {
            dbuf_putc(&bc_out, OP_special_object);
            dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_ARGUMENTS);
        } else {
            dbuf_putc(&bc_out, OP_special_object);
            dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_MAPPED_ARGUMENTS);
        }
        if (s->arguments_arg_idx >= 0)
            put_short_code(&bc_out, OP_set_loc, s->arguments_arg_idx);
        put_short_code(&bc_out, OP_put_loc, s->arguments_var_idx);
    }
    /* initialize a reference to the current function if needed */
    if (s->func_var_idx >= 0) {
        dbuf_putc(&bc_out, OP_special_object);
        dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_THIS_FUNC);
        put_short_code(&bc_out, OP_put_loc, s->func_var_idx);
    }
    /* initialize the variable environment object if needed */
    if (s->var_object_idx >= 0) {
        dbuf_putc(&bc_out, OP_special_object);
        dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_VAR_OBJECT);
        put_short_code(&bc_out, OP_put_loc, s->var_object_idx);
    }
    if (s->arg_var_object_idx >= 0) {
        dbuf_putc(&bc_out, OP_special_object);
        dbuf_putc(&bc_out, OP_SPECIAL_OBJECT_VAR_OBJECT);
        put_short_code(&bc_out, OP_put_loc, s->arg_var_object_idx);
    }

    for (pos = 0; pos < bc_len; pos = pos_next) {
        int val;
        op = bc_buf[pos];
        len = opcode_info[op].size;
        pos_next = pos + len;
        switch(op) {
        case OP_line_num:
            /* line number info (for debug). We put it in a separate
               compressed table to reduce memory usage and get better
               performance */
            line_num = get_u32(bc_buf + pos + 1);
            break;

        case OP_label:
            {
                label = get_u32(bc_buf + pos + 1);
                assert(label >= 0 && label < s->label_count);
                ls = &label_slots[label];
                assert(ls->addr == -1);
                ls->addr = bc_out.size;
                /* resolve the relocation entries */
                for(re = ls->first_reloc; re != NULL; re = re_next) {
                    int diff = ls->addr - re->addr;
                    re_next = re->next;
                    switch (re->size) {
                    case 4:
                        put_u32(bc_out.buf + re->addr, diff);
                        break;
                    case 2:
                        assert(diff == (int16_t)diff);
                        put_u16(bc_out.buf + re->addr, diff);
                        break;
                    case 1:
                        assert(diff == (int8_t)diff);
                        put_u8(bc_out.buf + re->addr, diff);
                        break;
                    }
                    js_free(ctx, re);
                }
                ls->first_reloc = NULL;
            }
            break;

        case OP_call:
        case OP_call_method:
            {
                /* detect and transform tail calls */
                int argc;
                argc = get_u16(bc_buf + pos + 1);
                if (code_match(&cc, pos_next, OP_return, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    add_pc2line_info(s, bc_out.size, line_num);
                    put_short_code(&bc_out, op + 1, argc);
                    pos_next = skip_dead_code(s, bc_buf, bc_len, cc.pos, &line_num);
                    break;
                }
                add_pc2line_info(s, bc_out.size, line_num);
                put_short_code(&bc_out, op, argc);
                break;
            }
            goto no_change;

        case OP_return:
        case OP_return_undef:
        case OP_return_async:
        case OP_throw:
        case OP_throw_error:
            pos_next = skip_dead_code(s, bc_buf, bc_len, pos_next, &line_num);
            goto no_change;

        case OP_goto:
            label = get_u32(bc_buf + pos + 1);
        has_goto:
            if (OPTIMIZE) {
                int line1 = -1;
                /* Use custom matcher because multiple labels can follow */
                label = find_jump_target(s, label, &op1, &line1);
                if (code_has_label(&cc, pos_next, label)) {
                    /* jump to next instruction: remove jump */
                    update_label(s, label, -1);
                    break;
                }
                if (op1 == OP_return || op1 == OP_return_undef || op1 == OP_throw) {
                    /* jump to return/throw: remove jump, append return/throw */
                    /* updating the line number obfuscates assembly listing */
                    //if (line1 >= 0) line_num = line1;
                    update_label(s, label, -1);
                    add_pc2line_info(s, bc_out.size, line_num);
                    dbuf_putc(&bc_out, op1);
                    pos_next = skip_dead_code(s, bc_buf, bc_len, pos_next, &line_num);
                    break;
                }
                /* XXX: should duplicate single instructions followed by goto or return */
                /* For example, can match one of these followed by return:
                   push_i32 / push_const / push_atom_value / get_var /
                   undefined / null / push_false / push_true / get_ref_value /
                   get_loc / get_arg / get_var_ref
                 */
            }
            goto has_label;

        case OP_gosub:
            label = get_u32(bc_buf + pos + 1);
            if (0 && OPTIMIZE) {
                label = find_jump_target(s, label, &op1, NULL);
                if (op1 == OP_ret) {
                    update_label(s, label, -1);
                    /* empty finally clause: remove gosub */
                    break;
                }
            }
            goto has_label;

        case OP_catch:
            label = get_u32(bc_buf + pos + 1);
            goto has_label;

        case OP_if_true:
        case OP_if_false:
            label = get_u32(bc_buf + pos + 1);
            if (OPTIMIZE) {
                label = find_jump_target(s, label, &op1, NULL);
                /* transform if_false/if_true(l1) label(l1) -> drop label(l1) */
                if (code_has_label(&cc, pos_next, label)) {
                    update_label(s, label, -1);
                    dbuf_putc(&bc_out, OP_drop);
                    break;
                }
                /* transform if_false(l1) goto(l2) label(l1) -> if_false(l2) label(l1) */
                if (code_match(&cc, pos_next, OP_goto, -1)) {
                    int pos1 = cc.pos;
                    int line1 = cc.line_num;
                    if (code_has_label(&cc, pos1, label)) {
                        if (line1 >= 0) line_num = line1;
                        pos_next = pos1;
                        update_label(s, label, -1);
                        label = cc.label;
                        op ^= OP_if_true ^ OP_if_false;
                    }
                }
            }
        has_label:
            add_pc2line_info(s, bc_out.size, line_num);
            if (op == OP_goto) {
                pos_next = skip_dead_code(s, bc_buf, bc_len, pos_next, &line_num);
            }
            assert(label >= 0 && label < s->label_count);
            ls = &label_slots[label];
#if SHORT_OPCODES
            jp = &s->jump_slots[s->jump_count++];
            jp->op = op;
            jp->size = 4;
            jp->pos = bc_out.size + 1;
            jp->label = label;

            if (ls->addr == -1) {
                int diff = ls->pos2 - pos - 1;
                if (diff < 128 && (op == OP_if_false || op == OP_if_true || op == OP_goto)) {
                    jp->size = 1;
                    jp->op = OP_if_false8 + (op - OP_if_false);
                    dbuf_putc(&bc_out, OP_if_false8 + (op - OP_if_false));
                    dbuf_putc(&bc_out, 0);
                    if (!add_reloc(ctx, ls, bc_out.size - 1, 1))
                        goto fail;
                    break;
                }
                if (diff < 32768 && op == OP_goto) {
                    jp->size = 2;
                    jp->op = OP_goto16;
                    dbuf_putc(&bc_out, OP_goto16);
                    dbuf_put_u16(&bc_out, 0);
                    if (!add_reloc(ctx, ls, bc_out.size - 2, 2))
                        goto fail;
                    break;
                }
            } else {
                int diff = ls->addr - bc_out.size - 1;
                if (diff == (int8_t)diff && (op == OP_if_false || op == OP_if_true || op == OP_goto)) {
                    jp->size = 1;
                    jp->op = OP_if_false8 + (op - OP_if_false);
                    dbuf_putc(&bc_out, OP_if_false8 + (op - OP_if_false));
                    dbuf_putc(&bc_out, diff);
                    break;
                }
                if (diff == (int16_t)diff && op == OP_goto) {
                    jp->size = 2;
                    jp->op = OP_goto16;
                    dbuf_putc(&bc_out, OP_goto16);
                    dbuf_put_u16(&bc_out, diff);
                    break;
                }
            }
#endif
            dbuf_putc(&bc_out, op);
            dbuf_put_u32(&bc_out, ls->addr - bc_out.size);
            if (ls->addr == -1) {
                /* unresolved yet: create a new relocation entry */
                if (!add_reloc(ctx, ls, bc_out.size - 4, 4))
                    goto fail;
            }
            break;
        case OP_with_get_var:
        case OP_with_put_var:
        case OP_with_delete_var:
        case OP_with_make_ref:
        case OP_with_get_ref:
        case OP_with_get_ref_undef:
            {
                JSAtom atom;
                int is_with;

                atom = get_u32(bc_buf + pos + 1);
                label = get_u32(bc_buf + pos + 5);
                is_with = bc_buf[pos + 9];
                if (OPTIMIZE) {
                    label = find_jump_target(s, label, &op1, NULL);
                }
                assert(label >= 0 && label < s->label_count);
                ls = &label_slots[label];
                add_pc2line_info(s, bc_out.size, line_num);
#if SHORT_OPCODES
                jp = &s->jump_slots[s->jump_count++];
                jp->op = op;
                jp->size = 4;
                jp->pos = bc_out.size + 5;
                jp->label = label;
#endif
                dbuf_putc(&bc_out, op);
                dbuf_put_u32(&bc_out, atom);
                dbuf_put_u32(&bc_out, ls->addr - bc_out.size);
                if (ls->addr == -1) {
                    /* unresolved yet: create a new relocation entry */
                    if (!add_reloc(ctx, ls, bc_out.size - 4, 4))
                        goto fail;
                }
                dbuf_putc(&bc_out, is_with);
            }
            break;

        case OP_drop:
            if (OPTIMIZE) {
                /* remove useless drops before return */
                if (code_match(&cc, pos_next, OP_return_undef, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    break;
                }
            }
            goto no_change;

        case OP_null:
#if SHORT_OPCODES
            if (OPTIMIZE) {
                /* transform null strict_eq into is_null */
                if (code_match(&cc, pos_next, OP_strict_eq, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    add_pc2line_info(s, bc_out.size, line_num);
                    dbuf_putc(&bc_out, OP_is_null);
                    pos_next = cc.pos;
                    break;
                }
                /* transform null strict_neq if_false/if_true -> is_null if_true/if_false */
                if (code_match(&cc, pos_next, OP_strict_neq, M2(OP_if_false, OP_if_true), -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    add_pc2line_info(s, bc_out.size, line_num);
                    dbuf_putc(&bc_out, OP_is_null);
                    pos_next = cc.pos;
                    label = cc.label;
                    op = cc.op ^ OP_if_false ^ OP_if_true;
                    goto has_label;
                }
            }
#endif
            /* fall thru */
        case OP_push_false:
        case OP_push_true:
            if (OPTIMIZE) {
                val = (op == OP_push_true);
                if (code_match(&cc, pos_next, M2(OP_if_false, OP_if_true), -1)) {
                has_constant_test:
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    if (val == cc.op - OP_if_false) {
                        /* transform null if_false(l1) -> goto l1 */
                        /* transform false if_false(l1) -> goto l1 */
                        /* transform true if_true(l1) -> goto l1 */
                        pos_next = cc.pos;
                        op = OP_goto;
                        label = cc.label;
                        goto has_goto;
                    } else {
                        /* transform null if_true(l1) -> nop */
                        /* transform false if_true(l1) -> nop */
                        /* transform true if_false(l1) -> nop */
                        pos_next = cc.pos;
                        update_label(s, cc.label, -1);
                        break;
                    }
                }
            }
            goto no_change;

        case OP_push_i32:
            if (OPTIMIZE) {
                /* transform i32(val) neg -> i32(-val) */
                val = get_i32(bc_buf + pos + 1);
                if ((val != INT32_MIN && val != 0)
                &&  code_match(&cc, pos_next, OP_neg, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    if (code_match(&cc, cc.pos, OP_drop, -1)) {
                        if (cc.line_num >= 0) line_num = cc.line_num;
                    } else {
                        add_pc2line_info(s, bc_out.size, line_num);
                        push_short_int(&bc_out, -val);
                    }
                    pos_next = cc.pos;
                    break;
                }
                /* remove push/drop pairs generated by the parser */
                if (code_match(&cc, pos_next, OP_drop, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    pos_next = cc.pos;
                    break;
                }
                /* Optimize constant tests: `if (0)`, `if (1)`, `if (!0)`... */
                if (code_match(&cc, pos_next, M2(OP_if_false, OP_if_true), -1)) {
                    val = (val != 0);
                    goto has_constant_test;
                }
                add_pc2line_info(s, bc_out.size, line_num);
                push_short_int(&bc_out, val);
                break;
            }
            goto no_change;

#if SHORT_OPCODES
        case OP_push_const:
        case OP_fclosure:
            if (OPTIMIZE) {
                int idx = get_u32(bc_buf + pos + 1);
                if (idx < 256) {
                    add_pc2line_info(s, bc_out.size, line_num);
                    dbuf_putc(&bc_out, OP_push_const8 + op - OP_push_const);
                    dbuf_putc(&bc_out, idx);
                    break;
                }
            }
            goto no_change;

        case OP_get_field:
            if (OPTIMIZE) {
                JSAtom atom = get_u32(bc_buf + pos + 1);
                if (atom == JS_ATOM_length) {
                    JS_FreeAtom(ctx, atom);
                    add_pc2line_info(s, bc_out.size, line_num);
                    dbuf_putc(&bc_out, OP_get_length);
                    break;
                }
            }
            goto no_change;
#endif
        case OP_push_atom_value:
            if (OPTIMIZE) {
                JSAtom atom = get_u32(bc_buf + pos + 1);
                /* remove push/drop pairs generated by the parser */
                if (code_match(&cc, pos_next, OP_drop, -1)) {
                    JS_FreeAtom(ctx, atom);
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    pos_next = cc.pos;
                    break;
                }
#if SHORT_OPCODES
                if (atom == JS_ATOM_empty_string) {
                    JS_FreeAtom(ctx, atom);
                    add_pc2line_info(s, bc_out.size, line_num);
                    dbuf_putc(&bc_out, OP_push_empty_string);
                    break;
                }
#endif
            }
            goto no_change;

        case OP_to_propkey:
        case OP_to_propkey2:
            if (OPTIMIZE) {
                /* remove redundant to_propkey/to_propkey2 opcodes when storing simple data */
                if (code_match(&cc, pos_next, M3(OP_get_loc, OP_get_arg, OP_get_var_ref), -1, OP_put_array_el, -1)
                ||  code_match(&cc, pos_next, M3(OP_push_i32, OP_push_const, OP_push_atom_value), OP_put_array_el, -1)
                ||  code_match(&cc, pos_next, M4(OP_undefined, OP_null, OP_push_true, OP_push_false), OP_put_array_el, -1)) {
                    break;
                }
            }
            goto no_change;

        case OP_undefined:
            if (OPTIMIZE) {
                /* remove push/drop pairs generated by the parser */
                if (code_match(&cc, pos_next, OP_drop, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    pos_next = cc.pos;
                    break;
                }
                /* transform undefined return -> return_undefined */
                if (code_match(&cc, pos_next, OP_return, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    add_pc2line_info(s, bc_out.size, line_num);
                    dbuf_putc(&bc_out, OP_return_undef);
                    pos_next = cc.pos;
                    break;
                }
                /* transform undefined if_true(l1)/if_false(l1) -> nop/goto(l1) */
                if (code_match(&cc, pos_next, M2(OP_if_false, OP_if_true), -1)) {
                    val = 0;
                    goto has_constant_test;
                }
#if SHORT_OPCODES
                /* transform undefined strict_eq -> is_undefined */
                if (code_match(&cc, pos_next, OP_strict_eq, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    add_pc2line_info(s, bc_out.size, line_num);
                    dbuf_putc(&bc_out, OP_is_undefined);
                    pos_next = cc.pos;
                    break;
                }
                /* transform undefined strict_neq if_false/if_true -> is_undefined if_true/if_false */
                if (code_match(&cc, pos_next, OP_strict_neq, M2(OP_if_false, OP_if_true), -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    add_pc2line_info(s, bc_out.size, line_num);
                    dbuf_putc(&bc_out, OP_is_undefined);
                    pos_next = cc.pos;
                    label = cc.label;
                    op = cc.op ^ OP_if_false ^ OP_if_true;
                    goto has_label;
                }
#endif
            }
            goto no_change;

        case OP_insert2:
            if (OPTIMIZE) {
                /* Transformation:
                   insert2 put_field(a) drop -> put_field(a)
                   insert2 put_var_strict(a) drop -> put_var_strict(a)
                */
                if (code_match(&cc, pos_next, M2(OP_put_field, OP_put_var_strict), OP_drop, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    add_pc2line_info(s, bc_out.size, line_num);
                    dbuf_putc(&bc_out, cc.op);
                    dbuf_put_u32(&bc_out, cc.atom);
                    pos_next = cc.pos;
                    break;
                }
            }
            goto no_change;

        case OP_dup:
            if (OPTIMIZE) {
                /* Transformation: dup put_x(n) drop -> put_x(n) */
                int op1, line2 = -1;
                /* Transformation: dup put_x(n) -> set_x(n) */
                if (code_match(&cc, pos_next, M3(OP_put_loc, OP_put_arg, OP_put_var_ref), -1, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    op1 = cc.op + 1;  /* put_x -> set_x */
                    pos_next = cc.pos;
                    if (code_match(&cc, cc.pos, OP_drop, -1)) {
                        if (cc.line_num >= 0) line_num = cc.line_num;
                        op1 -= 1; /* set_x drop -> put_x */
                        pos_next = cc.pos;
                        if (code_match(&cc, cc.pos, op1 - 1, cc.idx, -1)) {
                            line2 = cc.line_num; /* delay line number update */
                            op1 += 1;   /* put_x(n) get_x(n) -> set_x(n) */
                            pos_next = cc.pos;
                        }
                    }
                    add_pc2line_info(s, bc_out.size, line_num);
                    put_short_code(&bc_out, op1, cc.idx);
                    if (line2 >= 0) line_num = line2;
                    break;
                }
            }
            goto no_change;

        case OP_get_loc:
            if (OPTIMIZE) {
                /* transformation:
                   get_loc(n) post_dec put_loc(n) drop -> dec_loc(n)
                   get_loc(n) post_inc put_loc(n) drop -> inc_loc(n)
                   get_loc(n) dec dup put_loc(n) drop -> dec_loc(n)
                   get_loc(n) inc dup put_loc(n) drop -> inc_loc(n)
                 */
                int idx;
                idx = get_u16(bc_buf + pos + 1);
                if (idx >= 256)
                    goto no_change;
                if (code_match(&cc, pos_next, M2(OP_post_dec, OP_post_inc), OP_put_loc, idx, OP_drop, -1) ||
                    code_match(&cc, pos_next, M2(OP_dec, OP_inc), OP_dup, OP_put_loc, idx, OP_drop, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    add_pc2line_info(s, bc_out.size, line_num);
                    dbuf_putc(&bc_out, (cc.op == OP_inc || cc.op == OP_post_inc) ? OP_inc_loc : OP_dec_loc);
                    dbuf_putc(&bc_out, idx);
                    pos_next = cc.pos;
                    break;
                }
                /* transformation:
                   get_loc(n) push_atom_value(x) add dup put_loc(n) drop -> push_atom_value(x) add_loc(n)
                 */
                if (code_match(&cc, pos_next, OP_push_atom_value, OP_add, OP_dup, OP_put_loc, idx, OP_drop, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    add_pc2line_info(s, bc_out.size, line_num);
#if SHORT_OPCODES
                    if (cc.atom == JS_ATOM_empty_string) {
                        JS_FreeAtom(ctx, cc.atom);
                        dbuf_putc(&bc_out, OP_push_empty_string);
                    } else
#endif
                    {
                        dbuf_putc(&bc_out, OP_push_atom_value);
                        dbuf_put_u32(&bc_out, cc.atom);
                    }
                    dbuf_putc(&bc_out, OP_add_loc);
                    dbuf_putc(&bc_out, idx);
                    pos_next = cc.pos;
                    break;
                }
                /* transformation:
                   get_loc(n) push_i32(x) add dup put_loc(n) drop -> push_i32(x) add_loc(n)
                 */
                if (code_match(&cc, pos_next, OP_push_i32, OP_add, OP_dup, OP_put_loc, idx, OP_drop, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    add_pc2line_info(s, bc_out.size, line_num);
                    push_short_int(&bc_out, cc.label);
                    dbuf_putc(&bc_out, OP_add_loc);
                    dbuf_putc(&bc_out, idx);
                    pos_next = cc.pos;
                    break;
                }
                /* transformation: XXX: also do these:
                   get_loc(n) get_loc(x) add dup put_loc(n) drop -> get_loc(x) add_loc(n)
                   get_loc(n) get_arg(x) add dup put_loc(n) drop -> get_arg(x) add_loc(n)
                   get_loc(n) get_var_ref(x) add dup put_loc(n) drop -> get_var_ref(x) add_loc(n)
                 */
                if (code_match(&cc, pos_next, M3(OP_get_loc, OP_get_arg, OP_get_var_ref), -1, OP_add, OP_dup, OP_put_loc, idx, OP_drop, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    add_pc2line_info(s, bc_out.size, line_num);
                    put_short_code(&bc_out, cc.op, cc.idx);
                    dbuf_putc(&bc_out, OP_add_loc);
                    dbuf_putc(&bc_out, idx);
                    pos_next = cc.pos;
                    break;
                }
                add_pc2line_info(s, bc_out.size, line_num);
                put_short_code(&bc_out, op, idx);
                break;
            }
            goto no_change;
#if SHORT_OPCODES
        case OP_get_arg:
        case OP_get_var_ref:
            if (OPTIMIZE) {
                int idx;
                idx = get_u16(bc_buf + pos + 1);
                add_pc2line_info(s, bc_out.size, line_num);
                put_short_code(&bc_out, op, idx);
                break;
            }
            goto no_change;
#endif
        case OP_put_loc:
        case OP_put_arg:
        case OP_put_var_ref:
            if (OPTIMIZE) {
                /* transformation: put_x(n) get_x(n) -> set_x(n) */
                int idx;
                idx = get_u16(bc_buf + pos + 1);
                if (code_match(&cc, pos_next, op - 1, idx, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    add_pc2line_info(s, bc_out.size, line_num);
                    put_short_code(&bc_out, op + 1, idx);
                    pos_next = cc.pos;
                    break;
                }
                add_pc2line_info(s, bc_out.size, line_num);
                put_short_code(&bc_out, op, idx);
                break;
            }
            goto no_change;

        case OP_post_inc:
        case OP_post_dec:
            if (OPTIMIZE) {
                /* transformation:
                   post_inc put_x drop -> inc put_x
                   post_inc perm3 put_field drop -> inc put_field
                   post_inc perm3 put_var_strict drop -> inc put_var_strict
                   post_inc perm4 put_array_el drop -> inc put_array_el
                 */
                int op1, idx;
                if (code_match(&cc, pos_next, M3(OP_put_loc, OP_put_arg, OP_put_var_ref), -1, OP_drop, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    op1 = cc.op;
                    idx = cc.idx;
                    pos_next = cc.pos;
                    if (code_match(&cc, cc.pos, op1 - 1, idx, -1)) {
                        if (cc.line_num >= 0) line_num = cc.line_num;
                        op1 += 1;   /* put_x(n) get_x(n) -> set_x(n) */
                        pos_next = cc.pos;
                    }
                    add_pc2line_info(s, bc_out.size, line_num);
                    dbuf_putc(&bc_out, OP_dec + (op - OP_post_dec));
                    put_short_code(&bc_out, op1, idx);
                    break;
                }
                if (code_match(&cc, pos_next, OP_perm3, M2(OP_put_field, OP_put_var_strict), OP_drop, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    add_pc2line_info(s, bc_out.size, line_num);
                    dbuf_putc(&bc_out, OP_dec + (op - OP_post_dec));
                    dbuf_putc(&bc_out, cc.op);
                    dbuf_put_u32(&bc_out, cc.atom);
                    pos_next = cc.pos;
                    break;
                }
                if (code_match(&cc, pos_next, OP_perm4, OP_put_array_el, OP_drop, -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    add_pc2line_info(s, bc_out.size, line_num);
                    dbuf_putc(&bc_out, OP_dec + (op - OP_post_dec));
                    dbuf_putc(&bc_out, OP_put_array_el);
                    pos_next = cc.pos;
                    break;
                }
            }
            goto no_change;

#if SHORT_OPCODES
        case OP_typeof:
            if (OPTIMIZE) {
                /* simplify typeof tests */
                if (code_match(&cc, pos_next, OP_push_atom_value, M4(OP_strict_eq, OP_strict_neq, OP_eq, OP_neq), -1)) {
                    if (cc.line_num >= 0) line_num = cc.line_num;
                    int op1 = (cc.op == OP_strict_eq || cc.op == OP_eq) ? OP_strict_eq : OP_strict_neq;
                    int op2 = -1;
                    switch (cc.atom) {
                    case JS_ATOM_undefined:
                        op2 = OP_typeof_is_undefined;
                        break;
                    case JS_ATOM_function:
                        op2 = OP_typeof_is_function;
                        break;
                    }
                    if (op2 >= 0) {
                        /* transform typeof(s) == "<type>" into is_<type> */
                        if (op1 == OP_strict_eq) {
                            add_pc2line_info(s, bc_out.size, line_num);
                            dbuf_putc(&bc_out, op2);
                            JS_FreeAtom(ctx, cc.atom);
                            pos_next = cc.pos;
                            break;
                        }
                        if (op1 == OP_strict_neq && code_match(&cc, cc.pos, OP_if_false, -1)) {
                            /* transform typeof(s) != "<type>" if_false into is_<type> if_true */
                            if (cc.line_num >= 0) line_num = cc.line_num;
                            add_pc2line_info(s, bc_out.size, line_num);
                            dbuf_putc(&bc_out, op2);
                            JS_FreeAtom(ctx, cc.atom);
                            pos_next = cc.pos;
                            label = cc.label;
                            op = OP_if_true;
                            goto has_label;
                        }
                    }
                }
            }
            goto no_change;
#endif

        default:
        no_change:
            add_pc2line_info(s, bc_out.size, line_num);
            dbuf_put(&bc_out, bc_buf + pos, len);
            break;
        }
    }

    /* check that there were no missing labels */
    for(i = 0; i < s->label_count; i++) {
        assert(label_slots[i].first_reloc == NULL);
    }
#if SHORT_OPCODES
    if (OPTIMIZE) {
        /* more jump optimizations */
        int patch_offsets = 0;
        for (i = 0, jp = s->jump_slots; i < s->jump_count; i++, jp++) {
            LabelSlot *ls;
            JumpSlot *jp1;
            int j, pos, diff, delta;

            delta = 3;
            switch (op = jp->op) {
            case OP_goto16:
                delta = 1;
                /* fall thru */
            case OP_if_false:
            case OP_if_true:
            case OP_goto:
                pos = jp->pos;
                diff = s->label_slots[jp->label].addr - pos;
                if (diff >= -128 && diff <= 127 + delta) {
                    //put_u8(bc_out.buf + pos, diff);
                    jp->size = 1;
                    if (op == OP_goto16) {
                        bc_out.buf[pos - 1] = jp->op = OP_goto8;
                    } else {
                        bc_out.buf[pos - 1] = jp->op = OP_if_false8 + (op - OP_if_false);
                    }
                    goto shrink;
                } else
                if (diff == (int16_t)diff && op == OP_goto) {
                    //put_u16(bc_out.buf + pos, diff);
                    jp->size = 2;
                    delta = 2;
                    bc_out.buf[pos - 1] = jp->op = OP_goto16;
                shrink:
                    /* XXX: should reduce complexity, using 2 finger copy scheme */
                    memmove(bc_out.buf + pos + jp->size, bc_out.buf + pos + jp->size + delta,
                            bc_out.size - pos - jp->size - delta);
                    bc_out.size -= delta;
                    patch_offsets++;
                    for (j = 0, ls = s->label_slots; j < s->label_count; j++, ls++) {
                        if (ls->addr > pos)
                            ls->addr -= delta;
                    }
                    for (j = i + 1, jp1 = jp + 1; j < s->jump_count; j++, jp1++) {
                        if (jp1->pos > pos)
                            jp1->pos -= delta;
                    }
                    for (j = 0; j < s->line_number_count; j++) {
                        if (s->line_number_slots[j].pc > pos)
                            s->line_number_slots[j].pc -= delta;
                    }
                    continue;
                }
                break;
            }
        }
        if (patch_offsets) {
            JumpSlot *jp1;
            int j;
            for (j = 0, jp1 = s->jump_slots; j < s->jump_count; j++, jp1++) {
                int diff1 = s->label_slots[jp1->label].addr - jp1->pos;
                switch (jp1->size) {
                case 1:
                    put_u8(bc_out.buf + jp1->pos, diff1);
                    break;
                case 2:
                    put_u16(bc_out.buf + jp1->pos, diff1);
                    break;
                case 4:
                    put_u32(bc_out.buf + jp1->pos, diff1);
                    break;
                }
            }
        }
    }
    js_free(ctx, s->jump_slots);
    s->jump_slots = NULL;
#endif
    js_free(ctx, s->label_slots);
    s->label_slots = NULL;
    /* XXX: should delay until copying to runtime bytecode function */
    compute_pc2line_info(s);
    js_free(ctx, s->line_number_slots);
    s->line_number_slots = NULL;
    /* set the new byte code */
    dbuf_free(&s->byte_code);
    s->byte_code = bc_out;
    s->use_short_opcodes = TRUE;
    if (dbuf_error(&s->byte_code)) {
        JS_ThrowOutOfMemory(ctx);
        return -1;
    }
    return 0;
 fail:
    /* XXX: not safe */
    dbuf_free(&bc_out);
    return -1;
}

/* compute the maximum stack size needed by the function */

typedef struct StackSizeState {
    int bc_len;
    int stack_len_max;
    uint16_t *stack_level_tab;
    int *pc_stack;
    int pc_stack_len;
    int pc_stack_size;
} StackSizeState;

/* 'op' is only used for error indication */
static __exception int ss_check(JSContext *ctx, StackSizeState *s,
                                int pos, int op, int stack_len)
{
    if ((unsigned)pos >= s->bc_len) {
        JS_ThrowInternalError(ctx, "bytecode buffer overflow (op=%d, pc=%d)", op, pos);
        return -1;
    }
    if (stack_len > s->stack_len_max) {
        s->stack_len_max = stack_len;
        if (s->stack_len_max > JS_STACK_SIZE_MAX) {
            JS_ThrowInternalError(ctx, "stack overflow (op=%d, pc=%d)", op, pos);
            return -1;
        }
    }
    if (s->stack_level_tab[pos] != 0xffff) {
        /* already explored: check that the stack size is consistent */
        if (s->stack_level_tab[pos] != stack_len) {
            JS_ThrowInternalError(ctx, "unconsistent stack size: %d %d (pc=%d)",
                                  s->stack_level_tab[pos], stack_len, pos);
            return -1;
        } else {
            return 0;
        }
    }

    /* mark as explored and store the stack size */
    s->stack_level_tab[pos] = stack_len;

    /* queue the new PC to explore */
    if (js_resize_array(ctx, (void **)&s->pc_stack, sizeof(s->pc_stack[0]),
                        &s->pc_stack_size, s->pc_stack_len + 1))
        return -1;
    s->pc_stack[s->pc_stack_len++] = pos;
    return 0;
}

static __exception int compute_stack_size(JSContext *ctx,
                                          JSFunctionDef *fd,
                                          int *pstack_size)
{
    StackSizeState s_s, *s = &s_s;
    int i, diff, n_pop, pos_next, stack_len, pos, op;
    const JSOpCode *oi;
    const uint8_t *bc_buf;

    bc_buf = fd->byte_code.buf;
    s->bc_len = fd->byte_code.size;
    /* bc_len > 0 */
    s->stack_level_tab = js_malloc(ctx, sizeof(s->stack_level_tab[0]) *
                                   s->bc_len);
    if (!s->stack_level_tab)
        return -1;
    for(i = 0; i < s->bc_len; i++)
        s->stack_level_tab[i] = 0xffff;
    s->stack_len_max = 0;
    s->pc_stack = NULL;
    s->pc_stack_len = 0;
    s->pc_stack_size = 0;

    /* breadth-first graph exploration */
    if (ss_check(ctx, s, 0, OP_invalid, 0))
        goto fail;

    while (s->pc_stack_len > 0) {
        pos = s->pc_stack[--s->pc_stack_len];
        stack_len = s->stack_level_tab[pos];
        op = bc_buf[pos];
        if (op == 0 || op >= OP_COUNT) {
            JS_ThrowInternalError(ctx, "invalid opcode (op=%d, pc=%d)", op, pos);
            goto fail;
        }
        oi = &short_opcode_info(op);
        pos_next = pos + oi->size;
        if (pos_next > s->bc_len) {
            JS_ThrowInternalError(ctx, "bytecode buffer overflow (op=%d, pc=%d)", op, pos);
            goto fail;
        }
        n_pop = oi->n_pop;
        /* call pops a variable number of arguments */
        if (oi->fmt == OP_FMT_npop || oi->fmt == OP_FMT_npop_u16) {
            n_pop += get_u16(bc_buf + pos + 1);
        } else {
#if SHORT_OPCODES
            if (oi->fmt == OP_FMT_npopx) {
                n_pop += op - OP_call0;
            }
#endif
        }

        if (stack_len < n_pop) {
            JS_ThrowInternalError(ctx, "stack underflow (op=%d, pc=%d)", op, pos);
            goto fail;
        }
        stack_len += oi->n_push - n_pop;
        if (stack_len > s->stack_len_max) {
            s->stack_len_max = stack_len;
            if (s->stack_len_max > JS_STACK_SIZE_MAX) {
                JS_ThrowInternalError(ctx, "stack overflow (op=%d, pc=%d)", op, pos);
                goto fail;
            }
        }
        switch(op) {
        case OP_tail_call:
        case OP_tail_call_method:
        case OP_return:
        case OP_return_undef:
        case OP_return_async:
        case OP_throw:
        case OP_throw_error:
        case OP_ret:
            goto done_insn;
        case OP_goto:
            diff = get_u32(bc_buf + pos + 1);
            pos_next = pos + 1 + diff;
            break;
#if SHORT_OPCODES
        case OP_goto16:
            diff = (int16_t)get_u16(bc_buf + pos + 1);
            pos_next = pos + 1 + diff;
            break;
        case OP_goto8:
            diff = (int8_t)bc_buf[pos + 1];
            pos_next = pos + 1 + diff;
            break;
        case OP_if_true8:
        case OP_if_false8:
            diff = (int8_t)bc_buf[pos + 1];
            if (ss_check(ctx, s, pos + 1 + diff, op, stack_len))
                goto fail;
            break;
#endif
        case OP_if_true:
        case OP_if_false:
        case OP_catch:
            diff = get_u32(bc_buf + pos + 1);
            if (ss_check(ctx, s, pos + 1 + diff, op, stack_len))
                goto fail;
            break;
        case OP_gosub:
            diff = get_u32(bc_buf + pos + 1);
            if (ss_check(ctx, s, pos + 1 + diff, op, stack_len + 1))
                goto fail;
            break;
        case OP_with_get_var:
        case OP_with_delete_var:
            diff = get_u32(bc_buf + pos + 5);
            if (ss_check(ctx, s, pos + 5 + diff, op, stack_len + 1))
                goto fail;
            break;
        case OP_with_make_ref:
        case OP_with_get_ref:
        case OP_with_get_ref_undef:
            diff = get_u32(bc_buf + pos + 5);
            if (ss_check(ctx, s, pos + 5 + diff, op, stack_len + 2))
                goto fail;
            break;
        case OP_with_put_var:
            diff = get_u32(bc_buf + pos + 5);
            if (ss_check(ctx, s, pos + 5 + diff, op, stack_len - 1))
                goto fail;
            break;

        default:
            break;
        }
        if (ss_check(ctx, s, pos_next, op, stack_len))
            goto fail;
    done_insn: ;
    }
    js_free(ctx, s->stack_level_tab);
    js_free(ctx, s->pc_stack);
    *pstack_size = s->stack_len_max;
    return 0;
 fail:
    js_free(ctx, s->stack_level_tab);
    js_free(ctx, s->pc_stack);
    *pstack_size = 0;
    return -1;
}

static int add_module_variables(JSContext *ctx, JSFunctionDef *fd)
{
    int i, idx;
    JSModuleDef *m = fd->module;
    JSExportEntry *me;
    JSGlobalVar *hf;

    /* The imported global variables were added as closure variables
       in js_parse_import(). We add here the module global
       variables. */

    for(i = 0; i < fd->global_var_count; i++) {
        hf = &fd->global_vars[i];
        if (add_closure_var(ctx, fd, TRUE, FALSE, i, hf->var_name, hf->is_const,
                            hf->is_lexical, FALSE) < 0)
            return -1;
    }

    /* resolve the variable names of the local exports */
    for(i = 0; i < m->export_entries_count; i++) {
        me = &m->export_entries[i];
        if (me->export_type == JS_EXPORT_TYPE_LOCAL) {
            idx = find_closure_var(ctx, fd, me->local_name);
            if (idx < 0) {
                JS_ThrowSyntaxErrorAtom(ctx, "exported variable '%s' does not exist",
                                        me->local_name);
                return -1;
            }
            me->u.local.var_idx = idx;
        }
    }
    return 0;
}

/* create a function object from a function definition. The function
   definition is freed. All the child functions are also created. It
   must be done this way to resolve all the variables. */
static JSValue js_create_function(JSContext *ctx, JSFunctionDef *fd)
{
    JSValue func_obj;
    JSFunctionBytecode *b;
    struct list_head *el, *el1;
    int stack_size, scope, idx;
    int function_size, byte_code_offset, cpool_offset;
    int closure_var_offset, vardefs_offset;

    /* recompute scope linkage */
    for (scope = 0; scope < fd->scope_count; scope++) {
        fd->scopes[scope].first = -1;
    }
    if (fd->has_parameter_expressions) {
        /* special end of variable list marker for the argument scope */
        fd->scopes[ARG_SCOPE_INDEX].first = ARG_SCOPE_END;
    }
    for (idx = 0; idx < fd->var_count; idx++) {
        JSVarDef *vd = &fd->vars[idx];
        vd->scope_next = fd->scopes[vd->scope_level].first;
        fd->scopes[vd->scope_level].first = idx;
    }
    for (scope = 2; scope < fd->scope_count; scope++) {
        JSVarScope *sd = &fd->scopes[scope];
        if (sd->first < 0)
            sd->first = fd->scopes[sd->parent].first;
    }
    for (idx = 0; idx < fd->var_count; idx++) {
        JSVarDef *vd = &fd->vars[idx];
        if (vd->scope_next < 0 && vd->scope_level > 1) {
            scope = fd->scopes[vd->scope_level].parent;
            vd->scope_next = fd->scopes[scope].first;
        }
    }

    /* if the function contains an eval call, the closure variables
       are used to compile the eval and they must be ordered by scope,
       so it is necessary to create the closure variables before any
       other variable lookup is done. */
    if (fd->has_eval_call)
        add_eval_variables(ctx, fd);

    /* add the module global variables in the closure */
    if (fd->module) {
        if (add_module_variables(ctx, fd))
            goto fail;
    }

    /* first create all the child functions */
    list_for_each_safe(el, el1, &fd->child_list) {
        JSFunctionDef *fd1;
        int cpool_idx;

        fd1 = list_entry(el, JSFunctionDef, link);
        cpool_idx = fd1->parent_cpool_idx;
        func_obj = js_create_function(ctx, fd1);
        if (JS_IsException(func_obj))
            goto fail;
        /* save it in the constant pool */
        assert(cpool_idx >= 0);
        fd->cpool[cpool_idx] = func_obj;
    }

#if defined(DUMP_BYTECODE) && (DUMP_BYTECODE & 4)
    if (!(fd->js_mode & JS_MODE_STRIP)) {
        printf("pass 1\n");
        dump_byte_code(ctx, 1, fd->byte_code.buf, fd->byte_code.size,
                       fd->args, fd->arg_count, fd->vars, fd->var_count,
                       fd->closure_var, fd->closure_var_count,
                       fd->cpool, fd->cpool_count, fd->source, fd->line_num,
                       fd->label_slots, NULL);
        printf("\n");
    }
#endif

    if (resolve_variables(ctx, fd))
        goto fail;

#if defined(DUMP_BYTECODE) && (DUMP_BYTECODE & 2)
    if (!(fd->js_mode & JS_MODE_STRIP)) {
        printf("pass 2\n");
        dump_byte_code(ctx, 2, fd->byte_code.buf, fd->byte_code.size,
                       fd->args, fd->arg_count, fd->vars, fd->var_count,
                       fd->closure_var, fd->closure_var_count,
                       fd->cpool, fd->cpool_count, fd->source, fd->line_num,
                       fd->label_slots, NULL);
        printf("\n");
    }
#endif

    if (resolve_labels(ctx, fd))
        goto fail;

    if (compute_stack_size(ctx, fd, &stack_size) < 0)
        goto fail;

    if (fd->js_mode & JS_MODE_STRIP) {
        function_size = offsetof(JSFunctionBytecode, debug);
    } else {
        function_size = sizeof(*b);
    }
    cpool_offset = function_size;
    function_size += fd->cpool_count * sizeof(*fd->cpool);
    vardefs_offset = function_size;
    if (!(fd->js_mode & JS_MODE_STRIP) || fd->has_eval_call) {
        function_size += (fd->arg_count + fd->var_count) * sizeof(*b->vardefs);
    }
    closure_var_offset = function_size;
    function_size += fd->closure_var_count * sizeof(*fd->closure_var);
    byte_code_offset = function_size;
    function_size += fd->byte_code.size;

    b = js_mallocz(ctx, function_size);
    if (!b)
        goto fail;
    b->header.ref_count = 1;

    b->byte_code_buf = (void *)((uint8_t*)b + byte_code_offset);
    b->byte_code_len = fd->byte_code.size;
    memcpy(b->byte_code_buf, fd->byte_code.buf, fd->byte_code.size);
    js_free(ctx, fd->byte_code.buf);
    fd->byte_code.buf = NULL;

    b->func_name = fd->func_name;
    if (fd->arg_count + fd->var_count > 0) {
        if ((fd->js_mode & JS_MODE_STRIP) && !fd->has_eval_call) {
            /* Strip variable definitions not needed at runtime */
            int i;
            for(i = 0; i < fd->var_count; i++) {
                JS_FreeAtom(ctx, fd->vars[i].var_name);
            }
            for(i = 0; i < fd->arg_count; i++) {
                JS_FreeAtom(ctx, fd->args[i].var_name);
            }
            for(i = 0; i < fd->closure_var_count; i++) {
                JS_FreeAtom(ctx, fd->closure_var[i].var_name);
                fd->closure_var[i].var_name = JS_ATOM_NULL;
            }
        } else {
            b->vardefs = (void *)((uint8_t*)b + vardefs_offset);
            memcpy(b->vardefs, fd->args, fd->arg_count * sizeof(fd->args[0]));
            memcpy(b->vardefs + fd->arg_count, fd->vars, fd->var_count * sizeof(fd->vars[0]));
        }
        b->var_count = fd->var_count;
        b->arg_count = fd->arg_count;
        b->defined_arg_count = fd->defined_arg_count;
        js_free(ctx, fd->args);
        js_free(ctx, fd->vars);
    }
    b->cpool_count = fd->cpool_count;
    if (b->cpool_count) {
        b->cpool = (void *)((uint8_t*)b + cpool_offset);
        memcpy(b->cpool, fd->cpool, b->cpool_count * sizeof(*b->cpool));
    }
    js_free(ctx, fd->cpool);
    fd->cpool = NULL;

    b->stack_size = stack_size;

    if (fd->js_mode & JS_MODE_STRIP) {
        JS_FreeAtom(ctx, fd->filename);
        dbuf_free(&fd->pc2line);    // probably useless
    } else {
        /* XXX: source and pc2line info should be packed at the end of the
           JSFunctionBytecode structure, avoiding allocation overhead
         */
        b->has_debug = 1;
        b->debug.filename = fd->filename;
        b->debug.line_num = fd->line_num;

        //DynBuf pc2line;
        //compute_pc2line_info(fd, &pc2line);
        //js_free(ctx, fd->line_number_slots)
        b->debug.pc2line_buf = js_realloc(ctx, fd->pc2line.buf, fd->pc2line.size);
        if (!b->debug.pc2line_buf)
            b->debug.pc2line_buf = fd->pc2line.buf;
        b->debug.pc2line_len = fd->pc2line.size;
        b->debug.source = fd->source;
        b->debug.source_len = fd->source_len;
    }
    if (fd->scopes != fd->def_scope_array)
        js_free(ctx, fd->scopes);

    b->closure_var_count = fd->closure_var_count;
    if (b->closure_var_count) {
        b->closure_var = (void *)((uint8_t*)b + closure_var_offset);
        memcpy(b->closure_var, fd->closure_var, b->closure_var_count * sizeof(*b->closure_var));
    }
    js_free(ctx, fd->closure_var);
    fd->closure_var = NULL;

    b->has_prototype = fd->has_prototype;
    b->has_simple_parameter_list = fd->has_simple_parameter_list;
    b->js_mode = fd->js_mode;
    b->is_derived_class_constructor = fd->is_derived_class_constructor;
    b->func_kind = fd->func_kind;
    b->need_home_object = (fd->home_object_var_idx >= 0 ||
                           fd->need_home_object);
    b->new_target_allowed = fd->new_target_allowed;
    b->super_call_allowed = fd->super_call_allowed;
    b->super_allowed = fd->super_allowed;
    b->arguments_allowed = fd->arguments_allowed;
    b->backtrace_barrier = fd->backtrace_barrier;
    b->realm = JS_DupContext(ctx);

    add_gc_object(ctx->rt, &b->header, JS_GC_OBJ_TYPE_FUNCTION_BYTECODE);
    
#if defined(DUMP_BYTECODE) && (DUMP_BYTECODE & 1)
    if (!(fd->js_mode & JS_MODE_STRIP)) {
        js_dump_function_bytecode(ctx, b);
    }
#endif

    if (fd->parent) {
        /* remove from parent list */
        list_del(&fd->link);
    }

    js_free(ctx, fd);
    return JS_MKPTR(JS_TAG_FUNCTION_BYTECODE, b);
 fail:
    js_free_function_def(ctx, fd);
    return JS_EXCEPTION;
}

static void free_function_bytecode(JSRuntime *rt, JSFunctionBytecode *b)
{
    int i;

#if 0
    {
        char buf[ATOM_GET_STR_BUF_SIZE];
        printf("freeing %s\n",
               JS_AtomGetStrRT(rt, buf, sizeof(buf), b->func_name));
    }
#endif
    free_bytecode_atoms(rt, b->byte_code_buf, b->byte_code_len, TRUE);

    if (b->vardefs) {
        for(i = 0; i < b->arg_count + b->var_count; i++) {
            JS_FreeAtomRT(rt, b->vardefs[i].var_name);
        }
    }
    for(i = 0; i < b->cpool_count; i++)
        JS_FreeValueRT(rt, b->cpool[i]);

    for(i = 0; i < b->closure_var_count; i++) {
        JSClosureVar *cv = &b->closure_var[i];
        JS_FreeAtomRT(rt, cv->var_name);
    }
    if (b->realm)
        JS_FreeContext(b->realm);

    JS_FreeAtomRT(rt, b->func_name);
    if (b->has_debug) {
        JS_FreeAtomRT(rt, b->debug.filename);
        js_free_rt(rt, b->debug.pc2line_buf);
        js_free_rt(rt, b->debug.source);
    }

    remove_gc_object(&b->header);
    if (rt->gc_phase == JS_GC_PHASE_REMOVE_CYCLES && b->header.ref_count != 0) {
        list_add_tail(&b->header.link, &rt->gc_zero_ref_count_list);
    } else {
        js_free_rt(rt, b);
    }
}

static __exception int js_parse_directives(JSParseState *s)
{
    char str[20];
    JSParsePos pos;
    BOOL has_semi;

    if (s->token.val != TOK_STRING)
        return 0;

    js_parse_get_pos(s, &pos);

    while(s->token.val == TOK_STRING) {
        /* Copy actual source string representation */
        snprintf(str, sizeof str, "%.*s",
                 (int)(s->buf_ptr - s->token.ptr - 2), s->token.ptr + 1);

        if (next_token(s))
            return -1;

        has_semi = FALSE;
        switch (s->token.val) {
        case ';':
            if (next_token(s))
                return -1;
            has_semi = TRUE;
            break;
        case '}':
        case TOK_EOF:
            has_semi = TRUE;
            break;
        case TOK_NUMBER:
        case TOK_STRING:
        case TOK_TEMPLATE:
        case TOK_IDENT:
        case TOK_REGEXP:
        case TOK_DEC:
        case TOK_INC:
        case TOK_NULL:
        case TOK_FALSE:
        case TOK_TRUE:
        case TOK_IF:
        case TOK_RETURN:
        case TOK_VAR:
        case TOK_THIS:
        case TOK_DELETE:
        case TOK_TYPEOF:
        case TOK_NEW:
        case TOK_DO:
        case TOK_WHILE:
        case TOK_FOR:
        case TOK_SWITCH:
        case TOK_THROW:
        case TOK_TRY:
        case TOK_FUNCTION:
        case TOK_DEBUGGER:
        case TOK_WITH:
        case TOK_CLASS:
        case TOK_CONST:
        case TOK_ENUM:
        case TOK_EXPORT:
        case TOK_IMPORT:
        case TOK_SUPER:
        case TOK_INTERFACE:
        case TOK_LET:
        case TOK_PACKAGE:
        case TOK_PRIVATE:
        case TOK_PROTECTED:
        case TOK_PUBLIC:
        case TOK_STATIC:
            /* automatic insertion of ';' */
            if (s->got_lf)
                has_semi = TRUE;
            break;
        default:
            break;
        }
        if (!has_semi)
            break;
        if (!strcmp(str, "use strict")) {
            s->cur_func->has_use_strict = TRUE;
            s->cur_func->js_mode |= JS_MODE_STRICT;
        }
#if !defined(DUMP_BYTECODE) || !(DUMP_BYTECODE & 8)
        else if (!strcmp(str, "use strip")) {
            s->cur_func->js_mode |= JS_MODE_STRIP;
        }
#endif
#ifdef CONFIG_BIGNUM
        else if (s->ctx->bignum_ext && !strcmp(str, "use math")) {
            s->cur_func->js_mode |= JS_MODE_MATH;
        }
#endif
    }
    return js_parse_seek_token(s, &pos);
}

static int js_parse_function_check_names(JSParseState *s, JSFunctionDef *fd,
                                         JSAtom func_name)
{
    JSAtom name;
    int i, idx;

    if (fd->js_mode & JS_MODE_STRICT) {
        if (!fd->has_simple_parameter_list && fd->has_use_strict) {
            return js_parse_error(s, "\"use strict\" not allowed in function with default or destructuring parameter");
        }
        if (func_name == JS_ATOM_eval || func_name == JS_ATOM_arguments) {
            return js_parse_error(s, "invalid function name in strict code");
        }
        for (idx = 0; idx < fd->arg_count; idx++) {
            name = fd->args[idx].var_name;

            if (name == JS_ATOM_eval || name == JS_ATOM_arguments) {
                return js_parse_error(s, "invalid argument name in strict code");
            }
        }
    }
    /* check async_generator case */
    if ((fd->js_mode & JS_MODE_STRICT)
    ||  !fd->has_simple_parameter_list
    ||  (fd->func_type == JS_PARSE_FUNC_METHOD && fd->func_kind == JS_FUNC_ASYNC)
    ||  fd->func_type == JS_PARSE_FUNC_ARROW
    ||  fd->func_type == JS_PARSE_FUNC_METHOD) {
        for (idx = 0; idx < fd->arg_count; idx++) {
            name = fd->args[idx].var_name;
            if (name != JS_ATOM_NULL) {
                for (i = 0; i < idx; i++) {
                    if (fd->args[i].var_name == name)
                        goto duplicate;
                }
                /* Check if argument name duplicates a destructuring parameter */
                /* XXX: should have a flag for such variables */
                for (i = 0; i < fd->var_count; i++) {
                    if (fd->vars[i].var_name == name &&
                        fd->vars[i].scope_level == 0)
                        goto duplicate;
                }
            }
        }
    }
    return 0;

duplicate:
    return js_parse_error(s, "duplicate argument names not allowed in this context");
}

/* create a function to initialize class fields */
static JSFunctionDef *js_parse_function_class_fields_init(JSParseState *s)
{
    JSFunctionDef *fd;
    
    fd = js_new_function_def(s->ctx, s->cur_func, FALSE, FALSE,
                             s->filename, 0);
    if (!fd)
        return NULL;
    fd->func_name = JS_ATOM_NULL;
    fd->has_prototype = FALSE;
    fd->has_home_object = TRUE;
    
    fd->has_arguments_binding = FALSE;
    fd->has_this_binding = TRUE;
    fd->is_derived_class_constructor = FALSE;
    fd->new_target_allowed = TRUE;
    fd->super_call_allowed = FALSE;
    fd->super_allowed = fd->has_home_object;
    fd->arguments_allowed = FALSE;
    
    fd->func_kind = JS_FUNC_NORMAL;
    fd->func_type = JS_PARSE_FUNC_METHOD;
    return fd;
}

/* func_name must be JS_ATOM_NULL for JS_PARSE_FUNC_STATEMENT and
   JS_PARSE_FUNC_EXPR, JS_PARSE_FUNC_ARROW and JS_PARSE_FUNC_VAR */
static __exception int js_parse_function_decl2(JSParseState *s,
                                               JSParseFunctionEnum func_type,
                                               JSFunctionKindEnum func_kind,
                                               JSAtom func_name,
                                               const uint8_t *ptr,
                                               int function_line_num,
                                               JSParseExportEnum export_flag,
                                               JSFunctionDef **pfd)
{
    JSContext *ctx = s->ctx;
    JSFunctionDef *fd = s->cur_func;
    BOOL is_expr;
    int func_idx, lexical_func_idx = -1;
    BOOL has_opt_arg;
    BOOL create_func_var = FALSE;

    is_expr = (func_type != JS_PARSE_FUNC_STATEMENT &&
               func_type != JS_PARSE_FUNC_VAR);

    if (func_type == JS_PARSE_FUNC_STATEMENT ||
        func_type == JS_PARSE_FUNC_VAR ||
        func_type == JS_PARSE_FUNC_EXPR) {
        if (func_kind == JS_FUNC_NORMAL &&
            token_is_pseudo_keyword(s, JS_ATOM_async) &&
            peek_token(s, TRUE) != '\n') {
            if (next_token(s))
                return -1;
            func_kind = JS_FUNC_ASYNC;
        }
        if (next_token(s))
            return -1;
        if (s->token.val == '*') {
            if (next_token(s))
                return -1;
            func_kind |= JS_FUNC_GENERATOR;
        }

        if (s->token.val == TOK_IDENT) {
            if (s->token.u.ident.is_reserved ||
                (s->token.u.ident.atom == JS_ATOM_yield &&
                 func_type == JS_PARSE_FUNC_EXPR &&
                 (func_kind & JS_FUNC_GENERATOR)) ||
                (s->token.u.ident.atom == JS_ATOM_await &&
                 func_type == JS_PARSE_FUNC_EXPR &&
                 (func_kind & JS_FUNC_ASYNC))) {
                return js_parse_error_reserved_identifier(s);
            }
        }
        if (s->token.val == TOK_IDENT ||
            (((s->token.val == TOK_YIELD && !(fd->js_mode & JS_MODE_STRICT)) ||
             (s->token.val == TOK_AWAIT && !s->is_module)) &&
             func_type == JS_PARSE_FUNC_EXPR)) {
            func_name = JS_DupAtom(ctx, s->token.u.ident.atom);
            if (next_token(s)) {
                JS_FreeAtom(ctx, func_name);
                return -1;
            }
        } else {
            if (func_type != JS_PARSE_FUNC_EXPR &&
                export_flag != JS_PARSE_EXPORT_DEFAULT) {
                return js_parse_error(s, "function name expected");
            }
        }
    } else if (func_type != JS_PARSE_FUNC_ARROW) {
        func_name = JS_DupAtom(ctx, func_name);
    }

    if (fd->is_eval && fd->eval_type == JS_EVAL_TYPE_MODULE &&
        (func_type == JS_PARSE_FUNC_STATEMENT || func_type == JS_PARSE_FUNC_VAR)) {
        JSGlobalVar *hf;
        hf = find_global_var(fd, func_name);
        /* XXX: should check scope chain */
        if (hf && hf->scope_level == fd->scope_level) {
            js_parse_error(s, "invalid redefinition of global identifier in module code");
            JS_FreeAtom(ctx, func_name);
            return -1;
        }
    }

    if (func_type == JS_PARSE_FUNC_VAR) {
        if (!(fd->js_mode & JS_MODE_STRICT)
        && func_kind == JS_FUNC_NORMAL
        &&  find_lexical_decl(ctx, fd, func_name, fd->scope_first, FALSE) < 0
        &&  !((func_idx = find_var(ctx, fd, func_name)) >= 0 && (func_idx & ARGUMENT_VAR_OFFSET))
        &&  !(func_name == JS_ATOM_arguments && fd->has_arguments_binding)) {
            create_func_var = TRUE;
        }
        /* Create the lexical name here so that the function closure
           contains it */
        if (fd->is_eval &&
            (fd->eval_type == JS_EVAL_TYPE_GLOBAL ||
             fd->eval_type == JS_EVAL_TYPE_MODULE) &&
            fd->scope_level == fd->body_scope) {
            /* avoid creating a lexical variable in the global
               scope. XXX: check annex B */
            JSGlobalVar *hf;
            hf = find_global_var(fd, func_name);
            /* XXX: should check scope chain */
            if (hf && hf->scope_level == fd->scope_level) {
                js_parse_error(s, "invalid redefinition of global identifier");
                JS_FreeAtom(ctx, func_name);
                return -1;
            }
        } else {
            /* Always create a lexical name, fail if at the same scope as
               existing name */
            /* Lexical variable will be initialized upon entering scope */
            lexical_func_idx = define_var(s, fd, func_name,
                                          func_kind != JS_FUNC_NORMAL ?
                                          JS_VAR_DEF_NEW_FUNCTION_DECL :
                                          JS_VAR_DEF_FUNCTION_DECL);
            if (lexical_func_idx < 0) {
                JS_FreeAtom(ctx, func_name);
                return -1;
            }
        }
    }

    fd = js_new_function_def(ctx, fd, FALSE, is_expr,
                             s->filename, function_line_num);
    if (!fd) {
        JS_FreeAtom(ctx, func_name);
        return -1;
    }
    if (pfd)
        *pfd = fd;
    s->cur_func = fd;
    fd->func_name = func_name;
    /* XXX: test !fd->is_generator is always false */
    fd->has_prototype = (func_type == JS_PARSE_FUNC_STATEMENT ||
                         func_type == JS_PARSE_FUNC_VAR ||
                         func_type == JS_PARSE_FUNC_EXPR) &&
                        func_kind == JS_FUNC_NORMAL;
    fd->has_home_object = (func_type == JS_PARSE_FUNC_METHOD ||
                           func_type == JS_PARSE_FUNC_GETTER ||
                           func_type == JS_PARSE_FUNC_SETTER ||
                           func_type == JS_PARSE_FUNC_CLASS_CONSTRUCTOR ||
                           func_type == JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR);
    fd->has_arguments_binding = (func_type != JS_PARSE_FUNC_ARROW);
    fd->has_this_binding = fd->has_arguments_binding;
    fd->is_derived_class_constructor = (func_type == JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR);
    if (func_type == JS_PARSE_FUNC_ARROW) {
        fd->new_target_allowed = fd->parent->new_target_allowed;
        fd->super_call_allowed = fd->parent->super_call_allowed;
        fd->super_allowed = fd->parent->super_allowed;
        fd->arguments_allowed = fd->parent->arguments_allowed;
    } else {
        fd->new_target_allowed = TRUE;
        fd->super_call_allowed = fd->is_derived_class_constructor;
        fd->super_allowed = fd->has_home_object;
        fd->arguments_allowed = TRUE;
    }

    /* fd->in_function_body == FALSE prevents yield/await during the parsing
       of the arguments in generator/async functions. They are parsed as
       regular identifiers for other function kinds. */
    fd->func_kind = func_kind;
    fd->func_type = func_type;

    if (func_type == JS_PARSE_FUNC_CLASS_CONSTRUCTOR ||
        func_type == JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR) {
        /* error if not invoked as a constructor */
        emit_op(s, OP_check_ctor);
    }

    if (func_type == JS_PARSE_FUNC_CLASS_CONSTRUCTOR) {
        emit_class_field_init(s);
    }
    
    /* parse arguments */
    fd->has_simple_parameter_list = TRUE;
    fd->has_parameter_expressions = FALSE;
    has_opt_arg = FALSE;
    if (func_type == JS_PARSE_FUNC_ARROW && s->token.val == TOK_IDENT) {
        JSAtom name;
        if (s->token.u.ident.is_reserved) {
            js_parse_error_reserved_identifier(s);
            goto fail;
        }
        name = s->token.u.ident.atom;
        if (add_arg(ctx, fd, name) < 0)
            goto fail;
        fd->defined_arg_count = 1;
    } else {
        if (s->token.val == '(') {
            int skip_bits;
            /* if there is an '=' inside the parameter list, we
               consider there is a parameter expression inside */
            js_parse_skip_parens_token(s, &skip_bits, FALSE);
            if (skip_bits & SKIP_HAS_ASSIGNMENT)
                fd->has_parameter_expressions = TRUE;
            if (next_token(s))
                goto fail;
        } else {
            if (js_parse_expect(s, '('))
                goto fail;
        }

        if (fd->has_parameter_expressions) {
            fd->scope_level = -1; /* force no parent scope */
            if (push_scope(s) < 0)
                return -1;
        }
        
        while (s->token.val != ')') {
            JSAtom name;
            BOOL rest = FALSE;
            int idx, has_initializer;

            if (s->token.val == TOK_ELLIPSIS) {
                fd->has_simple_parameter_list = FALSE;
                rest = TRUE;
                if (next_token(s))
                    goto fail;
            }
            if (s->token.val == '[' || s->token.val == '{') {
                fd->has_simple_parameter_list = FALSE;
                if (rest) {
                    emit_op(s, OP_rest);
                    emit_u16(s, fd->arg_count);
                } else {
                    /* unnamed arg for destructuring */
                    idx = add_arg(ctx, fd, JS_ATOM_NULL);
                    emit_op(s, OP_get_arg);
                    emit_u16(s, idx);
                }
                has_initializer = js_parse_destructuring_element(s, fd->has_parameter_expressions ? TOK_LET : TOK_VAR, 1, TRUE, -1, TRUE);
                if (has_initializer < 0)
                    goto fail;
                if (has_initializer)
                    has_opt_arg = TRUE;
                if (!has_opt_arg)
                    fd->defined_arg_count++;
            } else if (s->token.val == TOK_IDENT) {
                if (s->token.u.ident.is_reserved) {
                    js_parse_error_reserved_identifier(s);
                    goto fail;
                }
                name = s->token.u.ident.atom;
                if (name == JS_ATOM_yield && fd->func_kind == JS_FUNC_GENERATOR) {
                    js_parse_error_reserved_identifier(s);
                    goto fail;
                }
                if (fd->has_parameter_expressions) {
                    if (define_var(s, fd, name, JS_VAR_DEF_LET) < 0)
                        goto fail;
                }
                /* XXX: could avoid allocating an argument if rest is true */
                idx = add_arg(ctx, fd, name);
                if (idx < 0)
                    goto fail;
                if (next_token(s))
                    goto fail;
                if (rest) {
                    emit_op(s, OP_rest);
                    emit_u16(s, idx);
                    if (fd->has_parameter_expressions) {
                        emit_op(s, OP_dup);
                        emit_op(s, OP_scope_put_var_init);
                        emit_atom(s, name);
                        emit_u16(s, fd->scope_level);
                    }
                    emit_op(s, OP_put_arg);
                    emit_u16(s, idx);
                    fd->has_simple_parameter_list = FALSE;
                    has_opt_arg = TRUE;
                } else if (s->token.val == '=') {
                    int label;
                    
                    fd->has_simple_parameter_list = FALSE;
                    has_opt_arg = TRUE;

                    if (next_token(s))
                        goto fail;

                    label = new_label(s);
                    emit_op(s, OP_get_arg);
                    emit_u16(s, idx);
                    emit_op(s, OP_dup);
                    emit_op(s, OP_undefined);
                    emit_op(s, OP_strict_eq);
                    emit_goto(s, OP_if_false, label);
                    emit_op(s, OP_drop);
                    if (js_parse_assign_expr(s))
                        goto fail;
                    set_object_name(s, name);
                    emit_op(s, OP_dup);
                    emit_op(s, OP_put_arg);
                    emit_u16(s, idx);
                    emit_label(s, label);
                    emit_op(s, OP_scope_put_var_init);
                    emit_atom(s, name);
                    emit_u16(s, fd->scope_level);
                } else {
                    if (!has_opt_arg) {
                        fd->defined_arg_count++;
                    }
                    if (fd->has_parameter_expressions) {
                        /* copy the argument to the argument scope */
                        emit_op(s, OP_get_arg);
                        emit_u16(s, idx);
                        emit_op(s, OP_scope_put_var_init);
                        emit_atom(s, name);
                        emit_u16(s, fd->scope_level);
                    }
                }
            } else {
                js_parse_error(s, "missing formal parameter");
                goto fail;
            }
            if (rest && s->token.val != ')') {
                js_parse_expect(s, ')');
                goto fail;
            }
            if (s->token.val == ')')
                break;
            if (js_parse_expect(s, ','))
                goto fail;
        }
        if ((func_type == JS_PARSE_FUNC_GETTER && fd->arg_count != 0) ||
            (func_type == JS_PARSE_FUNC_SETTER && fd->arg_count != 1)) {
            js_parse_error(s, "invalid number of arguments for getter or setter");
            goto fail;
        }
    }

    if (fd->has_parameter_expressions) {
        int idx;

        /* Copy the variables in the argument scope to the variable
           scope (see FunctionDeclarationInstantiation() in spec). The
           normal arguments are already present, so no need to copy
           them. */
        idx = fd->scopes[fd->scope_level].first;
        while (idx >= 0) {
            JSVarDef *vd = &fd->vars[idx];
            if (vd->scope_level != fd->scope_level)
                break;
            if (find_var(ctx, fd, vd->var_name) < 0) {
                if (add_var(ctx, fd, vd->var_name) < 0)
                    goto fail;
                vd = &fd->vars[idx]; /* fd->vars may have been reallocated */
                emit_op(s, OP_scope_get_var);
                emit_atom(s, vd->var_name);
                emit_u16(s, fd->scope_level);
                emit_op(s, OP_scope_put_var);
                emit_atom(s, vd->var_name);
                emit_u16(s, 0);
            }
            idx = vd->scope_next;
        }
        
        /* the argument scope has no parent, hence we don't use pop_scope(s) */
        emit_op(s, OP_leave_scope);
        emit_u16(s, fd->scope_level);

        /* set the variable scope as the current scope */
        fd->scope_level = 0;
        fd->scope_first = fd->scopes[fd->scope_level].first;
    }
    
    if (next_token(s))
        goto fail;

    /* generator function: yield after the parameters are evaluated */
    if (func_kind == JS_FUNC_GENERATOR ||
        func_kind == JS_FUNC_ASYNC_GENERATOR)
        emit_op(s, OP_initial_yield);

    /* in generators, yield expression is forbidden during the parsing
       of the arguments */
    fd->in_function_body = TRUE;
    push_scope(s);  /* enter body scope */
    fd->body_scope = fd->scope_level;

    if (s->token.val == TOK_ARROW) {
        if (next_token(s))
            goto fail;

        if (s->token.val != '{') {
            if (js_parse_function_check_names(s, fd, func_name))
                goto fail;

            if (js_parse_assign_expr(s))
                goto fail;

            if (func_kind != JS_FUNC_NORMAL)
                emit_op(s, OP_return_async);
            else
                emit_op(s, OP_return);

            if (!(fd->js_mode & JS_MODE_STRIP)) {
                /* save the function source code */
                /* the end of the function source code is after the last
                   token of the function source stored into s->last_ptr */
                fd->source_len = s->last_ptr - ptr;
                fd->source = js_strndup(ctx, (const char *)ptr, fd->source_len);
                if (!fd->source)
                    goto fail;
            }
            goto done;
        }
    }

    if (js_parse_expect(s, '{'))
        goto fail;

    if (js_parse_directives(s))
        goto fail;

    /* in strict_mode, check function and argument names */
    if (js_parse_function_check_names(s, fd, func_name))
        goto fail;

    while (s->token.val != '}') {
        if (js_parse_source_element(s))
            goto fail;
    }
    if (!(fd->js_mode & JS_MODE_STRIP)) {
        /* save the function source code */
        fd->source_len = s->buf_ptr - ptr;
        fd->source = js_strndup(ctx, (const char *)ptr, fd->source_len);
        if (!fd->source)
            goto fail;
    }

    if (next_token(s)) {
        /* consume the '}' */
        goto fail;
    }

    /* in case there is no return, add one */
    if (js_is_live_code(s)) {
        emit_return(s, FALSE);
    }
done:
    s->cur_func = fd->parent;

    /* create the function object */
    {
        int idx;
        JSAtom func_name = fd->func_name;

        /* the real object will be set at the end of the compilation */
        idx = cpool_add(s, JS_NULL);
        fd->parent_cpool_idx = idx;

        if (is_expr) {
            /* for constructors, no code needs to be generated here */
            if (func_type != JS_PARSE_FUNC_CLASS_CONSTRUCTOR &&
                func_type != JS_PARSE_FUNC_DERIVED_CLASS_CONSTRUCTOR) {
                /* OP_fclosure creates the function object from the bytecode
                   and adds the scope information */
                emit_op(s, OP_fclosure);
                emit_u32(s, idx);
                if (func_name == JS_ATOM_NULL) {
                    emit_op(s, OP_set_name);
                    emit_u32(s, JS_ATOM_NULL);
                }
            }
        } else if (func_type == JS_PARSE_FUNC_VAR) {
            emit_op(s, OP_fclosure);
            emit_u32(s, idx);
            if (create_func_var) {
                if (s->cur_func->is_global_var) {
                    JSGlobalVar *hf;
                    /* the global variable must be defined at the start of the
                       function */
                    hf = add_global_var(ctx, s->cur_func, func_name);
                    if (!hf)
                        goto fail;
                    /* it is considered as defined at the top level
                       (needed for annex B.3.3.4 and B.3.3.5
                       checks) */
                    hf->scope_level = 0; 
                    hf->force_init = ((s->cur_func->js_mode & JS_MODE_STRICT) != 0);
                    /* store directly into global var, bypass lexical scope */
                    emit_op(s, OP_dup);
                    emit_op(s, OP_scope_put_var);
                    emit_atom(s, func_name);
                    emit_u16(s, 0);
                } else {
                    /* do not call define_var to bypass lexical scope check */
                    func_idx = find_var(ctx, s->cur_func, func_name);
                    if (func_idx < 0) {
                        func_idx = add_var(ctx, s->cur_func, func_name);
                        if (func_idx < 0)
                            goto fail;
                    }
                    /* store directly into local var, bypass lexical catch scope */
                    emit_op(s, OP_dup);
                    emit_op(s, OP_scope_put_var);
                    emit_atom(s, func_name);
                    emit_u16(s, 0);
                }
            }
            if (lexical_func_idx >= 0) {
                /* lexical variable will be initialized upon entering scope */
                s->cur_func->vars[lexical_func_idx].func_pool_idx = idx;
                emit_op(s, OP_drop);
            } else {
                /* store function object into its lexical name */
                /* XXX: could use OP_put_loc directly */
                emit_op(s, OP_scope_put_var_init);
                emit_atom(s, func_name);
                emit_u16(s, s->cur_func->scope_level);
            }
        } else {
            if (!s->cur_func->is_global_var) {
                int var_idx = define_var(s, s->cur_func, func_name, JS_VAR_DEF_VAR);

                if (var_idx < 0)
                    goto fail;
                /* the variable will be assigned at the top of the function */
                if (var_idx & ARGUMENT_VAR_OFFSET) {
                    s->cur_func->args[var_idx - ARGUMENT_VAR_OFFSET].func_pool_idx = idx;
                } else {
                    s->cur_func->vars[var_idx].func_pool_idx = idx;
                }
            } else {
                JSAtom func_var_name;
                JSGlobalVar *hf;
                if (func_name == JS_ATOM_NULL)
                    func_var_name = JS_ATOM__default_; /* export default */
                else
                    func_var_name = func_name;
                /* the variable will be assigned at the top of the function */
                hf = add_global_var(ctx, s->cur_func, func_var_name);
                if (!hf)
                    goto fail;
                hf->cpool_idx = idx;
                if (export_flag != JS_PARSE_EXPORT_NONE) {
                    if (!add_export_entry(s, s->cur_func->module, func_var_name,
                                          export_flag == JS_PARSE_EXPORT_NAMED ? func_var_name : JS_ATOM_default, JS_EXPORT_TYPE_LOCAL))
                        goto fail;
                }
            }
        }
    }
    return 0;
 fail:
    s->cur_func = fd->parent;
    js_free_function_def(ctx, fd);
    if (pfd)
        *pfd = NULL;
    return -1;
}

static __exception int js_parse_function_decl(JSParseState *s,
                                              JSParseFunctionEnum func_type,
                                              JSFunctionKindEnum func_kind,
                                              JSAtom func_name,
                                              const uint8_t *ptr,
                                              int function_line_num)
{
    return js_parse_function_decl2(s, func_type, func_kind, func_name, ptr,
                                   function_line_num, JS_PARSE_EXPORT_NONE,
                                   NULL);
}

static __exception int js_parse_program(JSParseState *s)
{
    JSFunctionDef *fd = s->cur_func;
    int idx;

    if (next_token(s))
        return -1;

    if (js_parse_directives(s))
        return -1;

    fd->is_global_var = (fd->eval_type == JS_EVAL_TYPE_GLOBAL) ||
        (fd->eval_type == JS_EVAL_TYPE_MODULE) ||
        !(fd->js_mode & JS_MODE_STRICT);

    if (!s->is_module) {
        /* hidden variable for the return value */
        fd->eval_ret_idx = idx = add_var(s->ctx, fd, JS_ATOM__ret_);
        if (idx < 0)
            return -1;
    }

    while (s->token.val != TOK_EOF) {
        if (js_parse_source_element(s))
            return -1;
    }

    if (!s->is_module) {
        /* return the value of the hidden variable eval_ret_idx  */
        emit_op(s, OP_get_loc);
        emit_u16(s, fd->eval_ret_idx);

        emit_op(s, OP_return);
    } else {
        emit_op(s, OP_return_undef);
    }

    return 0;
}

static void js_parse_init(JSContext *ctx, JSParseState *s,
                          const char *input, size_t input_len,
                          const char *filename)
{
    memset(s, 0, sizeof(*s));
    s->ctx = ctx;
    s->filename = filename;
    s->line_num = 1;
    s->buf_ptr = (const uint8_t *)input;
    s->buf_end = s->buf_ptr + input_len;
    s->token.val = ' ';
    s->token.line_num = 1;
}

static JSValue JS_EvalFunctionInternal(JSContext *ctx, JSValue fun_obj,
                                       JSValueConst this_obj,
                                       JSVarRef **var_refs, JSStackFrame *sf)
{
    JSValue ret_val;
    uint32_t tag;

    tag = JS_VALUE_GET_TAG(fun_obj);
    if (tag == JS_TAG_FUNCTION_BYTECODE) {
        fun_obj = js_closure(ctx, fun_obj, var_refs, sf);
        ret_val = JS_CallFree(ctx, fun_obj, this_obj, 0, NULL);
    } else if (tag == JS_TAG_MODULE) {
        JSModuleDef *m;
        m = JS_VALUE_GET_PTR(fun_obj);
        /* the module refcount should be >= 2 */
        JS_FreeValue(ctx, fun_obj);
        if (js_create_module_function(ctx, m) < 0)
            goto fail;
        if (js_link_module(ctx, m) < 0)
            goto fail;
        ret_val = js_evaluate_module(ctx, m);
        if (JS_IsException(ret_val)) {
        fail:
            js_free_modules(ctx, JS_FREE_MODULE_NOT_EVALUATED);
            return JS_EXCEPTION;
        }
    } else {
        JS_FreeValue(ctx, fun_obj);
        ret_val = JS_ThrowTypeError(ctx, "bytecode function expected");
    }
    return ret_val;
}

JSValue JS_EvalFunction(JSContext *ctx, JSValue fun_obj)
{
    return JS_EvalFunctionInternal(ctx, fun_obj, ctx->global_obj, NULL, NULL);
}

static void skip_shebang(JSParseState *s)
{
    const uint8_t *p = s->buf_ptr;
    int c;

    if (p[0] == '#' && p[1] == '!') {
        p += 2;
        while (p < s->buf_end) {
            if (*p == '\n' || *p == '\r') {
                break;
            } else if (*p >= 0x80) {
                c = unicode_from_utf8(p, UTF8_CHAR_LEN_MAX, &p);
                if (c == CP_LS || c == CP_PS) {
                    break;
                } else if (c == -1) {
                    p++; /* skip invalid UTF-8 */
                }
            } else {
                p++;
            }
        }
        s->buf_ptr = p;
    }
}

/* 'input' must be zero terminated i.e. input[input_len] = '\0'. */
static JSValue __JS_EvalInternal(JSContext *ctx, JSValueConst this_obj,
                                 const char *input, size_t input_len,
                                 const char *filename, int flags, int scope_idx)
{
    JSParseState s1, *s = &s1;
    int err, js_mode, eval_type;
    JSValue fun_obj, ret_val;
    JSStackFrame *sf;
    JSVarRef **var_refs;
    JSFunctionBytecode *b;
    JSFunctionDef *fd;
    JSModuleDef *m;

    js_parse_init(ctx, s, input, input_len, filename);
    skip_shebang(s);

    eval_type = flags & JS_EVAL_TYPE_MASK;
    m = NULL;
    if (eval_type == JS_EVAL_TYPE_DIRECT) {
        JSObject *p;
        sf = ctx->rt->current_stack_frame;
        assert(sf != NULL);
        assert(JS_VALUE_GET_TAG(sf->cur_func) == JS_TAG_OBJECT);
        p = JS_VALUE_GET_OBJ(sf->cur_func);
        assert(js_class_has_bytecode(p->class_id));
        b = p->u.func.function_bytecode;
        var_refs = p->u.func.var_refs;
        js_mode = b->js_mode;
    } else {
        sf = NULL;
        b = NULL;
        var_refs = NULL;
        js_mode = 0;
        if (flags & JS_EVAL_FLAG_STRICT)
            js_mode |= JS_MODE_STRICT;
        if (flags & JS_EVAL_FLAG_STRIP)
            js_mode |= JS_MODE_STRIP;
        if (eval_type == JS_EVAL_TYPE_MODULE) {
            JSAtom module_name = JS_NewAtom(ctx, filename);
            if (module_name == JS_ATOM_NULL)
                return JS_EXCEPTION;
            m = js_new_module_def(ctx, module_name);
            if (!m)
                return JS_EXCEPTION;
            js_mode |= JS_MODE_STRICT;
        }
    }
    fd = js_new_function_def(ctx, NULL, TRUE, FALSE, filename, 1);
    if (!fd)
        goto fail1;
    s->cur_func = fd;
    fd->eval_type = eval_type;
    fd->has_this_binding = (eval_type != JS_EVAL_TYPE_DIRECT);
    fd->backtrace_barrier = ((flags & JS_EVAL_FLAG_BACKTRACE_BARRIER) != 0);
    if (eval_type == JS_EVAL_TYPE_DIRECT) {
        fd->new_target_allowed = b->new_target_allowed;
        fd->super_call_allowed = b->super_call_allowed;
        fd->super_allowed = b->super_allowed;
        fd->arguments_allowed = b->arguments_allowed;
    } else {
        fd->new_target_allowed = FALSE;
        fd->super_call_allowed = FALSE;
        fd->super_allowed = FALSE;
        fd->arguments_allowed = TRUE;
    }
    fd->js_mode = js_mode;
    fd->func_name = JS_DupAtom(ctx, JS_ATOM__eval_);
    if (b) {
        if (add_closure_variables(ctx, fd, b, scope_idx))
            goto fail;
    }
    fd->module = m;
    s->is_module = (m != NULL);
    s->allow_html_comments = !s->is_module;

    push_scope(s); /* body scope */
    fd->body_scope = fd->scope_level;
    
    err = js_parse_program(s);
    if (err) {
    fail:
        free_token(s, &s->token);
        js_free_function_def(ctx, fd);
        goto fail1;
    }

    /* create the function object and all the enclosed functions */
    fun_obj = js_create_function(ctx, fd);
    if (JS_IsException(fun_obj))
        goto fail1;
    /* Could add a flag to avoid resolution if necessary */
    if (m) {
        m->func_obj = fun_obj;
        if (js_resolve_module(ctx, m) < 0)
            goto fail1;
        fun_obj = JS_DupValue(ctx, JS_MKPTR(JS_TAG_MODULE, m));
    }
    if (flags & JS_EVAL_FLAG_COMPILE_ONLY) {
        ret_val = fun_obj;
    } else {
        ret_val = JS_EvalFunctionInternal(ctx, fun_obj, this_obj, var_refs, sf);
    }
    return ret_val;
 fail1:
    /* XXX: should free all the unresolved dependencies */
    if (m)
        js_free_module_def(ctx, m);
    return JS_EXCEPTION;
}

/* the indirection is needed to make 'eval' optional */
static JSValue JS_EvalInternal(JSContext *ctx, JSValueConst this_obj,
                               const char *input, size_t input_len,
                               const char *filename, int flags, int scope_idx)
{
    if (unlikely(!ctx->eval_internal)) {
        return JS_ThrowTypeError(ctx, "eval is not supported");
    }
    return ctx->eval_internal(ctx, this_obj, input, input_len, filename,
                              flags, scope_idx);
}

static JSValue JS_EvalObject(JSContext *ctx, JSValueConst this_obj,
                             JSValueConst val, int flags, int scope_idx)
{
    JSValue ret;
    const char *str;
    size_t len;

    if (!JS_IsString(val))
        return JS_DupValue(ctx, val);
    str = JS_ToCStringLen(ctx, &len, val);
    if (!str)
        return JS_EXCEPTION;
    ret = JS_EvalInternal(ctx, this_obj, str, len, "<input>", flags, scope_idx);
    JS_FreeCString(ctx, str);
    return ret;

}

JSValue JS_EvalThis(JSContext *ctx, JSValueConst this_obj,
                    const char *input, size_t input_len,
                    const char *filename, int eval_flags)
{
    int eval_type = eval_flags & JS_EVAL_TYPE_MASK;
    JSValue ret;

    assert(eval_type == JS_EVAL_TYPE_GLOBAL ||
           eval_type == JS_EVAL_TYPE_MODULE);
    ret = JS_EvalInternal(ctx, this_obj, input, input_len, filename,
                          eval_flags, -1);
    return ret;
}

JSValue JS_Eval(JSContext *ctx, const char *input, size_t input_len,
                const char *filename, int eval_flags)
{
    return JS_EvalThis(ctx, ctx->global_obj, input, input_len, filename,
                       eval_flags);
}

int JS_ResolveModule(JSContext *ctx, JSValueConst obj)
{
    if (JS_VALUE_GET_TAG(obj) == JS_TAG_MODULE) {
        JSModuleDef *m = JS_VALUE_GET_PTR(obj);
        if (js_resolve_module(ctx, m) < 0) {
            js_free_modules(ctx, JS_FREE_MODULE_NOT_RESOLVED);
            return -1;
        }
    }
    return 0;
}

/*******************************************************************/
/* object list */

typedef struct {
    JSObject *obj;
    uint32_t hash_next; /* -1 if no next entry */
} JSObjectListEntry;

/* XXX: reuse it to optimize weak references */
typedef struct {
    JSObjectListEntry *object_tab;
    int object_count;
    int object_size;
    uint32_t *hash_table;
    uint32_t hash_size;
} JSObjectList;

static void js_object_list_init(JSObjectList *s)
{
    memset(s, 0, sizeof(*s));
}

static uint32_t js_object_list_get_hash(JSObject *p, uint32_t hash_size)
{
    return ((uintptr_t)p * 3163) & (hash_size - 1);
}

static int js_object_list_resize_hash(JSContext *ctx, JSObjectList *s,
                                 uint32_t new_hash_size)
{
    JSObjectListEntry *e;
    uint32_t i, h, *new_hash_table;

    new_hash_table = js_malloc(ctx, sizeof(new_hash_table[0]) * new_hash_size);
    if (!new_hash_table)
        return -1;
    js_free(ctx, s->hash_table);
    s->hash_table = new_hash_table;
    s->hash_size = new_hash_size;
    
    for(i = 0; i < s->hash_size; i++) {
        s->hash_table[i] = -1;
    }
    for(i = 0; i < s->object_count; i++) {
        e = &s->object_tab[i];
        h = js_object_list_get_hash(e->obj, s->hash_size); 
        e->hash_next = s->hash_table[h];
        s->hash_table[h] = i;
    }
    return 0;
}

/* the reference count of 'obj' is not modified. Return 0 if OK, -1 if
   memory error */
static int js_object_list_add(JSContext *ctx, JSObjectList *s, JSObject *obj)
{
    JSObjectListEntry *e;
    uint32_t h, new_hash_size;
    
    if (js_resize_array(ctx, (void *)&s->object_tab,
                        sizeof(s->object_tab[0]),
                        &s->object_size, s->object_count + 1))
        return -1;
    if (unlikely((s->object_count + 1) >= s->hash_size)) {
        new_hash_size = max_uint32(s->hash_size, 4);
        while (new_hash_size <= s->object_count)
            new_hash_size *= 2;
        if (js_object_list_resize_hash(ctx, s, new_hash_size))
            return -1;
    }
    e = &s->object_tab[s->object_count++];
    h = js_object_list_get_hash(obj, s->hash_size); 
    e->obj = obj;
    e->hash_next = s->hash_table[h];
    s->hash_table[h] = s->object_count - 1;
    return 0;
}

/* return -1 if not present or the object index */
static int js_object_list_find(JSContext *ctx, JSObjectList *s, JSObject *obj)
{
    JSObjectListEntry *e;
    uint32_t h, p;

    /* must test empty size because there is no hash table */
    if (s->object_count == 0)
        return -1;
    h = js_object_list_get_hash(obj, s->hash_size); 
    p = s->hash_table[h];
    while (p != -1) {
        e = &s->object_tab[p];
        if (e->obj == obj)
            return p;
        p = e->hash_next;
    }
    return -1;
}

static void js_object_list_end(JSContext *ctx, JSObjectList *s)
{
    js_free(ctx, s->object_tab);
    js_free(ctx, s->hash_table);
}

/*******************************************************************/
/* binary object writer & reader */

typedef enum BCTagEnum {
    BC_TAG_NULL = 1,
    BC_TAG_UNDEFINED,
    BC_TAG_BOOL_FALSE,
    BC_TAG_BOOL_TRUE,
    BC_TAG_INT32,
    BC_TAG_FLOAT64,
    BC_TAG_STRING,
    BC_TAG_OBJECT,
    BC_TAG_ARRAY,
    BC_TAG_BIG_INT,
    BC_TAG_BIG_FLOAT,
    BC_TAG_BIG_DECIMAL,
    BC_TAG_TEMPLATE_OBJECT,
    BC_TAG_FUNCTION_BYTECODE,
    BC_TAG_MODULE,
    BC_TAG_TYPED_ARRAY,
    BC_TAG_ARRAY_BUFFER,
    BC_TAG_SHARED_ARRAY_BUFFER,
    BC_TAG_DATE,
    BC_TAG_OBJECT_VALUE,
    BC_TAG_OBJECT_REFERENCE,
} BCTagEnum;

#ifdef CONFIG_BIGNUM
#define BC_BASE_VERSION 2
#else
#define BC_BASE_VERSION 1
#endif
#define BC_BE_VERSION 0x40
#ifdef WORDS_BIGENDIAN
#define BC_VERSION (BC_BASE_VERSION | BC_BE_VERSION)
#else
#define BC_VERSION BC_BASE_VERSION
#endif

typedef struct BCWriterState {
    JSContext *ctx;
    DynBuf dbuf;
    BOOL byte_swap : 8;
    BOOL allow_bytecode : 8;
    BOOL allow_sab : 8;
    BOOL allow_reference : 8;
    uint32_t first_atom;
    uint32_t *atom_to_idx;
    int atom_to_idx_size;
    JSAtom *idx_to_atom;
    int idx_to_atom_count;
    int idx_to_atom_size;
    uint8_t **sab_tab;
    int sab_tab_len;
    int sab_tab_size;
    /* list of referenced objects (used if allow_reference = TRUE) */
    JSObjectList object_list;
} BCWriterState;

#ifdef DUMP_READ_OBJECT
static const char * const bc_tag_str[] = {
    "invalid",
    "null",
    "undefined",
    "false",
    "true",
    "int32",
    "float64",
    "string",
    "object",
    "array",
    "bigint",
    "bigfloat",
    "bigdecimal",
    "template",
    "function",
    "module",
    "TypedArray",
    "ArrayBuffer",
    "SharedArrayBuffer",
    "Date",
    "ObjectValue",
    "ObjectReference",
};
#endif

static void bc_put_u8(BCWriterState *s, uint8_t v)
{
    dbuf_putc(&s->dbuf, v);
}

static void bc_put_u16(BCWriterState *s, uint16_t v)
{
    if (s->byte_swap)
        v = bswap16(v);
    dbuf_put_u16(&s->dbuf, v);
}

static __maybe_unused void bc_put_u32(BCWriterState *s, uint32_t v)
{
    if (s->byte_swap)
        v = bswap32(v);
    dbuf_put_u32(&s->dbuf, v);
}

static void bc_put_u64(BCWriterState *s, uint64_t v)
{
    if (s->byte_swap)
        v = bswap64(v);
    dbuf_put(&s->dbuf, (uint8_t *)&v, sizeof(v));
}

static void bc_put_leb128(BCWriterState *s, uint32_t v)
{
    dbuf_put_leb128(&s->dbuf, v);
}

static void bc_put_sleb128(BCWriterState *s, int32_t v)
{
    dbuf_put_sleb128(&s->dbuf, v);
}

static void bc_set_flags(uint32_t *pflags, int *pidx, uint32_t val, int n)
{
    *pflags = *pflags | (val << *pidx);
    *pidx += n;
}

static int bc_atom_to_idx(BCWriterState *s, uint32_t *pres, JSAtom atom)
{
    uint32_t v;

    if (atom < s->first_atom || __JS_AtomIsTaggedInt(atom)) {
        *pres = atom;
        return 0;
    }
    atom -= s->first_atom;
    if (atom < s->atom_to_idx_size && s->atom_to_idx[atom] != 0) {
        *pres = s->atom_to_idx[atom];
        return 0;
    }
    if (atom >= s->atom_to_idx_size) {
        int old_size, i;
        old_size = s->atom_to_idx_size;
        if (js_resize_array(s->ctx, (void **)&s->atom_to_idx,
                            sizeof(s->atom_to_idx[0]), &s->atom_to_idx_size,
                            atom + 1))
            return -1;
        /* XXX: could add a specific js_resize_array() function to do it */
        for(i = old_size; i < s->atom_to_idx_size; i++)
            s->atom_to_idx[i] = 0;
    }
    if (js_resize_array(s->ctx, (void **)&s->idx_to_atom,
                        sizeof(s->idx_to_atom[0]),
                        &s->idx_to_atom_size, s->idx_to_atom_count + 1))
        goto fail;

    v = s->idx_to_atom_count++;
    s->idx_to_atom[v] = atom + s->first_atom;
    v += s->first_atom;
    s->atom_to_idx[atom] = v;
    *pres = v;
    return 0;
 fail:
    *pres = 0;
    return -1;
}

static int bc_put_atom(BCWriterState *s, JSAtom atom)
{
    uint32_t v;

    if (__JS_AtomIsTaggedInt(atom)) {
        v = (__JS_AtomToUInt32(atom) << 1) | 1;
    } else {
        if (bc_atom_to_idx(s, &v, atom))
            return -1;
        v <<= 1;
    }
    bc_put_leb128(s, v);
    return 0;
}

static void bc_byte_swap(uint8_t *bc_buf, int bc_len)
{
    int pos, len, op, fmt;

    pos = 0;
    while (pos < bc_len) {
        op = bc_buf[pos];
        len = short_opcode_info(op).size;
        fmt = short_opcode_info(op).fmt;
        switch(fmt) {
        case OP_FMT_u16:
        case OP_FMT_i16:
        case OP_FMT_label16:
        case OP_FMT_npop:
        case OP_FMT_loc:
        case OP_FMT_arg:
        case OP_FMT_var_ref:
            put_u16(bc_buf + pos + 1,
                    bswap16(get_u16(bc_buf + pos + 1)));
            break;
        case OP_FMT_i32:
        case OP_FMT_u32:
        case OP_FMT_const:
        case OP_FMT_label:
        case OP_FMT_atom:
        case OP_FMT_atom_u8:
            put_u32(bc_buf + pos + 1,
                    bswap32(get_u32(bc_buf + pos + 1)));
            break;
        case OP_FMT_atom_u16:
        case OP_FMT_label_u16:
            put_u32(bc_buf + pos + 1,
                    bswap32(get_u32(bc_buf + pos + 1)));
            put_u16(bc_buf + pos + 1 + 4,
                    bswap16(get_u16(bc_buf + pos + 1 + 4)));
            break;
        case OP_FMT_atom_label_u8:
        case OP_FMT_atom_label_u16:
            put_u32(bc_buf + pos + 1,
                    bswap32(get_u32(bc_buf + pos + 1)));
            put_u32(bc_buf + pos + 1 + 4,
                    bswap32(get_u32(bc_buf + pos + 1 + 4)));
            if (fmt == OP_FMT_atom_label_u16) {
                put_u16(bc_buf + pos + 1 + 4 + 4,
                        bswap16(get_u16(bc_buf + pos + 1 + 4 + 4)));
            }
            break;
        case OP_FMT_npop_u16:
            put_u16(bc_buf + pos + 1,
                    bswap16(get_u16(bc_buf + pos + 1)));
            put_u16(bc_buf + pos + 1 + 2,
                    bswap16(get_u16(bc_buf + pos + 1 + 2)));
            break;
        default:
            break;
        }
        pos += len;
    }
}

static int JS_WriteFunctionBytecode(BCWriterState *s,
                                    const uint8_t *bc_buf1, int bc_len)
{
    int pos, len, op;
    JSAtom atom;
    uint8_t *bc_buf;
    uint32_t val;

    bc_buf = js_malloc(s->ctx, bc_len);
    if (!bc_buf)
        return -1;
    memcpy(bc_buf, bc_buf1, bc_len);

    pos = 0;
    while (pos < bc_len) {
        op = bc_buf[pos];
        len = short_opcode_info(op).size;
        switch(short_opcode_info(op).fmt) {
        case OP_FMT_atom:
        case OP_FMT_atom_u8:
        case OP_FMT_atom_u16:
        case OP_FMT_atom_label_u8:
        case OP_FMT_atom_label_u16:
            atom = get_u32(bc_buf + pos + 1);
            if (bc_atom_to_idx(s, &val, atom))
                goto fail;
            put_u32(bc_buf + pos + 1, val);
            break;
        default:
            break;
        }
        pos += len;
    }

    if (s->byte_swap)
        bc_byte_swap(bc_buf, bc_len);

    dbuf_put(&s->dbuf, bc_buf, bc_len);

    js_free(s->ctx, bc_buf);
    return 0;
 fail:
    js_free(s->ctx, bc_buf);
    return -1;
}

static void JS_WriteString(BCWriterState *s, JSString *p)
{
    int i;
    bc_put_leb128(s, ((uint32_t)p->len << 1) | p->is_wide_char);
    if (p->is_wide_char) {
        for(i = 0; i < p->len; i++)
            bc_put_u16(s, p->u.str16[i]);
    } else {
        dbuf_put(&s->dbuf, p->u.str8, p->len);
    }
}

#ifdef CONFIG_BIGNUM
static int JS_WriteBigNum(BCWriterState *s, JSValueConst obj)
{
    uint32_t tag, tag1;
    int64_t e;
    JSBigFloat *bf = JS_VALUE_GET_PTR(obj);
    bf_t *a = &bf->num;
    size_t len, i, n1, j;
    limb_t v;

    tag = JS_VALUE_GET_TAG(obj);
    switch(tag) {
    case JS_TAG_BIG_INT:
        tag1 = BC_TAG_BIG_INT;
        break;
    case JS_TAG_BIG_FLOAT:
        tag1 = BC_TAG_BIG_FLOAT;
        break;
    case JS_TAG_BIG_DECIMAL:
        tag1 = BC_TAG_BIG_DECIMAL;
        break;
    default:
        abort();
    }
    bc_put_u8(s, tag1);

    /* sign + exponent */
    if (a->expn == BF_EXP_ZERO)
        e = 0;
    else if (a->expn == BF_EXP_INF)
        e = 1;
    else if (a->expn == BF_EXP_NAN)
        e = 2;
    else if (a->expn >= 0)
        e = a->expn + 3;
    else
        e = a->expn;
    e = (e << 1) | a->sign;
    if (e < INT32_MIN || e > INT32_MAX) {
        JS_ThrowInternalError(s->ctx, "bignum exponent is too large");
        return -1;
    }
    bc_put_sleb128(s, e);

    /* mantissa */
    if (a->len != 0) {
        if (tag != JS_TAG_BIG_DECIMAL) {
            i = 0;
            while (i < a->len && a->tab[i] == 0)
                i++;
            assert(i < a->len);
            v = a->tab[i];
            n1 = sizeof(limb_t);
            while ((v & 0xff) == 0) {
                n1--;
                v >>= 8;
            }
            i++;
            len = (a->len - i) * sizeof(limb_t) + n1;
            if (len > INT32_MAX) {
                JS_ThrowInternalError(s->ctx, "bignum is too large");
                return -1;
            }
            bc_put_leb128(s, len);
            /* always saved in byte based little endian representation */
            for(j = 0; j < n1; j++) {
                dbuf_putc(&s->dbuf, v >> (j * 8));
            }
            for(; i < a->len; i++) {
                limb_t v = a->tab[i];
#if LIMB_BITS == 32
#ifdef WORDS_BIGENDIAN
                v = bswap32(v);
#endif
                dbuf_put_u32(&s->dbuf, v);
#else
#ifdef WORDS_BIGENDIAN
                v = bswap64(v);
#endif
                dbuf_put_u64(&s->dbuf, v);
#endif
            }
        } else {
            int bpos, d;
            uint8_t v8;
            size_t i0;
            
            /* little endian BCD */
            i = 0;
            while (i < a->len && a->tab[i] == 0)
                i++;
            assert(i < a->len);
            len = a->len * LIMB_DIGITS;
            v = a->tab[i];
            j = 0;
            while ((v % 10) == 0) {
                j++;
                v /= 10;
            }
            len -= j;
            assert(len > 0);
            if (len > INT32_MAX) {
                JS_ThrowInternalError(s->ctx, "bignum is too large");
                return -1;
            }
            bc_put_leb128(s, len);
            
            bpos = 0;
            v8 = 0;
            i0 = i;
            for(; i < a->len; i++) {
                if (i != i0) {
                    v = a->tab[i];
                    j = 0;
                }
                for(; j < LIMB_DIGITS; j++) {
                    d = v % 10;
                    v /= 10;
                    if (bpos == 0) {
                        v8 = d;
                        bpos = 1;
                    } else {
                        dbuf_putc(&s->dbuf, v8 | (d << 4));
                        bpos = 0;
                    }
                }
            }
            /* flush the last digit */
            if (bpos) {
                dbuf_putc(&s->dbuf, v8);
            }
        }
    }
    return 0;
}
#endif /* CONFIG_BIGNUM */

static int JS_WriteObjectRec(BCWriterState *s, JSValueConst obj);

static int JS_WriteFunctionTag(BCWriterState *s, JSValueConst obj)
{
    JSFunctionBytecode *b = JS_VALUE_GET_PTR(obj);
    uint32_t flags;
    int idx, i;
    
    bc_put_u8(s, BC_TAG_FUNCTION_BYTECODE);
    flags = idx = 0;
    bc_set_flags(&flags, &idx, b->has_prototype, 1);
    bc_set_flags(&flags, &idx, b->has_simple_parameter_list, 1);
    bc_set_flags(&flags, &idx, b->is_derived_class_constructor, 1);
    bc_set_flags(&flags, &idx, b->need_home_object, 1);
    bc_set_flags(&flags, &idx, b->func_kind, 2);
    bc_set_flags(&flags, &idx, b->new_target_allowed, 1);
    bc_set_flags(&flags, &idx, b->super_call_allowed, 1);
    bc_set_flags(&flags, &idx, b->super_allowed, 1);
    bc_set_flags(&flags, &idx, b->arguments_allowed, 1);
    bc_set_flags(&flags, &idx, b->has_debug, 1);
    bc_set_flags(&flags, &idx, b->backtrace_barrier, 1);
    assert(idx <= 16);
    bc_put_u16(s, flags);
    bc_put_u8(s, b->js_mode);
    bc_put_atom(s, b->func_name);
    
    bc_put_leb128(s, b->arg_count);
    bc_put_leb128(s, b->var_count);
    bc_put_leb128(s, b->defined_arg_count);
    bc_put_leb128(s, b->stack_size);
    bc_put_leb128(s, b->closure_var_count);
    bc_put_leb128(s, b->cpool_count);
    bc_put_leb128(s, b->byte_code_len);
    if (b->vardefs) {
        /* XXX: this field is redundant */
        bc_put_leb128(s, b->arg_count + b->var_count);
        for(i = 0; i < b->arg_count + b->var_count; i++) {
            JSVarDef *vd = &b->vardefs[i];
            bc_put_atom(s, vd->var_name);
            bc_put_leb128(s, vd->scope_level);
            bc_put_leb128(s, vd->scope_next + 1);
            flags = idx = 0;
            bc_set_flags(&flags, &idx, vd->var_kind, 4);
            bc_set_flags(&flags, &idx, vd->is_const, 1);
            bc_set_flags(&flags, &idx, vd->is_lexical, 1);
            bc_set_flags(&flags, &idx, vd->is_captured, 1);
            assert(idx <= 8);
            bc_put_u8(s, flags);
        }
    } else {
        bc_put_leb128(s, 0);
    }
    
    for(i = 0; i < b->closure_var_count; i++) {
        JSClosureVar *cv = &b->closure_var[i];
        bc_put_atom(s, cv->var_name);
        bc_put_leb128(s, cv->var_idx);
        flags = idx = 0;
        bc_set_flags(&flags, &idx, cv->is_local, 1);
        bc_set_flags(&flags, &idx, cv->is_arg, 1);
        bc_set_flags(&flags, &idx, cv->is_const, 1);
        bc_set_flags(&flags, &idx, cv->is_lexical, 1);
        bc_set_flags(&flags, &idx, cv->var_kind, 4);
        assert(idx <= 8);
        bc_put_u8(s, flags);
    }
    
    if (JS_WriteFunctionBytecode(s, b->byte_code_buf, b->byte_code_len))
        goto fail;
    
    if (b->has_debug) {
        bc_put_atom(s, b->debug.filename);
        bc_put_leb128(s, b->debug.line_num);
        bc_put_leb128(s, b->debug.pc2line_len);
        dbuf_put(&s->dbuf, b->debug.pc2line_buf, b->debug.pc2line_len);
    }
    
    for(i = 0; i < b->cpool_count; i++) {
        if (JS_WriteObjectRec(s, b->cpool[i]))
            goto fail;
    }
    return 0;
 fail:
    return -1;
}

static int JS_WriteModule(BCWriterState *s, JSValueConst obj)
{
    JSModuleDef *m = JS_VALUE_GET_PTR(obj);
    int i;
    
    bc_put_u8(s, BC_TAG_MODULE);
    bc_put_atom(s, m->module_name);
    
    bc_put_leb128(s, m->req_module_entries_count);
    for(i = 0; i < m->req_module_entries_count; i++) {
        JSReqModuleEntry *rme = &m->req_module_entries[i];
        bc_put_atom(s, rme->module_name);
    }
    
    bc_put_leb128(s, m->export_entries_count);
    for(i = 0; i < m->export_entries_count; i++) {
        JSExportEntry *me = &m->export_entries[i];
        bc_put_u8(s, me->export_type);
        if (me->export_type == JS_EXPORT_TYPE_LOCAL) {
            bc_put_leb128(s, me->u.local.var_idx);
        } else {
            bc_put_leb128(s, me->u.req_module_idx);
            bc_put_atom(s, me->local_name);
        }
        bc_put_atom(s, me->export_name);
    }
    
    bc_put_leb128(s, m->star_export_entries_count);
    for(i = 0; i < m->star_export_entries_count; i++) {
        JSStarExportEntry *se = &m->star_export_entries[i];
        bc_put_leb128(s, se->req_module_idx);
    }
    
    bc_put_leb128(s, m->import_entries_count);
    for(i = 0; i < m->import_entries_count; i++) {
        JSImportEntry *mi = &m->import_entries[i];
        bc_put_leb128(s, mi->var_idx);
        bc_put_atom(s, mi->import_name);
        bc_put_leb128(s, mi->req_module_idx);
    }
    
    if (JS_WriteObjectRec(s, m->func_obj))
        goto fail;
    return 0;
 fail:
    return -1;
}

static int JS_WriteArray(BCWriterState *s, JSValueConst obj)
{
    JSObject *p = JS_VALUE_GET_OBJ(obj);
    uint32_t i, len;
    JSValue val;
    int ret;
    BOOL is_template;
    
    if (s->allow_bytecode && !p->extensible) {
        /* not extensible array: we consider it is a
           template when we are saving bytecode */
        bc_put_u8(s, BC_TAG_TEMPLATE_OBJECT);
        is_template = TRUE;
    } else {
        bc_put_u8(s, BC_TAG_ARRAY);
        is_template = FALSE;
    }
    if (js_get_length32(s->ctx, &len, obj))
        goto fail1;
    bc_put_leb128(s, len);
    for(i = 0; i < len; i++) {
        val = JS_GetPropertyUint32(s->ctx, obj, i);
        if (JS_IsException(val))
            goto fail1;
        ret = JS_WriteObjectRec(s, val);
        JS_FreeValue(s->ctx, val);
        if (ret)
            goto fail1;
    }
    if (is_template) {
        val = JS_GetProperty(s->ctx, obj, JS_ATOM_raw);
        if (JS_IsException(val))
            goto fail1;
        ret = JS_WriteObjectRec(s, val);
        JS_FreeValue(s->ctx, val);
        if (ret)
            goto fail1;
    }
    return 0;
 fail1:
    return -1;
}

static int JS_WriteObjectTag(BCWriterState *s, JSValueConst obj)
{
    JSObject *p = JS_VALUE_GET_OBJ(obj);
    uint32_t i, prop_count;
    JSShape *sh;
    JSShapeProperty *pr;
    int pass;
    JSAtom atom;

    bc_put_u8(s, BC_TAG_OBJECT);
    prop_count = 0;
    sh = p->shape;
    for(pass = 0; pass < 2; pass++) {
        if (pass == 1)
            bc_put_leb128(s, prop_count);
        for(i = 0, pr = get_shape_prop(sh); i < sh->prop_count; i++, pr++) {
            atom = pr->atom;
            if (atom != JS_ATOM_NULL &&
                JS_AtomIsString(s->ctx, atom) &&
                (pr->flags & JS_PROP_ENUMERABLE)) {
                if (pr->flags & JS_PROP_TMASK) {
                    JS_ThrowTypeError(s->ctx, "only value properties are supported");
                    goto fail;
                }
                if (pass == 0) {
                    prop_count++;
                } else {
                    bc_put_atom(s, atom);
                    if (JS_WriteObjectRec(s, p->prop[i].u.value))
                        goto fail;
                }
            }
        }
    }
    return 0;
 fail:
    return -1;
}

static int JS_WriteTypedArray(BCWriterState *s, JSValueConst obj)
{
    JSObject *p = JS_VALUE_GET_OBJ(obj);
    JSTypedArray *ta = p->u.typed_array;

    bc_put_u8(s, BC_TAG_TYPED_ARRAY);
    bc_put_u8(s, p->class_id - JS_CLASS_UINT8C_ARRAY);
    bc_put_leb128(s, p->u.array.count);
    bc_put_leb128(s, ta->offset);
    if (JS_WriteObjectRec(s, JS_MKPTR(JS_TAG_OBJECT, ta->buffer)))
        return -1;
    return 0;
}

static int JS_WriteArrayBuffer(BCWriterState *s, JSValueConst obj)
{
    JSObject *p = JS_VALUE_GET_OBJ(obj);
    JSArrayBuffer *abuf = p->u.array_buffer;
    if (abuf->detached) {
        JS_ThrowTypeErrorDetachedArrayBuffer(s->ctx);
        return -1;
    }
    bc_put_u8(s, BC_TAG_ARRAY_BUFFER);
    bc_put_leb128(s, abuf->byte_length);
    dbuf_put(&s->dbuf, abuf->data, abuf->byte_length);
    return 0;
}

static int JS_WriteSharedArrayBuffer(BCWriterState *s, JSValueConst obj)
{
    JSObject *p = JS_VALUE_GET_OBJ(obj);
    JSArrayBuffer *abuf = p->u.array_buffer;
    assert(!abuf->detached); /* SharedArrayBuffer are never detached */
    bc_put_u8(s, BC_TAG_SHARED_ARRAY_BUFFER);
    bc_put_leb128(s, abuf->byte_length);
    bc_put_u64(s, (uintptr_t)abuf->data);
    if (js_resize_array(s->ctx, (void **)&s->sab_tab, sizeof(s->sab_tab[0]),
                        &s->sab_tab_size, s->sab_tab_len + 1))
        return -1;
    /* keep the SAB pointer so that the user can clone it or free it */
    s->sab_tab[s->sab_tab_len++] = abuf->data;
    return 0;
}

static int JS_WriteObjectRec(BCWriterState *s, JSValueConst obj)
{
    uint32_t tag;

    if (js_check_stack_overflow(s->ctx->rt, 0)) {
        JS_ThrowStackOverflow(s->ctx);
        return -1;
    }

    tag = JS_VALUE_GET_NORM_TAG(obj);
    switch(tag) {
    case JS_TAG_NULL:
        bc_put_u8(s, BC_TAG_NULL);
        break;
    case JS_TAG_UNDEFINED:
        bc_put_u8(s, BC_TAG_UNDEFINED);
        break;
    case JS_TAG_BOOL:
        bc_put_u8(s, BC_TAG_BOOL_FALSE + JS_VALUE_GET_INT(obj));
        break;
    case JS_TAG_INT:
        bc_put_u8(s, BC_TAG_INT32);
        bc_put_sleb128(s, JS_VALUE_GET_INT(obj));
        break;
    case JS_TAG_FLOAT64:
        {
            JSFloat64Union u;
            bc_put_u8(s, BC_TAG_FLOAT64);
            u.d = JS_VALUE_GET_FLOAT64(obj);
            bc_put_u64(s, u.u64);
        }
        break;
    case JS_TAG_STRING:
        {
            JSString *p = JS_VALUE_GET_STRING(obj);
            bc_put_u8(s, BC_TAG_STRING);
            JS_WriteString(s, p);
        }
        break;
    case JS_TAG_FUNCTION_BYTECODE:
        if (!s->allow_bytecode)
            goto invalid_tag;
        if (JS_WriteFunctionTag(s, obj))
            goto fail;
        break;
    case JS_TAG_MODULE:
        if (!s->allow_bytecode)
            goto invalid_tag;
        if (JS_WriteModule(s, obj))
            goto fail;
        break;
    case JS_TAG_OBJECT:
        {
            JSObject *p = JS_VALUE_GET_OBJ(obj);
            int ret, idx;
            
            if (s->allow_reference) {
                idx = js_object_list_find(s->ctx, &s->object_list, p);
                if (idx >= 0) {
                    bc_put_u8(s, BC_TAG_OBJECT_REFERENCE);
                    bc_put_leb128(s, idx);
                    break;
                } else {
                    if (js_object_list_add(s->ctx, &s->object_list, p))
                        goto fail;
                }
            } else {
                if (p->tmp_mark) {
                    JS_ThrowTypeError(s->ctx, "circular reference");
                    goto fail;
                }
                p->tmp_mark = 1;
            }
            switch(p->class_id) {
            case JS_CLASS_ARRAY:
                ret = JS_WriteArray(s, obj);
                break;
            case JS_CLASS_OBJECT:
                ret = JS_WriteObjectTag(s, obj);
                break;
            case JS_CLASS_ARRAY_BUFFER:
                ret = JS_WriteArrayBuffer(s, obj);
                break;
            case JS_CLASS_SHARED_ARRAY_BUFFER:
                if (!s->allow_sab)
                    goto invalid_tag;
                ret = JS_WriteSharedArrayBuffer(s, obj);
                break;
            case JS_CLASS_DATE:
                bc_put_u8(s, BC_TAG_DATE);
                ret = JS_WriteObjectRec(s, p->u.object_data);
                break;
            case JS_CLASS_NUMBER:
            case JS_CLASS_STRING:
            case JS_CLASS_BOOLEAN:
#ifdef CONFIG_BIGNUM
            case JS_CLASS_BIG_INT:
            case JS_CLASS_BIG_FLOAT:
            case JS_CLASS_BIG_DECIMAL:
#endif
                bc_put_u8(s, BC_TAG_OBJECT_VALUE);
                ret = JS_WriteObjectRec(s, p->u.object_data);
                break;
            default:
                if (p->class_id >= JS_CLASS_UINT8C_ARRAY &&
                    p->class_id <= JS_CLASS_FLOAT64_ARRAY) {
                    ret = JS_WriteTypedArray(s, obj);
                } else {
                    JS_ThrowTypeError(s->ctx, "unsupported object class");
                    ret = -1;
                }
                break;
            }
            p->tmp_mark = 0;
            if (ret)
                goto fail;
        }
        break;
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
    case JS_TAG_BIG_FLOAT:
    case JS_TAG_BIG_DECIMAL:
        if (JS_WriteBigNum(s, obj))
            goto fail;
        break;
#endif
    default:
    invalid_tag:
        JS_ThrowInternalError(s->ctx, "unsupported tag (%d)", tag);
        goto fail;
    }
    return 0;

 fail:
    return -1;
}

/* create the atom table */
static int JS_WriteObjectAtoms(BCWriterState *s)
{
    JSRuntime *rt = s->ctx->rt;
    DynBuf dbuf1;
    int i, atoms_size;
    uint8_t version;

    dbuf1 = s->dbuf;
    js_dbuf_init(s->ctx, &s->dbuf);

    version = BC_VERSION;
    if (s->byte_swap)
        version ^= BC_BE_VERSION;
    bc_put_u8(s, version);

    bc_put_leb128(s, s->idx_to_atom_count);
    for(i = 0; i < s->idx_to_atom_count; i++) {
        JSAtomStruct *p = rt->atom_array[s->idx_to_atom[i]];
        JS_WriteString(s, p);
    }
    /* XXX: should check for OOM in above phase */

    /* move the atoms at the start */
    /* XXX: could just append dbuf1 data, but it uses more memory if
       dbuf1 is larger than dbuf */
    atoms_size = s->dbuf.size;
    if (dbuf_realloc(&dbuf1, dbuf1.size + atoms_size))
        goto fail;
    memmove(dbuf1.buf + atoms_size, dbuf1.buf, dbuf1.size);
    memcpy(dbuf1.buf, s->dbuf.buf, atoms_size);
    dbuf1.size += atoms_size;
    dbuf_free(&s->dbuf);
    s->dbuf = dbuf1;
    return 0;
 fail:
    dbuf_free(&dbuf1);
    return -1;
}

uint8_t *JS_WriteObject2(JSContext *ctx, size_t *psize, JSValueConst obj,
                         int flags, uint8_t ***psab_tab, size_t *psab_tab_len)
{
    BCWriterState ss, *s = &ss;

    memset(s, 0, sizeof(*s));
    s->ctx = ctx;
    /* XXX: byte swapped output is untested */
    s->byte_swap = ((flags & JS_WRITE_OBJ_BSWAP) != 0);
    s->allow_bytecode = ((flags & JS_WRITE_OBJ_BYTECODE) != 0);
    s->allow_sab = ((flags & JS_WRITE_OBJ_SAB) != 0);
    s->allow_reference = ((flags & JS_WRITE_OBJ_REFERENCE) != 0);
    /* XXX: could use a different version when bytecode is included */
    if (s->allow_bytecode)
        s->first_atom = JS_ATOM_END;
    else
        s->first_atom = 1;
    js_dbuf_init(ctx, &s->dbuf);
    js_object_list_init(&s->object_list);
    
    if (JS_WriteObjectRec(s, obj))
        goto fail;
    if (JS_WriteObjectAtoms(s))
        goto fail;
    js_object_list_end(ctx, &s->object_list);
    js_free(ctx, s->atom_to_idx);
    js_free(ctx, s->idx_to_atom);
    *psize = s->dbuf.size;
    if (psab_tab)
        *psab_tab = s->sab_tab;
    if (psab_tab_len)
        *psab_tab_len = s->sab_tab_len;
    return s->dbuf.buf;
 fail:
    js_object_list_end(ctx, &s->object_list);
    js_free(ctx, s->atom_to_idx);
    js_free(ctx, s->idx_to_atom);
    dbuf_free(&s->dbuf);
    *psize = 0;
    if (psab_tab)
        *psab_tab = NULL;
    if (psab_tab_len)
        *psab_tab_len = 0;
    return NULL;
}

uint8_t *JS_WriteObject(JSContext *ctx, size_t *psize, JSValueConst obj,
                        int flags)
{
    return JS_WriteObject2(ctx, psize, obj, flags, NULL, NULL);
}

typedef struct BCReaderState {
    JSContext *ctx;
    const uint8_t *buf_start, *ptr, *buf_end;
    uint32_t first_atom;
    uint32_t idx_to_atom_count;
    JSAtom *idx_to_atom;
    int error_state;
    BOOL allow_sab : 8;
    BOOL allow_bytecode : 8;
    BOOL is_rom_data : 8;
    BOOL allow_reference : 8;
    /* object references */
    JSObject **objects;
    int objects_count;
    int objects_size;
    
#ifdef DUMP_READ_OBJECT
    const uint8_t *ptr_last;
    int level;
#endif
} BCReaderState;

#ifdef DUMP_READ_OBJECT
static void __attribute__((format(printf, 2, 3))) bc_read_trace(BCReaderState *s, const char *fmt, ...) {
    va_list ap;
    int i, n, n0;

    if (!s->ptr_last)
        s->ptr_last = s->buf_start;

    n = n0 = 0;
    if (s->ptr > s->ptr_last || s->ptr == s->buf_start) {
        n0 = printf("%04x: ", (int)(s->ptr_last - s->buf_start));
        n += n0;
    }
    for (i = 0; s->ptr_last < s->ptr; i++) {
        if ((i & 7) == 0 && i > 0) {
            printf("\n%*s", n0, "");
            n = n0;
        }
        n += printf(" %02x", *s->ptr_last++);
    }
    if (*fmt == '}')
        s->level--;
    if (n < 32 + s->level * 2) {
        printf("%*s", 32 + s->level * 2 - n, "");
    }
    va_start(ap, fmt);
    vfprintf(stdout, fmt, ap);
    va_end(ap);
    if (strchr(fmt, '{'))
        s->level++;
}
#else
#define bc_read_trace(...)
#endif

static int bc_read_error_end(BCReaderState *s)
{
    if (!s->error_state) {
        JS_ThrowSyntaxError(s->ctx, "read after the end of the buffer");
    }
    return s->error_state = -1;
}

static int bc_get_u8(BCReaderState *s, uint8_t *pval)
{
    if (unlikely(s->buf_end - s->ptr < 1)) {
        *pval = 0; /* avoid warning */
        return bc_read_error_end(s);
    }
    *pval = *s->ptr++;
    return 0;
}

static int bc_get_u16(BCReaderState *s, uint16_t *pval)
{
    if (unlikely(s->buf_end - s->ptr < 2)) {
        *pval = 0; /* avoid warning */
        return bc_read_error_end(s);
    }
    *pval = get_u16(s->ptr);
    s->ptr += 2;
    return 0;
}

static __maybe_unused int bc_get_u32(BCReaderState *s, uint32_t *pval)
{
    if (unlikely(s->buf_end - s->ptr < 4)) {
        *pval = 0; /* avoid warning */
        return bc_read_error_end(s);
    }
    *pval = get_u32(s->ptr);
    s->ptr += 4;
    return 0;
}

static int bc_get_u64(BCReaderState *s, uint64_t *pval)
{
    if (unlikely(s->buf_end - s->ptr < 8)) {
        *pval = 0; /* avoid warning */
        return bc_read_error_end(s);
    }
    *pval = get_u64(s->ptr);
    s->ptr += 8;
    return 0;
}

static int bc_get_leb128(BCReaderState *s, uint32_t *pval)
{
    int ret;
    ret = get_leb128(pval, s->ptr, s->buf_end);
    if (unlikely(ret < 0))
        return bc_read_error_end(s);
    s->ptr += ret;
    return 0;
}

static int bc_get_sleb128(BCReaderState *s, int32_t *pval)
{
    int ret;
    ret = get_sleb128(pval, s->ptr, s->buf_end);
    if (unlikely(ret < 0))
        return bc_read_error_end(s);
    s->ptr += ret;
    return 0;
}

/* XXX: used to read an `int` with a positive value */
static int bc_get_leb128_int(BCReaderState *s, int *pval)
{
    return bc_get_leb128(s, (uint32_t *)pval);
}

static int bc_get_leb128_u16(BCReaderState *s, uint16_t *pval)
{
    uint32_t val;
    if (bc_get_leb128(s, &val)) {
        *pval = 0;
        return -1;
    }
    *pval = val;
    return 0;
}

static int bc_get_buf(BCReaderState *s, uint8_t *buf, uint32_t buf_len)
{
    if (buf_len != 0) {
        if (unlikely(!buf || s->buf_end - s->ptr < buf_len))
            return bc_read_error_end(s);
        memcpy(buf, s->ptr, buf_len);
        s->ptr += buf_len;
    }
    return 0;
}

static int bc_idx_to_atom(BCReaderState *s, JSAtom *patom, uint32_t idx)
{
    JSAtom atom;

    if (__JS_AtomIsTaggedInt(idx)) {
        atom = idx;
    } else if (idx < s->first_atom) {
        atom = JS_DupAtom(s->ctx, idx);
    } else {
        idx -= s->first_atom;
        if (idx >= s->idx_to_atom_count) {
            JS_ThrowSyntaxError(s->ctx, "invalid atom index (pos=%u)",
                                (unsigned int)(s->ptr - s->buf_start));
            *patom = JS_ATOM_NULL;
            return s->error_state = -1;
        }
        atom = JS_DupAtom(s->ctx, s->idx_to_atom[idx]);
    }
    *patom = atom;
    return 0;
}

static int bc_get_atom(BCReaderState *s, JSAtom *patom)
{
    uint32_t v;
    if (bc_get_leb128(s, &v))
        return -1;
    if (v & 1) {
        *patom = __JS_AtomFromUInt32(v >> 1);
        return 0;
    } else {
        return bc_idx_to_atom(s, patom, v >> 1);
    }
}

static JSString *JS_ReadString(BCReaderState *s)
{
    uint32_t len;
    size_t size;
    BOOL is_wide_char;
    JSString *p;

    if (bc_get_leb128(s, &len))
        return NULL;
    is_wide_char = len & 1;
    len >>= 1;
    p = js_alloc_string(s->ctx, len, is_wide_char);
    if (!p) {
        s->error_state = -1;
        return NULL;
    }
    size = (size_t)len << is_wide_char;
    if ((s->buf_end - s->ptr) < size) {
        bc_read_error_end(s);
        js_free_string(s->ctx->rt, p);
        return NULL;
    }
    memcpy(p->u.str8, s->ptr, size);
    s->ptr += size;
    if (!is_wide_char) {
        p->u.str8[size] = '\0'; /* add the trailing zero for 8 bit strings */
    }
#ifdef DUMP_READ_OBJECT
    JS_DumpString(s->ctx->rt, p); printf("\n");
#endif
    return p;
}

static uint32_t bc_get_flags(uint32_t flags, int *pidx, int n)
{
    uint32_t val;
    /* XXX: this does not work for n == 32 */
    val = (flags >> *pidx) & ((1U << n) - 1);
    *pidx += n;
    return val;
}

static int JS_ReadFunctionBytecode(BCReaderState *s, JSFunctionBytecode *b,
                                   int byte_code_offset, uint32_t bc_len)
{
    uint8_t *bc_buf;
    int pos, len, op;
    JSAtom atom;
    uint32_t idx;

    if (s->is_rom_data) {
        /* directly use the input buffer */
        if (unlikely(s->buf_end - s->ptr < bc_len))
            return bc_read_error_end(s);
        bc_buf = (uint8_t *)s->ptr;
        s->ptr += bc_len;
    } else {
        bc_buf = (void *)((uint8_t*)b + byte_code_offset);
        if (bc_get_buf(s, bc_buf, bc_len))
            return -1;
    }
    b->byte_code_buf = bc_buf;

    pos = 0;
    while (pos < bc_len) {
        op = bc_buf[pos];
        len = short_opcode_info(op).size;
        switch(short_opcode_info(op).fmt) {
        case OP_FMT_atom:
        case OP_FMT_atom_u8:
        case OP_FMT_atom_u16:
        case OP_FMT_atom_label_u8:
        case OP_FMT_atom_label_u16:
            idx = get_u32(bc_buf + pos + 1);
            if (s->is_rom_data) {
                /* just increment the reference count of the atom */
                JS_DupAtom(s->ctx, (JSAtom)idx);
            } else {
                if (bc_idx_to_atom(s, &atom, idx)) {
                    /* Note: the atoms will be freed up to this position */
                    b->byte_code_len = pos;
                    return -1;
                }
                put_u32(bc_buf + pos + 1, atom);
#ifdef DUMP_READ_OBJECT
                bc_read_trace(s, "at %d, fixup atom: ", pos + 1); print_atom(s->ctx, atom); printf("\n");
#endif
            }
            break;
        default:
            break;
        }
        pos += len;
    }
    return 0;
}

#ifdef CONFIG_BIGNUM
static JSValue JS_ReadBigNum(BCReaderState *s, int tag)
{
    JSValue obj = JS_UNDEFINED;
    uint8_t v8;
    int32_t e;
    uint32_t len;
    limb_t l, i, n, j;
    JSBigFloat *p;
    limb_t v;
    bf_t *a;
    int bpos, d;
    
    p = js_new_bf(s->ctx);
    if (!p)
        goto fail;
    switch(tag) {
    case BC_TAG_BIG_INT:
        obj = JS_MKPTR(JS_TAG_BIG_INT, p);
        break;
    case BC_TAG_BIG_FLOAT:
        obj = JS_MKPTR(JS_TAG_BIG_FLOAT, p);
        break;
    case BC_TAG_BIG_DECIMAL:
        obj = JS_MKPTR(JS_TAG_BIG_DECIMAL, p);
        break;
    default:
        abort();
    }

    /* sign + exponent */
    if (bc_get_sleb128(s, &e))
        goto fail;

    a = &p->num;
    a->sign = e & 1;
    e >>= 1;
    if (e == 0)
        a->expn = BF_EXP_ZERO;
    else if (e == 1)
        a->expn = BF_EXP_INF;
    else if (e == 2)
        a->expn = BF_EXP_NAN;
    else if (e >= 3)
        a->expn = e - 3;
    else
        a->expn = e;

    /* mantissa */
    if (a->expn != BF_EXP_ZERO &&
        a->expn != BF_EXP_INF &&
        a->expn != BF_EXP_NAN) {
        if (bc_get_leb128(s, &len))
            goto fail;
        bc_read_trace(s, "len=%" PRId64 "\n", (int64_t)len);
        if (len == 0) {
            JS_ThrowInternalError(s->ctx, "invalid bignum length");
            goto fail;
        }
        if (tag != BC_TAG_BIG_DECIMAL)
            l = (len + sizeof(limb_t) - 1) / sizeof(limb_t);
        else
            l = (len + LIMB_DIGITS - 1) / LIMB_DIGITS;
        if (bf_resize(a, l)) {
            JS_ThrowOutOfMemory(s->ctx);
            goto fail;
        }
        if (tag != BC_TAG_BIG_DECIMAL) {
            n = len & (sizeof(limb_t) - 1);
            if (n != 0) {
                v = 0;
                for(i = 0; i < n; i++) {
                    if (bc_get_u8(s, &v8))
                        goto fail;
                    v |= (limb_t)v8 << ((sizeof(limb_t) - n + i) * 8);
                }
                a->tab[0] = v;
                i = 1;
            } else {
                i = 0;
            }
            for(; i < l; i++) {
#if LIMB_BITS == 32
                if (bc_get_u32(s, &v))
                    goto fail;
#ifdef WORDS_BIGENDIAN
                v = bswap32(v);
#endif
#else
                if (bc_get_u64(s, &v))
                    goto fail;
#ifdef WORDS_BIGENDIAN
                v = bswap64(v);
#endif
#endif
                a->tab[i] = v;
            }
        } else {
            bpos = 0;
            for(i = 0; i < l; i++) {
                if (i == 0 && (n = len % LIMB_DIGITS) != 0) {
                    j = LIMB_DIGITS - n;
                } else {
                    j = 0;
                }
                v = 0;
                for(; j < LIMB_DIGITS; j++) {
                    if (bpos == 0) {
                        if (bc_get_u8(s, &v8))
                            goto fail;
                        d = v8 & 0xf;
                        bpos = 1;
                    } else {
                        d = v8 >> 4;
                        bpos = 0;
                    }
                    if (d >= 10) {
                        JS_ThrowInternalError(s->ctx, "invalid digit");
                        goto fail;
                    }
                    v += mp_pow_dec[j] * d;
                }
                a->tab[i] = v;
            }
        }
    }
    bc_read_trace(s, "}\n");
    return obj;
 fail:
    JS_FreeValue(s->ctx, obj);
    return JS_EXCEPTION;
}
#endif /* CONFIG_BIGNUM */

static JSValue JS_ReadObjectRec(BCReaderState *s);

static int BC_add_object_ref1(BCReaderState *s, JSObject *p)
{
    if (s->allow_reference) {
        if (js_resize_array(s->ctx, (void *)&s->objects,
                            sizeof(s->objects[0]),
                            &s->objects_size, s->objects_count + 1))
            return -1;
        s->objects[s->objects_count++] = p;
    }
    return 0;
}

static int BC_add_object_ref(BCReaderState *s, JSValueConst obj)
{
    return BC_add_object_ref1(s, JS_VALUE_GET_OBJ(obj));
}

static JSValue JS_ReadFunctionTag(BCReaderState *s)
{
    JSContext *ctx = s->ctx;
    JSFunctionBytecode bc, *b;
    JSValue obj = JS_UNDEFINED;
    uint16_t v16;
    uint8_t v8;
    int idx, i, local_count;
    int function_size, cpool_offset, byte_code_offset;
    int closure_var_offset, vardefs_offset;

    memset(&bc, 0, sizeof(bc));
    bc.header.ref_count = 1;
    //bc.gc_header.mark = 0;

    if (bc_get_u16(s, &v16))
        goto fail;
    idx = 0;
    bc.has_prototype = bc_get_flags(v16, &idx, 1);
    bc.has_simple_parameter_list = bc_get_flags(v16, &idx, 1);
    bc.is_derived_class_constructor = bc_get_flags(v16, &idx, 1);
    bc.need_home_object = bc_get_flags(v16, &idx, 1);
    bc.func_kind = bc_get_flags(v16, &idx, 2);
    bc.new_target_allowed = bc_get_flags(v16, &idx, 1);
    bc.super_call_allowed = bc_get_flags(v16, &idx, 1);
    bc.super_allowed = bc_get_flags(v16, &idx, 1);
    bc.arguments_allowed = bc_get_flags(v16, &idx, 1);
    bc.has_debug = bc_get_flags(v16, &idx, 1);
    bc.backtrace_barrier = bc_get_flags(v16, &idx, 1);
    bc.read_only_bytecode = s->is_rom_data;
    if (bc_get_u8(s, &v8))
        goto fail;
    bc.js_mode = v8;
    if (bc_get_atom(s, &bc.func_name))  //@ atom leak if failure
        goto fail;
    if (bc_get_leb128_u16(s, &bc.arg_count))
        goto fail;
    if (bc_get_leb128_u16(s, &bc.var_count))
        goto fail;
    if (bc_get_leb128_u16(s, &bc.defined_arg_count))
        goto fail;
    if (bc_get_leb128_u16(s, &bc.stack_size))
        goto fail;
    if (bc_get_leb128_int(s, &bc.closure_var_count))
        goto fail;
    if (bc_get_leb128_int(s, &bc.cpool_count))
        goto fail;
    if (bc_get_leb128_int(s, &bc.byte_code_len))
        goto fail;
    if (bc_get_leb128_int(s, &local_count))
        goto fail;

    if (bc.has_debug) {
        function_size = sizeof(*b);
    } else {
        function_size = offsetof(JSFunctionBytecode, debug);
    }
    cpool_offset = function_size;
    function_size += bc.cpool_count * sizeof(*bc.cpool);
    vardefs_offset = function_size;
    function_size += local_count * sizeof(*bc.vardefs);
    closure_var_offset = function_size;
    function_size += bc.closure_var_count * sizeof(*bc.closure_var);
    byte_code_offset = function_size;
    if (!bc.read_only_bytecode) {
        function_size += bc.byte_code_len;
    }

    b = js_mallocz(ctx, function_size);
    if (!b)
        return JS_EXCEPTION;
            
    memcpy(b, &bc, offsetof(JSFunctionBytecode, debug));
    b->header.ref_count = 1;
    if (local_count != 0) {
        b->vardefs = (void *)((uint8_t*)b + vardefs_offset);
    }
    if (b->closure_var_count != 0) {
        b->closure_var = (void *)((uint8_t*)b + closure_var_offset);
    }
    if (b->cpool_count != 0) {
        b->cpool = (void *)((uint8_t*)b + cpool_offset);
    }
    
    add_gc_object(ctx->rt, &b->header, JS_GC_OBJ_TYPE_FUNCTION_BYTECODE);
            
    obj = JS_MKPTR(JS_TAG_FUNCTION_BYTECODE, b);

#ifdef DUMP_READ_OBJECT
    bc_read_trace(s, "name: "); print_atom(s->ctx, b->func_name); printf("\n");
#endif
    bc_read_trace(s, "args=%d vars=%d defargs=%d closures=%d cpool=%d\n",
                  b->arg_count, b->var_count, b->defined_arg_count,
                  b->closure_var_count, b->cpool_count);
    bc_read_trace(s, "stack=%d bclen=%d locals=%d\n",
                  b->stack_size, b->byte_code_len, local_count);

    if (local_count != 0) {
        bc_read_trace(s, "vars {\n");
        for(i = 0; i < local_count; i++) {
            JSVarDef *vd = &b->vardefs[i];
            if (bc_get_atom(s, &vd->var_name))
                goto fail;
            if (bc_get_leb128_int(s, &vd->scope_level))
                goto fail;
            if (bc_get_leb128_int(s, &vd->scope_next))
                goto fail;
            vd->scope_next--;
            if (bc_get_u8(s, &v8))
                goto fail;
            idx = 0;
            vd->var_kind = bc_get_flags(v8, &idx, 4);
            vd->is_const = bc_get_flags(v8, &idx, 1);
            vd->is_lexical = bc_get_flags(v8, &idx, 1);
            vd->is_captured = bc_get_flags(v8, &idx, 1);
#ifdef DUMP_READ_OBJECT
            bc_read_trace(s, "name: "); print_atom(s->ctx, vd->var_name); printf("\n");
#endif
        }
        bc_read_trace(s, "}\n");
    }
    if (b->closure_var_count != 0) {
        bc_read_trace(s, "closure vars {\n");
        for(i = 0; i < b->closure_var_count; i++) {
            JSClosureVar *cv = &b->closure_var[i];
            int var_idx;
            if (bc_get_atom(s, &cv->var_name))
                goto fail;
            if (bc_get_leb128_int(s, &var_idx))
                goto fail;
            cv->var_idx = var_idx;
            if (bc_get_u8(s, &v8))
                goto fail;
            idx = 0;
            cv->is_local = bc_get_flags(v8, &idx, 1);
            cv->is_arg = bc_get_flags(v8, &idx, 1);
            cv->is_const = bc_get_flags(v8, &idx, 1);
            cv->is_lexical = bc_get_flags(v8, &idx, 1);
            cv->var_kind = bc_get_flags(v8, &idx, 4);
#ifdef DUMP_READ_OBJECT
            bc_read_trace(s, "name: "); print_atom(s->ctx, cv->var_name); printf("\n");
#endif
        }
        bc_read_trace(s, "}\n");
    }
    {
        bc_read_trace(s, "bytecode {\n");
        if (JS_ReadFunctionBytecode(s, b, byte_code_offset, b->byte_code_len))
            goto fail;
        bc_read_trace(s, "}\n");
    }
    if (b->has_debug) {
        /* read optional debug information */
        bc_read_trace(s, "debug {\n");
        if (bc_get_atom(s, &b->debug.filename))
            goto fail;
        if (bc_get_leb128_int(s, &b->debug.line_num))
            goto fail;
        if (bc_get_leb128_int(s, &b->debug.pc2line_len))
            goto fail;
        if (b->debug.pc2line_len) {
            b->debug.pc2line_buf = js_mallocz(ctx, b->debug.pc2line_len);
            if (!b->debug.pc2line_buf)
                goto fail;
            if (bc_get_buf(s, b->debug.pc2line_buf, b->debug.pc2line_len))
                goto fail;
        }
#ifdef DUMP_READ_OBJECT
        bc_read_trace(s, "filename: "); print_atom(s->ctx, b->debug.filename); printf("\n");
#endif
        bc_read_trace(s, "}\n");
    }
    if (b->cpool_count != 0) {
        bc_read_trace(s, "cpool {\n");
        for(i = 0; i < b->cpool_count; i++) {
            JSValue val;
            val = JS_ReadObjectRec(s);
            if (JS_IsException(val))
                goto fail;
            b->cpool[i] = val;
        }
        bc_read_trace(s, "}\n");
    }
    b->realm = JS_DupContext(ctx);
    return obj;
 fail:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue JS_ReadModule(BCReaderState *s)
{
    JSContext *ctx = s->ctx;
    JSValue obj;
    JSModuleDef *m = NULL;
    JSAtom module_name;
    int i;
    uint8_t v8;
    
    if (bc_get_atom(s, &module_name))
        goto fail;
#ifdef DUMP_READ_OBJECT
    bc_read_trace(s, "name: "); print_atom(s->ctx, module_name); printf("\n");
#endif
    m = js_new_module_def(ctx, module_name);
    if (!m)
        goto fail;
    obj = JS_DupValue(ctx, JS_MKPTR(JS_TAG_MODULE, m));
    if (bc_get_leb128_int(s, &m->req_module_entries_count))
        goto fail;
    if (m->req_module_entries_count != 0) {
        m->req_module_entries_size = m->req_module_entries_count;
        m->req_module_entries = js_mallocz(ctx, sizeof(m->req_module_entries[0]) * m->req_module_entries_size);
        if (!m->req_module_entries)
            goto fail;
        for(i = 0; i < m->req_module_entries_count; i++) {
            JSReqModuleEntry *rme = &m->req_module_entries[i];
            if (bc_get_atom(s, &rme->module_name))
                goto fail;
        }
    }

    if (bc_get_leb128_int(s, &m->export_entries_count))
        goto fail;
    if (m->export_entries_count != 0) {
        m->export_entries_size = m->export_entries_count;
        m->export_entries = js_mallocz(ctx, sizeof(m->export_entries[0]) * m->export_entries_size);
        if (!m->export_entries)
            goto fail;
        for(i = 0; i < m->export_entries_count; i++) {
            JSExportEntry *me = &m->export_entries[i];
            if (bc_get_u8(s, &v8))
                goto fail;
            me->export_type = v8;
            if (me->export_type == JS_EXPORT_TYPE_LOCAL) {
                if (bc_get_leb128_int(s, &me->u.local.var_idx))
                    goto fail;
            } else {
                if (bc_get_leb128_int(s, &me->u.req_module_idx))
                    goto fail;
                if (bc_get_atom(s, &me->local_name))
                    goto fail;
            }
            if (bc_get_atom(s, &me->export_name))
                goto fail;
        }
    }

    if (bc_get_leb128_int(s, &m->star_export_entries_count))
        goto fail;
    if (m->star_export_entries_count != 0) {
        m->star_export_entries_size = m->star_export_entries_count;
        m->star_export_entries = js_mallocz(ctx, sizeof(m->star_export_entries[0]) * m->star_export_entries_size);
        if (!m->star_export_entries)
            goto fail;
        for(i = 0; i < m->star_export_entries_count; i++) {
            JSStarExportEntry *se = &m->star_export_entries[i];
            if (bc_get_leb128_int(s, &se->req_module_idx))
                goto fail;
        }
    }

    if (bc_get_leb128_int(s, &m->import_entries_count))
        goto fail;
    if (m->import_entries_count != 0) {
        m->import_entries_size = m->import_entries_count;
        m->import_entries = js_mallocz(ctx, sizeof(m->import_entries[0]) * m->import_entries_size);
        if (!m->import_entries)
            goto fail;
        for(i = 0; i < m->import_entries_count; i++) {
            JSImportEntry *mi = &m->import_entries[i];
            if (bc_get_leb128_int(s, &mi->var_idx))
                goto fail;
            if (bc_get_atom(s, &mi->import_name))
                goto fail;
            if (bc_get_leb128_int(s, &mi->req_module_idx))
                goto fail;
        }
    }

    m->func_obj = JS_ReadObjectRec(s);
    if (JS_IsException(m->func_obj))
        goto fail;
    return obj;
 fail:
    if (m) {
        js_free_module_def(ctx, m);
    }
    return JS_EXCEPTION;
}

static JSValue JS_ReadObjectTag(BCReaderState *s)
{
    JSContext *ctx = s->ctx;
    JSValue obj;
    uint32_t prop_count, i;
    JSAtom atom;
    JSValue val;
    int ret;
    
    obj = JS_NewObject(ctx);
    if (BC_add_object_ref(s, obj))
        goto fail;
    if (bc_get_leb128(s, &prop_count))
        goto fail;
    for(i = 0; i < prop_count; i++) {
        if (bc_get_atom(s, &atom))
            goto fail;
#ifdef DUMP_READ_OBJECT
        bc_read_trace(s, "propname: "); print_atom(s->ctx, atom); printf("\n");
#endif
        val = JS_ReadObjectRec(s);
        if (JS_IsException(val)) {
            JS_FreeAtom(ctx, atom);
            goto fail;
        }
        ret = JS_DefinePropertyValue(ctx, obj, atom, val, JS_PROP_C_W_E);
        JS_FreeAtom(ctx, atom);
        if (ret < 0)
            goto fail;
    }
    return obj;
 fail:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue JS_ReadArray(BCReaderState *s, int tag)
{
    JSContext *ctx = s->ctx;
    JSValue obj;
    uint32_t len, i;
    JSValue val;
    int ret, prop_flags;
    BOOL is_template;

    obj = JS_NewArray(ctx);
    if (BC_add_object_ref(s, obj))
        goto fail;
    is_template = (tag == BC_TAG_TEMPLATE_OBJECT);
    if (bc_get_leb128(s, &len))
        goto fail;
    for(i = 0; i < len; i++) {
        val = JS_ReadObjectRec(s);
        if (JS_IsException(val))
            goto fail;
        if (is_template)
            prop_flags = JS_PROP_ENUMERABLE;
        else
            prop_flags = JS_PROP_C_W_E;
        ret = JS_DefinePropertyValueUint32(ctx, obj, i, val,
                                           prop_flags);
        if (ret < 0)
            goto fail;
    }
    if (is_template) {
        val = JS_ReadObjectRec(s);
        if (JS_IsException(val))
            goto fail;
        if (!JS_IsUndefined(val)) {
            ret = JS_DefinePropertyValue(ctx, obj, JS_ATOM_raw, val, 0);
            if (ret < 0)
                goto fail;
        }
        JS_PreventExtensions(ctx, obj);
    }
    return obj;
 fail:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue JS_ReadTypedArray(BCReaderState *s)
{
    JSContext *ctx = s->ctx;
    JSValue obj = JS_UNDEFINED, array_buffer = JS_UNDEFINED;
    uint8_t array_tag;
    JSValueConst args[3];
    uint32_t offset, len, idx;
    
    if (bc_get_u8(s, &array_tag))
        return JS_EXCEPTION;
    if (array_tag >= JS_TYPED_ARRAY_COUNT)
        return JS_ThrowTypeError(ctx, "invalid typed array");
    if (bc_get_leb128(s, &len))
        return JS_EXCEPTION;
    if (bc_get_leb128(s, &offset))
        return JS_EXCEPTION;
    /* XXX: this hack could be avoided if the typed array could be
       created before the array buffer */
    idx = s->objects_count;
    if (BC_add_object_ref1(s, NULL))
        goto fail;
    array_buffer = JS_ReadObjectRec(s);
    if (JS_IsException(array_buffer))
        return JS_EXCEPTION;
    if (!js_get_array_buffer(ctx, array_buffer)) {
        JS_FreeValue(ctx, array_buffer);
        return JS_EXCEPTION;
    }
    args[0] = array_buffer;
    args[1] = JS_NewInt64(ctx, offset);
    args[2] = JS_NewInt64(ctx, len);
    obj = js_typed_array_constructor(ctx, JS_UNDEFINED,
                                     3, args,
                                     JS_CLASS_UINT8C_ARRAY + array_tag);
    if (JS_IsException(obj))
        goto fail;
    if (s->allow_reference) {
        s->objects[idx] = JS_VALUE_GET_OBJ(obj);
    }
    JS_FreeValue(ctx, array_buffer);
    return obj;
 fail:
    JS_FreeValue(ctx, array_buffer);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue JS_ReadArrayBuffer(BCReaderState *s)
{
    JSContext *ctx = s->ctx;
    uint32_t byte_length;
    JSValue obj;
    
    if (bc_get_leb128(s, &byte_length))
        return JS_EXCEPTION;
    if (unlikely(s->buf_end - s->ptr < byte_length)) {
        bc_read_error_end(s);
        return JS_EXCEPTION;
    }
    obj = JS_NewArrayBufferCopy(ctx, s->ptr, byte_length);
    if (JS_IsException(obj))
        goto fail;
    if (BC_add_object_ref(s, obj))
        goto fail;
    s->ptr += byte_length;
    return obj;
 fail:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue JS_ReadSharedArrayBuffer(BCReaderState *s)
{
    JSContext *ctx = s->ctx;
    uint32_t byte_length;
    uint8_t *data_ptr;
    JSValue obj;
    uint64_t u64;
    
    if (bc_get_leb128(s, &byte_length))
        return JS_EXCEPTION;
    if (bc_get_u64(s, &u64))
        return JS_EXCEPTION;
    data_ptr = (uint8_t *)(uintptr_t)u64;
    /* the SharedArrayBuffer is cloned */
    obj = js_array_buffer_constructor3(ctx, JS_UNDEFINED, byte_length,
                                       JS_CLASS_SHARED_ARRAY_BUFFER,
                                       data_ptr,
                                       NULL, NULL, FALSE);
    if (JS_IsException(obj))
        goto fail;
    if (BC_add_object_ref(s, obj))
        goto fail;
    return obj;
 fail:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue JS_ReadDate(BCReaderState *s)
{
    JSContext *ctx = s->ctx;
    JSValue val, obj = JS_UNDEFINED;

    val = JS_ReadObjectRec(s);
    if (JS_IsException(val))
        goto fail;
    if (!JS_IsNumber(val)) {
        JS_ThrowTypeError(ctx, "Number tag expected for date");
        goto fail;
    }
    obj = JS_NewObjectProtoClass(ctx, ctx->class_proto[JS_CLASS_DATE],
                                 JS_CLASS_DATE);
    if (JS_IsException(obj))
        goto fail;
    if (BC_add_object_ref(s, obj))
        goto fail;
    JS_SetObjectData(ctx, obj, val);
    return obj;
 fail:
    JS_FreeValue(ctx, val);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue JS_ReadObjectValue(BCReaderState *s)
{
    JSContext *ctx = s->ctx;
    JSValue val, obj = JS_UNDEFINED;

    val = JS_ReadObjectRec(s);
    if (JS_IsException(val))
        goto fail;
    obj = JS_ToObject(ctx, val);
    if (JS_IsException(obj))
        goto fail;
    if (BC_add_object_ref(s, obj))
        goto fail;
    JS_FreeValue(ctx, val);
    return obj;
 fail:
    JS_FreeValue(ctx, val);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue JS_ReadObjectRec(BCReaderState *s)
{
    JSContext *ctx = s->ctx;
    uint8_t tag;
    JSValue obj = JS_UNDEFINED;

    if (js_check_stack_overflow(ctx->rt, 0))
        return JS_ThrowStackOverflow(ctx);

    if (bc_get_u8(s, &tag))
        return JS_EXCEPTION;

    bc_read_trace(s, "%s {\n", bc_tag_str[tag]);

    switch(tag) {
    case BC_TAG_NULL:
        obj = JS_NULL;
        break;
    case BC_TAG_UNDEFINED:
        obj = JS_UNDEFINED;
        break;
    case BC_TAG_BOOL_FALSE:
    case BC_TAG_BOOL_TRUE:
        obj = JS_NewBool(ctx, tag - BC_TAG_BOOL_FALSE);
        break;
    case BC_TAG_INT32:
        {
            int32_t val;
            if (bc_get_sleb128(s, &val))
                return JS_EXCEPTION;
            bc_read_trace(s, "%d\n", val);
            obj = JS_NewInt32(ctx, val);
        }
        break;
    case BC_TAG_FLOAT64:
        {
            JSFloat64Union u;
            if (bc_get_u64(s, &u.u64))
                return JS_EXCEPTION;
            bc_read_trace(s, "%g\n", u.d);
            obj = __JS_NewFloat64(ctx, u.d);
        }
        break;
    case BC_TAG_STRING:
        {
            JSString *p;
            p = JS_ReadString(s);
            if (!p)
                return JS_EXCEPTION;
            obj = JS_MKPTR(JS_TAG_STRING, p);
        }
        break;
    case BC_TAG_FUNCTION_BYTECODE:
        if (!s->allow_bytecode)
            goto invalid_tag;
        obj = JS_ReadFunctionTag(s);
        break;
    case BC_TAG_MODULE:
        if (!s->allow_bytecode)
            goto invalid_tag;
        obj = JS_ReadModule(s);
        break;
    case BC_TAG_OBJECT:
        obj = JS_ReadObjectTag(s);
        break;
    case BC_TAG_ARRAY:
    case BC_TAG_TEMPLATE_OBJECT:
        obj = JS_ReadArray(s, tag);
        break;
    case BC_TAG_TYPED_ARRAY:
        obj = JS_ReadTypedArray(s);
        break;
    case BC_TAG_ARRAY_BUFFER:
        obj = JS_ReadArrayBuffer(s);
        break;
    case BC_TAG_SHARED_ARRAY_BUFFER:
        if (!s->allow_sab || !ctx->rt->sab_funcs.sab_dup)
            goto invalid_tag;
        obj = JS_ReadSharedArrayBuffer(s);
        break;
    case BC_TAG_DATE:
        obj = JS_ReadDate(s);
        break;
    case BC_TAG_OBJECT_VALUE:
        obj = JS_ReadObjectValue(s);
        break;
#ifdef CONFIG_BIGNUM
    case BC_TAG_BIG_INT:
    case BC_TAG_BIG_FLOAT:
    case BC_TAG_BIG_DECIMAL:
        obj = JS_ReadBigNum(s, tag);
        break;
#endif
    case BC_TAG_OBJECT_REFERENCE:
        {
            uint32_t val;
            if (!s->allow_reference)
                return JS_ThrowSyntaxError(ctx, "object references are not allowed");
            if (bc_get_leb128(s, &val))
                return JS_EXCEPTION;
            bc_read_trace(s, "%u\n", val);
            if (val >= s->objects_count) {
                return JS_ThrowSyntaxError(ctx, "invalid object reference (%u >= %u)",
                                           val, s->objects_count);
            }
            obj = JS_DupValue(ctx, JS_MKPTR(JS_TAG_OBJECT, s->objects[val]));
        }
        break;
    default:
    invalid_tag:
        return JS_ThrowSyntaxError(ctx, "invalid tag (tag=%d pos=%u)",
                                   tag, (unsigned int)(s->ptr - s->buf_start));
    }
    bc_read_trace(s, "}\n");
    return obj;
}

static int JS_ReadObjectAtoms(BCReaderState *s)
{
    uint8_t v8;
    JSString *p;
    int i;
    JSAtom atom;

    if (bc_get_u8(s, &v8))
        return -1;
    /* XXX: could support byte swapped input */
    if (v8 != BC_VERSION) {
        JS_ThrowSyntaxError(s->ctx, "invalid version (%d expected=%d)",
                            v8, BC_VERSION);
        return -1;
    }
    if (bc_get_leb128(s, &s->idx_to_atom_count))
        return -1;

    bc_read_trace(s, "%d atom indexes {\n", s->idx_to_atom_count);

    if (s->idx_to_atom_count != 0) {
        s->idx_to_atom = js_mallocz(s->ctx, s->idx_to_atom_count *
                                    sizeof(s->idx_to_atom[0]));
        if (!s->idx_to_atom)
            return s->error_state = -1;
    }
    for(i = 0; i < s->idx_to_atom_count; i++) {
        p = JS_ReadString(s);
        if (!p)
            return -1;
        atom = JS_NewAtomStr(s->ctx, p);
        if (atom == JS_ATOM_NULL)
            return s->error_state = -1;
        s->idx_to_atom[i] = atom;
        if (s->is_rom_data && (atom != (i + s->first_atom)))
            s->is_rom_data = FALSE; /* atoms must be relocated */
    }
    bc_read_trace(s, "}\n");
    return 0;
}

static void bc_reader_free(BCReaderState *s)
{
    int i;
    if (s->idx_to_atom) {
        for(i = 0; i < s->idx_to_atom_count; i++) {
            JS_FreeAtom(s->ctx, s->idx_to_atom[i]);
        }
        js_free(s->ctx, s->idx_to_atom);
    }
    js_free(s->ctx, s->objects);
}

JSValue JS_ReadObject(JSContext *ctx, const uint8_t *buf, size_t buf_len,
                       int flags)
{
    BCReaderState ss, *s = &ss;
    JSValue obj;

    ctx->binary_object_count += 1;
    ctx->binary_object_size += buf_len;

    memset(s, 0, sizeof(*s));
    s->ctx = ctx;
    s->buf_start = buf;
    s->buf_end = buf + buf_len;
    s->ptr = buf;
    s->allow_bytecode = ((flags & JS_READ_OBJ_BYTECODE) != 0);
    s->is_rom_data = ((flags & JS_READ_OBJ_ROM_DATA) != 0);
    s->allow_sab = ((flags & JS_READ_OBJ_SAB) != 0);
    s->allow_reference = ((flags & JS_READ_OBJ_REFERENCE) != 0);
    if (s->allow_bytecode)
        s->first_atom = JS_ATOM_END;
    else
        s->first_atom = 1;
    if (JS_ReadObjectAtoms(s)) {
        obj = JS_EXCEPTION;
    } else {
        obj = JS_ReadObjectRec(s);
    }
    bc_reader_free(s);
    return obj;
}

/*******************************************************************/
/* runtime functions & objects */

static JSValue js_string_constructor(JSContext *ctx, JSValueConst this_val,
                                     int argc, JSValueConst *argv);
static JSValue js_boolean_constructor(JSContext *ctx, JSValueConst this_val,
                                      int argc, JSValueConst *argv);
static JSValue js_number_constructor(JSContext *ctx, JSValueConst this_val,
                                     int argc, JSValueConst *argv);

static int check_function(JSContext *ctx, JSValueConst obj)
{
    if (likely(JS_IsFunction(ctx, obj)))
        return 0;
    JS_ThrowTypeError(ctx, "not a function");
    return -1;
}

static int check_exception_free(JSContext *ctx, JSValue obj)
{
    JS_FreeValue(ctx, obj);
    return JS_IsException(obj);
}

static JSAtom find_atom(JSContext *ctx, const char *name)
{
    JSAtom atom;
    int len;

    if (*name == '[') {
        name++;
        len = strlen(name) - 1;
        /* We assume 8 bit non null strings, which is the case for these
           symbols */
        for(atom = JS_ATOM_Symbol_toPrimitive; atom < JS_ATOM_END; atom++) {
            JSAtomStruct *p = ctx->rt->atom_array[atom];
            JSString *str = p;
            if (str->len == len && !memcmp(str->u.str8, name, len))
                return JS_DupAtom(ctx, atom);
        }
        abort();
    } else {
        atom = JS_NewAtom(ctx, name);
    }
    return atom;
}

static JSValue JS_InstantiateFunctionListItem2(JSContext *ctx, JSObject *p,
                                               JSAtom atom, void *opaque)
{
    const JSCFunctionListEntry *e = opaque;
    JSValue val;

    switch(e->def_type) {
    case JS_DEF_CFUNC:
        val = JS_NewCFunction2(ctx, e->u.func.cfunc.generic,
                               e->name, e->u.func.length, e->u.func.cproto, e->magic);
        break;
    case JS_DEF_PROP_STRING:
        val = JS_NewAtomString(ctx, e->u.str);
        break;
    case JS_DEF_OBJECT:
        val = JS_NewObject(ctx);
        JS_SetPropertyFunctionList(ctx, val, e->u.prop_list.tab, e->u.prop_list.len);
        break;
    default:
        abort();
    }
    return val;
}

static int JS_InstantiateFunctionListItem(JSContext *ctx, JSValueConst obj,
                                          JSAtom atom,
                                          const JSCFunctionListEntry *e)
{
    JSValue val;
    int prop_flags = e->prop_flags;

    switch(e->def_type) {
    case JS_DEF_ALIAS: /* using autoinit for aliases is not safe */
        {
            JSAtom atom1 = find_atom(ctx, e->u.alias.name);
            switch (e->u.alias.base) {
            case -1:
                val = JS_GetProperty(ctx, obj, atom1);
                break;
            case 0:
                val = JS_GetProperty(ctx, ctx->global_obj, atom1);
                break;
            case 1:
                val = JS_GetProperty(ctx, ctx->class_proto[JS_CLASS_ARRAY], atom1);
                break;
            default:
                abort();
            }
            JS_FreeAtom(ctx, atom1);
            if (atom == JS_ATOM_Symbol_toPrimitive) {
                /* Symbol.toPrimitive functions are not writable */
                prop_flags = JS_PROP_CONFIGURABLE;
            } else if (atom == JS_ATOM_Symbol_hasInstance) {
                /* Function.prototype[Symbol.hasInstance] is not writable nor configurable */
                prop_flags = 0;
            }
        }
        break;
    case JS_DEF_CFUNC:
        if (atom == JS_ATOM_Symbol_toPrimitive) {
            /* Symbol.toPrimitive functions are not writable */
            prop_flags = JS_PROP_CONFIGURABLE;
        } else if (atom == JS_ATOM_Symbol_hasInstance) {
            /* Function.prototype[Symbol.hasInstance] is not writable nor configurable */
            prop_flags = 0;
        }
        JS_DefineAutoInitProperty(ctx, obj, atom, JS_AUTOINIT_ID_PROP,
                                  (void *)e, prop_flags);
        return 0;
    case JS_DEF_CGETSET: /* XXX: use autoinit again ? */
    case JS_DEF_CGETSET_MAGIC:
        {
            JSValue getter, setter;
            char buf[64];

            getter = JS_UNDEFINED;
            if (e->u.getset.get.generic) {
                snprintf(buf, sizeof(buf), "get %s", e->name);
                getter = JS_NewCFunction2(ctx, e->u.getset.get.generic,
                                          buf, 0, e->def_type == JS_DEF_CGETSET_MAGIC ? JS_CFUNC_getter_magic : JS_CFUNC_getter,
                                          e->magic);
            }
            setter = JS_UNDEFINED;
            if (e->u.getset.set.generic) {
                snprintf(buf, sizeof(buf), "set %s", e->name);
                setter = JS_NewCFunction2(ctx, e->u.getset.set.generic,
                                          buf, 1, e->def_type == JS_DEF_CGETSET_MAGIC ? JS_CFUNC_setter_magic : JS_CFUNC_setter,
                                          e->magic);
            }
            JS_DefinePropertyGetSet(ctx, obj, atom, getter, setter, prop_flags);
            return 0;
        }
        break;
    case JS_DEF_PROP_INT32:
        val = JS_NewInt32(ctx, e->u.i32);
        break;
    case JS_DEF_PROP_INT64:
        val = JS_NewInt64(ctx, e->u.i64);
        break;
    case JS_DEF_PROP_DOUBLE:
        val = __JS_NewFloat64(ctx, e->u.f64);
        break;
    case JS_DEF_PROP_UNDEFINED:
        val = JS_UNDEFINED;
        break;
    case JS_DEF_PROP_STRING:
    case JS_DEF_OBJECT:
        JS_DefineAutoInitProperty(ctx, obj, atom, JS_AUTOINIT_ID_PROP,
                                  (void *)e, prop_flags);
        return 0;
    default:
        abort();
    }
    JS_DefinePropertyValue(ctx, obj, atom, val, prop_flags);
    return 0;
}

void JS_SetPropertyFunctionList(JSContext *ctx, JSValueConst obj,
                                const JSCFunctionListEntry *tab, int len)
{
    int i;

    for (i = 0; i < len; i++) {
        const JSCFunctionListEntry *e = &tab[i];
        JSAtom atom = find_atom(ctx, e->name);
        JS_InstantiateFunctionListItem(ctx, obj, atom, e);
        JS_FreeAtom(ctx, atom);
    }
}

int JS_AddModuleExportList(JSContext *ctx, JSModuleDef *m,
                           const JSCFunctionListEntry *tab, int len)
{
    int i;
    for(i = 0; i < len; i++) {
        if (JS_AddModuleExport(ctx, m, tab[i].name))
            return -1;
    }
    return 0;
}

int JS_SetModuleExportList(JSContext *ctx, JSModuleDef *m,
                           const JSCFunctionListEntry *tab, int len)
{
    int i;
    JSValue val;

    for(i = 0; i < len; i++) {
        const JSCFunctionListEntry *e = &tab[i];
        switch(e->def_type) {
        case JS_DEF_CFUNC:
            val = JS_NewCFunction2(ctx, e->u.func.cfunc.generic,
                                   e->name, e->u.func.length, e->u.func.cproto, e->magic);
            break;
        case JS_DEF_PROP_STRING:
            val = JS_NewString(ctx, e->u.str);
            break;
        case JS_DEF_PROP_INT32:
            val = JS_NewInt32(ctx, e->u.i32);
            break;
        case JS_DEF_PROP_INT64:
            val = JS_NewInt64(ctx, e->u.i64);
            break;
        case JS_DEF_PROP_DOUBLE:
            val = __JS_NewFloat64(ctx, e->u.f64);
            break;
        case JS_DEF_OBJECT:
            val = JS_NewObject(ctx);
            JS_SetPropertyFunctionList(ctx, val, e->u.prop_list.tab, e->u.prop_list.len);
            break;
        default:
            abort();
        }
        if (JS_SetModuleExport(ctx, m, e->name, val))
            return -1;
    }
    return 0;
}

/* Note: 'func_obj' is not necessarily a constructor */
static void JS_SetConstructor2(JSContext *ctx,
                               JSValueConst func_obj,
                               JSValueConst proto,
                               int proto_flags, int ctor_flags)
{
    JS_DefinePropertyValue(ctx, func_obj, JS_ATOM_prototype,
                           JS_DupValue(ctx, proto), proto_flags);
    JS_DefinePropertyValue(ctx, proto, JS_ATOM_constructor,
                           JS_DupValue(ctx, func_obj),
                           ctor_flags);
    set_cycle_flag(ctx, func_obj);
    set_cycle_flag(ctx, proto);
}

void JS_SetConstructor(JSContext *ctx, JSValueConst func_obj, 
                       JSValueConst proto)
{
    JS_SetConstructor2(ctx, func_obj, proto,
                       0, JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
}

static void JS_NewGlobalCConstructor2(JSContext *ctx,
                                      JSValue func_obj,
                                      const char *name,
                                      JSValueConst proto)
{
    JS_DefinePropertyValueStr(ctx, ctx->global_obj, name,
                           JS_DupValue(ctx, func_obj),
                           JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    JS_SetConstructor(ctx, func_obj, proto);
    JS_FreeValue(ctx, func_obj);
}

static JSValueConst JS_NewGlobalCConstructor(JSContext *ctx, const char *name,
                                             JSCFunction *func, int length,
                                             JSValueConst proto)
{
    JSValue func_obj;
    func_obj = JS_NewCFunction2(ctx, func, name, length, JS_CFUNC_constructor_or_func, 0);
    JS_NewGlobalCConstructor2(ctx, func_obj, name, proto);
    return func_obj;
}

static JSValueConst JS_NewGlobalCConstructorOnly(JSContext *ctx, const char *name,
                                                 JSCFunction *func, int length,
                                                 JSValueConst proto)
{
    JSValue func_obj;
    func_obj = JS_NewCFunction2(ctx, func, name, length, JS_CFUNC_constructor, 0);
    JS_NewGlobalCConstructor2(ctx, func_obj, name, proto);
    return func_obj;
}

static JSValue js_global_eval(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv)
{
    return JS_EvalObject(ctx, ctx->global_obj, argv[0], JS_EVAL_TYPE_INDIRECT, -1);
}

static JSValue js_global_isNaN(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv)
{
    double d;

    /* XXX: does this work for bigfloat? */
    if (unlikely(JS_ToFloat64(ctx, &d, argv[0])))
        return JS_EXCEPTION;
    return JS_NewBool(ctx, isnan(d));
}

static JSValue js_global_isFinite(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv)
{
    BOOL res;
    double d;
    if (unlikely(JS_ToFloat64(ctx, &d, argv[0])))
        return JS_EXCEPTION;
    res = isfinite(d);
    return JS_NewBool(ctx, res);
}

/* Object class */

static JSValue JS_ToObject(JSContext *ctx, JSValueConst val)
{
    int tag = JS_VALUE_GET_NORM_TAG(val);
    JSValue obj;

    switch(tag) {
    default:
    case JS_TAG_NULL:
    case JS_TAG_UNDEFINED:
        return JS_ThrowTypeError(ctx, "cannot convert to object");
    case JS_TAG_OBJECT:
    case JS_TAG_EXCEPTION:
        return JS_DupValue(ctx, val);
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
        obj = JS_NewObjectClass(ctx, JS_CLASS_BIG_INT);
        goto set_value;
    case JS_TAG_BIG_FLOAT:
        obj = JS_NewObjectClass(ctx, JS_CLASS_BIG_FLOAT);
        goto set_value;
    case JS_TAG_BIG_DECIMAL:
        obj = JS_NewObjectClass(ctx, JS_CLASS_BIG_DECIMAL);
        goto set_value;
#endif
    case JS_TAG_INT:
    case JS_TAG_FLOAT64:
        obj = JS_NewObjectClass(ctx, JS_CLASS_NUMBER);
        goto set_value;
    case JS_TAG_STRING:
        /* XXX: should call the string constructor */
        {
            JSString *p1 = JS_VALUE_GET_STRING(val);
            obj = JS_NewObjectClass(ctx, JS_CLASS_STRING);
            JS_DefinePropertyValue(ctx, obj, JS_ATOM_length, JS_NewInt32(ctx, p1->len), 0);
        }
        goto set_value;
    case JS_TAG_BOOL:
        obj = JS_NewObjectClass(ctx, JS_CLASS_BOOLEAN);
        goto set_value;
    case JS_TAG_SYMBOL:
        obj = JS_NewObjectClass(ctx, JS_CLASS_SYMBOL);
    set_value:
        if (!JS_IsException(obj))
            JS_SetObjectData(ctx, obj, JS_DupValue(ctx, val));
        return obj;
    }
}

static JSValue JS_ToObjectFree(JSContext *ctx, JSValue val)
{
    JSValue obj = JS_ToObject(ctx, val);
    JS_FreeValue(ctx, val);
    return obj;
}

static int js_obj_to_desc(JSContext *ctx, JSPropertyDescriptor *d,
                          JSValueConst desc)
{
    JSValue val, getter, setter;
    int flags;

    if (!JS_IsObject(desc)) {
        JS_ThrowTypeErrorNotAnObject(ctx);
        return -1;
    }
    flags = 0;
    val = JS_UNDEFINED;
    getter = JS_UNDEFINED;
    setter = JS_UNDEFINED;
    if (JS_HasProperty(ctx, desc, JS_ATOM_configurable)) {
        JSValue prop = JS_GetProperty(ctx, desc, JS_ATOM_configurable);
        if (JS_IsException(prop))
            goto fail;
        flags |= JS_PROP_HAS_CONFIGURABLE;
        if (JS_ToBoolFree(ctx, prop))
            flags |= JS_PROP_CONFIGURABLE;
    }
    if (JS_HasProperty(ctx, desc, JS_ATOM_writable)) {
        JSValue prop = JS_GetProperty(ctx, desc, JS_ATOM_writable);
        if (JS_IsException(prop))
            goto fail;
        flags |= JS_PROP_HAS_WRITABLE;
        if (JS_ToBoolFree(ctx, prop))
            flags |= JS_PROP_WRITABLE;
    }
    if (JS_HasProperty(ctx, desc, JS_ATOM_enumerable)) {
        JSValue prop = JS_GetProperty(ctx, desc, JS_ATOM_enumerable);
        if (JS_IsException(prop))
            goto fail;
        flags |= JS_PROP_HAS_ENUMERABLE;
        if (JS_ToBoolFree(ctx, prop))
            flags |= JS_PROP_ENUMERABLE;
    }
    if (JS_HasProperty(ctx, desc, JS_ATOM_value)) {
        flags |= JS_PROP_HAS_VALUE;
        val = JS_GetProperty(ctx, desc, JS_ATOM_value);
        if (JS_IsException(val))
            goto fail;
    }
    if (JS_HasProperty(ctx, desc, JS_ATOM_get)) {
        flags |= JS_PROP_HAS_GET;
        getter = JS_GetProperty(ctx, desc, JS_ATOM_get);
        if (JS_IsException(getter) ||
            !(JS_IsUndefined(getter) || JS_IsFunction(ctx, getter))) {
            JS_ThrowTypeError(ctx, "invalid getter");
            goto fail;
        }
    }
    if (JS_HasProperty(ctx, desc, JS_ATOM_set)) {
        flags |= JS_PROP_HAS_SET;
        setter = JS_GetProperty(ctx, desc, JS_ATOM_set);
        if (JS_IsException(setter) ||
            !(JS_IsUndefined(setter) || JS_IsFunction(ctx, setter))) {
            JS_ThrowTypeError(ctx, "invalid setter");
            goto fail;
        }
    }
    if ((flags & (JS_PROP_HAS_SET | JS_PROP_HAS_GET)) &&
        (flags & (JS_PROP_HAS_VALUE | JS_PROP_HAS_WRITABLE))) {
        JS_ThrowTypeError(ctx, "cannot have setter/getter and value or writable");
        goto fail;
    }
    d->flags = flags;
    d->value = val;
    d->getter = getter;
    d->setter = setter;
    return 0;
 fail:
    JS_FreeValue(ctx, val);
    JS_FreeValue(ctx, getter);
    JS_FreeValue(ctx, setter);
    return -1;
}

static __exception int JS_DefinePropertyDesc(JSContext *ctx, JSValueConst obj,
                                             JSAtom prop, JSValueConst desc,
                                             int flags)
{
    JSPropertyDescriptor d;
    int ret;

    if (js_obj_to_desc(ctx, &d, desc) < 0)
        return -1;

    ret = JS_DefineProperty(ctx, obj, prop,
                            d.value, d.getter, d.setter, d.flags | flags);
    js_free_desc(ctx, &d);
    return ret;
}

static __exception int JS_ObjectDefineProperties(JSContext *ctx,
                                                 JSValueConst obj,
                                                 JSValueConst properties)
{
    JSValue props, desc;
    JSObject *p;
    JSPropertyEnum *atoms;
    uint32_t len, i;
    int ret = -1;

    if (!JS_IsObject(obj)) {
        JS_ThrowTypeErrorNotAnObject(ctx);
        return -1;
    }
    desc = JS_UNDEFINED;
    props = JS_ToObject(ctx, properties);
    if (JS_IsException(props))
        return -1;
    p = JS_VALUE_GET_OBJ(props);
    if (JS_GetOwnPropertyNamesInternal(ctx, &atoms, &len, p, JS_GPN_ENUM_ONLY | JS_GPN_STRING_MASK | JS_GPN_SYMBOL_MASK) < 0)
        goto exception;
    for(i = 0; i < len; i++) {
        JS_FreeValue(ctx, desc);
        desc = JS_GetProperty(ctx, props, atoms[i].atom);
        if (JS_IsException(desc))
            goto exception;
        if (JS_DefinePropertyDesc(ctx, obj, atoms[i].atom, desc, JS_PROP_THROW) < 0)
            goto exception;
    }
    ret = 0;

exception:
    js_free_prop_enum(ctx, atoms, len);
    JS_FreeValue(ctx, props);
    JS_FreeValue(ctx, desc);
    return ret;
}

static JSValue js_object_constructor(JSContext *ctx, JSValueConst new_target,
                                     int argc, JSValueConst *argv)
{
    JSValue ret;
    if (!JS_IsUndefined(new_target) &&
        JS_VALUE_GET_OBJ(new_target) !=
        JS_VALUE_GET_OBJ(JS_GetActiveFunction(ctx))) {
        ret = js_create_from_ctor(ctx, new_target, JS_CLASS_OBJECT);
    } else {
        int tag = JS_VALUE_GET_NORM_TAG(argv[0]);
        switch(tag) {
        case JS_TAG_NULL:
        case JS_TAG_UNDEFINED:
            ret = JS_NewObject(ctx);
            break;
        default:
            ret = JS_ToObject(ctx, argv[0]);
            break;
        }
    }
    return ret;
}

static JSValue js_object_create(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    JSValueConst proto, props;
    JSValue obj;

    proto = argv[0];
    if (!JS_IsObject(proto) && !JS_IsNull(proto))
        return JS_ThrowTypeError(ctx, "not a prototype");
    obj = JS_NewObjectProto(ctx, proto);
    if (JS_IsException(obj))
        return JS_EXCEPTION;
    props = argv[1];
    if (!JS_IsUndefined(props)) {
        if (JS_ObjectDefineProperties(ctx, obj, props)) {
            JS_FreeValue(ctx, obj);
            return JS_EXCEPTION;
        }
    }
    return obj;
}

static JSValue js_object_getPrototypeOf(JSContext *ctx, JSValueConst this_val,
                                        int argc, JSValueConst *argv, int magic)
{
    JSValueConst val;

    val = argv[0];
    if (JS_VALUE_GET_TAG(val) != JS_TAG_OBJECT) {
        /* ES6 feature non compatible with ES5.1: primitive types are
           accepted */
        if (magic || JS_VALUE_GET_TAG(val) == JS_TAG_NULL ||
            JS_VALUE_GET_TAG(val) == JS_TAG_UNDEFINED)
            return JS_ThrowTypeErrorNotAnObject(ctx);
    }
    return JS_GetPrototype(ctx, val);
}

static JSValue js_object_setPrototypeOf(JSContext *ctx, JSValueConst this_val,
                                        int argc, JSValueConst *argv)
{
    JSValueConst obj;
    obj = argv[0];
    if (JS_SetPrototypeInternal(ctx, obj, argv[1], TRUE) < 0)
        return JS_EXCEPTION;
    return JS_DupValue(ctx, obj);
}

/* magic = 1 if called as Reflect.defineProperty */
static JSValue js_object_defineProperty(JSContext *ctx, JSValueConst this_val,
                                        int argc, JSValueConst *argv, int magic)
{
    JSValueConst obj, prop, desc;
    int ret, flags;
    JSAtom atom;

    obj = argv[0];
    prop = argv[1];
    desc = argv[2];

    if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)
        return JS_ThrowTypeErrorNotAnObject(ctx);
    atom = JS_ValueToAtom(ctx, prop);
    if (unlikely(atom == JS_ATOM_NULL))
        return JS_EXCEPTION;
    flags = 0;
    if (!magic)
        flags |= JS_PROP_THROW;
    ret = JS_DefinePropertyDesc(ctx, obj, atom, desc, flags);
    JS_FreeAtom(ctx, atom);
    if (ret < 0) {
        return JS_EXCEPTION;
    } else if (magic) {
        return JS_NewBool(ctx, ret);
    } else {
        return JS_DupValue(ctx, obj);
    }
}

static JSValue js_object_defineProperties(JSContext *ctx, JSValueConst this_val,
                                          int argc, JSValueConst *argv)
{
    // defineProperties(obj, properties)
    JSValueConst obj = argv[0];

    if (JS_ObjectDefineProperties(ctx, obj, argv[1]))
        return JS_EXCEPTION;
    else
        return JS_DupValue(ctx, obj);
}

/* magic = 1 if called as __defineSetter__ */
static JSValue js_object___defineGetter__(JSContext *ctx, JSValueConst this_val,
                                          int argc, JSValueConst *argv, int magic)
{
    JSValue obj;
    JSValueConst prop, value, get, set;
    int ret, flags;
    JSAtom atom;

    prop = argv[0];
    value = argv[1];

    obj = JS_ToObject(ctx, this_val);
    if (JS_IsException(obj))
        return JS_EXCEPTION;

    if (check_function(ctx, value)) {
        JS_FreeValue(ctx, obj);
        return JS_EXCEPTION;
    }
    atom = JS_ValueToAtom(ctx, prop);
    if (unlikely(atom == JS_ATOM_NULL)) {
        JS_FreeValue(ctx, obj);
        return JS_EXCEPTION;
    }
    flags = JS_PROP_THROW |
        JS_PROP_HAS_ENUMERABLE | JS_PROP_ENUMERABLE |
        JS_PROP_HAS_CONFIGURABLE | JS_PROP_CONFIGURABLE;
    if (magic) {
        get = JS_UNDEFINED;
        set = value;
        flags |= JS_PROP_HAS_SET;
    } else {
        get = value;
        set = JS_UNDEFINED;
        flags |= JS_PROP_HAS_GET;
    }
    ret = JS_DefineProperty(ctx, obj, atom, JS_UNDEFINED, get, set, flags);
    JS_FreeValue(ctx, obj);
    JS_FreeAtom(ctx, atom);
    if (ret < 0) {
        return JS_EXCEPTION;
    } else {
        return JS_UNDEFINED;
    }
}

static JSValue js_object_getOwnPropertyDescriptor(JSContext *ctx, JSValueConst this_val,
                                                  int argc, JSValueConst *argv, int magic)
{
    JSValueConst prop;
    JSAtom atom;
    JSValue ret, obj;
    JSPropertyDescriptor desc;
    int res, flags;

    if (magic) {
        /* Reflect.getOwnPropertyDescriptor case */
        if (JS_VALUE_GET_TAG(argv[0]) != JS_TAG_OBJECT)
            return JS_ThrowTypeErrorNotAnObject(ctx);
        obj = JS_DupValue(ctx, argv[0]);
    } else {
        obj = JS_ToObject(ctx, argv[0]);
        if (JS_IsException(obj))
            return obj;
    }
    prop = argv[1];
    atom = JS_ValueToAtom(ctx, prop);
    if (unlikely(atom == JS_ATOM_NULL))
        goto exception;
    ret = JS_UNDEFINED;
    if (JS_VALUE_GET_TAG(obj) == JS_TAG_OBJECT) {
        res = JS_GetOwnPropertyInternal(ctx, &desc, JS_VALUE_GET_OBJ(obj), atom);
        if (res < 0)
            goto exception;
        if (res) {
            ret = JS_NewObject(ctx);
            if (JS_IsException(ret))
                goto exception1;
            flags = JS_PROP_C_W_E | JS_PROP_THROW;
            if (desc.flags & JS_PROP_GETSET) {
                if (JS_DefinePropertyValue(ctx, ret, JS_ATOM_get, JS_DupValue(ctx, desc.getter), flags) < 0
                ||  JS_DefinePropertyValue(ctx, ret, JS_ATOM_set, JS_DupValue(ctx, desc.setter), flags) < 0)
                    goto exception1;
            } else {
                if (JS_DefinePropertyValue(ctx, ret, JS_ATOM_value, JS_DupValue(ctx, desc.value), flags) < 0
                ||  JS_DefinePropertyValue(ctx, ret, JS_ATOM_writable,
                                           JS_NewBool(ctx, (desc.flags & JS_PROP_WRITABLE) != 0), flags) < 0)
                    goto exception1;
            }
            if (JS_DefinePropertyValue(ctx, ret, JS_ATOM_enumerable,
                                       JS_NewBool(ctx, (desc.flags & JS_PROP_ENUMERABLE) != 0), flags) < 0
            ||  JS_DefinePropertyValue(ctx, ret, JS_ATOM_configurable,
                                       JS_NewBool(ctx, (desc.flags & JS_PROP_CONFIGURABLE) != 0), flags) < 0)
                goto exception1;
            js_free_desc(ctx, &desc);
        }
    }
    JS_FreeAtom(ctx, atom);
    JS_FreeValue(ctx, obj);
    return ret;

exception1:
    js_free_desc(ctx, &desc);
    JS_FreeValue(ctx, ret);
exception:
    JS_FreeAtom(ctx, atom);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_object_getOwnPropertyDescriptors(JSContext *ctx, JSValueConst this_val,
                                                   int argc, JSValueConst *argv)
{
    //getOwnPropertyDescriptors(obj)
    JSValue obj, r;
    JSObject *p;
    JSPropertyEnum *props;
    uint32_t len, i;

    r = JS_UNDEFINED;
    obj = JS_ToObject(ctx, argv[0]);
    if (JS_IsException(obj))
        return JS_EXCEPTION;

    p = JS_VALUE_GET_OBJ(obj);
    if (JS_GetOwnPropertyNamesInternal(ctx, &props, &len, p,
                               JS_GPN_STRING_MASK | JS_GPN_SYMBOL_MASK))
        goto exception;
    r = JS_NewObject(ctx);
    if (JS_IsException(r))
        goto exception;
    for(i = 0; i < len; i++) {
        JSValue atomValue, desc;
        JSValueConst args[2];

        atomValue = JS_AtomToValue(ctx, props[i].atom);
        if (JS_IsException(atomValue))
            goto exception;
        args[0] = obj;
        args[1] = atomValue;
        desc = js_object_getOwnPropertyDescriptor(ctx, JS_UNDEFINED, 2, args, 0);
        JS_FreeValue(ctx, atomValue);
        if (JS_IsException(desc))
            goto exception;
        if (!JS_IsUndefined(desc)) {
            if (JS_DefinePropertyValue(ctx, r, props[i].atom, desc,
                                       JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                goto exception;
        }
    }
    js_free_prop_enum(ctx, props, len);
    JS_FreeValue(ctx, obj);
    return r;

exception:
    js_free_prop_enum(ctx, props, len);
    JS_FreeValue(ctx, obj);
    JS_FreeValue(ctx, r);
    return JS_EXCEPTION;
}

static JSValue JS_GetOwnPropertyNames2(JSContext *ctx, JSValueConst obj1,
                                       int flags, int kind)
{
    JSValue obj, r, val, key, value;
    JSObject *p;
    JSPropertyEnum *atoms;
    uint32_t len, i, j;

    r = JS_UNDEFINED;
    val = JS_UNDEFINED;
    obj = JS_ToObject(ctx, obj1);
    if (JS_IsException(obj))
        return JS_EXCEPTION;
    p = JS_VALUE_GET_OBJ(obj);
    if (JS_GetOwnPropertyNamesInternal(ctx, &atoms, &len, p, flags & ~JS_GPN_ENUM_ONLY))
        goto exception;
    r = JS_NewArray(ctx);
    if (JS_IsException(r))
        goto exception;
    for(j = i = 0; i < len; i++) {
        JSAtom atom = atoms[i].atom;
        if (flags & JS_GPN_ENUM_ONLY) {
            JSPropertyDescriptor desc;
            int res;

            /* Check if property is still enumerable */
            res = JS_GetOwnPropertyInternal(ctx, &desc, p, atom);
            if (res < 0)
                goto exception;
            if (!res)
                continue;
            js_free_desc(ctx, &desc);
            if (!(desc.flags & JS_PROP_ENUMERABLE))
                continue;
        }
        switch(kind) {
        default:
        case JS_ITERATOR_KIND_KEY:
            val = JS_AtomToValue(ctx, atom);
            if (JS_IsException(val))
                goto exception;
            break;
        case JS_ITERATOR_KIND_VALUE:
            val = JS_GetProperty(ctx, obj, atom);
            if (JS_IsException(val))
                goto exception;
            break;
        case JS_ITERATOR_KIND_KEY_AND_VALUE:
            val = JS_NewArray(ctx);
            if (JS_IsException(val))
                goto exception;
            key = JS_AtomToValue(ctx, atom);
            if (JS_IsException(key))
                goto exception1;
            if (JS_CreateDataPropertyUint32(ctx, val, 0, key, JS_PROP_THROW) < 0)
                goto exception1;
            value = JS_GetProperty(ctx, obj, atom);
            if (JS_IsException(value))
                goto exception1;
            if (JS_CreateDataPropertyUint32(ctx, val, 1, value, JS_PROP_THROW) < 0)
                goto exception1;
            break;
        }
        if (JS_CreateDataPropertyUint32(ctx, r, j++, val, 0) < 0)
            goto exception;
    }
    goto done;

exception1:
    JS_FreeValue(ctx, val);
exception:
    JS_FreeValue(ctx, r);
    r = JS_EXCEPTION;
done:
    js_free_prop_enum(ctx, atoms, len);
    JS_FreeValue(ctx, obj);
    return r;
}

static JSValue js_object_getOwnPropertyNames(JSContext *ctx, JSValueConst this_val,
                                             int argc, JSValueConst *argv)
{
    return JS_GetOwnPropertyNames2(ctx, argv[0],
                                   JS_GPN_STRING_MASK, JS_ITERATOR_KIND_KEY);
}

static JSValue js_object_getOwnPropertySymbols(JSContext *ctx, JSValueConst this_val,
                                             int argc, JSValueConst *argv)
{
    return JS_GetOwnPropertyNames2(ctx, argv[0],
                                   JS_GPN_SYMBOL_MASK, JS_ITERATOR_KIND_KEY);
}

static JSValue js_object_keys(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv, int kind)
{
    return JS_GetOwnPropertyNames2(ctx, argv[0],
                                   JS_GPN_ENUM_ONLY | JS_GPN_STRING_MASK, kind);
}

static JSValue js_object_isExtensible(JSContext *ctx, JSValueConst this_val,
                                      int argc, JSValueConst *argv, int reflect)
{
    JSValueConst obj;
    int ret;

    obj = argv[0];
    if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT) {
        if (reflect)
            return JS_ThrowTypeErrorNotAnObject(ctx);
        else
            return JS_FALSE;
    }
    ret = JS_IsExtensible(ctx, obj);
    if (ret < 0)
        return JS_EXCEPTION;
    else
        return JS_NewBool(ctx, ret);
}

static JSValue js_object_preventExtensions(JSContext *ctx, JSValueConst this_val,
                                           int argc, JSValueConst *argv, int reflect)
{
    JSValueConst obj;
    int ret;

    obj = argv[0];
    if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT) {
        if (reflect)
            return JS_ThrowTypeErrorNotAnObject(ctx);
        else
            return JS_DupValue(ctx, obj);
    }
    ret = JS_PreventExtensions(ctx, obj);
    if (ret < 0)
        return JS_EXCEPTION;
    if (reflect) {
        return JS_NewBool(ctx, ret);
    } else {
        if (!ret)
            return JS_ThrowTypeError(ctx, "proxy preventExtensions handler returned false");
        return JS_DupValue(ctx, obj);
    }
}

static JSValue js_object_hasOwnProperty(JSContext *ctx, JSValueConst this_val,
                                        int argc, JSValueConst *argv)
{
    JSValue obj;
    JSAtom atom;
    JSObject *p;
    BOOL ret;

    atom = JS_ValueToAtom(ctx, argv[0]); /* must be done first */
    if (unlikely(atom == JS_ATOM_NULL))
        return JS_EXCEPTION;
    obj = JS_ToObject(ctx, this_val);
    if (JS_IsException(obj)) {
        JS_FreeAtom(ctx, atom);
        return obj;
    }
    p = JS_VALUE_GET_OBJ(obj);
    ret = JS_GetOwnPropertyInternal(ctx, NULL, p, atom);
    JS_FreeAtom(ctx, atom);
    JS_FreeValue(ctx, obj);
    if (ret < 0)
        return JS_EXCEPTION;
    else
        return JS_NewBool(ctx, ret);
}

static JSValue js_object_valueOf(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    return JS_ToObject(ctx, this_val);
}

static JSValue js_object_toString(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv)
{
    JSValue obj, tag;
    int is_array;
    JSAtom atom;
    JSObject *p;

    if (JS_IsNull(this_val)) {
        tag = JS_NewString(ctx, "Null");
    } else if (JS_IsUndefined(this_val)) {
        tag = JS_NewString(ctx, "Undefined");
    } else {
        obj = JS_ToObject(ctx, this_val);
        if (JS_IsException(obj))
            return obj;
        is_array = JS_IsArray(ctx, obj);
        if (is_array < 0) {
            JS_FreeValue(ctx, obj);
            return JS_EXCEPTION;
        }
        if (is_array) {
            atom = JS_ATOM_Array;
        } else if (JS_IsFunction(ctx, obj)) {
            atom = JS_ATOM_Function;
        } else {
            p = JS_VALUE_GET_OBJ(obj);
            switch(p->class_id) {
            case JS_CLASS_STRING:
            case JS_CLASS_ARGUMENTS:
            case JS_CLASS_MAPPED_ARGUMENTS:
            case JS_CLASS_ERROR:
            case JS_CLASS_BOOLEAN:
            case JS_CLASS_NUMBER:
            case JS_CLASS_DATE:
            case JS_CLASS_REGEXP:
                atom = ctx->rt->class_array[p->class_id].class_name;
                break;
            default:
                atom = JS_ATOM_Object;
                break;
            }
        }
        tag = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_toStringTag);
        JS_FreeValue(ctx, obj);
        if (JS_IsException(tag))
            return JS_EXCEPTION;
        if (!JS_IsString(tag)) {
            JS_FreeValue(ctx, tag);
            tag = JS_AtomToString(ctx, atom);
        }
    }
    return JS_ConcatString3(ctx, "[object ", tag, "]");
}

static JSValue js_object_toLocaleString(JSContext *ctx, JSValueConst this_val,
                                        int argc, JSValueConst *argv)
{
    return JS_Invoke(ctx, this_val, JS_ATOM_toString, 0, NULL);
}

static JSValue js_object_assign(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    // Object.assign(obj, source1)
    JSValue obj, s;
    int i;

    s = JS_UNDEFINED;
    obj = JS_ToObject(ctx, argv[0]);
    if (JS_IsException(obj))
        goto exception;
    for (i = 1; i < argc; i++) {
        if (!JS_IsNull(argv[i]) && !JS_IsUndefined(argv[i])) {
            s = JS_ToObject(ctx, argv[i]);
            if (JS_IsException(s))
                goto exception;
            if (JS_CopyDataProperties(ctx, obj, s, JS_UNDEFINED, TRUE))
                goto exception;
            JS_FreeValue(ctx, s);
        }
    }
    return obj;
exception:
    JS_FreeValue(ctx, obj);
    JS_FreeValue(ctx, s);
    return JS_EXCEPTION;
}

static JSValue js_object_seal(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv, int freeze_flag)
{
    JSValueConst obj = argv[0];
    JSObject *p;
    JSPropertyEnum *props;
    uint32_t len, i;
    int flags, desc_flags, res;

    if (!JS_IsObject(obj))
        return JS_DupValue(ctx, obj);

    res = JS_PreventExtensions(ctx, obj);
    if (res < 0)
        return JS_EXCEPTION;
    if (!res) {
        return JS_ThrowTypeError(ctx, "proxy preventExtensions handler returned false");
    }
    
    p = JS_VALUE_GET_OBJ(obj);
    flags = JS_GPN_STRING_MASK | JS_GPN_SYMBOL_MASK;
    if (JS_GetOwnPropertyNamesInternal(ctx, &props, &len, p, flags))
        return JS_EXCEPTION;

    for(i = 0; i < len; i++) {
        JSPropertyDescriptor desc;
        JSAtom prop = props[i].atom;

        desc_flags = JS_PROP_THROW | JS_PROP_HAS_CONFIGURABLE;
        if (freeze_flag) {
            res = JS_GetOwnPropertyInternal(ctx, &desc, p, prop);
            if (res < 0)
                goto exception;
            if (res) {
                if (desc.flags & JS_PROP_WRITABLE)
                    desc_flags |= JS_PROP_HAS_WRITABLE;
                js_free_desc(ctx, &desc);
            }
        }
        if (JS_DefineProperty(ctx, obj, prop, JS_UNDEFINED,
                              JS_UNDEFINED, JS_UNDEFINED, desc_flags) < 0)
            goto exception;
    }
    js_free_prop_enum(ctx, props, len);
    return JS_DupValue(ctx, obj);

 exception:
    js_free_prop_enum(ctx, props, len);
    return JS_EXCEPTION;
}

static JSValue js_object_isSealed(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv, int is_frozen)
{
    JSValueConst obj = argv[0];
    JSObject *p;
    JSPropertyEnum *props;
    uint32_t len, i;
    int flags, res;
    
    if (!JS_IsObject(obj))
        return JS_TRUE;

    p = JS_VALUE_GET_OBJ(obj);
    flags = JS_GPN_STRING_MASK | JS_GPN_SYMBOL_MASK;
    if (JS_GetOwnPropertyNamesInternal(ctx, &props, &len, p, flags))
        return JS_EXCEPTION;

    for(i = 0; i < len; i++) {
        JSPropertyDescriptor desc;
        JSAtom prop = props[i].atom;

        res = JS_GetOwnPropertyInternal(ctx, &desc, p, prop);
        if (res < 0)
            goto exception;
        if (res) {
            js_free_desc(ctx, &desc);
            if ((desc.flags & JS_PROP_CONFIGURABLE)
            ||  (is_frozen && (desc.flags & JS_PROP_WRITABLE))) {
                res = FALSE;
                goto done;
            }
        }
    }
    res = JS_IsExtensible(ctx, obj);
    if (res < 0)
        return JS_EXCEPTION;
    res ^= 1;
done:        
    js_free_prop_enum(ctx, props, len);
    return JS_NewBool(ctx, res);

exception:
    js_free_prop_enum(ctx, props, len);
    return JS_EXCEPTION;
}

static JSValue js_object_fromEntries(JSContext *ctx, JSValueConst this_val,
                                     int argc, JSValueConst *argv)
{
    JSValue obj, iter, next_method = JS_UNDEFINED;
    JSValueConst iterable;
    BOOL done;

    /*  RequireObjectCoercible() not necessary because it is tested in
        JS_GetIterator() by JS_GetProperty() */
    iterable = argv[0];

    obj = JS_NewObject(ctx);
    if (JS_IsException(obj))
        return obj;
    
    iter = JS_GetIterator(ctx, iterable, FALSE);
    if (JS_IsException(iter))
        goto fail;
    next_method = JS_GetProperty(ctx, iter, JS_ATOM_next);
    if (JS_IsException(next_method))
        goto fail;
    
    for(;;) {
        JSValue key, value, item;
        item = JS_IteratorNext(ctx, iter, next_method, 0, NULL, &done);
        if (JS_IsException(item))
            goto fail;
        if (done) {
            JS_FreeValue(ctx, item);
            break;
        }
        
        key = JS_UNDEFINED;
        value = JS_UNDEFINED;
        if (!JS_IsObject(item)) {
            JS_ThrowTypeErrorNotAnObject(ctx);
            goto fail1;
        }
        key = JS_GetPropertyUint32(ctx, item, 0);
        if (JS_IsException(key))
            goto fail1;
        value = JS_GetPropertyUint32(ctx, item, 1);
        if (JS_IsException(value)) {
            JS_FreeValue(ctx, key);
            goto fail1;
        }
        if (JS_DefinePropertyValueValue(ctx, obj, key, value,
                                        JS_PROP_C_W_E | JS_PROP_THROW) < 0) {
        fail1:
            JS_FreeValue(ctx, item);
            goto fail;
        }
        JS_FreeValue(ctx, item);
    }
    JS_FreeValue(ctx, next_method);
    JS_FreeValue(ctx, iter);
    return obj;
 fail:
    if (JS_IsObject(iter)) {
        /* close the iterator object, preserving pending exception */
        JS_IteratorClose(ctx, iter, TRUE);
    }
    JS_FreeValue(ctx, next_method);
    JS_FreeValue(ctx, iter);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

#if 0
/* Note: corresponds to ECMA spec: CreateDataPropertyOrThrow() */
static JSValue js_object___setOwnProperty(JSContext *ctx, JSValueConst this_val,
                                          int argc, JSValueConst *argv)
{
    int ret;
    ret = JS_DefinePropertyValueValue(ctx, argv[0], JS_DupValue(ctx, argv[1]),
                                      JS_DupValue(ctx, argv[2]),
                                      JS_PROP_C_W_E | JS_PROP_THROW);
    if (ret < 0)
        return JS_EXCEPTION;
    else
        return JS_NewBool(ctx, ret);
}

static JSValue js_object___toObject(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv)
{
    return JS_ToObject(ctx, argv[0]);
}

static JSValue js_object___toPrimitive(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv)
{
    int hint = HINT_NONE;

    if (JS_VALUE_GET_TAG(argv[1]) == JS_TAG_INT)
        hint = JS_VALUE_GET_INT(argv[1]);

    return JS_ToPrimitive(ctx, argv[0], hint);
}
#endif

/* return an empty string if not an object */
static JSValue js_object___getClass(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv)
{
    JSAtom atom;
    JSObject *p;
    uint32_t tag;
    int class_id;

    tag = JS_VALUE_GET_NORM_TAG(argv[0]);
    if (tag == JS_TAG_OBJECT) {
        p = JS_VALUE_GET_OBJ(argv[0]);
        class_id = p->class_id;
        if (class_id == JS_CLASS_PROXY && JS_IsFunction(ctx, argv[0]))
            class_id = JS_CLASS_BYTECODE_FUNCTION;
        atom = ctx->rt->class_array[class_id].class_name;
    } else {
        atom = JS_ATOM_empty_string;
    }
    return JS_AtomToString(ctx, atom);
}

static JSValue js_object_is(JSContext *ctx, JSValueConst this_val,
                            int argc, JSValueConst *argv)
{
    return JS_NewBool(ctx, js_same_value(ctx, argv[0], argv[1]));
}

#if 0
static JSValue js_object___getObjectData(JSContext *ctx, JSValueConst this_val,
                                         int argc, JSValueConst *argv)
{
    return JS_GetObjectData(ctx, argv[0]);
}

static JSValue js_object___setObjectData(JSContext *ctx, JSValueConst this_val,
                                         int argc, JSValueConst *argv)
{
    if (JS_SetObjectData(ctx, argv[0], JS_DupValue(ctx, argv[1])))
        return JS_EXCEPTION;
    return JS_DupValue(ctx, argv[1]);
}

static JSValue js_object___toPropertyKey(JSContext *ctx, JSValueConst this_val,
                                         int argc, JSValueConst *argv)
{
    return JS_ToPropertyKey(ctx, argv[0]);
}

static JSValue js_object___isObject(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv)
{
    return JS_NewBool(ctx, JS_IsObject(argv[0]));
}

static JSValue js_object___isSameValueZero(JSContext *ctx, JSValueConst this_val,
                                           int argc, JSValueConst *argv)
{
    return JS_NewBool(ctx, js_same_value_zero(ctx, argv[0], argv[1]));
}

static JSValue js_object___isConstructor(JSContext *ctx, JSValueConst this_val,
                                         int argc, JSValueConst *argv)
{
    return JS_NewBool(ctx, JS_IsConstructor(ctx, argv[0]));
}
#endif

static JSValue JS_SpeciesConstructor(JSContext *ctx, JSValueConst obj,
                                     JSValueConst defaultConstructor)
{
    JSValue ctor, species;

    if (!JS_IsObject(obj))
        return JS_ThrowTypeErrorNotAnObject(ctx);
    ctor = JS_GetProperty(ctx, obj, JS_ATOM_constructor);
    if (JS_IsException(ctor))
        return ctor;
    if (JS_IsUndefined(ctor))
        return JS_DupValue(ctx, defaultConstructor);
    if (!JS_IsObject(ctor)) {
        JS_FreeValue(ctx, ctor);
        return JS_ThrowTypeErrorNotAnObject(ctx);
    }
    species = JS_GetProperty(ctx, ctor, JS_ATOM_Symbol_species);
    JS_FreeValue(ctx, ctor);
    if (JS_IsException(species))
        return species;
    if (JS_IsUndefined(species) || JS_IsNull(species))
        return JS_DupValue(ctx, defaultConstructor);
    if (!JS_IsConstructor(ctx, species)) {
        JS_FreeValue(ctx, species);
        return JS_ThrowTypeError(ctx, "not a constructor");
    }
    return species;
}

#if 0
static JSValue js_object___speciesConstructor(JSContext *ctx, JSValueConst this_val,
                                              int argc, JSValueConst *argv)
{
    return JS_SpeciesConstructor(ctx, argv[0], argv[1]);
}
#endif

static JSValue js_object_get___proto__(JSContext *ctx, JSValueConst this_val)
{
    JSValue val, ret;

    val = JS_ToObject(ctx, this_val);
    if (JS_IsException(val))
        return val;
    ret = JS_GetPrototype(ctx, val);
    JS_FreeValue(ctx, val);
    return ret;
}

static JSValue js_object_set___proto__(JSContext *ctx, JSValueConst this_val,
                                       JSValueConst proto)
{
    if (JS_IsUndefined(this_val) || JS_IsNull(this_val))
        return JS_ThrowTypeErrorNotAnObject(ctx);
    if (!JS_IsObject(proto) && !JS_IsNull(proto))
        return JS_UNDEFINED;
    if (JS_SetPrototypeInternal(ctx, this_val, proto, TRUE) < 0)
        return JS_EXCEPTION;
    else
        return JS_UNDEFINED;
}

static JSValue js_object_isPrototypeOf(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv)
{
    JSValue obj, v1;
    JSValueConst v;
    int res;

    v = argv[0];
    if (!JS_IsObject(v))
        return JS_FALSE;
    obj = JS_ToObject(ctx, this_val);
    if (JS_IsException(obj))
        return JS_EXCEPTION;
    v1 = JS_DupValue(ctx, v);
    for(;;) {
        v1 = JS_GetPrototypeFree(ctx, v1);
        if (JS_IsException(v1))
            goto exception;
        if (JS_IsNull(v1)) {
            res = FALSE;
            break;
        }
        if (JS_VALUE_GET_OBJ(obj) == JS_VALUE_GET_OBJ(v1)) {
            res = TRUE;
            break;
        }
        /* avoid infinite loop (possible with proxies) */
        if (js_poll_interrupts(ctx))
            goto exception;
    }
    JS_FreeValue(ctx, v1);
    JS_FreeValue(ctx, obj);
    return JS_NewBool(ctx, res);

exception:
    JS_FreeValue(ctx, v1);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_object_propertyIsEnumerable(JSContext *ctx, JSValueConst this_val,
                                              int argc, JSValueConst *argv)
{
    JSValue obj, res = JS_EXCEPTION;
    JSAtom prop = JS_ATOM_NULL;
    JSPropertyDescriptor desc;
    int has_prop;

    obj = JS_ToObject(ctx, this_val);
    if (JS_IsException(obj))
        goto exception;
    prop = JS_ValueToAtom(ctx, argv[0]);
    if (unlikely(prop == JS_ATOM_NULL))
        goto exception;

    has_prop = JS_GetOwnPropertyInternal(ctx, &desc, JS_VALUE_GET_OBJ(obj), prop);
    if (has_prop < 0)
        goto exception;
    if (has_prop) {
        res = JS_NewBool(ctx, (desc.flags & JS_PROP_ENUMERABLE) != 0);
        js_free_desc(ctx, &desc);
    } else {
        res = JS_FALSE;
    }

exception:
    JS_FreeAtom(ctx, prop);
    JS_FreeValue(ctx, obj);
    return res;
}

static JSValue js_object___lookupGetter__(JSContext *ctx, JSValueConst this_val,
                                          int argc, JSValueConst *argv, int setter)
{
    JSValue obj, res = JS_EXCEPTION;
    JSAtom prop = JS_ATOM_NULL;
    JSPropertyDescriptor desc;
    int has_prop;

    obj = JS_ToObject(ctx, this_val);
    if (JS_IsException(obj))
        goto exception;
    prop = JS_ValueToAtom(ctx, argv[0]);
    if (unlikely(prop == JS_ATOM_NULL))
        goto exception;

    for (;;) {
        has_prop = JS_GetOwnPropertyInternal(ctx, &desc, JS_VALUE_GET_OBJ(obj), prop);
        if (has_prop < 0)
            goto exception;
        if (has_prop) {
            if (desc.flags & JS_PROP_GETSET)
                res = JS_DupValue(ctx, setter ? desc.setter : desc.getter);
            else
                res = JS_UNDEFINED;
            js_free_desc(ctx, &desc);
            break;
        }
        obj = JS_GetPrototypeFree(ctx, obj);
        if (JS_IsException(obj))
            goto exception;
        if (JS_IsNull(obj)) {
            res = JS_UNDEFINED;
            break;
        }
        /* avoid infinite loop (possible with proxies) */
        if (js_poll_interrupts(ctx))
            goto exception;
    }

exception:
    JS_FreeAtom(ctx, prop);
    JS_FreeValue(ctx, obj);
    return res;
}

static const JSCFunctionListEntry js_object_funcs[] = {
    JS_CFUNC_DEF("create", 2, js_object_create ),
    JS_CFUNC_MAGIC_DEF("getPrototypeOf", 1, js_object_getPrototypeOf, 0 ),
    JS_CFUNC_DEF("setPrototypeOf", 2, js_object_setPrototypeOf ),
    JS_CFUNC_MAGIC_DEF("defineProperty", 3, js_object_defineProperty, 0 ),
    JS_CFUNC_DEF("defineProperties", 2, js_object_defineProperties ),
    JS_CFUNC_DEF("getOwnPropertyNames", 1, js_object_getOwnPropertyNames ),
    JS_CFUNC_DEF("getOwnPropertySymbols", 1, js_object_getOwnPropertySymbols ),
    JS_CFUNC_MAGIC_DEF("keys", 1, js_object_keys, JS_ITERATOR_KIND_KEY ),
    JS_CFUNC_MAGIC_DEF("values", 1, js_object_keys, JS_ITERATOR_KIND_VALUE ),
    JS_CFUNC_MAGIC_DEF("entries", 1, js_object_keys, JS_ITERATOR_KIND_KEY_AND_VALUE ),
    JS_CFUNC_MAGIC_DEF("isExtensible", 1, js_object_isExtensible, 0 ),
    JS_CFUNC_MAGIC_DEF("preventExtensions", 1, js_object_preventExtensions, 0 ),
    JS_CFUNC_MAGIC_DEF("getOwnPropertyDescriptor", 2, js_object_getOwnPropertyDescriptor, 0 ),
    JS_CFUNC_DEF("getOwnPropertyDescriptors", 1, js_object_getOwnPropertyDescriptors ),
    JS_CFUNC_DEF("is", 2, js_object_is ),
    JS_CFUNC_DEF("assign", 2, js_object_assign ),
    JS_CFUNC_MAGIC_DEF("seal", 1, js_object_seal, 0 ),
    JS_CFUNC_MAGIC_DEF("freeze", 1, js_object_seal, 1 ),
    JS_CFUNC_MAGIC_DEF("isSealed", 1, js_object_isSealed, 0 ),
    JS_CFUNC_MAGIC_DEF("isFrozen", 1, js_object_isSealed, 1 ),
    JS_CFUNC_DEF("__getClass", 1, js_object___getClass ),
    //JS_CFUNC_DEF("__isObject", 1, js_object___isObject ),
    //JS_CFUNC_DEF("__isConstructor", 1, js_object___isConstructor ),
    //JS_CFUNC_DEF("__toObject", 1, js_object___toObject ),
    //JS_CFUNC_DEF("__setOwnProperty", 3, js_object___setOwnProperty ),
    //JS_CFUNC_DEF("__toPrimitive", 2, js_object___toPrimitive ),
    //JS_CFUNC_DEF("__toPropertyKey", 1, js_object___toPropertyKey ),
    //JS_CFUNC_DEF("__speciesConstructor", 2, js_object___speciesConstructor ),
    //JS_CFUNC_DEF("__isSameValueZero", 2, js_object___isSameValueZero ),
    //JS_CFUNC_DEF("__getObjectData", 1, js_object___getObjectData ),
    //JS_CFUNC_DEF("__setObjectData", 2, js_object___setObjectData ),
    JS_CFUNC_DEF("fromEntries", 1, js_object_fromEntries ),
};

static const JSCFunctionListEntry js_object_proto_funcs[] = {
    JS_CFUNC_DEF("toString", 0, js_object_toString ),
    JS_CFUNC_DEF("toLocaleString", 0, js_object_toLocaleString ),
    JS_CFUNC_DEF("valueOf", 0, js_object_valueOf ),
    JS_CFUNC_DEF("hasOwnProperty", 1, js_object_hasOwnProperty ),
    JS_CFUNC_DEF("isPrototypeOf", 1, js_object_isPrototypeOf ),
    JS_CFUNC_DEF("propertyIsEnumerable", 1, js_object_propertyIsEnumerable ),
    JS_CGETSET_DEF("__proto__", js_object_get___proto__, js_object_set___proto__ ),
    JS_CFUNC_MAGIC_DEF("__defineGetter__", 2, js_object___defineGetter__, 0 ),
    JS_CFUNC_MAGIC_DEF("__defineSetter__", 2, js_object___defineGetter__, 1 ),
    JS_CFUNC_MAGIC_DEF("__lookupGetter__", 1, js_object___lookupGetter__, 0 ),
    JS_CFUNC_MAGIC_DEF("__lookupSetter__", 1, js_object___lookupGetter__, 1 ),
};

/* Function class */

static JSValue js_function_proto(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    return JS_UNDEFINED;
}

/* XXX: add a specific eval mode so that Function("}), ({") is rejected */
static JSValue js_function_constructor(JSContext *ctx, JSValueConst new_target,
                                       int argc, JSValueConst *argv, int magic)
{
    JSFunctionKindEnum func_kind = magic;
    int i, n, ret;
    JSValue s, proto, obj = JS_UNDEFINED;
    StringBuffer b_s, *b = &b_s;

    string_buffer_init(ctx, b, 0);
    string_buffer_putc8(b, '(');
    
    if (func_kind == JS_FUNC_ASYNC || func_kind == JS_FUNC_ASYNC_GENERATOR) {
        string_buffer_puts8(b, "async ");
    }
    string_buffer_puts8(b, "function");

    if (func_kind == JS_FUNC_GENERATOR || func_kind == JS_FUNC_ASYNC_GENERATOR) {
        string_buffer_putc8(b, '*');
    }
    string_buffer_puts8(b, " anonymous(");

    n = argc - 1;
    for(i = 0; i < n; i++) {
        if (i != 0) {
            string_buffer_putc8(b, ',');
        }
        if (string_buffer_concat_value(b, argv[i]))
            goto fail;
    }
    string_buffer_puts8(b, "\n) {\n");
    if (n >= 0) {
        if (string_buffer_concat_value(b, argv[n]))
            goto fail;
    }
    string_buffer_puts8(b, "\n})");
    s = string_buffer_end(b);
    if (JS_IsException(s))
        goto fail1;

    obj = JS_EvalObject(ctx, ctx->global_obj, s, JS_EVAL_TYPE_INDIRECT, -1);
    JS_FreeValue(ctx, s);
    if (JS_IsException(obj))
        goto fail1;
    if (!JS_IsUndefined(new_target)) {
        /* set the prototype */
        proto = JS_GetProperty(ctx, new_target, JS_ATOM_prototype);
        if (JS_IsException(proto))
            goto fail1;
        if (!JS_IsObject(proto)) {
            JSContext *realm;
            JS_FreeValue(ctx, proto);
            realm = JS_GetFunctionRealm(ctx, new_target);
            if (!realm)
                goto fail1;
            proto = JS_DupValue(ctx, realm->class_proto[func_kind_to_class_id[func_kind]]);
        }
        ret = JS_SetPrototypeInternal(ctx, obj, proto, TRUE);
        JS_FreeValue(ctx, proto);
        if (ret < 0)
            goto fail1;
    }
    return obj;

 fail:
    string_buffer_free(b);
 fail1:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static __exception int js_get_length32(JSContext *ctx, uint32_t *pres,
                                       JSValueConst obj)
{
    JSValue len_val;
    len_val = JS_GetProperty(ctx, obj, JS_ATOM_length);
    if (JS_IsException(len_val)) {
        *pres = 0;
        return -1;
    }
    return JS_ToUint32Free(ctx, pres, len_val);
}

static __exception int js_get_length64(JSContext *ctx, int64_t *pres,
                                       JSValueConst obj)
{
    JSValue len_val;
    len_val = JS_GetProperty(ctx, obj, JS_ATOM_length);
    if (JS_IsException(len_val)) {
        *pres = 0;
        return -1;
    }
    return JS_ToLengthFree(ctx, pres, len_val);
}

static void free_arg_list(JSContext *ctx, JSValue *tab, uint32_t len)
{
    uint32_t i;
    for(i = 0; i < len; i++) {
        JS_FreeValue(ctx, tab[i]);
    }
    js_free(ctx, tab);
}

/* XXX: should use ValueArray */
static JSValue *build_arg_list(JSContext *ctx, uint32_t *plen,
                               JSValueConst array_arg)
{
    uint32_t len, i;
    JSValue *tab, ret;
    JSObject *p;

    if (JS_VALUE_GET_TAG(array_arg) != JS_TAG_OBJECT) {
        JS_ThrowTypeError(ctx, "not a object");
        return NULL;
    }
    if (js_get_length32(ctx, &len, array_arg))
        return NULL;
    if (len > JS_MAX_LOCAL_VARS) {
        JS_ThrowInternalError(ctx, "too many arguments");
        return NULL;
    }
    /* avoid allocating 0 bytes */
    tab = js_mallocz(ctx, sizeof(tab[0]) * max_uint32(1, len));
    if (!tab)
        return NULL;
    p = JS_VALUE_GET_OBJ(array_arg);
    if ((p->class_id == JS_CLASS_ARRAY || p->class_id == JS_CLASS_ARGUMENTS) &&
        p->fast_array &&
        len == p->u.array.count) {
        for(i = 0; i < len; i++) {
            tab[i] = JS_DupValue(ctx, p->u.array.u.values[i]);
        }
    } else {
        for(i = 0; i < len; i++) {
            ret = JS_GetPropertyUint32(ctx, array_arg, i);
            if (JS_IsException(ret)) {
                free_arg_list(ctx, tab, i);
                return NULL;
            }
            tab[i] = ret;
        }
    }
    *plen = len;
    return tab;
}

/* magic value: 0 = normal apply, 1 = apply for constructor, 2 =
   Reflect.apply */
static JSValue js_function_apply(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv, int magic)
{
    JSValueConst this_arg, array_arg;
    uint32_t len;
    JSValue *tab, ret;

    if (check_function(ctx, this_val))
        return JS_EXCEPTION;
    this_arg = argv[0];
    array_arg = argv[1];
    if ((JS_VALUE_GET_TAG(array_arg) == JS_TAG_UNDEFINED ||
         JS_VALUE_GET_TAG(array_arg) == JS_TAG_NULL) && magic != 2) {
        return JS_Call(ctx, this_val, this_arg, 0, NULL);
    }
    tab = build_arg_list(ctx, &len, array_arg);
    if (!tab)
        return JS_EXCEPTION;
    if (magic & 1) {
        ret = JS_CallConstructor2(ctx, this_val, this_arg, len, (JSValueConst *)tab);
    } else {
        ret = JS_Call(ctx, this_val, this_arg, len, (JSValueConst *)tab);
    }
    free_arg_list(ctx, tab, len);
    return ret;
}

static JSValue js_function_call(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    if (argc <= 0) {
        return JS_Call(ctx, this_val, JS_UNDEFINED, 0, NULL);
    } else {
        return JS_Call(ctx, this_val, argv[0], argc - 1, argv + 1);
    }
}

static JSValue js_function_bind(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    JSBoundFunction *bf;
    JSValue func_obj, name1, len_val;
    JSObject *p;
    int arg_count, i, ret;

    if (check_function(ctx, this_val))
        return JS_EXCEPTION;

    func_obj = JS_NewObjectProtoClass(ctx, ctx->function_proto,
                                 JS_CLASS_BOUND_FUNCTION);
    if (JS_IsException(func_obj))
        return JS_EXCEPTION;
    p = JS_VALUE_GET_OBJ(func_obj);
    p->is_constructor = JS_IsConstructor(ctx, this_val);
    arg_count = max_int(0, argc - 1);
    bf = js_malloc(ctx, sizeof(*bf) + arg_count * sizeof(JSValue));
    if (!bf)
        goto exception;
    bf->func_obj = JS_DupValue(ctx, this_val);
    bf->this_val = JS_DupValue(ctx, argv[0]);
    bf->argc = arg_count;
    for(i = 0; i < arg_count; i++) {
        bf->argv[i] = JS_DupValue(ctx, argv[i + 1]);
    }
    p->u.bound_function = bf;

    /* XXX: the spec could be simpler by only using GetOwnProperty */
    ret = JS_GetOwnProperty(ctx, NULL, this_val, JS_ATOM_length);
    if (ret < 0)
        goto exception;
    if (!ret) {
        len_val = JS_NewInt32(ctx, 0);
    } else {
        len_val = JS_GetProperty(ctx, this_val, JS_ATOM_length);
        if (JS_IsException(len_val))
            goto exception;
        if (JS_VALUE_GET_TAG(len_val) == JS_TAG_INT) {
            /* most common case */
            int len1 = JS_VALUE_GET_INT(len_val);
            if (len1 <= arg_count)
                len1 = 0;
            else
                len1 -= arg_count;
            len_val = JS_NewInt32(ctx, len1);
        } else if (JS_VALUE_GET_NORM_TAG(len_val) == JS_TAG_FLOAT64) {
            double d = JS_VALUE_GET_FLOAT64(len_val);
            if (isnan(d)) {
                d = 0.0;
            } else {
                d = trunc(d);
                if (d <= (double)arg_count)
                    d = 0.0;
                else
                    d -= (double)arg_count; /* also converts -0 to +0 */
            }
            len_val = JS_NewFloat64(ctx, d);
        } else {
            JS_FreeValue(ctx, len_val);
            len_val = JS_NewInt32(ctx, 0);
        }
    }
    JS_DefinePropertyValue(ctx, func_obj, JS_ATOM_length,
                           len_val, JS_PROP_CONFIGURABLE);

    name1 = JS_GetProperty(ctx, this_val, JS_ATOM_name);
    if (JS_IsException(name1))
        goto exception;
    if (!JS_IsString(name1)) {
        JS_FreeValue(ctx, name1);
        name1 = JS_AtomToString(ctx, JS_ATOM_empty_string);
    }
    name1 = JS_ConcatString3(ctx, "bound ", name1, "");
    if (JS_IsException(name1))
        goto exception;
    JS_DefinePropertyValue(ctx, func_obj, JS_ATOM_name, name1,
                           JS_PROP_CONFIGURABLE);
    return func_obj;
 exception:
    JS_FreeValue(ctx, func_obj);
    return JS_EXCEPTION;
}

static JSValue js_function_toString(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv)
{
    JSObject *p;
    JSFunctionKindEnum func_kind = JS_FUNC_NORMAL;

    if (check_function(ctx, this_val))
        return JS_EXCEPTION;

    p = JS_VALUE_GET_OBJ(this_val);
    if (js_class_has_bytecode(p->class_id)) {
        JSFunctionBytecode *b = p->u.func.function_bytecode;
        if (b->has_debug && b->debug.source) {
            return JS_NewStringLen(ctx, b->debug.source, b->debug.source_len);
        }
        func_kind = b->func_kind;
    }
    {
        JSValue name;
        const char *pref, *suff;

        switch(func_kind) {
        default:
        case JS_FUNC_NORMAL:
            pref = "function ";
            break;
        case JS_FUNC_GENERATOR:
            pref = "function *";
            break;
        case JS_FUNC_ASYNC:
            pref = "async function ";
            break;
        case JS_FUNC_ASYNC_GENERATOR:
            pref = "async function *";
            break;
        }
        suff = "() {\n    [native code]\n}";
        name = JS_GetProperty(ctx, this_val, JS_ATOM_name);
        if (JS_IsUndefined(name))
            name = JS_AtomToString(ctx, JS_ATOM_empty_string);
        return JS_ConcatString3(ctx, pref, name, suff);
    }
}

static JSValue js_function_hasInstance(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv)
{
    int ret;
    ret = JS_OrdinaryIsInstanceOf(ctx, argv[0], this_val);
    if (ret < 0)
        return JS_EXCEPTION;
    else
        return JS_NewBool(ctx, ret);
}

static const JSCFunctionListEntry js_function_proto_funcs[] = {
    JS_CFUNC_DEF("call", 1, js_function_call ),
    JS_CFUNC_MAGIC_DEF("apply", 2, js_function_apply, 0 ),
    JS_CFUNC_DEF("bind", 1, js_function_bind ),
    JS_CFUNC_DEF("toString", 0, js_function_toString ),
    JS_CFUNC_DEF("[Symbol.hasInstance]", 1, js_function_hasInstance ),
    JS_CGETSET_DEF("fileName", js_function_proto_fileName, NULL ),
    JS_CGETSET_DEF("lineNumber", js_function_proto_lineNumber, NULL ),
};

/* Error class */

static JSValue iterator_to_array(JSContext *ctx, JSValueConst items)
{
    JSValue iter, next_method = JS_UNDEFINED;
    JSValue v, r = JS_UNDEFINED;
    int64_t k;
    BOOL done;
    
    iter = JS_GetIterator(ctx, items, FALSE);
    if (JS_IsException(iter))
        goto exception;
    next_method = JS_GetProperty(ctx, iter, JS_ATOM_next);
    if (JS_IsException(next_method))
        goto exception;
    r = JS_NewArray(ctx);
    if (JS_IsException(r))
        goto exception;
    for (k = 0;; k++) {
        v = JS_IteratorNext(ctx, iter, next_method, 0, NULL, &done);
        if (JS_IsException(v))
            goto exception_close;
        if (done)
            break;
        if (JS_DefinePropertyValueInt64(ctx, r, k, v,
                                        JS_PROP_C_W_E | JS_PROP_THROW) < 0)
            goto exception_close;
    }
 done:
    JS_FreeValue(ctx, next_method);
    JS_FreeValue(ctx, iter);
    return r;
 exception_close:
    JS_IteratorClose(ctx, iter, TRUE);
 exception:
    JS_FreeValue(ctx, r);
    r = JS_EXCEPTION;
    goto done;
}

static JSValue js_error_constructor(JSContext *ctx, JSValueConst new_target,
                                    int argc, JSValueConst *argv, int magic)
{
    JSValue obj, msg, proto;
    JSValueConst message;

    if (JS_IsUndefined(new_target))
        new_target = JS_GetActiveFunction(ctx);
    proto = JS_GetProperty(ctx, new_target, JS_ATOM_prototype);
    if (JS_IsException(proto))
        return proto;
    if (!JS_IsObject(proto)) {
        JSContext *realm;
        JSValueConst proto1;
        
        JS_FreeValue(ctx, proto);
        realm = JS_GetFunctionRealm(ctx, new_target);
        if (!realm)
            return JS_EXCEPTION;
        if (magic < 0) {
            proto1 = realm->class_proto[JS_CLASS_ERROR];
        } else {
            proto1 = realm->native_error_proto[magic];
        }
        proto = JS_DupValue(ctx, proto1);
    }
    obj = JS_NewObjectProtoClass(ctx, proto, JS_CLASS_ERROR);
    JS_FreeValue(ctx, proto);
    if (JS_IsException(obj))
        return obj;
    if (magic == JS_AGGREGATE_ERROR) {
        message = argv[1];
    } else {
        message = argv[0];
    }

    if (!JS_IsUndefined(message)) {
        msg = JS_ToString(ctx, message);
        if (unlikely(JS_IsException(msg)))
            goto exception;
        JS_DefinePropertyValue(ctx, obj, JS_ATOM_message, msg,
                               JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    }

    if (magic == JS_AGGREGATE_ERROR) {
        JSValue error_list = iterator_to_array(ctx, argv[0]);
        if (JS_IsException(error_list))
            goto exception;
        JS_DefinePropertyValue(ctx, obj, JS_ATOM_errors, error_list,
                               JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    }

    /* skip the Error() function in the backtrace */
    build_backtrace(ctx, obj, NULL, 0, JS_BACKTRACE_FLAG_SKIP_FIRST_LEVEL);
    return obj;
 exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_error_toString(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    JSValue name, msg;

    if (!JS_IsObject(this_val))
        return JS_ThrowTypeErrorNotAnObject(ctx);
    name = JS_GetProperty(ctx, this_val, JS_ATOM_name);
    if (JS_IsUndefined(name))
        name = JS_AtomToString(ctx, JS_ATOM_Error);
    else
        name = JS_ToStringFree(ctx, name);
    if (JS_IsException(name))
        return JS_EXCEPTION;

    msg = JS_GetProperty(ctx, this_val, JS_ATOM_message);
    if (JS_IsUndefined(msg))
        msg = JS_AtomToString(ctx, JS_ATOM_empty_string);
    else
        msg = JS_ToStringFree(ctx, msg);
    if (JS_IsException(msg)) {
        JS_FreeValue(ctx, name);
        return JS_EXCEPTION;
    }
    if (!JS_IsEmptyString(name) && !JS_IsEmptyString(msg))
        name = JS_ConcatString3(ctx, "", name, ": ");
    return JS_ConcatString(ctx, name, msg);
}

static const JSCFunctionListEntry js_error_proto_funcs[] = {
    JS_CFUNC_DEF("toString", 0, js_error_toString ),
    JS_PROP_STRING_DEF("name", "Error", JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE ),
    JS_PROP_STRING_DEF("message", "", JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE ),
};

/* AggregateError */

/* used by C code. */
static JSValue js_aggregate_error_constructor(JSContext *ctx,
                                              JSValueConst errors)
{
    JSValue obj;
    
    obj = JS_NewObjectProtoClass(ctx,
                                 ctx->native_error_proto[JS_AGGREGATE_ERROR],
                                 JS_CLASS_ERROR);
    if (JS_IsException(obj))
        return obj;
    JS_DefinePropertyValue(ctx, obj, JS_ATOM_errors, JS_DupValue(ctx, errors),
                           JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    return obj;
}

/* Array */

static int JS_CopySubArray(JSContext *ctx,
                           JSValueConst obj, int64_t to_pos,
                           int64_t from_pos, int64_t count, int dir)
{
    int64_t i, from, to;
    JSValue val;
    int fromPresent;

    /* XXX: should special case fast arrays */
    for (i = 0; i < count; i++) {
        if (dir < 0) {
            from = from_pos + count - i - 1;
            to = to_pos + count - i - 1;
        } else {
            from = from_pos + i;
            to = to_pos + i;
        }
        fromPresent = JS_TryGetPropertyInt64(ctx, obj, from, &val);
        if (fromPresent < 0)
            goto exception;

        if (fromPresent) {
            if (JS_SetPropertyInt64(ctx, obj, to, val) < 0)
                goto exception;
        } else {
            if (JS_DeletePropertyInt64(ctx, obj, to, JS_PROP_THROW) < 0)
                goto exception;
        }
    }
    return 0;

 exception:
    return -1;
}

static JSValue js_array_constructor(JSContext *ctx, JSValueConst new_target,
                                    int argc, JSValueConst *argv)
{
    JSValue obj;
    int i;

    obj = js_create_from_ctor(ctx, new_target, JS_CLASS_ARRAY);
    if (JS_IsException(obj))
        return obj;
    if (argc == 1 && JS_IsNumber(argv[0])) {
        uint32_t len;
        if (JS_ToArrayLengthFree(ctx, &len, JS_DupValue(ctx, argv[0]), TRUE))
            goto fail;
        if (JS_SetProperty(ctx, obj, JS_ATOM_length, JS_NewUint32(ctx, len)) < 0)
            goto fail;
    } else {
        for(i = 0; i < argc; i++) {
            if (JS_SetPropertyUint32(ctx, obj, i, JS_DupValue(ctx, argv[i])) < 0)
                goto fail;
        }
    }
    return obj;
fail:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_from(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv)
{
    // from(items, mapfn = void 0, this_arg = void 0)
    JSValueConst items = argv[0], mapfn, this_arg;
    JSValueConst args[2];
    JSValue stack[2];
    JSValue iter, r, v, v2, arrayLike;
    int64_t k, len;
    int done, mapping;

    mapping = FALSE;
    mapfn = JS_UNDEFINED;
    this_arg = JS_UNDEFINED;
    r = JS_UNDEFINED;
    arrayLike = JS_UNDEFINED;
    stack[0] = JS_UNDEFINED;
    stack[1] = JS_UNDEFINED;

    if (argc > 1) {
        mapfn = argv[1];
        if (!JS_IsUndefined(mapfn)) {
            if (check_function(ctx, mapfn))
                goto exception;
            mapping = 1;
            if (argc > 2)
                this_arg = argv[2];
        }
    }
    iter = JS_GetProperty(ctx, items, JS_ATOM_Symbol_iterator);
    if (JS_IsException(iter))
        goto exception;
    if (!JS_IsUndefined(iter)) {
        JS_FreeValue(ctx, iter);
        if (JS_IsConstructor(ctx, this_val))
            r = JS_CallConstructor(ctx, this_val, 0, NULL);
        else
            r = JS_NewArray(ctx);
        if (JS_IsException(r))
            goto exception;
        stack[0] = JS_DupValue(ctx, items);
        if (js_for_of_start(ctx, &stack[1], FALSE))
            goto exception;
        for (k = 0;; k++) {
            v = JS_IteratorNext(ctx, stack[0], stack[1], 0, NULL, &done);
            if (JS_IsException(v))
                goto exception_close;
            if (done)
                break;
            if (mapping) {
                args[0] = v;
                args[1] = JS_NewInt32(ctx, k);
                v2 = JS_Call(ctx, mapfn, this_arg, 2, args);
                JS_FreeValue(ctx, v);
                v = v2;
                if (JS_IsException(v))
                    goto exception_close;
            }
            if (JS_DefinePropertyValueInt64(ctx, r, k, v,
                                            JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                goto exception_close;
        }
    } else {
        arrayLike = JS_ToObject(ctx, items);
        if (JS_IsException(arrayLike))
            goto exception;
        if (js_get_length64(ctx, &len, arrayLike) < 0)
            goto exception;
        v = JS_NewInt64(ctx, len);
        args[0] = v;
        if (JS_IsConstructor(ctx, this_val)) {
            r = JS_CallConstructor(ctx, this_val, 1, args);
        } else {
            r = js_array_constructor(ctx, JS_UNDEFINED, 1, args);
        }
        JS_FreeValue(ctx, v);
        if (JS_IsException(r))
            goto exception;
        for(k = 0; k < len; k++) {
            v = JS_GetPropertyInt64(ctx, arrayLike, k);
            if (JS_IsException(v))
                goto exception;
            if (mapping) {
                args[0] = v;
                args[1] = JS_NewInt32(ctx, k);
                v2 = JS_Call(ctx, mapfn, this_arg, 2, args);
                JS_FreeValue(ctx, v);
                v = v2;
                if (JS_IsException(v))
                    goto exception;
            }
            if (JS_DefinePropertyValueInt64(ctx, r, k, v,
                                            JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                goto exception;
        }
    }
    if (JS_SetProperty(ctx, r, JS_ATOM_length, JS_NewUint32(ctx, k)) < 0)
        goto exception;
    goto done;

 exception_close:
    if (!JS_IsUndefined(stack[0]))
        JS_IteratorClose(ctx, stack[0], TRUE);
 exception:
    JS_FreeValue(ctx, r);
    r = JS_EXCEPTION;
 done:
    JS_FreeValue(ctx, arrayLike);
    JS_FreeValue(ctx, stack[0]);
    JS_FreeValue(ctx, stack[1]);
    return r;
}

static JSValue js_array_of(JSContext *ctx, JSValueConst this_val,
                           int argc, JSValueConst *argv)
{
    JSValue obj, args[1];
    int i;

    if (JS_IsConstructor(ctx, this_val)) {
        args[0] = JS_NewInt32(ctx, argc);
        obj = JS_CallConstructor(ctx, this_val, 1, (JSValueConst *)args);
    } else {
        obj = JS_NewArray(ctx);
    }
    if (JS_IsException(obj))
        return JS_EXCEPTION;
    for(i = 0; i < argc; i++) {
        if (JS_CreateDataPropertyUint32(ctx, obj, i, JS_DupValue(ctx, argv[i]),
                                        JS_PROP_THROW) < 0) {
            goto fail;
        }
    }
    if (JS_SetProperty(ctx, obj, JS_ATOM_length, JS_NewUint32(ctx, argc)) < 0) {
    fail:
        JS_FreeValue(ctx, obj);
        return JS_EXCEPTION;
    }
    return obj;
}

static JSValue js_array_isArray(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    int ret;
    ret = JS_IsArray(ctx, argv[0]);
    if (ret < 0)
        return JS_EXCEPTION;
    else
        return JS_NewBool(ctx, ret);
}

static JSValue js_get_this(JSContext *ctx,
                           JSValueConst this_val)
{
    return JS_DupValue(ctx, this_val);
}

static JSValue JS_ArraySpeciesCreate(JSContext *ctx, JSValueConst obj,
                                     JSValueConst len_val)
{
    JSValue ctor, ret, species;
    int res;
    JSContext *realm;
    
    res = JS_IsArray(ctx, obj);
    if (res < 0)
        return JS_EXCEPTION;
    if (!res)
        return js_array_constructor(ctx, JS_UNDEFINED, 1, &len_val);
    ctor = JS_GetProperty(ctx, obj, JS_ATOM_constructor);
    if (JS_IsException(ctor))
        return ctor;
    if (JS_IsConstructor(ctx, ctor)) {
        /* legacy web compatibility */
        realm = JS_GetFunctionRealm(ctx, ctor);
        if (!realm) {
            JS_FreeValue(ctx, ctor);
            return JS_EXCEPTION;
        }
        if (realm != ctx &&
            js_same_value(ctx, ctor, realm->array_ctor)) {
            JS_FreeValue(ctx, ctor);
            ctor = JS_UNDEFINED;
        }
    }
    if (JS_IsObject(ctor)) {
        species = JS_GetProperty(ctx, ctor, JS_ATOM_Symbol_species);
        JS_FreeValue(ctx, ctor);
        if (JS_IsException(species))
            return species;
        ctor = species;
        if (JS_IsNull(ctor))
            ctor = JS_UNDEFINED;
    }
    if (JS_IsUndefined(ctor)) {
        return js_array_constructor(ctx, JS_UNDEFINED, 1, &len_val);
    } else {
        ret = JS_CallConstructor(ctx, ctor, 1, &len_val);
        JS_FreeValue(ctx, ctor);
        return ret;
    }
}

static const JSCFunctionListEntry js_array_funcs[] = {
    JS_CFUNC_DEF("isArray", 1, js_array_isArray ),
    JS_CFUNC_DEF("from", 1, js_array_from ),
    JS_CFUNC_DEF("of", 0, js_array_of ),
    JS_CGETSET_DEF("[Symbol.species]", js_get_this, NULL ),
};

static int JS_isConcatSpreadable(JSContext *ctx, JSValueConst obj)
{
    JSValue val;

    if (!JS_IsObject(obj))
        return FALSE;
    val = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_isConcatSpreadable);
    if (JS_IsException(val))
        return -1;
    if (!JS_IsUndefined(val))
        return JS_ToBoolFree(ctx, val);
    return JS_IsArray(ctx, obj);
}

static JSValue js_array_concat(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv)
{
    JSValue obj, arr, val;
    JSValueConst e;
    int64_t len, k, n;
    int i, res;

    arr = JS_UNDEFINED;
    obj = JS_ToObject(ctx, this_val);
    if (JS_IsException(obj))
        goto exception;

    arr = JS_ArraySpeciesCreate(ctx, obj, JS_NewInt32(ctx, 0));
    if (JS_IsException(arr))
        goto exception;
    n = 0;
    for (i = -1; i < argc; i++) {
        if (i < 0)
            e = obj;
        else
            e = argv[i];

        res = JS_isConcatSpreadable(ctx, e);
        if (res < 0)
            goto exception;
        if (res) {
            if (js_get_length64(ctx, &len, e))
                goto exception;
            if (n + len > MAX_SAFE_INTEGER) {
                JS_ThrowTypeError(ctx, "Array loo long");
                goto exception;
            }
            for (k = 0; k < len; k++, n++) {
                res = JS_TryGetPropertyInt64(ctx, e, k, &val);
                if (res < 0)
                    goto exception;
                if (res) {
                    if (JS_DefinePropertyValueInt64(ctx, arr, n, val,
                                                    JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                        goto exception;
                }
            }
        } else {
            if (n >= MAX_SAFE_INTEGER) {
                JS_ThrowTypeError(ctx, "Array loo long");
                goto exception;
            }
            if (JS_DefinePropertyValueInt64(ctx, arr, n, JS_DupValue(ctx, e),
                                            JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                goto exception;
            n++;
        }
    }
    if (JS_SetProperty(ctx, arr, JS_ATOM_length, JS_NewInt64(ctx, n)) < 0)
        goto exception;

    JS_FreeValue(ctx, obj);
    return arr;

exception:
    JS_FreeValue(ctx, arr);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

#define special_every    0
#define special_some     1
#define special_forEach  2
#define special_map      3
#define special_filter   4
#define special_TA       8

static int js_typed_array_get_length_internal(JSContext *ctx, JSValueConst obj);

static JSValue js_typed_array___speciesCreate(JSContext *ctx,
                                              JSValueConst this_val,
                                              int argc, JSValueConst *argv);

static JSValue js_array_every(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv, int special)
{
    JSValue obj, val, index_val, res, ret;
    JSValueConst args[3];
    JSValueConst func, this_arg;
    int64_t len, k, n;
    int present;

    ret = JS_UNDEFINED;
    val = JS_UNDEFINED;
    if (special & special_TA) {
        obj = JS_DupValue(ctx, this_val);
        len = js_typed_array_get_length_internal(ctx, obj);
        if (len < 0)
            goto exception;
    } else {
        obj = JS_ToObject(ctx, this_val);
        if (js_get_length64(ctx, &len, obj))
            goto exception;
    }
    func = argv[0];
    this_arg = JS_UNDEFINED;
    if (argc > 1)
        this_arg = argv[1];
        
    if (check_function(ctx, func))
        goto exception;

    switch (special) {
    case special_every:
    case special_every | special_TA:
        ret = JS_TRUE;
        break;
    case special_some:
    case special_some | special_TA:
        ret = JS_FALSE;
        break;
    case special_map:
        /* XXX: JS_ArraySpeciesCreate should take int64_t */
        ret = JS_ArraySpeciesCreate(ctx, obj, JS_NewInt64(ctx, len));
        if (JS_IsException(ret))
            goto exception;
        break;
    case special_filter:
        ret = JS_ArraySpeciesCreate(ctx, obj, JS_NewInt32(ctx, 0));
        if (JS_IsException(ret))
            goto exception;
        break;
    case special_map | special_TA:
        args[0] = obj;
        args[1] = JS_NewInt32(ctx, len);
        ret = js_typed_array___speciesCreate(ctx, JS_UNDEFINED, 2, args);
        if (JS_IsException(ret))
            goto exception;
        break;
    case special_filter | special_TA:
        ret = JS_NewArray(ctx);
        if (JS_IsException(ret))
            goto exception;
        break;
    }
    n = 0;

    for(k = 0; k < len; k++) {
        if (special & special_TA) {
            val = JS_GetPropertyInt64(ctx, obj, k);
            if (JS_IsException(val))
                goto exception;
            present = TRUE;
        } else {
            present = JS_TryGetPropertyInt64(ctx, obj, k, &val);
            if (present < 0)
                goto exception;
        }
        if (present) {
            index_val = JS_NewInt64(ctx, k);
            if (JS_IsException(index_val))
                goto exception;
            args[0] = val;
            args[1] = index_val;
            args[2] = obj;
            res = JS_Call(ctx, func, this_arg, 3, args);
            JS_FreeValue(ctx, index_val);
            if (JS_IsException(res))
                goto exception;
            switch (special) {
            case special_every:
            case special_every | special_TA:
                if (!JS_ToBoolFree(ctx, res)) {
                    ret = JS_FALSE;
                    goto done;
                }
                break;
            case special_some:
            case special_some | special_TA:
                if (JS_ToBoolFree(ctx, res)) {
                    ret = JS_TRUE;
                    goto done;
                }
                break;
            case special_map:
                if (JS_DefinePropertyValueInt64(ctx, ret, k, res,
                                                JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                    goto exception;
                break;
            case special_map | special_TA:
                if (JS_SetPropertyValue(ctx, ret, JS_NewInt32(ctx, k), res, JS_PROP_THROW) < 0)
                    goto exception;
                break;
            case special_filter:
            case special_filter | special_TA:
                if (JS_ToBoolFree(ctx, res)) {
                    if (JS_DefinePropertyValueInt64(ctx, ret, n++, JS_DupValue(ctx, val),
                                                    JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                        goto exception;
                }
                break;
            default:
                JS_FreeValue(ctx, res);
                break;
            }
            JS_FreeValue(ctx, val);
            val = JS_UNDEFINED;
        }
    }
done:
    if (special == (special_filter | special_TA)) {
        JSValue arr;
        args[0] = obj;
        args[1] = JS_NewInt32(ctx, n);
        arr = js_typed_array___speciesCreate(ctx, JS_UNDEFINED, 2, args);
        if (JS_IsException(arr))
            goto exception;
        args[0] = ret;
        res = JS_Invoke(ctx, arr, JS_ATOM_set, 1, args);
        if (check_exception_free(ctx, res))
            goto exception;
        JS_FreeValue(ctx, ret);
        ret = arr;
    }
    JS_FreeValue(ctx, val);
    JS_FreeValue(ctx, obj);
    return ret;

exception:
    JS_FreeValue(ctx, ret);
    JS_FreeValue(ctx, val);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

#define special_reduce       0
#define special_reduceRight  1

static JSValue js_array_reduce(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv, int special)
{
    JSValue obj, val, index_val, acc, acc1;
    JSValueConst args[4];
    JSValueConst func;
    int64_t len, k, k1;
    int present;

    acc = JS_UNDEFINED;
    val = JS_UNDEFINED;
    if (special & special_TA) {
        obj = JS_DupValue(ctx, this_val);
        len = js_typed_array_get_length_internal(ctx, obj);
        if (len < 0)
            goto exception;
    } else {
        obj = JS_ToObject(ctx, this_val);
        if (js_get_length64(ctx, &len, obj))
            goto exception;
    }
    func = argv[0];

    if (check_function(ctx, func))
        goto exception;

    k = 0;
    if (argc > 1) {
        acc = JS_DupValue(ctx, argv[1]);
    } else {
        for(;;) {
            if (k >= len) {
                JS_ThrowTypeError(ctx, "empty array");
                goto exception;
            }
            k1 = (special & special_reduceRight) ? len - k - 1 : k;
            k++;
            if (special & special_TA) {
                acc = JS_GetPropertyInt64(ctx, obj, k1);
                if (JS_IsException(acc))
                    goto exception;
                break;
            } else {
                present = JS_TryGetPropertyInt64(ctx, obj, k1, &acc);
                if (present < 0)
                    goto exception;
                if (present)
                    break;
            }
        }
    }
    for (; k < len; k++) {
        k1 = (special & special_reduceRight) ? len - k - 1 : k;
        if (special & special_TA) {
            val = JS_GetPropertyInt64(ctx, obj, k1);
            if (JS_IsException(val))
                goto exception;
            present = TRUE;
        } else {
            present = JS_TryGetPropertyInt64(ctx, obj, k1, &val);
            if (present < 0)
                goto exception;
        }
        if (present) {
            index_val = JS_NewInt64(ctx, k1);
            if (JS_IsException(index_val))
                goto exception;
            args[0] = acc;
            args[1] = val;
            args[2] = index_val;
            args[3] = obj;
            acc1 = JS_Call(ctx, func, JS_UNDEFINED, 4, args);
            JS_FreeValue(ctx, index_val);
            JS_FreeValue(ctx, val);
            val = JS_UNDEFINED;
            if (JS_IsException(acc1))
                goto exception;
            JS_FreeValue(ctx, acc);
            acc = acc1;
        }
    }
    JS_FreeValue(ctx, obj);
    return acc;

exception:
    JS_FreeValue(ctx, acc);
    JS_FreeValue(ctx, val);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_fill(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv)
{
    JSValue obj;
    int64_t len, start, end;

    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    start = 0;
    if (argc > 1 && !JS_IsUndefined(argv[1])) {
        if (JS_ToInt64Clamp(ctx, &start, argv[1], 0, len, len))
            goto exception;
    }

    end = len;
    if (argc > 2 && !JS_IsUndefined(argv[2])) {
        if (JS_ToInt64Clamp(ctx, &end, argv[2], 0, len, len))
            goto exception;
    }

    /* XXX: should special case fast arrays */
    while (start < end) {
        if (JS_SetPropertyInt64(ctx, obj, start,
                                JS_DupValue(ctx, argv[0])) < 0)
            goto exception;
        start++;
    }
    return obj;

 exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_includes(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    JSValue obj, val;
    int64_t len, n, res;
    JSValue *arrp;
    uint32_t count;

    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    res = FALSE;
    if (len > 0) {
        n = 0;
        if (argc > 1) {
            if (JS_ToInt64Clamp(ctx, &n, argv[1], 0, len, len))
                goto exception;
        }
        if (js_get_fast_array(ctx, obj, &arrp, &count)) {
            for (; n < count; n++) {
                if (js_strict_eq2(ctx, JS_DupValue(ctx, argv[0]),
                                  JS_DupValue(ctx, arrp[n]),
                                  JS_EQ_SAME_VALUE_ZERO)) {
                    res = TRUE;
                    goto done;
                }
            }
        }
        for (; n < len; n++) {
            val = JS_GetPropertyInt64(ctx, obj, n);
            if (JS_IsException(val))
                goto exception;
            if (js_strict_eq2(ctx, JS_DupValue(ctx, argv[0]), val,
                              JS_EQ_SAME_VALUE_ZERO)) {
                res = TRUE;
                break;
            }
        }
    }
 done:
    JS_FreeValue(ctx, obj);
    return JS_NewBool(ctx, res);

 exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_indexOf(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    JSValue obj, val;
    int64_t len, n, res;
    JSValue *arrp;
    uint32_t count;

    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    res = -1;
    if (len > 0) {
        n = 0;
        if (argc > 1) {
            if (JS_ToInt64Clamp(ctx, &n, argv[1], 0, len, len))
                goto exception;
        }
        if (js_get_fast_array(ctx, obj, &arrp, &count)) {
            for (; n < count; n++) {
                if (js_strict_eq2(ctx, JS_DupValue(ctx, argv[0]),
                                  JS_DupValue(ctx, arrp[n]), JS_EQ_STRICT)) {
                    res = n;
                    goto done;
                }
            }
        }
        for (; n < len; n++) {
            int present = JS_TryGetPropertyInt64(ctx, obj, n, &val);
            if (present < 0)
                goto exception;
            if (present) {
                if (js_strict_eq2(ctx, JS_DupValue(ctx, argv[0]), val, JS_EQ_STRICT)) {
                    res = n;
                    break;
                }
            }
        }
    }
 done:
    JS_FreeValue(ctx, obj);
    return JS_NewInt64(ctx, res);

 exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_lastIndexOf(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv)
{
    JSValue obj, val;
    int64_t len, n, res;
    int present;

    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    res = -1;
    if (len > 0) {
        n = len - 1;
        if (argc > 1) {
            if (JS_ToInt64Clamp(ctx, &n, argv[1], -1, len - 1, len))
                goto exception;
        }
        /* XXX: should special case fast arrays */
        for (; n >= 0; n--) {
            present = JS_TryGetPropertyInt64(ctx, obj, n, &val);
            if (present < 0)
                goto exception;
            if (present) {
                if (js_strict_eq2(ctx, JS_DupValue(ctx, argv[0]), val, JS_EQ_STRICT)) {
                    res = n;
                    break;
                }
            }
        }
    }
    JS_FreeValue(ctx, obj);
    return JS_NewInt64(ctx, res);

 exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_find(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv, int findIndex)
{
    JSValueConst func, this_arg;
    JSValueConst args[3];
    JSValue obj, val, index_val, res;
    int64_t len, k;

    index_val = JS_UNDEFINED;
    val = JS_UNDEFINED;
    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    func = argv[0];
    if (check_function(ctx, func))
        goto exception;

    this_arg = JS_UNDEFINED;
    if (argc > 1)
        this_arg = argv[1];

    for(k = 0; k < len; k++) {
        index_val = JS_NewInt64(ctx, k);
        if (JS_IsException(index_val))
            goto exception;
        val = JS_GetPropertyValue(ctx, obj, index_val);
        if (JS_IsException(val))
            goto exception;
        args[0] = val;
        args[1] = index_val;
        args[2] = this_val;
        res = JS_Call(ctx, func, this_arg, 3, args);
        if (JS_IsException(res))
            goto exception;
        if (JS_ToBoolFree(ctx, res)) {
            if (findIndex) {
                JS_FreeValue(ctx, val);
                JS_FreeValue(ctx, obj);
                return index_val;
            } else {
                JS_FreeValue(ctx, index_val);
                JS_FreeValue(ctx, obj);
                return val;
            }
        }
        JS_FreeValue(ctx, val);
        JS_FreeValue(ctx, index_val);
    }
    JS_FreeValue(ctx, obj);
    if (findIndex)
        return JS_NewInt32(ctx, -1);
    else
        return JS_UNDEFINED;

exception:
    JS_FreeValue(ctx, index_val);
    JS_FreeValue(ctx, val);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_toString(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    JSValue obj, method, ret;

    obj = JS_ToObject(ctx, this_val);
    if (JS_IsException(obj))
        return JS_EXCEPTION;
    method = JS_GetProperty(ctx, obj, JS_ATOM_join);
    if (JS_IsException(method)) {
        ret = JS_EXCEPTION;
    } else
    if (!JS_IsFunction(ctx, method)) {
        /* Use intrinsic Object.prototype.toString */
        JS_FreeValue(ctx, method);
        ret = js_object_toString(ctx, obj, 0, NULL);
    } else {
        ret = JS_CallFree(ctx, method, obj, 0, NULL);
    }
    JS_FreeValue(ctx, obj);
    return ret;
}

static JSValue js_array_join(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv, int toLocaleString)
{
    JSValue obj, sep = JS_UNDEFINED, el;
    StringBuffer b_s, *b = &b_s;
    JSString *p = NULL;
    int64_t i, n;
    int c;

    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &n, obj))
        goto exception;

    c = ',';    /* default separator */
    if (!toLocaleString && argc > 0 && !JS_IsUndefined(argv[0])) {
        sep = JS_ToString(ctx, argv[0]);
        if (JS_IsException(sep))
            goto exception;
        p = JS_VALUE_GET_STRING(sep);
        if (p->len == 1 && !p->is_wide_char)
            c = p->u.str8[0];
        else
            c = -1;
    }
    string_buffer_init(ctx, b, 0);

    for(i = 0; i < n; i++) {
        if (i > 0) {
            if (c >= 0) {
                string_buffer_putc8(b, c);
            } else {
                string_buffer_concat(b, p, 0, p->len);
            }
        }
        el = JS_GetPropertyUint32(ctx, obj, i);
        if (JS_IsException(el))
            goto fail;
        if (!JS_IsNull(el) && !JS_IsUndefined(el)) {
            if (toLocaleString) {
                el = JS_ToLocaleStringFree(ctx, el);
            }
            if (string_buffer_concat_value_free(b, el))
                goto fail;
        }
    }
    JS_FreeValue(ctx, sep);
    JS_FreeValue(ctx, obj);
    return string_buffer_end(b);

fail:
    string_buffer_free(b);
    JS_FreeValue(ctx, sep);
exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_pop(JSContext *ctx, JSValueConst this_val,
                            int argc, JSValueConst *argv, int shift)
{
    JSValue obj, res = JS_UNDEFINED;
    int64_t len, newLen;
    JSValue *arrp;
    uint32_t count32;

    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;
    newLen = 0;
    if (len > 0) {
        newLen = len - 1;
        /* Special case fast arrays */
        if (js_get_fast_array(ctx, obj, &arrp, &count32) && count32 == len) {
            JSObject *p = JS_VALUE_GET_OBJ(obj);
            if (shift) {
                res = arrp[0];
                memmove(arrp, arrp + 1, (count32 - 1) * sizeof(*arrp));
                p->u.array.count--;
            } else {
                res = arrp[count32 - 1];
                p->u.array.count--;
            }
        } else {
            if (shift) {
                res = JS_GetPropertyInt64(ctx, obj, 0);
                if (JS_IsException(res))
                    goto exception;
                if (JS_CopySubArray(ctx, obj, 0, 1, len - 1, +1))
                    goto exception;
            } else {
                res = JS_GetPropertyInt64(ctx, obj, newLen);
                if (JS_IsException(res))
                    goto exception;
            }
            if (JS_DeletePropertyInt64(ctx, obj, newLen, JS_PROP_THROW) < 0)
                goto exception;
        }
    }
    if (JS_SetProperty(ctx, obj, JS_ATOM_length, JS_NewInt64(ctx, newLen)) < 0)
        goto exception;

    JS_FreeValue(ctx, obj);
    return res;

 exception:
    JS_FreeValue(ctx, res);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_push(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv, int unshift)
{
    JSValue obj;
    int i;
    int64_t len, from, newLen;

    obj = JS_ToObject(ctx, this_val);

    if (JS_VALUE_GET_TAG(obj) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(obj);
        if (p->class_id != JS_CLASS_ARRAY ||
            !p->fast_array || !p->extensible)
            goto generic_case;
        /* length must be writable */
        if (unlikely(!(get_shape_prop(p->shape)->flags & JS_PROP_WRITABLE)))
            goto generic_case;
        /* check the length */
        if (unlikely(JS_VALUE_GET_TAG(p->prop[0].u.value) != JS_TAG_INT))
            goto generic_case;
        len = JS_VALUE_GET_INT(p->prop[0].u.value);
        /* we don't support holes */
        if (unlikely(len != p->u.array.count))
            goto generic_case;
        newLen = len + argc;
        if (unlikely(newLen > INT32_MAX))
            goto generic_case;
        if (newLen > p->u.array.u1.size) {
            if (expand_fast_array(ctx, p, newLen))
                goto exception;
        }
        if (unshift && argc > 0) {
            memmove(p->u.array.u.values + argc, p->u.array.u.values,
                    len * sizeof(p->u.array.u.values[0]));
            from = 0;
        } else {
            from = len;
        }
        for(i = 0; i < argc; i++) {
            p->u.array.u.values[from + i] = JS_DupValue(ctx, argv[i]);
        }
        p->u.array.count = newLen;
        p->prop[0].u.value = JS_NewInt32(ctx, newLen);
    } else {
    generic_case:
        if (js_get_length64(ctx, &len, obj))
            goto exception;
        newLen = len + argc;
        if (newLen > MAX_SAFE_INTEGER) {
            JS_ThrowTypeError(ctx, "Array loo long");
            goto exception;
        }
        from = len;
        if (unshift && argc > 0) {
            if (JS_CopySubArray(ctx, obj, argc, 0, len, -1))
                goto exception;
            from = 0;
        }
        for(i = 0; i < argc; i++) {
            if (JS_SetPropertyInt64(ctx, obj, from + i,
                                    JS_DupValue(ctx, argv[i])) < 0)
                goto exception;
        }
        if (JS_SetProperty(ctx, obj, JS_ATOM_length, JS_NewInt64(ctx, newLen)) < 0)
            goto exception;
    }
    JS_FreeValue(ctx, obj);
    return JS_NewInt64(ctx, newLen);

 exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_reverse(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    JSValue obj, lval, hval;
    JSValue *arrp;
    int64_t len, l, h;
    int l_present, h_present;
    uint32_t count32;

    lval = JS_UNDEFINED;
    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    /* Special case fast arrays */
    if (js_get_fast_array(ctx, obj, &arrp, &count32) && count32 == len) {
        uint32_t ll, hh;

        if (count32 > 1) {
            for (ll = 0, hh = count32 - 1; ll < hh; ll++, hh--) {
                lval = arrp[ll];
                arrp[ll] = arrp[hh];
                arrp[hh] = lval;
            }
        }
        return obj;
    }

    for (l = 0, h = len - 1; l < h; l++, h--) {
        l_present = JS_TryGetPropertyInt64(ctx, obj, l, &lval);
        if (l_present < 0)
            goto exception;
        h_present = JS_TryGetPropertyInt64(ctx, obj, h, &hval);
        if (h_present < 0)
            goto exception;
        if (h_present) {
            if (JS_SetPropertyInt64(ctx, obj, l, hval) < 0)
                goto exception;

            if (l_present) {
                if (JS_SetPropertyInt64(ctx, obj, h, lval) < 0) {
                    lval = JS_UNDEFINED;
                    goto exception;
                }
                lval = JS_UNDEFINED;
            } else {
                if (JS_DeletePropertyInt64(ctx, obj, h, JS_PROP_THROW) < 0)
                    goto exception;
            }
        } else {
            if (l_present) {
                if (JS_DeletePropertyInt64(ctx, obj, l, JS_PROP_THROW) < 0)
                    goto exception;
                if (JS_SetPropertyInt64(ctx, obj, h, lval) < 0) {
                    lval = JS_UNDEFINED;
                    goto exception;
                }
                lval = JS_UNDEFINED;
            }
        }
    }
    return obj;

 exception:
    JS_FreeValue(ctx, lval);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_array_slice(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv, int splice)
{
    JSValue obj, arr, val, len_val;
    int64_t len, start, k, final, n, count, del_count, new_len;
    int kPresent;
    JSValue *arrp;
    uint32_t count32, i, item_count;

    arr = JS_UNDEFINED;
    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    if (JS_ToInt64Clamp(ctx, &start, argv[0], 0, len, len))
        goto exception;

    if (splice) {
        if (argc == 0) {
            item_count = 0;
            del_count = 0;
        } else
        if (argc == 1) {
            item_count = 0;
            del_count = len - start;
        } else {
            item_count = argc - 2;
            if (JS_ToInt64Clamp(ctx, &del_count, argv[1], 0, len - start, 0))
                goto exception;
        }
        if (len + item_count - del_count > MAX_SAFE_INTEGER) {
            JS_ThrowTypeError(ctx, "Array loo long");
            goto exception;
        }
        count = del_count;
    } else {
        item_count = 0; /* avoid warning */
        final = len;
        if (!JS_IsUndefined(argv[1])) {
            if (JS_ToInt64Clamp(ctx, &final, argv[1], 0, len, len))
                goto exception;
        }
        count = max_int64(final - start, 0);
    }
    len_val = JS_NewInt64(ctx, count);
    arr = JS_ArraySpeciesCreate(ctx, obj, len_val);
    JS_FreeValue(ctx, len_val);
    if (JS_IsException(arr))
        goto exception;

    k = start;
    final = start + count;
    n = 0;
    /* The fast array test on arr ensures that
       JS_CreateDataPropertyUint32() won't modify obj in case arr is
       an exotic object */
    /* Special case fast arrays */
    if (js_get_fast_array(ctx, obj, &arrp, &count32) &&
        js_is_fast_array(ctx, arr)) {
        /* XXX: should share code with fast array constructor */
        for (; k < final && k < count32; k++, n++) {
            if (JS_CreateDataPropertyUint32(ctx, arr, n, JS_DupValue(ctx, arrp[k]), JS_PROP_THROW) < 0)
                goto exception;
        }
    }
    /* Copy the remaining elements if any (handle case of inherited properties) */
    for (; k < final; k++, n++) {
        kPresent = JS_TryGetPropertyInt64(ctx, obj, k, &val);
        if (kPresent < 0)
            goto exception;
        if (kPresent) {
            if (JS_CreateDataPropertyUint32(ctx, arr, n, val, JS_PROP_THROW) < 0)
                goto exception;
        }
    }
    if (JS_SetProperty(ctx, arr, JS_ATOM_length, JS_NewInt64(ctx, n)) < 0)
        goto exception;

    if (splice) {
        new_len = len + item_count - del_count;
        if (item_count != del_count) {
            if (JS_CopySubArray(ctx, obj, start + item_count,
                                start + del_count, len - (start + del_count),
                                item_count <= del_count ? +1 : -1) < 0)
                goto exception;

            for (k = len; k-- > new_len; ) {
                if (JS_DeletePropertyInt64(ctx, obj, k, JS_PROP_THROW) < 0)
                    goto exception;
            }
        }
        for (i = 0; i < item_count; i++) {
            if (JS_SetPropertyInt64(ctx, obj, start + i, JS_DupValue(ctx, argv[i + 2])) < 0)
                goto exception;
        }
        if (JS_SetProperty(ctx, obj, JS_ATOM_length, JS_NewInt64(ctx, new_len)) < 0)
            goto exception;
    }
    JS_FreeValue(ctx, obj);
    return arr;

 exception:
    JS_FreeValue(ctx, obj);
    JS_FreeValue(ctx, arr);
    return JS_EXCEPTION;
}

static JSValue js_array_copyWithin(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv)
{
    JSValue obj;
    int64_t len, from, to, final, count;

    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    if (JS_ToInt64Clamp(ctx, &to, argv[0], 0, len, len))
        goto exception;

    if (JS_ToInt64Clamp(ctx, &from, argv[1], 0, len, len))
        goto exception;

    final = len;
    if (argc > 2 && !JS_IsUndefined(argv[2])) {
        if (JS_ToInt64Clamp(ctx, &final, argv[2], 0, len, len))
            goto exception;
    }

    count = min_int64(final - from, len - to);

    if (JS_CopySubArray(ctx, obj, to, from, count,
                        (from < to && to < from + count) ? -1 : +1))
        goto exception;

    return obj;

 exception:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static int64_t JS_FlattenIntoArray(JSContext *ctx, JSValueConst target,
                                   JSValueConst source, int64_t sourceLen,
                                   int64_t targetIndex, int depth,
                                   JSValueConst mapperFunction,
                                   JSValueConst thisArg)
{
    JSValue element;
    int64_t sourceIndex, elementLen;
    int present, is_array;

    if (js_check_stack_overflow(ctx->rt, 0)) {
        JS_ThrowStackOverflow(ctx);
        return -1;
    }

    for (sourceIndex = 0; sourceIndex < sourceLen; sourceIndex++) {
        present = JS_TryGetPropertyInt64(ctx, source, sourceIndex, &element);
        if (present < 0)
            return -1;
        if (!present)
            continue;
        if (!JS_IsUndefined(mapperFunction)) {
            JSValueConst args[3] = { element, JS_NewInt64(ctx, sourceIndex), source };
            element = JS_Call(ctx, mapperFunction, thisArg, 3, args);
            JS_FreeValue(ctx, (JSValue)args[0]);
            JS_FreeValue(ctx, (JSValue)args[1]);
            if (JS_IsException(element))
                return -1;
        }
        if (depth > 0) {
            is_array = JS_IsArray(ctx, element);
            if (is_array < 0)
                goto fail;
            if (is_array) {
                if (js_get_length64(ctx, &elementLen, element) < 0)
                    goto fail;
                targetIndex = JS_FlattenIntoArray(ctx, target, element,
                                                  elementLen, targetIndex,
                                                  depth - 1,
                                                  JS_UNDEFINED, JS_UNDEFINED);
                if (targetIndex < 0)
                    goto fail;
                JS_FreeValue(ctx, element);
                continue;
            }
        }
        if (targetIndex >= MAX_SAFE_INTEGER) {
            JS_ThrowTypeError(ctx, "Array too long");
            goto fail;
        }
        if (JS_DefinePropertyValueInt64(ctx, target, targetIndex, element,
                                        JS_PROP_C_W_E | JS_PROP_THROW) < 0)
            return -1;
        targetIndex++;
    }
    return targetIndex;

fail:
    JS_FreeValue(ctx, element);
    return -1;
}

static JSValue js_array_flatten(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv, int map)
{
    JSValue obj, arr;
    JSValueConst mapperFunction, thisArg;
    int64_t sourceLen;
    int depthNum;

    arr = JS_UNDEFINED;
    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &sourceLen, obj))
        goto exception;

    depthNum = 1;
    mapperFunction = JS_UNDEFINED;
    thisArg = JS_UNDEFINED;
    if (map) {
        mapperFunction = argv[0];
        if (argc > 1) {
            thisArg = argv[1];
        }
        if (check_function(ctx, mapperFunction))
            goto exception;
    } else {
        if (argc > 0 && !JS_IsUndefined(argv[0])) {
            if (JS_ToInt32Sat(ctx, &depthNum, argv[0]) < 0)
                goto exception;
        }
    }
    arr = JS_ArraySpeciesCreate(ctx, obj, JS_NewInt32(ctx, 0));
    if (JS_IsException(arr))
        goto exception;
    if (JS_FlattenIntoArray(ctx, arr, obj, sourceLen, 0, depthNum,
                            mapperFunction, thisArg) < 0)
        goto exception;
    JS_FreeValue(ctx, obj);
    return arr;

exception:
    JS_FreeValue(ctx, obj);
    JS_FreeValue(ctx, arr);
    return JS_EXCEPTION;
}

/* Array sort */

typedef struct ValueSlot {
    JSValue val;
    JSString *str;
    int64_t pos;
} ValueSlot;

struct array_sort_context {
    JSContext *ctx;
    int exception;
    int has_method;
    JSValueConst method;
};

static int js_array_cmp_generic(const void *a, const void *b, void *opaque) {
    struct array_sort_context *psc = opaque;
    JSContext *ctx = psc->ctx;
    JSValueConst argv[2];
    JSValue res;
    ValueSlot *ap = (ValueSlot *)(void *)a;
    ValueSlot *bp = (ValueSlot *)(void *)b;
    int cmp;

    if (psc->exception)
        return 0;

    if (psc->has_method) {
        /* custom sort function is specified as returning 0 for identical
         * objects: avoid method call overhead.
         */
        if (!memcmp(&ap->val, &bp->val, sizeof(ap->val)))
            goto cmp_same;
        argv[0] = ap->val;
        argv[1] = bp->val;
        res = JS_Call(ctx, psc->method, JS_UNDEFINED, 2, argv);
        if (JS_IsException(res))
            goto exception;
        if (JS_VALUE_GET_TAG(res) == JS_TAG_INT) {
            int val = JS_VALUE_GET_INT(res);
            cmp = (val > 0) - (val < 0);
        } else {
            double val;
            if (JS_ToFloat64Free(ctx, &val, res) < 0)
                goto exception;
            cmp = (val > 0) - (val < 0);
        }
    } else {
        /* Not supposed to bypass ToString even for identical objects as
         * tested in test262/test/built-ins/Array/prototype/sort/bug_596_1.js
         */
        if (!ap->str) {
            JSValue str = JS_ToString(ctx, ap->val);
            if (JS_IsException(str))
                goto exception;
            ap->str = JS_VALUE_GET_STRING(str);
        }
        if (!bp->str) {
            JSValue str = JS_ToString(ctx, bp->val);
            if (JS_IsException(str))
                goto exception;
            bp->str = JS_VALUE_GET_STRING(str);
        }
        cmp = js_string_compare(ctx, ap->str, bp->str);
    }
    if (cmp != 0)
        return cmp;
cmp_same:
    /* make sort stable: compare array offsets */
    return (ap->pos > bp->pos) - (ap->pos < bp->pos);

exception:
    psc->exception = 1;
    return 0;
}

static JSValue js_array_sort(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv)
{
    struct array_sort_context asc = { ctx, 0, 0, argv[0] };
    JSValue obj = JS_UNDEFINED;
    ValueSlot *array = NULL;
    size_t array_size = 0, pos = 0, n = 0;
    int64_t i, len, undefined_count = 0;
    int present;

    if (!JS_IsUndefined(asc.method)) {
        if (check_function(ctx, asc.method))
            goto exception;
        asc.has_method = 1;
    }
    obj = JS_ToObject(ctx, this_val);
    if (js_get_length64(ctx, &len, obj))
        goto exception;

    /* XXX: should special case fast arrays */
    for (i = 0; i < len; i++) {
        if (pos >= array_size) {
            size_t new_size, slack;
            ValueSlot *new_array;
            new_size = (array_size + (array_size >> 1) + 31) & ~15;
            new_array = js_realloc2(ctx, array, new_size * sizeof(*array), &slack);
            if (new_array == NULL)
                goto exception;
            new_size += slack / sizeof(*new_array);
            array = new_array;
            array_size = new_size;
        }
        present = JS_TryGetPropertyInt64(ctx, obj, i, &array[pos].val);
        if (present < 0)
            goto exception;
        if (present == 0)
            continue;
        if (JS_IsUndefined(array[pos].val)) {
            undefined_count++;
            continue;
        }
        array[pos].str = NULL;
        array[pos].pos = i;
        pos++;
    }
    rqsort(array, pos, sizeof(*array), js_array_cmp_generic, &asc);
    if (asc.exception)
        goto exception;

    /* XXX: should special case fast arrays */
    while (n < pos) {
        if (array[n].str)
            JS_FreeValue(ctx, JS_MKPTR(JS_TAG_STRING, array[n].str));
        if (array[n].pos == n) {
            JS_FreeValue(ctx, array[n].val);
        } else {
            if (JS_SetPropertyInt64(ctx, obj, n, array[n].val) < 0) {
                n++;
                goto exception;
            }
        }
        n++;
    }
    js_free(ctx, array);
    for (i = n; undefined_count-- > 0; i++) {
        if (JS_SetPropertyInt64(ctx, obj, i, JS_UNDEFINED) < 0)
            goto fail;
    }
    for (; i < len; i++) {
        if (JS_DeletePropertyInt64(ctx, obj, i, JS_PROP_THROW) < 0)
            goto fail;
    }
    return obj;

exception:
    for (; n < pos; n++) {
        JS_FreeValue(ctx, array[n].val);
        if (array[n].str)
            JS_FreeValue(ctx, JS_MKPTR(JS_TAG_STRING, array[n].str));
    }
    js_free(ctx, array);
fail:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

typedef struct JSArrayIteratorData {
    JSValue obj;
    JSIteratorKindEnum kind;
    uint32_t idx;
} JSArrayIteratorData;

static void js_array_iterator_finalizer(JSRuntime *rt, JSValue val)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSArrayIteratorData *it = p->u.array_iterator_data;
    if (it) {
        JS_FreeValueRT(rt, it->obj);
        js_free_rt(rt, it);
    }
}

static void js_array_iterator_mark(JSRuntime *rt, JSValueConst val,
                                   JS_MarkFunc *mark_func)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSArrayIteratorData *it = p->u.array_iterator_data;
    if (it) {
        JS_MarkValue(rt, it->obj, mark_func);
    }
}

static JSValue js_create_array(JSContext *ctx, int len, JSValueConst *tab)
{
    JSValue obj;
    int i;

    obj = JS_NewArray(ctx);
    if (JS_IsException(obj))
        return JS_EXCEPTION;
    for(i = 0; i < len; i++) {
        if (JS_CreateDataPropertyUint32(ctx, obj, i, JS_DupValue(ctx, tab[i]), 0) < 0) {
            JS_FreeValue(ctx, obj);
            return JS_EXCEPTION;
        }
    }
    return obj;
}

static JSValue js_create_array_iterator(JSContext *ctx, JSValueConst this_val,
                                        int argc, JSValueConst *argv, int magic)
{
    JSValue enum_obj, arr;
    JSArrayIteratorData *it;
    JSIteratorKindEnum kind;
    int class_id;

    kind = magic & 3;
    if (magic & 4) {
        /* string iterator case */
        arr = JS_ToStringCheckObject(ctx, this_val);
        class_id = JS_CLASS_STRING_ITERATOR;
    } else {
        arr = JS_ToObject(ctx, this_val);
        class_id = JS_CLASS_ARRAY_ITERATOR;
    }
    if (JS_IsException(arr))
        goto fail;
    enum_obj = JS_NewObjectClass(ctx, class_id);
    if (JS_IsException(enum_obj))
        goto fail;
    it = js_malloc(ctx, sizeof(*it));
    if (!it)
        goto fail1;
    it->obj = arr;
    it->kind = kind;
    it->idx = 0;
    JS_SetOpaque(enum_obj, it);
    return enum_obj;
 fail1:
    JS_FreeValue(ctx, enum_obj);
 fail:
    JS_FreeValue(ctx, arr);
    return JS_EXCEPTION;
}

static JSValue js_array_iterator_next(JSContext *ctx, JSValueConst this_val,
                                      int argc, JSValueConst *argv,
                                      BOOL *pdone, int magic)
{
    JSArrayIteratorData *it;
    uint32_t len, idx;
    JSValue val, obj;
    JSObject *p;

    it = JS_GetOpaque2(ctx, this_val, JS_CLASS_ARRAY_ITERATOR);
    if (!it)
        goto fail1;
    if (JS_IsUndefined(it->obj))
        goto done;
    p = JS_VALUE_GET_OBJ(it->obj);
    if (p->class_id >= JS_CLASS_UINT8C_ARRAY &&
        p->class_id <= JS_CLASS_FLOAT64_ARRAY) {
        if (typed_array_is_detached(ctx, p)) {
            JS_ThrowTypeErrorDetachedArrayBuffer(ctx);
            goto fail1;
        }
        len = p->u.array.count;
    } else {
        if (js_get_length32(ctx, &len, it->obj)) {
        fail1:
            *pdone = FALSE;
            return JS_EXCEPTION;
        }
    }
    idx = it->idx;
    if (idx >= len) {
        JS_FreeValue(ctx, it->obj);
        it->obj = JS_UNDEFINED;
    done:
        *pdone = TRUE;
        return JS_UNDEFINED;
    }
    it->idx = idx + 1;
    *pdone = FALSE;
    if (it->kind == JS_ITERATOR_KIND_KEY) {
        return JS_NewUint32(ctx, idx);
    } else {
        val = JS_GetPropertyUint32(ctx, it->obj, idx);
        if (JS_IsException(val))
            return JS_EXCEPTION;
        if (it->kind == JS_ITERATOR_KIND_VALUE) {
            return val;
        } else {
            JSValueConst args[2];
            JSValue num;
            num = JS_NewUint32(ctx, idx);
            args[0] = num;
            args[1] = val;
            obj = js_create_array(ctx, 2, args);
            JS_FreeValue(ctx, val);
            JS_FreeValue(ctx, num);
            return obj;
        }
    }
}

static JSValue js_iterator_proto_iterator(JSContext *ctx, JSValueConst this_val,
                                          int argc, JSValueConst *argv)
{
    return JS_DupValue(ctx, this_val);
}

static const JSCFunctionListEntry js_iterator_proto_funcs[] = {
    JS_CFUNC_DEF("[Symbol.iterator]", 0, js_iterator_proto_iterator ),
};

static const JSCFunctionListEntry js_array_proto_funcs[] = {
    JS_CFUNC_DEF("concat", 1, js_array_concat ),
    JS_CFUNC_MAGIC_DEF("every", 1, js_array_every, special_every ),
    JS_CFUNC_MAGIC_DEF("some", 1, js_array_every, special_some ),
    JS_CFUNC_MAGIC_DEF("forEach", 1, js_array_every, special_forEach ),
    JS_CFUNC_MAGIC_DEF("map", 1, js_array_every, special_map ),
    JS_CFUNC_MAGIC_DEF("filter", 1, js_array_every, special_filter ),
    JS_CFUNC_MAGIC_DEF("reduce", 1, js_array_reduce, special_reduce ),
    JS_CFUNC_MAGIC_DEF("reduceRight", 1, js_array_reduce, special_reduceRight ),
    JS_CFUNC_DEF("fill", 1, js_array_fill ),
    JS_CFUNC_MAGIC_DEF("find", 1, js_array_find, 0 ),
    JS_CFUNC_MAGIC_DEF("findIndex", 1, js_array_find, 1 ),
    JS_CFUNC_DEF("indexOf", 1, js_array_indexOf ),
    JS_CFUNC_DEF("lastIndexOf", 1, js_array_lastIndexOf ),
    JS_CFUNC_DEF("includes", 1, js_array_includes ),
    JS_CFUNC_MAGIC_DEF("join", 1, js_array_join, 0 ),
    JS_CFUNC_DEF("toString", 0, js_array_toString ),
    JS_CFUNC_MAGIC_DEF("toLocaleString", 0, js_array_join, 1 ),
    JS_CFUNC_MAGIC_DEF("pop", 0, js_array_pop, 0 ),
    JS_CFUNC_MAGIC_DEF("push", 1, js_array_push, 0 ),
    JS_CFUNC_MAGIC_DEF("shift", 0, js_array_pop, 1 ),
    JS_CFUNC_MAGIC_DEF("unshift", 1, js_array_push, 1 ),
    JS_CFUNC_DEF("reverse", 0, js_array_reverse ),
    JS_CFUNC_DEF("sort", 1, js_array_sort ),
    JS_CFUNC_MAGIC_DEF("slice", 2, js_array_slice, 0 ),
    JS_CFUNC_MAGIC_DEF("splice", 2, js_array_slice, 1 ),
    JS_CFUNC_DEF("copyWithin", 2, js_array_copyWithin ),
    JS_CFUNC_MAGIC_DEF("flatMap", 1, js_array_flatten, 1 ),
    JS_CFUNC_MAGIC_DEF("flat", 0, js_array_flatten, 0 ),
    JS_CFUNC_MAGIC_DEF("values", 0, js_create_array_iterator, JS_ITERATOR_KIND_VALUE ),
    JS_ALIAS_DEF("[Symbol.iterator]", "values" ),
    JS_CFUNC_MAGIC_DEF("keys", 0, js_create_array_iterator, JS_ITERATOR_KIND_KEY ),
    JS_CFUNC_MAGIC_DEF("entries", 0, js_create_array_iterator, JS_ITERATOR_KIND_KEY_AND_VALUE ),
};

static const JSCFunctionListEntry js_array_iterator_proto_funcs[] = {
    JS_ITERATOR_NEXT_DEF("next", 0, js_array_iterator_next, 0 ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "Array Iterator", JS_PROP_CONFIGURABLE ),
};

/* Number */

static JSValue js_number_constructor(JSContext *ctx, JSValueConst new_target,
                                     int argc, JSValueConst *argv)
{
    JSValue val, obj;
    if (argc == 0) {
        val = JS_NewInt32(ctx, 0);
    } else {
        val = JS_ToNumeric(ctx, argv[0]);
        if (JS_IsException(val))
            return val;
        switch(JS_VALUE_GET_TAG(val)) {
#ifdef CONFIG_BIGNUM
        case JS_TAG_BIG_INT:
        case JS_TAG_BIG_FLOAT:
            {
                JSBigFloat *p = JS_VALUE_GET_PTR(val);
                double d;
                bf_get_float64(&p->num, &d, BF_RNDN);
                JS_FreeValue(ctx, val);
                val = __JS_NewFloat64(ctx, d);
            }
            break;
        case JS_TAG_BIG_DECIMAL:
            val = JS_ToStringFree(ctx, val);
            if (JS_IsException(val))
                return val;
            val = JS_ToNumberFree(ctx, val);
            if (JS_IsException(val))
                return val;
            break;
#endif
        default:
            break;
        }
    }
    if (!JS_IsUndefined(new_target)) {
        obj = js_create_from_ctor(ctx, new_target, JS_CLASS_NUMBER);
        if (!JS_IsException(obj))
            JS_SetObjectData(ctx, obj, val);
        return obj;
    } else {
        return val;
    }
}

#if 0
static JSValue js_number___toInteger(JSContext *ctx, JSValueConst this_val,
                                     int argc, JSValueConst *argv)
{
    return JS_ToIntegerFree(ctx, JS_DupValue(ctx, argv[0]));
}

static JSValue js_number___toLength(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv)
{
    int64_t v;
    if (JS_ToLengthFree(ctx, &v, JS_DupValue(ctx, argv[0])))
        return JS_EXCEPTION;
    return JS_NewInt64(ctx, v);
}
#endif

static JSValue js_number_isNaN(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv)
{
    if (!JS_IsNumber(argv[0]))
        return JS_FALSE;
    return js_global_isNaN(ctx, this_val, argc, argv);
}

static JSValue js_number_isFinite(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv)
{
    if (!JS_IsNumber(argv[0]))
        return JS_FALSE;
    return js_global_isFinite(ctx, this_val, argc, argv);
}

static JSValue js_number_isInteger(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv)
{
    int ret;
    ret = JS_NumberIsInteger(ctx, argv[0]);
    if (ret < 0)
        return JS_EXCEPTION;
    else
        return JS_NewBool(ctx, ret);
}

static JSValue js_number_isSafeInteger(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv)
{
    double d;
    if (!JS_IsNumber(argv[0]))
        return JS_FALSE;
    if (unlikely(JS_ToFloat64(ctx, &d, argv[0])))
        return JS_EXCEPTION;
    return JS_NewBool(ctx, is_safe_integer(d));
}

static const JSCFunctionListEntry js_number_funcs[] = {
    /* global ParseInt and parseFloat should be defined already or delayed */
    JS_ALIAS_BASE_DEF("parseInt", "parseInt", 0 ),
    JS_ALIAS_BASE_DEF("parseFloat", "parseFloat", 0 ),
    JS_CFUNC_DEF("isNaN", 1, js_number_isNaN ),
    JS_CFUNC_DEF("isFinite", 1, js_number_isFinite ),
    JS_CFUNC_DEF("isInteger", 1, js_number_isInteger ),
    JS_CFUNC_DEF("isSafeInteger", 1, js_number_isSafeInteger ),
    JS_PROP_DOUBLE_DEF("MAX_VALUE", 1.7976931348623157e+308, 0 ),
    JS_PROP_DOUBLE_DEF("MIN_VALUE", 5e-324, 0 ),
    JS_PROP_DOUBLE_DEF("NaN", NAN, 0 ),
    JS_PROP_DOUBLE_DEF("NEGATIVE_INFINITY", -INFINITY, 0 ),
    JS_PROP_DOUBLE_DEF("POSITIVE_INFINITY", INFINITY, 0 ),
    JS_PROP_DOUBLE_DEF("EPSILON", 2.220446049250313e-16, 0 ), /* ES6 */
    JS_PROP_DOUBLE_DEF("MAX_SAFE_INTEGER", 9007199254740991.0, 0 ), /* ES6 */
    JS_PROP_DOUBLE_DEF("MIN_SAFE_INTEGER", -9007199254740991.0, 0 ), /* ES6 */
    //JS_CFUNC_DEF("__toInteger", 1, js_number___toInteger ),
    //JS_CFUNC_DEF("__toLength", 1, js_number___toLength ),
};

static JSValue js_thisNumberValue(JSContext *ctx, JSValueConst this_val)
{
    if (JS_IsNumber(this_val))
        return JS_DupValue(ctx, this_val);

    if (JS_VALUE_GET_TAG(this_val) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(this_val);
        if (p->class_id == JS_CLASS_NUMBER) {
            if (JS_IsNumber(p->u.object_data))
                return JS_DupValue(ctx, p->u.object_data);
        }
    }
    return JS_ThrowTypeError(ctx, "not a number");
}

static JSValue js_number_valueOf(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    return js_thisNumberValue(ctx, this_val);
}

static int js_get_radix(JSContext *ctx, JSValueConst val)
{
    int radix;
    if (JS_ToInt32Sat(ctx, &radix, val))
        return -1;
    if (radix < 2 || radix > 36) {
        JS_ThrowRangeError(ctx, "radix must be between 2 and 36");
        return -1;
    }
    return radix;
}

static JSValue js_number_toString(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv, int magic)
{
    JSValue val;
    int base;
    double d;

    val = js_thisNumberValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    if (magic || JS_IsUndefined(argv[0])) {
        base = 10;
    } else {
        base = js_get_radix(ctx, argv[0]);
        if (base < 0)
            goto fail;
    }
    if (JS_ToFloat64Free(ctx, &d, val))
        return JS_EXCEPTION;
    return js_dtoa(ctx, d, base, 0, JS_DTOA_VAR_FORMAT);
 fail:
    JS_FreeValue(ctx, val);
    return JS_EXCEPTION;
}

static JSValue js_number_toFixed(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    JSValue val;
    int f;
    double d;

    val = js_thisNumberValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    if (JS_ToFloat64Free(ctx, &d, val))
        return JS_EXCEPTION;
    if (JS_ToInt32Sat(ctx, &f, argv[0]))
        return JS_EXCEPTION;
    if (f < 0 || f > 100)
        return JS_ThrowRangeError(ctx, "invalid number of digits");
    if (fabs(d) >= 1e21) {
        return JS_ToStringFree(ctx, __JS_NewFloat64(ctx, d));
    } else {
        return js_dtoa(ctx, d, 10, f, JS_DTOA_FRAC_FORMAT);
    }
}

static JSValue js_number_toExponential(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv)
{
    JSValue val;
    int f, flags;
    double d;

    val = js_thisNumberValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    if (JS_ToFloat64Free(ctx, &d, val))
        return JS_EXCEPTION;
    if (JS_ToInt32Sat(ctx, &f, argv[0]))
        return JS_EXCEPTION;
    if (!isfinite(d)) {
        return JS_ToStringFree(ctx,  __JS_NewFloat64(ctx, d));
    }
    if (JS_IsUndefined(argv[0])) {
        flags = 0;
        f = 0;
    } else {
        if (f < 0 || f > 100)
            return JS_ThrowRangeError(ctx, "invalid number of digits");
        f++;
        flags = JS_DTOA_FIXED_FORMAT;
    }
    return js_dtoa(ctx, d, 10, f, flags | JS_DTOA_FORCE_EXP);
}

static JSValue js_number_toPrecision(JSContext *ctx, JSValueConst this_val,
                                     int argc, JSValueConst *argv)
{
    JSValue val;
    int p;
    double d;

    val = js_thisNumberValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    if (JS_ToFloat64Free(ctx, &d, val))
        return JS_EXCEPTION;
    if (JS_IsUndefined(argv[0]))
        goto to_string;
    if (JS_ToInt32Sat(ctx, &p, argv[0]))
        return JS_EXCEPTION;
    if (!isfinite(d)) {
    to_string:
        return JS_ToStringFree(ctx,  __JS_NewFloat64(ctx, d));
    }
    if (p < 1 || p > 100)
        return JS_ThrowRangeError(ctx, "invalid number of digits");
    return js_dtoa(ctx, d, 10, p, JS_DTOA_FIXED_FORMAT);
}

static const JSCFunctionListEntry js_number_proto_funcs[] = {
    JS_CFUNC_DEF("toExponential", 1, js_number_toExponential ),
    JS_CFUNC_DEF("toFixed", 1, js_number_toFixed ),
    JS_CFUNC_DEF("toPrecision", 1, js_number_toPrecision ),
    JS_CFUNC_MAGIC_DEF("toString", 1, js_number_toString, 0 ),
    JS_CFUNC_MAGIC_DEF("toLocaleString", 0, js_number_toString, 1 ),
    JS_CFUNC_DEF("valueOf", 0, js_number_valueOf ),
};

static JSValue js_parseInt(JSContext *ctx, JSValueConst this_val,
                           int argc, JSValueConst *argv)
{
    const char *str, *p;
    int radix, flags;
    JSValue ret;

    str = JS_ToCString(ctx, argv[0]);
    if (!str)
        return JS_EXCEPTION;
    if (JS_ToInt32(ctx, &radix, argv[1])) {
        JS_FreeCString(ctx, str);
        return JS_EXCEPTION;
    }
    if (radix != 0 && (radix < 2 || radix > 36)) {
        ret = JS_NAN;
    } else {
        p = str;
        p += skip_spaces(p);
        flags = ATOD_INT_ONLY | ATOD_ACCEPT_PREFIX_AFTER_SIGN;
        ret = js_atof(ctx, p, NULL, radix, flags);
    }
    JS_FreeCString(ctx, str);
    return ret;
}

static JSValue js_parseFloat(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv)
{
    const char *str, *p;
    JSValue ret;

    str = JS_ToCString(ctx, argv[0]);
    if (!str)
        return JS_EXCEPTION;
    p = str;
    p += skip_spaces(p);
    ret = js_atof(ctx, p, NULL, 10, 0);
    JS_FreeCString(ctx, str);
    return ret;
}

/* Boolean */
static JSValue js_boolean_constructor(JSContext *ctx, JSValueConst new_target,
                                     int argc, JSValueConst *argv)
{
    JSValue val, obj;
    val = JS_NewBool(ctx, JS_ToBool(ctx, argv[0]));
    if (!JS_IsUndefined(new_target)) {
        obj = js_create_from_ctor(ctx, new_target, JS_CLASS_BOOLEAN);
        if (!JS_IsException(obj))
            JS_SetObjectData(ctx, obj, val);
        return obj;
    } else {
        return val;
    }
}

static JSValue js_thisBooleanValue(JSContext *ctx, JSValueConst this_val)
{
    if (JS_VALUE_GET_TAG(this_val) == JS_TAG_BOOL)
        return JS_DupValue(ctx, this_val);

    if (JS_VALUE_GET_TAG(this_val) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(this_val);
        if (p->class_id == JS_CLASS_BOOLEAN) {
            if (JS_VALUE_GET_TAG(p->u.object_data) == JS_TAG_BOOL)
                return p->u.object_data;
        }
    }
    return JS_ThrowTypeError(ctx, "not a boolean");
}

static JSValue js_boolean_toString(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv)
{
    JSValue val = js_thisBooleanValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    return JS_AtomToString(ctx, JS_VALUE_GET_BOOL(val) ?
                       JS_ATOM_true : JS_ATOM_false);
}

static JSValue js_boolean_valueOf(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv)
{
    return js_thisBooleanValue(ctx, this_val);
}

static const JSCFunctionListEntry js_boolean_proto_funcs[] = {
    JS_CFUNC_DEF("toString", 0, js_boolean_toString ),
    JS_CFUNC_DEF("valueOf", 0, js_boolean_valueOf ),
};

/* String */

static int js_string_get_own_property(JSContext *ctx,
                                      JSPropertyDescriptor *desc,
                                      JSValueConst obj, JSAtom prop)
{
    JSObject *p;
    JSString *p1;
    uint32_t idx, ch;

    /* This is a class exotic method: obj class_id is JS_CLASS_STRING */
    if (__JS_AtomIsTaggedInt(prop)) {
        p = JS_VALUE_GET_OBJ(obj);
        if (JS_VALUE_GET_TAG(p->u.object_data) == JS_TAG_STRING) {
            p1 = JS_VALUE_GET_STRING(p->u.object_data);
            idx = __JS_AtomToUInt32(prop);
            if (idx < p1->len) {
                if (desc) {
                    if (p1->is_wide_char)
                        ch = p1->u.str16[idx];
                    else
                        ch = p1->u.str8[idx];
                    desc->flags = JS_PROP_ENUMERABLE;
                    desc->value = js_new_string_char(ctx, ch);
                    desc->getter = JS_UNDEFINED;
                    desc->setter = JS_UNDEFINED;
                }
                return TRUE;
            }
        }
    }
    return FALSE;
}

static int js_string_define_own_property(JSContext *ctx,
                                         JSValueConst this_obj,
                                         JSAtom prop, JSValueConst val,
                                         JSValueConst getter,
                                         JSValueConst setter, int flags)
{
    uint32_t idx;
    JSObject *p;
    JSString *p1, *p2;
    
    if (__JS_AtomIsTaggedInt(prop)) {
        idx = __JS_AtomToUInt32(prop);
        p = JS_VALUE_GET_OBJ(this_obj);
        if (JS_VALUE_GET_TAG(p->u.object_data) != JS_TAG_STRING)
            goto def;
        p1 = JS_VALUE_GET_STRING(p->u.object_data);
        if (idx >= p1->len)
            goto def;
        if (!check_define_prop_flags(JS_PROP_ENUMERABLE, flags))
            goto fail;
        /* check that the same value is configured */
        if (flags & JS_PROP_HAS_VALUE) {
            if (JS_VALUE_GET_TAG(val) != JS_TAG_STRING)
                goto fail;
            p2 = JS_VALUE_GET_STRING(val);
            if (p2->len != 1)
                goto fail;
            if (string_get(p1, idx) != string_get(p2, 0)) {
            fail:
                return JS_ThrowTypeErrorOrFalse(ctx, flags, "property is not configurable");
            }
        }
        return TRUE;
    } else {
    def:
        return JS_DefineProperty(ctx, this_obj, prop, val, getter, setter,
                                 flags | JS_PROP_NO_EXOTIC);
    }
}

static int js_string_delete_property(JSContext *ctx,
                                     JSValueConst obj, JSAtom prop)
{
    uint32_t idx;

    if (__JS_AtomIsTaggedInt(prop)) {
        idx = __JS_AtomToUInt32(prop);
        if (idx < js_string_obj_get_length(ctx, obj)) {
            return FALSE;
        }
    }
    return TRUE;
}

static const JSClassExoticMethods js_string_exotic_methods = {
    .get_own_property = js_string_get_own_property,
    .define_own_property = js_string_define_own_property,
    .delete_property = js_string_delete_property,
};

static JSValue js_string_constructor(JSContext *ctx, JSValueConst new_target,
                                     int argc, JSValueConst *argv)
{
    JSValue val, obj;
    if (argc == 0) {
        val = JS_AtomToString(ctx, JS_ATOM_empty_string);
    } else {
        if (JS_IsUndefined(new_target) && JS_IsSymbol(argv[0])) {
            JSAtomStruct *p = JS_VALUE_GET_PTR(argv[0]);
            val = JS_ConcatString3(ctx, "Symbol(", JS_AtomToString(ctx, js_get_atom_index(ctx->rt, p)), ")");
        } else {
            val = JS_ToString(ctx, argv[0]);
        }
        if (JS_IsException(val))
            return val;
    }
    if (!JS_IsUndefined(new_target)) {
        JSString *p1 = JS_VALUE_GET_STRING(val);

        obj = js_create_from_ctor(ctx, new_target, JS_CLASS_STRING);
        if (!JS_IsException(obj)) {
            JS_SetObjectData(ctx, obj, val);
            JS_DefinePropertyValue(ctx, obj, JS_ATOM_length, JS_NewInt32(ctx, p1->len), 0);
        }
        return obj;
    } else {
        return val;
    }
}

static JSValue js_thisStringValue(JSContext *ctx, JSValueConst this_val)
{
    if (JS_VALUE_GET_TAG(this_val) == JS_TAG_STRING)
        return JS_DupValue(ctx, this_val);

    if (JS_VALUE_GET_TAG(this_val) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(this_val);
        if (p->class_id == JS_CLASS_STRING) {
            if (JS_VALUE_GET_TAG(p->u.object_data) == JS_TAG_STRING)
                return JS_DupValue(ctx, p->u.object_data);
        }
    }
    return JS_ThrowTypeError(ctx, "not a string");
}

static JSValue js_string_fromCharCode(JSContext *ctx, JSValueConst this_val,
                                      int argc, JSValueConst *argv)
{
    int i;
    StringBuffer b_s, *b = &b_s;

    string_buffer_init(ctx, b, argc);

    for(i = 0; i < argc; i++) {
        int32_t c;
        if (JS_ToInt32(ctx, &c, argv[i]) || string_buffer_putc16(b, c & 0xffff)) {
            string_buffer_free(b);
            return JS_EXCEPTION;
        }
    }
    return string_buffer_end(b);
}

static JSValue js_string_fromCodePoint(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv)
{
    double d;
    int i, c;
    StringBuffer b_s, *b = &b_s;

    /* XXX: could pre-compute string length if all arguments are JS_TAG_INT */

    if (string_buffer_init(ctx, b, argc))
        goto fail;
    for(i = 0; i < argc; i++) {
        if (JS_VALUE_GET_TAG(argv[i]) == JS_TAG_INT) {
            c = JS_VALUE_GET_INT(argv[i]);
            if (c < 0 || c > 0x10ffff)
                goto range_error;
        } else {
            if (JS_ToFloat64(ctx, &d, argv[i]))
                goto fail;
            if (d < 0 || d > 0x10ffff || (c = (int)d) != d)
                goto range_error;
        }
        if (string_buffer_putc(b, c))
            goto fail;
    }
    return string_buffer_end(b);

 range_error:
    JS_ThrowRangeError(ctx, "invalid code point");
 fail:
    string_buffer_free(b);
    return JS_EXCEPTION;
}

static JSValue js_string_raw(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv)
{
    // raw(temp,...a)
    JSValue cooked, val, raw;
    StringBuffer b_s, *b = &b_s;
    int64_t i, n;

    string_buffer_init(ctx, b, 0);
    raw = JS_UNDEFINED;
    cooked = JS_ToObject(ctx, argv[0]);
    if (JS_IsException(cooked))
        goto exception;
    raw = JS_ToObjectFree(ctx, JS_GetProperty(ctx, cooked, JS_ATOM_raw));
    if (JS_IsException(raw))
        goto exception;
    if (js_get_length64(ctx, &n, raw) < 0)
        goto exception;
        
    for (i = 0; i < n; i++) {
        val = JS_ToStringFree(ctx, JS_GetPropertyInt64(ctx, raw, i));
        if (JS_IsException(val))
            goto exception;
        string_buffer_concat_value_free(b, val);
        if (i < n - 1 && i + 1 < argc) {
            if (string_buffer_concat_value(b, argv[i + 1]))
                goto exception;
        }
    }
    JS_FreeValue(ctx, cooked);
    JS_FreeValue(ctx, raw);
    return string_buffer_end(b);

exception:
    JS_FreeValue(ctx, cooked);
    JS_FreeValue(ctx, raw);
    string_buffer_free(b);
    return JS_EXCEPTION;
}

/* only used in test262 */
JSValue js_string_codePointRange(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    uint32_t start, end, i, n;
    StringBuffer b_s, *b = &b_s;

    if (JS_ToUint32(ctx, &start, argv[0]) ||
        JS_ToUint32(ctx, &end, argv[1]))
        return JS_EXCEPTION;
    end = min_uint32(end, 0x10ffff + 1);

    if (start > end) {
        start = end;
    }
    n = end - start;
    if (end > 0x10000) {
        n += end - max_uint32(start, 0x10000);
    }
    if (string_buffer_init2(ctx, b, n, end >= 0x100))
        return JS_EXCEPTION;
    for(i = start; i < end; i++) {
        string_buffer_putc(b, i);
    }
    return string_buffer_end(b);
}

#if 0
static JSValue js_string___isSpace(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv)
{
    int c;
    if (JS_ToInt32(ctx, &c, argv[0]))
        return JS_EXCEPTION;
    return JS_NewBool(ctx, lre_is_space(c));
}
#endif

static JSValue js_string_charCodeAt(JSContext *ctx, JSValueConst this_val,
                                     int argc, JSValueConst *argv)
{
    JSValue val, ret;
    JSString *p;
    int idx, c;

    val = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(val))
        return val;
    p = JS_VALUE_GET_STRING(val);
    if (JS_ToInt32Sat(ctx, &idx, argv[0])) {
        JS_FreeValue(ctx, val);
        return JS_EXCEPTION;
    }
    if (idx < 0 || idx >= p->len) {
        ret = JS_NAN;
    } else {
        if (p->is_wide_char)
            c = p->u.str16[idx];
        else
            c = p->u.str8[idx];
        ret = JS_NewInt32(ctx, c);
    }
    JS_FreeValue(ctx, val);
    return ret;
}

static JSValue js_string_charAt(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    JSValue val, ret;
    JSString *p;
    int idx, c;

    val = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(val))
        return val;
    p = JS_VALUE_GET_STRING(val);
    if (JS_ToInt32Sat(ctx, &idx, argv[0])) {
        JS_FreeValue(ctx, val);
        return JS_EXCEPTION;
    }
    if (idx < 0 || idx >= p->len) {
        ret = js_new_string8(ctx, NULL, 0);
    } else {
        if (p->is_wide_char)
            c = p->u.str16[idx];
        else
            c = p->u.str8[idx];
        ret = js_new_string_char(ctx, c);
    }
    JS_FreeValue(ctx, val);
    return ret;
}

static JSValue js_string_codePointAt(JSContext *ctx, JSValueConst this_val,
                                     int argc, JSValueConst *argv)
{
    JSValue val, ret;
    JSString *p;
    int idx, c;

    val = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(val))
        return val;
    p = JS_VALUE_GET_STRING(val);
    if (JS_ToInt32Sat(ctx, &idx, argv[0])) {
        JS_FreeValue(ctx, val);
        return JS_EXCEPTION;
    }
    if (idx < 0 || idx >= p->len) {
        ret = JS_UNDEFINED;
    } else {
        c = string_getc(p, &idx);
        ret = JS_NewInt32(ctx, c);
    }
    JS_FreeValue(ctx, val);
    return ret;
}

static JSValue js_string_concat(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    JSValue r;
    int i;

    /* XXX: Use more efficient method */
    /* XXX: This method is OK if r has a single refcount */
    /* XXX: should use string_buffer? */
    r = JS_ToStringCheckObject(ctx, this_val);
    for (i = 0; i < argc; i++) {
        if (JS_IsException(r))
            break;
        r = JS_ConcatString(ctx, r, JS_DupValue(ctx, argv[i]));
    }
    return r;
}

static int string_cmp(JSString *p1, JSString *p2, int x1, int x2, int len)
{
    int i, c1, c2;
    for (i = 0; i < len; i++) {
        if ((c1 = string_get(p1, x1 + i)) != (c2 = string_get(p2, x2 + i)))
            return c1 - c2;
    }
    return 0;
}

static int string_indexof_char(JSString *p, int c, int from)
{
    /* assuming 0 <= from <= p->len */
    int i, len = p->len;
    if (p->is_wide_char) {
        for (i = from; i < len; i++) {
            if (p->u.str16[i] == c)
                return i;
        }
    } else {
        if ((c & ~0xff) == 0) {
            for (i = from; i < len; i++) {
                if (p->u.str8[i] == (uint8_t)c)
                    return i;
            }
        }
    }
    return -1;
}

static int string_indexof(JSString *p1, JSString *p2, int from)
{
    /* assuming 0 <= from <= p1->len */
    int c, i, j, len1 = p1->len, len2 = p2->len;
    if (len2 == 0)
        return from;
    for (i = from, c = string_get(p2, 0); i + len2 <= len1; i = j + 1) {
        j = string_indexof_char(p1, c, i);
        if (j < 0 || j + len2 > len1)
            break;
        if (!string_cmp(p1, p2, j + 1, 1, len2 - 1))
            return j;
    }
    return -1;
}

static int64_t string_advance_index(JSString *p, int64_t index, BOOL unicode)
{
    if (!unicode || index >= p->len || !p->is_wide_char) {
        index++;
    } else {
        int index32 = (int)index;
        string_getc(p, &index32);
        index = index32;
    }
    return index;
}

static JSValue js_string_indexOf(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv, int lastIndexOf)
{
    JSValue str, v;
    int i, len, v_len, pos, start, stop, ret, inc;
    JSString *p;
    JSString *p1;

    str = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(str))
        return str;
    v = JS_ToString(ctx, argv[0]);
    if (JS_IsException(v))
        goto fail;
    p = JS_VALUE_GET_STRING(str);
    p1 = JS_VALUE_GET_STRING(v);
    len = p->len;
    v_len = p1->len;
    if (lastIndexOf) {
        pos = len - v_len;
        if (argc > 1) {
            double d;
            if (JS_ToFloat64(ctx, &d, argv[1]))
                goto fail;
            if (!isnan(d)) {
                if (d <= 0)
                    pos = 0;
                else if (d < pos)
                    pos = d;
            }
        }
        start = pos;
        stop = 0;
        inc = -1;
    } else {
        pos = 0;
        if (argc > 1) {
            if (JS_ToInt32Clamp(ctx, &pos, argv[1], 0, len, 0))
                goto fail;
        }
        start = pos;
        stop = len - v_len;
        inc = 1;
    }
    ret = -1;
    if (len >= v_len && inc * (stop - start) >= 0) {
        for (i = start;; i += inc) {
            if (!string_cmp(p, p1, i, 0, v_len)) {
                ret = i;
                break;
            }
            if (i == stop)
                break;
        }
    }
    JS_FreeValue(ctx, str);
    JS_FreeValue(ctx, v);
    return JS_NewInt32(ctx, ret);

fail:
    JS_FreeValue(ctx, str);
    JS_FreeValue(ctx, v);
    return JS_EXCEPTION;
}

/* return < 0 if exception or TRUE/FALSE */
static int js_is_regexp(JSContext *ctx, JSValueConst obj);

static JSValue js_string_includes(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv, int magic)
{
    JSValue str, v = JS_UNDEFINED;
    int i, len, v_len, pos, start, stop, ret;
    JSString *p;
    JSString *p1;

    str = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(str))
        return str;
    ret = js_is_regexp(ctx, argv[0]);
    if (ret) {
        if (ret > 0)
            JS_ThrowTypeError(ctx, "regex not supported");
        goto fail;
    }
    v = JS_ToString(ctx, argv[0]);
    if (JS_IsException(v))
        goto fail;
    p = JS_VALUE_GET_STRING(str);
    p1 = JS_VALUE_GET_STRING(v);
    len = p->len;
    v_len = p1->len;
    pos = (magic == 2) ? len : 0;
    if (argc > 1 && !JS_IsUndefined(argv[1])) {
        if (JS_ToInt32Clamp(ctx, &pos, argv[1], 0, len, 0))
            goto fail;
    }
    len -= v_len;
    ret = 0;
    if (magic == 0) {
        start = pos;
        stop = len;
    } else {
        if (magic == 1) {
            if (pos > len)
                goto done;
        } else {
            pos -= v_len;
        }
        start = stop = pos;
    }
    if (start >= 0 && start <= stop) {
        for (i = start;; i++) {
            if (!string_cmp(p, p1, i, 0, v_len)) {
                ret = 1;
                break;
            }
            if (i == stop)
                break;
        }
    }
 done:
    JS_FreeValue(ctx, str);
    JS_FreeValue(ctx, v);
    return JS_NewBool(ctx, ret);

fail:
    JS_FreeValue(ctx, str);
    JS_FreeValue(ctx, v);
    return JS_EXCEPTION;
}

static int check_regexp_g_flag(JSContext *ctx, JSValueConst regexp)
{
    int ret;
    JSValue flags;
    
    ret = js_is_regexp(ctx, regexp);
    if (ret < 0)
        return -1;
    if (ret) {
        flags = JS_GetProperty(ctx, regexp, JS_ATOM_flags);
        if (JS_IsException(flags))
            return -1;
        if (JS_IsUndefined(flags) || JS_IsNull(flags)) {
            JS_ThrowTypeError(ctx, "cannot convert to object");
            return -1;
        }
        flags = JS_ToStringFree(ctx, flags);
        if (JS_IsException(flags))
            return -1;
        ret = string_indexof_char(JS_VALUE_GET_STRING(flags), 'g', 0);
        JS_FreeValue(ctx, flags);
        if (ret < 0) {
            JS_ThrowTypeError(ctx, "regexp must have the 'g' flag");
            return -1;
        }
    }
    return 0;
}

static JSValue js_string_match(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv, int atom)
{
    // match(rx), search(rx), matchAll(rx)
    // atom is JS_ATOM_Symbol_match, JS_ATOM_Symbol_search, or JS_ATOM_Symbol_matchAll
    JSValueConst O = this_val, regexp = argv[0], args[2];
    JSValue matcher, S, rx, result, str;
    int args_len;

    if (JS_IsUndefined(O) || JS_IsNull(O))
        return JS_ThrowTypeError(ctx, "cannot convert to object");

    if (!JS_IsUndefined(regexp) && !JS_IsNull(regexp)) {
        matcher = JS_GetProperty(ctx, regexp, atom);
        if (JS_IsException(matcher))
            return JS_EXCEPTION;
        if (atom == JS_ATOM_Symbol_matchAll) {
            if (check_regexp_g_flag(ctx, regexp) < 0) {
                JS_FreeValue(ctx, matcher);
                return JS_EXCEPTION;
            }
        }
        if (!JS_IsUndefined(matcher) && !JS_IsNull(matcher)) {
            return JS_CallFree(ctx, matcher, regexp, 1, &O);
        }
    }
    S = JS_ToString(ctx, O);
    if (JS_IsException(S))
        return JS_EXCEPTION;
    args_len = 1;
    args[0] = regexp;
    str = JS_UNDEFINED;
    if (atom == JS_ATOM_Symbol_matchAll) {
        str = JS_NewString(ctx, "g");
        if (JS_IsException(str))
            goto fail;
        args[args_len++] = (JSValueConst)str;
    }
    rx = JS_CallConstructor(ctx, ctx->regexp_ctor, args_len, args);
    JS_FreeValue(ctx, str);
    if (JS_IsException(rx)) {
    fail:
        JS_FreeValue(ctx, S);
        return JS_EXCEPTION;
    }
    result = JS_InvokeFree(ctx, rx, atom, 1, (JSValueConst *)&S);
    JS_FreeValue(ctx, S);
    return result;
}

static JSValue js_string___GetSubstitution(JSContext *ctx, JSValueConst this_val,
                                           int argc, JSValueConst *argv)
{
    // GetSubstitution(matched, str, position, captures, namedCaptures, rep)
    JSValueConst matched, str, captures, namedCaptures, rep;
    JSValue capture, name, s;
    uint32_t position, len, matched_len, captures_len;
    int i, j, j0, k, k1;
    int c, c1;
    StringBuffer b_s, *b = &b_s;
    JSString *sp, *rp;

    matched = argv[0];
    str = argv[1];
    captures = argv[3];
    namedCaptures = argv[4];
    rep = argv[5];

    if (!JS_IsString(rep) || !JS_IsString(str))
        return JS_ThrowTypeError(ctx, "not a string");

    sp = JS_VALUE_GET_STRING(str);
    rp = JS_VALUE_GET_STRING(rep);

    string_buffer_init(ctx, b, 0);

    captures_len = 0;
    if (!JS_IsUndefined(captures)) {
        if (js_get_length32(ctx, &captures_len, captures))
            goto exception;
    }
    if (js_get_length32(ctx, &matched_len, matched))
        goto exception;
    if (JS_ToUint32(ctx, &position, argv[2]) < 0)
        goto exception;

    len = rp->len;
    i = 0;
    for(;;) {
        j = string_indexof_char(rp, '$', i);
        if (j < 0 || j + 1 >= len)
            break;
        string_buffer_concat(b, rp, i, j);
        j0 = j++;
        c = string_get(rp, j++);
        if (c == '$') {
            string_buffer_putc8(b, '$');
        } else if (c == '&') {
            if (string_buffer_concat_value(b, matched))
                goto exception;
        } else if (c == '`') {
            string_buffer_concat(b, sp, 0, position);
        } else if (c == '\'') {
            string_buffer_concat(b, sp, position + matched_len, sp->len);
        } else if (c >= '0' && c <= '9') {
            k = c - '0';
            if (j < len) {
                c1 = string_get(rp, j);
                if (c1 >= '0' && c1 <= '9') {
                    /* This behavior is specified in ES6 and refined in ECMA 2019 */
                    /* ECMA 2019 does not have the extra test, but
                       Test262 S15.5.4.11_A3_T1..3 require this behavior */
                    k1 = k * 10 + c1 - '0';
                    if (k1 >= 1 && k1 < captures_len) {
                        k = k1;
                        j++;
                    }
                }
            }
            if (k >= 1 && k < captures_len) {
                s = JS_GetPropertyInt64(ctx, captures, k);
                if (JS_IsException(s))
                    goto exception;
                if (!JS_IsUndefined(s)) {
                    if (string_buffer_concat_value_free(b, s))
                        goto exception;
                }
            } else {
                goto norep;
            }
        } else if (c == '<' && !JS_IsUndefined(namedCaptures)) {
            k = string_indexof_char(rp, '>', j);
            if (k < 0)
                goto norep;
            name = js_sub_string(ctx, rp, j, k);
            if (JS_IsException(name))
                goto exception;
            capture = JS_GetPropertyValue(ctx, namedCaptures, name);
            if (JS_IsException(capture))
                goto exception;
            if (!JS_IsUndefined(capture)) {
                if (string_buffer_concat_value_free(b, capture))
                    goto exception;
            }
            j = k + 1;
        } else {
        norep:
            string_buffer_concat(b, rp, j0, j);
        }
        i = j;
    }
    string_buffer_concat(b, rp, i, rp->len);
    return string_buffer_end(b);
exception:
    string_buffer_free(b);
    return JS_EXCEPTION;
}

static JSValue js_string_replace(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv,
                                 int is_replaceAll)
{
    // replace(rx, rep)
    JSValueConst O = this_val, searchValue = argv[0], replaceValue = argv[1];
    JSValueConst args[6];
    JSValue str, search_str, replaceValue_str, repl_str;
    JSString *sp, *searchp;
    StringBuffer b_s, *b = &b_s;
    int pos, functionalReplace, endOfLastMatch;
    BOOL is_first;

    if (JS_IsUndefined(O) || JS_IsNull(O))
        return JS_ThrowTypeError(ctx, "cannot convert to object");

    search_str = JS_UNDEFINED;
    replaceValue_str = JS_UNDEFINED;
    repl_str = JS_UNDEFINED;

    if (!JS_IsUndefined(searchValue) && !JS_IsNull(searchValue)) {
        JSValue replacer;
        if (is_replaceAll) {
            if (check_regexp_g_flag(ctx, searchValue) < 0)
                return JS_EXCEPTION;
        }
        replacer = JS_GetProperty(ctx, searchValue, JS_ATOM_Symbol_replace);
        if (JS_IsException(replacer))
            return JS_EXCEPTION;
        if (!JS_IsUndefined(replacer) && !JS_IsNull(replacer)) {
            args[0] = O;
            args[1] = replaceValue;
            return JS_CallFree(ctx, replacer, searchValue, 2, args);
        }
    }
    string_buffer_init(ctx, b, 0);

    str = JS_ToString(ctx, O);
    if (JS_IsException(str))
        goto exception;
    search_str = JS_ToString(ctx, searchValue);
    if (JS_IsException(search_str))
        goto exception;
    functionalReplace = JS_IsFunction(ctx, replaceValue);
    if (!functionalReplace) {
        replaceValue_str = JS_ToString(ctx, replaceValue);
        if (JS_IsException(replaceValue_str))
            goto exception;
    }

    sp = JS_VALUE_GET_STRING(str);
    searchp = JS_VALUE_GET_STRING(search_str);
    endOfLastMatch = 0;
    is_first = TRUE;
    for(;;) {
        if (unlikely(searchp->len == 0)) {
            if (is_first)
                pos = 0;
            else if (endOfLastMatch >= sp->len)
                pos = -1;
            else
                pos = endOfLastMatch + 1;
        } else {
            pos = string_indexof(sp, searchp, endOfLastMatch);
        }
        if (pos < 0) {
            if (is_first) {
                string_buffer_free(b);
                JS_FreeValue(ctx, search_str);
                JS_FreeValue(ctx, replaceValue_str);
                return str;
            } else {
                break;
            }
        }
        if (functionalReplace) {
            args[0] = search_str;
            args[1] = JS_NewInt32(ctx, pos);
            args[2] = str;
            repl_str = JS_ToStringFree(ctx, JS_Call(ctx, replaceValue, JS_UNDEFINED, 3, args));
        } else {
            args[0] = search_str;
            args[1] = str;
            args[2] = JS_NewInt32(ctx, pos);
            args[3] = JS_UNDEFINED;
            args[4] = JS_UNDEFINED;
            args[5] = replaceValue_str;
            repl_str = js_string___GetSubstitution(ctx, JS_UNDEFINED, 6, args);
        }
        if (JS_IsException(repl_str))
            goto exception;
        
        string_buffer_concat(b, sp, endOfLastMatch, pos);
        string_buffer_concat_value_free(b, repl_str);
        endOfLastMatch = pos + searchp->len;
        is_first = FALSE;
        if (!is_replaceAll)
            break;
    }
    string_buffer_concat(b, sp, endOfLastMatch, sp->len);
    JS_FreeValue(ctx, search_str);
    JS_FreeValue(ctx, replaceValue_str);
    JS_FreeValue(ctx, str);
    return string_buffer_end(b);

exception:
    string_buffer_free(b);
    JS_FreeValue(ctx, search_str);
    JS_FreeValue(ctx, replaceValue_str);
    JS_FreeValue(ctx, str);
    return JS_EXCEPTION;
}

static JSValue js_string_split(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv)
{
    // split(sep, limit)
    JSValueConst O = this_val, separator = argv[0], limit = argv[1];
    JSValueConst args[2];
    JSValue S, A, R, T;
    uint32_t lim, lengthA;
    int64_t p, q, s, r, e;
    JSString *sp, *rp;

    if (JS_IsUndefined(O) || JS_IsNull(O))
        return JS_ThrowTypeError(ctx, "cannot convert to object");

    S = JS_UNDEFINED;
    A = JS_UNDEFINED;
    R = JS_UNDEFINED;

    if (!JS_IsUndefined(separator) && !JS_IsNull(separator)) {
        JSValue splitter;
        splitter = JS_GetProperty(ctx, separator, JS_ATOM_Symbol_split);
        if (JS_IsException(splitter))
            return JS_EXCEPTION;
        if (!JS_IsUndefined(splitter) && !JS_IsNull(splitter)) {
            args[0] = O;
            args[1] = limit;
            return JS_CallFree(ctx, splitter, separator, 2, args);
        }
    }
    S = JS_ToString(ctx, O);
    if (JS_IsException(S))
        goto exception;
    A = JS_NewArray(ctx);
    if (JS_IsException(A))
        goto exception;
    lengthA = 0;
    if (JS_IsUndefined(limit)) {
        lim = 0xffffffff;
    } else {
        if (JS_ToUint32(ctx, &lim, limit) < 0)
            goto exception;
    }
    sp = JS_VALUE_GET_STRING(S);
    s = sp->len;
    R = JS_ToString(ctx, separator);
    if (JS_IsException(R))
        goto exception;
    rp = JS_VALUE_GET_STRING(R);
    r = rp->len;
    p = 0;
    if (lim == 0)
        goto done;
    if (JS_IsUndefined(separator))
        goto add_tail;
    if (s == 0) {
        if (r != 0)
            goto add_tail;
        goto done;
    }
    q = p;
    for (q = p; (q += !r) <= s - r - !r; q = p = e + r) {
        e = string_indexof(sp, rp, q);
        if (e < 0)
            break;
        T = js_sub_string(ctx, sp, p, e);
        if (JS_IsException(T))
            goto exception;
        if (JS_CreateDataPropertyUint32(ctx, A, lengthA++, T, 0) < 0)
            goto exception;
        if (lengthA == lim)
            goto done;
    }
add_tail:
    T = js_sub_string(ctx, sp, p, s);
    if (JS_IsException(T))
        goto exception;
    if (JS_CreateDataPropertyUint32(ctx, A, lengthA++, T,0 ) < 0)
        goto exception;
done:
    JS_FreeValue(ctx, S);
    JS_FreeValue(ctx, R);
    return A;

exception:
    JS_FreeValue(ctx, A);
    JS_FreeValue(ctx, S);
    JS_FreeValue(ctx, R);
    return JS_EXCEPTION;
}

static JSValue js_string_substring(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv)
{
    JSValue str, ret;
    int a, b, start, end;
    JSString *p;

    str = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(str))
        return str;
    p = JS_VALUE_GET_STRING(str);
    if (JS_ToInt32Clamp(ctx, &a, argv[0], 0, p->len, 0)) {
        JS_FreeValue(ctx, str);
        return JS_EXCEPTION;
    }
    b = p->len;
    if (!JS_IsUndefined(argv[1])) {
        if (JS_ToInt32Clamp(ctx, &b, argv[1], 0, p->len, 0)) {
            JS_FreeValue(ctx, str);
            return JS_EXCEPTION;
        }
    }
    if (a < b) {
        start = a;
        end = b;
    } else {
        start = b;
        end = a;
    }
    ret = js_sub_string(ctx, p, start, end);
    JS_FreeValue(ctx, str);
    return ret;
}

static JSValue js_string_substr(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    JSValue str, ret;
    int a, len, n;
    JSString *p;

    str = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(str))
        return str;
    p = JS_VALUE_GET_STRING(str);
    len = p->len;
    if (JS_ToInt32Clamp(ctx, &a, argv[0], 0, len, len)) {
        JS_FreeValue(ctx, str);
        return JS_EXCEPTION;
    }
    n = len - a;
    if (!JS_IsUndefined(argv[1])) {
        if (JS_ToInt32Clamp(ctx, &n, argv[1], 0, len - a, 0)) {
            JS_FreeValue(ctx, str);
            return JS_EXCEPTION;
        }
    }
    ret = js_sub_string(ctx, p, a, a + n);
    JS_FreeValue(ctx, str);
    return ret;
}

static JSValue js_string_slice(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv)
{
    JSValue str, ret;
    int len, start, end;
    JSString *p;

    str = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(str))
        return str;
    p = JS_VALUE_GET_STRING(str);
    len = p->len;
    if (JS_ToInt32Clamp(ctx, &start, argv[0], 0, len, len)) {
        JS_FreeValue(ctx, str);
        return JS_EXCEPTION;
    }
    end = len;
    if (!JS_IsUndefined(argv[1])) {
        if (JS_ToInt32Clamp(ctx, &end, argv[1], 0, len, len)) {
            JS_FreeValue(ctx, str);
            return JS_EXCEPTION;
        }
    }
    ret = js_sub_string(ctx, p, start, max_int(end, start));
    JS_FreeValue(ctx, str);
    return ret;
}

static JSValue js_string_pad(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv, int padEnd)
{
    JSValue str, v = JS_UNDEFINED;
    StringBuffer b_s, *b = &b_s;
    JSString *p, *p1 = NULL;
    int n, len, c = ' ';

    str = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(str))
        goto fail1;
    if (JS_ToInt32Sat(ctx, &n, argv[0]))
        goto fail2;
    p = JS_VALUE_GET_STRING(str);
    len = p->len;
    if (len >= n)
        return str;
    if (argc > 1 && !JS_IsUndefined(argv[1])) {
        v = JS_ToString(ctx, argv[1]);
        if (JS_IsException(v))
            goto fail2;
        p1 = JS_VALUE_GET_STRING(v);
        if (p1->len == 0) {
            JS_FreeValue(ctx, v);
            return str;
        }
        if (p1->len == 1) {
            c = string_get(p1, 0);
            p1 = NULL;
        }
    }
    if (n > JS_STRING_LEN_MAX) {
        JS_ThrowInternalError(ctx, "string too long");
        goto fail2;
    }
    if (string_buffer_init(ctx, b, n))
        goto fail3;
    n -= len;
    if (padEnd) {
        if (string_buffer_concat(b, p, 0, len))
            goto fail;
    }
    if (p1) {
        while (n > 0) {
            int chunk = min_int(n, p1->len);
            if (string_buffer_concat(b, p1, 0, chunk))
                goto fail;
            n -= chunk;
        }
    } else {
        if (string_buffer_fill(b, c, n))
            goto fail;
    }
    if (!padEnd) {
        if (string_buffer_concat(b, p, 0, len))
            goto fail;
    }
    JS_FreeValue(ctx, v);
    JS_FreeValue(ctx, str);
    return string_buffer_end(b);

fail:
    string_buffer_free(b);
fail3:
    JS_FreeValue(ctx, v);
fail2:
    JS_FreeValue(ctx, str);
fail1:
    return JS_EXCEPTION;
}

static JSValue js_string_repeat(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    JSValue str;
    StringBuffer b_s, *b = &b_s;
    JSString *p;
    int64_t val;
    int n, len;

    str = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(str))
        goto fail;
    if (JS_ToInt64Sat(ctx, &val, argv[0]))
        goto fail;
    if (val < 0 || val > 2147483647) {
        JS_ThrowRangeError(ctx, "invalid repeat count");
        goto fail;
    }
    n = val;
    p = JS_VALUE_GET_STRING(str);
    len = p->len;
    if (len == 0 || n == 1)
        return str;
    if (val * len > JS_STRING_LEN_MAX) {
        JS_ThrowInternalError(ctx, "string too long");
        goto fail;
    }
    if (string_buffer_init2(ctx, b, n * len, p->is_wide_char))
        goto fail;
    if (len == 1) {
        string_buffer_fill(b, string_get(p, 0), n);
    } else {
        while (n-- > 0) {
            string_buffer_concat(b, p, 0, len);
        }
    }
    JS_FreeValue(ctx, str);
    return string_buffer_end(b);

fail:
    JS_FreeValue(ctx, str);
    return JS_EXCEPTION;
}

static JSValue js_string_trim(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv, int magic)
{
    JSValue str, ret;
    int a, b, len;
    JSString *p;

    str = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(str))
        return str;
    p = JS_VALUE_GET_STRING(str);
    a = 0;
    b = len = p->len;
    if (magic & 1) {
        while (a < len && lre_is_space(string_get(p, a)))
            a++;
    }
    if (magic & 2) {
        while (b > a && lre_is_space(string_get(p, b - 1)))
            b--;
    }
    ret = js_sub_string(ctx, p, a, b);
    JS_FreeValue(ctx, str);
    return ret;
}

static JSValue js_string___quote(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    return JS_ToQuotedString(ctx, this_val);
}

/* return 0 if before the first char */
static int string_prevc(JSString *p, int *pidx)
{
    int idx, c, c1;

    idx = *pidx;
    if (idx <= 0)
        return 0;
    idx--;
    if (p->is_wide_char) {
        c = p->u.str16[idx];
        if (c >= 0xdc00 && c < 0xe000 && idx > 0) {
            c1 = p->u.str16[idx - 1];
            if (c1 >= 0xd800 && c1 <= 0xdc00) {
                c = (((c1 & 0x3ff) << 10) | (c & 0x3ff)) + 0x10000;
                idx--;
            }
        }
    } else {
        c = p->u.str8[idx];
    }
    *pidx = idx;
    return c;
}

static BOOL test_final_sigma(JSString *p, int sigma_pos)
{
    int k, c1;

    /* before C: skip case ignorable chars and check there is
       a cased letter */
    k = sigma_pos;
    for(;;) {
        c1 = string_prevc(p, &k);
        if (!lre_is_case_ignorable(c1))
            break;
    }
    if (!lre_is_cased(c1))
        return FALSE;

    /* after C: skip case ignorable chars and check there is
       no cased letter */
    k = sigma_pos + 1;
    for(;;) {
        if (k >= p->len)
            return TRUE;
        c1 = string_getc(p, &k);
        if (!lre_is_case_ignorable(c1))
            break;
    }
    return !lre_is_cased(c1);
}

static JSValue js_string_localeCompare(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv)
{
    JSValue a, b;
    int cmp;

    a = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(a))
        return JS_EXCEPTION;
    b = JS_ToString(ctx, argv[0]);
    if (JS_IsException(b)) {
        JS_FreeValue(ctx, a);
        return JS_EXCEPTION;
    }
    cmp = js_string_compare(ctx, JS_VALUE_GET_STRING(a), JS_VALUE_GET_STRING(b));
    JS_FreeValue(ctx, a);
    JS_FreeValue(ctx, b);
    return JS_NewInt32(ctx, cmp);
}

static JSValue js_string_toLowerCase(JSContext *ctx, JSValueConst this_val,
                                     int argc, JSValueConst *argv, int to_lower)
{
    JSValue val;
    StringBuffer b_s, *b = &b_s;
    JSString *p;
    int i, c, j, l;
    uint32_t res[LRE_CC_RES_LEN_MAX];

    val = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(val))
        return val;
    p = JS_VALUE_GET_STRING(val);
    if (p->len == 0)
        return val;
    if (string_buffer_init(ctx, b, p->len))
        goto fail;
    for(i = 0; i < p->len;) {
        c = string_getc(p, &i);
        if (c == 0x3a3 && to_lower && test_final_sigma(p, i - 1)) {
            res[0] = 0x3c2; /* final sigma */
            l = 1;
        } else {
            l = lre_case_conv(res, c, to_lower);
        }
        for(j = 0; j < l; j++) {
            if (string_buffer_putc(b, res[j]))
                goto fail;
        }
    }
    JS_FreeValue(ctx, val);
    return string_buffer_end(b);
 fail:
    JS_FreeValue(ctx, val);
    string_buffer_free(b);
    return JS_EXCEPTION;
}

#ifdef CONFIG_ALL_UNICODE

/* return (-1, NULL) if exception, otherwise (len, buf) */
static int JS_ToUTF32String(JSContext *ctx, uint32_t **pbuf, JSValueConst val1)
{
    JSValue val;
    JSString *p;
    uint32_t *buf;
    int i, j, len;

    val = JS_ToString(ctx, val1);
    if (JS_IsException(val))
        return -1;
    p = JS_VALUE_GET_STRING(val);
    len = p->len;
    /* UTF32 buffer length is len minus the number of correct surrogates pairs */
    buf = js_malloc(ctx, sizeof(buf[0]) * max_int(len, 1));
    if (!buf) {
        JS_FreeValue(ctx, val);
        goto fail;
    }
    for(i = j = 0; i < len;)
        buf[j++] = string_getc(p, &i);
    JS_FreeValue(ctx, val);
    *pbuf = buf;
    return j;
 fail:
    *pbuf = NULL;
    return -1;
}

static JSValue JS_NewUTF32String(JSContext *ctx, const uint32_t *buf, int len)
{
    int i;
    StringBuffer b_s, *b = &b_s;
    if (string_buffer_init(ctx, b, len))
        return JS_EXCEPTION;
    for(i = 0; i < len; i++) {
        if (string_buffer_putc(b, buf[i]))
            goto fail;
    }
    return string_buffer_end(b);
 fail:
    string_buffer_free(b);
    return JS_EXCEPTION;
}

static JSValue js_string_normalize(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv)
{
    const char *form, *p;
    size_t form_len;
    int is_compat, buf_len, out_len;
    UnicodeNormalizationEnum n_type;
    JSValue val;
    uint32_t *buf, *out_buf;

    val = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(val))
        return val;
    buf_len = JS_ToUTF32String(ctx, &buf, val);
    JS_FreeValue(ctx, val);
    if (buf_len < 0)
        return JS_EXCEPTION;

    if (argc == 0 || JS_IsUndefined(argv[0])) {
        n_type = UNICODE_NFC;
    } else {
        form = JS_ToCStringLen(ctx, &form_len, argv[0]);
        if (!form)
            goto fail1;
        p = form;
        if (p[0] != 'N' || p[1] != 'F')
            goto bad_form;
        p += 2;
        is_compat = FALSE;
        if (*p == 'K') {
            is_compat = TRUE;
            p++;
        }
        if (*p == 'C' || *p == 'D') {
            n_type = UNICODE_NFC + is_compat * 2 + (*p - 'C');
            if ((p + 1 - form) != form_len)
                goto bad_form;
        } else {
        bad_form:
            JS_FreeCString(ctx, form);
            JS_ThrowRangeError(ctx, "bad normalization form");
        fail1:
            js_free(ctx, buf);
            return JS_EXCEPTION;
        }
        JS_FreeCString(ctx, form);
    }

    out_len = unicode_normalize(&out_buf, buf, buf_len, n_type,
                                ctx->rt, (DynBufReallocFunc *)js_realloc_rt);
    js_free(ctx, buf);
    if (out_len < 0)
        return JS_EXCEPTION;
    val = JS_NewUTF32String(ctx, out_buf, out_len);
    js_free(ctx, out_buf);
    return val;
}
#endif /* CONFIG_ALL_UNICODE */

/* also used for String.prototype.valueOf */
static JSValue js_string_toString(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv)
{
    return js_thisStringValue(ctx, this_val);
}

#if 0
static JSValue js_string___toStringCheckObject(JSContext *ctx, JSValueConst this_val,
                                               int argc, JSValueConst *argv)
{
    return JS_ToStringCheckObject(ctx, argv[0]);
}

static JSValue js_string___toString(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv)
{
    return JS_ToString(ctx, argv[0]);
}

static JSValue js_string___advanceStringIndex(JSContext *ctx, JSValueConst
                                              this_val,
                                              int argc, JSValueConst *argv)
{
    JSValue str;
    int idx;
    BOOL is_unicode;
    JSString *p;

    str = JS_ToString(ctx, argv[0]);
    if (JS_IsException(str))
        return str;
    if (JS_ToInt32Sat(ctx, &idx, argv[1])) {
        JS_FreeValue(ctx, str);
        return JS_EXCEPTION;
    }
    is_unicode = JS_ToBool(ctx, argv[2]);
    p = JS_VALUE_GET_STRING(str);
    if (!is_unicode || (unsigned)idx >= p->len || !p->is_wide_char) {
        idx++;
    } else {
        string_getc(p, &idx);
    }
    JS_FreeValue(ctx, str);
    return JS_NewInt32(ctx, idx);
}
#endif

/* String Iterator */

static JSValue js_string_iterator_next(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv,
                                       BOOL *pdone, int magic)
{
    JSArrayIteratorData *it;
    uint32_t idx, c, start;
    JSString *p;

    it = JS_GetOpaque2(ctx, this_val, JS_CLASS_STRING_ITERATOR);
    if (!it) {
        *pdone = FALSE;
        return JS_EXCEPTION;
    }
    if (JS_IsUndefined(it->obj))
        goto done;
    p = JS_VALUE_GET_STRING(it->obj);
    idx = it->idx;
    if (idx >= p->len) {
        JS_FreeValue(ctx, it->obj);
        it->obj = JS_UNDEFINED;
    done:
        *pdone = TRUE;
        return JS_UNDEFINED;
    }

    start = idx;
    c = string_getc(p, (int *)&idx);
    it->idx = idx;
    *pdone = FALSE;
    if (c <= 0xffff) {
        return js_new_string_char(ctx, c);
    } else {
        return js_new_string16(ctx, p->u.str16 + start, 2);
    }
}

/* ES6 Annex B 2.3.2 etc. */
enum {
    magic_string_anchor,
    magic_string_big,
    magic_string_blink,
    magic_string_bold,
    magic_string_fixed,
    magic_string_fontcolor,
    magic_string_fontsize,
    magic_string_italics,
    magic_string_link,
    magic_string_small,
    magic_string_strike,
    magic_string_sub,
    magic_string_sup,
};

static JSValue js_string_CreateHTML(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv, int magic)
{
    JSValue str;
    const JSString *p;
    StringBuffer b_s, *b = &b_s;
    static struct { const char *tag, *attr; } const defs[] = {
        { "a", "name" }, { "big", NULL }, { "blink", NULL }, { "b", NULL },
        { "tt", NULL }, { "font", "color" }, { "font", "size" }, { "i", NULL },
        { "a", "href" }, { "small", NULL }, { "strike", NULL }, 
        { "sub", NULL }, { "sup", NULL },
    };

    str = JS_ToStringCheckObject(ctx, this_val);
    if (JS_IsException(str))
        return JS_EXCEPTION;
    string_buffer_init(ctx, b, 7);
    string_buffer_putc8(b, '<');
    string_buffer_puts8(b, defs[magic].tag);
    if (defs[magic].attr) {
        // r += " " + attr + "=\"" + value + "\"";
        JSValue value;
        int i;

        string_buffer_putc8(b, ' ');
        string_buffer_puts8(b, defs[magic].attr);
        string_buffer_puts8(b, "=\"");
        value = JS_ToStringCheckObject(ctx, argv[0]);
        if (JS_IsException(value)) {
            JS_FreeValue(ctx, str);
            string_buffer_free(b);
            return JS_EXCEPTION;
        }
        p = JS_VALUE_GET_STRING(value);
        for (i = 0; i < p->len; i++) {
            int c = string_get(p, i);
            if (c == '"') {
                string_buffer_puts8(b, "&quot;");
            } else {
                string_buffer_putc16(b, c);
            }
        }
        JS_FreeValue(ctx, value);
        string_buffer_putc8(b, '\"');
    }
    // return r + ">" + str + "</" + tag + ">";
    string_buffer_putc8(b, '>');
    string_buffer_concat_value_free(b, str);
    string_buffer_puts8(b, "</");
    string_buffer_puts8(b, defs[magic].tag);
    string_buffer_putc8(b, '>');
    return string_buffer_end(b);
}

static const JSCFunctionListEntry js_string_funcs[] = {
    JS_CFUNC_DEF("fromCharCode", 1, js_string_fromCharCode ),
    JS_CFUNC_DEF("fromCodePoint", 1, js_string_fromCodePoint ),
    JS_CFUNC_DEF("raw", 1, js_string_raw ),
    //JS_CFUNC_DEF("__toString", 1, js_string___toString ),
    //JS_CFUNC_DEF("__isSpace", 1, js_string___isSpace ),
    //JS_CFUNC_DEF("__toStringCheckObject", 1, js_string___toStringCheckObject ),
    //JS_CFUNC_DEF("__advanceStringIndex", 3, js_string___advanceStringIndex ),
    //JS_CFUNC_DEF("__GetSubstitution", 6, js_string___GetSubstitution ),
};

static const JSCFunctionListEntry js_string_proto_funcs[] = {
    JS_PROP_INT32_DEF("length", 0, JS_PROP_CONFIGURABLE ),
    JS_CFUNC_DEF("charCodeAt", 1, js_string_charCodeAt ),
    JS_CFUNC_DEF("charAt", 1, js_string_charAt ),
    JS_CFUNC_DEF("concat", 1, js_string_concat ),
    JS_CFUNC_DEF("codePointAt", 1, js_string_codePointAt ),
    JS_CFUNC_MAGIC_DEF("indexOf", 1, js_string_indexOf, 0 ),
    JS_CFUNC_MAGIC_DEF("lastIndexOf", 1, js_string_indexOf, 1 ),
    JS_CFUNC_MAGIC_DEF("includes", 1, js_string_includes, 0 ),
    JS_CFUNC_MAGIC_DEF("endsWith", 1, js_string_includes, 2 ),
    JS_CFUNC_MAGIC_DEF("startsWith", 1, js_string_includes, 1 ),
    JS_CFUNC_MAGIC_DEF("match", 1, js_string_match, JS_ATOM_Symbol_match ),
    JS_CFUNC_MAGIC_DEF("matchAll", 1, js_string_match, JS_ATOM_Symbol_matchAll ),
    JS_CFUNC_MAGIC_DEF("search", 1, js_string_match, JS_ATOM_Symbol_search ),
    JS_CFUNC_DEF("split", 2, js_string_split ),
    JS_CFUNC_DEF("substring", 2, js_string_substring ),
    JS_CFUNC_DEF("substr", 2, js_string_substr ),
    JS_CFUNC_DEF("slice", 2, js_string_slice ),
    JS_CFUNC_DEF("repeat", 1, js_string_repeat ),
    JS_CFUNC_MAGIC_DEF("replace", 2, js_string_replace, 0 ),
    JS_CFUNC_MAGIC_DEF("replaceAll", 2, js_string_replace, 1 ),
    JS_CFUNC_MAGIC_DEF("padEnd", 1, js_string_pad, 1 ),
    JS_CFUNC_MAGIC_DEF("padStart", 1, js_string_pad, 0 ),
    JS_CFUNC_MAGIC_DEF("trim", 0, js_string_trim, 3 ),
    JS_CFUNC_MAGIC_DEF("trimEnd", 0, js_string_trim, 2 ),
    JS_ALIAS_DEF("trimRight", "trimEnd" ),
    JS_CFUNC_MAGIC_DEF("trimStart", 0, js_string_trim, 1 ),
    JS_ALIAS_DEF("trimLeft", "trimStart" ),
    JS_CFUNC_DEF("toString", 0, js_string_toString ),
    JS_CFUNC_DEF("valueOf", 0, js_string_toString ),
    JS_CFUNC_DEF("__quote", 1, js_string___quote ),
    JS_CFUNC_DEF("localeCompare", 1, js_string_localeCompare ),
    JS_CFUNC_MAGIC_DEF("toLowerCase", 0, js_string_toLowerCase, 1 ),
    JS_CFUNC_MAGIC_DEF("toUpperCase", 0, js_string_toLowerCase, 0 ),
    JS_CFUNC_MAGIC_DEF("toLocaleLowerCase", 0, js_string_toLowerCase, 1 ),
    JS_CFUNC_MAGIC_DEF("toLocaleUpperCase", 0, js_string_toLowerCase, 0 ),
    JS_CFUNC_MAGIC_DEF("[Symbol.iterator]", 0, js_create_array_iterator, JS_ITERATOR_KIND_VALUE | 4 ),
    /* ES6 Annex B 2.3.2 etc. */
    JS_CFUNC_MAGIC_DEF("anchor", 1, js_string_CreateHTML, magic_string_anchor ),
    JS_CFUNC_MAGIC_DEF("big", 0, js_string_CreateHTML, magic_string_big ),
    JS_CFUNC_MAGIC_DEF("blink", 0, js_string_CreateHTML, magic_string_blink ),
    JS_CFUNC_MAGIC_DEF("bold", 0, js_string_CreateHTML, magic_string_bold ),
    JS_CFUNC_MAGIC_DEF("fixed", 0, js_string_CreateHTML, magic_string_fixed ),
    JS_CFUNC_MAGIC_DEF("fontcolor", 1, js_string_CreateHTML, magic_string_fontcolor ),
    JS_CFUNC_MAGIC_DEF("fontsize", 1, js_string_CreateHTML, magic_string_fontsize ),
    JS_CFUNC_MAGIC_DEF("italics", 0, js_string_CreateHTML, magic_string_italics ),
    JS_CFUNC_MAGIC_DEF("link", 1, js_string_CreateHTML, magic_string_link ),
    JS_CFUNC_MAGIC_DEF("small", 0, js_string_CreateHTML, magic_string_small ),
    JS_CFUNC_MAGIC_DEF("strike", 0, js_string_CreateHTML, magic_string_strike ),
    JS_CFUNC_MAGIC_DEF("sub", 0, js_string_CreateHTML, magic_string_sub ),
    JS_CFUNC_MAGIC_DEF("sup", 0, js_string_CreateHTML, magic_string_sup ),
};

static const JSCFunctionListEntry js_string_iterator_proto_funcs[] = {
    JS_ITERATOR_NEXT_DEF("next", 0, js_string_iterator_next, 0 ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "String Iterator", JS_PROP_CONFIGURABLE ),
};

#ifdef CONFIG_ALL_UNICODE
static const JSCFunctionListEntry js_string_proto_normalize[] = {
    JS_CFUNC_DEF("normalize", 0, js_string_normalize ),
};
#endif

void JS_AddIntrinsicStringNormalize(JSContext *ctx)
{
#ifdef CONFIG_ALL_UNICODE
    JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_STRING], js_string_proto_normalize,
                               countof(js_string_proto_normalize));
#endif
}

/* Math */

/* precondition: a and b are not NaN */
static double js_fmin(double a, double b)
{
    if (a == 0 && b == 0) {
        JSFloat64Union a1, b1;
        a1.d = a;
        b1.d = b;
        a1.u64 |= b1.u64;
        return a1.d;
    } else {
        return fmin(a, b);
    }
}

/* precondition: a and b are not NaN */
static double js_fmax(double a, double b)
{
    if (a == 0 && b == 0) {
        JSFloat64Union a1, b1;
        a1.d = a;
        b1.d = b;
        a1.u64 &= b1.u64;
        return a1.d;
    } else {
        return fmax(a, b);
    }
}

static JSValue js_math_min_max(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv, int magic)
{
    BOOL is_max = magic;
    double r, a;
    int i;
    uint32_t tag;

    if (unlikely(argc == 0)) {
        return __JS_NewFloat64(ctx, is_max ? -1.0 / 0.0 : 1.0 / 0.0);
    }

    tag = JS_VALUE_GET_TAG(argv[0]);
    if (tag == JS_TAG_INT) {
        int a1, r1 = JS_VALUE_GET_INT(argv[0]);
        for(i = 1; i < argc; i++) {
            tag = JS_VALUE_GET_TAG(argv[i]);
            if (tag != JS_TAG_INT) {
                r = r1;
                goto generic_case;
            }
            a1 = JS_VALUE_GET_INT(argv[i]);
            if (is_max)
                r1 = max_int(r1, a1);
            else
                r1 = min_int(r1, a1);

        }
        return JS_NewInt32(ctx, r1);
    } else {
        if (JS_ToFloat64(ctx, &r, argv[0]))
            return JS_EXCEPTION;
        i = 1;
    generic_case:
        while (i < argc) {
            if (JS_ToFloat64(ctx, &a, argv[i]))
                return JS_EXCEPTION;
            if (!isnan(r)) {
                if (isnan(a)) {
                    r = a;
                } else {
                    if (is_max)
                        r = js_fmax(r, a);
                    else
                        r = js_fmin(r, a);
                }
            }
            i++;
        }
        return JS_NewFloat64(ctx, r);
    }
}

static double js_math_sign(double a)
{
    if (isnan(a) || a == 0.0)
        return a;
    if (a < 0)
        return -1;
    else
        return 1;
}

static double js_math_round(double a)
{
    JSFloat64Union u;
    uint64_t frac_mask, one;
    unsigned int e, s;

    u.d = a;
    e = (u.u64 >> 52) & 0x7ff;
    if (e < 1023) {
        /* abs(a) < 1 */
        if (e == (1023 - 1) && u.u64 != 0xbfe0000000000000) {
            /* abs(a) > 0.5 or a = 0.5: return +/-1.0 */
            u.u64 = (u.u64 & ((uint64_t)1 << 63)) | ((uint64_t)1023 << 52);
        } else {
            /* return +/-0.0 */
            u.u64 &= (uint64_t)1 << 63;
        }
    } else if (e < (1023 + 52)) {
        s = u.u64 >> 63;
        one = (uint64_t)1 << (52 - (e - 1023));
        frac_mask = one - 1;
        u.u64 += (one >> 1) - s;
        u.u64 &= ~frac_mask; /* truncate to an integer */
    }
    /* otherwise: abs(a) >= 2^52, or NaN, +/-Infinity: no change */
    return u.d;
}

static JSValue js_math_hypot(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv)
{
    double r, a;
    int i;

    r = 0;
    if (argc > 0) {
        if (JS_ToFloat64(ctx, &r, argv[0]))
            return JS_EXCEPTION;
        if (argc == 1) {
            r = fabs(r);
        } else {
            /* use the built-in function to minimize precision loss */
            for (i = 1; i < argc; i++) {
                if (JS_ToFloat64(ctx, &a, argv[i]))
                    return JS_EXCEPTION;
                r = hypot(r, a);
            }
        }
    }
    return JS_NewFloat64(ctx, r);
}

static double js_math_fround(double a)
{
    return (float)a;
}

static JSValue js_math_imul(JSContext *ctx, JSValueConst this_val,
                            int argc, JSValueConst *argv)
{
    int a, b;

    if (JS_ToInt32(ctx, &a, argv[0]))
        return JS_EXCEPTION;
    if (JS_ToInt32(ctx, &b, argv[1]))
        return JS_EXCEPTION;
    /* purposely ignoring overflow */
    return JS_NewInt32(ctx, a * b);
}

static JSValue js_math_clz32(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv)
{
    uint32_t a, r;

    if (JS_ToUint32(ctx, &a, argv[0]))
        return JS_EXCEPTION;
    if (a == 0)
        r = 32;
    else
        r = clz32(a);
    return JS_NewInt32(ctx, r);
}

/* xorshift* random number generator by Marsaglia */
static uint64_t xorshift64star(uint64_t *pstate)
{
    uint64_t x;
    x = *pstate;
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    *pstate = x;
    return x * 0x2545F4914F6CDD1D;
}

static void js_random_init(JSContext *ctx)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    ctx->random_state = ((int64_t)tv.tv_sec * 1000000) + tv.tv_usec;
    /* the state must be non zero */
    if (ctx->random_state == 0)
        ctx->random_state = 1;
}

static JSValue js_math_random(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv)
{
    JSFloat64Union u;
    uint64_t v;

    v = xorshift64star(&ctx->random_state);
    /* 1.0 <= u.d < 2 */
    u.u64 = ((uint64_t)0x3ff << 52) | (v >> 12);
    return __JS_NewFloat64(ctx, u.d - 1.0);
}

static const JSCFunctionListEntry js_math_funcs[] = {
    JS_CFUNC_MAGIC_DEF("min", 2, js_math_min_max, 0 ),
    JS_CFUNC_MAGIC_DEF("max", 2, js_math_min_max, 1 ),
    JS_CFUNC_SPECIAL_DEF("abs", 1, f_f, fabs ),
    JS_CFUNC_SPECIAL_DEF("floor", 1, f_f, floor ),
    JS_CFUNC_SPECIAL_DEF("ceil", 1, f_f, ceil ),
    JS_CFUNC_SPECIAL_DEF("round", 1, f_f, js_math_round ),
    JS_CFUNC_SPECIAL_DEF("sqrt", 1, f_f, sqrt ),

    JS_CFUNC_SPECIAL_DEF("acos", 1, f_f, acos ),
    JS_CFUNC_SPECIAL_DEF("asin", 1, f_f, asin ),
    JS_CFUNC_SPECIAL_DEF("atan", 1, f_f, atan ),
    JS_CFUNC_SPECIAL_DEF("atan2", 2, f_f_f, atan2 ),
    JS_CFUNC_SPECIAL_DEF("cos", 1, f_f, cos ),
    JS_CFUNC_SPECIAL_DEF("exp", 1, f_f, exp ),
    JS_CFUNC_SPECIAL_DEF("log", 1, f_f, log ),
    JS_CFUNC_SPECIAL_DEF("pow", 2, f_f_f, js_pow ),
    JS_CFUNC_SPECIAL_DEF("sin", 1, f_f, sin ),
    JS_CFUNC_SPECIAL_DEF("tan", 1, f_f, tan ),
    /* ES6 */
    JS_CFUNC_SPECIAL_DEF("trunc", 1, f_f, trunc ),
    JS_CFUNC_SPECIAL_DEF("sign", 1, f_f, js_math_sign ),
    JS_CFUNC_SPECIAL_DEF("cosh", 1, f_f, cosh ),
    JS_CFUNC_SPECIAL_DEF("sinh", 1, f_f, sinh ),
    JS_CFUNC_SPECIAL_DEF("tanh", 1, f_f, tanh ),
    JS_CFUNC_SPECIAL_DEF("acosh", 1, f_f, acosh ),
    JS_CFUNC_SPECIAL_DEF("asinh", 1, f_f, asinh ),
    JS_CFUNC_SPECIAL_DEF("atanh", 1, f_f, atanh ),
    JS_CFUNC_SPECIAL_DEF("expm1", 1, f_f, expm1 ),
    JS_CFUNC_SPECIAL_DEF("log1p", 1, f_f, log1p ),
    JS_CFUNC_SPECIAL_DEF("log2", 1, f_f, log2 ),
    JS_CFUNC_SPECIAL_DEF("log10", 1, f_f, log10 ),
    JS_CFUNC_SPECIAL_DEF("cbrt", 1, f_f, cbrt ),
    JS_CFUNC_DEF("hypot", 2, js_math_hypot ),
    JS_CFUNC_DEF("random", 0, js_math_random ),
    JS_CFUNC_SPECIAL_DEF("fround", 1, f_f, js_math_fround ),
    JS_CFUNC_DEF("imul", 2, js_math_imul ),
    JS_CFUNC_DEF("clz32", 1, js_math_clz32 ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "Math", JS_PROP_CONFIGURABLE ),
    JS_PROP_DOUBLE_DEF("E", 2.718281828459045, 0 ),
    JS_PROP_DOUBLE_DEF("LN10", 2.302585092994046, 0 ),
    JS_PROP_DOUBLE_DEF("LN2", 0.6931471805599453, 0 ),
    JS_PROP_DOUBLE_DEF("LOG2E", 1.4426950408889634, 0 ),
    JS_PROP_DOUBLE_DEF("LOG10E", 0.4342944819032518, 0 ),
    JS_PROP_DOUBLE_DEF("PI", 3.141592653589793, 0 ),
    JS_PROP_DOUBLE_DEF("SQRT1_2", 0.7071067811865476, 0 ),
    JS_PROP_DOUBLE_DEF("SQRT2", 1.4142135623730951, 0 ),
};

static const JSCFunctionListEntry js_math_obj[] = {
    JS_OBJECT_DEF("Math", js_math_funcs, countof(js_math_funcs), JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE ),
};

/* Date */

#if 0
/* OS dependent: return the UTC time in ms since 1970. */
static JSValue js___date_now(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv)
{
    int64_t d;
    struct timeval tv;
    gettimeofday(&tv, NULL);
    d = (int64_t)tv.tv_sec * 1000 + (tv.tv_usec / 1000);
    return JS_NewInt64(ctx, d);
}
#endif

/* OS dependent: return the UTC time in microseconds since 1970. */
static JSValue js___date_clock(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv)
{
    int64_t d;
    struct timeval tv;
    gettimeofday(&tv, NULL);
    d = (int64_t)tv.tv_sec * 1000000 + tv.tv_usec;
    return JS_NewInt64(ctx, d);
}

/* OS dependent. d = argv[0] is in ms from 1970. Return the difference
   between UTC time and local time 'd' in minutes */
static int getTimezoneOffset(int64_t time) {
#if defined(_WIN32)
    /* XXX: TODO */
    return 0;
#else
    time_t ti;
    struct tm tm;

    time /= 1000; /* convert to seconds */
    if (sizeof(time_t) == 4) {
        /* on 32-bit systems, we need to clamp the time value to the
           range of `time_t`. This is better than truncating values to
           32 bits and hopefully provides the same result as 64-bit
           implementation of localtime_r.
         */
        if ((time_t)-1 < 0) {
            if (time < INT32_MIN) {
                time = INT32_MIN;
            } else if (time > INT32_MAX) {
                time = INT32_MAX;
            }
        } else {
            if (time < 0) {
                time = 0;
            } else if (time > UINT32_MAX) {
                time = UINT32_MAX;
            }
        }
    }
    ti = time;
    localtime_r(&ti, &tm);
    return -tm.tm_gmtoff / 60;
#endif
}

#if 0
static JSValue js___date_getTimezoneOffset(JSContext *ctx, JSValueConst this_val,
                                           int argc, JSValueConst *argv)
{
    double dd;

    if (JS_ToFloat64(ctx, &dd, argv[0]))
        return JS_EXCEPTION;
    if (isnan(dd))
        return __JS_NewFloat64(ctx, dd);
    else
        return JS_NewInt32(ctx, getTimezoneOffset((int64_t)dd));
}

static JSValue js_get_prototype_from_ctor(JSContext *ctx, JSValueConst ctor,
                                          JSValueConst def_proto)
{
    JSValue proto;
    proto = JS_GetProperty(ctx, ctor, JS_ATOM_prototype);
    if (JS_IsException(proto))
        return proto;
    if (!JS_IsObject(proto)) {
        JS_FreeValue(ctx, proto);
        proto = JS_DupValue(ctx, def_proto);
    }
    return proto;
}

/* create a new date object */
static JSValue js___date_create(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    JSValue obj, proto;
    proto = js_get_prototype_from_ctor(ctx, argv[0], argv[1]);
    if (JS_IsException(proto))
        return proto;
    obj = JS_NewObjectProtoClass(ctx, proto, JS_CLASS_DATE);
    JS_FreeValue(ctx, proto);
    if (!JS_IsException(obj))
        JS_SetObjectData(ctx, obj, JS_DupValue(ctx, argv[2]));
    return obj;
}
#endif

/* RegExp */

static void js_regexp_finalizer(JSRuntime *rt, JSValue val)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSRegExp *re = &p->u.regexp;
    JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_STRING, re->bytecode));
    JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_STRING, re->pattern));
}

/* create a string containing the RegExp bytecode */
static JSValue js_compile_regexp(JSContext *ctx, JSValueConst pattern,
                                 JSValueConst flags)
{
    const char *str;
    int re_flags, mask;
    uint8_t *re_bytecode_buf;
    size_t i, len;
    int re_bytecode_len;
    JSValue ret;
    char error_msg[64];

    re_flags = 0;
    if (!JS_IsUndefined(flags)) {
        str = JS_ToCStringLen(ctx, &len, flags);
        if (!str)
            return JS_EXCEPTION;
        /* XXX: re_flags = LRE_FLAG_OCTAL unless strict mode? */
        for (i = 0; i < len; i++) {
            switch(str[i]) {
            case 'g':
                mask = LRE_FLAG_GLOBAL;
                break;
            case 'i':
                mask = LRE_FLAG_IGNORECASE;
                break;
            case 'm':
                mask = LRE_FLAG_MULTILINE;
                break;
            case 's':
                mask = LRE_FLAG_DOTALL;
                break;
            case 'u':
                mask = LRE_FLAG_UTF16;
                break;
            case 'y':
                mask = LRE_FLAG_STICKY;
                break;
            default:
                goto bad_flags;
            }
            if ((re_flags & mask) != 0) {
            bad_flags:
                JS_FreeCString(ctx, str);
                return JS_ThrowSyntaxError(ctx, "invalid regular expression flags");
            }
            re_flags |= mask;
        }
        JS_FreeCString(ctx, str);
    }

    str = JS_ToCStringLen2(ctx, &len, pattern, !(re_flags & LRE_FLAG_UTF16));
    if (!str)
        return JS_EXCEPTION;
    re_bytecode_buf = lre_compile(&re_bytecode_len, error_msg,
                                  sizeof(error_msg), str, len, re_flags, ctx);
    JS_FreeCString(ctx, str);
    if (!re_bytecode_buf) {
        JS_ThrowSyntaxError(ctx, "%s", error_msg);
        return JS_EXCEPTION;
    }

    ret = js_new_string8(ctx, re_bytecode_buf, re_bytecode_len);
    js_free(ctx, re_bytecode_buf);
    return ret;
}

/* create a RegExp object from a string containing the RegExp bytecode
   and the source pattern */
static JSValue js_regexp_constructor_internal(JSContext *ctx, JSValueConst ctor,
                                              JSValue pattern, JSValue bc)
{
    JSValue obj;
    JSObject *p;
    JSRegExp *re;

    /* sanity check */
    if (JS_VALUE_GET_TAG(bc) != JS_TAG_STRING ||
        JS_VALUE_GET_TAG(pattern) != JS_TAG_STRING) {
        JS_ThrowTypeError(ctx, "string expected");
    fail:
        JS_FreeValue(ctx, bc);
        JS_FreeValue(ctx, pattern);
        return JS_EXCEPTION;
    }

    obj = js_create_from_ctor(ctx, ctor, JS_CLASS_REGEXP);
    if (JS_IsException(obj))
        goto fail;
    p = JS_VALUE_GET_OBJ(obj);
    re = &p->u.regexp;
    re->pattern = JS_VALUE_GET_STRING(pattern);
    re->bytecode = JS_VALUE_GET_STRING(bc);
    JS_DefinePropertyValue(ctx, obj, JS_ATOM_lastIndex, JS_NewInt32(ctx, 0),
                           JS_PROP_WRITABLE);
    return obj;
}

static JSRegExp *js_get_regexp(JSContext *ctx, JSValueConst obj, BOOL throw_error)
{
    if (JS_VALUE_GET_TAG(obj) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(obj);
        if (p->class_id == JS_CLASS_REGEXP)
            return &p->u.regexp;
    }
    if (throw_error) {
        JS_ThrowTypeErrorInvalidClass(ctx, JS_CLASS_REGEXP);
    }
    return NULL;
}

/* return < 0 if exception or TRUE/FALSE */
static int js_is_regexp(JSContext *ctx, JSValueConst obj)
{
    JSValue m;

    if (!JS_IsObject(obj))
        return FALSE;
    m = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_match);
    if (JS_IsException(m))
        return -1;
    if (!JS_IsUndefined(m))
        return JS_ToBoolFree(ctx, m);
    return js_get_regexp(ctx, obj, FALSE) != NULL;
}

static JSValue js_regexp_constructor(JSContext *ctx, JSValueConst new_target,
                                     int argc, JSValueConst *argv)
{
    JSValue pattern, flags, bc, val;
    JSValueConst pat, flags1;
    JSRegExp *re;
    int pat_is_regexp;

    pat = argv[0];
    flags1 = argv[1];
    pat_is_regexp = js_is_regexp(ctx, pat);
    if (pat_is_regexp < 0)
        return JS_EXCEPTION;
    if (JS_IsUndefined(new_target)) {
        /* called as a function */
        new_target = JS_GetActiveFunction(ctx);
        if (pat_is_regexp && JS_IsUndefined(flags1)) {
            JSValue ctor;
            BOOL res;
            ctor = JS_GetProperty(ctx, pat, JS_ATOM_constructor);
            if (JS_IsException(ctor))
                return ctor;
            res = js_same_value(ctx, ctor, new_target);
            JS_FreeValue(ctx, ctor);
            if (res)
                return JS_DupValue(ctx, pat);
        }
    }
    re = js_get_regexp(ctx, pat, FALSE);
    if (re) {
        pattern = JS_DupValue(ctx, JS_MKPTR(JS_TAG_STRING, re->pattern));
        if (JS_IsUndefined(flags1)) {
            bc = JS_DupValue(ctx, JS_MKPTR(JS_TAG_STRING, re->bytecode));
            goto no_compilation;
        } else {
            flags = JS_ToString(ctx, flags1);
            if (JS_IsException(flags))
                goto fail;
        }
    } else {
        flags = JS_UNDEFINED;
        if (pat_is_regexp) {
            pattern = JS_GetProperty(ctx, pat, JS_ATOM_source);
            if (JS_IsException(pattern))
                goto fail;
            if (JS_IsUndefined(flags1)) {
                flags = JS_GetProperty(ctx, pat, JS_ATOM_flags);
                if (JS_IsException(flags))
                    goto fail;
            } else {
                flags = JS_DupValue(ctx, flags1);
            }
        } else {
            pattern = JS_DupValue(ctx, pat);
            flags = JS_DupValue(ctx, flags1);
        }
        if (JS_IsUndefined(pattern)) {
            pattern = JS_AtomToString(ctx, JS_ATOM_empty_string);
        } else {
            val = pattern;
            pattern = JS_ToString(ctx, val);
            JS_FreeValue(ctx, val);
            if (JS_IsException(pattern))
                goto fail;
        }
    }
    bc = js_compile_regexp(ctx, pattern, flags);
    if (JS_IsException(bc))
        goto fail;
    JS_FreeValue(ctx, flags);
 no_compilation:
    return js_regexp_constructor_internal(ctx, new_target, pattern, bc);
 fail:
    JS_FreeValue(ctx, pattern);
    JS_FreeValue(ctx, flags);
    return JS_EXCEPTION;
}

static JSValue js_regexp_compile(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    JSRegExp *re1, *re;
    JSValueConst pattern1, flags1;
    JSValue bc, pattern;

    re = js_get_regexp(ctx, this_val, TRUE);
    if (!re)
        return JS_EXCEPTION;
    pattern1 = argv[0];
    flags1 = argv[1];
    re1 = js_get_regexp(ctx, pattern1, FALSE);
    if (re1) {
        if (!JS_IsUndefined(flags1))
            return JS_ThrowTypeError(ctx, "flags must be undefined");
        pattern = JS_DupValue(ctx, JS_MKPTR(JS_TAG_STRING, re1->pattern));
        bc = JS_DupValue(ctx, JS_MKPTR(JS_TAG_STRING, re1->bytecode));
    } else {
        bc = JS_UNDEFINED;
        if (JS_IsUndefined(pattern1))
            pattern = JS_AtomToString(ctx, JS_ATOM_empty_string);
        else
            pattern = JS_ToString(ctx, pattern1);
        if (JS_IsException(pattern))
            goto fail;
        bc = js_compile_regexp(ctx, pattern, flags1);
        if (JS_IsException(bc))
            goto fail;
    }
    JS_FreeValue(ctx, JS_MKPTR(JS_TAG_STRING, re->pattern));
    JS_FreeValue(ctx, JS_MKPTR(JS_TAG_STRING, re->bytecode));
    re->pattern = JS_VALUE_GET_STRING(pattern);
    re->bytecode = JS_VALUE_GET_STRING(bc);
    if (JS_SetProperty(ctx, this_val, JS_ATOM_lastIndex,
                       JS_NewInt32(ctx, 0)) < 0)
        return JS_EXCEPTION;
    return JS_DupValue(ctx, this_val);
 fail:
    JS_FreeValue(ctx, pattern);
    JS_FreeValue(ctx, bc);
    return JS_EXCEPTION;
}

#if 0
static JSValue js_regexp_get___source(JSContext *ctx, JSValueConst this_val)
{
    JSRegExp *re = js_get_regexp(ctx, this_val, TRUE);
    if (!re)
        return JS_EXCEPTION;
    return JS_DupValue(ctx, JS_MKPTR(JS_TAG_STRING, re->pattern));
}

static JSValue js_regexp_get___flags(JSContext *ctx, JSValueConst this_val)
{
    JSRegExp *re = js_get_regexp(ctx, this_val, TRUE);
    int flags;

    if (!re)
        return JS_EXCEPTION;
    flags = lre_get_flags(re->bytecode->u.str8);
    return JS_NewInt32(ctx, flags);
}
#endif

static JSValue js_regexp_get_source(JSContext *ctx, JSValueConst this_val)
{
    JSRegExp *re;
    JSString *p;
    StringBuffer b_s, *b = &b_s;
    int i, n, c, c2, bra;

    if (JS_VALUE_GET_TAG(this_val) != JS_TAG_OBJECT)
        return JS_ThrowTypeErrorNotAnObject(ctx);

    if (js_same_value(ctx, this_val, ctx->class_proto[JS_CLASS_REGEXP]))
        goto empty_regex;

    re = js_get_regexp(ctx, this_val, TRUE);
    if (!re)
        return JS_EXCEPTION;

    p = re->pattern;

    if (p->len == 0) {
    empty_regex:
        return JS_NewString(ctx, "(?:)");
    }    
    string_buffer_init2(ctx, b, p->len, p->is_wide_char);

    /* Escape '/' and newline sequences as needed */
    bra = 0;
    for (i = 0, n = p->len; i < n;) {
        c2 = -1;
        switch (c = string_get(p, i++)) {
        case '\\':
            if (i < n)
                c2 = string_get(p, i++);
            break;
        case ']':
            bra = 0;
            break;
        case '[':
            if (!bra) {
                if (i < n && string_get(p, i) == ']')
                    c2 = string_get(p, i++);
                bra = 1;
            }
            break;
        case '\n':
            c = '\\';
            c2 = 'n';
            break;
        case '\r':
            c = '\\';
            c2 = 'r';
            break;
        case '/':
            if (!bra) {
                c = '\\';
                c2 = '/';
            }
            break;
        }
        string_buffer_putc16(b, c);
        if (c2 >= 0)
            string_buffer_putc16(b, c2);
    }
    return string_buffer_end(b);
}

static JSValue js_regexp_get_flag(JSContext *ctx, JSValueConst this_val, int mask)
{
    JSRegExp *re;
    int flags;

    if (JS_VALUE_GET_TAG(this_val) != JS_TAG_OBJECT)
        return JS_ThrowTypeErrorNotAnObject(ctx);

    re = js_get_regexp(ctx, this_val, FALSE);
    if (!re) {
        if (js_same_value(ctx, this_val, ctx->class_proto[JS_CLASS_REGEXP]))
            return JS_UNDEFINED;
        else
            return JS_ThrowTypeErrorInvalidClass(ctx, JS_CLASS_REGEXP);
    }
    
    flags = lre_get_flags(re->bytecode->u.str8);
    return JS_NewBool(ctx, (flags & mask) != 0);
}

static JSValue js_regexp_get_flags(JSContext *ctx, JSValueConst this_val)
{
    char str[8], *p = str;
    int res;

    if (JS_VALUE_GET_TAG(this_val) != JS_TAG_OBJECT)
        return JS_ThrowTypeErrorNotAnObject(ctx);

    res = JS_ToBoolFree(ctx, JS_GetProperty(ctx, this_val, JS_ATOM_global));
    if (res < 0)
        goto exception;
    if (res)
        *p++ = 'g';
    res = JS_ToBoolFree(ctx, JS_GetPropertyStr(ctx, this_val, "ignoreCase"));
    if (res < 0)
        goto exception;
    if (res)
        *p++ = 'i';
    res = JS_ToBoolFree(ctx, JS_GetPropertyStr(ctx, this_val, "multiline"));
    if (res < 0)
        goto exception;
    if (res)
        *p++ = 'm';
    res = JS_ToBoolFree(ctx, JS_GetPropertyStr(ctx, this_val, "dotAll"));
    if (res < 0)
        goto exception;
    if (res)
        *p++ = 's';
    res = JS_ToBoolFree(ctx, JS_GetProperty(ctx, this_val, JS_ATOM_unicode));
    if (res < 0)
        goto exception;
    if (res)
        *p++ = 'u';
    res = JS_ToBoolFree(ctx, JS_GetPropertyStr(ctx, this_val, "sticky"));
    if (res < 0)
        goto exception;
    if (res)
        *p++ = 'y';
    return JS_NewStringLen(ctx, str, p - str);

exception:
    return JS_EXCEPTION;
}

static JSValue js_regexp_toString(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv)
{
    JSValue pattern, flags;
    StringBuffer b_s, *b = &b_s;

    if (!JS_IsObject(this_val))
        return JS_ThrowTypeErrorNotAnObject(ctx);

    string_buffer_init(ctx, b, 0);
    string_buffer_putc8(b, '/');
    pattern = JS_GetProperty(ctx, this_val, JS_ATOM_source);
    if (string_buffer_concat_value_free(b, pattern))
        goto fail;
    string_buffer_putc8(b, '/');
    flags = JS_GetProperty(ctx, this_val, JS_ATOM_flags);
    if (string_buffer_concat_value_free(b, flags))
        goto fail;
    return string_buffer_end(b);

fail:
    string_buffer_free(b);
    return JS_EXCEPTION;
}

BOOL lre_check_stack_overflow(void *opaque, size_t alloca_size)
{
    JSContext *ctx = opaque;
    return js_check_stack_overflow(ctx->rt, alloca_size);
}

void *lre_realloc(void *opaque, void *ptr, size_t size)
{
    JSContext *ctx = opaque;
    /* No JS exception is raised here */
    return js_realloc_rt(ctx->rt, ptr, size);
}

static JSValue js_regexp_exec(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv)
{
    JSRegExp *re = js_get_regexp(ctx, this_val, TRUE);
    JSString *str;
    JSValue str_val, obj, val, groups = JS_UNDEFINED;
    uint8_t *re_bytecode;
    int ret;
    uint8_t **capture, *str_buf;
    int capture_count, shift, i, re_flags;
    int64_t last_index;
    const char *group_name_ptr;

    if (!re)
        return JS_EXCEPTION;
    str_val = JS_ToString(ctx, argv[0]);
    if (JS_IsException(str_val))
        return str_val;
    val = JS_GetProperty(ctx, this_val, JS_ATOM_lastIndex);
    if (JS_IsException(val) ||
        JS_ToLengthFree(ctx, &last_index, val)) {
        JS_FreeValue(ctx, str_val);
        return JS_EXCEPTION;
    }
    re_bytecode = re->bytecode->u.str8;
    re_flags = lre_get_flags(re_bytecode);
    if ((re_flags & (LRE_FLAG_GLOBAL | LRE_FLAG_STICKY)) == 0) {
        last_index = 0;
    }
    str = JS_VALUE_GET_STRING(str_val);
    capture_count = lre_get_capture_count(re_bytecode);
    capture = NULL;
    if (capture_count > 0) {
        capture = js_malloc(ctx, sizeof(capture[0]) * capture_count * 2);
        if (!capture) {
            JS_FreeValue(ctx, str_val);
            return JS_EXCEPTION;
        }
    }
    shift = str->is_wide_char;
    str_buf = str->u.str8;
    if (last_index > str->len) {
        ret = 2;
    } else {
        ret = lre_exec(capture, re_bytecode,
                       str_buf, last_index, str->len,
                       shift, ctx);
    }
    obj = JS_NULL;
    if (ret != 1) {
        if (ret >= 0) {
            if (ret == 2 || (re_flags & (LRE_FLAG_GLOBAL | LRE_FLAG_STICKY))) {
                if (JS_SetProperty(ctx, this_val, JS_ATOM_lastIndex,
                                   JS_NewInt32(ctx, 0)) < 0)
                    goto fail;
            }
        } else {
            JS_ThrowInternalError(ctx, "out of memory in regexp execution");
            goto fail;
        }
        JS_FreeValue(ctx, str_val);
    } else {
        int prop_flags;
        if (re_flags & (LRE_FLAG_GLOBAL | LRE_FLAG_STICKY)) {
            if (JS_SetProperty(ctx, this_val, JS_ATOM_lastIndex,
                               JS_NewInt32(ctx, (capture[1] - str_buf) >> shift)) < 0)
                goto fail;
        }
        obj = JS_NewArray(ctx);
        if (JS_IsException(obj))
            goto fail;
        prop_flags = JS_PROP_C_W_E | JS_PROP_THROW;
        group_name_ptr = lre_get_groupnames(re_bytecode);
        if (group_name_ptr) {
            groups = JS_NewObjectProto(ctx, JS_NULL);
            if (JS_IsException(groups))
                goto fail;
        }

        for(i = 0; i < capture_count; i++) {
            int start, end;
            JSValue val;
            if (capture[2 * i] == NULL ||
                capture[2 * i + 1] == NULL) {
                val = JS_UNDEFINED;
            } else {
                start = (capture[2 * i] - str_buf) >> shift;
                end = (capture[2 * i + 1] - str_buf) >> shift;
                val = js_sub_string(ctx, str, start, end);
                if (JS_IsException(val))
                    goto fail;
            }
            if (group_name_ptr && i > 0) {
                if (*group_name_ptr) {
                    if (JS_DefinePropertyValueStr(ctx, groups, group_name_ptr,
                                                  JS_DupValue(ctx, val),
                                                  prop_flags) < 0) {
                        JS_FreeValue(ctx, val);
                        goto fail;
                    }
                }
                group_name_ptr += strlen(group_name_ptr) + 1;
            }
            if (JS_DefinePropertyValueUint32(ctx, obj, i, val, prop_flags) < 0)
                goto fail;
        }
        if (JS_DefinePropertyValue(ctx, obj, JS_ATOM_groups,
                                   groups, prop_flags) < 0)
            goto fail;
        if (JS_DefinePropertyValue(ctx, obj, JS_ATOM_index,
                                   JS_NewInt32(ctx, (capture[0] - str_buf) >> shift), prop_flags) < 0)
            goto fail;
        if (JS_DefinePropertyValue(ctx, obj, JS_ATOM_input, str_val, prop_flags) < 0)
            goto fail1;
    }
    js_free(ctx, capture);
    return obj;
fail:
    JS_FreeValue(ctx, groups);
    JS_FreeValue(ctx, str_val);
fail1:
    JS_FreeValue(ctx, obj);
    js_free(ctx, capture);
    return JS_EXCEPTION;
}

/* delete portions of a string that match a given regex */
static JSValue JS_RegExpDelete(JSContext *ctx, JSValueConst this_val, JSValueConst arg)
{
    JSRegExp *re = js_get_regexp(ctx, this_val, TRUE);
    JSString *str;
    JSValue str_val, val;
    uint8_t *re_bytecode;
    int ret;
    uint8_t **capture, *str_buf;
    int capture_count, shift, re_flags;
    int next_src_pos, start, end;
    int64_t last_index;
    StringBuffer b_s, *b = &b_s;

    if (!re)
        return JS_EXCEPTION;

    string_buffer_init(ctx, b, 0);

    capture = NULL;
    str_val = JS_ToString(ctx, arg);
    if (JS_IsException(str_val))
        goto fail;
    str = JS_VALUE_GET_STRING(str_val);
    re_bytecode = re->bytecode->u.str8;
    re_flags = lre_get_flags(re_bytecode);
    if ((re_flags & (LRE_FLAG_GLOBAL | LRE_FLAG_STICKY)) == 0) {
        last_index = 0;
    } else {
        val = JS_GetProperty(ctx, this_val, JS_ATOM_lastIndex);
        if (JS_IsException(val) || JS_ToLengthFree(ctx, &last_index, val))
            goto fail;
    }
    capture_count = lre_get_capture_count(re_bytecode);
    if (capture_count > 0) {
        capture = js_malloc(ctx, sizeof(capture[0]) * capture_count * 2);
        if (!capture)
            goto fail;
    }
    shift = str->is_wide_char;
    str_buf = str->u.str8;
    next_src_pos = 0;
    for (;;) {
        if (last_index > str->len)
            break;

        ret = lre_exec(capture, re_bytecode,
                       str_buf, last_index, str->len, shift, ctx);
        if (ret != 1) {
            if (ret >= 0) {
                if (ret == 2 || (re_flags & (LRE_FLAG_GLOBAL | LRE_FLAG_STICKY))) {
                    if (JS_SetProperty(ctx, this_val, JS_ATOM_lastIndex,
                                       JS_NewInt32(ctx, 0)) < 0)
                        goto fail;
                }
            } else {
                JS_ThrowInternalError(ctx, "out of memory in regexp execution");
                goto fail;
            }
            break;
        }
        start = (capture[0] - str_buf) >> shift;
        end = (capture[1] - str_buf) >> shift;
        last_index = end;
        if (next_src_pos < start) {
            if (string_buffer_concat(b, str, next_src_pos, start))
                goto fail;
        }
        next_src_pos = end;
        if (!(re_flags & LRE_FLAG_GLOBAL)) {
            if (JS_SetProperty(ctx, this_val, JS_ATOM_lastIndex,
                               JS_NewInt32(ctx, end)) < 0)
                goto fail;
            break;
        }
        if (end == start) {
            if (!(re_flags & LRE_FLAG_UTF16) || (unsigned)end >= str->len || !str->is_wide_char) {
                end++;
            } else {
                string_getc(str, &end);
            }
        }
        last_index = end;
    }
    if (string_buffer_concat(b, str, next_src_pos, str->len))
        goto fail;
    JS_FreeValue(ctx, str_val);
    js_free(ctx, capture);
    return string_buffer_end(b);
fail:
    JS_FreeValue(ctx, str_val);
    js_free(ctx, capture);
    string_buffer_free(b);
    return JS_EXCEPTION;
}

static JSValue JS_RegExpExec(JSContext *ctx, JSValueConst r, JSValueConst s)
{
    JSValue method, ret;

    method = JS_GetProperty(ctx, r, JS_ATOM_exec);
    if (JS_IsException(method))
        return method;
    if (JS_IsFunction(ctx, method)) {
        ret = JS_CallFree(ctx, method, r, 1, &s);
        if (JS_IsException(ret))
            return ret;
        if (!JS_IsObject(ret) && !JS_IsNull(ret)) {
            JS_FreeValue(ctx, ret);
            return JS_ThrowTypeError(ctx, "RegExp exec method must return an object or null");
        }
        return ret;
    }
    JS_FreeValue(ctx, method);
    return js_regexp_exec(ctx, r, 1, &s);
}

#if 0
static JSValue js_regexp___RegExpExec(JSContext *ctx, JSValueConst this_val,
                                      int argc, JSValueConst *argv)
{
    return JS_RegExpExec(ctx, argv[0], argv[1]);
}
static JSValue js_regexp___RegExpDelete(JSContext *ctx, JSValueConst this_val,
                                        int argc, JSValueConst *argv)
{
    return JS_RegExpDelete(ctx, argv[0], argv[1]);
}
#endif

static JSValue js_regexp_test(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv)
{
    JSValue val;
    BOOL ret;

    val = JS_RegExpExec(ctx, this_val, argv[0]);
    if (JS_IsException(val))
        return JS_EXCEPTION;
    ret = !JS_IsNull(val);
    JS_FreeValue(ctx, val);
    return JS_NewBool(ctx, ret);
}

static JSValue js_regexp_Symbol_match(JSContext *ctx, JSValueConst this_val,
                                      int argc, JSValueConst *argv)
{
    // [Symbol.match](str)
    JSValueConst rx = this_val;
    JSValue A, S, result, matchStr;
    int global, n, fullUnicode, isEmpty;
    JSString *p;

    if (!JS_IsObject(rx))
        return JS_ThrowTypeErrorNotAnObject(ctx);

    A = JS_UNDEFINED;
    result = JS_UNDEFINED;
    matchStr = JS_UNDEFINED;
    S = JS_ToString(ctx, argv[0]);
    if (JS_IsException(S))
        goto exception;

    global = JS_ToBoolFree(ctx, JS_GetProperty(ctx, rx, JS_ATOM_global));
    if (global < 0)
        goto exception;

    if (!global) {
        A = JS_RegExpExec(ctx, rx, S);
    } else {
        fullUnicode = JS_ToBoolFree(ctx, JS_GetProperty(ctx, rx, JS_ATOM_unicode));
        if (fullUnicode < 0)
            goto exception;

        if (JS_SetProperty(ctx, rx, JS_ATOM_lastIndex, JS_NewInt32(ctx, 0)) < 0)
            goto exception;
        A = JS_NewArray(ctx);
        if (JS_IsException(A))
            goto exception;
        n = 0;
        for(;;) {
            JS_FreeValue(ctx, result);
            result = JS_RegExpExec(ctx, rx, S);
            if (JS_IsException(result))
                goto exception;
            if (JS_IsNull(result))
                break;
            matchStr = JS_ToStringFree(ctx, JS_GetPropertyInt64(ctx, result, 0));
            if (JS_IsException(matchStr))
                goto exception;
            isEmpty = JS_IsEmptyString(matchStr);
            if (JS_SetPropertyInt64(ctx, A, n++, matchStr) < 0)
                goto exception;
            if (isEmpty) {
                int64_t thisIndex, nextIndex;
                if (JS_ToLengthFree(ctx, &thisIndex,
                                    JS_GetProperty(ctx, rx, JS_ATOM_lastIndex)) < 0)
                    goto exception;
                p = JS_VALUE_GET_STRING(S);
                nextIndex = string_advance_index(p, thisIndex, fullUnicode);
                if (JS_SetProperty(ctx, rx, JS_ATOM_lastIndex, JS_NewInt64(ctx, nextIndex)) < 0)
                    goto exception;
            }
        }
        if (n == 0) {
            JS_FreeValue(ctx, A);
            A = JS_NULL;
        }
    }
    JS_FreeValue(ctx, result);
    JS_FreeValue(ctx, S);
    return A;

exception:
    JS_FreeValue(ctx, A);
    JS_FreeValue(ctx, result);
    JS_FreeValue(ctx, S);
    return JS_EXCEPTION;
}

typedef struct JSRegExpStringIteratorData {
    JSValue iterating_regexp;
    JSValue iterated_string;
    BOOL global;
    BOOL unicode;
    BOOL done;
} JSRegExpStringIteratorData;

static void js_regexp_string_iterator_finalizer(JSRuntime *rt, JSValue val)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSRegExpStringIteratorData *it = p->u.regexp_string_iterator_data;
    if (it) {
        JS_FreeValueRT(rt, it->iterating_regexp);
        JS_FreeValueRT(rt, it->iterated_string);
        js_free_rt(rt, it);
    }
}

static void js_regexp_string_iterator_mark(JSRuntime *rt, JSValueConst val,
                                           JS_MarkFunc *mark_func)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSRegExpStringIteratorData *it = p->u.regexp_string_iterator_data;
    if (it) {
        JS_MarkValue(rt, it->iterating_regexp, mark_func);
        JS_MarkValue(rt, it->iterated_string, mark_func);
    }
}

static JSValue js_regexp_string_iterator_next(JSContext *ctx,
                                              JSValueConst this_val,
                                              int argc, JSValueConst *argv,
                                              BOOL *pdone, int magic)
{
    JSRegExpStringIteratorData *it;
    JSValueConst R, S;
    JSValue matchStr = JS_UNDEFINED, match = JS_UNDEFINED;
    JSString *sp;

    it = JS_GetOpaque2(ctx, this_val, JS_CLASS_REGEXP_STRING_ITERATOR);
    if (!it)
        goto exception;
    if (it->done) {
        *pdone = TRUE;
        return JS_UNDEFINED;
    }
    R = it->iterating_regexp;
    S = it->iterated_string;
    match = JS_RegExpExec(ctx, R, S);
    if (JS_IsException(match))
        goto exception;
    if (JS_IsNull(match)) {
        it->done = TRUE;
        *pdone = TRUE;
        return JS_UNDEFINED;
    } else if (it->global) {
        matchStr = JS_ToStringFree(ctx, JS_GetPropertyInt64(ctx, match, 0));
        if (JS_IsException(matchStr))
            goto exception;
        if (JS_IsEmptyString(matchStr)) {
            int64_t thisIndex, nextIndex;
            if (JS_ToLengthFree(ctx, &thisIndex,
                                JS_GetProperty(ctx, R, JS_ATOM_lastIndex)) < 0)
                goto exception;
            sp = JS_VALUE_GET_STRING(S);
            nextIndex = string_advance_index(sp, thisIndex, it->unicode);
            if (JS_SetProperty(ctx, R, JS_ATOM_lastIndex,
                               JS_NewInt64(ctx, nextIndex)) < 0)
                goto exception;
        }
        JS_FreeValue(ctx, matchStr);
    } else {
        it->done = TRUE;
    }
    *pdone = FALSE;
    return match;
 exception:
    JS_FreeValue(ctx, match);
    JS_FreeValue(ctx, matchStr);
    *pdone = FALSE;
    return JS_EXCEPTION;
}

static JSValue js_regexp_Symbol_matchAll(JSContext *ctx, JSValueConst this_val,
                                         int argc, JSValueConst *argv)
{
    // [Symbol.matchAll](str)
    JSValueConst R = this_val;
    JSValue S, C, flags, matcher, iter;
    JSValueConst args[2];
    JSString *strp;
    int64_t lastIndex;
    JSRegExpStringIteratorData *it;
    
    if (!JS_IsObject(R))
        return JS_ThrowTypeErrorNotAnObject(ctx);

    C = JS_UNDEFINED;
    flags = JS_UNDEFINED;
    matcher = JS_UNDEFINED;
    iter = JS_UNDEFINED;
    
    S = JS_ToString(ctx, argv[0]);
    if (JS_IsException(S))
        goto exception;
    C = JS_SpeciesConstructor(ctx, R, ctx->regexp_ctor);
    if (JS_IsException(C))
        goto exception;
    flags = JS_ToStringFree(ctx, JS_GetProperty(ctx, R, JS_ATOM_flags));
    if (JS_IsException(flags))
        goto exception;
    args[0] = R;
    args[1] = flags;
    matcher = JS_CallConstructor(ctx, C, 2, args);
    if (JS_IsException(matcher))
        goto exception;
    if (JS_ToLengthFree(ctx, &lastIndex,
                        JS_GetProperty(ctx, R, JS_ATOM_lastIndex)))
        goto exception;
    if (JS_SetProperty(ctx, matcher, JS_ATOM_lastIndex,
                       JS_NewInt64(ctx, lastIndex)) < 0)
        goto exception;
    
    iter = JS_NewObjectClass(ctx, JS_CLASS_REGEXP_STRING_ITERATOR);
    if (JS_IsException(iter))
        goto exception;
    it = js_malloc(ctx, sizeof(*it));
    if (!it)
        goto exception;
    it->iterating_regexp = matcher;
    it->iterated_string = S;
    strp = JS_VALUE_GET_STRING(flags);
    it->global = string_indexof_char(strp, 'g', 0) >= 0;
    it->unicode = string_indexof_char(strp, 'u', 0) >= 0;
    it->done = FALSE;
    JS_SetOpaque(iter, it);

    JS_FreeValue(ctx, C);
    JS_FreeValue(ctx, flags);
    return iter;
 exception:
    JS_FreeValue(ctx, S);
    JS_FreeValue(ctx, C);
    JS_FreeValue(ctx, flags);
    JS_FreeValue(ctx, matcher);
    JS_FreeValue(ctx, iter);
    return JS_EXCEPTION;
}

typedef struct ValueBuffer {
    JSContext *ctx;
    JSValue *arr;
    JSValue def[4];
    int len;
    int size;
    int error_status;
} ValueBuffer;

static int value_buffer_init(JSContext *ctx, ValueBuffer *b)
{
    b->ctx = ctx;
    b->len = 0;
    b->size = 4;
    b->error_status = 0;
    b->arr = b->def;
    return 0;
}

static void value_buffer_free(ValueBuffer *b)
{
    while (b->len > 0)
        JS_FreeValue(b->ctx, b->arr[--b->len]);
    if (b->arr != b->def)
        js_free(b->ctx, b->arr);
    b->arr = b->def;
    b->size = 4;
}

static int value_buffer_append(ValueBuffer *b, JSValue val)
{
    if (b->error_status)
        return -1;

    if (b->len >= b->size) {
        int new_size = (b->len + (b->len >> 1) + 31) & ~16;
        size_t slack;
        JSValue *new_arr;

        if (b->arr == b->def) {
            new_arr = js_realloc2(b->ctx, NULL, sizeof(*b->arr) * new_size, &slack);
            if (new_arr)
                memcpy(new_arr, b->def, sizeof b->def);
        } else {
            new_arr = js_realloc2(b->ctx, b->arr, sizeof(*b->arr) * new_size, &slack);
        }
        if (!new_arr) {
            value_buffer_free(b);
            JS_FreeValue(b->ctx, val);
            b->error_status = -1;
            return -1;
        }
        new_size += slack / sizeof(*new_arr);
        b->arr = new_arr;
        b->size = new_size;
    }
    b->arr[b->len++] = val;
    return 0;
}

static int js_is_standard_regexp(JSContext *ctx, JSValueConst rx)
{
    JSValue val;
    int res;

    val = JS_GetProperty(ctx, rx, JS_ATOM_constructor);
    if (JS_IsException(val))
        return -1;
    // rx.constructor === RegExp
    res = js_same_value(ctx, val, ctx->regexp_ctor);
    JS_FreeValue(ctx, val);
    if (res) {
        val = JS_GetProperty(ctx, rx, JS_ATOM_exec);
        if (JS_IsException(val))
            return -1;
        // rx.exec === RE_exec
        res = JS_IsCFunction(ctx, val, js_regexp_exec, 0);
        JS_FreeValue(ctx, val);
    }
    return res;
}

static JSValue js_regexp_Symbol_replace(JSContext *ctx, JSValueConst this_val,
                                        int argc, JSValueConst *argv)
{
    // [Symbol.replace](str, rep)
    JSValueConst rx = this_val, rep = argv[1];
    JSValueConst args[6];
    JSValue str, rep_val, matched, tab, rep_str, namedCaptures, res;
    JSString *sp, *rp;
    StringBuffer b_s, *b = &b_s;
    ValueBuffer v_b, *results = &v_b;
    int nextSourcePosition, n, j, functionalReplace, is_global, fullUnicode;
    uint32_t nCaptures;
    int64_t position;

    if (!JS_IsObject(rx))
        return JS_ThrowTypeErrorNotAnObject(ctx);

    string_buffer_init(ctx, b, 0);
    value_buffer_init(ctx, results);

    rep_val = JS_UNDEFINED;
    matched = JS_UNDEFINED;
    tab = JS_UNDEFINED;
    rep_str = JS_UNDEFINED;
    namedCaptures = JS_UNDEFINED;

    str = JS_ToString(ctx, argv[0]);
    if (JS_IsException(str))
        goto exception;
        
    sp = JS_VALUE_GET_STRING(str);
    rp = NULL;
    functionalReplace = JS_IsFunction(ctx, rep);
    if (!functionalReplace) {
        rep_val = JS_ToString(ctx, rep);
        if (JS_IsException(rep_val))
            goto exception;
        rp = JS_VALUE_GET_STRING(rep_val);
    }
    fullUnicode = 0;
    is_global = JS_ToBoolFree(ctx, JS_GetProperty(ctx, rx, JS_ATOM_global));
    if (is_global < 0)
        goto exception;
    if (is_global) {
        fullUnicode = JS_ToBoolFree(ctx, JS_GetProperty(ctx, rx, JS_ATOM_unicode));
        if (fullUnicode < 0)
            goto exception;
        if (JS_SetProperty(ctx, rx, JS_ATOM_lastIndex, JS_NewInt32(ctx, 0)) < 0)
            goto exception;
    }

    if (rp && rp->len == 0 && is_global && js_is_standard_regexp(ctx, rx)) {
        /* use faster version for simple cases */
        res = JS_RegExpDelete(ctx, rx, str);
        goto done;
    }
    for(;;) {
        JSValue result;
        result = JS_RegExpExec(ctx, rx, str);
        if (JS_IsException(result))
            goto exception;
        if (JS_IsNull(result))
            break;
        if (value_buffer_append(results, result) < 0)
            goto exception;
        if (!is_global)
            break;
        JS_FreeValue(ctx, matched);
        matched = JS_ToStringFree(ctx, JS_GetPropertyInt64(ctx, result, 0));
        if (JS_IsException(matched))
            goto exception;
        if (JS_IsEmptyString(matched)) {
            /* always advance of at least one char */
            int64_t thisIndex, nextIndex;
            if (JS_ToLengthFree(ctx, &thisIndex, JS_GetProperty(ctx, rx, JS_ATOM_lastIndex)) < 0)
                goto exception;
            nextIndex = string_advance_index(sp, thisIndex, fullUnicode);
            if (JS_SetProperty(ctx, rx, JS_ATOM_lastIndex, JS_NewInt64(ctx, nextIndex)) < 0)
                goto exception;
        }
    }
    nextSourcePosition = 0;
    for(j = 0; j < results->len; j++) {
        JSValueConst result;
        result = results->arr[j];
        if (js_get_length32(ctx, &nCaptures, result) < 0)
            goto exception;
        JS_FreeValue(ctx, matched);
        matched = JS_ToStringFree(ctx, JS_GetPropertyInt64(ctx, result, 0));
        if (JS_IsException(matched))
            goto exception;
        if (JS_ToLengthFree(ctx, &position, JS_GetProperty(ctx, result, JS_ATOM_index)))
            goto exception;
        if (position > sp->len)
            position = sp->len;
        else if (position < 0)
            position = 0;
        /* ignore substition if going backward (can happen
           with custom regexp object) */
        JS_FreeValue(ctx, tab);
        tab = JS_NewArray(ctx);
        if (JS_IsException(tab))
            goto exception;
        if (JS_DefinePropertyValueInt64(ctx, tab, 0, JS_DupValue(ctx, matched),
                                        JS_PROP_C_W_E | JS_PROP_THROW) < 0)
            goto exception;
        for(n = 1; n < nCaptures; n++) {
            JSValue capN;
            capN = JS_GetPropertyInt64(ctx, result, n);
            if (JS_IsException(capN))
                goto exception;
            if (!JS_IsUndefined(capN)) {
                capN = JS_ToStringFree(ctx, capN);
                if (JS_IsException(capN))
                    goto exception;
            }
            if (JS_DefinePropertyValueInt64(ctx, tab, n, capN,
                                            JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                goto exception;
        }
        JS_FreeValue(ctx, namedCaptures);
        namedCaptures = JS_GetProperty(ctx, result, JS_ATOM_groups);
        if (JS_IsException(namedCaptures))
            goto exception;
        if (functionalReplace) {
            if (JS_DefinePropertyValueInt64(ctx, tab, n++, JS_NewInt32(ctx, position), JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                goto exception;
            if (JS_DefinePropertyValueInt64(ctx, tab, n++, JS_DupValue(ctx, str), JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                goto exception;
            if (!JS_IsUndefined(namedCaptures)) {
                if (JS_DefinePropertyValueInt64(ctx, tab, n++, JS_DupValue(ctx, namedCaptures), JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                    goto exception;
            }
            args[0] = JS_UNDEFINED;
            args[1] = tab;
            JS_FreeValue(ctx, rep_str);
            rep_str = JS_ToStringFree(ctx, js_function_apply(ctx, rep, 2, args, 0));
        } else {
            JSValue namedCaptures1;
            if (!JS_IsUndefined(namedCaptures)) {
                namedCaptures1 = JS_ToObject(ctx, namedCaptures);
                if (JS_IsException(namedCaptures1))
                    goto exception;
            } else {
                namedCaptures1 = JS_UNDEFINED;
            }
            args[0] = matched;
            args[1] = str;
            args[2] = JS_NewInt32(ctx, position);
            args[3] = tab;
            args[4] = namedCaptures1;
            args[5] = rep_val;
            JS_FreeValue(ctx, rep_str);
            rep_str = js_string___GetSubstitution(ctx, JS_UNDEFINED, 6, args);
            JS_FreeValue(ctx, namedCaptures1);
        }
        if (JS_IsException(rep_str))
            goto exception;
        if (position >= nextSourcePosition) {
            string_buffer_concat(b, sp, nextSourcePosition, position);
            string_buffer_concat_value(b, rep_str);
            nextSourcePosition = position + JS_VALUE_GET_STRING(matched)->len;
        }
    }
    string_buffer_concat(b, sp, nextSourcePosition, sp->len);
    res = string_buffer_end(b);
    goto done1;

exception:
    res = JS_EXCEPTION;
done:
    string_buffer_free(b);
done1:
    value_buffer_free(results);
    JS_FreeValue(ctx, rep_val);
    JS_FreeValue(ctx, matched);
    JS_FreeValue(ctx, tab);
    JS_FreeValue(ctx, rep_str);
    JS_FreeValue(ctx, namedCaptures);
    JS_FreeValue(ctx, str);
    return res;
}

static JSValue js_regexp_Symbol_search(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv)
{
    JSValueConst rx = this_val;
    JSValue str, previousLastIndex, currentLastIndex, result, index;

    if (!JS_IsObject(rx))
        return JS_ThrowTypeErrorNotAnObject(ctx);

    result = JS_UNDEFINED;
    currentLastIndex = JS_UNDEFINED;
    previousLastIndex = JS_UNDEFINED;
    str = JS_ToString(ctx, argv[0]);
    if (JS_IsException(str))
        goto exception;

    previousLastIndex = JS_GetProperty(ctx, rx, JS_ATOM_lastIndex);
    if (JS_IsException(previousLastIndex))
        goto exception;

    if (!js_same_value(ctx, previousLastIndex, JS_NewInt32(ctx, 0))) {
        if (JS_SetProperty(ctx, rx, JS_ATOM_lastIndex, JS_NewInt32(ctx, 0)) < 0) {
            goto exception;
        }
    }
    result = JS_RegExpExec(ctx, rx, str);
    if (JS_IsException(result))
        goto exception;
    currentLastIndex = JS_GetProperty(ctx, rx, JS_ATOM_lastIndex);
    if (JS_IsException(currentLastIndex))
        goto exception;
    if (js_same_value(ctx, currentLastIndex, previousLastIndex)) {
        JS_FreeValue(ctx, previousLastIndex);
    } else {
        if (JS_SetProperty(ctx, rx, JS_ATOM_lastIndex, previousLastIndex) < 0) {
            previousLastIndex = JS_UNDEFINED;
            goto exception;
        }
    }
    JS_FreeValue(ctx, str);
    JS_FreeValue(ctx, currentLastIndex);

    if (JS_IsNull(result)) {
        return JS_NewInt32(ctx, -1);
    } else {
        index = JS_GetProperty(ctx, result, JS_ATOM_index);
        JS_FreeValue(ctx, result);
        return index;
    }

exception:
    JS_FreeValue(ctx, result);
    JS_FreeValue(ctx, str);
    JS_FreeValue(ctx, currentLastIndex);
    JS_FreeValue(ctx, previousLastIndex);
    return JS_EXCEPTION;
}

static JSValue js_regexp_Symbol_split(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv)
{
    // [Symbol.split](str, limit)
    JSValueConst rx = this_val;
    JSValueConst args[2];
    JSValue str, ctor, splitter, A, flags, z, sub;
    JSString *strp;
    uint32_t lim, size, p, q;
    int unicodeMatching;
    int64_t lengthA, e, numberOfCaptures, i;

    if (!JS_IsObject(rx))
        return JS_ThrowTypeErrorNotAnObject(ctx);

    ctor = JS_UNDEFINED;
    splitter = JS_UNDEFINED;
    A = JS_UNDEFINED;
    flags = JS_UNDEFINED;
    z = JS_UNDEFINED;
    str = JS_ToString(ctx, argv[0]);
    if (JS_IsException(str))
        goto exception;
    ctor = JS_SpeciesConstructor(ctx, rx, ctx->regexp_ctor);
    if (JS_IsException(ctor))
        goto exception;
    flags = JS_ToStringFree(ctx, JS_GetProperty(ctx, rx, JS_ATOM_flags));
    if (JS_IsException(flags))
        goto exception;
    strp = JS_VALUE_GET_STRING(flags);
    unicodeMatching = string_indexof_char(strp, 'u', 0) >= 0;
    if (string_indexof_char(strp, 'y', 0) < 0) {
        flags = JS_ConcatString3(ctx, "", flags, "y");
        if (JS_IsException(flags))
            goto exception;
    }
    args[0] = rx;
    args[1] = flags;
    splitter = JS_CallConstructor(ctx, ctor, 2, args);
    if (JS_IsException(splitter))
        goto exception;
    A = JS_NewArray(ctx);
    if (JS_IsException(A))
        goto exception;
    lengthA = 0;
    if (JS_IsUndefined(argv[1])) {
        lim = 0xffffffff;
    } else {
        if (JS_ToUint32(ctx, &lim, argv[1]) < 0)
            goto exception;
        if (lim == 0)
            goto done;
    }
    strp = JS_VALUE_GET_STRING(str);
    p = q = 0;
    size = strp->len;
    if (size == 0) {
        z = JS_RegExpExec(ctx, splitter, str);
        if (JS_IsException(z))
            goto exception;
        if (JS_IsNull(z))
            goto add_tail;
        goto done;
    }
    while (q < size) {
        if (JS_SetProperty(ctx, splitter, JS_ATOM_lastIndex, JS_NewInt32(ctx, q)) < 0)
            goto exception;
        JS_FreeValue(ctx, z);    
        z = JS_RegExpExec(ctx, splitter, str);
        if (JS_IsException(z))
            goto exception;
        if (JS_IsNull(z)) {
            q = string_advance_index(strp, q, unicodeMatching);
        } else {
            if (JS_ToLengthFree(ctx, &e, JS_GetProperty(ctx, splitter, JS_ATOM_lastIndex)))
                goto exception;
            if (e > size)
                e = size;
            if (e == p) {
                q = string_advance_index(strp, q, unicodeMatching);
            } else {
                sub = js_sub_string(ctx, strp, p, q);
                if (JS_IsException(sub))
                    goto exception;
                if (JS_DefinePropertyValueInt64(ctx, A, lengthA++, sub,
                                                JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                    goto exception;
                if (lengthA == lim)
                    goto done;
                p = e;
                if (js_get_length64(ctx, &numberOfCaptures, z))
                    goto exception;
                for(i = 1; i < numberOfCaptures; i++) {
                    sub = JS_ToStringFree(ctx, JS_GetPropertyInt64(ctx, z, i));
                    if (JS_IsException(sub))
                        goto exception;
                    if (JS_DefinePropertyValueInt64(ctx, A, lengthA++, sub, JS_PROP_C_W_E | JS_PROP_THROW) < 0)
                        goto exception;
                    if (lengthA == lim)
                        goto done;
                }
                q = p;
            }
        }
    }
add_tail:
    if (p > size)
        p = size;
    sub = js_sub_string(ctx, strp, p, size);
    if (JS_IsException(sub))
        goto exception;
    if (JS_DefinePropertyValueInt64(ctx, A, lengthA++, sub, JS_PROP_C_W_E | JS_PROP_THROW) < 0)
        goto exception;
    goto done;
exception:
    JS_FreeValue(ctx, A);
    A = JS_EXCEPTION;
done:
    JS_FreeValue(ctx, str);
    JS_FreeValue(ctx, ctor);
    JS_FreeValue(ctx, splitter);
    JS_FreeValue(ctx, flags);
    JS_FreeValue(ctx, z);    
    return A;
}

static const JSCFunctionListEntry js_regexp_funcs[] = {
    JS_CGETSET_DEF("[Symbol.species]", js_get_this, NULL ),
    //JS_CFUNC_DEF("__RegExpExec", 2, js_regexp___RegExpExec ),
    //JS_CFUNC_DEF("__RegExpDelete", 2, js_regexp___RegExpDelete ),
};

static const JSCFunctionListEntry js_regexp_proto_funcs[] = {
    JS_CGETSET_DEF("flags", js_regexp_get_flags, NULL ),
    JS_CGETSET_DEF("source", js_regexp_get_source, NULL ),
    JS_CGETSET_MAGIC_DEF("global", js_regexp_get_flag, NULL, 1 ),
    JS_CGETSET_MAGIC_DEF("ignoreCase", js_regexp_get_flag, NULL, 2 ),
    JS_CGETSET_MAGIC_DEF("multiline", js_regexp_get_flag, NULL, 4 ),
    JS_CGETSET_MAGIC_DEF("dotAll", js_regexp_get_flag, NULL, 8 ),
    JS_CGETSET_MAGIC_DEF("unicode", js_regexp_get_flag, NULL, 16 ),
    JS_CGETSET_MAGIC_DEF("sticky", js_regexp_get_flag, NULL, 32 ),
    JS_CFUNC_DEF("exec", 1, js_regexp_exec ),
    JS_CFUNC_DEF("compile", 2, js_regexp_compile ),
    JS_CFUNC_DEF("test", 1, js_regexp_test ),
    JS_CFUNC_DEF("toString", 0, js_regexp_toString ),
    JS_CFUNC_DEF("[Symbol.replace]", 2, js_regexp_Symbol_replace ),
    JS_CFUNC_DEF("[Symbol.match]", 1, js_regexp_Symbol_match ),
    JS_CFUNC_DEF("[Symbol.matchAll]", 1, js_regexp_Symbol_matchAll ),
    JS_CFUNC_DEF("[Symbol.search]", 1, js_regexp_Symbol_search ),
    JS_CFUNC_DEF("[Symbol.split]", 2, js_regexp_Symbol_split ),
    //JS_CGETSET_DEF("__source", js_regexp_get___source, NULL ),
    //JS_CGETSET_DEF("__flags", js_regexp_get___flags, NULL ),
};

static const JSCFunctionListEntry js_regexp_string_iterator_proto_funcs[] = {
    JS_ITERATOR_NEXT_DEF("next", 0, js_regexp_string_iterator_next, 0 ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "RegExp String Iterator", JS_PROP_CONFIGURABLE ),
};

void JS_AddIntrinsicRegExpCompiler(JSContext *ctx)
{
    ctx->compile_regexp = js_compile_regexp;
}

void JS_AddIntrinsicRegExp(JSContext *ctx)
{
    JSValueConst obj;

    JS_AddIntrinsicRegExpCompiler(ctx);

    ctx->class_proto[JS_CLASS_REGEXP] = JS_NewObject(ctx);
    JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_REGEXP], js_regexp_proto_funcs,
                               countof(js_regexp_proto_funcs));
    obj = JS_NewGlobalCConstructor(ctx, "RegExp", js_regexp_constructor, 2,
                                   ctx->class_proto[JS_CLASS_REGEXP]);
    ctx->regexp_ctor = JS_DupValue(ctx, obj);
    JS_SetPropertyFunctionList(ctx, obj, js_regexp_funcs, countof(js_regexp_funcs));

    ctx->class_proto[JS_CLASS_REGEXP_STRING_ITERATOR] =
        JS_NewObjectProto(ctx, ctx->iterator_proto);
    JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_REGEXP_STRING_ITERATOR],
                               js_regexp_string_iterator_proto_funcs,
                               countof(js_regexp_string_iterator_proto_funcs));
}

/* JSON */

static int json_parse_expect(JSParseState *s, int tok)
{
    if (s->token.val != tok) {
        /* XXX: dump token correctly in all cases */
        return js_parse_error(s, "expecting '%c'", tok);
    }
    return json_next_token(s);
}

static JSValue json_parse_value(JSParseState *s)
{
    JSContext *ctx = s->ctx;
    JSValue val = JS_NULL;
    int ret;

    switch(s->token.val) {
    case '{':
        {
            JSValue prop_val;
            JSAtom prop_name;
            
            if (json_next_token(s))
                goto fail;
            val = JS_NewObject(ctx);
            if (JS_IsException(val))
                goto fail;
            if (s->token.val != '}') {
                for(;;) {
                    if (s->token.val == TOK_STRING) {
                        prop_name = JS_ValueToAtom(ctx, s->token.u.str.str);
                        if (prop_name == JS_ATOM_NULL)
                            goto fail;
                    } else if (s->ext_json && s->token.val == TOK_IDENT) {
                        prop_name = JS_DupAtom(ctx, s->token.u.ident.atom);
                    } else {
                        js_parse_error(s, "expecting property name");
                        goto fail;
                    }
                    if (json_next_token(s))
                        goto fail1;
                    if (json_parse_expect(s, ':'))
                        goto fail1;
                    prop_val = json_parse_value(s);
                    if (JS_IsException(prop_val)) {
                    fail1:
                        JS_FreeAtom(ctx, prop_name);
                        goto fail;
                    }
                    ret = JS_DefinePropertyValue(ctx, val, prop_name,
                                                 prop_val, JS_PROP_C_W_E);
                    JS_FreeAtom(ctx, prop_name);
                    if (ret < 0)
                        goto fail;

                    if (s->token.val != ',')
                        break;
                    if (json_next_token(s))
                        goto fail;
                    if (s->ext_json && s->token.val == '}')
                        break;
                }
            }
            if (json_parse_expect(s, '}'))
                goto fail;
        }
        break;
    case '[':
        {
            JSValue el;
            uint32_t idx;

            if (json_next_token(s))
                goto fail;
            val = JS_NewArray(ctx);
            if (JS_IsException(val))
                goto fail;
            if (s->token.val != ']') {
                idx = 0;
                for(;;) {
                    el = json_parse_value(s);
                    if (JS_IsException(el))
                        goto fail;
                    ret = JS_DefinePropertyValueUint32(ctx, val, idx, el, JS_PROP_C_W_E);
                    if (ret < 0)
                        goto fail;
                    if (s->token.val != ',')
                        break;
                    if (json_next_token(s))
                        goto fail;
                    idx++;
                    if (s->ext_json && s->token.val == ']')
                        break;
                }
            }
            if (json_parse_expect(s, ']'))
                goto fail;
        }
        break;
    case TOK_STRING:
        val = JS_DupValue(ctx, s->token.u.str.str);
        if (json_next_token(s))
            goto fail;
        break;
    case TOK_NUMBER:
        val = s->token.u.num.val;
        if (json_next_token(s))
            goto fail;
        break;
    case TOK_IDENT:
        if (s->token.u.ident.atom == JS_ATOM_false ||
            s->token.u.ident.atom == JS_ATOM_true) {
            val = JS_NewBool(ctx, (s->token.u.ident.atom == JS_ATOM_true));
        } else if (s->token.u.ident.atom == JS_ATOM_null) {
            val = JS_NULL;
        } else {
            goto def_token;
        }
        if (json_next_token(s))
            goto fail;
        break;
    default:
    def_token:
        if (s->token.val == TOK_EOF) {
            js_parse_error(s, "unexpected end of input");
        } else {
            js_parse_error(s, "unexpected token: '%.*s'",
                           (int)(s->buf_ptr - s->token.ptr), s->token.ptr);
        }
        goto fail;
    }
    return val;
 fail:
    JS_FreeValue(ctx, val);
    return JS_EXCEPTION;
}

JSValue JS_ParseJSON2(JSContext *ctx, const char *buf, size_t buf_len,
                      const char *filename, int flags)
{
    JSParseState s1, *s = &s1;
    JSValue val = JS_UNDEFINED;

    js_parse_init(ctx, s, buf, buf_len, filename);
    s->ext_json = ((flags & JS_PARSE_JSON_EXT) != 0);
    if (json_next_token(s))
        goto fail;
    val = json_parse_value(s);
    if (JS_IsException(val))
        goto fail;
    if (s->token.val != TOK_EOF) {
        if (js_parse_error(s, "unexpected data at the end"))
            goto fail;
    }
    return val;
 fail:
    JS_FreeValue(ctx, val);
    free_token(s, &s->token);
    return JS_EXCEPTION;
}

JSValue JS_ParseJSON(JSContext *ctx, const char *buf, size_t buf_len,
                     const char *filename)
{
    return JS_ParseJSON2(ctx, buf, buf_len, filename, 0); 
}

static JSValue internalize_json_property(JSContext *ctx, JSValueConst holder,
                                         JSAtom name, JSValueConst reviver)
{
    JSValue val, new_el, name_val, res;
    JSValueConst args[2];
    int ret, is_array;
    uint32_t i, len = 0;
    JSAtom prop;
    JSPropertyEnum *atoms = NULL;

    if (js_check_stack_overflow(ctx->rt, 0)) {
        return JS_ThrowStackOverflow(ctx);
    }

    val = JS_GetProperty(ctx, holder, name);
    if (JS_IsException(val))
        return val;
    if (JS_IsObject(val)) {
        is_array = JS_IsArray(ctx, val);
        if (is_array < 0)
            goto fail;
        if (is_array) {
            if (js_get_length32(ctx, &len, val))
                goto fail;
        } else {
            ret = JS_GetOwnPropertyNamesInternal(ctx, &atoms, &len, JS_VALUE_GET_OBJ(val), JS_GPN_ENUM_ONLY | JS_GPN_STRING_MASK);
            if (ret < 0)
                goto fail;
        }
        for(i = 0; i < len; i++) {
            if (is_array) {
                prop = JS_NewAtomUInt32(ctx, i);
                if (prop == JS_ATOM_NULL)
                    goto fail;
            } else {
                prop = JS_DupAtom(ctx, atoms[i].atom);
            }
            new_el = internalize_json_property(ctx, val, prop, reviver);
            if (JS_IsException(new_el)) {
                JS_FreeAtom(ctx, prop);
                goto fail;
            }
            if (JS_IsUndefined(new_el)) {
                ret = JS_DeleteProperty(ctx, val, prop, 0);
            } else {
                ret = JS_DefinePropertyValue(ctx, val, prop, new_el, JS_PROP_C_W_E);
            }
            JS_FreeAtom(ctx, prop);
            if (ret < 0)
                goto fail;
        }
    }
    js_free_prop_enum(ctx, atoms, len);
    atoms = NULL;
    name_val = JS_AtomToValue(ctx, name);
    if (JS_IsException(name_val))
        goto fail;
    args[0] = name_val;
    args[1] = val;
    res = JS_Call(ctx, reviver, holder, 2, args);
    JS_FreeValue(ctx, name_val);
    JS_FreeValue(ctx, val);
    return res;
 fail:
    js_free_prop_enum(ctx, atoms, len);
    JS_FreeValue(ctx, val);
    return JS_EXCEPTION;
}

static JSValue js_json_parse(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv)
{
    JSValue obj, root;
    JSValueConst reviver;
    const char *str;
    size_t len;

    str = JS_ToCStringLen(ctx, &len, argv[0]);
    if (!str)
        return JS_EXCEPTION;
    obj = JS_ParseJSON(ctx, str, len, "<input>");
    JS_FreeCString(ctx, str);
    if (JS_IsException(obj))
        return obj;
    if (argc > 1 && JS_IsFunction(ctx, argv[1])) {
        reviver = argv[1];
        root = JS_NewObject(ctx);
        if (JS_IsException(root)) {
            JS_FreeValue(ctx, obj);
            return JS_EXCEPTION;
        }
        if (JS_DefinePropertyValue(ctx, root, JS_ATOM_empty_string, obj,
                                   JS_PROP_C_W_E) < 0) {
            JS_FreeValue(ctx, root);
            return JS_EXCEPTION;
        }
        obj = internalize_json_property(ctx, root, JS_ATOM_empty_string,
                                        reviver);
        JS_FreeValue(ctx, root);
    }
    return obj;
}

typedef struct JSONStringifyContext {
    JSValueConst replacer_func;
    JSValue stack;
    JSValue property_list;
    JSValue gap;
    JSValue empty;
    StringBuffer *b;
} JSONStringifyContext;

static JSValue JS_ToQuotedStringFree(JSContext *ctx, JSValue val) {
    JSValue r = JS_ToQuotedString(ctx, val);
    JS_FreeValue(ctx, val);
    return r;
}

static JSValue js_json_check(JSContext *ctx, JSONStringifyContext *jsc,
                             JSValueConst holder, JSValue val, JSValueConst key)
{
    JSValue v;
    JSValueConst args[2];

    if (JS_IsObject(val)
#ifdef CONFIG_BIGNUM
    ||  JS_IsBigInt(ctx, val)   /* XXX: probably useless */
#endif
        ) {
            JSValue f = JS_GetProperty(ctx, val, JS_ATOM_toJSON);
            if (JS_IsException(f))
                goto exception;
            if (JS_IsFunction(ctx, f)) {
                v = JS_CallFree(ctx, f, val, 1, &key);
                JS_FreeValue(ctx, val);
                val = v;
                if (JS_IsException(val))
                    goto exception;
            } else {
                JS_FreeValue(ctx, f);
            }
        }

    if (!JS_IsUndefined(jsc->replacer_func)) {
        args[0] = key;
        args[1] = val;
        v = JS_Call(ctx, jsc->replacer_func, holder, 2, args);
        JS_FreeValue(ctx, val);
        val = v;
        if (JS_IsException(val))
            goto exception;
    }

    switch (JS_VALUE_GET_NORM_TAG(val)) {
    case JS_TAG_OBJECT:
        if (JS_IsFunction(ctx, val))
            break;
    case JS_TAG_STRING:
    case JS_TAG_INT:
    case JS_TAG_FLOAT64:
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_FLOAT:
#endif
    case JS_TAG_BOOL:
    case JS_TAG_NULL:
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
#endif
    case JS_TAG_EXCEPTION:
        return val;
    default:
        break;
    }
    JS_FreeValue(ctx, val);
    return JS_UNDEFINED;

exception:
    JS_FreeValue(ctx, val);
    return JS_EXCEPTION;
}

static int js_json_to_str(JSContext *ctx, JSONStringifyContext *jsc,
                          JSValueConst holder, JSValue val,
                          JSValueConst indent)
{
    JSValue indent1, sep, sep1, tab, v, prop;
    JSObject *p;
    int64_t i, len;
    int cl, ret;
    BOOL has_content;
    
    indent1 = JS_UNDEFINED;
    sep = JS_UNDEFINED;
    sep1 = JS_UNDEFINED;
    tab = JS_UNDEFINED;
    prop = JS_UNDEFINED;

    switch (JS_VALUE_GET_NORM_TAG(val)) {
    case JS_TAG_OBJECT:
        p = JS_VALUE_GET_OBJ(val);
        cl = p->class_id;
        if (cl == JS_CLASS_STRING) {
            val = JS_ToStringFree(ctx, val);
            if (JS_IsException(val))
                goto exception;
            val = JS_ToQuotedStringFree(ctx, val);
            if (JS_IsException(val))
                goto exception;
            return string_buffer_concat_value_free(jsc->b, val);
        } else if (cl == JS_CLASS_NUMBER) {
            val = JS_ToNumberFree(ctx, val);
            if (JS_IsException(val))
                goto exception;
            return string_buffer_concat_value_free(jsc->b, val);
        } else if (cl == JS_CLASS_BOOLEAN) {
            ret = string_buffer_concat_value(jsc->b, p->u.object_data);
            JS_FreeValue(ctx, val);
            return ret;
        }
#ifdef CONFIG_BIGNUM
        else if (cl == JS_CLASS_BIG_FLOAT) {
            return string_buffer_concat_value_free(jsc->b, val);
        } else if (cl == JS_CLASS_BIG_INT) {
            JS_ThrowTypeError(ctx, "bigint are forbidden in JSON.stringify");
            goto exception;
        }
#endif
        v = js_array_includes(ctx, jsc->stack, 1, (JSValueConst *)&val);
        if (JS_IsException(v))
            goto exception;
        if (JS_ToBoolFree(ctx, v)) {
            JS_ThrowTypeError(ctx, "circular reference");
            goto exception;
        }
        indent1 = JS_ConcatString(ctx, JS_DupValue(ctx, indent), JS_DupValue(ctx, jsc->gap));
        if (JS_IsException(indent1))
            goto exception;
        if (!JS_IsEmptyString(jsc->gap)) {
            sep = JS_ConcatString3(ctx, "\n", JS_DupValue(ctx, indent1), "");
            if (JS_IsException(sep))
                goto exception;
            sep1 = JS_NewString(ctx, " ");
            if (JS_IsException(sep1))
                goto exception;
        } else {
            sep = JS_DupValue(ctx, jsc->empty);
            sep1 = JS_DupValue(ctx, jsc->empty);
        }
        v = js_array_push(ctx, jsc->stack, 1, (JSValueConst *)&val, 0);
        if (check_exception_free(ctx, v))
            goto exception;
        ret = JS_IsArray(ctx, val);
        if (ret < 0)
            goto exception;
        if (ret) {
            if (js_get_length64(ctx, &len, val))
                goto exception;
            string_buffer_putc8(jsc->b, '[');
            for(i = 0; i < len; i++) {
                if (i > 0)
                    string_buffer_putc8(jsc->b, ',');
                string_buffer_concat_value(jsc->b, sep);
                v = JS_GetPropertyInt64(ctx, val, i);
                if (JS_IsException(v))
                    goto exception;
                /* XXX: could do this string conversion only when needed */
                prop = JS_ToStringFree(ctx, JS_NewInt64(ctx, i));
                if (JS_IsException(prop))
                    goto exception;
                v = js_json_check(ctx, jsc, val, v, prop);
                JS_FreeValue(ctx, prop);
                prop = JS_UNDEFINED;
                if (JS_IsException(v))
                    goto exception;
                if (JS_IsUndefined(v))
                    v = JS_NULL;
                if (js_json_to_str(ctx, jsc, val, v, indent1))
                    goto exception;
            }
            if (len > 0 && !JS_IsEmptyString(jsc->gap)) {
                string_buffer_putc8(jsc->b, '\n');
                string_buffer_concat_value(jsc->b, indent);
            }
            string_buffer_putc8(jsc->b, ']');
        } else {
            if (!JS_IsUndefined(jsc->property_list))
                tab = JS_DupValue(ctx, jsc->property_list);
            else
                tab = js_object_keys(ctx, JS_UNDEFINED, 1, (JSValueConst *)&val, JS_ITERATOR_KIND_KEY);
            if (JS_IsException(tab))
                goto exception;
            if (js_get_length64(ctx, &len, tab))
                goto exception;
            string_buffer_putc8(jsc->b, '{');
            has_content = FALSE;
            for(i = 0; i < len; i++) {
                JS_FreeValue(ctx, prop);
                prop = JS_GetPropertyInt64(ctx, tab, i);
                if (JS_IsException(prop))
                    goto exception;
                v = JS_GetPropertyValue(ctx, val, JS_DupValue(ctx, prop));
                if (JS_IsException(v))
                    goto exception;
                v = js_json_check(ctx, jsc, val, v, prop);
                if (JS_IsException(v))
                    goto exception;
                if (!JS_IsUndefined(v)) {
                    if (has_content)
                        string_buffer_putc8(jsc->b, ',');
                    prop = JS_ToQuotedStringFree(ctx, prop);
                    if (JS_IsException(prop)) {
                        JS_FreeValue(ctx, v);
                        goto exception;
                    }
                    string_buffer_concat_value(jsc->b, sep);
                    string_buffer_concat_value(jsc->b, prop);
                    string_buffer_putc8(jsc->b, ':');
                    string_buffer_concat_value(jsc->b, sep1);
                    if (js_json_to_str(ctx, jsc, val, v, indent1))
                        goto exception;
                    has_content = TRUE;
                }
            }
            if (has_content && JS_VALUE_GET_STRING(jsc->gap)->len != 0) {
                string_buffer_putc8(jsc->b, '\n');
                string_buffer_concat_value(jsc->b, indent);
            }
            string_buffer_putc8(jsc->b, '}');
        }
        if (check_exception_free(ctx, js_array_pop(ctx, jsc->stack, 0, NULL, 0)))
            goto exception;
        JS_FreeValue(ctx, val);
        JS_FreeValue(ctx, tab);
        JS_FreeValue(ctx, sep);
        JS_FreeValue(ctx, sep1);
        JS_FreeValue(ctx, indent1);
        JS_FreeValue(ctx, prop);
        return 0;
    case JS_TAG_STRING:
        val = JS_ToQuotedStringFree(ctx, val);
        if (JS_IsException(val))
            goto exception;
        goto concat_value;
    case JS_TAG_FLOAT64:
        if (!isfinite(JS_VALUE_GET_FLOAT64(val))) {
            val = JS_NULL;
        }
        goto concat_value;
    case JS_TAG_INT:
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_FLOAT:
#endif
    case JS_TAG_BOOL:
    case JS_TAG_NULL:
    concat_value:
        return string_buffer_concat_value_free(jsc->b, val);
#ifdef CONFIG_BIGNUM
    case JS_TAG_BIG_INT:
        JS_ThrowTypeError(ctx, "bigint are forbidden in JSON.stringify");
        goto exception;
#endif
    default:
        JS_FreeValue(ctx, val);
        return 0;
    }
    
exception:
    JS_FreeValue(ctx, val);
    JS_FreeValue(ctx, tab);
    JS_FreeValue(ctx, sep);
    JS_FreeValue(ctx, sep1);
    JS_FreeValue(ctx, indent1);
    JS_FreeValue(ctx, prop);
    return -1;
}

JSValue JS_JSONStringify(JSContext *ctx, JSValueConst obj,
                         JSValueConst replacer, JSValueConst space0)
{
    StringBuffer b_s;
    JSONStringifyContext jsc_s, *jsc = &jsc_s;
    JSValue val, v, space, ret, wrapper;
    int res;
    int64_t i, j, n;

    jsc->replacer_func = JS_UNDEFINED;
    jsc->stack = JS_UNDEFINED;
    jsc->property_list = JS_UNDEFINED;
    jsc->gap = JS_UNDEFINED;
    jsc->b = &b_s;
    jsc->empty = JS_AtomToString(ctx, JS_ATOM_empty_string);
    ret = JS_UNDEFINED;
    wrapper = JS_UNDEFINED;

    string_buffer_init(ctx, jsc->b, 0);
    jsc->stack = JS_NewArray(ctx);
    if (JS_IsException(jsc->stack))
        goto exception;
    if (JS_IsFunction(ctx, replacer)) {
        jsc->replacer_func = replacer;
    } else {
        res = JS_IsArray(ctx, replacer);
        if (res < 0)
            goto exception;
        if (res) {
            /* XXX: enumeration is not fully correct */
            jsc->property_list = JS_NewArray(ctx);
            if (JS_IsException(jsc->property_list))
                goto exception;
            if (js_get_length64(ctx, &n, replacer))
                goto exception;
            for (i = j = 0; i < n; i++) {
                JSValue present;
                v = JS_GetPropertyInt64(ctx, replacer, i);
                if (JS_IsException(v))
                    goto exception;
                if (JS_IsObject(v)) {
                    JSObject *p = JS_VALUE_GET_OBJ(v);
                    if (p->class_id == JS_CLASS_STRING ||
                        p->class_id == JS_CLASS_NUMBER) {
                        v = JS_ToStringFree(ctx, v);
                        if (JS_IsException(v))
                            goto exception;
                    } else {
                        JS_FreeValue(ctx, v);
                        continue;
                    }
                } else if (JS_IsNumber(v)) {
                    v = JS_ToStringFree(ctx, v);
                    if (JS_IsException(v))
                        goto exception;
                } else if (!JS_IsString(v)) {
                    JS_FreeValue(ctx, v);
                    continue;
                }
                present = js_array_includes(ctx, jsc->property_list,
                                            1, (JSValueConst *)&v);
                if (JS_IsException(present)) {
                    JS_FreeValue(ctx, v);
                    goto exception;
                }
                if (!JS_ToBoolFree(ctx, present)) {
                    JS_SetPropertyInt64(ctx, jsc->property_list, j++, v);
                } else {
                    JS_FreeValue(ctx, v);
                }
            }
        }
    }
    space = JS_DupValue(ctx, space0);
    if (JS_IsObject(space)) {
        JSObject *p = JS_VALUE_GET_OBJ(space);
        if (p->class_id == JS_CLASS_NUMBER) {
            space = JS_ToNumberFree(ctx, space);
        } else if (p->class_id == JS_CLASS_STRING) {
            space = JS_ToStringFree(ctx, space);
        }
        if (JS_IsException(space)) {
            JS_FreeValue(ctx, space);
            goto exception;
        }
    }
    if (JS_IsNumber(space)) {
        int n;
        if (JS_ToInt32Clamp(ctx, &n, space, 0, 10, 0))
            goto exception;
        jsc->gap = JS_NewStringLen(ctx, "          ", n);
    } else if (JS_IsString(space)) {
        JSString *p = JS_VALUE_GET_STRING(space);
        jsc->gap = js_sub_string(ctx, p, 0, min_int(p->len, 10));
    } else {
        jsc->gap = JS_DupValue(ctx, jsc->empty);
    }
    JS_FreeValue(ctx, space);
    if (JS_IsException(jsc->gap))
        goto exception;
    wrapper = JS_NewObject(ctx);
    if (JS_IsException(wrapper))
        goto exception;
    if (JS_DefinePropertyValue(ctx, wrapper, JS_ATOM_empty_string,
                               JS_DupValue(ctx, obj), JS_PROP_C_W_E) < 0)
        goto exception;
    val = JS_DupValue(ctx, obj);
                           
    val = js_json_check(ctx, jsc, wrapper, val, jsc->empty);
    if (JS_IsException(val))
        goto exception;
    if (JS_IsUndefined(val)) {
        ret = JS_UNDEFINED;
        goto done1;
    }
    if (js_json_to_str(ctx, jsc, wrapper, val, jsc->empty))
        goto exception;

    ret = string_buffer_end(jsc->b);
    goto done;

exception:
    ret = JS_EXCEPTION;
done1:
    string_buffer_free(jsc->b);
done:
    JS_FreeValue(ctx, wrapper);
    JS_FreeValue(ctx, jsc->empty);
    JS_FreeValue(ctx, jsc->gap);
    JS_FreeValue(ctx, jsc->property_list);
    JS_FreeValue(ctx, jsc->stack);
    return ret;
}

static JSValue js_json_stringify(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    // stringify(val, replacer, space)
    return JS_JSONStringify(ctx, argv[0], argv[1], argv[2]);
}

static const JSCFunctionListEntry js_json_funcs[] = {
    JS_CFUNC_DEF("parse", 2, js_json_parse ),
    JS_CFUNC_DEF("stringify", 3, js_json_stringify ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "JSON", JS_PROP_CONFIGURABLE ),
};

static const JSCFunctionListEntry js_json_obj[] = {
    JS_OBJECT_DEF("JSON", js_json_funcs, countof(js_json_funcs), JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE ),
};

void JS_AddIntrinsicJSON(JSContext *ctx)
{
    /* add JSON as autoinit object */
    JS_SetPropertyFunctionList(ctx, ctx->global_obj, js_json_obj, countof(js_json_obj));
}

/* Reflect */

static JSValue js_reflect_apply(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    return js_function_apply(ctx, argv[0], max_int(0, argc - 1), argv + 1, 2);
}

static JSValue js_reflect_construct(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv)
{
    JSValueConst func, array_arg, new_target;
    JSValue *tab, ret;
    uint32_t len;

    func = argv[0];
    array_arg = argv[1];
    if (argc > 2) {
        new_target = argv[2];
        if (!JS_IsConstructor(ctx, new_target))
            return JS_ThrowTypeError(ctx, "not a constructor");
    } else {
        new_target = func;
    }
    tab = build_arg_list(ctx, &len, array_arg);
    if (!tab)
        return JS_EXCEPTION;
    ret = JS_CallConstructor2(ctx, func, new_target, len, (JSValueConst *)tab);
    free_arg_list(ctx, tab, len);
    return ret;
}

static JSValue js_reflect_deleteProperty(JSContext *ctx, JSValueConst this_val,
                                         int argc, JSValueConst *argv)
{
    JSValueConst obj;
    JSAtom atom;
    int ret;

    obj = argv[0];
    if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)
        return JS_ThrowTypeErrorNotAnObject(ctx);
    atom = JS_ValueToAtom(ctx, argv[1]);
    if (unlikely(atom == JS_ATOM_NULL))
        return JS_EXCEPTION;
    ret = JS_DeleteProperty(ctx, obj, atom, 0);
    JS_FreeAtom(ctx, atom);
    if (ret < 0)
        return JS_EXCEPTION;
    else
        return JS_NewBool(ctx, ret);
}

static JSValue js_reflect_get(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv)
{
    JSValueConst obj, prop, receiver;
    JSAtom atom;
    JSValue ret;

    obj = argv[0];
    prop = argv[1];
    if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)
        return JS_ThrowTypeErrorNotAnObject(ctx);
    if (argc > 2)
        receiver = argv[2];
    else
        receiver = obj;
    atom = JS_ValueToAtom(ctx, prop);
    if (unlikely(atom == JS_ATOM_NULL))
        return JS_EXCEPTION;
    ret = JS_GetPropertyInternal(ctx, obj, atom, receiver, FALSE);
    JS_FreeAtom(ctx, atom);
    return ret;
}

static JSValue js_reflect_has(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv)
{
    JSValueConst obj, prop;
    JSAtom atom;
    int ret;

    obj = argv[0];
    prop = argv[1];
    if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)
        return JS_ThrowTypeErrorNotAnObject(ctx);
    atom = JS_ValueToAtom(ctx, prop);
    if (unlikely(atom == JS_ATOM_NULL))
        return JS_EXCEPTION;
    ret = JS_HasProperty(ctx, obj, atom);
    JS_FreeAtom(ctx, atom);
    if (ret < 0)
        return JS_EXCEPTION;
    else
        return JS_NewBool(ctx, ret);
}

static JSValue js_reflect_set(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv)
{
    JSValueConst obj, prop, val, receiver;
    int ret;
    JSAtom atom;

    obj = argv[0];
    prop = argv[1];
    val = argv[2];
    if (argc > 3)
        receiver = argv[3];
    else
        receiver = obj;
    if (JS_VALUE_GET_TAG(obj) != JS_TAG_OBJECT)
        return JS_ThrowTypeErrorNotAnObject(ctx);
    atom = JS_ValueToAtom(ctx, prop);
    if (unlikely(atom == JS_ATOM_NULL))
        return JS_EXCEPTION;
    ret = JS_SetPropertyGeneric(ctx, obj, atom,
                                JS_DupValue(ctx, val), receiver, 0);
    JS_FreeAtom(ctx, atom);
    if (ret < 0)
        return JS_EXCEPTION;
    else
        return JS_NewBool(ctx, ret);
}

static JSValue js_reflect_setPrototypeOf(JSContext *ctx, JSValueConst this_val,
                                         int argc, JSValueConst *argv)
{
    int ret;
    ret = JS_SetPrototypeInternal(ctx, argv[0], argv[1], FALSE);
    if (ret < 0)
        return JS_EXCEPTION;
    else
        return JS_NewBool(ctx, ret);
}

static JSValue js_reflect_ownKeys(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv)
{
    if (JS_VALUE_GET_TAG(argv[0]) != JS_TAG_OBJECT)
        return JS_ThrowTypeErrorNotAnObject(ctx);
    return JS_GetOwnPropertyNames2(ctx, argv[0],
                                   JS_GPN_STRING_MASK | JS_GPN_SYMBOL_MASK,
                                   JS_ITERATOR_KIND_KEY);
}

static const JSCFunctionListEntry js_reflect_funcs[] = {
    JS_CFUNC_DEF("apply", 3, js_reflect_apply ),
    JS_CFUNC_DEF("construct", 2, js_reflect_construct ),
    JS_CFUNC_MAGIC_DEF("defineProperty", 3, js_object_defineProperty, 1 ),
    JS_CFUNC_DEF("deleteProperty", 2, js_reflect_deleteProperty ),
    JS_CFUNC_DEF("get", 2, js_reflect_get ),
    JS_CFUNC_MAGIC_DEF("getOwnPropertyDescriptor", 2, js_object_getOwnPropertyDescriptor, 1 ),
    JS_CFUNC_MAGIC_DEF("getPrototypeOf", 1, js_object_getPrototypeOf, 1 ),
    JS_CFUNC_DEF("has", 2, js_reflect_has ),
    JS_CFUNC_MAGIC_DEF("isExtensible", 1, js_object_isExtensible, 1 ),
    JS_CFUNC_DEF("ownKeys", 1, js_reflect_ownKeys ),
    JS_CFUNC_MAGIC_DEF("preventExtensions", 1, js_object_preventExtensions, 1 ),
    JS_CFUNC_DEF("set", 3, js_reflect_set ),
    JS_CFUNC_DEF("setPrototypeOf", 2, js_reflect_setPrototypeOf ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "Reflect", JS_PROP_CONFIGURABLE ),
};

static const JSCFunctionListEntry js_reflect_obj[] = {
    JS_OBJECT_DEF("Reflect", js_reflect_funcs, countof(js_reflect_funcs), JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE ),
};

/* Proxy */

static void js_proxy_finalizer(JSRuntime *rt, JSValue val)
{
    JSProxyData *s = JS_GetOpaque(val, JS_CLASS_PROXY);
    if (s) {
        JS_FreeValueRT(rt, s->target);
        JS_FreeValueRT(rt, s->handler);
        js_free_rt(rt, s);
    }
}

static void js_proxy_mark(JSRuntime *rt, JSValueConst val,
                          JS_MarkFunc *mark_func)
{
    JSProxyData *s = JS_GetOpaque(val, JS_CLASS_PROXY);
    if (s) {
        JS_MarkValue(rt, s->target, mark_func);
        JS_MarkValue(rt, s->handler, mark_func);
    }
}

static JSValue JS_ThrowTypeErrorRevokedProxy(JSContext *ctx)
{
    return JS_ThrowTypeError(ctx, "revoked proxy");
}

static JSProxyData *get_proxy_method(JSContext *ctx, JSValue *pmethod,
                                     JSValueConst obj, JSAtom name)
{
    JSProxyData *s = JS_GetOpaque(obj, JS_CLASS_PROXY);
    JSValue method;

    /* safer to test recursion in all proxy methods */
    if (js_check_stack_overflow(ctx->rt, 0)) {
        JS_ThrowStackOverflow(ctx);
        return NULL;
    }
    
    /* 's' should never be NULL */
    if (s->is_revoked) {
        JS_ThrowTypeErrorRevokedProxy(ctx);
        return NULL;
    }
    method = JS_GetProperty(ctx, s->handler, name);
    if (JS_IsException(method))
        return NULL;
    if (JS_IsNull(method))
        method = JS_UNDEFINED;
    *pmethod = method;
    return s;
}

static JSValue js_proxy_getPrototypeOf(JSContext *ctx, JSValueConst obj)
{
    JSProxyData *s;
    JSValue method, ret, proto1;
    int res;

    s = get_proxy_method(ctx, &method, obj, JS_ATOM_getPrototypeOf);
    if (!s)
        return JS_EXCEPTION;
    if (JS_IsUndefined(method))
        return JS_GetPrototype(ctx, s->target);
    ret = JS_CallFree(ctx, method, s->handler, 1, (JSValueConst *)&s->target);
    if (JS_IsException(ret))
        return ret;
    if (JS_VALUE_GET_TAG(ret) != JS_TAG_NULL &&
        JS_VALUE_GET_TAG(ret) != JS_TAG_OBJECT) {
        goto fail;
    }
    res = JS_IsExtensible(ctx, s->target);
    if (res < 0) {
        JS_FreeValue(ctx, ret);
        return JS_EXCEPTION;
    }
    if (!res) {
        /* check invariant */
        proto1 = JS_GetPrototype(ctx, s->target);
        if (JS_IsException(proto1)) {
            JS_FreeValue(ctx, ret);
            return JS_EXCEPTION;
        }
        if (JS_VALUE_GET_OBJ(proto1) != JS_VALUE_GET_OBJ(ret)) {
            JS_FreeValue(ctx, proto1);
        fail:
            JS_FreeValue(ctx, ret);
            return JS_ThrowTypeError(ctx, "proxy: inconsistent prototype");
        }
        JS_FreeValue(ctx, proto1);
    }
    return ret;
}

static int js_proxy_setPrototypeOf(JSContext *ctx, JSValueConst obj,
                                   JSValueConst proto_val, BOOL throw_flag)
{
    JSProxyData *s;
    JSValue method, ret, proto1;
    JSValueConst args[2];
    BOOL res;
    int res2;

    s = get_proxy_method(ctx, &method, obj, JS_ATOM_setPrototypeOf);
    if (!s)
        return -1;
    if (JS_IsUndefined(method))
        return JS_SetPrototypeInternal(ctx, s->target, proto_val, throw_flag);
    args[0] = s->target;
    args[1] = proto_val;
    ret = JS_CallFree(ctx, method, s->handler, 2, args);
    if (JS_IsException(ret))
        return -1;
    res = JS_ToBoolFree(ctx, ret);
    if (!res) {
        if (throw_flag) {
            JS_ThrowTypeError(ctx, "proxy: bad prototype");
            return -1;
        } else {
            return FALSE;
        }
    }
    res2 = JS_IsExtensible(ctx, s->target);
    if (res2 < 0)
        return -1;
    if (!res2) {
        proto1 = JS_GetPrototype(ctx, s->target);
        if (JS_IsException(proto1))
            return -1;
        if (JS_VALUE_GET_OBJ(proto_val) != JS_VALUE_GET_OBJ(proto1)) {
            JS_FreeValue(ctx, proto1);
            JS_ThrowTypeError(ctx, "proxy: inconsistent prototype");
            return -1;
        }
        JS_FreeValue(ctx, proto1);
    }
    return TRUE;
}

static int js_proxy_isExtensible(JSContext *ctx, JSValueConst obj)
{
    JSProxyData *s;
    JSValue method, ret;
    BOOL res;
    int res2;

    s = get_proxy_method(ctx, &method, obj, JS_ATOM_isExtensible);
    if (!s)
        return -1;
    if (JS_IsUndefined(method))
        return JS_IsExtensible(ctx, s->target);
    ret = JS_CallFree(ctx, method, s->handler, 1, (JSValueConst *)&s->target);
    if (JS_IsException(ret))
        return -1;
    res = JS_ToBoolFree(ctx, ret);
    res2 = JS_IsExtensible(ctx, s->target);
    if (res2 < 0)
        return res2;
    if (res != res2) {
        JS_ThrowTypeError(ctx, "proxy: inconsistent isExtensible");
        return -1;
    }
    return res;
}

static int js_proxy_preventExtensions(JSContext *ctx, JSValueConst obj)
{
    JSProxyData *s;
    JSValue method, ret;
    BOOL res;
    int res2;

    s = get_proxy_method(ctx, &method, obj, JS_ATOM_preventExtensions);
    if (!s)
        return -1;
    if (JS_IsUndefined(method))
        return JS_PreventExtensions(ctx, s->target);
    ret = JS_CallFree(ctx, method, s->handler, 1, (JSValueConst *)&s->target);
    if (JS_IsException(ret))
        return -1;
    res = JS_ToBoolFree(ctx, ret);
    if (res) {
        res2 = JS_IsExtensible(ctx, s->target);
        if (res2 < 0)
            return res2;
        if (res2) {
            JS_ThrowTypeError(ctx, "proxy: inconsistent preventExtensions");
            return -1;
        }
    }
    return res;
}

static int js_proxy_has(JSContext *ctx, JSValueConst obj, JSAtom atom)
{
    JSProxyData *s;
    JSValue method, ret1, atom_val;
    int ret, res;
    JSObject *p;
    JSValueConst args[2];
    BOOL res2;

    s = get_proxy_method(ctx, &method, obj, JS_ATOM_has);
    if (!s)
        return -1;
    if (JS_IsUndefined(method))
        return JS_HasProperty(ctx, s->target, atom);
    atom_val = JS_AtomToValue(ctx, atom);
    if (JS_IsException(atom_val)) {
        JS_FreeValue(ctx, method);
        return -1;
    }
    args[0] = s->target;
    args[1] = atom_val;
    ret1 = JS_CallFree(ctx, method, s->handler, 2, args);
    JS_FreeValue(ctx, atom_val);
    if (JS_IsException(ret1))
        return -1;
    ret = JS_ToBoolFree(ctx, ret1);
    if (!ret) {
        JSPropertyDescriptor desc;
        p = JS_VALUE_GET_OBJ(s->target);
        res = JS_GetOwnPropertyInternal(ctx, &desc, p, atom);
        if (res < 0)
            return -1;
        if (res) {
            res2 = !(desc.flags & JS_PROP_CONFIGURABLE);
            js_free_desc(ctx, &desc);
            if (res2 || !p->extensible) {
                JS_ThrowTypeError(ctx, "proxy: inconsistent has");
                return -1;
            }
        }
    }
    return ret;
}

static JSValue js_proxy_get(JSContext *ctx, JSValueConst obj, JSAtom atom,
                            JSValueConst receiver)
{
    JSProxyData *s;
    JSValue method, ret, atom_val;
    int res;
    JSValueConst args[3];
    JSPropertyDescriptor desc;

    s = get_proxy_method(ctx, &method, obj, JS_ATOM_get);
    if (!s)
        return JS_EXCEPTION;
    /* Note: recursion is possible thru the prototype of s->target */
    if (JS_IsUndefined(method))
        return JS_GetPropertyInternal(ctx, s->target, atom, receiver, FALSE);
    atom_val = JS_AtomToValue(ctx, atom);
    if (JS_IsException(atom_val)) {
        JS_FreeValue(ctx, method);
        return JS_EXCEPTION;
    }
    args[0] = s->target;
    args[1] = atom_val;
    args[2] = receiver;
    ret = JS_CallFree(ctx, method, s->handler, 3, args);
    JS_FreeValue(ctx, atom_val);
    if (JS_IsException(ret))
        return JS_EXCEPTION;
    res = JS_GetOwnPropertyInternal(ctx, &desc, JS_VALUE_GET_OBJ(s->target), atom);
    if (res < 0)
        return JS_EXCEPTION;
    if (res) {
        if ((desc.flags & (JS_PROP_GETSET | JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE)) == 0) {
            if (!js_same_value(ctx, desc.value, ret)) {
                goto fail;
            }
        } else if ((desc.flags & (JS_PROP_GETSET | JS_PROP_CONFIGURABLE)) == JS_PROP_GETSET) {
            if (JS_IsUndefined(desc.getter) && !JS_IsUndefined(ret)) {
            fail:
                js_free_desc(ctx, &desc);
                JS_FreeValue(ctx, ret);
                return JS_ThrowTypeError(ctx, "proxy: inconsistent get");
            }
        }
        js_free_desc(ctx, &desc);
    }
    return ret;
}

static int js_proxy_set(JSContext *ctx, JSValueConst obj, JSAtom atom,
                        JSValueConst value, JSValueConst receiver, int flags)
{
    JSProxyData *s;
    JSValue method, ret1, atom_val;
    int ret, res;
    JSValueConst args[4];

    s = get_proxy_method(ctx, &method, obj, JS_ATOM_set);
    if (!s)
        return -1;
    if (JS_IsUndefined(method)) {
        return JS_SetPropertyGeneric(ctx, s->target, atom,
                                     JS_DupValue(ctx, value), receiver,
                                     flags);
    }
    atom_val = JS_AtomToValue(ctx, atom);
    if (JS_IsException(atom_val)) {
        JS_FreeValue(ctx, method);
        return -1;
    }
    args[0] = s->target;
    args[1] = atom_val;
    args[2] = value;
    args[3] = receiver;
    ret1 = JS_CallFree(ctx, method, s->handler, 4, args);
    JS_FreeValue(ctx, atom_val);
    if (JS_IsException(ret1))
        return -1;
    ret = JS_ToBoolFree(ctx, ret1);
    if (ret) {
        JSPropertyDescriptor desc;
        res = JS_GetOwnPropertyInternal(ctx, &desc, JS_VALUE_GET_OBJ(s->target), atom);
        if (res < 0)
            return -1;
        if (res) {
            if ((desc.flags & (JS_PROP_GETSET | JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE)) == 0) {
                if (!js_same_value(ctx, desc.value, value)) {
                    goto fail;
                }
            } else if ((desc.flags & (JS_PROP_GETSET | JS_PROP_CONFIGURABLE)) == JS_PROP_GETSET && JS_IsUndefined(desc.setter)) {
                fail:
                    js_free_desc(ctx, &desc);
                    JS_ThrowTypeError(ctx, "proxy: inconsistent set");
                    return -1;
            }
            js_free_desc(ctx, &desc);
        }
    } else {
        if ((flags & JS_PROP_THROW) ||
            ((flags & JS_PROP_THROW_STRICT) && is_strict_mode(ctx))) {
            JS_ThrowTypeError(ctx, "proxy: cannot set property");
            return -1;
        }
    }
    return ret;
}

static JSValue js_create_desc(JSContext *ctx, JSValueConst val,
                              JSValueConst getter, JSValueConst setter,
                              int flags)
{
    JSValue ret;
    ret = JS_NewObject(ctx);
    if (JS_IsException(ret))
        return ret;
    if (flags & JS_PROP_HAS_GET) {
        JS_DefinePropertyValue(ctx, ret, JS_ATOM_get, JS_DupValue(ctx, getter),
                               JS_PROP_C_W_E);
    }
    if (flags & JS_PROP_HAS_SET) {
        JS_DefinePropertyValue(ctx, ret, JS_ATOM_set, JS_DupValue(ctx, setter),
                               JS_PROP_C_W_E);
    }
    if (flags & JS_PROP_HAS_VALUE) {
        JS_DefinePropertyValue(ctx, ret, JS_ATOM_value, JS_DupValue(ctx, val),
                               JS_PROP_C_W_E);
    }
    if (flags & JS_PROP_HAS_WRITABLE) {
        JS_DefinePropertyValue(ctx, ret, JS_ATOM_writable,
                               JS_NewBool(ctx, (flags & JS_PROP_WRITABLE) != 0),
                               JS_PROP_C_W_E);
    }
    if (flags & JS_PROP_HAS_ENUMERABLE) {
        JS_DefinePropertyValue(ctx, ret, JS_ATOM_enumerable,
                               JS_NewBool(ctx, (flags & JS_PROP_ENUMERABLE) != 0),
                               JS_PROP_C_W_E);
    }
    if (flags & JS_PROP_HAS_CONFIGURABLE) {
        JS_DefinePropertyValue(ctx, ret, JS_ATOM_configurable,
                               JS_NewBool(ctx, (flags & JS_PROP_CONFIGURABLE) != 0),
                               JS_PROP_C_W_E);
    }
    return ret;
}

static int js_proxy_get_own_property(JSContext *ctx, JSPropertyDescriptor *pdesc,
                                     JSValueConst obj, JSAtom prop)
{
    JSProxyData *s;
    JSValue method, trap_result_obj, prop_val;
    int res, target_desc_ret, ret;
    JSObject *p;
    JSValueConst args[2];
    JSPropertyDescriptor result_desc, target_desc;

    s = get_proxy_method(ctx, &method, obj, JS_ATOM_getOwnPropertyDescriptor);
    if (!s)
        return -1;
    p = JS_VALUE_GET_OBJ(s->target);
    if (JS_IsUndefined(method)) {
        return JS_GetOwnPropertyInternal(ctx, pdesc, p, prop);
    }
    prop_val = JS_AtomToValue(ctx, prop);
    if (JS_IsException(prop_val)) {
        JS_FreeValue(ctx, method);
        return -1;
    }
    args[0] = s->target;
    args[1] = prop_val;
    trap_result_obj = JS_CallFree(ctx, method, s->handler, 2, args);
    JS_FreeValue(ctx, prop_val);
    if (JS_IsException(trap_result_obj))
        return -1;
    if (!JS_IsObject(trap_result_obj) && !JS_IsUndefined(trap_result_obj)) {
        JS_FreeValue(ctx, trap_result_obj);
        goto fail;
    }
    target_desc_ret = JS_GetOwnPropertyInternal(ctx, &target_desc, p, prop);
    if (target_desc_ret < 0) {
        JS_FreeValue(ctx, trap_result_obj);
        return -1;
    }
    if (target_desc_ret)
        js_free_desc(ctx, &target_desc);
    if (JS_IsUndefined(trap_result_obj)) {
        if (target_desc_ret) {
            if (!(target_desc.flags & JS_PROP_CONFIGURABLE) || !p->extensible)
                goto fail;
        }
        ret = FALSE;
    } else {
        int flags1, extensible_target;
        extensible_target = JS_IsExtensible(ctx, s->target);
        if (extensible_target < 0) {
            JS_FreeValue(ctx, trap_result_obj);
            return -1;
        }
        res = js_obj_to_desc(ctx, &result_desc, trap_result_obj);
        JS_FreeValue(ctx, trap_result_obj);
        if (res < 0)
            return -1;
        
        if (target_desc_ret) {
            /* convert result_desc.flags to defineProperty flags */
            flags1 = result_desc.flags | JS_PROP_HAS_CONFIGURABLE | JS_PROP_HAS_ENUMERABLE;
            if (result_desc.flags & JS_PROP_GETSET)
                flags1 |= JS_PROP_HAS_GET | JS_PROP_HAS_SET;
            else
                flags1 |= JS_PROP_HAS_VALUE | JS_PROP_HAS_WRITABLE;
            /* XXX: not complete check: need to compare value &
               getter/setter as in defineproperty */
            if (!check_define_prop_flags(target_desc.flags, flags1))
                goto fail1;
        } else {
            if (!extensible_target)
                goto fail1;
        }
        if (!(result_desc.flags & JS_PROP_CONFIGURABLE)) {
            if (!target_desc_ret || (target_desc.flags & JS_PROP_CONFIGURABLE))
                goto fail1;
            if ((result_desc.flags &
                 (JS_PROP_GETSET | JS_PROP_WRITABLE)) == 0 &&
                target_desc_ret &&
                (target_desc.flags & JS_PROP_WRITABLE) != 0) {
                /* proxy-missing-checks */
            fail1:
                js_free_desc(ctx, &result_desc);
            fail:
                JS_ThrowTypeError(ctx, "proxy: inconsistent getOwnPropertyDescriptor");
                return -1;
            }
        }
        ret = TRUE;
        if (pdesc) {
            *pdesc = result_desc;
        } else {
            js_free_desc(ctx, &result_desc);
        }
    }
    return ret;
}

static int js_proxy_define_own_property(JSContext *ctx, JSValueConst obj,
                                        JSAtom prop, JSValueConst val,
                                        JSValueConst getter, JSValueConst setter,
                                        int flags)
{
    JSProxyData *s;
    JSValue method, ret1, prop_val, desc_val;
    int res, ret;
    JSObject *p;
    JSValueConst args[3];
    JSPropertyDescriptor desc;
    BOOL setting_not_configurable;

    s = get_proxy_method(ctx, &method, obj, JS_ATOM_defineProperty);
    if (!s)
        return -1;
    if (JS_IsUndefined(method)) {
        return JS_DefineProperty(ctx, s->target, prop, val, getter, setter, flags);
    }
    prop_val = JS_AtomToValue(ctx, prop);
    if (JS_IsException(prop_val)) {
        JS_FreeValue(ctx, method);
        return -1;
    }
    desc_val = js_create_desc(ctx, val, getter, setter, flags);
    if (JS_IsException(desc_val)) {
        JS_FreeValue(ctx, prop_val);
        JS_FreeValue(ctx, method);
        return -1;
    }
    args[0] = s->target;
    args[1] = prop_val;
    args[2] = desc_val;
    ret1 = JS_CallFree(ctx, method, s->handler, 3, args);
    JS_FreeValue(ctx, prop_val);
    JS_FreeValue(ctx, desc_val);
    if (JS_IsException(ret1))
        return -1;
    ret = JS_ToBoolFree(ctx, ret1);
    if (!ret) {
        if (flags & JS_PROP_THROW) {
            JS_ThrowTypeError(ctx, "proxy: defineProperty exception");
            return -1;
        } else {
            return 0;
        }
    }
    p = JS_VALUE_GET_OBJ(s->target);
    res = JS_GetOwnPropertyInternal(ctx, &desc, p, prop);
    if (res < 0)
        return -1;
    setting_not_configurable = ((flags & (JS_PROP_HAS_CONFIGURABLE |
                                          JS_PROP_CONFIGURABLE)) ==
                                JS_PROP_HAS_CONFIGURABLE);
    if (!res) {
        if (!p->extensible || setting_not_configurable)
            goto fail;
    } else {
        if (!check_define_prop_flags(desc.flags, flags) ||
            ((desc.flags & JS_PROP_CONFIGURABLE) && setting_not_configurable)) {
            goto fail1;
        }
        if (flags & (JS_PROP_HAS_GET | JS_PROP_HAS_SET)) {
            if ((desc.flags & (JS_PROP_GETSET | JS_PROP_CONFIGURABLE)) ==
                JS_PROP_GETSET) {
                if ((flags & JS_PROP_HAS_GET) &&
                    !js_same_value(ctx, getter, desc.getter)) {
                    goto fail1;
                }
                if ((flags & JS_PROP_HAS_SET) &&
                    !js_same_value(ctx, setter, desc.setter)) {
                    goto fail1;
                }
            }
        } else if (flags & JS_PROP_HAS_VALUE) {
            if ((desc.flags & (JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE)) ==
                JS_PROP_WRITABLE && !(flags & JS_PROP_WRITABLE)) {
                /* missing-proxy-check feature */
                goto fail1;
            } else if ((desc.flags & (JS_PROP_CONFIGURABLE | JS_PROP_WRITABLE)) == 0 &&
                !js_same_value(ctx, val, desc.value)) {
                goto fail1;
            }
        }
        if (flags & JS_PROP_HAS_WRITABLE) {
            if ((desc.flags & (JS_PROP_GETSET | JS_PROP_CONFIGURABLE |
                               JS_PROP_WRITABLE)) == JS_PROP_WRITABLE) {
                /* proxy-missing-checks */
            fail1:
                js_free_desc(ctx, &desc);
            fail:
                JS_ThrowTypeError(ctx, "proxy: inconsistent defineProperty");
                return -1;
            }
        }
        js_free_desc(ctx, &desc);
    }
    return 1;
}

static int js_proxy_delete_property(JSContext *ctx, JSValueConst obj,
                                    JSAtom atom)
{
    JSProxyData *s;
    JSValue method, ret, atom_val;
    int res, res2, is_extensible;
    JSValueConst args[2];

    s = get_proxy_method(ctx, &method, obj, JS_ATOM_deleteProperty);
    if (!s)
        return -1;
    if (JS_IsUndefined(method)) {
        return JS_DeleteProperty(ctx, s->target, atom, 0);
    }
    atom_val = JS_AtomToValue(ctx, atom);;
    if (JS_IsException(atom_val)) {
        JS_FreeValue(ctx, method);
        return -1;
    }
    args[0] = s->target;
    args[1] = atom_val;
    ret = JS_CallFree(ctx, method, s->handler, 2, args);
    JS_FreeValue(ctx, atom_val);
    if (JS_IsException(ret))
        return -1;
    res = JS_ToBoolFree(ctx, ret);
    if (res) {
        JSPropertyDescriptor desc;
        res2 = JS_GetOwnPropertyInternal(ctx, &desc, JS_VALUE_GET_OBJ(s->target), atom);
        if (res2 < 0)
            return -1;
        if (res2) {
            if (!(desc.flags & JS_PROP_CONFIGURABLE))
                goto fail;
            is_extensible = JS_IsExtensible(ctx, s->target);
            if (is_extensible < 0)
                goto fail1;
            if (!is_extensible) {
                /* proxy-missing-checks */
            fail:
                JS_ThrowTypeError(ctx, "proxy: inconsistent deleteProperty");
            fail1:
                js_free_desc(ctx, &desc);
                return -1;
            }
            js_free_desc(ctx, &desc);
        }
    }
    return res;
}

/* return the index of the property or -1 if not found */
static int find_prop_key(const JSPropertyEnum *tab, int n, JSAtom atom)
{
    int i;
    for(i = 0; i < n; i++) {
        if (tab[i].atom == atom)
            return i;
    }
    return -1;
}

static int js_proxy_get_own_property_names(JSContext *ctx,
                                           JSPropertyEnum **ptab,
                                           uint32_t *plen,
                                           JSValueConst obj)
{
    JSProxyData *s;
    JSValue method, prop_array, val;
    uint32_t len, i, len2;
    JSPropertyEnum *tab, *tab2;
    JSAtom atom;
    JSPropertyDescriptor desc;
    int res, is_extensible, idx;

    s = get_proxy_method(ctx, &method, obj, JS_ATOM_ownKeys);
    if (!s)
        return -1;
    if (JS_IsUndefined(method)) {
        return JS_GetOwnPropertyNamesInternal(ctx, ptab, plen,
                                      JS_VALUE_GET_OBJ(s->target),
                                      JS_GPN_STRING_MASK | JS_GPN_SYMBOL_MASK);
    }
    prop_array = JS_CallFree(ctx, method, s->handler, 1, (JSValueConst *)&s->target);
    if (JS_IsException(prop_array))
        return -1;
    tab = NULL;
    len = 0;
    tab2 = NULL;
    len2 = 0;
    if (js_get_length32(ctx, &len, prop_array))
        goto fail;
    if (len > 0) {
        tab = js_mallocz(ctx, sizeof(tab[0]) * len);
        if (!tab)
            goto fail;
    }
    for(i = 0; i < len; i++) {
        val = JS_GetPropertyUint32(ctx, prop_array, i);
        if (JS_IsException(val))
            goto fail;
        if (!JS_IsString(val) && !JS_IsSymbol(val)) {
            JS_FreeValue(ctx, val);
            JS_ThrowTypeError(ctx, "proxy: properties must be strings or symbols");
            goto fail;
        }
        atom = JS_ValueToAtom(ctx, val);
        JS_FreeValue(ctx, val);
        if (atom == JS_ATOM_NULL)
            goto fail;
        tab[i].atom = atom;
        tab[i].is_enumerable = FALSE; /* XXX: redundant? */
    }

    /* check duplicate properties (XXX: inefficient, could store the
     * properties an a temporary object to use the hash) */
    for(i = 1; i < len; i++) {
        if (find_prop_key(tab, i, tab[i].atom) >= 0) {
            JS_ThrowTypeError(ctx, "proxy: duplicate property");
            goto fail;
        }
    }

    is_extensible = JS_IsExtensible(ctx, s->target);
    if (is_extensible < 0)
        goto fail;

    /* check if there are non configurable properties */
    if (s->is_revoked) {
        JS_ThrowTypeErrorRevokedProxy(ctx);
        goto fail;
    }
    if (JS_GetOwnPropertyNamesInternal(ctx, &tab2, &len2, JS_VALUE_GET_OBJ(s->target),
                               JS_GPN_STRING_MASK | JS_GPN_SYMBOL_MASK))
        goto fail;
    for(i = 0; i < len2; i++) {
        if (s->is_revoked) {
            JS_ThrowTypeErrorRevokedProxy(ctx);
            goto fail;
        }
        res = JS_GetOwnPropertyInternal(ctx, &desc, JS_VALUE_GET_OBJ(s->target),
                                tab2[i].atom);
        if (res < 0)
            goto fail;
        if (res) {  /* safety, property should be found */
            js_free_desc(ctx, &desc);
            if (!(desc.flags & JS_PROP_CONFIGURABLE) || !is_extensible) {
                idx = find_prop_key(tab, len, tab2[i].atom);
                if (idx < 0) {
                    JS_ThrowTypeError(ctx, "proxy: target property must be present in proxy ownKeys");
                    goto fail;
                }
                /* mark the property as found */
                if (!is_extensible)
                    tab[idx].is_enumerable = TRUE;
            }
        }
    }
    if (!is_extensible) {
        /* check that all property in 'tab' were checked */
        for(i = 0; i < len; i++) {
            if (!tab[i].is_enumerable) {
                JS_ThrowTypeError(ctx, "proxy: property not present in target were returned by non extensible proxy");
                goto fail;
            }
        }
    }

    js_free_prop_enum(ctx, tab2, len2);
    JS_FreeValue(ctx, prop_array);
    *ptab = tab;
    *plen = len;
    return 0;
 fail:
    js_free_prop_enum(ctx, tab2, len2);
    js_free_prop_enum(ctx, tab, len);
    JS_FreeValue(ctx, prop_array);
    return -1;
}

static JSValue js_proxy_call_constructor(JSContext *ctx, JSValueConst func_obj,
                                         JSValueConst new_target,
                                         int argc, JSValueConst *argv)
{
    JSProxyData *s;
    JSValue method, arg_array, ret;
    JSValueConst args[3];

    s = get_proxy_method(ctx, &method, func_obj, JS_ATOM_construct);
    if (!s)
        return JS_EXCEPTION;
    if (!JS_IsConstructor(ctx, s->target))
        return JS_ThrowTypeError(ctx, "not a constructor");
    if (JS_IsUndefined(method))
        return JS_CallConstructor2(ctx, s->target, new_target, argc, argv);
    arg_array = js_create_array(ctx, argc, argv);
    if (JS_IsException(arg_array)) {
        ret = JS_EXCEPTION;
        goto fail;
    }
    args[0] = s->target;
    args[1] = arg_array;
    args[2] = new_target;
    ret = JS_Call(ctx, method, s->handler, 3, args);
    if (!JS_IsException(ret) && JS_VALUE_GET_TAG(ret) != JS_TAG_OBJECT) {
        JS_FreeValue(ctx, ret);
        ret = JS_ThrowTypeErrorNotAnObject(ctx);
    }
 fail:
    JS_FreeValue(ctx, method);
    JS_FreeValue(ctx, arg_array);
    return ret;
}

static JSValue js_proxy_call(JSContext *ctx, JSValueConst func_obj,
                             JSValueConst this_obj,
                             int argc, JSValueConst *argv, int flags)
{
    JSProxyData *s;
    JSValue method, arg_array, ret;
    JSValueConst args[3];

    if (flags & JS_CALL_FLAG_CONSTRUCTOR)
        return js_proxy_call_constructor(ctx, func_obj, this_obj, argc, argv);
    
    s = get_proxy_method(ctx, &method, func_obj, JS_ATOM_apply);
    if (!s)
        return JS_EXCEPTION;
    if (!s->is_func) {
        JS_FreeValue(ctx, method);
        return JS_ThrowTypeError(ctx, "not a function");
    }
    if (JS_IsUndefined(method))
        return JS_Call(ctx, s->target, this_obj, argc, argv);
    arg_array = js_create_array(ctx, argc, argv);
    if (JS_IsException(arg_array)) {
        ret = JS_EXCEPTION;
        goto fail;
    }
    args[0] = s->target;
    args[1] = this_obj;
    args[2] = arg_array;
    ret = JS_Call(ctx, method, s->handler, 3, args);
 fail:
    JS_FreeValue(ctx, method);
    JS_FreeValue(ctx, arg_array);
    return ret;
}

static int js_proxy_isArray(JSContext *ctx, JSValueConst obj)
{
    JSProxyData *s = JS_GetOpaque(obj, JS_CLASS_PROXY);
    if (!s)
        return FALSE;
    if (s->is_revoked) {
        JS_ThrowTypeErrorRevokedProxy(ctx);
        return -1;
    }
    return JS_IsArray(ctx, s->target);
}

static const JSClassExoticMethods js_proxy_exotic_methods = {
    .get_own_property = js_proxy_get_own_property,
    .define_own_property = js_proxy_define_own_property,
    .delete_property = js_proxy_delete_property,
    .get_own_property_names = js_proxy_get_own_property_names,
    .has_property = js_proxy_has,
    .get_property = js_proxy_get,
    .set_property = js_proxy_set,
};

static JSValue js_proxy_constructor(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv)
{
    JSValueConst target, handler;
    JSValue obj;
    JSProxyData *s;

    target = argv[0];
    handler = argv[1];
    if (JS_VALUE_GET_TAG(target) != JS_TAG_OBJECT ||
        JS_VALUE_GET_TAG(handler) != JS_TAG_OBJECT)
        return JS_ThrowTypeErrorNotAnObject(ctx);

    obj = JS_NewObjectProtoClass(ctx, JS_NULL, JS_CLASS_PROXY);
    if (JS_IsException(obj))
        return obj;
    s = js_malloc(ctx, sizeof(JSProxyData));
    if (!s) {
        JS_FreeValue(ctx, obj);
        return JS_EXCEPTION;
    }
    s->target = JS_DupValue(ctx, target);
    s->handler = JS_DupValue(ctx, handler);
    s->is_func = JS_IsFunction(ctx, target);
    s->is_revoked = FALSE;
    JS_SetOpaque(obj, s);
    JS_SetConstructorBit(ctx, obj, JS_IsConstructor(ctx, target));
    return obj;
}

static JSValue js_proxy_revoke(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv, int magic,
                               JSValue *func_data)
{
    JSProxyData *s = JS_GetOpaque(func_data[0], JS_CLASS_PROXY);
    if (s) {
        /* We do not free the handler and target in case they are
           referenced as constants in the C call stack */
        s->is_revoked = TRUE;
        JS_FreeValue(ctx, func_data[0]);
        func_data[0] = JS_NULL;
    }
    return JS_UNDEFINED;
}

static JSValue js_proxy_revoke_constructor(JSContext *ctx,
                                           JSValueConst proxy_obj)
{
    return JS_NewCFunctionData(ctx, js_proxy_revoke, 0, 0, 1, &proxy_obj);
}

static JSValue js_proxy_revocable(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    JSValue proxy_obj, revoke_obj = JS_UNDEFINED, obj;

    proxy_obj = js_proxy_constructor(ctx, JS_UNDEFINED, argc, argv);
    if (JS_IsException(proxy_obj))
        goto fail;
    revoke_obj = js_proxy_revoke_constructor(ctx, proxy_obj);
    if (JS_IsException(revoke_obj))
        goto fail;
    obj = JS_NewObject(ctx);
    if (JS_IsException(obj))
        goto fail;
    // XXX: exceptions?
    JS_DefinePropertyValue(ctx, obj, JS_ATOM_proxy, proxy_obj, JS_PROP_C_W_E);
    JS_DefinePropertyValue(ctx, obj, JS_ATOM_revoke, revoke_obj, JS_PROP_C_W_E);
    return obj;
 fail:
    JS_FreeValue(ctx, proxy_obj);
    JS_FreeValue(ctx, revoke_obj);
    return JS_EXCEPTION;
}

static const JSCFunctionListEntry js_proxy_funcs[] = {
    JS_CFUNC_DEF("revocable", 2, js_proxy_revocable ),
};

static const JSClassShortDef js_proxy_class_def[] = {
    { JS_ATOM_Object, js_proxy_finalizer, js_proxy_mark }, /* JS_CLASS_PROXY */
};

void JS_AddIntrinsicProxy(JSContext *ctx)
{
    JSRuntime *rt = ctx->rt;
    JSValue obj1;

    if (!JS_IsRegisteredClass(rt, JS_CLASS_PROXY)) {
        init_class_range(rt, js_proxy_class_def, JS_CLASS_PROXY,
                         countof(js_proxy_class_def));
        rt->class_array[JS_CLASS_PROXY].exotic = &js_proxy_exotic_methods;
        rt->class_array[JS_CLASS_PROXY].call = js_proxy_call;
    }

    obj1 = JS_NewCFunction2(ctx, js_proxy_constructor, "Proxy", 2,
                            JS_CFUNC_constructor, 0);
    JS_SetConstructorBit(ctx, obj1, TRUE);
    JS_SetPropertyFunctionList(ctx, obj1, js_proxy_funcs,
                               countof(js_proxy_funcs));
    JS_DefinePropertyValueStr(ctx, ctx->global_obj, "Proxy",
                              obj1, JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
}

/* Symbol */

static JSValue js_symbol_constructor(JSContext *ctx, JSValueConst new_target,
                                     int argc, JSValueConst *argv)
{
    JSValue str;
    JSString *p;

    if (!JS_IsUndefined(new_target))
        return JS_ThrowTypeError(ctx, "not a constructor");
    if (argc == 0 || JS_IsUndefined(argv[0])) {
        p = NULL;
    } else {
        str = JS_ToString(ctx, argv[0]);
        if (JS_IsException(str))
            return JS_EXCEPTION;
        p = JS_VALUE_GET_STRING(str);
    }
    return JS_NewSymbol(ctx, p, JS_ATOM_TYPE_SYMBOL);
}

static JSValue js_thisSymbolValue(JSContext *ctx, JSValueConst this_val)
{
    if (JS_VALUE_GET_TAG(this_val) == JS_TAG_SYMBOL)
        return JS_DupValue(ctx, this_val);

    if (JS_VALUE_GET_TAG(this_val) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(this_val);
        if (p->class_id == JS_CLASS_SYMBOL) {
            if (JS_VALUE_GET_TAG(p->u.object_data) == JS_TAG_SYMBOL)
                return JS_DupValue(ctx, p->u.object_data);
        }
    }
    return JS_ThrowTypeError(ctx, "not a symbol");
}

static JSValue js_symbol_toString(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv)
{
    JSValue val, ret;
    val = js_thisSymbolValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    /* XXX: use JS_ToStringInternal() with a flags */
    ret = js_string_constructor(ctx, JS_UNDEFINED, 1, (JSValueConst *)&val);
    JS_FreeValue(ctx, val);
    return ret;
}

static JSValue js_symbol_valueOf(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    return js_thisSymbolValue(ctx, this_val);
}

static JSValue js_symbol_get_description(JSContext *ctx, JSValueConst this_val)
{
    JSValue val, ret;
    JSAtomStruct *p;

    val = js_thisSymbolValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    p = JS_VALUE_GET_PTR(val);
    if (p->len == 0 && p->is_wide_char != 0) {
        ret = JS_UNDEFINED;
    } else {
        ret = JS_AtomToString(ctx, js_get_atom_index(ctx->rt, p));
    }
    JS_FreeValue(ctx, val);
    return ret;
}

static const JSCFunctionListEntry js_symbol_proto_funcs[] = {
    JS_CFUNC_DEF("toString", 0, js_symbol_toString ),
    JS_CFUNC_DEF("valueOf", 0, js_symbol_valueOf ),
    // XXX: should have writable: false
    JS_CFUNC_DEF("[Symbol.toPrimitive]", 1, js_symbol_valueOf ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "Symbol", JS_PROP_CONFIGURABLE ),
    JS_CGETSET_DEF("description", js_symbol_get_description, NULL ),
};

static JSValue js_symbol_for(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv)
{
    JSValue str;

    str = JS_ToString(ctx, argv[0]);
    if (JS_IsException(str))
        return JS_EXCEPTION;
    return JS_NewSymbol(ctx, JS_VALUE_GET_STRING(str), JS_ATOM_TYPE_GLOBAL_SYMBOL);
}

static JSValue js_symbol_keyFor(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    JSAtomStruct *p;

    if (!JS_IsSymbol(argv[0]))
        return JS_ThrowTypeError(ctx, "not a symbol");
    p = JS_VALUE_GET_PTR(argv[0]);
    if (p->atom_type != JS_ATOM_TYPE_GLOBAL_SYMBOL)
        return JS_UNDEFINED;
    return JS_DupValue(ctx, JS_MKPTR(JS_TAG_STRING, p));
}

static const JSCFunctionListEntry js_symbol_funcs[] = {
    JS_CFUNC_DEF("for", 1, js_symbol_for ),
    JS_CFUNC_DEF("keyFor", 1, js_symbol_keyFor ),
};

/* Set/Map/WeakSet/WeakMap */

typedef struct JSMapRecord {
    int ref_count; /* used during enumeration to avoid freeing the record */
    BOOL empty; /* TRUE if the record is deleted */
    struct JSMapState *map;
    struct JSMapRecord *next_weak_ref;
    struct list_head link;
    struct list_head hash_link;
    JSValue key;
    JSValue value;
} JSMapRecord;

typedef struct JSMapState {
    BOOL is_weak; /* TRUE if WeakSet/WeakMap */
    struct list_head records; /* list of JSMapRecord.link */
    uint32_t record_count;
    struct list_head *hash_table;
    uint32_t hash_size; /* must be a power of two */
    uint32_t record_count_threshold; /* count at which a hash table
                                        resize is needed */
} JSMapState;

#define MAGIC_SET (1 << 0)
#define MAGIC_WEAK (1 << 1)

static JSValue js_map_constructor(JSContext *ctx, JSValueConst new_target,
                                  int argc, JSValueConst *argv, int magic)
{
    JSMapState *s;
    JSValue obj, adder = JS_UNDEFINED, iter = JS_UNDEFINED, next_method = JS_UNDEFINED;
    JSValueConst arr;
    BOOL is_set, is_weak;

    is_set = magic & MAGIC_SET;
    is_weak = ((magic & MAGIC_WEAK) != 0);
    obj = js_create_from_ctor(ctx, new_target, JS_CLASS_MAP + magic);
    if (JS_IsException(obj))
        return JS_EXCEPTION;
    s = js_mallocz(ctx, sizeof(*s));
    if (!s)
        goto fail;
    init_list_head(&s->records);
    s->is_weak = is_weak;
    JS_SetOpaque(obj, s);
    s->hash_size = 1;
    s->hash_table = js_malloc(ctx, sizeof(s->hash_table[0]) * s->hash_size);
    if (!s->hash_table)
        goto fail;
    init_list_head(&s->hash_table[0]);
    s->record_count_threshold = 4;

    arr = JS_UNDEFINED;
    if (argc > 0)
        arr = argv[0];
    if (!JS_IsUndefined(arr) && !JS_IsNull(arr)) {
        JSValue item, ret;
        BOOL done;

        adder = JS_GetProperty(ctx, obj, is_set ? JS_ATOM_add : JS_ATOM_set);
        if (JS_IsException(adder))
            goto fail;
        if (!JS_IsFunction(ctx, adder)) {
            JS_ThrowTypeError(ctx, "set/add is not a function");
            goto fail;
        }

        iter = JS_GetIterator(ctx, arr, FALSE);
        if (JS_IsException(iter))
            goto fail;
        next_method = JS_GetProperty(ctx, iter, JS_ATOM_next);
        if (JS_IsException(next_method))
            goto fail;

        for(;;) {
            item = JS_IteratorNext(ctx, iter, next_method, 0, NULL, &done);
            if (JS_IsException(item))
                goto fail;
            if (done) {
                JS_FreeValue(ctx, item);
                break;
            }
            if (is_set) {
                ret = JS_Call(ctx, adder, obj, 1, (JSValueConst *)&item);
                if (JS_IsException(ret)) {
                    JS_FreeValue(ctx, item);
                    goto fail;
                }
            } else {
                JSValue key, value;
                JSValueConst args[2];
                key = JS_UNDEFINED;
                value = JS_UNDEFINED;
                if (!JS_IsObject(item)) {
                    JS_ThrowTypeErrorNotAnObject(ctx);
                    goto fail1;
                }
                key = JS_GetPropertyUint32(ctx, item, 0);
                if (JS_IsException(key))
                    goto fail1;
                value = JS_GetPropertyUint32(ctx, item, 1);
                if (JS_IsException(value))
                    goto fail1;
                args[0] = key;
                args[1] = value;
                ret = JS_Call(ctx, adder, obj, 2, args);
                if (JS_IsException(ret)) {
                fail1:
                    JS_FreeValue(ctx, item);
                    JS_FreeValue(ctx, key);
                    JS_FreeValue(ctx, value);
                    goto fail;
                }
                JS_FreeValue(ctx, key);
                JS_FreeValue(ctx, value);
            }
            JS_FreeValue(ctx, ret);
            JS_FreeValue(ctx, item);
        }
        JS_FreeValue(ctx, next_method);
        JS_FreeValue(ctx, iter);
        JS_FreeValue(ctx, adder);
    }
    return obj;
 fail:
    if (JS_IsObject(iter)) {
        /* close the iterator object, preserving pending exception */
        JS_IteratorClose(ctx, iter, TRUE);
    }
    JS_FreeValue(ctx, next_method);
    JS_FreeValue(ctx, iter);
    JS_FreeValue(ctx, adder);
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

/* XXX: could normalize strings to speed up comparison */
static JSValueConst map_normalize_key(JSContext *ctx, JSValueConst key)
{
    uint32_t tag = JS_VALUE_GET_TAG(key);
    /* convert -0.0 to +0.0 */
    if (JS_TAG_IS_FLOAT64(tag) && JS_VALUE_GET_FLOAT64(key) == 0.0) {
        key = JS_NewInt32(ctx, 0);
    }
    return key;
}

/* XXX: better hash ? */
static uint32_t map_hash_key(JSContext *ctx, JSValueConst key)
{
    uint32_t tag = JS_VALUE_GET_NORM_TAG(key);
    uint32_t h;
    double d;
    JSFloat64Union u;

    switch(tag) {
    case JS_TAG_BOOL:
        h = JS_VALUE_GET_INT(key);
        break;
    case JS_TAG_STRING:
        h = hash_string(JS_VALUE_GET_STRING(key), 0);
        break;
    case JS_TAG_OBJECT:
    case JS_TAG_SYMBOL:
        h = (uintptr_t)JS_VALUE_GET_PTR(key) * 3163;
        break;
    case JS_TAG_INT:
        d = JS_VALUE_GET_INT(key) * 3163;
        goto hash_float64;
    case JS_TAG_FLOAT64:
        d = JS_VALUE_GET_FLOAT64(key);
        /* normalize the NaN */
        if (isnan(d))
            d = JS_FLOAT64_NAN;
    hash_float64:
        u.d = d;
        h = (u.u32[0] ^ u.u32[1]) * 3163;
        break;
    default:
        h = 0; /* XXX: bignum support */
        break;
    }
    h ^= tag;
    return h;
}

static JSMapRecord *map_find_record(JSContext *ctx, JSMapState *s,
                                    JSValueConst key)
{
    struct list_head *el;
    JSMapRecord *mr;
    uint32_t h;
    h = map_hash_key(ctx, key) & (s->hash_size - 1);
    list_for_each(el, &s->hash_table[h]) {
        mr = list_entry(el, JSMapRecord, hash_link);
        if (js_same_value_zero(ctx, mr->key, key))
            return mr;
    }
    return NULL;
}

static void map_hash_resize(JSContext *ctx, JSMapState *s)
{
    uint32_t new_hash_size, i, h;
    size_t slack;
    struct list_head *new_hash_table, *el;
    JSMapRecord *mr;

    /* XXX: no reporting of memory allocation failure */
    if (s->hash_size == 1)
        new_hash_size = 4;
    else
        new_hash_size = s->hash_size * 2;
    new_hash_table = js_realloc2(ctx, s->hash_table,
                                 sizeof(new_hash_table[0]) * new_hash_size, &slack);
    if (!new_hash_table)
        return;
    new_hash_size += slack / sizeof(*new_hash_table);

    for(i = 0; i < new_hash_size; i++)
        init_list_head(&new_hash_table[i]);

    list_for_each(el, &s->records) {
        mr = list_entry(el, JSMapRecord, link);
        if (!mr->empty) {
            h = map_hash_key(ctx, mr->key) & (new_hash_size - 1);
            list_add_tail(&mr->hash_link, &new_hash_table[h]);
        }
    }
    s->hash_table = new_hash_table;
    s->hash_size = new_hash_size;
    s->record_count_threshold = new_hash_size * 2;
}

static JSMapRecord *map_add_record(JSContext *ctx, JSMapState *s,
                                   JSValueConst key)
{
    uint32_t h;
    JSMapRecord *mr;

    mr = js_malloc(ctx, sizeof(*mr));
    if (!mr)
        return NULL;
    mr->ref_count = 1;
    mr->map = s;
    mr->empty = FALSE;
    if (s->is_weak) {
        JSObject *p = JS_VALUE_GET_OBJ(key);
        /* Add the weak reference */
        mr->next_weak_ref = p->first_weak_ref;
        p->first_weak_ref = mr;
    } else {
        JS_DupValue(ctx, key);
    }
    mr->key = (JSValue)key;
    h = map_hash_key(ctx, key) & (s->hash_size - 1);
    list_add_tail(&mr->hash_link, &s->hash_table[h]);
    list_add_tail(&mr->link, &s->records);
    s->record_count++;
    if (s->record_count >= s->record_count_threshold) {
        map_hash_resize(ctx, s);
    }
    return mr;
}

/* Remove the weak reference from the object weak
   reference list. we don't use a doubly linked list to
   save space, assuming a given object has few weak
       references to it */
static void delete_weak_ref(JSRuntime *rt, JSMapRecord *mr)
{
    JSMapRecord **pmr, *mr1;
    JSObject *p;

    p = JS_VALUE_GET_OBJ(mr->key);
    pmr = &p->first_weak_ref;
    for(;;) {
        mr1 = *pmr;
        assert(mr1 != NULL);
        if (mr1 == mr)
            break;
        pmr = &mr1->next_weak_ref;
    }
    *pmr = mr1->next_weak_ref;
}

static void map_delete_record(JSRuntime *rt, JSMapState *s, JSMapRecord *mr)
{
    if (mr->empty)
        return;
    list_del(&mr->hash_link);
    if (s->is_weak) {
        delete_weak_ref(rt, mr);
    } else {
        JS_FreeValueRT(rt, mr->key);
    }
    JS_FreeValueRT(rt, mr->value);
    if (--mr->ref_count == 0) {
        list_del(&mr->link);
        js_free_rt(rt, mr);
    } else {
        /* keep a zombie record for iterators */
        mr->empty = TRUE;
        mr->key = JS_UNDEFINED;
        mr->value = JS_UNDEFINED;
    }
    s->record_count--;
}

static void map_decref_record(JSRuntime *rt, JSMapRecord *mr)
{
    if (--mr->ref_count == 0) {
        /* the record can be safely removed */
        assert(mr->empty);
        list_del(&mr->link);
        js_free_rt(rt, mr);
    }
}

static void reset_weak_ref(JSRuntime *rt, JSObject *p)
{
    JSMapRecord *mr, *mr_next;
    JSMapState *s;
    
    /* first pass to remove the records from the WeakMap/WeakSet
       lists */
    for(mr = p->first_weak_ref; mr != NULL; mr = mr->next_weak_ref) {
        s = mr->map;
        assert(s->is_weak);
        assert(!mr->empty); /* no iterator on WeakMap/WeakSet */
        list_del(&mr->hash_link);
        list_del(&mr->link);
    }
    
    /* second pass to free the values to avoid modifying the weak
       reference list while traversing it. */
    for(mr = p->first_weak_ref; mr != NULL; mr = mr_next) {
        mr_next = mr->next_weak_ref;
        JS_FreeValueRT(rt, mr->value);
        js_free_rt(rt, mr);
    }

    p->first_weak_ref = NULL; /* fail safe */
}

static JSValue js_map_set(JSContext *ctx, JSValueConst this_val,
                          int argc, JSValueConst *argv, int magic)
{
    JSMapState *s = JS_GetOpaque2(ctx, this_val, JS_CLASS_MAP + magic);
    JSMapRecord *mr;
    JSValueConst key, value;

    if (!s)
        return JS_EXCEPTION;
    key = map_normalize_key(ctx, argv[0]);
    if (s->is_weak && !JS_IsObject(key))
        return JS_ThrowTypeErrorNotAnObject(ctx);
    if (magic & MAGIC_SET)
        value = JS_UNDEFINED;
    else
        value = argv[1];
    mr = map_find_record(ctx, s, key);
    if (mr) {
        JS_FreeValue(ctx, mr->value);
    } else {
        mr = map_add_record(ctx, s, key);
        if (!mr)
            return JS_EXCEPTION;
    }
    mr->value = JS_DupValue(ctx, value);
    return JS_DupValue(ctx, this_val);
}

static JSValue js_map_get(JSContext *ctx, JSValueConst this_val,
                          int argc, JSValueConst *argv, int magic)
{
    JSMapState *s = JS_GetOpaque2(ctx, this_val, JS_CLASS_MAP + magic);
    JSMapRecord *mr;
    JSValueConst key;

    if (!s)
        return JS_EXCEPTION;
    key = map_normalize_key(ctx, argv[0]);
    mr = map_find_record(ctx, s, key);
    if (!mr)
        return JS_UNDEFINED;
    else
        return JS_DupValue(ctx, mr->value);
}

static JSValue js_map_has(JSContext *ctx, JSValueConst this_val,
                          int argc, JSValueConst *argv, int magic)
{
    JSMapState *s = JS_GetOpaque2(ctx, this_val, JS_CLASS_MAP + magic);
    JSMapRecord *mr;
    JSValueConst key;

    if (!s)
        return JS_EXCEPTION;
    key = map_normalize_key(ctx, argv[0]);
    mr = map_find_record(ctx, s, key);
    return JS_NewBool(ctx, (mr != NULL));
}

static JSValue js_map_delete(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv, int magic)
{
    JSMapState *s = JS_GetOpaque2(ctx, this_val, JS_CLASS_MAP + magic);
    JSMapRecord *mr;
    JSValueConst key;

    if (!s)
        return JS_EXCEPTION;
    key = map_normalize_key(ctx, argv[0]);
    mr = map_find_record(ctx, s, key);
    if (!mr)
        return JS_FALSE;
    map_delete_record(ctx->rt, s, mr);
    return JS_TRUE;
}

static JSValue js_map_clear(JSContext *ctx, JSValueConst this_val,
                            int argc, JSValueConst *argv, int magic)
{
    JSMapState *s = JS_GetOpaque2(ctx, this_val, JS_CLASS_MAP + magic);
    struct list_head *el, *el1;
    JSMapRecord *mr;

    if (!s)
        return JS_EXCEPTION;
    list_for_each_safe(el, el1, &s->records) {
        mr = list_entry(el, JSMapRecord, link);
        map_delete_record(ctx->rt, s, mr);
    }
    return JS_UNDEFINED;
}

static JSValue js_map_get_size(JSContext *ctx, JSValueConst this_val, int magic)
{
    JSMapState *s = JS_GetOpaque2(ctx, this_val, JS_CLASS_MAP + magic);
    if (!s)
        return JS_EXCEPTION;
    return JS_NewUint32(ctx, s->record_count);
}

static JSValue js_map_forEach(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv, int magic)
{
    JSMapState *s = JS_GetOpaque2(ctx, this_val, JS_CLASS_MAP + magic);
    JSValueConst func, this_arg;
    JSValue ret, args[3];
    struct list_head *el;
    JSMapRecord *mr;

    if (!s)
        return JS_EXCEPTION;
    func = argv[0];
    if (argc > 1)
        this_arg = argv[1];
    else
        this_arg = JS_UNDEFINED;
    if (check_function(ctx, func))
        return JS_EXCEPTION;
    /* Note: the list can be modified while traversing it, but the
       current element is locked */
    el = s->records.next;
    while (el != &s->records) {
        mr = list_entry(el, JSMapRecord, link);
        if (!mr->empty) {
            mr->ref_count++;
            /* must duplicate in case the record is deleted */
            args[1] = JS_DupValue(ctx, mr->key);
            if (magic)
                args[0] = args[1];
            else
                args[0] = JS_DupValue(ctx, mr->value);
            args[2] = (JSValue)this_val;
            ret = JS_Call(ctx, func, this_arg, 3, (JSValueConst *)args);
            JS_FreeValue(ctx, args[0]);
            if (!magic)
                JS_FreeValue(ctx, args[1]);
            el = el->next;
            map_decref_record(ctx->rt, mr);
            if (JS_IsException(ret))
                return ret;
            JS_FreeValue(ctx, ret);
        } else {
            el = el->next;
        }
    }
    return JS_UNDEFINED;
}

static void js_map_finalizer(JSRuntime *rt, JSValue val)
{
    JSObject *p;
    JSMapState *s;
    struct list_head *el, *el1;
    JSMapRecord *mr;

    p = JS_VALUE_GET_OBJ(val);
    s = p->u.map_state;
    if (s) {
        /* if the object is deleted we are sure that no iterator is
           using it */
        list_for_each_safe(el, el1, &s->records) {
            mr = list_entry(el, JSMapRecord, link);
            if (!mr->empty) {
                if (s->is_weak)
                    delete_weak_ref(rt, mr);
                else
                    JS_FreeValueRT(rt, mr->key);
                JS_FreeValueRT(rt, mr->value);
            }
            js_free_rt(rt, mr);
        }
        js_free_rt(rt, s->hash_table);
        js_free_rt(rt, s);
    }
}

static void js_map_mark(JSRuntime *rt, JSValueConst val, JS_MarkFunc *mark_func)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSMapState *s;
    struct list_head *el;
    JSMapRecord *mr;

    s = p->u.map_state;
    if (s) {
        list_for_each(el, &s->records) {
            mr = list_entry(el, JSMapRecord, link);
            if (!s->is_weak)
                JS_MarkValue(rt, mr->key, mark_func);
            JS_MarkValue(rt, mr->value, mark_func);
        }
    }
}

/* Map Iterator */

typedef struct JSMapIteratorData {
    JSValue obj;
    JSIteratorKindEnum kind;
    JSMapRecord *cur_record;
} JSMapIteratorData;

static void js_map_iterator_finalizer(JSRuntime *rt, JSValue val)
{
    JSObject *p;
    JSMapIteratorData *it;

    p = JS_VALUE_GET_OBJ(val);
    it = p->u.map_iterator_data;
    if (it) {
        /* During the GC sweep phase the Map finalizer may be
           called before the Map iterator finalizer */
        if (JS_IsLiveObject(rt, it->obj) && it->cur_record) {
            map_decref_record(rt, it->cur_record);
        }
        JS_FreeValueRT(rt, it->obj);
        js_free_rt(rt, it);
    }
}

static void js_map_iterator_mark(JSRuntime *rt, JSValueConst val,
                                 JS_MarkFunc *mark_func)
{
    JSObject *p = JS_VALUE_GET_OBJ(val);
    JSMapIteratorData *it;
    it = p->u.map_iterator_data;
    if (it) {
        /* the record is already marked by the object */
        JS_MarkValue(rt, it->obj, mark_func);
    }
}

static JSValue js_create_map_iterator(JSContext *ctx, JSValueConst this_val,
                                      int argc, JSValueConst *argv, int magic)
{
    JSIteratorKindEnum kind;
    JSMapState *s;
    JSMapIteratorData *it;
    JSValue enum_obj;

    kind = magic >> 2;
    magic &= 3;
    s = JS_GetOpaque2(ctx, this_val, JS_CLASS_MAP + magic);
    if (!s)
        return JS_EXCEPTION;
    enum_obj = JS_NewObjectClass(ctx, JS_CLASS_MAP_ITERATOR + magic);
    if (JS_IsException(enum_obj))
        goto fail;
    it = js_malloc(ctx, sizeof(*it));
    if (!it) {
        JS_FreeValue(ctx, enum_obj);
        goto fail;
    }
    it->obj = JS_DupValue(ctx, this_val);
    it->kind = kind;
    it->cur_record = NULL;
    JS_SetOpaque(enum_obj, it);
    return enum_obj;
 fail:
    return JS_EXCEPTION;
}

static JSValue js_map_iterator_next(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv,
                                    BOOL *pdone, int magic)
{
    JSMapIteratorData *it;
    JSMapState *s;
    JSMapRecord *mr;
    struct list_head *el;

    it = JS_GetOpaque2(ctx, this_val, JS_CLASS_MAP_ITERATOR + magic);
    if (!it) {
        *pdone = FALSE;
        return JS_EXCEPTION;
    }
    if (JS_IsUndefined(it->obj))
        goto done;
    s = JS_GetOpaque(it->obj, JS_CLASS_MAP + magic);
    assert(s != NULL);
    if (!it->cur_record) {
        el = s->records.next;
    } else {
        mr = it->cur_record;
        el = mr->link.next;
        map_decref_record(ctx->rt, mr); /* the record can be freed here */
    }
    for(;;) {
        if (el == &s->records) {
            /* no more record  */
            it->cur_record = NULL;
            JS_FreeValue(ctx, it->obj);
            it->obj = JS_UNDEFINED;
        done:
            /* end of enumeration */
            *pdone = TRUE;
            return JS_UNDEFINED;
        }
        mr = list_entry(el, JSMapRecord, link);
        if (!mr->empty)
            break;
        /* get the next record */
        el = mr->link.next;
    }

    /* lock the record so that it won't be freed */
    mr->ref_count++;
    it->cur_record = mr;
    *pdone = FALSE;

    if (it->kind == JS_ITERATOR_KIND_KEY) {
        return JS_DupValue(ctx, mr->key);
    } else {
        JSValueConst args[2];
        args[0] = mr->key;
        if (magic)
            args[1] = mr->key;
        else
            args[1] = mr->value;
        if (it->kind == JS_ITERATOR_KIND_VALUE) {
            return JS_DupValue(ctx, args[1]);
        } else {
            return js_create_array(ctx, 2, args);
        }
    }
}

static const JSCFunctionListEntry js_map_funcs[] = {
    JS_CGETSET_DEF("[Symbol.species]", js_get_this, NULL ),
};

static const JSCFunctionListEntry js_map_proto_funcs[] = {
    JS_CFUNC_MAGIC_DEF("set", 2, js_map_set, 0 ),
    JS_CFUNC_MAGIC_DEF("get", 1, js_map_get, 0 ),
    JS_CFUNC_MAGIC_DEF("has", 1, js_map_has, 0 ),
    JS_CFUNC_MAGIC_DEF("delete", 1, js_map_delete, 0 ),
    JS_CFUNC_MAGIC_DEF("clear", 0, js_map_clear, 0 ),
    JS_CGETSET_MAGIC_DEF("size", js_map_get_size, NULL, 0),
    JS_CFUNC_MAGIC_DEF("forEach", 1, js_map_forEach, 0 ),
    JS_CFUNC_MAGIC_DEF("values", 0, js_create_map_iterator, (JS_ITERATOR_KIND_VALUE << 2) | 0 ),
    JS_CFUNC_MAGIC_DEF("keys", 0, js_create_map_iterator, (JS_ITERATOR_KIND_KEY << 2) | 0 ),
    JS_CFUNC_MAGIC_DEF("entries", 0, js_create_map_iterator, (JS_ITERATOR_KIND_KEY_AND_VALUE << 2) | 0 ),
    JS_ALIAS_DEF("[Symbol.iterator]", "entries" ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "Map", JS_PROP_CONFIGURABLE ),
};

static const JSCFunctionListEntry js_map_iterator_proto_funcs[] = {
    JS_ITERATOR_NEXT_DEF("next", 0, js_map_iterator_next, 0 ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "Map Iterator", JS_PROP_CONFIGURABLE ),
};

static const JSCFunctionListEntry js_set_proto_funcs[] = {
    JS_CFUNC_MAGIC_DEF("add", 1, js_map_set, MAGIC_SET ),
    JS_CFUNC_MAGIC_DEF("has", 1, js_map_has, MAGIC_SET ),
    JS_CFUNC_MAGIC_DEF("delete", 1, js_map_delete, MAGIC_SET ),
    JS_CFUNC_MAGIC_DEF("clear", 0, js_map_clear, MAGIC_SET ),
    JS_CGETSET_MAGIC_DEF("size", js_map_get_size, NULL, MAGIC_SET ),
    JS_CFUNC_MAGIC_DEF("forEach", 1, js_map_forEach, MAGIC_SET ),
    JS_CFUNC_MAGIC_DEF("values", 0, js_create_map_iterator, (JS_ITERATOR_KIND_KEY << 2) | MAGIC_SET ),
    JS_ALIAS_DEF("keys", "values" ),
    JS_ALIAS_DEF("[Symbol.iterator]", "values" ),
    JS_CFUNC_MAGIC_DEF("entries", 0, js_create_map_iterator, (JS_ITERATOR_KIND_KEY_AND_VALUE << 2) | MAGIC_SET ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "Set", JS_PROP_CONFIGURABLE ),
};

static const JSCFunctionListEntry js_set_iterator_proto_funcs[] = {
    JS_ITERATOR_NEXT_DEF("next", 0, js_map_iterator_next, MAGIC_SET ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "Set Iterator", JS_PROP_CONFIGURABLE ),
};

static const JSCFunctionListEntry js_weak_map_proto_funcs[] = {
    JS_CFUNC_MAGIC_DEF("set", 2, js_map_set, MAGIC_WEAK ),
    JS_CFUNC_MAGIC_DEF("get", 1, js_map_get, MAGIC_WEAK ),
    JS_CFUNC_MAGIC_DEF("has", 1, js_map_has, MAGIC_WEAK ),
    JS_CFUNC_MAGIC_DEF("delete", 1, js_map_delete, MAGIC_WEAK ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "WeakMap", JS_PROP_CONFIGURABLE ),
};

static const JSCFunctionListEntry js_weak_set_proto_funcs[] = {
    JS_CFUNC_MAGIC_DEF("add", 1, js_map_set, MAGIC_SET | MAGIC_WEAK ),
    JS_CFUNC_MAGIC_DEF("has", 1, js_map_has, MAGIC_SET | MAGIC_WEAK ),
    JS_CFUNC_MAGIC_DEF("delete", 1, js_map_delete, MAGIC_SET | MAGIC_WEAK ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "WeakSet", JS_PROP_CONFIGURABLE ),
};

static const JSCFunctionListEntry * const js_map_proto_funcs_ptr[6] = {
    js_map_proto_funcs,
    js_set_proto_funcs,
    js_weak_map_proto_funcs,
    js_weak_set_proto_funcs,
    js_map_iterator_proto_funcs,
    js_set_iterator_proto_funcs,
};

static const uint8_t js_map_proto_funcs_count[6] = {
    countof(js_map_proto_funcs),
    countof(js_set_proto_funcs),
    countof(js_weak_map_proto_funcs),
    countof(js_weak_set_proto_funcs),
    countof(js_map_iterator_proto_funcs),
    countof(js_set_iterator_proto_funcs),
};

void JS_AddIntrinsicMapSet(JSContext *ctx)
{
    int i;
    JSValue obj1;
    char buf[ATOM_GET_STR_BUF_SIZE];

    for(i = 0; i < 4; i++) {
        const char *name = JS_AtomGetStr(ctx, buf, sizeof(buf),
                                         JS_ATOM_Map + i);
        ctx->class_proto[JS_CLASS_MAP + i] = JS_NewObject(ctx);
        JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_MAP + i],
                                   js_map_proto_funcs_ptr[i],
                                   js_map_proto_funcs_count[i]);
        obj1 = JS_NewCFunctionMagic(ctx, js_map_constructor, name, 0,
                                    JS_CFUNC_constructor_magic, i);
        if (i < 2) {
            JS_SetPropertyFunctionList(ctx, obj1, js_map_funcs,
                                       countof(js_map_funcs));
        }
        JS_NewGlobalCConstructor2(ctx, obj1, name, ctx->class_proto[JS_CLASS_MAP + i]);
    }

    for(i = 0; i < 2; i++) {
        ctx->class_proto[JS_CLASS_MAP_ITERATOR + i] =
            JS_NewObjectProto(ctx, ctx->iterator_proto);
        JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_MAP_ITERATOR + i],
                                   js_map_proto_funcs_ptr[i + 4],
                                   js_map_proto_funcs_count[i + 4]);
    }
}

/* Generator */
static const JSCFunctionListEntry js_generator_function_proto_funcs[] = {
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "GeneratorFunction", JS_PROP_CONFIGURABLE),
};

static const JSCFunctionListEntry js_generator_proto_funcs[] = {
    JS_ITERATOR_NEXT_DEF("next", 1, js_generator_next, GEN_MAGIC_NEXT ),
    JS_ITERATOR_NEXT_DEF("return", 1, js_generator_next, GEN_MAGIC_RETURN ),
    JS_ITERATOR_NEXT_DEF("throw", 1, js_generator_next, GEN_MAGIC_THROW ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "Generator", JS_PROP_CONFIGURABLE),
};

/* Promise */

typedef enum JSPromiseStateEnum {
    JS_PROMISE_PENDING,
    JS_PROMISE_FULFILLED,
    JS_PROMISE_REJECTED,
} JSPromiseStateEnum;

typedef struct JSPromiseData {
    JSPromiseStateEnum promise_state;
    /* 0=fulfill, 1=reject, list of JSPromiseReactionData.link */
    struct list_head promise_reactions[2];
    BOOL is_handled; /* Note: only useful to debug */
    JSValue promise_result;
} JSPromiseData;

typedef struct JSPromiseFunctionDataResolved {
    int ref_count;
    BOOL already_resolved;
} JSPromiseFunctionDataResolved;

typedef struct JSPromiseFunctionData {
    JSValue promise;
    JSPromiseFunctionDataResolved *presolved;
} JSPromiseFunctionData;

typedef struct JSPromiseReactionData {
    struct list_head link; /* not used in promise_reaction_job */
    JSValue resolving_funcs[2];
    JSValue handler;
} JSPromiseReactionData;

static int js_create_resolving_functions(JSContext *ctx, JSValue *args,
                                         JSValueConst promise);

static void promise_reaction_data_free(JSRuntime *rt,
                                       JSPromiseReactionData *rd)
{
    JS_FreeValueRT(rt, rd->resolving_funcs[0]);
    JS_FreeValueRT(rt, rd->resolving_funcs[1]);
    JS_FreeValueRT(rt, rd->handler);
    js_free_rt(rt, rd);
}

static JSValue promise_reaction_job(JSContext *ctx, int argc,
                                    JSValueConst *argv)
{
    JSValueConst handler, arg, func;
    JSValue res, res2;
    BOOL is_reject;

    assert(argc == 5);
    handler = argv[2];
    is_reject = JS_ToBool(ctx, argv[3]);
    arg = argv[4];
#ifdef DUMP_PROMISE
    printf("promise_reaction_job: is_reject=%d\n", is_reject);
#endif

    if (JS_IsUndefined(handler)) {
        if (is_reject) {
            res = JS_Throw(ctx, JS_DupValue(ctx, arg));
        } else {
            res = JS_DupValue(ctx, arg);
        }
    } else {
        res = JS_Call(ctx, handler, JS_UNDEFINED, 1, &arg);
    }
    is_reject = JS_IsException(res);
    if (is_reject)
        res = JS_GetException(ctx);
    func = argv[is_reject];
    /* as an extension, we support undefined as value to avoid
       creating a dummy promise in the 'await' implementation of async
       functions */
    if (!JS_IsUndefined(func)) {
        res2 = JS_Call(ctx, func, JS_UNDEFINED,
                       1, (JSValueConst *)&res);
    } else {
        res2 = JS_UNDEFINED;
    }
    JS_FreeValue(ctx, res);

    return res2;
}

void JS_SetHostPromiseRejectionTracker(JSRuntime *rt,
                                       JSHostPromiseRejectionTracker *cb,
                                       void *opaque)
{
    rt->host_promise_rejection_tracker = cb;
    rt->host_promise_rejection_tracker_opaque = opaque;
}

static void fulfill_or_reject_promise(JSContext *ctx, JSValueConst promise,
                                      JSValueConst value, BOOL is_reject)
{
    JSPromiseData *s = JS_GetOpaque(promise, JS_CLASS_PROMISE);
    struct list_head *el, *el1;
    JSPromiseReactionData *rd;
    JSValueConst args[5];

    if (!s || s->promise_state != JS_PROMISE_PENDING)
        return; /* should never happen */
    set_value(ctx, &s->promise_result, JS_DupValue(ctx, value));
    s->promise_state = JS_PROMISE_FULFILLED + is_reject;
#ifdef DUMP_PROMISE
    printf("fulfill_or_reject_promise: is_reject=%d\n", is_reject);
#endif
    if (s->promise_state == JS_PROMISE_REJECTED && !s->is_handled) {
        JSRuntime *rt = ctx->rt;
        if (rt->host_promise_rejection_tracker) {
            rt->host_promise_rejection_tracker(ctx, promise, value, FALSE,
                                               rt->host_promise_rejection_tracker_opaque);
        }
    }

    list_for_each_safe(el, el1, &s->promise_reactions[is_reject]) {
        rd = list_entry(el, JSPromiseReactionData, link);
        args[0] = rd->resolving_funcs[0];
        args[1] = rd->resolving_funcs[1];
        args[2] = rd->handler;
        args[3] = JS_NewBool(ctx, is_reject);
        args[4] = value;
        JS_EnqueueJob(ctx, promise_reaction_job, 5, args);
        list_del(&rd->link);
        promise_reaction_data_free(ctx->rt, rd);
    }

    list_for_each_safe(el, el1, &s->promise_reactions[1 - is_reject]) {
        rd = list_entry(el, JSPromiseReactionData, link);
        list_del(&rd->link);
        promise_reaction_data_free(ctx->rt, rd);
    }
}

static void reject_promise(JSContext *ctx, JSValueConst promise,
                           JSValueConst value)
{
    fulfill_or_reject_promise(ctx, promise, value, TRUE);
}

static JSValue js_promise_resolve_thenable_job(JSContext *ctx,
                                               int argc, JSValueConst *argv)
{
    JSValueConst promise, thenable, then;
    JSValue args[2], res;

#ifdef DUMP_PROMISE
    printf("js_promise_resolve_thenable_job\n");
#endif
    assert(argc == 3);
    promise = argv[0];
    thenable = argv[1];
    then = argv[2];
    if (js_create_resolving_functions(ctx, args, promise) < 0)
        return JS_EXCEPTION;
    res = JS_Call(ctx, then, thenable, 2, (JSValueConst *)args);
    if (JS_IsException(res)) {
        JSValue error = JS_GetException(ctx);
        res = JS_Call(ctx, args[1], JS_UNDEFINED, 1, (JSValueConst *)&error);
        JS_FreeValue(ctx, error);
    }
    JS_FreeValue(ctx, args[0]);
    JS_FreeValue(ctx, args[1]);
    return res;
}

static void js_promise_resolve_function_free_resolved(JSRuntime *rt,
                                                      JSPromiseFunctionDataResolved *sr)
{
    if (--sr->ref_count == 0) {
        js_free_rt(rt, sr);
    }
}

static int js_create_resolving_functions(JSContext *ctx,
                                         JSValue *resolving_funcs,
                                         JSValueConst promise)

{
    JSValue obj;
    JSPromiseFunctionData *s;
    JSPromiseFunctionDataResolved *sr;
    int i, ret;

    sr = js_malloc(ctx, sizeof(*sr));
    if (!sr)
        return -1;
    sr->ref_count = 1;
    sr->already_resolved = FALSE; /* must be shared between the two functions */
    ret = 0;
    for(i = 0; i < 2; i++) {
        obj = JS_NewObjectProtoClass(ctx, ctx->function_proto,
                                     JS_CLASS_PROMISE_RESOLVE_FUNCTION + i);
        if (JS_IsException(obj))
            goto fail;
        s = js_malloc(ctx, sizeof(*s));
        if (!s) {
            JS_FreeValue(ctx, obj);
        fail:

            if (i != 0)
                JS_FreeValue(ctx, resolving_funcs[0]);
            ret = -1;
            break;
        }
        sr->ref_count++;
        s->presolved = sr;
        s->promise = JS_DupValue(ctx, promise);
        JS_SetOpaque(obj, s);
        js_function_set_properties(ctx, obj, JS_ATOM_empty_string, 1);
        resolving_funcs[i] = obj;
    }
    js_promise_resolve_function_free_resolved(ctx->rt, sr);
    return ret;
}

static void js_promise_resolve_function_finalizer(JSRuntime *rt, JSValue val)
{
    JSPromiseFunctionData *s = JS_VALUE_GET_OBJ(val)->u.promise_function_data;
    if (s) {
        js_promise_resolve_function_free_resolved(rt, s->presolved);
        JS_FreeValueRT(rt, s->promise);
        js_free_rt(rt, s);
    }
}

static void js_promise_resolve_function_mark(JSRuntime *rt, JSValueConst val,
                                             JS_MarkFunc *mark_func)
{
    JSPromiseFunctionData *s = JS_VALUE_GET_OBJ(val)->u.promise_function_data;
    if (s) {
        JS_MarkValue(rt, s->promise, mark_func);
    }
}

static JSValue js_promise_resolve_function_call(JSContext *ctx,
                                                JSValueConst func_obj,
                                                JSValueConst this_val,
                                                int argc, JSValueConst *argv,
                                                int flags)
{
    JSObject *p = JS_VALUE_GET_OBJ(func_obj);
    JSPromiseFunctionData *s;
    JSValueConst resolution, args[3];
    JSValue then;
    BOOL is_reject;

    s = p->u.promise_function_data;
    if (!s || s->presolved->already_resolved)
        return JS_UNDEFINED;
    s->presolved->already_resolved = TRUE;
    is_reject = p->class_id - JS_CLASS_PROMISE_RESOLVE_FUNCTION;
    if (argc > 0)
        resolution = argv[0];
    else
        resolution = JS_UNDEFINED;
#ifdef DUMP_PROMISE
    printf("js_promise_resolving_function_call: is_reject=%d resolution=", is_reject);
    JS_DumpValue(ctx, resolution);
    printf("\n");
#endif
    if (is_reject || !JS_IsObject(resolution)) {
        goto done;
    } else if (js_same_value(ctx, resolution, s->promise)) {
        JS_ThrowTypeError(ctx, "promise self resolution");
        goto fail_reject;
    }
    then = JS_GetProperty(ctx, resolution, JS_ATOM_then);
    if (JS_IsException(then)) {
        JSValue error;
    fail_reject:
        error = JS_GetException(ctx);
        reject_promise(ctx, s->promise, error);
        JS_FreeValue(ctx, error);
    } else if (!JS_IsFunction(ctx, then)) {
        JS_FreeValue(ctx, then);
    done:
        fulfill_or_reject_promise(ctx, s->promise, resolution, is_reject);
    } else {
        args[0] = s->promise;
        args[1] = resolution;
        args[2] = then;
        JS_EnqueueJob(ctx, js_promise_resolve_thenable_job, 3, args);
        JS_FreeValue(ctx, then);
    }
    return JS_UNDEFINED;
}

static void js_promise_finalizer(JSRuntime *rt, JSValue val)
{
    JSPromiseData *s = JS_GetOpaque(val, JS_CLASS_PROMISE);
    struct list_head *el, *el1;
    int i;

    if (!s)
        return;
    for(i = 0; i < 2; i++) {
        list_for_each_safe(el, el1, &s->promise_reactions[i]) {
            JSPromiseReactionData *rd =
                list_entry(el, JSPromiseReactionData, link);
            promise_reaction_data_free(rt, rd);
        }
    }
    JS_FreeValueRT(rt, s->promise_result);
    js_free_rt(rt, s);
}

static void js_promise_mark(JSRuntime *rt, JSValueConst val,
                            JS_MarkFunc *mark_func)
{
    JSPromiseData *s = JS_GetOpaque(val, JS_CLASS_PROMISE);
    struct list_head *el;
    int i;

    if (!s)
        return;
    for(i = 0; i < 2; i++) {
        list_for_each(el, &s->promise_reactions[i]) {
            JSPromiseReactionData *rd =
                list_entry(el, JSPromiseReactionData, link);
            JS_MarkValue(rt, rd->resolving_funcs[0], mark_func);
            JS_MarkValue(rt, rd->resolving_funcs[1], mark_func);
            JS_MarkValue(rt, rd->handler, mark_func);
        }
    }
    JS_MarkValue(rt, s->promise_result, mark_func);
}

static JSValue js_promise_constructor(JSContext *ctx, JSValueConst new_target,
                                      int argc, JSValueConst *argv)
{
    JSValueConst executor;
    JSValue obj;
    JSPromiseData *s;
    JSValue args[2], ret;
    int i;

    executor = argv[0];
    if (check_function(ctx, executor))
        return JS_EXCEPTION;
    obj = js_create_from_ctor(ctx, new_target, JS_CLASS_PROMISE);
    if (JS_IsException(obj))
        return JS_EXCEPTION;
    s = js_mallocz(ctx, sizeof(*s));
    if (!s)
        goto fail;
    s->promise_state = JS_PROMISE_PENDING;
    s->is_handled = FALSE;
    for(i = 0; i < 2; i++)
        init_list_head(&s->promise_reactions[i]);
    s->promise_result = JS_UNDEFINED;
    JS_SetOpaque(obj, s);
    if (js_create_resolving_functions(ctx, args, obj))
        goto fail;
    ret = JS_Call(ctx, executor, JS_UNDEFINED, 2, (JSValueConst *)args);
    if (JS_IsException(ret)) {
        JSValue ret2, error;
        error = JS_GetException(ctx);
        ret2 = JS_Call(ctx, args[1], JS_UNDEFINED, 1, (JSValueConst *)&error);
        JS_FreeValue(ctx, error);
        if (JS_IsException(ret2))
            goto fail1;
        JS_FreeValue(ctx, ret2);
    }
    JS_FreeValue(ctx, ret);
    JS_FreeValue(ctx, args[0]);
    JS_FreeValue(ctx, args[1]);
    return obj;
 fail1:
    JS_FreeValue(ctx, args[0]);
    JS_FreeValue(ctx, args[1]);
 fail:
    JS_FreeValue(ctx, obj);
    return JS_EXCEPTION;
}

static JSValue js_promise_executor(JSContext *ctx,
                                   JSValueConst this_val,
                                   int argc, JSValueConst *argv,
                                   int magic, JSValue *func_data)
{
    int i;

    for(i = 0; i < 2; i++) {
        if (!JS_IsUndefined(func_data[i]))
            return JS_ThrowTypeError(ctx, "resolving function already set");
        func_data[i] = JS_DupValue(ctx, argv[i]);
    }
    return JS_UNDEFINED;
}

static JSValue js_promise_executor_new(JSContext *ctx)
{
    JSValueConst func_data[2];

    func_data[0] = JS_UNDEFINED;
    func_data[1] = JS_UNDEFINED;
    return JS_NewCFunctionData(ctx, js_promise_executor, 2,
                               0, 2, func_data);
}

static JSValue js_new_promise_capability(JSContext *ctx,
                                         JSValue *resolving_funcs,
                                         JSValueConst ctor)
{
    JSValue executor, result_promise;
    JSCFunctionDataRecord *s;
    int i;

    executor = js_promise_executor_new(ctx);
    if (JS_IsException(executor))
        return executor;

    if (JS_IsUndefined(ctor)) {
        result_promise = js_promise_constructor(ctx, ctor, 1,
                                                (JSValueConst *)&executor);
    } else {
        result_promise = JS_CallConstructor(ctx, ctor, 1,
                                            (JSValueConst *)&executor);
    }
    if (JS_IsException(result_promise))
        goto fail;
    s = JS_GetOpaque(executor, JS_CLASS_C_FUNCTION_DATA);
    for(i = 0; i < 2; i++) {
        if (check_function(ctx, s->data[i]))
            goto fail;
    }
    for(i = 0; i < 2; i++)
        resolving_funcs[i] = JS_DupValue(ctx, s->data[i]);
    JS_FreeValue(ctx, executor);
    return result_promise;
 fail:
    JS_FreeValue(ctx, executor);
    JS_FreeValue(ctx, result_promise);
    return JS_EXCEPTION;
}

JSValue JS_NewPromiseCapability(JSContext *ctx, JSValue *resolving_funcs)
{
    return js_new_promise_capability(ctx, resolving_funcs, JS_UNDEFINED);
}

static JSValue js_promise_resolve(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv, int magic)
{
    JSValue result_promise, resolving_funcs[2], ret;
    BOOL is_reject = magic;

    if (!JS_IsObject(this_val))
        return JS_ThrowTypeErrorNotAnObject(ctx);
    if (!is_reject && JS_GetOpaque(argv[0], JS_CLASS_PROMISE)) {
        JSValue ctor;
        BOOL is_same;
        ctor = JS_GetProperty(ctx, argv[0], JS_ATOM_constructor);
        if (JS_IsException(ctor))
            return ctor;
        is_same = js_same_value(ctx, ctor, this_val);
        JS_FreeValue(ctx, ctor);
        if (is_same)
            return JS_DupValue(ctx, argv[0]);
    }
    result_promise = js_new_promise_capability(ctx, resolving_funcs, this_val);
    if (JS_IsException(result_promise))
        return result_promise;
    ret = JS_Call(ctx, resolving_funcs[is_reject], JS_UNDEFINED, 1, argv);
    JS_FreeValue(ctx, resolving_funcs[0]);
    JS_FreeValue(ctx, resolving_funcs[1]);
    if (JS_IsException(ret)) {
        JS_FreeValue(ctx, result_promise);
        return ret;
    }
    JS_FreeValue(ctx, ret);
    return result_promise;
}

#if 0
static JSValue js_promise___newPromiseCapability(JSContext *ctx,
                                                 JSValueConst this_val,
                                                 int argc, JSValueConst *argv)
{
    JSValue result_promise, resolving_funcs[2], obj;
    JSValueConst ctor;
    ctor = argv[0];
    if (!JS_IsObject(ctor))
        return JS_ThrowTypeErrorNotAnObject(ctx);
    result_promise = js_new_promise_capability(ctx, resolving_funcs, ctor);
    if (JS_IsException(result_promise))
        return result_promise;
    obj = JS_NewObject(ctx);
    if (JS_IsException(obj)) {
        JS_FreeValue(ctx, resolving_funcs[0]);
        JS_FreeValue(ctx, resolving_funcs[1]);
        JS_FreeValue(ctx, result_promise);
        return JS_EXCEPTION;
    }
    JS_DefinePropertyValue(ctx, obj, JS_ATOM_promise, result_promise, JS_PROP_C_W_E);
    JS_DefinePropertyValue(ctx, obj, JS_ATOM_resolve, resolving_funcs[0], JS_PROP_C_W_E);
    JS_DefinePropertyValue(ctx, obj, JS_ATOM_reject, resolving_funcs[1], JS_PROP_C_W_E);
    return obj;
}
#endif

static __exception int remainingElementsCount_add(JSContext *ctx,
                                                  JSValueConst resolve_element_env,
                                                  int addend)
{
    JSValue val;
    int remainingElementsCount;

    val = JS_GetPropertyUint32(ctx, resolve_element_env, 0);
    if (JS_IsException(val))
        return -1;
    if (JS_ToInt32Free(ctx, &remainingElementsCount, val))
        return -1;
    remainingElementsCount += addend;
    if (JS_SetPropertyUint32(ctx, resolve_element_env, 0,
                             JS_NewInt32(ctx, remainingElementsCount)) < 0)
        return -1;
    return (remainingElementsCount == 0);
}

#define PROMISE_MAGIC_all        0
#define PROMISE_MAGIC_allSettled 1
#define PROMISE_MAGIC_any        2

static JSValue js_promise_all_resolve_element(JSContext *ctx,
                                              JSValueConst this_val,
                                              int argc, JSValueConst *argv,
                                              int magic,
                                              JSValue *func_data)
{
    int resolve_type = magic & 3;
    int is_reject = magic & 4;
    BOOL alreadyCalled = JS_ToBool(ctx, func_data[0]);
    JSValueConst values = func_data[2];
    JSValueConst resolve = func_data[3];
    JSValueConst resolve_element_env = func_data[4];
    JSValue ret, obj;
    int is_zero, index;
    
    if (JS_ToInt32(ctx, &index, func_data[1]))
        return JS_EXCEPTION;
    if (alreadyCalled)
        return JS_UNDEFINED;
    func_data[0] = JS_NewBool(ctx, TRUE);

    if (resolve_type == PROMISE_MAGIC_allSettled) {
        JSValue str;
        
        obj = JS_NewObject(ctx);
        if (JS_IsException(obj))
            return JS_EXCEPTION;
        str = JS_NewString(ctx, is_reject ? "rejected" : "fulfilled");
        if (JS_IsException(str))
            goto fail1;
        if (JS_DefinePropertyValue(ctx, obj, JS_ATOM_status,
                                   str,
                                   JS_PROP_C_W_E) < 0)
            goto fail1;
        if (JS_DefinePropertyValue(ctx, obj,
                                   is_reject ? JS_ATOM_reason : JS_ATOM_value,
                                   JS_DupValue(ctx, argv[0]),
                                   JS_PROP_C_W_E) < 0) {
        fail1:
            JS_FreeValue(ctx, obj);
            return JS_EXCEPTION;
        }
    } else {
        obj = JS_DupValue(ctx, argv[0]);
    }
    if (JS_DefinePropertyValueUint32(ctx, values, index,
                                     obj, JS_PROP_C_W_E) < 0)
        return JS_EXCEPTION;
    
    is_zero = remainingElementsCount_add(ctx, resolve_element_env, -1);
    if (is_zero < 0)
        return JS_EXCEPTION;
    if (is_zero) {
        if (resolve_type == PROMISE_MAGIC_any) {
            JSValue error;
            error = js_aggregate_error_constructor(ctx, values);
            if (JS_IsException(error))
                return JS_EXCEPTION;
            ret = JS_Call(ctx, resolve, JS_UNDEFINED, 1, (JSValueConst *)&error);
            JS_FreeValue(ctx, error);
        } else {
            ret = JS_Call(ctx, resolve, JS_UNDEFINED, 1, (JSValueConst *)&values);
        }
        if (JS_IsException(ret))
            return ret;
        JS_FreeValue(ctx, ret);
    }
    return JS_UNDEFINED;
}

/* magic = 0: Promise.all 1: Promise.allSettled */
static JSValue js_promise_all(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv, int magic)
{
    JSValue result_promise, resolving_funcs[2], item, next_promise, ret;
    JSValue next_method = JS_UNDEFINED, values = JS_UNDEFINED;
    JSValue resolve_element_env = JS_UNDEFINED, resolve_element, reject_element;
    JSValue promise_resolve = JS_UNDEFINED, iter = JS_UNDEFINED;
    JSValueConst then_args[2], resolve_element_data[5];
    BOOL done;
    int index, is_zero, is_promise_any = (magic == PROMISE_MAGIC_any);
    
    if (!JS_IsObject(this_val))
        return JS_ThrowTypeErrorNotAnObject(ctx);
    result_promise = js_new_promise_capability(ctx, resolving_funcs, this_val);
    if (JS_IsException(result_promise))
        return result_promise;
    promise_resolve = JS_GetProperty(ctx, this_val, JS_ATOM_resolve);
    if (JS_IsException(promise_resolve) ||
        check_function(ctx, promise_resolve))
        goto fail_reject;
    iter = JS_GetIterator(ctx, argv[0], FALSE);
    if (JS_IsException(iter)) {
        JSValue error;
    fail_reject:
        error = JS_GetException(ctx);
        ret = JS_Call(ctx, resolving_funcs[1], JS_UNDEFINED, 1,
                       (JSValueConst *)&error);
        JS_FreeValue(ctx, error);
        if (JS_IsException(ret))
            goto fail;
        JS_FreeValue(ctx, ret);
    } else {
        next_method = JS_GetProperty(ctx, iter, JS_ATOM_next);
        if (JS_IsException(next_method))
            goto fail_reject;
        values = JS_NewArray(ctx);
        if (JS_IsException(values))
            goto fail_reject;
        resolve_element_env = JS_NewArray(ctx);
        if (JS_IsException(resolve_element_env))
            goto fail_reject;
        /* remainingElementsCount field */
        if (JS_DefinePropertyValueUint32(ctx, resolve_element_env, 0,
                                         JS_NewInt32(ctx, 1),
                                         JS_PROP_CONFIGURABLE | JS_PROP_ENUMERABLE | JS_PROP_WRITABLE) < 0)
            goto fail_reject;
        
        index = 0;
        for(;;) {
            /* XXX: conformance: should close the iterator if error on 'done'
               access, but not on 'value' access */
            item = JS_IteratorNext(ctx, iter, next_method, 0, NULL, &done);
            if (JS_IsException(item))
                goto fail_reject;
            if (done)
                break;
            next_promise = JS_Call(ctx, promise_resolve, 
                                   this_val, 1, (JSValueConst *)&item);
            JS_FreeValue(ctx, item);
            if (JS_IsException(next_promise)) {
            fail_reject1:
                JS_IteratorClose(ctx, iter, TRUE);
                goto fail_reject;
            }
            resolve_element_data[0] = JS_NewBool(ctx, FALSE);
            resolve_element_data[1] = (JSValueConst)JS_NewInt32(ctx, index);
            resolve_element_data[2] = values;
            resolve_element_data[3] = resolving_funcs[is_promise_any];
            resolve_element_data[4] = resolve_element_env;
            resolve_element =
                JS_NewCFunctionData(ctx, js_promise_all_resolve_element, 1,
                                    magic, 5, resolve_element_data);
            if (JS_IsException(resolve_element)) {
                JS_FreeValue(ctx, next_promise);
                goto fail_reject1;
            }
            
            if (magic == PROMISE_MAGIC_allSettled) {
                reject_element =
                    JS_NewCFunctionData(ctx, js_promise_all_resolve_element, 1,
                                        magic | 4, 5, resolve_element_data);
                if (JS_IsException(reject_element)) {
                    JS_FreeValue(ctx, next_promise);
                    goto fail_reject1;
                }
            } else if (magic == PROMISE_MAGIC_any) {
                if (JS_DefinePropertyValueUint32(ctx, values, index,
                                                 JS_UNDEFINED, JS_PROP_C_W_E) < 0)
                    goto fail_reject1;
                reject_element = resolve_element;
                resolve_element = JS_DupValue(ctx, resolving_funcs[0]);
            } else {
                reject_element = JS_DupValue(ctx, resolving_funcs[1]);
            }

            if (remainingElementsCount_add(ctx, resolve_element_env, 1) < 0) {
                JS_FreeValue(ctx, next_promise);
                JS_FreeValue(ctx, resolve_element);
                JS_FreeValue(ctx, reject_element);
                goto fail_reject1;
            }

            then_args[0] = resolve_element;
            then_args[1] = reject_element;
            ret = JS_InvokeFree(ctx, next_promise, JS_ATOM_then, 2, then_args);
            JS_FreeValue(ctx, resolve_element);
            JS_FreeValue(ctx, reject_element);
            if (check_exception_free(ctx, ret))
                goto fail_reject1;
            index++;
        }

        is_zero = remainingElementsCount_add(ctx, resolve_element_env, -1);
        if (is_zero < 0)
            goto fail_reject;
        if (is_zero) {
            if (magic == PROMISE_MAGIC_any) {
                JSValue error;
                error = js_aggregate_error_constructor(ctx, values);
                if (JS_IsException(error))
                    goto fail_reject;
                JS_FreeValue(ctx, values);
                values = error;
            }
            ret = JS_Call(ctx, resolving_funcs[is_promise_any], JS_UNDEFINED,
                          1, (JSValueConst *)&values);
            if (check_exception_free(ctx, ret))
                goto fail_reject;
        }
    }
 done:
    JS_FreeValue(ctx, promise_resolve);
    JS_FreeValue(ctx, resolve_element_env);
    JS_FreeValue(ctx, values);
    JS_FreeValue(ctx, next_method);
    JS_FreeValue(ctx, iter);
    JS_FreeValue(ctx, resolving_funcs[0]);
    JS_FreeValue(ctx, resolving_funcs[1]);
    return result_promise;
 fail:
    JS_FreeValue(ctx, result_promise);
    result_promise = JS_EXCEPTION;
    goto done;
}

static JSValue js_promise_race(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv)
{
    JSValue result_promise, resolving_funcs[2], item, next_promise, ret;
    JSValue next_method = JS_UNDEFINED, iter = JS_UNDEFINED;
    JSValue promise_resolve = JS_UNDEFINED;
    BOOL done;

    if (!JS_IsObject(this_val))
        return JS_ThrowTypeErrorNotAnObject(ctx);
    result_promise = js_new_promise_capability(ctx, resolving_funcs, this_val);
    if (JS_IsException(result_promise))
        return result_promise;
    promise_resolve = JS_GetProperty(ctx, this_val, JS_ATOM_resolve);
    if (JS_IsException(promise_resolve) ||
        check_function(ctx, promise_resolve))
        goto fail_reject;
    iter = JS_GetIterator(ctx, argv[0], FALSE);
    if (JS_IsException(iter)) {
        JSValue error;
    fail_reject:
        error = JS_GetException(ctx);
        ret = JS_Call(ctx, resolving_funcs[1], JS_UNDEFINED, 1,
                       (JSValueConst *)&error);
        JS_FreeValue(ctx, error);
        if (JS_IsException(ret))
            goto fail;
        JS_FreeValue(ctx, ret);
    } else {
        next_method = JS_GetProperty(ctx, iter, JS_ATOM_next);
        if (JS_IsException(next_method))
            goto fail_reject;

        for(;;) {
            /* XXX: conformance: should close the iterator if error on 'done'
               access, but not on 'value' access */
            item = JS_IteratorNext(ctx, iter, next_method, 0, NULL, &done);
            if (JS_IsException(item))
                goto fail_reject;
            if (done)
                break;
            next_promise = JS_Call(ctx, promise_resolve,
                                   this_val, 1, (JSValueConst *)&item);
            JS_FreeValue(ctx, item);
            if (JS_IsException(next_promise)) {
            fail_reject1:
                JS_IteratorClose(ctx, iter, TRUE);
                goto fail_reject;
            }
            ret = JS_InvokeFree(ctx, next_promise, JS_ATOM_then, 2,
                                (JSValueConst *)resolving_funcs);
            if (check_exception_free(ctx, ret))
                goto fail_reject1;
        }
    }
 done:
    JS_FreeValue(ctx, promise_resolve);
    JS_FreeValue(ctx, next_method);
    JS_FreeValue(ctx, iter);
    JS_FreeValue(ctx, resolving_funcs[0]);
    JS_FreeValue(ctx, resolving_funcs[1]);
    return result_promise;
 fail:
    //JS_FreeValue(ctx, next_method); // why not???
    JS_FreeValue(ctx, result_promise);
    result_promise = JS_EXCEPTION;
    goto done;
}

static __exception int perform_promise_then(JSContext *ctx,
                                            JSValueConst promise,
                                            JSValueConst *resolve_reject,
                                            JSValueConst *cap_resolving_funcs)
{
    JSPromiseData *s = JS_GetOpaque(promise, JS_CLASS_PROMISE);
    JSPromiseReactionData *rd_array[2], *rd;
    int i, j;

    rd_array[0] = NULL;
    rd_array[1] = NULL;
    for(i = 0; i < 2; i++) {
        JSValueConst handler;
        rd = js_mallocz(ctx, sizeof(*rd));
        if (!rd) {
            if (i == 1)
                promise_reaction_data_free(ctx->rt, rd_array[0]);
            return -1;
        }
        for(j = 0; j < 2; j++)
            rd->resolving_funcs[j] = JS_DupValue(ctx, cap_resolving_funcs[j]);
        handler = resolve_reject[i];
        if (!JS_IsFunction(ctx, handler))
            handler = JS_UNDEFINED;
        rd->handler = JS_DupValue(ctx, handler);
        rd_array[i] = rd;
    }

    if (s->promise_state == JS_PROMISE_PENDING) {
        for(i = 0; i < 2; i++)
            list_add_tail(&rd_array[i]->link, &s->promise_reactions[i]);
    } else {
        JSValueConst args[5];
        if (s->promise_state == JS_PROMISE_REJECTED && !s->is_handled) {
            JSRuntime *rt = ctx->rt;
            if (rt->host_promise_rejection_tracker) {
                rt->host_promise_rejection_tracker(ctx, promise, s->promise_result,
                                                   TRUE, rt->host_promise_rejection_tracker_opaque);
            }
        }
        i = s->promise_state - JS_PROMISE_FULFILLED;
        rd = rd_array[i];
        args[0] = rd->resolving_funcs[0];
        args[1] = rd->resolving_funcs[1];
        args[2] = rd->handler;
        args[3] = JS_NewBool(ctx, i);
        args[4] = s->promise_result;
        JS_EnqueueJob(ctx, promise_reaction_job, 5, args);
        for(i = 0; i < 2; i++)
            promise_reaction_data_free(ctx->rt, rd_array[i]);
    }
    s->is_handled = TRUE;
    return 0;
}

static JSValue js_promise_then(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv)
{
    JSValue ctor, result_promise, resolving_funcs[2];
    JSPromiseData *s;
    int i, ret;

    s = JS_GetOpaque2(ctx, this_val, JS_CLASS_PROMISE);
    if (!s)
        return JS_EXCEPTION;

    ctor = JS_SpeciesConstructor(ctx, this_val, JS_UNDEFINED);
    if (JS_IsException(ctor))
        return ctor;
    result_promise = js_new_promise_capability(ctx, resolving_funcs, ctor);
    JS_FreeValue(ctx, ctor);
    if (JS_IsException(result_promise))
        return result_promise;
    ret = perform_promise_then(ctx, this_val, argv,
                               (JSValueConst *)resolving_funcs);
    for(i = 0; i < 2; i++)
        JS_FreeValue(ctx, resolving_funcs[i]);
    if (ret) {
        JS_FreeValue(ctx, result_promise);
        return JS_EXCEPTION;
    }
    return result_promise;
}

static JSValue js_promise_catch(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    JSValueConst args[2];
    args[0] = JS_UNDEFINED;
    args[1] = argv[0];
    return JS_Invoke(ctx, this_val, JS_ATOM_then, 2, args);
}

static JSValue js_promise_finally_value_thunk(JSContext *ctx, JSValueConst this_val,
                                              int argc, JSValueConst *argv,
                                              int magic, JSValue *func_data)
{
    return JS_DupValue(ctx, func_data[0]);
}

static JSValue js_promise_finally_thrower(JSContext *ctx, JSValueConst this_val,
                                          int argc, JSValueConst *argv,
                                          int magic, JSValue *func_data)
{
    return JS_Throw(ctx, JS_DupValue(ctx, func_data[0]));
}

static JSValue js_promise_then_finally_func(JSContext *ctx, JSValueConst this_val,
                                            int argc, JSValueConst *argv,
                                            int magic, JSValue *func_data)
{
    JSValueConst ctor = func_data[0];
    JSValueConst onFinally = func_data[1];
    JSValue res, promise, ret, then_func;

    res = JS_Call(ctx, onFinally, JS_UNDEFINED, 0, NULL);
    if (JS_IsException(res))
        return res;
    promise = js_promise_resolve(ctx, ctor, 1, (JSValueConst *)&res, 0);
    JS_FreeValue(ctx, res);
    if (JS_IsException(promise))
        return promise;
    if (magic == 0) {
        then_func = JS_NewCFunctionData(ctx, js_promise_finally_value_thunk, 0,
                                        0, 1, argv);
    } else {
        then_func = JS_NewCFunctionData(ctx, js_promise_finally_thrower, 0,
                                        0, 1, argv);
    }
    if (JS_IsException(then_func)) {
        JS_FreeValue(ctx, promise);
        return then_func;
    }
    ret = JS_InvokeFree(ctx, promise, JS_ATOM_then, 1, (JSValueConst *)&then_func);
    JS_FreeValue(ctx, then_func);
    return ret;
}

static JSValue js_promise_finally(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv)
{
    JSValueConst onFinally = argv[0];
    JSValue ctor, ret;
    JSValue then_funcs[2];
    JSValueConst func_data[2];
    int i;

    ctor = JS_SpeciesConstructor(ctx, this_val, JS_UNDEFINED);
    if (JS_IsException(ctor))
        return ctor;
    if (!JS_IsFunction(ctx, onFinally)) {
        then_funcs[0] = JS_DupValue(ctx, onFinally);
        then_funcs[1] = JS_DupValue(ctx, onFinally);
    } else {
        func_data[0] = ctor;
        func_data[1] = onFinally;
        for(i = 0; i < 2; i++) {
            then_funcs[i] = JS_NewCFunctionData(ctx, js_promise_then_finally_func, 1, i, 2, func_data);
            if (JS_IsException(then_funcs[i])) {
                if (i == 1)
                    JS_FreeValue(ctx, then_funcs[0]);
                JS_FreeValue(ctx, ctor);
                return JS_EXCEPTION;
            }
        }
    }
    JS_FreeValue(ctx, ctor);
    ret = JS_Invoke(ctx, this_val, JS_ATOM_then, 2, (JSValueConst *)then_funcs);
    JS_FreeValue(ctx, then_funcs[0]);
    JS_FreeValue(ctx, then_funcs[1]);
    return ret;
}

static const JSCFunctionListEntry js_promise_funcs[] = {
    JS_CFUNC_MAGIC_DEF("resolve", 1, js_promise_resolve, 0 ),
    JS_CFUNC_MAGIC_DEF("reject", 1, js_promise_resolve, 1 ),
    JS_CFUNC_MAGIC_DEF("all", 1, js_promise_all, PROMISE_MAGIC_all ),
    JS_CFUNC_MAGIC_DEF("allSettled", 1, js_promise_all, PROMISE_MAGIC_allSettled ),
    JS_CFUNC_MAGIC_DEF("any", 1, js_promise_all, PROMISE_MAGIC_any ),
    JS_CFUNC_DEF("race", 1, js_promise_race ),
    //JS_CFUNC_DEF("__newPromiseCapability", 1, js_promise___newPromiseCapability ),
    JS_CGETSET_DEF("[Symbol.species]", js_get_this, NULL),
};

static const JSCFunctionListEntry js_promise_proto_funcs[] = {
    JS_CFUNC_DEF("then", 2, js_promise_then ),
    JS_CFUNC_DEF("catch", 1, js_promise_catch ),
    JS_CFUNC_DEF("finally", 1, js_promise_finally ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "Promise", JS_PROP_CONFIGURABLE ),
};

/* AsyncFunction */
static const JSCFunctionListEntry js_async_function_proto_funcs[] = {
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "AsyncFunction", JS_PROP_CONFIGURABLE ),
};

static JSValue js_async_from_sync_iterator_unwrap(JSContext *ctx,
                                                  JSValueConst this_val,
                                                  int argc, JSValueConst *argv,
                                                  int magic, JSValue *func_data)
{
    return js_create_iterator_result(ctx, JS_DupValue(ctx, argv[0]),
                                     JS_ToBool(ctx, func_data[0]));
}

static JSValue js_async_from_sync_iterator_unwrap_func_create(JSContext *ctx,
                                                              BOOL done)
{
    JSValueConst func_data[1];

    func_data[0] = (JSValueConst)JS_NewBool(ctx, done);
    return JS_NewCFunctionData(ctx, js_async_from_sync_iterator_unwrap,
                               1, 0, 1, func_data);
}

/* AsyncIteratorPrototype */

static const JSCFunctionListEntry js_async_iterator_proto_funcs[] = {
    JS_CFUNC_DEF("[Symbol.asyncIterator]", 0, js_iterator_proto_iterator ),
};

/* AsyncFromSyncIteratorPrototype */

typedef struct JSAsyncFromSyncIteratorData {
    JSValue sync_iter;
    JSValue next_method;
} JSAsyncFromSyncIteratorData;

static void js_async_from_sync_iterator_finalizer(JSRuntime *rt, JSValue val)
{
    JSAsyncFromSyncIteratorData *s =
        JS_GetOpaque(val, JS_CLASS_ASYNC_FROM_SYNC_ITERATOR);
    if (s) {
        JS_FreeValueRT(rt, s->sync_iter);
        JS_FreeValueRT(rt, s->next_method);
        js_free_rt(rt, s);
    }
}

static void js_async_from_sync_iterator_mark(JSRuntime *rt, JSValueConst val,
                                             JS_MarkFunc *mark_func)
{
    JSAsyncFromSyncIteratorData *s =
        JS_GetOpaque(val, JS_CLASS_ASYNC_FROM_SYNC_ITERATOR);
    if (s) {
        JS_MarkValue(rt, s->sync_iter, mark_func);
        JS_MarkValue(rt, s->next_method, mark_func);
    }
}

static JSValue JS_CreateAsyncFromSyncIterator(JSContext *ctx,
                                              JSValueConst sync_iter)
{
    JSValue async_iter, next_method;
    JSAsyncFromSyncIteratorData *s;

    next_method = JS_GetProperty(ctx, sync_iter, JS_ATOM_next);
    if (JS_IsException(next_method))
        return JS_EXCEPTION;
    async_iter = JS_NewObjectClass(ctx, JS_CLASS_ASYNC_FROM_SYNC_ITERATOR);
    if (JS_IsException(async_iter)) {
        JS_FreeValue(ctx, next_method);
        return async_iter;
    }
    s = js_mallocz(ctx, sizeof(*s));
    if (!s) {
        JS_FreeValue(ctx, async_iter);
        JS_FreeValue(ctx, next_method);
        return JS_EXCEPTION;
    }
    s->sync_iter = JS_DupValue(ctx, sync_iter);
    s->next_method = next_method;
    JS_SetOpaque(async_iter, s);
    return async_iter;
}

static JSValue js_async_from_sync_iterator_next(JSContext *ctx, JSValueConst this_val,
                                                int argc, JSValueConst *argv,
                                                int magic)
{
    JSValue promise, resolving_funcs[2], value, err, method;
    JSAsyncFromSyncIteratorData *s;
    int done;
    int is_reject;

    promise = JS_NewPromiseCapability(ctx, resolving_funcs);
    if (JS_IsException(promise))
        return JS_EXCEPTION;
    s = JS_GetOpaque(this_val, JS_CLASS_ASYNC_FROM_SYNC_ITERATOR);
    if (!s) {
        JS_ThrowTypeError(ctx, "not an Async-from-Sync Iterator");
        goto reject;
    }

    if (magic == GEN_MAGIC_NEXT) {
        method = JS_DupValue(ctx, s->next_method);
    } else {
        method = JS_GetProperty(ctx, s->sync_iter,
                                magic == GEN_MAGIC_RETURN ? JS_ATOM_return :
                                JS_ATOM_throw);
        if (JS_IsException(method))
            goto reject;
        if (JS_IsUndefined(method) || JS_IsNull(method)) {
            if (magic == GEN_MAGIC_RETURN) {
                err = js_create_iterator_result(ctx, JS_DupValue(ctx, argv[0]), TRUE);
                is_reject = 0;
            } else {
                err = JS_DupValue(ctx, argv[0]);
                is_reject = 1;
            }
            goto done_resolve;
        }
    }
    value = JS_IteratorNext2(ctx, s->sync_iter, method,
                             argc >= 1 ? 1 : 0, argv, &done);
    JS_FreeValue(ctx, method);
    if (JS_IsException(value))
        goto reject;
    if (done == 2) {
        JSValue obj = value;
        value = JS_IteratorGetCompleteValue(ctx, obj, &done);
        JS_FreeValue(ctx, obj);
        if (JS_IsException(value))
            goto reject;
    }

    if (JS_IsException(value)) {
        JSValue res2;
    reject:
        err = JS_GetException(ctx);
        is_reject = 1;
    done_resolve:
        res2 = JS_Call(ctx, resolving_funcs[is_reject], JS_UNDEFINED,
                       1, (JSValueConst *)&err);
        JS_FreeValue(ctx, err);
        JS_FreeValue(ctx, res2);
        JS_FreeValue(ctx, resolving_funcs[0]);
        JS_FreeValue(ctx, resolving_funcs[1]);
        return promise;
    }
    {
        JSValue value_wrapper_promise, resolve_reject[2];
        int res;

        value_wrapper_promise = js_promise_resolve(ctx, ctx->promise_ctor,
                                                   1, (JSValueConst *)&value, 0);
        if (JS_IsException(value_wrapper_promise)) {
            JS_FreeValue(ctx, value);
            goto reject;
        }

        resolve_reject[0] =
            js_async_from_sync_iterator_unwrap_func_create(ctx, done);
        if (JS_IsException(resolve_reject[0])) {
            JS_FreeValue(ctx, value_wrapper_promise);
            goto fail;
        }
        JS_FreeValue(ctx, value);
        resolve_reject[1] = JS_UNDEFINED;

        res = perform_promise_then(ctx, value_wrapper_promise,
                                   (JSValueConst *)resolve_reject,
                                   (JSValueConst *)resolving_funcs);
        JS_FreeValue(ctx, resolve_reject[0]);
        JS_FreeValue(ctx, value_wrapper_promise);
        JS_FreeValue(ctx, resolving_funcs[0]);
        JS_FreeValue(ctx, resolving_funcs[1]);
        if (res) {
            JS_FreeValue(ctx, promise);
            return JS_EXCEPTION;
        }
    }
    return promise;
 fail:
    JS_FreeValue(ctx, value);
    JS_FreeValue(ctx, resolving_funcs[0]);
    JS_FreeValue(ctx, resolving_funcs[1]);
    JS_FreeValue(ctx, promise);
    return JS_EXCEPTION;
}

static const JSCFunctionListEntry js_async_from_sync_iterator_proto_funcs[] = {
    JS_CFUNC_MAGIC_DEF("next", 1, js_async_from_sync_iterator_next, GEN_MAGIC_NEXT ),
    JS_CFUNC_MAGIC_DEF("return", 1, js_async_from_sync_iterator_next, GEN_MAGIC_RETURN ),
    JS_CFUNC_MAGIC_DEF("throw", 1, js_async_from_sync_iterator_next, GEN_MAGIC_THROW ),
};

/* AsyncGeneratorFunction */

static const JSCFunctionListEntry js_async_generator_function_proto_funcs[] = {
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "AsyncGeneratorFunction", JS_PROP_CONFIGURABLE ),
};

/* AsyncGenerator prototype */

static const JSCFunctionListEntry js_async_generator_proto_funcs[] = {
    JS_CFUNC_MAGIC_DEF("next", 1, js_async_generator_next, GEN_MAGIC_NEXT ),
    JS_CFUNC_MAGIC_DEF("return", 1, js_async_generator_next, GEN_MAGIC_RETURN ),
    JS_CFUNC_MAGIC_DEF("throw", 1, js_async_generator_next, GEN_MAGIC_THROW ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "AsyncGenerator", JS_PROP_CONFIGURABLE ),
};

static JSClassShortDef const js_async_class_def[] = {
    { JS_ATOM_Promise, js_promise_finalizer, js_promise_mark },                      /* JS_CLASS_PROMISE */
    { JS_ATOM_PromiseResolveFunction, js_promise_resolve_function_finalizer, js_promise_resolve_function_mark }, /* JS_CLASS_PROMISE_RESOLVE_FUNCTION */
    { JS_ATOM_PromiseRejectFunction, js_promise_resolve_function_finalizer, js_promise_resolve_function_mark }, /* JS_CLASS_PROMISE_REJECT_FUNCTION */
    { JS_ATOM_AsyncFunction, js_bytecode_function_finalizer, js_bytecode_function_mark },  /* JS_CLASS_ASYNC_FUNCTION */
    { JS_ATOM_AsyncFunctionResolve, js_async_function_resolve_finalizer, js_async_function_resolve_mark }, /* JS_CLASS_ASYNC_FUNCTION_RESOLVE */
    { JS_ATOM_AsyncFunctionReject, js_async_function_resolve_finalizer, js_async_function_resolve_mark }, /* JS_CLASS_ASYNC_FUNCTION_REJECT */
    { JS_ATOM_empty_string, js_async_from_sync_iterator_finalizer, js_async_from_sync_iterator_mark }, /* JS_CLASS_ASYNC_FROM_SYNC_ITERATOR */
    { JS_ATOM_AsyncGeneratorFunction, js_bytecode_function_finalizer, js_bytecode_function_mark },  /* JS_CLASS_ASYNC_GENERATOR_FUNCTION */
    { JS_ATOM_AsyncGenerator, js_async_generator_finalizer, js_async_generator_mark },  /* JS_CLASS_ASYNC_GENERATOR */
};

void JS_AddIntrinsicPromise(JSContext *ctx)
{
    JSRuntime *rt = ctx->rt;
    JSValue obj1;

    if (!JS_IsRegisteredClass(rt, JS_CLASS_PROMISE)) {
        init_class_range(rt, js_async_class_def, JS_CLASS_PROMISE,
                         countof(js_async_class_def));
        rt->class_array[JS_CLASS_PROMISE_RESOLVE_FUNCTION].call = js_promise_resolve_function_call;
        rt->class_array[JS_CLASS_PROMISE_REJECT_FUNCTION].call = js_promise_resolve_function_call;
        rt->class_array[JS_CLASS_ASYNC_FUNCTION].call = js_async_function_call;
        rt->class_array[JS_CLASS_ASYNC_FUNCTION_RESOLVE].call = js_async_function_resolve_call;
        rt->class_array[JS_CLASS_ASYNC_FUNCTION_REJECT].call = js_async_function_resolve_call;
        rt->class_array[JS_CLASS_ASYNC_GENERATOR_FUNCTION].call = js_async_generator_function_call;
    }

    /* Promise */
    ctx->class_proto[JS_CLASS_PROMISE] = JS_NewObject(ctx);
    JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_PROMISE],
                               js_promise_proto_funcs,
                               countof(js_promise_proto_funcs));
    obj1 = JS_NewCFunction2(ctx, js_promise_constructor, "Promise", 1,
                            JS_CFUNC_constructor, 0);
    ctx->promise_ctor = JS_DupValue(ctx, obj1);
    JS_SetPropertyFunctionList(ctx, obj1,
                               js_promise_funcs,
                               countof(js_promise_funcs));
    JS_NewGlobalCConstructor2(ctx, obj1, "Promise",
                              ctx->class_proto[JS_CLASS_PROMISE]);

    /* AsyncFunction */
    ctx->class_proto[JS_CLASS_ASYNC_FUNCTION] = JS_NewObjectProto(ctx, ctx->function_proto);
    obj1 = JS_NewCFunction3(ctx, (JSCFunction *)js_function_constructor,
                            "AsyncFunction", 1,
                            JS_CFUNC_constructor_or_func_magic, JS_FUNC_ASYNC,
                            ctx->function_ctor);
    JS_SetPropertyFunctionList(ctx,
                               ctx->class_proto[JS_CLASS_ASYNC_FUNCTION],
                               js_async_function_proto_funcs,
                               countof(js_async_function_proto_funcs));
    JS_SetConstructor2(ctx, obj1, ctx->class_proto[JS_CLASS_ASYNC_FUNCTION],
                       0, JS_PROP_CONFIGURABLE);
    JS_FreeValue(ctx, obj1);

    /* AsyncIteratorPrototype */
    ctx->async_iterator_proto = JS_NewObject(ctx);
    JS_SetPropertyFunctionList(ctx, ctx->async_iterator_proto,
                               js_async_iterator_proto_funcs,
                               countof(js_async_iterator_proto_funcs));

    /* AsyncFromSyncIteratorPrototype */
    ctx->class_proto[JS_CLASS_ASYNC_FROM_SYNC_ITERATOR] =
        JS_NewObjectProto(ctx, ctx->async_iterator_proto);
    JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_ASYNC_FROM_SYNC_ITERATOR],
                               js_async_from_sync_iterator_proto_funcs,
                               countof(js_async_from_sync_iterator_proto_funcs));

    /* AsyncGeneratorPrototype */
    ctx->class_proto[JS_CLASS_ASYNC_GENERATOR] =
        JS_NewObjectProto(ctx, ctx->async_iterator_proto);
    JS_SetPropertyFunctionList(ctx,
                               ctx->class_proto[JS_CLASS_ASYNC_GENERATOR],
                               js_async_generator_proto_funcs,
                               countof(js_async_generator_proto_funcs));

    /* AsyncGeneratorFunction */
    ctx->class_proto[JS_CLASS_ASYNC_GENERATOR_FUNCTION] =
        JS_NewObjectProto(ctx, ctx->function_proto);
    obj1 = JS_NewCFunction3(ctx, (JSCFunction *)js_function_constructor,
                            "AsyncGeneratorFunction", 1,
                            JS_CFUNC_constructor_or_func_magic,
                            JS_FUNC_ASYNC_GENERATOR,
                            ctx->function_ctor);
    JS_SetPropertyFunctionList(ctx,
                               ctx->class_proto[JS_CLASS_ASYNC_GENERATOR_FUNCTION],
                               js_async_generator_function_proto_funcs,
                               countof(js_async_generator_function_proto_funcs));
    JS_SetConstructor2(ctx, ctx->class_proto[JS_CLASS_ASYNC_GENERATOR_FUNCTION],
                       ctx->class_proto[JS_CLASS_ASYNC_GENERATOR],
                       JS_PROP_CONFIGURABLE, JS_PROP_CONFIGURABLE);
    JS_SetConstructor2(ctx, obj1, ctx->class_proto[JS_CLASS_ASYNC_GENERATOR_FUNCTION],
                       0, JS_PROP_CONFIGURABLE);
    JS_FreeValue(ctx, obj1);
}

/* URI handling */

static int string_get_hex(JSString *p, int k, int n) {
    int c = 0, h;
    while (n-- > 0) {
        if ((h = from_hex(string_get(p, k++))) < 0)
            return -1;
        c = (c << 4) | h;
    }
    return c;
}

static int isURIReserved(int c) {
    return c < 0x100 && memchr(";/?:@&=+$,#", c, sizeof(";/?:@&=+$,#") - 1) != NULL;
}

static int __attribute__((format(printf, 2, 3))) js_throw_URIError(JSContext *ctx, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    JS_ThrowError(ctx, JS_URI_ERROR, fmt, ap);
    va_end(ap);
    return -1;
}

static int hex_decode(JSContext *ctx, JSString *p, int k) {
    int c;

    if (k >= p->len || string_get(p, k) != '%')
        return js_throw_URIError(ctx, "expecting %%");
    if (k + 2 >= p->len || (c = string_get_hex(p, k + 1, 2)) < 0)
        return js_throw_URIError(ctx, "expecting hex digit");

    return c;
}

static JSValue js_global_decodeURI(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv, int isComponent)
{
    JSValue str;
    StringBuffer b_s, *b = &b_s;
    JSString *p;
    int k, c, c1, n, c_min;

    str = JS_ToString(ctx, argv[0]);
    if (JS_IsException(str))
        return str;

    string_buffer_init(ctx, b, 0);

    p = JS_VALUE_GET_STRING(str);
    for (k = 0; k < p->len;) {
        c = string_get(p, k);
        if (c == '%') {
            c = hex_decode(ctx, p, k);
            if (c < 0)
                goto fail;
            k += 3;
            if (c < 0x80) {
                if (!isComponent && isURIReserved(c)) {
                    c = '%';
                    k -= 2;
                }
            } else {
                /* Decode URI-encoded UTF-8 sequence */
                if (c >= 0xc0 && c <= 0xdf) {
                    n = 1;
                    c_min = 0x80;
                    c &= 0x1f;
                } else if (c >= 0xe0 && c <= 0xef) {
                    n = 2;
                    c_min = 0x800;
                    c &= 0xf;
                } else if (c >= 0xf0 && c <= 0xf7) {
                    n = 3;
                    c_min = 0x10000;
                    c &= 0x7;
                } else {
                    n = 0;
                    c_min = 1;
                    c = 0;
                }
                while (n-- > 0) {
                    c1 = hex_decode(ctx, p, k);
                    if (c1 < 0)
                        goto fail;
                    k += 3;
                    if ((c1 & 0xc0) != 0x80) {
                        c = 0;
                        break;
                    }
                    c = (c << 6) | (c1 & 0x3f);
                }
                if (c < c_min || c > 0x10FFFF ||
                    (c >= 0xd800 && c < 0xe000)) {
                    js_throw_URIError(ctx, "malformed UTF-8");
                    goto fail;
                }
            }
        } else {
            k++;
        }
        string_buffer_putc(b, c);
    }
    JS_FreeValue(ctx, str);
    return string_buffer_end(b);

fail:
    JS_FreeValue(ctx, str);
    string_buffer_free(b);
    return JS_EXCEPTION;
}

static int isUnescaped(int c) {
    static char const unescaped_chars[] =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz"
        "0123456789"
        "@*_+-./";
    return c < 0x100 &&
        memchr(unescaped_chars, c, sizeof(unescaped_chars) - 1);
}

static int isURIUnescaped(int c, int isComponent) {
    return c < 0x100 &&
        ((c >= 0x61 && c <= 0x7a) ||
         (c >= 0x41 && c <= 0x5a) ||
         (c >= 0x30 && c <= 0x39) ||
         memchr("-_.!~*'()", c, sizeof("-_.!~*'()") - 1) != NULL ||
         (!isComponent && isURIReserved(c)));
}

static int encodeURI_hex(StringBuffer *b, int c) {
    uint8_t buf[6];
    int n = 0;
    const char *hex = "0123456789ABCDEF";

    buf[n++] = '%';
    if (c >= 256) {
        buf[n++] = 'u';
        buf[n++] = hex[(c >> 12) & 15];
        buf[n++] = hex[(c >>  8) & 15];
    }
    buf[n++] = hex[(c >> 4) & 15];
    buf[n++] = hex[(c >> 0) & 15];
    return string_buffer_write8(b, buf, n);
}

static JSValue js_global_encodeURI(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv,
                                   int isComponent)
{
    JSValue str;
    StringBuffer b_s, *b = &b_s;
    JSString *p;
    int k, c, c1;

    str = JS_ToString(ctx, argv[0]);
    if (JS_IsException(str))
        return str;

    p = JS_VALUE_GET_STRING(str);
    string_buffer_init(ctx, b, p->len);
    for (k = 0; k < p->len;) {
        c = string_get(p, k);
        k++;
        if (isURIUnescaped(c, isComponent)) {
            string_buffer_putc16(b, c);
        } else {
            if (c >= 0xdc00 && c <= 0xdfff) {
                js_throw_URIError(ctx, "invalid character");
                goto fail;
            } else if (c >= 0xd800 && c <= 0xdbff) {
                if (k >= p->len) {
                    js_throw_URIError(ctx, "expecting surrogate pair");
                    goto fail;
                }
                c1 = string_get(p, k);
                k++;
                if (c1 < 0xdc00 || c1 > 0xdfff) {
                    js_throw_URIError(ctx, "expecting surrogate pair");
                    goto fail;
                }
                c = (((c & 0x3ff) << 10) | (c1 & 0x3ff)) + 0x10000;
            }
            if (c < 0x80) {
                encodeURI_hex(b, c);
            } else {
                /* XXX: use C UTF-8 conversion ? */
                if (c < 0x800) {
                    encodeURI_hex(b, (c >> 6) | 0xc0);
                } else {
                    if (c < 0x10000) {
                        encodeURI_hex(b, (c >> 12) | 0xe0);
                    } else {
                        encodeURI_hex(b, (c >> 18) | 0xf0);
                        encodeURI_hex(b, ((c >> 12) & 0x3f) | 0x80);
                    }
                    encodeURI_hex(b, ((c >> 6) & 0x3f) | 0x80);
                }
                encodeURI_hex(b, (c & 0x3f) | 0x80);
            }
        }
    }
    JS_FreeValue(ctx, str);
    return string_buffer_end(b);

fail:
    JS_FreeValue(ctx, str);
    string_buffer_free(b);
    return JS_EXCEPTION;
}

static JSValue js_global_escape(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    JSValue str;
    StringBuffer b_s, *b = &b_s;
    JSString *p;
    int i, len, c;

    str = JS_ToString(ctx, argv[0]);
    if (JS_IsException(str))
        return str;

    p = JS_VALUE_GET_STRING(str);
    string_buffer_init(ctx, b, p->len);
    for (i = 0, len = p->len; i < len; i++) {
        c = string_get(p, i);
        if (isUnescaped(c)) {
            string_buffer_putc16(b, c);
        } else {
            encodeURI_hex(b, c);
        }
    }
    JS_FreeValue(ctx, str);
    return string_buffer_end(b);
}

static JSValue js_global_unescape(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv)
{
    JSValue str;
    StringBuffer b_s, *b = &b_s;
    JSString *p;
    int i, len, c, n;

    str = JS_ToString(ctx, argv[0]);
    if (JS_IsException(str))
        return str;

    string_buffer_init(ctx, b, 0);
    p = JS_VALUE_GET_STRING(str);
    for (i = 0, len = p->len; i < len; i++) {
        c = string_get(p, i);
        if (c == '%') {
            if (i + 6 <= len
            &&  string_get(p, i + 1) == 'u'
            &&  (n = string_get_hex(p, i + 2, 4)) >= 0) {
                c = n;
                i += 6 - 1;
            } else
            if (i + 3 <= len
            &&  (n = string_get_hex(p, i + 1, 2)) >= 0) {
                c = n;
                i += 3 - 1;
            }
        }
        string_buffer_putc16(b, c);
    }
    JS_FreeValue(ctx, str);
    return string_buffer_end(b);
}

/* global object */

static const JSCFunctionListEntry js_global_funcs[] = {
    JS_CFUNC_DEF("parseInt", 2, js_parseInt ),
    JS_CFUNC_DEF("parseFloat", 1, js_parseFloat ),
    JS_CFUNC_DEF("isNaN", 1, js_global_isNaN ),
    JS_CFUNC_DEF("isFinite", 1, js_global_isFinite ),

    JS_CFUNC_MAGIC_DEF("decodeURI", 1, js_global_decodeURI, 0 ),
    JS_CFUNC_MAGIC_DEF("decodeURIComponent", 1, js_global_decodeURI, 1 ),
    JS_CFUNC_MAGIC_DEF("encodeURI", 1, js_global_encodeURI, 0 ),
    JS_CFUNC_MAGIC_DEF("encodeURIComponent", 1, js_global_encodeURI, 1 ),
    JS_CFUNC_DEF("escape", 1, js_global_escape ),
    JS_CFUNC_DEF("unescape", 1, js_global_unescape ),
    JS_PROP_DOUBLE_DEF("Infinity", 1.0 / 0.0, 0 ),
    JS_PROP_DOUBLE_DEF("NaN", NAN, 0 ),
    JS_PROP_UNDEFINED_DEF("undefined", 0 ),

    /* for the 'Date' implementation */
    JS_CFUNC_DEF("__date_clock", 0, js___date_clock ),
    //JS_CFUNC_DEF("__date_now", 0, js___date_now ),
    //JS_CFUNC_DEF("__date_getTimezoneOffset", 1, js___date_getTimezoneOffset ),
    //JS_CFUNC_DEF("__date_create", 3, js___date_create ),
};

/* Date */

static int64_t math_mod(int64_t a, int64_t b) {
    /* return positive modulo */
    int64_t m = a % b;
    return m + (m < 0) * b;
}

static int64_t floor_div(int64_t a, int64_t b) {
    /* integer division rounding toward -Infinity */
    int64_t m = a % b;
    return (a - (m + (m < 0) * b)) / b;
}

static JSValue js_Date_parse(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv);

static __exception int JS_ThisTimeValue(JSContext *ctx, double *valp, JSValueConst this_val)
{
    if (JS_VALUE_GET_TAG(this_val) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(this_val);
        if (p->class_id == JS_CLASS_DATE && JS_IsNumber(p->u.object_data))
            return JS_ToFloat64(ctx, valp, p->u.object_data);
    }
    JS_ThrowTypeError(ctx, "not a Date object");
    return -1;
}

static JSValue JS_SetThisTimeValue(JSContext *ctx, JSValueConst this_val, double v)
{
    if (JS_VALUE_GET_TAG(this_val) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(this_val);
        if (p->class_id == JS_CLASS_DATE) {
            JS_FreeValue(ctx, p->u.object_data);
            p->u.object_data = JS_NewFloat64(ctx, v);
            return JS_DupValue(ctx, p->u.object_data);
        }
    }
    return JS_ThrowTypeError(ctx, "not a Date object");
}

static int64_t days_from_year(int64_t y) {
    return 365 * (y - 1970) + floor_div(y - 1969, 4) -
        floor_div(y - 1901, 100) + floor_div(y - 1601, 400);
}

static int64_t days_in_year(int64_t y) {
    return 365 + !(y % 4) - !(y % 100) + !(y % 400);
}

/* return the year, update days */
static int64_t year_from_days(int64_t *days) {
    int64_t y, d1, nd, d = *days;
    y = floor_div(d * 10000, 3652425) + 1970;
    /* the initial approximation is very good, so only a few
       iterations are necessary */
    for(;;) {
        d1 = d - days_from_year(y);
        if (d1 < 0) {
            y--;
            d1 += days_in_year(y);
        } else {
            nd = days_in_year(y);
            if (d1 < nd)
                break;
            d1 -= nd;
            y++;
        }
    }
    *days = d1;
    return y;
}

static int const month_days[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
static char const month_names[] = "JanFebMarAprMayJunJulAugSepOctNovDec";
static char const day_names[] = "SunMonTueWedThuFriSat";

static __exception int get_date_fields(JSContext *ctx, JSValueConst obj,
                                       double fields[9], int is_local, int force)
{
    double dval;
    int64_t d, days, wd, y, i, md, h, m, s, ms, tz = 0;

    if (JS_ThisTimeValue(ctx, &dval, obj))
        return -1;

    if (isnan(dval)) {
        if (!force)
            return FALSE; /* NaN */
        d = 0;        /* initialize all fields to 0 */
    } else {
        d = dval;
        if (is_local) {
            tz = -getTimezoneOffset(d);
            d += tz * 60000;
        }
    }

    /* result is >= 0, we can use % */
    h = math_mod(d, 86400000);
    days = (d - h) / 86400000;
    ms = h % 1000;
    h = (h - ms) / 1000;
    s = h % 60;
    h = (h - s) / 60;
    m = h % 60;
    h = (h - m) / 60;
    wd = math_mod(days + 4, 7); /* week day */
    y = year_from_days(&days);

    for(i = 0; i < 11; i++) {
        md = month_days[i];
        if (i == 1)
            md += days_in_year(y) - 365;
        if (days < md)
            break;
        days -= md;
    }
    fields[0] = y;
    fields[1] = i;
    fields[2] = days + 1;
    fields[3] = h;
    fields[4] = m;
    fields[5] = s;
    fields[6] = ms;
    fields[7] = wd;
    fields[8] = tz;
    return TRUE;
}

static double time_clip(double t) {
    if (t >= -8.64e15 && t <= 8.64e15)
        return trunc(t) + 0.0;  /* convert -0 to +0 */
    else
        return NAN;
}

/* The spec mandates the use of 'double' and it fixes the order
   of the operations */
static double set_date_fields(double fields[], int is_local) {
    int64_t y;
    double days, d, h, m1;
    int i, m, md;
    
    m1 = fields[1];
    m = fmod(m1, 12);
    if (m < 0)
        m += 12;
    y = (int64_t)(fields[0] + floor(m1 / 12));
    days = days_from_year(y);

    for(i = 0; i < m; i++) {
        md = month_days[i];
        if (i == 1)
            md += days_in_year(y) - 365;
        days += md;
    }
    days += fields[2] - 1;
    h = fields[3] * 3600000 + fields[4] * 60000 + 
        fields[5] * 1000 + fields[6];
    d = days * 86400000 + h;
    if (is_local)
        d += getTimezoneOffset(d) * 60000;
    return time_clip(d);
}

static JSValue get_date_field(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv, int magic)
{
    // get_date_field(obj, n, is_local)
    double fields[9];
    int res, n, is_local;

    is_local = magic & 0x0F;
    n = (magic >> 4) & 0x0F;
    res = get_date_fields(ctx, this_val, fields, is_local, 0);
    if (res < 0)
        return JS_EXCEPTION;
    if (!res)
        return JS_NAN;

    if (magic & 0x100) {    // getYear
        fields[0] -= 1900;
    }
    return JS_NewFloat64(ctx, fields[n]);
}

static JSValue set_date_field(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv, int magic)
{
    // _field(obj, first_field, end_field, args, is_local)
    double fields[9];
    int res, first_field, end_field, is_local, i, n;
    double d, a;

    d = NAN;
    first_field = (magic >> 8) & 0x0F;
    end_field = (magic >> 4) & 0x0F;
    is_local = magic & 0x0F;

    res = get_date_fields(ctx, this_val, fields, is_local, first_field == 0);
    if (res < 0)
        return JS_EXCEPTION;
    if (res && argc > 0) {
        n = end_field - first_field;
        if (argc < n)
            n = argc;
        for(i = 0; i < n; i++) {
            if (JS_ToFloat64(ctx, &a, argv[i]))
                return JS_EXCEPTION;
            if (!isfinite(a))
                goto done;
            fields[first_field + i] = trunc(a);
        }
        d = set_date_fields(fields, is_local);
    }
done:
    return JS_SetThisTimeValue(ctx, this_val, d);
}

/* fmt:
   0: toUTCString: "Tue, 02 Jan 2018 23:04:46 GMT"
   1: toString: "Wed Jan 03 2018 00:05:22 GMT+0100 (CET)"
   2: toISOString: "2018-01-02T23:02:56.927Z"
   3: toLocaleString: "1/2/2018, 11:40:40 PM"
   part: 1=date, 2=time 3=all
   XXX: should use a variant of strftime().
 */
static JSValue get_date_string(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv, int magic)
{
    // _string(obj, fmt, part)
    char buf[64];
    double fields[9];
    int res, fmt, part, pos;
    int y, mon, d, h, m, s, ms, wd, tz;

    fmt = (magic >> 4) & 0x0F;
    part = magic & 0x0F;

    res = get_date_fields(ctx, this_val, fields, fmt & 1, 0);
    if (res < 0)
        return JS_EXCEPTION;
    if (!res) {
        if (fmt == 2)
            return JS_ThrowRangeError(ctx, "Date value is NaN");
        else
            return JS_NewString(ctx, "Invalid Date");
    }

    y = fields[0];
    mon = fields[1];
    d = fields[2];
    h = fields[3];
    m = fields[4];
    s = fields[5];
    ms = fields[6];
    wd = fields[7];
    tz = fields[8];

    pos = 0;

    if (part & 1) { /* date part */
        switch(fmt) {
        case 0:
            pos += snprintf(buf + pos, sizeof(buf) - pos,
                            "%.3s, %02d %.3s %0*d ",
                            day_names + wd * 3, d,
                            month_names + mon * 3, 4 + (y < 0), y);
            break;
        case 1:
            pos += snprintf(buf + pos, sizeof(buf) - pos,
                            "%.3s %.3s %02d %0*d",
                            day_names + wd * 3,
                            month_names + mon * 3, d, 4 + (y < 0), y);
            if (part == 3) {
                buf[pos++] = ' ';
            }
            break;
        case 2:
            if (y >= 0 && y <= 9999) {
                pos += snprintf(buf + pos, sizeof(buf) - pos,
                                "%04d", y);
            } else {
                pos += snprintf(buf + pos, sizeof(buf) - pos,
                                "%+07d", y);
            }
            pos += snprintf(buf + pos, sizeof(buf) - pos,
                            "-%02d-%02dT", mon + 1, d);
            break;
        case 3:
            pos += snprintf(buf + pos, sizeof(buf) - pos,
                            "%02d/%02d/%0*d", mon + 1, d, 4 + (y < 0), y);
            if (part == 3) {
                buf[pos++] = ',';
                buf[pos++] = ' ';
            }
            break;
        }
    }
    if (part & 2) { /* time part */
        switch(fmt) {
        case 0:
            pos += snprintf(buf + pos, sizeof(buf) - pos,
                            "%02d:%02d:%02d GMT", h, m, s);
            break;
        case 1:
            pos += snprintf(buf + pos, sizeof(buf) - pos,
                            "%02d:%02d:%02d GMT", h, m, s);
            if (tz < 0) {
                buf[pos++] = '-';
                tz = -tz;
            } else {
                buf[pos++] = '+';
            }
            /* tz is >= 0, can use % */
            pos += snprintf(buf + pos, sizeof(buf) - pos,
                            "%02d%02d", tz / 60, tz % 60);
            /* XXX: tack the time zone code? */
            break;
        case 2:
            pos += snprintf(buf + pos, sizeof(buf) - pos,
                            "%02d:%02d:%02d.%03dZ", h, m, s, ms);
            break;
        case 3:
            pos += snprintf(buf + pos, sizeof(buf) - pos,
                            "%02d:%02d:%02d %cM", (h + 1) % 12 - 1, m, s,
                            (h < 12) ? 'A' : 'P');
            break;
        }
    }
    return JS_NewStringLen(ctx, buf, pos);
}

/* OS dependent: return the UTC time in ms since 1970. */
static int64_t date_now(void) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (int64_t)tv.tv_sec * 1000 + (tv.tv_usec / 1000);
}

static JSValue js_date_constructor(JSContext *ctx, JSValueConst new_target,
                                   int argc, JSValueConst *argv)
{
    // Date(y, mon, d, h, m, s, ms)
    JSValue rv;
    int i, n;
    double a, val;

    if (JS_IsUndefined(new_target)) {
        /* invoked as function */
        argc = 0;
    }
    n = argc;
    if (n == 0) {
        val = date_now();
    } else if (n == 1) {
        JSValue v, dv;
        if (JS_VALUE_GET_TAG(argv[0]) == JS_TAG_OBJECT) {
            JSObject *p = JS_VALUE_GET_OBJ(argv[0]);
            if (p->class_id == JS_CLASS_DATE && JS_IsNumber(p->u.object_data)) {
                if (JS_ToFloat64(ctx, &val, p->u.object_data))
                    return JS_EXCEPTION;
                val = time_clip(val);
                goto has_val;
            }
        }
        v = JS_ToPrimitive(ctx, argv[0], HINT_NONE);
        if (JS_IsString(v)) {
            dv = js_Date_parse(ctx, JS_UNDEFINED, 1, (JSValueConst *)&v);
            JS_FreeValue(ctx, v);
            if (JS_IsException(dv))
                return JS_EXCEPTION;
            if (JS_ToFloat64Free(ctx, &val, dv))
                return JS_EXCEPTION;
        } else {
            if (JS_ToFloat64Free(ctx, &val, v))
                return JS_EXCEPTION;
        }
        val = time_clip(val);
    } else {
        double fields[] = { 0, 0, 1, 0, 0, 0, 0 };
        if (n > 7)
            n = 7;
        for(i = 0; i < n; i++) {
            if (JS_ToFloat64(ctx, &a, argv[i]))
                return JS_EXCEPTION;
            if (!isfinite(a))
                break;
            fields[i] = trunc(a);
            if (i == 0 && fields[0] >= 0 && fields[0] < 100)
                fields[0] += 1900;
        }
        val = (i == n) ? set_date_fields(fields, 1) : NAN;
    }
has_val:
#if 0
    JSValueConst args[3];
    args[0] = new_target;
    args[1] = ctx->class_proto[JS_CLASS_DATE];
    args[2] = JS_NewFloat64(ctx, val);
    rv = js___date_create(ctx, JS_UNDEFINED, 3, args);
#else
    rv = js_create_from_ctor(ctx, new_target, JS_CLASS_DATE);
    if (!JS_IsException(rv))
        JS_SetObjectData(ctx, rv, JS_NewFloat64(ctx, val));
#endif
    if (!JS_IsException(rv) && JS_IsUndefined(new_target)) {
        /* invoked as a function, return (new Date()).toString(); */
        JSValue s;
        s = get_date_string(ctx, rv, 0, NULL, 0x13);
        JS_FreeValue(ctx, rv);
        rv = s;
    }
    return rv;
}

static JSValue js_Date_UTC(JSContext *ctx, JSValueConst this_val,
                           int argc, JSValueConst *argv)
{
    // UTC(y, mon, d, h, m, s, ms)
    double fields[] = { 0, 0, 1, 0, 0, 0, 0 };
    int i, n;
    double a;

    n = argc;
    if (n == 0)
        return JS_NAN;
    if (n > 7)
        n = 7;
    for(i = 0; i < n; i++) {
        if (JS_ToFloat64(ctx, &a, argv[i]))
            return JS_EXCEPTION;
        if (!isfinite(a))
            return JS_NAN;
        fields[i] = trunc(a);
        if (i == 0 && fields[0] >= 0 && fields[0] < 100)
            fields[0] += 1900;
    }
    return JS_NewFloat64(ctx, set_date_fields(fields, 0));
}

static void string_skip_spaces(JSString *sp, int *pp) {
    while (*pp < sp->len && string_get(sp, *pp) == ' ')
        *pp += 1;
}

static void string_skip_non_spaces(JSString *sp, int *pp) {
    while (*pp < sp->len && string_get(sp, *pp) != ' ')
        *pp += 1;
}

/* parse a numeric field with an optional sign if accept_sign is TRUE */
static int string_get_digits(JSString *sp, int *pp, int64_t *pval) {
    int64_t v = 0;
    int c, p = *pp, p_start;
    
    if (p >= sp->len)
        return -1;
    p_start = p;
    while (p < sp->len) {
        c = string_get(sp, p);
        if (!(c >= '0' && c <= '9')) {
            if (p == p_start)
                return -1;
            else
                break;
        }
        v = v * 10 + c - '0';
        p++;
    }
    *pval = v;
    *pp = p;
    return 0;
}

static int string_get_signed_digits(JSString *sp, int *pp, int64_t *pval) {
    int res, sgn, p = *pp;
    
    if (p >= sp->len)
        return -1;

    sgn = string_get(sp, p);
    if (sgn == '-' || sgn == '+')
        p++;
 
    res = string_get_digits(sp, &p, pval);
    if (res == 0 && sgn == '-')
        *pval = -*pval;
    *pp = p;
    return res;
}

/* parse a fixed width numeric field */
static int string_get_fixed_width_digits(JSString *sp, int *pp, int n, int64_t *pval) {
    int64_t v = 0;
    int i, c, p = *pp;

    for(i = 0; i < n; i++) {
        if (p >= sp->len)
            return -1;
        c = string_get(sp, p);
        if (!(c >= '0' && c <= '9'))
            return -1;
        v = v * 10 + c - '0';
        p++;
    }
    *pval = v;
    *pp = p;
    return 0;
}

static int string_get_milliseconds(JSString *sp, int *pp, int64_t *pval) {
    /* parse milliseconds as a fractional part, round to nearest */
    /* XXX: the spec does not indicate which rounding should be used */
    int mul = 1000, ms = 0, p = *pp, c, p_start;
    if (p >= sp->len)
        return -1;
    p_start = p;
    while (p < sp->len) {
        c = string_get(sp, p);
        if (!(c >= '0' && c <= '9')) {
            if (p == p_start)
                return -1;
            else
                break;
        }
        if (mul == 1 && c >= '5')
            ms += 1;
        ms += (c - '0') * (mul /= 10);
        p++;
    }
    *pval = ms;
    *pp = p;
    return 0;
}


static int find_abbrev(JSString *sp, int p, const char *list, int count) {
    int n, i;

    if (p + 3 <= sp->len) {
        for (n = 0; n < count; n++) {
            for (i = 0; i < 3; i++) {
                if (string_get(sp, p + i) != month_names[n * 3 + i])
                    goto next;
            }
            return n;
        next:;
        }
    }
    return -1;
}

static int string_get_month(JSString *sp, int *pp, int64_t *pval) {
    int n;

    string_skip_spaces(sp, pp);
    n = find_abbrev(sp, *pp, month_names, 12);
    if (n < 0)
        return -1;

    *pval = n;
    *pp += 3;
    return 0;
}

static JSValue js_Date_parse(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv)
{
    // parse(s)
    JSValue s, rv;
    int64_t fields[] = { 0, 1, 1, 0, 0, 0, 0 };
    double fields1[7];
    int64_t tz, hh, mm;
    double d;
    int p, i, c, sgn, l;
    JSString *sp;
    BOOL is_local;
    
    rv = JS_NAN;

    s = JS_ToString(ctx, argv[0]);
    if (JS_IsException(s))
        return JS_EXCEPTION;
    
    sp = JS_VALUE_GET_STRING(s);
    p = 0;
    if (p < sp->len && (((c = string_get(sp, p)) >= '0' && c <= '9') || c == '+' || c == '-')) {
        /* ISO format */
        /* year field can be negative */
        if (string_get_signed_digits(sp, &p, &fields[0]))
            goto done;

        for (i = 1; i < 7; i++) {
            if (p >= sp->len)
                break;
            switch(i) {
            case 1:
            case 2:
                c = '-';
                break;
            case 3:
                c = 'T';
                break;
            case 4:
            case 5:
                c = ':';
                break;
            case 6:
                c = '.';
                break;
            }
            if (string_get(sp, p) != c)
                break;
            p++;
            if (i == 6) {
                if (string_get_milliseconds(sp, &p, &fields[i]))
                    goto done;
            } else {
                if (string_get_digits(sp, &p, &fields[i]))
                    goto done;
            }
        }
        /* no time: UTC by default */
        is_local = (i > 3);
        fields[1] -= 1;

        /* parse the time zone offset if present: [+-]HH:mm or [+-]HHmm */
        tz = 0;
        if (p < sp->len) {
            sgn = string_get(sp, p);
            if (sgn == '+' || sgn == '-') {
                p++;
                l = sp->len - p;
                if (l != 4 && l != 5)
                    goto done;
                if (string_get_fixed_width_digits(sp, &p, 2, &hh))
                    goto done;
                if (l == 5) {
                    if (string_get(sp, p) != ':')
                        goto done;
                    p++;
                }
                if (string_get_fixed_width_digits(sp, &p, 2, &mm))
                    goto done;
                tz = hh * 60 + mm;
                if (sgn == '-')
                    tz = -tz;
                is_local = FALSE;
            } else if (sgn == 'Z') {
                p++;
                is_local = FALSE;
            } else {
                goto done;
            }
            /* error if extraneous characters */
            if (p != sp->len)
                goto done;
        }
    } else {
        /* toString or toUTCString format */
        /* skip the day of the week */
        string_skip_non_spaces(sp, &p);
        string_skip_spaces(sp, &p);
        if (p >= sp->len)
            goto done;
        c = string_get(sp, p);
        if (c >= '0' && c <= '9') {
            /* day of month first */
            if (string_get_digits(sp, &p, &fields[2]))
                goto done;
            if (string_get_month(sp, &p, &fields[1]))
                goto done;
        } else {
            /* month first */
            if (string_get_month(sp, &p, &fields[1]))
                goto done;
            string_skip_spaces(sp, &p);
            if (string_get_digits(sp, &p, &fields[2]))
                goto done;
        }
        /* year */
        string_skip_spaces(sp, &p);
        if (string_get_signed_digits(sp, &p, &fields[0]))
            goto done;

        /* hour, min, seconds */
        string_skip_spaces(sp, &p);
        for(i = 0; i < 3; i++) {
            if (i == 1 || i == 2) {
                if (p >= sp->len)
                    goto done;
                if (string_get(sp, p) != ':')
                    goto done;
                p++;
            }
            if (string_get_digits(sp, &p, &fields[3 + i]))
                goto done;
        }
        // XXX: parse optional milliseconds?

        /* parse the time zone offset if present: [+-]HHmm */
        is_local = FALSE;
        tz = 0;
        for (tz = 0; p < sp->len; p++) {
            sgn = string_get(sp, p);
            if (sgn == '+' || sgn == '-') {
                p++;
                if (string_get_fixed_width_digits(sp, &p, 2, &hh))
                    goto done;
                if (string_get_fixed_width_digits(sp, &p, 2, &mm))
                    goto done;
                tz = hh * 60 + mm;
                if (sgn == '-')
                    tz = -tz;
                break;
            }
        }
    }
    for(i = 0; i < 7; i++)
        fields1[i] = fields[i];
    d = set_date_fields(fields1, is_local) - tz * 60000;
    rv = JS_NewFloat64(ctx, d);

done:
    JS_FreeValue(ctx, s);
    return rv;
}

static JSValue js_Date_now(JSContext *ctx, JSValueConst this_val,
                           int argc, JSValueConst *argv)
{
    // now()
    return JS_NewInt64(ctx, date_now());
}

static JSValue js_date_Symbol_toPrimitive(JSContext *ctx, JSValueConst this_val,
                                          int argc, JSValueConst *argv)
{
    // Symbol_toPrimitive(hint)
    JSValueConst obj = this_val;
    JSAtom hint = JS_ATOM_NULL;
    int hint_num;

    if (!JS_IsObject(obj))
        return JS_ThrowTypeErrorNotAnObject(ctx);

    if (JS_IsString(argv[0])) {
        hint = JS_ValueToAtom(ctx, argv[0]);
        if (hint == JS_ATOM_NULL)
            return JS_EXCEPTION;
        JS_FreeAtom(ctx, hint);
    }
    switch (hint) {
    case JS_ATOM_number:
#ifdef CONFIG_BIGNUM
    case JS_ATOM_integer:
#endif
        hint_num = HINT_NUMBER;
        break;
    case JS_ATOM_string:
    case JS_ATOM_default:
        hint_num = HINT_STRING;
        break;
    default:
        return JS_ThrowTypeError(ctx, "invalid hint");
    }
    return JS_ToPrimitive(ctx, obj, hint_num | HINT_FORCE_ORDINARY);
}

static JSValue js_date_getTimezoneOffset(JSContext *ctx, JSValueConst this_val,
                                         int argc, JSValueConst *argv)
{
    // getTimezoneOffset()
    double v;

    if (JS_ThisTimeValue(ctx, &v, this_val))
        return JS_EXCEPTION;
    if (isnan(v))
        return JS_NAN;
    else
        return JS_NewInt64(ctx, getTimezoneOffset((int64_t)trunc(v)));
}

static JSValue js_date_getTime(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv)
{
    // getTime()
    double v;

    if (JS_ThisTimeValue(ctx, &v, this_val))
        return JS_EXCEPTION;
    return JS_NewFloat64(ctx, v);
}

static JSValue js_date_setTime(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv)
{
    // setTime(v)
    double v;

    if (JS_ThisTimeValue(ctx, &v, this_val) || JS_ToFloat64(ctx, &v, argv[0]))
        return JS_EXCEPTION;
    return JS_SetThisTimeValue(ctx, this_val, time_clip(v));
}

static JSValue js_date_setYear(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv)
{
    // setYear(y)
    double y;
    JSValueConst args[1];

    if (JS_ThisTimeValue(ctx, &y, this_val) || JS_ToFloat64(ctx, &y, argv[0]))
        return JS_EXCEPTION;
    y = +y;
    if (isfinite(y)) {
        y = trunc(y);
        if (y >= 0 && y < 100)
            y += 1900;
    }
    args[0] = JS_NewFloat64(ctx, y);
    return set_date_field(ctx, this_val, 1, args, 0x011);
}

static JSValue js_date_toJSON(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv)
{
    // toJSON(key)
    JSValue obj, tv, method, rv;
    double d;

    rv = JS_EXCEPTION;
    tv = JS_UNDEFINED;

    obj = JS_ToObject(ctx, this_val);
    tv = JS_ToPrimitive(ctx, obj, HINT_NUMBER);
    if (JS_IsException(tv))
        goto exception;
    if (JS_IsNumber(tv)) {
        if (JS_ToFloat64(ctx, &d, tv) < 0)
            goto exception;
        if (!isfinite(d)) {
            rv = JS_NULL;
            goto done;
        }
    }
    method = JS_GetPropertyStr(ctx, obj, "toISOString");
    if (JS_IsException(method))
        goto exception;
    if (!JS_IsFunction(ctx, method)) {
        JS_ThrowTypeError(ctx, "object needs toISOString method");
        JS_FreeValue(ctx, method);
        goto exception;
    }
    rv = JS_CallFree(ctx, method, obj, 0, NULL);
exception:
done:
    JS_FreeValue(ctx, obj);
    JS_FreeValue(ctx, tv);
    return rv;
}

static const JSCFunctionListEntry js_date_funcs[] = {
    JS_CFUNC_DEF("now", 0, js_Date_now ),
    JS_CFUNC_DEF("parse", 1, js_Date_parse ),
    JS_CFUNC_DEF("UTC", 7, js_Date_UTC ),
};

static const JSCFunctionListEntry js_date_proto_funcs[] = {
    JS_CFUNC_DEF("valueOf", 0, js_date_getTime ),
    JS_CFUNC_MAGIC_DEF("toString", 0, get_date_string, 0x13 ),
    JS_CFUNC_DEF("[Symbol.toPrimitive]", 1, js_date_Symbol_toPrimitive ),
    JS_CFUNC_MAGIC_DEF("toUTCString", 0, get_date_string, 0x03 ),
    JS_ALIAS_DEF("toGMTString", "toUTCString" ),
    JS_CFUNC_MAGIC_DEF("toISOString", 0, get_date_string, 0x23 ),
    JS_CFUNC_MAGIC_DEF("toDateString", 0, get_date_string, 0x11 ),
    JS_CFUNC_MAGIC_DEF("toTimeString", 0, get_date_string, 0x12 ),
    JS_CFUNC_MAGIC_DEF("toLocaleString", 0, get_date_string, 0x33 ),
    JS_CFUNC_MAGIC_DEF("toLocaleDateString", 0, get_date_string, 0x31 ),
    JS_CFUNC_MAGIC_DEF("toLocaleTimeString", 0, get_date_string, 0x32 ),
    JS_CFUNC_DEF("getTimezoneOffset", 0, js_date_getTimezoneOffset ),
    JS_CFUNC_DEF("getTime", 0, js_date_getTime ),
    JS_CFUNC_MAGIC_DEF("getYear", 0, get_date_field, 0x101 ),
    JS_CFUNC_MAGIC_DEF("getFullYear", 0, get_date_field, 0x01 ),
    JS_CFUNC_MAGIC_DEF("getUTCFullYear", 0, get_date_field, 0x00 ),
    JS_CFUNC_MAGIC_DEF("getMonth", 0, get_date_field, 0x11 ),
    JS_CFUNC_MAGIC_DEF("getUTCMonth", 0, get_date_field, 0x10 ),
    JS_CFUNC_MAGIC_DEF("getDate", 0, get_date_field, 0x21 ),
    JS_CFUNC_MAGIC_DEF("getUTCDate", 0, get_date_field, 0x20 ),
    JS_CFUNC_MAGIC_DEF("getHours", 0, get_date_field, 0x31 ),
    JS_CFUNC_MAGIC_DEF("getUTCHours", 0, get_date_field, 0x30 ),
    JS_CFUNC_MAGIC_DEF("getMinutes", 0, get_date_field, 0x41 ),
    JS_CFUNC_MAGIC_DEF("getUTCMinutes", 0, get_date_field, 0x40 ),
    JS_CFUNC_MAGIC_DEF("getSeconds", 0, get_date_field, 0x51 ),
    JS_CFUNC_MAGIC_DEF("getUTCSeconds", 0, get_date_field, 0x50 ),
    JS_CFUNC_MAGIC_DEF("getMilliseconds", 0, get_date_field, 0x61 ),
    JS_CFUNC_MAGIC_DEF("getUTCMilliseconds", 0, get_date_field, 0x60 ),
    JS_CFUNC_MAGIC_DEF("getDay", 0, get_date_field, 0x71 ),
    JS_CFUNC_MAGIC_DEF("getUTCDay", 0, get_date_field, 0x70 ),
    JS_CFUNC_DEF("setTime", 1, js_date_setTime ),
    JS_CFUNC_MAGIC_DEF("setMilliseconds", 1, set_date_field, 0x671 ),
    JS_CFUNC_MAGIC_DEF("setUTCMilliseconds", 1, set_date_field, 0x670 ),
    JS_CFUNC_MAGIC_DEF("setSeconds", 2, set_date_field, 0x571 ),
    JS_CFUNC_MAGIC_DEF("setUTCSeconds", 2, set_date_field, 0x570 ),
    JS_CFUNC_MAGIC_DEF("setMinutes", 3, set_date_field, 0x471 ),
    JS_CFUNC_MAGIC_DEF("setUTCMinutes", 3, set_date_field, 0x470 ),
    JS_CFUNC_MAGIC_DEF("setHours", 4, set_date_field, 0x371 ),
    JS_CFUNC_MAGIC_DEF("setUTCHours", 4, set_date_field, 0x370 ),
    JS_CFUNC_MAGIC_DEF("setDate", 1, set_date_field, 0x231 ),
    JS_CFUNC_MAGIC_DEF("setUTCDate", 1, set_date_field, 0x230 ),
    JS_CFUNC_MAGIC_DEF("setMonth", 2, set_date_field, 0x131 ),
    JS_CFUNC_MAGIC_DEF("setUTCMonth", 2, set_date_field, 0x130 ),
    JS_CFUNC_DEF("setYear", 1, js_date_setYear ),
    JS_CFUNC_MAGIC_DEF("setFullYear", 3, set_date_field, 0x031 ),
    JS_CFUNC_MAGIC_DEF("setUTCFullYear", 3, set_date_field, 0x030 ),
    JS_CFUNC_DEF("toJSON", 1, js_date_toJSON ),
};

void JS_AddIntrinsicDate(JSContext *ctx)
{
    JSValueConst obj;

    /* Date */
    ctx->class_proto[JS_CLASS_DATE] = JS_NewObject(ctx);
    JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_DATE], js_date_proto_funcs,
                               countof(js_date_proto_funcs));
    obj = JS_NewGlobalCConstructor(ctx, "Date", js_date_constructor, 7,
                                   ctx->class_proto[JS_CLASS_DATE]);
    JS_SetPropertyFunctionList(ctx, obj, js_date_funcs, countof(js_date_funcs));
}

/* eval */

void JS_AddIntrinsicEval(JSContext *ctx)
{
    ctx->eval_internal = __JS_EvalInternal;
}

#ifdef CONFIG_BIGNUM

/* Operators */

static void js_operator_set_finalizer(JSRuntime *rt, JSValue val)
{
    JSOperatorSetData *opset = JS_GetOpaque(val, JS_CLASS_OPERATOR_SET);
    int i, j;
    JSBinaryOperatorDefEntry *ent;
    
    if (opset) {
        for(i = 0; i < JS_OVOP_COUNT; i++) {
            if (opset->self_ops[i])
                JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_OBJECT, opset->self_ops[i]));
        }
        for(j = 0; j < opset->left.count; j++) {
            ent = &opset->left.tab[j];
            for(i = 0; i < JS_OVOP_BINARY_COUNT; i++) {
                if (ent->ops[i])
                    JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_OBJECT, ent->ops[i]));
            }
        }
        js_free_rt(rt, opset->left.tab);
        for(j = 0; j < opset->right.count; j++) {
            ent = &opset->right.tab[j];
            for(i = 0; i < JS_OVOP_BINARY_COUNT; i++) {
                if (ent->ops[i])
                    JS_FreeValueRT(rt, JS_MKPTR(JS_TAG_OBJECT, ent->ops[i]));
            }
        }
        js_free_rt(rt, opset->right.tab);
        js_free_rt(rt, opset);
    }
}

static void js_operator_set_mark(JSRuntime *rt, JSValueConst val,
                                 JS_MarkFunc *mark_func)
{
    JSOperatorSetData *opset = JS_GetOpaque(val, JS_CLASS_OPERATOR_SET);
    int i, j;
    JSBinaryOperatorDefEntry *ent;
    
    if (opset) {
        for(i = 0; i < JS_OVOP_COUNT; i++) {
            if (opset->self_ops[i])
                JS_MarkValue(rt, JS_MKPTR(JS_TAG_OBJECT, opset->self_ops[i]),
                             mark_func);
        }
        for(j = 0; j < opset->left.count; j++) {
            ent = &opset->left.tab[j];
            for(i = 0; i < JS_OVOP_BINARY_COUNT; i++) {
                if (ent->ops[i])
                    JS_MarkValue(rt, JS_MKPTR(JS_TAG_OBJECT, ent->ops[i]),
                                 mark_func);
            }
        }
        for(j = 0; j < opset->right.count; j++) {
            ent = &opset->right.tab[j];
            for(i = 0; i < JS_OVOP_BINARY_COUNT; i++) {
                if (ent->ops[i])
                    JS_MarkValue(rt, JS_MKPTR(JS_TAG_OBJECT, ent->ops[i]),
                                 mark_func);
            }
        }
    }
}


/* create an OperatorSet object */
static JSValue js_operators_create_internal(JSContext *ctx,
                                            int argc, JSValueConst *argv,
                                            BOOL is_primitive)
{
    JSValue opset_obj, prop, obj;
    JSOperatorSetData *opset, *opset1;
    JSBinaryOperatorDef *def;
    JSValueConst arg;
    int i, j;
    JSBinaryOperatorDefEntry *new_tab;
    JSBinaryOperatorDefEntry *ent;
    uint32_t op_count;

    if (ctx->rt->operator_count == UINT32_MAX) {
        return JS_ThrowTypeError(ctx, "too many operators");
    }
    opset_obj = JS_NewObjectProtoClass(ctx, JS_NULL, JS_CLASS_OPERATOR_SET);
    if (JS_IsException(opset_obj))
        goto fail;
    opset = js_mallocz(ctx, sizeof(*opset));
    if (!opset)
        goto fail;
    JS_SetOpaque(opset_obj, opset);
    if (argc >= 1) {
        arg = argv[0];
        /* self operators */
        for(i = 0; i < JS_OVOP_COUNT; i++) {
            prop = JS_GetPropertyStr(ctx, arg, js_overloadable_operator_names[i]);
            if (JS_IsException(prop))
                goto fail;
            if (!JS_IsUndefined(prop)) {
                if (check_function(ctx, prop)) {
                    JS_FreeValue(ctx, prop);
                    goto fail;
                }
                opset->self_ops[i] = JS_VALUE_GET_OBJ(prop);
            }
        }
    }
    /* left & right operators */
    for(j = 1; j < argc; j++) {
        arg = argv[j];
        prop = JS_GetPropertyStr(ctx, arg, "left");
        if (JS_IsException(prop))
            goto fail;
        def = &opset->right;
        if (JS_IsUndefined(prop)) {
            prop = JS_GetPropertyStr(ctx, arg, "right");
            if (JS_IsException(prop))
                goto fail;
            if (JS_IsUndefined(prop)) {
                JS_ThrowTypeError(ctx, "left or right property must be present");
                goto fail;
            }
            def = &opset->left;
        }
        /* get the operator set */
        obj = JS_GetProperty(ctx, prop, JS_ATOM_prototype);
        JS_FreeValue(ctx, prop);
        if (JS_IsException(obj))
            goto fail;
        prop = JS_GetProperty(ctx, obj, JS_ATOM_Symbol_operatorSet);
        JS_FreeValue(ctx, obj);
        if (JS_IsException(prop))
            goto fail;
        opset1 = JS_GetOpaque2(ctx, prop, JS_CLASS_OPERATOR_SET);
        if (!opset1) {
            JS_FreeValue(ctx, prop);
            goto fail;
        }
        op_count = opset1->operator_counter;
        JS_FreeValue(ctx, prop);
        
        /* we assume there are few entries */
        new_tab = js_realloc(ctx, def->tab,
                             (def->count + 1) * sizeof(def->tab[0]));
        if (!new_tab)
            goto fail;
        def->tab = new_tab;
        def->count++;
        ent = def->tab + def->count - 1;
        memset(ent, 0, sizeof(def->tab[0]));
        ent->operator_index = op_count;
        
        for(i = 0; i < JS_OVOP_BINARY_COUNT; i++) {
            prop = JS_GetPropertyStr(ctx, arg,
                                     js_overloadable_operator_names[i]);
            if (JS_IsException(prop))
                goto fail;
            if (!JS_IsUndefined(prop)) {
                if (check_function(ctx, prop)) {
                    JS_FreeValue(ctx, prop);
                    goto fail;
                }
                ent->ops[i] = JS_VALUE_GET_OBJ(prop);
            }
        }
    }
    opset->is_primitive = is_primitive;
    opset->operator_counter = ctx->rt->operator_count++;
    return opset_obj;
 fail:
    JS_FreeValue(ctx, opset_obj);
    return JS_EXCEPTION;
}

static JSValue js_operators_create(JSContext *ctx, JSValueConst this_val,
                                int argc, JSValueConst *argv)
{
    return js_operators_create_internal(ctx, argc, argv, FALSE);
}

static JSValue js_operators_updateBigIntOperators(JSContext *ctx, JSValueConst this_val,
                                                  int argc, JSValueConst *argv)
{
    JSValue opset_obj, prop;
    JSOperatorSetData *opset;
    const JSOverloadableOperatorEnum ops[2] = { JS_OVOP_DIV, JS_OVOP_POW };
    JSOverloadableOperatorEnum op;
    int i;
    
    opset_obj = JS_GetProperty(ctx, ctx->class_proto[JS_CLASS_BIG_INT],
                               JS_ATOM_Symbol_operatorSet);
    if (JS_IsException(opset_obj))
        goto fail;
    opset = JS_GetOpaque2(ctx, opset_obj, JS_CLASS_OPERATOR_SET);
    if (!opset)
        goto fail;
    for(i = 0; i < countof(ops); i++) {
        op = ops[i];
        prop = JS_GetPropertyStr(ctx, argv[0],
                                 js_overloadable_operator_names[op]);
        if (JS_IsException(prop))
            goto fail;
        if (!JS_IsUndefined(prop)) {
            if (!JS_IsNull(prop) && check_function(ctx, prop)) {
                JS_FreeValue(ctx, prop);
                goto fail;
            }
            if (opset->self_ops[op])
                JS_FreeValue(ctx, JS_MKPTR(JS_TAG_OBJECT, opset->self_ops[op]));
            if (JS_IsNull(prop)) {
                opset->self_ops[op] = NULL;
            } else {
                opset->self_ops[op] = JS_VALUE_GET_PTR(prop);
            }
        }
    }
    JS_FreeValue(ctx, opset_obj);
    return JS_UNDEFINED;
 fail:
    JS_FreeValue(ctx, opset_obj);
    return JS_EXCEPTION;
}

static int js_operators_set_default(JSContext *ctx, JSValueConst obj)
{
    JSValue opset_obj;

    if (!JS_IsObject(obj)) /* in case the prototype is not defined */
        return 0;
    opset_obj = js_operators_create_internal(ctx, 0, NULL, TRUE);
    if (JS_IsException(opset_obj))
        return -1;
    /* cannot be modified by the user */
    JS_DefinePropertyValue(ctx, obj, JS_ATOM_Symbol_operatorSet,
                           opset_obj, 0);
    return 0;
}

static JSValue js_dummy_operators_ctor(JSContext *ctx, JSValueConst new_target,
                                       int argc, JSValueConst *argv)
{
    return js_create_from_ctor(ctx, new_target, JS_CLASS_OBJECT);
}

static JSValue js_global_operators(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv)
{
    JSValue func_obj, proto, opset_obj;

    func_obj = JS_UNDEFINED;
    proto = JS_NewObject(ctx);
    if (JS_IsException(proto))
        return JS_EXCEPTION;
    opset_obj = js_operators_create_internal(ctx, argc, argv, FALSE);
    if (JS_IsException(opset_obj))
        goto fail;
    JS_DefinePropertyValue(ctx, proto, JS_ATOM_Symbol_operatorSet,
                           opset_obj, JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    func_obj = JS_NewCFunction2(ctx, js_dummy_operators_ctor, "Operators",
                                0, JS_CFUNC_constructor, 0);
    if (JS_IsException(func_obj))
        goto fail;
    JS_SetConstructor2(ctx, func_obj, proto,
                       0, JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    JS_FreeValue(ctx, proto);
    return func_obj;
 fail:
    JS_FreeValue(ctx, proto);
    JS_FreeValue(ctx, func_obj);
    return JS_EXCEPTION;
}

static const JSCFunctionListEntry js_operators_funcs[] = {
    JS_CFUNC_DEF("create", 1, js_operators_create ),
    JS_CFUNC_DEF("updateBigIntOperators", 2, js_operators_updateBigIntOperators ),
};

/* must be called after all overloadable base types are initialized */
void JS_AddIntrinsicOperators(JSContext *ctx)
{
    JSValue obj;

    ctx->allow_operator_overloading = TRUE;
    obj = JS_NewCFunction(ctx, js_global_operators, "Operators", 1);
    JS_SetPropertyFunctionList(ctx, obj,
                               js_operators_funcs,
                               countof(js_operators_funcs));
    JS_DefinePropertyValue(ctx, ctx->global_obj, JS_ATOM_Operators,
                           obj,
                           JS_PROP_WRITABLE | JS_PROP_CONFIGURABLE);
    /* add default operatorSets */
    js_operators_set_default(ctx, ctx->class_proto[JS_CLASS_BOOLEAN]);
    js_operators_set_default(ctx, ctx->class_proto[JS_CLASS_NUMBER]);
    js_operators_set_default(ctx, ctx->class_proto[JS_CLASS_STRING]);
    js_operators_set_default(ctx, ctx->class_proto[JS_CLASS_BIG_INT]);
    js_operators_set_default(ctx, ctx->class_proto[JS_CLASS_BIG_FLOAT]);
    js_operators_set_default(ctx, ctx->class_proto[JS_CLASS_BIG_DECIMAL]);
}

/* BigInt */

static JSValue JS_ToBigIntCtorFree(JSContext *ctx, JSValue val)
{
    uint32_t tag;

 redo:
    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
    case JS_TAG_INT:
    case JS_TAG_BOOL:
        val = JS_NewBigInt64(ctx, JS_VALUE_GET_INT(val));
        break;
    case JS_TAG_BIG_INT:
        break;
    case JS_TAG_FLOAT64:
    case JS_TAG_BIG_FLOAT:
        {
            bf_t *a, a_s;
            
            a = JS_ToBigFloat(ctx, &a_s, val);
            if (!bf_is_finite(a)) {
                JS_FreeValue(ctx, val);
                val = JS_ThrowRangeError(ctx, "cannot convert NaN or Infinity to bigint");
            } else {
                JSValue val1 = JS_NewBigInt(ctx);
                bf_t *r;
                int ret;
                if (JS_IsException(val1)) {
                    JS_FreeValue(ctx, val);
                    return JS_EXCEPTION;
                }
                r = JS_GetBigInt(val1);
                ret = bf_set(r, a);
                ret |= bf_rint(r, BF_RNDZ);
                JS_FreeValue(ctx, val);
                if (ret & BF_ST_MEM_ERROR) {
                    JS_FreeValue(ctx, val1);
                    val = JS_ThrowOutOfMemory(ctx);
                } else if (ret & BF_ST_INEXACT) {
                    JS_FreeValue(ctx, val1);
                    val = JS_ThrowRangeError(ctx, "cannot convert to bigint: not an integer");
                } else {
                    val = JS_CompactBigInt(ctx, val1);
                }
            }
            if (a == &a_s)
                bf_delete(a);
        }
        break;
    case JS_TAG_BIG_DECIMAL:
        val = JS_ToStringFree(ctx, val);
         if (JS_IsException(val))
            break;
        goto redo;
    case JS_TAG_STRING:
        val = JS_StringToBigIntErr(ctx, val);
        break;
    case JS_TAG_OBJECT:
        val = JS_ToPrimitiveFree(ctx, val, HINT_NUMBER);
        if (JS_IsException(val))
            break;
        goto redo;
    case JS_TAG_NULL:
    case JS_TAG_UNDEFINED:
    default:
        JS_FreeValue(ctx, val);
        return JS_ThrowTypeError(ctx, "cannot convert to bigint");
    }
    return val;
}

static JSValue js_bigint_constructor(JSContext *ctx,
                                     JSValueConst new_target,
                                     int argc, JSValueConst *argv)
{
    if (!JS_IsUndefined(new_target))
        return JS_ThrowTypeError(ctx, "not a constructor");
    return JS_ToBigIntCtorFree(ctx, JS_DupValue(ctx, argv[0]));
}

static JSValue js_thisBigIntValue(JSContext *ctx, JSValueConst this_val)
{
    if (JS_IsBigInt(ctx, this_val))
        return JS_DupValue(ctx, this_val);

    if (JS_VALUE_GET_TAG(this_val) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(this_val);
        if (p->class_id == JS_CLASS_BIG_INT) {
            if (JS_IsBigInt(ctx, p->u.object_data))
                return JS_DupValue(ctx, p->u.object_data);
        }
    }
    return JS_ThrowTypeError(ctx, "not a bigint");
}

static JSValue js_bigint_toString(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv)
{
    JSValue val;
    int base;
    JSValue ret;

    val = js_thisBigIntValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    if (argc == 0 || JS_IsUndefined(argv[0])) {
        base = 10;
    } else {
        base = js_get_radix(ctx, argv[0]);
        if (base < 0)
            goto fail;
    }
    ret = js_bigint_to_string1(ctx, val, base);
    JS_FreeValue(ctx, val);
    return ret;
 fail:
    JS_FreeValue(ctx, val);
    return JS_EXCEPTION;
}

static JSValue js_bigint_valueOf(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    return js_thisBigIntValue(ctx, this_val);
}

static JSValue js_bigint_div(JSContext *ctx,
                              JSValueConst this_val,
                              int argc, JSValueConst *argv, int magic)
{
    bf_t a_s, b_s, *a, *b, *r, *q;
    int status;
    JSValue q_val, r_val;
    
    q_val = JS_NewBigInt(ctx);
    if (JS_IsException(q_val))
        return JS_EXCEPTION;
    r_val = JS_NewBigInt(ctx);
    if (JS_IsException(r_val))
        goto fail;
    b = NULL;
    a = JS_ToBigInt(ctx, &a_s, argv[0]);
    if (!a)
        goto fail;
    b = JS_ToBigInt(ctx, &b_s, argv[1]);
    if (!b) {
        JS_FreeBigInt(ctx, a, &a_s);
        goto fail;
    }
    q = JS_GetBigInt(q_val);
    r = JS_GetBigInt(r_val);
    status = bf_divrem(q, r, a, b, BF_PREC_INF, BF_RNDZ, magic & 0xf);
    JS_FreeBigInt(ctx, a, &a_s);
    JS_FreeBigInt(ctx, b, &b_s);
    if (unlikely(status)) {
        throw_bf_exception(ctx, status);
        goto fail;
    }
    q_val = JS_CompactBigInt(ctx, q_val);
    if (magic & 0x10) {
        JSValue ret;
        ret = JS_NewArray(ctx);
        if (JS_IsException(ret))
            goto fail;
        JS_SetPropertyUint32(ctx, ret, 0, q_val);
        JS_SetPropertyUint32(ctx, ret, 1, JS_CompactBigInt(ctx, r_val));
        return ret;
    } else {
        JS_FreeValue(ctx, r_val);
        return q_val;
    }
 fail:
    JS_FreeValue(ctx, q_val);
    JS_FreeValue(ctx, r_val);
    return JS_EXCEPTION;
}

static JSValue js_bigint_sqrt(JSContext *ctx,
                               JSValueConst this_val,
                               int argc, JSValueConst *argv, int magic)
{
    bf_t a_s, *a, *r, *rem;
    int status;
    JSValue r_val, rem_val;
    
    r_val = JS_NewBigInt(ctx);
    if (JS_IsException(r_val))
        return JS_EXCEPTION;
    rem_val = JS_NewBigInt(ctx);
    if (JS_IsException(rem_val))
        return JS_EXCEPTION;
    r = JS_GetBigInt(r_val);
    rem = JS_GetBigInt(rem_val);

    a = JS_ToBigInt(ctx, &a_s, argv[0]);
    if (!a)
        goto fail;
    status = bf_sqrtrem(r, rem, a);
    JS_FreeBigInt(ctx, a, &a_s);
    if (unlikely(status & ~BF_ST_INEXACT)) {
        throw_bf_exception(ctx, status);
        goto fail;
    }
    r_val = JS_CompactBigInt(ctx, r_val);
    if (magic) {
        JSValue ret;
        ret = JS_NewArray(ctx);
        if (JS_IsException(ret))
            goto fail;
        JS_SetPropertyUint32(ctx, ret, 0, r_val);
        JS_SetPropertyUint32(ctx, ret, 1, JS_CompactBigInt(ctx, rem_val));
        return ret;
    } else {
        JS_FreeValue(ctx, rem_val);
        return r_val;
    }
 fail:
    JS_FreeValue(ctx, r_val);
    JS_FreeValue(ctx, rem_val);
    return JS_EXCEPTION;
}

static JSValue js_bigint_op1(JSContext *ctx,
                              JSValueConst this_val,
                              int argc, JSValueConst *argv,
                              int magic)
{
    bf_t a_s, *a;
    int64_t res;

    a = JS_ToBigInt(ctx, &a_s, argv[0]);
    if (!a)
        return JS_EXCEPTION;
    switch(magic) {
    case 0: /* floorLog2 */
        if (a->sign || a->expn <= 0) {
            res = -1;
        } else {
            res = a->expn - 1;
        }
        break;
    case 1: /* ctz */
        if (bf_is_zero(a)) {
            res = -1;
        } else {
            res = bf_get_exp_min(a);
        }
        break;
    default:
        abort();
    }
    JS_FreeBigInt(ctx, a, &a_s);
    return JS_NewBigInt64(ctx, res);
}

static JSValue js_bigint_asUintN(JSContext *ctx,
                                  JSValueConst this_val,
                                  int argc, JSValueConst *argv, int asIntN)
{
    uint64_t bits;
    bf_t a_s, *a = &a_s, *r, mask_s, *mask = &mask_s;
    JSValue res;
    
    if (JS_ToIndex(ctx, &bits, argv[0]))
        return JS_EXCEPTION;
    res = JS_NewBigInt(ctx);
    if (JS_IsException(res))
        return JS_EXCEPTION;
    r = JS_GetBigInt(res);
    a = JS_ToBigInt(ctx, &a_s, argv[1]);
    if (!a) {
        JS_FreeValue(ctx, res);
        return JS_EXCEPTION;
    }
    /* XXX: optimize */
    r = JS_GetBigInt(res);
    bf_init(ctx->bf_ctx, mask);
    bf_set_ui(mask, 1);
    bf_mul_2exp(mask, bits, BF_PREC_INF, BF_RNDZ);
    bf_add_si(mask, mask, -1, BF_PREC_INF, BF_RNDZ);
    bf_logic_and(r, a, mask);
    if (asIntN && bits != 0) {
        bf_set_ui(mask, 1);
        bf_mul_2exp(mask, bits - 1, BF_PREC_INF, BF_RNDZ);
        if (bf_cmpu(r, mask) >= 0) {
            bf_set_ui(mask, 1);
            bf_mul_2exp(mask, bits, BF_PREC_INF, BF_RNDZ);
            bf_sub(r, r, mask, BF_PREC_INF, BF_RNDZ);
        }
    }
    bf_delete(mask);
    JS_FreeBigInt(ctx, a, &a_s);
    return JS_CompactBigInt(ctx, res);
}

static const JSCFunctionListEntry js_bigint_funcs[] = {
    JS_CFUNC_MAGIC_DEF("asUintN", 2, js_bigint_asUintN, 0 ),
    JS_CFUNC_MAGIC_DEF("asIntN", 2, js_bigint_asUintN, 1 ),
    /* QuickJS extensions */
    JS_CFUNC_MAGIC_DEF("tdiv", 2, js_bigint_div, BF_RNDZ ),
    JS_CFUNC_MAGIC_DEF("fdiv", 2, js_bigint_div, BF_RNDD ),
    JS_CFUNC_MAGIC_DEF("cdiv", 2, js_bigint_div, BF_RNDU ),
    JS_CFUNC_MAGIC_DEF("ediv", 2, js_bigint_div, BF_DIVREM_EUCLIDIAN ),
    JS_CFUNC_MAGIC_DEF("tdivrem", 2, js_bigint_div, BF_RNDZ | 0x10 ),
    JS_CFUNC_MAGIC_DEF("fdivrem", 2, js_bigint_div, BF_RNDD | 0x10 ),
    JS_CFUNC_MAGIC_DEF("cdivrem", 2, js_bigint_div, BF_RNDU | 0x10 ),
    JS_CFUNC_MAGIC_DEF("edivrem", 2, js_bigint_div, BF_DIVREM_EUCLIDIAN | 0x10 ),
    JS_CFUNC_MAGIC_DEF("sqrt", 1, js_bigint_sqrt, 0 ),
    JS_CFUNC_MAGIC_DEF("sqrtrem", 1, js_bigint_sqrt, 1 ),
    JS_CFUNC_MAGIC_DEF("floorLog2", 1, js_bigint_op1, 0 ),
    JS_CFUNC_MAGIC_DEF("ctz", 1, js_bigint_op1, 1 ),
};

static const JSCFunctionListEntry js_bigint_proto_funcs[] = {
    JS_CFUNC_DEF("toString", 0, js_bigint_toString ),
    JS_CFUNC_DEF("valueOf", 0, js_bigint_valueOf ),
    JS_PROP_STRING_DEF("[Symbol.toStringTag]", "BigInt", JS_PROP_CONFIGURABLE ),
};

void JS_AddIntrinsicBigInt(JSContext *ctx)
{
    JSRuntime *rt = ctx->rt;
    JSValueConst obj1;

    rt->bigint_ops.to_string = js_bigint_to_string;
    rt->bigint_ops.from_string = js_string_to_bigint;
    rt->bigint_ops.unary_arith = js_unary_arith_bigint;
    rt->bigint_ops.binary_arith = js_binary_arith_bigint;
    rt->bigint_ops.compare = js_compare_bigfloat;
    
    ctx->class_proto[JS_CLASS_BIG_INT] = JS_NewObject(ctx);
    JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_BIG_INT],
                               js_bigint_proto_funcs,
                               countof(js_bigint_proto_funcs));
    obj1 = JS_NewGlobalCConstructor(ctx, "BigInt", js_bigint_constructor, 1,
                                    ctx->class_proto[JS_CLASS_BIG_INT]);
    JS_SetPropertyFunctionList(ctx, obj1, js_bigint_funcs,
                               countof(js_bigint_funcs));
}

/* BigFloat */

static JSValue js_thisBigFloatValue(JSContext *ctx, JSValueConst this_val)
{
    if (JS_IsBigFloat(this_val))
        return JS_DupValue(ctx, this_val);

    if (JS_VALUE_GET_TAG(this_val) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(this_val);
        if (p->class_id == JS_CLASS_BIG_FLOAT) {
            if (JS_IsBigFloat(p->u.object_data))
                return JS_DupValue(ctx, p->u.object_data);
        }
    }
    return JS_ThrowTypeError(ctx, "not a bigfloat");
}

static JSValue js_bigfloat_toString(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv)
{
    JSValue val;
    int base;
    JSValue ret;

    val = js_thisBigFloatValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    if (argc == 0 || JS_IsUndefined(argv[0])) {
        base = 10;
    } else {
        base = js_get_radix(ctx, argv[0]);
        if (base < 0)
            goto fail;
    }
    ret = js_ftoa(ctx, val, base, 0, BF_RNDN | BF_FTOA_FORMAT_FREE_MIN);
    JS_FreeValue(ctx, val);
    return ret;
 fail:
    JS_FreeValue(ctx, val);
    return JS_EXCEPTION;
}

static JSValue js_bigfloat_valueOf(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv)
{
    return js_thisBigFloatValue(ctx, this_val);
}

static int bigfloat_get_rnd_mode(JSContext *ctx, JSValueConst val)
{
    int rnd_mode;
    if (JS_ToInt32Sat(ctx, &rnd_mode, val))
        return -1;
    if (rnd_mode < BF_RNDN || rnd_mode > BF_RNDF) {
        JS_ThrowRangeError(ctx, "invalid rounding mode");
        return -1;
    }
    return rnd_mode;
}

static JSValue js_bigfloat_toFixed(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    JSValue val, ret;
    int64_t f;
    int rnd_mode, radix;

    val = js_thisBigFloatValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    if (JS_ToInt64Sat(ctx, &f, argv[0]))
        goto fail;
    if (f < 0 || f > BF_PREC_MAX) {
        JS_ThrowRangeError(ctx, "invalid number of digits");
        goto fail;
    }
    rnd_mode = BF_RNDNA;
    radix = 10;
    /* XXX: swap parameter order for rounding mode and radix */
    if (argc > 1) {
        rnd_mode = bigfloat_get_rnd_mode(ctx, argv[1]);
        if (rnd_mode < 0)
            goto fail;
    }
    if (argc > 2) {
        radix = js_get_radix(ctx, argv[2]);
        if (radix < 0)
            goto fail;
    }
    ret = js_ftoa(ctx, val, radix, f, rnd_mode | BF_FTOA_FORMAT_FRAC);
    JS_FreeValue(ctx, val);
    return ret;
 fail:
    JS_FreeValue(ctx, val);
    return JS_EXCEPTION;
}

static BOOL js_bigfloat_is_finite(JSContext *ctx, JSValueConst val)
{
    BOOL res;
    uint32_t tag;

    tag = JS_VALUE_GET_NORM_TAG(val);
    switch(tag) {
    case JS_TAG_BIG_FLOAT:
        {
            JSBigFloat *p = JS_VALUE_GET_PTR(val);
            res = bf_is_finite(&p->num);
        }
        break;
    default:
        res = FALSE;
        break;
    }
    return res;
}

static JSValue js_bigfloat_toExponential(JSContext *ctx, JSValueConst this_val,
                                       int argc, JSValueConst *argv)
{
    JSValue val, ret;
    int64_t f;
    int rnd_mode, radix;

    val = js_thisBigFloatValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    if (JS_ToInt64Sat(ctx, &f, argv[0]))
        goto fail;
    if (!js_bigfloat_is_finite(ctx, val)) {
        ret = JS_ToString(ctx, val);
    } else if (JS_IsUndefined(argv[0])) {
        ret = js_ftoa(ctx, val, 10, 0,
                      BF_RNDN | BF_FTOA_FORMAT_FREE_MIN | BF_FTOA_FORCE_EXP);
    } else {
        if (f < 0 || f > BF_PREC_MAX) {
            JS_ThrowRangeError(ctx, "invalid number of digits");
            goto fail;
        }
        rnd_mode = BF_RNDNA;
        radix = 10;
        if (argc > 1) {
            rnd_mode = bigfloat_get_rnd_mode(ctx, argv[1]);
            if (rnd_mode < 0)
                goto fail;
        }
        if (argc > 2) {
            radix = js_get_radix(ctx, argv[2]);
            if (radix < 0)
                goto fail;
        }
        ret = js_ftoa(ctx, val, radix, f + 1,
                      rnd_mode | BF_FTOA_FORMAT_FIXED | BF_FTOA_FORCE_EXP);
    }
    JS_FreeValue(ctx, val);
    return ret;
 fail:
    JS_FreeValue(ctx, val);
    return JS_EXCEPTION;
}

static JSValue js_bigfloat_toPrecision(JSContext *ctx, JSValueConst this_val,
                                     int argc, JSValueConst *argv)
{
    JSValue val, ret;
    int64_t p;
    int rnd_mode, radix;

    val = js_thisBigFloatValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    if (JS_IsUndefined(argv[0]))
        goto to_string;
    if (JS_ToInt64Sat(ctx, &p, argv[0]))
        goto fail;
    if (!js_bigfloat_is_finite(ctx, val)) {
    to_string:
        ret = JS_ToString(ctx, this_val);
    } else {
        if (p < 1 || p > BF_PREC_MAX) {
            JS_ThrowRangeError(ctx, "invalid number of digits");
            goto fail;
        }
        rnd_mode = BF_RNDNA;
        radix = 10;
        if (argc > 1) {
            rnd_mode = bigfloat_get_rnd_mode(ctx, argv[1]);
            if (rnd_mode < 0)
                goto fail;
        }
        if (argc > 2) {
            radix = js_get_radix(ctx, argv[2]);
            if (radix < 0)
                goto fail;
        }
        ret = js_ftoa(ctx, val, radix, p, rnd_mode | BF_FTOA_FORMAT_FIXED);
    }
    JS_FreeValue(ctx, val);
    return ret;
 fail:
    JS_FreeValue(ctx, val);
    return JS_EXCEPTION;
}

static const JSCFunctionListEntry js_bigfloat_proto_funcs[] = {
    JS_CFUNC_DEF("toString", 0, js_bigfloat_toString ),
    JS_CFUNC_DEF("valueOf", 0, js_bigfloat_valueOf ),
    JS_CFUNC_DEF("toPrecision", 1, js_bigfloat_toPrecision ),
    JS_CFUNC_DEF("toFixed", 1, js_bigfloat_toFixed ),
    JS_CFUNC_DEF("toExponential", 1, js_bigfloat_toExponential ),
};

static JSValue js_bigfloat_constructor(JSContext *ctx,
                                       JSValueConst new_target,
                                       int argc, JSValueConst *argv)
{
    JSValue val;
    if (!JS_IsUndefined(new_target))
        return JS_ThrowTypeError(ctx, "not a constructor");
    if (argc == 0) {
        bf_t *r;
        val = JS_NewBigFloat(ctx);
        if (JS_IsException(val))
            return val;
        r = JS_GetBigFloat(val);
        bf_set_zero(r, 0);
    } else {
        val = JS_DupValue(ctx, argv[0]);
    redo:
        switch(JS_VALUE_GET_NORM_TAG(val)) {
        case JS_TAG_BIG_FLOAT:
            break;
        case JS_TAG_FLOAT64:
            {
                bf_t *r;
                double d = JS_VALUE_GET_FLOAT64(val);
                val = JS_NewBigFloat(ctx);
                if (JS_IsException(val))
                    break;
                r = JS_GetBigFloat(val);
                if (bf_set_float64(r, d))
                    goto fail;
            }
            break;
        case JS_TAG_INT:
            {
                bf_t *r;
                int32_t v = JS_VALUE_GET_INT(val);
                val = JS_NewBigFloat(ctx);
                if (JS_IsException(val))
                    break;
                r = JS_GetBigFloat(val);
                if (bf_set_si(r, v))
                    goto fail;
            }
            break;
        case JS_TAG_BIG_INT:
            /* We keep the full precision of the integer */
            {
                JSBigFloat *p = JS_VALUE_GET_PTR(val);
                val = JS_MKPTR(JS_TAG_BIG_FLOAT, p);
            }
            break;
        case JS_TAG_BIG_DECIMAL:
            val = JS_ToStringFree(ctx, val);
            if (JS_IsException(val))
                break;
            goto redo;
        case JS_TAG_STRING:
            {
                const char *str, *p;
                size_t len;
                int err;

                str = JS_ToCStringLen(ctx, &len, val);
                JS_FreeValue(ctx, val);
                if (!str)
                    return JS_EXCEPTION;
                p = str;
                p += skip_spaces(p);
                if ((p - str) == len) {
                    bf_t *r;
                    val = JS_NewBigFloat(ctx);
                    if (JS_IsException(val))
                        break;
                    r = JS_GetBigFloat(val);
                    bf_set_zero(r, 0);
                    err = 0;
                } else {
                    val = js_atof(ctx, p, &p, 0, ATOD_ACCEPT_BIN_OCT |
                                  ATOD_TYPE_BIG_FLOAT |
                                  ATOD_ACCEPT_PREFIX_AFTER_SIGN);
                    if (JS_IsException(val)) {
                        JS_FreeCString(ctx, str);
                        return JS_EXCEPTION;
                    }
                    p += skip_spaces(p);
                    err = ((p - str) != len);
                }
                JS_FreeCString(ctx, str);
                if (err) {
                    JS_FreeValue(ctx, val);
                    return JS_ThrowSyntaxError(ctx, "invalid bigfloat literal");
                }
            }
            break;
        case JS_TAG_OBJECT:
            val = JS_ToPrimitiveFree(ctx, val, HINT_NUMBER);
            if (JS_IsException(val))
                break;
            goto redo;
        case JS_TAG_NULL:
        case JS_TAG_UNDEFINED:
        default:
            JS_FreeValue(ctx, val);
            return JS_ThrowTypeError(ctx, "cannot convert to bigfloat");
        }
    }
    return val;
 fail:
    JS_FreeValue(ctx, val);
    return JS_EXCEPTION;
}

static JSValue js_bigfloat_get_const(JSContext *ctx,
                                     JSValueConst this_val, int magic)
{
    bf_t *r;
    JSValue val;
    val = JS_NewBigFloat(ctx);
    if (JS_IsException(val))
        return val;
    r = JS_GetBigFloat(val);
    switch(magic) {
    case 0: /* PI */
        bf_const_pi(r, ctx->fp_env.prec, ctx->fp_env.flags);
        break;
    case 1: /* LN2 */
        bf_const_log2(r, ctx->fp_env.prec, ctx->fp_env.flags);
        break;
    case 2: /* MIN_VALUE */
    case 3: /* MAX_VALUE */
        {
            slimb_t e_range, e;
            e_range = (limb_t)1 << (bf_get_exp_bits(ctx->fp_env.flags) - 1);
            bf_set_ui(r, 1);
            if (magic == 2) {
                e = -e_range + 2;
                if (ctx->fp_env.flags & BF_FLAG_SUBNORMAL)
                    e -= ctx->fp_env.prec - 1;
                bf_mul_2exp(r, e, ctx->fp_env.prec, ctx->fp_env.flags);
            } else {
                bf_mul_2exp(r, ctx->fp_env.prec, ctx->fp_env.prec,
                            ctx->fp_env.flags);
                bf_add_si(r, r, -1, ctx->fp_env.prec, ctx->fp_env.flags);
                bf_mul_2exp(r, e_range - ctx->fp_env.prec, ctx->fp_env.prec,
                            ctx->fp_env.flags);
            }
        }
        break;
    case 4: /* EPSILON */
        bf_set_ui(r, 1);
        bf_mul_2exp(r, 1 - ctx->fp_env.prec,
                    ctx->fp_env.prec, ctx->fp_env.flags);
        break;
    default:
        abort();
    }
    return val;
}

static JSValue js_bigfloat_parseFloat(JSContext *ctx, JSValueConst this_val,
                                      int argc, JSValueConst *argv)
{
    bf_t *a;
    const char *str;
    JSValue ret;
    int radix;
    JSFloatEnv *fe;

    str = JS_ToCString(ctx, argv[0]);
    if (!str)
        return JS_EXCEPTION;
    if (JS_ToInt32(ctx, &radix, argv[1])) {
    fail:
        JS_FreeCString(ctx, str);
        return JS_EXCEPTION;
    }
    if (radix != 0 && (radix < 2 || radix > 36)) {
        JS_ThrowRangeError(ctx, "radix must be between 2 and 36");
        goto fail;
    }
    fe = &ctx->fp_env;
    if (argc > 2) {
        fe = JS_GetOpaque2(ctx, argv[2], JS_CLASS_FLOAT_ENV);
        if (!fe)
            goto fail;
    }
    ret = JS_NewBigFloat(ctx);
    if (JS_IsException(ret))
        goto done;
    a = JS_GetBigFloat(ret);
    /* XXX: use js_atof() */
    bf_atof(a, str, NULL, radix, fe->prec, fe->flags);
 done:
    JS_FreeCString(ctx, str);
    return ret;
}

static JSValue js_bigfloat_isFinite(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv)
{
    JSValueConst val = argv[0];
    JSBigFloat *p;
    
    if (JS_VALUE_GET_NORM_TAG(val) != JS_TAG_BIG_FLOAT)
        return JS_FALSE;
    p = JS_VALUE_GET_PTR(val);
    return JS_NewBool(ctx, bf_is_finite(&p->num));
}

static JSValue js_bigfloat_isNaN(JSContext *ctx, JSValueConst this_val,
                                 int argc, JSValueConst *argv)
{
    JSValueConst val = argv[0];
    JSBigFloat *p;
    
    if (JS_VALUE_GET_NORM_TAG(val) != JS_TAG_BIG_FLOAT)
        return JS_FALSE;
    p = JS_VALUE_GET_PTR(val);
    return JS_NewBool(ctx, bf_is_nan(&p->num));
}

enum {
    MATH_OP_ABS,
    MATH_OP_FLOOR,
    MATH_OP_CEIL,
    MATH_OP_ROUND,
    MATH_OP_TRUNC,
    MATH_OP_SQRT,
    MATH_OP_FPROUND,
    MATH_OP_ACOS,
    MATH_OP_ASIN,
    MATH_OP_ATAN,
    MATH_OP_ATAN2,
    MATH_OP_COS,
    MATH_OP_EXP,
    MATH_OP_LOG,
    MATH_OP_POW,
    MATH_OP_SIN,
    MATH_OP_TAN,
    MATH_OP_FMOD,
    MATH_OP_REM,
    MATH_OP_SIGN,

    MATH_OP_ADD,
    MATH_OP_SUB,
    MATH_OP_MUL,
    MATH_OP_DIV,
};

static JSValue js_bigfloat_fop(JSContext *ctx, JSValueConst this_val,
                           int argc, JSValueConst *argv, int magic)
{
    bf_t a_s, *a, *r;
    JSFloatEnv *fe;
    int rnd_mode;
    JSValue op1, res;

    op1 = JS_ToNumeric(ctx, argv[0]);
    if (JS_IsException(op1))
        return op1;
    a = JS_ToBigFloat(ctx, &a_s, op1);
    fe = &ctx->fp_env;
    if (argc > 1) {
        fe = JS_GetOpaque2(ctx, argv[1], JS_CLASS_FLOAT_ENV);
        if (!fe)
            goto fail;
    }
    res = JS_NewBigFloat(ctx);
    if (JS_IsException(res)) {
    fail:
        if (a == &a_s)
            bf_delete(a);
        JS_FreeValue(ctx, op1);
        return JS_EXCEPTION;
    }
    r = JS_GetBigFloat(res);
    switch (magic) {
    case MATH_OP_ABS:
        bf_set(r, a);
        r->sign = 0;
        break;
    case MATH_OP_FLOOR:
        rnd_mode = BF_RNDD;
        goto rint;
    case MATH_OP_CEIL:
        rnd_mode = BF_RNDU;
        goto rint;
    case MATH_OP_ROUND:
        rnd_mode = BF_RNDNA;
        goto rint;
    case MATH_OP_TRUNC:
        rnd_mode = BF_RNDZ;
    rint:
        bf_set(r, a);
        fe->status |= bf_rint(r, rnd_mode);
        break;
    case MATH_OP_SQRT:
        fe->status |= bf_sqrt(r, a, fe->prec, fe->flags);
        break;
    case MATH_OP_FPROUND:
        bf_set(r, a);
        fe->status |= bf_round(r, fe->prec, fe->flags);
        break;
    case MATH_OP_ACOS:
        fe->status |= bf_acos(r, a, fe->prec, fe->flags);
        break;
    case MATH_OP_ASIN:
        fe->status |= bf_asin(r, a, fe->prec, fe->flags);
        break;
    case MATH_OP_ATAN:
        fe->status |= bf_atan(r, a, fe->prec, fe->flags);
        break;
    case MATH_OP_COS:
        fe->status |= bf_cos(r, a, fe->prec, fe->flags);
        break;
    case MATH_OP_EXP:
        fe->status |= bf_exp(r, a, fe->prec, fe->flags);
        break;
    case MATH_OP_LOG:
        fe->status |= bf_log(r, a, fe->prec, fe->flags);
        break;
    case MATH_OP_SIN:
        fe->status |= bf_sin(r, a, fe->prec, fe->flags);
        break;
    case MATH_OP_TAN:
        fe->status |= bf_tan(r, a, fe->prec, fe->flags);
        break;
    case MATH_OP_SIGN:
        if (bf_is_nan(a) || bf_is_zero(a)) {
            bf_set(r, a);
        } else {
            bf_set_si(r, 1 - 2 * a->sign);
        }
        break;
    default:
        abort();
    }
    if (a == &a_s)
        bf_delete(a);
    JS_FreeValue(ctx, op1);
    return res;
}

static JSValue js_bigfloat_fop2(JSContext *ctx, JSValueConst this_val,
                            int argc, JSValueConst *argv, int magic)
{
    bf_t a_s, *a, b_s, *b, r_s, *r = &r_s;
    JSFloatEnv *fe;
    JSValue op1, op2, res;

    op1 = JS_ToNumeric(ctx, argv[0]);
    if (JS_IsException(op1))
        return op1;
    op2 = JS_ToNumeric(ctx, argv[1]);
    if (JS_IsException(op2)) {
        JS_FreeValue(ctx, op1);
        return op2;
    }
    a = JS_ToBigFloat(ctx, &a_s, op1);
    b = JS_ToBigFloat(ctx, &b_s, op2);
    fe = &ctx->fp_env;
    if (argc > 2) {
        fe = JS_GetOpaque2(ctx, argv[2], JS_CLASS_FLOAT_ENV);
        if (!fe)
            goto fail;
    }
    res = JS_NewBigFloat(ctx);
    if (JS_IsException(res)) {
    fail:
        if (a == &a_s)
            bf_delete(a);
        if (b == &b_s)
            bf_delete(b);
        JS_FreeValue(ctx, op1);
        JS_FreeValue(ctx, op2);
        return JS_EXCEPTION;
    }
    r = JS_GetBigFloat(res);
    switch (magic) {
    case MATH_OP_ATAN2:
        fe->status |= bf_atan2(r, a, b, fe->prec, fe->flags);
        break;
    case MATH_OP_POW:
        fe->status |= bf_pow(r, a, b, fe->prec, fe->flags | BF_POW_JS_QUIRKS);
        break;
    case MATH_OP_FMOD:
        fe->status |= bf_rem(r, a, b, fe->prec, fe->flags, BF_RNDZ);
        break;
    case MATH_OP_REM:
        fe->status |= bf_rem(r, a, b, fe->prec, fe->flags, BF_RNDN);
        break;
    case MATH_OP_ADD:
        fe->status |= bf_add(r, a, b, fe->prec, fe->flags);
        break;
    case MATH_OP_SUB:
        fe->status |= bf_sub(r, a, b, fe->prec, fe->flags);
        break;
    case MATH_OP_MUL:
        fe->status |= bf_mul(r, a, b, fe->prec, fe->flags);
        break;
    case MATH_OP_DIV:
        fe->status |= bf_div(r, a, b, fe->prec, fe->flags);
        break;
    default:
        abort();
    }
    if (a == &a_s)
        bf_delete(a);
    if (b == &b_s)
        bf_delete(b);
    JS_FreeValue(ctx, op1);
    JS_FreeValue(ctx, op2);
    return res;
}

static const JSCFunctionListEntry js_bigfloat_funcs[] = {
    JS_CGETSET_MAGIC_DEF("PI", js_bigfloat_get_const, NULL, 0 ),
    JS_CGETSET_MAGIC_DEF("LN2", js_bigfloat_get_const, NULL, 1 ),
    JS_CGETSET_MAGIC_DEF("MIN_VALUE", js_bigfloat_get_const, NULL, 2 ),
    JS_CGETSET_MAGIC_DEF("MAX_VALUE", js_bigfloat_get_const, NULL, 3 ),
    JS_CGETSET_MAGIC_DEF("EPSILON", js_bigfloat_get_const, NULL, 4 ),
    JS_CFUNC_DEF("parseFloat", 1, js_bigfloat_parseFloat ),
    JS_CFUNC_DEF("isFinite", 1, js_bigfloat_isFinite ),
    JS_CFUNC_DEF("isNaN", 1, js_bigfloat_isNaN ),
    JS_CFUNC_MAGIC_DEF("abs", 1, js_bigfloat_fop, MATH_OP_ABS ),
    JS_CFUNC_MAGIC_DEF("fpRound", 1, js_bigfloat_fop, MATH_OP_FPROUND ),
    JS_CFUNC_MAGIC_DEF("floor", 1, js_bigfloat_fop, MATH_OP_FLOOR ),
    JS_CFUNC_MAGIC_DEF("ceil", 1, js_bigfloat_fop, MATH_OP_CEIL ),
    JS_CFUNC_MAGIC_DEF("round", 1, js_bigfloat_fop, MATH_OP_ROUND ),
    JS_CFUNC_MAGIC_DEF("trunc", 1, js_bigfloat_fop, MATH_OP_TRUNC ),
    JS_CFUNC_MAGIC_DEF("sqrt", 1, js_bigfloat_fop, MATH_OP_SQRT ),
    JS_CFUNC_MAGIC_DEF("acos", 1, js_bigfloat_fop, MATH_OP_ACOS ),
    JS_CFUNC_MAGIC_DEF("asin", 1, js_bigfloat_fop, MATH_OP_ASIN ),
    JS_CFUNC_MAGIC_DEF("atan", 1, js_bigfloat_fop, MATH_OP_ATAN ),
    JS_CFUNC_MAGIC_DEF("atan2", 2, js_bigfloat_fop2, MATH_OP_ATAN2 ),
    JS_CFUNC_MAGIC_DEF("cos", 1, js_bigfloat_fop, MATH_OP_COS ),
    JS_CFUNC_MAGIC_DEF("exp", 1, js_bigfloat_fop, MATH_OP_EXP ),
    JS_CFUNC_MAGIC_DEF("log", 1, js_bigfloat_fop, MATH_OP_LOG ),
    JS_CFUNC_MAGIC_DEF("pow", 2, js_bigfloat_fop2, MATH_OP_POW ),
    JS_CFUNC_MAGIC_DEF("sin", 1, js_bigfloat_fop, MATH_OP_SIN ),
    JS_CFUNC_MAGIC_DEF("tan", 1, js_bigfloat_fop, MATH_OP_TAN ),
    JS_CFUNC_MAGIC_DEF("sign", 1, js_bigfloat_fop, MATH_OP_SIGN ),
    JS_CFUNC_MAGIC_DEF("add", 2, js_bigfloat_fop2, MATH_OP_ADD ),
    JS_CFUNC_MAGIC_DEF("sub", 2, js_bigfloat_fop2, MATH_OP_SUB ),
    JS_CFUNC_MAGIC_DEF("mul", 2, js_bigfloat_fop2, MATH_OP_MUL ),
    JS_CFUNC_MAGIC_DEF("div", 2, js_bigfloat_fop2, MATH_OP_DIV ),
    JS_CFUNC_MAGIC_DEF("fmod", 2, js_bigfloat_fop2, MATH_OP_FMOD ),
    JS_CFUNC_MAGIC_DEF("remainder", 2, js_bigfloat_fop2, MATH_OP_REM ),
};

/* FloatEnv */

static JSValue js_float_env_constructor(JSContext *ctx,
                                        JSValueConst new_target,
                                        int argc, JSValueConst *argv)
{
    JSValue obj;
    JSFloatEnv *fe;
    int64_t prec;
    int flags, rndmode;

    prec = ctx->fp_env.prec;
    flags = ctx->fp_env.flags;
    if (!JS_IsUndefined(argv[0])) {
        if (JS_ToInt64Sat(ctx, &prec, argv[0]))
            return JS_EXCEPTION;
        if (prec < BF_PREC_MIN || prec > BF_PREC_MAX)
            return JS_ThrowRangeError(ctx, "invalid precision");
        flags = BF_RNDN; /* RNDN, max exponent size, no subnormal */
        if (argc > 1 && !JS_IsUndefined(argv[1])) {
            if (JS_ToInt32Sat(ctx, &rndmode, argv[1]))
                return JS_EXCEPTION;
            if (rndmode < BF_RNDN || rndmode > BF_RNDF)
                return JS_ThrowRangeError(ctx, "invalid rounding mode");
            flags = rndmode;
        }
    }

    obj = JS_NewObjectClass(ctx, JS_CLASS_FLOAT_ENV);
    if (JS_IsException(obj))
        return JS_EXCEPTION;
    fe = js_malloc(ctx, sizeof(*fe));
    if (!fe)
        return JS_EXCEPTION;
    fe->prec = prec;
    fe->flags = flags;
    fe->status = 0;
    JS_SetOpaque(obj, fe);
    return obj;
}

static void js_float_env_finalizer(JSRuntime *rt, JSValue val)
{
    JSFloatEnv *fe = JS_GetOpaque(val, JS_CLASS_FLOAT_ENV);
    js_free_rt(rt, fe);
}

static JSValue js_float_env_get_prec(JSContext *ctx, JSValueConst this_val)
{
    return JS_NewInt64(ctx, ctx->fp_env.prec);
}

static JSValue js_float_env_get_expBits(JSContext *ctx, JSValueConst this_val)
{
    return JS_NewInt32(ctx, bf_get_exp_bits(ctx->fp_env.flags));
}

static JSValue js_float_env_setPrec(JSContext *ctx,
                                    JSValueConst this_val,
                                    int argc, JSValueConst *argv)
{
    JSValueConst func;
    int exp_bits, flags, saved_flags;
    JSValue ret;
    limb_t saved_prec;
    int64_t prec;

    func = argv[0];
    if (JS_ToInt64Sat(ctx, &prec, argv[1]))
        return JS_EXCEPTION;
    if (prec < BF_PREC_MIN || prec > BF_PREC_MAX)
        return JS_ThrowRangeError(ctx, "invalid precision");
    exp_bits = BF_EXP_BITS_MAX;

    if (argc > 2 && !JS_IsUndefined(argv[2])) {
        if (JS_ToInt32Sat(ctx, &exp_bits, argv[2]))
            return JS_EXCEPTION;
        if (exp_bits < BF_EXP_BITS_MIN || exp_bits > BF_EXP_BITS_MAX)
            return JS_ThrowRangeError(ctx, "invalid number of exponent bits");
    }

    flags = BF_RNDN | BF_FLAG_SUBNORMAL | bf_set_exp_bits(exp_bits);

    saved_prec = ctx->fp_env.prec;
    saved_flags = ctx->fp_env.flags;

    ctx->fp_env.prec = prec;
    ctx->fp_env.flags = flags;

    ret = JS_Call(ctx, func, JS_UNDEFINED, 0, NULL);
    /* always restore the floating point precision */
    ctx->fp_env.prec = saved_prec;
    ctx->fp_env.flags = saved_flags;
    return ret;
}

#define FE_PREC      (-1)
#define FE_EXP       (-2)
#define FE_RNDMODE   (-3)
#define FE_SUBNORMAL (-4)

static JSValue js_float_env_proto_get_status(JSContext *ctx, JSValueConst this_val, int magic)
{
    JSFloatEnv *fe;
    fe = JS_GetOpaque2(ctx, this_val, JS_CLASS_FLOAT_ENV);
    if (!fe)
        return JS_EXCEPTION;
    switch(magic) {
    case FE_PREC:
        return JS_NewInt64(ctx, fe->prec);
    case FE_EXP:
        return JS_NewInt32(ctx, bf_get_exp_bits(fe->flags));
    case FE_RNDMODE:
        return JS_NewInt32(ctx, fe->flags & BF_RND_MASK);
    case FE_SUBNORMAL:
        return JS_NewBool(ctx, (fe->flags & BF_FLAG_SUBNORMAL) != 0);
    default:
        return JS_NewBool(ctx, (fe->status & magic) != 0);
    }
}

static JSValue js_float_env_proto_set_status(JSContext *ctx, JSValueConst this_val, JSValueConst val, int magic)
{
    JSFloatEnv *fe;
    int b;
    int64_t prec;

    fe = JS_GetOpaque2(ctx, this_val, JS_CLASS_FLOAT_ENV);
    if (!fe)
        return JS_EXCEPTION;
    switch(magic) {
    case FE_PREC:
        if (JS_ToInt64Sat(ctx, &prec, val))
            return JS_EXCEPTION;
        if (prec < BF_PREC_MIN || prec > BF_PREC_MAX)
            return JS_ThrowRangeError(ctx, "invalid precision");
        fe->prec = prec;
        break;
    case FE_EXP:
        if (JS_ToInt32Sat(ctx, &b, val))
            return JS_EXCEPTION;
        if (b < BF_EXP_BITS_MIN || b > BF_EXP_BITS_MAX)
            return JS_ThrowRangeError(ctx, "invalid number of exponent bits");
        fe->flags = (fe->flags & ~(BF_EXP_BITS_MASK << BF_EXP_BITS_SHIFT)) |
            bf_set_exp_bits(b);
        break;
    case FE_RNDMODE:
        b = bigfloat_get_rnd_mode(ctx, val);
        if (b < 0)
            return JS_EXCEPTION;
        fe->flags = (fe->flags & ~BF_RND_MASK) | b;
        break;
    case FE_SUBNORMAL:
        b = JS_ToBool(ctx, val);
        fe->flags = (fe->flags & ~BF_FLAG_SUBNORMAL) | (b ? BF_FLAG_SUBNORMAL: 0);
        break;
    default:
        b = JS_ToBool(ctx, val);
        fe->status = (fe->status & ~magic) & ((-b) & magic);
        break;
    }
    return JS_UNDEFINED;
}

static JSValue js_float_env_clearStatus(JSContext *ctx,
                                        JSValueConst this_val,
                                        int argc, JSValueConst *argv)
{
    JSFloatEnv *fe = JS_GetOpaque2(ctx, this_val, JS_CLASS_FLOAT_ENV);
    if (!fe)
        return JS_EXCEPTION;
    fe->status = 0;
    return JS_UNDEFINED;
}

static const JSCFunctionListEntry js_float_env_funcs[] = {
    JS_CGETSET_DEF("prec", js_float_env_get_prec, NULL ),
    JS_CGETSET_DEF("expBits", js_float_env_get_expBits, NULL ),
    JS_CFUNC_DEF("setPrec", 2, js_float_env_setPrec ),
    JS_PROP_INT32_DEF("RNDN", BF_RNDN, 0 ),
    JS_PROP_INT32_DEF("RNDZ", BF_RNDZ, 0 ),
    JS_PROP_INT32_DEF("RNDU", BF_RNDU, 0 ),
    JS_PROP_INT32_DEF("RNDD", BF_RNDD, 0 ),
    JS_PROP_INT32_DEF("RNDNA", BF_RNDNA, 0 ),
    JS_PROP_INT32_DEF("RNDA", BF_RNDA, 0 ),
    JS_PROP_INT32_DEF("RNDF", BF_RNDF, 0 ),
    JS_PROP_INT32_DEF("precMin", BF_PREC_MIN, 0 ),
    JS_PROP_INT64_DEF("precMax", BF_PREC_MAX, 0 ),
    JS_PROP_INT32_DEF("expBitsMin", BF_EXP_BITS_MIN, 0 ),
    JS_PROP_INT32_DEF("expBitsMax", BF_EXP_BITS_MAX, 0 ),
};

static const JSCFunctionListEntry js_float_env_proto_funcs[] = {
    JS_CGETSET_MAGIC_DEF("prec", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, FE_PREC ),
    JS_CGETSET_MAGIC_DEF("expBits", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, FE_EXP ),
    JS_CGETSET_MAGIC_DEF("rndMode", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, FE_RNDMODE ),
    JS_CGETSET_MAGIC_DEF("subnormal", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, FE_SUBNORMAL ),
    JS_CGETSET_MAGIC_DEF("invalidOperation", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, BF_ST_INVALID_OP ),
    JS_CGETSET_MAGIC_DEF("divideByZero", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, BF_ST_DIVIDE_ZERO ),
    JS_CGETSET_MAGIC_DEF("overflow", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, BF_ST_OVERFLOW ),
    JS_CGETSET_MAGIC_DEF("underflow", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, BF_ST_UNDERFLOW ),
    JS_CGETSET_MAGIC_DEF("inexact", js_float_env_proto_get_status,
                         js_float_env_proto_set_status, BF_ST_INEXACT ),
    JS_CFUNC_DEF("clearStatus", 0, js_float_env_clearStatus ),
};

void JS_AddIntrinsicBigFloat(JSContext *ctx)
{
    JSRuntime *rt = ctx->rt;
    JSValueConst obj1;
    
    rt->bigfloat_ops.to_string = js_bigfloat_to_string;
    rt->bigfloat_ops.from_string = js_string_to_bigfloat;
    rt->bigfloat_ops.unary_arith = js_unary_arith_bigfloat;
    rt->bigfloat_ops.binary_arith = js_binary_arith_bigfloat;
    rt->bigfloat_ops.compare = js_compare_bigfloat;
    rt->bigfloat_ops.mul_pow10_to_float64 = js_mul_pow10_to_float64;
    rt->bigfloat_ops.mul_pow10 = js_mul_pow10;
    
    ctx->class_proto[JS_CLASS_BIG_FLOAT] = JS_NewObject(ctx);
    JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_BIG_FLOAT],
                               js_bigfloat_proto_funcs,
                               countof(js_bigfloat_proto_funcs));
    obj1 = JS_NewGlobalCConstructor(ctx, "BigFloat", js_bigfloat_constructor, 1,
                                    ctx->class_proto[JS_CLASS_BIG_FLOAT]);
    JS_SetPropertyFunctionList(ctx, obj1, js_bigfloat_funcs,
                               countof(js_bigfloat_funcs));

    ctx->class_proto[JS_CLASS_FLOAT_ENV] = JS_NewObject(ctx);
    JS_SetPropertyFunctionList(ctx, ctx->class_proto[JS_CLASS_FLOAT_ENV],
                               js_float_env_proto_funcs,
                               countof(js_float_env_proto_funcs));
    obj1 = JS_NewGlobalCConstructorOnly(ctx, "BigFloatEnv",
                                        js_float_env_constructor, 1,
                                        ctx->class_proto[JS_CLASS_FLOAT_ENV]);
    JS_SetPropertyFunctionList(ctx, obj1, js_float_env_funcs,
                               countof(js_float_env_funcs));
}

/* BigDecimal */

static JSValue JS_ToBigDecimalFree(JSContext *ctx, JSValue val,
                                   BOOL allow_null_or_undefined)
{
 redo:
    switch(JS_VALUE_GET_NORM_TAG(val)) {
    case JS_TAG_BIG_DECIMAL:
        break;
    case JS_TAG_NULL:
        if (!allow_null_or_undefined)
            goto fail;
        /* fall thru */
    case JS_TAG_BOOL:
    case JS_TAG_INT:
        {
            bfdec_t *r;
            int32_t v = JS_VALUE_GET_INT(val);

            val = JS_NewBigDecimal(ctx);
            if (JS_IsException(val))
                break;
            r = JS_GetBigDecimal(val);
            if (bfdec_set_si(r, v)) {
                JS_FreeValue(ctx, val);
                val = JS_EXCEPTION;
                break;
            }
        }
        break;
    case JS_TAG_FLOAT64:
    case JS_TAG_BIG_INT:
    case JS_TAG_BIG_FLOAT:
        val = JS_ToStringFree(ctx, val);
        if (JS_IsException(val))
            break;
        goto redo;
    case JS_TAG_STRING:
        {
            const char *str, *p;
            size_t len;
            int err;

            str = JS_ToCStringLen(ctx, &len, val);
            JS_FreeValue(ctx, val);
            if (!str)
                return JS_EXCEPTION;
            p = str;
            p += skip_spaces(p);
            if ((p - str) == len) {
                bfdec_t *r;
                val = JS_NewBigDecimal(ctx);
                if (JS_IsException(val))
                    break;
                r = JS_GetBigDecimal(val);
                bfdec_set_zero(r, 0);
                err = 0;
            } else {
                val = js_atof(ctx, p, &p, 0, ATOD_TYPE_BIG_DECIMAL);
                if (JS_IsException(val)) {
                    JS_FreeCString(ctx, str);
                    return JS_EXCEPTION;
                }
                p += skip_spaces(p);
                err = ((p - str) != len);
            }
            JS_FreeCString(ctx, str);
            if (err) {
                JS_FreeValue(ctx, val);
                return JS_ThrowSyntaxError(ctx, "invalid bigdecimal literal");
            }
        }
        break;
    case JS_TAG_OBJECT:
        val = JS_ToPrimitiveFree(ctx, val, HINT_NUMBER);
        if (JS_IsException(val))
            break;
        goto redo;
    case JS_TAG_UNDEFINED:
        {
            bfdec_t *r;
            if (!allow_null_or_undefined)
                goto fail;
            val = JS_NewBigDecimal(ctx);
            if (JS_IsException(val))
                break;
            r = JS_GetBigDecimal(val);
            bfdec_set_nan(r);
        }
        break;
    default:
    fail:
        JS_FreeValue(ctx, val);
        return JS_ThrowTypeError(ctx, "cannot convert to bigdecimal");
    }
    return val;
}

static JSValue js_bigdecimal_constructor(JSContext *ctx,
                                         JSValueConst new_target,
                                         int argc, JSValueConst *argv)
{
    JSValue val;
    if (!JS_IsUndefined(new_target))
        return JS_ThrowTypeError(ctx, "not a constructor");
    if (argc == 0) {
        bfdec_t *r;
        val = JS_NewBigDecimal(ctx);
        if (JS_IsException(val))
            return val;
        r = JS_GetBigDecimal(val);
        bfdec_set_zero(r, 0);
    } else {
        val = JS_ToBigDecimalFree(ctx, JS_DupValue(ctx, argv[0]), FALSE);
    }
    return val;
}

static JSValue js_thisBigDecimalValue(JSContext *ctx, JSValueConst this_val)
{
    if (JS_IsBigDecimal(this_val))
        return JS_DupValue(ctx, this_val);

    if (JS_VALUE_GET_TAG(this_val) == JS_TAG_OBJECT) {
        JSObject *p = JS_VALUE_GET_OBJ(this_val);
        if (p->class_id == JS_CLASS_BIG_DECIMAL) {
            if (JS_IsBigDecimal(p->u.object_data))
                return JS_DupValue(ctx, p->u.object_data);
        }
    }
    return JS_ThrowTypeError(ctx, "not a bigdecimal");
}

static JSValue js_bigdecimal_toString(JSContext *ctx, JSValueConst this_val,
                                      int argc, JSValueConst *argv)
{
    JSValue val;

    val = js_thisBigDecimalValue(ctx, this_val);
    if (JS_IsException(val))
        return val;
    return JS_ToStringFree(ctx, val);
}

static JSValue js_bigdecimal_valueOf(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv)
{
    return js_thisBigDecimalValue(ctx, this_val);
}

static int js_bigdecimal_get_rnd_mode(JSContext *ctx, JSValueConst obj)
{
    const char *str;
    size_t size;
    int rnd_mode;
    
    str = JS_ToCStringLen(ctx, &size, obj);
    if (!str)
        return -1;
    if (strlen(str) != size)
        goto invalid_rounding_mode;
    if (!strcmp(str, "floor")) {
        rnd_mode = BF_RNDD;
    } else if (!strcmp(str, "ceiling")) {
        rnd_mode = BF_RNDU;
    } else if (!strcmp(str, "down")) {
        rnd_mode = BF_RNDZ;
    } else if (!strcmp(str, "up")) {
        rnd_mode = BF_RNDA;
    } else if (!strcmp(str, "half-even")) {
        rnd_mode = BF_RNDN;
    } else if (!strcmp(str, "half-up")) {
        rnd_mode = BF_RNDNA;
    } else {
    invalid_rounding_mode:
        JS_FreeCString(ctx, str);
        JS_ThrowTypeError(ctx, "invalid rounding mode");
        return -1;
    }
    JS_FreeCString(ctx, str);
    return rnd_mode;
}

typedef struct {
    int64_t prec;
    bf_flags_t flags;
} BigDecimalEnv;

static int js_bigdecimal_get_env(JSContext *ctx, BigDecimalEnv *fe,
                                 JSValueConst obj)
{
    JSValue prop;
    int64_t val;
    BOOL has_prec;
    int rnd_mode;
    
    if (!JS_IsObject(obj)) {
        JS_ThrowTypeErrorNotAnObject(ctx);
        return -1;
    }
    prop = JS_G