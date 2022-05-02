#[derive(PartialEq, Clone, Copy, Debug)]
pub enum VariableModifiers {
    SingleAssignment,
    Mutable,
    // Can add other modifiers here
}

// TODO: I don't like that I store the whole token by value so much. But it's prolly fine
#[derive(Debug)]
struct Local<'src> {
    token: Token<'src>,
    depth: usize,
    modifiers: VariableModifiers,
}

struct ScopeInformation<'src> {
    // Locals are appended to the array, so the locals with deepest scope
    // are always at the end
    locals: Vec<Local<'src>>,
    scope_depth: usize,
}

impl<'src> ScopeInformation<'src> {
    const MAX_DEPTH: usize = usize::MAX / 2;
    const UNINITIALIZED_DEPTH: usize = Self::MAX_DEPTH + 1;
    const MAX_LOCAL_COUNT: usize = LocalIndex::MAX as usize + 1;

    fn add_local(
        &mut self,
        token: Token<'src>,
        modifiers: VariableModifiers,
    ) -> Result<(), CompiletimeError> {
        // Note: UNDEFINED_DEPTH must be greater than MAX_DEPTH

        if self.locals.len() >= Self::MAX_LOCAL_COUNT {
            return Err(CompiletimeError {
                msg: format!(
                    "Too many local variables defined in scope. Max {}.",
                    Self::MAX_LOCAL_COUNT
                ),
            });
        }

        if self
            .locals_at_current_depth()
            .any(|local| local.token.lexeme == token.lexeme)
        {
            return Err(CompiletimeError {
                msg: format!("Redefinition of local variable '{}'", token.lexeme),
            });
        }

        self.locals.push(Local {
            token,
            // To prevent self-referential definition, split definition into two phases
            // and initialize depth at the end of the defintion
            depth: Self::UNINITIALIZED_DEPTH,
            modifiers,
        });

        Ok(())
    }

    fn pop_scope(&mut self) -> usize {
        self.scope_depth -= 1;

        let popped_locals = self
            .locals
            .iter()
            .rev()
            .take_while(|local| local.depth > self.scope_depth)
            .count();

        // TODO: Indices correct?
        self.locals.truncate(self.locals.len() - popped_locals);

        popped_locals
    }

    fn resolve_local(
        &self,
        identifier: &'src str,
        parser: &mut Parser<'src>, // TODO: This shouldn't be needed, just properly propagate errors
    ) -> Option<(LocalIndex, VariableModifiers)> {
        self.locals
            .iter()
            .rev()
            .position(|local| {
                let is_equal = local.token.lexeme == identifier;
                if is_equal && local.depth == Self::UNINITIALIZED_DEPTH {
                    parser.report_error_at_previous(
                        "Can't read local variable in it's own initializer.",
                    );
                }
                is_equal
            })
            .map(|pos| (pos as LocalIndex, self.get_local(pos).modifiers))
        // Save because max locals limit is enforced in add_local
    }

    fn locals_at_current_depth(&self) -> impl Iterator<Item = &Local<'src>> {
        self.locals
            .iter()
            .rev()
            // Take all in undefined or deeper scope
            .take_while(move |local| local.depth >= self.scope_depth) // >= to capture UNDEFINED_DEPTH
    }

    fn get_local(&self, index: usize) -> &Local<'src> {
        &self.locals[self.locals.len() - (index + 1)]
    }

    fn is_global(&self) -> bool {
        self.scope_depth == 0
    }

    fn mark_last_local_initialized(&mut self) {
        self.locals.last_mut().unwrap().depth = self.scope_depth;
    }

    fn is_local_initialized(&self, index: LocalIndex) -> bool {
        self.get_local(index as usize).depth != Self::UNINITIALIZED_DEPTH
    }

    fn begin_scope(&mut self) -> MustCloseScope {
        if self.scope_information.scope_depth >= ScopeInformation::MAX_DEPTH {
            self.parser
                .report_error_at_previous("Too many nested scopes.");
            return MustCloseScope {};
        }

        self.scope_information.scope_depth += 1;

        MustCloseScope {}
    }

    fn end_scope(&mut self, _: MustCloseScope) {
        for _ in 0..self.scope_information.pop_scope() {
            // Pop value from the stack at runtime
            self.emit_op(OpCodeWithoutArg::Pop);
        }
    }
}
