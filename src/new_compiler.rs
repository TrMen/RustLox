struct FunctionCompiler {
    chunk: Chunk,
    args: CliArgs,
    scope_information: ScopeInformation<'src>,
}

impl FunctionCompiler {
    fn emit_op(&mut self, op: OpCodeWithoutArg) {
        let line = self.parser.previous.line;
        self.chunk.append_op(op.into(), line);
    }

    fn emit_ops(&mut self, first: OpCodeWithoutArg, second: OpCodeWithoutArg) {
        self.emit_op(first);
        self.emit_op(second);
    }

    fn emit_op_with_arg(&mut self, op: OpCodeWithArg, arg: TwoByteArg) {
        let line = self.parser.previous.line;
        self.chunk.append_op(op.into(), line);

        let line = self.parser.previous.line;
        self.chunk.append_arg(arg, line);
    }

    fn emit_constant(&mut self, value: Value) {
        let constant_index = match self.chunk.add_constant(value) {
            Ok(constant_index) => constant_index,
            Err(err_message) => {
                self.parser.report_error_at_previous(err_message);
                u16::MAX // Not sure this is a good idea, but should be fine since I reported error
            }
        };

        self.emit_op_with_arg(OpCodeWithArg::Constant, constant_index);
    }

    fn emit_jmp_backwards(&mut self, loop_start: CodeIndex) {
        // emit_jmp_with_placeholder and backpatch_jmp combined, because the offset is swapped
        // and we don't need to backpatch.

        // +3 because op itself wasn't emitted yet, unlike in backpatch_jmp
        let distance_backwards = self.chunk.code_bytes_len() - loop_start + 3;

        if let Ok(distance) = distance_backwards.try_into() {
            self.emit_op_with_arg(OpCodeWithArg::JumpBackward, distance);
        } else {
            self.parser
                .report_error_at_previous("Too much code to jump over.");
        }
    }

    #[must_use]
    fn emit_jmp_with_placeholder(&mut self, op: OpCodeWithArg) -> CodeIndex {
        self.emit_op_with_arg(op, JumpOffset::MAX);

        self.chunk.code_bytes_len() - 2
    }

    fn backpatch_jmp(&mut self, jmp_instr_index: CodeIndex) {
        let jump_distance = self.chunk.code_bytes_len() - (jmp_instr_index + 2);

        if let Ok(distance) = jump_distance.try_into() {
            self.chunk
                .change_arg_at_code_index(jmp_instr_index as usize, distance);
        } else {
            self.parser
                .report_error_at_previous("Too much code to jump over.");
        }
    }

    fn declare_local_variable(&mut self, modifiers: VariableModifiers) {
        let identifier_token = &self.parser.previous;

        if let Err(e) = self
            .scope_information
            .add_local(identifier_token.clone(), modifiers)
        {
            // TODO: Is this enough error handling? This likely has false-positive follow up errors if
            // we check that locals are actually defined at comptime. But this is unlikely to be a big deal.
            self.parser.report_error_at_previous(&e.msg);
        }
    }

    fn define_variable(&mut self, identifier: &'src str, global: ConstantIndex) {
        if !self.scope_information.is_global() {
            self.scope_information.mark_last_local_initialized();

            // There is no code to define local variables at runtime, the VM already
            // executed the initializer and the result is a temporary at the top of stack stack.
            // So the temp simply becomes the local variable, that's it. Scoping is implicit.
            return;
        }

        let global_usage = UsedGlobal::Defined(identifier);

        if let Some(UsedGlobal::Defined(_)) = self.accessed_globals.get(&global_usage) {
            self.parser.report_error_at_previous(&format!(
                "Redefinition of global variable '{identifier}'"
            ));
        } else {
            // TODO: Super hacky because of bad Eq impl that compares Defined(iden) == Accessed(iden)

            let existed = !self.accessed_globals.insert(global_usage.clone());

            if existed {
                self.accessed_globals.remove(&global_usage);
                self.accessed_globals.insert(global_usage);
            }

            self.emit_op_with_arg(OpCodeWithArg::DefineGlobal, global);
        }
    }
}
