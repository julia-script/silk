const Scope = @import("./Scope.zig");
const Sema = @import("../../../sema/Sema.zig");

pub fn emit(scope: *Scope, index: Sema.Instruction.Index) !void {
    const block_instruction = scope.getInstruction(index);
    const instruction_list = scope.program.sema.getList(block_instruction.data.block.instructions_list);

    for (instruction_list) |instruction_index| {
        // const instruction = scope.getInstruction(instruction_index);
        // if (instruction.liveness == 0) continue;
        try scope.emit(instruction_index);
    }
}
