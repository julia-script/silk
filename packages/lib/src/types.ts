export type AstNode = {
  "start_token": number,
  "end_token": number,
  "data": {
      "root": {
        "list": Array<number>
      }      
    } | {
      "add": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "div": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "mod": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "mul": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "pow": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "sub": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "eq": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "gt": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "gte": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "lt": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "lte": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "ne": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "and": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "or": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "xor": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "prop_access": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "band": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "bor": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "bshl": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "bshr": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "bxor": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "bnot": {
        "node": number
      }      
    } | {
      "neg": {
        "node": number
      }      
    } | {
      "not": {
        "node": number
      }      
    } | {
      "decrement": {
        "node": number
      }      
    } | {
      "increment": {
        "node": number
      }      
    } | {
      "export": {
        "node": number
      }      
    } | {
      "extern": {
        "node": number
      }      
    } | {
      "pub": {
        "node": number
      }      
    } | {
      "assign": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "ty_assign": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "char_literal": {
        "token": number
      }      
    } | {
      "false_literal": {
        "token": number
      }      
    } | {
      "hex_literal": {
        "token": number
      }      
    } | {
      "identifier": {
        "token": number
      }      
    } | {
      "number_literal": {
        "token": number
      }      
    } | {
      "string_literal": {
        "token": number
      }      
    } | {
      "true_literal": {
        "token": number
      }      
    } | {
      "ty_boolean": {
        "token": number
      }      
    } | {
      "ty_number": {
        "token": number
      }      
    } | {
      "ty_option": {
        "token": number
      }      
    } | {
      "ty_string": {
        "token": number
      }      
    } | {
      "ty_void": {
        "token": number
      }      
    } | {
      "ty_generic": {
        "name": number,
        "args_list": Array<number>
      }      
    } | {
      "const_decl": {
        "name": number,
        "type": number,
        "value": number
      }      
    } | {
      "var_decl": {
        "name": number,
        "type": number,
        "value": number
      }      
    } | {
      "fn_decl": {
        "proto": number,
        "body": number
      }      
    } | {
      "fn_proto": {
        "name": number,
        "params_list": Array<number>,
        "ret_type": number
      }      
    } | {
      "fn_param": {
        "name": number,
        "ty": number
      }      
    } | {
      "fn_call": {
        "callee": number,
        "args_list": Array<number>
      }      
    } | {
      "expr": {
        "list": Array<number>
      }      
    } | {
      "group": {
        "node": number
      }      
    } | {
      "if_expr": {
        "condition": number,
        "then_branch": number,
        "else_branch": number
      }      
    } | {
      "ret_expression": {
        "node": number
      }      
    } | {
      "block": {
        "list": Array<number>
      }      
    } | {
      "while_loop": {
        "condition": number,
        "body": number
      }      
    } | {
      "ty_i32": {
        "token": number
      }      
    } | {
      "ty_i64": {
        "token": number
      }      
    } | {
      "ty_f32": {
        "token": number
      }      
    } | {
      "ty_f64": {
        "token": number
      }      
    } | {
      "opt": null | {
            "ty": null | number
          }      
    }
};

export type HirInstruction = {
  "fn_decl": {
    "name_node": number,
    "return_type": number,
    "params": number,
    "init": null | number
  }  
} | {
  "mod_decl": {
    "name_node": null | number,
    "declarations_list": Array<number>
  }  
} | {
  "param_decl": {
    "name_node": number,
    "ty": number
  }  
} | {
  "param_get": {
    "operand": number
  }  
} | {
  "global_get": {
    "operand": number
  }  
} | {
  "global_decl": {
    "name_node": number,
    "extern": boolean,
    "is_fn": boolean,
    "visibility": Visibility,
    "type": null | number,
    "exported": boolean,
    "mutable": boolean,
    "init": number
  }  
} | {
  "local_get": {
    "operand": number
  }  
} | {
  "block": {
    "name_node": null | number,
    "instructions_list": Array<number>
  }  
} | {
  "inline_block": {
    "name_node": null | number,
    "instructions_list": Array<number>
  }  
} | {
  "local": {
    "name_node": number
  }  
} | {
  "local_set": {
    "lhs": number,
    "rhs": number
  }  
} | {
  "decl_ref": number  
} | {
  "loop": {
    "body": number
  }  
} | {
  "assign": {
    "lhs": number,
    "rhs": number
  }  
} | {
  "comptime_number": number  
} | {
  "ty_number": number  
} | {
  "ty_boolean": number  
} | {
  "undefined_value": null | number  
} | {
  "as": {
    "lhs": number,
    "rhs": number
  }  
} | {
  "add": {
    "lhs": number,
    "rhs": number
  }  
} | {
  "sub": {
    "lhs": number,
    "rhs": number
  }  
} | {
  "mul": {
    "lhs": number,
    "rhs": number
  }  
} | {
  "div": {
    "lhs": number,
    "rhs": number
  }  
} | {
  "gt": {
    "lhs": number,
    "rhs": number
  }  
} | {
  "lt": {
    "lhs": number,
    "rhs": number
  }  
} | {
  "gte": {
    "lhs": number,
    "rhs": number
  }  
} | {
  "lte": {
    "lhs": number,
    "rhs": number
  }  
} | {
  "eq": {
    "lhs": number,
    "rhs": number
  }  
} | {
  "neq": {
    "lhs": number,
    "rhs": number
  }  
} | {
  "typeof": {
    "operand": number
  }  
} | {
  "ret": {
    "operand": number
  }  
} | {
  "if_expr": {
    "cond": number,
    "then_body": number,
    "else_body": null | number
  }  
} | {
  "br": {
    "operand": number
  }  
} | {
  "ty_i32": number  
} | {
  "ty_i64": number  
} | {
  "ty_f32": number  
} | {
  "ty_f64": number  
} | {
  "debug_var": {
    "name_node": number,
    "instruction": number
  }  
};

export type Visibility = "public" | "private";
export type MirInstruction = {
  "op": MirInstructionOp,
  "data": {
      "binOp": {
        "lhs": number,
        "rhs": number
      }      
    } | {
      "instruction": number      
    } | {
      "value": MirValueIndex      
    } | {
      "type": MirTypeIndex      
    } | {
      "void": Object      
    } | {
      "if_expr": {
        "cond": number,
        "then_body": MirTypeIndex,
        "else_body": null | MirTypeIndex
      }      
    } | {
      "scoped": {
        "name": string,
        "index": number
      }      
    } | {
      "local": {
        "name": string,
        "type": MirTypeIndex,
        "index": number
      }      
    },
  "type": MirTypeIndex,
  "value": MirValueIndex,
  "liveness": number
};

export type MirInstructionOp = "type" | "constant" | "param" | "param_get" | "param_set" | "local" | "local_get" | "local_set" | "global_get" | "global_set" | "ret" | "as" | "add" | "sub" | "mul" | "div" | "gt" | "lt" | "eq" | "neq" | "block" | "if_expr" | "loop" | "br";
export type MirValueIndex = "runtime" | "true" | "false" | "none" | "undefined" | "type_number" | "type_f32" | "type_f64" | "type_i32" | "type_i64" | "type_boolean" | number;
export type MirTypeIndex = "unknown" | "param" | "boolean" | "number" | "string" | "void" | "i32" | "i64" | "f32" | "f64" | "type" | "type_number" | "type_string" | "type_boolean" | "type_void" | "type_i32" | "type_i64" | "type_f32" | "type_f64" | number;
export type MirType = {
  "fn": {
    "name": string,
    "params": number,
    "return_type": MirTypeIndex,
    "body": null | MirTypeIndex
  }  
} | {
  "while_loop": {
    "condition": number,
    "body": MirTypeIndex
  }  
} | {
  "optional": {
    "child": MirTypeIndex
  }  
} | {
  "module": {
    "decls": number
  }  
} | {
  "global": {
    "name": string,
    "type": MirTypeIndex,
    "init": null | MirValueIndex
  }  
} | {
  "param": {
    "name": string,
    "type": MirTypeIndex
  }  
} | {
  "block": {
    "locals": number,
    "name": null | string,
    "instructions": number
  }  
};

export type MirValue = {
  "type": MirTypeIndex  
} | {
  "fn": {
    "type": MirTypeIndex,
    "instructions": number
  }  
} | {
  "float": number  
};

