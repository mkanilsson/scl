# scl (Shitty C-like language)

A simple c like language that is pretty shit. Compiles to QBE.

> This is a for-fun project for me to learn some compile stuff

## Syntax

Both existing and planned

Extern procs:
```scl
extern proc printf(string, ...) u32
```

Unions and procs:
```scl
union Ast {
    Number(string),
    Ident(name: string),
    BinOp(lhs: *Ast, op: Op, rhs: *Ast),
    Bool(bool),
}

proc print_ast(ast: Ast) Ast {
    switch (ast) {
        Number |value| => {
            printf("%s\n", value);
        }, 
        Ident |name| => {
            printf("%s!\n", name);
        }
        BinOp |binop| => {
            printf("%s!\n", binop.op.as_string());
        }
        else => {
            printf("Some other thing");
        }
    }

    ret ::Ident("foo");
}
```

Defer:
```scl
defer file.close();
```

Structs:
```scl
struct Vec {
    x: u32,
    y: u32,
}

proc vec_new() Vec {
    ret Vec {
        x: 50,
        y: 60,
    };
}
```

Use:
```scl
use std::io::printf 
```
