# SubC

SubC is a minimal C-like language that compiles a restricted subset of C to NASM (i386) assembly code. It is designed for educational purposes and simple code generation, focusing on clarity and simplicity.

## Features
- Only global integer variables: `int Name;` (no initializers)
- Only standalone functions: `void Name() { ... }`
- No function parameters or local variables
- Only `return;` (no return values)
- Supported statements: blocks, `if`/`else`, `while`, expression statements, function call statements
- Supported expressions: integer literals, global identifiers, assignment, arithmetic (`+`, `-`, `*`, `/`, `%`), comparisons, parentheses, unary operators (`+`, `-`, `!`)
- Function calls are statements only: `Foo();` (calls are void)

## NASM Output
- Emits a `bits 32` header
- Globals are emitted first, aligned to 4 bytes, as `Name dd 0`
- Function label and body follow
- Instruction lines are indented by 2 spaces
- No `global` directive is emitted (intended for flat binary NASM integration)

## Usage
- The program reads a `.subc` source file and writes NASM assembly to standard output.
- .\x64\Debug\SubC.exe Test.subc > Test.asm
- Very simple example for Test.subc:
```
int FortyTwo; 
void TestIt()
{
  FortyTwo = 42; 
  return;
}
```
- Output:
```
bits 32

align 4
FortyTwo dd 0

TestIt:
  mov   eax, 42
  mov   [FortyTwo], eax
  ret
```

## Building
Open the solution in Visual Studio and build the project. The main source file is `SubC.c`.

## License
This project is licensed under the UNLICENSE. See the LICENSE file for details.