/* subc.c
*
* SubC -> NASM (i386) snippet generator
*
* SubC rules (as implemented here):
* - Only global variables:   int Name;
*   - No initializers. Always emits: Name: dd 0
* - Only standalone functions: void Name() { ... }
* - No parameters. No local variables.
* - return; only (no return value)
* - Statements: blocks, if/else, while, expression statements, function call statements
* - Expressions: integer literals, global identifiers, assignment, + - * / %, comparisons,
*                parentheses, unary + - !
* - Function calls are statements only: Foo();  (calls are void)
*
* NASM output:
* - "bits 32" header
* - Globals first (align 4, dd 0)
* - Function label and body
* - Instruction lines are indented 2 spaces
* - No "global" directive (you are integrating into a flat binary build with NASM-only)
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* ----------------------------- Utilities ----------------------------- */

static void Fail(const char *Message)
{
  fprintf(stderr, "Error: %s\n", Message);
  exit(1);
}

static void *MallocOrDie(size_t Size)
{
  void *Ptr = malloc(Size);
  if (!Ptr)
  {
    Fail("Out of memory.");
  }
  return Ptr;
}

static char *StrDupOrDie(const char *Text)
{
  size_t Len = strlen(Text);
  char *Out = (char *)MallocOrDie(Len + 1);
  memcpy(Out, Text, Len + 1);
  return Out;
}

static char *ReadEntireFile(const char *Path)
{
  FILE *File = fopen(Path, "rb");
  long Size;
  char *Buffer;

  if (!File)
  {
    fprintf(stderr, "Error: Could not open file: %s\n", Path);
    exit(1);
  }

  if (fseek(File, 0, SEEK_END) != 0)
  {
    fclose(File);
    Fail("Could not seek to end of file.");
  }

  Size = ftell(File);
  if (Size < 0)
  {
    fclose(File);
    Fail("Could not determine file size.");
  }

  if (fseek(File, 0, SEEK_SET) != 0)
  {
    fclose(File);
    Fail("Could not seek to start of file.");
  }

  Buffer = (char *)MallocOrDie((size_t)Size + 1);
  if (fread(Buffer, 1, (size_t)Size, File) != (size_t)Size)
  {
    fclose(File);
    Fail("Could not read file.");
  }
  Buffer[Size] = '\0';

  fclose(File);
  return Buffer;
}

/* ----------------------------- Tokenizer ----------------------------- */

typedef enum TokenKind
{
  TokenEnd = 0,

  TokenIdentifier,
  TokenNumber,

  /* Keywords */
  TokenInt,
  TokenVoid,
  TokenIf,
  TokenElse,
  TokenWhile,
  TokenReturn,

  /* Punctuation / operators */
  TokenLParen,    /* ( */
  TokenRParen,    /* ) */
  TokenLBrace,    /* { */
  TokenRBrace,    /* } */
  TokenSemicolon, /* ; */

  TokenAssign,    /* = */
  TokenPlus,      /* + */
  TokenMinus,     /* - */
  TokenStar,      /* * */
  TokenSlash,     /* / */
  TokenPercent,   /* % */

  TokenEqEq,      /* == */
  TokenNotEq,     /* != */
  TokenLt,        /* < */
  TokenLtEq,      /* <= */
  TokenGt,        /* > */
  TokenGtEq,      /* >= */

  TokenBang       /* ! */
} TokenKind;

typedef struct Token
{
  TokenKind Kind;
  const char *Start; /* pointer into Source */
  int Length;

  long NumberValue;  /* for TokenNumber */

  int Line;
  int Column;
} Token;

/* Global lexer state (simple and readable) */
static const char *SourceText = NULL;
static const char *Cursor = NULL;
static int CurrentLine = 1;
static int CurrentColumn = 1;

static Token CurrentToken;
static Token LookaheadToken;

static int IsIdentifierStart(int C)
{
  return (isalpha(C) || C == '_');
}

static int IsIdentifierPart(int C)
{
  return (isalnum(C) || C == '_');
}

static void AdvanceChar(void)
{
  if (*Cursor == '\n')
  {
    Cursor++;
    CurrentLine++;
    CurrentColumn = 1;
    return;
  }

  if (*Cursor != '\0')
  {
    Cursor++;
    CurrentColumn++;
  }
}

static void SkipWhitespaceAndComments(void)
{
  for (;;)
  {
    /* Skip whitespace */
    while (*Cursor == ' ' || *Cursor == '\t' || *Cursor == '\r' || *Cursor == '\n')
    {
      AdvanceChar();
    }

    /* Skip // comments */
    if (Cursor[0] == '/' && Cursor[1] == '/')
    {
      while (*Cursor != '\0' && *Cursor != '\n')
      {
        AdvanceChar();
      }
      continue;
    }

    /* Skip /* comments */
    if (Cursor[0] == '/' && Cursor[1] == '*')
    {
      AdvanceChar(); /* / */
      AdvanceChar(); /* * */
      while (*Cursor != '\0')
      {
        if (Cursor[0] == '*' && Cursor[1] == '/')
        {
          AdvanceChar(); /* * */
          AdvanceChar(); /* / */
          break;
        }
        AdvanceChar();
      }
      continue;
    }

    break;
  }
}

static Token MakeToken(TokenKind Kind, const char *Start, int Length, int Line, int Column)
{
  Token T;
  T.Kind = Kind;
  T.Start = Start;
  T.Length = Length;
  T.NumberValue = 0;
  T.Line = Line;
  T.Column = Column;
  return T;
}

static int TokenTextEquals(const Token *T, const char *Text)
{
  int Len = (int)strlen(Text);
  if (T->Length != Len)
  {
    return 0;
  }
  return (memcmp(T->Start, Text, (size_t)Len) == 0);
}

static Token LexNextToken(void)
{
  Token T;
  const char *Start;
  int Line;
  int Column;

  SkipWhitespaceAndComments();

  Start = Cursor;
  Line = CurrentLine;
  Column = CurrentColumn;

  if (*Cursor == '\0')
  {
    return MakeToken(TokenEnd, Cursor, 0, Line, Column);
  }

  /* Identifiers / keywords */
  if (IsIdentifierStart((unsigned char)*Cursor))
  {
    AdvanceChar();
    while (IsIdentifierPart((unsigned char)*Cursor))
    {
      AdvanceChar();
    }

    T = MakeToken(TokenIdentifier, Start, (int)(Cursor - Start), Line, Column);

    if (TokenTextEquals(&T, "int"))   T.Kind = TokenInt;
    if (TokenTextEquals(&T, "void"))  T.Kind = TokenVoid;
    if (TokenTextEquals(&T, "if"))    T.Kind = TokenIf;
    if (TokenTextEquals(&T, "else"))  T.Kind = TokenElse;
    if (TokenTextEquals(&T, "while")) T.Kind = TokenWhile;
    if (TokenTextEquals(&T, "return"))T.Kind = TokenReturn;

    return T;
  }

  /* Numbers (decimal only) */
  if (isdigit((unsigned char)*Cursor))
  {
    long Value = 0;

    while (isdigit((unsigned char)*Cursor))
    {
      int Digit = (*Cursor - '0');
      Value = Value * 10 + Digit;
      AdvanceChar();
    }

    T = MakeToken(TokenNumber, Start, (int)(Cursor - Start), Line, Column);
    T.NumberValue = Value;
    return T;
  }

  /* Two-character operators */
  if (Cursor[0] == '=' && Cursor[1] == '=')
  {
    AdvanceChar(); AdvanceChar();
    return MakeToken(TokenEqEq, Start, 2, Line, Column);
  }
  if (Cursor[0] == '!' && Cursor[1] == '=')
  {
    AdvanceChar(); AdvanceChar();
    return MakeToken(TokenNotEq, Start, 2, Line, Column);
  }
  if (Cursor[0] == '<' && Cursor[1] == '=')
  {
    AdvanceChar(); AdvanceChar();
    return MakeToken(TokenLtEq, Start, 2, Line, Column);
  }
  if (Cursor[0] == '>' && Cursor[1] == '=')
  {
    AdvanceChar(); AdvanceChar();
    return MakeToken(TokenGtEq, Start, 2, Line, Column);
  }

  /* Single-character tokens */
  switch (*Cursor)
  {
  case '(': AdvanceChar(); return MakeToken(TokenLParen, Start, 1, Line, Column);
  case ')': AdvanceChar(); return MakeToken(TokenRParen, Start, 1, Line, Column);
  case '{': AdvanceChar(); return MakeToken(TokenLBrace, Start, 1, Line, Column);
  case '}': AdvanceChar(); return MakeToken(TokenRBrace, Start, 1, Line, Column);
  case ';': AdvanceChar(); return MakeToken(TokenSemicolon, Start, 1, Line, Column);

  case '=': AdvanceChar(); return MakeToken(TokenAssign, Start, 1, Line, Column);
  case '+': AdvanceChar(); return MakeToken(TokenPlus, Start, 1, Line, Column);
  case '-': AdvanceChar(); return MakeToken(TokenMinus, Start, 1, Line, Column);
  case '*': AdvanceChar(); return MakeToken(TokenStar, Start, 1, Line, Column);
  case '/': AdvanceChar(); return MakeToken(TokenSlash, Start, 1, Line, Column);
  case '%': AdvanceChar(); return MakeToken(TokenPercent, Start, 1, Line, Column);

  case '<': AdvanceChar(); return MakeToken(TokenLt, Start, 1, Line, Column);
  case '>': AdvanceChar(); return MakeToken(TokenGt, Start, 1, Line, Column);
  case '!': AdvanceChar(); return MakeToken(TokenBang, Start, 1, Line, Column);
  default:
    break;
  }

  {
    char Buf[128];
    snprintf(Buf, sizeof(Buf), "Unexpected character '%c' at %d:%d.", *Cursor, Line, Column);
    Fail(Buf);
  }

  return MakeToken(TokenEnd, Cursor, 0, Line, Column);
}

static void AdvanceToken(void)
{
  CurrentToken = LookaheadToken;
  LookaheadToken = LexNextToken();
}

static void InitLexer(const char *Text)
{
  SourceText = Text;
  Cursor = SourceText;
  CurrentLine = 1;
  CurrentColumn = 1;

  CurrentToken = LexNextToken();
  LookaheadToken = LexNextToken();
}

static void ParseErrorHere(const char *Message)
{
  fprintf(stderr, "Parse error at %d:%d: %s\n", CurrentToken.Line, CurrentToken.Column, Message);
  exit(1);
}

static void Expect(TokenKind Kind, const char *What)
{
  if (CurrentToken.Kind != Kind)
  {
    char Buf[256];
    snprintf(Buf, sizeof(Buf), "Expected %s.", What);
    ParseErrorHere(Buf);
  }
  AdvanceToken();
}

static char *ConsumeIdentifierOrFail(const char *What)
{
  char *Name;

  if (CurrentToken.Kind != TokenIdentifier)
  {
    ParseErrorHere(What);
  }

  Name = (char *)MallocOrDie((size_t)CurrentToken.Length + 1);
  memcpy(Name, CurrentToken.Start, (size_t)CurrentToken.Length);
  Name[CurrentToken.Length] = '\0';

  AdvanceToken();
  return Name;
}

/* ----------------------------- Globals List ----------------------------- */

typedef struct StringList
{
  char **Items;
  int Count;
  int Capacity;
} StringList;

static void StringListInit(StringList *List)
{
  List->Items = NULL;
  List->Count = 0;
  List->Capacity = 0;
}

static void StringListAdd(StringList *List, char *Item)
{
  if (List->Count >= List->Capacity)
  {
    int NewCapacity = (List->Capacity == 0) ? 16 : (List->Capacity * 2);
    char **NewItems = (char **)MallocOrDie(sizeof(char *) * (size_t)NewCapacity);
    if (List->Items)
    {
      memcpy(NewItems, List->Items, sizeof(char *) * (size_t)List->Count);
      free(List->Items);
    }
    List->Items = NewItems;
    List->Capacity = NewCapacity;
  }

  List->Items[List->Count++] = Item;
}

static int StringListContains(const StringList *List, const char *Item)
{
  int I;
  for (I = 0; I < List->Count; I++)
  {
    if (strcmp(List->Items[I], Item) == 0)
    {
      return 1;
    }
  }
  return 0;
}

/* ----------------------------- Code Generation ----------------------------- */

static FILE *OutFile = NULL;
static const char *CurrentFunctionName = NULL;
static int LabelCounter = 1;

static void EmitLine0(const char *Text)
{
  fputs(Text, OutFile);
  fputc('\n', OutFile);
}

static void EmitLine2(const char *Text)
{
  /* NASM instruction lines are indented 2 spaces */
  fputs("  ", OutFile);
  fputs(Text, OutFile);
  fputc('\n', OutFile);
}

static void EmitLabel(const char *Label)
{
  fputs(Label, OutFile);
  fputs(":\n", OutFile);
}

static void EmitFmt2(const char *Fmt, const char *A)
{
  char Buf[512];
  snprintf(Buf, sizeof(Buf), Fmt, A);
  EmitLine2(Buf);
}

static void EmitFmt2A(const char *Fmt, const char *A, const char *B)
{
  char Buf[512];
  snprintf(Buf, sizeof(Buf), Fmt, A, B);
  EmitLine2(Buf);
}

static void EmitFmt2I(const char *Fmt, long Value)
{
  char Buf[512];
  snprintf(Buf, sizeof(Buf), Fmt, Value);
  EmitLine2(Buf);
}

static void MakeUniqueLabel(char *Out, size_t OutSize, const char *Suffix)
{
  snprintf(Out, OutSize, ".L%s_%s_%d", CurrentFunctionName, Suffix, LabelCounter++);
}

/* ----------------------------- Expression Parser + Emitter ----------------------------- */

typedef enum BinaryOp
{
  BinAdd,
  BinSub,
  BinMul,
  BinDiv,
  BinMod,

  BinEq,
  BinNe,
  BinLt,
  BinLe,
  BinGt,
  BinGe
} BinaryOp;

static void EmitExpression(void);

static void EmitPrimary(void)
{
  if (CurrentToken.Kind == TokenNumber)
  {
    EmitFmt2I("mov eax, %ld", CurrentToken.NumberValue);
    AdvanceToken();
    return;
  }

  if (CurrentToken.Kind == TokenIdentifier)
  {
    /* Calls are statements only in SubC (void-only). Reject as expression. */
    if (LookaheadToken.Kind == TokenLParen)
    {
      ParseErrorHere("Function calls are void and allowed only as statements like Foo();");
    }

    {
      char *Name = ConsumeIdentifierOrFail("Expected identifier.");
      EmitFmt2("mov eax, dword [%s]", Name);
      free(Name);
      return;
    }
  }

  if (CurrentToken.Kind == TokenLParen)
  {
    AdvanceToken();
    EmitExpression();
    Expect(TokenRParen, "')'");
    return;
  }

  ParseErrorHere("Expected number, identifier, or parenthesized expression.");
}

static void EmitUnary(void)
{
  if (CurrentToken.Kind == TokenPlus)
  {
    AdvanceToken();
    EmitUnary();
    return;
  }

  if (CurrentToken.Kind == TokenMinus)
  {
    AdvanceToken();
    EmitUnary();
    EmitLine2("neg eax");
    return;
  }

  if (CurrentToken.Kind == TokenBang)
  {
    AdvanceToken();
    EmitUnary();
    EmitLine2("cmp eax, 0");
    EmitLine2("sete al");
    EmitLine2("movzx eax, al");
    return;
  }

  EmitPrimary();
}

static void EmitMul(void)
{
  EmitUnary();

  while (CurrentToken.Kind == TokenStar ||
    CurrentToken.Kind == TokenSlash ||
    CurrentToken.Kind == TokenPercent)
  {
    TokenKind Op = CurrentToken.Kind;
    AdvanceToken();

    EmitLine2("push eax");
    EmitUnary();
    EmitLine2("pop ecx"); /* ECX = left, EAX = right */

    if (Op == TokenStar)
    {
      EmitLine2("imul eax, ecx");
    }
    else if (Op == TokenSlash || Op == TokenPercent)
    {
      /* Signed division: left / right or left % right
      * idiv uses EDX:EAX as dividend and a register/mem operand as divisor.
      * We'll put divisor in EBX, while preserving EBX across the operation.
      */
      EmitLine2("push ebx");
      EmitLine2("mov ebx, eax"); /* EBX = right (divisor) */
      EmitLine2("mov eax, ecx"); /* EAX = left (dividend low) */
      EmitLine2("cdq");          /* sign-extend EAX into EDX:EAX */
      EmitLine2("idiv ebx");     /* quotient in EAX, remainder in EDX */

      if (Op == TokenPercent)
      {
        EmitLine2("mov eax, edx");
      }

      EmitLine2("pop ebx");
    }
  }
}

static void EmitAdd(void)
{
  EmitMul();

  while (CurrentToken.Kind == TokenPlus || CurrentToken.Kind == TokenMinus)
  {
    TokenKind Op = CurrentToken.Kind;
    AdvanceToken();

    EmitLine2("push eax");
    EmitMul();
    EmitLine2("pop ecx"); /* ECX = left, EAX = right */

    if (Op == TokenPlus)
    {
      EmitLine2("add eax, ecx"); /* EAX = right + left */
    }
    else
    {
      EmitLine2("sub ecx, eax"); /* ECX = left - right */
      EmitLine2("mov eax, ecx");
    }
  }
}

static void EmitRelational(void)
{
  EmitAdd();

  while (CurrentToken.Kind == TokenLt ||
    CurrentToken.Kind == TokenLtEq ||
    CurrentToken.Kind == TokenGt ||
    CurrentToken.Kind == TokenGtEq)
  {
    TokenKind Op = CurrentToken.Kind;
    AdvanceToken();

    EmitLine2("push eax");
    EmitAdd();
    EmitLine2("pop ecx"); /* ECX = left, EAX = right */

    EmitLine2("cmp ecx, eax");

    if (Op == TokenLt)   EmitLine2("setl al");
    if (Op == TokenLtEq) EmitLine2("setle al");
    if (Op == TokenGt)   EmitLine2("setg al");
    if (Op == TokenGtEq) EmitLine2("setge al");

    EmitLine2("movzx eax, al");
  }
}

static void EmitEquality(void)
{
  EmitRelational();

  while (CurrentToken.Kind == TokenEqEq || CurrentToken.Kind == TokenNotEq)
  {
    TokenKind Op = CurrentToken.Kind;
    AdvanceToken();

    EmitLine2("push eax");
    EmitRelational();
    EmitLine2("pop ecx"); /* ECX = left, EAX = right */

    EmitLine2("cmp ecx, eax");

    if (Op == TokenEqEq)  EmitLine2("sete al");
    if (Op == TokenNotEq) EmitLine2("setne al");

    EmitLine2("movzx eax, al");
  }
}

static void EmitAssignmentOrEquality(void)
{
  /* assignment := Ident '=' assignment | equality
  *
  * With 2-token lookahead, we can detect "Identifier =" easily.
  */
  if (CurrentToken.Kind == TokenIdentifier && LookaheadToken.Kind == TokenAssign)
  {
    char *Name = ConsumeIdentifierOrFail("Expected identifier on left side of assignment.");
    Expect(TokenAssign, "'='");

    EmitAssignmentOrEquality();

    /* store result into global variable */
    EmitFmt2("mov dword [%s], eax", Name);

    free(Name);
    return;
  }

  EmitEquality();
}

static void EmitExpression(void)
{
  EmitAssignmentOrEquality();
}

/* ----------------------------- Statement Parser + Emitter ----------------------------- */

static void EmitStatement(void);

static void EmitBlock(void)
{
  Expect(TokenLBrace, "'{'");

  while (CurrentToken.Kind != TokenRBrace)
  {
    if (CurrentToken.Kind == TokenEnd)
    {
      ParseErrorHere("Unexpected end of input inside block.");
    }
    EmitStatement();
  }

  Expect(TokenRBrace, "'}'");
}

static void EmitIf(void)
{
  char ElseLabel[128];
  char EndLabel[128];

  Expect(TokenIf, "'if'");
  Expect(TokenLParen, "'('");
  EmitExpression();
  Expect(TokenRParen, "')'");

  MakeUniqueLabel(ElseLabel, sizeof(ElseLabel), "else");
  MakeUniqueLabel(EndLabel, sizeof(EndLabel), "endif");

  EmitLine2("cmp eax, 0");
  EmitFmt2("je %s", ElseLabel);

  EmitStatement();

  if (CurrentToken.Kind == TokenElse)
  {
    AdvanceToken();
    EmitFmt2("jmp %s", EndLabel);
    EmitLabel(ElseLabel);
    EmitStatement();
    EmitLabel(EndLabel);
  }
  else
  {
    EmitLabel(ElseLabel);
  }
}

static void EmitWhile(void)
{
  char TopLabel[128];
  char EndLabel[128];

  Expect(TokenWhile, "'while'");
  MakeUniqueLabel(TopLabel, sizeof(TopLabel), "while_top");
  MakeUniqueLabel(EndLabel, sizeof(EndLabel), "while_end");

  EmitLabel(TopLabel);

  Expect(TokenLParen, "'('");
  EmitExpression();
  Expect(TokenRParen, "')'");

  EmitLine2("cmp eax, 0");
  EmitFmt2("je %s", EndLabel);

  EmitStatement();

  EmitFmt2("jmp %s", TopLabel);
  EmitLabel(EndLabel);
}

static void EmitReturn(void)
{
  Expect(TokenReturn, "'return'");

  /* SubC: return; only */
  if (CurrentToken.Kind != TokenSemicolon)
  {
    ParseErrorHere("SubC only supports 'return;' (no return value).");
  }

  Expect(TokenSemicolon, "';'");
  EmitLine2("ret");
}

static void EmitCallStatement(void)
{
  char *Name = ConsumeIdentifierOrFail("Expected function name.");

  Expect(TokenLParen, "'('");
  Expect(TokenRParen, "')'");
  Expect(TokenSemicolon, "';'");
  EmitFmt2("call %s", Name);

  free(Name);
}

static void EmitExpressionStatement(void)
{
  EmitExpression();
  Expect(TokenSemicolon, "';'");
  /* We ignore the expression result (in EAX). */
}

static void EmitStatement(void)
{
  if (CurrentToken.Kind == TokenLBrace)
  {
    EmitBlock();
    return;
  }

  if (CurrentToken.Kind == TokenIf)
  {
    EmitIf();
    return;
  }

  if (CurrentToken.Kind == TokenWhile)
  {
    EmitWhile();
    return;
  }

  if (CurrentToken.Kind == TokenReturn)
  {
    EmitReturn();
    return;
  }

  /* Calls are statements only: Identifier '(' ')' ';' */
  if (CurrentToken.Kind == TokenIdentifier && LookaheadToken.Kind == TokenLParen)
  {
    EmitCallStatement();
    return;
  }

  /* Disallow local declarations */
  if (CurrentToken.Kind == TokenInt)
  {
    ParseErrorHere("SubC does not allow local variables. Only global 'int Name;' declarations are allowed.");
  }

  /* Otherwise, treat as expression statement */
  EmitExpressionStatement();
}

/* ----------------------------- Top-Level Parser ----------------------------- */

static StringList DefinedGlobals;
static char *FunctionName = NULL;

static void ParseGlobalDeclarations(void)
{
  while (CurrentToken.Kind == TokenInt)
  {
    char *Name;

    AdvanceToken(); /* consume 'int' */

    Name = ConsumeIdentifierOrFail("Expected global variable name after 'int'.");

    /* SubC rule: zero-only. So no initializer. */
    if (CurrentToken.Kind != TokenSemicolon)
    {
      ParseErrorHere("SubC global variables must be declared as 'int Name;' (no initializer).");
    }

    Expect(TokenSemicolon, "';'");
    StringListAdd(&DefinedGlobals, Name);
  }
}

static void ParseOneVoidFunction(void)
{
  Expect(TokenVoid, "'void'");
  FunctionName = ConsumeIdentifierOrFail("Expected function name after 'void'.");

  Expect(TokenLParen, "'('");
  if (CurrentToken.Kind != TokenRParen)
  {
    ParseErrorHere("SubC functions take no parameters. Use 'void Name()'.");
  }
  Expect(TokenRParen, "')'");

  CurrentFunctionName = FunctionName;
  EmitBlock();

  /* If the function ends without an explicit return;, we do NOT auto-insert ret.
  * That keeps behavior explicit and simple.
  * You can always write 'return;' at the end.
  */
}

static void EmitGlobals(void)
{
  int I;

  for (I = 0; I < DefinedGlobals.Count; I++)
  {
    EmitLine0("align 4");
    {
      char Buf[512];
      snprintf(Buf, sizeof(Buf), "%s: dd 0", DefinedGlobals.Items[I]);
      EmitLine0(Buf);
    }
  }

  if (DefinedGlobals.Count > 0)
  {
    EmitLine0("");
  }
}

static void ParseUnitAndEmit(void)
{
  /* unit := (global_decl)* func_def End */

  ParseGlobalDeclarations();

  if (CurrentToken.Kind != TokenVoid)
  {
    ParseErrorHere("Expected a function definition starting with 'void'.");
  }

  /* Emit header + globals */
  EmitLine0("bits 32");
  EmitLine0("");
  EmitGlobals();

  /* Emit function label, then body */
  Expect(TokenVoid, "'void'");
  FunctionName = ConsumeIdentifierOrFail("Expected function name after 'void'.");

  Expect(TokenLParen, "'('");
  if (CurrentToken.Kind != TokenRParen)
  {
    ParseErrorHere("SubC functions take no parameters. Use 'void Name()'.");
  }
  Expect(TokenRParen, "')'");

  CurrentFunctionName = FunctionName;
  EmitLabel(FunctionName);
  EmitBlock();

  /* Ensure there's no extra trailing tokens (allow whitespace/comments only) */
  if (CurrentToken.Kind != TokenEnd)
  {
    ParseErrorHere("Unexpected extra text after the end of the function.");
  }
}

/* ----------------------------- Main ----------------------------- */

int main(int Argc, char **Argv)
{
  char *InputText;

  StringListInit(&DefinedGlobals);

  if (Argc != 2)
  {
    fprintf(stderr, "Usage: %s <input.subc>\n", Argv[0]);
    return 1;
  }

  InputText = ReadEntireFile(Argv[1]);

  OutFile = stdout;

  InitLexer(InputText);

  ParseUnitAndEmit();

  free(InputText);
  return 0;
}