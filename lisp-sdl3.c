/* Lisp with SDL3 graphics by Kartik Agaram 2025 */
/* Based on lisp.c by Robert A. van Engelen 2022 BSD-3 license (https://github.com/Robert-van-Engelen/lisp)
    - double precision floating point, symbols, strings, lists, proper closures, and macros
    - over 40 built-in Lisp primitives
    - lexically-scoped locals in lambda, let, let*, letrec, letrec*
    - proper tail-recursion, including tail calls through begin, cond, if, let, let*, letrec, letrec*
    - exceptions and error handling with safe return to REPL after an error
    - break with CTRL-C to return to the REPL
    - REPL with readline
    - load Lisp source code files
    - execution tracing to display Lisp evaluation steps
    - mark-sweep garbage collector to recycle unused cons pair cells
    - compacting garbage collector to recycle unused atoms and strings */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>             /* int64_t, uint64_t, uint32_t (or we can use e.g. unsigned long long instead) */
#include <stdbool.h>
#include <string.h>
#include <setjmp.h>

#include <signal.h>             /* to catch CTRL-C and continue the REPL */
#define BREAK_ON  signal(SIGINT, (void(*)(int))err)
#define BREAK_OFF signal(SIGINT, SIG_IGN)

#include <readline/readline.h>  /* for convenient line editing ... */
#include <readline/history.h>   /* ... and a history of previous Lisp input */

#include <sys/select.h>         /* for select() in non-blocking stdin check */
#include <unistd.h>             /* for STDIN_FILENO */
#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>
#include <SDL3_ttf/SDL_ttf.h>

/* floating point output format */
#define FLOAT "%.17lg"

/* DEBUG: always run GC when allocating cells and atoms/strings on the heap */
#ifdef DEBUG
#define ALWAYS_GC 1
#else
#define ALWAYS_GC 0
#endif

/*----------------------------------------------------------------------------*\
 |      LISP EXPRESSION TYPES AND NAN BOXING                                  |
\*----------------------------------------------------------------------------*/

/* we only need two types to implement a Lisp interpreter:
        I      unsigned integer (32 bit unsigned)
        L      Lisp expression (double with NaN boxing)
   I variables and function parameters are named as follows:
        i,j,k  any unsigned integer, e.g. a NaN-boxed ordinal value or index
        t      a NaN-boxing tag
   L variables and function parameters are named as follows:
        x,y    any Lisp expression
        n      number
        t,s    list
        f      function or Lisp primitive
        p      pair, a cons of two Lisp expressions
        e,d    environment, a list of pairs, e.g. created with (define v x)
        v      the name of a variable (an atom) or a list of variables */
#define I uint32_t
#define L double

/* T(x) returns the tag bits of a NaN-boxed Lisp expression x */
#define T(x) (*(uint64_t*)&x >> 48)

/* primitive, atom, string, cons, closure, macro and nil tags for NaN boxing (reserve 0x7ff8 for nan) */
enum { PRIM = 0x7ff9, ATOM = 0x7ffa, STRG = 0x7ffb, CONS = 0x7ffc, CLOS = 0x7ffe, MACR = 0x7fff, NIL = 0xffff };

/* box(t,i): returns a new NaN-boxed double with tag t and ordinal i
   ord(x):   returns the ordinal of the NaN-boxed double x
   num(n):   convert or check number n (does nothing, e.g. could check for NaN)
   equ(x,y): returns nonzero if x equals y */
L box(I t, I i) { L x; *(uint64_t*)&x = (uint64_t)t << 48 | i; return x; }
I ord(L x)      { return *(uint64_t*)&x; }              /* narrow return to 32 bit to remove the tag */
L num(L n)      { return n; }                           /* could check for a valid number return n == n ? n : err(5); */
I equ(L x, L y) { return *(uint64_t*)&x == *(uint64_t*)&y; }

/*----------------------------------------------------------------------------*\
 |      ERROR HANDLING AND ERROR MESSAGES                                     |
\*----------------------------------------------------------------------------*/

/* setjmp-longjmp jump buffer */
jmp_buf jb;

/* report and throw an exception */
#define ERR(n, ...) (fprintf(stderr, __VA_ARGS__), err(n))
__attribute__((noreturn)) L err(int n) { longjmp(jb, n); }


typedef enum  {
    ERR_UNKNOWN    = 0,
    ERR_NOT_PAIR   = 1,
    ERR_BREAK      = 2,
    ERR_UNBOUND    = 3,
    ERR_CANT_APPLY = 4,
    ERR_ARGUMENTS  = 5,
    ERR_STACK_OVER = 6,
    ERR_OUT_OF_MEM = 7,
    ERR_SYNTAX     = 8,
    NUM_ERRORS     = 9
} LispRuntimeError;

const char *errors[NUM_ERRORS+1] = {
  "unknown", "not a pair", "break", "unbound symbol", "cannot apply", "arguments", "stack over", "out of memory", "syntax"
};

void errorInLocation(const char* where, int e) {
    const char *error = errors[e > 0 && e <= NUM_ERRORS ? e : 0];
    fprintf(stderr, "\e[31;1mError in %s: %s\e[m\n", where, error);
}

void errorInSDLInit(const char* where) {
    fprintf(stderr, "\e[31;1m%s failed: %s\n", where, SDL_GetError());
}


/*----------------------------------------------------------------------------*\
 |      MEMORY MANAGEMENT AND RECYCLING                                       |
\*----------------------------------------------------------------------------*/

/* number of cells to allocate for the cons pair pool, increase P as desired */
#define P 8192

/* number of cells to allocate for the shared stack and heap, increase S as desired */
#define S 8192

/* total number of cells to allocate = P+S */
#define N (P+S)

/* base address of the atom/string heap */
#define A (char*)cell

/* heap address start offset, the heap starts at address A+H immediately above the pool */
#define H (8*P)

/* size Z of the atom/string size field at the base address of each atom/string on the heap */
#define Z sizeof(I)

/* array of Lisp expressions, shared by the pool, heap and stack */
L cell[N];

/* fp: free pointer points to free cell pair in the pool, next free pair is ord(cell[fp]) unless fp=0
   hp: heap pointer, A+hp points free atom/string heap space above the pool and below the stack
   sp: stack pointer, the stack starts at the top of cell[] with sp=N
   tr: 0 when tracing is off, 1 or 2 to trace Lisp evaluation steps */
I fp = 0, hp = H, sp = N, tr = 0;

/* Lisp constant expressions () (nil) and #t, and the global environment env */
L nil, tru, env;

/* bit vector corresponding to the pairs of cells in the pool marked 'used' (car and cdr cells are marked together) */
uint32_t used[(P+63)/64];

/* mark-sweep garbage collector recycles cons pair pool cells, finds and marks cells that are used */
void mark(I i) {
  while (!(used[i/64] & 1 << i/2%32)) {         /* while i'th cell pair is not used in the pool */
    used[i/64] |= 1 << i/2%32;                  /* mark i'th cell pair as used */
    if ((T(cell[i]) & ~(CONS^MACR)) == CONS)    /* recursively mark car cell[i] if car refers to a pair */
      mark(ord(cell[i]));
    if ((T(cell[i+1]) & ~(CONS^MACR)) != CONS)  /* if cdr cell[i+1] is not a pair, then break and return */
      break;
    i = ord(cell[i+1]);                         /* iteratively mark cdr cell[i+1] */
  }
}

/* mark-sweep garbage collector recycles cons pair pool cells, returns total number of free cells in the pool */
I sweep() {
  I i, j;
  for (fp = 0, i = P/2, j = 0; i--; ) {         /* for each cons pair (two cells) in the pool, from top to bottom */
    if (!(used[i/32] & 1 << i%32)) {            /* if the cons pair cell[2*i] and cell[2*i+1] are not used */
      cell[2*i] = box(NIL, fp);                 /* then add it to the linked list of free cells pairs as a NIL box */
      fp = 2*i;                                 /* free pointer points to the last added free pair */
      j += 2;                                   /* two more cells freed */
    }
  }
  return j;                                     /* return number of cells freed */
}

/* add i'th cell to the linked list of cells that refer to the same atom/string */
void chain(I i) {
  I k = *(I*)(A+ord(cell[i])-Z);                /* atom/string link k is the k'th cell that uses the atom/string */
  *(I*)(A+ord(cell[i])-Z) = i;                  /* add k'th cell to the linked list of atom/string cells */
  cell[i] = box(T(cell[i]), k);                 /* by updating the i'th cell atom/string ordinal to k */
}

/* compacting garbage collector recycles heap by removing unused atoms/strings and by moving used ones */
void compact() {
  I i, j, k, l, n;
  for (i = H; i < hp; i += n+Z) {               /* for each atom/string set its linked lists sentinel (end of list) */
    n = *(I*)(A+i);                             /* get the atom/string size > 0 (data size + 1 for zero byte) */
    *(I*)(A+i) = n+H;                           /* linked list sentinel is H+size where 0 < size < hp-H */
  }
  for (i = 0; i < P; ++i)                       /* add each used atom/string cell in the pool to its linked list */
    if (used[i/64] & 1 << i/2%32 && (T(cell[i]) & ~(ATOM^STRG)) == ATOM)
      chain(i);
  for (i = sp; i < N; ++i)                      /* add each used atom/string cell on the stack to its linked list */
    if ((T(cell[i]) & ~(ATOM^STRG)) == ATOM)
      chain(i);
  for (i = H, j = hp, hp = H; i < j; i += n) {  /* for each atom/string on the heap */
    for (k = *(I*)(A+i), l = H; k < H || k > j; k = l) {
      l = ord(cell[k]);
      cell[k] = box(T(cell[k]), hp+Z);          /* hp+Z is the new location of the atom/string after compaction */
    }
    n = k-H+Z;                                  /* the atom/string size+Z, i+n is the next atom/string to compact */
    if (l != H) {                               /* if this atom/string is used in the pool or stack, then keep it */
      *(I*)(A+i) = k-H;                         /* restore the atom/string size from linked list sentinel k = H+size */
      if (hp < i)
        memmove(A+hp, A+i, n);                  /* move atom/string further down the heap to hp to compact the heap */
      hp += n;                                  /* update heap pointer to the available space above the atom/string */
    }
  }
}

/* garbage collector, returns number of free cells in the pool or raises ERR_OUT_OF_MEM */
I gc() {
  I i;
  BREAK_OFF;                                    /* do not interrupt GC */
  memset(used, 0, sizeof(used));                /* clear all used[] bits */
  if (T(env) == CONS)
    mark(ord(env));                             /* mark all globally-used cons cell pairs referenced from env list */
  for (i = sp; i < N; ++i)
    if ((T(cell[i]) & ~(CONS^MACR)) == CONS)
      mark(ord(cell[i]));                       /* mark all cons cell pairs referenced from the stack */
  i = sweep();                                  /* remove unused cons cell pairs from the pool */
  compact();                                    /* remove unused atoms and strings from the heap */
  BREAK_ON;                                     /* enable interrupt */
  return i ? i : err((int) ERR_OUT_OF_MEM);
}

/* push x on the stack to protect it from being recycled, returns pointer to cell pair (e.g. to update the value) */
L *push(L x) {
  cell[--sp] = x;                               /* we must save x on the stack so it won't get GC'ed */
  if (hp > (sp-1) << 3 || ALWAYS_GC) {          /* if insufficient stack space is available, then GC */
    gc();                                       /* GC */
    if (hp > (sp-1) << 3)                       /* GC did not free up heap space to enlarge the stack */
      err((int) ERR_STACK_OVER);
  }
  return &cell[sp];
}

/* pop from the stack and return value */
L pop() {
  return cell[sp++];
}

/* unwind the stack up to position i, where i=N clears the stack */
void unwind(I i) {
  sp = i;
}

/*----------------------------------------------------------------------------*\
 |      LISP EXPRESSION CONSTRUCTION AND INSPECTION                           |
\*----------------------------------------------------------------------------*/

/* allocate n+1 bytes on the heap, returns heap offset of the allocated space */
I alloc(I n) {
  I i;
  if (hp+Z+n+1 > (sp-1) << 3 || ALWAYS_GC) {    /* if insufficient heap space is available, then GC */
    gc();                                       /* GC */
    if (hp+Z+n+1 > (sp-1) << 3)                 /* GC did not free up sufficient heap space */
      err((int) ERR_STACK_OVER);
  }
  *(I*)(A+hp) = n+1;                            /* store the size n+1 (data size + 1) in the size field */
  i = hp+Z;
  *(A+i+n) = '\0';                              /* end the allocated block with a terminating zero byte */
  hp = i+n+1;                                   /* update heap pointer to the available space above the atom/string */
  return i;
}

/* copy string s to the heap, returns heap offset of the string on the heap */
I copy(const char *s) {
  return strcpy(A+alloc(strlen(s)), s)-A;       /* copy string+\0 to the heap */
}

/* interning of atom names (symbols), returns a unique NaN-boxed ATOM */
L atom(const char *s) {
  I i = H+Z;
  while (i < hp && strcmp(A+i, s))              /* search the heap for matching atom (or string) s */
    i += *(I*)(A+i-Z)+Z;
  if (i >= hp)                                  /* if not found, then copy s to the heap for the new atom */
    i = copy(s);
  return box(ATOM, i);                          /* return unique NaN-boxed ATOM */
}

/* store string s on the heap, returns a NaN-boxed STRG with heap offset */
L string(const char *s) {
  return box(STRG, copy(s));                    /* copy string+\0 to the heap, return NaN-boxed STRG */
}

/* construct pair (x . y) returns a NaN-boxed CONS */
L cons(L x, L y) {
  L p; I i = fp;                                /* i'th cons cell pair car cell[i] and cdr cell[i+1] is free */
  fp = ord(cell[i]);                            /* update free pointer to next free cell pair, zero if none are free */
  cell[i] = x;                                  /* save x into car cell[i] */
  cell[i+1] = y;                                /* save y into cdr cell[i+1] */
  p = box(CONS, i);                             /* new cons pair NaN-boxed CONS */
  if (!fp || ALWAYS_GC) {                       /* if no more free cell pairs */
    push(p);                                    /* save new cons pair p on the stack so it won't get GC'ed */
    gc();                                       /* GC */
    pop();                                      /* rebalance the stack */
  }
  return p;                                     /* return NaN-boxed CONS */
}

/* construct a pair to add to environment e, returns the list ((v . x) . e) */
L pair(L v, L x, L e) {
  return cons(cons(v, x), e);
}

/* construct a closure, returns a NaN-boxed CLOS */
L closure(L v, L x, L e) {
  return box(CLOS, ord(pair(v, x, equ(e, env) ? nil : e)));
}

/* construct a macro, returns a NaN-boxed MACR */
L macro(L v, L x) {
  return box(MACR, ord(cons(v, x)));
}

/* return the car of a cons/closure/macro pair; CAR(p) provides direct memory access */
#define CAR(p) cell[ord(p)]
L car(L p) {
  return (T(p) & ~(CONS^MACR)) == CONS ? CAR(p) : err((int) ERR_NOT_PAIR);
}

/* return the cdr of a cons/closure/macro pair; CDR(p) provides direct memory access */
#define CDR(p) cell[ord(p)+1]
L cdr(L p) {
  return (T(p) & ~(CONS^MACR)) == CONS ? CDR(p) : err((int) ERR_NOT_PAIR);
}

/* look up a symbol in an environment, returns its value */
L assoc(L v, L e) {
  while (T(e) == CONS && !equ(v, car(car(e))))
    e = cdr(e);
  if (T(e) == NIL)  // not found
    return nil;
  else if(T(e) == CONS) // found it.
    return cdr(car(e));
  else // error
    if (T(v) == ATOM)
      ERR((int) ERR_UNBOUND, "no such key %s ", A+ord(v));
    else
      err((int) ERR_UNBOUND);
}

/* check if a symbol is bound in an environment (for callback checking) */
I bound(L v, L e) {
  while (T(e) == CONS && !equ(v, car(car(e))))
    e = cdr(e);
  return T(e) == CONS;
}

/* not(x) is nonzero if x is the Lisp () empty list */
I not(L x) {
  return T(x) == NIL;
}

/* more(t) is nonzero if list t has more than one item */
I more(L t) {
  return !not(t) && !not(cdr(t));
}


/*----------------------------------------------------------------------------*\
 |      BATCH READ                                                           |
 |      read_b -> parse_b -> scan_b -> get_b                                 |
 |      unbalanced '(' is an error                                           |
\*----------------------------------------------------------------------------*/

typedef struct {
  FILE *f;
  char see;
  char buf[256];
} BatchInput;

/* return the character we see, advance to the next character */
char get_b(BatchInput *in) {
  int look = in->see;
  in->see = getc(in->f);                        /* read a character */
  if (in->see == EOF)
    in->see = '\0';                             /* pretend we see a null character at eof */
  return look;                                  /* return the previous character we were looking at */
}

bool white(char see) {
  return see > 0 && see <= ' ';
}

/* tokenize into in->buf[], return first character of in->buf[] */
char scan_b(BatchInput *in) {
  I i = 0;
  while (white(in->see) || in->see == ';')      /* skip white space and ;-comments */
    if (get_b(in) == ';')
      while (in->see != '\n' && in->see != '\0') /* skip ;-comment until newline */
        get_b(in);
  if (in->see == '\0') {
    /* do nothing */
  }
  else if (in->see == '"') {                    /* tokenize a quoted string */
    do {
      in->buf[i++] = get_b(in);
      while (in->see == '\\' && i < sizeof(in->buf)-1) {
        static const char *abtnvfr = "abtnvfr"; /* \a, \b, \t, \n, \v, \f, \r escape codes */
        const char *esc;
        get_b(in);
        esc = strchr(abtnvfr, in->see);
        in->buf[i++] = esc ? esc-abtnvfr+7 : in->see;
        get_b(in);
      }
    } while (i < sizeof(in->buf)-1 && in->see != '"' && in->see != '\n');
    if (get_b(in) != '"')
      ERR((int) ERR_SYNTAX, "missing \" ");
  }
  else if (in->see == '(' || in->see == ')' || in->see == '\'' ||
           in->see == '`' || in->see == ',') {
    in->buf[i++] = get_b(in);                   /* ( ) ' ` , are single-character tokens */
  }
  else {                                        /* tokenize a symbol or a number */
    do {
      in->buf[i++] = get_b(in);
    } while (i < sizeof(in->buf)-1 && !white(in->see)
        && in->see != '(' && in->see != ')' && in->see != '\0');
  }
  in->buf[i] = '\0';
  return *in->buf;                              /* return first character of token in buf[] */
}

/* return the Lisp expression parsed and read from input */
/* on EOF, return nil */
L parse_b(BatchInput *in);

L read_b2(BatchInput *in) {
  return (scan_b(in) == '\0') ? nil : parse_b(in);
}

L read_b(FILE *f) {
  BatchInput in; in.f = f, in.see = ' ', *in.buf = '\0';
  if (scan_b(&in) == '\0') return nil;
  return parse_b(&in);
}

/* return a parsed Lisp list */
L list_b(BatchInput *in) {
  L *p = push(nil);                             /* push the new list to protect it from getting GC'ed */
  while (scan_b(in) != ')') {
    if (*in->buf == '\0')
      ERR((int) ERR_SYNTAX, "unexpected EOF in list");
    if (*in->buf == '.' && !in->buf[1]) {       /* parse list with dot pair ( <expr> ... <expr> . <expr> ) */
      *p = read_b2(in);
      if (scan_b(in) != ')')
        ERR((int) ERR_SYNTAX, "expecting ) ");
      break;
    }
    *p = cons(parse_b(in), nil);                /* add parsed expression to end of the list by replacing the last nil */
    p = &CDR(*p);                               /* p points to the cdr nil to replace it with the rest of the list */
  }
  return pop();
}

/* return a list/quote-converted Lisp expression (backquote aka. backtick) */
L tick_b(BatchInput *in) {
  L *p;
  if (*in->buf == ',')
    return read_b2(in);                         /* parse and return Lisp expression */
  if (*in->buf != '(')
    return cons(atom("quote"), cons(parse_b(in), nil)); /* parse expression and return (quote <expr>) */
  p = push(cons(atom("list"), nil));
  while (scan_b(in) != ')') {
    if (*in->buf == '\0')
      ERR((int) ERR_SYNTAX, "unexpected EOF in backtick");
    p = &CDR(*p);                               /* p points to the cdr nil to replace it with the rest of the list */
    if (*in->buf == '.' && !in->buf[1]) {       /* tick list with dot pair ( <expr> ... <expr> . <expr> ) */
      *p = read_b2(in);                         /* read expression to replace the last nil at the end of the list */
      if (scan_b(in) != ')')
        ERR((int) ERR_SYNTAX, "expecting ) ");
      break;
    }
    *p = cons(tick_b(in), nil);                 /* add ticked expression to end of the list by replacing the last nil */
  }
  return pop();                                 /* return (list <expr> ... <expr>) */
}

/* return a parsed Lisp expression */
L parse_b(BatchInput *in) {
  L x; I i;
  switch (*in->buf) {
    case '\0': return ERR((int) ERR_SYNTAX, "unexpected EOF");
    case '(':  return list_b(in);               /* if token is ( then parse a list */
    case '\'': return cons(atom("quote"), cons(read_b2(in), nil)); /* if token is ' then quote an expression */
    case '`':  scan_b(in); return tick_b(in);   /* if token is a ` then list/quote-convert an expression */
    case '"':  return string(in->buf+1);        /* if token is a string, then return a new string */
    case ')':  return ERR((int) ERR_SYNTAX, "unexpected ) ");
  }
  if (sscanf(in->buf, "%lg%n", &x, &i) > 0 && !in->buf[i])
    return x;                                   /* return a number, including inf, -inf and nan */
  return atom(in->buf);                         /* return an atom (a symbol) */
}

/*----------------------------------------------------------------------------*\
 |      INTERACTIVE READ                                                     |
 |      read_i -> parse_i -> scan_i -> get_i                                 |
 |      unbalanced '(' is not an error                                       |
\*----------------------------------------------------------------------------*/

/* Readline callback state for non-blocking REPL */
bool running = true;
char *pending_line = NULL;
bool line_ready = false;
char ps[20];
char *accumulated_input = NULL;

typedef struct {
  const char *ptr;
  char see;
  char buf[256];
  bool *incomplete;
} InteractiveInput;

/* return the character we see, advance to the next character */
char get_i(InteractiveInput *in) {
  int look = in->see;
  if (*in->ptr == '\0') {
    in->see = '\0';
  } else {
    in->see = *in->ptr++;                       /* look at the next character in the readline line */
  }
  return look;                                  /* return the previous character we were looking at */
}

/* tokenize into in->buf[], return first character of in->buf[] */
char scan_i(InteractiveInput *in) {
  I i = 0;
  while ((white(in->see) || in->see == ';') && in->see != '\0') /* skip white space and ;-comments */
    if (get_i(in) == ';')
      while (in->see != '\n' && in->see != '\0') /* skip ;-comment until newline */
        get_i(in);
  if (in->see == '\0') {
    /* do nothing */
  }
  else if (in->see == '"') {                    /* tokenize a quoted string */
    do {
      in->buf[i++] = get_i(in);
      while (in->see == '\\' && i < sizeof(in->buf)-1) {
        static const char *abtnvfr = "abtnvfr"; /* \a, \b, \t, \n, \v, \f, \r escape codes */
        const char *esc;
        get_i(in);
        esc = strchr(abtnvfr, in->see);
        in->buf[i++] = esc ? esc-abtnvfr+7 : in->see;
        get_i(in);
      }
    } while (i < sizeof(in->buf)-1 && in->see != '"' && in->see != '\n' && in->see != '\0');
    if (get_i(in) != '"')
      ERR((int) ERR_SYNTAX, "missing \" ");
  }
  else if (in->see == '(' || in->see == ')' || in->see == '\'' ||
           in->see == '`' || in->see == ',') {
    in->buf[i++] = get_i(in);                   /* ( ) ' ` , are single-character tokens */
  }
  else {                                        /* tokenize a symbol or a number */
    do {
      in->buf[i++] = get_i(in);
    } while (i < sizeof(in->buf)-1 && !white(in->see)
        && in->see != '(' && in->see != ')' && in->see != '\0');
  }
  in->buf[i] = '\0';
  return *in->buf;                              /* return first character of token in buf[] */
}

/* return the Lisp expression parsed and read from input */
/* if incomplete, set *in->incomplete and return nil */
L parse_i(InteractiveInput *in);

L read_i2(InteractiveInput *in) {
  return (scan_i(in) == '\0') ? nil : parse_i(in);
}

L read_i(const char *line, bool *incomplete) {
  InteractiveInput in; in.ptr = line; in.see = ' '; *in.buf = '\0'; in.incomplete = incomplete;
  return read_i2(&in);
}

/* return a parsed Lisp list */
L list_i(InteractiveInput *in) {
  L *p = push(nil);                             /* push the new list to protect it from getting GC'ed */
  while (scan_i(in) != ')') {
    if (*in->buf == '\0') {                     /* the list isn't ready yet */
      *in->incomplete = true;
      pop();                                    /* we'll try again after the next line of input */
      return nil;
    }
    if (*in->buf == '.' && !in->buf[1]) {       /* parse list with dot pair ( <expr> ... <expr> . <expr> ) */
      *p = read_i2(in);                         /* read expression to replace the last nil at the end of the list */
      if (scan_i(in) != ')') {
        if (*in->buf == '\0') {                 /* either the final expr after the dot or ')' isn't typed in yet */
          *in->incomplete = true;
          pop();                                /* we'll try again after the next line of input */
          return nil;
        }
        ERR((int) ERR_SYNTAX, "expecting ) ");
      }
      break;
    }
    *p = cons(parse_i(in), nil);                /* add parsed expression to end of the list by replacing the last nil */
    p = &CDR(*p);                               /* p points to the cdr nil to replace it with the rest of the list */
  }
  return pop();                                 /* pop list and return it */
}

/* return a list/quote-converted Lisp expression (backquote aka. backtick) */
L tick_i(InteractiveInput *in) {
  L *p;
  if (*in->buf == ',')
    return read_i2(in);                         /* parse and return Lisp expression */
  if (*in->buf != '(')
    return cons(atom("quote"), cons(parse_i(in), nil)); /* parse expression and return (quote <expr>) */
  p = push(cons(atom("list"), nil));
  while (scan_i(in) != ')') {
    if (*in->buf == '\0') {                     /* the list isn't ready yet */
      *in->incomplete = true;
      pop();                                    /* we'll try again after the next line of input */
      return nil;
    }
    p = &CDR(*p);                               /* p points to the cdr nil to replace it with the rest of the list */
    if (*in->buf == '.' && !in->buf[1]) {
      *p = read_i2(in);                         /* read expression to replace the last nil at the end of the list */
      if (scan_i(in) != ')') {
        if (*in->buf == '\0') {                 /* either the final expr after the dot or ')' isn't typed in yet */
          *in->incomplete = true;
          pop();                                /* we'll try again after the next line of input */
          return nil;
        }
        ERR((int) ERR_SYNTAX, "expecting ) ");
      }
      break;
    }
    *p = cons(tick_i(in), nil);                 /* add ticked expression to end of the list by replacing the last nil */
  }
  return pop();                                 /* return (list <expr> ... <expr>) */
}

/* return a parsed Lisp expression */
L parse_i(InteractiveInput *in) {
  L x; I i;
  switch (*in->buf) {
    case '\0': *in->incomplete = true; return nil;
    case '(':  return list_i(in);               /* if token is ( then parse a list */
    case '\'': return cons(atom("quote"), cons(read_i2(in), nil)); /* if token is ' then quote an expression */
    case '`':  scan_i(in); return tick_i(in);   /* if token is a ` then list/quote-convert an expression */
    case '"':  return string(in->buf+1);        /* if token is a string, then return a new string */
    case ')':  return ERR((int) ERR_SYNTAX, "unexpected ) ");
  }
  if (sscanf(in->buf, "%lg%n", &x, &i) > 0 && !in->buf[i])
    return x;                                   /* return a number, including inf, -inf and nan */
  return atom(in->buf);                         /* return an atom (a symbol) */
}

/*----------------------------------------------------------------------------*\
 |      PRIMITIVES -- SEE THE TABLE WITH COMMENTS FOR DETAILS                 |
\*----------------------------------------------------------------------------*/

/* the file we are writing to, stdout by default */
FILE *out;

/* SDL3 global variables */
SDL_Window *sdl_window = NULL;
SDL_Renderer *sdl_renderer = NULL;

/* Input and rendering state */
int current_r = 255, current_g = 255, current_b = 255, current_a = 255;
TTF_Font *current_font = NULL;
float mouse_wheel_x = 0.0f;
float mouse_wheel_y = 0.0f;
SDL_Keymod modifiers = SDL_KMOD_NONE;  // Allows key-down? to work

/* construct a new list of evaluated expressions in list t, i.e. the arguments passed to a function or primitive */
L eval(L, L);
L evlis(L t, L e) {
  L *p = push(nil);                             /* push the new list to protect it from getting GC'ed */
  for (; T(t) == CONS; t = cdr(t)) {            /* for each expression in list t */
    *p = cons(eval(car(t), e), nil);            /* evaluate it and add it to the end of the list replacing last nil */
    p = &CDR(*p);                               /* p points to the cdr nil to replace it with the rest of the list */
  }
  if (T(t) == ATOM)                             /* if the list t ends in a symbol */
    *p = assoc(t, e);                           /* evaluate t to replace the last nil at the end of the new list */
  return pop();                                 /* pop new list and return it */
}

L f_type(L t, L *_) {
  L x = car(t);
  return T(x) == NIL ? -1.0 : T(x) >= PRIM && T(x) <= MACR ? T(x) - PRIM + 1 : 0.0;
}

L f_ident(L t, L *_) {
  return car(t);
}

L f_cons(L t, L *_) {
  return cons(car(t), car(cdr(t)));
}

L f_car(L t, L *_) {
  return car(car(t));
}

L f_cdr(L t, L *_) {
  return cdr(car(t));
}

L f_add(L t, L *_) {
  L n = car(t);
  while (!not(t = cdr(t)))
    n += car(t);
  return num(n);
}

L f_sub(L t, L *_) {
  L n = not(cdr(t)) ? -car(t) : car(t);
  while (!not(t = cdr(t)))
    n -= car(t);
  return num(n);
}

L f_bin_and(L t, L *_) {
  L a = car(t);
  if (not(a))
   return err(ERR_ARGUMENTS);
  if (not(t = cdr(t)))
   return err(ERR_ARGUMENTS);
  L b = car(t);
  if (not(b))
   return err(ERR_ARGUMENTS);
  int a_int = (int) a;
  int b_int = (int) b;
  return num(a_int & b_int);
}

L f_bin_or(L t, L *_) {
  L a = car(t);
  if (not(a))
   return err(ERR_ARGUMENTS);
  if (not(t = cdr(t)))
   return err(ERR_ARGUMENTS);
  L b = car(t);
  if (not(b))
   return err(ERR_ARGUMENTS);
  int a_int = (int) a;
  int b_int = (int) b;
  return num(a_int | b_int);
}

L f_bin_xor(L t, L *_) {
  L a = car(t);
  if (not(a))
   return err(ERR_ARGUMENTS);
  if (not(t = cdr(t)))
   return err(ERR_ARGUMENTS);
  L b = car(t);
  if (not(b))
   return err(ERR_ARGUMENTS);
  int a_int = (int) a;
  int b_int = (int) b;
  return num(a_int ^ b_int);
}

L f_bin_shl(L t, L *_) {
  L a = car(t);
  if (not(a))
   return err(ERR_ARGUMENTS);
  if (not(t = cdr(t)))
   return err(ERR_ARGUMENTS);
  L b = car(t);
  if (not(b))
   return err(ERR_ARGUMENTS);
  int a_int = (int) a;
  int b_int = (int) b;
  return num(a_int << b_int);
}

L f_bin_shr(L t, L *_) {
  L a = car(t);
  if (not(a))
   return err(ERR_ARGUMENTS);
  if (not(t = cdr(t)))
   return err(ERR_ARGUMENTS);
  L b = car(t);
  if (not(b))
   return err(ERR_ARGUMENTS);
  int a_int = (int) a;
  int b_int = (int) b;
  return num(a_int >> b_int);
}

L f_mul(L t, L *_) {
  L n = car(t);
  while (!not(t = cdr(t)))
    n *= car(t);
  return num(n);
}

L f_div(L t, L *_) {
  L n = not(cdr(t)) ? 1.0/car(t) : car(t);
  while (!not(t = cdr(t)))
    n /= car(t);
  return num(n);
}

L f_int(L t, L *_) {
  L n = car(t);
  return n < 1e16 && n > -1e16 ? (int64_t)n : n;
}

L f_lt(L t, L *_) {
  L x = car(t), y = car(cdr(t));
  return (T(x) == T(y) && (T(x) & ~(ATOM^STRG)) == ATOM ? strcmp(A+ord(x), A+ord(y)) < 0 :
      x == x && y == y ? x < y : /* x == x is false when x is NaN i.e. a tagged Lisp expression */
      *(int64_t*)&x < *(int64_t*)&y) ? tru : nil;
}

bool iso(L x, L y) {
  if (equ(x, y)) return true;
  if (T(x) != T(y)) return false;
  if (T(x) == STRG) return !strcmp(A+ord(x), A+ord(y));
  if (T(x) == CONS)
    return iso(car(x), car(y)) && iso(cdr(x), cdr(y));
  return false;
}

L f_iso(L t, L *_) {
  L x = car(t), y = car(cdr(t));
  return iso(x, y) ? tru : nil;
}

L f_not(L t, L *_) {
  return not(car(t)) ? tru : nil;
}

L f_or(L t, L *e) {
  L x = nil;
  while (T(t) != NIL && not(x = eval(car(t), *e)))
    t = cdr(t);
  return x;
}

L f_and(L t, L *e) {
  L x = tru;
  while (T(t) != NIL && !not(x = eval(car(t), *e)))
    t = cdr(t);
  return x;
}

L f_list(L t, L *_) {
  return t;
}

L f_begin(L t, L *e) {
  for (; more(t); t = cdr(t))
    eval(car(t), *e);
  return T(t) == NIL ? nil : car(t);
}

L f_while(L t, L *e) {
  L s, x = nil;
  while (!not(eval(car(t), *e)))
    for (s = cdr(t); T(s) != NIL; s = cdr(s))
      x = eval(car(s), *e);
  return x;
}

L f_cond(L t, L *e) {
  while (T(t) != NIL && not(eval(car(car(t)), *e)))
    t = cdr(t);
  return T(t) != NIL ? f_begin(cdr(car(t)), e) : nil;
}

L f_if(L t, L *e) {
  return not(eval(car(t), *e)) ? f_begin(cdr(cdr(t)), e) : car(cdr(t));
}

L f_lambda(L t, L *e) {
  return closure(car(t), car(cdr(t)), *e);
}

L f_macro(L t, L *_) {
  return macro(car(t), car(cdr(t)));
}

L f_define(L t, L *e) {
  L x = eval(car(cdr(t)), *e), v = car(t), d = *e;
  while (T(d) == CONS && !equ(v, car(car(d))))
    d = cdr(d);
  if (T(d) == CONS)
    CDR(car(d)) = x;
  else
    env = pair(v, x, env);
  return v;
}

L f_assoc(L t, L *_) {
  return assoc(car(t), car(cdr(t)));
}

L f_env(L _, L *e) {
  return *e;
}

L f_let(L t, L *e) {
  L d = *e;
  for (; more(t); t = cdr(t))
    *e = pair(car(car(t)), eval(f_begin(cdr(car(t)), &d), d), *e);
  return T(t) == NIL ? nil : car(t);
}

L f_leta(L t, L *e) {
  for (; more(t); t = cdr(t))
    *e = pair(car(car(t)), eval(f_begin(cdr(car(t)), e), *e), *e);
  return T(t) == NIL ? nil : car(t);
}

L f_letrec(L t, L *e) {
  L s;
  for (s = t; more(s); s = cdr(s))
    *e = pair(car(car(s)), nil, *e);
  for (s = *e; more(t); s = cdr(s), t = cdr(t))
    CDR(car(s)) = eval(f_begin(cdr(car(t)), e), *e);
  return T(t) == NIL ? nil : car(t);
}

L f_letreca(L t, L *e) {
  for (; more(t); t = cdr(t)) {
    *e = pair(car(car(t)), nil, *e);
    CDR(car(*e)) = eval(f_begin(cdr(car(t)), e), *e);
  }
  return T(t) == NIL ? nil : car(t);
}

L f_setq(L t, L *e) {
  L x = eval(car(cdr(t)), *e), v = car(t), d = *e;
  while (T(d) == CONS && !equ(v, car(car(d))))
    d = cdr(d);
  return T(d) == CONS ? CDR(car(d)) = x : T(v) == ATOM ? ERR(3, "unbound %s ", A+ord(v)) : err((int) ERR_UNBOUND);
}

L f_setcar(L t, L *_) {
  L p = car(t);
  return T(p) == CONS ? CAR(p) = car(cdr(t)) : err((int) ERR_NOT_PAIR);
}

L f_setcdr(L t, L *_) {
  L p = car(t);
  return T(p) == CONS ? CDR(p) = car(cdr(t)) : err((int) ERR_NOT_PAIR);
}

L f_read(L t, L *_) {
  // TODO
  *ps = 0;
  return nil;
}

void print(L);
L f_print(L t, L *_) {
  for (; T(t) != NIL; t = cdr(t))
    print(car(t));
  return nil;
}

L f_println(L t, L *e) {
  f_print(t, e);
  putc('\n', out);
  return nil;
}

L f_write(L t, L *_) {
  L x;
  for (; T(t) != NIL; t = cdr(t)) {
    x = car(t);
    if (T(x) == STRG)
      fprintf(out, "%s", A+ord(x));
    else
      print(x);
  }
  return nil;
}

L f_string(L t, L *_) {
  I i, j; L s;
  char tmp[256];
  for (i = 0, s = t; T(s) != NIL; s = cdr(s)) {
    L x = car(s);
    if ((T(x) & ~(ATOM^STRG)) == ATOM)
      i += strlen(A+ord(x));
    else if (T(x) == CONS)
      for (; T(x) == CONS; x = cdr(x))
        ++i;
    else if (x == x) /* false when x is NaN i.e. a tagged Lisp expression */
      i += snprintf(tmp, sizeof(tmp), FLOAT, x);
  }
  i = j = alloc(i);
  for (s = t; T(s) != NIL; s = cdr(s)) {
    L x = car(s);
    if ((T(x) & ~(ATOM^STRG)) == ATOM)
      i += strlen(strcpy(A+i, A+ord(x)));
    else if (T(x) == CONS)
      for (; T(x) == CONS; x = cdr(x))
        *(A+i++) = car(x);
    else if (x == x) /* false when x is NaN i.e. a tagged Lisp expression */
      i += snprintf(A+i, sizeof(tmp), FLOAT, x);
  }
  return box(STRG, j);
}

void load(const char *filename);
L f_load(L t, L *e) {
  L x = f_string(t, e);
  const char *filename = A+ord(x);
  load(filename);
  return nil;
}

L f_trace(L t, L *e) {
  I savedtr = tr;
  tr = T(t) == NIL ? 1 : car(t);
  return more(t) ? t = eval(car(cdr(t)), *e), tr = savedtr, t : tr;
}

L f_catch(L t, L *e) {
  L x; I savedsp = sp;
  jmp_buf savedjb;
  memcpy(savedjb, jb, sizeof(jb));
  x = setjmp(jb);
  x = x ? cons(atom("ERR"), x) : eval(car(t), *e);
  memcpy(jb, savedjb, sizeof(jb));
  sp = savedsp;
  return x;
}

__attribute__((noreturn)) L f_throw(L t, L *_) {
  longjmp(jb, num(car(t)));
}

__attribute__((noreturn)) L f_quit(L t, L *_) {
  exit(0);
}

/* SDL3 primitives */
L f_sdl_clear(L t, L *_) {
  if (!sdl_renderer) return nil;
  SDL_SetRenderDrawColor(sdl_renderer, current_r, current_g, current_b, current_a);
  SDL_RenderClear(sdl_renderer);
  return tru;
}

L f_sdl_present(L t, L *_) {
  if (!sdl_renderer) return nil;
  SDL_RenderPresent(sdl_renderer);
  SDL_PollEvent(NULL);
  return tru;
}

L f_sdl_color(L t, L *_) {
  if (!sdl_renderer) return nil;
  current_r = (int)num(car(t));
  current_g = (int)num(car(cdr(t)));
  current_b = (int)num(car(cdr(cdr(t))));
  L a_arg = cdr(cdr(cdr(t)));
  current_a = T(a_arg) == NIL ? 255 : (int)num(car(a_arg));
  SDL_SetRenderDrawColor(sdl_renderer, current_r, current_g, current_b, current_a);
  return tru;
}

L f_sdl_rect(L t, L *_) {
  if (!sdl_renderer) return nil;
  SDL_FRect rect;
  rect.x = num(car(t));
  rect.y = num(car(cdr(t)));
  rect.w = num(car(cdr(cdr(t))));
  rect.h = num(car(cdr(cdr(cdr(t)))));
  SDL_RenderFillRect(sdl_renderer, &rect);
  return tru;
}

L f_sdl_line(L t, L *_) {
  if (!sdl_renderer) return nil;
  float x1 = num(car(t));
  float y1 = num(car(cdr(t)));
  float x2 = num(car(cdr(cdr(t))));
  float y2 = num(car(cdr(cdr(cdr(t)))));
  SDL_RenderLine(sdl_renderer, x1, y1, x2, y2);
  return tru;
}

L f_sdl_delay(L t, L *_) {
  SDL_Delay((unsigned int)num(car(t)));
  return tru;
}

L f_load_font(L t, L *_) {
  L filename = car(t);
  L ptsize = car(cdr(t));

  if (T(filename) != ATOM && T(filename) != STRG) return err((int) ERR_UNBOUND);

  if (current_font) {
    TTF_CloseFont(current_font);
    current_font = NULL;
  }

  current_font = TTF_OpenFont(A+ord(filename), (float)num(ptsize));
  if (!current_font) {
    fprintf(stderr, "Error loading font: %s\n", SDL_GetError());
    return nil;
  }

  return tru;
}

L f_text(L t, L *_) {
  if (!sdl_renderer || !current_font) return nil;

  float x = num(car(t));
  float y = num(car(cdr(t)));
  L text_atom = car(cdr(cdr(t)));

  if (T(text_atom) != ATOM && T(text_atom) != STRG) return err((int) ERR_UNBOUND);
  const char *text_str = A+ord(text_atom);
  if (!strlen(text_str)) return tru;

  SDL_Color text_color;
  text_color.r = current_r;
  text_color.g = current_g;
  text_color.b = current_b;
  text_color.a = current_a;
  SDL_Surface *text_surface = TTF_RenderText_Blended(current_font, text_str, 0, text_color);
  if (!text_surface) {
    fprintf(stderr, "Error rendering text: %s\n", SDL_GetError());
    return nil;
  }

  SDL_Texture *text_texture = SDL_CreateTextureFromSurface(sdl_renderer, text_surface);
  if (!text_texture) {
    SDL_DestroySurface(text_surface);
    fprintf(stderr, "Error creating texture: %s\n", SDL_GetError());
    return nil;
  }

  int text_w = text_surface->w;
  int text_h = text_surface->h;
  SDL_FRect dest = {x, y, (float)text_w, (float)text_h};

  SDL_RenderTexture(sdl_renderer, text_texture, NULL, &dest);

  SDL_DestroyTexture(text_texture);
  SDL_DestroySurface(text_surface);

  return tru;
}

L f_text_width(L t, L *_) {
  if (!current_font) return num(0);

  L text_atom = car(t);
  if (T(text_atom) != ATOM && T(text_atom) != STRG) return err((int) ERR_UNBOUND);
  const char *text_str = A+ord(text_atom);

  int w = 0, h = 0;
  if (!TTF_GetStringSize(current_font, text_str, 0, &w, &h)) {
    fprintf(stderr, "Error measuring text: %s\n", SDL_GetError());
    return num(0);
  }

  return num(w);
}

L f_text_height(L t, L *_) {
  if (!current_font) return num(0);

  L text_atom = car(t);
  if (T(text_atom) != ATOM && T(text_atom) != STRG) return err((int) ERR_UNBOUND);
  const char *text_str = A+ord(text_atom);

  int w = 0, h = 0;
  if (!TTF_GetStringSize(current_font, text_str, 0, &w, &h)) {
    fprintf(stderr, "Error measuring text: %s\n", SDL_GetError());
    return num(0);
  }

  return num(h);
}

L f_env_get_platform(L t, L *_) {
    const char* platformRaw = SDL_GetPlatform();
    if(platformRaw == 0)
        return nil;
    else
        return string(platformRaw);
}


L f_window_set_title(L t, L *_) {
   // Set the window title verbatim (expects valid UTF-8)
   L text_atom = car(t);
   if (T(text_atom) != ATOM && T(text_atom) != STRG) return err((int) ERR_UNBOUND);
   const char *text_str = A+ord(text_atom);
   if(! SDL_SetWindowTitle(sdl_window, text_str)) {
     fprintf(stderr, "Error setting window title: %s\n", SDL_GetError());
     return nil;
   }

   return tru;
}

L f_key_down(L t, L *_) {
  int keycode = (int)num(car(t));
  int scancode = SDL_GetScancodeFromKey(keycode, &modifiers);
  // May be brittle relative to events? See doc notes on event handling.
  const bool *state = SDL_GetKeyboardState(NULL);
  return state && state[scancode] ? tru : nil;
}

L f_key_name(L t, L *_) {
  int keycode = (int)num(car(t)); 
  const char* keyNameRaw = SDL_GetKeyName(keycode);
  if(keyNameRaw == 0)
       return nil;
  else
       return string(keyNameRaw);
}

L f_mouse_x(L t, L *_) {
  float x, y;
  SDL_GetMouseState(&x, &y);
  return num((int)x);
}

L f_mouse_y(L t, L *_) {
  float x, y;
  SDL_GetMouseState(&x, &y);
  return num((int)y);
}

L f_mouse_button(L t, L *_) {
  int button = (int)num(car(t));
  Uint32 state = SDL_GetMouseState(NULL, NULL);

  int pressed = 0;
  switch(button) {
    case 1: pressed = (state & SDL_BUTTON_LMASK) != 0; break;
    case 2: pressed = (state & SDL_BUTTON_MMASK) != 0; break;
    case 3: pressed = (state & SDL_BUTTON_RMASK) != 0; break;
    // Extra buttons are present on some pointing devices
    case 4: pressed = (state & SDL_BUTTON_X1MASK) != 0; break;
    case 5: pressed = (state & SDL_BUTTON_X2MASK) != 0; break;
  }
  return pressed ? tru : nil;
}

L f_mouse_wheel_x(L t, L *_) {
  return num(mouse_wheel_x);
}

L f_mouse_wheel_y(L t, L *_) {
  return num(mouse_wheel_y);
}

/* table of Lisp primitives, each has a name s, a function pointer f, and an evaluation mode m */
struct {
  const char *s;
  L (*f)(L, L*);
  enum { NORMAL, SPECIAL, TAILCALL } m;
} prim[] = {
  {"type",     f_type,    NORMAL},              /* (type x) => <type> value between -1 and 7 */
  {"eval",     f_ident,   NORMAL|TAILCALL},     /* (eval <quoted-expr>) => <value-of-expr> */
  {"quote",    f_ident,   SPECIAL},             /* (quote <expr>) => <expr> -- protect <expr> from evaluation */
  {"cons",     f_cons,    NORMAL},              /* (cons x y) => (x . y) -- construct a pair */
  {"car",      f_car,     NORMAL},              /* (car <pair>) => x -- "deconstruct" <pair> (x . y) */
  {"cdr",      f_cdr,     NORMAL},              /* (cdr <pair>) => y -- "deconstruct" <pair> (x . y) */
  // Basic arithmetic
  {"+",        f_add,     NORMAL},              /* (+ n1 n2 ... nk) => n1+n2+...+nk */
  {"-",        f_sub,     NORMAL},              /* (- n1 n2 ... nk) => n1-n2-...-nk or -n1 if k=1 */
  {"*",        f_mul,     NORMAL},              /* (* n1 n2 ... nk) => n1*n2*...*nk */
  {"/",        f_div,     NORMAL},              /* (/ n1 n2 ... nk) => n1/n2/.../nk or 1/n1 if k=1 */
  {"int",      f_int,     NORMAL},              /* (int <integer.frac>) => <integer> */
  // Boolean logic
  {"<",        f_lt,      NORMAL},              /* (< n1 n2) => #t if n1<n2 else () */
  {"iso",      f_iso,     NORMAL},              /* (iso x y) => structural equality */
  {"not",      f_not,     NORMAL},              /* (not x) => #t if x==() else () */
  {"or",       f_or,      SPECIAL},             /* (or x1 x2 ... xk) => #t if any x1 is not () else () */
  {"and",      f_and,     SPECIAL},             /* (and x1 x2 ... xk) => #t if all x1 are not () else () */
  // Bit-level binary logic operators
  {"&",        f_bin_and, NORMAL},              /* (& a b) => binary and of values. */
  {"^",        f_bin_xor, NORMAL},              /* (^ a b) => binary and of values. */
  {"|",        f_bin_or,  NORMAL},              /* (| a b) => binary and of values. */
  {">>",       f_bin_shr, NORMAL},              /* (& a b) => shift a right by b bits (both treated as ints) */
  {"<<",       f_bin_shl, NORMAL},              /* (& a b) => shift a left by by bits (both treated as ints) */
  // Flow control and data
  {"list",     f_list,    NORMAL},              /* (list x1 x2 ... xk) => (x1 x2 ... xk) -- evaluates x1, x2 ... xk */
  {"begin",    f_begin,   SPECIAL|TAILCALL},    /* (begin x1 x2 ... xk) => xk -- evaluates x1, x2 to xk */
  {"while",    f_while,   SPECIAL},             /* (while x y1 y2 ... yk) -- while x is not () evaluate y1, y2 ... yk */
  {"cond",     f_cond,    SPECIAL|TAILCALL},    /* (cond (x1 y1) (x2 y2) ... (xk yk)) => yi for first xi!=() */
  {"if",       f_if,      SPECIAL|TAILCALL},    /* (if x y z) => if x!=() then y else z */
  {"lambda",   f_lambda,  SPECIAL},             /* (lambda <parameters> <expr>) => {closure} */
  {"macro",    f_macro,   SPECIAL},             /* (macro <parameters> <expr>) => [macro] */
  {"define",   f_define,  SPECIAL},             /* (define <symbol> <expr>) -- globally defines <symbol> */
  {"assoc",    f_assoc,   NORMAL},              /* (assoc <quoted-symbol> <environment>) => <value-of-symbol> */
  {"env",      f_env,     NORMAL},              /* (env) => <environment> */
  {"let",      f_let,     SPECIAL|TAILCALL},    /* (let (v1 x1) (v2 x2) ... (vk xk) y) => y with scope of bindings */
  {"let*",     f_leta,    SPECIAL|TAILCALL},    /* (let* (v1 x1) (v2 x2) ... (vk xk) y) => y with scope of bindings */
  {"letrec",   f_letrec,  SPECIAL|TAILCALL},    /* (letrec (v1 x1) (v2 x2) ... (vk xk) y) => y with recursive scope */
  {"letrec*",  f_letreca, SPECIAL|TAILCALL},    /* (letrec* (v1 x1) (v2 x2) ... (vk xk) y) => y with recursive scope */
  {"setq",     f_setq,    SPECIAL},             /* (setq <symbol> x) -- changes value of <symbol> in scope to x */
  {"set-car!", f_setcar,  NORMAL},              /* (set-car! <pair> x) -- changes car of <pair> to x in memory */
  {"set-cdr!", f_setcdr,  NORMAL},              /* (set-cdr! <pair> y) -- changes cdr of <pair> to y in memory */
  {"read",     f_read,    NORMAL},              /* (read) => <value-of-input> */
  {"print",    f_print,   NORMAL},              /* (print x1 x2 ... xk) => () -- prints the values x1 x2 ... xk */
  {"println",  f_println, NORMAL},              /* (println x1 x2 ... xk) => () -- prints with newline */
  {"write",    f_write,   NORMAL},              /* (write x1 x2 ... xk) => () -- prints without quoting strings */
  {"string",   f_string,  NORMAL},              /* (string x1 x2 ... xk) => <string> -- string of x1 x2 ... xk */
  {"load",     f_load,    NORMAL},              /* (load <name>) -- loads file <name> (an atom or string name) */
  {"trace",    f_trace,   SPECIAL},             /* (trace flag [<expr>]) -- flag 0=off, 1=on, 2=keypress */
  {"catch",    f_catch,   SPECIAL},             /* (catch <expr>) => <value-of-expr> if no exception else (ERR . n) */
  {"throw",    f_throw,   NORMAL},              /* (throw n) -- raise exception error code n (integer != 0) */
  {"quit",     f_quit,    NORMAL},              /* (quit) -- bye! */
  // Graphics
  {"clear",    f_sdl_clear,   NORMAL},          /* (clear) -- clear screen with current color */
  {"present",  f_sdl_present, NORMAL},          /* (present) -- update display */
  {"color",    f_sdl_color,   NORMAL},          /* (color r g b [a]) -- set drawing color (0-255) */
  {"rect",     f_sdl_rect,    NORMAL},          /* (rect x y w h) -- draw filled rectangle */
  {"line",     f_sdl_line,    NORMAL},          /* (line x1 y1 x2 y2) -- draw line */
  {"delay",    f_sdl_delay,   NORMAL},          /* (delay ms) -- delay milliseconds */
  {"load-font",     f_load_font,     NORMAL},   /* (load-font path size) -- load TrueType font */
  {"text",          f_text,          NORMAL},   /* (text x y string) -- render text at position */
  {"text-width",    f_text_width,    NORMAL},   /* (text-width string) -- get text width in pixels */
  {"text-height",   f_text_height,   NORMAL},   /* (text-height string) -- get text height in pixels */
  {"key-down?",     f_key_down,      NORMAL},   /* (key-down? keycode) -- check if key pressed */
  {"key-name",      f_key_name,      NORMAL},   /* (key-name keycode) -- get the name for a keycode or "" if none known. */
  {"mouse-x",       f_mouse_x,       NORMAL},   /* (mouse-x) -- get mouse X position */
  {"mouse-y",       f_mouse_y,       NORMAL},   /* (mouse-y) -- get mouse Y position */
  {"mouse-button?", f_mouse_button,  NORMAL},   /* (mouse-button? btn) -- check mouse button state */
  {"mouse-wheel-x", f_mouse_wheel_x, NORMAL},   /* (mouse-wheel-x) -- get horizontal wheel movement */
  {"mouse-wheel-y", f_mouse_wheel_y, NORMAL},   /* (mouse-wheel-y) -- get vertical wheel movement */
  {"window-set-title", f_window_set_title,  NORMAL}, /* (window-set-title string) set the window title to a string. */
  {"get-platform", f_env_get_platform, NORMAL}, /* (get-platform) get the SDL3 platform string ("Windows", "macOS", "Linux", etc.) */
  {0}
};

/*----------------------------------------------------------------------------*\
 |      EVAL                                                                  |
\*----------------------------------------------------------------------------*/

/* when tracing is enabled, then display the evaluation of w => x */
void trace(L w, L x) {
  printf("\e[32m%4u: \e[33m", N-sp); print(w);  /* <stack depth>: unevaluated expression */
  printf("\e[36m => \e[33m");        print(x);  /* => value of the expression */
  printf("\e[m\t");
  if (tr > 1)                                   /* wait for ENTER key or other CTRL */
    while (getchar() >= ' ')
      continue;
  else
    putchar('\n');
}

/* evaluate x in environment e, returns value of x, tail-call optimized */
L eval(L x, L e) {
  L *f, v, w, *d, *y, *z; I k = sp;             /* save sp to unwind the stack back to sp afterwards */
  f = push(nil);                                /* protect closure f from getting GC'ed */
  d = push(nil);                                /* protect new bindings d from getting GC'ed */
  y = push(nil);                                /* protect alias y of new x from getting GC'ed */
  z = push(nil);                                /* protect alias z of new e from getting GC'ed */
  while (1) {
    w = x;                                      /* save x to trace w => x when tracing is enabled */
    if (T(x) == ATOM) {                         /* if x is an atom, then return its associated value */
      x = assoc(x, e);
      break;
    }
    if (T(x) != CONS)                           /* if x is not a list or pair, then return x itself */
      break;
    *f = eval(car(x), e);                       /* the function/primitive is at the head of the list */
    x = cdr(x);                                 /* ... and its actual arguments are the rest of the list */
    if (T(*f) == PRIM) {                        /* if f is a primitive, then apply it to the actual arguments x */
      I i = ord(*f);
      if (!(prim[i].m & SPECIAL))               /* if the primitive is NORMAL mode, */
        x = *y = evlis(x, e);                   /* ... then evaluate actual arguments x */
      *z = e;
      x = *y = prim[i].f(x, z);                 /* call the primitive with arguments x, put return value back in x */
      e = *z;                                   /* the new environment e is d to evaluate x, put in *z to protect */
      if (!(prim[i].m & TAILCALL))              /* if the primitive is TAILCALL mode, then continue */
        break;                                  /* else break to return value x */
    }
    else if (T(*f) == CLOS) {                   /* if f is a closure, then */
      *d = cdr(*f);                             /* construct an extended local environment d from f's static scope */
      if (T(*d) == NIL)                         /* if f's static scope is nil, then use global env as static scope */
        *d = env;
      v = car(car(*f));                         /* get the parameters v of closure f */
      while (T(v) == CONS && T(x) == CONS) {    /* bind parameters v to argument values x to extend the local scope d */
        *d = pair(car(v), eval(car(x), e), *d); /* add new binding to the front of d */
        v = cdr(v);
        x = cdr(x);
      }
      if (T(v) == CONS) {                       /* continue binding v if x is after a dot (... . x) by evaluating x */
        x = *y = eval(x, e);                    /* evaluate x and save its value y to protect it from getting GC'ed */
        while (T(v) == CONS && T(x) == CONS) {
          *d = pair(car(v), car(x), *d);        /* add new binding to the front of d */
          v = cdr(v);
          x = cdr(x);
        }
        if (T(v) == CONS)                       /* error if insufficient actual arguments x are provided */
          err((int) ERR_ARGUMENTS);
      }
      else if (T(x) == CONS)                    /* if more arguments x are provided then evaluate them all */
        x = evlis(x, e);
      else if (T(x) != NIL)                     /* else if last argument x is after a dot (... . x) then evaluate x */
        x = eval(x, e);
      if (T(v) != NIL)                          /* if last parameter v is after a dot (... . v) then bind it to x */
        *d = pair(v, x, *d);
      x = *y = cdr(car(*f));                    /* tail recursion optimization: evaluate the body x of closure f next */
      e = *z = *d;                              /* the new environment e is d to evaluate x, put in *z to protect */
    }
    else if (T(*f) == MACR) {                   /* else if f is a macro, then */
      *d = env;                                 /* construct an extended local environment d from global env */
      v = car(*f);                              /* get the parameters v of macro f */
      while (T(v) == CONS && T(x) == CONS) {    /* bind parameters v to arguments x to extend the local scope d */
        *d = pair(car(v), car(x), *d);
        v = cdr(v);
        x = cdr(x);
      }
      if (T(v) == CONS)                         /* error if insufficient actual arguments x are provided */
        err((int) ERR_ARGUMENTS);
      if (T(v) != NIL)                          /* if last parameter v is after a dot (... . v) then bind it to x */
        *d = pair(v, x, *d);
      x = *y = eval(cdr(*f), *d);               /* evaluated body of the macro to evaluate next, put in *y to protect */
    }
    else
      err((int) ERR_CANT_APPLY);                /* if f is not a closure or macro, then we cannot apply it */
    if (tr)                                     /* if tracing is enabled then display evaluation step w => x */
      trace(w, x);
  }
  unwind(k);                                    /* unwind the stack to allow GC to collect unused temporaries */
  if (tr)                                       /* if tracing is enabled then display evaluation step w => x */
    trace(w, x);
  return x;                                     /* return x evaluated */
}

/*----------------------------------------------------------------------------*\
 |      PRINT                                                                 |
\*----------------------------------------------------------------------------*/

/* output Lisp list t */
void printlist(L t) {
  putc('(', out);
  while (1) {
    print(car(t));
    t = cdr(t);
    if (T(t) == NIL)
      break;
    if (T(t) != CONS) {
      fprintf(out, " . ");
      print(t);
      break;
    }
    putc(' ', out);
  }
  putc(')', out);
}

/* output Lisp expression x */
void print(L x) {
  switch (T(x)) {
    case NIL:  fprintf(out, "()");                   break;
    case PRIM: fprintf(out, "<%s>", prim[ord(x)].s); break;
    case ATOM: fprintf(out, "%s", A+ord(x));         break;
    case STRG: fprintf(out, "\"%s\"", A+ord(x));     break;
    case CONS: printlist(x);                         break;
    case CLOS: fprintf(out, "{%u}", ord(x));         break;
    case MACR: fprintf(out, "[%u]", ord(x));         break;
    default:   fprintf(out, FLOAT, x);               break;
  }
}

/*----------------------------------------------------------------------------*\
 |      REPL                                                                  |
\*----------------------------------------------------------------------------*/

/* readline callback for non-blocking input */
void readline_callback(char *input_line) {
  if (input_line == NULL) {
    /* EOF (ctrl+d) */
    running = false;
    return;
  }

  if (*input_line) {
    add_history(input_line);
  }

  pending_line = input_line;
  line_ready = true;
}

/* check if stdin has data ready to read */
int stdin_ready() {
  fd_set readfds;
  struct timeval timeout;

  FD_ZERO(&readfds);
  FD_SET(STDIN_FILENO, &readfds);

  timeout.tv_sec = 0;
  timeout.tv_usec = 0;

  int result = select(STDIN_FILENO + 1, &readfds, NULL, NULL, &timeout);
  return result > 0 && FD_ISSET(STDIN_FILENO, &readfds);
}

void load(const char *filename) {
  FILE *f = fopen(filename, "r");
  if (!f) {
    printf("\e[31;1mError: cannot open %s\e[m\n", filename);
    return;
  }
  jmp_buf saved_jb;
  memcpy(saved_jb, jb, sizeof(jmp_buf));
  int catch;
  if ((catch = setjmp(jb)) == 0) {
    while (!feof(f)) {
      L expr = read_b(f);
      push(expr);
      eval(cell[sp], env);
      pop();
    }
  } else {
    errorInLocation(filename, catch);
  }
  fclose(f);
  memcpy(jb, saved_jb, sizeof(jmp_buf));
}

void noisy_load(const char *filename) {
  printf("Loading %s...", filename);
  fflush(stdout);
  load(filename);
  printf(" done\n");
}

/* entry point with Lisp initialization, error handling and REPL */
int main(int argc, char **argv) {
  int i, catch;
  printf("Lisp with SDL3 Graphics\n\n");

  /* Initialize SDL3 */
  if (!SDL_Init(SDL_INIT_VIDEO)) {
    errorInSDLInit("SDL_Init");
    return 1;
  }

  if (!TTF_Init()) {
    errorInSDLInit("TTF_Init");
    SDL_Quit();
    return 1;
  }

  /* Create window and renderer */
  if (!SDL_CreateWindowAndRenderer("Lisp SDL3 Graphics", 800, 600,
                                     SDL_WINDOW_RESIZABLE, &sdl_window, &sdl_renderer)) {
    errorInSDLInit("Window and renderer creation");
    TTF_Quit();
    SDL_Quit();
    return 1;
  }

  SDL_SetRenderDrawBlendMode(sdl_renderer, SDL_BLENDMODE_BLEND);
  current_font = TTF_OpenFont("DejaVuSans", 20);

  printf("Available graphics commands:\n");
  printf("  (clear)             - clear screen with current color\n");
  printf("  (color r g b [a])   - set drawing color (0-255)\n");
  printf("  (rect x y w h)      - draw filled rectangle\n");
  printf("  (line x1 y1 x2 y2)  - draw line\n");
  printf("  (present)           - update display\n");
  printf("  (delay ms)          - delay milliseconds\n");
  printf("\n");
  printf("Text rendering:\n");
  printf("  (load-font path size)   - load TrueType font\n");
  printf("  (text x y string)       - draw text at position\n");
  printf("  (text-width string)     - get text width in pixels\n");
  printf("  (text-height string)    - get text height in pixels\n");
  printf("\n");
  printf("Input (keyboard and mouse):\n");
  printf("  (key-down? keycode)     - check if key pressed (SDL keycode)\n");
  printf("  (key-name  keycode)     - get a string name for a keycode (may be blank)\n");
  printf("  (mouse-x)               - get mouse X position\n");
  printf("  (mouse-y)               - get mouse Y position\n");
  printf("  (mouse-button? btn)     - check mouse button (1=left, 2=mid, 3=right)\n");
  printf("  (mouse-wheel-x)         - get horizontal wheel movement this frame\n");
  printf("  (mouse-wheel-y)         - get vertical wheel movement this frame\n");
  printf("\n");
  printf("Callbacks:\n");
  printf("  (define draw (lambda () ...))     - called each frame\n");
  printf("  (define update (lambda (dt) ...)) - called each frame with delta time\n");
  printf("  (define keypressed (lambda (keycode modifiers isrepeat scancode) ...))\n");
  printf("  (define keyreleased (lambda (keycode modifiers scancode) ...))\n");
  printf("  (define mousepressed (lambda (x y button) ...))\n");
  printf("  (define mousereleased (lambda (x y button) ...))\n");
  printf("  (define mousemoved (lambda (x y dx dy) ...))\n");
  printf("  (define wheelmoved (lambda (x y) ...))\n");
  printf("\n");
  printf("Application state:\n");
  printf("  (quit)                      - quit the program and close the window.\n");
  printf("  (window-set-title string)   - set the title of the window.\n");
  printf("  (get-platform)              - get the SDL3 platform string (\"Windows\", \"macOS\", \"Linux\", etc.)\n");
  printf("\n");

  /* Initialize Lisp environment */
  out = stdout;
  if (setjmp(jb))                               /* if something goes wrong before REPL, it is fatal */
    abort();
  sweep();                                      /* clear the pool and heap */
  nil = box(NIL, 0);                            /* set the constant nil (empty list) */
  tru = atom("#t");                             /* set the constant #t */
  env = pair(tru, tru, nil);                    /* create environment with symbolic constant #t */
  for (i = 0; prim[i].s; ++i)                   /* expand environment with primitives */
    env = pair(atom(prim[i].s), box(PRIM, i), env);

  /* Callback symbols */
  L draw_sym = atom("draw");
  L update_sym = atom("update");
  L keypressed_sym = atom("keypressed");
  L keyreleased_sym = atom("keyreleased");
  L mousepressed_sym = atom("mousepressed");
  L mousereleased_sym = atom("mousereleased");
  L mousemoved_sym = atom("mousemoved");
  L wheelmoved_sym = atom("wheelmoved");

  /* Pre-allocate callback expressions and protect from GC */
  L draw_expr = cons(draw_sym, nil);
  env = pair(atom("__draw_expr__"), draw_expr, env);

  L update_args = cons(num(0), nil);
  L update_expr = cons(update_sym, update_args);
  env = pair(atom("__update_expr__"), update_expr, env);

  L keypressed_args = cons(num(0), cons(num(0), cons(nil, cons(num(0), nil))));
  L keypressed_expr = cons(keypressed_sym, keypressed_args);
  env = pair(atom("__keypressed_expr__"), keypressed_expr, env);

  L keyreleased_args = cons(num(0), cons(num(0), cons(num(0), nil)));
  L keyreleased_expr = cons(keyreleased_sym, keyreleased_args);
  env = pair(atom("__keyreleased_expr__"), keyreleased_expr, env);

  L mousepressed_args = cons(num(0), cons(num(0), cons(nil, nil)));
  L mousepressed_expr = cons(mousepressed_sym, mousepressed_args);
  env = pair(atom("__mousepressed_expr__"), mousepressed_expr, env);

  L mousereleased_args = cons(num(0), cons(num(0), cons(num(0), nil)));
  L mousereleased_expr = cons(mousereleased_sym, mousereleased_args);
  env = pair(atom("__mousereleased_expr__"), mousereleased_expr, env);

  L mousemoved_args = cons(num(0), cons(num(0), cons(num(0), cons(num(0), nil))));
  L mousemoved_expr = cons(mousemoved_sym, mousemoved_args);
  env = pair(atom("__mousemoved_expr__"), mousemoved_expr, env);

  L wheelmoved_args = cons(num(0), cons(num(0), nil));
  L wheelmoved_expr = cons(wheelmoved_sym, wheelmoved_args);
  env = pair(atom("__wheelmoved_expr__"), wheelmoved_expr, env);

  /* Load init file */
  noisy_load("init.lisp");

  /* Load all command-line files */
  for (i = 1; i < argc; ++i) {
    noisy_load(argv[i]);
  }

  /* Set up non-blocking REPL */
  using_history();
  BREAK_ON;                                     /* enable CTRL-C break to throw error 2 */

  snprintf(ps, sizeof(ps), "> ");
  printf("\nType Lisp expressions or '(quit)' to quit.\n");
  rl_callback_handler_install(ps, readline_callback);

  /* Main event loop */
  SDL_Event event;
  Uint64 last_time = SDL_GetTicks();

  while (running) {
    gc();

    /* Check for stdin input */
    if (stdin_ready())
      rl_callback_read_char();

    /* Process a complete line if ready */
    if (line_ready && pending_line) {
      line_ready = false;

      /* Remove handler to prevent prompt during output */
      rl_callback_handler_remove();

      /* Accumulate input if we're in multi-line mode */
      if (accumulated_input) {
        size_t old_len = strlen(accumulated_input);
        size_t new_len = strlen(pending_line);
        accumulated_input = realloc(accumulated_input, old_len + new_len + 2);
        accumulated_input[old_len] = '\n';
        strcpy(accumulated_input + old_len + 1, pending_line);
      } else {
        accumulated_input = strdup(pending_line);
      }

      free(pending_line);
      pending_line = NULL;

      /* Try to parse the accumulated input */
      bool incomplete = false;
      if ((catch = setjmp(jb)) > 0) {
        errorInLocation("REPL input", catch);
        free(accumulated_input);
        accumulated_input = NULL;
        incomplete = false;
      } else {
        L x = read_i(accumulated_input, &incomplete);
        if (!incomplete) {
          if (T(x) != NIL) {
            printf("\r");
            rl_clear_visible_line();
            print(eval(x, env));
            printf("\n");
          }
          free(accumulated_input);
          accumulated_input = NULL;
        }
      }

      /* Reinstall handler with appropriate prompt */
      fflush(stdout);
      rl_callback_handler_install(incomplete ? "... " : "> ", readline_callback);
    }

    /* Reset mouse wheel state */
    mouse_wheel_x = 0.0f;
    mouse_wheel_y = 0.0f;

    /* Process SDL events */
    while (SDL_PollEvent(&event)) {
      if (event.type == SDL_EVENT_QUIT)
        running = false;

      if (event.type == SDL_EVENT_KEY_DOWN && bound(keypressed_sym, env)) {
        if ((catch = setjmp(jb)) == 0) {
          // printf("  (define keypressed (lambda (keycode modifiers isrepeat scancode) ...))\n");
          CAR(keypressed_args) = num(event.key.key);
          modifiers = event.key.mod;
          CAR(CDR(keypressed_args)) = num((int) modifiers);
          CAR(CDR(CDR(keypressed_args))) = event.key.repeat ? tru : nil;
          CAR(CDR(CDR(CDR(keypressed_args)))) = num(event.key.scancode);
          eval(keypressed_expr, env);

        } else {
          errorInLocation("keypressed", catch);
        }
      }

      if (event.type == SDL_EVENT_KEY_UP && bound(keyreleased_sym, env)) {
        if ((catch = setjmp(jb)) == 0) {
          CAR(keyreleased_args) = num(event.key.key);
          modifiers = event.key.mod;
          CAR(CDR(keyreleased_args)) = num((int) modifiers);
          CAR(CDR(CDR(CDR(keyreleased_args)))) = num(event.key.scancode);

          eval(keyreleased_expr, env);
        } else {
          errorInLocation("keyreleased", catch);
        }
      }

      if (event.type == SDL_EVENT_MOUSE_BUTTON_DOWN && bound(mousepressed_sym, env)) {
        if ((catch = setjmp(jb)) == 0) {
          CAR(mousepressed_args) = num((int)event.button.x);
          CAR(CDR(mousepressed_args)) = num((int)event.button.y);
          CAR(CDR(CDR(mousepressed_args))) = num(event.button.button);
          eval(mousepressed_expr, env);
        } else {
          errorInLocation("mousepressed", catch);
        }
      }

      if (event.type == SDL_EVENT_MOUSE_BUTTON_UP && bound(mousereleased_sym, env)) {
        if ((catch = setjmp(jb)) == 0) {
          CAR(mousereleased_args) = num((int)event.button.x);
          CAR(CDR(mousereleased_args)) = num((int)event.button.y);
          CAR(CDR(CDR(mousereleased_args))) = num(event.button.button);
          eval(mousereleased_expr, env);
        } else {
          errorInLocation("mousereleased", catch);
        }
      }

      if (event.type == SDL_EVENT_MOUSE_MOTION && bound(mousemoved_sym, env)) {
        if ((catch = setjmp(jb)) == 0) {
          CAR(mousemoved_args) = num((int)event.motion.x);
          CAR(CDR(mousemoved_args)) = num((int)event.motion.y);
          CAR(CDR(CDR(mousemoved_args))) = num((int)event.motion.xrel);
          CAR(CDR(CDR(CDR(mousemoved_args)))) = num((int)event.motion.yrel);
          eval(mousemoved_expr, env);
        } else {
          errorInLocation("mousemoved", catch);
        }
      }

      if (event.type == SDL_EVENT_MOUSE_WHEEL && bound(wheelmoved_sym, env)) {
        if ((catch = setjmp(jb)) == 0) {
          mouse_wheel_x += event.wheel.x;
          mouse_wheel_y += event.wheel.y;
          CAR(wheelmoved_args) = num((int)event.wheel.x);
          CAR(CDR(wheelmoved_args)) = num((int)event.wheel.y);
          eval(wheelmoved_expr, env);
        } else {
          errorInLocation("wheelmoved", catch);
        }
      }
    }

    /* Update callback */
    if (bound(update_sym, env)) {
      if ((catch = setjmp(jb)) == 0) {
        Uint64 current_time = SDL_GetTicks();
        CAR(update_args) = num((int)(current_time - last_time));
        last_time = current_time;
        eval(update_expr, env);
      } else {
        errorInLocation("update", catch);
      }
    }

    /* Draw callback */
    if (bound(draw_sym, env)) {
      if ((catch = setjmp(jb)) == 0) {
        SDL_SetRenderDrawColor(sdl_renderer, 0,0,0, 0);
        SDL_RenderClear(sdl_renderer);
        eval(draw_expr, env);
        SDL_RenderPresent(sdl_renderer);
      } else {
        errorInLocation("draw", catch);
      }
    }

    SDL_Delay(16);  /* ~60 FPS */
  }

  /* Cleanup */
  rl_callback_handler_remove();
  if (current_font)
    TTF_CloseFont(current_font);
  SDL_DestroyRenderer(sdl_renderer);
  SDL_DestroyWindow(sdl_window);
  TTF_Quit();
  SDL_Quit();
  return 0;
}
