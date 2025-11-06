/* TinyLisp with extras and SDL3 graphics by Kartik Agaram 2025 */
/* TinyLisp with extras by Robert A. van Engelen 2025 */

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <sys/select.h>
#include <unistd.h>
#include <fcntl.h>
#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>
#include <SDL3_ttf/SDL_ttf.h>

/* we only need two types to implement a Lisp interpreter:
        I      unsigned integer (either 16 bit, 32 bit or 64 bit unsigned)
        L      Lisp expression (double with NaN boxing)
   I variables and function parameters are named as follows:
        i      any unsigned integer, e.g. a NaN-boxed ordinal value or index
        t      a NaN-boxed tag
        a      dot operator argument flag, used with evarg()
   L variables and function parameters are named as follows:
        x,y    any Lisp expression
        n      number
        t,s    list
        f,g    function, a lambda closure or Lisp primitive or macro
        p      pair, a cons of two Lisp expressions
        e,d,h  environment, a list of pairs, e.g. created with (define v x)
        v      the name of a variable (an atom) or a list of variables */
#define I unsigned
#define L double

/* T(x) returns the tag bits of a NaN-boxed Lisp expression x */
#define T(x) *(unsigned long long*)&x>>48

/* address of the atom heap is at the bottom of the cell stack */
#define A (char*)cell

/* number of cells for the shared stack of cells and atom heap, increase N as desired */
#define N 8192

/* section 12: adding readline with history */
#include <readline/readline.h>
#include <readline/history.h>
FILE *in = NULL;
char buf[40],see = ' ',*ptr = "",*line = NULL,ps[20];

/* Readline callback state for non-blocking REPL */
char *pending_line = NULL;
bool line_ready = false;

/* forward proto declarations */
L eval(L,L),Read(),parse(),err(I,L); void print(L);

/* section 4: constructing Lisp expressions */
/* hp: top of the atom heap pointer, A+hp with hp=0 points to the first atom string in cell[]
   sp: cell stack pointer, the stack starts at the top of cell[] with sp=N
   tr: tracing off (0), on (1), wait on ENTER (2), dump and wait (3)
   safety invariant: hp <= sp<<3 */
I hp = 0,sp = N,tr = 0;
/* atom, primitive, cons, closure and nil tags for NaN boxing */
enum { ATOM = 0x7ff8,STR = 0xf779,PRIM = 0x7ffa,CONS = 0x7ffb,CLOS = 0x7ffc,MACR = 0x7ffd,NIL = 0x7ffe };
/* cell[N] array of Lisp expressions, shared by the stack and atom heap */
L cell[N];
/* Lisp constant expressions () (nil), #t, and the global environment env */
L nil,tru,env;
/* NaN-boxing specific functions:
   box(t,i): returns a new NaN-boxed double with tag t and ordinal i
   ord(x):   returns the ordinal of the NaN-boxed double x
   num(n):   convert or check number n (does nothing, e.g. could check for NaN)
   equ(x,y): returns nonzero if x equals y */
L box(I t,I i) { L x; *(unsigned long long*)&x = (unsigned long long)t<<48|i; return x; }
I ord(L x) { return *(unsigned long long*)&x; }
L num(L n) { return n; }
I equ(L x,L y) { return *(unsigned long long*)&x == *(unsigned long long*)&y; }
/* interning of atom names (Lisp symbols), returns a unique NaN-boxed ATOM */
L atom2(I t, const char *s) {
 assert(*s != '('); assert(*s != ')'); assert(*s != '\''); assert(*s != '`'); assert(*s != ','); assert(*s != '@');
 I i = 0; while (i < hp && strcmp(A+i,s)) i += strlen(A+i)+1;
 return i == hp && (hp += strlen(strcpy(A+i,s))+1) > sp<<3 ? err(4,nil) : box(t,i);
}
L str(const char *s) { return atom2(STR, s+1); }  /* strip leading quote */
L atom(const char *s) { return atom2(ATOM, s); }

/* section 14: error handling and exceptions
   ERR 1: not a pair
   ERR 2: unbound symbol
   ERR 3: cannot apply
   ERR 4: out of memory
   ERR 5: program stopped */
#include <setjmp.h>
#include <signal.h>
jmp_buf jb;

/* Get descriptive error message for error code */
const char* err_msg(I i) {
 switch(i) {
  case 1: return "not a pair";
  case 2: return "unbound symbol";
  case 3: return "cannot apply";
  case 4: return "out of memory";
  case 5: return "program stopped";
  default: return "unknown error";
 }
}

/* Print error with context and throw exception */
L err(I i,L x) {
 /* Always print ERR 2 (unbound symbol) with variable name */
 if (i == 2 && T(x) != NIL) {
  printf("\n\e[31;1mError: %s '",err_msg(i));
  print(x);
  printf("'\e[m\n");
 }
 /* For other errors, only print when tracing is enabled */
 else if (tr && T(x) != NIL) {
  printf("\n\e[31;1mError: %s: ",err_msg(i));
  print(x);
  printf("\e[m\n");
 }
 longjmp(jb,i);
}

/* construct pair (x . y) returns a NaN-boxed CONS */
L cons(L x,L y) { cell[--sp] = x; cell[--sp] = y; if (hp+16 > sp<<3) err(4,nil); return box(CONS,sp); }
/* return the car of a pair or throw err(1) if not a pair */
L car(L p) { return T(p) == CONS || T(p) == CLOS || T(p) == MACR ? cell[ord(p)+1] : err(1,p); }
/* return the cdr of a pair or throw err(1) if not a pair */
L cdr(L p) { return T(p) == CONS || T(p) == CLOS || T(p) == MACR ? cell[ord(p)] : err(1,p); }
/* construct a pair to add to environment e, returns the list ((v . x) . e) */
L pair(L v,L x,L e) { return cons(cons(v,x),e); }
/* construct a lambda closure with variables v body x environment e, returns a NaN-boxed CLOS */
L closure(L v,L x,L e) { return box(CLOS,ord(pair(v,x,equ(e,env) ? nil : e))); }
/* construct a macro with variables v body x, returns a NaN-boxed MACR */
L macro(L v,L x) { return box(MACR,ord(cons(v,x))); }
/* look up a symbol v in environment e, return its value or throw err(2) if not found */
L assoc(L v,L e) { while (T(e) == CONS && !equ(v,car(car(e)))) e = cdr(e); return T(e) == CONS ? cdr(car(e)) : err(2,v); }
L bound(L v,L e) { while (T(e) == CONS && !equ(v,car(car(e)))) e = cdr(e); return T(e) == CONS; }
/* not(x) is nonzero if x is the Lisp () empty list a.k.a. nil or false */
I not(L x) { return T(x) == NIL; }
/* let(x) is nonzero if x has more than one item, used by let* */
I let(L x) { return !not(x) && !not(cdr(x)); }

/* section 16.1: replacing recursion with loops */
L evlis(L t,L e) {
 L s,*p;
 for (s = nil,p = &s; T(t) == CONS; p = cell+sp,t = cdr(t)) *p = cons(eval(car(t),e),nil);
 if (T(t) == ATOM) *p = assoc(t,e);
 return s;
}

/* section 16.4: optimizing the Lisp primitives */
L evarg(L *t,L *e,I *a) {
 L x;
 if (T(*t) == ATOM) *t = assoc(*t,*e),*a = 1;
 x = car(*t); *t = cdr(*t);
 return *a ? x : eval(x,*e);
}

/* section 6 Lisp primitives (optimized with evarg per section 16.4) */
L f_eval(L t,L *e) { I a = 0; return evarg(&t,e,&a); }
L f_quote(L t,L *_) { return car(t); }
L f_cons(L t,L *e) { I a = 0; L x = evarg(&t,e,&a); return cons(x,evarg(&t,e,&a)); }
L f_car(L t,L *e) { I a = 0; return car(evarg(&t,e,&a)); }
L f_cdr(L t,L *e) { I a = 0; return cdr(evarg(&t,e,&a)); }
L f_add(L t,L *e) { I a = 0; L n = evarg(&t,e,&a); while (!not(t)) n += evarg(&t,e,&a); return num(n); }
L f_sub(L t,L *e) { I a = 0; L n = evarg(&t,e,&a); while (!not(t)) n -= evarg(&t,e,&a); return num(n); }
L f_mul(L t,L *e) { I a = 0; L n = evarg(&t,e,&a); while (!not(t)) n *= evarg(&t,e,&a); return num(n); }
L f_div(L t,L *e) { I a = 0; L n = evarg(&t,e,&a); while (!not(t)) n /= evarg(&t,e,&a); return num(n); }
L f_int(L t,L *e) { I a = 0; L n = evarg(&t,e,&a); return n<1e16 && n>-1e16 ? (long long)n : n; }
L f_lt(L t,L *e) { I a = 0; L n = evarg(&t,e,&a); return n - evarg(&t,e,&a) < 0 ? tru : nil; }
L f_eq(L t,L *e) { I a = 0; L x = evarg(&t,e,&a); return equ(x,evarg(&t,e,&a)) ? tru : nil; }
L f_pair(L t,L *e) { I a = 0; L x = evarg(&t,e,&a); return T(x) == CONS ? tru : nil; }
L f_or(L t,L *e) { I a = 0; L x = nil; while (!not(t) && not(x)) x = evarg(&t,e,&a); return x; }
L f_and(L t,L *e) { I a = 0; L x = tru; while (!not(t) && !not(x)) x = evarg(&t,e,&a); return x; }
L f_not(L t,L *e) { I a = 0; return not(evarg(&t,e,&a)) ? tru : nil; }
L f_cond(L t,L *e) { while (not(eval(car(car(t)),*e))) t = cdr(t); return car(cdr(car(t))); }
L f_if(L t,L *e) { return car(cdr(not(eval(car(t),*e)) ? cdr(t) : t)); }
L f_leta(L t,L *e) { for (; let(t); t = cdr(t)) *e = pair(car(car(t)),eval(car(cdr(car(t))),*e),*e); return car(t); }
L f_lambda(L t,L *e) { return closure(car(t),car(cdr(t)),*e); }
L f_define(L t,L *e) { env = pair(car(t),eval(car(cdr(t)),*e),env); return car(t); }

/* section 11: additional Lisp primitives (optimized with evarg() see section 16.4) */
L f_assoc(L t,L *e) { I a = 0; L x = evarg(&t,e,&a); return assoc(x,evarg(&t,e,&a)); }
L f_env(L _,L *e) { return *e; }
L f_let(L t,L *e) {
 L d = *e;
 for (; let(t); t = cdr(t)) *e = pair(car(car(t)),eval(car(cdr(car(t))),d),*e);
 return car(t);
}
L f_letreca(L t,L *e) {
 for (; let(t); t = cdr(t)) *e = pair(car(car(t)),nil,*e),cell[ord(car(*e))] = eval(car(cdr(car(t))),*e);
 return car(t);
}
L f_letrec(L t,L *e) {
 L s,d,*p;
 for (s = t,d = *e,p = &d; let(s); s = cdr(s),p = &cell[ord(*p)]) *p = pair(car(car(s)),nil,*e);
 for (*e = d; let(t); t = cdr(t),d = cdr(d)) cell[ord(car(d))] = eval(car(cdr(car(t))),*e);
 return car(t);
}
L f_setq(L t,L *e) {
 L d = *e,v = car(t),x = eval(car(cdr(t)),d);
 while (T(d) == CONS && !equ(v,car(car(d)))) d = cdr(d);
 return T(d) == CONS ? cell[ord(car(d))] = x : err(2,v);
}
L f_setcar(L t,L *e) {
 I a = 0; L p = evarg(&t,e,&a);
 return (T(p) == CONS) ? cell[ord(p)+1] = evarg(&t,e,&a) : err(1,p);
}
L f_setcdr(L t,L *e) {
 I a = 0; L p = evarg(&t,e,&a);
 return (T(p) == CONS) ? cell[ord(p)] = evarg(&t,e,&a) : err(1,p);
}
L f_macro(L t,L *_) { return macro(car(t),car(cdr(t))); }
L f_read(L t,L *_) { L x; char c = see; see = ' '; x = Read(); see = c; return x; }
L f_print(L t,L *e) { I a = 0; while (!not(t)) print(evarg(&t,e,&a)); return nil; }
L f_println(L t,L *e) { f_print(t,e); putchar('\n'); return nil; }

/* section 12: adding readline with history */
L f_load(L t,L *_) { L x = car(t); if (!in && T(x) == ATOM) in = fopen(A+ord(x),"r"); return x; }

/* section 14: error handling and exceptions */
L f_catch(L t,L *e) {
 L x; I i;
 jmp_buf savedjb;
 memcpy(savedjb,jb,sizeof(jb));
 if ((i = setjmp(jb)) == 0) x = eval(car(t),*e);
 memcpy(jb,savedjb,sizeof(jb));
 return i == 0 ? x : i == 4 ? err(4,nil) : cons(atom("ERR"),i);
}
L f_throw(L t,L *_) { longjmp(jb,(I)num(car(t))); }

/* section 13: execution tracing */
L f_trace(L t,L *_) { tr = not(t) ? !tr : (I)num(car(t)); return num(tr); }

/* SDL3 primitives */
SDL_Window *sdl_window = NULL;
SDL_Renderer *sdl_renderer = NULL;
bool sdl_initialized = false;
int current_r = 255, current_g = 255, current_b = 255, current_a = 255;
TTF_Font *current_font = NULL;
float mouse_wheel_x = 0.0f;
float mouse_wheel_y = 0.0f;

L f_sdl_clear(L t,L *_) {
 if (!sdl_renderer) return nil;
 SDL_SetRenderDrawColor(sdl_renderer, current_r, current_g, current_b, current_a);
 SDL_RenderClear(sdl_renderer);
 return tru;
}

L f_sdl_present(L t,L *_) {
 if (!sdl_renderer) return nil;
 SDL_RenderPresent(sdl_renderer);
 SDL_PollEvent(NULL);
 return tru;
}

L f_sdl_color(L t,L *e) {
 if (!sdl_renderer) return nil;
 I a = 0;
 current_r = (int)num(evarg(&t,e,&a));
 current_g = (int)num(evarg(&t,e,&a));
 current_b = (int)num(evarg(&t,e,&a));
 current_a = not(t) ? 255 : (int)num(evarg(&t,e,&a));
 SDL_SetRenderDrawColor(sdl_renderer, current_r, current_g, current_b, current_a);
 return tru;
}

L f_sdl_rect(L t,L *e) {
 if (!sdl_renderer) return nil;
 I a = 0;
 SDL_FRect rect;
 rect.x = num(evarg(&t,e,&a));
 rect.y = num(evarg(&t,e,&a));
 rect.w = num(evarg(&t,e,&a));
 rect.h = num(evarg(&t,e,&a));
 SDL_RenderFillRect(sdl_renderer, &rect);
 return tru;
}

L f_sdl_line(L t,L *e) {
 if (!sdl_renderer) return nil;
 I a = 0;
 float x1 = num(evarg(&t,e,&a));
 float y1 = num(evarg(&t,e,&a));
 float x2 = num(evarg(&t,e,&a));
 float y2 = num(evarg(&t,e,&a));
 SDL_RenderLine(sdl_renderer, x1, y1, x2, y2);
 return tru;
}

L f_sdl_delay(L t,L *e) {
 I a = 0;
 SDL_Delay((unsigned int)num(evarg(&t,e,&a)));
 return tru;
}

L f_load_font(L t,L *e) {
 I a = 0;
 L filename = evarg(&t,e,&a);
 L ptsize = evarg(&t,e,&a);

 if (T(filename) != ATOM) return err(2,filename);

 if (current_font) {
  TTF_CloseFont(current_font);
  current_font = NULL;
 }

 current_font = TTF_OpenFont(A+ord(filename), (float)num(ptsize));
 if (!current_font) {
  printf("Error loading font: %s\n", SDL_GetError());
  return nil;
 }

 return tru;
}

L f_text(L t,L *e) {
 if (!sdl_renderer || !current_font) return nil;

 I a = 0;
 float x = num(evarg(&t,e,&a));
 float y = num(evarg(&t,e,&a));
 L text_atom = evarg(&t,e,&a);

 if (T(text_atom) != ATOM) return err(2,text_atom);
 const char *text_str = A+ord(text_atom);

 SDL_Color text_color;
 text_color.r = current_r, text_color.g = current_g, text_color.b = current_b, text_color.a = current_a;
 SDL_Surface *text_surface = TTF_RenderText_Blended(current_font, text_str, 0, text_color);
 if (!text_surface) {
  printf("Error rendering text: %s\n", SDL_GetError());
  return nil;
 }

 SDL_Texture *text_texture = SDL_CreateTextureFromSurface(sdl_renderer, text_surface);
 if (!text_texture) {
  SDL_DestroySurface(text_surface);
  printf("Error creating texture: %s\n", SDL_GetError());
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

L f_text_width(L t,L *e) {
 if (!current_font) return num(0);

 I a = 0;
 L text_atom = evarg(&t,e,&a);

 if (T(text_atom) != ATOM) return err(2,text_atom);
 const char *text_str = A+ord(text_atom);

 int w = 0, h = 0;
 if (!TTF_GetStringSize(current_font, text_str, 0, &w, &h)) {
  printf("Error measuring text: %s\n", SDL_GetError());
  return num(0);
 }

 return num(w);
}

L f_text_height(L t,L *e) {
 if (!current_font) return num(0);

 I a = 0;
 L text_atom = evarg(&t,e,&a);

 if (T(text_atom) != ATOM) return err(2,text_atom);
 const char *text_str = A+ord(text_atom);

 int w = 0, h = 0;
 if (!TTF_GetStringSize(current_font, text_str, 0, &w, &h)) {
  printf("Error measuring text: %s\n", SDL_GetError());
  return num(0);
 }

 return num(h);
}

L f_key_down(L t,L *e) {
 I a = 0;
 int scancode = (int)num(evarg(&t,e,&a));
 const bool *state = SDL_GetKeyboardState(NULL);
 return state && state[scancode] ? tru : nil;
}

L f_mouse_x(L t,L *e) {
 float x, y;
 SDL_GetMouseState(&x, &y);
 return num((int)x);
}

L f_mouse_y(L t,L *e) {
 float x, y;
 SDL_GetMouseState(&x, &y);
 return num((int)y);
}

L f_mouse_button(L t,L *e) {
 I a = 0;
 int button = (int)num(evarg(&t,e,&a));
 Uint32 state = SDL_GetMouseState(NULL, NULL);

 bool pressed = false;
 switch(button) {
  case 1: pressed = (state & SDL_BUTTON_LMASK) != 0; break;  /* Left */
  case 2: pressed = (state & SDL_BUTTON_MMASK) != 0; break;  /* Middle */
  case 3: pressed = (state & SDL_BUTTON_RMASK) != 0; break;  /* Right */
 }
 return pressed ? tru : nil;
}

L f_mouse_wheel_x(L t,L *e) {
 return num(mouse_wheel_x);
}

L f_mouse_wheel_y(L t,L *e) {
 return num(mouse_wheel_y);
}

struct { const char *s; L (*f)(L,L*); short t; } prim[] = {
 {"eval",    f_eval,   1},
 {"quote",   f_quote,  0},
 {"cons",    f_cons,   0},
 {"car",     f_car,    0},
 {"cdr",     f_cdr,    0},
 {"+",       f_add,    0},
 {"-",       f_sub,    0},
 {"*",       f_mul,    0},
 {"/",       f_div,    0},
 {"int",     f_int,    0},
 {"<",       f_lt,     0},
 {"eq?",     f_eq,     0},
 {"pair?",   f_pair,   0},
 {"or",      f_or,     0},
 {"and",     f_and,    0},
 {"not",     f_not,    0},
 {"cond",    f_cond,   1},
 {"if",      f_if,     1},
 {"let*",    f_leta,   1},
 {"lambda",  f_lambda, 0},
 {"define",  f_define, 0},
 {"assoc",   f_assoc,  0},
 {"env",     f_env,    0},
 {"let",     f_let,    1},
 {"letrec*", f_letreca,1},
 {"letrec" , f_letrec, 1},
 {"setq",    f_setq,   0},
 {"set-car!",f_setcar, 0},
 {"set-cdr!",f_setcdr, 0},
 {"macro",   f_macro,  0},
 {"read",    f_read,   0},
 {"print",   f_print,  0},
 {"println", f_println,0},
 {"load",    f_load,   0},
 {"catch",   f_catch,  0},
 {"throw",   f_throw,  0},
 {"trace",   f_trace,  0},
 {"clear",   f_sdl_clear,  0},
 {"present", f_sdl_present,0},
 {"color",   f_sdl_color,  0},
 {"rect",    f_sdl_rect,   0},
 {"line",    f_sdl_line,   0},
 {"delay",   f_sdl_delay,  0},
 {"load-font",   f_load_font,   0},
 {"text",        f_text,        0},
 {"text-width",  f_text_width,  0},
 {"text-height", f_text_height, 0},
 {"key-down?",   f_key_down,    0},
 {"mouse-x",     f_mouse_x,     0},
 {"mouse-y",     f_mouse_y,     0},
 {"mouse-button?", f_mouse_button, 0},
 {"mouse-wheel-x", f_mouse_wheel_x, 0},
 {"mouse-wheel-y", f_mouse_wheel_y, 0},
 {0}};

/* section 13: tracing (trace 1) with colorful output, to wait on ENTER (trace 2), with memory dump (trace 3) */
void dump(I i,I k,L e) {
 if (i < k) {
  printf("\n\e[35m==== DUMP ====");
  while (i < k--) {
   printf("\n\e[35m%s\e[32m%u \e[35m",k-1 == ord(e) ? "local env:\n" : k-1 == ord(env) ? "env:\n" : "",k);
   switch (T(cell[k])) {
    case ATOM: printf("ATOM "); printf("\e[32m%u ",ord(cell[k])); break;
    case PRIM: printf("PRIM "); break;
    case CONS: printf("CONS "); printf("\e[32m%u ",ord(cell[k])); break;
    case CLOS: printf("CLOS "); printf("\e[32m%u ",ord(cell[k])); break;
    case MACR: printf("MACR "); printf("\e[32m%u ",ord(cell[k])); break;
    case NIL:  printf("NIL  "); break;
    default:   printf("     "); break;
   }
   printf("\e[33m"); print(cell[k]); printf("\e[m%s",k % 2 ? "" : "\n");
  }
  printf("\e[35m==============\e[m\t");
 }
}
void trace(I s,L y,L x,L e) {
 if (tr > 2) dump(sp,s,e);
 printf("\n\e[32m%u \e[33m",sp); print(y); printf("\e[36m => \e[33m"); print(x); printf("\e[m\t");
 if (tr > 1) while (getchar() >= ' ') continue;
}

/* section 16.5: tail-call optimization (to overwrite closure arguments) */
void assign(L v,L x,L e) { while (!equ(v,car(car(e)))) e = cdr(e); cell[ord(car(e))] = x; }

/* section 16.2-5: tail-call optimization */
L eval(L x,L e) {
 I a,s = sp; L f,v,d,y,g = nil,h;
 while (1) {
  /* copy x to y to output y => x when tracing is enabled */
  y = x;
  /* if ix is an atom, then return its value; if x is not an application list (it is constant), then return x */
  if (T(x) == ATOM) { x = assoc(x,e); break; }
  if (T(x) == STR) { /* nothing */ break; }
  if (T(x) != CONS) break;
  /* evaluate f in the application (f . x) and get the list of arguments x */
  f = eval(car(x),e); x = cdr(x);
  if (T(f) == PRIM) {
   /* apply Lisp primitive to argument list x, return value in x */
   x = prim[ord(f)].f(x,&e);
   /* if tail-call then continue evaluating x, otherwise return x */
   if (prim[ord(f)].t) continue;
   break;
  }
  if (T(f) == MACR) {
   /* bind macro f variables v to the given arguments literally (i.e. without evaluating the arguments) */
   for (d = env,v = car(f); T(v) == CONS; v = cdr(v),x = cdr(x)) d = pair(car(v),car(x),d);
   if (T(v) == ATOM) d = pair(v,x,d);
   /* expand macro f, then continue evaluating the expanded x */
   x = eval(cdr(f),d);
//?    printf("macro expansion: "); print(x); printf("\n");
   continue;
  }
  if (T(f) != CLOS) return err(3,f);
  /* get the list of variables v of closure f */
  v = car(car(f));
  /* if closure f is tail-recursive, then we update its previous environment e, otherwise obey static scoping */
  if (equ(f,g)) d = e;
  else if (not(d = cdr(f))) d = env;
  /* bind closure f variables v to the evaluated argument values */
  for (a = 0; T(v) == CONS; v = cdr(v)) d = pair(car(v),evarg(&x,&e,&a),d);
  if (T(v) == ATOM) d = pair(v,a ? x : evlis(x,e),d);
  if (equ(f,g)) {
   /* reassign tail-recursive closure f vars in e to new values, delete bindings, but don't delete other stacked data */
   for (; !equ(d,e) && sp == ord(d); d = cdr(d),sp += 4) assign(car(car(d)),cdr(car(d)),e);
   /* delete let-locals of tail-recursive closure f, but don't delete other stacked data */
   for (; !equ(d,h) && sp == ord(d); d = cdr(d)) sp += 4;
  }
  /* next, evaluate body x of closure f in environment e = d, track tail-recursive closures g = f, also save h = e */
  x = cdr(car(f)); e = d; g = f; h = e;
  if (tr) trace(s,y,x,e);
 }
 if (tr) trace(s,y,x,e);
 return x;
}

/* section 12: adding readline with history */
void look() {
 if (in) {
  int c = getc(in);
  see = c;
  return;
 }
 if (!(see = *ptr++)) see = '\n';
}
I seeing(char c) { return c == ' ' ? see > 0 && see <= c : see == c; }
char get() { char c = see; look(); return c; }

/* section 7: parsing Lisp expressions */
char scan() {
 I i = 0;
 while (seeing(' ') || seeing(';')) if (get() == ';') while (!seeing('\n')) get();
 if (see == EOF) return buf[0] = 0;
 if (seeing('(') || seeing(')') || seeing('\'') || seeing('`') || seeing(',') || seeing('@')) buf[i++] = get();
 else if (seeing('"')) {
  while (true) {
    buf[i++] = get();
    if (i >= sizeof(buf)-1) { fprintf(stderr, "truncating!\n"); abort(); break; }
    if (seeing('"')) { /* skip trailing quote */ get(); break; }
  }
 }
 else {
  while (true) {
    buf[i++] = get();
    if (i >= sizeof(buf)-1) { fprintf(stderr, "truncating!\n"); abort(); break; }
    if (seeing('(')) break;
    if (seeing(')')) break;
    if (seeing(' ')) break;
  }
 }
 return buf[i] = 0,*buf;
}
L Read() { return scan(),parse(); }

/* section 16.1: replacing recursion with loops (in list parsing) */
L list() {
 L t,*p;
 for (t = nil,p = &t; ; *p = cons(parse(),nil),p = cell+sp) {
  if (scan() == ')') return t;
  if (*buf == '.' && !buf[1]) return *p = Read(),scan(),t;
 }
}
L tick() {
 if (*buf == ',') return Read();
 if (*buf == '@') return Read();
 if (*buf == ')') return nil;
 if (*buf != '(') return cons(atom("quote"),cons(parse(),nil));
 L tick2();
 return scan(), tick2();
}
L tick2() {
 if (*buf == ')') return nil;
 if (*buf == '@') return tick();
 /* TODO: this is needed upstream. Why isn't it needed here? */
//?  if (*buf == '@') {
//?    L car = tick();
//?    /* splice only supported at end of list */
//?    scan();
//?    assert(*buf == ')');
//?    return car;
//?  }
 L car = tick();
 scan();
 return cons(atom("cons"), cons(car, cons(tick2(), nil)));
}
L parse() {
 L n; I i;
//?  printf("%s\n", buf);
 if (*buf == '(') return list();
 if (*buf == '\'') return cons(atom("quote"),cons(Read(),nil));
 if (*buf == '`') return scan(),tick();
 if (*buf == '"') return str(buf);
 if (*buf == '\0') return nil;
 assert(*buf != ')');
 return sscanf(buf,"%lg%n",&n,&i) > 0 && !buf[i] ? n : atom(buf);
}

/* section 8: printing Lisp expressions */
void printlist(L t) {
 putchar('(');
 while (1) {
  print(car(t));
  if (not(t = cdr(t))) break;
  if (T(t) != CONS) { printf(" . "); print(t); break; }
  putchar(' ');
 }
 putchar(')');
}
void print(L x) {
 if (T(x) == NIL) printf("()");
 else if (T(x) == ATOM || T(x) == STR) printf("%s",A+ord(x));
 else if (T(x) == PRIM) printf("<%s>",prim[ord(x)].s);
 else if (T(x) == CONS) printlist(x);
 else if (T(x) == CLOS) printf("{%u}",ord(x));
 else if (T(x) == MACR) printf("[%u]",ord(x));
 else printf("%.10lg",x);
}

/* section 9: garbage collection */
void gc() {
 I i = sp = ord(env);
 for (hp = 0; i < N; ++i) if (T(cell[i]) == ATOM && ord(cell[i]) > hp) hp = ord(cell[i]);
 hp += strlen(A+hp)+1;
}

/* section 14: error handling and exceptions */
void stop(int i) {
 rl_callback_handler_remove();  /* Clean up readline on interrupt */
 if (line) err(5,nil); else abort();
}

void readline_callback(char *input_line) {
 if (input_line == NULL) {
  /* EOF (ctrl+d) */
  pending_line = NULL;
  line_ready = false;
  return;
 }

 if (*input_line) {
  add_history(input_line);
 }

 pending_line = input_line;
 line_ready = true;
}

bool stdin_ready() {
 fd_set readfds;
 struct timeval timeout;

 FD_ZERO(&readfds);
 FD_SET(STDIN_FILENO, &readfds);

 timeout.tv_sec = 0;
 timeout.tv_usec = 0;

 int result = select(STDIN_FILENO + 1, &readfds, NULL, NULL, &timeout);
 return result > 0 && FD_ISSET(STDIN_FILENO, &readfds);
}

/* section 10: read-eval-print loop (REPL) */
int main(int argc,char **argv) {
 I i, err; printf("TinyLisp with SDL3 Graphics\n");

 if (!SDL_Init(SDL_INIT_VIDEO)) {
  fprintf(stderr, "SDL_Init failed: %s\n", SDL_GetError());
  return 1;
 }

 if (!TTF_Init()) {
  fprintf(stderr, "TTF_Init failed: %s\n", SDL_GetError());
  SDL_Quit();
  return 1;
 }

 /* Create window and renderer in single step to avoid flicker */
 if (!SDL_CreateWindowAndRenderer("TinyLisp SDL3 Graphics", 800, 600,
                                    SDL_WINDOW_RESIZABLE, &sdl_window, &sdl_renderer)) {
  fprintf(stderr, "Window and renderer creation failed: %s\n", SDL_GetError());
  TTF_Quit();
  SDL_Quit();
  return 1;
 }

 sdl_initialized = true;

 printf("SDL3 initialized - window ready for graphics!\n");
 printf("Available graphics commands:\n");
 printf("  (clear)             - clear screen with current color\n");
 printf("  (color r g b [a])   - set drawing color (0-255)\n");
 printf("  (rect x y w h)      - draw filled rectangle\n");
 printf("  (line x1 y1 x2 y2)  - draw line\n");
 printf("  (present)           - update display\n");
 printf("  (delay ms)          - delay milliseconds\n\n");
 printf("Text rendering:\n");
 printf("  (load-font path size)   - load TrueType font\n");
 printf("  (text x y string)       - draw text at position\n");
 printf("  (text-width string)     - get text width in pixels\n");
 printf("  (text-height string)    - get text height in pixels\n\n");
 printf("Input (keyboard and mouse):\n");
 printf("  (key-down? scancode)    - check if key pressed (SDL scancode)\n");
 printf("  (mouse-x)               - get mouse X position\n");
 printf("  (mouse-y)               - get mouse Y position\n");
 printf("  (mouse-button? btn)     - check mouse button (1=left, 2=mid, 3=right)\n");
 printf("  (mouse-wheel-x)         - get horizontal wheel movement this frame\n");
 printf("  (mouse-wheel-y)         - get vertical wheel movement this frame\n\n");
 printf("Callbacks:\n");
 printf("  (define update (lambda (dt) ...)) - called each frame with delta time\n");
 printf("  (define draw (lambda () ...))     - called each frame\n");
 printf("  (define keypressed (lambda (scancode isrepeat) ...))\n");
 printf("  (define keyreleased (lambda (scancode) ...))\n");
 printf("  (define mousepressed (lambda (x y button) ...))\n");
 printf("  (define mousereleased (lambda (x y button) ...))\n");
 printf("  (define mousemoved (lambda (x y dx dy) ...))\n");
 printf("  (define wheelmoved (lambda (x y) ...))\n\n");

 nil = box(NIL,0); atom("ERR"); tru = atom("#t"); env = pair(tru,tru,nil);
 for (i = 0; prim[i].s; ++i) env = pair(atom(prim[i].s),box(PRIM,i),env);
 using_history();
 signal(SIGINT,stop);

 printf("Loading common.lisp ...");  fflush(stdout);
 in = fopen("common.lisp", "r");
 if (in) {
  if ((err = setjmp(jb)) == 0) {
   while (1) {
    gc();
    eval(Read(), env);
     if (see == EOF) break;
   }
  }
  fclose(in);
  in = NULL;
  see = ' ';
 }
 printf(" done\n");

 /* Load any files provided on the commandline */
 for (int arg = 1; arg < argc; arg++) {
  in = fopen(argv[arg], "r");
  if (in) {
   printf("Loading %s ...", argv[arg]);  fflush(stdout);
   if ((err = setjmp(jb)) > 0) {
    printf("Error while loading file: %s\n", err_msg(err));
   } else {
    while (1) {
     gc();
     eval(Read(), env);
     if (see == EOF) break;
    }
    printf(" done\n");
   }
   fclose(in);
   in = NULL;
   see = ' ';
  } else {
   fprintf(stderr, "Warning: Could not open file %s\n", argv[arg]);
  }
 }

 strcpy(ps, "> ");
 printf("\nTinyLisp REPL ready. Type Lisp expressions or press Ctrl+C to quit.\n");
 printf("You can also use the SDL3 window for graphics.\n\n");
 rl_callback_handler_install(ps, readline_callback);

 SDL_Event event;
 bool running = true;
 Uint64 last_time = SDL_GetTicks();

 /* Callbacks */
 L draw_sym = atom("draw");
 L update_sym = atom("update");
 L keypressed_sym = atom("keypressed");
 L keyreleased_sym = atom("keyreleased");
 L mousepressed_sym = atom("mousepressed");
 L mousereleased_sym = atom("mousereleased");
 L mousemoved_sym = atom("mousemoved");
 L wheelmoved_sym = atom("wheelmoved");

 /* (draw) */
 L draw_expr = cons(draw_sym, nil);
 env = pair(atom("__draw_expr__"), draw_expr, env);  /* protect against gc */

 /* (update dt) */
 L update_args = cons(num(0), nil);
 L update_expr = cons(update_sym, update_args);
 env = pair(atom("__update_expr__"), update_expr, env);

 /* (keypressed scancode isrepeat) */
 L keypressed_args = cons(num(0), cons(nil, nil));
 L keypressed_expr = cons(keypressed_sym, keypressed_args);
 env = pair(atom("__keypressed_expr__"), keypressed_expr, env);

 /* (keyreleased scancode) */
 L keyreleased_args = cons(num(0), nil);
 L keyreleased_expr = cons(keyreleased_sym, keyreleased_args);
 env = pair(atom("__keyreleased_expr__"), keyreleased_expr, env);

 /* (mousepressed x y button) */
 L mousepressed_args = cons(num(0), cons(num(0), cons(num(0), nil)));
 L mousepressed_expr = cons(mousepressed_sym, mousepressed_args);
 env = pair(atom("__mousepressed_expr__"), mousepressed_expr, env);

 /* (mousereleased x y button) */
 L mousereleased_args = cons(num(0), cons(num(0), cons(num(0), nil)));
 L mousereleased_expr = cons(mousereleased_sym, mousereleased_args);
 env = pair(atom("__mousereleased_expr__"), mousereleased_expr, env);

 /* (mousemoved x y dx dy) */
 L mousemoved_args = cons(num(0), cons(num(0), cons(num(0), cons(num(0), nil))));
 L mousemoved_expr = cons(mousemoved_sym, mousemoved_args);
 env = pair(atom("__mousemoved_expr__"), mousemoved_expr, env);

 /* (wheelmoved x y) */
 L wheelmoved_args = cons(num(0), cons(num(0), nil));
 L wheelmoved_expr = cons(wheelmoved_sym, wheelmoved_args);
 env = pair(atom("__wheelmoved_expr__"), wheelmoved_expr, env);

 while (running) {
  gc();

  if (stdin_ready()) rl_callback_read_char();

  /* If a complete line was read, process it */
  if (line_ready && pending_line) {
   line_ready = false;

   /* Remove handler first to prevent prompt display during output */
   rl_callback_handler_remove();

   ptr = pending_line;
   char *end = ptr + strlen(ptr);
   assert(*end == '\0');
   see = ' ';

   if ((err = setjmp(jb)) > 0) {
    printf("Error: %s\n", err_msg(err));
   } else {
    bool error_found = false;
    L x = Read();
    if (ptr <= end) {
      while (*ptr == ' ' || *ptr == '\t') ptr++;  /* Skip trailing whitespace */
      if (*ptr && *ptr != '\n') {
       printf("Error: Trailing characters: \"%s\"; only one expression per line please\n", ptr);
       error_found = true;
      }
    }
    if (!error_found) {
      L result = eval(x, env);
      print(result);
      printf("\n");
    }
   }

   free(pending_line);
   pending_line = NULL;

   /* Now install handler again - this displays the prompt AFTER output */
   fflush(stdout);
   rl_callback_handler_install("> ", readline_callback);
  }

  mouse_wheel_x = 0.0f;
  mouse_wheel_y = 0.0f;

  /* Process SDL events */
  while (SDL_PollEvent(&event)) {
   if (event.type == SDL_EVENT_QUIT) running = false;

   if (event.type == SDL_EVENT_KEY_DOWN && bound(keypressed_sym, env)) {
    if ((err = setjmp(jb)) == 0) {
     cell[ord(keypressed_args)+1] = num(event.key.scancode);
     cell[ord(cdr(keypressed_args))+1] = event.key.repeat ? tru : nil;
     eval(keypressed_expr, env);
    } else {
     printf("Error in keypressed callback: %s\n", err_msg(err));
    }
   }

   if (event.type == SDL_EVENT_KEY_UP && bound(keyreleased_sym, env)) {
    if ((err = setjmp(jb)) == 0) {
     cell[ord(keyreleased_args)+1] = num(event.key.scancode);
     eval(keyreleased_expr, env);
    } else {
     printf("Error in keyreleased callback: %s\n", err_msg(err));
    }
   }

   if (event.type == SDL_EVENT_MOUSE_BUTTON_DOWN && bound(mousepressed_sym, env)) {
    if ((err = setjmp(jb)) == 0) {
     cell[ord(mousepressed_args)+1] = num((int)event.button.x);
     cell[ord(cdr(mousepressed_args))+1] = num((int)event.button.y);
     cell[ord(cdr(cdr(mousepressed_args)))+1] = num(event.button.button);
     eval(mousepressed_expr, env);
    } else {
     printf("Error in mousepressed callback: %s\n", err_msg(err));
    }
   }

   if (event.type == SDL_EVENT_MOUSE_BUTTON_UP && bound(mousereleased_sym, env)) {
    if ((err = setjmp(jb)) == 0) {
     cell[ord(mousereleased_args)+1] = num((int)event.button.x);
     cell[ord(cdr(mousereleased_args))+1] = num((int)event.button.y);
     cell[ord(cdr(cdr(mousereleased_args)))+1] = num(event.button.button);
     eval(mousereleased_expr, env);
    } else {
     printf("Error in mousereleased callback: %s\n", err_msg(err));
    }
   }

   if (event.type == SDL_EVENT_MOUSE_MOTION && bound(mousemoved_sym, env)) {
    if ((err = setjmp(jb)) == 0) {
     cell[ord(mousemoved_args)+1] = num((int)event.motion.x);
     cell[ord(cdr(mousemoved_args))+1] = num((int)event.motion.y);
     cell[ord(cdr(cdr(mousemoved_args)))+1] = num((int)event.motion.xrel);
     cell[ord(cdr(cdr(cdr(mousemoved_args))))+1] = num((int)event.motion.yrel);
     eval(mousemoved_expr, env);
    } else {
     printf("Error in mousemoved callback: %s\n", err_msg(err));
    }
   }

   if (event.type == SDL_EVENT_MOUSE_WHEEL && bound(wheelmoved_sym, env)) {
    if ((err = setjmp(jb)) == 0) {
     mouse_wheel_x += event.wheel.x;
     mouse_wheel_y += event.wheel.y;
     cell[ord(wheelmoved_args)+1] = num((int)event.wheel.x);
     cell[ord(cdr(wheelmoved_args))+1] = num((int)event.wheel.y);
     eval(wheelmoved_expr, env);
    } else {
     printf("Error in wheelmoved callback: %s\n", err_msg(err));
    }
   }
  }

  /* update callback */
  if (bound(update_sym, env)) {
   if ((err = setjmp(jb)) == 0) {
    Uint64 current_time = SDL_GetTicks();
    cell[ord(update_args)+1] = num((int)(current_time - last_time));
    last_time = current_time;
    eval(update_expr, env);
   } else {
    printf("Error in update callback: %s\n", err_msg(err));
   }
  }

  /* draw callback */
  if (bound(draw_sym, env)) {
   if ((err = setjmp(jb)) == 0) {
    eval(draw_expr, env);
    SDL_RenderPresent(sdl_renderer);
   } else {
    printf("Error in draw callback: %s\n", err_msg(err));
   }
  }

  SDL_Delay(16);
 }

 rl_callback_handler_remove();
 if (current_font) TTF_CloseFont(current_font);
 SDL_DestroyRenderer(sdl_renderer);
 SDL_DestroyWindow(sdl_window);
 TTF_Quit();
 SDL_Quit();
 return 0;
}
