(define null? not)
(define number? (lambda (x) (iso (type x) 0)))
(define symbol? (lambda (x) (iso (type x) 2)))
(define string? (lambda (x) (iso (type x) 3)))
(define pair? (lambda (x) (iso (type x) 4)))
(define atom? (lambda (x) (not (pair? x))))
(define list?
    (lambda (x)
        (if (pair? x)
            (list? (cdr x))
            (not x))))
(define > (lambda (x y) (< y x)))
(define <= (lambda (x y) (not (< y x))))
(define >= (lambda (x y) (not (< x y))))
(define = (lambda (x y) (iso (- x y) 0)))


; Trigonometry and degree<->radian conversion (lisp-sdl3.c has radian trig)
(define PI  3.14159265359)
(define TAU (* PI 2))
(define HALF_PI (/ PI 2))

(define DEGPERRAD (/ 180 PI))
(define RADPERDEG (/ PI 180))


(define math-rad-to-deg
    (lambda (rad)
        (* rad DEGPERRAD)))

(define math-deg-to-rad
    (lambda (deg)
        (* deg RADPERDEG)))

(define math-cos-deg
    (lambda (deg)
        (math-cos (* deg RADPERDEG))))

(define math-sin-deg
    (lambda (deg)
        (math-sin (* deg RADPERDEG))))

(define math-tan-deg
    (lambda (deg)
        (math-tan (* deg RADPERDEG))))

(define math-atan-deg
    (lambda (x)
        (* (math-atan x) DEGPERRAD)))

(define math-atan2-deg
    (lambda (y x)
        (*
            (math-atan2 x y) ; emits radians
            DEGPERRAD)))

; Bounding and factoring arithmetic helpers
(define abs
    (lambda (n)
        (if (< n 0)
            (- 0 n)
            n)))
(define frac (lambda (n) (- n (int n))))
(define truncate int)
(define floor
    (lambda (n)
        (int
            (if (< n 0)
                (- n 1)
                n))))
(define ceiling (lambda (n) (- 0 (floor (- 0 n)))))
(define round (lambda (n) (floor (+ n 0.5))))
(define mod (lambda (n m) (- n (* m (int (/ n m))))))
(define gcd
    (lambda (n m)
        (if (iso m 0)
            n
            (gcd m (mod n m)))))
(define lcm (lambda (n m) (/ (* n m) (gcd n m))))
(define even? (lambda (n) (iso (mod n 2) 0)))
(define odd? (lambda (n) (iso (mod n 2) 1)))

(define length-tr
    (lambda (t n)
        (if t
            (length-tr (cdr t) (+ n 1))
            n)))
(define length (lambda (t) (length-tr t 0)))
(define append1
    (lambda (s t)
        (if s
            (cons (car s) (append1 (cdr s) t))
            t)))
(define append
    (lambda (t . args)
        (if args
            (append1 t (append . args))
            t)))
(define nthcdr
    (lambda (t n)
        (if (iso n 0)
            t
            (nthcdr (cdr t) (- n 1)))))
(define nth (lambda (t n) (car (nthcdr t n))))
(define set-nth!
    (lambda (t n v)
      (set-car! (nthcdr t n) v)))
(define reverse-tr
    (lambda (r t)
        (if t
            (reverse-tr (cons (car t) r) (cdr t))
            r)))
(define reverse (lambda (t) (reverse-tr () t)))
(define member
    (lambda (x t)
        (if t
            (if (iso x (car t))
                t
                (member x (cdr t)))
            t)))
(define foldr
    (lambda (f x t)
        (if t
            (f (car t) (foldr f x (cdr t)))
            x)))
(define foldl
    (lambda (f x t)
        (if t
            (foldl f (f (car t) x) (cdr t))
            x)))
(define min
    (lambda args
        (foldl
            (lambda (x y) (if (< x y) x y))
            inf
            args)))
(define max
    (lambda args
        (foldl
            (lambda (x y) (if (< x y) y x))
            -inf
            args)))
(define filter
    (lambda (f t)
        (if t
            (if (f (car t))
                (cons (car t) (filter f (cdr t)))
                (filter f (cdr t)))
            ())))
(define all?
    (lambda (f t)
        (if t
            (if (f (car t))
                (all? f (cdr t))
                ())
            #t)))
(define any?
    (lambda (f t)
        (if t
            (if (f (car t))
                #t
                (any? f (cdr t)))
            ())))
(define mapcar
    (lambda (f t)
        (if t
            (cons (f (car t)) (mapcar f (cdr t)))
            ())))
(define map
    (lambda (f . args)
        (if (any? null? args)
            ()
            (let*
                (x (mapcar car args))
                (t (mapcar cdr args))
                (cons (f . x) (map f . t))))))
(define zip (lambda args (map list . args)))
(define seq-prepend
    (lambda (t n m)
        (if (< n m)
            (seq-prepend (cons (- m 1) t) n (- m 1))
            t)))
(define seq (lambda (n m) (seq-prepend () n m)))
(define seqby-prepend
    (lambda (t n m k)
        (if (< 0 (* k (- m n)))
            (seqby-prepend (cons (- m k) t) n (- m k) k)
            t)))
(define seqby (lambda (n m k) (seqby-prepend () n (+ n k (* k (int (/ (- m n (if (< 0 k) 1 -1)) k)))) k)))
(define range
    (lambda (n m . args)
        (if args
            (seqby n m (car args))
            (seq n m))))

(define curry (lambda (f x) (lambda args (f x . args))))
(define compose (lambda (f g) (lambda args (f (g . args)))))
(define Y (lambda (f) (lambda args ((f (Y f)) . args))))

(define reveal
    (lambda (f)
        (cond
            ((iso (type f) 6) (cons 'lambda (cons (car (car f)) (cons (cdr (car f)) ()))))
            ((iso (type f) 7) (cons 'macro (cons (car f) (cons (cdr f) ()))))
            (#t  f))))

(define mac (macro (name params . body)
              `(define ,name (macro ,params
                               (begin . body)))))

(mac def (name params . body)
  `(define ,name (lambda ,params
                   (begin . body))))

(mac when (expr . body)
  `(if (not ,expr)
     ()
     . body))

(mac unless (expr . body)
  `(if ,expr
     ()
     . body))

(mac global (name init)
  `(define ,name ,init))

(mac dolist (binding . body)
  (let
    (var (car binding))
    (l (car (cdr binding)))
    `(let
       (_ ,l)
       (,var ())
       (while _
         (setq ,var (car _))
         (setq _ (cdr _))
         . body))))

(mac awhen (expr . body)
  `(let
     (it ,expr)
     (when it . body)))

(mac whenlet (var expr . body)
  `(let
     (,var ,expr)
     (when ,var . body)))

(def alref (key h)
  (awhen (assoc key h)
    (car it)))


(define platform (get-platform))

(define local-accel
    (if (or
          (iso platform "macOS")
          (iso platform "iOS"))
      (<< 1 1) ; Ctrl key
      (<< 1 5)) ; Command key on mac
)

(define 'mod-keys `(
    (shift       ,(<< 1 0))
    (ctrl        ,(<< 1 1))
    (alt         ,(<< 1 2))
    (capslock    ,(<< 1 3))
    (numlock     ,(<< 1 4))
    (windows     ,(<< 1 5))
    (command     ,(<< 1 6))
    (option      ,(<< 1 7))
    (scrolllock  ,(<< 1 8))
    (function    ,(<< 1 9))
    (accel      ,local-accel)
))

; Non-letter, non-"extended" keys have this base padding:
; https://wiki.libsdl.org/SDL3/SDL_Keycode

(def key-mask-hi (base) (| 0x40000000 base))
; Generated from SDL_keycode.h
(load "init-keycodes.lisp")
