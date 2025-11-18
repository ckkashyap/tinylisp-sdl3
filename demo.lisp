; Demo demonstrating drawing and keyboard/mouse queries

; Sections:
; 1. Assumptions
; 2. Color constants
; 3. UI helper functions
; 4. Draw function
;
; === 1. Assumptions ===
; 1. The following are loaded:
;   * init.lisp's floor and ceiling functions
;   * init-keycodes.lisp loaded with K_* keycodes
; 2. We can get a window with SDL

(window-set-title "Lisp with SDL3 Graphics (Keyboard and Mouse Input Demo)")

(def half (num) ; Get half of num
  (/ num 2))


; === 2. Color constants ===
;
; Use with the color function as follows:
;
;   (color . RGBA-INERT)
;
; The . character joins the quoted numbers to the symbol `color
; in the S-expression before it is applied (also known "called"
; in other languages.)

(define RGBA-BG           `( 30  30  40 255)) ; A deep grayish blue for the background.

; UI button colors colors
(define RGBA-INERT        `(100 100 100 255)) ; A gray for non-activated inputs.
(define RGBA-DIRECTION-ON `(100 255 100 255)) ; A direction button's pressed.

; Text element colors
(define RGBA-LABEL        `(200 200 200 255)) ; A lighter gray for the text fields.
(define RGBA-TITLE        `(255 255 100 255)) ; A yellowish color.
(define RGBA-INSTRUCTIONS `(150 150 150 255)) ; A medium gray for the instruction text.
(define RGBA-WHITE        `(255 255 255 255)) ; Pure white color.

; The mouse crosshair and overlays
(define RGBA-CROSSHAIR       `(100 200 255 255)) ; A teal-like color.
(define RGBA-MOUSE-OVERLAY-1 `(255 100 100  80)) ; Mouse button 1's overlay shape
(define RGBA-MOUSE-OVERLAY-2 `(100 255 100  80)) ; Mouse button 2's overlay shape


; === 3. UI Helper functions ===
;
; These make it easier to draw repeating UI elements.

; Center the given str over an x y w h rectangular region.
;
; The text will extend beyond the region if it is bigger.
; All pixel positions are rounded to the nearest whole px
; to avoid blurry text.
;   x   - The x center of the region in pixels
;   y   - The y center of the region in pixels
;   w   - The width of the region in pixels
;   h   - The height of the region in pixels
;   str - The string to draw.
(def center-text-in (x y w h str) ; Center text over an (x y w h) rectangle.
  (unless (iso str "")
    (text
      ; Snapping to integer bounds prevents blurred text
      (floor
        (- (+ x (half w)) (half (text-width  str))))
      (floor
        (- (+ y (half h)) (half (text-height str))))
      str)))

; Draw a crosshair around the x y position with 1px lines.
; Uses the current color for foreground drawing.
;
;   x      - the x position of the center
;   y      - the y position of the center
;   radius - the distance of each four spokes from the center
(def crosshair (x y radius) ; Draw a thin 1px crosshair
  (line
    (- x radius) y
    (+ x radius) y)
  (line
    x (- y radius)
    x (+ y radius)))

; Draw a fixed-size plus shape of 80px x 80x with 20px thick lines.
;   x - the x center of the shape in px
;   y - the y center of the shape in px
(def plusshape (x y)
  (rect (- x 40) (- y 5) 80 10)
  (rect (- x 5) (- y 40) 10 80))


(def square-around (x y radius)
  (rect
    (floor (- x radius))   (floor (- y radius))
    (ceiling (* 2 radius)) (ceiling (* 2 radius))
  ))


; Show a status indicator of the given color with white text.
(def show-status (str x y w h)
  (rect x y w h)
  (color . RGBA-WHITE)
  (center-text-in x y w h str))


(def show-mouse-status (button-id str x y w h)
  (color
    (if (mouse-button? button-id) 255 100)  ; red
      100
      100)
  (show-status str x y w h))

(def show-key-status (keycode str x y w h)
  ; TODO: ugly that it silently switches color
  (unless (key-down? keycode)
    (color . RGBA-INERT))
  (show-status str x y w h)
)

(def show-dir-status (keycode str x y w h)
  (color . RGBA-DIRECTION-ON)
  (show-key-status keycode str x y w h))

; === 4. Draw function ===
(def draw ()
  (color . RGBA-BG)
  (clear)

  (color . RGBA-TITLE)
  (text 20 20 "Keyboard and Mouse Input Demo")

  ; mouse button states
  (color . RGBA-LABEL)
  (text 20 60 "Mouse Buttons:")

  (show-mouse-status 1 "Left"
    200 55 60 35)
  (show-mouse-status 2 "Middle"
    270 55 80 35)
  (show-mouse-status 3 "Right"
    360 55 70 35)

  ; Underlined buttons for "extra" optional buttons
  (show-mouse-status 4 "X2"
    440 55 70 35)
  (show-mouse-status 5 "X2"
    520 55 70 35)

  ; The underline and label
  (color . RGBA-LABEL)
  (line 440 100 590 100)
  (text 440 110 "'Extra' buttons")

  ; some keyboard states
  (color . RGBA-LABEL)
  (text 20 110 "WASD Keys:")

  (show-dir-status K-W "W"
    253 105 40 35)
  (show-dir-status K-A "A"
    223 145 30 35)
  (show-dir-status K-S "S"
    259 145 30 35)
  (show-dir-status K-D "D"
    295 145 30 35)

  (color . RGBA-LABEL)
  (text 20 200 "Arrow Keys:")

  ; DejaVu Sans includes the following arrow glyphs:
  ; "←" : Leftwards Arrow   (https://www.compart.com/en/unicode/U+2190)
  ; "↑" : Upwards Arrow     (https://www.compart.com/en/unicode/U+2191)
  ; "→" : Rightwards Arrow  (https://www.compart.com/en/unicode/U+2192)
  ; "↓" : Downwards Arrow   (https://www.compart.com/en/unicode/U+2193)

  (show-dir-status K-UP    "↑"
    260 195 40 35)
  (show-dir-status K-LEFT  "←"
    200 235 50 35)
  (show-dir-status K-DOWN  "↓"
    255 235 65 35)
  (show-dir-status K-RIGHT "→"
    325 235 65 35)

  ; (text 20 290 "Spacebar:")

  (color . RGBA-LABEL)
  (show-key-status K-SPACE ""
     200 285 190 35)

  ; instructions
  (color . RGBA-LABEL)
  (text 20 340 "Move mouse and press keys/buttons")
  (text 20 370 "Crosshair follows mouse position")

  ; Draw cursors last to make sure they render over the rest

  ; cursor crosshair at mouse position
  (color . RGBA-CROSSHAIR)
  (crosshair (mouse-x) (mouse-y) 20 1)

  ; draw a translucent box around the mouse when left button is pressed
  (when (mouse-button? 1)
    (color . RGBA-MOUSE-OVERLAY-1)
    (square-around (mouse-x) (mouse-y) 60))

  ; draw a plus pattern when right button is pressed
  (when (mouse-button? 3)
    (color . RGBA-MOUSE-OVERLAY-2)
    (plusshape (mouse-x) (mouse-y)))

)
