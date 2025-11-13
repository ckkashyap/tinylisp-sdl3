; Demo demonstrating drawing and keyboard/mouse queries

; Assumes init.lisp's floor and ceiling functions are loaded.
(window-set-title "Lisp with SDL3 Graphics (Keyboard and Mouse Input Demo)")


(def half (num) ; Get half of num
    (/ num 2))


; Center the given str over an x y w h rectangular region.
; The text will extend beyond the region if it is bigger.
; All pixel positions are rounded to the nearest whole px
; to avoid blurry text.
;   x   - The x center of the region in pixels
;   y   - The y center of the region in pixels
;   w   - The width of the region in pixels
;   h   - The height of the region in pixels
;   str - The string to draw.
(def center-text-in (x y w h str) ; Center text over an (x y w h) rectangle.
   (text
        ; int-snapping's important to prevent blurriness
        (floor (- (+ x (half w)) (half (text-width  str))))
        (floor (- (+ y (half h)) (half (text-height str))))
        str))

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
        (floor (- x radius)) (floor (- y radius))
        (ceiling (* 2 radius)) (ceiling (* 2 radius))
    ))


; Show a status indicator of the given color with white text.
; 
(def show-status (str x y w h)
    (rect x y w h)
    (color 255 255 255)
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
      (color 100 100 100))
    (show-status str x y w h)
)

(def show-dir-status (keycode str x y w h)
   (color 100 255 100)
   (show-key-status keycode str x y w h))


(def draw ()
  (color 30 30 40)
  (clear)
  (color 255 255 100)
  (text 20 20 "Keyboard and Mouse Input Demo")

  ; mouse button states
  (color 200 200 200)
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
  (color 200 200 200)
  (line 440 100 590 100)
  (text 440 110 "'Extra' buttons")

  ; some keyboard states
  (color 200 200 200)
  (text 20 110 "WASD Keys:")

  (show-dir-status 26 "W"
    253 105 40 35)
  (show-dir-status  4 "A"
    223 145 30 35)
  (show-dir-status 22 "S"
    259 145 30 35)
  (show-dir-status  7 "D"
    295 145 30 35)

  (color 200 200 200)
  (text 20 200 "Arrow Keys:")

  ; DejaVu Sans includes the following arrow glyphs:
  ; "←" : Leftwards Arrow   (https://www.compart.com/en/unicode/U+2190)
  ; "↑" : Upwards Arrow     (https://www.compart.com/en/unicode/U+2191)
  ; "→" : Rightwards Arrow  (https://www.compart.com/en/unicode/U+2192)
  ; "↓" : Downwards Arrow   (https://www.compart.com/en/unicode/U+2193)

  (show-dir-status 82 "↑" ; Up arrow
    260 195 40 35)
  (show-dir-status 80 "←"
    200 235 50 35)
  (show-dir-status 81 "↓"  ; Down
    255 235 65 35)
  (show-dir-status 79 "→"
    325 235 65 35)

  (color 200 200 200)
  (text 20 290 "Spacebar:")
  (if (key-down? 44)
    (color 255 255 100)
    (color 100 100 100))
  (rect 200 285 190 35)

  ; instructions
  (color 150 150 150)
  (text 20 340 "Move mouse and press keys/buttons")
  (text 20 370 "Crosshair follows mouse position")

  ; Draw cursors last to make sure they render over the rest

  ; cursor crosshair at mouse position
  (color 100 200 255)
  (crosshair (mouse-x) (mouse-y) 20 1)

  ; draw a translucent box around the mouse when left button is pressed
  (when (mouse-button? 1)
    (color 255 100 100 80)
    (square-around (mouse-x) (mouse-y) 60))

  ; draw a plus pattern when right button is pressed
  (when (mouse-button? 3)
    (color 100 255 100 80)
    (plusshape (mouse-x) (mouse-y)))

)
