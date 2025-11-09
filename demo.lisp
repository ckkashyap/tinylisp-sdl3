; Demo demonstrating drawing and keyboard/mouse queries

(load-font 'DejaVuSans 20)

(def draw ()
  (color 30 30 40)
  (clear)
  (color 255 255 100)
  (text 20 20 "Keyboard and Mouse Input Demo")

  ; cursor crosshair at mouse position
  (color 100 200 255)
  (line (- (mouse-x) 10) (mouse-y) (+ (mouse-x) 10) (mouse-y))
  (line (mouse-x) (- (mouse-y) 10) (mouse-x) (+ (mouse-y) 10))
  (color 100 200 255 100)
  (rect (- (mouse-x) 3) (- (mouse-y) 3) 6 6)

  ; mouse button states
  (color 200 200 200)
  (text 20 60 "Mouse Buttons:")

  (if (mouse-button? 1)
    (color 255 100 100)
    (color 100 100 100))
  (rect 200 55 60 35)
  (color 255 255 255)
  (text 210 60 "Left")

  (if (mouse-button? 2)
    (color 255 100 100)
    (color 100 100 100))
  (rect 270 55 80 35)
  (color 255 255 255)
  (text 275 60 "Middle")

  (if (mouse-button? 3)
    (color 255 100 100)
    (color 100 100 100))
  (rect 360 55 70 35)
  (color 255 255 255)
  (text 370 60 "Right")

  ; draw a translucent box around the mouse when left button is pressed
  (when (mouse-button? 1)
    (color 255 100 100 80)
    (rect (- (mouse-x) 30) (- (mouse-y) 30) 60 60))

  ; draw a plus pattern when right button is pressed
  (when (mouse-button? 3)
    (color 100 255 100 80)
    (rect (- (mouse-x) 40) (- (mouse-y) 5) 80 10)
    (rect (- (mouse-x) 5) (- (mouse-y) 40) 10 80))

  ; some keyboard states
  (color 200 200 200)
  (text 20 110 "WASD Keys:")

  (if (key-down? 26)
    (color 100 255 100)
    (color 100 100 100))
  (rect 253 105 40 35)
  (color 255 255 255)
  (text 263 110 "W")

  (if (key-down? 4)
    (color 100 255 100)
    (color 100 100 100))
  (rect 223 145 30 35)
  (color 255 255 255)
  (text 231 150 "A")

  (if (key-down? 22)
    (color 100 255 100)
    (color 100 100 100))
  (rect 259 145 30 35)
  (color 255 255 255)
  (text 267 150 "S")

  (if (key-down? 7)
    (color 100 255 100)
    (color 100 100 100))
  (rect 295 145 30 35)
  (color 255 255 255)
  (text 303 150 "D")

  (color 200 200 200)
  (text 20 200 "Arrow Keys:")

  (if (key-down? 82)
    (color 100 255 100)
    (color 100 100 100))
  (rect 260 195 40 35)
  (color 255 255 255)
  (text 268 200 "Up")

  (if (key-down? 80)
    (color 100 255 100)
    (color 100 100 100))
  (rect 200 235 50 35)
  (color 255 255 255)
  (text 205 240 "Left")

  (if (key-down? 81)
    (color 100 255 100)
    (color 100 100 100))
  (rect 255 235 65 35)
  (color 255 255 255)
  (text 260 240 "Down")

  (if (key-down? 79)
    (color 100 255 100)
    (color 100 100 100))
  (rect 325 235 65 35)
  (color 255 255 255)
  (text 330 240 "Right")

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
)
