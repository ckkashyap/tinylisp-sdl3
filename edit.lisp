(global example-text '(
  "line 1"
  "line 2"
  ""
  "line 4"))

(def draw ()
  (color 255 255 255)
  (let*
    (margin-top 15)
    (margin-left 15)
    (y margin-top)
    (dolist (line example-text)
      (text margin-left y line)
      (setq y (+ y 30)))))
