(global example-text '(
  ""
  ))

(def draw ()
  (color 255 255 255)
  (let*
    (margin-top 15)
    (margin-left 15)
    (y margin-top)
    (dolist (line example-text)
      (text margin-left y line)
      (setq y (+ y 30)))))

(def text-input (t)
  (let (s (nth example-text 0))
    (set-nth! example-text 0 (string s t))))
