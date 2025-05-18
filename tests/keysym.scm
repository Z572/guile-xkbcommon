(use-modules (srfi srfi-64)
             (xkbcommon keysyms)
             (xkbcommon xkbcommon))

(test-begin "keysym")
(define test-xkb-context (xkb-context-new))
(define test-xkb-keymap (xkb-keymap-new test-xkb-context))
(define test-xkb-state (xkb-state-new test-xkb-keymap))

(test-equal "xkb-keysym-get-name"
  "A" (xkb-keysym-get-name (char->integer #\A)))

(test-equal "xkb-keysym-from-name"
  (char->integer #\A) (xkb-keysym-from-name "A"))

(test-equal "xkb-keysym-from-name"
  (char->integer #\A) (xkb-keysym-from-name "A"))

(test-equal "xkb-keysym-to-lower"
  (char->integer #\a)
  (xkb-keysym-to-lower
   (xkb-keysym-from-name "A")))

(test-equal "xkb-keysym-to-upper"
  (char->integer #\A)
  (xkb-keysym-to-upper
   (xkb-keysym-from-name "a")))

(test-end "keysym")
