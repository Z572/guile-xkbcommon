(use-modules (srfi srfi-64)
             (xkbcommon keysyms)
             (xkbcommon xkbcommon))

(test-begin "state")
(test-assert "xkb-state-new"
  (xkb-state-new (xkb-keymap-new (xkb-context-new))))
(define test-xkb-keymap (xkb-keymap-new (xkb-context-new)))
(define test-xkb-state
  (xkb-state-new test-xkb-keymap))
(test-error "xkb-state-new: fail"
            #t
            (xkb-state-new #f))

(test-assert "xkb-state-led-index-is-active"
  (begin (xkb-state-led-index-is-active test-xkb-state 10)
         #t))
(test-eq "xkb-state-get-keymap"
  test-xkb-keymap
  (xkb-state-get-keymap test-xkb-state))

(test-assert "xkb-state-update-key"
  (xkb-state-update-key test-xkb-state
                        9 XKB_KEY_Up))

(test-end "state")
