(use-modules (srfi srfi-64)
             (xkbcommon xkbcommon))

(test-begin "state")
(test-assert "xkb-state-new"
  (xkb-state-new (xkb-keymap-new (xkb-context-new))))

(test-error "xkb-state-new: fail"
            #t
            (xkb-state-new #f))

(test-end "state")
