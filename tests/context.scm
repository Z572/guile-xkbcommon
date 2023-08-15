(use-modules (srfi srfi-64)
             (xkbcommon xkbcommon))

(test-begin "context")
(test-assert "xkb-context-new: no flag"
  (xkb-context-new))
(test-assert "xkb-context-new: XKB_CONTEXT_NO_DEFAULT_INCLUDES"
  (xkb-context-new
   XKB_CONTEXT_NO_DEFAULT_INCLUDES))
(test-equal "xkb-context-num-include-paths: 1"
  1
  (xkb-context-num-include-paths (xkb-context-new)))
(test-equal "xkb-context-num-include-paths: XKB_CONTEXT_NO_DEFAULT_INCLUDES"
  0
  (xkb-context-num-include-paths
   (xkb-context-new
    XKB_CONTEXT_NO_DEFAULT_INCLUDES)))
(test-equal "xkb-context-include-path-reset-defaults"
  (xkb-context-include-path-get (xkb-context-new) 0)
  (let ((no-default (xkb-context-new
                     XKB_CONTEXT_NO_DEFAULT_INCLUDES)))
    (xkb-context-include-path-reset-defaults no-default)
    (xkb-context-include-path-get no-default 0)))
(test-assert "xkb-keymap-new-from-names"
  (xkb-keymap? (xkb-keymap-new-from-names (xkb-context-new))))
(test-assert "xkb-keymap-get-as-string"
  (xkb-keymap-get-as-string
   (xkb-keymap-new-from-names (xkb-context-new))
   XKB_KEYMAP_FORMAT_TEXT_V1))
(test-error "xkb-keymap-get-as-string: error"
            #t
            (xkb-keymap-get-as-string
             #f XKB_KEYMAP_FORMAT_TEXT_V1))

(test-assert "xkb-keymap-new-from-string"
  (let ((str (xkb-keymap-get-as-string
              (xkb-keymap-new-from-names (xkb-context-new))
              XKB_KEYMAP_FORMAT_TEXT_V1)))
    (xkb-keymap-new-from-string (xkb-context-new) str)))
(test-equal "xkb-keymap-new-from-string: fail"
  #f
  (xkb-keymap-new-from-string (xkb-context-new) "sdfd"))

(test-end "context")
