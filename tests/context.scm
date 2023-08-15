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
(test-end "context")
