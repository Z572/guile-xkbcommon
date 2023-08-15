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

(test-assert "xkb-keymap-min-keycode"
  (xkb-keymap-min-keycode (xkb-keymap-new-from-names (xkb-context-new))))
(test-error "xkb-keymap-min-keycode: fail"
            #t
            (xkb-keymap-min-keycode #f))
(test-assert "xkb-keymap-max-keycode"
  (xkb-keymap-max-keycode (xkb-keymap-new-from-names (xkb-context-new))))
(test-error "xkb-keymap-max-keycode: fail"
            #t
            (xkb-keymap-max-keycode #f))
(let ((keymap (xkb-keymap-new-from-names (xkb-context-new))))
  (test-equal "xkb-keymap-key-for-each"
    (+ (- (xkb-keymap-max-keycode keymap)
          (xkb-keymap-min-keycode keymap))
       1)

    (let ((n 0))
      (xkb-keymap-key-for-each
       keymap
       (lambda (km k)
         (set! n (1+ n))))
      n)))

(test-error "xkb-keymap-key-for-each: fail"
            #t
            (xkb-keymap-key-for-each
             (xkb-keymap-new-from-names (xkb-context-new))
             30))
(let ((keymap (xkb-keymap-new-from-names (xkb-context-new))))
  (test-assert "xkb-keymap-key-get-name"
    (xkb-keymap-key-get-name keymap (xkb-keymap-min-keycode keymap)))
  (test-error "xkb-keymap-key-get-name: fail"
              #t
              (xkb-keymap-key-get-name #f (xkb-keymap-min-keycode keymap)))
  (test-equal "xkb-keymap-key-get-name: no exists"
    #f
    (xkb-keymap-key-get-name keymap (- (xkb-keymap-min-keycode keymap) 1))))

(test-end "context")
