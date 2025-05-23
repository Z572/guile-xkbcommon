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
(test-assert "xkb-context-get-log-level"
  (xkb-context-get-log-level (xkb-context-new)))

(test-equal "xkb-context-set-log-level!"
  41
  (let ((ctx (xkb-context-new)))
    (xkb-context-set-log-level! ctx 41)
    (xkb-context-get-log-level ctx)))

(test-assert "xkb-keymap-new"
  (xkb-keymap? (xkb-keymap-new (xkb-context-new))))
(test-assert "xkb-keymap-get-as-string"
  (xkb-keymap-get-as-string
   (xkb-keymap-new (xkb-context-new))))
(test-error "xkb-keymap-get-as-string: error"
            #t
            (xkb-keymap-get-as-string
             #f))

(test-assert "xkb-keymap-new: string"
  (let ((str (xkb-keymap-get-as-string
              (xkb-keymap-new (xkb-context-new)))))
    (xkb-keymap? (xkb-keymap-new (xkb-context-new) str))))
(test-equal "xkb-keymap-new: string: fail"
  #f
  (xkb-keymap-new (xkb-context-new) "sdfd"))

(test-assert "xkb-keymap-min-keycode"
  (xkb-keymap-min-keycode (xkb-keymap-new (xkb-context-new))))
(test-error "xkb-keymap-min-keycode: fail"
            #t
            (xkb-keymap-min-keycode #f))
(test-assert "xkb-keymap-max-keycode"
  (xkb-keymap-max-keycode (xkb-keymap-new (xkb-context-new))))
(test-error "xkb-keymap-max-keycode: fail"
            #t
            (xkb-keymap-max-keycode #f))
(let ((keymap (xkb-keymap-new (xkb-context-new))))
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
             (xkb-keymap-new (xkb-context-new))
             30))
(let ((keymap (xkb-keymap-new (xkb-context-new))))
  (test-assert "xkb-keymap-key-get-name"
    (xkb-keymap-key-get-name keymap (xkb-keymap-min-keycode keymap)))
  (test-error "xkb-keymap-key-get-name: fail"
              #t
              (xkb-keymap-key-get-name #f (xkb-keymap-min-keycode keymap)))
  (test-equal "xkb-keymap-key-get-name: no exists"
    #f
    (xkb-keymap-key-get-name keymap (- (xkb-keymap-min-keycode keymap) 1))))

(let* ((keymap (xkb-keymap-new (xkb-context-new)))
       (minkeycode (xkb-keymap-min-keycode keymap)))
  (test-equal "xkb-keymap-key-by-name"
    minkeycode
    (xkb-keymap-key-by-name keymap
                            (xkb-keymap-key-get-name
                             keymap minkeycode)))
  (test-error "xkb-keymap-key-by-name: fail"
              #t
              (xkb-keymap-key-by-name #f (xkb-keymap-min-keycode keymap)))
  (test-error "xkb-keymap-key-by-name: no exists"
              #t
              (xkb-keymap-key-by-name keymap "Ndfsdlfjafkd ")))

(test-assert "xkb-keymap-num-mods"
  (xkb-keymap-num-mods (xkb-keymap-new (xkb-context-new))))

(test-assert "xkb-keymap-mod-get-name"
  (let ((keymap (xkb-keymap-new (xkb-context-new))))
    (xkb-keymap-mod-get-name
     keymap (- (xkb-keymap-num-mods keymap) 1))))

(let ((keymap (xkb-keymap-new (xkb-context-new))))
  (test-equal "xkb-keymap-mod-get-index"
    (- (xkb-keymap-num-mods keymap) 1)
    (xkb-keymap-mod-get-index
     keymap (xkb-keymap-mod-get-name
             keymap (- (xkb-keymap-num-mods keymap) 1)))))
(let ((keymap (xkb-keymap-new (xkb-context-new))))
  (test-assert "xkb-keymap-num-layouts"
    (xkb-keymap-num-layouts keymap))
  (test-assert "xkb-keymap-layout-get-name"
    (xkb-keymap-layout-get-name keymap (- (xkb-keymap-num-layouts keymap) 1)))
  (test-equal "xkb-keymap-layout-get-index"
    (- (xkb-keymap-num-layouts keymap) 1)
    (xkb-keymap-layout-get-index
     keymap (xkb-keymap-layout-get-name
             keymap (- (xkb-keymap-num-layouts keymap) 1)))))



(test-end "context")
