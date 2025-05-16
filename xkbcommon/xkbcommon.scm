(define-module (xkbcommon xkbcommon)
  #:use-module ((rnrs base) #:select (assert))
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (system foreign)
  #:use-module (oop goops)
  #:use-module (xkbcommon config)
  #:export (make-xkb-rule-names))

(define (pointer->string* ptr)
  (if (null-pointer? ptr)
      #f
      (pointer->string ptr)))
(define-inlinable (string->pointer* str)
  (if str
      (string->pointer str)
      %null-pointer))
(define-inlinable (not-zero? n)
  (not (zero? n)))

(define XKB_KEYCODE_INVALID #xffffffff)
(define-syntax-rule (pointer->procedure/deloy body ...)
  (delay (pointer->procedure body ...)))
(define-syntax define-simple-xkb-type
  (lambda (x)
    (syntax-case x ()
      ((_ name is wrap unwrap)
       #`(begin (define-wrapped-pointer-type name
                  is wrap unwrap
                  (lambda (b p)
                    (format p
                            #,(datum->syntax
                               #'name
                               (string-append "#<"
                                              (object->string
                                               (syntax->datum #'name))
                                              " ~a>"))
                            (number->string (pointer-address
                                             (unwrap b))
                                            16))))
                (export is))))))
(define-simple-xkb-type <xkb-context>
  xkb-context?
  wrap-xkb-context
  unwrap-xkb-context)

;; (include-paths
;;  #:allocation
;;  #:virtual
;;  #:slot-ref
;;  (lambda (x)
;;    (map (cut xkb-context-include-path-get x <>)
;;         (iota
;;          (xkb-context-num-include-paths x)) ))
;;  #:slot-set! (const #f))
(define-simple-xkb-type <xkb-keymap>
  xkb-keymap?
  wrap-xkb-keymap
  unwrap-xkb-keymap)

(define-simple-xkb-type <xkb-state>
  xkb-state?
  wrap-xkb-state
  unwrap-xkb-state)

(define-public xkb_keycode_t uint32)
(define-public xkb_keysym_t uint32)
(define-public xkb_layout_index_t uint32)
(define-public xkb_layout_mask_t uint32)
(define-public xkb_level_index_t uint32)
(define-public xkb_mod_index_t uint32)
(define-public xkb_mod_mask_t uint32)
(define-public xkb_led_index_t uint32)
(define-public xkb_led_mask_t uint32)

(define-class <xkb-rule-names> ()
  (rules #:init-value #f
         #:init-keyword #:rules
         #:accessor xkb-rule-names-rules)
  (model #:init-value #f
         #:init-keyword #:model
         #:accessor xkb-rule-names-model)
  (layout #:init-value #f #:init-keyword #:layout #:accessor xkb-rule-names-layout)
  (variant #:init-value #f #:init-keyword #:variant #:accessor xkb-rule-names-variant)
  (options #:init-value #f #:init-keyword #:options #:accessor xkb-rule-names-options))

(define (xkb-rule-names? obj)
  (is-a? obj <xkb-rule-names>))
(define* (make-xkb-rule-names #:key
                              (rules #f)
                              (model #f)
                              (layout #f)
                              (variant #f)
                              (options #f))
  (make <xkb-rule-names>
    #:rules rules
    #:model model
    #:layout layout
    #:variant variant
    #:options options))

(define (xkb-rule-names->pointer x)
  (make-c-struct
   '(* * * * *)
   (list (string->pointer* (xkb-rule-names-rules x))
         (string->pointer* (xkb-rule-names-model x))
         (string->pointer* (xkb-rule-names-layout x))
         (string->pointer* (xkb-rule-names-variant x))
         (string->pointer* (xkb-rule-names-options x)))))

(export xkb-rule-names?
        ;; xkb-rule-names->pointer
        xkb-rule-names-rules
        xkb-rule-names-model
        xkb-rule-names-layout
        xkb-rule-names-variant
        xkb-rule-names-options)

(define-public xkb-keysym-get-name
  (let ((%func (pointer->procedure/deloy
                int
                (dynamic-func
                 "xkb_keysym_get_name"
                 (force %libxkbcommon))
                (list uint32 '* size_t))))
    (lambda (keysym)
      (let* ((f (force %func))
             (len (f keysym %null-pointer 0)))
        (if (= len -1)
            #f
            (let* ((bv (make-bytevector len))
                   (p (bytevector->pointer bv)))
              (f keysym p (1+ len))
              (pointer->string p)))))))

(define-public XKB_KEYSYM_NO_FLAGS 0)
(define-public XKB_KEYSYM_CASE_INSENSITIVE 1)

(define-public xkb-keysym-from-name
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_keysym_from_name"
                 (force %libxkbcommon))
                (list '* int))))
    (lambda* (name #:optional (flags XKB_KEYSYM_NO_FLAGS))
      ((force %func) (string->pointer name) flags))))

;; (define-public xkb-keysym-to-utf8
;;   (let ((%func (pointer->procedure/deloy
;;                 int
;;                 (dynamic-func
;;                  "xkb_keysym_to_utf8"
;;                  (force %libxkbcommon))
;;                 (list uint32 '* size_t))))
;;     (lambda (keysym buffer size)
;;       ((force %func) keysym (string->pointer buffer) size))))

;; (define-public xkb-keysym-to-utf32
;;   (let ((%func (pointer->procedure/deloy
;;                  uint32
;;                  (dynamic-func
;;                    "xkb_keysym_to_utf32"
;;                    (force %libxkbcommon))
;;                  (list uint32))))
;;     (lambda (keysym) (%func keysym))))
;; (define-public xkb-utf32-to-keysym
;;   (let ((%func (pointer->procedure/deloy
;;                  uint32
;;                  (dynamic-func
;;                    "xkb_utf32_to_keysym"
;;                    (force %libxkbcommon))
;;                  (list uint32))))
;;     (lambda (ucs) (%func ucs))))
(define-public xkb-keysym-to-upper
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_keysym_to_upper"
                 (force %libxkbcommon))
                (list uint32))))
    (lambda (ks) (%func ks))))
(define-public xkb-keysym-to-lower
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_keysym_to_lower"
                 (force %libxkbcommon))
                (list uint32))))
    (lambda (ks) (%func ks))))


;; context

(define-public XKB_CONTEXT_NO_FLAGS 0)
(define-public XKB_CONTEXT_NO_DEFAULT_INCLUDES 1)
(define-public XKB_CONTEXT_NO_ENVIRONMENT_NAMES 2)
(define-public xkb-context-new
  (let ((%func (pointer->procedure/deloy
                '*
                (dynamic-func
                 "xkb_context_new"
                 (force %libxkbcommon))
                (list int)))
        (finalizer (delay (dynamic-func
                           "xkb_context_unref"
                           (force %libxkbcommon)))))
    (lambda* (#:optional (flags XKB_CONTEXT_NO_FLAGS))
      (let ((p ((force %func) flags)))
        (set-pointer-finalizer! p (force finalizer))
        (wrap-xkb-context p)))))

;; (define-public xkb-context-set-user-data
;;   (let ((%func (pointer->procedure/deloy
;;                  void
;;                  (dynamic-func
;;                    "xkb_context_set_user_data"
;;                    (force %libxkbcommon))
;;                  (list '* '*))))
;;     (lambda (context user_data)
;;       ((force %func) (unwrap-xkb-context context) user_data))))
;; (define-public xkb-context-get-user-data
;;   (let ((%func (pointer->procedure/deloy
;;                  '*
;;                  (dynamic-func
;;                    "xkb_context_get_user_data"
;;                    (force %libxkbcommon))
;;                  (list '*))))
;;     (lambda (context)
;;       ((force %func) (unwrap-xkb-context context)))))
(define-public xkb-context-include-path-append
  (let ((%func (pointer->procedure/deloy
                 int
                 (dynamic-func
                   "xkb_context_include_path_append"
                   (force %libxkbcommon))
                 (list '* '*))))
    (lambda (context path)
      ((force %func) (unwrap-xkb-context context)
             (string->pointer path)))))
(define-public xkb-context-include-path-append-default
  (let ((%func (pointer->procedure/deloy
                 int
                 (dynamic-func
                   "xkb_context_include_path_append_default"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (context)
      ((force %func) (unwrap-xkb-context context)))))
(define-public xkb-context-include-path-reset-defaults
  (let ((%func (pointer->procedure/deloy
                 int
                 (dynamic-func
                   "xkb_context_include_path_reset_defaults"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (context)
      ((force %func) (unwrap-xkb-context context)))))
(define-public xkb-context-include-path-clear
  (let ((%func (pointer->procedure/deloy
                 void
                 (dynamic-func
                   "xkb_context_include_path_clear"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (context)
      ((force %func) (unwrap-xkb-context context)))))
(define-public xkb-context-num-include-paths
  (let ((%func (pointer->procedure/deloy
                 unsigned-int
                 (dynamic-func
                   "xkb_context_num_include_paths"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (context)
      ((force %func) (unwrap-xkb-context context)))))
(define-public xkb-context-include-path-get
  (let ((%func (pointer->procedure/deloy
                '*
                (dynamic-func
                 "xkb_context_include_path_get"
                 (force %libxkbcommon))
                (list '* unsigned-int))))
    (lambda (context index)
      (pointer->string*
       ((force %func) (unwrap-xkb-context context) index)))))

(define-public XKB_LOG_LEVEL_CRITICAL 10)
(define-public XKB_LOG_LEVEL_ERROR 20)
(define-public XKB_LOG_LEVEL_WARNING 30)
(define-public XKB_LOG_LEVEL_INFO 40)
(define-public XKB_LOG_LEVEL_DEBUG 50)

(define-public xkb-context-set-log-level
  (let ((%func (pointer->procedure/deloy
                void
                (dynamic-func
                 "xkb_context_set_log_level"
                 (force %libxkbcommon))
                (list '* int))))
    (lambda (context level)
      ((force %func) (unwrap-xkb-context context)
       level))))

(define-public xkb-context-get-log-level
  (let ((%func (pointer->procedure/deloy
                int
                (dynamic-func
                 "xkb_context_get_log_level"
                 (force %libxkbcommon))
                (list '*))))
    (lambda (context)
      ((force %func) (unwrap-xkb-context context)))))

(define-public xkb-context-set-log-verbosity
  (let ((%func (pointer->procedure/deloy
                void
                (dynamic-func
                 "xkb_context_set_log_verbosity"
                 (force %libxkbcommon))
                (list '* int))))
    (lambda (context verbosity)
      ((force %func) (unwrap-xkb-context context) verbosity))))

(define-public xkb-context-get-log-verbosity
  (let ((%func (pointer->procedure/deloy
                int
                (dynamic-func
                 "xkb_context_get_log_verbosity"
                 (force %libxkbcommon))
                (list '*))))
    (lambda (context)
      ((force %func) (unwrap-xkb-context context)))))


;; (define-public xkb-context-set-log-fn
;;   (let ((%func (pointer->procedure/deloy
;;                 void
;;                 (dynamic-func
;;                  "xkb_context_set_log_fn"
;;                  (force %libxkbcommon))
;;                 (list '* '*))))
;;     (lambda (context log_fn)
;;       ((force %func) (unwrap-xkb-context context) log_fn))))

(define-public XKB_KEYMAP_COMPILE_NO_FLAGS 0)
(define-public XKB_KEYMAP_FORMAT_TEXT_V1 1)

(define-public xkb-keymap-new
  (let ((%func (pointer->procedure/deloy
                '*
                (dynamic-func
                 "xkb_keymap_new_from_names"
                 (force %libxkbcommon))
                (list '* '* int)))
        (finalizer (delay (dynamic-func
                           "xkb_keymap_unref"
                           (force %libxkbcommon))))
        (%from-string (pointer->procedure/deloy
                       '*
                       (dynamic-func
                        "xkb_keymap_new_from_string"
                        (force %libxkbcommon))
                       (list '* '* int int))))
    (lambda* (context
              #:optional (names-or-string (make <xkb-rule-names>))
              #:key
              (format XKB_KEYMAP_FORMAT_TEXT_V1)
              (flags XKB_KEYMAP_COMPILE_NO_FLAGS))
      (assert (xkb-context? context))
      (assert (or (xkb-rule-names? names-or-string)
                  (string? names-or-string)))
      (define is-string (string? names-or-string))
      (let ((p (apply (force (if is-string %from-string %func))
                      (unwrap-xkb-context context)
                      (if (string? names-or-string)
                          (string->pointer names-or-string)
                          (xkb-rule-names->pointer names-or-string))
                      (append (if is-string
                                  (list format)
                                  '())
                              (list flags)))))
        (assert (not (null-pointer? p)))
        (set-pointer-finalizer! p (force finalizer))
        (wrap-xkb-keymap p)))))

(define-public xkb-keymap-get-as-string
  (let ((%func (pointer->procedure/deloy
                '*
                (dynamic-func
                 "xkb_keymap_get_as_string"
                 (force %libxkbcommon))
                (list '* int))))
    (lambda* (keymap #:key
                     (format XKB_KEYMAP_FORMAT_TEXT_V1))
      (assert (xkb-keymap? keymap))
      (pointer->string*
       ((force %func) (unwrap-xkb-keymap keymap)
        format)))))

(define-public xkb-keymap-min-keycode
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_keymap_min_keycode"
                 (force %libxkbcommon))
                (list '*))))
    (lambda (keymap)
      (assert (xkb-keymap? keymap))
      ((force %func) (unwrap-xkb-keymap keymap)))))

(define-public xkb-keymap-max-keycode
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_keymap_max_keycode"
                 (force %libxkbcommon))
                (list '*))))
    (lambda (keymap)
      (assert (xkb-keymap? keymap))
      ((force %func) (unwrap-xkb-keymap keymap)))))

(define-public xkb-keymap-key-for-each
  (let ((%func (pointer->procedure/deloy
                void
                (dynamic-func
                 "xkb_keymap_key_for_each"
                 (force %libxkbcommon))
                (list '* '* '*))))
    (lambda (keymap proc)
      (assert (xkb-keymap? keymap))
      (assert (procedure? proc))

      (define (iter keymap key data)
        (proc (wrap-xkb-keymap keymap) key))
      ((force %func) (unwrap-xkb-keymap keymap)
             (procedure->pointer void iter `(* ,uint32 *))
             %null-pointer))))
(define-public xkb-keymap-key-get-name
  (let ((%func (pointer->procedure/deloy
                '*
                (dynamic-func
                 "xkb_keymap_key_get_name"
                 (force %libxkbcommon))
                (list '* uint32))))
    (lambda (keymap key)
      (assert (xkb-keymap? keymap))
      (pointer->string*
       ((force %func) (unwrap-xkb-keymap keymap) key)))))
(define-public xkb-keymap-key-by-name
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_keymap_key_by_name"
                 (force %libxkbcommon))
                (list '* '*))))
    (lambda (keymap name)
      (let ((o ((force %func) (unwrap-xkb-keymap keymap)
                (string->pointer name))))
        (if (= o XKB_KEYCODE_INVALID)
            (throw 'xkb-keycode-invalid "~a is invalib!" name)
            o)))))
(define-public xkb-keymap-num-mods
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_keymap_num_mods"
                 (force %libxkbcommon))
                (list '*))))
    (lambda (keymap)
      (assert (xkb-keymap? keymap))
      ((force %func) (unwrap-xkb-keymap keymap)))))

(define-public xkb-keymap-mod-get-name
  (let ((%func (pointer->procedure/deloy
                '*
                (dynamic-func
                 "xkb_keymap_mod_get_name"
                 (force %libxkbcommon))
                (list '* uint32))))
    (lambda (keymap idx)
      (assert (xkb-keymap? keymap))
      (pointer->string*
       ((force %func) (unwrap-xkb-keymap keymap) idx)))))

(define-public xkb-keymap-mod-get-index
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_keymap_mod_get_index"
                 (force %libxkbcommon))
                (list '* '*))))
    (lambda (keymap name)
      (assert (xkb-keymap? keymap))
      (assert (string? name))
      ((force %func) (unwrap-xkb-keymap keymap)
             (string->pointer name)))))
(define-public xkb-keymap-num-layouts
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_keymap_num_layouts"
                 (force %libxkbcommon))
                (list '*))))
    (lambda (keymap)
      (assert (xkb-keymap? keymap))
      ((force %func) (unwrap-xkb-keymap keymap)))))
(define-public xkb-keymap-layout-get-name
  (let ((%func (pointer->procedure/deloy
                '*
                (dynamic-func
                 "xkb_keymap_layout_get_name"
                 (force %libxkbcommon))
                (list '* uint32))))
    (lambda (keymap idx)
      (assert (xkb-keymap? keymap))
      (pointer->string*
       ((force %func) (unwrap-xkb-keymap keymap) idx)))))
(define-public xkb-keymap-layout-get-index
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_keymap_layout_get_index"
                 (force %libxkbcommon))
                (list '* '*))))
    (lambda (keymap name)
      (assert (xkb-keymap? keymap))
      ((force %func) (unwrap-xkb-keymap keymap)
             (string->pointer name)))))
(define-public xkb-keymap-num-leds
  (let ((%func (pointer->procedure/deloy
                 uint32
                 (dynamic-func
                   "xkb_keymap_num_leds"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (keymap)
      ((force %func) (unwrap-xkb-keymap keymap)))))
(define-public xkb-keymap-led-get-name
  (let ((%func (pointer->procedure/deloy
                 '*
                 (dynamic-func
                   "xkb_keymap_led_get_name"
                   (force %libxkbcommon))
                 (list '* uint32))))
    (lambda (keymap idx)
      (pointer->string*
        ((force %func) (unwrap-xkb-keymap keymap) idx)))))
(define-public xkb-keymap-led-get-index
  (let ((%func (pointer->procedure/deloy
                 uint32
                 (dynamic-func
                   "xkb_keymap_led_get_index"
                   (force %libxkbcommon))
                 (list '* '*))))
    (lambda (keymap name)
      ((force %func) (unwrap-xkb-keymap keymap)
             (string->pointer name)))))
(define-public xkb-keymap-num-layouts-for-key
  (let ((%func (pointer->procedure/deloy
                 uint32
                 (dynamic-func
                   "xkb_keymap_num_layouts_for_key"
                   (force %libxkbcommon))
                 (list '* uint32))))
    (lambda (keymap key)
      ((force %func) (unwrap-xkb-keymap keymap) key))))
(define-public xkb-keymap-num-levels-for-key
  (let ((%func (pointer->procedure/deloy
                 uint32
                 (dynamic-func
                   "xkb_keymap_num_levels_for_key"
                   (force %libxkbcommon))
                 (list '* uint32 uint32))))
    (lambda (keymap key layout)
      ((force %func) (unwrap-xkb-keymap keymap) key layout))))
(define-public xkb-keymap-key-get-mods-for-level
  (let ((%func (pointer->procedure/deloy
                 size_t
                 (dynamic-func
                   "xkb_keymap_key_get_mods_for_level"
                   (force %libxkbcommon))
                 (list '*
                       uint32
                       uint32
                       uint32
                       '*
                       size_t))))
    (lambda (keymap key layout level masks_out masks_size)
      ((force %func) (unwrap-xkb-keymap keymap)
             key
             layout
             level
             masks_out
             masks_size))))
(define-public xkb-keymap-key-get-syms-by-level
  (let ((%func (pointer->procedure/deloy
                 int
                 (dynamic-func
                   "xkb_keymap_key_get_syms_by_level"
                   (force %libxkbcommon))
                 (list '* uint32 uint32 uint32 '*))))
    (lambda (keymap key layout level syms_out)
      ((force %func) (unwrap-xkb-keymap keymap)
             key
             layout
             level
             syms_out))))
(define-public xkb-keymap-key-repeats
  (let ((%func (pointer->procedure/deloy
                 int
                 (dynamic-func
                   "xkb_keymap_key_repeats"
                   (force %libxkbcommon))
                 (list '* uint32))))
    (lambda (keymap key)
      ((force %func) (unwrap-xkb-keymap keymap) key))))
(define-public xkb-state-new
  (let ((finalizer (delay (dynamic-func
                           "xkb_state_unref"
                           (force %libxkbcommon))))
        (%func (pointer->procedure/deloy
                '*
                (dynamic-func
                 "xkb_state_new"
                 (force %libxkbcommon))
                (list '*))))
    (lambda (keymap)
      (let ((o ((force %func) (unwrap-xkb-keymap keymap))))
        (set-pointer-finalizer! o (force finalizer))
        (wrap-xkb-state o)))))

(define-public xkb-state-get-keymap
  (let ((%func (pointer->procedure/deloy
                '*
                (dynamic-func
                 "xkb_state_get_keymap"
                 (force %libxkbcommon))
                (list '*))))
    (lambda (state)
      (wrap-xkb-keymap
       ((force %func) (unwrap-xkb-state state))))))
(define-public XKB_KEY_UP 0)
(define-public XKB_KEY_DOWN 1)

(define-public XKB_STATE_MODS_DEPRESSED 1)
(define-public XKB_STATE_MODS_LATCHED 2)
(define-public XKB_STATE_MODS_LOCKED 4)
(define-public XKB_STATE_MODS_EFFECTIVE 8)
(define-public XKB_STATE_LAYOUT_DEPRESSED 16)
(define-public XKB_STATE_LAYOUT_LATCHED 32)
(define-public XKB_STATE_LAYOUT_LOCKED 64)
(define-public XKB_STATE_LAYOUT_EFFECTIVE 128)
(define-public XKB_STATE_LEDS 256)

(define-public xkb-state-update-key
  (let ((%func (pointer->procedure/deloy
                int
                (dynamic-func
                 "xkb_state_update_key"
                 (force %libxkbcommon))
                (list '* uint32 int))))
    (lambda (state key direction)
      ((force %func) (unwrap-xkb-state state)
       key
       direction))))
(define-public xkb-state-update-mask
  (let ((%func (pointer->procedure/deloy
                 int
                 (dynamic-func
                   "xkb_state_update_mask"
                   (force %libxkbcommon))
                 (list '*
                       uint32
                       uint32
                       uint32
                       uint32
                       uint32
                       uint32))))
    (lambda (state
             depressed_mods
             latched_mods
             locked_mods
             depressed_layout
             latched_layout
             locked_layout)
      ((force %func) (unwrap-xkb-state state)
             depressed_mods
             latched_mods
             locked_mods
             depressed_layout
             latched_layout
             locked_layout))))
(define-public xkb-state-key-get-syms
  (let ((%func (pointer->procedure/deloy
                int
                (dynamic-func
                 "xkb_state_key_get_syms"
                 (force %libxkbcommon))
                (list '* uint32 '*))))
    (lambda (state key syms_out)
      ((force %func) (unwrap-xkb-state state) key syms_out))))
;; (define-public xkb-state-key-get-utf8
;;   (let ((%func (pointer->procedure/deloy
;;                  int
;;                  (dynamic-func
;;                    "xkb_state_key_get_utf8"
;;                    (force %libxkbcommon))
;;                  (list '* uint32 '* size_t))))
;;     (lambda (state key buffer size)
;;       ((force %func) (unwrap-xkb-state state)
;;              key
;;              (string->pointer buffer)
;;              size))))
;; (define-public xkb-state-key-get-utf32
;;   (let ((%func (pointer->procedure/deloy
;;                  uint32
;;                  (dynamic-func
;;                    "xkb_state_key_get_utf32"
;;                    (force %libxkbcommon))
;;                  (list '* uint32))))
;;     (lambda (state key)
;;       ((force %func) (unwrap-xkb-state state) key))))
(define-public xkb-state-key-get-one-sym
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_state_key_get_one_sym"
                 (force %libxkbcommon))
                (list '* uint32))))
    (lambda (state key)
      ((force %func) (unwrap-xkb-state state) key))))
(define-public xkb-state-key-get-layout
  (let ((%func (pointer->procedure/deloy
                 uint32
                 (dynamic-func
                   "xkb_state_key_get_layout"
                   (force %libxkbcommon))
                 (list '* uint32))))
    (lambda (state key)
      ((force %func) (unwrap-xkb-state state) key))))
(define-public xkb-state-key-get-level
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_state_key_get_level"
                 (force %libxkbcommon))
                (list '* uint32 uint32))))
    (lambda (state key layout)
      ((force %func) (unwrap-xkb-state state) key layout))))
(define-public XKB_STATE_MATCH_ANY 1)
(define-public XKB_STATE_MATCH_ALL 2)
(define-public XKB_STATE_MATCH_NON_EXCLUSIVE 65536)

(define-public xkb-state-serialize-mods
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_state_serialize_mods"
                 (force %libxkbcommon))
                (list '* int))))
    (lambda (state components)
      ((force %func) (unwrap-xkb-state state)
       components))))

(define-public xkb-state-serialize-layout
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_state_serialize_layout"
                 (force %libxkbcommon))
                (list '* int))))
    (lambda (state components)
      ((force %func) (unwrap-xkb-state state)
       components))))
(define-public xkb-state-mod-name-is-active
  (let ((%func (pointer->procedure/deloy
                int
                (dynamic-func
                 "xkb_state_mod_name_is_active"
                 (force %libxkbcommon))
                (list '* '* int))))
    (lambda (state name type)
      ((force %func) (unwrap-xkb-state state)
       (string->pointer name)
       type))))
(define-public xkb-state-mod-index-is-active
  (let ((%func (pointer->procedure/deloy
                int
                (dynamic-func
                 "xkb_state_mod_index_is_active"
                 (force %libxkbcommon))
                (list '* uint32 int))))
    (lambda (state idx type)
      ((force %func) (unwrap-xkb-state state)
       idx
       type))))
(begin
  (define-public XKB_CONSUMED_MODE_XKB 0)
  (define-public XKB_CONSUMED_MODE_GTK 1))
(define-public xkb-state-key-get-consumed-mods2
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_state_key_get_consumed_mods2"
                 (force %libxkbcommon))
                (list '* uint32 int))))
    (lambda (state key mode)
      ((force %func) (unwrap-xkb-state state)
       key
       mode))))
(define-public xkb-state-key-get-consumed-mods
  (let ((%func (pointer->procedure/deloy
                 uint32
                 (dynamic-func
                   "xkb_state_key_get_consumed_mods"
                   (force %libxkbcommon))
                 (list '* uint32))))
    (lambda (state key)
      ((force %func) (unwrap-xkb-state state) key))))
(define-public xkb-state-mod-index-is-consumed2
  (let ((%func (pointer->procedure/deloy
                int
                (dynamic-func
                 "xkb_state_mod_index_is_consumed2"
                 (force %libxkbcommon))
                (list '* uint32 uint32 int))))
    (lambda (state key idx mode)
      ((force %func) (unwrap-xkb-state state)
       key
       idx
       mode))))
(define-public xkb-state-mod-index-is-consumed
  (let ((%func (pointer->procedure/deloy
                 int
                 (dynamic-func
                   "xkb_state_mod_index_is_consumed"
                   (force %libxkbcommon))
                 (list '* uint32 uint32))))
    (lambda (state key idx)
      ((force %func) (unwrap-xkb-state state) key idx))))
(define-public xkb-state-mod-mask-remove-consumed
  (let ((%func (pointer->procedure/deloy
                uint32
                (dynamic-func
                 "xkb_state_mod_mask_remove_consumed"
                 (force %libxkbcommon))
                (list '* uint32 uint32))))
    (lambda (state key mask)
      ((force %func) (unwrap-xkb-state state) key mask))))
(define-public xkb-state-layout-name-is-active
  (let ((%func (pointer->procedure/deloy
                int
                (dynamic-func
                 "xkb_state_layout_name_is_active"
                 (force %libxkbcommon))
                (list '* '* int))))
    (lambda (state name type)
      ((force %func) (unwrap-xkb-state state)
       (string->pointer name)
       type))))
(define-public xkb-state-layout-index-is-active
  (let ((%func (pointer->procedure/deloy
                int
                (dynamic-func
                 "xkb_state_layout_index_is_active"
                 (force %libxkbcommon))
                (list '* uint32 int))))
    (lambda (state idx type)
      ((force %func) (unwrap-xkb-state state)
       idx
       type))))
(define-public xkb-state-led-name-is-active
  (let ((%func (pointer->procedure/deloy
                int
                (dynamic-func
                 "xkb_state_led_name_is_active"
                 (force %libxkbcommon))
                (list '* '*))))
    (lambda (state name)
      ((force %func) (unwrap-xkb-state state)
       (string->pointer name)))))
(define-public xkb-state-led-index-is-active
  (let ((%func (pointer->procedure/deloy
                int
                (dynamic-func
                 "xkb_state_led_index_is_active"
                 (force %libxkbcommon))
                (list '* uint32))))
    (lambda (state idx)
      ((force %func) (unwrap-xkb-state state) idx))))
