(define-module (xkbcommon xkbcommon)
  #:use-module ((rnrs base) #:select (assert))
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (system foreign)
  #:use-module (oop goops)
  #:use-module (xkbcommon config)
  #:export (make-xkb-rule-names
            xkb-rule-names?
            xkb-rule-names-rules
            xkb-rule-names-model
            xkb-rule-names-layout
            xkb-rule-names-variant
            xkb-rule-names-options))

(define-syntax-parameter %
  (lambda (x)
    (syntax-violation '% "% used outside of a define-xkb-procedure" x)))

(define-syntax define-xkb-procedure
  (syntax-rules ()
    ((define-xkb-procedure (pname args ...)
       (c-return c-name c-args)
       body ...)
     (define-public pname
       (let ((%% (delay (pointer->procedure
                         c-return
                         (dynamic-func c-name (force %libxkbcommon))
                         c-args))))
         (syntax-parameterize
             ((% (make-variable-transformer
                  (lambda (x)
                    (syntax-case x ()
                      ((_ xargs (... ...))
                       #`((force %%) xargs (... ...)))
                      (o
                       (identifier? #'o)
                       #`(force %%)))))
                 ))
           (lambda* (args ...)
             #((name . pname))
             body ...)))))))

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
(define const-char-* '*)

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


(define-xkb-procedure (xkb-keysym-get-name keysym)
  (int "xkb_keysym_get_name" (list uint32 '* size_t))
  (let* ((f %)
         (len (f keysym %null-pointer 0)))
    (if (= len -1)
        #f
        (let* ((bv (make-bytevector len))
               (p (bytevector->pointer bv)))
          (f keysym p (1+ len))
          (pointer->string p)))))

(define-public XKB_KEYSYM_NO_FLAGS 0)
(define-public XKB_KEYSYM_CASE_INSENSITIVE 1)

(define-xkb-procedure (xkb-keysym-from-name
                       name
                       #:optional (flags XKB_KEYSYM_NO_FLAGS))
  (uint32 "xkb_keysym_from_name" (list '* int))
  (% (string->pointer name) flags))

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
(define-xkb-procedure (xkb-keysym-to-upper ks)
  (uint32 "xkb_keysym_to_upper" (list uint32))
  (% ks))

(define-xkb-procedure (xkb-keysym-to-lower ks)
  (uint32 "xkb_keysym_to_lower" (list uint32))
  (% ks))

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
(define-xkb-procedure (xkb-context-include-path-append context path)
  (int "xkb_context_include_path_append" (list '* '*))
  (assert (xkb-context? context))
  (% (unwrap-xkb-context context)
     (string->pointer path)))

(define-xkb-procedure (xkb-context-include-path-append-default context)
  (int "xkb_context_include_path_append_default" '(*))
  (assert (xkb-context? context))
  (% (unwrap-xkb-context context)))

(define-xkb-procedure (xkb-context-include-path-reset-defaults context)
  (int "xkb_context_include_path_reset_defaults" (list '*))
  (assert (xkb-context? context))
  (% (unwrap-xkb-context context)))

(define-xkb-procedure (xkb-context-include-path-clear context)
  (void "xkb_context_include_path_clear" (list '*))
  (assert (xkb-context? context))
  (% (unwrap-xkb-context context)))

(define-xkb-procedure (xkb-context-num-include-paths context)
  (unsigned-int "xkb_context_num_include_paths" (list '*))
  (assert (xkb-context? context))
  (% (unwrap-xkb-context context)))

(define-xkb-procedure (xkb-context-include-path-get context index)
  ('* "xkb_context_include_path_get" (list '* unsigned-int))
  (assert (xkb-context? context))
  (pointer->string*
   (% (unwrap-xkb-context context) index)))

;; (define-public (xkb-context-include-paths context)
;;   (map (cut xkb-context-include-path-get context <>)
;;        (iota (xkb-context-num-include-paths context))))

(define-public XKB_LOG_LEVEL_CRITICAL 10)
(define-public XKB_LOG_LEVEL_ERROR 20)
(define-public XKB_LOG_LEVEL_WARNING 30)
(define-public XKB_LOG_LEVEL_INFO 40)
(define-public XKB_LOG_LEVEL_DEBUG 50)

(define-xkb-procedure (xkb-context-set-log-level! context level)
  (void "xkb_context_set_log_level" (list '* int))
  (assert (xkb-context? context))
  (% (unwrap-xkb-context context) level))
(define-public set-xkb-context-log-level!
  xkb-context-set-log-level!)

(define-xkb-procedure (xkb-context-get-log-level context)
  (int "xkb_context_get_log_level" (list '*))
  (assert (xkb-context? context))
  (% (unwrap-xkb-context context)))

(define-public xkb-context-log-level
  (make-procedure-with-setter xkb-context-get-log-level
                              xkb-context-set-log-level!))

(define-xkb-procedure (xkb-context-set-log-verbosity! context verbosity)
  (void "xkb_context_set_log_verbosity" (list '* int))
  (assert (xkb-context? context))
  (% (unwrap-xkb-context context) verbosity))

(define-xkb-procedure (xkb-context-get-log-verbosity context)
  (int "xkb_context_get_log_verbosity" (list '*))
  (assert (xkb-context? context))
  (% (unwrap-xkb-context context)))

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
      #((name . xkb-keymap-new))
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

(define-xkb-procedure (xkb-keymap-get-as-string
                       keymap
                       #:key
                       (format XKB_KEYMAP_FORMAT_TEXT_V1))
  ('* "xkb_keymap_get_as_string" (list '* int))
  (assert (xkb-keymap? keymap))
  (let ((str-p (% (unwrap-xkb-keymap keymap) format)))
    (pointer->string* str-p)))

(define-xkb-procedure (xkb-keymap-min-keycode keymap)
  (uint32 "xkb_keymap_min_keycode" (list '*))
  (assert (xkb-keymap? keymap))
  (% (unwrap-xkb-keymap keymap)))

(define-xkb-procedure (xkb-keymap-max-keycode keymap)
  (uint32 "xkb_keymap_max_keycode" (list '*))
  (assert (xkb-keymap? keymap))
  (% (unwrap-xkb-keymap keymap)))

(define-xkb-procedure (xkb-keymap-key-for-each keymap proc)
  (void "xkb_keymap_key_for_each" (list '* '* '*))
  (assert (xkb-keymap? keymap))
  (assert (procedure? proc))
  (define (iter keymap key data)
    (proc (wrap-xkb-keymap keymap) key))
  (% (unwrap-xkb-keymap keymap)
     (procedure->pointer void iter `(* ,uint32 *))
     %null-pointer))

(define-xkb-procedure (xkb-keymap-key-get-name keymap key)
  (const-char-* "xkb_keymap_key_get_name" (list '* xkb_keycode_t))
  (assert (xkb-keymap? keymap))
  (pointer->string*
   (% (unwrap-xkb-keymap keymap) key)))

(define-xkb-procedure (xkb-keymap-key-by-name keymap name)
  (uint32 "xkb_keymap_key_by_name" (list '* '*))
  (assert (xkb-keymap? keymap))
  (let ((o (% (unwrap-xkb-keymap keymap) (string->pointer name))))
    (if (= o XKB_KEYCODE_INVALID)
        (throw 'xkb-keycode-invalid "~a is invalib!" name)
        o)))
(define-xkb-procedure (xkb-keymap-num-mods keymap)
  (uint32 "xkb_keymap_num_mods" (list '*))
  (assert (xkb-keymap? keymap))
  (% (unwrap-xkb-keymap keymap)))

(define-xkb-procedure (xkb-keymap-mod-get-name keymap idx)
  ('* "xkb_keymap_mod_get_name" (list '* uint32))
  (assert (xkb-keymap? keymap))
  (pointer->string*
   (% (unwrap-xkb-keymap keymap) idx)))

(define-xkb-procedure (xkb-keymap-mod-get-index keymap name)
  (uint32  "xkb_keymap_mod_get_index" (list '* '*))
  (assert (xkb-keymap? keymap))
  (assert (string? name))
  (% (unwrap-xkb-keymap keymap)
     (string->pointer name)))

(define-xkb-procedure (xkb-keymap-num-layouts keymap)
  (uint32 "xkb_keymap_num_layouts" (list '*))
  (assert (xkb-keymap? keymap))
  (% (unwrap-xkb-keymap keymap)))

(define-xkb-procedure (xkb-keymap-layout-get-name keymap idx)
  ('* "xkb_keymap_layout_get_name" (list '* uint32))
  (assert (xkb-keymap? keymap))
  (pointer->string*
   (% (unwrap-xkb-keymap keymap) idx)))

(define-xkb-procedure (xkb-keymap-layout-get-index keymap name)
  (uint32 "xkb_keymap_layout_get_index" (list '* '*))
  (assert (xkb-keymap? keymap))
  (% (unwrap-xkb-keymap keymap) (string->pointer name)))

(define-xkb-procedure (xkb-keymap-num-leds keymap)
  (uint32 "xkb_keymap_num_leds" (list '*))
  (% (unwrap-xkb-keymap keymap)))

(define-xkb-procedure (xkb-keymap-led-get-name keymap idx)
  ('* "xkb_keymap_led_get_name" (list '* uint32))
  (pointer->string*
   (% (unwrap-xkb-keymap keymap) idx)))

(define-xkb-procedure (xkb-keymap-led-get-index keymap name)
  (uint32 "xkb_keymap_led_get_index" (list '* '*))
  (% (unwrap-xkb-keymap keymap)
     (string->pointer name)))

(define-xkb-procedure (xkb-keymap-num-layouts-for-key keymap key)
  (uint32 "xkb_keymap_num_layouts_for_key" (list '* uint32))
  (% (unwrap-xkb-keymap keymap) key))

(define-xkb-procedure (xkb-keymap-num-levels-for-key keymap key layout)
  (xkb_level_index_t "xkb_keymap_num_levels_for_key"
                     (list '* xkb_keycode_t xkb_layout_index_t))
  (% (unwrap-xkb-keymap keymap) key layout))

;; (define-public xkb-keymap-key-get-mods-for-level
;;   (let ((%func (pointer->procedure/deloy
;;                 size_t
;;                 (dynamic-func
;;                  "xkb_keymap_key_get_mods_for_level"
;;                  (force %libxkbcommon))
;;                 (list '*
;;                       uint32
;;                       uint32
;;                       uint32
;;                       '*
;;                       size_t))))
;;     (lambda (keymap key layout level masks_out masks_size)
;;       ((force %func) (unwrap-xkb-keymap keymap)
;;        key
;;        layout
;;        level
;;        masks_out
;;        masks_size))))

;; (define-public xkb-keymap-key-get-syms-by-level
;;   (let ((%func (pointer->procedure/deloy
;;                 int
;;                 (dynamic-func
;;                  "xkb_keymap_key_get_syms_by_level"
;;                  (force %libxkbcommon))
;;                 (list '* uint32 uint32 uint32 '*))))
;;     (lambda (keymap key layout level syms_out)
;;       ((force %func) (unwrap-xkb-keymap keymap)
;;        key
;;        layout
;;        level
;;        syms_out))))

(define-xkb-procedure (xkb-keymap-key-repeats keymap key)
  (int "xkb_keymap_key_repeats" (list '* uint32))
  (% (unwrap-xkb-keymap keymap) key))

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

(define-xkb-procedure (xkb-state-get-keymap state)
  ('* "xkb_state_get_keymap" (list '*))
  (assert (xkb-state? state))
  (wrap-xkb-keymap
   (% (unwrap-xkb-state state))))

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

(define-xkb-procedure (xkb-state-update-key state key direction)
  (int "xkb_state_update_key" (list '* uint32 int))
  (% (unwrap-xkb-state state) key direction))

(define-xkb-procedure (xkb-state-update-mask state depressed_mods
                                             latched_mods
                                             locked_mods
                                             depressed_layout
                                             latched_layout
                                             locked_layout)
  (int "xkb_state_update_mask"
       (list '* uint32 uint32 uint32 uint32 uint32 uint32))
  (assert (xkb-state? state))
  (% (unwrap-xkb-state state)
     depressed_mods
     latched_mods
     locked_mods
     depressed_layout
     latched_layout
     locked_layout))
;; (define-xkb-procedure (xkb-state-key-get-syms state key syms_out)
;;   (int
;;    "xkb_state_key_get_syms" (list '* uint32 '*))
;;   (assert (xkb-state? state))
;;   (% (unwrap-xkb-state state) key syms_out))
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
(define-xkb-procedure (xkb-state-key-get-utf32 state key)
  (uint32 "xkb_state_key_get_utf32" (list '* xkb_keycode_t))
  (% (unwrap-xkb-state state) key))

(define-xkb-procedure (xkb-state-key-get-one-sym state key)
  (uint32 "xkb_state_key_get_one_sym" (list '* uint32))
  (% (unwrap-xkb-state state) key))

(define-xkb-procedure (xkb-state-key-get-layout state key)
  (uint32 "xkb_state_key_get_layout" (list '* uint32))
  (% (unwrap-xkb-state state) key))

(define-xkb-procedure (xkb-state-key-get-level state key layout)
  (uint32
   "xkb_state_key_get_level"
   (list '* uint32 uint32))

  (% (unwrap-xkb-state state) key layout))

(define-public XKB_STATE_MATCH_ANY 1)
(define-public XKB_STATE_MATCH_ALL 2)
(define-public XKB_STATE_MATCH_NON_EXCLUSIVE 65536)

(define-xkb-procedure (xkb-state-serialize-mods state components)
  (uint32 "xkb_state_serialize_mods" (list '* int))
  (% (unwrap-xkb-state state) components))

(define-xkb-procedure (xkb-state-serialize-layout state components)
  (uint32
   "xkb_state_serialize_layout" (list '* int))
  (% (unwrap-xkb-state state) components))

(define-xkb-procedure (xkb-state-mod-name-is-active state name type)
  (int "xkb_state_mod_name_is_active" (list '* '* int))
  (case-state-active
   (% (unwrap-xkb-state state) (string->pointer name) type)
   #:throw
   (throw 'xkb-state-invalid "modifier name ~a does not exist in the keymap!" name)))

(define-xkb-procedure (xkb-state-mod-index-is-active state idx type)
  (int "xkb_state_mod_index_is_active" (list '* uint32 int))
  (% (unwrap-xkb-state state) idx type))

(define-public XKB_CONSUMED_MODE_XKB 0)
(define-public XKB_CONSUMED_MODE_GTK 1)

(define-xkb-procedure (xkb-state-key-get-consumed-mods2 state key mode)
  (uint32 "xkb_state_key_get_consumed_mods2" (list '* uint32 int))
  (% (unwrap-xkb-state state) key mode))

(define-xkb-procedure (xkb-state-key-get-consumed-mods state key)
  (uint32 "xkb_state_key_get_consumed_mods" (list '* uint32))
  (% (unwrap-xkb-state state) key))

(define-syntax-rule (case-state-active
                     x
                     #:throw
                     throw)
  (case x
    ((1)#t)
    ((0) #f)
    ((-1) throw)))

(define-xkb-procedure (xkb-state-mod-index-is-consumed2 state key idx mode)
  (int "xkb_state_mod_index_is_consumed2" (list '* uint32 uint32 int))

  (case-state-active (% (unwrap-xkb-state state) key idx mode)
                     #:throw
                     (throw 'xkb-state-invalid "~a modifier index is not valid in the keymap!" idx)))

(define-xkb-procedure (xkb-state-mod-index-is-consumed state key idx)
  (int "xkb_state_mod_index_is_consumed" (list '* uint32 uint32))
  (case-state-active
   (% (unwrap-xkb-state state) key idx)
   #:throw
   (throw 'xkb-state-invalid "~a modifier index is not valid in the keymap!" idx)))

(define-xkb-procedure (xkb-state-mod-mask-remove-consumed state key mask)
  (uint32 "xkb_state_mod_mask_remove_consumed" (list '* uint32 uint32))
  (% (unwrap-xkb-state state) key mask))

(define-xkb-procedure (xkb-state-layout-name-is-active state name type)
  (int
   "xkb_state_layout_name_is_active" (list '* '* int))
  (% (unwrap-xkb-state state) (string->pointer name) type))

(define-xkb-procedure (xkb-state-layout-index-is-active state idx type)
  (int "xkb_state_layout_index_is_active" (list '* uint32 int))
  (% (unwrap-xkb-state state) idx type))

(define-xkb-procedure (xkb-state-led-name-is-active state name)
  (int "xkb_state_led_name_is_active" (list '* '*))
  (assert (xkb-state? state))
  (% (unwrap-xkb-state state) (string->pointer name)))


(define-xkb-procedure (xkb-state-led-index-is-active state idx)
  (int "xkb_state_led_index_is_active" (list '* xkb_led_index_t))
  (assert (xkb-state? state))
  (case-state-active
   (% (unwrap-xkb-state state) idx)
   #:throw
   (throw 'xkb-state-invalid "~a index is not valid in the keymap!" idx)))
