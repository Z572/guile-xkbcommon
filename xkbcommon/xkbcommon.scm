(define-module (xkbcommon xkbcommon)
  #:use-module ((rnrs base) #:select (assert))
  #:use-module (srfi srfi-26)
  #:use-module (bytestructure-class)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (bytestructures guile)
  #:use-module (oop goops)
  #:use-module (xkbcommon config))

(define (pointer->string* ptr)
  (if (ffi:null-pointer? ptr)
      #f
      (ffi:pointer->string ptr)))
(define-inlinable (not-zero? n)
  (not (zero? n)))

(define-public %xkb-context-struct (bs:unknow))
(define-bytestructure-class <xkb-context> ()
  %xkb-context-struct
  wrap-xkb-context
  unwrap-xkb-context
  xkb-context?
  (include-paths
   #:allocation
   #:virtual
   #:slot-ref
   (lambda (x)
     (map (cut xkb-context-include-path-get x <>)
          (iota
           (xkb-context-num-include-paths x)) ))
   #:slot-set! (const #f)))

(begin
  (define-public %xkb-keymap-struct (bs:unknow))
  (define-bytestructure-class
    <xkb-keymap>
    ()
    %xkb-keymap-struct
    wrap-xkb-keymap
    unwrap-xkb-keymap
    xkb-keymap?))
(begin
  (define-public %xkb-state-struct (bs:unknow))
  (define-bytestructure-class
    <xkb-state>
    ()
    %xkb-state-struct
    wrap-xkb-state
    unwrap-xkb-state
    xkb-state?))
(define-public xkb_keycode_t uint32)
(define-public xkb_keysym_t uint32)
(define-public xkb_layout_index_t uint32)
(define-public xkb_layout_mask_t uint32)
(define-public xkb_level_index_t uint32)
(define-public xkb_mod_index_t uint32)
(define-public xkb_mod_mask_t uint32)
(define-public xkb_led_index_t uint32)
(define-public xkb_led_mask_t uint32)
(begin
  (define-public %xkb-rule-names-struct
    (bs:struct
      `((rules ,cstring-pointer*)
        (model ,cstring-pointer*)
        (layout ,cstring-pointer*)
        (variant ,cstring-pointer*)
        (options ,cstring-pointer*))))
  (define-bytestructure-class
    <xkb-rule-names>
    ()
    %xkb-rule-names-struct
    wrap-xkb-rule-names
    unwrap-xkb-rule-names
    xkb-rule-names?
    (rules #:init-keyword #:rules #:accessor .rules)
    (model #:init-keyword #:model #:accessor .model)
    (layout
      #:init-keyword
      #:layout
      #:accessor
      .layout)
    (variant
      #:init-keyword
      #:variant
      #:accessor
      .variant)
    (options
      #:init-keyword
      #:options
      #:accessor
      .options))
  (export .rules .model .layout .variant .options))
(define-public xkb-keysym-get-name
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_keysym_get_name"
                   (force %libxkbcommon))
                 (list ffi:uint32 '* ffi:size_t))))
    (lambda (keysym buffer size)
      (%func keysym (ffi:string->pointer buffer) size))))
(begin
  (define-public %xkb-keysym-flags-enum
    (bs:enum
      '((XKB_KEYSYM_NO_FLAGS 0)
        (XKB_KEYSYM_CASE_INSENSITIVE 1))))
  (define-public XKB_KEYSYM_NO_FLAGS 0)
  (define-public XKB_KEYSYM_CASE_INSENSITIVE 1)
  (define-public (%xkb-keysym-flags-enum->number o)
    (bs:enum->integer %xkb-keysym-flags-enum o)))
(define-public xkb-keysym-from-name
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_keysym_from_name"
                   (force %libxkbcommon))
                 (list '* ffi:int))))
    (lambda (name flags)
      (%func (ffi:string->pointer name)
             (%xkb-keysym-flags-enum->number flags)))))
(define-public xkb-keysym-to-utf8
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_keysym_to_utf8"
                   (force %libxkbcommon))
                 (list ffi:uint32 '* ffi:size_t))))
    (lambda (keysym buffer size)
      (%func keysym (ffi:string->pointer buffer) size))))
(define-public xkb-keysym-to-utf32
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_keysym_to_utf32"
                   (force %libxkbcommon))
                 (list ffi:uint32))))
    (lambda (keysym) (%func keysym))))
(define-public xkb-utf32-to-keysym
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_utf32_to_keysym"
                   (force %libxkbcommon))
                 (list ffi:uint32))))
    (lambda (ucs) (%func ucs))))
(define-public xkb-keysym-to-upper
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_keysym_to_upper"
                   (force %libxkbcommon))
                 (list ffi:uint32))))
    (lambda (ks) (%func ks))))
(define-public xkb-keysym-to-lower
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_keysym_to_lower"
                   (force %libxkbcommon))
                 (list ffi:uint32))))
    (lambda (ks) (%func ks))))


;; context

(define-public %xkb-context-flags-enum
  (bs:enum
   '((XKB_CONTEXT_NO_FLAGS 0)
     (XKB_CONTEXT_NO_DEFAULT_INCLUDES 1)
     (XKB_CONTEXT_NO_ENVIRONMENT_NAMES 2))))
(define-public XKB_CONTEXT_NO_FLAGS 0)
(define-public XKB_CONTEXT_NO_DEFAULT_INCLUDES 1)
(define-public XKB_CONTEXT_NO_ENVIRONMENT_NAMES 2)
(define-public (%xkb-context-flags-enum->number o)
  (bs:enum->integer %xkb-context-flags-enum o))
(define-public xkb-context-new
  (let ((%func (ffi:pointer->procedure
                '*
                (dynamic-func
                 "xkb_context_new"
                 (force %libxkbcommon))
                (list ffi:int)))
        (finalizer (dynamic-func
                    "xkb_context_unref"
                    (force %libxkbcommon))))
    (lambda* (#:optional (flags XKB_CONTEXT_NO_FLAGS))
      (let ((p (%func (%xkb-context-flags-enum->number flags))))
        (ffi:set-pointer-finalizer! p finalizer)
        (wrap-xkb-context p)))))

(define-public xkb-context-set-user-data
  (let ((%func (ffi:pointer->procedure
                 ffi:void
                 (dynamic-func
                   "xkb_context_set_user_data"
                   (force %libxkbcommon))
                 (list '* '*))))
    (lambda (context user_data)
      (%func (unwrap-xkb-context context) user_data))))
(define-public xkb-context-get-user-data
  (let ((%func (ffi:pointer->procedure
                 '*
                 (dynamic-func
                   "xkb_context_get_user_data"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (context)
      (%func (unwrap-xkb-context context)))))
(define-public xkb-context-include-path-append
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_context_include_path_append"
                   (force %libxkbcommon))
                 (list '* '*))))
    (lambda (context path)
      (%func (unwrap-xkb-context context)
             (ffi:string->pointer path)))))
(define-public xkb-context-include-path-append-default
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_context_include_path_append_default"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (context)
      (%func (unwrap-xkb-context context)))))
(define-public xkb-context-include-path-reset-defaults
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_context_include_path_reset_defaults"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (context)
      (%func (unwrap-xkb-context context)))))
(define-public xkb-context-include-path-clear
  (let ((%func (ffi:pointer->procedure
                 ffi:void
                 (dynamic-func
                   "xkb_context_include_path_clear"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (context)
      (%func (unwrap-xkb-context context)))))
(define-public xkb-context-num-include-paths
  (let ((%func (ffi:pointer->procedure
                 ffi:unsigned-int
                 (dynamic-func
                   "xkb_context_num_include_paths"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (context)
      (%func (unwrap-xkb-context context)))))
(define-public xkb-context-include-path-get
  (let ((%func (ffi:pointer->procedure
                 '*
                 (dynamic-func
                   "xkb_context_include_path_get"
                   (force %libxkbcommon))
                 (list '* ffi:unsigned-int))))
    (lambda (context index)
      (pointer->string*
        (%func (unwrap-xkb-context context) index)))))
(begin
  (define-public %xkb-log-level-enum
    (bs:enum
      '((XKB_LOG_LEVEL_CRITICAL 10)
        (XKB_LOG_LEVEL_ERROR 20)
        (XKB_LOG_LEVEL_WARNING 30)
        (XKB_LOG_LEVEL_INFO 40)
        (XKB_LOG_LEVEL_DEBUG 50))))
  (define-public XKB_LOG_LEVEL_CRITICAL 10)
  (define-public XKB_LOG_LEVEL_ERROR 20)
  (define-public XKB_LOG_LEVEL_WARNING 30)
  (define-public XKB_LOG_LEVEL_INFO 40)
  (define-public XKB_LOG_LEVEL_DEBUG 50)
  (define-public (%xkb-log-level-enum->number o)
    (bs:enum->integer %xkb-log-level-enum o)))
(define-public xkb-context-set-log-level
  (let ((%func (ffi:pointer->procedure
                 ffi:void
                 (dynamic-func
                   "xkb_context_set_log_level"
                   (force %libxkbcommon))
                 (list '* ffi:int))))
    (lambda (context level)
      (%func (unwrap-xkb-context context)
             (%xkb-log-level-enum->number level)))))
(define-public xkb-context-get-log-level
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_context_get_log_level"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (context)
      (%func (unwrap-xkb-context context)))))
(define-public xkb-context-set-log-verbosity
  (let ((%func (ffi:pointer->procedure
                 ffi:void
                 (dynamic-func
                   "xkb_context_set_log_verbosity"
                   (force %libxkbcommon))
                 (list '* ffi:int))))
    (lambda (context verbosity)
      (%func (unwrap-xkb-context context) verbosity))))
(define-public xkb-context-get-log-verbosity
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_context_get_log_verbosity"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (context)
      (%func (unwrap-xkb-context context)))))
(define-public xkb-context-set-log-fn
  (let ((%func (ffi:pointer->procedure
                 ffi:void
                 (dynamic-func
                   "xkb_context_set_log_fn"
                   (force %libxkbcommon))
                 (list '* '*))))
    (lambda (context log_fn)
      (%func (unwrap-xkb-context context) log_fn))))
(begin
  (define-public %xkb-keymap-compile-flags-enum
    (bs:enum '((XKB_KEYMAP_COMPILE_NO_FLAGS 0))))
  (define-public XKB_KEYMAP_COMPILE_NO_FLAGS 0)
  (define-public (%xkb-keymap-compile-flags-enum->number o)
    (bs:enum->integer
      %xkb-keymap-compile-flags-enum
      o)))
(define-public xkb-keymap-new-from-names
  (let ((%func (ffi:pointer->procedure
                '*
                (dynamic-func
                 "xkb_keymap_new_from_names"
                 (force %libxkbcommon))
                (list '* '* ffi:int)))
        (finalizer (dynamic-func
                    "xkb_keymap_unref"
                    (force %libxkbcommon))))
    (lambda* (context #:optional names (flags XKB_KEYMAP_COMPILE_NO_FLAGS))
      (let ((p (%func (unwrap-xkb-context context)
                      (unwrap-xkb-rule-names names)
                      (%xkb-keymap-compile-flags-enum->number flags))))
        (ffi:set-pointer-finalizer! p finalizer)
        (wrap-xkb-keymap p)))))
(begin
  (define-public %xkb-keymap-format-enum
    (bs:enum '((XKB_KEYMAP_FORMAT_TEXT_V1 1))))
  (define-public XKB_KEYMAP_FORMAT_TEXT_V1 1)
  (define-public (%xkb-keymap-format-enum->number o)
    (bs:enum->integer %xkb-keymap-format-enum o)))

(define-public xkb-keymap-new-from-string
  (let ((%func (ffi:pointer->procedure
                '*
                (dynamic-func
                 "xkb_keymap_new_from_string"
                 (force %libxkbcommon))
                (list '* '* ffi:int ffi:int)))
        (finalizer (dynamic-func
                    "xkb_keymap_unref"
                    (force %libxkbcommon))))
    (lambda* (context string
                      #:optional
                      (format XKB_KEYMAP_FORMAT_TEXT_V1)
                      (flags XKB_KEYMAP_COMPILE_NO_FLAGS))
      (let ((p (%func (unwrap-xkb-context context)
                      (ffi:string->pointer string)
                      (%xkb-keymap-format-enum->number format)
                      (%xkb-keymap-compile-flags-enum->number flags))))
        (ffi:set-pointer-finalizer! p finalizer)
        (wrap-xkb-keymap p)))))

(define-public xkb-keymap-get-as-string
  (let ((%func (ffi:pointer->procedure
                '*
                (dynamic-func
                 "xkb_keymap_get_as_string"
                 (force %libxkbcommon))
                (list '* ffi:int))))
    (lambda (keymap format)
      (assert (xkb-keymap? keymap))
      (pointer->string*
       (%func (unwrap-xkb-keymap keymap)
              (%xkb-keymap-format-enum->number format))))))
(define-public xkb-keymap-min-keycode
  (let ((%func (ffi:pointer->procedure
                ffi:uint32
                (dynamic-func
                 "xkb_keymap_min_keycode"
                 (force %libxkbcommon))
                (list '*))))
    (lambda (keymap)
      (assert (xkb-keymap? keymap))
      (%func (unwrap-xkb-keymap keymap)))))
(define-public xkb-keymap-max-keycode
  (let ((%func (ffi:pointer->procedure
                ffi:uint32
                (dynamic-func
                 "xkb_keymap_max_keycode"
                 (force %libxkbcommon))
                (list '*))))
    (lambda (keymap)
      (assert (xkb-keymap? keymap))
      (%func (unwrap-xkb-keymap keymap)))))
(define-public xkb_keymap_key_iter_t
  (bs:pointer '*))
(define-public xkb-keymap-key-for-each
  (let ((%func (ffi:pointer->procedure
                 ffi:void
                 (dynamic-func
                   "xkb_keymap_key_for_each"
                   (force %libxkbcommon))
                 (list '* '* '*))))
    (lambda (keymap iter data)
      (%func (unwrap-xkb-keymap keymap) iter data))))
(define-public xkb-keymap-key-get-name
  (let ((%func (ffi:pointer->procedure
                 '*
                 (dynamic-func
                   "xkb_keymap_key_get_name"
                   (force %libxkbcommon))
                 (list '* ffi:uint32))))
    (lambda (keymap key)
      (pointer->string*
        (%func (unwrap-xkb-keymap keymap) key)))))
(define-public xkb-keymap-key-by-name
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_keymap_key_by_name"
                   (force %libxkbcommon))
                 (list '* '*))))
    (lambda (keymap name)
      (%func (unwrap-xkb-keymap keymap)
             (ffi:string->pointer name)))))
(define-public xkb-keymap-num-mods
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_keymap_num_mods"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (keymap)
      (%func (unwrap-xkb-keymap keymap)))))
(define-public xkb-keymap-mod-get-name
  (let ((%func (ffi:pointer->procedure
                 '*
                 (dynamic-func
                   "xkb_keymap_mod_get_name"
                   (force %libxkbcommon))
                 (list '* ffi:uint32))))
    (lambda (keymap idx)
      (pointer->string*
        (%func (unwrap-xkb-keymap keymap) idx)))))
(define-public xkb-keymap-mod-get-index
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_keymap_mod_get_index"
                   (force %libxkbcommon))
                 (list '* '*))))
    (lambda (keymap name)
      (%func (unwrap-xkb-keymap keymap)
             (ffi:string->pointer name)))))
(define-public xkb-keymap-num-layouts
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_keymap_num_layouts"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (keymap)
      (%func (unwrap-xkb-keymap keymap)))))
(define-public xkb-keymap-layout-get-name
  (let ((%func (ffi:pointer->procedure
                 '*
                 (dynamic-func
                   "xkb_keymap_layout_get_name"
                   (force %libxkbcommon))
                 (list '* ffi:uint32))))
    (lambda (keymap idx)
      (pointer->string*
        (%func (unwrap-xkb-keymap keymap) idx)))))
(define-public xkb-keymap-layout-get-index
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_keymap_layout_get_index"
                   (force %libxkbcommon))
                 (list '* '*))))
    (lambda (keymap name)
      (%func (unwrap-xkb-keymap keymap)
             (ffi:string->pointer name)))))
(define-public xkb-keymap-num-leds
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_keymap_num_leds"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (keymap)
      (%func (unwrap-xkb-keymap keymap)))))
(define-public xkb-keymap-led-get-name
  (let ((%func (ffi:pointer->procedure
                 '*
                 (dynamic-func
                   "xkb_keymap_led_get_name"
                   (force %libxkbcommon))
                 (list '* ffi:uint32))))
    (lambda (keymap idx)
      (pointer->string*
        (%func (unwrap-xkb-keymap keymap) idx)))))
(define-public xkb-keymap-led-get-index
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_keymap_led_get_index"
                   (force %libxkbcommon))
                 (list '* '*))))
    (lambda (keymap name)
      (%func (unwrap-xkb-keymap keymap)
             (ffi:string->pointer name)))))
(define-public xkb-keymap-num-layouts-for-key
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_keymap_num_layouts_for_key"
                   (force %libxkbcommon))
                 (list '* ffi:uint32))))
    (lambda (keymap key)
      (%func (unwrap-xkb-keymap keymap) key))))
(define-public xkb-keymap-num-levels-for-key
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_keymap_num_levels_for_key"
                   (force %libxkbcommon))
                 (list '* ffi:uint32 ffi:uint32))))
    (lambda (keymap key layout)
      (%func (unwrap-xkb-keymap keymap) key layout))))
(define-public xkb-keymap-key-get-mods-for-level
  (let ((%func (ffi:pointer->procedure
                 ffi:size_t
                 (dynamic-func
                   "xkb_keymap_key_get_mods_for_level"
                   (force %libxkbcommon))
                 (list '*
                       ffi:uint32
                       ffi:uint32
                       ffi:uint32
                       '*
                       ffi:size_t))))
    (lambda (keymap key layout level masks_out masks_size)
      (%func (unwrap-xkb-keymap keymap)
             key
             layout
             level
             masks_out
             masks_size))))
(define-public xkb-keymap-key-get-syms-by-level
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_keymap_key_get_syms_by_level"
                   (force %libxkbcommon))
                 (list '* ffi:uint32 ffi:uint32 ffi:uint32 '*))))
    (lambda (keymap key layout level syms_out)
      (%func (unwrap-xkb-keymap keymap)
             key
             layout
             level
             syms_out))))
(define-public xkb-keymap-key-repeats
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_keymap_key_repeats"
                   (force %libxkbcommon))
                 (list '* ffi:uint32))))
    (lambda (keymap key)
      (%func (unwrap-xkb-keymap keymap) key))))
(define-public xkb-state-new
  (let ((%func (ffi:pointer->procedure
                 '*
                 (dynamic-func
                   "xkb_state_new"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (keymap)
      (wrap-xkb-state
        (%func (unwrap-xkb-keymap keymap))))))
(define-public xkb-state-ref
  (let ((%func (ffi:pointer->procedure
                 '*
                 (dynamic-func
                   "xkb_state_ref"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (state)
      (wrap-xkb-state (%func (unwrap-xkb-state state))))))
(define-public xkb-state-unref
  (let ((%func (ffi:pointer->procedure
                 ffi:void
                 (dynamic-func
                   "xkb_state_unref"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (state) (%func (unwrap-xkb-state state)))))
(define-public xkb-state-get-keymap
  (let ((%func (ffi:pointer->procedure
                 '*
                 (dynamic-func
                   "xkb_state_get_keymap"
                   (force %libxkbcommon))
                 (list '*))))
    (lambda (state)
      (wrap-xkb-keymap
        (%func (unwrap-xkb-state state))))))
(begin
  (define-public %xkb-key-direction-enum
    (bs:enum '((XKB_KEY_UP 0) (XKB_KEY_DOWN 1))))
  (define-public XKB_KEY_UP 0)
  (define-public XKB_KEY_DOWN 1)
  (define-public (%xkb-key-direction-enum->number o)
    (bs:enum->integer %xkb-key-direction-enum o)))
(begin
  (define-public %xkb-state-component-enum
    (bs:enum
      '((XKB_STATE_MODS_DEPRESSED 1)
        (XKB_STATE_MODS_LATCHED 2)
        (XKB_STATE_MODS_LOCKED 4)
        (XKB_STATE_MODS_EFFECTIVE 8)
        (XKB_STATE_LAYOUT_DEPRESSED 16)
        (XKB_STATE_LAYOUT_LATCHED 32)
        (XKB_STATE_LAYOUT_LOCKED 64)
        (XKB_STATE_LAYOUT_EFFECTIVE 128)
        (XKB_STATE_LEDS 256))))
  (define-public XKB_STATE_MODS_DEPRESSED 1)
  (define-public XKB_STATE_MODS_LATCHED 2)
  (define-public XKB_STATE_MODS_LOCKED 4)
  (define-public XKB_STATE_MODS_EFFECTIVE 8)
  (define-public XKB_STATE_LAYOUT_DEPRESSED 16)
  (define-public XKB_STATE_LAYOUT_LATCHED 32)
  (define-public XKB_STATE_LAYOUT_LOCKED 64)
  (define-public XKB_STATE_LAYOUT_EFFECTIVE 128)
  (define-public XKB_STATE_LEDS 256)
  (define-public (%xkb-state-component-enum->number o)
    (bs:enum->integer %xkb-state-component-enum o)))
(define-public xkb-state-update-key
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_state_update_key"
                   (force %libxkbcommon))
                 (list '* ffi:uint32 ffi:int))))
    (lambda (state key direction)
      (%func (unwrap-xkb-state state)
             key
             (%xkb-key-direction-enum->number direction)))))
(define-public xkb-state-update-mask
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_state_update_mask"
                   (force %libxkbcommon))
                 (list '*
                       ffi:uint32
                       ffi:uint32
                       ffi:uint32
                       ffi:uint32
                       ffi:uint32
                       ffi:uint32))))
    (lambda (state
             depressed_mods
             latched_mods
             locked_mods
             depressed_layout
             latched_layout
             locked_layout)
      (%func (unwrap-xkb-state state)
             depressed_mods
             latched_mods
             locked_mods
             depressed_layout
             latched_layout
             locked_layout))))
(define-public xkb-state-key-get-syms
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_state_key_get_syms"
                   (force %libxkbcommon))
                 (list '* ffi:uint32 '*))))
    (lambda (state key syms_out)
      (%func (unwrap-xkb-state state) key syms_out))))
(define-public xkb-state-key-get-utf8
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_state_key_get_utf8"
                   (force %libxkbcommon))
                 (list '* ffi:uint32 '* ffi:size_t))))
    (lambda (state key buffer size)
      (%func (unwrap-xkb-state state)
             key
             (ffi:string->pointer buffer)
             size))))
(define-public xkb-state-key-get-utf32
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_state_key_get_utf32"
                   (force %libxkbcommon))
                 (list '* ffi:uint32))))
    (lambda (state key)
      (%func (unwrap-xkb-state state) key))))
(define-public xkb-state-key-get-one-sym
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_state_key_get_one_sym"
                   (force %libxkbcommon))
                 (list '* ffi:uint32))))
    (lambda (state key)
      (%func (unwrap-xkb-state state) key))))
(define-public xkb-state-key-get-layout
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_state_key_get_layout"
                   (force %libxkbcommon))
                 (list '* ffi:uint32))))
    (lambda (state key)
      (%func (unwrap-xkb-state state) key))))
(define-public xkb-state-key-get-level
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_state_key_get_level"
                   (force %libxkbcommon))
                 (list '* ffi:uint32 ffi:uint32))))
    (lambda (state key layout)
      (%func (unwrap-xkb-state state) key layout))))
(begin
  (define-public %xkb-state-match-enum
    (bs:enum
      '((XKB_STATE_MATCH_ANY 1)
        (XKB_STATE_MATCH_ALL 2)
        (XKB_STATE_MATCH_NON_EXCLUSIVE 65536))))
  (define-public XKB_STATE_MATCH_ANY 1)
  (define-public XKB_STATE_MATCH_ALL 2)
  (define-public XKB_STATE_MATCH_NON_EXCLUSIVE
    65536)
  (define-public (%xkb-state-match-enum->number o)
    (bs:enum->integer %xkb-state-match-enum o)))
(define-public xkb-state-serialize-mods
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_state_serialize_mods"
                   (force %libxkbcommon))
                 (list '* ffi:int))))
    (lambda (state components)
      (%func (unwrap-xkb-state state)
             (%xkb-state-component-enum->number components)))))
(define-public xkb-state-serialize-layout
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_state_serialize_layout"
                   (force %libxkbcommon))
                 (list '* ffi:int))))
    (lambda (state components)
      (%func (unwrap-xkb-state state)
             (%xkb-state-component-enum->number components)))))
(define-public xkb-state-mod-name-is-active
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_state_mod_name_is_active"
                   (force %libxkbcommon))
                 (list '* '* ffi:int))))
    (lambda (state name type)
      (%func (unwrap-xkb-state state)
             (ffi:string->pointer name)
             (%xkb-state-component-enum->number type)))))
(define-public xkb-state-mod-index-is-active
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_state_mod_index_is_active"
                   (force %libxkbcommon))
                 (list '* ffi:uint32 ffi:int))))
    (lambda (state idx type)
      (%func (unwrap-xkb-state state)
             idx
             (%xkb-state-component-enum->number type)))))
(begin
  (define-public %xkb-consumed-mode-enum
    (bs:enum
      '((XKB_CONSUMED_MODE_XKB 0)
        (XKB_CONSUMED_MODE_GTK 1))))
  (define-public XKB_CONSUMED_MODE_XKB 0)
  (define-public XKB_CONSUMED_MODE_GTK 1)
  (define-public (%xkb-consumed-mode-enum->number o)
    (bs:enum->integer %xkb-consumed-mode-enum o)))
(define-public xkb-state-key-get-consumed-mods2
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_state_key_get_consumed_mods2"
                   (force %libxkbcommon))
                 (list '* ffi:uint32 ffi:int))))
    (lambda (state key mode)
      (%func (unwrap-xkb-state state)
             key
             (%xkb-consumed-mode-enum->number mode)))))
(define-public xkb-state-key-get-consumed-mods
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_state_key_get_consumed_mods"
                   (force %libxkbcommon))
                 (list '* ffi:uint32))))
    (lambda (state key)
      (%func (unwrap-xkb-state state) key))))
(define-public xkb-state-mod-index-is-consumed2
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_state_mod_index_is_consumed2"
                   (force %libxkbcommon))
                 (list '* ffi:uint32 ffi:uint32 ffi:int))))
    (lambda (state key idx mode)
      (%func (unwrap-xkb-state state)
             key
             idx
             (%xkb-consumed-mode-enum->number mode)))))
(define-public xkb-state-mod-index-is-consumed
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_state_mod_index_is_consumed"
                   (force %libxkbcommon))
                 (list '* ffi:uint32 ffi:uint32))))
    (lambda (state key idx)
      (%func (unwrap-xkb-state state) key idx))))
(define-public xkb-state-mod-mask-remove-consumed
  (let ((%func (ffi:pointer->procedure
                 ffi:uint32
                 (dynamic-func
                   "xkb_state_mod_mask_remove_consumed"
                   (force %libxkbcommon))
                 (list '* ffi:uint32 ffi:uint32))))
    (lambda (state key mask)
      (%func (unwrap-xkb-state state) key mask))))
(define-public xkb-state-layout-name-is-active
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_state_layout_name_is_active"
                   (force %libxkbcommon))
                 (list '* '* ffi:int))))
    (lambda (state name type)
      (%func (unwrap-xkb-state state)
             (ffi:string->pointer name)
             (%xkb-state-component-enum->number type)))))
(define-public xkb-state-layout-index-is-active
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_state_layout_index_is_active"
                   (force %libxkbcommon))
                 (list '* ffi:uint32 ffi:int))))
    (lambda (state idx type)
      (%func (unwrap-xkb-state state)
             idx
             (%xkb-state-component-enum->number type)))))
(define-public xkb-state-led-name-is-active
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_state_led_name_is_active"
                   (force %libxkbcommon))
                 (list '* '*))))
    (lambda (state name)
      (%func (unwrap-xkb-state state)
             (ffi:string->pointer name)))))
(define-public xkb-state-led-index-is-active
  (let ((%func (ffi:pointer->procedure
                 ffi:int
                 (dynamic-func
                   "xkb_state_led_index_is_active"
                   (force %libxkbcommon))
                 (list '* ffi:uint32))))
    (lambda (state idx)
      (%func (unwrap-xkb-state state) idx))))
