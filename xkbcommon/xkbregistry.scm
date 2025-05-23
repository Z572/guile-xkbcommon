(define-module (xkbcommon xkbregistry)
  #:use-module (system foreign)
  #:use-module (xkbcommon config))

(define-syntax define-simple-rxkb-type
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

(define-syntax define-enum
  (lambda (x)
    (syntax-case x ()
      ((_ <name> <num->enum> <enum->num> (<entry-name> <entry-value>) ...)
       (syntax
        (begin
          (define <name> '((<entry-name> <entry-value>) ...))
          (define-public <entry-name> <entry-value>)
          ...
          (define (<num->enum> o)
            (or (assq-ref '((<entry-value> . <entry-name>) ...) o)
                (error "not found" '<name> o)))
          (define (<enum->num> o)
            (or (assq-ref '((<entry-name> . <entry-value>) ...) o)
                (error "not found" '<name> o)))
          ))))))

(define-syntax define-xkbregistry-procedure
  (lambda (x)
    (syntax-case x ()
      ((_ (pname args ...) (c-return c-name c-args) body ...)
       (with-syntax ((% (datum->syntax x '%)))
         (syntax
          (define-public pname
            (let ((% (pointer->procedure
                      c-return
                      (dynamic-func c-name (force %libxkbregistry))
                      c-args)))
              (lambda (args ...)
                #((name . pname))
                body ...)))))))))

(define (pointer->string* ptr)
  (if (null-pointer? ptr) #f (pointer->string ptr)))
(define non-zero? (negate zero?))
(define-simple-rxkb-type <rxkb-context>
  rxkb-context?
  wrap-rxkb-context
  unwrap-rxkb-context)

(define-simple-rxkb-type <rxkb-model>
  rxkb-model?
  wrap-rxkb-model
  unwrap-rxkb-model)
(define-simple-rxkb-type <rxkb-layout>
  rxkb-layout?
  wrap-rxkb-layout
  unwrap-rxkb-layout)

(define-simple-rxkb-type <rxkb-option-group>
  rxkb-option-group?
  wrap-rxkb-option-group
  unwrap-rxkb-option-group)

(define-simple-rxkb-type <rxkb-option>
  rxkb-option?
  wrap-rxkb-option
  unwrap-rxkb-option)

(define-simple-rxkb-type <rxkb-iso639-code>
  rxkb-iso639-code?
  wrap-rxkb-iso639-code
  unwrap-rxkb-iso639-code)

(define-simple-rxkb-type <rxkb-iso3166-code>
  rxkb-iso3166-code?
  wrap-rxkb-iso3166-code
  unwrap-rxkb-iso3166-code)

(define-enum %rxkb-popularity-enum
  number->%rxkb-popularity-enum
  %rxkb-popularity-enum->number
  (RXKB_POPULARITY_STANDARD 1)
  (RXKB_POPULARITY_EXOTIC 2))

(define-enum %rxkb-context-flags-enum
  number->%rxkb-context-flags-enum
  %rxkb-context-flags-enum->number
  (RXKB_CONTEXT_NO_FLAGS 0)
  (RXKB_CONTEXT_NO_DEFAULT_INCLUDES 1)
  (RXKB_CONTEXT_LOAD_EXOTIC_RULES 2))

(define-xkbregistry-procedure (rxkb-context-new flags)
  ('* "rxkb_context_new" (list int32))
  (wrap-rxkb-context (% (%rxkb-context-flags-enum->number flags))))

(define-enum %rxkb-log-level-enum
  number->%rxkb-log-level-enum
  %rxkb-log-level-enum->number
  (RXKB_LOG_LEVEL_CRITICAL 10)
  (RXKB_LOG_LEVEL_ERROR 20)
  (RXKB_LOG_LEVEL_WARNING 30)
  (RXKB_LOG_LEVEL_INFO 40)
  (RXKB_LOG_LEVEL_DEBUG 50))

(define-xkbregistry-procedure (rxkb-context-set-log-level ctx level)
  (void "rxkb_context_set_log_level" (list '* int32))
  (% (unwrap-rxkb-context ctx) (%rxkb-log-level-enum->number level)))

(define-xkbregistry-procedure
  (rxkb-context-get-log-level ctx) (int32 "rxkb_context_get_log_level" (list '*))
  (number->%rxkb-log-level-enum (% (unwrap-rxkb-context ctx))))
;; (define-xkbregistry-procedure (rxkb-context-set-log-fn ctx log_fn)
;;   (void "rxkb_context_set_log_fn" (list '* '*))
;;   (% (unwrap-rxkb-context ctx) log_fn))
(define-xkbregistry-procedure (rxkb-context-parse ctx ruleset)
  (int8 "rxkb_context_parse" (list '* '*))
  (non-zero? (% (unwrap-rxkb-context ctx) (string->pointer ruleset))))

(define-xkbregistry-procedure (rxkb-context-parse-default-ruleset ctx)
  (int8 "rxkb_context_parse_default_ruleset" (list '*))
  (non-zero? (% (unwrap-rxkb-context ctx))))

;; (define-xkbregistry-procedure (rxkb-context-ref ctx)
;;   ('* "rxkb_context_ref" (list '*))
;;   (wrap-rxkb-context (% (unwrap-rxkb-context ctx))))
;; (define-xkbregistry-procedure (rxkb-context-unref ctx)
;;   ('* "rxkb_context_unref" (list '*))
;;   (wrap-rxkb-context (% (unwrap-rxkb-context ctx))))
;; (define-xkbregistry-procedure (rxkb-context-set-user-data ctx user_data)
;;   (void "rxkb_context_set_user_data" (list '* '*))
;;   (% (unwrap-rxkb-context ctx) user_data))
;; (define-xkbregistry-procedure (rxkb-context-get-user-data ctx)
;;   ('* "rxkb_context_get_user_data" (list '*))
;;   (% (unwrap-rxkb-context ctx)))
(define-xkbregistry-procedure (rxkb-context-include-path-append ctx path)
  (int8 "rxkb_context_include_path_append" (list '* '*))
  (non-zero? (% (unwrap-rxkb-context ctx) (string->pointer path))))
(define-xkbregistry-procedure (rxkb-context-include-path-append-default ctx)
  (int8 "rxkb_context_include_path_append_default" (list '*))
  (non-zero? (% (unwrap-rxkb-context ctx))))
(define-xkbregistry-procedure (rxkb-model-first ctx)
  ('* "rxkb_model_first" (list '*))
  (wrap-rxkb-model (% (unwrap-rxkb-context ctx))))

(define-xkbregistry-procedure (rxkb-model-next m)
  ('* "rxkb_model_next" (list '*))
  (wrap-rxkb-model (% (unwrap-rxkb-model m))))

;; (define-xkbregistry-procedure
;;   (rxkb-model-ref m)
;;   ('* "rxkb_model_ref" (list '*))
;;   (wrap-rxkb-model (% (unwrap-rxkb-model m))))

;; (define-xkbregistry-procedure
;;   (rxkb-model-unref m)
;;   ('* "rxkb_model_unref" (list '*))
;;   (wrap-rxkb-model (% (unwrap-rxkb-model m))))

(define-xkbregistry-procedure
  (rxkb-model-get-name m)
  ('* "rxkb_model_get_name" (list '*))
  (pointer->string* (% (unwrap-rxkb-model m))))

(define-xkbregistry-procedure
  (rxkb-model-get-description m)
  ('* "rxkb_model_get_description" (list '*))
  (pointer->string* (% (unwrap-rxkb-model m))))

(define-xkbregistry-procedure
  (rxkb-model-get-vendor m)
  ('* "rxkb_model_get_vendor" (list '*))
  (pointer->string* (% (unwrap-rxkb-model m))))

(define-xkbregistry-procedure (rxkb-model-get-popularity m)
  (int32 "rxkb_model_get_popularity" (list '*))
  (number->%rxkb-popularity-enum (% (unwrap-rxkb-model m))))

(define-xkbregistry-procedure (rxkb-layout-first ctx)
  ('* "rxkb_layout_first" (list '*))
  (wrap-rxkb-layout (% (unwrap-rxkb-context ctx))))
(define-xkbregistry-procedure (rxkb-layout-next l)
  ('* "rxkb_layout_next" (list '*))
  (wrap-rxkb-layout (% (unwrap-rxkb-layout l))))

;; (define-xkbregistry-procedure
;;   (rxkb-layout-ref l)
;;   ('* "rxkb_layout_ref" (list '*))
;;   (wrap-rxkb-layout (% (unwrap-rxkb-layout l))))

;; (define-xkbregistry-procedure (rxkb-layout-unref l)
;;   ('* "rxkb_layout_unref" (list '*))
;;   (wrap-rxkb-layout (% (unwrap-rxkb-layout l))))

(define-xkbregistry-procedure
  (rxkb-layout-get-name l)
  ('* "rxkb_layout_get_name" (list '*))
  (pointer->string* (% (unwrap-rxkb-layout l))))
(define-xkbregistry-procedure
  (rxkb-layout-get-variant l)
  ('* "rxkb_layout_get_variant" (list '*))
  (pointer->string* (% (unwrap-rxkb-layout l))))
(define-xkbregistry-procedure
  (rxkb-layout-get-brief l)
  ('* "rxkb_layout_get_brief" (list '*))
  (pointer->string* (% (unwrap-rxkb-layout l))))
(define-xkbregistry-procedure
  (rxkb-layout-get-description l)
  ('* "rxkb_layout_get_description" (list '*))
  (pointer->string* (% (unwrap-rxkb-layout l))))
(define-xkbregistry-procedure
  (rxkb-layout-get-popularity l)
  (int32 "rxkb_layout_get_popularity" (list '*))
  (number->%rxkb-popularity-enum (% (unwrap-rxkb-layout l))))
(define-xkbregistry-procedure
  (rxkb-option-group-first ctx)
  ('* "rxkb_option_group_first" (list '*))
  (wrap-rxkb-option-group (% (unwrap-rxkb-context ctx))))
(define-xkbregistry-procedure
  (rxkb-option-group-next g)
  ('* "rxkb_option_group_next" (list '*))
  (wrap-rxkb-option-group (% (unwrap-rxkb-option-group g))))
;; (define-xkbregistry-procedure
;;   (rxkb-option-group-ref g)
;;   ('* "rxkb_option_group_ref" (list '*))
;;   (wrap-rxkb-option-group (% (unwrap-rxkb-option-group g))))
;; (define-xkbregistry-procedure
;;   (rxkb-option-group-unref g)
;;   ('* "rxkb_option_group_unref" (list '*))
;;   (wrap-rxkb-option-group (% (unwrap-rxkb-option-group g))))
(define-xkbregistry-procedure
  (rxkb-option-group-get-name m)
  ('* "rxkb_option_group_get_name" (list '*))
  (pointer->string* (% (unwrap-rxkb-option-group m))))
(define-xkbregistry-procedure
  (rxkb-option-group-get-description m)
  ('* "rxkb_option_group_get_description" (list '*))
  (pointer->string* (% (unwrap-rxkb-option-group m))))
(define-xkbregistry-procedure
  (rxkb-option-group-allows-multiple g)
  (int8 "rxkb_option_group_allows_multiple" (list '*))
  (non-zero? (% (unwrap-rxkb-option-group g))))
(define-xkbregistry-procedure
  (rxkb-option-group-get-popularity g)
  (int32 "rxkb_option_group_get_popularity" (list '*))
  (number->%rxkb-popularity-enum (% (unwrap-rxkb-option-group g))))
(define-xkbregistry-procedure
  (rxkb-option-first group)
  ('* "rxkb_option_first" (list '*))
  (wrap-rxkb-option (% (unwrap-rxkb-option-group group))))
(define-xkbregistry-procedure
  (rxkb-option-next o)
  ('* "rxkb_option_next" (list '*))
  (wrap-rxkb-option (% (unwrap-rxkb-option o))))
;; (define-xkbregistry-procedure
;;   (rxkb-option-ref o)
;;   ('* "rxkb_option_ref" (list '*))
;;   (wrap-rxkb-option (% (unwrap-rxkb-option o))))
;; (define-xkbregistry-procedure
;;   (rxkb-option-unref o)
;;   ('* "rxkb_option_unref" (list '*))
;;   (wrap-rxkb-option (% (unwrap-rxkb-option o))))
(define-xkbregistry-procedure
  (rxkb-option-get-name o)
  ('* "rxkb_option_get_name" (list '*))
  (pointer->string* (% (unwrap-rxkb-option o))))
(define-xkbregistry-procedure
  (rxkb-option-get-brief o)
  ('* "rxkb_option_get_brief" (list '*))
  (pointer->string* (% (unwrap-rxkb-option o))))
(define-xkbregistry-procedure
  (rxkb-option-get-description o)
  ('* "rxkb_option_get_description" (list '*))
  (pointer->string* (% (unwrap-rxkb-option o))))
(define-xkbregistry-procedure (rxkb-option-get-popularity o)
  (int32 "rxkb_option_get_popularity" (list '*))
  (number->%rxkb-popularity-enum (% (unwrap-rxkb-option o))))
;; (define-xkbregistry-procedure (rxkb-iso639-code-ref iso639)
;;   ('* "rxkb_iso639_code_ref" (list '*))
;;   (wrap-rxkb-iso639-code (% (unwrap-rxkb-iso639-code iso639))))
;; (define-xkbregistry-procedure (rxkb-iso639-code-unref iso639)
;;   ('* "rxkb_iso639_code_unref" (list '*))
;;   (wrap-rxkb-iso639-code (% (unwrap-rxkb-iso639-code iso639))))
(define-xkbregistry-procedure (rxkb-iso639-code-get-code iso639)
  ('* "rxkb_iso639_code_get_code" (list '*))
  (pointer->string* (% (unwrap-rxkb-iso639-code iso639))))
(define-xkbregistry-procedure (rxkb-layout-get-iso639-first layout)
  ('* "rxkb_layout_get_iso639_first" (list '*))
  (wrap-rxkb-iso639-code (% (unwrap-rxkb-layout layout))))
(define-xkbregistry-procedure (rxkb-iso639-code-next iso639)
  ('* "rxkb_iso639_code_next" (list '*))
  (wrap-rxkb-iso639-code (% (unwrap-rxkb-iso639-code iso639))))
;; (define-xkbregistry-procedure (rxkb-iso3166-code-ref iso3166)
;;   ('* "rxkb_iso3166_code_ref" (list '*))
;;   (wrap-rxkb-iso3166-code (% (unwrap-rxkb-iso3166-code iso3166))))
;; (define-xkbregistry-procedure (rxkb-iso3166-code-unref iso3166)
;;   ('* "rxkb_iso3166_code_unref" (list '*))
;;   (wrap-rxkb-iso3166-code (% (unwrap-rxkb-iso3166-code iso3166))))
(define-xkbregistry-procedure (rxkb-iso3166-code-get-code iso3166)
  ('* "rxkb_iso3166_code_get_code" (list '*))
  (pointer->string* (% (unwrap-rxkb-iso3166-code iso3166))))
(define-xkbregistry-procedure (rxkb-layout-get-iso3166-first layout)
  ('* "rxkb_layout_get_iso3166_first" (list '*))
  (wrap-rxkb-iso3166-code (% (unwrap-rxkb-layout layout))))
(define-xkbregistry-procedure (rxkb-iso3166-code-next iso3166)
  ('* "rxkb_iso3166_code_next" (list '*))
  (wrap-rxkb-iso3166-code (% (unwrap-rxkb-iso3166-code iso3166))))
