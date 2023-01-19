#!/usr/bin/env -S guile --no-auto-compile -e main
!#
(use-modules
 (ice-9 textual-ports)
 (ice-9 pretty-print)
 (ice-9 match)
 (ice-9 popen)
 (ice-9 string-fun)
 (srfi srfi-1)
 (srfi srfi-71)
 (srfi srfi-26)
 (oop goops))

(define c2ffi-program
  (make-parameter "c2ffi"))


(define (find-ffi a) (first a))
(define (find-bs a) (second a))
(define (find-wrap a) (third a))
(define (find-unwrap a) (fourth a))

(define (h x)
  (define out
    (match (typedef-remap (syntax->datum x))
      (':int (list 'ffi:int ''int #f #f))
      (':void (list 'ffi:void 'void #f #f))
      (':unsigned-int (list 'ffi:unsigned-int 'unsigned-int #f #f))
      (':double (list 'ffi:double 'double #f #f))
      (':float (list 'ffi:float 'float #f #f))
      ('int32_t (list 'ffi:int32 'int32 #f #f))
      (':_Bool (list 'ffi:int8 'stdbool 'non-zero? '(lambda (o) (if o 1 0))))
      ('uint32_t (list 'ffi:uint32 'uint32 #f #f))
      ('uint64_t (list 'ffi:uint64 'uint64 #f #f))
      ('size_t (list 'ffi:size_t 'size_t #f #f))
      ((':enum <name>)
       (list 'ffi:int32
             'int32
             (symbol-append 'number-> (make-%enum-name <name>))
             (symbol-append (make-%enum-name <name>) '->number)))
      ((':array <type> <length>)
       (list ''*
             `(bs:vector ,<length>
                         ,(handle-arg/bs (datum->syntax x <type>)) )

             #f ;; TODO

             `(lambda (a)
                (ffi:bytevector->pointer
                 (bytestructure-bytevector
                  (bytestructure
                   (bs:vector ,<length>
                              ,(handle-arg/bs
                                (datum->syntax x <type>)))

                   (cond ((vector? a) a)
                         ((list? a) (list->vector a)))))))))

      ((':pointer ':char) (list ''* 'cstring-pointer* 'pointer->string* 'ffi:string->pointer))
      ((':pointer ':void) (list ''* '(bs:pointer 'void) #f #f))
      ((':pointer ((or 'struct ':struct) <name>))
       (list ''*
             `(bs:pointer ,(find-bs-name <name>))

             (find-wrap-name <name>)
             (find-unwrap-name <name>)))
      ((':pointer <v>)
       (list ''*
             `(bs:pointer ,(handle-arg/bs (datum->syntax x <v>))) #f #f))
      (':function-pointer
       (list ''* '(bs:pointer '*) #f #f))

      (else (error "h" x))))
  (map (lambda (a)
         (and a (datum->syntax x a))) out))

(define (handle-ffi x)
  (find-ffi (h x)))

(define (handle-arg/bs x)
  (find-bs (h x)))

(define-method (_->- (v <string>))
  (string-replace-substring v "_" "-"))
(define-method (_->- (v <symbol>))
  (string->symbol (_->- (symbol->string v))))
(define (hand x proc)
  (datum->syntax
   x
   (proc (syntax->datum x))))


(define* (make-bytestructure-class name #:optional fields)
  (define* (make-bs:struct #:optional fields)
    (if fields
        (with-syntax ((((name value) ...)
                       (map (lambda (a) `(,(hand (first a) _->-) ,(handle-arg/bs (second a))))
                            fields)))
          #`(bs:struct `((name ,value) ...)))
        #'(bs:unknow)))
  (let ((base-name (hand name _->-)))
    (with-syntax ((%name (hand base-name make-%struct-name))
                  (class-name (hand base-name (cut symbol-append '< <> '>) ))
                  (wrap (hand base-name (cut symbol-append 'wrap- <>)))
                  (unwrap (hand base-name (cut symbol-append 'unwrap- <>)))
                  (is? (hand base-name (cut symbol-append <> '?))))
      #`(begin (define-public %name #,(make-bs:struct fields))
               (define-bytestructure-class class-name ()
                 %name wrap unwrap is?
                 #,@(if fields
                        (map (lambda (a)
                               (let* ((v (_->- (syntax->datum (car a))))
                                      (accessor (symbol-append '. v)))

                                 (datum->syntax
                                  (car a)
                                  (list v
                                        #:init-keyword
                                        (symbol->keyword v)
                                        #:accessor
                                        accessor)))
                               )
                             fields)
                        #'()))
               #,@(if fields
                      (list (cons #'export
                                  (map
                                   (lambda (a)
                                     (hand (car a)
                                           (compose (cut symbol-append '. <>)
                                                    _->-)))
                                   fields)))
                      #'())))))

(define (make-%struct-name o)
  ((compose _->- (cut symbol-append '% <> '-struct)) o))
(define (make-%enum-name o)
  ((compose _->- (cut symbol-append '% <> '-enum)) o))
(define %struct-table (make-hash-table 100))
(define (add-struct name scm-name)
  (hashq-set! %struct-table name scm-name))
(define (find-scm-struct name)
  (hashq-ref %struct-table name))

(define (find-bs-name name)
  (and=> (find-scm-struct name) make-%struct-name))
(define (find-wrap-name name)
  (and=> (find-scm-struct name) (cut symbol-append 'wrap- <>)))

(define (find-unwrap-name name)
  (and=> (find-scm-struct name) (cut symbol-append 'unwrap- <>)))

;; typedef
(define %typedef-table (make-hash-table 20))

(define (typedef-remap o)
  (hashq-ref %typedef-table o o))

(define (add-typedef name v)
  (hashq-set! %typedef-table name v))

(define (handle-need-wrap? n x)
  (define o (find-wrap (h x)))
  (if o (list o n) n))

(define (string-infix? s1 s2)
  (define first-char (string-ref s1 0))
  (define (index s) (string-index s first-char))
  (let loop ((s2 s2) (n (index s2)))
    (and n (or (string-prefix? s1 s2 0 (string-length s1) n)
               (let* ((str (substring/read-only s2 (1+ n))))
                 (loop str (index str)))))))

(define (handle-f x)
  (syntax-case x (struct enum function typedef extern)
    ((struct <name>)

     (begin (add-struct (syntax->datum #'<name>) (_->- (syntax->datum #'<name>)))
            (make-bytestructure-class #'<name>)))

    ((struct <name> (<entry-name> <entry-value>) ...)

     (begin (add-struct (syntax->datum #'<name>)
                        (_->- (syntax->datum #'<name>) ))
            (make-bytestructure-class #'<name>
                                      #'((<entry-name> <entry-value>) ...))))
    ((enum <enum-name> (<entry-name> <entry-value>) ...)
     (with-syntax ((<scm-enum-name> (hand #'<enum-name> make-%enum-name)))
       (with-syntax ((<num->enum> (hand #'<scm-enum-name>
                                        (cut symbol-append 'number-> <> )))
                     (<enum->num> (hand #'<scm-enum-name>
                                        (cut symbol-append <> '->number))))
         #'(define-enum <scm-enum-name> <num->enum> <enum->num>
             (<entry-name> <entry-value>) ...))))

    ((function <name> ((<s-name> <type>) ...) <return>)
     (with-syntax ((<func-name> (hand #'<name> (compose _->- string->symbol)))
                   ((<ffi:args> ...) (map handle-ffi #'(<type> ...)))
                   (<ffi:return> (handle-ffi #'<return>))
                   ((<handled-args> ...) (map (lambda (o)
                                                (apply (lambda (n x)
                                                         (define o (find-unwrap (h x)))
                                                         (if o (list o n) n))
                                                       o))
                                              #'((<s-name> <type>) ...))))
       #`(define-xkbregistry-procedure (<func-name> <s-name> ...)
           (<ffi:return> <name> (list <ffi:args> ...))
           #,(handle-need-wrap? #'(% <handled-args> ...) #'<return>))))

    ((function f-name ((s-name type) ...) return :variadic) #f)
    ((function f-name ((type) ...) return) #f)

    ((typedef o f)
     (begin (add-typedef (syntax->datum #'o) (syntax->datum #'f))
            (with-syntax ((ff (handle-arg/bs #'f)))
              #'(define-public o ff))))
    ((extern o ...) #f)))

(define-method (prefix? (s <string>) (s2 <string>))
  (string-prefix? s s2))
(define-method (prefix? s (s2 <symbol>))
  (prefix? s (symbol->string s2)))

(define (main- port)
  (define out (let loop ((value (read-syntax port))
                         (out '()))
                (if (eof-object? value)
                    (reverse out)

                    (loop (read-syntax port)
                          (if (let ((name (second (syntax->datum value))))
                                (prefix? "rxkb" name))
                              (cons (handle-f value) out)
                              out)))))
  (close-port port)
  out)

(define* (main self #:optional
               (file "/gnu/store/5sdcrp4591sb2m3h6903vhpdb6zy1cnm-libxkbcommon-1.3.0/include/xkbcommon/xkbregistry.h")
               #:rest arg)
  (for-each (lambda (o)
              (when o
                (pretty-print (syntax->datum o) #:max-expr-width 79)))
            (cons* `(define-module (xkbcommon xkbregistry)
                      #:use-module (bytestructure-class)
                      #:use-module ((system foreign) #:prefix ffi:)
                      #:use-module (bytestructures guile)
                      #:use-module (oop goops)
                      #:use-module (xkbcommon config))
                   `(define-syntax define-enum
                      (lambda (x)
                        (syntax-case x ()
                          ((_ <name>
                              <num->enum>
                              <enum->num>
                              (<entry-name> <entry-value>)
                              ...)
                           #'(begin
                               (define-public <name>
                                 (bs:enum '((<entry-name> <entry-value>) ...) ))
                               (define-public <entry-name> <entry-value>) ...
                               (define-public (<num->enum> o)
                                 (or (assq-ref '((<entry-value> <entry-name>) ...) o)
                                     (error "not found" '<name> o)))
                               (define-public (<enum->num> o)
                                 (bs:enum->integer <name> o)))))))
                   `(define-syntax define-xkbregistry-procedure
                      (lambda (x)
                        (syntax-case x ()
                          ((_ (name args ...)
                              (c-return c-name c-args)
                              body ...)
                           (with-syntax ((% (datum->syntax x '%)))
                             #'(define-public name
                                 (let ((% (ffi:pointer->procedure
                                           c-return
                                           (dynamic-func c-name (force %libxkbregistry)) c-args)))
                                   (lambda (args ...)
                                     body ...))))))))

                   `(define (pointer->string* ptr)
                      (if (ffi:null-pointer? ptr)
                          #f
                          (ffi:pointer->string ptr)))
                   `(define non-zero? (negate zero?))

                   (parameterize ((current-error-port (%make-void-port "w")))
                     (let ((port (open-pipe*
                                  OPEN_READ
                                  (c2ffi-program) "-D" "sexp" file)))
                       (set-port-filename! port #f)
                       (main- port))))))
