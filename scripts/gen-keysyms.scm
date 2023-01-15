(use-modules (ice-9 expect)
             (ice-9 regex)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 textual-ports)
             (ice-9 pretty-print))
(define file "/gnu/store/5sdcrp4591sb2m3h6903vhpdb6zy1cnm-libxkbcommon-1.3.0/include/xkbcommon/xkbcommon-keysyms.h")
(pretty-print
 '(define-module (xkbcommon keysyms)))
(call-with-input-file file
  (lambda (port)
    (define strings (get-string-all port))
    (define strings-list(string-split strings #\nl))
    (for-each
     (lambda (a)
       (display a)
       (newline))
     (filter-map
      (lambda (x)
        (and=> (string-match
                "^#define (XKB_KEY_[a-zA-Z_0-9]+) +0x([0-9a-fA-F]+).*$" x)
               (cut regexp-substitute
                    #f
                    <>
                    "(define-public " 1 " " "#x" 2 ")")))
      strings-list))))
