(use-modules
 (guix utils) (guix packages)
 ((guix licenses) #:prefix license:)
 (gnu packages xorg)
 (guix download)
 (guix git-download)
 (gnu packages gettext)
 (guix gexp)
 (gnu packages gl)
 (gnu packages xdisorg)
 (guix build-system gnu)
 (gnu packages bash)
 (gnu packages)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages gtk)
 (gnu packages guile-xyz)
 (gnu packages ibus)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages wm)
 (gnu packages xml)
 (gnu packages freedesktop))

(define %srcdir
  (dirname (current-filename)))

(define-public guile-xkbcommon
  (package
    (name "guile-xkbcommon")
    (version "0.1")
    (source (local-file "." "guile-xkbcommon-checkout"
                        #:recursive? #t
                        #:select? (git-predicate %srcdir)))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list "GUILE_AUTO_COMPILE=0")))
    (native-inputs
     (list autoconf automake
           pkg-config
           guile-3.0-latest))
    (inputs (list guile-3.0-latest
                  libxkbcommon
                  ;;; xkbregistry pc file require
                  libxml2))
    (propagated-inputs
     (list
      (primitive-load
       (string-append (dirname (dirname (current-filename)))
                      "/guile-bytestructure-class/guix.scm"))
      guile-bytestructures))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))
guile-xkbcommon
