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

(define-public guile-bytestructure-class
  (package
    (name "guile-bytestructure-class")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Z572/guile-bytestructure-class")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y3sryy79arp3f5smyxn8w7zra3j4bb0qdpl1p0bld3jicc4s86a"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~'("GUILE_AUTO_COMPILE=0")))
    (native-inputs
     (list autoconf
           automake
           pkg-config
           guile-3.0-latest))
    (inputs (list guile-3.0-latest))
    (propagated-inputs (list guile-bytestructures))
    (synopsis "bytestructure and goops")
    (description "This package combines bytestructure with goops,
and provide 4 new bytestructure-descriptor:
bs:unknow, cstring-pointer*, bs:enum, stdbool.")
    (home-page "https://github.com/Z572/guile-bytestructure-class")
    (license license:gpl3+)))

(define-public guile-xkbcommon
  (package
    (name "guile-xkbcommon")
    (version "0.0.1")
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
      guile-bytestructure-class
      guile-bytestructures))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))
guile-xkbcommon
