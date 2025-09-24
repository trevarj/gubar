(use-modules
  (gnu packages)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo)
  (guix build-system gnu)
  (guix download)
  (guix gexp)
  ((guix licenses) #:prefix license:)
  (guix packages)
  (srfi srfi-1))

(package
  (name "gubar")
  (version "0.1")
  (source
    (local-file
      (dirname (current-filename))
      #:recursive?
      #t
      #:select?
      (lambda (file stat)
        (not (any (lambda (my-string)
                    (string-contains file my-string))
                  (list ".git" ".dir-locals.el" "guix.scm"))))))
  (build-system gnu-build-system)
  (arguments
    (list #:modules
          `(((guix build guile-build-system)
             #:select
             (target-guile-effective-version))
            ,@%default-gnu-imported-modules)
          #:phases
          (with-imported-modules
            `((guix build guile-build-system)
              ,@%default-gnu-imported-modules)
            (gexp (modify-phases
                    %standard-phases
                    (add-after
                      'install
                      'hall-wrap-binaries
                      (lambda* (#:key inputs #:allow-other-keys)
                        (let* ((version (target-guile-effective-version))
                               (site-ccache
                                 (string-append
                                   "/lib/guile/"
                                   version
                                   "/site-ccache"))
                               (site (string-append
                                       "/share/guile/site/"
                                       version))
                               (dep-path
                                 (lambda (env path)
                                   (list env
                                         ":"
                                         'prefix
                                         (cons (string-append
                                                 (ungexp output)
                                                 path)
                                               (map (lambda (input)
                                                      (string-append
                                                        (assoc-ref
                                                          inputs
                                                          input)
                                                        path))
                                                    (list "guile-fibers"
                                                          "guile-json"))))))
                               (bin (string-append (ungexp output) "/bin/")))
                          (for-each
                            (lambda (file)
                              (wrap-program
                                (string-append bin file)
                                (dep-path "GUILE_LOAD_PATH" site)
                                (dep-path
                                  "GUILE_LOAD_COMPILED_PATH"
                                  site-ccache)
                                (dep-path "GUILE_EXTENSIONS_PATH" "/lib")))
                            (list "gubar"))))))))))
  (native-inputs
    (list autoconf automake pkg-config texinfo))
  (inputs (list guile-3.0))
  (propagated-inputs
    (list guile-fibers guile-json-4))
  (synopsis
    "Swaybar generator written in Guile Scheme.")
  (description
    "A bar generator for sway using the swaybar-protocol, written\nand configured with Guile Scheme. It is similar to a program like i3blocks where\nyou can define your own blocks or use some of the provided blocks, which will\nthen be displayed in the bar.")
  (home-page "https://codeberg.org/trevarj/gubar")
  (license license:gpl3+))

