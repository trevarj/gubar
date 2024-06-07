(use-modules (guix build-system guile)
             (guix gexp)
             (guix git-download)
             (guix licenses)
             (guix packages)
             (gnu packages guile)
             (gnu packages guile-xyz))

(define %source-dir (dirname (current-filename)))

(define gubar
  (package
    (name "gubar")
    (version "git")
    (source (local-file (string-append %source-dir "/src")
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system guile-build-system)
    (native-inputs (list guile-3.0))
    (propagated-inputs (list guile-fibers guile-json-4))    
    (synopsis "Swaybar generator written in Guile Scheme.")
    (description "A bar generator for sway using the swaybar-protocol, written
and configured with Guile Scheme. It is similar to a program like i3blocks where
you can define your own blocks or use some of the provided blocks, which will
then be displayed in the bar.")
    (home-page "https://github.com/trevarj/gubar")
    (license gpl3+)))

gubar
