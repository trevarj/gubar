;; A Guix manifest file
(use-modules (gnu packages guile)
             (gnu packages guile-xyz))

(packages->manifest (list
                      guile-3.0
                      guile-fibers
                      guile-json-4))
