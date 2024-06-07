;; A Guix manifest file
(use-modules (gnu packages guile)
             (gnu packages guile-xyz))

(packages->manifest (list
                      guile-next
                      guile-fibers
                      guile-json-4))
