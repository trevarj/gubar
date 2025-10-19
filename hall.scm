(hall-description
  (name "gubar")
  (prefix "")
  (version "0.1")
  (author "Trevor Arjeski")
  (email "tmarjeski@gmail.com")
  (copyright (2025))
  (synopsis
    "Swaybar generator written in Guile Scheme.")
  (description
    "A bar generator for sway using the swaybar-protocol, written\nand configured with Guile Scheme. It is similar to a program like i3blocks where\nyou can define your own blocks or use some of the provided blocks, which will\nthen be displayed in the bar.")
  (home-page "https://codeberg.org/trevarj/gubar")
  (license gpl3+)
  (dependencies
    (("guile-fibers" (fibers)) ("guile-json" (json))))
  (skip ())
  (features
    ((guix #t)
     (use-guix-specs-for-dependencies #t)))
  (files (libraries
           ((directory
              "gubar"
              ((directory
                 "blocks"
                 ((scheme-file "xkb-layout")
                  (scheme-file "volume-pipewire")
                  (scheme-file "network-manager-wifi")
                  (scheme-file "label")
                  (scheme-file "date-time")
                  (scheme-file "battery")
                  (scheme-file "brightness")))
               (scheme-file "hconfig")
               (scheme-file "main")
               (scheme-file "swaybar-protocol")
               (scheme-file "gublock")))))
         (tests ())
         (programs
           ((directory "scripts" ((in-file "gubar")))))
         (documentation
           ((text-file "COPYING")
            (markdown-file "README")
            (text-file "NEWS")
            (text-file "AUTHORS")
            (text-file "ChangeLog")))
         (infrastructure
           ((scheme-file "guix")
            (text-file ".gitignore")
            (scheme-file "hall")
            (directory
              "build-aux"
              ((tex-file "texinfo")
               (text-file "missing")
               (text-file "install-sh")
               (scheme-file "test-driver")))
            (autoconf-file "configure")
            (automake-file "Makefile")
            (in-file "pre-inst-env")))))
