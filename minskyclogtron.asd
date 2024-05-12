;;;; clog-fractalizer.asd

(asdf:defsystem #:minskyclogtron
  :description "minskyclogtron: minskytron viewer implemented with CLOG"
  :author "José M. Á. Ronquillo Rivera <jose@rufina.link>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:clog
               #:jonathan
               #:rtg-math
               #:mathkit)
  :components ((:file "package")
               (:file "minskytron")
               (:file "clog-webgl-patch")
               (:file "minskyclogtron")))
