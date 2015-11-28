;;;; spirograph.asd
;;;;
;;;; Copyright (c) 2015 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:spirograph
  :description "Interactively draw Epitrochoid spirograph patterns."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:qtools
               #:qtgui
               #:qtcore)
  :serial t
  :components ((:file "package")
               (:file "spirograph")))

