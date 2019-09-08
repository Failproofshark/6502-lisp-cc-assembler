(in-package :asdf-user)
(defsystem "6502-lisp-cc-assembler"
  :description "6502-lisp-x-assembler: A cross assembler written in common lisp"
  :version "0.0.1"
  :license "Public Domain"
  :depends-on (:cl-ppcre)
  :components ((:file "main")))
