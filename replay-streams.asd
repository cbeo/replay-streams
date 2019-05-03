;;;; replay-streams.asd

(asdf:defsystem #:replay-streams
  :description "`replay-streams` let the programmer rewind an input stream to a previous state so the contents may be read again."
  :author "Boutade <thegoofist@protonmail.com>"
  :license  "GPLv3"
  :version "0.1.0"
  :serial t
  :depends-on (#:trivial-gray-streams)
  :components ((:file "package")
               (:file "replay-streams")))
