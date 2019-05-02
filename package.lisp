;;;; package.lisp

(defpackage #:replay-streams
  (:use #:cl #:trivial-gray-streams)
  (:export
   #:checkpoint
   #:rewind-to
   #:free-checkpoint
   #:character-input-replay-stream
   #:static-text-replay-stream
   ))
