;;;; package.lisp

(defpackage #:replay-streams
  (:use #:cl #:trivial-gray-streams)
  (:export
   #:replay-character-stream
   #:rewind
   #:rewound-p
   #:replay-on))
