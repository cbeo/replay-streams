;;;; package.lisp

(defpackage #:replay-streams
  (:use #:cl #:trivial-gray-streams)
  (:export
   #:recover-source
   #:replay-character-stream
   #:replay-finished-p
   #:replay-on
   #:rewind
   #:rewound-p
   ))
