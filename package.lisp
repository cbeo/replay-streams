;;;; package.lisp

(defpackage #:replay-streams
  (:use #:cl #:trivial-gray-streams)
  (:export
   #:checkpoint
   #:recover-source
   #:replay-character-stream
   #:replay-finished-p
   #:replay-on
   #:rewind
   #:rewind-to
   #:rewound-p
   #:static-text-replay-stream
   ))
