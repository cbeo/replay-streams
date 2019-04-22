;;;; replay-streams.lisp

(in-package #:replay-streams)

(defclass replay-character-stream (fundamental-character-input-stream)
  ((source :initarg :source)
   (log :initform (make-string-output-stream))
   (replay-mode :initform nil)
   (replay-stream :initform nil)))


;; returns a char or :eof, as specified in trival-gray-streams documentation
(defmethod stream-read-char ((stream replay-character-stream))
  (with-slots (source log replay-mode replay-stream) stream
    (cond
      ;; If we are not in replay mode, we read from the source and write to the log.
      ((not replay-mode) (if (peek-char nil source nil nil)
                             (let ((c (read-char source)))
                               (write-char c log)
                               c)
                             :eof))
      ;; Otherwise we must be in replay mode.
      ;; We check for data in the replay-stream and return it
      ((and replay-stream (peek-char nil replay-stream nil nil)) (read-char replay-stream))

      ;; If the replay stream is null or empty, we make sure to close it if
      ;; necessary and then we just return characters from the source for the
      ;; rest of this instance's lifetime
      (t
       ;; destroy replay stream if necessary
       (when replay-stream
         (close replay-stream)
         (setf replay-stream nil))
       ;; and read froom source
       (if (peek-char nil source nil nil)
           (read-char source)
           :eof)))))

(defgeneric rewind (rp-stream)
  (:documentation "Rewinds a stream and returns it. Returns T if the stream was rewound. Returns NIL if the stream had already been rewound."))

(defmethod rewind ((rp-stream replay-character-stream))
  (with-slots (log replay-mode replay-stream) rp-stream
    (if replay-mode nil
      (let ((replay-content (get-output-stream-string log)))
        (close log)
        (setf log nil)
        (setf replay-mode t)
        (setf replay-stream (make-string-input-stream replay-content))))))

(defgeneric rewound-p (stream)
  (:documentation "Returns T if the stream has been rewound."))

(defmethod rewound-p ((stream replay-character-stream))
  (slot-value stream 'replay-mode))

(defgeneric replay-finished (stream)
  (:documentation "Returns T when reads replay is concluded, meaning that subsequent reads affect the underlying stream again"))

(defmethod replay-finished ((stream replay-character-stream))
  (with-slots (replay-mode replay-stream) stream
    (and replay-mode (not (peek-char nil replay-stream nil nil)))))

(defgeneric recover-source (stream)
  (:documentation "Recover the source stream of a replay stream"))

(defmethod recover-source ((stream replay-character-stream))
  (slot-value stream 'source))
