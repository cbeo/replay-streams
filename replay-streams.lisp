;;;; replay-streams.lisp

(in-package #:replay-streams)


;;;; THE STREAM GENERICS

(defgeneric checkpoint (stream)
  (:documentation "Creates a reference that can be used to rewind the stream at a later time."))

(defgeneric rewind-to (stream checkpoint)
  (:documentation "Rewinds the stream to the checkpoint"))


;; DEPRECATED
(defgeneric rewind (rp-stream)
  (:documentation "Rewinds a stream and returns it. Returns a second value, T if
  the stream was rewound. Returns NIL if the stream had already been rewound."))

;; DEPRECATED
(defgeneric rewound-p (stream)
  (:documentation "Returns T if the stream has been rewound."))

;; DEPRECATED
(defgeneric replay-finished-p (stream)
  (:documentation "Returns T when reads replay is concluded, meaning that subsequent reads affect the underlying stream again"))

;; DEPRECATED
(defgeneric recover-source (stream)
  (:documentation "Recover the source stream of a replay stream"))


;;;; THE CLASSES


;; DEPRECATED
(defclass replay-character-stream (fundamental-character-input-stream)
  ((source :initarg :source)
   (log :initform (make-array 8 :element-type 'character :adjustable t :fill-pointer 0))
   (replay-mode :initform nil)
   (replay-stream :initform nil)))

(defclass static-text-replay-stream (fundamental-character-input-stream)
  ((text :initarg :text)
   (head :initform 0)))


;;;; TRIVAL-GRAY-STREAMS SUPPORT

(defmethod stream-read-char ((stream static-text-replay-stream))
  (with-slots (text head) stream
    (if (>= head (length text))
        :eof
        (progn
          (incf head)
          (aref text (- head 1))))))

(defmethod stream-unread-char ((stream static-text-replay-stream) char)
  (with-slots (head) stream
    (when (> head 0) (decf head))
    nil))



(defun stream-log-push (log char)
  (destructuring-bind (size) (array-dimensions log)
    (when (<= size (length log))
      ;; thanks to (eq a (adjust-array a ...)) I don't need to worry
      (adjust-array log (* 2 size) :element-type 'character :fill-pointer (length log)))
  (vector-push char log)))

;; DEPRECATED
(defmethod stream-read-char ((stream replay-character-stream))
  (with-slots (source log replay-mode replay-stream) stream
    (cond
      ;; If we are not in replay mode, we read from the source and write to the log.
      ((not replay-mode) (if (peek-char nil source nil nil)
                             (let ((c (read-char source)))
                               (stream-log-push log c)
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

;; DEPRECATED
(defmethod stream-unread-char ((stream replay-character-stream) char)
  (with-slots (source log replay-mode replay-stream) stream
    (cond ((not replay-mode)
           ;; we're not replaying, so we have to remove the character from the
           ;; log and put and unread it from the source stream
           (vector-pop log)
           (unread-char char source))

          ((and replay-mode replay-stream)
           ;; we're currently replaying content from the non-nill replay-stream
           (unread-char char replay-stream))

          (t
           ;; we're in replay-mode (hence not logging), but we've already
           ;; consumed all the recorded content, so it's really just the source
           ;; stream that we need to unread.
           (unread-char char source)))))


(defmethod checkpoint ((stream static-text-replay-stream))
  (with-slots (head) stream
    head))
  ;(slot-value stream 'head))



(defmethod rewind-to ((stream static-text-replay-stream) checkpoint)
  (with-slots (head) stream
    (setf head checkpoint)))
  ;;(setf (slot-value stream 'head) checkpoint))

(defun replay-on (stream)
  (make-instance 'replay-character-stream :source stream))




(defmethod rewind ((rp-stream replay-character-stream))
  (with-slots (log replay-mode replay-stream) rp-stream
      (if replay-mode (values rp-stream nil)
          (progn
            (setf replay-mode t)
            (setf replay-stream (make-string-input-stream log))
            (values rp-stream t)))))


(defmethod rewound-p ((stream replay-character-stream))
  (slot-value stream 'replay-mode))


(defmethod replay-finished-p ((stream replay-character-stream))
  (with-slots (replay-mode replay-stream) stream
    (and replay-mode (not (peek-char nil replay-stream nil nil)))))


(defmethod recover-source ((stream replay-character-stream))
  (slot-value stream 'source))
