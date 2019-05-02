;;;; replay-streams.lisp

(in-package #:replay-streams)


;;;; THE STREAM GENERICS

(defgeneric checkpoint (stream)
  (:documentation "Creates a reference that can be used to rewind the stream at a later time."))

(defgeneric rewind-to (stream checkpoint)
  (:documentation "Rewinds the stream to the checkpoint."))

(defgeneric free-checkpoint (stream point)
  (:documentation "Indicates that a particular checkpoint is no longer needed."))

;;;; THE CLASSES

(defclass static-text-replay-stream (fundamental-character-input-stream)
  ((text :initarg :text)
   (head :initform 0)))


(defclass character-input-replay-stream (fundamental-character-input-stream)
  ((source :initarg :source)
   (source-head :initform 0)
   (head :initform 0)
   (checkpoints :initform nil)
   (log :initarg nil)
   (log-start :initarg nil)))

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


(defmethod stream-read-char ((stream character-input-replay-stream))
  (with-slots (source source-head head checkpoints log log-start) stream
    (cond
      ;; if there is no log and there are no checkpoints, then read normally
      ((and (null log)
            (null checkpoints))
       (if (peek-char nil source nil nil)
           (read-char source)
           :eof))

      ;; if the head is less than source-head, then we're reading from the log
      ((< head source-head)
       (incf head)   ;; order matters, we use the incremented value next
       (aref log (- head log-start 1)))

      ;; otherwise we're reading from the stream but we may be logging our reads
      ;; so if we're not at the end of the input, we've got some stuff to do
      ((peek-char nil source nil nil)
       (let ((char (read-char source)))
         (incf head)
         (incf source-head)

         (if checkpoints
             ;; if there are active checkpoints, we log this read
             (stream-log-push log char)
             ;; otherwise we set the log to nil
             (setf log nil))

         ;; finally we return the read char
         char))

      ;; otherwise we're at the end
      (t :eof))))


(defmethod stream-unread-char ((stream character-input-replay-stream) char)
  (with-slots (source source-head head checkpoints log) stream
    (cond
      ;; not logging, we're just working with the source stream
      ((and (null log) (null checkpoints))
       (decf head)
       (decf source-head)
       (unread-char char source))

      ;; we are reading-from the log
      ((< head source-head)
       (decf head))

      ;; otherwise we're reading from the source, but might be logging
      (t
       ;; if there are checpoints, then we're logging, so we pop the log
       (when checkpoints
         (vector-pop log))

       ;; otherwise this is just like the first condition
       (decf head)
       (decf source-head)
       (unread-char char source)))))


(defun stream-log-push (log char)
  (destructuring-bind (size) (array-dimensions log)
    (when (<= size (length log))
      ;; thanks to (eq a (adjust-array a ...)) I don't need to worry
      (adjust-array log (* 2 size) :element-type 'character :fill-pointer (length log)))
  (vector-push char log)))


;;; METHOD IMPLEMENTATIONS: STATIC-TEXT-REPLAY-STREAM

(defmethod checkpoint ((stream static-text-replay-stream))
  (with-slots (head) stream
    head))

(defmethod rewind-to ((stream static-text-replay-stream) checkpoint)
  (with-slots (head) stream
    (setf head checkpoint))
  t)

(defmethod free-checkpoint ((stream static-text-replay-stream) checkpoint)
  t)

;;; METHOD IMPLEMENTATIONS: CHARACTER-INPUT-REPLAY-STREAM

(defmethod checkpoint ((stream character-input-replay-stream))
  (with-slots (head checkpoints log log-start) stream

    ;; if the log does not already exist, make a new one and set the log-start
    (when (not log)
      (setf log (make-array 64 :element-type 'character :adjustable t :fill-pointer 0))
      (setf log-start head))

    ;; add the checkpoint and return it
    (push head checkpoints)
    head))

(defmethod rewind-to ((stream character-input-replay-stream) point)
  (with-slots (head checkpoints) stream
    (setf head point)
    ;; reqinding to a point clobbers all "future" checkpoints
    (setf checkpoints (remove-if (lambda (pt) (>= pt point)) checkpoints))
    t))

(defmethod free-checkpoint ((stream character-input-replay-stream) point)
  (with-slots (checkpoints) stream
    (setf checkpoints (remove point checkpoints)))
  t)
