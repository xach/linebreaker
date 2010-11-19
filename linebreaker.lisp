;;;; linebreaker.lisp

(defpackage #:linebreaker
  (:use #:cl)
  (:export #:make-break-stream
           #:advance
           #:mark-break
           #:mark-wide-break
           #:break-line
           #:finish)
  (:export #:*default-wide-break-chars*
           #:*default-new-line-chars*
           #:*default-break-chars*
           #:breaker-strategy
           #:wide-break-chars
           #:wide-break-char-p
           #:new-line-chars
           #:new-line-char-p
           #:break-chars
           #:break-char-p
           #:line-width
           #:break-into-lines
           #:width-breaker))

(in-package #:linebreaker)

;;; break-stream

(defclass break-stream ()
  ((input
    :initarg :input
    :accessor input)
   (pos
    :initarg :pos
    :initform 0
    :accessor pos
    :documentation
    "The position of the next object to return from INPUT.")
   (output-start
    :initarg :output-start
    :initform 0
    :accessor output-start
    :documentation
    "The start of the current output subsequence.")
   (output-end
    :initarg :output-end
    :accessor output-end
    :initform nil
    :documentation
    "The end of the current output subsequence.")
   (input-resume
    :initarg :input-resume
    :accessor input-resume
    :initform nil
    :documentation
    "The POS after a revert.")
   (lines
    :initarg :lines
    :accessor lines
    :initform nil)))

(defun make-break-stream (input)
  (make-instance 'break-stream :input input))

(defgeneric finished (break-stream)
  (:method (break-stream)
    (<= (length (input break-stream)) (pos break-stream))))

(defgeneric advance (break-stream)
  (:method (break-stream)
    (unless (finished break-stream)
      (prog1
          (aref (input break-stream) (pos break-stream))
        (incf (pos break-stream))))))

(defgeneric mark-break (break-stream)
  (:method (break-stream)
    (let ((pos (pos break-stream)))
      (setf (output-end break-stream) pos
            (input-resume break-stream) pos)
      t)))

(defgeneric mark-wide-break (break-stream)
  (:method (break-stream)
    (let ((pos (pos break-stream)))
      (setf (output-end break-stream) (1- pos)
            (input-resume break-stream) pos))))

(defgeneric break-line (break-stream)
  (:method (break-stream)
    (let ((start (output-start break-stream))
          (end (or (output-end break-stream) (1- (pos break-stream)))))
      (when (and (<= end start)
                 (not (output-end break-stream)))
        (error "Empty line break"))
      (let ((line (subseq (input break-stream) start end)))
        (push line (lines break-stream))
        (cond ((input-resume break-stream)
               (setf (pos break-stream) (input-resume break-stream)))
              (t
               (decf (pos break-stream))))
        (setf (output-start break-stream) (pos break-stream))
        ;; clear marks
        (setf (output-end break-stream) nil
              (input-resume break-stream) nil)
        line))))

(defgeneric finish (break-stream)
  (:method (break-stream)
    (let ((start (output-start break-stream))
          (end (min (pos break-stream)
                    (length (input break-stream)))))
      (push (subseq (input break-stream) start end)
            (lines break-stream))
      (nreverse (lines break-stream)))))


;;; Strategies

(defvar *default-wide-break-chars*
  '(#\Space))

(defvar *default-new-line-chars*
  '(#\Newline))

(defvar *default-break-chars*
  '(#\-))

(defclass breaker-strategy ()
  ((wide-break-chars
    :initarg :wide-break-chars
    :accessor wide-break-chars)
   (new-line-chars
    :initarg :new-line-chars
    :accessor new-line-chars)
   (break-chars
    :initarg :break-chars
    :accessor break-chars)
   (line-width
    :initarg :line-width
    :accessor line-width))
  (:default-initargs
   :wide-break-chars *default-wide-break-chars*
   :new-line-chars *default-new-line-chars*
   :break-chars *default-break-chars*))

(defgeneric wide-break-char-p (char strategy)
  (:method (char strategy)
    (member char (wide-break-chars strategy))))

(defgeneric new-line-char-p (char strategy)
  (:method (char strategy)
    (member char (new-line-chars strategy))))

(defgeneric break-char-p (char strategy)
  (:method (char strategy)
    (member char (break-chars strategy))))

(defgeneric char-width (char strategy)
  (:method (char strategy)
    1))

(defgeneric line-width (strategy))

(defgeneric break-into-lines (string strategy)
  (:method (string strategy)
    (let ((breaker (make-instance 'break-stream :input string))
          (width 0)
          (max-width (line-width strategy)))
      (loop
       (let ((char (advance breaker)))
         (when char
           (incf width (char-width char strategy)))
         (cond ((null char)
                (return (finish breaker)))
               ((wide-break-char-p char strategy)
                (mark-wide-break breaker))
               ((new-line-char-p char strategy)
                (setf width 0)
                (mark-wide-break breaker)
                (break-line breaker))
               ((break-char-p char strategy)
                (mark-break breaker)))
         (when (< max-width width)
           (setf width 0)
           (break-line breaker)))))))

(defun width-breaker (width)
  (make-instance 'breaker-strategy :line-width width))
