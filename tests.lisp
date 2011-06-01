(in-package :cl-user)

(defparameter *s-0-127*
  (let* ((base (with-output-to-string (s)
                 (loop repeat 1000
                       do (loop repeat (random 100)
                                for char = (loop for char = (code-char (random 128))
                                                 while (eql #\newline char)
                                                 finally (return char))
                                do (write-char char s))
                          (terpri s))))
         (cr (substitute #\return #\newline base))
         (crlf (with-output-to-string (s)
                 (loop for char across base
                       when (eql #\newline char)
                       do (write-char #\return s)
                          (write-char #\newline s)
                       else
                       do (write-char char s)))))
    (list :lf base :cr cr :crlf crlf)))

(defparameter *b-0-127*
  (mapcar (lambda (s)
            (if (stringp s)
                (coerce s 'base-string)
                s))
          *s-0-127*))

(defparameter *s-0-255*
  (let* ((base (with-output-to-string (s)
                 (loop repeat 1000
                       do (loop repeat (random 100)
                                for char = (loop for char = (code-char (random 256))
                                                 while (eql #\newline char)
                                                 finally (return char))
                                do (write-char char s))
                          (terpri s))))
         (cr (substitute #\return #\newline base))
         (crlf (with-output-to-string (s)
                 (loop for char across base
                       when (eql #\newline char)
                       do (write-char #\return s)
                          (write-char #\newline s)
                       else
                       do (write-char char s)))))
    (list :lf base :cr cr :crlf crlf)))

(defun test-encode (method strings format &optional (newline :lf))
  (ecase method
    (:new
     (sb-external-format:encode-string (getf strings :lf)
                                       :external-format (list format :eol-style newline)))
    (:old
     (sb-ext:string-to-octets (getf strings newline)
                              :external-format format))
    (:babel
     (babel:string-to-octets (getf strings newline)
                             :encoding format))))

(defun verify (strings format newline)
  (let ((a (test-encode :new strings format newline))
        (b (test-encode :old strings format newline))
        (c (test-encode :babel strings format newline)))
    (assert (= (length a) (length b) (length c)))
    (dotimes (i (length a))
      (unless (= (aref a i) (aref b i) (aref c i))
        (cerror "Continue" "~S: new: ~S, old: ~S, babel: ~S~%  ~
                 ~S~%  ~S~%  ~S"
                i (aref a i) (aref b i) (aref c i)
                (subseq a (max 0 (- i 3)) (min (length a) (+ i 3)))
                (subseq b (max 0 (- i 3)) (min (length b) (+ i 3)))
                (subseq c (max 0 (- i 3)) (min (length c) (+ i 3))))))
    t))