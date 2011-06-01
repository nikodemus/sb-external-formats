(in-package :cl-user)

(defparameter *s-0-127*
  (let* ((base (with-output-to-string (s)
                 (loop repeat 1000
                       do (loop repeat (random 100)
                                for char = (loop for char = (code-char (random 128))
                                                 while (member char '(#\return #\newline))
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

(defparameter *o-0-127*
  (mapcar (lambda (s)
            (if (stringp s)
                (map '(simple-array (unsigned-byte 8) (*))
                     #'char-code
                     s)
                s))
          *s-0-127*))

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
                                                 while (member char '(#\newline #\return))
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

(defparameter *o-0-255*
  (mapcar (lambda (s)
            (if (stringp s)
                (map '(simple-array (unsigned-byte 8) (*))
                     #'char-code
                     s)
                s))
          *s-0-255*))

(defparameter *utf-8-0-255*
  (mapcar (lambda (s)
            (if (stringp s)
                (string-to-octets s :external-format :utf-8)
                s))
          *s-0-255*))

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

(defun test-decode (method octets format &optional (newline :lf))
  (ecase method
    (:new
     (sb-external-format:decode-octets (getf octets newline)
                                       :external-format (list format :eol-style newline)))
    (:old
     (sb-ext:octets-to-string (getf octets :lf)
                              :external-format format))
    (:babel
     (babel:octets-to-string (getf octets :lf)
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
    (let ((orig (getf strings :lf))
          (back (sb-external-format:decode-octets a :external-format (list format :eol-style newline))))
      (assert (string= orig back)))
    t))