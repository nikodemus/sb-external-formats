;;;; By Nikodemus Siivola <nikodemus@random-state.net>, 2012.
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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

(defparameter *s-0-10000*
  (let* ((base (with-output-to-string (s)
                 (loop repeat 1000
                       do (loop repeat (random 100)
                                for char = (loop for char = (code-char (random 10001))
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

(defparameter *utf-8-0-10000*
  (mapcar (lambda (s)
            (if (stringp s)
                (string-to-octets s :external-format :utf-8)
                s))
          *s-0-10000*))

(defparameter *utf-16le-0-10000*
  (mapcar (lambda (s)
            (if (stringp s)
                (string-to-octets s :external-format :utf-16le)
                s))
          *s-0-10000*))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :babel))

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

(defun verify (strings format newline &optional dont-decode)
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
    (unless dont-decode
      (let ((orig (getf strings :lf))
            (back
              #+nil
              (octets-to-string a :external-format format)
              (sb-external-format:decode-octets a :external-format (list format :eol-style newline))))
        (assert (= (length orig) (length back)))
        (dotimes (i (length orig))
          (unless (eql (char orig i) (char back i))
            (cerror "Continue" "~S: orig: ~C (~S), back: ~C (~S)"
                    i
                    (char orig i) (char-code (char orig i))
                    (char back i) (char-code (char back i)))))))
    t))

(verify *s-0-127* :ascii :cr)
(verify *s-0-255* :latin-1 :lf)
(verify *s-0-10000* :utf-8 :crlf)
(verify *s-0-10000* :utf-16le :crlf)
(verify *s-0-10000* :utf-16be :cr)

(defun run-encode (method strings format n &optional (newline :lf))
  (declare (fixnum n))
  (loop repeat n
        do (test-encode method strings format newline)))

(defun run-decode (method octets format n &optional (newline :lf))
  (declare (fixnum n))
  (loop repeat n
        do (test-decode method octets format newline)))

(defun bench (strings format &optional (n 1000))
  (let ((octets (list :lf (test-encode :new strings format :lf)
                      :cr (test-encode :new strings format :cr)
                      :crlf (test-encode :new strings format :crlf))))
  (fresh-line)
  (write-line "============================")
  (format t "~&BENCHMARKING ~S characters~%" (length (getf strings :lf)))
    (format t "~&Old ~A encoding~%" format)
    (time (run-encode :old strings format n))
    (format t "~&Babel ~A encoding~%" format)
    (time (run-encode :babel strings format n))
    (format t "~&New ~A encoding LF~%" format)
    (time (run-encode :new strings format n :lf))
    (format t "~&New ~A encoding CR~%" format)
    (time (run-encode :new strings format n :cr))
    (format t "~&New ~A encoding CRLF~%" format)
    (time (run-encode :new strings format n :crlf))
    (write-line "---------------------------")
    (format t "~&Old ~A decoding~%" format)
    (time (run-decode :old octets format n))
    (format t "~&Babel ~A decoding~%" format)
    (time (run-decode :babel octets format n))
    (format t "~&New ~A decoding LF~%" format)
    (time (run-decode :new octets format n :lf))
    (format t "~&New ~A decoding CR~%" format)
    (time (run-decode :new octets format n :cr))
    (format t "~&New ~A decoding CRLF~%" format)
    (time (run-decode :new octets format n :crlf))))

(bench *s-0-127* :ascii)
(bench *s-0-127* :latin-1)
(bench *s-0-127* :utf-8)
(bench *s-0-127* :utf-16le)
(bench *s-0-127* :utf-16be)

(bench *s-0-255* :latin-1)
(bench *s-0-255* :utf-8)
(bench *s-0-255* :utf-16le)
(bench *s-0-255* :utf-16be)

(bench *s-0-10000* :utf-8)
(bench *s-0-10000* :utf-16le)
(bench *s-0-10000* :utf-16be)

(assert (string= "123456"
                 (let ((string "1Ä2Ö3Å4ä5ö6å"))
                   (sb-external-format:decode-octets
                    (handler-bind ((error #'continue))
                      (sb-external-format:encode-string string :external-format :ascii))
                    :external-format :ascii))))

(assert (string= "1A2O3A4a5o6a"
                 (let ((string "1Ä2Ö3Å4ä5ö6å"))
                   (sb-external-format:decode-octets
                    (handler-bind
                        ((error (lambda (c)
                                  (let ((replacement
                                          (case
                                              (sb-external-format::encoding-error-character c)
                                            ((#\Ä #\Å)
                                             #\A)
                                            (#\Ö
                                             #\O)
                                            ((#\ä #\å)
                                             #\a)
                                            (#\ö
                                             #\o))))
                                    (if replacement
                                        (use-value replacement c)
                                        (continue c))))))
                      (sb-external-format:encode-string string :external-format :ascii))
                    :external-format :ascii))))

(assert (string= "Look, a ? and a ?!"
                 (sb-external-format:decode-octets
                  (let ((string (format nil "Look, a ~A and a ~A!"
                                        #\GREEK_CAPITAL_LETTER_LAMDA
                                        #\GREEK_SMALL_LETTER_LAMDA)))
                    (assert (= 18 (length string)))
                    (sb-external-format:encode-string
                     string
                     :external-format '(:ascii :replacement #\?)))
                  :external-format :ascii)))

(assert (string= "Look, an x and an x!"
                 (sb-external-format:decode-octets
                  (let ((string (format nil "Look, an Ä and an Ö!")))
                    (assert (= 20 (length string)))
                    (sb-external-format:encode-string string
                                                      :external-format :latin-1))
                  :external-format '(:ascii :replacement #\x))))

(assert (string= "Föä"
                 (handler-bind
                     ((error (lambda (c)
                               (use-value
                                (code-char (car (sb-external-format::decoding-error-octets c)))
                                c))))
                   (sb-external-format:decode-octets
                    (sb-external-format:encode-string "Föä" :external-format :latin-1)
                    :external-format :ascii))))

(assert (string= "F!"
                 (handler-bind
                     ((error #'continue))
                   (sb-external-format:decode-octets
                    (sb-external-format:encode-string "Föä!" :external-format :latin-1)
                    :external-format :ascii))))

(assert (string= "fbar"
                 (handler-bind ((error #'continue))
                   (sb-external-format:decode-octets
                    (sb-external-format:encode-string "fööbar" :external-format :latin-1)
                    :external-format :utf-8))))

(assert (string= "fXbar"
                 (sb-external-format:decode-octets
                  (sb-external-format:encode-string "fööbar" :external-format :latin-1)
                  :external-format '(:utf-8 :replacement #\X))))
