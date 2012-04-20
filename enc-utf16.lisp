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

(in-package :sb-external-format)

(define-character-encoding :utf-16le)

(define-character-encoding :utf-16be)

(defun utf-16-decoding-error (x y)
  (error "oops ~S: ~S" x y))

(eval-when (:compile-toplevel)
  ;; This is just spliced into the bodies below.
  (defmacro encode-utf-16-using (sap-ref)
    `(macrolet ((set-16 (offset value)
                  `(setf (,',sap-ref dst (+ k ,(* 2 offset))) ,value)))
       (let ((limit-4 (- limit 4))
             (length length))
         (declare (index limit-4 length))
         (do ((i 0 (1+ i))
              (j 0))
             ((or (= i length) (>= j limit-4))
              (values i j))
           (declare (index i j) (optimize (safety 0) (speed 3)))
           (let ((k (+ dst-offset j)))
             (do-encode (code (char src (+ src-offset i)))
               (:cr
                (set-16 0 13)
                (incf j 2))
               (:crlf
                (set-16 0 13)
                (set-16 1 10)
                (incf j 4))
               (cond ((< code #x10000)
                      (set-16 0 code)
                      (incf j 2))
                     (t
                      (let* ((codeoid (- code #x10000))
                             (high (dpb (ldb (byte 10 10) codeoid) (byte 10 0)
                                        #xd800))
                             (low (dpb (ldb (byte 10 0) codeoid) (byte 10 0)
                                       #xdc00)))
                        (set-16 0 high)
                        (set-16 1 low)
                        (incf j 4))))))))))
  ;; Same thing.
  (defmacro decode-utf-16-using (sap-ref)
    `(block utf-16-decoder
         (do ((i 0)
              (j 0 (+ j 1)))
             ((or (= i length) (= j limit))
              (values i j nil))
           (declare (index i j) (optimize (safety 0) (speed 3)))
           (set-char-code j
               (let ((bits (,sap-ref src (+ src-offset i))))
                 (incf i 2)
                 (cond
                   ((or (<= #xdc00 bits #xdfff)
                        (<= #xfdd0 bits #xfdef)
                        (= (logand bits #xfffe) #xfffe))
                    (utf-16-decoding-error :bad bits))
                   ((<= #xd800 bits #xdbff)
                    (let ((next (if (> length (+ i 2))
                                    (,sap-ref src (+ src-offset i 2))
                                    (return-from utf-16-decoder
                                      (values i j t)))))
                      (incf i 2)
                      (unless (<= #xdc00 next #xdfff)
                        (utf-16-decoding-error :bad2 next))
                      (let ((code (dpb (ldb (byte 10 0) bits) (byte 10 10)
                                       (ldb (byte 10 0) next))))
                        (if (= (logand code #xfffe) #xfffe)
                            (utf-16-decoding-error :bad3 code)
                            (+ #x10000 code)))))
                   (t
                    bits))))))))

(defencoder :utf-16le (src src-offset dst dst-offset length limit)
  (encode-utf-16-using sb-impl::sap-ref-16le))

(defdecoder :utf-16le (src src-offset dst dst-offset length limit)
  (decode-utf-16-using sb-impl::sap-ref-16le))

(defencoder :utf-16be (src src-offset dst dst-offset length limit)
  (encode-utf-16-using sb-impl::sap-ref-16be))

(defdecoder :utf-16be (src src-offset dst dst-offset length limit)
  (decode-utf-16-using sb-impl::sap-ref-16be))
