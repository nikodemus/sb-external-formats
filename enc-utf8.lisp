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

(define-character-encoding :utf-8
  (:nicknames :utf8)
  (:documentation "Variable length encoding."))

(defencoder :utf-8 (src src-offset dst dst-offset length limit)
  (macrolet ((set-byte (offset value)
               `(setf (sap-ref-8 dst (+ k ,offset)) ,value)))
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
             (set-byte 0 13)
             (incf j 1))
            (:crlf
             (set-byte 0 13)
             (set-byte 1 10)
             (incf j 2))
            (cond ((< code #x80)
                   (set-byte 0 code)
                   (incf j))
                  ((< code #x800)
                   (set-byte 0 (logior #xc0 (ash code -6)))
                   (set-byte 1 (logior #x80 (logand #x3f code)))
                   (incf j 2))
                  ((< code #x10000)
                   (set-byte 0 (logior #xe0 (ash code -12)))
                   (set-byte 1 (logior #x80 (logand #x3f (ash code -6))))
                   (set-byte 2 (logior #x80 (logand #x3f code)))
                   (incf j 3))
                  (t
                   (set-byte 0 (logior #xf0 (logand #x07 (ash code -18))))
                   (set-byte 1 (logior #x80 (logand #x3f (ash code -12))))
                   (set-byte 2 (logior #x80 (logand #x3f (ash code -6))))
                   (set-byte 3 (logior #x80 (logand #x3f code)))
                   (incf j 4)))))))))

(defun utf-8-decoding-error (byte byte2 byte3 byte4)
  (cond (byte4
         (decoding-error :utf-8 (list byte byte2 byte3 byte4)))
        (byte3
         (decoding-error :utf-8 (list byte byte2 byte3)))
        (byte2
         (decoding-error :utf-8 (list byte byte2)))
        (t
         (decoding-error :utf-8 (list byte)))))

(defdecoder :utf-8 (src src-offset dst dst-offset length limit)
  (do ((i 0)
       (j 0 (1+ j)))
      ((or (= i length) (= j limit))
       (values i j nil))
    (declare (index i j) (optimize (safety 0) (speed 3)))
    (set-char-code
     j
     (flet ((utf-8-error (byte &optional byte2 byte3 byte4)
              (or (utf-8-decoding-error byte byte2 byte3 byte4)
                  (progn
                    (decf j)
                    (go skip)))))
       (macrolet ((with-byte ((var (&rest others) &optional startp) &body clauses)
                    `(let ((,var ,(if startp
                                      ;; For the first byte we know
                                      ;; it's in range -- the rest
                                      ;; we need to check. This
                                      ;; makes decoding high ranges
                                      ;; a bit slower, though.
                                      ;;
                                      ;; One option would be to
                                      ;; decode without the check
                                      ;; till LENGTH-4, and the
                                      ;; rest with the check.
                                      `(sap-ref-8 src (+ src-offset i))
                                      `(if (> length i)
                                           (sap-ref-8 src (+ src-offset i))
                                           (return-from utf-8-decoder
                                             (values (1- i) (1- j) t))))))
                       (incf i)
                       (cond ,@(unless startp
                                 `(((not (< #x7f ,var #xc0))
                                    ;; invalid continuation byte
                                    (utf-8-error ,@others ,var))))
                             ,@clauses))))
         (with-byte (byte () t)
           ((< byte #x80)
            ;; 1 byte
            byte)
           ((< byte #xc0)
            ;; invalid start byte
            (utf-8-error byte))
           (t
            (with-byte (byte2 (byte))
              ((< byte #xc2)
               ;; overlong
               (utf-8-error byte byte2))
              ((< byte #xe0)
               ;; 2 bytes
               (logior (ash (logand #x1f byte) 6)
                       (logxor #x80 byte2)))
              (t
               (with-byte (byte3 (byte byte2))
                 ((and (= byte #xe0) (< byte2 #xa0))
                  ;; overlong
                  (utf-8-error byte byte2 byte3))
                 ((< byte #xf0)
                  ;; 3 bytes
                  (logior (ash (logand #x0f byte) 12)
                          (ash (logand #x3f byte2) 6)
                          (logand #x3f byte3)))
                 (t
                  (with-byte (byte4 (byte byte2 byte3))
                    ((and (= byte #xf0) (< byte2 #x90))
                     ;; overlong
                     (utf-8-error byte byte2 byte3 byte4))
                    ((< byte #xf4)
                     (if (or (> byte #xf4) (and (= byte #xf4) (> byte2 #x8f)))
                         ;; FIXME: Code deletion hints at an logic problem here!
                         ;; character out of range
                         (utf-8-error byte byte2 byte3 byte4)
                         ;; 4 bytes
                         (logior (ash (logand #x7 byte) 18)
                                 (ash (logxor #x80 byte2) 12)
                                 (ash (logxor #x80 byte3) 6)
                                 (logxor #x80 byte4))))
                    (t
                     (utf-8-error byte byte2 byte3 byte4))))))))))))
   skip))

#+nil
(define-character-encoding :utf-8b)

#+nil
(defencoder :utf-8b (src src-offset dst dst-offset length limit)
  (macrolet ((set-byte (offset value)
               `(setf (sap-ref-8 dst (+ k ,offset)) ,value)))
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
             (set-byte 0 13)
             (incf j 1))
            (:crlf
             (set-byte 0 13)
             (set-byte 1 10)
             (incf j 2))
            (cond ((< code #x80)
                   (set-byte 0 code)
                   (incf j))
                  ((< code #x800)
                   (set-byte 0 (logior #xc0 (ash code -6)))
                   (set-byte 1 (logior #x80 (logand code #x3f)))
                   (incf j 2))
                  ((<= #xdc80 code #xdcff) ; invalid
                   (set-byte 0 (logand code #xff))
                   (incf j))
                  ((< code #x10000)
                   (set-byte 0 (logior #xe0 (ash code -12)))
                   (set-byte 1 (logior #x80 (logand #x3f (ash code -6))))
                   (set-byte 2 (logior #x80 (logand code #x3f)))
                   (incf j 3))
                  ;; 4 octets
                  (t
                   (set-byte 0 (logior #xf0 (logand #x07 (ash code -18))))
                   (set-byte 1 (logior #x80 (logand #x3f (ash code -12))))
                   (set-byte 2 (logior #x80 (logand #x3f (ash code -6))))
                   (set-byte 3 (logand #x3f code))
                   (incf j 4)))))))))

#+nil
(defdecoder :utf-8b (src src-offset dst dst-offset length limit)
  )
