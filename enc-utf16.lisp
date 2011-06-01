(in-package :sb-external-format)

(define-character-encoding :utf-16le)

(defencoder :utf-16le (src src-offset dst dst-offset length limit)
  (macrolet ((set-16 (offset value)
               `(setf (sb-impl::sap-ref-16le dst (+ k ,(* 2 offset))) ,value)))
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

(defun utf-16-decoding-error (x y)
  (error "oops ~S: ~S" x y))

(defdecoder :utf-16le (src src-offset dst dst-offset length limit)
  (do ((i 0)
       (j 0 (+ j 1)))
      ((or (= i length) (= j limit))
       (values i j nil))
    (declare (index i j) (optimize (safety 0) (speed 3)))
    (set-char-code j
        (let ((bits (sb-impl::sap-ref-16le src (+ src-offset i))))
          (incf i 2)
          (cond
            ((or (<= #xdc00 bits #xdfff)
                 (<= #xfdd0 bits #xfdef)
                 (= (logand bits #xfffe) #xfffe))
             (utf-16-decoding-error :bad bits))
            ((<= #xd800 bits #xdbff)
             (let ((next (if (> length (+ i 2))
                             (sb-impl::sap-ref-16le src (+ src-offset i 2))
                             (return-from utf-16le-decoder
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
             bits))))))