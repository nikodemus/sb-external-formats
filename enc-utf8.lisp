(in-package :sb-external-format)

(define-character-encoding :utf-8
  (:nicknames :utf8)
  (:documentation "Variable length encoding.")
  (:eol-info '((:lf 10)
               (:cr 13)
               (:crlf 13 10))))

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

(defdecoder :utf-8 (src src-offset dst dst-offset length limit)
  (do ((i 0)
       (j 0 (1+ j)))
      ((or (= i length) (= j limit))
       (values i j nil))
    (declare (index i j) (optimize (safety 0) (speed 3)))
    (flet ((invalid-utf-8-sequence ()
             (values (invalid-utf-8-sequence-error))))
      (set-char-code j
          (macrolet ((with-byte ((var &optional startp) &body clauses)
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
                                       (invalid-utf-8-sequence))))
                                ,@clauses))))
            (with-byte (byte t)
              ((< byte #x80)
               ;; 1 byte
               byte)
              ((< byte #xc0)
               ;; invalid start byte
               (invalid-utf-8-sequence))
              (t
               (with-byte (byte2)
                 ((< byte #xc2)
                  ;; overlong
                  (invalid-utf-8-sequence))
                 ((< byte #xe0)
                  ;; 2 bytes
                  (logior (ash (logand #x1f byte) 6)
                          (logxor #x80 byte2)))
                 (t
                  (with-byte (byte3)
                    ((and (= byte #xe0) (< byte2 #xa0))
                     ;; overlong
                     (invalid-utf-8-sequence))
                    ((< byte #xf0)
                     ;; 3 bytes
                     (logior (ash (logand #x0f byte) 12)
                             (ash (logand #x3f byte2) 6)
                             (logand #x3f byte3)))
                    (t
                     (with-byte (byte4)
                       ((and (= byte #xf0) (< byte2 #x90))
                        ;; overlong
                        (invalid-utf-8-sequence))
                       ((< byte #xf4)
                        (if (or (> byte #xf4) (and (= byte #xf4) (> byte2 #x8f)))
                            ;; FIXME: Code deletion hints at an logic problem here!
                            ;; character out of range
                            (invalid-utf-8-sequence)
                            ;; 4 bytes
                            (logior (ash (logand #x7 byte) 18)
                                    (ash (logxor #x80 byte2) 12)
                                    (ash (logxor #x80 byte3) 6)
                                    (logxor #x80 byte4))))
                       (t
                        (with-byte (byte5)
                          ((and (= byte #xf8) (< byte2 #x88))
                           ;; overlong
                           (invalid-utf-8-sequence))
                          ((< byte #xfc)
                           ;; character out of range
                           (invalid-utf-8-sequence))
                          (t
                           (with-byte (byte6)
                             ((and (= byte #xfc) (< byte2 #x84))
                              ;; overlong
                              (invalid-utf-8-sequence))
                             (t
                              ;; character out of range
                              (invalid-utf-8-sequence))))))))))))))))))
