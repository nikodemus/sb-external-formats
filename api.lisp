(in-package :sb-external-format)

(declaim (ftype (function (string &key
                                  (:start index)
                                  (:end (or null index))
                                  (:limit (or null index))
                                  (:external-format (or keyword external-format cons)))
                          (values index index &optional))
                encoded-length))
(defun encoded-length (string &key (start 0) end limit (external-format :default))
  "Returns the number of octets required to encode substring of STRING
delimited by START and END using EXTERNAL-FORMAT. If LIMIT is not null, it is
the maximum number of octets to encode as: secondary return value is the index
of the fist character of STRING not included in the count, which can be less
than the END if LIMIT is not sufficient for encoding all characters in the
substring."
  (let* ((format (find-external-format external-format))
         (encoding (external-format-character-encoding format)))
    (destructuring-bind (char-code-0 &optional char-code-1) (external-format-eol-info format)
      (sb-kernel:with-array-data ((s-string string)
                                  (s-start start)
                                  (s-end end)
                                  :check-fill-pointer t)
        (%encoded-length encoding s-string s-start (- s-end s-start) limit
                         char-code-0 char-code-1)))))

(declaim (ftype (function (sequence &key
                                    (:start index)
                                    (:end (or null index))
                                    (:limit (or null index))
                                    (:external-format (or keyword external-format)))
                          (values index index &optional))
                encoded-length))
(defun decoded-length (octets &key (start 0) end limit (external-format :default))
  "Returns the number of characters the subsequence of OCTETS delimited by
START and END decodes into using EXTERNAL-FORMAT. If LIMIT is not null,
it is the maximum number of characters to decode into: secondary return value
is the index of the first octet not included in the count, which can be less
than END if LIMIT is not sufficient for decoding all octets in the
subsequence.

Efficiency note: performance will be severely degraded unless OCTETS is of
type \(VECTOR \(UNSIGNED-BYTE 8))."
  (let* ((format (find-external-format external-format))
         (encoding (external-format-character-encoding format))
         (vector (coerce octets '(vector (unsigned-byte 8)))))
    (destructuring-bind (code0 code1) (external-format-eol-info format)
      (sb-kernel:with-array-data ((s-vector vector)
                                  (s-start start)
                                  (s-end end)
                                  :check-fill-pointer t)
        (with-pinned-objects (s-vector)
          (%decoded-length encoding
                           (vector-sap s-vector)
                           s-start
                           (- s-end s-start)
                           limit
                           code0
                           code1))))))

(declaim (index *average-newline-interval*))
(defvar *average-newline-interval* 30)

(defun encode-string (string &key (start 0) end (external-format :default))
  (declare (optimize speed))
  (let* ((format (find-external-format external-format))
         (encoding (external-format-character-encoding format)))
    (destructuring-bind (char-code-0 &optional char-code-1) (external-format-eol-info format)
      (sb-kernel:with-array-data ((s-string string)
                                  (s-start start)
                                  (s-end end)
                                  :check-fill-pointer t)
        (let ((count (- s-end s-start))
              (newline-interval 80)
              (offset s-start)
              (total-size 0)
              (buffers nil))
          (declare (index count total-size newline-interval offset))
          (loop for alloc = (max 64
                                 (if char-code-1
                                     ;; Guesstimate newlines
                                     (+ count (* 2 (1+ (truncate count newline-interval))))
                                     count))
                for buffer = (make-array alloc :element-type '(unsigned-byte 8))
                do (multiple-value-bind (consumed encoded)
                       (%encode encoding s-string offset buffer 0 count alloc
                                char-code-0 char-code-1)
                     (declare (index consumed encoded))
                     (incf offset consumed)
                     (incf total-size encoded)
                     (sb-kernel:%shrink-vector buffer encoded)
                     (cond ((eql consumed count)
                            (return-from encode-string
                              (if buffers
                                  (let ((result (make-array total-size :element-type '(unsigned-byte 8)))
                                        (p 0))
                                    (dolist (buf (nreverse (cons buffer buffers)))
                                      (declare (type (simple-array (unsigned-byte 8) (*)) buf))
                                      (replace result buf :start1 p)
                                      (incf p (length buf)))
                                    result)
                                  buffer)))
                           (t
                            (when (>= 20 newline-interval)
                              (setf newline-interval (truncate newline-interval 4)))
                            (push buffer buffers)
                            (decf count consumed))))))))))

(defun decode-octets (octets &key (start 0) end (external-format :default))
  (let* ((format (find-external-format external-format))
         (encoding (external-format-character-encoding format))
         (octets (coerce octets '(vector (unsigned-byte 8)))))
    (destructuring-bind (char-code-0 &optional char-code-1) (external-format-eol-info format)
      (sb-kernel:with-array-data ((s-octets octets)
                                  (s-start start)
                                  (s-end end)
                                  :check-fill-pointer t)
        (with-pinned-objects (s-octets)
          (let* ((sap (vector-sap s-octets))
                 (size (%decoded-length encoding sap s-start (- s-end s-start) nil
                                        char-code-0 char-code-1))
                (buffer (make-array (the fixnum size) :element-type 'character)))
            (%decode encoding sap
                     s-start buffer 0 (- s-end s-start)
                     nil char-code-0 char-code-1)
           buffer))))))
