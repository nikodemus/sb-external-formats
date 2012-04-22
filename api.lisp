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
         (encoding (external-format-character-encoding format))
         (eol (external-format-eol-style format))
         (replacement (external-format-replacement format)))
    (handler-bind
        ((encoding-error (lambda (c)
                           (when replacement
                             (use-value replacement c)))))
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
          ;; FIXME: better guesstimation
          (loop for alloc = (+ count 4)
                for buffer = (make-array alloc :element-type '(unsigned-byte 8))
                do (multiple-value-bind (consumed encoded)
                       (%encode encoding s-string offset buffer 0 count alloc eol)
                     (declare (index consumed encoded))
                     (incf offset consumed)
                     (incf total-size encoded)
                     (sb-kernel:%shrink-vector buffer encoded)
                     (cond ((eql consumed count)
                            (return-from encode-string
                              (if buffers
                                  (let ((result (make-array total-size
                                                            :element-type '(unsigned-byte 8)))
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
  (let ((octets (coerce octets '(vector (unsigned-byte 8)))))
    (sb-kernel:with-array-data ((s-octets octets)
                                (s-start start)
                                (s-end end)
                                :check-fill-pointer t)
      (let ((count (- s-end s-start)))
        (declare (index count))
        (multiple-value-bind (format skip)
            (select-external-format external-format s-octets s-start count)
          (declare (index skip))
          (decf count skip)
          (incf s-start skip)
          (let* ((encoding (external-format-character-encoding format))
                 (eol (external-format-eol-style format))
                 (replacement (external-format-replacement format))
                 (newline-interval 80)
                 (offset s-start)
                 (total-size 0)
                 (buffers nil))
            (declare (index total-size newline-interval offset))
            ;; FIXME: better guesstimation: add alloc-string and
            ;; alloc-octets methods to encoding: random-sampling of the
            ;; input to determine how many octets per character, and
            ;; trying to determine the size of typical lines.
            (handler-bind ((decoding-error (lambda (c)
                                             (when replacement
                                               (use-value replacement c)))))
              (loop for alloc = count
                    for buffer = (make-array alloc :element-type 'character)
                    do (multiple-value-bind (consumed decoded end)
                           (%decode encoding s-octets offset buffer 0 count alloc eol)
                         (declare (index consumed decoded))
                         (incf offset consumed)
                         (incf total-size decoded)
                         (sb-kernel:%shrink-vector buffer decoded)
                         (cond ((or end (eql consumed count))
                                (return-from decode-octets
                                  (let ((total-consumed (- offset s-start)))
                                    (if buffers
                                        (let ((result (make-array total-size
                                                                  :element-type 'character))
                                              (p 0))
                                          (dolist (buf (nreverse (cons buffer buffers)))
                                            (declare (type (simple-array character (*)) buf))
                                            (replace result buf :start1 p)
                                            (incf p (length buf)))
                                          (values result total-consumed))
                                        (values buffer total-consumed)))))
                               (t
                                (when (>= 20 newline-interval)
                                  (setf newline-interval (truncate newline-interval 4)))
                                (push buffer buffers)
                                (decf count consumed))))))))))))

(defun external-format-repertoire (external-format)
  "Returns a list describing the character repertoire of an
external format.

The lists starts with conses of inclusive character code bounds, describing
contiguous parts of the repertoire. These are followed solitary characters
outside those ranges.

Eg. a hypothetical external format capatable of encoding characters
in code ranges 65-90, and 97-122, and all odd numerals would return:

  ((65 . 90) (97 . 122) #\1 #\3 #\5 #\7 #\9)

Eg. ASCII repertoire is:

  ((0 . 127))
"
  (character-encoding-repertoire
   (external-format-character-encoding
    (find-external-format external-format))))

(defun binary-external-format-p (external-format)
  "Returns true if EXTERNAL-FORMAT can be used to round-trip arbitrary
binary data."
  (let ((format (find-external-format external-format)))
    (and (eq :lf (external-format-eol-style format))
         (character-encoding-binary
          (external-format-character-encoding format)))))
