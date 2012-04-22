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

(declaim (eol-style *default-eol-style*))
(defvar *default-eol-style* #+win32 :crlr #-win32 :lf)

(declaim (type (or null character) *default-replacement-character*))
(defvar *default-replacement-character* nil)

(defstruct external-format
  (character-encoding (missing-arg) :type character-encoding)
  (eol-style *default-eol-style* :type eol-style)
  (replacement *default-replacement-character* :type (or null character))
  (name))

(defconstant +bom-mark+ (code-char #xfeff))

(defvar *bom-mark-map*
  '(((#xef #xbb #xbf) :utf-8)
    ((#xff #xfe) :utf-16le)
    ((#xfe #xff) :utf-16be)))

(defun interpret-bom (octets start count)
  (when (>= count 2)
    (flet ((bom-from-sap (sap)
             (case (sap-ref-8 sap start)
               (#xef
                (when (and (>= count 3)
                           (= #xbb (sap-ref-8 sap (+ start 1)))
                           (= #xbf (sap-ref-8 sap (+ start 2))))
                  (values :utf-8 3)))
               (#xff
                (when (= #xfe (sap-ref-8 sap (+ start 1)))
                  (values :utf-16le 2)))
               (#xfe
                (when (= #xff (sap-ref-8 sap (+ start 1)))
                  (values :utf-16be 2))))))
      (etypecase octets
        (system-area-pointer
         (bom-from-sap octets))
        ((simple-array (unsigned-byte 8) (*))
         (with-pinned-objects (octets)
           (bom-from-sap (vector-sap octets))))))))

(defun find-external-format (external-format &optional (errorp t))
  (etypecase external-format
    (symbol
     (awhen (find-character-encoding external-format errorp)
       (make-external-format :character-encoding it)))
    (cons
     (destructuring-bind (name &key (eol-style *default-eol-style*)
                                    (replacement *default-replacement-character*))
         external-format
       (make-external-format
        :character-encoding (find-character-encoding name errorp)
        :eol-style eol-style
        :replacement replacement)))
    (external-format external-format)))

(defun parse-external-format (external-format)
  (etypecase external-format
    (symbol
     (values external-format *default-eol-style* *default-replacement-character*))
    (cons
     (destructuring-bind (name &key (eol-style *default-eol-style*)
                                    (replacement *default-replacement-character*))
         external-format
       (values name eol-style replacement)))
    (external-format
     (values (external-format-name external-format)
             (external-format-eol-style external-format)
             (external-format-replacement external-format)))))

(defun select-external-format (external-format octets start count)
  (multiple-value-bind (name eol-style replacement)
      (parse-external-format external-format)
    (multiple-value-bind (real-name skip)
        (case name
          (:utf-16
           ;; FIXME: Warn if BOM doesn't match the format
           (multiple-value-bind (format bytes)
               (interpret-bom octets start count)
             (if format
                 (values format bytes)
                 (values :utf-16be 0))))
          (t
           (values name 0)))
      (values
       (typecase external-format
         (external-format
          (if (eq real-name name)
              external-format
              (make-external-format
               :character-encoding (find-character-encoding real-name)
               :name name
               :eol-style eol-style
               :replacement replacement)))
         ((or cons symbol)
          (make-external-format
           :name name
           :character-encoding (find-character-encoding real-name)
           :eol-style eol-style
           :replacement replacement)))
       skip))))


