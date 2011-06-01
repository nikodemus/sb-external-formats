(in-package :sb-external-format)

(define-character-encoding :latin-1
  (:nicknames :latin1)
  (:documentation ""))

(define-unibyte-encoder :latin-1 (char-code)
  (if (> char-code 255)
      (handle-error)
      char-code))