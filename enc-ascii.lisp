(in-package :sb-external-format)

(define-character-encoding :ascii
  (:nicknames :us-ascii :ansi_x3.4-1968 :iso-646 :iso-646-us :|646|)
  (:documentation
   "A 7-bit encoding. Characters in code range 0-127 map to octets encoding
their character codes. Characters outside that range cannot be encoded.")
  (:eol-info '((:lf 10)
               (:cr 13)
               (:crlf 13 10))))

(define-unibyte-encoder :ascii (char-code)
  (if (> char-code 127)
      (handle-error)
      char-code))

;;; FIXME -- decoding and decoded length doesn't work yet
(define-unibyte-decoder :ascii (octet)
  (if (> octet 127)
      (handle-error)
      octet))
