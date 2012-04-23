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

;;;; Perhaps controversially, for most LATIN formats we canonicalize on the
;;;; more memorable name, LATIN N, and not the official ISO name, which are
;;;; harder for humans to recognize at a glance.

;;;; ISO 8859-1 (LATIN 1)

(define-character-encoding :iso-8859-1
  (:nicknames :iso-8859-1 :iso8859-1
              :latin-1 :latin1
              :iso-ir-100 :csisolatin1 :l1 :ibm819 :cp819)
  (:documentation
   "Western European encoding. Maps character codes 0-255 directly to
corresponding octets."))

(define-unibyte-mapping-from-file :iso-8859-1 "unicode/8859-1.TXT")

;;;; ISO 8859-2 (LATIN 2)

(define-character-encoding :iso-8859-2
  (:nicknames :iso8859-2 :latin-2 :latin2)
  (:documentation
   "Eastern European encoding. NOTE: Very different from :CP852 (MS-DOS Latin
2, PC Latin 2), which is also referred to as `Latin 2' in Czech and Slovak
regions."))

(define-unibyte-mapping-from-file :iso-8859-2 "unicode/8859-2.TXT")

;;;; ISO 8859-3 (LATIN 3)

(define-character-encoding :iso-8859-3
  (:nicknames :iso8859-3 :latin-3 :latin3)
  (:documentation "South European encoding."))

(define-unibyte-mapping-from-file :iso-8859-3 "unicode/8859-3.TXT")

;;;; ISO 8859-4 (LATIN 4)

(define-character-encoding :iso-8859-4
  (:nicknames :iso8859-4 :latin-4 :latin4)
  (:documentation "North European encoding."))

(define-unibyte-mapping-from-file :iso-8859-4 "unicode/8859-4.TXT")

;;;; ISO 8859-5 (LATIN/Cyrillic)

(define-character-encoding :iso-8859-5
  (:nicknames :iso8859-5 :latin/cyrillic))

(define-unibyte-mapping-from-file :iso-8859-5 "unicode/8859-5.TXT")

;;;; ISO 8859-6

(define-character-encoding :iso-8859-6
  (:nicknames :iso8859-6 :latin/arabic)
  (:documentation "Arabic alphabet encoding."))

(define-unibyte-mapping-from-file :iso-8859-6 "unicode/8859-6.TXT")

;;;; ISO 8859-7 (LATIN/GREEK)

(define-character-encoding :iso-8859-7
  (:nicknames :iso8859-7 :latin/greek)
  (:documentation
   "Modern Greek encoding. 2003 version with Euro, Drachma, and Greek
Ypogegrammeni symbols."))

(define-unibyte-mapping-from-file :iso-8859-7 "unicode/8859-7.TXT")

;;;; ISO 8859-8 (LATIN/HEBREW)

(define-character-encoding :iso-8859-8
  (:nicknames :iso8859-8 :latin/hebrew)
  (:documentation "Hebrew encoding."))

(define-unibyte-mapping-from-file :iso-8859-8 "unicode/8859-8.TXT")

;;;; ISO 8859-9 (LATIN 5)

(define-character-encoding :iso-8859-9
  (:nicknames :iso8859-9 :latin-5 :latin5)
  (:documentation "Turkish encoding."))

(define-unibyte-mapping-from-file :iso-8859-9 "unicode/8859-9.TXT")

;;;; ISO 8859-10 (LATIN 6)

(define-character-encoding :iso-8859-10
  (:nicknames :iso8859-10 :latin-6 :latin6)
  (:documentation "Nordic language encoding."))

(define-unibyte-mapping-from-file :iso-8859-10 "unicode/8859-10.TXT")

;;;; ISO 8859-11 (LATIN/THAI)

(define-character-encoding :iso-8859-11
  (:nicknames :iso8859-11 :latin/thai)
  (:documentation "Thai encoding."))

(define-unibyte-mapping-from-file :iso-8859-11 "unicode/8859-11.TXT")

;;;; ISO 8859-12 was never defined.

;;;; ISO 8859-13 (LATIN 7)

(define-character-encoding :iso-8859-13
  (:nicknames :iso8859-13 :latin-7 :latin7)
  (:documentation "Baltic language encoding."))

(define-unibyte-mapping-from-file :iso-8859-13 "unicode/8859-13.TXT")

;;;; ISO 8859-14 (LATIN 8)

(define-character-encoding :iso-8859-14
  (:nicknames :iso8859-14 :latin-8 :latin8)
  (:documentation "Celtic language encoding."))

(define-unibyte-mapping-from-file :iso-8859-14 "unicode/8859-14.TXT")

;;;; ISO 8859-15 (LATIN 9)

(define-character-encoding :iso-8859-15
  (:nicknames :iso8859-15 :latin-9 :latin9)
  (:documentation "Western European encoding with Euro-symbol."))

(define-unibyte-mapping-from-file :iso-8859-15 "unicode/8859-15.TXT")

;;;; ISO 8859-16 (LATIN 10)

(define-character-encoding :iso-8859-16
  (:nicknames :iso8859-16 :latin-10 :latin10)
  (:documentation "South-East European Encoding."))

(define-unibyte-mapping-from-file :iso-8859-16 "unicode/8859-16.TXT")
