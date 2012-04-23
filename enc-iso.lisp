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

;;;; ISO 8859-1 (LATIN 1)

(define-unibyte-character-encoding :iso-8859-1
    (:iso8859-1 :latin-1 :latin1 :l1 :iso-ir-100 :csisolatin1 :ibm819 :cp819)
  "Western European encoding. Maps character codes 0-255 directly to
corresponding octets."
  "unicode/8859-1.TXT")

;;;; ISO 8859-2 (LATIN 2)

(define-unibyte-character-encoding :iso-8859-2
    (:iso8859-2 :latin-2 :latin2 :l2 :iso-ir-101 :csisolatin2 :ibm912 :cp912)
  "Eastern European encoding. NOTE: Very different from :CP852 (MS-DOS Latin
2, PC Latin 2), which is also referred to as `Latin 2' in Czech and Slovak
regions."
  "unicode/8859-2.TXT")

;;;; ISO 8859-3 (LATIN 3)

(define-unibyte-character-encoding :iso-8859-3
    (:iso8859-3 :latin-3 :latin3 :l3 :iso-ir-109 :csisolatin3)
  "South European encoding."
  "unicode/8859-3.TXT")

;;;; ISO 8859-4 (LATIN 4)

(define-unibyte-character-encoding :iso-8859-4
    (:iso8859-4 :latin-4 :latin4 :l4 :csisolatin4)
  "North European encoding."
  "unicode/8859-4.TXT")

;;;; ISO 8859-5 (LATIN/Cyrillic)

(define-unibyte-character-encoding :iso-8859-5
    (:iso8859-5 :latin/cyrillic)
  "Cyrillic encoding."
  "unicode/8859-5.TXT")

;;;; ISO 8859-6

(define-unibyte-character-encoding :iso-8859-6
    (:iso8859-6 :latin/arabic)
  "Arabic encoding."
  "unicode/8859-6.TXT")

;;;; ISO 8859-7 (LATIN/GREEK)

(define-unibyte-character-encoding :iso-8859-7
    (:iso8859-7 :latin/greek)
  "Modern Greek encoding. 2003 version with Euro, Drachma, and Greek
Ypogegrammeni symbols."
  "unicode/8859-7.TXT")

;;;; ISO 8859-8 (LATIN/HEBREW)

(define-unibyte-character-encoding :iso-8859-8
    (:iso8859-8 :latin/hebrew)
  "Hebrew encoding."
  "unicode/8859-8.TXT")

;;;; ISO 8859-9 (LATIN 5)

(define-unibyte-character-encoding :iso-8859-9
    (:iso8859-9 :latin-5 :latin5)
  "Turkish encoding."
  "unicode/8859-9.TXT")

;;;; ISO 8859-10 (LATIN 6)

(define-unibyte-character-encoding :iso-8859-10
    (:iso8859-10 :latin-6 :latin6)
  "Nordic language encoding."
  "unicode/8859-10.TXT")

;;;; ISO 8859-11 (LATIN/THAI)

(define-unibyte-character-encoding :iso-8859-11
    (:iso8859-11 :latin/thai)
  "Thai encoding."
  "unicode/8859-11.TXT")

;;;; ISO 8859-12 was never defined.

;;;; ISO 8859-13 (LATIN 7)

(define-unibyte-character-encoding :iso-8859-13
  (:iso8859-13 :latin-7 :latin7)
  "Baltic language encoding."
  "unicode/8859-13.TXT")

;;;; ISO 8859-14 (LATIN 8)

(define-unibyte-character-encoding :iso-8859-14
    (:iso8859-14 :latin-8 :latin8)
  "Celtic language encoding."
  "unicode/8859-14.TXT")

;;;; ISO 8859-15 (LATIN 9)

(define-unibyte-character-encoding :iso-8859-15
    (:iso8859-15 :latin-9 :latin9)
  "Western European encoding with Euro-symbol."
  "unicode/8859-15.TXT")

;;;; ISO 8859-16 (LATIN 10)

(define-unibyte-character-encoding :iso-8859-16
  (:iso8859-16 :latin-10 :latin10)
  "South-East European Encoding."
  "unicode/8859-16.TXT")
