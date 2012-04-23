#!/bin/sh

rm 8859-*.TXT

# Yes, there's no 12.
for n in 1 2 3 4 5 6 7 8 9 10 11 13 14 15 16
do
    wget ftp://ftp.unicode.org/Public/MAPPINGS/ISO8859/8859-$n.TXT
done
