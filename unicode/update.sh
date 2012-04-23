#!/bin/sh

rm -f 8859-*.TXT

# Yes, there's no 12.
for n in 1 2 3 4 5 6 7 8 9 10 11 13 14 15 16
do
    wget ftp://ftp.unicode.org/Public/MAPPINGS/ISO8859/8859-$n.TXT
done

rm -f CP*.TXT

for p in 437 737 775 850 852 855 857 860 861 862 863 864 865 866 869 874
do
    wget ftp://ftp.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP$p.TXT
done
