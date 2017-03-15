perl filterExpr.pl Normalization.BRCA.txt 1 10 > Normalization.BRCA.minAveExpr1.maxExpr10.txt
perl filterExpr.pl Normalization.BRCA.txt 10 50 > Normalization.BRCA.minAveExpr10.maxExpr50.txt


# Final Commands to run and use
grep -v NONHSAG Normalization.BRCA.txt > Normalization.BRCA.protCoding.txt
perl filterExpr.pl Normalization.BRCA.protCoding.txt 10 50 > Normalization.BRCA.protCoding.minAveExpr10.maxExpr50.txt
