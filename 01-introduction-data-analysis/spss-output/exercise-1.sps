* compute count of missing data.
COMPUTE miss_count=nmiss(A1 to id).
EXECUTE.

* reverse items before computing scale scores (assumes 1 to 6 scale).
DO REPEAT x = A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5 /
xReversed = A1r A2r A3r A4r A5r C1r C2r C3r C4r C5r E1r E2r E3r E4r E5r N1r N2r N3r N4r N5r O1r O2r O3r O4r O5r /
xMultiplier = -1 1 1 1 1 1 1 1 -1 -1 -1 -1 1 1 1 1 1 1 1 1 1 -1 1 1 -1 .
compute xReversed = x.
if (xMultiplier = -1) xReversed = 7 - x.
END REPEAT.
EXECUTE .

*compute scale scores.
COMPUTE agreeableness = mean.3(A1r, A2r, A3r, A4r, A5r).
COMPUTE conscientiousness = mean.3(C1r, C2r, C3r, C4r, C5r).
COMPUTE extraversion = mean.3(E1r, E2r, E3r, E4r, E5r).
COMPUTE neuroticism  = mean.3(N1r, N2r, N3r, N4r, N5r).
COMPUTE openness = mean.3(O1r, O2r, O3r, O4r, O5r).
EXECUTE.



RELIABILITY  /VARIABLES=A1r A2r A3r A4r A5r  /SCALE('ALL VARIABLES') ALL  /MODEL=ALPHA  /SUMMARY=TOTAL.
RELIABILITY  /VARIABLES=c1r c2r c3r c4r c5r  /SCALE('ALL VARIABLES') ALL  /MODEL=ALPHA  /SUMMARY=TOTAL.
RELIABILITY  /VARIABLES=e1r e2r e3r e4r e5r  /SCALE('ALL VARIABLES') ALL  /MODEL=ALPHA  /SUMMARY=TOTAL.
RELIABILITY  /VARIABLES=n1r n2r n3r n4r n5r  /SCALE('ALL VARIABLES') ALL  /MODEL=ALPHA  /SUMMARY=TOTAL.
RELIABILITY  /VARIABLES=o1r o2r o3r o4r o5r  /SCALE('ALL VARIABLES') ALL  /MODEL=ALPHA  /SUMMARY=TOTAL.


