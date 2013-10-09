* Make this a unique dataset based on student number.
DESCRIPTIVES    id. 

* Replace number below with student number or first 10 numbers of student number.
SET SEED=1234567891.
FILTER OFF.
USE ALL. 
SAMPLE .90. 
EXECUTE.

descriptives id.
* save as "bfi-unique.sav".


* Clean data.
COMPUTE missing_count=NMISS(A1 to O5).
EXECUTE.

freq missing_count.

* remove cases with many missing items.
FILTER OFF.
USE ALL.
SELECT IF (missing_count < 3).
EXECUTE.

freq missing_count.

* save as "bfi-unique-2.sav".

* problematic cases based on mahalanobis.
REGRESSION
  /MISSING MEANSUBSTITUTION
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT id
  /METHOD=ENTER A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5
  /SAVE MAHAL(mahal_items).

FREQUENCIES VARIABLES=mahal_items  /HISTOGRAM  /ORDER=ANALYSIS.

USE ALL.
COMPUTE filter_$=(mahal_items > 70 ).
VARIABLE LABELS filter_$ 'mahal_items > 70  (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

SUMMARIZE
  /TABLES= A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5
  /FORMAT=VALIDLIST NOCASENUM TOTAL LIMIT=100
  /TITLE='Case Summaries'
  /MISSING=VARIABLE
  /CELLS=NONE.

USE ALL.

* Check that raw data ranges from 1 to 6.
DESCRIPTIVES VARIABLES=A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5
  /STATISTICS=MIN MAX.

* to run this on other computers adjust the path.
MVA VARIABLES=A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5 gender 
    education age missing_count mahal_items filter_$ 
  /ID=id
  /EM A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5 WITH A1 A2 A3 A4 
    A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5(TOLERANCE=0.001 CONVERGENCE=0.0001 
    ITERATIONS=25 OUTFILE='/Users/jeromy/teaching/org-research-methods/2013/content/05 Cluster and '+
    'factor analysis/exercises/data/bfi-unique-3.sav').

* switch to bfi-unique-3.sav.

* round items to nearest integer.
DO REPEAT R=A1 to O5.
COMPUTE R = rnd(R).
END REPEAT.
EXECUTE.

* check that rounding worked.
freq A1.


* split data files into 2. 
USE ALL.
do if $casenum=1.
compute #s_$_1=1236.
compute #s_$_2=2472.
end if.
do if  #s_$_2 > 0.
compute filter_$=uniform(1)* #s_$_2 < #s_$_1.
compute #s_$_1=#s_$_1 - filter_$.
compute #s_$_2=#s_$_2 - 1.
else.
compute filter_$=0.
end if.
VARIABLE LABELS filter_$ '1236 from the first 2472 cases (SAMPLE)'.
FORMATS filter_$ (f1.0).Î
FILTER  BY filter_$.
EXECUTE.

* renamed filter_$ into dataset and saved two new versions with just exploratory or confirmatory data.
* I coded 0 = exploratory and 1 = confirmatory and run the code below on the two new datsets.
FILTER OFF.
USE ALL.
SELECT IF (dataset=1).
EXECUTE.

FILTER OFF.
USE ALL.
SELECT IF (dataset=0).
EXECUTE.



* check the means of items.
descriptives A1 to O5 /statistics = mean.


* exploratory factor analysis.

* examine scree plot.
FACTOR
  /VARIABLES A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5
  /MISSING LISTWISE 
  /ANALYSIS A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5
  /PRINT INITIAL EXTRACTION
  /PLOT EIGEN
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /ROTATION NOROTATE
  /METHOD=CORRELATION.

* 5 factor solution.
FACTOR  /VARIABLES A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5
  /PRINT INITIAL EXTRACTION ROTATION  /FORMAT SORT BLANK(.25)
  /PLOT EIGEN   /CRITERIA FACTORS(5) ITERATE(25)   /EXTRACTION ML  /CRITERIA ITERATE(25)  /ROTATION PROMAX(4).

* 4 factor solution.
FACTOR  /VARIABLES A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5
  /PRINT INITIAL EXTRACTION ROTATION  /FORMAT SORT BLANK(.25)
  /PLOT EIGEN   /CRITERIA FACTORS(4) ITERATE(25)   /EXTRACTION ML  /CRITERIA ITERATE(25)  /ROTATION PROMAX(4).

* 6 factor solution.
FACTOR  /VARIABLES A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5
  /PRINT INITIAL EXTRACTION ROTATION  /FORMAT SORT BLANK(.25)
  /PLOT EIGEN   /CRITERIA FACTORS(6) ITERATE(25)   /EXTRACTION ML  /CRITERIA ITERATE(25)  /ROTATION PROMAX(4).


* 5 factor solution without E3 or O4.
FACTOR  /VARIABLES A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O5
  /PRINT INITIAL EXTRACTION ROTATION  /FORMAT SORT BLANK(.25)
  /PLOT EIGEN   /CRITERIA FACTORS(5) ITERATE(25)   /EXTRACTION ML  /CRITERIA ITERATE(25)  /ROTATION PROMAX(4).


* report descriptives.
DESCRIPTIVES VARIABLES=A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5
  /STATISTICS=MEAN STDDEV.

* score tests.
* create copy of variables where items are reversed if necessary.
DO REPEAT x = A1  A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5 /
     xReversed = A1r  A2r  A3r  A4r  A5r  C1r  C2r  C3r  C4r  C5r  E1r  E2r  E3r  E4r  E5r  N1r  N2r  N3r  N4r  N5r  O1r  O2r  O3r  O4r  O5r   /
  xMultiplier =-1  1  1  1  1  1  1  1  -1  -1  -1  -1  1  1  1  1  1  1  1  1  1  -1  1  1  -1.
compute xReversed = x.
if (xMultiplier = -1) xReversed = 6 - x.
END REPEAT.
EXECUTE .

* check that reversal worked.
* a1r should be reversed; a2r not reversed.
crosstabs a1 by a1r.
crosstabs a2 by a2r.

compute neuroticism= mean(n1r, n2r, n3r, n4r, n5r).
compute extraversion = mean(e1r, e2r, e4r, e5r).
compute openness = mean(o1r, o2r, o3r, o5r).
compute agreeableness = mean(a1r, a2r, a3r, a4r, a5r).
compute conscientiousness = mean(c1r, c2r, c3r, c4r, c5r).
execute.


* basic reporting.
descriptives neuroticism extraversion openness agreeableness conscientiousness.

RELIABILITY  /VARIABLES=n1r n2r n3r n4r n5r /MODEL=ALPHA  /SUMMARY=TOTAL.
RELIABILITY  /VARIABLES=e1r e2r e4r e5r /MODEL=ALPHA  /SUMMARY=TOTAL.
RELIABILITY  /VARIABLES=o1r o2r o3r o5r/MODEL=ALPHA  /SUMMARY=TOTAL.
RELIABILITY  /VARIABLES=A1r A2r A3r A4r A5r  /MODEL=ALPHA  /SUMMARY=TOTAL.
RELIABILITY  /VARIABLES=c1r c2r c3r c4r c5r  /MODEL=ALPHA  /SUMMARY=TOTAL.

CORRELATIONS  /VARIABLES=neuroticism extraversion openness agreeableness conscientiousness
  /PRINT=TWOTAIL SIG   /MISSING=PAIRWISE.


* gender.
T-TEST GROUPS=gender(1 2)
  /MISSING=ANALYSIS  /VARIABLES=neuroticism extraversion openness agreeableness conscientiousness
  /CRITERIA=CI(.95).

* education.
ONEWAY neuroticism extraversion openness agreeableness conscientiousness BY education
  /POLYNOMIAL=2
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS.

* age.


GRAPH
  /SCATTERPLOT(MATRIX)=age neuroticism extraversion openness agreeableness conscientiousness
  /MISSING=LISTWISE.

CORRELATIONS   neuroticism extraversion openness agreeableness conscientiousness with age.
