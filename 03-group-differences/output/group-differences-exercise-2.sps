

FREQUENCIES VARIABLES=Salary FTE Rank Articles Experience Sex
  /STATISTICS=STDDEV MEAN
  /HISTOGRAM
  /ORDER=ANALYSIS.

CORRELATIONS
  /VARIABLES=Salary FTE Rank Articles Experience Sex
  /PRINT=TWOTAIL SIG
  /MISSING=PAIRWISE.

T-TEST GROUPS=Sex(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=Salary
  /CRITERIA=CI(.95).

MEANS TABLES=Salary BY FTE
  /CELLS MEAN COUNT STDDEV.

GRAPH
  /SCATTERPLOT(MATRIX)=Salary Articles Experience
  /MISSING=LISTWISE.

UNIANOVA Salary BY Sex WITH Articles Experience FTE
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(Sex) WITH(Articles=MEAN Experience=MEAN FTE=MEAN) 
  /CRITERIA=ALPHA(.05)
  /DESIGN=Articles Experience FTE Sex.

USE ALL.
COMPUTE filter_$=(Rank ~= 1).
VARIABLE LABELS filter_$ 'Rank ~= 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

UNIANOVA Salary BY Sex WITH Articles Experience FTE
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(Sex) WITH(Articles=MEAN Experience=MEAN FTE=MEAN) 
  /CRITERIA=ALPHA(.05)
  /DESIGN=Articles Experience FTE Sex.

COMPUTE articles_per_year=Articles/Experience.
EXECUTE.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Salary
  /METHOD=ENTER articles_per_year Experience.

COMPUTE merit_salary=40885 + 1160 * Experience + 3643 * articles_per_year.
EXECUTE.

GRAPH
  /SCATTERPLOT(BIVAR)=merit_salary WITH Salary BY sex
  /MISSING=LISTWISE.

UNIANOVA Salary BY Sex WITH merit_salary
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(Sex) WITH(merit_salary=MEAN) 
  /CRITERIA=ALPHA(.05)
  /DESIGN=Sex*merit_salary Sex merit_salary.
