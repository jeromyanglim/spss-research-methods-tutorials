FREQUENCIES VARIABLES=district school county grades students teachers calworks lunch computer 
    expenditure income english read math
  /ORDER=ANALYSIS.

AUTORECODE VARIABLES=grades   /INTO grade_numeric  /PRINT.

COMPUTE teachers_per_student=teachers/students.
COMPUTE computers_per_student=computer/students.
COMPUTE log_students=LG10(students).
EXECUTE.

DESCRIPTIVES VARIABLES=read math   /STATISTICS=MEAN STDDEV MIN MAX.

DESCRIPTIVES VARIABLES=read math  /SAVE  /STATISTICS=MEAN STDDEV MIN MAX.

COMPUTE academic=Zread + Zmath.
EXECUTE.

CORRELATIONS
  /VARIABLES=calworks lunch expenditure income english grade_numeric 
    teachers_per_student computers_per_student log_students academic 
  /PRINT=TWOTAIL SIG  /MISSING=PAIRWISE.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT academic
  /METHOD=ENTER calworks lunch expenditure income english grade_numeric 
    teachers_per_student computers_per_student log_students
  /PARTIALPLOT ALL
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS HISTOGRAM(ZRESID)
  /CASEWISE PLOT(ZRESID) OUTLIERS(3).


REGRESSION  /DEPENDENT math
  /METHOD=ENTER calworks lunch expenditure income english grade_numeric 
    teachers_per_student computers_per_student log_students.

REGRESSION  /DEPENDENT read
  /METHOD=ENTER calworks lunch expenditure income english grade_numeric 
    teachers_per_student computers_per_student log_students.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL ZPP  /DEPENDENT academic
  /METHOD=ENTER calworks lunch income english grade_numeric log_students
  /METHOD=ENTER teachers_per_student .

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL ZPP  /DEPENDENT academic
  /METHOD=ENTER calworks lunch income english grade_numeric log_students
  /METHOD=ENTER computers_per_student .

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL ZPP  /DEPENDENT academic
  /METHOD=ENTER calworks lunch income english grade_numeric log_students
  /METHOD=ENTER expenditure .



