* Initial frequency analysis.
FREQUENCIES VARIABLES=affairs gender age yearsmarried children religiousness education occupation 
    rating  /HISTOGRAM   /ORDER=ANALYSIS.

* create new variables.
RECODE affairs (0=0) (1 thru Highest=1) INTO affairs_yes.
EXECUTE.
CROSSTABS  /TABLES=affairs BY affairs_yes.

RECODE gender ('female'=0) ('male'=1) INTO gender_male.
EXECUTE.
CROSSTABS  /TABLES=gender BY gender_male.


RECODE children ('yes'=1) ('no'=0) INTO children_yes.
EXECUTE.
CROSSTABS  /TABLES=children BY children_yes.


CORRELATIONS
  /VARIABLES=age yearsmarried religiousness education occupation rating affairs_yes 
    gender_male children_yes
  /PRINT=TWOTAIL SIG
  /MISSING=PAIRWISE.

LOGISTIC REGRESSION VARIABLES affairs_yes
  /METHOD=ENTER age yearsmarried religiousness education occupation rating gender_male children_yes     
  /CLASSPLOT
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.75).
