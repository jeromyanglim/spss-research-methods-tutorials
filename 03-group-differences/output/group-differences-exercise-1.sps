AUTORECODE VARIABLES=sex sport 
  /INTO sex_numeric sport_numeric
  /PRINT.

RECODE sport ('Swim'=1) ('T_400m'=2) ('B_Ball'=3) (ELSE=SYSMIS) INTO sport_abbreviated.
EXECUTE.

freq all /histogram.

CROSSTABS
  /TABLES=sport_numeric BY sex_numeric
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

PROXIMITIES rcc wcc hc hg ferr bmi ssf pcBfat lbm ht wt sex_numeric
  /VIEW=VARIABLE
  /MEASURE=CORRELATION
  /STANDARDIZE=NONE.

T-TEST GROUPS=sex_numeric(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=ht
  /CRITERIA=CI(.95).

UNIANOVA ht BY sex_numeric
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /CRITERIA=ALPHA(0.05)
  /DESIGN=sex_numeric.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT ht
  /METHOD=ENTER sex_numeric.

UNIANOVA ht BY sex_numeric
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /SAVE=RESID
  /PRINT=HOMOGENEITY DESCRIPTIVE
  /PLOT=RESIDUALS
  /CRITERIA=ALPHA(.05)
  /DESIGN=sex_numeric.

FREQUENCIES VARIABLES=RES_2
/histogram.

UNIANOVA bmi BY sport_numeric
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /POSTHOC=sport_numeric(TUKEY) 
  /PRINT=HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=sport_numeric.

UNIANOVA ht BY sport_abbreviated sex_numeric
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /SAVE=RESID
  /PLOT=PROFILE(sport_abbreviated*sex_numeric)
  /PRINT=HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=sport_abbreviated sex_numeric sport_abbreviated*sex_numeric.

UNIANOVA ht BY sport_abbreviated sex_numeric
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /SAVE=RESID
  /POSTHOC=sport_abbreviated(TUKEY) 
  /PLOT=PROFILE(sex_numeric*sport_abbreviated)
  /PRINT=HOMOGENEITY DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=sport_abbreviated sex_numeric sport_abbreviated*sex_numeric.


PROXIMITIES rcc wcc hc hg ferr 
  /VIEW=VARIABLE
  /MEASURE=CORRELATION
  /STANDARDIZE=NONE.

GLM rcc wcc hc hg ferr BY sex_numeric
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN= sex_numeric.

DISCRIMINANT
  /GROUPS=sex_numeric(1 2)
  /VARIABLES=rcc wcc hc hg ferr
  /ANALYSIS ALL
  /PRIORS EQUAL 
  /CLASSIFY=NONMISSING POOLED.

DESCRIPTIVES VARIABLES=rcc wcc hc hg ferr
  /SAVE
  /STATISTICS=MEAN STDDEV MIN MAX.

COMPUTE mean_blood=mean(Zrcc, Zwcc, Zhc, Zhg, Zferr).
EXECUTE.

UNIANOVA mean_blood BY sex_numeric
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /CRITERIA=ALPHA(0.05)
  /DESIGN=sex_numeric.
