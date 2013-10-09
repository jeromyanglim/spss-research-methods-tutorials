FREQUENCIES VARIABLES=gender ethnicity score fcollege mcollege home urban unemp wage distance 
    tuition education income region
  /HISTOGRAM
  /ORDER=ANALYSIS.

AUTORECODE VARIABLES=gender ethnicity fcollege mcollege home urban income region 
  /INTO gender_numeric ethnicity_numeric fcollege_numeric mcollege_numeric home_numeric 
    urban_numeric income_numeric region_numeric
  /PRINT.


compute parent_education = fcollege_numeric + mcollege_numeric - 2.
execute.

freq parent_education.


SOBEL y=education /x=parent_education /m=score /boot=100.

DESCRIPTIVES VARIABLES=parent_education education score
  /SAVE
  /STATISTICS=MEAN STDDEV MIN MAX.

SOBEL y=zeducation /x=zparent_education /m=zscore /boot=100.

RECODE gender fcollege mcollege home urban region income 
  ('female'=0) ('male'=1) 
  ('low'=0) ('high'=1) 
  ('other'=0) ('west'=1) 
  ('no'=0) ('yes'=1) INTO male fcollege_yes 
    mcollege_yes home_owns urban_yes region_west income_high.
EXECUTE.


compute ethnicity_afam = (ethnicity = 'afam').
compute ethnicity_hispanic = (ethnicity = 'hispanic').
execute.

crosstabs ethnicity_afam by ethnicity.
crosstabs ethnicity_hispanic by ethnicity.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT education
  /METHOD=ENTER score unemp wage distance tuition male fcollege_yes mcollege_yes home_owns 
    urban_yes region_west income_high ethnicity_afam ethnicity_hispanic.


compute fcollege_yesBYmcollege_yes = fcollege_yes * mcollege_yes.
compute maleBYethnicity_afam = male * ethnicity_afam.
compute maleBYethnicity_hispanic =  male * ethnicity_hispanic.
compute scoreBYmale = score * male.
compute scoreBYethnicity_afam = score * ethnicity_afam.
compute scoreBYethnicity_hispanic =  score * ethnicity_hispanic.
compute scoreBYtuition =  score * tuition.
execute.


REGRESSION    /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
/DEPENDENT education  
   /METHOD=ENTER score unemp wage distance tuition male fcollege_yes mcollege_yes home_owns 
    urban_yes region_west income_high ethnicity_afam ethnicity_hispanic
  /METHOD=ENTER fcollege_yesBYmcollege_yes .


REGRESSION    /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
/DEPENDENT education  
   /METHOD=ENTER score unemp wage distance tuition male fcollege_yes mcollege_yes home_owns 
    urban_yes region_west income_high ethnicity_afam ethnicity_hispanic
  /METHOD=ENTER scoreBYmale.

GRAPH
  /SCATTERPLOT(BIVAR)=score WITH education BY gender
  /MISSING=LISTWISE.

REGRESSION    /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
/DEPENDENT education  
   /METHOD=ENTER score unemp wage distance tuition male fcollege_yes mcollege_yes home_owns 
    urban_yes region_west income_high ethnicity_afam ethnicity_hispanic
  /METHOD=ENTER maleBYethnicity_afam maleBYethnicity_hispanic .

REGRESSION    /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
/DEPENDENT education  
   /METHOD=ENTER score unemp wage distance tuition male fcollege_yes mcollege_yes home_owns 
    urban_yes region_west income_high ethnicity_afam ethnicity_hispanic
  /METHOD=ENTER scoreBYethnicity_afam scoreBYethnicity_hispanic .


REGRESSION    /STATISTICS COEFF OUTS R ANOVA COLLIN TOL CHANGE ZPP
/DEPENDENT education  
   /METHOD=ENTER score unemp wage distance tuition male fcollege_yes mcollege_yes home_owns 
    urban_yes region_west income_high ethnicity_afam ethnicity_hispanic
  /METHOD=ENTER scoreBYtuition.










