/* Written by Andrew F. Hayes */.
/* School of Communication */.
/* The Ohio State University */.
/* See Preacher, KJ, & Hayes, AF (2004).  SPSS and SAS Procedures for Estimating */.
/* Indirect Effects in Simple Mediation Models, in Behavior Research Methods, Instruments */.
/* and Computers, 36, 717-731 */.
/* Version 3.6 uploaded April 13, 2011 */.
/* This version implements a new sorting routine to greatly decrease time to output */.
/* Since original publication, procedures have been added to SOBEL for estimating models with */.
/* dichotomous outcomes and the computation of five different effect sizes for indirect effects */.

DEFINE SOBEL (y = !charend('/')/x = !charend('/')/m = !charend('/')/varord = !charend('/') !default(2)/iterate = !charend('/') !default(10000)
   /converge = !charend('/') !default(0.0000001)/boot = !charend('/') !default(0)/effsize = !charend('/') !default(0)).
PRESERVE.
SET MXLOOPS = 10000001.
SET SEED = RANDOM.
SET PRINTBACK = OFF.
MATRIX.

/* READ ACTIVE SPSS DATA FILE */.
get dd/variables = !y !x !m/names = nms/MISSING = OMIT.
compute n = nrow(dd).
/* DO SOME CHECKS FOR DATA ERRORS */.
compute converrb = 0.
compute converre = 0.
compute daterr = 0.
do if (n < 5).
   compute daterr = 1.
end if.
compute ones = make(n,1,1).
compute sigma = (t(dd)*(ident(n)-(1/n)*ones*t(ones))*dd)*(1/(n-1)).
compute var = diag(sigma).
do if csum ((abs(var < 0.000000001)) > 0).
   compute daterr = 2.
end if.
do if (rank(dd(:,2:3)) < 2).
   compute daterr = 3.
end if.
compute mvals = ncol(design(dd(:,3))).
do if (mvals < 3).
   compute daterr = 4.
end if.
compute bdbp = 0.
compute corrxtt = 0.
compute sdchkt = 0.
/* START THE COMPUTATIONS */.
do if (daterr = 0).
compute mndd = csum(dd)/n.
compute std = sqrt(var).
compute vr = sqrt(var).
compute corrx = sigma&/(std*t(std)).
compute cpath = corrx(2,1)*vr(1,1)/vr(2,1).
/* DEFINE NUMBER OF BOOTSTRAP SAMPLES */.
do if (!boot > 999).
  compute btn = trunc(!boot/1000)*1000.
  compute btnp = btn+1.
  else.
  compute btn = 1000.
  compute btnp = btn+1.
end if.

compute res=make(btnp,1,0).
compute varord = !varord.
do if (varord <> 1).
   compute varord = 2.
end if.
compute ovals = ncol(design(dd(:,1))).
do if (ovals = 2).
   compute omx = cmax(dd(:,1)).
   compute omn = cmin(dd(:,1)).
   compute dd(:,1) = (dd(:,1) = omx).
   compute rcd = {omn, 0; omx, 1}.
end if.
compute dat = dd.

/* START OF THE LOOP FOR BOOTSTRAPPING */.
loop #j = 1 to btnp.
  do if (#j = 2 and !boot < 1000).
    BREAK.
  end if.
  /* DO THE RESAMPLING OF THE DATA */.
  do if (#j > 1).
     loop.
     compute v = trunc(uniform(n,1)*n)+1.
     compute dat(:,1:3)=dd(v,1:3).
     compute sig = (t(dat)*(ident(n)-(1/n)*ones*t(ones))*dat)*(1/(n-1)).
     compute vr = sqrt(diag(sig)).
     compute sdchk = (csum(vr < .00000001) > 0).
     compute corrxt = sig&/(vr*t(vr)).
     compute rsq = t(ryi)*bi.
     do if (!effsize = 1).
       compute r2my = corrxt(3,1)*corrxt(3,1).
       compute r2xy = corrxt(2,1)*corrxt(2,1).
       compute ryi = corrxt(2:3,1).
       compute rii = corrxt(2:3,2:3).
       compute bi=inv(rii)*ryi.
       compute rsq = t(ryi)*bi.
       compute r245 = r2my-(rsq-r2xy).
       compute cpath = corrxt(2,1)*vr(1,1)/vr(2,1).
     end if.
     compute corrxt = (abs(corrxt(3,2)) > .99999999).
     compute bdbp = bdbp+((sdchk = 1) or (corrxt = 1)).
     compute corrxtt = corrxtt + corrxt.
     compute sdchkt = sdchkt + sdchk.
     end loop if (corrxt = 0 and sdchk = 0).
   end if.
  /* SET UP THE DATA COLUMNS FOR PROCESSING */.
  compute y = dat(:,1).
  compute x = dat(:,2).
  compute z = dat(:,3).
  compute xz = dat(:,2:3).

  /* CALCULATE REGRESSION STATISTICS NEEDED TO COMPUTE indirect effect  */
  /* product of a and b is held as variable 'ind' */.

  compute con = make(n,1,1).
  compute xo = {con,x}.
  compute bzx = inv(t(xo)*xo)*t(xo)*z.
  compute bzx = bzx(2,1).
  compute xzo = {con,xz}.
  /* If Y is not dichotomous */.
  do if (ovals <> 2).
  compute byzx2 = inv(t(xzo)*xzo)*t(xzo)*y.
  compute byzx = byzx2(3,1).
  compute byxz = byzx2(2,1).
  compute converrb = 0.
  end if.
  /* if Y is dichotomous */.
  do if (ovals = 2).
  compute pt1 = make(n,1,0.5).
  compute bt1 = make(ncol(xzo),1,0).
  compute LL1 = 0.
  loop jjj = 1 to !iterate.
    compute vt1 = mdiag(pt1&*(1-pt1)).
    compute byzx2 = bt1+inv(t(xzo)*vt1*xzo)*t(xzo)*(y-pt1).
    compute pt1 = 1/(1+exp(-(xzo*byzx2))).
    compute LL = y&*ln(pt1)+(1-y)&*ln(1-pt1).
    compute LL2 = -2*csum(ll).
    do if (abs(LL1-LL2) < !converge).
      compute vt1 = mdiag(pt1&*(1-pt1)).
      compute byzx = byzx2(3,1).
      compute byxz = byzx2(2,1).
      compute covb = inv(t(xzo)*vt1*xzo).
      compute selgb = sqrt(diag(covb)).
      break.
    end if.
    compute bt1 = byzx2.
    compute LL1 = LL2.
  end loop.
  do if (jjj > !iterate).
      do if (#j > 1).
        compute converrb = 1.
      else if (#j = 1).
        compute converre = 1.
      end if.
      compute vt1 = mdiag(pt1&*(1-pt1)).
      compute byzx = byzx2(3,1).
      compute byxz = byzx2(2,1).
      compute covb = inv(t(xzo)*vt1*xzo).
      compute selgb = sqrt(diag(covb)).
  end if.
  end if.
  compute ind = bzx*byzx.
  compute res(#j,1) = ind.
 
  /* GENERATE STATISTICS FOR BARON AND KENNY AND NORMAL SOBEL SECTION OF OUTPUT */.
  do if (#j = 1).
    compute sd = sqrt(((n*cssq(dat))-(csum(dat)&**2))/((n-1)*n)).
    compute num = (n*sscp(dat)-(transpos(csum(dat))*(csum(dat)))).
    compute den = sqrt(transpos((n*cssq(dat))-(csum(dat)&**2))*((n*cssq(dat))-
    (csum(dat)&**2))).
    compute r = num&/den.
    compute sdbzx = (sd(1,3)/sd(1,2))*sqrt((1-(r(3,2)*r(3,2)))/(n-2)).
    compute ryi = r(2:3,1).
    compute rii = r(2:3,2:3).
    compute bi=inv(rii)*ryi.
    compute rsq = t(ryi)*bi.
    compute sec=sqrt((1-rsq)/(n-3))*sqrt(1/(1-(r(3,2)*r(3,2)))).
    compute sdyzx = (sd(1,1)/sd(1,3))*sec.
    compute sdyxz = (sd(1,1)/sd(1,2))*sec.
    compute byx = r(2,1)*sd(1,1)/sd(1,2).
    compute sebyx = (sd(1,1)/sd(1,2))*sqrt((1-(r(2,1)*r(2,1)))/(n-2)).
    compute amx = mndd(1,3)-(bzx*mndd(1,2)).
    compute msres = (z-(amx+bzx*x))&*(z-(amx+bzx*x)).
    compute msres = csum(msres)/(n-2).
    do if (!effsize = 1).
    compute eff = make(btnp,5,-999).
    compute r2my = r(3,1)*r(3,1).
    compute r2xy = r(2,1)*r(2,1).
    compute r245 = r2my-(rsq-r2xy).
    compute r245o = r245.
    compute pm = ind/byx.
    compute rm = ind/byxz.
    compute abps = ind/sd(1,1). 
    compute abcs = abps*sd(1,2).
    end if.
    do if (ovals = 2).
      compute sdyzx = selgb(3,1).
      compute sdyxz = selgb(2,1).
      compute xo = {con,x}.
      compute pt1 = make(n,1,0.5).
      compute bt1 = make(ncol(xo),1,0).
      compute ll1 = 0.
      loop jjj = 1 to !iterate.
        compute vt1 = mdiag(pt1&*(1-pt1)).
        compute byx = bt1+inv(t(xo)*vt1*xo)*t(xo)*(y-pt1).
        compute pt1 = 1/(1+exp(-(xo*byx))).
        compute LL = y&*ln(pt1)+(1-y)&*ln(1-pt1).
        compute LL2 = -2*csum(ll).
        do if (abs(LL1-LL2) < !converge).
          compute vt1 = mdiag(pt1&*(1-pt1)).
          compute byx = byx(2,1).
          compute covb = inv(t(xo)*vt1*xo).
          compute sebyx = sqrt(diag(covb)).
          compute sebyx = sebyx(2,1).
          break.
        end if.
        compute bt1 = byx.
        compute LL1 = LL2.
      end loop.
      do if (jjj > !iterate).
        compute converre = 1.
        compute vt1 = mdiag(pt1&*(1-pt1)).
        compute byx = byx(2,1).
        compute covb = inv(t(xo)*vt1*xo).
        compute sebyx = sqrt(diag(covb)).
        compute sebyx = sebyx(2,1).
      end if.
    end if.
    compute seind = sqrt(((byzx*byzx)*(sdbzx*sdbzx))+((bzx*bzx)*(sdyzx*sdyzx))+
    ((sdbzx*sdbzx)*(sdyzx*sdyzx))).
    do if (varord = 1).
    compute seind = sqrt(((byzx*byzx)*(sdbzx*sdbzx))+((bzx*bzx)*(sdyzx*sdyzx))).
    end if.
    compute se = {sebyx; sdbzx; sdyzx; sdyxz}.
    compute bb = {byx; bzx; byzx; byxz}.
    compute tt = bb&/se.
    compute p =2*(1-tcdf(abs(tt),n-2)).
    compute p(3,1)=2*(1-tcdf(abs(tt(3,1)),n-3)).
    compute p(4,1)=2*(1-tcdf(abs(tt(4,1)),n-3)).
    compute tst = ind/seind.
    compute bw = {bb,se,tt,p}.
    compute p2=2*(1-cdfnorm(abs(tst))).
    compute LL95 = ind-1.96*seind.
    compute UL95=ind+1.96*seind.
    compute byxstd = byx*sqrt(1+(((byzx*byzx)*msres)/((3.14159265*3.14159265)/3))).
    compute op={ind, seind, LL95,UL95, tst, p2}.
  end if. 
  do if (!effsize = 1 and !boot > 999 and ovals <> 2).
  compute eff(#j,3) = r245.
  compute eff(#j,2) = ind/byxz.
  compute eff(#j,1) = ind/cpath.
  compute eff(#j,4) = ind/vr(1,1).
  compute eff(#j,5) = ind*vr(2,1)/vr(1,1).
  end if.
end loop.
/* END OF BOOTSTRAPPING LOOP */.


do if (!boot > 999).
compute res10 = res((2:nrow(res)),1). 
save res10/outfile = bootstrp.sav/variables = bootstrp.
end if.

/* COMPUTE MEAN AND STANDARD DEV OF INDIRECT EFFECT ACROSS BOOTSTRAP SAMPLES */.

compute res = res(2:btnp,1).
do if (ovals <> 2 and !effsize = 1 and !boot > 999).
  compute res={res, eff(2:btnp, :)}.
end if.
compute mnbt = csum(res)/btn.
compute se = (sqrt(((btn*cssq(res))-(csum(res)&**2))/((btn-1)*btn))).


/* SORT THE BOOTSTRAP ESTIMATES */.
do if (!boot > 999).
loop #j = 1 to ncol(res).
compute res2 = res(:,#j).
compute res2(GRADE(res(:,#j))) = res(:,#j).
compute res(:,#j) = res2.
end loop.

/* GENERATE BOOTSTRAP CONFIDENCE INTERVAL FOR INDIRECT EFFECT */.
compute lower99 = res(.005*btn,1).
compute lower95 = res(.025*btn,1).
compute upper95 = res(1+.975*btn,1).
compute upper99 = res(1+.995*btn,1).
compute bt = {op(1,1),mnbt(1,1), se(1,1), lower99, lower95, upper95, upper99}.
end if.

/* GENERATE OUTPUT */.
print/title = "*************************************************************************".
print/title = "Preacher and Hayes (2004) SPSS Macro for Simple Mediation".
print/title = "Written by Andrew F. Hayes, The Ohio State University".
print/title = "http://www.comm.ohio-state.edu/ahayes/".
print/title = "For details, see Preacher, K. J., & Hayes, A. F. (2004). SPSS and SAS".
print/title = "procedures for estimating indirect effects in simple mediation models".
print/title = "Behavior Research Methods, Instruments, and Computers, 36, 717-731.".
compute temp = {"Y"; "X"; "M"}.
compute temp = {temp, t(nms)}.
print temp/title = "VARIABLES IN SIMPLE MEDIATION MODEL"/format = A8.
compute corrx = {t(mndd), std, corrx}.
compute nmsc = {"Mean", "SD", nms}.
print corrx/title = "DESCRIPTIVES STATISTICS AND PEARSON CORRELATIONS"/rnames = nms/cnames = nmsc/format = F9.4.
print n/title = "SAMPLE SIZE"/format F8.0.
do if (converre = 0).
print bw/title = "DIRECT AND TOTAL EFFECTS"/clabels = "Coeff" "s.e." "t " 
   "Sig(two)"/rlabels = "b(YX)" "b(MX)" "b(YM.X)" "b(YX.M)"/format f9.4.
do if (ovals = 2).
print byxstd/title = "TOTAL EFFECT, b(YX), STANDARDIZED TO THE METRIC OF THE DIRECT EFFECT, b(YX.M)"/
   clabels = "Coeff"/rlabels = "b(YX)"/format = F9.4.
end if.
print op/title = "INDIRECT EFFECT AND SIGNIFICANCE USING NORMAL DISTRIBUTION"/rlabels 
   = "Effect"/clabels = "Value" "s.e." "LL95CI" "UL95CI" "Z" "Sig(two)"/format f9.4.
do if (!boot > 999 and converrb = 0).
  print bt/title = "BOOTSTRAP RESULTS FOR INDIRECT EFFECT"/rlabels ="Effect"/clabels 
  "Data" "Mean" "s.e." "LL99 CI" "LL95CI" "UL95CI" "UL99CI"/format f9.4.
  print btn/title = "NUMBER OF BOOTSTRAP RESAMPLES"/format F8.0.
end if.
do if (ovals <> 2 and !effsize = 1).
compute effsz = {op(1,1); pm; rm; r245o; abps; abcs}.
do if (!boot > 999).
compute effsz = {effsz, t(mnbt), t(se)}.
compute cimat = make(6,4,-999).
loop #j = 1 to 6.
compute cimat(#j,1) = res((.005*btn),#j).
compute cimat(#j,2) = res((.025*btn),#j).
compute cimat(#j,3) = res((1+.975*btn),#j).
compute cimat(#j,4) = res((1+.995*btn),#j).
end loop.
compute effsz = {effsz, cimat}.
end if.
print effsz/title = "POINT AND INTERVAL ESTIMATES OF EFFECT SIZE FOR INDIRECT EFFECT"/rlabels = "ab" "P_m" "R_m" "R2_45" "ab_ps" "ab_cs"
  /clabels = "Data" "Mean" "s.e." "LL99CI" "LL95CI" "UL95CI" "UL99CI"/format F9.4.
end if.
end if.
print/title = "********************************* NOTES **********************************".
do if (ovals = 2 and converre = 0).
print/title = "Model coefficients involving the binary outcome are logistic regression coefficients.".
compute nm = {nms(1,1), "Analysis"}.
print rcd/title = "Coding of binary Y for analysis:"/cnames = nm/format = F9.2.
do if (ovals = 2 and !effsize = 1).
  print /title = "Effect size estimates not available for models with dichotomous outcomes.".
end if.
end if.
do if (converre = 1).
print/title = "Convergence error in estimation of binary logistic model.  Try adjusting iteration criteria.".
end if.
do if (converrb = 1 and converre <> 1 and !boot > 0).
print/title = "At least one convergence failure while bootstrapping, so bootstrap output is suppressed.".
end if.
do if (bdbp > 0).
print bdbp/title = "Bootstrap samples replaced due to singularity or constants after resampling:".
end if.
end if.
do if (daterr = 1).
print/title = "ERROR: Insufficient data for analysis. There are too few cases in your data file.".
else if (daterr = 2).
print/title = "ERROR: One of the variables in the model is constant.".
else if (daterr = 3).
print/title = "ERROR: M is a perfect function of X".
else if (daterr = 4).
print/title = "ERROR: There is no variance in M, or M is dichotomous".
end if.
do if (!boot < 1000 and daterr = 0).
print/title = "Bootstrap confidence intervals are preferred to the Sobel test for inference about indirect effects.".
print/title = "See Hayes, A.F.(2009). Beyond Baron and Kenny: Statistical mediation analysis in the new millennium.".
print/title = "Communication Monographs, 76, 408-420.".
end if.
END MATRIX.
RESTORE.
!ENDDEFINE.


