%let outpath = /home/u63677168/scs23024;

/* Read in the Data */
PROC IMPORT DATAFILE="&outpath/BD.csv" 
DBMS=CSV OUT=bd REPLACE; 
RUN; 

***Full covariance structure;
proc glimmix plots=all;
 class participant side task;
 y = log(response);
 model y = side|task / ddfm=kr;
 random intercept task / subject=participant;
 random side / subject=participant solution;
 output out=a1 resid=res;
run;

***Reduced Covariance structure;
proc glimmix plots=all;
 class participant side task;
 y = log(response);
 model y = side|task / ddfm=kr;
 random intercept task / subject=participant;
run;


***Task comparisons;
proc glimmix;
 class participant side task;
 y = log(response);
 model y = side|task / ddfm=kr;
 random intercept task / subject=participant;
* random side / subject=participant;
 lsmeans task / adjust=tukey;
 lsmestimate task '12 vs 3' -0.5 -0.5 1 0;
 lsmestimate task '12 vs 4' -0.5 -0.5 0 1;
run;


***Full covariance structure;
proc glimmix plots=all;
 class participant side task;
 y = log(response);
 model y = task / ddfm=kr;
 random intercept task / subject=participant;
* random side(participant);
 lsmeans task / adjust=tukey;
 lsmestimate task '12 vs 3' -0.5 -0.5 1 0;
 lsmestimate task '12 vs 4' -0.5 -0.5 0 1;
run;

