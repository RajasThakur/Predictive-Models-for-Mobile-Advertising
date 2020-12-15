ODS HTML;
ODS LISTING CLOSE;
ODS GRAPHICS ON;

LIBNAME Data 'H:\Predictive Analytics\Project';

/* Understanding the data */
/* Reading the dataset and storing it in games */
data games;
 set Data.DATA;
run;

/* Part I  a)  */
/* Creating a linear probability model */

/* We will convert device_platform_class into a binary 0/1 variable with its value being 1 if the device used is iOS and 0 if android */
/* Also, we will dropping the variable device_platform_class */
data games;
 set Data.DATA;
 if device_platform_class = 'iOS'
 then device_platform = 1;
 if device_platform_class = 'android'
 then device_platform = 0;
 drop device_platform_class;
run;

/* Creating train and test datasets from our original dataset with 70% of samples in training and remaining in test */
proc surveyselect data=games out=games_samples outall samprate=0.7 seed=10;
run;

data games_train games_test;
 set games_samples;
 if selected then output games_train; 
 else output games_test;
run;

/* Checking descriptive statistics about the data */

proc means data = games;
 var wifi device_volume device_height device_width;
 run;

proc univariate data = games;
 var wifi device_volume device_height device_width;
 run;

proc means data = games;
 class device_os_class device_make_class device_platform publisher_id_class;
 run;

/* Checking box plots of the continous variables */
proc sgplot data=games;
  vbox wifi;
run;

proc sgplot data=games;
  vbox device_volume;
run;

proc sgplot data=games;
  vbox device_height;
run;

proc sgplot data=games;
  vbox device_width;
run;

/* Checking unique values per category */
ods select nlevels;

proc freq data=games nlevels;
   tables device_os_class device_make_class device_platform publisher_id_class;
title 'Number of distinct values for each variable'; 
run; 

/* Plotting histogram of Y-variable to check whether this is a balanced dataset or no */
proc sgplot data = games;
	histogram install;
	run;
proc freq data=games nlevels;
   tables install;
run;

/* We will first plot a correlation matrix to check how the independent variables affect our Y variable */
proc corr data = games plots=matrix;
 var install wifi device_volume device_height device_width device_os_class device_make_class device_platform publisher_id_class resolution;
run;


/* ---------------------------------------------------------------------------------------------   */
/* MODEL BUILDING */

/* We will approach model buidling with first approach as Traditional Forward Selection */
proc glmselect data=games plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) wifi(split);
 model install = wifi device_volume device_height device_width device_os_class device_make_class device_platform publisher_id_class resolution
  /selection=forward(select=sl sle=0.10) stats=all showpvalues;
run;

/* We will now model with Traditional Backward Selection */
proc glmselect data=games plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) wifi(split);
 model install = wifi device_volume device_height device_width device_os_class device_make_class device_platform publisher_id_class resolution
/selection=backward(select=sl sls=0.10) stats=all showpvalues;
run;

/* We will now model with Traditional Stepwise Selection */
proc glmselect data=games plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) wifi(split);
 model install = wifi device_volume device_height device_width device_os_class device_make_class device_platform publisher_id_class resolution
/selection=stepwise(select=sl ) stats=all showpvalues;
run;

/* Best subsets regression */
proc glmmod data=games outdesign=games_with_indicators noprint; 
 class device_os_class device_make_class device_platform publisher_id_class wifi;
 model install = wifi device_volume device_height device_width device_os_class device_make_class device_platform publisher_id_class resolution/ noint;
run;

proc contents data=games_with_indicators; 
run;

proc reg data=games_with_indicators plots=none;
 model install = col2-col36 /selection=cp adjrsq aic bic best=10;
quit;

/* We will check the results again by mentioning device_height and device_width as class variables */
/* We will approach model buidling with first approach as Traditional Forward Selection */
proc glmselect data=games plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) wifi(split) device_height(split) device_width(split);
 model install = wifi device_volume device_os_class device_make_class device_platform publisher_id_class resolution
  /selection=forward(select=sl sle=0.10) stats=all showpvalues;
run;

/* We will now model with Traditional Backward Selection */
proc glmselect data=games plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) wifi(split) device_height(split) device_width(split);
 model install = wifi device_volume device_os_class device_make_class device_platform publisher_id_class resolution
/selection=backward(select=sl sls=0.10) stats=all showpvalues;
run;

/* We will now model with Traditional Stepwise Selection */
proc glmselect data=games plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) wifi(split) device_height(split) device_width(split);
 model install = wifi device_volume device_os_class device_make_class device_platform publisher_id_class resolution
/selection=stepwise(select=sl ) stats=all showpvalues;
run; 

/* We will now do log transformation of device_height and device_width */
data games;
set games;
log_device_height = log(device_height);
log_device_width = log(device_width);
run;

data games_train;
set games_train;
log_device_height = log(device_height);
log_device_width = log(device_width);
run;

data games_test;
set games_test;
log_device_height = log(device_height);
log_device_width = log(device_width);
run;

/* We will approach model buidling with first approach as Traditional Forward Selection */
proc glmselect data=games plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) wifi(split);
 model install = wifi device_volume log_device_height log_device_width device_os_class device_make_class device_platform publisher_id_class resolution
  /selection=forward(select=sl sle=0.10) stats=all showpvalues;
run;

/* We will now model with Traditional Backward Selection */
proc glmselect data=games plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) wifi(split);
 model install = wifi device_volume log_device_height log_device_width device_os_class device_make_class device_platform publisher_id_class resolution
/selection=backward(select=sl sls=0.10) stats=all showpvalues;
run;

/* We will now model with Traditional Stepwise Selection */
proc glmselect data=games plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) wifi(split);
 model install = wifi device_volume log_device_height log_device_width device_os_class device_make_class device_platform publisher_id_class resolution
/selection=stepwise(select=sl ) stats=all showpvalues;
run;



/* We will proceed with creating a small dataset to offer more room for methods to differ with limited data */
/* We will also try to include interaction between variables by adding '@2' at the end */ 
data games_small; 
 set games; 
 call streaminit(1);
 if rand('uniform') > 0.75;
run;

/* Forward selection with cross-validation*/
proc glmselect data=games_small testdata=games_test seed=2 plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) device_height(split) device_width(split) wifi(split);
 partition fraction(validate=0.2);
 model install = wifi|device_volume|device_height|device_width|resolution|device_os_class|device_make_class|device_platform|publisher_id_class @2
  /selection=forward(select=validate) hierarchy=single;
 performance buildsscp=incremental;
run;

/* Backward selection with cross-validation*/
proc glmselect data=games_small testdata=games_test seed=2 plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) device_height(split) device_width(split) wifi(split);
 partition fraction(validate=0.2);
 model install = wifi|device_volume|device_height|device_width|resolution|device_os_class|device_make_class|device_platform|publisher_id_class @2
  /selection=backward(select=validate) hierarchy=single;
 performance buildsscp=incremental;
run;

/* Stepwise selection with cross-validation*/
proc glmselect data=games_small testdata=games_test seed=2 plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) device_height(split) device_width(split) wifi(split);
 partition fraction(validate=0.2);
 model install = wifi|device_volume|device_height|device_width|resolution|device_os_class|device_make_class|device_platform|publisher_id_class @2
  /selection=stepwise(select=validate) hierarchy=single;
 performance buildsscp=incremental;
run;

/* Forward selection with 10-fold cross-validation*/
proc glmselect data=games_small testdata=games_test seed=2 plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) device_height(split) device_width(split) wifi(split);
 model install = wifi|device_volume|device_height|device_width|resolution|device_os_class|device_make_class|device_platform|publisher_id_class @2
  /selection=forward(select=cv) hierarchy=single cvmethod=random(10);
 performance buildsscp=incremental;
run;

/* Stepwise selection with 10-fold cross-validation*/
proc glmselect data=games_small testdata=games_test seed=2 plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) device_height(split) device_width(split) wifi(split);
 model install = wifi|device_volume|device_height|device_width|resolution|device_os_class|device_make_class|device_platform|publisher_id_class @2
  /selection=stepwise(select=cv) hierarchy=single cvmethod=random(10);
 performance buildsscp=incremental;
run;

/* Backward selection with 10-fold cross-validation*/
proc glmselect data=games_small testdata=games_test seed=2 plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) device_height(split) device_width(split) wifi(split);
 model install = wifi|device_volume|device_height|device_width|resolution|device_os_class|device_make_class|device_platform|publisher_id_class @2
  /selection=backward(select=cv) hierarchy=single cvmethod=random(10);
 performance buildsscp=incremental;
run;

/* Backward selection with sl to select next step, and cross-validation to choose best model*/
proc glmselect data=games_small testdata=games_test seed=2 plots=ase;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) device_height(split) device_width(split) wifi(split);
 partition fraction(validate=0.2); 
 model install = wifi|device_volume|device_height|device_width|resolution|device_os_class|device_make_class|device_platform|publisher_id_class @2
  /selection=backward(select=sl choose=validate) hierarchy=single ;
 performance buildsscp=incremental;
run;

/* Backward selection with cv to select next step, and cross-validation to choose best model*/
proc glmselect data=games_small testdata=games_test seed=2 plots=ase;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) device_height(split) device_width(split) wifi(split);
 partition fraction(validate=0.2); 
 model install = wifi|device_volume|device_height|device_width|resolution|device_os_class|device_make_class|device_platform|publisher_id_class @2
  /selection=backward(select=cv choose=validate) hierarchy=single ;
 performance buildsscp=incremental;
run;


/* Now we will try on our original full dataset(Train dataset) with the best performing model */
proc glmselect data=games_train testdata=games_test seed=2 plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) device_height(split) device_width(split) wifi(split);
 model install = wifi|device_volume|device_height|device_width|resolution|device_os_class|device_make_class|device_platform|publisher_id_class @2
  /selection=backward(select=cv) hierarchy=single cvmethod=random(10);
 performance buildsscp=incremental;
run;

proc glmselect data=games_train testdata=games_test seed=2 plots=all;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) device_height(split) device_width(split) wifi(split);
 model install = wifi|device_volume|device_height|device_width|resolution|device_os_class|device_make_class|device_platform|publisher_id_class @2
  /selection=stepwise(select=cv) hierarchy=single cvmethod=random(10);
 performance buildsscp=incremental;
run;

/* Backward selection with select=sl, and cross-validation to choose best model - FINAL MODEL*/
proc glmselect data=games_train testdata=games_test seed=2 plots=ase;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) device_height(split) device_width(split) wifi(split);
 partition fraction(validate=0.2); 
 model install = wifi|device_volume|device_height|device_width|resolution|device_os_class|device_make_class|device_platform|publisher_id_class @2
  /selection=backward(select=sl choose=validate) hierarchy=single ;
 performance buildsscp=incremental;
run;


/* ---------------------------------------------------------------------------------------------   */

/* Part I  b)  */
/* Creating a logistic regression model */

/* Creating a simple logistic model with all variables */
 proc hplogistic data=games seed=100;
	  class device_os_class(ref = '1') device_make_class(ref = '1') device_platform(ref = '1') publisher_id_class(ref = '1') wifi(ref='1');
	  model install (event='1')= wifi device_volume device_height device_width device_os_class device_make_class device_platform publisher_id_class resolution ;
  run;

/* Forward selection with significance level as default criteria*/
 proc hplogistic data=games seed=100;
	  class device_os_class(ref = '1') device_make_class(ref = '1') device_platform(ref = '1') publisher_id_class(ref = '1') wifi(ref='1');
	  model install (event='1')= wifi device_volume device_height device_width resolution device_os_class device_make_class device_platform publisher_id_class;
	  selection method=forward(select=sl sle=0.10);
  run;

  /* Backward selection with significance level as default criteria*/
 proc hplogistic data=games seed=100;
	  class device_os_class(ref = '1') device_make_class(ref = '1') device_platform(ref = '1') publisher_id_class(ref = '1') wifi(ref='1');
	  model install (event='1')= wifi device_volume device_height device_width resolution device_os_class device_make_class device_platform publisher_id_class;
	  selection method=backward(select=sl sls=0.10);
  run;

    /* Stepwise selection with significance level as default criteria*/
 proc hplogistic data=games seed=100;
	  class device_os_class(ref = '1') device_make_class(ref = '1') device_platform(ref = '1') publisher_id_class(ref = '1') wifi(ref='1');
	  model install (event='1')= wifi device_volume device_height device_width resolution device_os_class device_make_class device_platform publisher_id_class;
	  selection method=stepwise(select=sl sls=0.10);
  run;

  /* Stepwise selection with significance level as default criteria and with cross validation*/
 proc hplogistic data=games seed=100;
	  class device_os_class(ref = '1') device_make_class(ref = '1') device_platform(ref = '1') publisher_id_class(ref = '1') wifi(ref='1');
	  model install (event='1')= wifi device_volume device_height device_width resolution device_os_class device_make_class device_platform publisher_id_class;
      partition fraction(test=0.1 validate=0.1);
	  selection method=stepwise(select=sl choose=validate);
  run;


  /* Now adding interaction effects in the model */
 /* Forward selection with significance level as default criteria*/
 proc hplogistic data=games seed=100;
	  class device_os_class(ref = '1') device_make_class(ref = '1') device_platform(ref = '1') publisher_id_class(ref = '1') wifi(ref='1');
	  model install (event='1')= wifi|device_volume|device_height|resolution|device_width|device_os_class|device_make_class|device_platform|publisher_id_class @2;
	  partition fraction(test=0.1 validate=0.1);
	  selection method=forward(select=sl sle=0.10);
  run;

  /* Backward selection with significance level as default criteria*/
 proc hplogistic data=games seed=100;
	  class device_os_class(ref = '1') device_make_class(ref = '1') device_platform(ref = '1') publisher_id_class(ref = '1') wifi(ref='1');
	  model install (event='1')= wifi|device_volume|device_height|resolution|device_width|device_os_class|device_make_class|device_platform|publisher_id_class @2;
	  partition fraction(test=0.1 validate=0.1);
	  selection method=backward(select=sl sls=0.10);
  run;

  /* Stepwise selection with significance level as default criteria*/
   proc hplogistic data=games seed=100;
	  class device_os_class(ref = '1') device_make_class(ref = '1') device_platform(ref = '1') publisher_id_class(ref = '1') wifi(ref='1');
	  model install (event='1')= wifi|device_volume|device_height|resolution|device_width|device_os_class|device_make_class|device_platform|publisher_id_class @2;
	  partition fraction(test=0.1 validate=0.1);
	  selection method=stepwise(select=sl sls=0.10);
  run;


  /*  Using CV  */
  /* Forward selection with significance level as default criteria and using cross validation for choosing best model*/
 proc hplogistic data=games seed=100;
	  class device_os_class(ref = '1') device_make_class(ref = '1') device_platform(ref = '1') publisher_id_class(ref = '1') wifi(ref='1');
	  model install (event='1')= wifi|device_volume|device_height|resolution|device_width|device_os_class|device_make_class|device_platform|publisher_id_class @2;
	  partition fraction(test=0.1 validate=0.1);
	  selection method=forward(select=sl choose=validate hierarchy=single);
  run;

    /* Backward selection with significance level as default criteria and using cross validation for choosing best model*/
 proc hplogistic data=games seed=100;
	  class device_os_class(ref = '1') device_make_class(ref = '1') device_platform(ref = '1') publisher_id_class(ref = '1') wifi(ref='1');
	  model install (event='1')= wifi|device_volume|device_height|resolution|device_width|device_os_class|device_make_class|device_platform|publisher_id_class @2;
	  partition fraction(test=0.1 validate=0.1);
	  selection method=backward(select=sl choose=validate hierarchy=single);
  run;

  /* Stepwise selection with significance level as default criteria and using cross validation for choosing best model */
   proc hplogistic data=games seed=100;
	  class device_os_class(ref = '1') device_make_class(ref = '1') device_platform(ref = '1') publisher_id_class(ref = '1') wifi(ref='1');
	  model install (event='1')= wifi|device_volume|device_height|resolution|device_width|device_os_class|device_make_class|device_platform|publisher_id_class @2;
	  partition fraction(test=0.1 validate=0.1);
	  selection method=stepwise(select=sl choose=validate hierarchy=single);
  run;


  	/* Modelling of Rare events */
  	/* We will perform oversampling for this using data command.*/
	data games_rare2;
        set games;
        if install=1 or (install=0 and ranuni(75302)<1/99) then output;
        run;

	 proc freq data=games_rare2;
        table install ;
        title "Install variable distribution after oversampling";
        run;

	  /* We then run our best model on this new oversampled dataset */
	  /* Stepwise selection with significance level as default criteria and using cross validation for choosing best model */
 proc hplogistic data=games seed=100;
	  class device_os_class(ref = '1') device_make_class(ref = '1') device_platform(ref = '1') publisher_id_class(ref = '1') wifi(ref='1');
	  model install (event='1')= wifi device_volume device_height device_width resolution device_os_class device_make_class device_platform publisher_id_class;
      partition fraction(test=0.1 validate=0.1);
	  selection method=stepwise(select=sl choose=validate);
  run;
	/* Running the best model obtained on oversampled dataset using proc logistic*/
 proc logistic data=games_rare2;
	  class publisher_id_class(ref = '1') device_make_class(ref = '1') wifi(ref='1');
	  logit: model install (event='1')= wifi device_volume device_make_class device_height publisher_id_class;
  run;




  /* ---------------------------------------------------------------------------------------------   */

  /* Part II  a)  */

  /* Creating the ROC tables */
  /* Logistic regression model */

  /* Making predictions*/
proc logistic data=games_train ;
 class device_make_class(ref = '1') publisher_id_class(ref = '1') wifi(ref = '1');
 logit: model install (event='1')= wifi device_volume device_height device_make_class publisher_id_class ;
 score data=games_test out=games_logit_predict; 
run;

  /*ROC curve */
proc logistic data=games_logit_predict plots=roc(id=prob);
 class device_make_class(ref = '1') publisher_id_class(ref = '1') wifi(ref = '1');
 logit: model install (event='1')= wifi device_volume device_height device_make_class publisher_id_class / nofit;
 roc pred=p_1;
run;

/* Creating the ROC table */
proc logistic data=games_train outmodel=Logitmodel;
  class device_make_class(ref = '1') publisher_id_class(ref = '1') wifi(ref = '1');
  logit: model install (event='1')= wifi device_volume device_height device_make_class publisher_id_class ;
run;

proc logistic inmodel=Logitmodel;
 score data=games_test outroc=games_logit_roc;
run;

/* Here, the false positive cost will be 1 cent whereas false negative cost will be 100 cents */
data games_roc;
 set games_logit_roc;
 TotalCost = _FALPOS_*1 + _FALNEG_*100;
run;

/* Sorting in ascending order to get minimum cost*/
proc sort data = games_roc;
by TotalCost;
Run;


/* Linear Probability Models */
/* Linear probability model with Backward selection */
proc glmselect data=games_samples testdata=games_test seed=2 plots=ase outdesign=LinearModel;
 class device_os_class(split) device_make_class(split) device_platform(split) publisher_id_class(split) device_height(split) device_width(split) wifi(split);
 partition fraction(validate=0.2); 
 linear: model install = wifi|device_volume|device_height|device_width|resolution|device_os_class|device_make_class|device_platform|publisher_id_class @2
  /selection=backward(select=sl choose=validate) hierarchy=single ;
 performance buildsscp=incremental;
run;

Proc Contents data = LinearModel varnum;
run;

%put &_GLSMod;

data select;
	set games_samples;
	keep selected;
	run;

Data Main;
merge select LinearModel;
run;

/* Running our model and storing results in Lin_predictions */
proc reg data = Main;
	model install = &_GLSMod;
	weight selected;
	output out=Lin_predictions p = linear_predictions;
	run;
quit;

/* Plotting the ROC curve*/
proc logistic data=Lin_predictions plots=roc(id=prob);
 model install (event = '1') = &_GLSMod / nofit;
 roc pred=linear_predictions;
 where selected=0;
run;

/* Creating ROC table for Linear Probability Model */
/* We will create datasets for different threshold values and we will consider only 5 columns */

/* Threshold - 0.001 */
data threshold_001 (keep = threshold linear_predictions predicted_values FP FN );
	set Lin_predictions ;
	where selected = 0;   /* Selecting only test samples */
	if linear_predictions > 0.001 then predicted_values = 1;
	if install = 1 and predicted_values = 0 then FN = 1;
	if install = 0 and predicted_values = 1 then FP = 1;
	threshold = 0.001;
run;

/* Threshold - 0.005 */
data threshold_005 (keep = threshold linear_predictions predicted_values FP FN );
	set Lin_predictions ;
	where selected = 0;   /* Selecting only test samples */
	if linear_predictions > 0.005 then predicted_values = 1;
	if install = 1 and predicted_values = 0 then FN = 1;
	if install = 0 and predicted_values = 1 then FP = 1;
	threshold = 0.005;
run;

/* Threshold - 0.010 */
data threshold_010 (keep = threshold linear_predictions predicted_values FP FN );
	set Lin_predictions ;
	where selected = 0;   /* Selecting only test samples */
	if linear_predictions > 0.010 then predicted_values = 1;
	if install = 1 and predicted_values = 0 then FN = 1;
	if install = 0 and predicted_values = 1 then FP = 1;
	threshold = 0.010;
run;

/* Threshold - 0.015 */
data threshold_015 (keep = threshold linear_predictions predicted_values FP FN );
	set Lin_predictions ;
	where selected = 0;   /* Selecting only test samples */
	if linear_predictions > 0.015 then predicted_values = 1;
	if install = 1 and predicted_values = 0 then FN = 1;
	if install = 0 and predicted_values = 1 then FP = 1;
	threshold = 0.015;
run;

/* Threshold - 0.020 */
data threshold_020 (keep = threshold linear_predictions predicted_values FP FN );
	set Lin_predictions ;
	where selected = 0;   /* Selecting only test samples */
	if linear_predictions > 0.020 then predicted_values = 1;
	if install = 1 and predicted_values = 0 then FN = 1;
	if install = 0 and predicted_values = 1 then FP = 1;
	threshold = 0.020;
run;

/* Threshold - 0.025 */
data threshold_025 (keep = threshold linear_predictions predicted_values FP FN );
	set Lin_predictions ;
	where selected = 0;   /* Selecting only test samples */
	if linear_predictions > 0.025 then predicted_values = 1;
	if install = 1 and predicted_values = 0 then FN = 1;
	if install = 0 and predicted_values = 1 then FP = 1;
	threshold = 0.025;
run;

/* Threshold - 0.030 */
data threshold_030 (keep = threshold linear_predictions predicted_values FP FN );
	set Lin_predictions ;
	where selected = 0;   /* Selecting only test samples */
	if linear_predictions > 0.030 then predicted_values = 1;
	if install = 1 and predicted_values = 0 then FN = 1;
	if install = 0 and predicted_values = 1 then FP = 1;
	threshold = 0.030;
run;

/* Threshold - 0.035 */
data threshold_035 (keep = threshold linear_predictions predicted_values FP FN );
	set Lin_predictions ;
	where selected = 0;   /* Selecting only test samples */
	if linear_predictions > 0.035 then predicted_values = 1;
	if install = 1 and predicted_values = 0 then FN = 1;
	if install = 0 and predicted_values = 1 then FP = 1;
	threshold = 0.035;
run;

/* Threshold - 0.040 */
data threshold_040 (keep = threshold linear_predictions predicted_values FP FN );
	set Lin_predictions ;
	where selected = 0;   /* Selecting only test samples */
	if linear_predictions > 0.040 then predicted_values = 1;
	if install = 1 and predicted_values = 0 then FN = 1;
	if install = 0 and predicted_values = 1 then FP = 1;
	threshold = 0.040;
run;

/* Threshold - 0.045 */
data threshold_045 (keep = threshold linear_predictions predicted_values FP FN );
	set Lin_predictions ;
	where selected = 0;   /* Selecting only test samples */
	if linear_predictions > 0.045 then predicted_values = 1;
	if install = 1 and predicted_values = 0 then FN = 1;
	if install = 0 and predicted_values = 1 then FP = 1;
	threshold = 0.045;
run;

/* Threshold - 0.050 */
data threshold_050 (keep = threshold linear_predictions predicted_values FP FN );
	set Lin_predictions ;
	where selected = 0;   /* Selecting only test samples */
	if linear_predictions > 0.050 then predicted_values = 1;
	if install = 1 and predicted_values = 0 then FN = 1;
	if install = 0 and predicted_values = 1 then FP = 1;
	threshold = 0.050;
run;

/* Now we will create tables for each threshold value using proc sql */
proc sql;
	create table t_001 as select distinct count(FP) as FP, count(FN) as FN, threshold from threshold_001;

	create table t_005 as select distinct count(FP) as FP, count(FN) as FN, threshold from threshold_005;

	create table t_010 as select distinct count(FP) as FP, count(FN) as FN, threshold from threshold_010;

	create table t_015 as select distinct count(FP) as FP, count(FN) as FN, threshold from threshold_015;

	create table t_020 as select distinct count(FP) as FP, count(FN) as FN, threshold from threshold_020;

	create table t_025 as select distinct count(FP) as FP, count(FN) as FN, threshold from threshold_025;

	create table t_030 as select distinct count(FP) as FP, count(FN) as FN, threshold from threshold_030;

	create table t_035 as select distinct count(FP) as FP, count(FN) as FN, threshold from threshold_035;

	create table t_045 as select distinct count(FP) as FP, count(FN) as FN, threshold from threshold_040;

	create table t_045 as select distinct count(FP) as FP, count(FN) as FN, threshold from threshold_045;

	create table t_050 as select distinct count(FP) as FP, count(FN) as FN, threshold from threshold_050;
	quit;


/* Now we will append all the data in the tables in one single table t_001*/

PROC APPEND BASE=t_001
 DATA=t_005;
RUN;

PROC APPEND BASE=t_001
 DATA=t_010;
RUN;

PROC APPEND BASE=t_001
 DATA=t_015;
RUN;

PROC APPEND BASE=t_001
 DATA=t_020;
RUN;

PROC APPEND BASE=t_001
 DATA=t_025;
RUN;

PROC APPEND BASE=t_001
 DATA=t_030;
RUN;

PROC APPEND BASE=t_001
 DATA=t_035;
RUN;

PROC APPEND BASE=t_001
 DATA=t_040;
RUN;

PROC APPEND BASE=t_001
 DATA=t_045;
RUN;

PROC APPEND BASE=t_001
 DATA=t_050;
RUN;

