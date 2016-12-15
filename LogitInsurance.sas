%let PATH = /folders/myfolders/sasuser.v94/Data;
%let NAME = INS;
%let LIB = &NAME..;

libname &NAME. "&PATH.";

%let INFILE = &LIB.LOGIT_INSURANCE;
%let TEMPFILE = TEMPFILE;
%let SCRUBFILE 	= SCRUBFILE;
%let SCOREME = &LIB.LOGIT_INSURANCE_TEST;


proc print data=&INFILE.(obs=6);
run;



proc freq data=&INFILE.;
table _character_ /missing;
run;


proc means data=&INFILE. n nmiss mean median std min max;
var _numeric_;
run;


proc univariate data=&INFILE. plot;
var TARGET_FLAG;
run;



proc corr data=&INFILE. plots=matrix;
var _numeric_;
with TARGET_FLAG;
run;



Proc freq data=&INFILE.;
table _character_ /plcorr;
run;


ods graphics on;
proc corr data=&INFILE. plots=matrix(histogram);
var _numeric_;
run;
ods graphics off;




data &TEMPFILE.;
set &INFILE.;
drop INDEX;
drop TARGET_AMT;
run;

proc freq data=&TEMPFILE.;
table _character_ /missing;
run;


proc means data=&TEMPFILE. n nmiss mean median;
var _numeric_;
run;



********************;
* MEAN IMPUTATION;
********************;

data &SCRUBFILE.;
set &TEMPFILE.;


IMP_INCOME = INCOME;
M_INCOME = 0;
if missing(IMP_INCOME) then do;
	M_INCOME = 1;
	IMP_INCOME = 98400;
	if job in ("STUDENT"			)	then IMP_INCOME = 6300;
	if job in ("Z_BLUE COLLAR"	) 	then IMP_INCOME = 59000;
	if job in ("PROFESSIONAL"	) 	then IMP_INCOME = 76600;
	if job in ("DOCTOR"			) 	then IMP_INCOME = 129000;
	if job in ("LAWYER"			) 	then IMP_INCOME = 88000;
	if job in ("CLERICAL"		) 	then IMP_INCOME = 34000;
	if job in ("HOME MAKER"		) 	then IMP_INCOME = 12000;
end;
drop INCOME;
 
IMP_AGE = AGE;
if missing( IMP_AGE ) 	then IMP_AGE = 45;
drop AGE;

IMP_JOB = JOB;
if missing(IMP_JOB) 	then IMP_JOB = "z_Blue Collar";
drop JOB;

IMP_YOJ = YOJ;
if missing(YOJ) then IMP_YOJ = 10;
drop YOJ;

IMP_CAR_AGE = CAR_AGE;
if missing(CAR_AGE) then IMP_CAR_AGE = 8;
drop CAR_AGE;

IMP_HOME_VAL = HOME_VAL;
if missing(HOME_VAL) then IMP_HOME_VAL = 154867;
drop HOME_VAL;

if (IMP_CAR_AGE) < 0 then IMP_CAR_AGE = 3;

run;


proc print data=&SCRUBFILE.(obs=8);
run;

proc freq data=&SCRUBFILE.;
table _character_ /missing;
run;

proc means data=&SCRUBFILE. n nmiss mean median;
var _numeric_;
run;




*****************************;
* DECISION TREE IMPUTATION;
*****************************;

data &SCRUBFILE.;
set &TEMPFILE.;

IMP_AGE = AGE;
AGE_FLAG = 0;
if missing (IMP_AGE) then do;
	AGE_FLAG = 1;
	IMP_AGE = 36;
	if (HOMEKIDS >= 0.5) and (KIDSDRIV < 0.5) and (YOJ > 14) then IMP_AGE = 42;
	if (HOMEKIDS >= 0.5) and (KIDSDRIV > 0.5) then IMP_AGE = 43;
	if (HOMEKIDS < 0.5) and (YOJ < 12) then IMP_AGE = 47;
	if (HOMEKIDS < 0.5) and (YOJ > 12) then IMP_AGE = 51;
drop AGE;
end;

IMP_YOJ = YOJ;
YOJ_FLAG = 0;
if missing (IMP_YOJ) then do;
	YOJ_FLAG = 1;
	IMP_YOJ = 0;
	if (INCOME > 2.5) and (MSTATUS in ("z_No")) then IMP_YOJ = 11;
	if (INCOME > 2.5) and (MSTATUS in ("Yes")) and (HOMEKIDS < 1.5) then IMP_YOJ = 12;
	if (INCOME > 2.5) and (MSTATUS in ("Yes")) and (HOMEKIDS > 1.5) then IMP_YOJ = 13;
drop YOJ;
end;

IMP_CAR_AGE  = CAR_AGE;
CAR_AGE_FLAG = 0;
if missing (IMP_CAR_AGE) then do;
	CAR_AGE_FLAG = 1;
	IMP_CAR_AGE = 4.1;
	if (EDUCATION in ("<High School","Bachelors","z_High School")) and (EDUCATION not in ("<High School","z_High School")) then IMP_CAR_AGE = 8.8;
	if (EDUCATION not in ("<High School","Bachelors","z_High School")) then IMP_CAR_AGE = 14;
drop CAR_AGE;
end;

IMP_INCOME = INCOME;
INCOME_FLAG = 0;
if missing (IMP_INCOME) then do;
	INCOME_FLAG = 1;
	IMP_INCOME = 20e+3;
	if (HOME_VAL < 268e+3) and (JOB not in ("Clerical","Home Maker","Student")) then IMP_INCOME = 66e+3;
	if (HOME_VAL > 268e+3) and (HOME_VAL < 401e+3) then IMP_INCOME = 109e+3;
	if (HOME_VAL > 268e+3) and (HOME_VAL > 401e+3) then IMP_INCOME = 187e+3;
drop INCOME;
end;

IMP_HOME_VAL = HOME_VAL;
HOME_VAL_FLAG = 0;
if missing (IMP_HOME_VAL) then do;
	HOME_VAL_FLAG = 1;
	IMP_HOME_VAL = 61e+3;
	if (INCOME < 86e+3) and (MSTATUS in ("Yes")) and (INCOME < 26e+3) then IMP_HOME_VAL = 80e+3;
	if (INCOME < 86e+3) and (MSTATUS in ("Yes")) and (INCOME > 26e+3) then IMP_HOME_VAL = 195e+3;
	if (INCOME > 86e+3) and (MSTATUS in ("z_No")) then IMP_HOME_VAL = 143e+3;
	if (INCOME > 86e+3) and (MSTATUS in ("Yes")) then IMP_HOME_VAL = 341e+3;
drop HOME_VAL;
end;

IMP_JOB = JOB;
JOB_FLAG = 0;
if missing (IMP_JOB) then do;
	JOB_FLAG = 1;
	IMP_JOB = "Doctor";
	if (EDUCATION in ("Masters","PhD")) and (EDUCATION not in ("PhD")) and (CAR_USE in ("Private")) then IMP_JOB = "Lawyer";
	if (EDUCATION in ("Masters","PhD")) and (EDUCATION not in ("PhD")) and (CAR_USE not in ("Private")) then IMP_JOB = "Manager";
	if (EDUCATION not in ("Masters","PhD")) and (INCOME < 15e+3) then IMP_JOB = "Student";
	if (EDUCATION not in ("Masters","PhD")) and (INCOME > 15e+3) and (CAR_USE in ("Private")) and (INCOME < 45e+3) then IMP_JOB = "Clerical";
	if (EDUCATION not in ("Masters","PhD")) and (INCOME > 15e+3) and (CAR_USE in ("Private")) and (INCOME > 45e+3) then IMP_JOB = "Professional";
	if (EDUCATION not in ("Masters","PhD")) and (INCOME > 15e+3) and (CAR_USE not in ("Private")) then IMP_JOB = "z_Blue_Collar";
drop JOB;	
end;

if (IMP_CAR_AGE < 0) then IMP_CAR_AGE = 3;

if (IMP_HOME_VAL > 497746) then IMP_HOME_VAL = 497746;
if (BLUEBOOK > 39090) then BLUEBOOK = 39090;
if (TRAVTIME > 75) then TRAVTIME = 75;

run;


proc print data=&SCRUBFILE.(obs=8);
run;

proc freq data=&SCRUBFILE.;
table _character_ /missing;
run;

proc means data=&SCRUBFILE. n nmiss mean median;
var _numeric_;
run;


*** COMPARE ORIGINAL AND LOG TRANSFORMED VARIABLES ***;
proc univariate data=&SCRUBFILE. plot;
var IMP_HOME_VAL;
run;
proc univariate data=&SCRUBFILE. plot;
var BLUEBOOK;
run;
proc univariate data=&SCRUBFILE. plot;
var TRAVTIME;
run;




* MODEL 1 PROC LOGISTIC FULL *;
proc logistic data=&SCRUBFILE. plot(only)=(roc(ID=prob));
class _character_ /param=ref;
model TARGET_FLAG( ref="0" ) = 
					IMP_AGE
					BLUEBOOK
					IMP_CAR_AGE
					CAR_TYPE
					CAR_USE
					CLM_FREQ
					EDUCATION
					HOMEKIDS
					IMP_HOME_VAL
					IMP_INCOME
					IMP_JOB
					KIDSDRIV
					MSTATUS
					MVR_PTS
					OLDCLAIM
					PARENT1
					RED_CAR
					REVOKED
					SEX
					TIF
					TRAVTIME
					URBANICITY
					IMP_YOJ
					AGE_FLAG
					CAR_AGE_FLAG
					HOME_VAL_FLAG
					INCOME_FLAG
					JOB_FLAG
					YOJ_FLAG
					/roceps=0.1
					;
/*score data=&SCRUBFILE.*/output out=SCOREDFILE p=p_crash;
run;


* MODEL 2 PROC LOGISTIC PROBIT FULL *;
proc logistic data=&SCRUBFILE. plot(only)=(roc(ID=prob));
class _character_ /param=ref;
model TARGET_FLAG( ref="0" ) = 
					IMP_AGE
					BLUEBOOK
					IMP_CAR_AGE
					CAR_TYPE
					CAR_USE
					CLM_FREQ
					EDUCATION
					HOMEKIDS
					IMP_HOME_VAL
					IMP_INCOME
					IMP_JOB
					KIDSDRIV
					MSTATUS
					MVR_PTS
					OLDCLAIM
					PARENT1
					RED_CAR
					REVOKED
					SEX
					TIF
					TRAVTIME
					URBANICITY
					IMP_YOJ
					AGE_FLAG
					CAR_AGE_FLAG
					HOME_VAL_FLAG
					INCOME_FLAG
					JOB_FLAG
					YOJ_FLAG
					/link=probit roceps=0.1
					;
/*score data=&SCRUBFILE.*/output out=SCOREDFILE p=p_crash;
run;



* MODEL 3 PROC LOGISTIC STEPWISE *;
proc logistic data=&SCRUBFILE. plot(only)=(roc(ID=prob));
class _character_ /param=ref;
model TARGET_FLAG( ref="0" ) = 
					IMP_AGE
					BLUEBOOK
					IMP_CAR_AGE
					CAR_TYPE
					CAR_USE
					CLM_FREQ
					EDUCATION
					HOMEKIDS
					IMP_HOME_VAL
					IMP_INCOME
					IMP_JOB
					KIDSDRIV
					MSTATUS
					MVR_PTS
					OLDCLAIM
					PARENT1
					RED_CAR
					REVOKED
					SEX
					TIF
					TRAVTIME
					URBANICITY
					IMP_YOJ
					AGE_FLAG
					CAR_AGE_FLAG
					HOME_VAL_FLAG
					INCOME_FLAG
					JOB_FLAG
					YOJ_FLAG
					/selection=stepwise
					;
/*score data=&SCRUBFILE.*/output out=SCOREDFILE p=p_crash;
run;



* MODEL 4 PROC LOGISTIC WITH DECISION TREE VARIABLES *;
proc logistic data=&SCRUBFILE. plot(only)=(roc(ID=prob));
class CAR_TYPE IMP_JOB URBANICITY /param=ref;
model TARGET_FLAG( ref="0" ) = 
					CAR_TYPE
					IMP_JOB
					IMP_HOME_VAL
					OLDCLAIM
					TRAVTIME
					URBANICITY
					/roceps=0.1
					;
/*score data=&SCRUBFILE.*/output out=SCOREDFILE p=p_crash;
roc;
roccontrast;
run;



* ROC AND KS STAT *;
proc npar1way data=SCOREDFILE wilcoxon edf;
    class target_flag;
    var p_crash;
run;

proc npar1way data=SCOREDFILE df plots=edfplot;
    class target_flag;
    var p_crash;
run;



* PROC GENMOD MODEL FOR BINGO BONUS *;
proc genmod data=&SCRUBFILE.;
class _character_ /param=ref;
model TARGET_FLAG( ref="0" ) = 
					IMP_AGE
					BLUEBOOK
					IMP_CAR_AGE
					CAR_TYPE
					CAR_USE
					CLM_FREQ
					EDUCATION
					HOMEKIDS
					IMP_HOME_VAL
					IMP_INCOME
					IMP_JOB
					KIDSDRIV
					MSTATUS
					MVR_PTS
					OLDCLAIM
					PARENT1
					RED_CAR
					REVOKED
					SEX
					TIF
					TRAVTIME
					URBANICITY
					IMP_YOJ
					AGE_FLAG
					CAR_AGE_FLAG
					HOME_VAL_FLAG
					INCOME_FLAG
					JOB_FLAG
					YOJ_FLAG
					;
run;





*****************************;
* SCORING CODE SAS DATA STEP
*****************************;

%macro SCORE( INFILE, OUTFILE );

data &OUTFILE.;
set &INFILE.;

IMP_AGE = AGE;
AGE_FLAG = 0;
if missing (IMP_AGE) then do;
	AGE_FLAG = 1;
	IMP_AGE = 36;
	if (HOMEKIDS >= 0.5) and (KIDSDRIV < 0.5) and (YOJ > 14) then IMP_AGE = 42;
	if (HOMEKIDS >= 0.5) and (KIDSDRIV > 0.5) then IMP_AGE = 43;
	if (HOMEKIDS < 0.5) and (YOJ < 12) then IMP_AGE = 47;
	if (HOMEKIDS < 0.5) and (YOJ > 12) then IMP_AGE = 51;
drop AGE;
end;

IMP_YOJ = YOJ;
YOJ_FLAG = 0;
if missing (IMP_YOJ) then do;
	YOJ_FLAG = 1;
	IMP_YOJ = 0;
	if (INCOME > 2.5) and (MSTATUS in ("z_No")) then IMP_YOJ = 11;
	if (INCOME > 2.5) and (MSTATUS in ("Yes")) and (HOMEKIDS < 1.5) then IMP_YOJ = 12;
	if (INCOME > 2.5) and (MSTATUS in ("Yes")) and (HOMEKIDS > 1.5) then IMP_YOJ = 13;
drop YOJ;
end;

IMP_CAR_AGE  = CAR_AGE;
CAR_AGE_FLAG = 0;
if missing (IMP_CAR_AGE) then do;
	CAR_AGE_FLAG = 1;
	IMP_CAR_AGE = 4.1;
	if (EDUCATION in ("<High School","Bachelors","z_High School")) and (EDUCATION not in ("<High School","z_High School")) then IMP_CAR_AGE = 8.8;
	if (EDUCATION not in ("<High School","Bachelors","z_High School")) then IMP_CAR_AGE = 14;
drop CAR_AGE;
end;

IMP_INCOME = INCOME;
INCOME_FLAG = 0;
if missing (IMP_INCOME) then do;
	INCOME_FLAG = 1;
	IMP_INCOME = 20e+3;
	if (HOME_VAL < 268e+3) and (JOB not in ("Clerical","Home Maker","Student")) then IMP_INCOME = 66e+3;
	if (HOME_VAL > 268e+3) and (HOME_VAL < 401e+3) then IMP_INCOME = 109e+3;
	if (HOME_VAL > 268e+3) and (HOME_VAL > 401e+3) then IMP_INCOME = 187e+3;
drop INCOME;
end;

IMP_HOME_VAL = HOME_VAL;
HOME_VAL_FLAG = 0;
if missing (IMP_HOME_VAL) then do;
	HOME_VAL_FLAG = 1;
	IMP_HOME_VAL = 61e+3;
	if (INCOME < 86e+3) and (MSTATUS in ("Yes")) and (INCOME < 26e+3) then IMP_HOME_VAL = 80e+3;
	if (INCOME < 86e+3) and (MSTATUS in ("Yes")) and (INCOME > 26e+3) then IMP_HOME_VAL = 195e+3;
	if (INCOME > 86e+3) and (MSTATUS in ("z_No")) then IMP_HOME_VAL = 143e+3;
	if (INCOME > 86e+3) and (MSTATUS in ("Yes")) then IMP_HOME_VAL = 341e+3;
drop HOME_VAL;
end;

IMP_JOB = JOB;
JOB_FLAG = 0;
if missing (IMP_JOB) then do;
	JOB_FLAG = 1;
	IMP_JOB = "Doctor";
	if (EDUCATION in ("Masters","PhD")) and (EDUCATION not in ("PhD")) and (CAR_USE in ("Private")) then IMP_JOB = "Lawyer";
	if (EDUCATION in ("Masters","PhD")) and (EDUCATION not in ("PhD")) and (CAR_USE not in ("Private")) then IMP_JOB = "Manager";
	if (EDUCATION not in ("Masters","PhD")) and (INCOME < 15e+3) then IMP_JOB = "Student";
	if (EDUCATION not in ("Masters","PhD")) and (INCOME > 15e+3) and (CAR_USE in ("Private")) and (INCOME < 45e+3) then IMP_JOB = "Clerical";
	if (EDUCATION not in ("Masters","PhD")) and (INCOME > 15e+3) and (CAR_USE in ("Private")) and (INCOME > 45e+3) then IMP_JOB = "Professional";
	if (EDUCATION not in ("Masters","PhD")) and (INCOME > 15e+3) and (CAR_USE not in ("Private")) then IMP_JOB = "z_Blue_Collar";
drop JOB;	
end;

if (IMP_CAR_AGE < 0) then IMP_CAR_AGE = 3;

if (IMP_HOME_VAL > 497746) then IMP_HOME_VAL = 497746;
if (BLUEBOOK > 39090) then BLUEBOOK = 39090;
if (TRAVTIME > 75) then TRAVTIME = 75;


LOG_ODDS	=	-1.3507
				-0.00002	*	BLUEBOOK
				-0.7124		*	(CAR_TYPE in ("Minivan"))
				-0.0837		*	(CAR_TYPE in ("Panel Truck"))
				-0.1633		*	(CAR_TYPE in ("Pickup"))
				+0.2588		*	(CAR_TYPE in ("Sports Car"))
				-0.0473		*	(CAR_TYPE in ("Van"))
				+0.7720		*	(CAR_USE in ("Commercial"))
				+0.1962		*	CLM_FREQ
				-0.0128		*	(EDUCATION in ("<High School"))
				-0.3505		*	(EDUCATION in ("Bachelors"))
				-0.3361		*	(EDUCATION in ("Masters"))
				-0.0211		*	(EDUCATION in ("PhD"))
				-1.48E-6	*	IMP_HOME_VAL
				-3.29E-6	*	IMP_INCOME
				+0.0978		*	(IMP_JOB in ("Clerical"))
				-1.0909		*	(IMP_JOB in ("Doctor"))
				-0.0524		*	(IMP_JOB in ("Home Maker"))
				-0.1972		*	(IMP_JOB in ("Lawyer"))
				-0.8491		*	(IMP_JOB in ("Manager"))
				-0.1462		*	(IMP_JOB in ("Professional"))
				-0.0582		*	(IMP_JOB in ("Student"))
				+0.4197		*	KIDSDRIV
				-0.4520		*	(MSTATUS in ("Yes"))
				+0.1145		*	MVR_PTS
				-0.00001	*	OLDCLAIM
				-0.4540		*	(PARENT1 in ("No"))
				-0.8937		*	(REVOKED in ("No"))
				-0.0555		*	TIF
				+0.0150		*	TRAVTIME
				+2.3887		*	(URBANICITY in ("Highly Urban/ Urban"))
				+0.5222		*	JOB_FLAG
				;
ODDS = exp( LOG_ODDS );
P_TARGET_FLAG = ODDS / (1+ODDS);


keep INDEX P_TARGET_FLAG;
run;


%mend;


%score( &SCOREME., SCOREDFILE );

proc print data=SCOREDFILE(obs=10);
run;

proc means data=SCOREDFILE N NMISS MIN MEAN MAX;
var P_TARGET_FLAG;
run;


proc export data=SCOREDFILE
outfile=" /folders/myfolders/sasuser.v94/Unit02/DTMODEL.csv "
dbms=csv;
run;


data "/folders/myfolders/sasuser.v94/Unit02/KEVIN_WONG_INSURANCE_SCORED";
set SCOREDFILE;
run;




