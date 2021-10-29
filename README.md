******************************** SHEFFIELD ARABLE FARM MODEL (SAFMOD) **********************************

SAFMOD is an arable farm level model, which consists of four modules: profit maximisation, nitrate leaching and 
risk minimisation models. The model combines mixed-integer, risk (MOTAD) and weighted goal programming approaches
to optimise three arable farming objectives: profit, nitrate leaching and risk subject to the following constraints:

1. Workable hours constraint
2. Sequential and non-sequential constraints
3. Crop rotation/sequencing constraint
4. Total farm area constraint
5. Crop proportion constraint

The model was developed using the R programming language and uses R version of the GLPK solver (Rglpk) to solve
the mixed-integer models

SAFMOD parameter data and matrices are stored in CSV files, which are are extracted and applied in the model using a 
series of functions.

In terms of Crops or Activities, the model consist of 10 Activities (9 Crops and Set-aside)
a. Winter wheat
b. Spring wheat
c. Winter barley
d. Spring barley
e. Winter beans 
f. Spring beans
g. Ware potato
h. Winter oilseed rape
i. Sugar beet
j. Set-aside

SAFMOD is developed to run in sequential order and thus the model run starts by:

** Selecting/setting the soil type and rainfall (Stored in the file, "Farm_location.csv") (Lines 58 and 89) **
The soil type first determines and sets the recommended fertiliser rate (kg/ha) for each of the crops
based on which the Variable Costs (£/ha) are estimated for each crop. The recommended fertiliser rates are 
stored in the file, "Soil_fert.csv".

The N fertiliser rates determine (Line 110) by the soil type then (in addition to the soil type) determine the
the Crop Yield (t/ha) for each of the crops. 

The yields (Line 216) are then used in the estimation of the Crop Output
(Yield * Price). The Gross Margins for each of the crops are estimated taking into consideration the Single
Farm Payment (£207/ha). The prices of crops are based on 5-year historical data. All crop input and output data or 
information are stored in the file, "Crop_input_output.csv".

The Income Deviation and Standard Deviation estimates used as measures of risk are then estimated based on the 
crop yields and the historical price data.

The N fertiliser rates are also used to determine the Nitrate Leaching (kg N/ha) estimates (Line 399). The Nitrate Leaching and Standard
Deviation estimates are also stored in the file, "Crop_input_output.csv".

** The function 'cropData' updates all the gross margin, risk (standard deviation and income deviation) and nitrate leaching
estimates and stores data in the file "Crop_input_output.csv". (Line 458)

** Determination of Workable hours (Lines 498 to 837)**
The soil type and the rainfall amount are then used to detrmine the Workable Hours in each of the two-week periods into which
the cropping season has been divided.The estimated Workable Hours for the 26 two-week periods and they are estimated using the function 'workableHoursCal' and 'periodWorkableHours' and are then stored in the file, "Workable_hours.csv". 

*The workable hours with respect to each of the operations are estimated using the function 'opsWorkableHours' and 
'labWorkableHours' and are stored in the files "Ops_Workablehours.csv" and "Lab_Workablehours.csv".

** Estimation of work rates and Operation cost (Line 1120) **
The workrates with respect to each of the operation are estimated as function of factors such as soil types, crop yield,
fertiliser rates and machine sizes. The work rates and subsequently the operations costs are calculated 
using the functions 'workRateCal' and 'operationCost' and the estimates are store in the file "Workrates.csv".

** Estimation of fixed costs (Line 1221)**
The fixed costs are estimated with respect to machine types and labour using the function 'fixedCost' and stored in the 
file "Machines.csv".

** Self rotation penalties (Line 1306)**
The penalties with respect to continuous cropping and are estimated using the function 'selfRotPen' and are stored in the files "Self_Rot_Pen.csv"

** Rotational and yield penalties (Lines 1359 to 1439)**
The Rotaional penalties and yield penalties (penalties due to sub-optimal operation) are estimated using the functions
'rotPenalty' and 'yieldPenalty' and are stored in the file "Yield_Pen.csv".

** MODEL MATRIX GENERATION ***
** Operation cost vector (Line 1536)***
The vector of operation costs in the objective function for the profit maximisation is created using the function 'objOpsCost'
and stored the file "Obj_Ops_Cost.csv"

** Constraint matrices (from Line 1641) **
The matrix to enforce the constraint linking the total crop area to the area of first operation is created using the
function 'areaFirstConsMatrix' and stored in the file "Area_First_Matrix.csv".

** Sequential/non-sequential operation workrate matrix (Lines 1714 to 1953)**
The matrices for sequential/non-sequential operation and labour work rates are created using the functions
'seqWorkRateMatrix' and 'seqOpLabourMatrix' and are stored in "Seq_Ops_Matrix.csv" and "Seq_Lab_Matrix.csv" respectively.

** Sequential/non-sequential operations constraint matrices (Lines 2149 to 2524)**
The matrices for sequential and non-sequential operations constraints are created using the functions 'seqConstraint' and 
'nonSeqOpConstraint' and are stored in the files "Seq_Cons_Matrix.csv" and "Nonseq_Cons_Matrix.csv" respectively.

** Crop sequencing constraint (Line 2577)**
The matrix for the crop sequencing constraint is created using the function 'rotSeqMatrix' and stored in the file
"Rot_Seq_Matrix.csv".

** Rotational penalty vetcor (Line 2811)**
The vector for the crop sequencing or rotational penalties in the objective function is created using the function 
'modRotPen' and stored in the file "Rot_Pen.csv".

** Model output display functions (Line 2905)**
The model display of crop rotation matrix, crop areas and machine/labour numbers table are created using the functions
'cropRotation', 'cropAreas' and 'machineLabour'.

** Periodic workable hours matrix (from Line 3088)**
The matric assigning workable hours to periods with respect to operations is created using the function 'workerMatrix'
and stored in the file "Workers.csv".

** Model constraint matrix (from Line 3265) **
All the constraint matrices are put together by the function 'consMatrix' and stored in the file "Cons_Matrix.csv".

** Objective funtions (Lines 3309 to 3375)**
The objective function for profit maximisation, nitrate leaching minimisation and risk minimisation are created 
by the functions 'objFun1', 'objFun2' and 'objFun3'.

** The right hand side (RHS) vector (from Line 3448) **
The RHS vector is created using the function 'modRHS' and stored in the file "RHS.csv".

** Constraint directions vector (Line 3473) **
The vector for constraint direction (e.g. <=, = or >=) is created by the function 'varDir' and stored in the file "DIR.csv".

** All matrices and vectors (Line 3494) **
Model matrices and vectors are put together by the function 'module1'. It also ensures the that the model is run for 
a monocropping scenario.

** Solving models ( from Lines 3685) to 4146) **
The pure profit model, nitrated leaching model, risk model, the goal-programming model and the overall SAFMOD model are 
solved using the functions 'solveProfitMod','solveNleachMod','solveRiskMod','solveGoalProg',
'solveSAFMOD' respectively. Different information and features with of the different models can be found their respective 
functions.

** Parameter variations (from Lines 4187) **
The factors for varying model parameters are stored in the file "Relative_change.csv".
** Set soil type and rainfall **
The soil type and rainfall can be set using the function 'setFarmSoilRain'.

** Set relative input and output amounts **
The relative crop input and output amounts can be set using the function 'setInputOutputAmount'.

** Monocropping scenario **
The models can be set to run for a mono-cropping scenario using the function 'setMonoCropping'.

** Set relative farm area **
The relative farm area can be set using the function 'setFarmArea'.

** Set relative economic factors **
The relative economic factors (fuel price, interest rate and inflation can be set using the function 
'setEconomicFactors'.

** Set rotation **
The crop rotation approach can be set or changed using the function 'setRotation'. The function set the rotation based crop 
proportions (Individual crop constraint). Rotation = "absolute" means model uses absolute percent of total crop area.  Rotation = "proportional" means model uses proporions on terms of which crop gives permission to the next crop. Rotation = "none" means no individual crop constraint. 
