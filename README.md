******************************** SHEFFIELD ARABLE FARM MODEL (SAFMOD) **********************************

SAFMOD is an arable farm level model, which consist of four modules: profit maximisation, nitrate leaching and 
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

* Selecting the soil type and rainfall (Stored in the file, "Farm_location.csv")
The soil type first determines and sets the recommended fertiliser rate (kg/ha) for each of the crops
based on which the Variable Costs (£/ha) are estimated for each crop. The recommended fertiliser rates are 
stored in the file, "Soil_fert.csv".

The N fertiliser rates determine by the soil type then (in addition to the soil type) determine the
the Crop Yield (t/ha) for each of the crops. The yields are then used in the estimation of the Crop Output
(Yield * Price). The Gross Margins for each of the crops are estimated taking into consideration the Single
Farm Payment (£207/ha). The prices of crops are based on 5-year historical data. All crop input and output data or 
information are stored in the file, "Crop_input_output.csv".

The Income Deviation and Standard Deviation estimates used as measures of risk are then estimated based on the 
crop yields and the historical price data.

The N fertiliser rates are also used to determine the Nitrate Leaching (kg N/ha) estimates. The Nitrate Leaching and Standard
Deviation estimates are also stored in the file, "Crop_input_output.csv".

The soil type and the rainfall amount are then used to detrmine the Workable Hours in each of the two-week periods into which
the cropping season has been divided.The estimated Workable Hours for the 26 two-week periods are then stored in the file, 
"Workable_hours.csv". 
