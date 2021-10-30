
#*****************************************************************************************************************************************************
#******************************************************** SHEFFIELD ARABLE FARM MODEL (SAFMOD) *******************************************************
#*****************************************************************************************************************************************************

# The Sheffield Arable Farm Model (SAFMOD) is a mixed-integer programming model, which optimises 
# economic (farm profit & risk) and environmental (nitrate leaching) outcomes in (UK) arable farming systems.
# The model assumes one farmer and optimises farm profit, risk or nitrate leaching by selecting optimal crop 
# plan (activities) and farm machine and labour numbers, which can be either integers or otherwise

# The model was designed and built by Dr Kwadjo Ahodo.

RunSAFMOD <- function(modelChoice,wP,wN,wR,profitTarget,nleachTarget,riskTarget,steps,
                      farmArea,soilType,rain,intMachines,rotationBasis,monoCropping,fuelPrice,interestRate,inflation,
                      fertPrices,seedRates,seedPrices,BGHerb,BGHerbPrice,WOHerb,WOHerbPrice,primaryYieldPrices,deflators,
                      secondaryYieldPrice,subsidy,sbeetTransCost,otherCosts,machinePrices,annualLabCost,depRates,
                      repCostRates,replaceYears,historicalYields,modInputChange,paraVarAnalysis){  
 
  
  require("Rglpk") # The solver used to solve the mixed-integer model
  #require("Rsymphony") # An alternative solver
  require("pracma")
  
  # primaryYieldPrices = Crop Prices: Must be based on historical prices for 5 or more years for all the nine crops, to be able to estimate deviation in income
  
  
  # Set the soil type
  # The soil type influences
  # the fertiliser amounts, which in turn influence crop yields, gross margin,
  # variable costs, profits. 
  # The soil type also influences the workable hours which 
  # in turn affect timing of operations and machine/labour selection.
  
  soilType <- soilType
  soi <- c(0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5)
  
  if(soilType%in%soi==TRUE){
    soil <- soilType 
  }else{
    soil <- 2.5 # This sets the soil type to heavy soil (Clay)
    warning("Enter correct soil type: must be between 0.5 and 2.5 at an interval of 0.25")
  }
  
  # Crop input-output data ***************
  Input_Output <- c("Nfert.kg.ha","NfertPrice.£.kg","Pfert.kg.ha","PfertPrice.£.kg","Kfert.kg.ha","KfertPrice.£.kg",
                    "SeedAmount.kg.ha","SeedPrice.£.kg","BGHerb.litres.ha","BGHerbPrice.£.litre","WOHerb.litres.ha","WOHerbPrice.£.litre",
                    "PrimaryYield.t.ha","SecondaryYield.t.ha","PrimaryYieldPrice.£.t","SecondaryYieldPrice.£.t","Subsidy.£.ha","SbeetTransportYield.t.ha",
                    "SbeetTransportCost.£.ha","OtherCostsUnit","OtherCosts.£.ha","VariableCost.£.ha","Output.£.ha","GrossMargin.£.ha",
                    "MOTADRisk.£.ha","NitrateLeaching.kg.ha")
  
  WWHT <- SWHT <- WBAR <- SBAR <- WBEA <- SBEA <- WPOT <- WOSR <- SBEE <- SETA <- c(rep(0,26))
  da <- data.frame(Input_Output,WWHT,SWHT,WBAR,SBAR,WBEA,SBEA,WPOT,WOSR,SBEE,SETA)
  
  if(is.null(fertPrices)){ #Setting Fertilizer Prices
    # Default fertiliser prices
    da[2,seq(2,11)] <- 0.80; da[4,seq(2,11)] <- 0.63; da[6,seq(2,11)] <- 0.47
  }else{
    da[2,seq(2,11)] <- fertPrices[1]; da[4,seq(2,11)] <- fertPrices[2]; da[6,seq(2,11)] <- fertPrices[3]
  }
  
  if(is.null(seedRates)){ #Setting Seed Rates
    # Default Seed Rates
    da[7,seq(2,11)] <-c(185,195,175,175,200,225,2800,7,6,0)
  }else{
    da[7,seq(2,11)] <- seedRates
  }
  
  if(is.null(seedPrices)){ #Setting Seed Prices
    # Default Seed Prices
    da[8,seq(2,11)] <-c(0.40,0.38,0.37,0.38,0.41,0.43,0.25,7.37,91,0)
  }else{
    da[8,seq(2,11)] <- seedPrices
  }
  
  if(is.null(BGHerb)){ #Setting 
    # Default 
    da[9,seq(2,11)] <- 2.02 # Figure from the Farm R model
  }else{
    da[9,seq(2,11)] <- BGHerb
  }
  
  if(is.null(BGHerbPrice)){ #Setting 
    # Default 
    da[10,seq(2,11)] <- 0 # Set to zero because its cost was not estimated by default
  }else{
    da[10,seq(2,11)] <- BGHerbPrice
  }
  
  if(is.null(WOHerb)){ #Setting 
    # Default 
    da[11,seq(2,11)] <- 3.09 # From the Farm R model
  }else{
    da[11,seq(2,11)] <- WOHerb
  }
  
  if(is.null(WOHerbPrice)){ #Setting 
    # Default 
    da[12,seq(2,11)] <- 0 # Set to zero becuase its cost was not estimated by default
  }else{
    da[12,seq(2,11)] <- WOHerbPrice
  }
  
  if(is.null(secondaryYieldPrice)){ #Setting 
    # Default 
    da[16,seq(4,5)] <- 60 # Barley straw price
  }else{
    da[16,seq(4,5)] <- secondaryYieldPrice
  }
  
  if(is.null(subsidy)){ #Setting Subsidy: Basic Farm Payment
    # Default Subsidy
    da[17,seq(2,11)] <- 207 # 2014 Single Farm Payment from Nix (2014): Farm Management Pocketbook
  }else{
    da[17,seq(2,11)] <- subsidy
  }
  
  if(is.null(sbeetTransCost)){ #Setting Sugarbeet Transport Cost
    # Default Sugarbeet Transport Cost
    da[18,10] <- 65 # Consider automating by linking to yield function
    da[19,10] <- 5 # £5/t obtained from Nix (2014): Farm Management Pocketbook
  }else{
    da[18,10] <- 65 # Consider automating by linking to yield function
    da[19,10] <- sbeetTransCost
  }
  
  if(is.null(otherCosts)){ #Setting Other or Sundry Costs
    # Default Sundry Costs
    da[20,seq(2,11)] <- 1
    da[21,seq(2,11)] <- c(211,140,154,108,98,92,2262,193,220,30)
  }else{
    da[20,seq(2,11)] <- 1
    da[21,seq(2,11)] <- otherCosts
  }
  
  # Crop Data Table
  cropDataTable <- da
  cropInputOutputData <- da
  
  # Relative Change Table *********************************************************************************************
  
  relativeChange <- function(modInputChange){
    # Crop input-output data ***************
    Input_Output <- c("Nfert.kg.ha","NfertPrice.£.kg","Pfert.kg.ha","PfertPrice.£.kg","Kfert.kg.ha","KfertPrice.£.kg",
                      "SeedAmount.kg.ha","SeedPrice.£.kg","BGHerb.litres.ha","BGHerbPrice.£.litre","WOHerb.litres.ha","WOHerbPrice.£.litre",
                      "PrimaryYield.t.ha","SecondaryYield.t.ha","PrimaryYieldPrice.£.t","SecondaryYieldPrice.£.t","Subsidy.£.ha")
    
    WWHT <- SWHT <- WBAR <- SBAR <- WBEA <- SBEA <- WPOT <- WOSR <- SBEE <- SETA <- c(rep(1,length(Input_Output)))
    rg1 <- data.frame(Input_Output,WWHT,SWHT,WBAR,SBAR,WBEA,SBEA,WPOT,WOSR,SBEE,SETA)
    rg <- rg1[-1]
    rg2 <- rg1[-1]
    
    nfertiliser <- function(crop, per){
      if(crop=="winterwheat"){
        rg[1,1] <- rg[1,1]*per
      }else if(crop=="springwheat"){
        rg[1,2] <- rg[1,2]*per
      }else if(crop=="winterbarley"){
        rg[1,3] <- rg[1,3]*per
      }else if(crop=="springbarley"){
        rg[1,4] <- rg[1,4]*per
      }else if(crop=="winterbeans"){
        rg[1,5] <- rg[1,5]*per
      }else if(crop=="springbeans"){
        rg[1,6] <- rg[1,6]*per
      }else if(crop=="warepotatoes"){
        rg[1,7] <- rg[1,7]*per
      }else if(crop=="wosr"){
        rg[1,8] <- rg[1,8]*per
      }else if(crop=="sugarbeet"){
        rg[1,9] <- rg[1,9]*per
      }else if(crop=="setaside"){
        rg[1,10] <- rg[1,10]*per
      }else if(crop=="allcrops"){
        rg[1,] <- rg[1,]*per
      }else{rg[1,] <- 1}
      #rg
      rgs <- cbind(Inputs_Output=rg1[,1],rg)
    }
    
    pfertiliser <- function(crop, per){
      if(crop=="winterwheat"){
        rg[3,1] <- rg[3,1]*per
      }else if(crop=="springwheat"){
        rg[3,2] <- rg[3,2]*per
      }else if(crop=="winterbarley"){
        rg[3,3] <- rg[3,3]*per
      }else if(crop=="springbarley"){
        rg[3,4] <- rg[3,4]*per
      }else if(crop=="winterbeans"){
        rg[3,5] <- rg[3,5]*per
      }else if(crop=="springbeans"){
        rg[3,6] <- rg[3,6]*per
      }else if(crop=="warepotatoes"){
        rg[3,7] <- rg[3,7]*per
      }else if(crop=="wosr"){
        rg[3,8] <- rg[3,8]*per
      }else if(crop=="sugarbeet"){
        rg[3,9] <- rg[3,9]*per
      }else if(crop=="setaside"){
        rg[3,10] <- rg[3,10]*per
      }else if(crop=="allcrops"){
        rg[3,] <- rg[3,]*per
      }else{rg[3,] <- 1}
      #rg
      rgs <- cbind(Inputs_Output=rg1[,1],rg)
    }
    
    kfertiliser <- function(crop, per){
      if(crop=="winterwheat"){
        rg[5,1] <- rg[5,1]*per
      }else if(crop=="springwheat"){
        rg[5,2] <- rg[5,2]*per
      }else if(crop=="winterbarley"){
        rg[5,3] <- rg[5,3]*per
      }else if(crop=="springbarley"){
        rg[5,4] <- rg[5,4]*per
      }else if(crop=="winterbeans"){
        rg[5,5] <- rg[5,5]*per
      }else if(crop=="springbeans"){
        rg[5,6] <- rg[5,6]*per
      }else if(crop=="warepotatoes"){
        rg[5,7] <- rg[5,7]*per
      }else if(crop=="wosr"){
        rg[5,8] <- rg[5,8]*per
      }else if(crop=="sugarbeet"){
        rg[5,9] <- rg[5,9]*per
      }else if(crop=="setaside"){
        rg[5,10] <- rg[5,10]*per
      }else if(crop=="allcrops"){
        rg[5,] <- rg[5,]*per
      }else{rg[5,] <- 1}
      #rg
      rgs <- cbind(Inputs_Output=rg1[,1],rg)
    }
    
    seed <- function(crop, per){
      if(crop=="winterwheat"){
        rg[7,1] <- rg[7,1]*per
      }else if(crop=="springwheat"){
        rg[7,2] <- rg[7,2]*per
      }else if(crop=="winterbarley"){
        rg[7,3] <- rg[7,3]*per
      }else if(crop=="springbarley"){
        rg[7,4] <- rg[7,4]*per
      }else if(crop=="winterbeans"){
        rg[7,5] <- rg[7,5]*per
      }else if(crop=="springbeans"){
        rg[7,6] <- rg[7,6]*per
      }else if(crop=="warepotatoes"){
        rg[7,7] <- rg[7,7]*per
      }else if(crop=="wosr"){
        rg[7,8] <- rg[7,8]*per
      }else if(crop=="sugarbeet"){
        rg[7,9] <- rg[7,9]*per
      }else if(crop=="setaside"){
        rg[7,10] <- rg[7,10]*per
      }else if(crop=="allcrops"){
        rg[7,] <- rg[7,]*per
      }else{rg[7,] <- 1}
      #rg
      rgs <- cbind(Inputs_Output=rg1[,1],rg)
    }
    
    cropyield <- function(crop, per){
      if(crop=="winterwheat"){
        rg[13,1] <- rg[13,1]*per
      }else if(crop=="springwheat"){
        rg[13,2] <- rg[13,2]*per
      }else if(crop=="winterbarley"){
        rg[13,3] <- rg[13,3]*per
      }else if(crop=="springbarley"){
        rg[13,4] <- rg[13,4]*per
      }else if(crop=="winterbeans"){
        rg[13,5] <- rg[13,5]*per
      }else if(crop=="springbeans"){
        rg[13,6] <- rg[13,6]*per
      }else if(crop=="warepotatoes"){
        rg[13,7] <- rg[13,7]*per
      }else if(crop=="wosr"){
        rg[13,8] <- rg[13,8]*per
      }else if(crop=="sugarbeet"){
        rg[13,9] <- rg[13,9]*per
      }else if(crop=="setaside"){
        rg[13,10] <- rg[13,10]*per
      }else if(crop=="allcrops"){
        rg[13,] <- rg[13,]*per
      }else{rg[1,] <- 1}
      #rg
      rgs <- cbind(Inputs_Output=rg1[,1],rg)
    }
    
    nfertiliserprice <- function(crop, per){
      if(crop=="allcrops"){
        rg[2,] <- rg[2,]*per
      }else{rg[2,] <- 1}
      rg
      rgs <- cbind(Inputs_Output=rg1[,1],rg)
    }
    
    pfertiliserprice <- function(crop, per){
      if(crop=="allcrops"){
        rg[4,] <- rg[4,]*per
      }else{rg[4,] <- 1}
      rg
      rgs <- cbind(Inputs_Output=rg1[,1],rg)
    }
    
    kfertiliserprice <- function(crop, per){
      if(crop=="allcrops"){
        rg[6,] <- rg[6,]*per
      }else{rg[6,] <- 1}
      #rg
      rgs <- cbind(Inputs_Output=rg1[,1],rg)
    }
    
    seedprice <- function(crop, per){
      if(crop=="winterwheat"){
        rg[8,1] <- rg[8,1]*per
      }else if(crop=="springwheat"){
        rg[8,2] <- rg[8,2]*per
      }else if(crop=="winterbarley"){
        rg[8,3] <- rg[8,3]*per
      }else if(crop=="springbarley"){
        rg[8,4] <- rg[8,4]*per
      }else if(crop=="winterbeans"){
        rg[8,5] <- rg[8,5]*per
      }else if(crop=="springbeans"){
        rg[8,6] <- rg[8,6]*per
      }else if(crop=="warepotatoes"){
        rg[8,7] <- rg[8,7]*per
      }else if(crop=="wosr"){
        rg[8,8] <- rg[8,8]*per
      }else if(crop=="sugarbeet"){
        rg[8,9] <- rg[8,9]*per
      }else if(crop=="setaside"){
        rg[8,10] <- rg[8,10]*per
      }else if(crop=="allcrops"){
        rg[8,] <- rg[8,]*per
      }else{rg[8,] <- 1}
      #rg
      rgs <- cbind(Inputs_Output=rg1[,1],rg)
    }
    
    cropprice <- function(crop, per){
      if(crop=="winterwheat"){
        rg[15,1] <- rg[15,1]*per
      }else if(crop=="springwheat"){
        rg[15,2] <- rg[15,2]*per
      }else if(crop=="winterbarley"){
        rg[15,3] <- rg[15,3]*per
      }else if(crop=="springbarley"){
        rg[15,4] <- rg[15,4]*per
      }else if(crop=="winterbeans"){
        rg[15,5] <- rg[15,5]*per
      }else if(crop=="springbeans"){
        rg[15,6] <- rg[15,6]*per
      }else if(crop=="warepotatoes"){
        rg[15,7] <- rg[15,7]*per
      }else if(crop=="wosr"){
        rg[15,8] <- rg[15,8]*per
      }else if(crop=="sugarbeet"){
        rg[15,9] <- rg[15,9]*per
      }else if(crop=="setaside"){
        rg[15,10] <- rg[15,10]*per
      }else if(crop=="allcrops"){
        rg[15,] <- rg[15,]*per
      }else{rg[15,] <- 1}
      #rg
      rgs <- cbind(Inputs_Output=rg1[,1],rg)
    }
    
    subsidy <- function(crop, per){
      if(crop=="allcrops"){
        rg[17,] <- rg[17,]*per
      }else{rg[17,] <- 1}
      #rg
      rgs <- cbind(Inputs_Output=rg1[,1],rg)
    }
    
    inpt <- c("nfertiliser","pfertiliser","kfertiliser","seed","cropyield",
              "nfertiliserprice","pfertiliserprice","kfertiliserprice",
              "seedprice","cropprice","subsidy")
    
    inputType <- modInputChange[1]
    crop <- modInputChange[2]
    per <- as.numeric(modInputChange[3])
    
    if(inputType%in%inpt==TRUE){
      kk<- switch(inputType,
                  nfertiliser = nfertiliser(crop, per),
                  pfertiliser = pfertiliser(crop, per),
                  kfertiliser = kfertiliser(crop, per),
                  seed = seed(crop, per),
                  cropyield = cropyield(crop, per),
                  nfertiliserprice = nfertiliserprice(crop, per),
                  pfertiliserprice = pfertiliserprice(crop, per),
                  kfertiliserprice = kfertiliserprice(crop, per),
                  seedprice = seedprice(crop, per),
                  cropprice = cropprice(crop, per),
                  subsidy = subsidy(crop, per))
      
    }else if(inputType%in%inpt==FALSE){
      kk <- rg1
      #warning("InputType NOT spelt correctly (MUST be small letters)")
    }
    
  }
  
  #rc <- relativeChange(modInputChange)
  
  # ==========FERTILISER AMOUNT FUNCTION====================
  
  soilFertiliser <- function(soil){
    
    # The soilFertiliser function chooses the fertiliser amounts based on soil type
    # Thus any change in the soil type changes the fertiliser amounts required by crops
    # The recommended fertilier amounts were based on recommended amounts by Defra.
    
    # N Fertiliser Requirements by Crops & Setaside
    Soil.0.5.N <- Soil.0.75.N <- Soil.1.0.N <- c(130,130,120,80,0,0,220,190,100,0)
    Soil.1.25.N <-  Soil.1.50.N <- Soil.1.75.N <- Soil.2.0.N <- Soil.2.25.N <- Soil.2.5.N <- c(220,180,170,140,0,0,220,190,100,0)
    
    # P Fertiliser Requirements by Crops & Setaside
    Soil.0.5.P <- Soil.0.75.P <- Soil.1.0.P <- Soil.1.25.P <- Soil.1.50.P <- Soil.1.75.P <- 
      Soil.2.0.P <- Soil.2.25.P <- Soil.2.5.P <- c(95,80,95,80,70,70,210,80,80,0)
    
    # K Fertiliser Requirements by Crops & Setaside
    Soil.0.5.K <- Soil.0.75.K <- Soil.1.0.K <- Soil.1.25.K <-  Soil.1.50.K <- 
      Soil.1.75.K <- Soil.2.0.K <- Soil.2.25.K <- Soil.2.5.K <- c(115,100,115,100,70,70,330,70,130,0)
    
    newfi <- cropDataTable # Crop Data Table
    rc <- relativeChange(modInputChange)[-1]
    
    if(missing(soil)){
      warning("soil argument missing")
    }else{
      soi <- soil
      
      if(soi==0.5){
        newfi[1,seq(2,11)] <- Soil.0.5.N*rc[1,]
        newfi[3,seq(2,11)] <- Soil.0.5.P*rc[3,]
        newfi[5,seq(2,11)] <- Soil.0.5.K*rc[5,]
      }else if(soi==0.75){
        newfi[1,seq(2,11)] <- Soil.0.75.N*rc[1,]
        newfi[3,seq(2,11)] <- Soil.0.75.P*rc[3,]
        newfi[5,seq(2,11)] <- Soil.0.75.K*rc[5,]
      }else if(soi==1){
        newfi[1,seq(2,11)] <- Soil.1.0.N*rc[1,]
        newfi[3,seq(2,11)] <- Soil.1.0.P*rc[3,]
        newfi[5,seq(2,11)] <- Soil.1.0.K*rc[5,]
      }else if(soi==1.25){
        newfi[1,seq(2,11)] <- Soil.1.25.N*rc[1,]
        newfi[3,seq(2,11)] <- Soil.1.25.P*rc[3,]
        newfi[5,seq(2,11)] <- Soil.1.25.K*rc[5,]
      }else if(soi==1.5){
        newfi[1,seq(2,11)] <- Soil.1.50.N*rc[1,]
        newfi[3,seq(2,11)] <- Soil.1.50.P*rc[3,]
        newfi[5,seq(2,11)] <- Soil.1.50.K*rc[5,]
      }else if(soi==1.75){
        newfi[1,seq(2,11)] <- Soil.1.75.N*rc[1,]
        newfi[3,seq(2,11)] <- Soil.1.75.P*rc[3,]
        newfi[5,seq(2,11)] <- Soil.1.75.K*rc[5,]
      }else if(soi==2){
        newfi[1,seq(2,11)] <- Soil.2.0.N*rc[1,]
        newfi[3,seq(2,11)] <- Soil.2.0.P*rc[3,]
        newfi[5,seq(2,11)] <- Soil.2.0.K*rc[5,]
      }else if(soi==2.25){
        newfi[1,seq(2,11)] <- Soil.2.25.N*rc[1,]
        newfi[3,seq(2,11)] <- Soil.2.25.P*rc[3,]
        newfi[5,seq(2,11)] <- Soil.2.25.K*rc[5,]
      }else if(soi==2.5){
        newfi[1,seq(2,11)] <- Soil.2.5.N*rc[1,]
        newfi[3,seq(2,11)] <- Soil.2.5.P*rc[3,]
        newfi[5,seq(2,11)] <- Soil.2.5.K*rc[5,]
      }else {
        # Sets the default to 2.5 (heavy soil)
        newfi[1,seq(2,11)] <- Soil.2.5.N*rc[1,]
        newfi[3,seq(2,11)] <- Soil.2.5.P*rc[3,]
        newfi[5,seq(2,11)] <- Soil.2.5.K*rc[5,]
      }
    }
    ka <- newfi
  }
  
  #================================ ESTIMATE THE CROP YIELD ============================
  
  cropYield <- function(soil){
    
    # This function estimates the crop yields for the crops using equations
    # which takes into consideration N fertiliser amounts and soil types
    # For some of the crops such as beans only the soil types is used to estimate the yield
    # whereas for other such as sugar beet and potatoes only the N fertiliser amounts were used
    # The equations were adopted from the farmR model
    
    if(missing(soil)){
      warning("soil argument missing")
    }else{
      # rc file constians relative change in model parameters 
      # used when carrying out sensitivity analysis and hence why it is attached to yield estimates
      
      fi1 <- soilFertiliser(soil) # Select fertiliser amounts using soil types
      rc <- relativeChange(modInputChange)[-1]
      
      if(is.null(historicalYields)){
        
        # The formulae below estimate the primary and secondary yields of crops
        wwhtp <- (11.841-(9.211*(0.9907^(fi1$WWHT[1])))-(0.0075*(fi1$WWHT[1])))*(0.743+0.1714*(soil))*rc[13,1]
        swhtp <- (5.885-(2.893*(0.984^(fi1$SWHT[1]))))*(0.73+0.18*(soil))*rc[13,2]
        wbarp <- (((12.967-(10.029*(0.993^(fi1$WBAR[1])))-(0.0147*(fi1$WBAR[1])))*(0.76+0.16*(soil)))*0.94)*rc[13,3]
        sbarp <- (((19.98-(18.164*(0.9952^(fi1$SBAR[1])))-(0.0364*(fi1$SBAR[1])))*(0.887+0.075*(soil)))*1.04)*rc[13,4]
        wbeap <- (((0.95+1.3625*(soil))*1.1)*1.2)*rc[13,5]
        sbeap <- (((0.7+1.25*(soil))*1.05)*1.2)*rc[13,6]
        wosrp <- (((3.35+(0.0-0.623*(0.010^(fi1$WOSR[1])))-0.000324*(fi1$WOSR[1]))*(0.655+0.23*(soil)))*0.796)*rc[13,7]
        wpotp <- (44.507-(29.135*(0.992^fi1$WPOT[1])))*rc[13,8]
        sbeep <- (54.543-(0.05*37.82*(0.984^fi1$SBEE[1])))*1.205*rc[13,9]
        setap <- 0
        
        fi1[13,-1] <- round(c(wwhtp,swhtp,wbarp,sbarp,wbeap,sbeap,wpotp,wosrp,sbeep,setap),2)
        
        # The secondary yields were assumed to be maily baled straw from wheat and barley crops. 
        wwhts <- wwhtp*0.5*rc[13,1]
        swhts <- swhtp*0.5*rc[13,2]
        wbars <- wbarp*0.5*rc[13,3]
        sbars <- sbarp*0.5*rc[13,4]
        wbeas <- 0
        sbeas <- 0
        wpots <- 0
        wosrs <- 0
        sbees <- 0
        setas <- 0
        
        fi1[14,-1] <- round(c(wwhts,swhts,wbars,sbars,wbeas,sbeas,wpots,wosrs,sbees,setas),2)
        fi1$SBEE[18] <- round(sbeep,2)
        # The yield data will be updated with changes in soil types
        newcp <- fi1
        #newcp
        #write.table(newcp,file="Crop_input_output.csv",row.names=FALSE,sep=",") 
        #cp1 <- newcp
        #kk <- newcp
        
      }else{
        # Estimating yields based on historical data
        yds <- apply(historicalYields,2,mean) # Estimating the mean yield based on historical data
        
        wwhtp <- yds[1]*rc[13,1]; swhtp <- yds[2]*rc[13,2]; wbarp <- yds[3]*rc[13,3]
        sbarp <- yds[4]*rc[13,4]; wbeap <- yds[5]*rc[13,5]; sbeap <- yds[6]*rc[13,6]
        wosrp <- yds[7]*rc[13,7]; wpotp <- yds[8]*rc[13,8]; sbeep <- yds[9]*rc[13,9]
        setap <- 0
        
        fi1[13,-1] <- round(c(wwhtp,swhtp,wbarp,sbarp,wbeap,sbeap,wpotp,wosrp,sbeep,setap),2)
        
        # The secondary yields were assumed to be maily baled straw from wheat and barley crops. 
        wwhts <- wwhtp*0.5*rc[13,1]; swhts <- swhtp*0.5*rc[13,2]; wbars <- wbarp*0.5*rc[13,3]
        sbars <- sbarp*0.5*rc[13,4]; wbeas <- 0; sbeas <- 0; wpots <- 0; wosrs <- 0
        sbees <- 0; setas <- 0
        
        fi1[14,-1] <- round(c(wwhts,swhts,wbars,sbars,wbeas,sbeas,wpots,wosrs,sbees,setas),2)
        fi1$SBEE[18] <- round(sbeep,2)
        # The yield data will be updated with changes in soil types
        newcp <- fi1
        #newcp
        #write.table(newcp,file="Crop_input_output.csv",row.names=FALSE,sep=",") 
        #cp1 <- newcp
        #kk <- newcp
      }
    }
  }
  
  #========== VARIABLE COST ESTIMATES ============
  
  variableCost <- function (soil){ 
    
    # This function estimates the variable cost based on soil type which influence fertiliser 
    # amounts and hence varaible costs
    
    if(missing(soil)){
      warning("soil argument missing")
    }else{
      vc <- cropYield(soil)
      rc <- relativeChange(modInputChange)[-1]
      
      # Transport cost for Sugar beet to a sugar factory: 30 km @ 
      nfert <- vc[1,-1]*rc[1,] * vc[2,-1]*rc[2,] # N fertiliser cost 
      pfert <- vc[3,-1]*rc[3,] * vc[4,-1]*rc[4,] # P fertiliser
      kfert <- vc[5,-1]*rc[5,] * vc[6,-1]*rc[6,] # K fertiliser cost
      seed  <- vc[7,-1]*rc[7,] * vc[8,-1]*rc[8,] # Seed cost
      
      # Application rates were not used: standard cost estimates were used
      # Future extension can take this into consideration
      bgherb <- vc[9,-1]*rc[9,] * vc[10,-1]*rc[10,] # Black grass herbicide cost (set to zero at the moment)
      woherb <- vc[11,-1]*rc[11,] * vc[12,-1]*rc[12,] # Wild oats herbicide cost (set to zero at the moment)
      
      # The transport cost for sugar beet was estimated based on yield and a cost of
      # £5/t obtained from Nix (2014): Farm Management Pocketbook
      trans <- vc[18,-1] * vc[19,-1] 
      # Sundry cost estimates (included chemical costs) were also obtained from
      # Nix (2014)
      sund <- vc[20,-1] * vc[21,-1] # Estimates the sundries cost per ha
      
      vc[22,-1] <- nfert + pfert + kfert + seed + bgherb + woherb + trans + sund
      newvc <- vc # This input data and variable cost data 
    }
  }
  
  #========================== ESTIMATE FARM OUTPUT BASED ON YIELD ================================
  
  outputYield <- function(soil){
    
    # This function estimates the farm output (£/ha) based on soil type which changes in crop yield (t/ha).
    # It also estimates the gross margin (£/ha)
    # The MOTAD risk deviations and standard deviations are estimated by this function
    # The subsidy amount (£/ha) is based on the flat rate for lowland farmers in England @ £207/ha
    
    if(is.null(deflators)){
      defs <- c(1,1,1,1,1)
    }else{
      defs <- deflators
    }
    
    if(missing(soil)){
      warning("soil argumnent missing")
    }else{
      
      cy <- variableCost(soil) # Changes in soil type re-estimates the yield and variable costs
      #ip <- variableCost(fs$Value[[2]]) # Updates the variable cost data
      rc <- relativeChange(modInputChange)[-1] # Relative Change Table
      
      if(is.null(primaryYieldPrices)){
        # Default prices: 5 year historical prices, 2010-2014
        wwht <- c(128,172,175,188,153)*rc[15,1]
        swht <- c(128,172,175,188,153)*rc[15,2]
        wbar <- c(112,163,180,166,134)*rc[15,3]
        sbar <- c(112,163,180,166,134)*rc[15,4]
        wbea <- c(109,133,147,181,142)*rc[15,5]
        sbea <- c(109,133,147,181,142)*rc[15,6]
        wpot <- c(152,156,174,207,148)*rc[15,7]
        wosr <- c(329,426,401,355,278)*rc[15,8]
        sbee <- c(33,32,32,33,34)*rc[15,9]
        pr1 <- data.frame(wwht,swht,wbar,sbar,wbea,sbea,wpot,wosr,sbee)*defs #defs = deflators
      }else{
        #pr1 <- primaryYieldPrices # Crop price must be 5 year historical prices for the nine crops
        # Model will be updated to allow for more years
        wwht <- primaryYieldPrices[,1]*rc[15,1]
        swht <- primaryYieldPrices[,2]*rc[15,2]
        wbar <- primaryYieldPrices[,3]*rc[15,3]
        sbar <- primaryYieldPrices[,4]*rc[15,4]
        wbea <- primaryYieldPrices[,5]*rc[15,5]
        sbea <- primaryYieldPrices[,6]*rc[15,6]
        wpot <- primaryYieldPrices[,7]*rc[15,7]
        wosr <- primaryYieldPrices[,8]*rc[15,8]
        sbee <- primaryYieldPrices[,9]*rc[15,9]
        pr1 <- data.frame(wwht,swht,wbar,sbar,wbea,sbea,wpot,wosr,sbee)*defs #defs = deflators
      }
      
      #setwd("~/Pte Docs/SAFMOD/Google Drive/SAFMOD/Mod_Data")
      #se <- read.csv("Self_Rot_Pen.csv") # File for self rotation penalty
      nac <- 10
      vl <- 5
      crp <- 13
      
      # Update the Crop_input_output file with crop prices
      cy[15,seq(2,10)] <- round(apply(pr1,2,mean))
      
      
      # Estimation of crop output
      # Farm output = (primary_yield * primary_price) + (secondary yield * secondary price) 
      
      pr <- pr1
      
      if(is.null(historicalYields)){
        
        vcp <- 1
        vcw <- 1
        vcwo <- 1
        
        out <- matrix(rep(0,nac*vl),ncol=nac)
        out[,1] <- rc[15,1]*pr[,1]*cy$WWHT[13]+(cy$WWHT[14]*cy$WWHT[16])
        out[,2] <- rc[15,2]*pr[,2]*cy$SWHT[13]+(cy$SWHT[14]*cy$SWHT[16]) 
        out[,3] <- rc[15,3]*pr[,3]*cy$WBAR[13]+(cy$WBAR[14]*cy$WBAR[16]) 
        out[,4] <- rc[15,4]*pr[,4]*cy$SBAR[13]+(cy$SBAR[14]*cy$SBAR[16]) 
        out[,5] <- rc[15,5]*pr[,5]*cy$WBEA[13]+(cy$WBEA[14]*cy$WBEA[16])
        out[,6] <- rc[15,6]*pr[,6]*cy$SBEA[13]+(cy$SBEA[14]*cy$SBEA[16]) 
        out[,7] <- rc[15,7]*pr[,7]*cy$WPOT[13]+(cy$WPOT[14]*cy$WPOT[16]) 
        out[,8] <- rc[15,8]*pr[,8]*cy$WOSR[13]+(cy$WOSR[14]*cy$WOSR[16]) 
        out[,9] <- rc[15,9]*pr[,9]*cy$SBEE[13]+(cy$SBEE[14]*cy$SBEE[16]) 
        out[,10] <- cy$SETA[17]
        nout <- round(out) 
        
      }else{
        hy <- historicalYields
        cy[15,seq(2,10)] <- round(apply(hy,2,mean))
        cy[16,seq(2,5)] <- round(apply(hy,2,mean))[seq(1,4)]*0.5
        
        vcp <- 0.88
        vcw <- 1#0.87
        vcwo <- 0.95
        
        out <- matrix(rep(0,nac*vl),ncol=nac)
        out[,1] <- rc[15,1]*pr[,1]*hy[,1]*rc[13,1]+(cy$WWHT[16]*rc[16,1]*hy[,1]*rc[14,1]*0.5)
        out[,2] <- rc[15,2]*pr[,2]*hy[,2]*rc[13,2]+(cy$SWHT[16]*rc[16,2]*hy[,2]*rc[14,2]*0.5) 
        out[,3] <- rc[15,3]*pr[,3]*hy[,3]*rc[13,3]+(cy$WBAR[16]*rc[16,3]*hy[,3]*rc[14,3]*0.5) 
        out[,4] <- rc[15,4]*pr[,4]*hy[,4]*rc[13,4]+(cy$SBAR[16]*rc[16,4]*hy[,4]*rc[14,4]*0.5) 
        out[,5] <- rc[15,5]*pr[,5]*hy[,5]*rc[13,5]+(cy$WBEA[16]*rc[16,5]*hy[,5]*rc[14,5]*0.5)
        out[,6] <- rc[15,6]*pr[,6]*hy[,6]*rc[13,6]+(cy$SBEA[16]*rc[16,6]*hy[,6]*rc[14,6]*0.5) 
        out[,7] <- rc[15,7]*pr[,7]*hy[,7]*rc[13,7]+(cy$WPOT[16]*rc[16,7]*hy[,7]*rc[14,7]*0.5) 
        out[,8] <- rc[15,8]*pr[,8]*hy[,8]*rc[13,8]+(cy$WOSR[16]*rc[16,8]*hy[,8]*rc[14,8]*0.5) 
        out[,9] <- rc[15,9]*pr[,9]*hy[,9]*rc[13,9]+(cy$SBEE[16]*rc[16,9]*hy[,9]*rc[14,9]*0.5) 
        out[,10] <- cy$SETA[17]
        nout <- round(out) 
      }
      
      
      mouts <- apply(nout,2,mean)
      
      cy[23,seq(2,11)] <- mouts 
      
      
      # Gross Margin = (Farm output (£/ha) - Variable cost (£/ha)) + subsidy
      nout[,1] <- (nout[,1]-(cy$WWHT[22]*vcw))+cy$WWHT[17];
      nout[,2] <- (nout[,2]-cy$SWHT[22])+cy$SWHT[17]
      nout[,3] <- (nout[,3]-cy$WBAR[22])+cy$WBAR[17]
      nout[,4] <- (nout[,4]-cy$SBAR[22])+cy$SBAR[17]
      nout[,5] <- (nout[,5]-cy$WBEA[22])+cy$WBEA[17]
      nout[,6] <- (nout[,6]-cy$SBEA[22])+cy$SBEA[17]
      nout[,7] <- (nout[,7]-(cy$WPOT[22]*vcp))+cy$WPOT[17]
      nout[,8] <- vcwo*((nout[,8]-cy$WOSR[22])+cy$WOSR[17])
      nout[,9] <- (nout[,9]-cy$SBEE[22])+cy$SBEE[17]
      nout[,10] <- nout[,10]-cy$SETA[22]
      nouts <- nout 
      
      pout <- round(apply(nouts,2,mean))
      
      cy[24,seq(2,11)] <- pout;
      round(apply(pr1,2,mean))
      
      #=MOTAD RISK ESTIMATES (Standard deviation estimates for the goal prog model)===
      sdm <- round(apply(nouts,2,sd))
      cy[25,seq(2,11)] <- sdm
      newcy <- cy
      
      # Estimation of the gross margin for the four variants of the winter wheat crops
      mat <- matrix(rep(0,crp*vl),ncol=crp)
      
      # Self rotation penalties 
      yieldlosscost <- round((c(11,11,15)*0.01*cy$WWHT[13]*cy$WWHT[15])+20) # Yield loss cost
      bgcost <- c(1,2,3)*cy$WWHT[9]*cy$WWHT[10] # Extra BG herb cost
      wocost <- c(2,2,3)*cy$WWHT[11]*cy$WWHT[12] # Extra WO herb cost
      selfrotcost <- yieldlosscost+bgcost+wocost
      
      mat[,1] <- nouts[,1]; mat[,2] <- nouts[,1]-selfrotcost[1];
      mat[,3] <- nouts[,1]-selfrotcost[2];mat[,4] <- nouts[,1]-selfrotcost[3];
      mat[,seq(5,13)] <- nouts[,seq(2,10)]
      nmat <- mat
      
      # Saves the returns for the five-year periods for each of the crops
      #write.table(nmat ,file="Returns.csv",row.names=FALSE,sep=",")
      
      # Estimation of the income deviation for the MOTAD model based on historical prices
      nmats <- round(apply(nmat,2,mean))
      nmat[,1] <- nmat[,1]-nmats[1]; nmat[,2] <- nmat[,2]-nmats[2]
      nmat[,3] <- nmat[,3]-nmats[3]; nmat[,4] <- nmat[,4]-nmats[4]
      nmat[,5] <- nmat[,5]-nmats[5]; nmat[,6] <- nmat[,6]-nmats[6]
      nmat[,7] <- nmat[,7]-nmats[7]; nmat[,8] <- nmat[,8]-nmats[8]
      nmat[,9] <- nmat[,9]-nmats[9]; nmat[,10] <- nmat[,10]-nmats[10];
      nmat[,11] <- nmat[,11]-nmats[11];nmat[,12] <- nmat[,12]-nmats[12];
      nmat[,13] <- nmat[,13]-nmats[13];
      newmat <- nmat
      
      #write.table(newmat ,file="Mot_Dev.csv",row.names=FALSE,sep=",")
      
      # Saves the updated Crop_input_output data
      #write.table(newcy ,file="Crop_input_output.csv",row.names=FALSE,sep=",")
      kk <- list(newcy,newmat) 
    }
  }
  
  #======================== NITRATE LEACHING ESTIMATES ======================================================
  
  nitrateLeaching <- function(soil){
    
    # This function estimates the nitrate leaching with respect to crops based 
    # on the N fertiliser balanced approach
    
    if(missing(soil)){
      warning("soil argument missing")
    }else{
      
      oy <- outputYield(soil)[[1]] # Update the crop data
      
      #=== Atmospheric deposition (kg N/ha/year)====
      # Atmospheric deposition is the values of N deposition values 
      # for 187 arable fields
      atmos <- 20
      
      # Soil N Supply (SNS) kg/ha
      SNS <- 80
      
      # Assumed N fixed by legumes
      nfix <- 285
      
      # N applied 
      N_fert <- oy[1,seq(2,11)]
      N_fert[5:6] <- nfix
      
      totalN <-  N_fert + atmos + SNS
      
      # Volatilisation of ammonia (NH3) and nitrous oxide (N2O)
      vf1 <- 2/100
      vf2 <- 0.5/100
      
      # N uptake = totalN times N uptake efficiency
      # N uptake efficiency data were obtained from work by Prof Sylvester_Bradley
      # It is possible other experiment may find different N uptakes
      nup <- c(0.65,0.68,0.54,0.39,0.51,0.51,0.81,0.85,1.07,0)
      
      Nuptake <- totalN * nup
      
      volNH3 <- totalN * vf1
      volN2O <- totalN * vf2
      
      # Estimates of Nleaching as 30% of total N balance
      Nleach <- 0.3*(totalN -  Nuptake- volNH3 - volN2O)
      nnl <- Nleach 
      oy[26,seq(2,11)] <- nnl
      newoy <- oy
    }
  }
  
  #=================================== CROP DATA ===========================================
  
  cropData <- function(soil){ 
    
    # This functions puts variable costs, outputs, gross margin, MOTAD risk and nitrate leaching by crops together
    # It also produces the income deviation matrix for the MOTAD model
    
    if(missing(soil)){
      warning("soil argument missing")
    }else{
      
      oy <- outputYield(soil) # Updates Output and margin estimates
      nl <- nitrateLeaching(soil) # Nitrate leaching
      oy[[1]][26,seq(2,11)] <- round(nl[26,seq(2,11)],1)  
      newoy <- oy  
      
      kk <- newoy 
    } 
  }
  
  #======================= WORKABLE HOURS ===================================
  
  workableHoursCal <-  function(Ad1, Ad2, soil, rain){
    
    # This function estimates the workable hours  
    # using soil type, rainfall ad the day number
    # Ad1 = day number at the beggining of period
    # Ad2 = day number at the end of period
    
    # The function was adopted from the farmR and SFARMOD models but was adapted.
    
    Lhrday <- 9.95 # Assumed average hours per day in summer
    
    if(missing (soil)||missing(rain)){
      warning("soil or rain argument missing")
    }else{ 
      
      # breakpoint constants
      b1 <- 113 # part of formula W=9.95-(0.5075-0.00253*LTI)*(113-d) where d=day number
      b2 <- 212 # break point for start August 212=31st July
      b3 <- 365 # break point for end of year
      ds <- Ad1 - 0.5
      df <- Ad2 - 0.5
      
      # Land Type Indicator (LTI)
      x <- (1.257 - 0.257 * soil) * (rain * 0.001) + 0.762 * (soil - 1)-0.1
      
      if(rain < 500){
        x <- x - 0.005 * (500 - rain)
      }
      # At this point x is on the order 1-2
      LTI <- 20.6*x*x - 89*x + 212
      
      isbeta <- (0.5075 - 0.00253 * LTI)
      if( isbeta < 0.01) 
        isbeta <- 0.01
      
      IDONE <- 0
      HHH <- 0
      
      # Setup start and finish days for the period. 
      # Use periodic boundaries conditions
      ds <- Ad1 - 0.5
      if(ds > b3) ds <- ds - b3
      
      df <- Ad2 - 0.5
      if(df > b3) df <- df - b3  
      #===============================================
      
      calWkhrs <- function(SD, FD, ALPHA, BETA){
        
        kka <- ALPHA + BETA * SD
        kkb <- ALPHA + BETA * FD
        if (kka <= 0) 
          SD <- -ALPHA / BETA
        if (kkb <= 0) 
          FD <- -ALPHA / BETA
        
        h <- (FD - SD) * (ALPHA + BETA * (FD + SD) / 2)
        if (h < 0) h <- 0
        HHH <- 0
        HHH <- HHH + h
        
        # Multiply by factor of 0.85 for unproductive time etc (Adopted from farmR). 
        Wkhrs <- round(HHH * 0.85,1)
        Wkhrs
      }
      
      # January to April
      Jan_April <- function(){
        
        isbeta <- (0.5075 - 0.00253 * LTI)
        if( isbeta < 0.01) 
          isbeta <- 0.01
        
        SD <- ds # SD <- Ad1 - 0.5
        if (df > b1) { 
          FD <- b1 - 0.5
          ds <- b1 - 0.5
        } else {
          FD <- df # FD <- Ad2 - 0.5
          IDONE = 1
        }
        
        BETA <- isbeta
        ALPHA <- Lhrday - b1 * BETA
        return(calWkhrs(SD,FD,ALPHA,BETA))
      }
      
      # May to July
      May_July <- function(){
        SD <- ds
        if (df < b1 - 1 || df > b2 + 1){
          FD <- b2 + 0.5
          ds <- b2 + 0.5 
        } else {
          FD <- df
          IDONE = 1
        }
        
        BETA <- 0
        ALPHA <- Lhrday
        return(calWkhrs(SD,FD,ALPHA,BETA))
      }
      
      # August to December
      August_Dec <- function(){
        if (ds < b1 - 1)  return(Jan_April())
        if (ds < b2)  return(May_July())
        
        SD <- ds - b2
        if (df < b2) { 
          FD <- b3 - 0.5 - b2
          ds <- 0.5
        } else {
          IDONE = 1
          FD <- df - b2
        }
        
        was <- 1.29 * LTI - 155
        
        if (SD > was) {
          BETA <- -(9.95 - isbeta * was / 10) / (LTI - was)
          ALPHA <- -BETA * LTI
        } else {
          ALPHA <- Lhrday;
          BETA <- -isbeta / 10;
        }
        return(calWkhrs(SD,FD,ALPHA,BETA))
      }
      
      begin <- function(){
        # Part of the firstBit function in the farmR model
        IDONE = 0
        ds <- Ad1 - 0.5
        if (ds > b3) ds <- ds - b3
        
        df <- Ad2 - 0.5
        if (df > b3) df <- df - b3;
        return(August_Dec())
      }
      
      if(IDONE == 0) return(August_Dec())
      
      #idone <- function(){
      #  if(IDONE==0) return(August_Dec())
      #}
      if((1<=Ad1&&Ad1<=120) & (1 <=Ad2&&Ad2<=120)) {
        hours <- return(Jan_April())
      }else if((121<=Ad1&&Ad1<=213) & (121<=Ad2&&Ad2<=212)) {
        hours <- return(May_July())
      }else if(213<=Ad1&&Ad1<=365 & 213<=Ad2&&Ad2<=365) {
        hours <- return(August_Dec())
      }else if(Ad1-0.5 > 365 & Ad2-0.5 > 365) {
        hours <- return(begin())
      }
    }
  } 
  
  #===========================  WORKABLE HOURS IN A PERIOD ========================================
  
  periodWorkableHours <- function(soil,rain){ 
    # This function estimates the workable hours for the 26 two-week periods
    # using soil type, rainfall and day numbers in the year
    
    # The function estimates the workable hours for each period
    # based on the soil type and rainfall amount at the farm location.
    
    if(missing(soil)||missing(rain)){
      warning("soil or rail argument missing")
    }else{
      
      Ad1 <- seq(1,351,14); Ad2 <- seq(15,365,14)
      Days <- rep(14,26)
      
      dd <- seq(1,26)
      Wkhrs<- c()
      
      for(i in dd){
        Wkhrs <- c(Wkhrs,workableHoursCal(Ad1[i],Ad2[i],soil,rain))
      }
      
      Wkhrs[9] <- p9 <- workableHoursCal(Ad1=113, Ad2=120, soil, rain) + workableHoursCal(Ad1=121, Ad2=127, soil, rain)
      Wkhrs[16] <- p16 <- workableHoursCal(Ad1=211, Ad2=212, soil, rain) + workableHoursCal(Ad1=213, Ad2=225, soil, rain)
      
      Period <- c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15",
                  "P16","P17","P18","P19","P20","P21","P22","P23","P24","P25","P26")
      
      kk <- data.frame(Period,Ad1,Ad2,Days,Wkhrs)
      kk
    }
  }
  
  #prw <- periodWorkableHours(2.5,600)
  
  #============================= WORKABLE HOURS FOR FARM OPERATIONS =============================
  
  opsWorkableHours <- function(soil,rain){
    
    # Tractor Hours ========
    
    if(missing(soil)||missing(rain)){
      warning("soil or rain argument missing")
    }else{
      
      Period <- c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15",
                  "P16","P17","P18","P19","P20","P21","P22","P23","P24","P25","P26")
      
      Days <- rep(14,26)
      
      trac_pkfert <- trac_plough <- trac_harrow <- trac_plant <- trac_broadcast <- 
        trac_plant_potato <- trac_roll <- trac_ridge <- trac_spray <- trac_combine <- 
        trac_pot_harvest <- trac_sbee_harvest <- trac_bale_wht <- trac_bale_bar <- trac_nfert <- 
        trac_hoeing <- p_harrow <- spra_spray <- combine <- pot_harvester <- sbee_harvester <- bal_baler <- rep(0,26)
      
      opwkhs <- data.frame(Period,Days,trac_pkfert,trac_plough,trac_harrow,trac_plant,trac_broadcast, 
                           trac_plant_potato,trac_roll,trac_ridge,trac_spray,trac_combine, 
                           trac_pot_harvest,trac_sbee_harvest,trac_bale_wht,trac_bale_bar,trac_nfert,trac_hoeing, 
                           p_harrow,spra_spray,combine,pot_harvester,sbee_harvester,bal_baler)
      
      pwkhs <- periodWorkableHours(soil,rain)
      
      # Tractor number for: NPK fertiliser spreading, plouhing,harrowing, 
      # planting & broadcasting,
      # rolling, ridging, spraying, combine lnse
      tn1 <- 1 #1  
      tn2 <- 1#2 # 1 #2
      tn3 <- 1#3 #1 #3
      tn4 <- 1#4 #1 #4
      
      # Workability Types ==== The percentage hours
      r100 <- 100/100 # For ploughing
      r80 <- 80/100 # For P/K fert spreading & Rolling, WPOT and SBEE harvesting
      # r80 is also for Hoeing and Harrowing
      r70 <- 70/100 # For Planting, Combine & Baling  
      r60 <- 60/100 # For Spraying
      r28 <- 28/100 # For Combine of Linseed
      
      # Plough workability 
      opwkhs$trac_plough <- r100 * tn1
      
      # Workability for: NPK fertiliser spreading; Rolling; Harrowing, Hoeing,
      # Ridging and WPOT and SBEE harvesting
      opwkhs$trac_pkfert <- r80*tn1; opwkhs$trac_nfert <- 
        r80*tn1; opwkhs$trac_harrow <- r80*tn1; opwkhs$trac_roll <- r80*tn1;
      opwkhs$trac_ridge <- r80*tn1; opwkhs$trac_pot_harvest <- 
        r80*tn4; opwkhs$trac_sbee_harvest <- r80*tn3
      opwkhs$trac_hoeing <- r80*tn1
      
      # Workability for: Planting, Broadcast, Combine and Baling
      opwkhs$trac_plant <- r70*tn1; opwkhs$trac_broadcast <- 
        r70*tn1; opwkhs$trac_combine <- r70*tn2; 
      opwkhs$trac_bale_wht <- 
        r70*tn2; opwkhs$trac_bale_bar <- r70*tn3;
      opwkhs$trac_plant_potato <- r70*tn3; 
      #opwkhs$trac_combine_lnse <- r28*tn1;
      
      # Workability for: Spraying
      opwkhs$trac_spray <- r60*tn1
      
      # Other Machines
      opwkhs$p_harrow <- r80*tn1; opwkhs$spra_spray <- r60*tn1; 
      opwkhs$combine <- r70*tn1; #opwkhs$combine_lnse <- r28*tn1
      opwkhs$pot_harvester <- r80*tn1; opwkhs$sbee_harvester <- 
        r80*tn1; opwkhs$bal_baler <- r80*tn1
      
      nwks <- opwkhs
      
      # Workable Hours =====
      
      nwks[seq(1,26),seq(3,24)] <- round(nwks[seq(1,26),seq(3,24)]* pwkhs[seq(1,26),5])#
      nnwks <- nwks
      
      kk <- nnwks
    }  
  }
  
  #================================================================================
  
  labWorkableHours <- function(soil,rain){
    
    # Tractor Hours ========
    
    if(missing(soil)||missing(rain)){
      warning("soil or rain argument missing")
    }else{
      
      Period <- c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15",
                  "P16","P17","P18","P19","P20","P21","P22","P23","P24","P25","P26")
      
      Days <- rep(14,26)
      
      lab_pkfert <- lab_plough <- lab_harrow <- lab_plant <- lab_broadcast <- 
        lab_plant_potato <- lab_roll <- lab_ridge <- lab_spray <- lab_combine <- 
        lab_pot_harvest <- lab_sbee_harvest <- lab_bale_wht <- lab_bale_bar <- lab_nfert <- lab_hoeing <- rep(0,26)
      
      opwkhs <- data.frame(Period,Days,lab_pkfert,lab_plough,lab_harrow,lab_plant,lab_broadcast, 
                           lab_plant_potato,lab_roll,lab_ridge,lab_spray,lab_combine, 
                           lab_pot_harvest,lab_sbee_harvest,lab_bale_wht,lab_bale_bar,lab_nfert,lab_hoeing)
      
      pwkhs <- periodWorkableHours(soil,rain)
      
      # Labour number for: NPK fertiliser spreading, 
      # plouhing,harrowing, planting & broadcasting,
      # rolling, ridging, spraying, combine lnse
      tn1 <- 1 
      tn2 <- 1#2
      tn3 <- 1#3
      tn4 <- 1#4
      
      # Workability Types ==== The percentage hours
      r100 <- 100/100 # For ploughing
      r80 <- 80/100 # For P/K fert spreading & Rolling, WPOT and SBEE harvesting
      # r80 is also for Hoeing and Harrowing
      r70 <- 70/100 # For Planting, Combine & Baling  
      r60 <- 60/100 # For Spraying
      r28 <- 28/100 # For Combine of Linseed
      
      # Plough workability 
      opwkhs$lab_plough <- r100 * tn1
      
      # Workability for: NPK fertiliser spreading; Rolling; Harrowing, Hoeing,
      # Ridging and WPOT and SBEE harvesting
      opwkhs$lab_pkfert <- r80*tn1; opwkhs$lab_nfert <- 
        r80*tn1; opwkhs$lab_harrow <- r80*tn1; opwkhs$lab_roll <- r80*tn1;
      opwkhs$lab_ridge <- r80*tn1; opwkhs$lab_pot_harvest <- 
        r80*tn4; opwkhs$lab_sbee_harvest <- r80*tn3
      opwkhs$lab_hoeing <- r80*tn1
      
      # Workability for: Planting, Broadcast, Combine and Baling
      opwkhs$lab_plant <- r70*tn1; opwkhs$lab_broadcast <- 
        r70*tn1; opwkhs$lab_combine <- r70*tn3; 
      opwkhs$lab_bale_wht <- 
        r70*tn2; opwkhs$lab_bale_bar <- r70*tn3;
      opwkhs$lab_plant_potato <- r70*tn3; 
      #opwkhs$lab_combine_lnse <- r28*tn2;
      
      # Workability for: Spraying
      opwkhs$lab_spray <- r60*tn1
      
      nwks <- opwkhs
      
      # Workable Hours =====
      
      nwks[seq(1,26),seq(3,18)] <- 
        round(nwks[seq(1,26),seq(3,18)]* pwkhs[seq(1,26),5])
      nnwks <- nwks
      
      #opw <- opsWorkableHours(soil,rain)
      
      kk <- nnwks
    }
  }
  
  #======================== WORK RATE FOR WORKERS =================================
  
  workRateCal <- function(soil){
    
    # This function estimates the work rates for farm operations 
    
    operation <- as.character(expression(pkfert_wwht,plough_wwht,plant_wwht,roll_wwht,spray_wwht,harvest_wwht,
                                         bale_wwht,pkfert_swht,plough_swht,plant_swht,roll_swht,harvest_swht, bale_swht,pkfert_wbar,
                                         plough_wbar,plant_wbar,roll_wbar,spray_wbar,harvest_wbar, bale_wbar,
                                         pkfert_sbar,plough_sbar,plant_sbar,roll_sbar,harvest_sbar, bale_sbar,   
                                         pkfert_wbea,plant_wbea,roll_wbea,spray_wbea,harvest_wbea,pkfert_sbea,plough_sbea,plant_sbea,
                                         harvest_sbea,plough_wpot,harrow_wpot,plant_wpot,ridge_wpot,harvest_wpot,pkfert_wosr,plough_wosr,
                                         plant_wosr,harvest_wosr,plough_sbee,harrow_sbee,plant_sbee,harvest_sbee,start_seta,plough_seta,end_seta,
                                         spray_wwht,fertN_wwht,spray_swht,fertN_swht,spray_wbar,fertN_wbar,spray_sbar,fertN_sbar,
                                         spray_wbea,spray_sbea,spray_wpot,pkfert_wpot,hoe_wpot,spray_wosr,fertN_wosr,
                                         spray_sbee,pkfert_sbee,hoe_sbee,spray_seta))
    
    tractor <- labour <- power_harrow <- sprayer <- combine_have <- baler <- pot_harvester <- sbee_harvester <- 
      macCost <- labourCost <- opCost <- rep(0,length(operation))
    
    wr <- data.frame(operation,tractor,labour,power_harrow,sprayer,combine_have,baler,pot_harvester,sbee_harvester, 
                     macCost,labourCost,opCost)
    
    wr[,seq(2,3)] <- 1; wr[c(6,7,12,13,19,25,31,35,44),2] <- 2; wr[c(20,26,38,48),2] <- 3; 
    wr[c(49,51),seq(2,3)] <- 0; wr[40,seq(2,3)] <- 4
    wr[c(7,13),3] <- 2; wr[c(6,12,19,20,25,26,31,35,38,44,48),3] <- 3; 
    wr[c(37,46),4] <- 1;wr[c(5,18,30,52,54,56,58,60,61,62,65,67,70),5] <- 1;
    wr[c(6,12,19,25,31,35,44),6] <- 1; wr[c(7,13,20,26),7] <- 1; wr[40,8] <- 1;wr[48,9] <- 1;
    
    cr <- cropData(soil)[[1]] # Crop data from which fertilizer amount and yield and extracted
    
    # Workability Types ==== The percentage hours
    r100 <- 1 #100/100 # For ploughing
    r80 <- 1 #80/100 # For P/K fert spreading & Rolling
    r70 <- 1 #70/100 # For Planting, Harvesting & Baling  
    r60 <- 1 #60/100 # For Spraying
    r28 <- 1 #28/100 # For Combine of Linseed
    
    #========= Machine Sizes ===========
    tractor <- 100# Tractor Size or power (kW) 100kW Assuming 90% efficiency
    sprayer <- 1400 # Sprayer Size (size of tank in litres) 1400 litres
    tsize <- 1400
    combsize <- 125 # 170kW
    # The size of the combine harvester measured in tonnes/hour 
    # was derived on pro rata basis based on information from
    # Agricultural Notebook. A combine harvester with a power of 90kW can harvest 10t/h 
    # Thus a combine harvester of 125kW can harvest 
    # approximately 14/h. Hence 19t/h was used to estimate the work rate
    
    extfactor <- round(10/90,2)
    
    combine <- round(combsize*extfactor) #14 # Represents combine harvester size
    
    tpspeed <- 4 # Speed for rolling (km/hr)
    rwidth <- 6 # Roller width
    
    # Work rates for winter wheat operations =======
    wr[1,2] <- wr[1,3] <- round(((0.06+0.00025*({cr$WWHT[3]}+{cr$WWHT[5]}))+
                                   (64.48+0.094*({cr$WWHT[3]}+{cr$WWHT[5]}))/ tsize)*r80,2) # Spread P/K fert
    wr[2,2] <- wr[2,3] <- round((1.44*(50*{soil}+20))/tractor,2)*r100 # Plough
    wr[3,2] <- wr[3,3] <- round((0.06+0.00069*{cr$WWHT[7]})+
                                  (58.82+41.5*{soil}+0.00626*{cr$WWHT[7]})/tractor*r70,2) # Plant
    wr[4,2] <- wr[4,3] <- round((1.5/(tpspeed*rwidth/10))*0.85*r80,2) #round(1.96/6*r80,2) # Roll
    #wr[5,2] <- wr[5,3] <- wr[5,5] <- round(0.11+191.6/sprayer*r60,2) 
    wr[5,2] <- wr[5,3] <- wr[5,5] <- round(0.11+204.2/sprayer*r60,2) # Spraying 
    # From the reference used for farmR the work rate: 0.11+191.6/Hopper or tank capacity (kg or l)is for gantries 
    cbwwht <- round((1.00*({cr$WWHT[13]}+20)/4)/combine*r70,2) # Harvesting; Combine harvester
    wr[6,2] <- 2* cbwwht; wr[6,3] <- 3* cbwwht; wr[6,6] <- cbwwht
    bawwht <- round((({cr$WWHT[14]}+13)/4)/combine*0.5*r70,2) # Bale; (The baler size needs to be confirmed)
    wr[7,2] <- 2* bawwht; wr[7,3] <- 2* bawwht; wr[7,7] <- bawwht
    
    # Work rates for spring wheat operations =======
    wr[8,2] <- wr[8,3] <- round(((0.06+0.00025*({cr$SWHT[3]}+{cr$SWHT[5]}))+
                                   (64.48+0.094*({cr$SWHT[3]}+{cr$SWHT[5]}))/ tsize)*r80,2)
    wr[9,2] <- wr[9,3] <- round((1.44*(50*{soil}+20))/tractor*r100,2) # plough
    wr[10,2] <- wr[10,3] <- round(((0.06+0.00069*{cr$SWHT[7]})+
                                     (58.82+41.5*{soil}+0.00626*{cr$SWHT[7]})/tractor)*r70,2)
    wr[11,2] <- wr[11,3] <- round((1.5/(tpspeed*rwidth/10))*0.85*r80,2) #round(0.33*r80,2)
    cbswht <- round(2*(1.00*({cr$SWHT[13]}+20)/4)/combine*r70,2)
    wr[12,2] <- 2* cbswht; wr[12,3] <- 3* cbswht; wr[12,6] <- cbswht
    baswht <- round((({cr$SWHT[14]}+13)/4)/combine*0.5*r70,2) # round(0.87*r70,2); 
    wr[13,2] <- 2* baswht;  wr[13,3] <- 2* baswht; wr[13,7] <- baswht
    # round(0.87*r70,2); wr[13,7] <- round(0.87*r70,2)
    
    
    # Work rates for winter barley operations =====
    wr[14,2] <- wr[14,3] <- round(((0.06+0.00025*({cr$WBAR[3]}+{cr$WBAR[5]}))+
                                     (64.48+0.094*({cr$WBAR[3]}+{cr$WBAR[5]}))/ tsize)*r80,2)
    wr[15,2] <- wr[15,3] <- round((1.44*(50*{soil}+20))/tractor*r100,2)
    wr[16,2] <- wr[16,3] <- round(((0.06+0.00069*{cr$WBAR[7]})+
                                     (58.82+41.5*{soil}+0.00626*{cr$WBAR[7]})/tractor)*r70,2)
    wr[17,2] <- wr[17,3] <- round((1.5/(tpspeed*rwidth/10))*0.85*r80,2) #round(0.33* r80, 2) # roll
    #wr[18,2] <- wr[18,3] <- wr[18,5] <- round(0.11+191.6/sprayer*r60,2)
    wr[18,2] <- wr[18,3] <- wr[18,5] <- round(0.11+204.2/sprayer*r60,2) # Spray 
    # From the reference used for farmR the work rate: 0.11+191.6/Hopper or 
    # tank capacity (kg or l)is for gantries 
    cbwbar <- round((1.15*({cr$WBAR[13]}+24)/4)/combine*r70,2) # combine
    wr[19,2] <- 2* cbwbar; wr[19,3] <- 3* cbwbar; wr[19,6] <- cbwbar
    bawbar <- round((({cr$WBAR[14]}+13)/4)/combine*0.5*r70,2) 
    wr[20,2] <- 3* bawbar; wr[20,3] <- 3* bawbar; wr[20,7] <- bawbar
    
    # Work rates for spring barley operations =====
    wr[21,2] <- wr[21,3] <- round(((0.06+0.00025*({cr$SBAR[3]}+{cr$SBAR[5]}))+
                                     (64.48+0.094*({cr$SBAR[3]}+{cr$SBAR[5]}))/ tsize)*r80,2)
    wr[22,2] <- wr[22,3] <- round((1.44*(50*{soil}+20))/tractor*r100,2)
    wr[23,2] <- wr[23,3] <- round(((0.06+0.00069*{cr$SBAR[7]})+
                                     (58.82+41.5*{soil}+0.00626*{cr$SBAR[7]})/tractor)*r70,2)
    wr[24,2] <- wr[24,3] <- round((1.5/(tpspeed*rwidth/10))*0.85*r80,2) #round(0.33*r80,2) # roll
    cbsbar <- round(((1.15*{cr$SBAR[13]}+24)/4)/combine*r70,2)
    wr[25,2] <- 2* cbsbar; wr[25,3] <- 3* cbsbar; wr[25,6] <- cbsbar
    basbar <- round((({cr$SBAR[14]}+13)/4)/combine*0.5*r70,2) 
    wr[26,2] <- 3* basbar; wr[26,3] <- 3* basbar; wr[26,7] <- basbar
    
    # Work rates for winter beans operations
    wr[27,2] <- wr[27,3] <- 
      round(((0.06+0.00025*({cr$WBEA[3]}+{cr$WBEA[5]}))+
               (64.48+0.094*({cr$WBEA[3]}+{cr$WBEA[5]}))/ tsize)*r80,2)
    wr[28,2] <- wr[28,3] <- round((3*(0.114+0.00033*{cr$WBEA[7]})+
                                     (54*{soil}+21.6)/tractor)*r70,2) # Broadcast winter beans
    wr[29,2] <- wr[29,3] <- round((1.5/(tpspeed*rwidth/10))*0.85*r80,2) #round(0.33*r80,2) # Roll
    #wr[30,2] <- wr[30,3] <- wr[30,5] <- round(0.11+191.6/sprayer*r60,2) #Spray
    wr[30,2] <- wr[30,3] <- wr[30,5] <- round(0.11+204.2/sprayer*r60,2) #Spray
    # From the reference used for farmR the work rate: 0.11+191.6/Hopper or 
    # tank capacity (kg or l)is for gantries 
    cbwbea <- round((4.05*({cr$WBEA[13]}+24)/4)/combine*r70,2)
    wr[31,2] <- 2* cbwbea; wr[31,3] <- 3* cbwbea; wr[31,6] <- cbwbea
    
    # Work rates for spring beans operations
    wr[32,2] <- wr[32,3] <- 
      round(((0.06+0.00025*({cr$SBEA[3]}+{cr$SBEA[5]}))+
               (64.48+0.094*({cr$SBEA[3]}+{cr$SBEA[5]}))/ tsize)*r80,2)
    wr[33,2] <- wr[33,3] <- round((1.44*(50*{soil}+20))/tractor*r100,2)
    wr[34,2] <- wr[34,3] <- round(((0.06+0.00069*{cr$SBEA[7]})+
                                     (92.42+0.00626*{cr$SBEA[7]}+41.5*{soil})/tractor)*r70,2)
    cbsbea <- round((4.05*({cr$SBEA[13]}+24)/4)/combine*r70,2)
    wr[35,2] <- 2* cbsbea; wr[35,3] <- 3* cbsbea; wr[35,6] <- cbsbea
    
    # Work rates for ware potatoes operations
    wr[36,2] <- wr[36,3] <- 
      round((1.80*(50*{soil}+20))/tractor*r100,2) # Ploughing 
    wr[37,2] <- wr[37,3] <- 
      wr[37,4] <- round(((25*{soil}+33)/tractor)*r80,2) # Harrowing
    wr[38,2] <- wr[38,3] <- 
      round(((278/tractor+0.04+0.55*{cr$WPOT[7]})/2000)*r70,2)*3 # Plant potatoes
    wr[39,2] <- wr[39,3] <- round(((40*{soil}+33)/tractor)*r80,2) # Ridging
    
    hpot <- round(((403/600)+2/(3*(1.25+0.51*{2.5})*({39.43}/37.728)))*2.51*r80,2) # Harvest pot
    wr[40,2] <- hpot*4 #round(2.32*4*r80,2); 
    wr[40,3] <- hpot*4 #round(2.32*4*r80,2); 
    #wr[40,2] <- 4* round(2.32*r80,2); wr[40,3] <- 4* round(2.32*r80,2);
    wr[40,8] <- hpot #round(2.32*r80,2)# Harvest
    
    # Work rates for wosr operations
    wr[41,2] <- wr[41,3] <- round(((0.06+0.00025*({cr$WOSR[3]}+{cr$WOSR[5]}))+
                                     (64.48+0.094*({cr$WOSR[3]}+{cr$WOSR[5]}))/ tsize)*r80,2)
    wr[42,2] <- wr[42,3] <- round((1.44*(50*{soil}+20))/tractor*r100,2) # Ploughing
    wr[43,2] <- wr[43,3] <- round(((0.387+0.00069*cr$WOSR[7])+(99.42+0.00626*cr$WOSR[7])/tractor)*r70,2)
    cbwosr <- round(((4.05*({cr$WOSR[13]}+24)/4)/combine)*r70,2)
    wr[44,2] <- 2* cbwosr; wr[44,3] <- 3* cbwosr; wr[44,6] <- cbwosr
    
    # Work rates for sugarbeet operations
    wr[45,2] <- wr[45,3] <- round((1.80*(50*{soil}+20))/tractor*r100,2) # Ploughing 
    wr[46,2] <- wr[46,3]  <- wr[46,4] <- round(((25*{soil}+33)/tractor)*r80,2) # Harrowing
    wr[47,2] <- wr[47,3] <- round((0.39+157/tractor)*r70, 2)
    hvsbee <- round(((403/600)+2/(3*(1.25+0.51*{soil})*({cr$SBEE[13]}/37.728)))*r80,2)
    wr[48,2] <- 3* hvsbee; wr[48,3] <- 3* hvsbee; wr[48,9] <- hvsbee
    
    # Work rates for setaside operations
    wr[49,2] <- wr[49,3] <- 0 # Start
    wr[50,2] <- wr[50,3] <- round((1.44*(50*{soil}+20))/tractor*r100,2) # Ploughing
    wr[51,2] <- wr[51,3] <- 0 # End
    
    
    # NON SEQUENTIAL OPERATION 
    #================================
    sphoe <- 19 #Assumed speed for hoeing (19km/h)
    rowsp <- 0.6 #Assumed row space (0.6m)
    
    # Non-sequential operations for winter wheat
    wr[52,2] <- wr[52,3] <- wr[52,5] <- 
      round(0.11+204.2/sprayer*r60,2) # Spray winter wheat
    # Spread N fertiliser
    wr[53,2] <- wr[53,3] <- 
      round(((0.06+0.00025*({cr$WWHT[1]}))+(64.68+0.094*({cr$WWHT[1]}))/ tsize)*r80,2)
    
    # Non-sequential operations for spring wheat
    wr[54,2] <- wr[54,3] <- 
      wr[54,5] <- round(0.11+204.2/sprayer*r60,2)
    wr[55,2] <- wr[55,3] <- 
      round(((0.06+0.00025*({cr$SWHT[1]}))+(64.68+0.094*({cr$SWHT[1]}))/ tsize)*r80,2)
    
    # Non-sequential operations for winter barley
    wr[56,2] <- wr[56,3] <- 
      wr[56,5] <- round(0.11+204.2/sprayer*r60,2)
    wr[57,2] <- wr[57,3] <- 
      round(((0.06+0.00025*({cr$WBAR[1]}))+(64.68+0.094*({cr$WBAR[1]}))/ tsize)*r80,2)
    
    # Non-sequential operations for spring barley
    wr[58,2] <- wr[58,3] <- 
      wr[58,5] <- round(0.11+204.2/sprayer*r60,2)
    wr[59,2] <- wr[59,3] <- 
      round(((0.06+0.00025*({cr$SBAR[1]}))+(64.68+0.094*({cr$SBAR[1]}))/ tsize)*r80,2)
    
    # Non-sequential operations for winter beans
    wr[60,2] <- wr[60,3] <- 
      wr[60,5] <- round(0.11+204.2/sprayer*r60,2)
    
    # Non-sequential operations for spring beans
    wr[61,2] <- wr[61,3] <- 
      wr[61,5] <- round(0.11+204.2/sprayer*r60,2)
    
    # Non-sequential operations for ware potatoes
    wr[62,2] <- wr[62,3] <- wr[62,5] <- round(0.11+204.2/sprayer*r60,2)
    wr[63,2] <- wr[63,3] <- 
      round(((0.06+0.00025*({cr$WPOT[3]}+{cr$WPOT[5]}))+
               (64.48+0.094*({cr$WPOT[3]}+{cr$WPOT[5]}))/ tsize)*r80,2)
    wr[64,2] <- wr[64,3] <- round(1/(sphoe*rowsp/10*0.8),2) #round(1.02*r80,2)
    
    # Non-sequential operations for wosr
    wr[65,2] <- wr[65,3] <- wr[65,5] <- round(0.11+204.2/sprayer*r60,2) 
    wr[66,2] <- wr[66,3] <- 
      round(((0.06+0.00025*({cr$WOSR[1]}))+(64.68+0.094*({cr$WOSR[1]}))/ tsize)*r80,2) 
    
    # Non-sequential operations for sugarbeet
    wr[67,2] <- wr[67,3] <- wr[67,5] <- round(0.11+204.2/sprayer*r60,2)
    wr[68,2] <- wr[68,3] <- round(((0.06+0.00025*({cr$SBEE[3]}+{cr$SBEE[5]}))+
                                     (64.48+0.094*({cr$SBEE[3]}+{cr$SBEE[5]}))/ tsize)*r80,2)
    wr[69,2] <- wr[69,3] <-  round(1/(sphoe*rowsp/10*0.8),2) # round(1.02*r80,2)
    
    # Non-sequential operations for setaside
    wr[70,2] <- wr[70,3] <- wr[70,5] <- round(0.11+204.2/sprayer*r60,2)
    
    #wr[,seq(2,9)] <- wr[,seq(2,9)]*1.2
    newwr <- wr 
    
    kk <- newwr
  }
  
  #=========================== COST OF OPERATIONS =====================================
  
  operationCost <- function(soil){
    
    if(missing(soil)){
      warning("soil argument missing")
    }else{
      
      wr <- workRateCal(soil)
      
      # Hourly Labour Rate (HLR) was obtained from Nix (2014) for a Standard Worker
      HLR <- 9.95
      fuelPrice <- fuelPrice
      # Operation machine cost (£/ha)
      
      # Tractor power in horsepower
      # 1kW = 1.341hp
      
      tractor <- 100 # A tractor power of 100kW was used
      combine <- 125 # Combine size
      
      TP <- round(tractor*1.341,0) # maximum PTO horsepower 
      CP <- round(combine*1.341,0)
      
      # For diesel tractor, fuel consumption is estimated from the formula below
      # Obtained from: http://www.extension.iastate.edu/agdm/crops/html/a3-29.html (16/06/2015)
      # 0.044 * maximum PTO horsepower for diesel engines
      
      fuel.cons_gal_hr <- round(0.044 * TP,2) # Gallons per hour
      
      fuel.cons_gal_hr_comb <- round(0.044 * CP,2) 
      # Convert gallons per hour to litres per hour
      # 1 gallon per hour = 4.546 litres per hour
      
      Fuel_Cons <- round(fuel.cons_gal_hr * 4.546,0) # Fuel consumption (litres/hour)
      Fuel_Cons_comb <- round(fuel.cons_gal_hr_comb * 4.546,0)
      # Fuel Cost (£/hour)
      fuelCost <- fuelPrice * Fuel_Cons # £/hour
      fuelCost_comb <- fuelPrice * Fuel_Cons_comb
      # Hourly cost of operation: Labour cost per hour + fuel cost per hour
      hourCost <- (HLR + fuelCost)#* 1.10 # 10% of fuel cost represents lubrication costs
      hourCost_comb <- (HLR + fuelCost_comb)#*1.10 
      # Tractor Operation Cost 
      #TFcons <- wr$tractor * Fuel_Cons # Work rate (h/ha) * Fuel consumption (l/h)
      tractorWorkRate <- wr$tractor
      # Work rate (h/ha) * Fuel cost (£/h)
      TFcost <- tractorWorkRate *  fuelCost # Tractor cost (£/ha)
      
      # Power-harrow Operation Cost ===
      pharrowWorkRate <- wr$power_harrow
      PHcost <- hourCost * pharrowWorkRate
      
      # Sprayer Operation Cost
      spWorkRate <- wr$sprayer
      SPcost <- hourCost * spWorkRate
      
      # Combine-harvester Operation Cost ===
      combineWorkRate <- wr$combine_have
      CBcost <- hourCost_comb * combineWorkRate
      
      # Baler Operation Cost ===
      balerWorkRate <- wr$baler
      BLcost <- hourCost * balerWorkRate
      
      # Potatoe-harvester Operation Cost ===
      phWorkRate <- wr$pot_harvester
      PTcost <- hourCost * phWorkRate
      
      # Sugarbeet-harvester Operation Cost ===
      sbWorkRate <- wr$sbee_harvester
      SBcost <- hourCost * sbWorkRate
      
      # Labour Cost (£/ha)
      labCost <- wr$labourCost <- round(HLR * wr$labour,0)
      
      # Machine Cost (£/ha)
      machineCost <- wr$macCost <- round(TFcost+PHcost+SPcost+CBcost+BLcost+PTcost+SBcost,0)
      # Total operation cost based on worker type
      totalOPCost <- wr$opCost <- labCost + machineCost
      
      newwr <- wr # Updated cost
    }
  }
  
  #============================== ESTIMATION OF FIXED COST ====================================
  
  fixedCost <- function(){# IR = Interest Rate; INF = Inflation
    
    # Depreciation = Purchase price minus average depreciation rate (%) times purchase price of machine
    # Replace = Number of years after which the machine should be replaced
    # Repair cost = Repair cost rate (%) times the purchase price
    # Lifetime cost = Depreciation + Repair cost
    # Annual cost = Lifetime cost / Replace
    
    # Capital Recovery Cost (CRF) takes into consideration a joint depreciation and interest
    
    Machine <- c("Tractor","Power_Harrow","Sprayer","Combine_Harvester",
                 "Baler","Potato_Harvester","Sugarbeet_Harvester","Labour")
    
    if(is.null(machinePrices)){
      # Default machine prices or cost
      Cost <- c(50000,16000,21000,95000,14000,80000,70000,0)
    }else{
      Cost <- c(machinePrices,0) # Machine Prices must be a vector of length 7
    }
    
    if(is.null(machinePrices)){
      # Default depreciation rates
      DepreciationRate <- c(22,14,18,18,11,18,18,0)
    }else{
      DepreciationRate <- c(depRates,0) # Depreciation rates must be a vector of length 7
    }
    
    if(is.null(machinePrices)){
      # Default repair cost rates 
      RepairCostRate <- c(12.05,5.0,6.8,5.8,5.5,6.0,5.0,0)
    }else{
      RepairCostRate <- c(repCostRates,0) # Repair cost rates must be a vector of length 7
    }
    
    if(is.null(machinePrices)){
      # Default replacement years
      ReplaceYear <- c(5,4,7,7,7,7,7,0)
    }else{
      ReplaceYear <- c(replaceYears,0) # Replacement years must be a vector of length 7
    }
    
    Depreciation <- RepairCost <-AnnualCost <- rep(0,length(Machine))
    
    ma <- data.frame(Machine,Cost,DepreciationRate,Depreciation,RepairCostRate,RepairCost,ReplaceYear,AnnualCost)
    
    re <- ma[seq(1,7),7] # Replacement years
    
    nIR <- interestRate
    nINF <- inflation
    
    if(nINF==0){
      ninf <- 1
    }else{
      ninf <- 1+(nINF*0.01)
    }
    
    # Annual Labour Cost (ALC) 
    # The ALC used was obtained from Nix(2014) for a standard worker type estimated for 2013/14 year
    # It includes overtime, NI contributions and Employers Liability Insurance
    if(is.null(annualLabCost)){
      ALC <- 21945 * ninf
    }else{annualLabCost * ninf}
    
    ma[8,8] <- round(ALC,0)
    
    # Capital Recovery Factor (CRF)
    CRF <- function(nIR, N){
      n <- N 
      if(nIR==0){
        i <- 0
        Num <- 1
        Denum <- 1
      }else{
        i <- nIR * 0.01
        Num <- (i*((1+i)^n))
        Denum <- (1+i)^n-1
      }
      crf <- Num/Denum
      round(crf,3)
    }
    
    nCR <- CRF(nIR, re)
    jdepsal <- (ma[seq(1,7),2]*ninf) * ma[seq(1,7),3]*0.01 # Salvage
    jdep <- (ma[seq(1,7),2]*ninf) - jdepsal # Total Depreciation
    
    if(nIR==0){
      ir <- 0
      Dep <- ma[seq(1,7),4] <- round((jdep/re) + (jdepsal*ir),0) # Depreciation
    }else{
      ir <- nIR * 0.01
      Dep <- ma[seq(1,7),4] <- round((jdep*nCR) + (jdepsal*ir),0) # Depreciation
    }
    
    #Dep <- ma[seq(1,7),4] <- CR # Depreciation
    Rep <- ma[seq(1,7),6] <- 
      round((((ma[seq(1,7),2]*ninf) * ma[seq(1,7),5]*0.01)*re)/re,0) # Repair cost
    Ann <- ma[seq(1,7),8] <- round((Dep + Rep),0) # Annual machinery cost
    newma <- ma 
  }
  
  #================= SELF ROTATION PENALTIES ======================
  
  selfRotPen <- function(soil){
    
    # This function estimates and stores the yield penalties and associated costs as well as variations 
    # in pesticide amounts due to a crop succeeding itself (successive or consecutive cropping)
    
    if(missing(soil)){
      warning("soil argument missing")
    }else{
      
      CostType <- c("wwht_YL","wwht_CO","wwht_BG","wwht_WO","TotalCost")
      Year2 <- Year3 <- Year4 <- Year5 <- Est2 <- Est3 <- Est4 <- Est5 <- Cost2 <- 
        Cost3 <- Cost4 <- Cost5 <- GM2 <- GM3 <- GM4 <- rep(0,5)
      srp1 <- data.frame(CostType,Year2,Year3,Year4,Year5,Est2,Est3,Est4,Est5,Cost2, 
                         Cost3,Cost4,Cost5,GM2,GM3,GM4)
      srp1[seq(1,4),2] <- c(11,20,1,2); srp1[seq(1,4),3] <- c(11,20,2,2);
      srp1[seq(1,4),4] <- c(15,20,3,3); srp1[seq(1,4),5] <- c(15,20,5,4);
      
      srp <- srp1
      cr <- cropData(soil)[[1]] # Crop data
      
      # Winter wheat
      srp[1,seq(6,9)] <- round(srp[1,seq(2,5)] * 
                                 0.01* cr$WWHT[13],2) # Yield loss
      srp[1,seq(10,13)] <- round(srp[1,seq(2,5)] * 
                                   0.01* cr$WWHT[13]* cr$WWHT[15],0) # Cost of yield loss (£/ha)
      srp[2,seq(10,13)] <- srp[2,seq(2,5)] # Rotational cost per hectare
      # Change in Blackgrass herbicide amount and cost
      srp[3,seq(6,9)] <- srp[3,seq(2,5)] * 
        cr$WWHT[9]; srp[3,seq(10,13)] <- round(srp[3,seq(6,9)] * cr$WWHT[10],0)
      # Change in Wild oats herbicide amount and cost
      srp[4,seq(6,9)] <- srp[4,seq(2,5)] * 
        cr$WWHT[9]; srp[4,seq(10,13)] <- round(srp[4,seq(6,9)] * cr$WWHT[10],0)
      
      nsrp <- srp # Updated self-rotational penalties 
      nsrp
      
      rp2 <- sum(nsrp[seq(1,4),10])
      rp3 <- sum(nsrp[seq(1,4),11])
      rp4 <- sum(nsrp[seq(1,4),12])
      
      nsrp[5,14] <- rp2
      nsrp[5,15] <- rp3
      nsrp[5,16] <- rp4
      
      kk <- nsrp
    } 
  }
  
  #=============== ROTATION PENALTIES =================================================================
  
  rotPenalty <- function(soil){ 
    # In data: A = Yield Loss (%); B = Cost/ha; C = Blackgrass Herbicide; D = Wild Oats Herbicide;
    # E = N Fertiliser Increase; NA = Not Allowed
    # This function estimates and stores the rotational penalties associated with a following a particular crop
    
    if(missing(soil)){
      warning("soil argument missing")
    }else{
      
      CropRot <- as.character(expression(wwht_bar,wwht_pea_bea,wwht_brassica, wwht_wpot,wwht_sbee,wwht_lnse,wwht_seta,
                                         swht_bar,swht_pea_bea,swht_brassica,swht_wpot,swht_sbee,swht_lnse,swht_seta,wbar_wht,
                                         wbar_pea_bea,wbar_brassica,wbar_wpot,wbar_sbee,wbar_lnse,wbar_seta,sbar_wht,sbar_pea_bea,
                                         sbar_brassica,sbar_wpot,sbar_sbee,sbar_lnse,sbar_seta,wbea_wht,wbea_bar,wbea_brassica,
                                         wbea_wpot,wbea_sbee,wbea_lnse,wbea_seta,sbea_wht,sbea_bar,sbea_brassica, sbea_wpot,
                                         sbea_sbee,sbea_lnse,sbea_seta,wpot_wht,wpot_bar,wpot_pea_bea,wpot_brassica, wpot_sbee,wpot_lnse,
                                         wpot_seta,wosr_wht,wosr_bar,wosr_pea_bea,wosr_brassica,wosr_sbee,wosr_lnse,wosr_seta,
                                         sbee_wht,sbee_bar,sbee_pea_bea,sbee_brassica,sbee_wpot,sbee_lnse,sbee_seta,seta_wht,seta_bar,
                                         seta_pea_bea,seta_brassica, seta_wpot,seta_sbee,seta_lnse))
      
      A <- B <- C <- D <- E <- pen <- rep(0,length(CropRot))
      
      
      rp1 <- data.frame(CropRot,A,B,C,D,E,pen)
      rp1[c(1,8,15,22),2] <- 11; rp1[47,2] <- 5; rp1[c(7,14,21,28),2] <- 6;
      rp1[c(17,31,33,38,40,52,54,55,60,61),2] <- NA; rp1[c(33,40,69),3] <- NA;
      rp1[c(1,8,10,15,22,43,50,57),3] <- 20; rp1[3,3] <- 32;
      rp1[c(15,22,43,44,57,58),4] <- 3;
      rp <- rp1
      
      cr <- cropData(soil)[[1]] # Crop Data
      #sr <- selfRotPen(soil) # Self Rotation Penalty
      
      # Yield Penalties === Winter wheat
      rp[1,7] <- round((rp[1,2] * 0.01 * cr$WWHT[13] * 
                          cr$WWHT[15]) + rp[1,3],0) # Winter wheat following a barley crop
      rp[3,7] <- rp[3,3] # Winter wheat following a brassica (rotational penalty per hectare)
      rp[7,7] <- round(rp[7,2] * 0.01 * 
                         cr$WWHT[13] * cr$WWHT[15]) # Winter wheat following a setaside
      
      
      # Yield Penalties === Spring wheat
      rp[8,7] <- round((rp[8,2] * 0.01 * 
                          cr$SWHT[13] * cr$SWHT[15]) + rp[8,3],0) # Following barley
      rp[10,7] <- rp[10,3] # Following a brassica crop
      rp[14,7] <- round(rp[14,2] * 0.01 *
                          cr$SWHT[13] * cr$SWHT[15],0) # Following a rotational setaside
      
      # Winter barley
      # Following a wheat crop 
      rp[15,7] <- round(rp[15,2] * 0.01 * 
                          cr$WBAR[13] * cr$WBAR[15] + (rp[15,3]) + 
                          (rp[15,4]*cr$WBAR[9]*cr$WBAR[10]),0)
      rp[17,7] <- rp[17,2] # Winter barley is forbiden (Not Allowed) to follow a brassica crop
      rp[21,7] <- round(rp[21,2] * 0.01 * 
                          cr$WBAR[13] * cr$WBAR[15],0) # Following a rotational setaside
      
      # Spring barley
      # Following a wheat crop
      rp[22,7] <- round(rp[22,2] * 0.01 * 
                          cr$SBAR[13] * cr$SBAR[15] + (rp[22,3]) + 
                          (rp[22,4]*cr$SBAR[9]*cr$SBAR[10]),0)
      rp[28,7] <- round(rp[28,2] * 
                          0.01 * cr$SBAR[13] * cr$SBAR[15],0) # Following a rotational setaside
      
      # Winter beans
      rp[31,7] <- rp[33,7] <- 
        rp[31,2] # Not Allowed to follow brassica and sugarbeet crops
      
      # Spring beans
      rp[38,7] <- rp[40,7] <- rp[38,2] # Not Allowed to follow brassica and sugarbeet crops
      
      # Ware potatoes
      # Following a wheat crop; Increase in black grass herbicide
      rp[43,7] <- round(rp[43,3] + 
                          (rp[43,4]*cr$WPOT[9]*cr$WPOT[10]),0) 
      rp[44,7] <- round(rp[44,4]*cr$WPOT[9]*cr$WPOT[10],0) # Following a barley crop
      rp[47,7] <- round((rp[47,2] * 0.01 * cr$WPOT[13] * cr$WPOT[15]),0) # Following sugarbeet
      
      # Winter oilseed rape
      rp[50,7] <- rp[50,3] # Rotational cost per ha for following a wheat crop
      # Forbiden to follow pea/beans, sugarbeet and linseed crops
      rp[52,7] <- rp[54,7] <- rp[55,7] <- rp[52,2] 
      
      # Sugarbeet
      rp[57,7] <- round(rp[57,3] + (rp[57,4]*cr$SBEE[9]*cr$SBEE[10]),0) # Following a wheat crop
      rp[58,7] <- round(rp[58,4]*cr$SBEE[9]*cr$SBEE[10]) # Following a barley crop
      rp[60,7] <- rp[61,7] <- rp[60,2] # Not Allowed to follow brassica and potato crops
      
      # Setaside 
      rp[69,7] <- rp[69,3] # Not Allowed to follow a sugarbeet crop
      
      nrp <- rp
    }
  }
  
  #============================= YIELD PENALTIES ============================================================
  
  yieldPenalty <- function(soil){
    # This function estimates the yield penalties due to timing of farm operations
    # Yield penalty is a percentage (%) reduction in yield due to the time an 
    # operation was carried out.
    # Cost due to yield penalty = percentage yield reduction times crop price.
    
    
    if(missing(soil)){
      warning("soil argument missing")
    }else{
      
      Ops_period <- as.character(expression(pkfert_wwht,plough_wwht,plant_wwht,roll_wwht,
                                            spray_wwht,harvest_wwht, bale_wwht,pkfert_swht,plough_swht,plant_swht,
                                            roll_swht,harvest_swht, bale_swht,pkfert_wbar,
                                            plough_wbar,plant_wbar,roll_wbar,spray_wbar,harvest_wbar, bale_wbar,
                                            pkfert_sbar,plough_sbar,plant_sbar,roll_sbar,harvest_sbar, bale_sbar,   
                                            pkfert_wbea,plant_wbea,roll_wbea,spray_wbea,harvest_wbea,pkfert_sbea,
                                            plough_sbea,plant_sbea,harvest_sbea,plough_wpot,harrow_wpot,
                                            plant_wpot,ridge_wpot,harvest_wpot,pkfert_wosr,plough_wosr,
                                            plant_wosr,harvest_wosr,plough_sbee,harrow_sbee,plant_sbee,
                                            harvest_sbee,start_seta,plough_seta,end_seta))
      
      p0 <- p1 <- p2 <- p3 <- p4 <- p5 <- p6 <- p7 <- p8 <- p9 <- p10 <- p11 <- p12 <- 
        p13 <- p14 <- p15 <- p16 <- p17 <- p18 <- p19 <- p20 <- p21 <- p22 <- p23 <- p24 <-
        p25 <- p26 <- p27 <- p28 <- p29 <- p30 <- p31 <- p32 <- p33 <- p34 <- p35 <- p36 <- p37 <- rep(0,length(Ops_period)) 
      
      opf1 <- data.frame(Ops_period,p0 ,p1 ,p2 ,p3 ,p4 ,p5 ,p6 ,p7 ,p8 ,p9 ,p10 ,p11 ,p12 ,
                         p13 ,p14 ,p15 ,p16 ,p17 ,p18 ,p19 ,p20 ,p21 ,p22 ,p23 , p24,
                         p25 ,p26 ,p27 ,p28 ,p29 ,p30 ,p31 ,p32 ,p33 ,p34 ,p35 ,p36 ,p37) # Operation cost data
      
      # Yield penalties (%)
      opf1[3,seq(6,13)] <- c(2,0,5,9,11,13,15,16); opf1[5,seq(14,19)] <- seq(1,6); opf1[6,seq(31,32)] <- c(2,0);
      opf1[7,seq(31,32)] <- c(0,20); opf1[10,seq(14,20)] <- c(19,7,1,0,4,9,17); opf1[12,seq(32,33)] <- c(5,0); opf1[13,seq(32,33)] <- c(20,0);
      opf1[16,seq(8,13)] <- c(1,2,9,15,18,18); opf1[19,seq(29,30)] <- c(5,0); opf1[23,seq(14,20)] <- c(2,0,1,3,7,12,18); 
      opf1[25,seq(31,32)] <- c(2,0); opf1[26,seq(32,33)] <- c(20,0); opf1[31,seq(31,32)] <- c(2,0); opf1[34,seq(19,20)] <- c(3,8);
      opf1[35,seq(32,33)] <- c(4,0); opf1[38,seq(19,22)] <- c(5,0,5,15); opf1[40,seq(35,36)] <- c(5,0); opf1[43,seq(3,6)] <- c(8,0,1,5); 
      opf1[44,seq(29,30)] <- c(10,0); opf1[47,seq(20,22)] <- c(1,5,11); opf1[48,seq(33,38)] <- c(5,2,0,0,4,8);
      
      opf <- opf1
      
      py <- cropData(soil)[[1]] # Data to get Primary Yield
      
      # Winter Wheat
      # Penalties due to planting, spraying, harvesting and baling
      plpen <- py$WWHT[13]*opf[3,seq(6,13)]*0.01*py$WWHT[15]; 
      opf[3,seq(6,13)] <- round(plpen,0)
      sppen <- py$WWHT[13]*opf[5,seq(9,19)]*0.01*py$WWHT[15]; 
      opf[5,seq(9,19)] <- round(sppen,0)
      hapen <- py$WWHT[13]*opf[6,seq(30,31)]*0.01*py$WWHT[15];
      opf[6,seq(30,31)] <- round(hapen,0)
      bapen <- py$WWHT[13]*opf[7,seq(30,32)]*0.01*py$WWHT[15]; 
      opf[7,seq(30,32)] <- round(bapen,0)
      
      #Spring wheat 
      plpen1 <- py$SWHT[13]*opf[10,seq(14,20)]*0.01*py$SWHT[15]; 
      opf[10,seq(14,20)] <- round(plpen1,0)
      hapen1 <- py$SWHT[13]*opf[12,seq(31,32)]*0.01*py$SWHT[15]; 
      opf[12,seq(31,32)] <- round(hapen1,0)
      bapen1 <- py$SWHT[13]*opf[13,seq(30,32)]*0.01*py$SWHT[15]; 
      opf[13,seq(30,32)] <- round(bapen1,0) 
      
      # Winter barley
      plpen2 <- py$WBAR[13]*opf[16,seq(8,15)]*0.01*py$WBAR[15]; 
      opf[16,seq(8,15)] <- round(plpen2,0)
      opf[19,seq(28,29)] <- round(py$WBAR[13]*opf[19,seq(28,29)]*
                                    0.01*py$WBAR[15],0)
      
      # Spring barley
      plpen3 <- py$SBAR[13]*opf[23,seq(14,20)]*0.01*py$SBAR[15]; 
      opf[23,seq(14,20)] <- round(plpen3,0)
      hapen3 <- py$SBAR[13]*opf[25,seq(30,31)]*0.01*py$SBAR[15]; 
      opf[25,seq(30,31)] <- round(hapen3,0)
      bapen3 <- py$SBAR[13]*opf[26,seq(30,32)]*0.01*py$SBAR[15]; 
      opf[26,seq(30,32)] <- round(bapen3,0)
      
      # Winter beans
      haben4 <- py$WBEA[13]*opf[31,seq(30,31)]*0.01*py$WBEA[15]; 
      opf[31,seq(30,31)] <- round(haben4,0)
      
      # Spring beans
      plpen5 <- py$SBEA[13]*opf[34,seq(17,20)]*0.01*py$SBEA[15]; 
      opf[34,seq(17,20)] <- round(plpen5,0)
      hapen5 <- py$SBEA[13]*opf[35,seq(31,32)]*0.01*py$SBEA[15];  
      opf[35,seq(31,32)] <- round(hapen5,0)
      
      # Ware potatoes
      plpen6 <- py$WPOT[13]*opf[38,seq(19,22)]*0.01*py$WPOT[15]; 
      opf[38,seq(19,22)] <- round(plpen6,0)
      hapen6 <- py$WPOT[13]*opf[40,seq(33,35)]*0.01*py$WPOT[15]; 
      opf[40,seq(33,35)] <- round(hapen6,0)
      
      # Winter oilseed rape
      plpen7 <- py$WOSR[13]*opf[43,seq(3,6)]*0.01*py$WOSR[15]; 
      opf[43,seq(3,6)] <- round(plpen7,0)
      hapen7 <- py$WOSR[13]*opf[44,seq(28,29)]*0.01*py$WOSR[15]; 
      opf[44,seq(28,29)] <- round(hapen7,0)
      
      # Sugarbeet
      plpen10 <- py$SBEE[13]*opf[47,seq(19,22)]*0.01*py$SBEE[15]; 
      opf[47,seq(19,22)] <- round(plpen10,0)
      hapen10 <- py$SBEE[13]*opf[48,seq(33,38)]*0.01*py$SBEE[15]; 
      opf[48,seq(33,38)] <- round(hapen10,0)
      
      
      newopf <- opf
      
    }
  }
  
  #============================== OPERATION COST ==============================
  
  objOpsCost <- function(soil){
    
    # This function generates the operation cost vector for the 
    # objective function
    
    #fi <- relativeChange("modInputChange")
    op <- operationCost(soil)
    yp <- as.matrix(yieldPenalty(soil)[,c(-1,-2)])
    ct <- -1 
    
    opm <- matrix(rep(0,1*59), ncol=59) 
    oc <- ct*c(rep(op$opCost[1],8),rep(op$opCost[2],11),
               rep(op$opCost[3],8)+yp[3,4:11],rep(op$opCost[4],8),
               rep(op$opCost[5],11)+yp[5,7:17],rep(op$opCost[6],2)+yp[6,28:29],
               rep(op$opCost[7],3)+yp[7,28:30],
               rep(op$opCost[52],6),rep(op$opCost[53],2))
    opm[1,seq(1,59)] <- oc; wwht_opc <- opm  
    
    opm <- matrix(rep(0,1*44), ncol=44)
    oc <- ct*c(rep(op$opCost[8],8),rep(op$opCost[9],13),
               rep(op$opCost[10],7)+yp[10,12:18],rep(op$opCost[11],5),
               rep(op$opCost[12],2)+yp[12,29:30], 
               rep(op$opCost[13],3)+yp[13,28:30],
               rep(op$opCost[54],4),rep(op$opCost[55],2)) 
    opm[1,seq(1,44)] <- oc; 
    swht_opc <- opm 
    
    opm <- matrix(rep(0,1*63), ncol=63)  
    oc <- ct*c(rep(op$opCost[14],8),rep(op$opCost[15],11), 
               rep(op$opCost[16],8)+yp[16,4:11],rep(op$opCost[17],8),
               rep(op$opCost[18],11),rep(op$opCost[19],2)+yp[19,26:27],
               rep(op$opCost[20],3), rep(op$opCost[56],11),op$opCost[57]) 
    opm[1,seq(1,63)] <- oc; wbar_opc <- opm  
    
    opm <- matrix(rep(0,1*44), ncol=44)
    oc <- ct*c(rep(op$opCost[21],8),rep(op$opCost[22],13),
               rep(op$opCost[23],7)+yp[23,12:18],rep(op$opCost[24],5),
               rep(op$opCost[25],2)+yp[25,28:29], 
               rep(op$opCost[26],3)+yp[26,28:30],
               rep(op$opCost[58],4),rep(op$opCost[59],2)) 
    opm[1,seq(1,44)] <- oc; sbar_opc <- opm 
    
    #wbea
    opm <- matrix(rep(0,1*36), ncol=36)
    oc <- ct*c(rep(op$opCost[27],8),rep(op$opCost[28],3),
               rep(op$opCost[29],8),rep(op$opCost[30],11),
               rep(op$opCost[31],2)+yp[31,28:29],rep(op$opCost[60],4))   
    opm[1,seq(1,36)] <- oc; wbea_opc <- opm
    
    opm <- matrix(rep(0,1*31), ncol=31)
    oc <- ct*c(rep(op$opCost[32],8),rep(op$opCost[33],13),
               rep(op$opCost[34],4)+yp[34,15:18],
               rep(op$opCost[35],2)+yp[35,29:30],rep(op$opCost[61],4))   
    opm[1,seq(1,31)] <- oc; sbea_opc <- opm
    
    opm <- matrix(rep(0,1*48), ncol=48)
    oc <- ct*c(rep(op$opCost[36],13),rep(op$opCost[37],4),
               rep(op$opCost[38],4)+yp[38,17:20],rep(op$opCost[39],3),
               rep(op$opCost[40],3)+yp[40,31:33],rep(op$opCost[62],10),
               rep(op$opCost[63],6),rep(op$opCost[64],5))   
    opm[1,seq(1,48)] <- oc; wpot_opc <- opm   
    
    #=wosr
    opm <- matrix(rep(0,1*38), ncol=38)
    oc <- ct*c(rep(op$opCost[41],8),rep(op$opCost[42],11),
               rep(op$opCost[43],4)+yp[43,1:4],
               rep(op$opCost[44],2)+yp[44,26:27],rep(op$opCost[65],8),
               rep(op$opCost[66],5))   
    opm[1,seq(1,38)] <- oc; wosr_opc <- opm  
    
    #sbeet
    opm <- matrix(rep(0,1*46), ncol=46)
    oc <- ct*c(rep(op$opCost[45],13),rep(op$opCost[46],4),
               rep(op$opCost[47],4)+yp[47,17:20],
               rep(op$opCost[48],6)+yp[48,31:36],rep(op$opCost[67],8),
               rep(op$opCost[68],6),rep(op$opCost[69],5))   
    opm[1,seq(1,46)] <- oc; sbee_opc <- opm  
    
    #seta
    opm <- matrix(rep(0,1*6), ncol=6)
    oc <- ct*c(op$opCost[49],rep(op$opCost[50],2),op$opCost[51],
               rep(op$opCost[70],2))    
    opm[1,seq(1,6)] <- oc; seta_opc <- opm 
    
    opc <- cbind(wwht_opc,wwht_opc,wwht_opc,wwht_opc,swht_opc,wbar_opc,sbar_opc,
                 wbea_opc,sbea_opc,wpot_opc,wosr_opc,sbee_opc,seta_opc) 
    
    nfi <- opc
  }
  
  #============================= AREA OF FIRST OPERATION ======================
  
  areaFirstConsMatrix <- function(){
    
    # This function generates a matrix which will equate the area of 
    # first operation on a crop to the total area of that crop
    nro <- 32
    
    ca1 <- matrix(rep(0,nro*59), ncol=59)
    ca1[1,seq(47,48)] <- rep(1, length(ca1[1,seq(47,48)]))
    wwht_ca <- ca1 
    
    ca1 <- matrix(rep(0,nro*59), ncol=59)
    ca1[2,seq(47,48)] <- rep(1, length(ca1[1,seq(47,48)]))
    wwht_ca2 <- ca1
    
    ca1 <- matrix(rep(0,nro*59), ncol=59)
    ca1[3,seq(47,48)] <- rep(1, length(ca1[1,seq(47,48)]))
    wwht_ca3 <- ca1
    
    ca1 <- matrix(rep(0,nro*59), ncol=59)
    ca1[4,seq(47,48)] <- rep(1, length(ca1[1,seq(47,48)]))
    wwht_ca4 <- ca1
    
    ca1 <- matrix(rep(0,nro*44), ncol=44)
    ca1[5,seq(34,35)] <- rep(1, length(ca1[1,seq(34,35)]))
    swht_ca <- ca1 
    
    ca1 <- matrix(rep(0,nro*63), ncol=63)
    ca1[6,seq(49,51)] <- rep(1, length(ca1[1,seq(49,51)]))
    wbar_ca <- ca1
    
    ca1 <- matrix(rep(0,nro*44), ncol=44)
    ca1[7,seq(36,38)] <- rep(1, length(ca1[1,seq(36,38)]))
    sbar_ca <- ca1
    
    ca1 <- matrix(rep(0,nro*36), ncol=36)
    ca1[8,seq(31,32)] <- rep(1, length(ca1[1,seq(31,32)]))
    wbea_ca <- ca1 
    
    ca1 <- matrix(rep(0,nro*31), ncol=31)
    ca1[9,seq(26,27)] <- rep(1, length(ca1[1,seq(26,27)]))
    sbea_ca <- ca1
    
    ca1 <- matrix(rep(0,nro*48), ncol=48)
    ca1[10,seq(25,27)] <- rep(1, length(ca1[1,seq(25,27)]))
    wpot_ca <- ca1
    
    ca1 <- matrix(rep(0,nro*38), ncol=38)
    ca1[11,seq(24,25)] <- rep(1, length(ca1[1,seq(24,25)]))
    wosr_ca <- ca1 
    
    ca1 <- matrix(rep(0,nro*46), ncol=46)
    ca1[12,seq(22,27)] <- rep(1, length(ca1[1,seq(22,27)]))
    sbee_ca <- ca1 
    
    ca1 <- matrix(rep(0,nro*6), ncol=6)
    ca1[13,4] <- 1 #rep(1, length(ca1[13,1]))
    seta_ca <- ca1
    
    crp <- cbind(wwht_ca,wwht_ca2,wwht_ca3,wwht_ca4,swht_ca,wbar_ca,sbar_ca,wbea_ca,
                 sbea_ca,wpot_ca,wosr_ca,sbee_ca,seta_ca)
    
    kk <- crp 
  } 
  
  #======================== WORK RATE FOR CROP OPERATIONS (MATRIX) =================================
  
  seqWorkRateMatrix <- function(soil){
    # This function creates the work rate (h/ha) matrix for 
    # the LP problem and stores it ====
    # This estimates 
    # the work rate for each operation performed on a crop enterprise
    
    # Periodic Work rate for Winter wheat sequential operations
    # Creates a matrix with zero values
    
    if(missing(soil)){
      warning("soil argument missing")
    }else{
      WR <- workRateCal(soil)
      vl <- 72
      
      wwhtm <- matrix(rep(0,vl*59), ncol=59)
      wwhtm[seq(16,23),seq(1,8)] <- diag(WR$tractor[1],8,8) #P/K spreading
      wwhtm[seq(16,26),seq(9,19)] <- diag(WR$tractor[2],11,11) #Plough
      wwhtm[seq(19,26),seq(20,27)] <- diag(WR$tractor[3],8,8) # Plant
      wwhtm[seq(19,26),seq(28,35)] <- diag(WR$tractor[4],8,8) # Roll
      wwhtm[seq(22,26),seq(36,40)] <-  wwhtm[seq(49,53),seq(36,40)] <-
        diag(WR$tractor[5],5,5) #Spray===
      wwhtm[seq(1,6),seq(41,46)] <-wwhtm[seq(31,36),seq(41,46)] <- 
        diag(WR$sprayer[5],6,6)# Spray===
      #========
      wwhtm[seq(17,18),seq(47,48)] <- diag(WR$tractor[6],2,2)# Combine
      wwhtm[seq(56,57),seq(47,48)] <- diag(WR$combine_have[6],2,2)# Combine===
      wwhtm[seq(17,19),seq(49,51)] <- diag(WR$tractor[7],3,3) #Bale
      wwhtm[seq(61,63),seq(49,51)] <- diag(WR$baler[7],3,3) #Bale===
      
      #Non-seq
      wwhtm[seq(5,10),seq(52,57)] <- wwhtm[seq(35,40),seq(52,57)] <- 
        diag(WR$tractor[52],6,6)
      wwhtm[seq(7,8),seq(58,59)] <- diag(WR$tractor[53],2,2)
      
      newwwhtm1 <- wwhtm#*0.5
      
      newwwhtm1[,seq(49,51)] <- 0 # Remove wheat bailing
      newwwhtm <- newwwhtm1 #=====
      
      # Work rate for spring wheat sequential operations 
      # Creates a matrix with zero values for SWHT
      swhtm <- matrix(rep(0,vl*44), ncol=44) 
      swhtm[seq(16,23),seq(1,8)] <- diag(WR$tractor[8],8,8)
      swhtm[seq(20,26),seq(9,15)] <- diag(WR$tractor[9],7,7)
      swhtm[seq(1,6),seq(16,21)] <- diag(WR$tractor[9],6,6)
      swhtm[seq(1,7),seq(22,28)] <- diag(WR$tractor[10],7,7)
      swhtm[seq(5,9),seq(29,33)] <- diag(WR$tractor[11],5,5)
      
      #======================
      swhtm[seq(18,19),seq(34,35)] <- diag(WR$tractor[12],2,2)
      swhtm[seq(57,58),seq(34,35)] <- diag(WR$combine_have[12],2,2)#===
      swhtm[seq(17,19),seq(36,38)] <- diag(WR$tractor[13],3,3)
      swhtm[seq(61,63),seq(36,38)] <- diag(WR$baler[13],3,3) #====
      
      # Non-seq
      swhtm[seq(7,10),seq(39,42)] <- swhtm[seq(37,40),seq(39,42)] <- 
        diag(WR$tractor[54],4,4)
      swhtm[seq(7,8),seq(43,44)] <- diag(WR$tractor[55],2,2)
      newswhtm1 <- swhtm
      
      newswhtm1[,seq(36,38)] <- 0 # Remove wheat bailing
      newswhtm <-  newswhtm1
      
      # Periodic Work rate for winter barley sequential operations
      wbarm <- matrix(rep(0,vl*63), ncol=63) 
      
      wbarm[seq(16,23),seq(1,8)] <- diag(WR$tractor[14],8,8)
      wbarm[seq(16,26),seq(9,19)] <- diag(WR$tractor[15],11,11)
      wbarm[seq(19,26),seq(20,27)] <- diag(WR$tractor[16],8,8)
      wbarm[seq(19,26),seq(28,35)] <- diag(WR$tractor[17],8,8)
      
      wbarm[seq(22,26),seq(36,40)] <- wbarm[seq(49,53),seq(36,40)] <- 
        diag(WR$tractor[18],5,5)
      wbarm[seq(1,6),seq(41,46)] <- wbarm[seq(31,36),seq(41,46)] <- 
        diag(WR$tractor[18],6,6)
      
      #===============
      wbarm[seq(15,16),seq(47,48)] <- diag(WR$tractor[19],2,2)
      wbarm[seq(54,55),seq(47,48)] <- diag(WR$combine_have[19],2,2)#===
      wbarm[seq(15,17),seq(49,51)] <- diag(WR$tractor[20],3,3)
      wbarm[seq(59,61),seq(49,51)] <- diag(WR$baler[20],3,3)#===
      
      # Non-seq
      wbarm[seq(5,10),seq(52,57)] <- wbarm[seq(35,40),seq(52,57)] <- 
        diag(WR$tractor[70],6,6)
      wbarm[seq(22,26),seq(58,62)] <- wbarm[seq(49,53),seq(58,62)] <- 
        diag(WR$tractor[56],5,5)
      wbarm[6,63] <- diag(WR$tractor[57],1,1)
      
      newwbarm <- wbarm
      
      # Periodic work rate for spring barley
      # Creates a matrix with zero values for SBAR
      sbarm <- matrix(rep(0,vl*44), ncol=44) 
      
      sbarm[seq(16,23),seq(1,8)] <- diag(WR$tractor[21],8,8)
      sbarm[seq(20,26),seq(9,15)] <- diag(WR$tractor[22],7,7)
      sbarm[seq(1,6),seq(16,21)] <- diag(WR$tractor[22],6,6)
      sbarm[seq(1,7),seq(22,28)] <- diag(WR$tractor[23],7,7)
      sbarm[seq(5,9),seq(29,33)] <- diag(WR$tractor[24],5,5)
      
      #======================
      sbarm[seq(17,18),seq(34,35)] <- diag(WR$tractor[25],2,2)
      sbarm[seq(56,57),seq(34,35)] <- diag(WR$combine_have[25],2,2)#===
      sbarm[seq(17,19),seq(36,38)] <- diag(WR$tractor[26],3,3)
      sbarm[seq(61,63),seq(36,38)] <- diag(WR$baler[26],3,3) #====
      
      # Non-seq
      sbarm[seq(7,10),seq(39,42)] <- sbarm[seq(37,40),seq(39,42)] <- 
        diag(WR$tractor[58],4,4)
      sbarm[seq(7,8),seq(43,44)] <- diag(WR$tractor[59],2,2)
      newsbarm <- sbarm
      
      # Periodic Work rates for winter beans
      wbeam <- matrix(rep(0,vl*36), ncol=36)
      wbeam[seq(16,23),seq(1,8)] <- diag(WR$tractor[27],8,8)
      wbeam[seq(21,23),seq(9,11)] <- diag(WR$tractor[28],3,3)
      wbeam[seq(19,26),seq(12,19)] <- diag(WR$tractor[29],8,8)
      
      wbeam[seq(22,26),seq(20,24)] <- wbeam[seq(49,53),seq(20,24)] <- 
        diag(WR$tractor[30],5,5)
      wbeam[seq(1,6),seq(25,30)] <- wbeam[seq(31,36),seq(25,30)] <- 
        diag(WR$tractor[30],6,6)
      
      #======
      wbeam[seq(17,18),seq(31,32)] <- diag(WR$tractor[31],2,2)
      wbeam[seq(17,18),seq(31,32)] <- diag(WR$combine_have[31],2,2)#==
      
      #Non_Seq
      wbeam[seq(9,10),seq(33,34)] <- wbeam[seq(39,40),seq(33,34)] <- 
        diag(WR$tractor[60],2,2)
      wbeam[seq(12,13),seq(35,36)] <- wbeam[seq(42,43),seq(35,36)] <- 
        diag(WR$tractor[60],2,2)
      newwbeam <- wbeam
      
      # Spring beans
      sbeam <- matrix(rep(0,vl*31), ncol=31)  
      sbeam[seq(16,23),seq(1,8)] <- diag(WR$tractor[32],8,8)
      sbeam[seq(20,26),seq(9,15)] <- diag(WR$tractor[33],7,7)
      sbeam[seq(1,6),seq(16,21)] <- diag(WR$tractor[33],6,6)
      sbeam[seq(4,7),seq(22,25)] <- diag(WR$tractor[34],4,4)
      
      #=======
      sbeam[seq(18,19),seq(26,27)] <- diag(WR$tractor[35],2,2)
      sbeam[seq(57,58),seq(26,27)] <- diag(WR$combine_have[35],2,2)#====
      
      # Non-seq
      sbeam[seq(9,10),seq(28,29)] <- sbeam[seq(39,40),seq(28,29)] <- 
        diag(WR$tractor[61],2,2)
      sbeam[seq(12,13),seq(30,31)] <- sbeam[seq(42,43),seq(30,31)] <- 
        diag(WR$tractor[61],2,2)
      newsbeam <- sbeam
      
      # Ware potatoes 
      wpotm <- matrix(rep(0,vl*48), ncol=48)
      wpotm[seq(20,26),seq(1,7)] <- diag(WR$tractor[36],7,7)
      wpotm[seq(1,6),seq(8,13)] <- diag(WR$tractor[36],6,6)
      wpotm[seq(5,8),seq(14,17)] <- wpotm[seq(27,30),seq(14,17)] <- 
        diag(WR$tractor[37],4,4)#===
      wpotm[seq(6,9),seq(18,21)] <- diag(WR$tractor[38],4,4)
      wpotm[seq(7,9),seq(22,24)] <- diag(WR$tractor[39],3,3)
      #======
      wpotm[seq(20,22),seq(25,27)] <- diag(WR$tractor[40],3,3)
      wpotm[seq(64,66),seq(25,27)] <- diag(WR$pot_harvester[40],3,3)#===
      
      # Non-seq
      wpotm[seq(5,8),seq(28,31)] <- wpotm[seq(35,38),seq(28,31)] <- 
        diag(WR$tractor[62],4,4)
      wpotm[seq(12,17),seq(32,37)] <- wpotm[seq(42,47),seq(32,37)] <- 
        diag(WR$tractor[62],6,6)
      wpotm[seq(1,6),seq(38,43)] <- diag(WR$tractor[63],6,6)
      wpotm[seq(9,13),seq(44,48)] <- diag(WR$tractor[64],5,5)
      newwpotm <- wpotm
      
      # Winter oilseed rape 
      wosrm <- matrix(rep(0,vl*38), ncol=38)
      wosrm[seq(16,23),seq(1,8)] <- diag(WR$tractor[41],8,8)
      wosrm[seq(16,26),seq(9,19)] <- diag(WR$tractor[42],11,11)
      wosrm[seq(16,19),seq(20,23)] <- diag(WR$tractor[43],4,4)
      wosrm[seq(15,16),seq(24,25)] <- diag(WR$tractor[44],2,2)
      wosrm[seq(54,55),seq(24,25)] <- diag(WR$combine_have[44],2,2)#===
      
      # Non-seq
      wosrm[seq(5,8),seq(26,29)] <- wosrm[seq(35,38),seq(26,29)] <- 
        diag(WR$tractor[65],4,4)
      wosrm[seq(21,24),seq(30,33)] <- wosrm[seq(48,51),seq(30,33)] <- 
        diag(WR$tractor[65],4,4)
      wosrm[seq(4,6),seq(34,36)] <- diag(WR$tractor[66],3,3)
      wosrm[seq(8,9),seq(37,38)] <- diag(WR$tractor[66],2,2)
      newwosrm <- wosrm
      
      # Sugarbeet
      sbeem <- matrix(rep(0,vl*46), ncol=46)
      sbeem[seq(20,26),seq(1,7)] <- diag(WR$tractor[45],7,7)
      sbeem[seq(1,6),seq(8,13)] <- diag(WR$tractor[45],6,6)
      sbeem[seq(5,8),seq(14,17)] <- sbeem[seq(27,30),seq(14,17)] <- 
        diag(WR$tractor[46],4,4)#===
      sbeem[seq(6,9),seq(18,21)] <- diag(WR$tractor[47],4,4)
      sbeem[seq(20,25),seq(22,27)] <- diag(WR$tractor[48],6,6)
      sbeem[seq(67,72),seq(22,27)] <- diag(WR$sbee_harvester[48],6,6)#===
      
      # Non-seq
      sbeem[seq(5,10),seq(28,33)] <- sbeem[seq(35,40),seq(28,33)] <- 
        diag(WR$tractor[67],6,6)#==
      sbeem[seq(14,15),seq(34,35)] <- sbeem[seq(44,45),seq(34,35)] <- 
        diag(WR$tractor[67],2,2)#==
      sbeem[seq(1,6),seq(36,41)] <- diag(WR$tractor[68],6,6)
      sbeem[seq(9,13),seq(42,46)] <- diag(WR$tractor[69],5,5)
      newsbeem <- sbeem
      
      # Setaside 
      setam <- matrix(rep(0,vl*6), ncol=6)
      setam[seq(10,11),seq(2,3)] <- diag(WR$tractor[50],2,2)
      #Non-seq
      setam[seq(9,10),seq(5,6)] <- setam[seq(39,40),seq(5,6)] <- 
        diag(WR$tractor[70],2,2)
      newsetam <- setam
      
      per <-1
      # Matrix for sequential operation work rates
      seqOpMatrix <- cbind(newwwhtm,newwwhtm,newwwhtm,newwwhtm,newswhtm,newwbarm,
                           newsbarm,newwbeam,newsbeam,newwpotm,newwosrm,newsbeem,
                           newsetam)*per
      
      kk <- seqOpMatrix
      
    }
  }
  
  #============================ LABOUR HOURS FOR SEQUENTIAL AND NON-SEQUENTIAL OPERATIONS ====================
  
  seqOpLabourMatrix <- function(soil){
    
    # This function creates the work rate (h/ha) matrix for the LP problem and stores it ====
    # This estimates the work rate for each operation performed on a crop enterprise
    
    if(missing(soil)){
      warning("soil argument missing")
    }else{
      
      WR <- workRateCal(soil)
      sew <- seqWorkRateMatrix(soil)
      vl <- 26
      
      wwhtm <- matrix(rep(0,vl*59), ncol=59)
      wwhtm[seq(16,23),seq(1,8)] <- diag(WR$labour[1],8,8) #P/K spreading
      wwhtm[seq(16,26),seq(9,19)] <- diag(WR$labour[2],11,11) #Plough
      wwhtm[seq(19,26),seq(20,27)] <- diag(WR$labour[3],8,8) # Plant
      wwhtm[seq(19,26),seq(28,35)] <- diag(WR$labour[4],8,8) # Roll
      wwhtm[seq(22,26),seq(36,40)] <- diag(WR$labour[5],5,5) #Spray===
      wwhtm[seq(1,6),seq(41,46)] <- diag(WR$labour[5],6,6)# Spray===
      #========
      wwhtm[seq(17,18),seq(47,48)] <- diag(WR$labour[6],2,2)# Combine
      wwhtm[seq(17,19),seq(49,51)] <- diag(WR$labour[7],3,3) #Bale
      
      #Non-seq
      wwhtm[seq(5,10),seq(52,57)] <- diag(WR$labour[52],6,6)
      wwhtm[seq(7,8),seq(58,59)] <- diag(WR$labour[53],2,2)
      
      newwwhtm1 <- wwhtm
      newwwhtm1[,seq(49,51)] <- 0 # Remove wheat bailing
      newwwhtm <- newwwhtm1 #=====
      
      # Work rate for spring wheat sequential operations 
      # Creates a matrix with zero values for SWHT
      swhtm <- matrix(rep(0,vl*44), ncol=44) 
      swhtm[seq(16,23),seq(1,8)] <- diag(WR$labour[8],8,8)
      swhtm[seq(20,26),seq(9,15)] <- diag(WR$labour[9],7,7)
      swhtm[seq(1,6),seq(16,21)] <- diag(WR$labour[9],6,6)
      swhtm[seq(1,7),seq(22,28)] <- diag(WR$labour[10],7,7)
      swhtm[seq(5,9),seq(29,33)] <- diag(WR$labour[11],5,5)
      #======================
      swhtm[seq(18,19),seq(34,35)] <- diag(WR$labour[12],2,2)
      swhtm[seq(17,19),seq(36,38)] <- diag(WR$labour[13],3,3)
      
      # Non-seq
      swhtm[seq(7,10),seq(39,42)] <- diag(WR$labour[54],4,4)
      swhtm[seq(7,8),seq(43,44)] <- diag(WR$labour[55],2,2)
      newswhtm1 <- swhtm
      newswhtm1[,seq(36,38)] <- 0 # Remove wheat bailing
      newswhtm <-  newswhtm1
      
      # Periodic Work rate for winter barley sequential operations
      wbarm <- matrix(rep(0,vl*63), ncol=63) 
      wbarm[seq(16,23),seq(1,8)] <- diag(WR$labour[14],8,8)
      wbarm[seq(16,26),seq(9,19)] <- diag(WR$labour[15],11,11)
      wbarm[seq(19,26),seq(20,27)] <- diag(WR$labour[16],8,8)
      wbarm[seq(19,26),seq(28,35)] <- diag(WR$labour[17],8,8)
      wbarm[seq(22,26),seq(36,40)] <- diag(WR$labour[18],5,5)
      wbarm[seq(1,6),seq(41,46)] <- diag(WR$labour[18],6,6)
      
      #===============
      wbarm[seq(15,16),seq(47,48)] <- diag(WR$labour[19],2,2)
      wbarm[seq(15,17),seq(49,51)] <- diag(WR$labour[20],3,3)
      
      # Non-seq
      wbarm[seq(5,10),seq(52,57)] <- diag(WR$labour[56],6,6)
      wbarm[seq(22,26),seq(58,62)] <- diag(WR$labour[56],5,5)
      wbarm[6,63] <- diag(WR$labour[57],1,1)
      newwbarm <- wbarm
      
      # Periodic work rate for spring barley
      # Creates a matrix with zero values for SBAR
      sbarm <- matrix(rep(0,vl*44), ncol=44) 
      
      sbarm[seq(16,23),seq(1,8)] <- diag(WR$labour[8],8,8)
      sbarm[seq(20,26),seq(9,15)] <- diag(WR$labour[9],7,7)
      sbarm[seq(1,6),seq(16,21)] <- diag(WR$labour[9],6,6)
      sbarm[seq(1,7),seq(22,28)] <- diag(WR$labour[10],7,7)
      sbarm[seq(5,9),seq(29,33)] <- diag(WR$labour[11],5,5)
      
      #======================
      sbarm[seq(17,18),seq(34,35)] <- diag(WR$labour[12],2,2)
      sbarm[seq(17,19),seq(36,38)] <- diag(WR$labour[13],3,3)
      
      # Non-seq
      sbarm[seq(7,10),seq(39,42)] <- diag(WR$labour[58],4,4)
      sbarm[seq(7,8),seq(43,44)] <- diag(WR$labour[59],2,2)
      newsbarm <- sbarm
      
      
      # Periodic Work rates for winter beans
      wbeam <- matrix(rep(0,vl*36), ncol=36)
      wbeam[seq(16,23),seq(1,8)] <- diag(WR$labour[27],8,8)
      wbeam[seq(21,23),seq(9,11)] <- diag(WR$labour[28],3,3)
      wbeam[seq(19,26),seq(12,19)] <- diag(WR$labour[29],8,8)
      wbeam[seq(22,26),seq(20,24)] <- diag(WR$labour[30],5,5)
      wbeam[seq(1,6),seq(25,30)] <- diag(WR$labour[30],6,6)
      
      #======
      wbeam[seq(17,18),seq(31,32)] <- diag(WR$labour[31],2,2)
      
      #Non_Seq
      wbeam[seq(9,10),seq(33,34)] <- diag(WR$labour[60],2,2)
      wbeam[seq(12,13),seq(35,36)] <- diag(WR$labour[60],2,2)
      newwbeam <- wbeam
      
      # Spring beans
      sbeam <- matrix(rep(0,vl*31), ncol=31)  
      sbeam[seq(16,23),seq(1,8)] <- diag(WR$labour[32],8,8)
      sbeam[seq(20,26),seq(9,15)] <- diag(WR$labour[33],7,7)
      sbeam[seq(1,6),seq(16,21)] <- diag(WR$labour[33],6,6)
      sbeam[seq(4,7),seq(22,25)] <- diag(WR$labour[34],4,4)
      
      #=======
      sbeam[seq(18,19),seq(26,27)] <- diag(WR$labour[35],2,2)
      
      # Non-seq
      sbeam[seq(9,10),seq(28,29)] <- diag(WR$labour[61],2,2)
      sbeam[seq(12,13),seq(30,31)] <- diag(WR$labour[61],2,2)
      newsbeam <- sbeam 
      
      # Ware potatoes ========
      wpotm <- matrix(rep(0,vl*48), ncol=48)
      wpotm[seq(20,26),seq(1,7)] <- diag(WR$labour[36],7,7)
      wpotm[seq(1,6),seq(8,13)] <- diag(WR$labour[36],6,6)
      wpotm[seq(5,8),seq(14,17)] <- diag(WR$labour[37],4,4)#===
      wpotm[seq(6,9),seq(18,21)] <- diag(WR$labour[38],4,4)
      wpotm[seq(7,9),seq(22,24)] <- diag(WR$labour[39],3,3)
      #======
      wpotm[seq(20,22),seq(25,27)] <- diag(WR$labour[40],3,3)
      
      # Non-seq
      wpotm[seq(5,8),seq(28,31)] <- diag(WR$labour[62],4,4)
      wpotm[seq(12,17),seq(32,37)] <- diag(WR$labour[62],6,6)
      wpotm[seq(1,6),seq(38,43)] <- diag(WR$labour[63],6,6)
      wpotm[seq(9,13),seq(44,48)] <- diag(WR$labour[64],5,5)
      newwpotm <- wpotm
      
      # Winter oilseed rape 
      wosrm <- matrix(rep(0,vl*38), ncol=38)
      wosrm[seq(16,23),seq(1,8)] <- diag(WR$labour[41],8,8)
      wosrm[seq(16,26),seq(9,19)] <- diag(WR$labour[42],11,11)
      wosrm[seq(16,19),seq(20,23)] <- diag(WR$labour[43],4,4)
      wosrm[seq(15,16),seq(24,25)] <- diag(WR$labour[44],2,2)
      
      # Non-seq
      wosrm[seq(5,8),seq(26,29)] <- diag(WR$labour[65],4,4)
      wosrm[seq(21,24),seq(30,33)] <- diag(WR$labour[65],4,4)
      wosrm[seq(4,6),seq(34,36)] <- diag(WR$labour[66],3,3)
      wosrm[seq(8,9),seq(37,38)] <- diag(WR$labour[66],2,2)
      newwosrm <- wosrm 
      
      # Sugarbeet
      sbeem <- matrix(rep(0,vl*46), ncol=46)
      sbeem[seq(20,26),seq(1,7)] <- diag(WR$labour[45],7,7)
      sbeem[seq(1,6),seq(8,13)] <- diag(WR$labour[45],6,6)
      sbeem[seq(5,8),seq(14,17)] <- diag(WR$labour[46],4,4)#===
      sbeem[seq(6,9),seq(18,21)] <- diag(WR$labour[47],4,4)
      sbeem[seq(20,25),seq(22,27)] <- diag(WR$labour[48],6,6)
      
      # Non-seq
      sbeem[seq(5,10),seq(28,33)] <- diag(WR$labour[67],6,6)#==
      sbeem[seq(14,15),seq(34,35)] <- diag(WR$labour[67],2,2)#==
      sbeem[seq(1,6),seq(36,41)] <- diag(WR$labour[68],6,6)
      sbeem[seq(9,13),seq(42,46)] <- diag(WR$labour[69],5,5)
      newsbeem <- sbeem
      
      # Setaside 
      setam <- matrix(rep(0,vl*6), ncol=6)
      setam[seq(10,11),seq(2,3)] <- diag(WR$labour[50],2,2)
      #Non-seq
      setam[seq(9,10),seq(5,6)] <- diag(WR$labour[70],2,2)
      newsetam <- setam
      
      per <-1.1
      # Matrix for sequential operation work rates
      seqLabMatrix <- cbind(newwwhtm,newwwhtm,newwwhtm,newwwhtm,newswhtm,newwbarm,
                            newsbarm,newwbeam,newsbeam,newwpotm,newwosrm,newsbeem,
                            newsetam)*per
      #,newnsosrm,newnlnsem,newndpeam
      
      kk <- seqLabMatrix
    }
  }
  
  #===================== CONSTRUCT THE CONSTRAINT MATRIX ======================
  
  seqOpConstraint <- function(){
    
    #===== Sequential Operation Constraints =====
    
    cv0 <- 0 
    cv <- -1
    cv1 <- 1
    
    # Winter wheat sequential operations
    wwht <- matrix(rep(0,53*59), ncol=59, nrow=53)
    ww <- matrix(rep(cv1,8*8),8,8); ww[upper.tri(ww)] <- cv0
    wwht[seq(1,8),seq(1,8)] <- ww
    wwht[seq(9,10),seq(1,8)] <- cv1
    ww0 <- matrix(rep(cv,8*8),8,8); ww0[upper.tri(ww0)] <- cv0
    wwht[seq(11,18),seq(1,8)] <- ww0
    wwht[seq(19,21),seq(1,8)] <- cv
    
    ww1 <- matrix(rep(cv1,11*11),11); ww1[upper.tri(ww1)] <- cv0
    wwht[seq(11,21),seq(9,19)] <- ww1
    wwht[seq(22,29),seq(9,12)] <- cv
    ww2 <- matrix(rep(cv,7*7),7); ww2[upper.tri(ww2)] <- cv0
    wwht[seq(23,29),seq(13,19)] <- ww2
    ww3 <- matrix(rep(cv1,8*8),8); ww3[upper.tri(ww3)] <- cv0
    wwht[seq(22,29),seq(20,27)] <- ww3; 
    ww4 <- matrix(rep(cv,8*8),8,8); ww4[upper.tri(ww4)] <- cv0
    wwht[seq(30,37),seq(20,27)] <- ww4
    
    ww5 <- matrix(rep(cv1,8*8),8,8); ww5[upper.tri(ww5)] <- cv0
    wwht[seq(30,37),seq(28,35)] <- ww5
    wwht[seq(38,42),seq(28,30)] <- cv
    ww6 <- matrix(rep(cv,5*5),5); ww6[upper.tri(ww6)] <- cv0
    wwht[seq(38,42),seq(31,35)] <- ww6; 
    wwht[seq(43,48),seq(28,35)] <- cv
    ww7 <- matrix(rep(cv1,11*11),11); ww7[upper.tri(ww7)] <- cv0
    wwht[seq(38,48),seq(36,46)] <- ww7
    wwht[seq(49,50),seq(36,46)] <- cv
    wwht[49,47] <- cv1; wwht[50,seq(47,48)] <- cv1
    wwht[51,47] <- cv; wwht[seq(52,53),seq(47,48)] <- cv
    ww9 <- matrix(rep(cv1,3*3),3); ww9[upper.tri(ww9)] <- cv0
    wwht[seq(51,53),seq(49,51)] <- ww9
    
    nwwht <- wwht #==
    
    ww2 <- nwwht
    ww2[c(1,9,10),] <- cv0; 
    nwwht2 <- ww2
    
    ww3 <- nwwht
    ww3[c(1,9,10),] <- cv0; nwwht3 <- ww3
    
    ww4 <- nwwht
    ww4[c(1,9,10),] <- cv0; nwwht4 <- ww4
    
    
    # Spring wheat sequential operations
    swht <- matrix(rep(0,39*44), ncol=44, nrow=39)
    sw <- matrix(rep(cv1,8*8),8,8); sw[upper.tri(sw)] <- cv0
    swht[seq(1,8),seq(1,8)] <- sw
    swht[seq(9,10),seq(1,8)] <- cv1
    swht[seq(11,14),seq(1,4)] <- cv 
    sw0 <- matrix(rep(cv,4*4),4); sw0[upper.tri(sw0)] <- cv0
    swht[seq(11,14),seq(5,8)] <- sw0 
    swht[seq(15,23),seq(1,8)] <- cv 
    
    sw1 <- matrix(rep(cv1,13*13),13); sw1[upper.tri(sw1)] <- cv0 
    swht[seq(11,23),seq(9,21)] <- sw1
    
    swht[seq(24,29),seq(9,15)] <- cv
    sw2 <- matrix(rep(cv,6*6),6); sw2[upper.tri(sw2)] <- cv0 
    swht[seq(24,29),seq(16,21)] <- sw2
    swht[30,seq(9,21)] <- cv
    sw3 <- matrix(rep(cv1,7*7),7); sw3[upper.tri(sw3)] <- cv0 
    swht[seq(24,30),seq(22,28)] <- sw3
    swht[seq(31,35),seq(22,28)] <- cv
    swht[31,c(27,28)] <- swht[32,28] <- cv0
    
    sw5 <- matrix(rep(cv1,5*5),5); sw5[upper.tri(sw5)] <- cv0 
    swht[seq(31,35),seq(29,33)] <- sw5
    swht[seq(36,37),seq(29,33)] <- cv
    swht[36,34] <- swht[37,seq(34,35)] <- cv1
    swht[38,34] <- swht[39,seq(34,35)] <- cv
    swht[38,seq(36,37)] <- swht[39,seq(36,38)] <- cv1
    
    nswht <- swht #==
    
    
    # Winter barley ======
    wbar <- matrix(rep(0,53*63), ncol=63, nrow=53)
    wb <- matrix(rep(cv1,8*8),8,8); wb[upper.tri(wb)] <- cv0
    wbar[seq(1,8),seq(1,8)] <- wb
    wbar[seq(9,10),seq(1,8)] <- cv1
    wb0 <- matrix(rep(cv,8*8),8,8); wb0[upper.tri(wb0)] <- cv0
    wbar[seq(11,18),seq(1,8)] <- wb0
    wbar[seq(19,21),seq(1,8)] <- cv
    wb1 <- matrix(rep(cv1,11*11),11); wb1[upper.tri(wb1)] <- cv0
    wbar[seq(11,21),seq(9,19)] <- wb1
    wbar[seq(22,29),seq(9,11)] <- cv
    wb2 <- matrix(rep(cv,8*8),8); wb2[upper.tri(wb2)] <- cv0
    wbar[seq(22,29),seq(12,19)] <- wb2
    
    wb3 <- matrix(rep(cv1,8*8),8); wb3[upper.tri(wb3)] <- cv0
    wbar[seq(22,29),seq(20,27)] <- wb3; 
    wb4 <- matrix(rep(cv,8*8),8); wb4[upper.tri(wb4)] <- cv0
    wbar[seq(30,37),seq(20,27)] <- wb4
    wb5 <- matrix(rep(cv1,8*8),8,8); wb5[upper.tri(wb5)] <- cv0
    wbar[seq(30,37),seq(28,35)] <- wb5
    wbar[seq(38,42),seq(28,30)] <- cv
    wb6 <- matrix(rep(cv,5*5),5); wb6[upper.tri(wb6)] <- cv0
    wbar[seq(38,42),seq(31,35)] <- wb6; 
    wbar[seq(43,48),seq(28,35)] <- cv 
    wb7 <- matrix(rep(cv1,11*11),11); wb7[upper.tri(wb7)] <- cv0
    wbar[seq(38,48),seq(36,46)] <- wb7
    wbar[seq(49,50),seq(36,46)] <- cv
    wbar[49,47] <- wbar[50,seq(47,48)] <- cv1
    wbar[51,47] <- wbar[seq(52,53),seq(47,48)] <- cv
    wb9 <- matrix(rep(cv1,3*3),3); wb9[upper.tri(wb9)] <- cv0
    wbar[seq(51,53),seq(49,51)] <- wb9
    
    nwbar <- wbar #==
    
    
    #== Spring barley
    sbar <- matrix(rep(0,40*44), ncol=44, nrow=40)
    sb <- matrix(rep(cv1,8*8),8); sb[upper.tri(sb)] <- cv0
    sbar[seq(1,8),seq(1,8)] <- sb
    sbar[seq(9,10),seq(1,8)] <- cv1
    sbar[seq(11,14),seq(1,4)] <- cv
    sb0 <- matrix(rep(cv,4*4),4); sb0[upper.tri(sb0)] <- cv0
    sbar[seq(11,14),seq(5,8)] <- sb0
    sbar[seq(15,23),seq(1,8)] <- cv
    sb1 <- matrix(rep(cv1,13*13),13); sb1[upper.tri(sb1)] <- cv0
    sbar[seq(11,23),seq(9,21)] <- sb1; 
    
    sbar[seq(24,29),seq(9,15)] <- cv
    sb2 <- matrix(rep(cv,6*6),6); sb2[upper.tri(sb2)] <- cv0
    sbar[seq(24,29),seq(16,21)] <- sb2; 
    sbar[30,seq(9,21)] <- cv
    sb3 <- matrix(rep(cv1,7*7),7); sb3[upper.tri(sb3)] <- cv0
    sbar[seq(24,30),seq(22,28)] <- sb3
    sbar[seq(31,35),seq(22,28)] <- cv
    sbar[31,seq(27,28)] <- sbar[32,28] <- cv0
    sb4 <- matrix(rep(cv1,5*5),5); sb4[upper.tri(sb4)] <- cv0
    sbar[seq(31,35),seq(29,33)] <- sb4; 
    sbar[seq(36,37),seq(29,33)] <- cv
    sbar[36,34] <- sbar[37,seq(34,35)] <- cv1 
    sbar[38,34] <- sbar[seq(39,40),seq(34,35)] <- cv
    sb5 <- matrix(rep(cv1,3*3),3); sb5[upper.tri(sb5)] <- cv0
    sbar[seq(38,40),seq(36,38)] <- sb5
    
    nsbar <- sbar #==
    
    # Winter beans ======
    wbea <- matrix(rep(0,30*36), ncol=36, nrow=30)
    wn <- matrix(rep(cv1,8*8),8); wn[upper.tri(wn)] <- cv0
    wbea[seq(1,8),seq(1,8)] <- wn
    wbea[seq(9,11),seq(1,8)] <- cv
    wbea[9,seq(7,8)] <- wbea[10,8] <- cv0
    wn1 <- matrix(rep(cv1,3*3),3); wn1[upper.tri(wn1)] <- cv0
    wbea[seq(9,11),seq(9,11)] <- wn1; 
    wn2 <- matrix(rep(cv,3*3),3); wn2[upper.tri(wn2)] <- cv0
    wbea[seq(12,14),seq(9,11)] <- wn2; wbea[seq(15,17),seq(9,11)] <- cv
    wbea[seq(12,17),seq(12,13)] <- cv1
    wn3 <- matrix(rep(cv1,6*6),6); wn3[upper.tri(wn3)] <- cv0
    wbea[seq(12,17),seq(14,19)] <- wn3
    
    wbea[seq(18,22),seq(12,14)] <- cv
    wn4 <- matrix(rep(cv,5*5),5); wn4[upper.tri(wn4)] <- cv0
    wbea[seq(18,22),seq(15,19)] <- wn4; wbea[seq(23,28),seq(12,19)] <- cv
    wn5 <- matrix(rep(cv1,11*11),11); wn5[upper.tri(wn5)] <- cv0
    wbea[seq(18,28),seq(20,30)] <- wn5
    wbea[seq(29,30),seq(20,30)] <- cv
    wbea[29,31] <- wbea[30,seq(31,32)] <- cv1
    
    nwbea <- wbea #=====
    
    
    #=== Spring barley 
    sbea <- matrix(rep(0,27*31), ncol=31, nrow=27)
    sn <- matrix(rep(cv1,8*8),8); sn[upper.tri(sn)] <- cv0
    sbea[seq(1,8),seq(1,8)] <- sn
    sbea[seq(9,21),seq(1,8)] <- cv
    sbea[9,seq(6,8)] <- sbea[10,seq(7,8)] <- sbea[11,8] <- cv0 
    sn1 <- matrix(rep(cv1,13*13),13); sn1[upper.tri(sn1)] <- cv0 
    sbea[seq(9,21),seq(9,21)] <- sn1
    sbea[seq(22,25),seq(9,21)] <- cv
    sbea[22,seq(20,21)] <- sbea[23,21] <- cv0
    sn3 <- matrix(rep(cv1,4*4),4); sn3[upper.tri(sn3)] <- cv0 
    sbea[seq(22,25),seq(22,25)] <- sn3
    sbea[seq(26,27),seq(22,25)] <- cv
    sn4 <- matrix(rep(cv1,2*2),2); sn4[upper.tri(sn4)] <- cv0 
    sbea[seq(26,27),seq(26,27)] <- sn4
    
    nsbea <- sbea #===
    
    #=== Ware (Main) potatoes
    wpot <- matrix(rep(0,27*48), ncol=48, nrow=27)
    wp <- matrix(rep(cv1,13*13),13); wp[upper.tri(wp)] <- cv0 
    wpot[seq(1,13),seq(1,13)] <- wp
    wpot[seq(14,17),seq(1,13)] <- cv
    wpot[14,13] <- cv0 
    wp1 <- matrix(rep(cv1,4*4),4); wp1[upper.tri(wp1)] <- cv0 
    wpot[seq(14,17),seq(14,17)] <- wp1
    wpot[seq(18,21),seq(14,17)] <- cv
    wpot[18,seq(16,17)] <- wpot[19,17] <- cv0
    wp2 <- matrix(rep(cv1,4*4),4); wp2[upper.tri(wp2)] <- cv0 
    wpot[seq(18,21),seq(18,21)] <- wp2
    wpot[seq(22,24),seq(18,21)] <- cv
    wpot[22,seq(20,21)] <- wpot[23,21] <- cv0
    wp4 <- matrix(rep(cv1,3*3),3); wp4[upper.tri(wp4)] <- cv0 
    wpot[seq(22,24),seq(22,24)] <- wp4
    wpot[seq(25,27),seq(22,24)] <- cv
    wp5 <- matrix(rep(cv1,3*3),3); wp5[upper.tri(wp5)] <- cv0 
    wpot[seq(25,27),seq(25,27)] <- wp5; 
    
    nwpot <- wpot #====
    
    
    #=== Winter oilseed rape 
    wosr <- matrix(rep(0,32*38), ncol=38, nrow=32)
    wo <- matrix(rep(cv1,8*8),8); wo[upper.tri(wo)] <- cv0
    wosr[seq(1,8),seq(1,8)] <- wo
    wo0 <- matrix(rep(cv,8*8),8); wo0[upper.tri(wo0)] <- cv0
    wosr[seq(9,16),seq(1,8)] <- wo0
    wosr[seq(17,19),seq(1,8)] <- cv
    wo1 <- matrix(rep(cv1,11*11),11); wo1[upper.tri(wo1)] <- cv0
    wosr[seq(9,19),seq(9,19)] <- wo1
    wo2 <- matrix(rep(cv,11*11),11); wo2[upper.tri(wo2)] <- cv0
    wosr[seq(20,30),seq(9,19)] <- wo2
    
    wo3 <- matrix(rep(cv1,4*4),4); wo3[upper.tri(wo3)] <- cv0
    wosr[seq(20,23),seq(20,23)] <- wo3
    wosr[seq(24,30),seq(20,23)] <- cv1
    wosr[seq(31,32),seq(20,23)] <- cv
    wo4 <- matrix(rep(cv1,2*2),2); wo4[upper.tri(wo4)] <- cv0
    wosr[seq(31,32),seq(24,25)] <- wo4
    
    nwosr <- wosr #=====
    
    #=== Sugarbeet 
    sbee <- matrix(rep(0,27*46), ncol=46, nrow=27)
    st <- matrix(rep(cv1,13*13),13); st[upper.tri(st)] <- cv0 
    sbee[seq(1,13),seq(1,13)] <- st
    sbee[seq(14,17),seq(1,13)] <- cv; sbee[14,13] <- cv0
    st1 <- matrix(rep(cv1,4*4),4); st1[upper.tri(st1)] <- cv0 
    sbee[seq(14,17),seq(14,17)] <- st1
    sbee[seq(18,21),seq(14,17)] <- cv
    sbee[18,seq(16,17)] <- sbee[19,17] <- cv0
    
    st2 <- matrix(rep(cv1,4*4),4); st2[upper.tri(st2)] <- cv0 
    sbee[seq(18,21),seq(18,21)] <- st2
    sbee[seq(22,27),seq(18,21)] <- cv
    st5 <- matrix(rep(cv1,6*6),6); st5[upper.tri(st5)] <- cv0
    sbee[seq(22,27),seq(22,27)] <- st5
    
    nsbee <- sbee
    
    #=== Setaside 
    seta <- matrix(rep(0,4*6), ncol= 6, nrow=4)
    seta[1,1] <- cv1 ; seta[2,1] <- cv;
    seta[2,2] <- cv1; seta[3,1] <- cv; seta[3,seq(2,3)] <-  cv1; 
    seta[4,seq(2,3)] <- cv; seta[4,4] <-  cv1
    
    ma <- matrix(rep(cv0,6*7),ncol=6)
    ma[,1] <- cv1; ma1 <- ma
    nseta <- rbind(ma,seta)
    
    vl <- 498; hl <- 592
    newmat <- matrix(rep(0,vl*hl),ncol=hl)
    newmat[seq(1,53),seq(1,59)] <- nwwht;
    newmat[seq(54,106),seq(60,118)] <- nwwht2 
    newmat[seq(107,159),seq(119,177)] <- nwwht3 
    newmat[seq(160,212),seq(178,236)] <- nwwht4 
    newmat[seq(213,251),seq(237,280)] <- nswht
    newmat[seq(252,304),seq(281,343)] <- nwbar; 
    newmat[seq(305,344),seq(344,387)] <- nsbar; 
    newmat[seq(345,374),seq(388,423)] <- nwbea
    newmat[seq(375,401),seq(424,454)] <- nsbea; 
    newmat[seq(402,428),seq(455,502)] <- nwpot; 
    newmat[seq(429,460),seq(503,540)] <- nwosr
    newmat[seq(461,487),seq(541,586)] <- nsbee; 
    newmat[seq(488,vl),seq(587,592)] <- nseta;
    
    # Adjust Seta constratints
    newmat[seq(488,494),] <- cv0
    seqConsMat <- newmat
    
    kk <-  seqConsMat 
  } 
  
  #================== NON-SEQUENTIAL CONSTRAINT ========================= 
  
  nonseqOpConstraint <- function(){
    
    # This function generates the non-sequential constraint 
    # This matrix will be linked to the total crop area as well as
    # all possible crops following a crop in a rotation
    nro <- 38
    cv1 <- 1
    ns <- matrix(rep(0,nro*59), ncol=59)
    ns[seq(1,2),seq(52,57)] <- ns[3,seq(58,59)] <- cv1
    wwht_ns <- ns
    
    ns <- matrix(rep(0,nro*59), ncol=59)
    ns[seq(4,5),seq(52,57)] <- ns[6,seq(58,59)] <- cv1
    wwht_ns2 <- ns
    
    ns <- matrix(rep(0,nro*59), ncol=59)
    ns[seq(7,8),seq(52,57)] <- ns[9,seq(58,59)] <- cv1
    wwht_ns3 <- ns
    
    ns <- matrix(rep(0,nro*59), ncol=59)
    ns[seq(10,11),seq(52,57)] <- ns[12,seq(58,59)] <- cv1
    wwht_ns4 <- ns
    
    #============
    ns <- matrix(rep(0,nro*44), ncol=44)
    ns[seq(13,14),seq(39,42)] <- ns[15,seq(43,44)] <- cv1
    swht_ns <- ns
    
    ns <- matrix(rep(0,nro*63), ncol=63)
    ns[seq(16,17),seq(52,62)] <- ns[18,63] <- cv1
    wbar_ns <- ns
    
    ns <- matrix(rep(0,nro*44), ncol=44)
    ns[seq(19,20),seq(39,42)] <- ns[21,seq(43,44)] <- cv1
    sbar_ns <- ns
    
    ns <- matrix(rep(0,nro*36), ncol=36)
    ns[seq(22,23),seq(33,36)] <- cv1
    wbea_ns <- ns
    
    ns <- matrix(rep(0,nro*31), ncol=31)
    ns[seq(24,25),seq(28,31)] <- cv1
    sbea_ns <- ns
    
    ns <- matrix(rep(0,nro*48), ncol=48)
    ns[seq(26,27),seq(28,37)] <- ns[28,seq(38,43)] <- cv1
    ns[29,seq(44,48)] <- cv1
    wpot_ns <- ns
    
    ns <- matrix(rep(0,nro*38), ncol=38)
    ns[seq(30,31),seq(26,33)] <- ns[32,seq(34,38)] <- cv1
    wosr_ns <- ns
    
    ns <- matrix(rep(0,nro*46), ncol=46)
    ns[seq(33,34),seq(28,35)] <- ns[35,seq(36,41)] <- cv1
    ns[36,seq(42,46)] <- cv1
    sbee_ns <- ns
    
    ns <- matrix(rep(0,nro*6), ncol=6)
    ns[seq(37,38),seq(5,6)] <- cv1
    seta_ns <- ns
    
    nso <- cbind(wwht_ns,wwht_ns2,wwht_ns3,wwht_ns4,swht_ns,wbar_ns,
                 sbar_ns,wbea_ns,sbea_ns,wpot_ns,wosr_ns,sbee_ns,seta_ns)
    
    #,sosr_ns,lnse_ns,dpea_ns
    
    kk <- nso
  }
  
  #======================== ROTATION SEQUENCING MATRIX ==========================================
  
  lastOpRotMatrix <- function(){
    
    # this function genertates a matrix for the last operation
    # of each crop in a specific period.
    # This matrix will link the last operations to all the possible 
    # crops in the rotation (crop) sequence. 
    non <- nonseqOpConstraint()
    cv <- -1
    vl <- 37
    rs <- matrix(rep(0,vl*length(non[1,])), length(non[1,]), nrow=vl)
    # Winter wheat
    rs[1,47] <- rs[2,48] <- cv
    rs[4,106] <- rs[5,107] <- cv
    rs[7,165] <- rs[8,166] <- cv
    rs[10,224] <- rs[11,225] <- cv
    
    # Spring wheat
    rs[13,270] <- rs[14,271] <- cv
    
    #rs[13,272] <- rs[14,273] <- rs[15,274] <- cv
    
    # Winter barley
    rs[16,329] <- rs[17,330] <- rs[18,331] <- cv
    # Spring barley
    rs[19,379] <- rs[20,380] <- rs[21,381] <- cv
    # Winter beans
    rs[22,418] <- rs[23,419] <- cv
    # Spring beans
    rs[24,449] <- rs[25,450] <- cv
    # Ware potatoes
    rs[26,479] <- rs[27,480] <- rs[28,481] <- cv
    # Winter oilseed rape
    rs[29,526] <- rs[30,527] <- cv
    # Sugarbeet
    rs[31,562] <- rs[32,563] <- rs[33,564] <- cv
    rs[34,565] <- rs[35,566] <- rs[36,567] <- cv
    # Setaside
    rs[37,590] <- cv
    
    lop <- rs
  }
  
  #============================ ROTATION MATRIX ===============================
  
  rotSeqMatrix <- function(){
    
    # This function create the matrix for the crop rotation/sequence by 
    # Linking the area of a crop rotation from other crops to the 
    # first operation of that that crop. 
    
    # About 281 possible crop sequences in terms of crops which can follow 
    # others were created.
    con <- seqOpConstraint()
    ncon <- nonseqOpConstraint()
    lop <- lastOpRotMatrix()
    
    vl <- length(con[,1]) #+length(ncon[,1])
    vl1 <- length(ncon[,1])
    vl2 <- length(lop[,1])
    hl <- 281
    #cs <- crops
    cv0 <- 0
    cv <- -1 # 
    cv1 <- 1
    rm <- matrix(rep(cv0,vl*hl),ncol=hl)
    
    vt2 <- rep(cv,2)
    vt3 <- rep(cv,3)
    vt4 <- rep(cv,4)
    vt5 <- rep(cv,5)
    vt6 <- rep(cv,6)
    vt7 <- rep(cv,7)
    vt8 <- rep(cv,8)
    vt9 <- rep(cv,9)
    vt10 <- rep(cv,10)
    vt11 <- rep(cv,11)
    vt12 <- rep(cv,12)
    vt13 <- rep(cv,13)
    
    #===1
    rm[seq(1,10), c(133,141)] <- vt10; 
    rm[seq(2,10),c(149,157)] <- vt9
    rm[seq(3,10),c(165)] <- vt8; rm[seq(4,10),c(173)] <- vt7
    rm[seq(2,10),181] <- vt9; rm[seq(3,10),c(188,195)] <- vt8
    rm[seq(4,10),202] <- vt7; rm[seq(5,10),209] <- vt6
    rm[seq(6,10),217] <- vt5; rm[seq(7,10),225] <- vt4
    rm[seq(1,10),c(233,238)] <- vt10; rm[seq(5,10),243] <- vt6
    rm[seq(6,10),248] <- vt5; rm[seq(7,10),253] <- vt4
    rm[seq(8,10),258] <- vt3; rm[seq(9,10),263] <- vt2
    rm[10,268] <- cv; rm[seq(1,10),273] <- vt10
    
    #===
    rm[seq(55,61),1] <- vt7; rm[seq(56,61),10] <- vt6; rm[seq(57,61),19] <- vt5
    rm[seq(108,114),29] <- vt7; rm[seq(109,114),38] <- vt6; rm[seq(110,114),47] <- vt5 
    rm[seq(161,167),c(55,82)] <- vt7; rm[seq(162,167),c(64,91)] <- vt6; 
    rm[seq(163,167),c(73,100)] <- vt5;
    
    #====2
    rm[seq(213,222),c(140,148)] <- vt10; rm[seq(214,222),c(156,164)] <- vt9;
    rm[seq(215,222),172] <- vt8;  rm[seq(213,222),c(237,242,281)] <- vt10;
    rm[seq(214,222),187] <- vt9;  rm[seq(215,222),c(194,201)] <- vt8;
    rm[seq(216,222),c(180,208)] <- vt7;  rm[seq(217,222),c(216,247)] <- vt6;
    rm[seq(218,222),c(224,252)] <- vt5; rm[seq(219,222),c(232,257)] <- vt4;
    rm[seq(220,222),262] <- vt3; rm[seq(221,222),267] <- vt2;
    rm[222,272] <- cv;
    
    #====3
    rm[seq(252,261),274] <- vt10; rm[seq(253,261),c(2,28,56,83,109,182)] <- vt9
    rm[seq(254,261),c(11,37,65,92,117,189,196)] <- vt8
    rm[seq(255,261),c(20,46,74,101,125,203)] <- vt7
    rm[seq(256,261),c(210,244)] <- vt6; rm[seq(257,261),c(218,249)] <- vt5
    rm[seq(258,261),c(226,254)] <- vt4; rm[seq(259,261),259] <- vt3
    rm[seq(260,261),264] <- vt2; rm[261,269] <- cv
    
    #======4
    rm[seq(305,314),c(234,239,275)] <- vt10; 
    rm[seq(306,314),c(3,30,57,84,110,183)] <- vt9
    rm[seq(307,314),c(12,39,66,93,118,190,197)] <- vt8
    rm[seq(308,314),c(21,48,75,102,126,204)] <- vt7
    rm[seq(309,314),c(211,245)] <- vt6; rm[seq(310,314),c(219,250)] <- vt5
    rm[seq(311,314),c(227,255)] <- vt4; rm[seq(312,314),260] <- vt3
    rm[seq(313,314),265] <- vt2; rm[314,270] <- cv
    
    #======5
    rm[seq(345,352),c(134,142,276)] <- vt8; 
    rm[seq(346,352),c(4,31,58,85,111,150,158)] <- vt7
    rm[seq(347,352),c(13,40,67,94,119,166)] <- vt6; 
    rm[seq(348,352),c(22,49,76,103,127,174)] <- vt5
    rm[seq(349,352),212] <- vt4; rm[seq(350,352),220] <- vt3
    rm[seq(351,352),228] <- vt2
    
    #===Potatoes-Beans
    rm[,c(212,220,228)] <- 0; 
    
    #=====6
    rm[seq(375,382),c(135,143,277)] <- vt8
    rm[seq(376,382),c(5,32,59,86,112,151,159)] <- vt7
    rm[seq(377,382),c(14,41,68,95,120,167)] <- vt6
    rm[seq(378,382),c(23,50,77,104,128,175)] <- vt5
    rm[seq(379,382),213] <- vt4; rm[seq(380,382),221] <- vt3
    rm[seq(381,382),229] <- vt2
    
    #===Potatoes-Beans
    rm[,c(213,221,229)] <- 0;
    
    #======7
    rm[seq(402,414),c(7,16,25,34,43,52,61,70,79,88,97,106,114,122,130,137,
                      145,153,161,169,177,184,191,198,205,235,
                      240,246,279)] <- vt13 
    rm[seq(403,414),251] <- vt12; rm[seq(404,414),256] <- vt11
    rm[seq(405,414),261] <- vt10; rm[seq(406,414),266] <- vt9
    rm[seq(407,414),271] <- vt8
    
    #======8
    rm[seq(429,436),c(136,144,278)] <- vt8
    rm[seq(430,436),c(6,33,60,87,113,152,160)] <- vt7
    rm[seq(431,436),c(15,42,69,96,121,168)] <- vt6
    rm[seq(432,436),c(24,51,78,105,129,179)] <- vt5
    rm[seq(433,436),214] <- vt4; rm[seq(434,436),222] <- vt3
    rm[seq(435,436),230] <- vt2 
    
    #======9 
    rm[seq(461,473),c(8,17,26,35,44,53,62,71,80,89,98,107,115,
                      123,131,138,146,154,162,170,178,185,192,199,206,280)] <- vt13
    
    #======10
    rm[seq(488,495),c(139,236)] <- vt8; rm[seq(489,495),c(147,241)] <- vt7
    rm[seq(490,495),c(9,36,63,90,116,155,163,186)] <- vt6
    rm[seq(491,495),c(18,45,72,99,124,171,193,200)] <- vt5
    rm[seq(492,495),c(27,54,81,108,132,179,207)] <- vt4
    rm[seq(493,495),215] <- vt3; rm[seq(494,495),223] <- vt2
    rm[495,231] <- cv
    
    
    newrm <- rm
    
    # First operation of Non-sequential operations to rotation matrix 
    #=====
    rm1 <- matrix(rep(cv0,vl1*hl),ncol=hl)
    #1
    rm1[1,c(133,141,149,157,165,173,181,188,195,202,
            209,217,225,233,238,243,248,253,258,263,268,273)] <- 
      rm1[4,c(1,10,19)] <- rm1[7,c(29,38,47)] <-
      rm1[10,c(55,64,73,82,91,100)] <- cv
    #2
    rm1[13,c(140,148,156,164,172,180,187,194,201,208,216,
             224,232,237,242,247,252,257,262,267,272,281)] <- cv
    #3
    rm1[16,c(2,11,20,28,37,46,56,65,74,83,92,101,109,
             117,125,182,189,196,203,210,218,226,244,249,
             254,259,264,269,274)] <- cv 
    # 4
    rm1[19,c(3,12,21,30,39,48,57,66,75,84,93,102,110,118,126,
             183,190,197,204,211,219,227,234,239,245,250,
             255,260,265,270,275)] <- cv
    # 5
    rm1[22,c(4,13,22,31,40,49,58,67,76,85,94,103,111,119,127,
             134,142,150,158,166,174,212,220,228,276)] <- cv
    # 6
    rm1[24,c(5,14,23,32,41,50,59,68,77,86,95,
             104,112,120,128,135,143,151,159,167,175,213,221,
             229,277)] <- cv
    # 7
    rm1[26,c(7,16,25,34,43,52,61,70,79,88,97,106,114,
             122,130,137,145,153,161,169,177,184,191,198,205,235,
             240,246,251,256,261,266,271,279)] <- cv
    
    # 8
    rm1[30,c(6,15,24,33,42,51,60,69,78,87,96,105,113,121,129,
             136,144,152,160,168,176,214,222,230,278)] <- cv
    # 9
    rm1[33,c(8,17,26,35,44,53,62,71,80,89,98,107,115,
             123,131,138,146,154,162,170,178,185,192,199,206,280)] <- cv
    #10
    rm1[37,c(9,18,27,36,45,54,63,72,81,90,99,108,116,124,132,139,
             147,155,163,171,179,186,193,200,207,
             215,223,231,236,241)] <- cv
    
    rm1[37,c(27,54,81,108,132)] <- 0
    
    newrm1 <- rm1 #====
    
    #==================
    #cv2 <- 0
    # Possible rotation matrix
    fcr <- matrix(rep(cv0,vl2*hl),ncol=hl)
    fcr[1,seq(1,9)] <- fcr[2,seq(10,18)] <- 
      fcr[3,seq(19,27)] <- cv1
    fcr[4,seq(28,36)] <- fcr[5,seq(37,45)] <- 
      fcr[6,seq(46,54)] <- cv1
    fcr[7,seq(55,63)] <- fcr[8,seq(64,72)] <- 
      fcr[9,seq(73,81)] <- cv1
    fcr[10,seq(82,90)] <- fcr[11,seq(91,99)] <- 
      fcr[12,seq(100,108)] <- cv1
    
    fcr[13,seq(109,116)] <- fcr[14,seq(117,124)] <- 
      fcr[15,seq(125,132)] <- cv1
    
    fcr[16,seq(133,140)] <- fcr[17,seq(141,148)] <- 
      fcr[18,seq(149,156)] <- cv1 
    fcr[19,seq(157,164)] <-  fcr[20,seq(165,172)] <- 
      fcr[21,seq(173,180)] <-   cv1
    
    fcr[22,seq(181,187)] <- fcr[23,seq(188,194)] <- cv1
    fcr[24,seq(195,201)] <- fcr[25,seq(202,208)] <- cv1
    
    fcr[26,seq(209,216)] <- fcr[27,seq(217,224)] <- 
      fcr[28,seq(225,232)] <- cv1
    
    fcr[29,seq(233,237)] <- fcr[30,seq(238,242)] <- cv1
    
    fcr[31,seq(243,247)] <- fcr[32,seq(248,252)] <- 
      fcr[33,seq(253,257)] <- cv1
    fcr[34,seq(258,262)] <- fcr[35,seq(263,267)] <- 
      fcr[36,seq(268,272)] <- cv1 
    
    fcr[37,seq(273,281)] <- cv1
    
    nfcr <- fcr
    
    rot <- rbind(newrm,newrm1,nfcr)
    
    rot[,c(seq(19,27),seq(46,54),seq(73,81),seq(100,108),
           seq(125,132))] <- 0 # Remove bailing
    
    rot1 <- rot
  }
  
  #=================================================================================
  
  modRotPen <- function(soil){
    
    # This function creates a matrix for the rotational 
    # penalties/cost to be used in the objective function
    # for profit maximization model
    
    if(missing(soil)){
      warning("soil argument missing")
    }else{
      
      rp <- rotPenalty(soil)
      se <- selfRotPen(soil)
      
      #cs <- crops
      cv <- -1
      cv2 <- 0
      
      w <- matrix(rep(0,2*27),ncol=27)
      w2 <- matrix(rep(0,2*27),ncol=27)
      w3 <- matrix(rep(0,2*27),ncol=27)
      w4 <- matrix(rep(0,2*27),ncol=27)
      
      w[1,c(1,10,19)] <- cv*se[5,14];w[1,c(2,11,20)] <- cv*rp[15,7] 
      w[1,c(3,12,21)] <- cv*rp[22,7]; w[1,c(7,16,25)] <- cv*rp[43,7]
      w[1,c(6,15,24)] <- cv*rp[50,7]; w[1,c(8,9,17,18,26)] <- cv*rp[57,7] #======
      ww <- w
      
      w2[1,c(2,11,20)] <- cv*se[5,15]; w2[1,c(1,10,19)] <- cv*rp[15,7]
      w2[1,c(3,12,21)] <- cv*rp[22,7]; w2[1,c(7,16,25)] <- cv*rp[43,7]
      w2[1,c(6,15,24)] <- cv*rp[50,7]; w2[1,c(8,9,17,18,26)] <- cv*rp[57,7]
      ww2 <- w2
      
      
      w3[1,c(1,10,19)] <- cv*se[5,16]; w3[1,c(2,11,20)] <- cv*rp[15,7]
      w3[1,c(3,12,21)] <- cv*rp[22,7]; w3[1,c(7,16,25)] <- cv*rp[43,7]
      w3[1,c(6,15,24)] <- cv*rp[50,7]; w3[1,c(8,9,17,18,26)] <- cv*rp[57,7]
      ww3 <- w3
      
      w4[1,c(1,10,19)] <- cv*se[5,16]; w4[1,c(2,11,20)] <- cv*rp[15,7]
      w4[1,c(3,12,21)] <- cv*rp[22,7]; w4[1,c(7,16,25)] <- cv*rp[43,7]
      w4[1,c(6,15,24)] <- cv*rp[50,7]; w4[1,c(8,9,17,18,26)] <- cv*rp[57,7]
      ww4 <- w4
      wwht <- cbind(ww,ww2,ww3,ww4)
      
      sw <- matrix(rep(0,2*24),ncol=24)
      sw[1,c(1,9,17)] <- cv*rp[15,7]
      sw[1,c(2,10,18)] <- cv*rp[22,7]; sw[1,c(6,14,22)] <- cv*rp[43,7]
      sw[1,c(5,13,21)] <- cv*rp[50,7]; sw[1,c(7,15,23)] <- cv*rp[57,7]
      swht <- sw
      
      
      wb <- matrix(rep(0,2*24),ncol=24)
      wb[1,c(1,9,17)] <- cv*rp[1,7]; wb[1,c(8,16,24)] <- cv*rp[8,7];
      wb[1,c(7,15,23)] <- cv*rp[57,7];#==========
      wb[1,c(5,13,21)] <- cv*rp[44,7];  
      wb[1,c(6,14,22)] <- cv*rp[58,7]; 
      wbar <- wb    
      
      
      sb <- matrix(rep(0,2*24),ncol=24)
      sb[1,c(1,9,17)] <- cv*rp[1,7]; sb[1,c(8,16,24)] <- cv*rp[8,7];
      sb[1,c(5,13,21)] <- cv*rp[44,7];  
      sb[1,c(6,14,22)] <- cv*rp[58,7];
      sbar <- sb
      
      wbea <- matrix(rep(0,2*14),ncol=14) 
      
      sbea <- matrix(rep(0,2*14),ncol=14) 
      
      wpot <- matrix(rep(0,2*24),ncol=24)
      
      wo <- matrix(rep(0,2*10),ncol=10)
      wo[1,c(1,6)] <- cv*rp[3,7]; wo[1,c(5,10)] <- cv*rp[10,7];
      wosr <- wo
      
      sb <- matrix(rep(0,2*30),ncol=30)
      sb[1,c(4,9,14,19,24,29)] <- cv*rp[47,7];
      sbee <- sb
      
      st <- matrix(rep(0,2*9),ncol=9)
      st[1,1] <- cv*rp[7,7]; st[1,9] <- cv*rp[14,7];
      st[1,2] <- cv*rp[21,7]; st[1,3] <- cv*rp[28,7];
      seta <- st
      
      rot <- cbind(wwht,swht,wbar,sbar,wbea,sbea,wpot,
                   wosr,sbee,seta) 
      rot1 <- rot
    }
  }
  
  #========= FUNCTIONS FOR MODEL OUTPUT DISPLAY =======================
  
  cropRotation <- function(rotation){  
    #===1
    rot <- round(rotation)
    #rot <- round(lpa$solution[606:886],1)
    wwht <- "WWHT"; wwht2 <- "WWHT2"; wwht3 <- "WWHT3"; wwht4 <- "WWHT4"
    swht <- "SWHT"; wbar <- "WBAR"; sbar <- "SBAR"; wbea <- "WBEA"
    sbea <- "SBEA"; wpot <- "WPOT"; wosr <- "WOSR"; sbee <- "SBEE"; seta <- "SETA"
    
    crp <- c(wwht,wwht2,wwht3,wwht4,swht,wbar,sbar,wbea,sbea,wpot,wosr,sbee,seta)
    actn <- length(crp)
    rotm <- matrix(rep(0,actn*actn),actn,dimnames = list(crp,crp))
    rotm1 <- rotm
    #=====1
    rotm[1,6] <- sum(rot[c(133,141,149)]); 
    rotm[1,7] <- sum(rot[c(157,165,173)])
    rotm[1,8] <- sum(rot[c(181,188)]); rotm[1,9] <- sum(rot[c(195,202)])
    rotm[1,10] <- sum(rot[c(209,217,225)]); rotm[1,11] <- sum(rot[c(233,238)]); 
    rotm[1,12] <- sum(rot[c(243,248,253,258,263,268)]); 
    rotm[1,13] <- sum(rot[273])
    #====2&3&4
    rotm[2,1] <- sum(rot[c(1,10,19)]); rotm[3,2] <- sum(rot[c(29,38,47)])
    rotm[4,3] <- sum(rot[c(55,64,73)]); rotm[4,4] <- sum(rot[c(82,91,100)])
    #====5
    rotm[5,6] <- sum(rot[c(140,148,156)]);  
    rotm[5,7] <- sum(rot[c(164,172,180)]);
    rotm[5,8] <- sum(rot[c(187,194)]); rotm[5,9] <- sum(rot[c(201,208)]);
    rotm[5,10] <- sum(rot[c(216,224,232)]); rotm[5,11] <- sum(rot[c(237,242)]);
    rotm[5,12] <- sum(rot[c(247,252,257,262,267,272)]); 
    rotm[5,13] <- sum(rot[c(281)]);
    #===6
    rotm[6,1] <- sum(rot[c(2,11,20)]); 
    rotm[6,2] <- sum(rot[c(28,37,46)]); rotm[6,3] <- sum(rot[c(56,65,74)]);
    rotm[6,4] <- sum(rot[c(83,92,101)]); rotm[6,5] <- sum(rot[c(109,117,125)]);
    rotm[6,8] <- sum(rot[c(182,189)]); rotm[6,9] <- sum(rot[c(196,203)]);
    rotm[6,10] <- sum(rot[c(210,218,226)]); 
    rotm[6,12] <- sum(rot[c(244,249,254,259,264,269)]); 
    rotm[6,13] <- sum(rot[c(274)]); 
    #===7
    rotm[7,1] <- sum(rot[c(3,12,21)]); rotm[7,2] <- sum(rot[c(30,39,48)]); 
    rotm[7,3] <- sum(rot[c(57,66,75)]); rotm[7,4] <- sum(rot[c(84,93,102)]); 
    rotm[7,5] <- sum(rot[c(110,118,126)]); rotm[7,8] <- sum(rot[c(183,190)]); 
    rotm[7,9] <- sum(rot[c(197,204)]); rotm[7,10] <- sum(rot[c(211,219,227)]); 
    rotm[7,11] <- sum(rot[c(234,239)]);
    rotm[7,12] <- sum(rot[c(245,250,255,260,265,270)]); 
    rotm[7,13] <- sum(rot[c(275)]);
    #==8 
    rotm[8,1] <- sum(rot[c(4,13,22)]);
    rotm[8,2] <- sum(rot[c(31,40,49)]);
    rotm[8,3] <- sum(rot[c(58,67,76)]); rotm[8,4] <- sum(rot[c(85,94,103)]);
    rotm[8,5] <- sum(rot[c(111,119,127)]); rotm[8,6] <- sum(rot[c(134,142,150)]);
    rotm[8,7] <- sum(rot[c(158,166,174)]); rotm[8,10] <- sum(rot[c(212,220,228)]);
    rotm[8,13] <- sum(rot[c(276)]);
    #===9
    rotm[9,1] <- sum(rot[c(5,14,23)]);
    rotm[9,2] <- sum(rot[c(32,41,50)]); rotm[9,3] <- sum(rot[c(59,68,77)]);
    rotm[9,4] <- sum(rot[c(86,95,104)]); rotm[9,5] <- sum(rot[c(112,120,128)]);
    rotm[9,6] <- sum(rot[c(135,143,151)]); rotm[9,7] <- sum(rot[c(159,167,175)]);
    rotm[9,10] <- sum(rot[c(213,221,229)]); rotm[9,13] <- sum(rot[c(277)]);
    #===10
    rotm[10,1] <- sum(rot[c(7,16,25)]);
    rotm[10,2] <- sum(rot[c(34,43,52)]); rotm[10,3] <- sum(rot[c(61,70,79)]);
    rotm[10,4] <- sum(rot[c(88,97,106)]); rotm[10,5] <- sum(rot[c(114,122,130)]);
    rotm[10,6] <- sum(rot[c(137,145,153)]);rotm[10,7] <- sum(rot[c(161,169,177)]);
    rotm[10,8] <- sum(rot[c(184,191)]);
    rotm[10,9] <- sum(rot[c(198,205)]); rotm[10,11] <- sum(rot[c(235,240)]);
    rotm[10,12] <- sum(rot[c(246,251,256,261,266,271)]); 
    rotm[10,13] <- sum(rot[c(279)]);
    #===11
    rotm[11,1] <- sum(rot[c(6,15,24)]); rotm[11,2] <- sum(rot[c(33,42,51)]);
    rotm[11,3] <- sum(rot[c(60,69,78)]); rotm[11,4] <- sum(rot[c(87,96,105)]);
    rotm[11,5] <- sum(rot[c(113,121,129)]); rotm[11,6] <- sum(rot[c(136,144,152)]);
    rotm[11,7] <- sum(rot[c(160,168,176)]); rotm[11,10] <- sum(rot[c(214,222,230)]);
    rotm[11,13] <- sum(rot[c(278)]);
    #===12
    rotm[12,1] <- sum(rot[c(8,17,26)]); 
    rotm[12,2] <- sum(rot[c(35,44,53)]);
    rotm[12,3] <- sum(rot[c(62,71,80)]); rotm[12,4] <- sum(rot[c(89,98,107)]);
    rotm[12,5] <- sum(rot[c(115,123,131)]); rotm[12,6] <- sum(rot[c(138,146,154)]);
    rotm[12,7] <- sum(rot[c(162,170,178)]); rotm[12,8] <- sum(rot[c(185,192)]);
    rotm[12,9] <- sum(rot[c(199,206)]); rotm[12,13] <- sum(rot[c(280)]);
    #====13
    rotm[13,1] <- sum(rot[c(9,18,27)]);
    rotm[13,2] <- sum(rot[c(36,45,54)]);
    rotm[13,3] <- sum(rot[c(63,72,81)]); rotm[13,4] <- sum(rot[c(90,99,108)]);
    rotm[13,5] <- sum(rot[c(116,124,132)]); rotm[13,6] <- sum(rot[c(139,147,155)]);
    rotm[13,7] <- sum(rot[c(163,171,179)]); rotm[13,8] <- sum(rot[c(186,193)]);
    rotm[13,9] <- sum(rot[c(200,207)]);
    rotm[13,10] <- sum(rot[c(215,223,231)]); rotm[13,11] <- sum(rot[c(236,241)]);
    rotmat <- rotm
    
    suppressWarnings( ab <- rotmat[rowSums(rotmat[,])>0,])
    
    suppressWarnings(abb <- ab[,colSums(ab !=0)>0])
    
  }     
  
  #===============================================================
  cropAreas <- function(cropping){
    
    cp <- cropping
    #=== Cropping 
    
    
    #Total_Area_ha <- ab[,colSums(ab !=0)>0]
    
    #kk <- cbind(Total_Area_ha)
    
    if(cp[1]<=0){
      Winter_wheat <- NULL
    }else{Winter_wheat <-cp[1]}
    
    if(cp[2]<=0){
      Winter_wheat_2nd <- NULL
    }else{Winter_wheat_2nd <-cp[2]}
    
    if(cp[3]<=0){
      Winter_wheat_3rd <- NULL
    }else{Winter_wheat_3rd <-cp[3]}
    
    if(cp[4]<=0){
      Winter_wheat_4th <- NULL
    }else{Winter_wheat_4th <-cp[4]}
    
    if(cp[5]<=0){
      Spring_wheat <- NULL
    }else{Spring_wheat <-cp[5]}
    
    if(cp[6]<=0){
      Winter_barley <- NULL
    }else{Winter_barley <-cp[6]}
    
    if(cp[7]<=0){
      Spring_barley <- NULL
    }else{Spring_barley <-cp[7]}
    
    if(cp[8]<=0){
      Winter_beans <- NULL
    }else{Winter_beans<-cp[8]}
    
    if(cp[9]<=0){
      Spring_beans <- NULL
    }else{Spring_beans <-cp[9]}
    
    if(cp[10]<=0){
      Ware_potatoes <- NULL
    }else{Ware_potatoes <-cp[10]}
    
    if(cp[11]<=0){
      Winter_OSR <- NULL
    }else{Winter_OSR <-cp[11]}
    
    if(cp[12]<=0){
      Sugar_beet <- NULL
    }else{Sugar_beet <-cp[12]}
    
    if(cp[13]<=0){
      Setaside <- NULL
    }else{Setaside <-cp[13]}
    
    kk <- rbind(Winter_wheat,Winter_wheat_2nd,Winter_wheat_3rd,Winter_wheat_4th,Spring_wheat,
                Winter_barley,Spring_barley,Winter_beans,Spring_beans, 
                Ware_potatoes,Winter_OSR,Sugar_beet,Setaside)
    
    colnames(kk)[1] <- "Total_Area_ha"
    
    kk
  }
  
  #=====================================================
  
  machineLabour <- function(machine){
    
    mach <- machine
    
    nmac <- c("Farmer","Tractor","P_harrow","Sprayer","Combine","Baler",
              "Pot_harvester","Sbeet_harvester","Labour")
    
    mc <- matrix(round(mach),ncol=1,dimnames = list(nmac,"Number"))
    
  }
  
  #========================= ACTIVITY MATRIX ==========================
  
  actMatrix <- function(soil,rotationBasis){ 
    
    # Number of activities
    
    if(missing(soil)){
      warning("soil argument missing")
    }else{
      nac <- 13
      cv <- -1
      cv0 <- 0
      cv1 <- 1
      cv2 <- 2
      cv3 <- 3
      cv4 <- 4
      cv5 <- -3
      cv6 <- -19
      cv7 <- -4
      cv8 <- -16
      cv11 <- 11
      
      ar <- areaFirstConsMatrix()
      sw <- seqWorkRateMatrix(soil)
      sl <- seqOpLabourMatrix(soil)
      se <- seqOpConstraint()
      no <- nonseqOpConstraint()
      la <- lastOpRotMatrix()
      
      vl <- length(sw[,1])+length(sl[,1])
      vl1 <- length(se[,1])+vl #+length(no[,1])+length(la[,1])
      vl2 <- length(no[,1])
      
      ac1 <- matrix(rep(cv0,nac*length(ar[,1])),ncol=nac)
      # Activity areas
      ac1[seq(1,13),] <- diag(cv,nac,nac)
      ac1[14,] <- cv1
      
      #Activity proportions as part of the rotational sequencing
      ac1[16,1] <- cv1 # Winter wheat
      ac1[17,c(1,2,3,4,5,seq(6,7))] <- cv1 # Barley and Cereals
      ac1[18,seq(8,9)] <- cv1# Beans Max
      ac1[19,seq(8,8)] <- cv1 # wbea Min
      #ac1[20,seq(6,7)] <- cv1 # Barleys
      #ac1[21,6] <- cv1 # wbar min
      #ac1[22,7] <- cv1 # sbar min
      #ac1[23,8] <- cv1 # wbea
      ac1[24,9] <- cv1 # sbea min
      ac1[25,10] <- cv1 # wpot
      ac1[26,11] <- cv1 # wosr
      ac1[27,12] <- cv1 # sbee
      ac1[28,13] <- cv1 # seta
      #ac1[29,2] <- cv1 # wwht2
      #ac1[30,3] <- cv1 # wwht3
      #ac1[31,4] <- cv1 # wwht4
      #ac1[32,5] <- cv1 # swht
      
      act <- ac1
      
      ma <- matrix(rep(cv0,nac*vl1),ncol=nac)
      
      nm <- matrix(rep(0,nac*vl2),ncol=nac)
      nm[seq(2,3),1] <- nm[seq(5,6),2] <- nm[seq(8,9),3] <- 
        nm[seq(11,12),4] <- cv
      nm[seq(14,15),5] <- nm[seq(17,18),6] <- nm[seq(20,21),7] <- cv
      nm[23,8] <- nm[25,9] <- cv
      nm[seq(27,29),10] <- nm[seq(31,32),11] <- nm[seq(34,36),12] <- cv
      nm[38,13] <- cv
      nma <- nm
      
      lm <- matrix(rep(cv0,nac*length(la[,1])),ncol=nac)
      
      ac <- rbind(act,ma,nma,lm)
      
      #=========
      
      rotation <- rotationBasis
      ro <- c("absolute", "proportional","none")
      
      eq <- "=="
      ls <- "<="
      gs <- ">="
      
      cv <- -1
      cv0 <- 0
      cv1 <- 1
      cv2 <- 2
      cv3 <- 3
      cv4 <- 4
      cv5 <- -3
      cv6 <- -19
      cv7 <- -4
      cv8 <- -15
      cv11 <- 11
      
      
      if(rotation%in%ro==TRUE){
        if(rotation=="absolute"){
          ac[seq(16,32),] <- 0
          
          #Activity proportions as part of the rotational sequencing
          ac[16,1] <- cv1 # Winter wheat
          ac[17,c(1,2,3,4,5,seq(6,7))] <- cv1 # Barley and Cereals
          ac[18,seq(8,9)] <- cv1# Beans Max
          ac[19,seq(8,8)] <- cv1 # wbea Min
          #ac[20,seq(6,7)] <- cv1 # Barleys
          #ac[21,6] <- cv1 # wbar min
          #ac[22,7] <- cv1 # sbar min
          #ac[23,8] <- cv1 # wbea
          ac[24,9] <- cv1 # sbea min
          ac[25,10] <- cv1 # wpot
          ac[26,11] <- cv1 # wosr
          ac[27,12] <- cv1 # sbee
          ac[28,13] <- cv1 # seta
          #ac[29,2] <- cv1 # wwht2
          #ac[30,3] <- cv1 # wwht3
          #ac[31,4] <- cv1 # wwht4
          #ac[32,5] <- cv1 # swht
          acs <- ac
          
        }else if(rotation=="proportional"){
          ac[seq(16,32),] <- 0
          
          #Activity proportions as part of the rotational sequencing
          ac[16,1] <- cv1; ac[16,seq(2,12)] <- cv
          ac[17,seq(1,7)] <- cv1; ac[17,seq(8,12)] <- cv5
          ac[18,c(seq(1,7),seq(10,12))] <- cv; ac[18,seq(8,9)] <- cv4
          ac[19,c(seq(1,7),seq(10,12))] <- cv1; ac[19,seq(8,9)] <- cv8
          ac[20,c(seq(1,9),seq(11,12))] <- cv; ac[20,10] <- cv3
          ac[21,c(seq(1,10),12)] <- cv; ac[21,11] <- cv2
          ac[22,seq(1,11)] <- cv; ac[22,12] <- cv3
          ac[23,seq(1,12)] <- cv; ac[23,13] <- cv3
          #ac[24,c(seq(1,7),seq(10,12))] <- cv; ac[24,seq(8,9)] <- cv4
          acs <- ac
          
        }else if(rotation=="none"){
          ac[seq(16,32),] <- 0
          acs <- ac
          
        }else{
          warning("Rotation basis not typed correctly (MUST be absolute, proportional OR none)")
        }
      }
    }
  } 
  
  #================================================================
  
  workersMatrix <- function(soil){ 
    
    if(missing(soil)){
      warning("soil argument missing")
    }else{
      
      nac <- 9
      cv <- -1
      cv1 <- 1
      cv2 <- 2
      cv3 <- 3
      cv4 <- 4
      cv5 <- 5
      per <- 1
      rotation <- rotationBasis
      ar <- areaFirstConsMatrix()
      sw <- seqWorkRateMatrix(soil)
      sl <- seqOpLabourMatrix(soil)
      se <- seqOpConstraint()
      no <- nonseqOpConstraint()
      la <- lastOpRotMatrix()
      op2 <- opsWorkableHours(soil,rain)
      op <- 1.32*op2[,seq(3,24)]
      mh2 <- labWorkableHours(soil,rain)
      mh <- 1.32*mh2[,seq(3,18)]
      
      #am <- actMatrix(soil,rotation)
      
      opw <- periodWorkableHours(soil,rain)
      
      #vl <- length(sw[,1])+length(sl[,1])
      vl1 <- length(se[,1])+length(no[,1])+length(la[,1])
      
      
      ma <- matrix(rep(0,nac*length(ar[,1])),ncol=nac)
      ma[15,1] <- cv1
      ma_ <- ma
      ma1 <- matrix(rep(0,nac*vl1),ncol=nac)
      
      #kk <- op[seq(1,18)+1,4] 
      #kk1 <- mh[seq(1,18),4]
      
      r100 <- 100/100 # For ploughing
      r80 <- 80/100 # For P/K fert spreading & Rolling, WPOT and SBEE harvesting
      # r80 is also for Hoeing and Harrowing
      r70 <- 70/100 # For Planting, Combine & Baling  
      r60 <- 60/100 # For Spraying
      
      
      
      macn <- 1 #c(rep(pl,cv5),rep(pltp,cv4),rep(pl,cv2),rep(ith,cv3),
      #rep(bab,cv5),rep(hpt,cv3),rep(hsb,cv3),pl)*r80
      
      wt <- matrix(rep(0,nac*(length(sw[,1])+26)),ncol=nac)
      wt[seq(1,26),2] <- macn*opw$Wkhrs[1:26]*cv*1.17#1.16#0.95#1.02
      #pp <- 1.28
      
      
      #Pharrow
      wt[seq(27,30),3] <- opw$Wkhrs[5:8]*cv#*r80
      
      #Sprayer
      wt[seq(31,53),4] <- opw$Wkhrs[c(seq(1,17),seq(21,26))]*cv#*r60#*0.9
      
      #Combine
      wt[seq(54,58),5] <- opw$Wkhrs[15:19]*cv#*r70
      
      #Baling
      wt[seq(59,63),6] <- opw$Wkhrs[15:19]*cv#*r70
      
      #Potato harvesting
      wt[seq(64,66),7] <- opw$Wkhrs[20:22]*cv#*r80; 
      
      #Sugarbeet harvesting
      wt[seq(67,72),8] <- opw$Wkhrs[20:25]*cv#*r80
      
      #===Labour
      wt[seq(73,98),9] <- macn*opw$Wkhrs[1:26]*cv*1.02
      
      wts <- wt
      
      wks <- rbind(ma_,wts,ma1)
      
      wkss <-wks
      
    }
  }
  
  #====================== CONSTRAINT MATRIX ========================
  
  consMatrix <- function(){  
    
    # This function puts together all the constraint matrices
    # Activity area and first operations
    
    #if(missing(soil)){
    #warning("soil argument missing")
    #}else{ 
    soil <- soil
    rotation <- rotationBasis
    ar <- areaFirstConsMatrix()
    sw <- seqWorkRateMatrix(soil)
    sl <- seqOpLabourMatrix(soil)
    se <- seqOpConstraint()
    no <- nonseqOpConstraint()
    la <- lastOpRotMatrix()
    #op <- read.csv("Ops_Workablehours.csv")#opsWorkableHours(fa$Value[[2]])
    #mh <- read.csv("Lab_Workablehours.csv")#labWorkableHours(fa$Value[[2]])
    am <- actMatrix(soil,rotation)
    wk <- workersMatrix(soil)
    rm <- rotSeqMatrix()
    
    vl <- length(ar[,1])+length(sw[,1])+length(sl[,1])
    nac <- length(rm[1,])
    
    ma1 <- matrix(rep(0,nac*vl),ncol=nac)
    
    rots <- rbind(ma1,rm)
    con <- rbind(ar,sw,sl,se,no,la)
    
    mats <- cbind(am,con,rots,wk)
    
    mats[c(seq(181,183),seq(234,236),seq(287,289),
           seq(340,342),seq(380,381)),] <- 0
    
    matss <- mats
    
    #} 
  } 
  #cons <- consMatrix()
  
  #=============================================================
  
  objFun1 <- function(){
    
    #if(missing(soil)){
    #warning("soil argument missing")
    #}else{
    
    soil <- soil
    nac <- 13
    cv <- -1
    cv1 <- 1
    ncr <- cropData(soil)[[1]] 
    opc <- objOpsCost(soil)
    fc <- fixedCost()
    rt <- modRotPen(soil)
    rt1 <- matrix(rt[1,],nrow=1)
    nsr <- selfRotPen(soil)
    
    
    fc1 <- matrix(rep(0,1*9),ncol=9)
    fc1[cv1,] <- c(fc$AnnualCost[8],fc$AnnualCost)*cv
    nfc <- fc1
    
    cr <- ncr[24,seq(2,11)]
    sr <- nsr[5,seq(14,16)]
    gm <- as.vector(cbind(cr[1],WW2=cr[1]-sr[1],WW3=cr[1]-sr[2],WW3=cr[1]-sr[3],cr[2:10]))
    
    obj <- cbind(gm,opc,rt1,nfc)
    
    objs <- obj
    
    #} 
  }
  
  #obf <- objFun1()
  
  #=================================================================
  objFun2 <- function(){
    
    #if(missing(soil)){
    #warning("soil argument missing")
    #}else{
    
    soil <- soil
    nac <- 13
    cv <- -1
    cv1 <- 1
    ncr <- cropData(soil)[[1]] 
    opc <- objOpsCost(soil)
    fc <- fixedCost()
    rt <- modRotPen(soil)
    nsr <- selfRotPen(soil)
    
    fc1 <- matrix(rep(0,1*9),ncol=9)
    #fc1[cv1,] <- c(fc$AnnualCost[8],fc$AnnualCost)
    #nfc <- fc1
    
    cr <- ncr[26,seq(2,11)]
    #sr <- nsr[5,seq(14,16)]
    nopc <- matrix(0,length(opc),nrow=cv1)
    nrt <- matrix(0,length(rt[1,]),nrow=cv1)
    nl <- as.vector(cbind(cr[1],WW2=cr[1],WW3=cr[1],WW3=cr[1],cr[2:10]))
    
    obj <- cbind(nl,nopc,nrt,fc1)
    
    #} 
  }
  #================================================================================
  
  objFun3 <- function(){
    
    #if(missing(soil)){
    #warning("soil argument missing")
    #}else{
    soil <- soil
    nac <- 13
    cv <- -1
    cv0 <- 0
    cv1 <- 1
    ncr <- cropData(soil)[[1]] 
    opc <- objOpsCost(soil)
    fc <- fixedCost()
    rt <- modRotPen(soil)
    nsr <- selfRotPen(soil)
    
    fc1 <- matrix(rep(0,1*9),ncol=9)
    #fc1[cv1,] <- c(fc$AnnualCost[8],fc$AnnualCost)
    #nfc <- fc1
    
    cr <- ncr[25,seq(2,11)]
    #sr <- nsr[4,seq(14,16)]
    nopc <- matrix(cv0,length(opc),nrow=cv1)
    nrt <- matrix(cv0,length(rt[1,]),nrow=cv1)
    rsk <- cbind(cr[1],cr[1]+nsr$GM2[4],cr[1]+nsr$GM3[4],cr[1]+nsr$GM4[4],cr[2:10])
    
    obj11 <- cbind(rsk,nopc,nrt,fc1)
  }
  
  #============================================================================
  
  modRHS <- function(){
    
    # This function creates the a vector for the RHS  
    # of the model 
    
    tarea <- farmArea # Total farm area
    cv1 <- 1
    crops <- 13
    ma <- consMatrix()
    
    rh <- matrix(rep(0,cv1*length(ma[,1])))
    rh[14,] <- tarea
    rh[15,] <- cv1
    rh[16,] <- tarea*0.5 # Winter wheat
    rh[17,] <- tarea*0.70#5 #Barle + Cereals
    rh[18,] <- tarea/3 # Beans Max
    rh[19,] <- tarea*0.06 # wbea Min
    #rh[20,] <- tarea*0.18 # Barleys
    #rh[21,] <- (tarea*0.18*0.5)*0.5 # Wbar Min
    #rh[22,] <- (tarea*0.18*0.5)*0.55 # Sbar Min
    #rh[23,] <- 0 # wbea
    rh[24,] <- tarea*0.06 # sbea Min
    rh[25,] <- tarea*0.25 # Potatoes
    rh[26,] <- tarea/3 # WOSR
    rh[27,] <- tarea/3*0.542 # Sugar beet
    rh[28,] <- tarea*0.70#5 # Set-aside
    #rh[29,] <- 0 # wwht2
    #rh[30,] <- 0 # wwht3
    #rh[31,] <- 0 # wwht4
    #rh[32,] <- 0 # swht
    
    rotation <- rotationBasis
    ro <- c("absolute", "proportional","none")
    
    if(rotation%in%ro==TRUE){
      if(rotation=="absolute"){
        
        rh[16:32] <- 0
        rh[16,] <- tarea*0.5 # Winter wheat
        rh[17,] <- tarea*0.70#5 #Barle + Cereals
        rh[18,] <- tarea/3 # Beans Max
        rh[19,] <- tarea*0.06 # wbea Min
        #rh[20,] <- tarea*0.18 # Barleys
        #rh[21,] <- (tarea*0.18*0.5)*0.5 # Wbar Min
        #rh[22,] <- (tarea*0.18*0.5)*0.55 # Sbar Min
        #rh[23,] <- 0 # wbea
        rh[24,] <- tarea*0.06 # sbea Min
        rh[25,] <- tarea*0.25 # Potatoes
        rh[26,] <- tarea/3 # WOSR
        rh[27,] <- tarea/3*0.542 # Sugar beet
        rh[28,] <- tarea*0.70#5 # Set-aside
        #rh[29,] <- 0 # wwht2
        #rh[30,] <- 0 # wwht3
        #rh[31,] <- 0 # wwht4
        #rh[32,] <- 0 # swht
        rhs <- rh
        
      }else if(rotation=="proportional"){
        rh[16:32] <- 0
        rhs <- matrix(rh)
        
      }else if(rotation=="none"){
        rh[16:32] <- 0
        rhs <- matrix(rh)
        
      }else{
        warning("Rotation basis not typed correctly (MUST be absolute, proportional OR none)")
      }
    }
    
    monoCrop <- monoCropping[1]
    crop <- monoCropping[2]
    
    if(monoCrop==TRUE){
      if(crop=="winterwheat"){
        rhs[16,] <-  tarea
      }else if(crop=="springwheat"){
        rhs[32,] <-  tarea
      }else if(crop=="winterbarley"){
        rhs[21,] <-  tarea
      }else if(crop=="springbarley"){
        rhs[22,] <-  tarea
      }else if(crop=="winterbeans"){
        rhs[19,] <-  tarea
      }else if(crop=="springbeans"){
        rhs[24,] <-  tarea
      }else if(crop=="warepotatoes"){
        rhs[25,] <-  tarea
      }else if(crop=="wosr"){
        rhs[26,] <-  tarea
      }else if(crop=="sugarbeet"){
        rhs[27,] <-  tarea
      }else if(crop=="setaside"){
        rhs[28,] <-  tarea
      }else if(crop=="allcrops"){
        rhs <- rhs
      }else{
        rhs <- rhs
      }
      
    }
    kk <- rhs
  }
  
  #============================================================================
  
  varDir <- function(){
    
    # This function creates the vector for the constraint
    # inequality direction (e.g. == or <=)
    
    eq <- "=="
    ls <- "<="
    gs <- ">="
    
    soil <- soil
    ar <- areaFirstConsMatrix()
    sw <- seqWorkRateMatrix(soil)
    sl <- seqOpLabourMatrix(soil)
    se <- seqOpConstraint()
    no <- nonseqOpConstraint()
    la <- lastOpRotMatrix()
    
    di1 <- rep(ls,length(ar[,1])) 
    di1[seq(1,15)] <- eq
    ndi <- di1
    
    lgt <- length(sw[,1])+length(sl[,1])+length(se[,1])
    di2 <- rep(ls,lgt)
    
    di3 <- rep(ls,length(no[,1]))
    di3[c(1,4,7,10,13,16,19,22,24,26,30,33,37)] <- eq
    ndi3 <- di3
    
    di4 <- rep(ls,length(la[,1]))
    
    dir <- c(ndi,di2,ndi3,di4)
    dir[c(14)] <- eq
    #[c(18)] <- eq
    dir[c(19)] <- eq #beans min
    #dir[c(20)] <- ls # barleys
    #dir[c(21)] <- gs # wbar min
    #dir[c(22)] <- gs # sbar min
    dir[c(24)] <- eq # sbea min
    #dir[c(32)] <- gs
    
    di <- dir
    
    rotation <- rotationBasis
    ro <- c("absolute", "proportional","none")
    
    if(rotation%in%ro==TRUE){
      if(rotation=="absolute"){
        
        di[18:32] <- ls
        #di[c(18)] <- eq
        di[c(19)] <- gs #beans min
        #di[c(20)] <- ls # barleys
        #di[c(21)] <- gs # wbar min
        #di[c(22)] <- gs # sbar min
        di[c(24)] <- gs # sbea min
        #di[c(32)] <- gs
        ndi <- di
        
      }else if(rotation=="proportional"){
        
        di[16:32] <- ls
        ndi <- di
      }else if(rotation=="none"){
        
        di[16:32] <- ls
        ndi <- di
      }else{
        warning("Rotation basis not typed correctly (MUST be absolute, proportional OR none)")
      }
    }
    
  }
  
  #============================================================================
  
  module1 <- function(){
    
    # This function puts together all the matrices and vectors
    # of the profit maximisation model to be solved
    
    tarea <- farmArea # Total farm area
    
    obj <- objFun1()
    con <- consMatrix()
    ob1 <- objFun2()
    ob2 <- objFun3()
    rh <- modRHS()
    di <- varDir()
    
    eq <- "=="
    ls <- "<="
    gs <- ">="
    
    
    if(rh[16,]==tarea){
      rh[16,] <- tarea
      di[16] <- eq #wwht
      di[17:32] <- eq#ls
      rh[seq(17,32),] <- 0
      con[seq(17,32),] <- 0
      con[16,1] <- 1
      con[c(seq(131,140),seq(185,193),seq(234,246),seq(287,299),
            seq(340,352),seq(380,391),seq(435,444),seq(475,482),seq(505,512),
            seq(532,544),seq(559,566),seq(591,603),seq(618,625)),] <- 0
      
      con[c(629,632,635,638,641,644,647,650,652,654,658,661,665),] <- 0
      con[,seq(73,886)] <- 0
      #con[14,] <- 0
      #rh[14,] <- 0
    } else if(rh[32,]==tarea){
      di[32] <- eq #swht
      di[16:31] <- eq#ls
      rh[seq(16,31),] <- 0
      con[seq(16,32),] <- 0
      con[32,5] <- 1
      con[c(seq(131,140),seq(185,193),seq(234,246),seq(287,299),
            seq(340,352),seq(380,391),seq(435,444),seq(475,482),seq(505,512),
            seq(532,544),seq(559,566),seq(591,603),seq(618,625)),] <- 0
      
      con[c(629,632,635,638,641,644,647,650,652,654,658,661,665),] <- 0
      con[,c(seq(14,249),seq(294,886))] <- 0
      #con[14,] <- 0
      #rh[14,] <- 0
    }else if(rh[21,]==tarea){
      di[21] <- eq #wbar
      di[c(seq(16,20),seq(22,32))] <- eq#ls
      rh[c(seq(16,20),seq(22,32)),] <- 0
      con[seq(16,32),] <- 0
      con[21,6] <- 1
      con[c(seq(131,140),seq(185,193),seq(234,246),seq(287,299),
            seq(340,352),seq(380,391),seq(435,444),seq(475,482),seq(505,512),
            seq(532,544),seq(559,566),seq(591,603),seq(618,625)),] <- 0
      
      con[c(629,632,635,638,641,644,647,650,652,654,658,661,665),] <- 0
      con[,c(seq(14,293),seq(357,886))] <- 0
      #con[14,] <- 0
      #rh[14,] <- 0
    }else if(rh[22,]==tarea){
      di[22] <- eq #sbar
      di[c(seq(16,21),seq(23,32))] <- eq#ls
      rh[c(seq(16,21),seq(23,32)),] <- 0
      con[seq(16,32),] <- 0
      con[22,7] <- 1
      con[c(seq(131,140),seq(185,193),seq(234,246),seq(287,299),
            seq(340,352),seq(380,391),seq(435,444),seq(475,482),seq(505,512),
            seq(532,544),seq(559,566),seq(591,603),seq(618,625)),] <- 0
      
      con[c(629,632,635,638,641,644,647,650,652,654,658,661,665),] <- 0
      con[,c(seq(14,356),seq(401,886))] <- 0
      #con[14,] <- 0
      #rh[14,] <- 0
    }else if(rh[19,]==tarea){
      di[19] <- eq #wbea
      di[c(seq(16,18),seq(20,32))] <- eq#ls
      rh[c(seq(16,18),seq(20,32)),] <- 0
      con[seq(16,32),] <- 0
      con[19,8] <- 1
      con[c(seq(131,140),seq(185,193),seq(234,246),seq(287,299),
            seq(340,352),seq(380,391),seq(435,444),seq(475,482),seq(505,512),
            seq(532,544),seq(559,566),seq(591,603),seq(618,625)),] <- 0
      
      con[c(629,632,635,638,641,644,647,650,652,654,658,661,665),] <- 0
      con[,c(seq(14,400),seq(437,886))] <- 0
      #con[14,] <- 0
      #rh[14,] <- 0
    }else if(rh[24,]==tarea){
      di[24] <- eq #sbea
      di[c(seq(16,23),seq(25,32))] <- eq#ls
      rh[c(seq(16,23),seq(25,32)),] <- 0
      con[seq(16,32),] <- 0
      con[24,9] <- 1
      con[c(seq(131,140),seq(185,193),seq(234,246),seq(287,299),
            seq(340,352),seq(380,391),seq(435,444),seq(475,482),seq(505,512),
            seq(532,544),seq(559,566),seq(591,603),seq(618,625)),] <- 0
      
      con[c(629,632,635,638,641,644,647,650,652,654,658,661,665),] <- 0
      con[,c(seq(14,436),seq(468,886))] <- 0
      #con[14,] <- 0
      #rh[14,] <- 0
    }else if(rh[25,]==tarea){
      di[25] <- eq #wpot
      di[c(seq(16,24),seq(26,32))] <- eq#ls 
      rh[c(seq(16,24),seq(26,32)),] <- 0
      con[seq(16,32),] <- 0
      con[25,10] <- 1
      con[c(seq(131,140),seq(185,193),seq(234,246),seq(287,299),
            seq(340,352),seq(380,391),seq(435,444),seq(475,482),seq(505,512),
            seq(532,544),seq(559,566),seq(591,603),seq(618,625)),] <- 0
      
      con[c(629,632,635,638,641,644,647,650,652,654,658,661,665),] <- 0
      con[,c(seq(14,467),seq(516,886))] <- 0
      #con[14,] <- 0
      #rh[14,] <- 0
    }else if(rh[26,]==tarea){
      di[26] <- eq #wosr
      di[c(seq(16,25),seq(27,32))] <- eq#ls
      rh[c(seq(16,25),seq(27,32)),] <- 0
      con[seq(16,32),] <- 0
      con[26,11] <- 1
      con[c(seq(131,140),seq(185,193),seq(234,246),seq(287,299),
            seq(340,352),seq(380,391),seq(435,444),seq(475,482),seq(505,512),
            seq(532,544),seq(559,566),seq(591,603),seq(618,625)),] <- 0
      
      con[c(629,632,635,638,641,644,647,650,652,654,658,661,665),] <- 0
      con[,c(seq(14,515),seq(554,886))] <- 0
      #con[14,] <- 0
      #rh[14,] <- 0
    }else if(rh[27,]==tarea){
      di[27] <- eq #sbee
      di[c(seq(16,26),seq(28,32))] <- eq#ls
      rh[c(seq(16,26),seq(28,32)),] <- 0
      con[seq(16,32),] <- 0
      con[27,12] <- 1
      con[c(seq(131,140),seq(185,193),seq(234,246),seq(287,299),
            seq(340,352),seq(380,391),seq(435,444),seq(475,482),seq(505,512),
            seq(532,544),seq(559,566),seq(591,603),seq(618,625)),] <- 0
      
      con[c(629,632,635,638,641,644,647,650,652,654,658,661,665),] <- 0
      con[,c(seq(14,553),seq(600,886))] <- 0
      #con[14,] <- 0
      #rh[14,] <- 0
    }else if(rh[28,]==tarea){
      di[28] <- eq #seta
      di[c(seq(16,27),seq(29,32))] <- eq#ls
      rh[c(seq(16,27),seq(29,32)),] <- 0
      con[seq(16,32),] <- 0
      con[28,13] <- 1
      con[c(seq(131,140),seq(185,193),seq(234,246),seq(287,299),
            seq(340,352),seq(380,391),seq(435,444),seq(475,482),seq(505,512),
            seq(532,544),seq(559,566),seq(591,603),seq(618,625)),] <- 0
      
      con[c(629,632,635,638,641,644,647,650,652,654,658,661,665),] <- 0
      con[,c(seq(14,599),seq(606,886))] <- 0
      #con[14,] <- 0
      #rh[14,] <- 0
    }
    
    # Variable types: whether continuos or
    # integer. 
    # Machine numbers to be selected by the model are
    # set to integer.
    cont <- "C"; int <- "I"
    vt <- rep(cont,length(obj[1,]))
    vt[887] <- int
    vt[888:895] <- int #cont
    vtype <- vt
    
    cons <- con
    rhs <- matrix(rh,ncol=1)
    dir <- di
    
    if(length(obj)!=length(cons[1,])){
      warning("Objective function and constraint matrix must have same length")
    }
    
    if(length(rhs[,1])!=length(dir)){
      warning("Right hand side and constraint direction must have same length")
    }
    
    md <- list(obj,cons,rhs,dir,vtype)
  }
  
  #mod1 <- module1()
  
  #============================================================================
  
  
  #============= PROFIT MAXIMISATION MODEL ===============
  
  solveProfitMod <- function(sensitivityAnalysis){ 
    # This function solves the profit maximisation
    # model
    
    #require("Rglpk") # Solver for solving the model
    
    mods <- module1()
    objs <- mods[[1]] # Objective function
    obj1 <- objs
    mat1 <- mods[[2]] # Constraint matrix 
    rhs <- mods[[3]] # RHS of model  
    dir <- mods[[4]] # Constraint direction 
    type1 <- mods[[5]] # Variable types 
    
    #mat1[seq(629,666),] <- 0
    mat <- mat1 #cbind(mat1,slack)
    
    types <- type1 #c(type1,rep("C",slklgt))
    
    #types[888:895] <- "C"
    obj <- obj1 #cbind(obj1,matrix(rep(0,slklgt),nrow=1))
    
    #bounds <- list(lower = list(ind = c(seq(1L,886L),887L,seq(888L,895L)),
    #val = c(rep(0,886),1,rep(0,8))),upper = list(ind =
    #c(seq(1L,886L),887L,seq(888L,895L)), val = c(rep(Inf,886),1,rep(4,8))))  
    
    if(intMachines=="yes"){
      types[888:895] <- "I"
    }else{
      types[888:895] <- "C"
    }
    
    rsy <- Rglpk_solve_LP(obj, mat, dir, rhs, bounds =NULL,
                          types =types, max =TRUE ,verbose=TRUE)  
    
    
    #list(round(lpa$solution[1:13],1),round(lpa$solution[887:895],1))
    
    #mat1 <- as.matrix(mat)
    #obj[14:886] <- obj[14:886]*1.1
    #obj[,c(887,888,895)] <- obj[,c(887,888,895)]*1.1
    
    #rsy <-  Rsymphony_solve_LP(obj, mat1, dir, rhs, bounds=NULL,
    #types = types, max =T,verbosity=-1)
    
    #rsy
    
    profit <- round(rsy$optimum) #rsy$objval
    
    ncrops <- round(rsy$solution[1:13],1)
    cropsa <- cropAreas(ncrops)
    
    #rtation <- round(rsy$solution[606:886])
    #rot1 <- cropRotation(rtation)
    
    if(sum(rsy$solution[606:886])<=0){
      rot1 <- "No Rotation"
    }else{
      rot1 <- cropRotation(rsy$solution[606:886])
    }
    
    wkrs <- rsy$solution[887:895]
    nwkrs <- machineLabour(wkrs)
    
    fcost <- sum(obj[887:895]*-1*rsy$solution[887:895])
    
    if(paraVarAnalysis=="yes"){
      wkrs1 <- wkrs[-1]
      c(profit,ncrops,wkrs1,fcost)
      
    }else{
      list(Profit=profit,Cropping=cropsa,Rotation_Matrix=rot1,Machines_Labour=nwkrs,
           Total_Fixed_Cost=fcost)
    }
    
  } 
  
  #pmf <- solveProfitMod()
  
  #============ NITRATE LEACHING MODEL ======================
  
  solveNleachMod <- function(profitTarget){
    # The profit target is set based on the the maximum profit generated
    # by the profit maximisation model which up to £238000
    # Increasing profit above this value will generate infeasible solution
    # This function solves the nitrate leaching 
    # minimisation model
    
    Rain <- rain
    ptarget <- profitTarget 
    ob1 <- objFun1()
    mods <- module1()
    obj <-  objFun2()*1 # Objective function
    obj[887:895] <- 0
    mat <- rbind(as.matrix(ob1),as.matrix(mods[[2]])) # Constraint matrix
    rhs <- rbind(matrix(ptarget),mods[[3]]) # RHS of model
    
    
    dir <- c(">=",mods[[4]]) # Constraint direction
    types <- mods[[5]] # Variable types
    
    
    bounds <- list(lower = list(ind = c(seq(1L,886L),887L,seq(888L,895L)),
                                val = c(rep(0,886),1,rep(0,8))),upper = list(ind =
                                 c(seq(1L,886L),887L,seq(888L,895L)), val = c(rep(Inf,886),1,rep(5,8)))) 
    
    
    if(profitTarget==0){
      mat[1,] <- 0
      rhs[1,] <- 0
    }
    
    if(intMachines=="yes"){
      types[888:895] <- "I"
    }else{
      types[888:895] <- "C"
    }
    
    rsy <- Rglpk_solve_LP(obj, mat, dir, rhs, bounds = NULL,
                          types=types, max =FALSE ,verbose=TRUE) 
    
    mat2 <- as.matrix(mat) 
    
    #rsy <-  Rsymphony_solve_LP(obj, mat2, dir, rhs, types = types, max =F,verbosity=-1)
    
    nleach <- round(rsy$optimum) #round(rsy$objval)
    NO3_gl <- round((62/14)*(nleach/(10*Rain)),1)
    
    profit <- round(sum(mat[1,]*rsy$solution)) # Profit
    
    ncrops <- round(rsy$solution[1:13],1)
    cropsa <- cropAreas(ncrops)
    
    #rtation <- round(rsy$solution[606:886])
    #rot1 <- cropRotation(rtation)
    
    if(sum(rsy$solution[606:886])<=0){
      rot1 <- "No Rotation"
    }else{
      rot1 <- cropRotation(rsy$solution[606:886])
    }
    
    wkrs <- rsy$solution[887:895]
    nwkrs <- machineLabour(wkrs)
    
    
    
    fcost <- sum(mat2[1,c(887,888,889,890,891,892,893,
                          894,895)]*-1*rsy$solution[887:895])
    
    
    if(profitTarget==0){
      fcost <- sum(ob1[887:895]*-1*rsy$solution[887:895])
    }
    
    if(paraVarAnalysis=="yes"){
      wkrs1 <- wkrs[-1]
      c(profit,nleach,NO3_gl,ncrops,wkrs1,fcost)
  
    }else{
      list(N_Leaching=nleach,NO3_Leaching_gl=NO3_gl,Profit=profit,Cropping=cropsa,
           Rotation_Matrix=rot1,Machines_Labour=nwkrs,Total_Fixed_Cost=fcost)
    }
    
  } 
  
  #nl <- solveNleachMod(profitTarget=80000)
  
  #============================================================================
  
  solveRiskMod <- function(riskMAD=riskTarget, nleach=nleachTarget){
    
    # The risk value can be varied between 20000 and a very large number
    # When the risk is set to its minimum, the minimum nitrate leaching (nleach) 
    # target must be set minimum 4590 but can be set to as low as 2500 when the risk
    # target is set at 165000 and above.
    
    soil <- soil
    Rain <- rain
    
    mods <- module1()
    mat1 <- as.matrix(mods[[2]])
    ob2 <- as.matrix(objFun2())
    
    dev <- cropData(soil)[[2]]
    
    cv <- -1
    cv1 <- 1
    cv2 <- 2
    cv11 <- 11
    nd <- 10
    nac <- 13
    kstate <- nd/cv2
    
    mt <- matrix(rep(0,kstate*nd),ncol=nd)
    mt[1,1] <- mt[2,3] <- mt[3,5] <- mt[4,7] <- mt[5,9] <- cv1 #********
    mt[1,2] <- mt[2,4] <- mt[3,6] <- mt[4,8] <- mt[5,10] <- cv1
    mt1 <- mt
    
    ma1 <- mods[[2]]
    hlgt <- length(ma1[1,])
    dmat <- matrix(rep(0,kstate*hlgt),nrow=kstate)
    dmat[,seq(cv1,nac)] <- dev
    ndmat <- dmat
    
    vle <- (length(mat1[,1])+length(mt1[,1])+cv2)-cv1
    
    mo <- matrix(rep(0,nd*vle),nrow=vle)
    mo[seq(2,6),] <- mt1
    mo[,c(2,4,6,8,10)] <- 0 #mo[,c(1,3,5,7,9)] <- 0 # Remove the positive deviations
    
    nmo <- mo
    
    mat2 <- cbind(rbind(ob2,ndmat,mat1),nmo)
    
    # Probability 
    prob <- 0.2
    pb <- matrix(rep(0,cv1*(hlgt+nd)),nrow=cv1)
    pb[,seq(896,905)] <- prob
    pb[,c(897,899,901,903,905)] <- 0
    prb <- pb
    
    ndv <- matrix(rep(prob,cv1*nd),ncol=nd)
    #ndv[,seq(1,9,2)] <- ndv[,seq(1,9,2)]*cv
    ndv[,seq(2,10,2)] <- 0
    nndv <- ndv
    
    obj <- cbind(as.matrix(mods[[1]]),nndv)
    mat1 <- rbind(mat2,prb)
    
    
    dir1 <- c("<=",rep(">=",5),mods[[4]],"<=")
    
    risklevel <- riskMAD # Total Negative Deviation
    nl <- nleach
    rhs1 <- rbind(matrix(nl),matrix(rep(0,kstate*cv1),ncol=cv1),mods[[3]],matrix(risklevel))
    types <- c(mods[[5]],rep("C",nd))
    
    bounds <- list(lower = list(ind = c(seq(1L,886L),887L,seq(888L,895L)),
                                val = c(rep(0,886),1,rep(0,8))),upper = list(ind =
                               c(seq(1L,886L),887L,seq(888L,895L)), val = c(rep(Inf,886),1,rep(4,8))))  
    
    if(nleach==0){
      mat1[1,] <- 0
      rhs1[1,] <- 0
    }
    
    if(intMachines=="yes"){
      types[888:895] <- "I"
    }else{
      types[888:895] <- "C"
    }
    
    rsy <- Rglpk_solve_LP(obj, mat1, dir1, rhs1, bounds =NULL,
                          types=types, max =T ,verbose=TRUE) 
    
    
    mat3 <- as.matrix(mat1) 
    #rsy <-  Rsymphony_solve_LP(obj, mat3, dir1, 
    #rhs1, types = types, max =T,verbosity=-1)
    
    risk <- round(sum(obj[896:905]*rsy$solution[896:905]))
    
    profit <- round(rsy$optimum-risk)
    
    NO3_gl <- round((62/14)*(nleach/(10*Rain)),1)
    
    ncrops <- round(rsy$solution[1:13],1)
    cropsa <- cropAreas(ncrops)
    
    #rtation <- round(rsy$solution[606:886])
    #rot1 <- cropRotation(rtation)
    
    if(sum(rsy$solution[606:886])<=0){
      rot1 <- noquote("No Rotation")
    }else{
      rot1 <- cropRotation(rsy$solution[606:886])
    }
    
    wkrs <- rsy$solution[887:895]
    nwkrs <- machineLabour(wkrs)
    
    # Standard deviation
    N <- length(mt1[,1])
    NPI <- 2*pi #pi*N 
    dnm <- N*(N-cv1) #2*(N-cv1)
    st <- NPI/dnm
    stdv <- round(risk*(st^0.5))
    
    cv <- round(stdv/profit,1) 
    
    fcost <- sum(obj[887:895]*-1*rsy$solution[887:895])
    
    if(paraVarAnalysis=="yes"){
      wkrs1 <- wkrs[-1]
      c(profit,nleach,NO3_gl,risk,stdv,cv,ncrops,wkrs1,fcost)
      
    }else{
      list(Profit=profit, N_Leaching=nleach,NO3_Leaching_gl=NO3_gl,MOTAD_Risk=risk,Standard_Deviation=stdv,
           Coefficient_of_Variation=cv,Cropping=cropsa,Rotation_Matrix=rot1,Machines_Labour=nwkrs,
           Total_Fixed_Cost=fcost)
     
    }
  } 
  
  #rm <- solveRiskMod(riskMAD=20000, nleach=0)
  
  #========= GOAL PROGRAMMING MODEL ======================================
  
  goalWeights <- function(G1,G2,G3){
    
    # G1 = Goal 1: Profit Maximisation
    # G2 = Goal 2: Nitrate Leaching Minimisation
    # G3 = Goal 3: Risk Minimisation (MOTAD)
    
    # If a goal is considered more important than the others and is 
    # assigned a weight of 1, then sum of the weights for the 
    # remaining goal must be less than 1. 
    # The weight of the second important goal must be 
    # greater than the least important goal.
    # if all goals are important then a weight of 1 
    # should be assigned to each.
    
    if((G1+G2+G3)==1||(G1+G2+G3)<1){
      warning("Sum of weights must be greater than 1")
    }
    
    if(G1>1||G2>1||G3>1){
      warning("The highest weight assigned must be equal to 1")
    }
    
    # Weight Scenarios
    
    if(G1==1&&G2==1&&G3==1){
      wgt <- c(G1,G2,G3)
      #print("All three goals are equally important",quote=F)
    }else if(G1==1&&G1>G2+G3&&G2>G3){
      wgt <- c(G1,G2,G3)
      #print("Goal Preference: 1st=Profit; 2nd=Nitrate Leaching; 3rd=Risk",quote=F)
    }else if(G1==1&&G1>G2+G3&&G3>G2){
      wgt <- c(G1,G2,G3)
      #print("Goal Preference: 1st=Profit; 2nd=Risk; 3rd=Nitrate Leaching",quote=F)
    }else if(G1==1&&G2==G3&&G2+G3<1){
      wgt <- c(G1,G2,G3)
      #print("Goal Preference: 1st=Profit; 2nd=Nitrate Leaching & Risk",quote=F)
    }else if(G2==1&&G2>G1+G3&&G1>G3){
      wgt <- c(G1,G2,G3)
      #print("Goal Preference: 1st=Nitrate Leaching; 2nd=Profit; 3rd=Risk",quote=F)
    }else if(G2==1&&G2>G1+G3&&G3>G1){
      wgt <- c(G1,G2,G3)
      #print("Goal Preference: 1st=Nitrate Leaching; 2nd=Risk; 3rd=Profit",quote=F)
    }else if(G2==1&&G1==G3&&G1+G3<1){
      wgt <- c(G1,G2,G3)
      #print("Goal Preference: 1st=Nitrate Leaching; 2nd=Risk & Profit",quote=F)
    }else if(G3==1&&G3>G1+G2&&G1>G2){
      wgt <- c(G1,G2,G3)
      #print("Goal Preference: 1st=Risk; 2nd=Profit; 3rd=Nitrate Leaching",quote=F)
    }else if(G3==1&&G3>G1+G2&&G2>G1){
      wgt <- c(G1,G2,G3)
      #print("Goal Preference: 1st=Risk; 2nd=Nitrate Leaching; 3rd=Profit",quote=F)
    }else if(G3==1&&G1==G2&&G1+G2<1){
      wgt <- c(G1,G2,G3)
      #print("Goal Preference: 1st=Risk; 2nd=Nitrate Leaching & Profit",quote=F)
    }else {
      wgt <- NULL
      warning("No weights attached because weighting rule has been violated:
            If weight of most important goal is 1, sum of weights of other goals must be less than 1")
    }
    kk <- wgt
  }
  
  solveGoalProgMod <- function(wP, wN, wR, profitTarget, nleachTarget, riskTarget){ 
    
    # Due to the difficulty of incorporating the traditional MOTAD formulation 
    # into the Weighted Goal Programming formulation, the risk values used 
    # are standard deviation of gross margins of crops.
    
    Rain <- rain
    
    mods <- module1()
    wgt <- goalWeights(G1=wP,G2=wN,G3=wR)
    G1 <- wP
    G2 <- wN
    G3 <- wR
    
    ob1 <- as.matrix(objFun1())
    ob2 <- as.matrix(objFun2())
    ob3 <- as.matrix(objFun3())
    
    cv <- -1
    cv0 <- 0
    cv1 <- 1
    cv3 <- 3
    cv6 <- 6
    per <- 100
    
    dv <- matrix(cv0,nrow=cv3,ncol=cv6)
    dv[1,1] <- dv[2,3] <- dv[3,5] <- cv1
    dv[1,2] <- dv[2,4] <- dv[3,6] <- cv
    ndv <- dv
    
    #ndv[,c(2,3,5)] <- ndv[,c(2,3,5)]*0
    
    
    ob4 <- rbind(ob1,ob2,ob3); ob5 <- cbind(ob4,ndv)
    mt1 <- cbind(as.matrix(mods[[2]]),matrix(cv0,ncol=cv6,nrow=length(mods[[2]][,1])))
    mat1 <- rbind(ob5,mt1)
    
    dgoal1 <- wgt[1]*(per/profitTarget)
    dgoal2 <- wgt[2]*(per/nleachTarget)
    dgoal3 <- wgt[3]*(per/riskTarget)
    
    ob6 <- matrix(cv0,ncol=length(ob5[1,]))
    ob6[896] <- dgoal1; ob6[899] <- dgoal2; ob6[901] <- dgoal3
    
    obj <- ob6
    mat <- mat1
    rhs <- rbind(matrix(profitTarget),matrix(nleachTarget),
                 matrix(riskTarget),as.matrix(mods[[3]]))
    dir <- c(rep("==",cv3),mods[[4]])
    types <- c(mods[[5]],rep("C",cv6))
    
    
    
    if(profitTarget==0){
      mat[1,] <- 0
      rhs[1] <- 0 
      obj[,c(896,897)] <- 0
    }
    
    if(nleachTarget==0){
      mat[2,] <- 0
      rhs[2] <- 0 
      obj[,c(898,899)] <- 0
    }
    
    if(riskTarget==0){
      mat[3,] <- 0
      rhs[3,] <- 0 
      obj[,c(900,901)] <- 0
    }
    
    if(intMachines=="yes"){
      types[888:895] <- "I"
    }else{
      types[888:895] <- "C"
    }
    
    rsg <- Rglpk_solve_LP(obj, mat, dir, rhs, bounds = NULL,
                          types=types, max =F ,verbose=TRUE) 
    mat4 <- as.matrix(mat) 
    #rsg <-  Rsymphony_solve_LP(obj, mat4, dir, rhs, types = types, max =F,verbosity=-1)
    
    prft <- profitTarget
    prfa <- sum(rsg$solution[1:895]*mat4[1,seq(1,895)])
    dvpf <- abs(sum(rsg$solution[896:901]*mat4[1,seq(896,901)]))
    
    nlta <- sum(rsg$solution[1:895]*mat4[2,seq(1,895)])
    nla <- abs(round(sum(rsg$solution[896:901]*mat4[2,seq(896,901)]),1))
    
    NO3_gl1 <- round((62/14)*(nleachTarget/(10*Rain)),1)
    NO3_gl2 <- round((62/14)*(nlta/(10*Rain)),1)
    NO3_gl3 <- abs(NO3_gl1 - NO3_gl2)
    
    
    rska <- sum(rsg$solution[1:895]*mat4[3,seq(1,895)])
    rskd <- abs(round(sum(rsg$solution[896:901]*mat4[3,seq(896,901)]),1))
    
    tdev <- round(rsg$optimum,1) #round(rsg$objval,1) 
    
    crops <- round(rsg$solution[1:13],1)
    
    ncrops <- round(rsg$solution[1:13],1)
    cropsa <- cropAreas(ncrops)
    
    #rtation <- round(rsg$solution[606:886])
    #rot1 <- cropRotation(rtation)
    
    if(sum(rsg$solution[606:886])<=0){
      rot1 <- "No Rotation"
    }else{
      rot1 <- cropRotation(rsg$solution[606:886])
    }
    
    wkrs <- rsg$solution[887:895]
    nwkrs <- machineLabour(wkrs)
    
    fcost <- sum(mat4[1,c(887,888,889,890,891,892,
                          893,894,895)]*-1*rsg$solution[887:895])
    
    if(paraVarAnalysis=="yes"){ 
      nwkrs <- wkrs[-1]
      kk <- c(tdev,prft,prfa,dvpf,nleachTarget,nlta,nla,NO3_gl1,NO3_gl2,NO3_gl3,
              riskTarget,rska,rskd,ncrops,nwkrs,fcost)
      
    }else{
      kk <- list(Total_Percentage_Deviation=tdev,ProfitTarget=prft,Profit_Achieved=prfa,
                 Deviation_from_Profit_Target=dvpf,N_Leaching_Target=nleachTarget,N_Leaching_Acvhieved=nlta,
                 Deviation_from_N_Leaching_Target=nla,NO3_Leaching_gl_Target=NO3_gl1,NO3_Leaching_gl_Acvhieved=NO3_gl2,
                 Deviation_from_NO3_Leaching_gl_Target=NO3_gl3,Risk_Target=riskTarget,Risk_Level_Achieved=rska,
                 Deviation_from_Risk_Target=rskd,Cropping=cropsa,Rotation_Matrix=rot1,Machines_Labour=nwkrs,
                 Total_Fixed_Cost=fcost,
                 
                 
                 Goal_Preference = if(G1==1&&G2==1&&G3==1){
                   noquote("All three goals are equally important")
                 }else if(G1==1&&G1>G2+G3&&G2>G3){
                   noquote("1st=Profit; 2nd=Nitrate Leaching; 3rd=Risk")
                 }else if(G1==1&&G1>G2+G3&&G3>G2){
                   noquote("1st=Profit; 2nd=Risk; 3rd=Nitrate Leaching")
                 }else if(G1==1&&G2==G3&&G2+G3<1){
                   noquote("1st=Profit; 2nd=Nitrate Leaching & Risk")
                 }else if(G2==1&&G2>G1+G3&&G1>G3){
                   noquote("1st=Nitrate Leaching; 2nd=Profit; 3rd=Risk")
                 }else if(G2==1&&G2>G1+G3&&G3>G1){
                   noquote("1st=Nitrate Leaching; 2nd=Risk; 3rd=Profit")
                 }else if(G2==1&&G1==G3&&G1+G3<1){
                   noquote("1st=Nitrate Leaching; 2nd=Risk & Profit")
                 }else if(G3==1&&G3>G1+G2&&G1>G2){
                   noquote("1st=Risk; 2nd=Profit; 3rd=Nitrate Leaching")
                 }else if(G3==1&&G3>G1+G2&&G2>G1){
                   noquote("1st=Risk; 2nd=Nitrate Leaching; 3rd=Profit")
                 }else if(G3==1&&G1==G2&&G1+G2<1){
                   noquote("1st=Risk; 2nd=Nitrate Leaching & Profit")
                 }else {
                   warning("No weights attached because weighting rule has been violated:
                 If weight of most important goal is 1, sum of weights of other goals must be less than 1")
                 }, 
                 
                 
                 Profit_Achievement_Status =   if(round(prfa-profitTarget)==0){
                   noquote("Profit goal was achieved")
                 }else if(prfa>profitTarget){
                   noquote("Profit goal was overachieved")
                 }else if( prfa<profitTarget){
                   noquote("Profit goal was underachieved")
                 },
                 
                 Nitrate_Leaching_Achievement_Status =   if(nlta<nleachTarget){
                   noquote("Nitrate leaching goal was underachieved")
                 }else if(nlta>nleachTarget){
                   noquote("Nitrate leaching goal was overachieved")
                 }else{
                   noquote("Nitrate leaching goal was achieved")
                 },
                 
                 Risk_Achievement_Status =   if(rska<riskTarget){
                   noquote("Risk goal was underachieved")
                 }else if(rska>riskTarget){
                   noquote("Risk goal was overachieved")
                 }else{
                   noquote("Risk goal was achieved")
                 }
      )
      
    }
    
  } 
  
  #gp <- solveGoalProgMod(wP, wN, wR, profitTarget, nleachTarget, riskTarget)
  
  #====================== THE COMBINED MODEL ==================================
  
  solveSAFMOD <- function(modelChoice,wP,wN,wR,profitTarget,nleachTarget,riskTarget,steps){
    # The SAFMOD function combines all the three models
    #*** MODEL CHOICEs ARE: "profit", "nleaching", "motadrisk" and "goalprog"
    
    # Setting steps to TRUE means solving the models using one model's
    # optimum solution as a target for the other model.
    
    
    if(steps==TRUE){
      
      if(modelChoice=="profit"){
        modsol <- solveProfitMod()
      }else if(modelChoice=="nleaching"){
        pp <- solveProfitMod()
        modsol <- solveNleachMod(pp$Profit)
      }else if(modelChoice=="motadrisk"){
        pp <- solveProfitMod()
        kk <- solveNleachMod(pp$Profit)
        modsol <- solveRiskMod(riskTarget,kk$N_Leaching)
      }else if(modelChoice=="goalprog"){
        pp <- solveProfitMod()
        kk <- solveNleachMod(pp$Profit)
        kr <- solveRiskMod(riskTarget,kk$N_Leaching)
        modsol <- solveGoalProgMod(wP,wN,wR,kr$Profit,kr$N_Leaching,kr$MOTAD_Risk) 
      }
    }else if(steps==FALSE){
      modsol <- switch(modelChoice,
                       profit = solveProfitMod(),
                       nleaching = solveNleachMod(profitTarget),
                       motadrisk = solveRiskMod(riskTarget,nleachTarget),
                       goalprog = solveGoalProgMod(wP,wN,wR,profitTarget,nleachTarget,riskTarget))
    }
    
    modsol
  }
  
  sf <- solveSAFMOD(modelChoice,wP,wN,wR,profitTarget,nleachTarget,riskTarget,steps)
  sf
  
} # End Bracket


############################################################################################################################################################################
############################################################################################################################################################################


fertPrices=NULL
seedRates=NULL
seedPrices=NULL
BGHerb=NULL
BGHerbPrice=NULL
WOHerb=NULL
WOHerbPrice=NULL
primaryYieldPrices=NULL
secondaryYieldPrice=NULL
deflators <- c(1.158,1.120,1.094,1.076,1.054)
subsidy=NULL
sbeetTransCost=NULL
otherCosts=NULL
machinePrices=NULL
annualLabCost=NULL
depRates=NULL
repCostRates=NULL
replaceYears=NULL
historicalYields=NULL
steps=TRUE
modInputChange <- c("none","allcrops",1)


#oilType <- 2.5
rain <- 600
fuelPrice <- 0.62
interestRate <- 0.5
inflation <- 0.25
farmArea=250
wP<-1
wN<-0.1
wR<-0.1
profitTarget<-80000
nleachTarget<-8000
riskTarget<-90933
intMachines<-"yes"
rotationBasis <- "proportional"
monoCropping <- c(TRUE,"winterwheat")

RunSAFMOD(modelChoice="motadrisk",wP=1,wN=0.5,wR=0.1,profitTarget=120000,nleachTarget=0,riskTarget=60000,steps=FALSE,farmArea=250,soilType=2.5,
          rain=600,intMachines="no",rotationBasis="proportional",monoCropping=c(TRUE,"allcrops"),fuelPrice=1,interestRate=0.5,inflation=0.25,
          fertPrices=NULL,seedRates=NULL,seedPrices=NULL,BGHerb=NULL,BGHerbPrice=NULL,WOHerb=NULL,WOHerbPrice=NULL,primaryYieldPrices=NULL,deflators=NULL,
          secondaryYieldPrice=NULL,subsidy=NULL,sbeetTransCost=NULL,otherCosts=NULL,machinePrices=NULL,annualLabCost=NULL,depRates=NULL,
          repCostRates=NULL,replaceYears=NULL,historicalYields=NULL,modInputChange=c("none","allcrops",1),paraVarAnalysis="no")

