
farmSoil <- function(soil){
  # This function sets the soil type for the model. The soil type influences
  # the fertiliser amounts, which in turn influence crop yields, gross margin,
  # variable costs, profits. 
  # The soil type also influences the workable hours which 
  # in turn affect machine/labour selection.
  # Takes soil type and returns new farm inputs
  
  soi <- c(0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5)
  so <- Files()[[3]]
  
  if(soil%in%soi==TRUE){
    so$Value[[2]] <- soil 
    
  }else{
    so$Value[[2]] <- 2.5 # This sets the soil type to heavy soil (Clay)
    #newso <- so
    warning("Enter correct soil type: must be between 0.5 and 2.5 at an interval of 0.25")
  }
  
  
  newso <- so
  
  #setwd("~/Google Drive/SAFMOD/Mod_Data")
  #write.table(newso,file="Farm_location.csv",row.names=FALSE,sep=",")
  
  kk <- newso
  
  kk
}


#=========================================================================================================
##
