#Loading DATA



#Cleaning DATA: one value was "kayık" fış fış kürek
algerian_forest$Classes[166] <- "fire" 
algerian_forest$FWI[166] <- NA
algerian_forest$Classes <- trimws(algerian_forest[,"Classes"])

#New Column fire_status
algerian_forest$fire_status <- ifelse(algerian_forest[,"Classes"]=="not fire",0,1)

#Factorizing
algerian_forest$Classes <- as.factor(algerian_forest$Classes)

algerian_forest$DC[166] <- 14.69

algerian_forest$DC1 <- as.numeric(algerian_forest$DC)

algerian_forest$FWI1 <- as.numeric(algerian_forest$FWI)

algerian_forest <- algerian_forest[,-c(1,2,3,7)]



#Rain data is too small to make any comment on it
subset(portugal_forest_fire, select = -rain)

#Determine whether there was a fire or not
portugal_forest_fire$fire_status <- ifelse(portugal_forest_fire[,"area"]>0,1,0)

portugal_forest_fire <- portugal_forest_fire[,-c(1,2,3,4,12)]
