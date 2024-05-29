



rm()
gc()
library(mgcv)

# plaice
# megrim
# haddock
# Lophius budegassa

path <- "output/gam_models_env"
load("input/data.list_new_spp.RData", verbose = T)
load("input/data.hh_MPI_ESM_2_HR_and_NEMO_MEDUSA.RData", verbose = T)

data.hh$haul.id <- paste0(data.hh$Survey,":",data.hh$Year,":",data.hh$Quarter,":",data.hh$Country,":",data.hh$Ship,":",data.hh$Gear,":",data.hh$StNo,":",data.hh$HaulNo)

print(species <- (names(data.list)))
stage   <- c("juv","adu","all") 
cases   <- rev(c("NS","WW","BS"))
# c <- "NS"
# j<-"adu"
# i<-species[3]
# 1 2 3 5
# Fri Mar 10 20:31:50 2023 ------------------------------


for(c in cases){
  print(c)
  for(i in species[1:6]) {
    print(i)
    
    spp <- data.list[[i]]
    # parse data types
    spp$Year <- as.numeric(as.character(spp$Year))
    spp <- spp[spp$Year >= 1980,]
    
    if(c == "NS") spp <- spp[spp$Ecoregion %in% "Greater North Sea",] 
    if(c == "WW") spp <- spp[spp$Ecoregion %in% c("Celtic Seas", "Bay of Biscay and the Iberian Coast") | spp$Area_27 == "7.e",]  # 4a 6a 6b1 6b2
    if(c == "BS") spp <- spp[spp$Ecoregion %in% "Baltic Sea",]
    
    if(c == "NS") spp <- spp[spp$Area_27 != "7.e",]
    if(c == "WW") spp <- spp[!(spp$Area_27 %in% c("4.a","6.a","6.b.1","6.b.2","7.d")),]
    # if(c == "BS") spp <- spp[spp$Ecoregion == "Baltic Sea",]
    
    # if(c == "NS") spp <- spp[spp$Area_27 %in% c("7.d","4.a","4.b","4.c"),]
    # if(c == "WW") spp <- spp[spp$Area_27 %in% c("7.e","7.f","7.g","7.j.2"),]
    # if(c == "BS") spp <- spp[spp$Ecoregion == "Baltic Sea",]
    
    if(nrow(spp)>0){
      # add environmental correlates
      spp$Salinity <- data.hh$NEMO_MEDUSA_Salinity[match(spp$haul.id,data.hh$haul.id)]
      spp$Temperature <- data.hh$NEMO_MEDUSA_Temperature[match(spp$haul.id,data.hh$haul.id)]
      spp$DIN <- data.hh$NEMO_MEDUSA_DIN[match(spp$haul.id,data.hh$haul.id)]
      spp$Detritus <- data.hh$NEMO_MEDUSA_Detritus[match(spp$haul.id,data.hh$haul.id)]
      spp$Phytoplankton <- data.hh$NEMO_MEDUSA_Phytoplankton[match(spp$haul.id,data.hh$haul.id)]
      
      
      spp$Gear <- as.character(spp$Gear)
      spp$ShipG <- as.factor(as.character(spp$ShipG))
     
      for(j in stage){
        print(j)
        if(!("bio" %in% colnames(spp))){
          sum.list <- list(
            "adu" = sum(spp$bio.adult, na.rm = T),
            "juv" = sum(spp$bio.juv, na.rm = T),
            "all" = sum(c(sum(spp$bio.adult, na.rm = T),sum(spp$bio.juv, na.rm = T)))
          )
        } else {
          sum.list <- list(
            "adu" = sum(spp$bio.adult, na.rm = T),
            "juv" = sum(spp$bio.juv, na.rm = T),
            "all" = sum(spp$bio, na.rm = T)
          )
        }
        
        
        
        # mod.name <- paste0("environment", i,"_",j,"_",c,"_biomass_gam.RData")  
        # if(!(mod.name) %in% list.files(path))  {
        if(sum.list[[j]] > 0){ #& (!(mod.name %in% list.files(path)))
          dat <- spp
          if(j == "all"){
            if(!("bio" %in% colnames(spp))){
              spp$bio <- spp$bio.juv + spp$bio.adult
            }
            formula <- bio ~	s(lon,lat,bs="ds",k=256,m=c(1,0.5)) + 
              s(ctime,bs="ds",k=ny,m=c(1,0)) +
              ti(ctime,lon,lat,d=c(1,2),bs=c("ds","ds"),k=c(ny,10),m=list(c(1,0),c(1,0.5))) +
              te(timeOfYear,lon,lat,d=c(1,2),bs=c("cc","ds"),k=c(6,30),m=list(c(1,0),c(1,0.5))) +
              Gear + 
              s(ShipG,bs="re") + 
              s(Depth,bs="ds",k=5,m=c(1,0)) +
              s(Salinity,bs="ds",k=5,m=c(1,0)) +
              s(Temperature,bs="ds",k=5,m=c(1,0)) +
              s(Phytoplankton,bs="ds",k=5,m=c(1,0)) + 
              offset(log(SweptArea))
            
            gear0 <- table(spp$Gear, spp$bio==0)[,"FALSE"]
            gear0 <- names(gear0[gear0==0])
            if(length(gear0)>0) dat <- dat[!(dat$Gear %in%  gear0),]
            ships0 <- table(spp$ShipG, spp$bio==0)[,"FALSE"]
            ships0 <- names(ships0[ships0==0])
            if(length(ships0)>0) dat <- dat[!(dat$ShipG %in%  ships0),]
          }
          
          
          if(j == "juv"){
            formula <- bio.juv ~	s(lon,lat,bs="ds",k=256,m=c(1,0.5)) + 
              s(ctime,bs="ds",k=ny,m=c(1,0)) +
              ti(ctime,lon,lat,d=c(1,2),bs=c("ds","ds"),k=c(ny,10),m=list(c(1,0),c(1,0.5))) +
              te(timeOfYear,lon,lat,d=c(1,2),bs=c("cc","ds"),k=c(6,30),m=list(c(1,0),c(1,0.5))) +
              Gear + 
              s(ShipG,bs="re") + 
              s(Depth,bs="ds",k=5,m=c(1,0)) +
              s(Salinity,bs="ds",k=5,m=c(1,0)) +
              s(Temperature,bs="ds",k=5,m=c(1,0)) +
              s(Phytoplankton,bs="ds",k=5,m=c(1,0)) + 
              offset(log(SweptArea))
            
            gear0 <- table(spp$Gear, spp$bio.juv==0)[,"FALSE"]
            gear0 <- names(gear0[gear0==0])
            if(length(gear0)>0) dat <- dat[!(dat$Gear %in%  gear0),]
            ships0 <- table(spp$ShipG, spp$bio.juv==0)[,"FALSE"]
            ships0 <- names(ships0[ships0==0])
            if(length(ships0)>0) dat <- dat[!(dat$ShipG %in%  ships0),]
          }
          if(j == "adu"){
            formula <- bio.adult ~	s(lon,lat,bs="ds",k=256,m=c(1,0.5)) + 
              s(ctime,bs="ds",k=ny,m=c(1,0)) +
              ti(ctime,lon,lat,d=c(1,2),bs=c("ds","ds"),k=c(ny,10),m=list(c(1,0),c(1,0.5))) +
              te(timeOfYear,lon,lat,d=c(1,2),bs=c("cc","ds"),k=c(6,30),m=list(c(1,0),c(1,0.5))) +
              Gear + 
              s(ShipG,bs="re") + 
              s(Depth,bs="ds",k=5,m=c(1,0)) + 
              s(Salinity,bs="ds",k=5,m=c(1,0)) +
              s(Temperature,bs="ds",k=5,m=c(1,0)) +
              s(Phytoplankton,bs="ds",k=5,m=c(1,0)) + 
              offset(log(SweptArea))
            table(spp$bio.adult==0)
            gear0 <- table(spp$Gear, spp$bio.adult==0)[,"FALSE"]
            gear0 <- names(gear0[gear0==0])
            if(length(gear0)>0) dat <- dat[!(dat$Gear %in%  gear0),]
            ships0 <- table(spp$ShipG, spp$bio.adult==0)[,"FALSE"]
            ships0 <- names(ships0[ships0==0])
            if(length(ships0)>0) dat <- dat[!(dat$ShipG %in%  ships0),]
          }
          ny <- length(unique(spp$Year))
          fit <- tryCatch(gam(formula, 
                              data = dat,
                              family = tw,
                              gamma = 1.4,
                              method = "ML"),
                          error = function(e) {print("not converged");
                            NA})
          
          tryCatch(save(fit, file = paste0("output/gam_models_env2/environment", i,"_",j,"_",c,"_biomass_gam.RData")),
                   error = function(e) {print("not converged");
                     NA})
        }
        fit <- NULL
      } 
    }
  }
}
