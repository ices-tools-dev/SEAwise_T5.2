library(mgcv)

env <- T
# case <- "WW"

## BS Whiting all

if(!env)load("input/pred.grid.fine_new_spp.RData",verbose = T)
# if(env)load("input/pred.grid.fine2.RData",verbose = T)

if(env){
  library(dplyr) 
  # grid <- pred.grid.fine[[2]][[length(pred.grid.fine[[2]])]]
  load("input/grid_NM_covariates.RData",verbose=T)
  
  #pred.grid <- pred.grid[pred.grid$Area_27 %in% c("4.a","4.b","4.c"),] 
}
# unique(cov_dat_bq$year)

if(env){
  g1 <- get(load("input/pred.grid.fine.RData",verbose = T))
  g2 <- get(load("input/pred.grid.fine2.RData",verbose = T))
  g3 <- get(load("input/pred.grid.fine_new_spp.RData",verbose = T))
  g4 <- get(load("input/pred.grid.fine_new_spp2.RData",verbose = T))
  g5 <- get(load("input/pred.grid.fine3.RData",verbose = T))
  
  grid.list <- append(append(append(append(g1,g2),g3),g4),g5)
  grid.list <- lapply(grid.list, function(x)do.call("rbind",x))
  grid.list <- lapply(grid.list, function(x){
    x$Year <- NULL
    x <- x[!duplicated(x),]
    x
  })
  grid <- do.call("rbind",grid.list)
  grid <- grid[!duplicated(grid),]
  rownames(grid)<- NULL
  # pred.grid <- left_join(cov_dat_bq, grid.list[[species_name]][,c("lon","lat","Area_27","SweptArea","Ecoregion")])
  pred.grid <- left_join(cov_dat_bq, grid[,c("lon","lat","Area_27","SweptArea","Ecoregion")])
  colnames(pred.grid)[c(1,3,4,5,6,7,10,11)] <-  c("Year","Salinity","Temperature","DIN","Detritus","Phytoplankton","ctime","Depth")
  grid.full <- pred.grid[pred.grid$Year %in% c(1980:2021,seq(2025,2060,5)),]
}
unique(grid.full$Ecoregion)
range(grid.full$lat[grid.full$Ecoregion == "Greater North Sea"], na.rm = T)
range(grid.full$lon[grid.full$Ecoregion == "Greater North Sea"], na.rm = T)

species   <- names(pred.grid.fine)
stages    <- c("all","juv","adu")[c(2)]

if(!env)gam_files <- file.path("output/gam_models_ML",list.files("output/gam_models_ML"))
if(env)gam_files <- file.path("output/gam_models_env2",list.files("output/gam_models_env2"))
if(env) pred_files <- list.files("output/future_predictions2/")
if(env){
  fnames <- lapply(strsplit(gam_files, "environment"),"[[",2)
  names  <- unlist(lapply(unlist(fnames), function(x)strsplit(x,"_biomass_gam.RData")))
  # if(case == "WW") names <- names[grepl(c("WW"),names)]
  # if(case == "BS") names <- names[grepl(c("BS"),names)]
  # if(case == "NS") names <- names[grepl(c("NS"),names)]
  species <- seq_along(names)
  # grid.full <- pred.grid
  # grid.full <- grid.full[grid.full$Year %in% seq(2020,2060,5),]
}

ctimes <- c(0.125,0.375,0.625,0.875)
#spp<-195
spp <- species[1]
stage <- stages[1]
# for(spp in species[-c(1:195)]){
for(spp in species){
  if(!env){
    grid <- pred.grid.fine[[spp]]
    grid <- do.call("rbind",grid)
    
    writeLines(paste0("doing : ",spp))
    
    for(stage in stages){
      writeLines(paste0("doing : ",stage))
      load(gam_files[grepl(paste0(spp,"_",stage),gam_files)],verbose=T)    
      if(class(fit)[1]=="gam"){
        
        for(q in seq_along(ctimes)){
          writeLines(paste0("doing quarter: ",q))
          #if(!is.na(fit)){
          stable <- summary(fit)$s.table
          sm     <- names(which(stable[,"p-value"] > 0.05))
          # add covariates: timeOfYear / ctime / ShipG (re) / Gear
          grid$ctime      <- grid$Year + ctimes[q]
          grid$timeOfYear <- grid$ctime - grid$Year #
          grid$Gear       <- names(sort(table(fit$model$Gear[fit$y>0]),decreasing = T))[1]
          grid$ShipG      <- names(sort(table(fit$model$ShipG[fit$y>0]),decreasing = T))[1]
          grid$model      <- "gam"
          
          #pred            <- tryCatch(predict(fit, grid, exclude = c("s(ShipG)",sm), se.fit = T),error = function(e) {print("something wrong");
          #                 NULL})
          pred            <- tryCatch(predict(fit, grid, exclude = c("s(ShipG)",sm), se.fit = T, type = "response"),error = function(e) {print("something wrong");
            NULL})
          if(!is.null(pred)){
            grid$prediction <- pred$fit
            grid$se         <- pred$se.fit
            print(range(grid$prediction/grid$SweptArea)/1000)
            if(env)save(grid, file = paste0("output/future_predictions/",names[spp],"_predictions_biomass.RData"))
            if(!env)save(grid, file = paste0("output/historical_predictions_ML/",spp,"_",stage,"_Q",q,"_ML_predictions_biomass.RData"))
          }
          pred <- NULL
        }  
      }
      
      
    }
      
      
  }
  
  if(env){
    
    fnam <- paste0(names[spp],"_predictions_biomass.RData")
    if(!(fnam %in% pred_files)){
      # species_name <- paste0(strsplit(names[spp],"_")[[1]][c(1,2)],collapse = "_")
      if(length(strsplit(names[spp],"_")[[1]])>3){
        grid <- grid.full
        case <- strsplit(names[spp],"_")[[1]][c(4)]
        # if(case == "BS"){grid <- grid.full[grid.full$Ecoregion == "Baltic Sea" & !is.na(grid.full$Ecoregion),]}
        # if(case == "WW"){grid <- grid.full[grid.full$Area_27 %in% c("7.e","7.f","7.g","7.j.2"),]}
        # if(case == "NS"){grid <- grid.full[grid.full$Area_27 %in% c("4.c","4.b","4.a","7.d"),]}
        
        if(case == "NS") grid <- grid[grid$Ecoregion %in% "Greater North Sea",] 
        if(case == "WW") grid <- grid[grid$Ecoregion %in% c("Celtic Seas", "Bay of Biscay and the Iberian Coast") | grid$Area_27 == "7.e",]  # 4a 6a 6b1 6b2
        if(case == "BS") grid <- grid[grid$Ecoregion %in% "Baltic Sea",]
        
        if(case == "NS") grid <- grid[grid$Area_27 != "7.e",]
        if(case == "WW") grid <- grid[!(grid$Area_27 %in% c("4.a","6.a","6.b.1","6.b.2","7.d")),]
        print(range(grid$lon))
        print(range(grid$lat))
        
        # if(grepl("BS",names[grid])){grid <- grid.full[grid.full$Ecoregion == "Baltic Sea" & !is.na(grid.full$Ecoregion),]}
        # if(grepl("WW",names[spp])){grid <- grid.full[grid.full$Area_27 %in% c("7.e","7.f","7.g","7.j.2"),]}
        # if(grepl("NS",names[spp])){grid <- grid.full[grid.full$Area_27 %in% c("4.c","4.b","4.a","7.d"),]}
        pred <- NULL
        # load the model
        
        writeLines(paste0("doing :",names[spp]))
        load(gam_files[grepl(names[spp],gam_files)],verbose=T)
        
        if(class(fit)[1]=="gam"){
          #if(!is.na(fit)){
          stable <- summary(fit)$s.table
          sm     <- names(which(stable[,"p-value"] > 0.05))
          # add covariates: timeOfYear / ctime / ShipG (re) / Gear
          # grid$ctime      <- grid$Year + ctimes[q]
          grid$timeOfYear <- grid$ctime - grid$Year #
          grid$Gear       <- names(sort(table(fit$model$Gear[fit$y>0]),decreasing = T))[1]
          grid$ShipG      <- names(sort(table(fit$model$ShipG[fit$y>0]),decreasing = T))[1]
          grid$model      <- "gam"
          
          #pred            <- tryCatch(predict(fit, grid, exclude = c("s(ShipG)",sm), se.fit = T),error = function(e) {print("something wrong");
          #                 NULL})
          pred            <- tryCatch(predict(fit, grid, exclude = c("s(ShipG)",sm), se.fit = T, type = "response"),error = function(e) {print("something wrong");
            NULL})
          if(!is.null(pred)){
            grid$prediction <- pred$fit
            grid$se         <- pred$se.fit
            grid$Gear       <- NULL
            grid$ShipG      <- NULL
            print(range(grid$prediction/grid$SweptArea)/1000)
            print(range(grid$lat))
            print(range(grid$lon))
            if(env)save(grid, file = paste0("output/future_predictions2/",names[spp],"_predictions_biomass.RData"))
            if(!env)save(grid, file = paste0("output/historical_predictions_ML/",spp,"_",stage,"_Q",q,"_ML_predictions_biomass.RData"))
          }
        }
        grid <- NULL
      }
    }
  }
}
# 
# specs <- unique(unlist(lapply(strsplit(list.files("output/historical_predictions_ML"),"_"),function(x){
#   paste0(x[c(1,2)],collapse = "_")
# })))
# 
# sort(names(grid.list))
# sort(specs)
# names(grid.list)[!(names(grid.list) %in% specs)]
