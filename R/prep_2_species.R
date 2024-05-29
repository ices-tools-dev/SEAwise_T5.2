## ---------------
## Prepare prediction grid (pred.grid.fine)
## 13/06/2022
## Tobias K. Mildenberger (tobm@aqua.dtu.dk)
## ---------------


## Load required packages
## ---------------------
 remotes::install_github("tokami/fishdist/fishdist",
                         auth_token = "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")
require(fishdist)


## Variables
## ---------------------
start.year <- 1967





## WK FISHDIS species
# wkfishdis <- c(
#     ## Cod (Gadus morhua) – represents a common species widespread from east to west
#     "Gadus morhua" = 126436,
#     ## Hake (Merluccius merluccius) – represents a common species widespread from
#     ## south to north
#     "Merluccius merluccius" = 126484,
#     ## Turbot (Scophthalmus maximus) - represents a rare species widespread from east
#     ## to west
#     "Scophthalmus maximus" = 127149,
#     ## Conger eel (Conger conger) – represents a rare species widespread from south to
#     ## north
#     "Conger conger" = 126285
# )




wkfishdis <- c(
  "Solea solea" = 127160,
  "Pleuronectes platessa" = 127143,
  "Melanogrammus aeglefinus" = 126437,
  "Merlangius merlangus" = 126438,
  "Lepidorhombus whiffiagonis" = 127146,
  "Lophius budegassa" = 126554,
  "Raja clavata" = 105883,
  "Raja brachyura" = 367297,
  "Raja montagui" = 105887,
  "Squalus acanthias" = 105923,
  "Scyliorhinus canicula" = 105814,
  "Sepia officinalis" = 141444,
  "Loligo vulgaris" =140271)


wkfishdis <- c("Eutrigla gurnardus" = 150637)

names(wkfishdis) %in% bio.pars$Species

nspec <- length(wkfishdis)
data.scenarios <- c("all","juv","adult")
lat.range <- c(36, 63)
lon.range <- c(-18, 25)


## directories
datdir <- "data"
datdir <- "input"

## Load data
## -----------
## surveys
load("../fishdist/data/datras.RData")
## bathymetric data
load(file.path(datdir, "bathy.sub.statrec.grid.RData"))
load("/media/klaas/Elements/Seawise/WP5/Task_5_2/Rcode/seawise_task5_2/hh_hl.RData")

data$hh
data$hl

datras <- list(HH = data$hh, HL = data$hl, CA = ca_data)
class(datras) <- 'fdist.datras'
## Prepare species data
## ---------------------
specs <- NULL
data.list <- vector("list", nspec)
pred.grid.fine <- vector("list", nspec)
i<-1
datras$HL

datras$HL$AphiaID <- datras$HL$Valid_Aphia
datras$CA$AphiaID <- datras$CA$Valid_Aphia
datras$CA$Age     <- datras$CA$AgeRings

datras$CA$AphiaID <- datras$CA$ValidAphiaID

for(i in 1:nspec){

    spec <- names(wkfishdis)[i]
    print(spec)
    specs <- c(specs,gsub(" ", "_", spec))

    ## HABITAT based on all years: overall realised habtiat -> at least one observation for each stat rec
    dati0 <- prep.data(datras, AphiaID = wkfishdis[i], est.n = TRUE, est.bio = TRUE,split.juv.adults = T)
    dati <- prep.species(dati0, min.gears = 0, min.surveys = 0, min.hauls = 0)
    data.list[[i]] <- dati
    if(FALSE){
        plotdist(dati0, plot.survey.dist = FALSE)
        ## dev.print(pdf,"problem_when_using_stat_rec_for_habitat_turbot.pdf", width = 12, height = 10)
    }

    ## prediction grid for each year: match by stat recs or ices area
    years <- unique(dati$Year)
    ny <- length(years)
    pred.grid.fine[[i]] <- vector("list", ny)
    for(y in 1:ny){
        print(years[y])
        ## datiy <- subset(dati, Year == years[y])
        datiy <- dati[which(as.numeric(as.character(dati$Year)) == years[y]),]
        # datiy$Year <- droplevels(datiy$Year)
        ## Prediction grid based on stat recs
        ## ind <- which(bathy.sub.statrec.grid$StatRec %in% unique(datiy$StatRec))
        ## Prediction grid based on ices area
        ind <- which(bathy.sub.statrec.grid$Area_27 %in% unique(datiy$Area_27))
        pred.grid.fine[[i]][[y]] <- bathy.sub.statrec.grid[ind,]
        if(FALSE){
            grid <- bathy.sub.statrec.grid[ind,]
            tmp <- reshape2::acast(grid, lon~lat, value.var = "Depth")
            diff(as.numeric(rownames(tmp)))
            diff(as.numeric(colnames(tmp)))
        }
        colnames(pred.grid.fine[[i]][[y]])[colnames(pred.grid.fine[[i]][[y]]) == "SubStatRec_area"] <- "SweptArea"
        ## NEW:
        pred.grid.fine[[i]][[y]]$Year <- as.numeric(as.character(years[y]))
    }
    names(pred.grid.fine[[i]]) <- as.character(years)

}
length(pred.grid.fine)

names(pred.grid.fine) <- specs[1]
names(data.list) <- specs[1]
## save data
datadir <- "input"
save(pred.grid.fine,data.list,
     file = file.path(datdir,paste0("inputs_",specs[1],".RData")), version = 2)

## save data
save(pred.grid.fine,
     file = file.path(datdir,"pred.grid.fine.RData"), version = 2)
