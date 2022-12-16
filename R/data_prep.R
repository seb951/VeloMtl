
library(lubridate)
library(dplyr)
library(wesanderson)
library(plotly)
library(leaflet)


#' read data
#'
#' read data
#' @return data
#' @param recalculate logical. If you want to recalculate from the raw files..
#' @param path Where is the data stored?
#' @examples
#' data = read_bike_data()
#' @export
read_bike_data = function(path = "VeloMtl/data/",recalculate = F){
  if(file.exists(paste0(path,"comptage_summarised_2019_2022.rds")) == F | recalculate == T){

    #get data 
    system("wget https://data.montreal.ca/dataset/f170fecc-18db-44bc-b4fe-5b0b6d2c7297/resource/fd3da18e-8f87-44e4-890b-30dff05c12b8/download/comptage_velo_2022.csv -P data/raw/")
    system("wget https://data.montreal.ca/dataset/f170fecc-18db-44bc-b4fe-5b0b6d2c7297/resource/b463fa29-8549-4664-ae68-b17ab604e0a5/download/comptage_velo_2021.csv -P data/raw/")
    system("wget https://data.montreal.ca/dataset/f170fecc-18db-44bc-b4fe-5b0b6d2c7297/resource/eec17749-1a50-47b2-bc4e-1960ddc09eff/download/comptage_velo_2020.csv -P data/raw/")
    system("wget https://data.montreal.ca/dataset/f170fecc-18db-44bc-b4fe-5b0b6d2c7297/resource/2cd0e082-f818-4014-9390-8e1c197ba806/download/comptage_velo_2019.csv -P data/raw/")
        
    comptage_files = list.files(paste0(path,"raw/"),pattern="comptage_velo_20",full.names = T)

    comptage_list = lapply(as.list(comptage_files),read.csv)

    comptage_complet = merge(comptage_list[[1]],comptage_list[[2]],by = intersect(names(comptage_list[[1]]), names(comptage_list[[4]])),all =T)
    comptage_complet = merge(comptage_complet,comptage_list[[3]],by = intersect(names(comptage_complet), names(comptage_list[[4]])),all =T)
    comptage_complet = merge(comptage_complet,comptage_list[[4]],by = intersect(names(comptage_complet), names(comptage_list[[4]])),all =T)
    
    colnames(comptage_complet) = gsub("compteur_","",colnames(comptage_complet))

    #July is a messy month for some reason
    july_messy = comptage_complet$Date[regexpr("juil. 2021",comptage_complet$Date)>0]
    july_messy = gsub("juil. 2021","2021-07",july_messy)

    july_day = vapply(strsplit(july_messy," "), `[`, 1, FUN.VALUE=character(1))
    july_day = ifelse(nchar(july_day)==1,paste0("0",july_day),july_day)

    july_year_month = vapply(strsplit(july_messy," "), `[`, 2, FUN.VALUE=character(1))
    july_hour = vapply(strsplit(july_messy," "), `[`, 3, FUN.VALUE=character(1))

    july_fixed = paste0(july_year_month,"-",july_day, " ",july_hour)

    comptage_complet$Date[regexpr("juil. 2021",comptage_complet$Date)>0] = july_fixed

    #some dates are missing seconds (LOL...)
    comptage_complet$Date = ifelse(regexpr(":[0-9]+:",comptage_complet[,1])>0,comptage_complet[,1],paste0(comptage_complet[,1],":00"))

    comptage_complet$Date = ymd_hms(comptage_complet$Date)

    comptage_summarised = comptage_complet %>% mutate(day_counts=floor_date(Date,unit= "day")) %>% group_by(day_counts) %>% summarise_at(colnames(comptage_complet)[-1],sum,na.rm=T)

    saveRDS(comptage_summarised, file = paste0(path,'comptage_summarised_2019_2022.rds'))
  }

  if(file.exists(paste0(path,"comptage_summarised_2019_2022.rds")) == T){
    comptage_summarised = readRDS(file = paste0(path,"comptage_summarised_2019_2022.rds"))  
  }

  comptage_summarised
}


#' parse bike data
#'
#' parse bike data
#' @return parsed bike data
#' @param path Where is the data?
#' @examples
#' parsed_bike_data = parse_bike_data()
#' @export
parse_bike_data = function(path = "VeloMtl/data/"){
    comptage_summarised = read_bike_data(path = path)

    #if(file.exists(paste0(path,"localisation_des_compteurs_velo.csv") == F)) {meta = readRDS(file = paste0(path,"localisation_des_compteurs_velo.rds"))}
    #if(file.exists(paste0(path,"localisation_des_compteurs_velo.csv"))) {meta = read.csv(paste0(path,"localisation_des_compteurs_velo.csv"))}
    meta = readRDS(file = paste0(path,"localisation_des_compteurs_velo.rds"))    
    meta$Nom[3]  = 'Berri'
    meta$Nom[9]  = "Brébeuf / Rachel"
    meta$Nom[12] = "René-Lévesque / Wolfe"
    meta$Nom[15] = "Boyer / Rosemont"
    meta$Nom[24] = "Viger / Saint-Urbain"
    meta$Nom[30] = "Notre-Dame Est / Bellerive "
    meta$Nom[32] = "Sainte-Croix / Du Collège"
    meta$Nom[35] = "Valois / la Fontaine"
    meta$Nom[36] = "Souligny / Saint-Émile"
    meta$Nom[37] = "16e Avenue / Bélanger"
    meta$Nom[57]  = "Eco-Display - Maisonneuve/Greene"


    ####prep data:
    data_meta_match = match(colnames(comptage_summarised)[-1],meta$ID)
    meta_match = meta[data_meta_match,]

    #add sums
    meta_match$sum = signif(apply(comptage_summarised[,-1],2,sum),4)
    meta_match$sum2022 = signif(apply(comptage_summarised[(comptage_summarised$day_counts >=as.Date("2022-01-01","%Y-%m-%d")) & (comptage_summarised$day_counts < as.Date("2023-01-01","%Y-%m-%d")) ,-1],2,sum),4)
    meta_match$sum2021 = signif(apply(comptage_summarised[(comptage_summarised$day_counts >=as.Date("2021-01-01","%Y-%m-%d")) & (comptage_summarised$day_counts < as.Date("2022-01-01","%Y-%m-%d")) ,-1],2,sum),4)
    meta_match$sum2020 = signif(apply(comptage_summarised[(comptage_summarised$day_counts >=as.Date("2020-01-01","%Y-%m-%d")) & (comptage_summarised$day_counts < as.Date("2021-01-01","%Y-%m-%d")) ,-1],2,sum),4)
    meta_match$sum2019 = signif(apply(comptage_summarised[(comptage_summarised$day_counts >=as.Date("2019-01-01","%Y-%m-%d")) & (comptage_summarised$day_counts < as.Date("2020-01-01","%Y-%m-%d")) ,-1],2,sum),4)
    meta_match = meta_match[order( meta_match$sum,decreasing=T),]

    #remove test stations
    meta_match = meta_match[regexpr("Test",meta_match$Nom)<0,]
    
    #ordered count data
    temp = comptage_summarised[,-1]
    data_meta_match = match(meta_match$ID,colnames(temp))
    comptage_summarised = data.frame(day_counts = comptage_summarised$day_counts,temp[,data_meta_match],check.names=F)

    #pretty labels for plots (map)
    meta_match$labels = paste0("<b>",
                               toupper(meta_match$Nom),
                               "</b><p>Statut: ",
                               meta_match$Statut,
                               "</b><p>Année implanté: ",
                               meta_match$Annee_implante,
                               "</b><p>Nombre de passages (2019-2022): ",
                               ifelse(meta_match$sum >100000,paste0(signif((meta_match$sum )/1000000,3),"M"),meta_match$sum ),
                               "</b><p>Latitude: ",
                               meta_match$Latitude,
                               "</b><p>Longitude: ",
                               meta_match$Longitude,
                               "</b><p>ID: ",
                               meta_match$ID
                               )

    #prepare data for plotly/ggplot
    comptage_summarised_plotly = data.frame(day_counts = rep(comptage_summarised$day_counts,ncol(comptage_summarised)-1))
    comptage_summarised_plotly$counts = unlist(c(comptage_summarised[,-1]))
    comptage_summarised_plotly$station = unlist(lapply(colnames(comptage_summarised)[-1],rep,nrow(comptage_summarised)))
    comptage_summarised_plotly$Nom = unlist(lapply(meta_match$Nom,rep,nrow(comptage_summarised)))

    global_average= data.frame(day_counts=comptage_summarised[,1], counts = rowMeans(comptage_summarised[,-1]), station = 999, Nom = "Moyenne")
    comptage_summarised_plotly = rbind(comptage_summarised_plotly,global_average)

    #loess smoothing
    loess_smooth = list()

    for(i in 2:ncol(comptage_summarised)){
      compt_loess = data.frame(counts = comptage_summarised[,i],index = 1:nrow(comptage_summarised))
      loess10 <- loess(counts ~ index, data = compt_loess, span=0.10) # 10% smoothing span
      smoothed10 <- predict(loess10)
      loess_smooth[[i-1]] = smoothed10

      if(i == ncol(comptage_summarised)){
        compt_loess = data.frame(counts = rowMeans(comptage_summarised[,-1]),index = 1:nrow(comptage_summarised))
        loess10 <- loess(counts ~ index, data = compt_loess, span=0.10) # 10% smoothing span
        smoothed10 <- predict(loess10)
        loess_smooth[[i]] = smoothed10
      }
    }

    comptage_summarised_plotly$loess_smooth = unlist(loess_smooth)
    comptage_summarised_plotly$loess_smooth[comptage_summarised_plotly$loess_smooth<0] = 0

    #final object
    parsed_bike_data = list(comptage_summarised_plotly,comptage_summarised,meta_match)
    parsed_bike_data
}



#' dummy data
#'
#' dummy data
#' @return a list
#' @examples
#' data_test = dummy_data()
#' @export
dummy_data = function(){
  list(
  data.frame(day_counts=c(rep(as.Date("2020-01-01","%Y-%m-%d"),10),rep(as.Date("2021-01-01","%Y-%m-%d"),10)),
             station=rep(c(1001,1e+08),10),
             counts = seq(1,20),
             loess_smooth =seq(1,20),
             Nom=rep(c('Boyer / Everett','moyenne'),10)),
  data.frame(Nom=letters[1:10],sum=sample(1:10)),
  data.frame(Nom=letters[1:10],sum=sample(1:10),ID = seq(1001,1010))
)
}


