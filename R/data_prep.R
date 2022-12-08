
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
#' @param path. Where is the data?
#' @examples
#' eg = read_bike_data();
#' @export
read_bike_data = function(path = "data/",recalculate = F){
  if(file.exists(paste0(path,"comptage_summarised_2020_2022.csv")) == F | recalculate == T){

    comptage_files = list.files(path,pattern="comptage_velo_202",full.names = T)

    comptage_list = lapply(as.list(comptage_files),read.csv)

    comptage_complet = merge(comptage_list[[1]],comptage_list[[2]],by = intersect(names(comptage_list[[1]]), names(comptage_list[[3]])),all =T)
    comptage_complet = merge(comptage_complet,comptage_list[[3]],by = intersect(names(comptage_complet), names(comptage_list[[3]])),all =T)

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

    write.csv(comptage_summarised,paste0(path,'comptage_summarised_2020_2022.csv'))

    comptage_summarised = read.csv(paste0(path,"comptage_summarised_2020_2022.csv"),row.names = 1,check.names = F)
    comptage_summarised$day_counts = ymd(comptage_summarised$day_counts)
  }

  if(file.exists(paste0(path,"comptage_summarised_2020_2022.csv")) == T){
    comptage_summarised = read.csv(paste0(path,"comptage_summarised_2020_2022.csv"),row.names = 1,check.names = F)
    comptage_summarised$day_counts = ymd(comptage_summarised$day_counts)
  }

  comptage_summarised
}


#' parse bike data
#'
#' parse bike data
#' @return parsed bike data
#' @param path. Where is the data?
#' @param recalculate logical. If you want to recalculate from the raw files.
#' @examples
#' eg = parse_bike_data();
#' @export
parse_bike_data = function(path = "data/",recalculate = F){
  if((file.exists(paste0(path,"comptage_summarised_plotly_2020_2022.csv")) == F) | recalculate == T) {
    comptage_summarised = read_bike_data()
    meta = read.csv(paste0(path,"localisation_des_compteurs_velo.csv"))

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

    #order metadata
    meta_match$sum = apply(comptage_summarised[,-1],2,sum)
    meta_match = meta_match[order( meta_match$sum,decreasing=T),]

    #ordered count data
    temp = comptage_summarised[,-1]
    data_meta_match = match(meta_match$ID,colnames(comptage_summarised)[-1])
    comptage_summarised = data.frame(day_counts = comptage_summarised$day_counts,temp[,data_meta_match],check.names=F)


    #pretty labels for plots (map)
    meta_match$labels = paste0("<b>",
                               toupper(meta_match$Nom),
                               "</b><p>Statut: ",
                               meta_match$Statut,
                               "</b><p>Année implanté: ",
                               meta_match$Annee_implante,
                               "</b><p>Nombre de passages (2020-2022): ",
                               ifelse(meta_match$sum >100000,paste0(signif((meta_match$sum )/1000000,3),"M"),meta_match$sum ))


    #prepare data for plotly/ggplot
    comptage_summarised_plotly = data.frame(day_counts = rep(comptage_summarised$day_counts,ncol(comptage_summarised)-1))
    comptage_summarised_plotly$counts = unlist(c(comptage_summarised[,-1]))
    comptage_summarised_plotly$station = unlist(lapply(colnames(comptage_summarised)[-1],rep,nrow(comptage_summarised)))
    comptage_summarised_plotly$Nom = unlist(lapply(meta_match$Nom,rep,nrow(comptage_summarised)))

    global_average= data.frame(day_counts=comptage_summarised[,1], counts = rowMeans(comptage_summarised[,-1]), station = 100000000, Nom = "Moyenne")
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

    #write/read outputs
    write.csv(comptage_summarised,paste0(path,'parsed_comptage_summarised_2020_2022.csv'))
    write.csv(comptage_summarised_plotly,paste0(path,'comptage_summarised_plotly_2020_2022.csv'))
    write.csv(meta_match,paste0(path,'parsed_localisation_des_compteurs_velo.csv'))

    comptage_summarised = read.csv(paste0(path,"parsed_comptage_summarised_2020_2022.csv"),row.names = 1,check.names = F)
    comptage_summarised$day_counts = ymd(comptage_summarised$day_counts)

    meta = read.csv(paste0(path,"parsed_localisation_des_compteurs_velo.csv"))

    comptage_summarised_plotly = read.csv(paste0(path,"comptage_summarised_plotly_2020_2022.csv"),row.names = 1,check.names = F)
    comptage_summarised_plotly$day_counts = ymd(comptage_summarised_plotly$day_counts)
    comptage_summarised_plotly$station = as.character(comptage_summarised_plotly$station)

    parsed_bike_data = list(comptage_summarised_plotly,comptage_summarised,meta)
  }

  if(file.exists(paste0(path,"comptage_summarised_plotly_2020_2022.csv"))) {
    comptage_summarised = read.csv(paste0(path,"parsed_comptage_summarised_2020_2022.csv"),row.names = 1,check.names = F)
    comptage_summarised$day_counts = ymd(comptage_summarised$day_counts)

    meta = read.csv(paste0(path,"parsed_localisation_des_compteurs_velo.csv"))

    comptage_summarised_plotly = read.csv(paste0(path,"comptage_summarised_plotly_2020_2022.csv"),row.names = 1,check.names = F)
    comptage_summarised_plotly$day_counts = ymd(comptage_summarised_plotly$day_counts)
    comptage_summarised_plotly$station = as.character(comptage_summarised_plotly$station)

    parsed_bike_data = list(comptage_summarised_plotly,comptage_summarised,meta)
  }
  parsed_bike_data
}



