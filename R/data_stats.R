
#library(wesanderson)
#library(lubridate)
#library(dplyr)
#library(wesanderson)
#library(plotly)
#library(leaflet)


wes_colors = c(wes_palettes$GrandBudapest1,wes_palettes$GrandBudapest2,wes_palettes$Zissou1,wes_palettes$Rushmore)
###

#' read data
#'
#' read data
#' @return data
#' @examples
#' eg = read_mesothelioma();
#' @export
read_bike_data = function(path = "data/",recalculate = F){
  if(file.exists(paste0(path,"comptage_summarised_2020_2022.csv")) == F | recalculate == T){

    comptage_files = list.files(path,pattern="comptage_velo_202",full.names = T)

    comptage_list = lapply(as.list(comptage_files),read.csv)

    comptage_complet = merge(comptage_list[[1]],comptage_list[[2]],by = intersect(names(comptage_list[[1]]), names(comptage_list[[3]])),all =T)
    comptage_complet = merge(comptage_complet,comptage_list[[3]],by = intersect(names(comptage_complet), names(comptage_list[[3]])),all =T)

    colnames(comptage_complet) = gsub("compteur_","",colnames(comptage_complet))

    #july is a messy name for some reason
    july_messy = comptage_complet$Date[regexpr("juil. 2021",comptage_complet$Date)>0]
    july_messy = gsub("juil. 2021","2021-07",july_messy)

    july_day = vapply(strsplit(july_messy," "), `[`, 1, FUN.VALUE=character(1))
    july_day = ifelse(nchar(july_day)==1,paste0("0",july_day),july_day)

    july_year_month = vapply(strsplit(july_messy," "), `[`, 2, FUN.VALUE=character(1))
    july_hour = vapply(strsplit(july_messy," "), `[`, 3, FUN.VALUE=character(1))

    july_fixed = paste0(july_year_month,"-",july_day, " ",july_hour)

    comptage_complet$Date[regexpr("juil. 2021",comptage_complet$Date)>0] = july_fixed

    #some dates are missing seconds... LOL...
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


read_metadata = function(path = "data/"){
meta = read.csv(paste0(path,"localisation_des_compteurs_velo.csv"))
meta
}


parse_bike_data = function(path = "data/",recalculate = F){
  if((file.exists(paste0(path,"comptage_summarised_plotly_2020_2022.csv")) == F) | recalculate == T) {
  comptage_summarised = read_bike_data()
  meta = read_metadata()

  meta$Nom[40] = "Notre-Dame Est / Bellerive "
  meta$Nom[45] = "16e Avenue / Bélanger"
  meta$Nom[44] = "Sainte-Croix / Du Collège"

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


  #labels for plots
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


  #loess smooth
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




####
barplotly_statistics = function(bike_data = parsed_bike_data[[3]],
                                datelim=c(as.Date("2020-01-01","%Y-%m-%d"),as.Date("2022-10-31","%Y-%m-%d"))
                              ){


  m <- list(b = 50,t = 50,pad = 4)

  plot_ly(
    x = signif(bike_data$sum,3),
    y = bike_data$Nom,
    name = "Total",
    type = "bar",
    height = 1000,
    color = bike_data$Nom
  ) %>%
    layout(yaxis = list(categoryorder = "total ascending",color = '#ffffff'),
           showlegend = FALSE,
           title = list(font= list(color='#ffffff'),text='Nombre de passages totaux (2020-2022)'),
           xaxis = list(title = 'Nombre de passages',color = '#ffffff'),
           paper_bgcolor="#222222",
           plot_bgcolor="#222222",
           margin = m
          )


}


#loess smooth
loess_plotly <- function(data = parsed_bike_data,stations="Brébeuf / Rachel Brébeuf",
                         datelim=c(as.Date("2020-01-01","%Y-%m-%d"),as.Date("2022-10-31","%Y-%m-%d")),add_similar=FALSE,add_trend=FALSE) {

  #get 2 stations before & after that look similar...
  if(add_similar){
  rank = c(1:nrow(data[[3]]))[data[[3]]$Nom %in% stations]
  similar_stations = c(rank-2,rank+2)
  similar_stations[1] = ifelse(similar_stations[1]<1,1,similar_stations[1])
  similar_stations[2] = ifelse(similar_stations[2]>nrow(data[[3]]),nrow(data[[3]]),similar_stations[2])

  station_id = data[[3]]$ID[c(similar_stations[1]:similar_stations[2])]

  }

  if(add_similar==F){
    station_id = data[[3]]$ID[data[[3]]$Nom %in% stations]
  }

  #datelim
  data_plot = data[[1]][data[[1]]$day_counts>=datelim[1] & data$day_counts<=datelim[2],]

  #stations
  data_stations = data[[1]][data[[1]]$station %in% station_id, ]


  #simplify for quicker load of plot.
  #data_stations = data_stations[seq(1,nrow(data_stations),by = 7),]

  moyenne = data[[1]][data[[1]]$station %in% 1e+08,]


  m <- list(
    b = 150,
    t = 50,
    pad = 4
  )

if(add_trend){
  fig = plot_ly(data = data_stations,
               x = ~day_counts,
               y = ~loess_smooth,
               color = ~Nom,
               text = ~Nom) %>%
  add_lines(hoverinfo = 'text') %>%
    add_trace(x = moyenne$day_counts,y = moyenne$loess_smooth,
              line=list(color='white',dash='dot'),
              name = 'Moyenne',text = "Moyenne",color='black',mode = 'lines',hoverinfo = 'text') %>%
    layout(title = list(font= list(color='#ffffff'),text='Tendances'),
           yaxis = list(title = 'Nombre de passages',color = '#ffffff'),
           xaxis = list(title='Date',color = '#ffffff'),
           margin = m,
           paper_bgcolor="#222222",
           plot_bgcolor="#222222",
           legend= list(
             orientation = 'h',
             y= -0.3,
             font = list(
               family = "sans-serif",
               size = 12,
               color = "#000"),
             bgcolor = "#E2E2E2",
             bordercolor = "#FFFFFF",
             borderwidth = 2)
           )
}

  if(add_trend==F)
  {fig = plot_ly(data = data_stations,
                 x = ~day_counts,
                 y = ~loess_smooth,
                 color = ~Nom,
                 text = ~Nom) %>%
    add_lines(hoverinfo = 'text') %>%
    layout(title = list(font= list(color='#ffffff'),text='Tendances'),
           yaxis = list(title = 'Nombre de passages',color = '#ffffff'),
           xaxis = list(title='Date',color = '#ffffff'),
           margin = m,
           paper_bgcolor="#222222",
           plot_bgcolor="#222222",
           legend= list(
             orientation = 'h',
             y= -0.3,
             font = list(
               family = "sans-serif",
               size = 12,
               color = "#000"),
             bgcolor = "#E2E2E2",
             bordercolor = "#FFFFFF",
             borderwidth = 2)
    )}

  fig

}

####
scatter_stats_plotly = function(data = parsed_bike_data,stations="Saint-Laurent/Bellechasse",
                                datelim=c(as.Date("2020-01-01","%Y-%m-%d"),as.Date("2022-10-31","%Y-%m-%d")),add_trend=FALSE) {

  station_id = data[[3]]$ID[data[[3]]$Nom %in% stations]
  data_plot = data[[1]][data[[1]]$station %in% station_id, ]
  data_plot = data_plot[data_plot$day_counts>=datelim[1] & data_plot$day_counts<=datelim[2],]

  m <- list(b = 50,t = 50,pad = 4)

  fig = NULL

  if(add_trend==T) {

  fig = plot_ly(data = data_plot,
               x = ~day_counts,
               y = ~counts,
               type = "scatter",
          text =  ~paste("<b>Date: </b>", day_counts, '<br><b>Décompte:</b>', counts),
          hovertemplate = paste('%{text}<extra></extra>')) %>%
    add_trace(y = ~loess_smooth,  line=list(color='white',dash='dot'),
              name = 'Moyenne',text = "Moyenne",mode = 'lines',hoverinfo = 'text') %>%
    layout(title = list(font= list(color='#ffffff'),text=paste0("Station ",stations)),
           xaxis = list(title = 'Date',color = '#ffffff'),
           yaxis = list(title = 'Nombre de passages',color = '#ffffff'),
           showlegend = FALSE,
           margin = m,
           paper_bgcolor="222222",
           plot_bgcolor="222222")
  }

  if(add_trend==F) {

    fig = plot_ly(data = data_plot,
                  x = ~day_counts,
                  y = ~counts,
                  type = "scatter",
                  text =  ~paste("<b>Date: </b>", day_counts, '<br><b>Décompte:</b>', counts),
                  hovertemplate = paste('%{text}<extra></extra>')) %>%
      layout(title = list(font= list(color='#ffffff'),text=paste0("Station ",stations)),
             xaxis = list(title = 'Date',color = '#ffffff'),
             yaxis = list(title = 'Nombre de passages',color = '#ffffff'),
             showlegend = FALSE,
             margin = m,
             paper_bgcolor="#222222",
             plot_bgcolor="#222222")
  }

  fig

}

dummy_plot = function(){
  hist(rnorm(1000))

}
