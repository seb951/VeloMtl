
library(lubridate)
library(dplyr)
library(wesanderson)
library(plotly)
library(leaflet)


#' default colors
#'
#' This generate a vector of colors
#' @return vector of colors
#' @param default logical. R color palette or palette based on Wes Anderson movies.
#' @param size int. number of colors you need.
#' @examples
#' colors = default_colors()
#' @export
default_colors = function(default = T,size = 55){
  if(default==F){
    default_colors = unlist(wes_palettes)[-c(3,4,10,12,17,18,19,20,21,22,29,30,35,36,40,41,48,49,51,54,5,56,59,64,66,67,68,69,70,71,76,83,85,87,89,91,92)]
    names(default_colors)=NULL
    if(size<=55)default_colors = default_colors[1:size]
    if(size>55) default_colors = default_colors[c(1:55,1:(size-55))]}
  ###
  if(default==T){
    default_colors = colors()[1:size]
  }
  default_colors
}


#' barplotly_statistics
#'
#' Create a bar plot for all stations
#' @return plotly bar plot
#' @param data list. input data from parse_bike_data(). A list of size 3.
#' @param datelim date. dates
#' @param plot_colors a vector of colors to look pretty.
#' @examples
#' fig = barplotly_statistics(data =list("a","b",data.frame(Nom=letters[1:10],sum=sample(1:10),ID = seq(1001,1010))) )
#' @export
barplotly_statistics = function(data = dummy_data(),
                                datelim = "",
                                plot_colors = default_colors(default = F)
                              ){
  meta = data[[3]]
  fig = plot_ly(
    x = meta[,colnames(meta) == paste0('sum',datelim)],
    y = meta$Nom,
    name = "Total",
    type = "bar",
    height = 1000,
    color = meta$Nom,
    colors= plot_colors[order(meta$Nom)]
  ) %>%
    layout(yaxis = list(categoryorder = "total ascending",color = '#ffffff',categoryorder = "array",categoryarray = meta$Nom),
           showlegend = FALSE,
           title = list(font= list(color='#ffffff'),text = paste0("Nombre de passages total (",ifelse(datelim=="","2019-2022",datelim),")")),
           xaxis = list(title = 'Nombre de passages',color = '#ffffff'),
           paper_bgcolor="#222222",
           plot_bgcolor="#222222",
           margin =  list(b = 50,t = 50,pad = 4)
          )
  fig
}



#' loess_plotly
#'
#' create a line plot of smoothed average
#' @return plotly line plot
#' @param data list. input data from parse_bike_data(). A list of size 3.
#' @param stations str. stations
#' @param datelim date. dates
#' @param moyenne_globale logical.
#' @param moyenne_mobile logical.
#' @param plot_colors a vector of colors to look pretty
#' @examples
#' fig = loess_plotly()
#' @export
loess_plotly <- function(data = dummy_data(),
                         stations="Brébeuf / Rachel",
                         datelim=c(as.Date("2019-01-01","%Y-%m-%d"),as.Date("2022-12-31","%Y-%m-%d")),
                         moyenne_globale = FALSE,
                         moyenne_mobile = FALSE,
                         plot_colors = default_colors(default = F)
                         ) {

  #get stations ID
  station_id = data[[3]]$ID[data[[3]]$Nom %in% stations]

  #colors
  plotly_colors = plot_colors[data[[3]]$Nom %in% stations]
  plotly_colors = plotly_colors[length(plotly_colors):1]

  #datelim
  data_plot = data[[1]][data[[1]]$day_counts>=datelim[1] & data[[1]]$day_counts<=datelim[2],]

  #stations
  data_stations = data_plot[data_plot$station %in% station_id, ]

  #global average
  moyenne = data_plot[data_plot$station %in% 999,]

  fig = plot_ly(data = data_stations,
               x = ~day_counts,
               y = ~loess_smooth,
               color = ~Nom,
               colors = plotly_colors,
               text = ~Nom) %>%
  add_lines(hoverinfo = 'text')

  #add the global average
  if(moyenne_globale){
  fig = fig %>% add_trace(x = moyenne$day_counts,y = moyenne$loess_smooth,
              line=list(color='white',dash='dot'),
              name = 'Moyenne globale',text = "Moyenne globale (toutes les stations combinées)",color='black',mode = 'lines',hoverinfo = 'text')
  }

  #layout
  fig = fig %>%
    layout(title = list(font= list(color='#ffffff'),text='Moyennes Mobiles (par station)'),
           yaxis = list(title = 'Nombre de passages',color = '#ffffff'),
           xaxis = list(title='Date',color = '#ffffff'),
           margin = list(b = 150,t = 50,pad = 4),
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
  fig
}



#' scatter_stats_plotly
#'
#' create a scatter plot of counts per day
#' @return a scatter plotly plot
#' @param data input data from parse_bike_data(). A list of size 3.
#' @param stations str. stations.
#' @param datelim date. dates.
#' @param moyenne_globale logical.
#' @param moyenne_mobile logical.
#' @param plot_colors a vector of colors to look pretty
#' @examples
#' fig = scatter_stats_plotly()
#' @export
scatter_stats_plotly = function(data = dummy_data(),
                                stations="Brébeuf / Rachel",
                                datelim=c(as.Date("2020-01-01","%Y-%m-%d"),as.Date("2022-12-31","%Y-%m-%d")),
                                moyenne_globale=FALSE,
                                moyenne_mobile = FALSE,
                                plot_colors = default_colors(default = F)
                                ) {


  #stations ID
  station_id = data[[3]]$ID[data[[3]]$Nom %in% stations]

  #colors
  plotly_colors = plot_colors[data[[3]]$Nom %in% stations]

  #datelim
  data_plot = data[[1]][data[[1]]$day_counts>=datelim[1] & data[[1]]$day_counts<=datelim[2],]

  #stations
  data_stations = data_plot[data_plot$station %in% station_id, ]

  #average
  moyenne = data_plot[data_plot$station %in% 999,]


  fig = plot_ly(data = data_stations,
               x = ~day_counts,
               y = ~counts,
               type = "scatter",
               color = ~Nom,
               colors = plotly_colors,
          text =  ~paste("<b>Date: </b>", day_counts, '<br><b>Décompte:</b>', counts),
          hovertemplate = paste('%{text}<extra></extra>'))

  #add smoothed average
  if(moyenne_mobile==T) {
    fig = fig %>% add_trace(y = ~loess_smooth,  line=list(color='yellow',dash='dot'),
              name = 'Moyenne mobile',text = paste0("Moyenne mobile (station: ",stations, ")"),mode = 'lines',hoverinfo = 'text')
  }

  #add global average
  if(moyenne_globale==T){
    fig = fig %>% add_trace(x = moyenne$day_counts,y = moyenne$loess_smooth,
                            line=list(color='white',dash='dot'),
                            name = 'Moyenne globale',text = "Moyenne globale (toutes les stations combinées)",color='black',mode = 'lines',hoverinfo = 'text')
  }

  fig = fig %>%
    layout(title = list(font= list(color='#ffffff'),text=paste0("Station ",stations)),
           xaxis = list(title = 'Date',color = '#ffffff'),
           yaxis = list(title = 'Nombre de passages',color = '#ffffff'),
           showlegend = FALSE,
           margin = list(b = 50,t = 50,pad = 4),
           paper_bgcolor="#222222",
           plot_bgcolor="#222222")
  fig
}



#' dummy hist
#'
#' this is a dummy plot to use as placeholder in the dashboard.
#' @return histogram
#' @examples
#' fig = dummy_plot()
#' @export
dummy_plot = function(){
  hist(rnorm(1000))
}







