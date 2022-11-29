
library(lubridate)
library(dplyr)
library(wesanderson)
library(plotly)
library(leaflet)


#' default colors
#'
#' default colors
#' @return data
#' @examples
#' eg = default_colors();
#' @export
default_colors = function(default = T){
  if(default==F){
    default_colors = unlist(wes_palettes)[-c(3,4,10,12,17,18,19,20,21,22,29,30,35,36,40,41,48,49,51,54,5,56,59,64,66,67,68,69,70,71,76,83,85,87,89,91,92)]
    names(default_colors)=NULL}###
  if(default==T){
    default_colors = colors()[1:55]
  }
  default_colors
}




####



#' plotly barplot
#'
#' pplotly barplot
#' @return plotly barplot
#' @examples
#' eg = barplotly_statistics();
#' @export
barplotly_statistics = function(bike_data = parsed_bike_data[[3]],
                                datelim=c(as.Date("2020-01-01","%Y-%m-%d"),as.Date("2022-10-31","%Y-%m-%d")),
                                          plot_colors = default_colors(default = F)
                              ){


  plot_ly(
    x = signif(bike_data$sum,3),
    y = bike_data$Nom,
    name = "Total",
    type = "bar",
    height = 1000,
    color = bike_data$Nom,
    colors= plot_colors[order(bike_data$Nom)]
  ) %>%
    layout(yaxis = list(categoryorder = "total ascending",color = '#ffffff',categoryorder = "array",categoryarray = bike_data$Nom),
           showlegend = FALSE,
           title = list(font= list(color='#ffffff'),text='Nombre de passages totaux (2020-2022)'),
           xaxis = list(title = 'Nombre de passages',color = '#ffffff'),
           paper_bgcolor="#222222",
           plot_bgcolor="#222222",
           margin =  list(b = 50,t = 50,pad = 4)
          )


}



#' loess_plotly
#'
#' loess_plotly
#' @return plotly
#' @examples
#' eg = loess_plotly();
#' @export
loess_plotly <- function(data = parsed_bike_data,
                         stations="Brébeuf / Rachel",
                         datelim=c(as.Date("2020-01-01","%Y-%m-%d"),as.Date("2022-10-31","%Y-%m-%d")),
                         add_similar=FALSE,
                         moyenne_globale = FALSE,
                         moyenne_mobile = FALSE,
                         plot_colors = default_colors(default = F)) {

  #get 2 stations before & after that look similar...
  if(add_similar){
  rank = c(1:nrow(data[[3]]))[data[[3]]$Nom %in% stations]
  similar_stations = c(rank-2,rank+2)
  similar_stations[1] = ifelse(similar_stations[1]<1,1,similar_stations[1])
  similar_stations[2] = ifelse(similar_stations[2]>nrow(data[[3]]),nrow(data[[3]]),similar_stations[2])

  station_id = data[[3]]$ID[c(similar_stations[1]:similar_stations[2])]

  plotly_colors = plot_colors[c(similar_stations[1]:similar_stations[2])]


  }

  if(add_similar==F){
    station_id = data[[3]]$ID[data[[3]]$Nom %in% stations]

    plotly_colors = plot_colors[data[[3]]$Nom %in% stations]
  }

  #datelim
  data_plot = data[[1]][data[[1]]$day_counts>=datelim[1] & data[[1]]$day_counts<=datelim[2],]

  #stations
  data_stations = data_plot[data_plot$station %in% station_id, ]

  #moyenne globale
  moyenne = data_plot[data_plot$station %in% 1e+08,]


  m <- list(b = 150,t = 50,pad = 4)

  fig = plot_ly(data = data_stations,
               x = ~day_counts,
               y = ~loess_smooth,
               color = ~Nom,
               colors = plotly_colors,
               text = ~Nom) %>%
  add_lines(hoverinfo = 'text')

  if(moyenne_globale){
  fig = fig %>% add_trace(x = moyenne$day_counts,y = moyenne$loess_smooth,
              line=list(color='white',dash='dot'),
              name = 'Moyenne globale',text = "Moyenne globale (toutes les stations combinées)",color='black',mode = 'lines',hoverinfo = 'text')
  }

    fig = fig %>%
    layout(title = list(font= list(color='#ffffff'),text='Moyennes Mobiles (par station)'),
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

  fig

}



#' scatter_stats_plotly
#'
#' scatter_stats_plotly
#' @return plotly
#' @examples
#' eg = scatter_stats_plotly();
#' @export
scatter_stats_plotly = function(data = parsed_bike_data,
                                stations="Brébeuf / Rachel",
                                datelim=c(as.Date("2020-01-01","%Y-%m-%d"),as.Date("2022-10-31","%Y-%m-%d")),
                                moyenne_globale=FALSE,
                                add_similar = FALSE,
                                moyenne_mobile = FALSE,
                                plot_colors = default_colors(default = F)) {


  #get 2 stations before & after that look similar...
  if(add_similar){
    rank = c(1:nrow(data[[3]]))[data[[3]]$Nom %in% stations]
    similar_stations = c(rank-2,rank+2)
    similar_stations[1] = ifelse(similar_stations[1]<1,1,similar_stations[1])
    similar_stations[2] = ifelse(similar_stations[2]>nrow(data[[3]]),nrow(data[[3]]),similar_stations[2])

    station_id = data[[3]]$ID[c(similar_stations[1]:similar_stations[2])]

    plotly_colors = plot_colors[c(similar_stations[1]:similar_stations[2])]


  }

  if(add_similar==F){
    station_id = data[[3]]$ID[data[[3]]$Nom %in% stations]

    plotly_colors = plot_colors[data[[3]]$Nom %in% stations]
  }

  #datelim
  data_plot = data[[1]][data[[1]]$day_counts>=datelim[1] & data[[1]]$day_counts<=datelim[2],]

  #stations
  data_stations = data_plot[data_plot$station %in% station_id, ]

  #moyenne
  moyenne = data_plot[data_plot$station %in% 1e+08,]


  fig = NULL
  fig = plot_ly(data = data_stations,
               x = ~day_counts,
               y = ~counts,
               type = "scatter",
               color = ~Nom,
               colors = plotly_colors,
          text =  ~paste("<b>Date: </b>", day_counts, '<br><b>Décompte:</b>', counts),
          hovertemplate = paste('%{text}<extra></extra>'))

  if(moyenne_mobile==T) {
    fig = fig %>% add_trace(y = ~loess_smooth,  line=list(color='yellow',dash='dot'),
              name = 'Moyenne mobile',text = paste0("Moyenne mobile (station: ",stations, ")"),mode = 'lines',hoverinfo = 'text')
  }

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
#' dummy hist
#' @return dummy
#' @examples
#' eg = dummy_plot();
#' @export
dummy_plot = function(){
  hist(rnorm(1000))

}







