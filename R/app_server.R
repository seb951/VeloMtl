#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import lubridate
#' @import dplyr
#' @import wesanderson
#' @import plotly
#' @import leaflet
#' @noRd

app_server <- function(input, output, session) {

  parsed_bike_data = parse_bike_data(recalculate = F)

  output$plotly_perstation = renderPlotly({
    scatter_stats_plotly(data = parsed_bike_data,
                         datelim=input$Dates_scat,
                         station=input$stationID,
                         add_trend=input$trend)
    })

  output$barplot_totals = renderPlotly({
    barplotly_statistics(bike_data=parsed_bike_data[[3]],
                       datelim=input$Dates_bar)
  })

  output$plotly_loess = renderPlotly({
    loess_plotly(data = parsed_bike_data,
                 stations=input$stationID_loess,
                 datelim=input$Dates_loess,
                 add_similar=input$similar,
                 add_trend=input$trend_trend)
  })

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = cbind(parsed_bike_data[[3]]$Longitude,parsed_bike_data[[3]]$Latitude),
                 label = lapply(parsed_bike_data[[3]]$labels,HTML))
  })

output$mymap2 = renderPlot({dummy_plot()})


  output$general =
    output$clinical <- renderUI({
      para7 <- "J'utilise les données d'ici:données publiques disponibles sur la plateforme de <a href='https://www.donneesquebec.ca/recherche/dataset/vmtl-velos-comptage'> données québec</a>. Les données sont préparées et formattées localement
      afin d'augmenter la rapidité de l'interface utilisateur.
        <br/><br/>
      sebastien.renaut@gmail.com --- 2022"
      HTML(para7)
    })




}


