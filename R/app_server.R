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
#' @import devtools
#' @noRd

app_server <- function(input, output, session) {

  parsed_bike_data = parse_bike_data(path = 'data/')

  output$plotly_perstation = renderPlotly({
    scatter_stats_plotly(data = parsed_bike_data,
                         datelim=input$Dates_scat,
                         stations=input$stationID,
                         moyenne_mobile=input$moyenne_mobile,
                         moyenne_globale=input$moyenne_globale

                         )
    })

  output$barplot_totals = renderPlotly({
    barplotly_statistics(data=parsed_bike_data,
                       datelim=input$Dates_bar)
  })

  output$plotly_loess = renderPlotly({
    loess_plotly(data = parsed_bike_data,
                 stations=input$Stations_loess,
                 datelim=input$Dates_loess,
                 moyenne_mobile=FALSE,
                 moyenne_globale=input$moyenne_globale_loess)
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


  output$general <- renderUI({
    para <- "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;J'utilise des statistiques sur le nombre de passages en vélo à Montréal en 2019-2022.
      Ses données sont disponibles publiquement sur la plateforme de <a href='https://www.donneesquebec.ca/recherche/dataset/vmtl-velos-comptage'>données québec</a>.
      Elles sont recueillies grâce à des boucles magnétiques sur des sites de comptage (pistes cyclables) réparties sur l'île de Montréal (56 sites de comptage et plus de 54 millions de passage).
      <br/><br/>"
    HTML(para)
  })

  output$metho <- renderUI({
    para <- "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;J'utilise des statistiques sur le nombre de passages en vélo à Montréal en 2019-2022.
      Ses données sont disponibles publiquement sur la plateforme de <a href='https://www.donneesquebec.ca/recherche/dataset/vmtl-velos-comptage'>données québec</a>.
      Elles sont recueillies grâce à des boucles magnétiques sur des sites de comptage (pistes cyclables) réparties sur l'île de Montréal (56 sites de comptage et plus de 54 millions de passage).
      <br/><br/>
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Les données ont été téléchargées et formatées localement
      afin d'augmenter la rapidité des requêtes. Toutes les analyses sont réalisés à l'aide
      du language <a href='https://cran.r-project.org/'>R version 4.2.1 </a>. Le tableau de bord est créé et déployé sur un
      serveur public grâce à la
      librairie <a href='https://shiny.rstudio.com/'>Shiny </a>.
      Le code est disponible (CC-BY 4.0) sur un <a href='https://github.com/seb951/VeloMtl'>dépot public</a>.
        <br/><br/>
      sebastien.renaut@gmail.com --- 2022
         <br/><br/>"
    HTML(para)
  })




}


