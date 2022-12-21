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
  
  
  observe({

    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(data = cbind(parsed_bike_data[[3]]$Longitude,parsed_bike_data[[3]]$Latitude),
                   label = lapply(ifelse(input$language%%2==0,parsed_bike_data[[3]]$labels,parsed_bike_data[[3]]$labels_en),HTML))
   })


  
    output$plotly_perstation = renderPlotly({
     scatter_stats_plotly(data = parsed_bike_data,
                           datelim=input$Dates_scat,
                           stations=input$stationID,
                           moyenne_mobile=input$moyenne_mobile,
                           moyenne_globale=input$moyenne_globale,
                           language = input$language
                         
      )
    })
    output$barplot_totals = renderPlotly({
      barplotly_statistics(data=parsed_bike_data,
                           datelim=input$Dates_bar,
                           language = input$language)
    })
  
    output$plotly_loess = renderPlotly({
      loess_plotly(data = parsed_bike_data,
                   stations=input$Stations_loess,
                   datelim=input$Dates_loess,
                   moyenne_globale=input$moyenne_globale_loess,
                   language = input$language)
    })
  #french
  if(input$language %% 2 ==0) {
    output$Localisation = renderUI({'Localisation'})
    output$Tendances = renderUI({'Tendances'})
    output$Montreal = renderUI({'Montréal - statistiques vélo'})
    output$add_average = renderUI({'Rajouter une moyenne'})
    output$add_average2 = renderUI({'Rajouter une moyenne'})
    output$mobile = renderUI({'mobile'})
    output$globale = renderUI({'globale'})
    output$globale2 = renderUI({'globale'})
    output$year = renderUI({'Années'})
    output$general <- renderUI({
      para <- "<h3>Description générale</h3>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;J'utilise des statistiques sur le nombre de passages en vélo à Montréal en 2019-2022.
      Ces données sont disponibles publiquement sur la plateforme de <a href='https://www.donneesquebec.ca/recherche/dataset/vmtl-velos-comptage'>données québec</a>.
      Elles sont recueillies grâce à des boucles magnétiques sur des sites de comptage (pistes cyclables) réparties sur l'île de Montréal (56 sites de comptage et plus de 54 millions de passage).
      <br/><br/>"
      HTML(para)
    })
    output$metho <- renderUI({
      para <- "<strong>Méthodologie</strong><br></br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;J'utilise des statistiques sur le nombre de passages en vélo à Montréal en 2019-2022.
      Ces données sont disponibles publiquement sur la plateforme de <a href='https://www.donneesquebec.ca/recherche/dataset/vmtl-velos-comptage'>données québec</a>.
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
  #english  
  if(input$language %% 2 ==1) {
    output$Localisation = renderUI({'Location'})
    output$Tendances = renderUI({'Trends'})
    output$Montreal = renderUI({'Montreal - biking statistics'})
    output$add_average = renderUI({'Add average'})
    output$add_average2 = renderUI({'Add average'})
    output$mobile = renderUI({'smoothed'})
    output$globale = renderUI({'global'})
    output$globale2 = renderUI({'global'})
    output$year = renderUI({'Year'})
    output$general <- renderUI({
      para <- "<h3>General Description</h3>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      I use statistics on the number of bike rides in Montreal in 2019-2022.
      Data are publicly available on the <a href='https://www.donneesquebec.ca/recherche/dataset/vmtl-velos-comptage'>québec data</a> platform.
      They are collected using magnetic loops on counting sites (bike paths) distributed on the island of Montreal (56 counting sites and more than 54 million passages).<br/><br/>"
      HTML(para)
    })
    output$metho <- renderUI({
      para <- "<strong>Methods</strong><br></br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;I use statistics on the number of bike rides in Montreal in 2019-2022.
      The data are publicly available on the <a href='https://www.donneesquebec.ca/recherche/dataset/vmtl-velos-comptage'>québec data</a> platform.
      They are collected using magnetic loops on counting sites (bike paths) distributed on the island of Montreal (56 counting sites and more than 54 million passages).
      <br/><br/>
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The data was downloaded and formatted locally
      to increase the speed of queries. All analyzes are performed using
      of the language <a href='https://cran.r-project.org/'>R version 4.2.1 </a>. The dashboard is created and deployed on a
      public server thanks to the
      <a href='https://shiny.rstudio.com/'>Shiny </a> library.
      The code is available (CC-BY 4.0) on a <a href='https://github.com/seb951/VeloMtl'>public repository</a>.
        <br/><br/>
      sebastien.renaut@gmail.com --- 2022
         <br/><br/>"
      HTML(para)
    })
    
  }
  
})




}


