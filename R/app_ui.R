#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import lubridate
#' @import dplyr
#' @import wesanderson
#' @import plotly
#' @import leaflet
#' @import shinythemes
#' @noRd

app_ui = function(request){
  tagList(
    golem_add_external_resources(),
    navbarPage(title = "Montréal - statistiques vélo",

               theme = shinytheme("darkly"),

               tabPanel("Localisation",
                        icon = icon("map-pin"),
                        mainPanel(h3('Localisation des stations de comptages'),
                                  width = 12,
                                  leafletOutput("mymap",width = 1200, height=600)
                                  )
               ),
               tabPanel("Stations",
                        icon = icon("bicycle"),
                        sidebarPanel(width = 3,
                          selectInput("stationID", "Station:",
                                      choices = as.list(parse_bike_data()[[3]]$Nom), selected = "Saint-Laurent/Bellechasse"),

                           sliderInput("Dates_scat",
                                    "Date:",
                                    min = as.Date("2020-01-01","%Y-%m-%d"),
                                    max = as.Date("2022-10-31","%Y-%m-%d"),
                                    value=c(as.Date("2020-01-01","%Y-%m-%d"),as.Date("2022-10-31","%Y-%m-%d")),
                                    timeFormat="%Y-%m-%d"),
                          br(),
                          strong('Rajouter une moyenne:'),
                          checkboxInput("moyenne_mobile","mobile", value = FALSE),
                          checkboxInput("moyenne_globale","globale", value = FALSE)

                        )
                        ,

                        mainPanel(width = 9,"",
                                  plotlyOutput("plotly_perstation")),


               ),
               tabPanel("Tendances",
                        icon = icon("chart-simple"),
                        sidebarPanel(width =3,
                                     sliderInput("Dates_loess",
                                                 "Date:",
                                                 min = as.Date("2020-01-01","%Y-%m-%d"),
                                                 max = as.Date("2022-10-31","%Y-%m-%d"),
                                                 value=c(as.Date("2020-01-01","%Y-%m-%d"),as.Date("2022-10-31","%Y-%m-%d")),
                                                 timeFormat="%Y-%m-%d"),
                                     br(),
                                     strong("Rajouter une moyenne:"),
                                     checkboxInput("moyenne_globale_loess","globale", value = FALSE),
                                     br(),
                                     checkboxGroupInput("Stations_loess","Stations:",choices = parse_bike_data()[[3]]$Nom,selected = "Saint-Laurent/Bellechasse"),
                        ),
                        mainPanel(width = 9,"",
                                  plotlyOutput('plotly_loess',height = '500px')),

               ),
               tabPanel("Total",
                        icon = icon("bars"),
                        mainPanel("",width =12,plotlyOutput('barplot_totals')),
               ),
               tabPanel("Information",
                        icon = icon("question"),
                        mainPanel(width =12,strong("Description générale"),
                                  htmlOutput("general")),
                        mainPanel(width =12,strong("Méthodologie"),
                                  htmlOutput("metho"))
               )
               )
               )
}





#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js use_favicon favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon()
,
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "VeloMtl"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
