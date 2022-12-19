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
#' @import devtools
#' @noRd

app_ui = function(request){
  choices = as.list(parse_bike_data(path = 'data/')[[3]]$Nom)
  tagList(
    golem_add_external_resources(),
    navbarPage(title = "Montréal - statistiques vélo",

               theme = shinytheme("darkly"),

               tabPanel("Localisation",
                        icon = icon("map-pin"),
                        mainPanel(width =12,
                                  h3("Description générale"),
                                  htmlOutput("general"),
                                  leafletOutput("mymap",width = 1200, height=600)
                                  )
               ),
               tabPanel("Stations",
                        icon = icon("bicycle"),
                        sidebarPanel(width = 3,
                          selectInput("stationID", "Station:",
                                      choices = choices, selected = "Saint-Laurent/Bellechasse"),

                           sliderInput("Dates_scat",
                                    "Date:",
                                    min = as.Date("2019-01-01","%Y-%m-%d"),
                                    max = as.Date("2022-12-31","%Y-%m-%d"),
                                    value=c(as.Date("2019-01-01","%Y-%m-%d"),as.Date("2022-12-31","%Y-%m-%d")),
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
                                                 min = as.Date("2019-01-01","%Y-%m-%d"),
                                                 max = as.Date("2022-12-31","%Y-%m-%d"),
                                                 value=c(as.Date("2019-01-01","%Y-%m-%d"),as.Date("2022-12-31","%Y-%m-%d")),
                                                 timeFormat="%Y-%m-%d"),
                                     br(),
                                     strong("Rajouter une moyenne:"),
                                     checkboxInput("moyenne_globale_loess","globale", value = FALSE),
                                     br(),
                                     checkboxGroupInput("Stations_loess","Stations:",choices = choices,selected = "Saint-Laurent/Bellechasse"),
                        ),
                        mainPanel(width = 9,"",
                                  plotlyOutput('plotly_loess',height = '500px')),

               ),
               tabPanel("Total",
                        icon = icon("bars"),
                        sidebarPanel(width =3,
                                     radioButtons("Dates_bar","Années:",choices =  c("Max" = "",
                                                                                     "2022" = "2022",
                                                                                     "2021" = "2021",
                                                                                     "2020" = "2020",
                                                                                     "2019" = "2019")
                                                  ,selected="")),
                        mainPanel("",width = 9,plotlyOutput('barplot_totals')),
               ),
               tabPanel("Information",
                        icon = icon("question"),
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
