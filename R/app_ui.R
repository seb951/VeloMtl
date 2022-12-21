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
    actionButton("language", "En / Fr",class = "btn-success",
                 style = "position: absolute; top: 8px; right: 7px; z-index:10000; font-size:80%;  border-color: #ad6415; background-color: #ad6415"),
    golem_add_external_resources(),
    navbarPage(title = htmlOutput("Montreal",inline=T),

               theme = shinytheme("darkly"),

               tabPanel(htmlOutput("Localisation",inline = T),
                        icon = icon("map-pin"),
                        mainPanel(width =12,
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
                          strong(htmlOutput("add_average",inline = T)),
                          checkboxInput("moyenne_mobile",htmlOutput("mobile",inline = T), value = FALSE),
                          checkboxInput("moyenne_globale",htmlOutput("globale",inline = T), value = FALSE)

                        )
                        ,

                        mainPanel(width = 9,"",
                                  plotlyOutput("plotly_perstation")),


               ),
               tabPanel(htmlOutput("Tendances",inline=T),
                        icon = icon("chart-simple"),
                        sidebarPanel(width =3,
                                     sliderInput("Dates_loess",
                                                 "Date:",
                                                 min = as.Date("2019-01-01","%Y-%m-%d"),
                                                 max = as.Date("2022-12-31","%Y-%m-%d"),
                                                 value=c(as.Date("2019-01-01","%Y-%m-%d"),as.Date("2022-12-31","%Y-%m-%d")),
                                                 timeFormat="%Y-%m-%d"),
                                     br(),
                                     strong(htmlOutput("add_average2",inline = T)),
                                     checkboxInput("moyenne_globale_loess",htmlOutput("globale2",inline = T), value = FALSE),
                                     br(),
                                     checkboxGroupInput("Stations_loess","Stations:",choices = choices,selected = "Saint-Laurent/Bellechasse"),
                        ),
                        mainPanel(width = 9,"",
                                  plotlyOutput('plotly_loess',height = '500px')),

               ),
               tabPanel("Total",
                        icon = icon("bars"),
                        sidebarPanel(width =3,
                                     radioButtons("Dates_bar",htmlOutput("year",inline = T),choices =  c("Max" = "",
                                                                                     "2022" = "2022",
                                                                                     "2021" = "2021",
                                                                                     "2020" = "2020",
                                                                                     "2019" = "2019")
                                                  ,selected="")),
                        mainPanel("",width = 9,plotlyOutput('barplot_totals')),
               ),
               tabPanel("Information",
                        icon = icon("question"),
                        mainPanel(width =12,
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
