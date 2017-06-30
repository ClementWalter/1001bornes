library(shinydashboard)
library(leaflet)

header <- dashboardHeader(title = "1001 bornes")

body <- dashboardBody(
  tags$style(type = "text/css", "#map {height: calc(120vh) !important;}"),
  tabItems(
    tabItem(tabName = "cost",
            h2("Modèle de coût tab content")
    ),
    tabItem(tabName = "bilan",
            h2("Bilan tab content")
    ),
    tabItem(tabName = "map",
            fluidRow(
              column(width = 9,
                     box(width = NULL, solidHeader = TRUE,
                         leafletOutput("map", height = 500)
                     )
              ),
              column(width = 3,
                     box(width = NULL, status = "warning",
                         checkboxGroupInput("selectLayer", label = h3("Data"), 
                                            choices = 
                                              list("Basse tension" = "bt", 
                                                   "Ligne GRDF" = "grdf",
                                                   "Haute tension" = "hta",
                                                   "Parking" = "parking",
                                                   "Recens" = "recens",
                                                   "Cons. élec/immeuble" = "conso",
                                                   "Zone" = "zone",
                                                   "Sociologique" = "socio", 
                                                   "Dépôt Ratp" = "ratp",
                                                   "Bornes électriquues" = "bornes",
                                                   "Gares SNCF" = "gares",
                                                   "Routes" = "route"),
                                            selected = NULL)
                         # p(
                         #   class = "text-muted",
                         #   paste("Note: a route number can have several different trips, each",
                         #         "with a different path. Only the most commonly-used path will",
                         #         "be displayed on the map."
                         #   )
                         # )
                     ),
                     box(width = NULL, status = "warning",
                         sliderInput("numberPlugs", value = 0, min = 0, max = 100, label = "Nombre de nouvelles bornes")),
                     box(width = NULL, status = "warning", title = "Options graphiques",
                         radioButtons("showLegend", "Afficher légende", choices = list("Oui" = TRUE, "Non" = FALSE), selected = FALSE),
                         actionButton("removePlugs", "Effacer bornes"),
                         p(h5(strong("Zoom sur"))),
                         actionButton("showParis", "Paris"),
                         actionButton("showSavoie", "Savoie")
                     )
              )
            )
    )
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
  menuItem("Carte", tabName = "map", icon = icon("map")),
  menuItem("Modèle de coût", icon = icon("money"), tabName = "cost"),
  menuItem("Bilans", icon = icon("th"), tabName = "bilan")
)
)

dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)
