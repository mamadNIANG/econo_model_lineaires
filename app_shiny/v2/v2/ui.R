library(shinydashboard)

header <- dashboardHeader(title = "PREDICTION PRIX")

sidebar <- dashboardSidebar( 
  sidebarMenu(
    menuItem("Analyse univariée", tabName = "AN1", icon = icon("list-alt")),
    menuItem("Analyse bivariée", tabName = "AN2", icon = icon("fas fa-chart-bar")),
    menuItem("Prédiction du prix médian", tabName = "Predic", icon = icon("fas fa-dollar-sign"))
  )
)

body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "AN1",
            fluidRow( column(4, selectInput("var",label='Choisissez la variable à analyser', choice = sort(names(x))), offset = 4)),
            fluidRow( verbatimTextOutput("summa")),
            fluidRow( verbatimTextOutput("manq")),
            fluidRow(box(title = "Distribution de la variable", plotOutput("densite")), box(title="Boîte à Moustaches", plotOutput("bplot")))
    ),                
    
    # Second tab content
    tabItem(tabName = "AN2",
            fluidRow( column(4, selectInput("var1",label='Choisissez la première variable', choice = list(`B` = "B",`CRIM` = "CRIM",`DIS` = "DIS",`LSTAT` = "LSTAT",`MEDV` = "MEDV",`NOX` = "NOX",`RAD` = "RAD",`RM` = "RM",`PTRATIO` = "PTRATIO",`ZN` = "ZN"))),
                      column(4, selectInput("var2",label='Choisissez la seconde variable', choice = list(`B` = "B",`CRIM` = "CRIM",`DIS` = "DIS",`LSTAT` = "LSTAT",`MEDV` = "MEDV",`NOX` = "NOX",`RAD` = "RAD",`RM` = "RM",`PTRATIO` = "PTRATIO",`ZN` = "ZN")))),
            
            fluidRow(plotOutput("plot2")),
            fluidRow(textOutput("correlation"))
    ),
    
    #Third tab content
    tabItem(tabName = "Predic",
            fluidRow(column(4, selectInput("CHAS",label='Choisissez 1 si le secteur d�limite la rivi�re', choice =c(0,1))), column(4, numericInput("CRIM", label = "Quel est le taux de criminalit�", value = 0.5, min=0))),
            fluidRow(column(4, numericInput("NOX", label = "Quel est la concentration d'acide nitrique", value = 0.5, min=0, max=1)), column(4, numericInput("RM", label = "Quel est le nombre moyen de pi�ces par logement", value = 4, min=0))),
            fluidRow(column(4, numericInput("DIS", label = "Distance pond�r�e à 5 centres d'emploi de Boston", value = 4, min=0)), column(4, numericInput("RAD", label = "Indice d'accessibilit� aux autoroutes radiales", value = 4, min=0))),
            fluidRow(column(4, numericInput("ZN", label = "Proportion de terrains résidentiels", value = 10, min=0)),column(4, numericInput("PTRATIO", label = "Quel est le ratio �l�ves-enseignant", value = 10, min=0))),
            fluidRow(column(4, numericInput("B", label = "Quelle est la proportion, pour 1000, de noirs", value = 356.67, min=0)), column(4, numericInput("LSTAT", label = "% de statut inf�rieur de la population", value = 11, min=0))),
            
            fluidPage(column(4, actionButton("go", "Cliquer pour pr�dire le prix"), offset=4)),
            fluidPage(textOutput("Pred"))
            
            
    )
  )
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body)