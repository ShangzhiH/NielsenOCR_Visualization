source("Fonctions.R")

shinyUI( 
  fluidPage(
 
    titlePanel("La performance des sites pour un ciblage choisi"),
    
    sidebarLayout(
      sidebarPanel(
        helpText("Choisir un ciblage et voir les differentes performances"),
        
        
        
        h3("Cocher pour choisir Facebook"),
        checkboxInput("checkbox", label = "Facebook", value = TRUE),
        
        h3("Cocher pour choisir le type de campaigne"),
        radioButtons("campaign_type", 
                           label = h3(""), 
                           choices = list("ALL" = 0, "VIDEO" = 1, 
                                          "STANDARD" = 2),
                           selected = 0, inline = TRUE),
        
        h3("Cocher pour choisir le type de site"),
        radioButtons("site_type", 
                     label = h3(""), 
                     choices = list("ALL" = 0, "RESEAUX" = 1, 
                                    "EDITEURS" = 2),
                     selected = 0, inline = TRUE),
      
        h3("Choisir le seuil(impression totale pour un ciblage)"),
        sliderInput("slider1", label = "",
                    min = 0, max = 10000000, value = 1000000),
        
        
        radioButtons("radio", label = h3("Ciblage"), choices = list("PAS DE FILTRAGE" = 0, "GENRE" = 1, "CAS PARTICULIER" = 2), selected = 0, inline = TRUE),
        uiOutput("ui"),
        uiOutput("campaign_number")
      ),
    
      
      mainPanel(
        
        tabsetPanel(tabPanel("Diagramme", showOutput("chart", "highcharts")),
        tabPanel("Tableau", uiOutput('download_table'),dataTableOutput('table'))
        )
        
      )
    )
    
  )
  
)