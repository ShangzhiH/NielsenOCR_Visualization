source("Fonctions.R")

shinyServer(
  function(input, output) {
    
    CampaignTypeInput <- reactive({
      if(input$campaign_type == 0)
        return("ALL")
      else if(input$campaign_type == 1)
        return("VIDEO")
      else if(input$campaign_type == 2)
        return("STANDARD")
    })
    
    SiteTypeInput <- reactive({
      if(input$site_type == 0)
        return("ALL")
      else if(input$site_type == 1)
        return("RESEAUX")
      else if(input$site_type == 2)
        return("EDITEURS")
    })
    
    TargetInput <- reactive({
      if(input$radio == 0)
        return("ALL")
      else if(input$radio == 1)
        return("GENRE")
      else if(input$radio == 2) {
        if(is.null(input$cas)) {
          return("Not ready")
        }
        else
          return(input$cas)
      }
    })

    GenreInput <- reactive({
      if(TargetInput() == "GENRE") {
        if(is.null(input$genre)) {
          return("Not ready")
        }
        else if(input$genre == 0) {
          return('M')
        }
        else if(input$genre == 1) {
          return('F')
        }
      }
      else { 
        return(NULL)
      }
    })
    
    
    
    GetData <- reactive({
    
      if(!is.null(GenreInput())  && GenreInput() == "Not ready")
        return(NULL)
      else if(!is.null(TargetInput()) && TargetInput() == "Not ready")
        return(NULL)
      
      DataSet = GET_DATA_FROM_BBD(LineColumns = c('SITE_NAME'), ResponseColumn = "Days_Delivery",SiteType = SiteTypeInput(), CampaignType = CampaignTypeInput(), Target = TargetInput(), GENRE = GenreInput())
      
      if(is.na(DataSet[1,1]))
        return(NULL)
      
      
      return(DataSet)
    })
    
    SupprimerFacebook <- reactive({
      DataSet = GetData()
      if(input$checkbox == FALSE) {
        DataSet = SUPPRIMER(DataSet, "Facebook")
      }
      return(DataSet)
    })
    
    SeuilData <- reactive({
      DataSet = SupprimerFacebook()
      
      Data = FUSION(DataSet)
      
      Data = Data[which(Data$Delivery_Sum > input$slider1),]
    })
    
 
    
    output$chart <- renderChart2({ 

      DataSet = SeuilData()
     
      
      
      if (is.null(DataSet))
        return(Rickshaw$new())
      h = AFFICHAGE(DataSet, input$slider1)
      h$exporting(enabled=T)
      return(h)
      
    })
    
    output$table <- renderDataTable({
      SeuilData()

    },options = list(lengthMenu = c(5, 30, 50),  pageLength = 5, paging = FALSE, scrollY = 500))
    
    output$ui <- renderUI({
      
      
      if (is.null(input$radio))
        return(Rickshaw$new()) 
      
      
      switch(input$radio,
             "1" = radioButtons("genre", label = h3("GENRE"), choices = list("HOMME" = 0, "FEMME" = 1), selected = 0, inline = TRUE),
             "2" = selectInput("cas", label = h3("CAS PARTICULIER"), 
                                              choices = list("B13-20",
                                                             "B13-24",
                                                             "B13-999",
                                                             "B18-34",
                                                             "B18-64",
                                                             "B25-34",
                                                             "B25-49",
                                                             "B2-999",
                                                             "B30-49",
                                                             "B35-49",
                                                             "B35-54",
                                                             "B35-999",
                                                             "B50-64",
                                                             "F13-49",
                                                             "F18-34",
                                                             "F18-999",
                                                             "F21-39",
                                                             "F21-49",
                                                             "F25-34",
                                                             "F25-44",
                                                             "F25-49",
                                                             "F25-64",
                                                             "F30-49",
                                                             "F35-49",
                                                             "F35-54",
                                                             "F35-64",
                                                             "F40-999",
                                                             "M13-34",
                                                             "M13-39",
                                                             "M13-49",
                                                             "M18-34",
                                                             "M25-34",
                                                             "M25-49",
                                                             "M25-64",
                                                             "M25-999",
                                                             "M35-64"), selected = "B13-20")
      )
      
    })
    
    output$campaign_number <- renderUI({
      if (input$radio == 2) {
        
        h6("Le nombre des campaignes associees a ce ciblage: ", GET_CAMPAIGN_NUMBER(Target = input$cas))
       
        
      }
      
    })
    
    output$downloadtable <- downloadHandler(
      filename = function() { paste('DataTable', '.csv', sep='') },
      content = function(file) {
        write.csv(SupprimerFacebook(), file)
      }
    )
    
    output$download_table <- renderUI({
      
      downloadButton('downloadtable', 'Telecharger la table')
    })
  
  }
)