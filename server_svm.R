library(shiny)
library(shinydashboard)
library(DT)


#pour lire la table
server <- function(input, output) {
  
  
  df <- read.csv(input$dataFile$datapath,
                 header = TRUE,
                 sep = "",
                 nrows=10
  )
},  options = list(scrollX = TRUE , dom = 't'))
  

#pour le bouton action et avoir les données dans une variable; 

data = reactiveValues()

observeEvent(input$import, {
  data$table = read.csv(input$dataFile$datapath,
                        header = TRUE,
                        sep = "",
                        nrows=10)
})


  
  output$point <- renderPlot({
   ech <- switch(input$ech,
                  und= peut on mettre le code pour cela #faire une reactive event peut être ici pour switcher entre les deux types d'echantillons
                   ros= )
   
                   )
    
   
  }
}
  