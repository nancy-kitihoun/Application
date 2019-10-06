library(shiny)
install.packages("shinydashboard")
library(shinydashboard)

dashboardPage(title = "Détection des fraudes sur les cartes de crédits",
              skin = ("green"),
              dashboardHeader(title = "Détection des fraudes sur les cartes de crédits"),
              
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Introduction", tabName = "intro", icon = icon("home")),
                  menuItem("Données", tabName = "datafile", icon = icon("table")),
                  menuItem("Analyse", tabName = "analysis", icon = icon("chart-bar"))
                )
              ))
              
              dashboardBody(
               tabItems(
                  # Introduction 
                  #faire un tabItem
        
                  # Données
                 #faire un tabItem
                  
                  # Analyse
                  tabItem(tabName="analysis",
                          mainPanel(
                    
                                tabsetPanel(type = "pills",
                                tabPanel("SVM", 
                                         radioButtons("ech", "Type d'echantillonage",
                                                      c("Undersampling" = "und",
                                                        "Synthetic data" = "ros")),
                                         box(width = 8, plotOutput("plotspoint", height = "300px")),
                                         box(width = 8, plotOutput("plottauxsvm", height = "300px"))
                                         
                                         
                                         ),
                                
                                tabPanel("Autres méthodes", 
                                         box(width = 8, plotOutput("plotcomp", height = "300px")))
                                )
                                         
                                    
                  
                  
                  ) 
                 
               )
               )
              )

              