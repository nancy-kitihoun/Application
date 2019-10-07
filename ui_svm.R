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
                 tabItem(tabName = "Donées",
                         style = "overflow-y:scroll;",
                         
                         tabBox(
                           
                           # The id lets us use input$tabset1 on the server to find the current tab
                           id = "tabset1", height = "250px",
                        tabPanel("Description1",
                            box(width = 12, 
                                title = "Créditscart", status = "primary",
                                DT::dataTableOutput("fraude")
                            ),
                         
                            box(width = 6, DT::verbatimTexOutput("Des1"),status = "warning"),
                            box(width = 6, DT::verbatimTexOutput("Des2"),status = "info")),
                         
                       tabPanel("Description2",
                              
                            box(width = 6, title="Cor", DT::dataTableOutput("tabCor") ,status = "primary"),
                            box(with=12 , status = "warning",
                                         sidebarPanel(
                                          #Definition 1er menu deroulant
                                          selectInput("variable", "Variable (axe des x):",
                                                      list("Time" , "V1" ,"V2", "V3" ,"V4" , "V5" ,"V6","V7" ,"V8" ,
                                                           "V9","V10","V11" , "V12","V13","V14"  , 
                                                            "V15", "V16"   , "V17"   , "V18" ,   "V19"  ,  "V20"  ,  "V21"  ,  
                                                             "V22", "V23", "V24", "V25" , "V26", "V27"  ,  "V28" , "Amount" )),
                                          
                                          #Definition du second menu deroulant
                                          selectInput("variable", "Variable (axe des y):",
                                                      list("Time" , "V1" ,"V2", "V3" ,"V4" , "V5" ,"V6","V7" ,"V8" ,
                                                           "V9","V10","V11" , "V12","V13","V14"  , 
                                                           "V15", "V16"   , "V17"   , "V18" ,   "V19"  ,  "V20"  ,  "V21"  ,  
                                                           "V22", "V23", "V24", "V25" , "V26", "V27"  ,  "V28" , "Amount")),
                                          
                                        plotOutput("Grap1", height = "300px"))),
                          box(with=6, status = "info",  plotOutput("Grap2", height = "300px"))),
                    tabPanel("UnTout",plotOutput("Grap3", height = "300px" ))
                    
                  )),
                  
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
                                
                                tabPanel("ArbreDeClassif", 
                                         radioButtons("ech", "Type d'echantillonage",
                                                      c("Undersampling" = "und",
                                                        "Synthetic data" = "ros")),
                                        box(width = 6, plotOutput("Tree", height = "300px")),
                                        box(width=6 ,DT::verbatimTexOutput("Tree_es"),status = "warning")),
                                        box(width = 6,DT::verbatimTexOutput("TabC"),status = "primary")),
                                tabPanel("LDA",
                                         radioButtons("ech", "Type d'echantillonage",
                                                      c("Undersampling" = "und",
                                                        "Synthetic data" = "ros")),
                                        box(width=6 ,DT::verbatimTexOutput("LDA"),status = "warning")),
                                       box(width = 6,DT::verbatimTexOutput("Tabc_lda"),status = "primary")),
                                 tabpanel("Comparaison",
                                          )
                                         
                                          )
                                )
                                         
                                    
                  
                  
                  ) 
                 
               )
               
              

              