
library(shiny)
ui<-navbarPage("Détection des fraudes sur les cartes de crédits",
               tabPanel("Data Import",
                        sidebarLayout(sidebarPanel( fileInput("file","télécharger nos données CSV",multiple = FALSE),
                                                    tags$hr(),
                                                    h5(helpText("Selectionner les données")),
                                                    checkboxInput(inputId = 'rose', label = 'rose', value = FALSE),
                                                    checkboxInput(inputId = "under", "under", FALSE)
                                                   
                        ),
                        mainPanel(uiOutput("tb1"))
                        ) ),
               tabPanel("Model_dev",
                        sidebarLayout(sidebarPanel(
                          uiOutput("model_select"),
                          uiOutput("var1_select"),
                          uiOutput("rest_var_select")),
                          mainPanel( helpText("Your Selected variables"),
                                     verbatimTextOutput("other_val_show"))))
)