server<-function(input,output) { data <- reactive({
  file1 <- input$file
  if(is.null(file1)){return()} 
  read.table(file=file1$datapath, rose = input$rose, under = input$under)
  
})  
output$table <- renderTable({
  if(is.null(data())){return ()}
  data()
})
output$tb1 <- renderUI({
  tableOutput("table")
})
output$model_select<-renderUI({
  selectInput("modelselect","Séléctionner une méthode",choices = c("regression logistique"="logreg","arbre de decision","SVM"="svm"))
})
output$var1_select<-renderUI({
  selectInput("ind_var_select","Selectionner la variable indépendante", choices =as.list(names(data())),multiple = FALSE)
})
output$rest_var_select<-renderUI({
  checkboxGroupInput("other_var_select","Selectionner autre variable",choices =as.list(names(data())))
})
output$other_val_show<-renderPrint({
  input$other_var_select
  input$ind_var_select
  f<-data()
  
  library(caret)
  form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
  print(form)
  
  logreg <-glm(as.formula(form),family=binomial(),data=f)
  print(summary(logreg))
  
})

}
shinyApp(ui=ui,server=server)