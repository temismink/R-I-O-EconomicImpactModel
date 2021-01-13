library(shinythemes)
library(shiny)
library(rhandsontable)
library(openxlsx) #used for writing data into excel xlsxFilfiles
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Line does not work below!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
#source("states.R")
getStates<- function(){ 
  c("AL",
    "AK",
    "AZ",
    "AR",
    "CA",
    "CO",
    "CT",
    "DE",
    "FL",
    "GA",
    "HI",
    "ID",
    "IL",
    "IN",
    "IA",
    "KS",
    "KY",
    "LA",
    "ME",
    "MD",
    "MA",
    "MI",
    "MN",
    "MS",
    "MO",
    "MT",
    "NE",
    "NV",
    "NH",
    "NJ",
    "NM",
    "NY",
    "NC",
    "ND",
    "OH",
    "OK",
    "OR",
    "PA",
    "RI",
    "SC",
    "SD",
    "TN",
    "TX",
    "UT",
    "VT",
    "VA",
    "WA",
    "WV",
    "WI",
    "WY");}





#Where we will get data from.



states <- getStates()
ui <- fluidPage(theme = shinytheme("slate"),
                titlePanel(
                  HTML(
                    "<h1> <font face=\"Rockwell Extra Bold\" color=\"#b42000\"><b><b>R/Econ</b></b></font> <font face=\"Lucida Calligraphy\" color=\"white\" >Model</font></h1>"
                  )
                ),
                sidebarLayout(
                  sidebarPanel(
                    #Get the font to be slightly bigger below:
                    h4(
                      p(
                        "After selecting a state, please click on ",
                        strong(span("Generate", style = "font-family: 'times'; font-si8pt ")),
                        "in order to create an input table.",
                        style = "font-family: 'times'; font-si25pt "
                      )#,
                      #Drop-down menu
                      #  selectInput(
                      #   inputId = "state",
                      #    label = "",
                      #    choices = states,
                      #    selected = NULL,
                      #    multiple = FALSE,
                      ##    selectize = TRUE,
                      #    width = NULL,
                      #    size = NULL
                      #  )
                    ),
                    selectInput(inputId="states",label="", choices=states, selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                    downloadButton('downloadData', 'Download'),
                    # downloadButton('downloadData2', 'Download'),
                    checkboxInput("somevalue", "RPC", F),
                    textOutput("helper")
                    
                  ),
                  mainPanel(# tableOutput('table')
                    rHandsontableOutput("tbl"))
                ))
server <- function(input, output) {
  #Variables defined here are global variables. 
  pA<-0
  CDF<-0
  M<-0
  #Initializing global variables below. 
  directEffectsSheet<-0
  df_new2<-0
  #defSomeVal is used to determine whether somevalue RPC checkbox has changed. False because that's the checkbox default value. 
  defsomevalue<-F
  
  directEffects<-0
  
  src <- getSrcDirectory(function(x) {x})
  
  #To write RPC and other information into a downloadable file. 
  wb2 <<-loadWorkbook(file = file.path(src,"www","Book1.xlsx") )
  
  
  
  df_old <- as.data.frame( matrix(1, nrow = 381, ncol = 4))
  
  df_gen <- reactive({
    
    if (is.null(input$tbl))  {
      df_new <- as.data.frame(matrix(1, nrow = 381, ncol = 4)) #What about DF?
      return (df_new)
    }  
    else{
      return(hot_to_r(input$tbl))
    }})
  comparison <- reactive({
    df_new<-df_gen()
    if (dim(df_new)[2]>4)
      df_new<-df_new[,-1]
    if (all(df_old == df_new))
    {
      
    }
    #In this case, "Jobs" is given. "Earnings" and "Output" must be calculated.
    else if (any(df_new[,2]!= df_old[,2]))
    {
      #   #This is a value-by-value operation, NOT a matrix operation. Both matrices must be of the same size.
      for (i in 1:dim(df_new)[1])
      {
        if(df_new[i,2]!=df_old[i,2]){
          df_new[i, 3] <- round(df_new[i, 2] / CDF[i,2],digits=2)
          df_new[i, 1] <- round(df_new[i, 3] *CDF[i,1],digits=2)
        }}
      df_old<-df_new
      
      
    } 
    else if ( any(df_new[, 1]!= df_old[,1]))
    {
      for (i in 1:dim(df_new)[1])
      {
        if(df_new[i,1]!=df_old[i,1]){
          #This is a value-by-value operation, NOT a matrix operation. Both matrices must be of the same size.
          df_new[i, 3] <- round(df_new[i, 1] / CDF[i, 1], digits=2) #Finding Output
          df_new[i, 2] <-round( df_new[i, 3] * CDF[i, 2], digits=2) #Finding Earnings
        }}
      
       
      
      df_old<-df_new
      
    }
    else 
    {
      for (i in 1:dim(df_new)[1])
      {
        if(df_new[i,3]!=df_old[i,3]){
          df_new[i, 2] <- round(df_new[i, 3] / CDF[i, 2], digits=2) #Finding Earnings
          df_new[i, 1] <- round(df_new[i, 3] *CDF[i, 1], digits=2) #Finding Jobs
          df_old<-df_new
          
        }}
    }
 
    if(input$somevalue != defsomevalue){
      
      if(input$somevalue==F){
        df_new[,4]<-0
        }
      else{
        df_new[,4]<-1}
      defsomevalue<<-input$somevalue
    }
      
    #  

    
    return(df_new)
    
  })
  
  output$tbl <- renderRHandsontable({
    
    
  
    
    CDF <<-read.xlsx(wb,sheet = 1,startRow = 1,cols = c(2, 3),skipEmptyRows = F) 
    
    CDF[is.na(CDF)] <<- 0
    # CDF<- matrix(7, nrow=381, ncol=3)
    NAICS<-read.xlsx(wb,sheet = 1,startRow = 1,cols = 8,skipEmptyRows = F) 
    
    
    CDF[1] <<- as.numeric(unlist(CDF[1]))
    
    CDF[2] <<- as.numeric(unlist(CDF[2]))  
    
    CDF[CDF==0]<<-1
    df_new2 <<- comparison()
    
    wb<-loadWorkbook(file = file.path(src, "www", "State_Vectors", paste(input$states,".xlsx",sep="")))
    
    #valueAddedPerOutput
    valueAddedPerOutput<-read.xlsx(wb, sheet=1,startRow=2,cols=5) 
    
    category<- read.xlsx(wb, sheet=1,startRow=1,cols=9)
    
    RPC<-read.xlsx(wb, sheet=1,cols=11)
    
    RPC_selected<-(df_new2[,4]==1)
    
    RPC[(!RPC_selected),]<-1 
    
    directEffectsOutput<-RPC*as.matrix((df_new2[,3])) #Output
    
    directEffectsEarnings<-as.data.frame(RPC*(df_new2[,2])) #Earnings
    
    directEffectsJobs<-RPC*df_new2[,1] #Jobs
    
    colnames(directEffectsJobs)<-'Direct Effects Jobs'
    
    colnames(directEffectsEarnings)<-'Direct Effects Earnings'
    
    colnames(directEffectsOutput)<-'Direct Effects Output'
    
    directEffects<<- cbind(directEffectsOutput,directEffectsEarnings,directEffectsJobs)
    
    df_new2<<-cbind(as.matrix(category)[,1],df_new2)
    
    df_old <<- df_new2[,-1]
    
    #valueAdded<-read.xlsx(wb, sheet=1,startRow=2,cols=5) #For later
    #Making the A matrix
    A<-read.xlsx(wb, sheet=2,startRow=1)
    #Modifying the A matrix to be a square matrix without description, NAICS, RIO
    A<-A[,-c(382:387)]
    #Creating the identity matrix
    I<-diag(dim(A)[1])
    
    A<-read.xlsx(wb, sheet=2,startRow=1)
    
    A<-A[,-c(382:387)]
    
    I<-diag(dim(A)[1])
    
    pA<<-sweep(as.matrix(A),1,as.matrix(RPC),"*")
    
    #Sweeps don't work on data frames. M stands for multiplier. 
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1CHANGED I-pA TO I-A!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    M<<- (I-pA)
    
    totalEffectsJobs<-sweep(M,1,as.matrix(directEffectsOutput),"*")
    
    totalEffectsOutput<-sweep(M,1,as.matrix(directEffectsEarnings),"*")
   
    totalEffectsEarnings=sweep(M,1,as.matrix(directEffectsJobs),"*")

    colnames(df_new2) <- c("Category","Jobs "," Earnings", "Output", paste("RPC\n Y=1, N=0"))
    
    directEffectsSheet<<-cbind(category,NAICS,df_new2,directEffects)
    
    rhandsontable(df_new2)
    
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      list_of_datasets <- list("Direct Effects" = directEffectsSheet, "I-pA"=M, "pA"=pA)
      write.xlsx(list_of_datasets, file)
      
    }
  )
}
runApp(shinyApp(ui, server), launch.browser = TRUE)
#shinyApp(ui = ui, server = server)



