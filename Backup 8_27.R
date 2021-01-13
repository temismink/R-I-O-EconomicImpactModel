library(shinythemes)
library(shiny)
library(matlib)
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
ui <- fluidPage(theme = shinytheme("united"),
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
                    actionButton("start","Generate"),
                    actionButton("update","Update"),
                    downloadButton('downloadData', 'Download'),
                    #renderRHandsontable("Stats"),
                    checkboxInput("somevalue", "RPC", F),
                    textOutput("helper")
                    
                  ),
                  mainPanel(# tableOutput('table')
                  rHandsontableOutput("tbl")
                  #textOutput("observation")
                    )
                ))

server <- function(input, output){
  
  #Variables defined here are global variables.
  #inputVals <- eventReactive(input$action, {runif(input$action)})
  
  pA<-0
  CDF<-0
  M<-0
  #Initializing global variables below. 
  #defSomeVal is used to determine whether somevalue RPC checkbox has changed. False because that's the checkbox default value. 
  defsomevalue<-F
  totalEffects<-0
  directEffects<-0
  taxaccounts <- 0
  main1 <- 0
  
  df_superSector2 <- as.data.frame(matrix(0, nrow = 14, ncol = 4))
  df_effectsDistribution <- as.data.frame(matrix(0, nrow = 5, ncol = 4))
  compensationtbl <- as.data.frame(matrix(0, nrow = 7, ncol = 4))
  taxaccounts <- as.data.frame(matrix(0, nrow = 5, ncol = 4))
  EPMIE <- as.data.frame(matrix(0, nrow = 5, ncol = 4))
  df_3digit <- as.data.frame(matrix(0, nrow = 76, ncol = 4))
  #src <- getSrcDirectory(function(x) {x})
  #To write RPC and other information into a downloadable file. 
  file = file.path("Workbook2.xlsx")
  wb2<<-loadWorkbook(file = file.path("Workbook2.xlsx"))

  
  df_old <- as.data.frame( matrix(0, nrow = 390, ncol = 4))
  df_tax2 <- as.data.frame( matrix(0, nrow = 390, ncol = 3))
  
  df_gen <- reactive({
    
    if (is.null(input$tbl))  {
      df_new <- as.data.frame(matrix(0, nrow = 390, ncol = 4)) #What about DF?
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
    if (any(df_new[,2]!= df_old[,2]))
    {
      #   #This is a value-by-value operation, NOT a matrix operation. Both matrices must be of the same size.
      for (i in 1:dim(df_new)[1])
      {
        if(df_new[i,2]!=df_old[i,2]){
          df_new[i, 3] <- round(df_new[i, 2] / CDF[i,2],digits=2) 
          df_new[i, 1] <- round(df_new[i, 3] * CDF[i,1],digits=2) 
          
        }}
      df_old[,2]<-df_new[,2]
    } 
    
    if ( any(df_new[,1]!= df_old[,1]))
    {
      for (i in 1:dim(df_new)[1])
      {
        if(df_new[i,1]!=df_old[i,1]){
          #This is a value-by-value operation, NOT a matrix operation. Both matrices must be of the same size.
          df_new[i, 3] <- round(df_new[i, 1] / CDF[i, 1], digits=2) #Finding Output
          df_new[i, 2] <-round( df_new[i, 3] * CDF[i, 2], digits=2) #Finding Earnings
    
        }}
      df_old[,1]<-df_new[,1]
    }
    
    if ( any(df_new[,3]!= df_old[,3]))
    {
      for (i in 1:dim(df_new)[1])
      {
        if(df_new[i,3]!=df_old[i,3]){
          df_new[i, 2] <- round(df_new[i, 3] * CDF[i, 2], digits=2) #Finding Earnings
          df_new[i, 1] <- round(df_new[i, 3] * CDF[i, 1], digits=2) #Finding Jobs
          
        }}
      df_old[,3]<-df_new[,3]
    }
    
    if(input$somevalue != defsomevalue){
      
      if(input$somevalue==F){
        df_new[,4]<-0
        }
      else{
        df_new[,4]<-1}
      defsomevalue<<-input$somevalue
    }
    
    return(df_new)
    
  })
               
  output$tbl <- renderRHandsontable({
    input$action
    wb<<-loadWorkbook(file = file.path("NJ2015.xlsx"))
    CDF <<-read.xlsx(wb,sheet = 8,startRow = 1,cols = c(24, 25, 27),skipEmptyRows = F) 
    Tax <<-read.xlsx(wb,sheet = 8,startRow = 1,cols = c(28, 29, 30),skipEmptyRows = F)
    
    CDF[is.na(CDF)] <<- 0
    
    CDF[1] <<- as.numeric(unlist(CDF[1])) 
    
    CDF[2] <<- as.numeric(unlist(CDF[2]))  
    
    CDF[3] <<- as.numeric(unlist(CDF[3]))
    
    Tax[1] <<- as.numeric(unlist(Tax[1]))
    
    Tax[2] <<- as.numeric(unlist(Tax[2]))
    
    Tax[3] <<- as.numeric(unlist(Tax[3]))
    
    CDF[CDF==0]<<-1
    
    category<- read.xlsx(wb, sheet=3,startRow=1,cols=2)
    RPC<<-read.xlsx(wb, sheet=8,startRow=1, cols=37) #389
  
    data1 <- eventReactive(input$start,{
      df_new2 <<- comparison()
      RPC_selected<<-(df_new2[,4]==1)
      
      RPC[(!RPC_selected),]<-1 
      directEffectsOutput<<-RPC*as.matrix((df_new2[,3])) #Output
      
      directEffectsEarnings<<-as.data.frame(RPC*(df_new2[,2])) #Earnings
      
      directEffectsJobs<<-RPC*df_new2[,1] #Jobs
      df_new2<<-cbind(as.matrix(category)[,1],df_new2)
      df_old <<- df_new2[,-1]
      colnames(df_new2) <<- c("Categories", "Jobs ", " Earnings", "Output", paste("RPC\n Y=1, N=0"))
    })
    
    data2 <- eventReactive(input$update,{
      df_new2 <<- comparison()
      RPC_selected<<-(df_new2[,4]==1)
      
      RPC[(!RPC_selected),]<-1 
      directEffectsOutput<<-RPC*as.matrix((df_new2[,3])) #Output
      
      directEffectsEarnings<<-as.data.frame(RPC*(df_new2[,2])) #Earnings
      
      directEffectsJobs<<-RPC*df_new2[,1] #Jobs
      
      df_new2<<-cbind(as.matrix(category)[,1],df_new2)
      df_old <<- df_new2[,-1]
    })
    
    if(input$update){
      data2()
      rhandsontable(df_new2)
    }
    if(input$start){
      data1()
      rhandsontable(df_new2)
    }
    
  })

  df_gen1 <- reactive({
    
    NAICS<<-read.xlsx(wb,sheet = 3,startRow = 3,cols = 3,skipEmptyRows = F) 
    
    valueAddedPerOutput<<-read.xlsx(wb, sheet=8,startRow=1,cols=27) 
    
    #valueAdded<-RPC*df_new2[,4]
    
    colnames(directEffectsJobs)<-'Direct Effects Jobs'
    
    colnames(directEffectsEarnings)<-'Direct Effects Earnings'
    
    colnames(directEffectsOutput)<-'Direct Effects Output'
    
    #colnames(valueAdded)<-'Value Added Per Output'
    
    directEffects<<- cbind(directEffectsOutput,directEffectsEarnings,directEffectsJobs)
    
    #valueAdded<-read.xlsx(wb, sheet=1,startRow=2,cols=5) #For later
    #Making the A matrix
    A<-read.xlsx(wb, sheet=14,startRow=2,cols = c(4:393))
    
    #Modifying the A matrix to be a square matrix without description, NAICS, RIO
    #Creating the identity matrix
    
    I<-diag(dim(A)[1])
    pA<<-sweep(as.matrix(A),1,as.matrix(RPC),"*")
    
    #Sweeps don't work on data frames. M stands for multiplier. 
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1CHANGED I-pA TO I-A!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    M <- solve(I-pA)
    
    totalEffectsOutput <<- as.matrix(M) %*% as.matrix(directEffectsOutput)
    totalEffectsJobs <<- c(totalEffectsOutput) * (CDF[1])
    totalEffectsJobs <<- rbind(totalEffectsJobs,0)
    totalEffectsEarnings <<- c(totalEffectsOutput) * (CDF[2])
    totalEffectsEarnings <<- rbind(totalEffectsEarnings,0)
    GDP <<- c(totalEffectsOutput) * (CDF[3])
    GDP <<- rbind(GDP,0)
    
    totalEffects <- cbind(totalEffectsOutput,totalEffectsEarnings,totalEffectsJobs,GDP)
    
    colnames(totalEffects) <- c("totalOutput","totalEarnings","totalJobs","GDP")
    ditto <<- cbind(category,NAICS,df_new2)
    directEffectsSheet<<-cbind(category,NAICS, totalEffects)
    
    return (directEffectsSheet)
  })
  df_gen2 <- reactive({
    colnames(df_tax2) <- c("Federal", "State", "Local")
    
    #Tax
    df_tax2[1] <<- totalEffectsOutput * Tax[1]
    df_tax2[2] <<- totalEffectsOutput * Tax[2]
    df_tax2[3] <<- totalEffectsOutput * Tax[3]#Not sure if right
    
    taxSheet <<- cbind(category, NAICS, df_tax2)
    return (taxSheet)
  })
  df_gen3 <- reactive({
    print("sauce")
    df_tax2[1] <<- totalEffectsOutput * Tax[1]
    df_tax2[2] <<- totalEffectsOutput * Tax[2]
    df_tax2[3] <<- totalEffectsOutput * Tax[3]
    
    colnames(df_superSector2) <- c("Output", "Earnings", "Employment", "GDP")
    #Aggregate
    aggregate <- function(matrixFit, col, df){
      df_superSector <- df
      df_superSector[1,col]  <- sum(matrixFit[1:13,])
      df_superSector[2,col]  <- sum(matrixFit[14:21,])
      df_superSector[3,col]  <- sum(matrixFit[22:24,])
      df_superSector[4,col]  <- sum(matrixFit[25:36,])
      df_superSector[5,col]  <- sum(matrixFit[37:274,])
      df_superSector[6,col]  <- sum(matrixFit[275,])
      df_superSector[7,col]  <- sum(matrixFit[276:279,])
      df_superSector[8,col]  <- sum(matrixFit[280:288,])
      df_superSector[9,col]  <- sum(matrixFit[289:303,])
      df_superSector[10,col] <- sum(matrixFit[304:316,])
      df_superSector[11,col] <- sum(matrixFit[317:340,])
      df_superSector[12,col] <- sum(matrixFit[341:356,])
      df_superSector[13,col] <- sum(matrixFit[357:368,])
      df_superSector[14,col] <- sum(matrixFit[369:390,])
      
      return (df_superSector)
    }
    
    #First Table
    df_superSector2 <- aggregate((GDP),4, (aggregate((totalEffectsJobs),3,(aggregate(((totalEffectsEarnings)),2,(aggregate((totalEffectsOutput),1,df_superSector2)))))))
    totals <- c(sum(df_superSector2[1]), sum(df_superSector2[2]), sum(df_superSector2[3]), sum(df_superSector2[4]))
    df_superSector2 <- rbind(df_superSector2, totals)
    colnames(df_superSector2) <- c("Output", "Earnings", "Employment", "GDP")
    
    labels1<-read.xlsx(wb,sheet = 1,startRow = 1, rows = 4:18, cols = 2,skipEmptyRows = F) 
    
    #Second Table
    temp <- rbind(CDF[3],0)
    totalDirects <- c(sum(df_new2[,4]),sum(df_new2[,3]),sum(df_new2[,2]),sum(directEffectsOutput*temp))
    indirect <- totals - totalDirects
    multipliers <- totals/totalDirects
    df_effectsDistribution <- rbind(totalDirects, indirect, totals, multipliers)
    colnames(df_effectsDistribution) <- c("Output", "Earnings", "Employment", "GDP")
    
    #Third Table
    comp <-read.xlsx(wb,sheet = 8,startRow = 1,cols = c(26),skipEmptyRows = F) 
    compensation <- sum(totalEffectsOutput*(comp))
    fed <- sum(df_tax2[1])
    sta <- sum(df_tax2[2])
    loc <- sum(df_tax2[3])
    tottax <- fed + sta + loc
    PDR <- sum(df_superSector2[4])-(compensation + tottax)
    compensationtbl[4] <- unlist(c(compensation, tottax, fed, sta, loc, PDR, sum(df_superSector2[4])))
    colnames(compensationtbl) <- c("Output", "Earnings", "Employment", "GDP")
    
    #Fourth Table
    HLI <- round(.9 * sum(df_superSector2[3]),2)
    H_loc <- round(loc * sum(Tax[3]),2)
    H_sta <- round(sta * sum(Tax[2]),2)
    H_fed <- round(fed * sum(Tax[1]),2)
    H_tottax <- H_loc + H_sta + H_fed
    household <- c(HLI, H_tottax, H_loc, H_sta, H_fed)
    business <- c(compensation, tottax, loc, sta, fed)
    total <- c((HLI + compensation),(H_tottax+tottax),(loc + H_loc),(sta + H_sta),(fed + H_fed))
    taxaccounts[2] <- unlist(household)
    taxaccounts[3] <- unlist(business)
    taxaccounts[4] <- unlist(total)
    colnames(taxaccounts) <- c("Output", "Earnings", "Employment", "GDP")
    
    Init_Expend <- 1000 * sum(df_new2[,3])
    
    #Fifth Table
    EmPM <- round((Init_Expend/1000000) * sum(df_superSector2[2]),2)
    EPM  <- round((sum(df_superSector2[3])*1000)/(Init_Expend/1000000),2)
    STPM <- round(((H_sta + sta) * 1000) / (Init_Expend/1000000),2)
    LTPM <- round(((H_loc + loc) * 1000) / (Init_Expend/1000000),2)
    GDPPM <- round((sum(df_superSector2[4]) * 1000) / (Init_Expend/1000000),2)
    EPMIE[4] <- unlist(c(EmPM,EPM, STPM, LTPM, GDPPM))
    colnames(EPMIE) <- c("Output", "Earnings", "Employment", "GDP")
    #print(EPMIE)
    
    hashbind <- function(..., hash = " ") {
      lst <- list(...)
      Nchar <- max(rapply(lst, function(y) nchar(as.character(y)))) + 2
      do.call(
        rbind, 
        lapply(lst, function(x) {
          rbind(x, substr(paste(rep(hash, Nchar), collapse = ""), 1, Nchar))
        }))
    }    
    df_effectsDistribution <- rbind(c(" "),df_effectsDistribution)
    compensationtbl <- rbind(c(" "),compensationtbl)
    taxaccounts <- rbind(c(" "),taxaccounts)
    EPMIE <- rbind(c(" "),EPMIE)
    
    main4 <<- hashbind(df_superSector2,df_effectsDistribution,compensationtbl, taxaccounts, EPMIE)
    names <- c("Agriculture,Forestry,Fishing,and.Hunting", "Mining", "Utilities", "Construction", "Manufacturing", "Wholesale Trade","Retail Trade", "Transportation and Warehousing","Information","Finance, Insurance, Real Estate, Rental, and Leasing","Professional and Business Services" ,"Educational Services, Health Care, and Social Assistance", "Arts, Entertainment, Recreation, and Hospitality","Other Services (including Government)","Total Effects","  ","Distribution of Effects and Multipliers", "Direct Effects", "Indirect/Induced Effects", "Total Effects", "Multipliers(= 4 / 1)", " ","Composition of GDP", "Compensation", "Taxes", "a. Local", "b. State", "c. Federal", "Profits, Dividends, Rents, and Other", "Total GDP", " ", "Tax Accounts                   Business  Local  Total", "Labor Income", "Taxes", "a. Local", "b. State", "c. Federal", " ","Effects per Million Dollars of Initial Expenditure(in Dollars)", "Employment/Jobs", "Earnings", "State Taxes", "Local Taxes", "GDP", " ")
    main4 <<- cbind(names, main4)
    return (main4)
  })
  df_gen4 <- reactive({
    aggregate2 <- function(matrixFit, col, df){
      
      df_superSector <- df
      df_superSector[2,col]  <- sum(matrixFit[1:10,])
      df_superSector[3,col]  <- sum(matrixFit[11:13,])
      df_superSector[5,col]  <- sum(matrixFit[14,])
      df_superSector[6,col]  <- sum(matrixFit[15:19,])
      df_superSector[7,col]  <- sum(matrixFit[20:21,])
      df_superSector[8,col]  <- sum(matrixFit[22:24,])
      df_superSector[9,col]  <- sum(matrixFit[25:36,])
      df_superSector[10,col]  <-sum(matrixFit[37:273,])
      df_superSector[11,col]  <- sum(matrixFit[195:223,])
      df_superSector[12,col]  <- sum(matrixFit[224:229,])
      df_superSector[13,col]  <- sum(matrixFit[230:231,])
      df_superSector[14,col]  <- sum(matrixFit[37:40,])
      df_superSector[15,col]  <- sum(matrixFit[232:239,])
      df_superSector[16,col] <- sum(matrixFit[240:241,])
      df_superSector[17,col] <- sum(matrixFit[242:245,])
      df_superSector[18,col] <- sum(matrixFit[246:264,])
      df_superSector[19,col] <- sum(matrixFit[264:274,])
      df_superSector[20,col] <- sum(matrixFit[41:52,])
      df_superSector[21,col] <- sum(matrixFit[53:63,])
      df_superSector[22,col] <- sum(matrixFit[64:83,])
      df_superSector[23,col] <- sum(matrixFit[84:113,])
      df_superSector[24,col] <- sum(matrixFit[114:133,])
      df_superSector[25,col] <- sum(matrixFit[134:150,])
      df_superSector[26,col] <- sum(matrixFit[151:164,])
      df_superSector[27,col] <- sum(matrixFit[165:175,])
      df_superSector[28,col] <- sum(matrixFit[176:183,])
      df_superSector[29,col] <- sum(matrixFit[184:194,])
      df_superSector[30,col] <- sum(matrixFit[275,])
      df_superSector[31,col] <- sum(matrixFit[276:279,])
      df_superSector[33,col] <- sum(matrixFit[280,])
      df_superSector[34,col] <- sum(matrixFit[281,])
      df_superSector[35,col] <- sum(matrixFit[282,])
      df_superSector[36,col] <- sum(matrixFit[283,])
      df_superSector[37,col] <- sum(matrixFit[284,])
      df_superSector[38,col] <- sum(matrixFit[285,])
      df_superSector[39,col] <- sum(matrixFit[286:287,])
      df_superSector[40,col] <- sum(matrixFit[288,])
      df_superSector[42,col] <- sum(matrixFit[289:293,])
      df_superSector[43,col] <- sum(matrixFit[294:295,])
      df_superSector[44,col] <- sum(matrixFit[296:297,])
      df_superSector[45,col] <- sum(matrixFit[298:303,])
      df_superSector[47,col] <- sum(matrixFit[304:305,])
      df_superSector[48,col] <- sum(matrixFit[305:307,])
      df_superSector[49,col] <- sum(matrixFit[308:309,])
      df_superSector[50,col] <- sum(matrixFit[310,])
      df_superSector[52,col] <- sum(matrixFit[311:312,])
      df_superSector[53,col] <- sum(matrixFit[313:316,])
      df_superSector[55,col] <- sum(matrixFit[317,])
      df_superSector[56,col] <- sum(matrixFit[321:330,])
      df_superSector[57,col] <- sum(matrixFit[318:320,])
      df_superSector[58,col] <- sum(matrixFit[331,])
      df_superSector[60,col] <- sum(matrixFit[332:339,])
      df_superSector[61,col] <- sum(matrixFit[340,])
      df_superSector[62,col] <- sum(matrixFit[341:343,])
      df_superSector[64,col] <- sum(matrixFit[344:350,])
      df_superSector[65,col] <- sum(matrixFit[351:353,])
      df_superSector[66,col] <- sum(matrixFit[354:356,])
      df_superSector[68,col] <- sum(matrixFit[357:361,])
      df_superSector[69,col] <- sum(matrixFit[362:364,])
      df_superSector[71,col] <- sum(matrixFit[365,])
      df_superSector[72,col] <- sum(matrixFit[366:368,])
      df_superSector[73,col] <- sum(matrixFit[369:380,])
      df_superSector[74,col] <- sum(matrixFit[381:389,])
      df_superSector[75,col] <- sum(matrixFit[390,])
      df_superSector[76,col] <- sum(matrixFit[1:390,]) #Households?
      
      df_superSector[1,col] <- sum(df_superSector[2:3,col])
      df_superSector[4,col] <- sum(df_superSector[5:7,col])
      df_superSector[32,col] <-sum(df_superSector[33:40,col])
      df_superSector[41,col] <-sum(df_superSector[42:45,col])
      df_superSector[46,col] <-sum(df_superSector[47:49,col])
      df_superSector[51,col] <-sum(df_superSector[52:53,col])
      df_superSector[54,col] <-sum(df_superSector[55:58,col])
      df_superSector[59,col] <-sum(df_superSector[60:61,col])
      df_superSector[63,col] <-sum(df_superSector[64:66,col])
      df_superSector[67,col] <-sum(df_superSector[68:69,col])
      df_superSector[70,col] <-sum(df_superSector[71:72,col])
      
      return (df_superSector)
    }
    
    names2<-read.xlsx(wb, sheet=2,startRow=1, cols=2)
    df_3digit <- aggregate2((GDP),4, (aggregate2((totalEffectsJobs),3,(aggregate2(((totalEffectsEarnings)),2,(aggregate2((totalEffectsOutput),1,df_3digit)))))))
    colnames(df_3digit) <- c("Output", "Earnings", "Employment", "GDP")
    df_3digit <- cbind(names2,df_3digit)
    return (df_3digit)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      list_of_datasets <- list("Super Sectors" = (df_gen3()),"3-digit Industry" = df_gen4(), "Detailed Industry" = df_gen1(),"Etc." = ditto, "Taxes" = df_gen2())
      write.xlsx(list_of_datasets, file)
    })
}
#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui = ui, server = server)

