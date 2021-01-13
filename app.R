library(shiny)
library(datasets)

# Define UI for dataset viewer application
 ui <- fluidPage(
  
  # Application title.
  titlePanel("Demonstration of actionButton() and isolate"),
  
  sidebarPanel(
    # selectInput widget for the selection of dataset
    selectInput("dataset", "Choose a dataset:", 
                choices = c("iris", "pressure", "mtcars")),
    
    # numericInput for selection of the number of observation that user wants to view
    numericInput("obs", "Number of observations:", 6),
    br(),
    
    
    
    p("In this example, changing the dataset will update the structure in the main panel. However, would not update the observation until the Update button is pressed. Basically by using the actionButton along with isolate(), we can control the reactiveness of user inputs at Server side"),
    br(),
    # actionButton to create dependency of the reactivity on the event of pressing of the button. Works along with isolate() in the server side
    actionButton("act", "Click to update/view the selected dataset!"),
    br(),
    p("To partially control the reactiveness at the server side and create dependency of reactivity on the event of pressing the button, isolate() is used in the server.r along with actionButton() in the ui.r")
  ),
  
  mainPanel(
    # just a header for the heading
    h4(textOutput("dataname")),
    # display the structure of the selected dataset
    verbatimTextOutput("structure"),
    
    # just a header for the heading
    h4(textOutput("observation")),
    # display the observations of the selected dataset - note that this is driven by the triggering of the button event - check in server.r
    tableOutput("view")
  )
)
 
 server <- function(input, output) {
   
   # to print the heading in the main panel for structure of the data set
   output$dataname <- renderText({
     paste("Structure of the dataset", input$dataset)
     
   })
   
   
   # Return  the dataset structure. We have used the str() function to get the structure of the dataset
   output$structure <- renderPrint({
     # str(datasetInput()) # this reactive function would show its effect only after the action button is pressed
     str(get(input$dataset))
   })
   
   # to print the heading in the main panel for observation of the data set
   output$observation <- renderText({
     input$act
     # Uncomment the below conditional statement if you do not want to display the observation relatd statement when the page loads the first time
     
     #         if(input$act==0)
     #           return()
     #         else
     isolate(paste("First", input$obs, "observations of the dataset", input$dataset))
     # This is the first isolated function controlled by the same actionButton.
     
   })
   
   
   
   # for dataset observations -  "n" observations as defined by user's input. The value of number of observation will come from the input$obs
   output$view <- renderTable({
     input$act
     #     # act contains the initial value as zero and increments as the button is pressed
     # uncomment the below conditional statements if you wish not to display the dataset observations when the page loads first time
     
     #      if(input$act==0)
     #        return()
     #      else
     isolate(head(get(input$dataset), n = input$obs))
     # This is the second isolated function controlled by the same actionButton.
     # isolate is used along with the action button as an event handler too.
     
     
   })
 }
 
 shinyApp(server = server, ui = ui)