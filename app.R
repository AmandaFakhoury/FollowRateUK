#Application allowing us to choose a publish year and publish month 
#Outputs a graph showing the number of members who are clicking on amazon UK
#Outputs Median number of members who are leaving a feedback Review 

library(shiny)
library(datasets)
library(tidyverse)
library(plotly)
library(memoise)

#Defining Lists needed for choices 

#List of Years----
years <- list("2018",
              "2019")

#list of Months----
months <- list("1",
               "2",
               "3",
               "4",
               "5",
               "6",
               "7",
               "8",
               "9",
               "10",
               "11",
               "12")

#Defining Global Server 

#Creating a function that subsets the data based on choices 
getdataframe <- memoise(function(year , month){
  
  df <- read.csv("pub_review_link.csv") #reading in dataframe 

  if(!(year %in% years))
    stop("Unknown year")
  
  if(!(month %in% months))
    stop("Unknown month")
  
  #get data 
  df[df$year == as.numeric(year) & df$month == as.numeric(month) , ]
  
})


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Follow Through Rate for members in UK who click on Amazon UK"),
  
  # Sidebar with a slider input for number of bins 
  #Sidebar layout with input and ouput definitions----
  sidebarLayout(
    
    #Sidebar panel for inputs----
    sidebarPanel(
      
      #Input: select a price range----
      selectInput("years","Choose a year:",
                  choices = years), 
      
      selectInput("months", "Choose a month:",
                  choices = months), 
      
      helpText("Note: This Application allows you to choose a year as well as month
               and outputs a plot showing Daily Median Feedback submitted as well as 
               a graph of the daily number of members who click on amazon uk
               Keep in Mind: These are members living in the UK 
               Books that were published between Jan 2018 through Feb 28 2019
               Another point some months will output only the number of rows which will be 0
               This means no members clicked on the link during that month
               Also for April 2018 there's only April 27 so the graph won't show much!"), 
      actionButton("update", "Change"), 
      hr()
      ), 
    
    # Show a plot of the generated distribution
    mainPanel(
      
      textOutput("B1"),

      h4("Daily Number of members clicking on amazon UK"), 
      plotlyOutput("P1"), 
      
      h4("Daily Median Feedback Submitted"), 
      plotlyOutput("P2")
      
      )
    )

)




# Define server logic required to draw a histogram
server <- function(input,output , session){
  
  dat <- reactive({
    
    input$update
    
    isolate({
      withProgress({
        setProgress(message = "Processing info...")
        getdataframe(input$years,
                     input$months)
        
      })
    })
    
  }) 
  
  output$B1 <- renderText({
    newf <- dat()
    paste("Number of rows in dataset:", nrow(newf))
  })
  
  
  output$P1<-renderPlotly({
    newd <- dat() 
    data <- newd %>%
      group_by(day) %>%
      count(type)
    
    p <- ggplot(data = data, aes(x= data$day, y = data$n)) + geom_line(colour = 'blue')
    gg <- ggplotly(p)
    layout(gg, dragmode = "pan")})
  
  
  
  output$P2<-renderPlotly({
    newd <- dat() 
    data <- newd %>%
      group_by(day) %>%
      summarise(feedback_submitted= median(feedback_submitted)) 
    
    p <- ggplot(data = data, aes(x= data$day, y = data$feedback_submitted)) + geom_line(colour = 'green')
    gg <- ggplotly(p)
    layout(gg, dragmode = "pan")})}



# Run the application 
shinyApp(ui = ui, server = server)

