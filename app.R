#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Investment Scenarios"),
   
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
       column(4, 
         sliderInput("amount",
                     "Initial Amount",
                     min = 0,
                     max = 1000000,
                     value = 1000,
                     step = 500,
                     pre = '$'),
        sliderInput("contribution",
                   "Annual Contribution",
                   min = 0,
                   max = 50000,
                   value = 2000,
                   step = 500,
                   pre = '$')),
     
     column(4, offset = 0,
        sliderInput("return",
                   "Return Rate (in %)",
                   min = 0,
                   max = 20,
                   post = '%',
                   value = 5,
                   step = 0.1),
        sliderInput("growth",
                    "Growth Rate (in %)",
                    min = 0,
                    max = 20,
                    post = '%',
                    value = 2,
                    step = 0.1)),
     
     column(4, offset = 0,
        sliderInput("years",
                    "Years",
                    min = 0,
                    max = 50,
                    step = 1,
                    value = 20),
        selectInput("facet",
                    "Facet?",
                    c("No" = FALSE,
                      "Yes" = TRUE)
                    ))
   ),
      # Show a plot of the generated distribution
      mainPanel(
        h4("Timelines"),
        plotOutput("plot"),
        
        h4("Balances"),
        verbatimTextOutput('balances')
      )
   )

#no_contrib function
#'@title future value 
#'@description calculates the future value of an investment with an annual rate of return
#'@param amount amount of initial investment
#'@param rate annual rate of return as a decimal
#'@param years number of years of the investment
#'@return calculated value of the investment after a set number of years

future_value <- function(amount, rate, years) {
  x = amount * (1 + rate)^years
  return(x)
}

#fixed_contrib function
#'@title future value of annuity
#'@description calculates the future value of savings that you put annually while the total amount increases with a rate of return
#'@param C contribution at the end of each year
#'@param r annual rate of return in decimal
#'@param t time in years
#'@param FVA future value of annuity
#'@return FVA

annuity <- function(contrib, rate, years) {
  FVA = contrib * (((1 + rate)^years - 1) / rate)
  return(FVA)
}


#growing_contrib function
#'@title future value of growing annuity
#'@description calculates the future value of an account that grows every year by a percentage while the account itself increaes in value according to a rate of return
#'@param C first contribution made on year one
#'@param r annual rate of return on the account in decimals
#'@param g growth rate of the account each year in decimals
#'@param t time in years 
#'@param FVGA future value of growing annuity
#'@return FVGA in dollars

growing_annuity <- function(contrib, rate, growth, years) {
  FVGA = contrib * ((((1 + rate)^years) - ((1 + growth)^years)) / (rate - growth))
  return(FVGA)
}


server <- function(input, output) {
  
  
  output$plot <- renderPlot({
    
    
    year <- seq(0, input$years, 1)
    no_contrib <- rep(0, length(year))
    fixed_contrib <- rep(0, length(year))
    growing_contrib <- rep(0, length(year))
    
    for(y in 0:input$years) {
      no_contrib[y + 1] <- future_value(input$amount, input$return / 100, y)
      fixed_contrib[y + 1] <- annuity(input$contribution, input$return / 100, y) + no_contrib[y + 1]
      growing_contrib[y + 1] <- growing_annuity(input$contribution, input$return / 100, input$growth / 100, y) + no_contrib[y + 1]
    }
    
    modalities2 <- data.frame(
      year = rep(0:input$years, 3),
      value = c(no_contrib, fixed_contrib, growing_contrib),
      type = c(rep('no_contrib', input$years + 1), rep('fixed_contrib', input$years + 1), rep('growing_contrib', input$years + 1))
    )
    
    if(input$facet == FALSE){
    ggplot(data = modalities2) + 
      geom_line(aes(x = year, y = value, color = type), size = 1.25) + 
      geom_point(aes(x = year, y = value, color = type), size = 2) +
      labs(x = 'year', y = 'value', title = 'Growth of Different Savings Modalities')
    }
    else{
      ggplot(data = modalities2) +
        geom_point(aes(x = year, y = value, color = type)) +
        geom_area(aes(x = year, y = value, fill = type, alpha = 0.5, color = type)) + 
        facet_grid( ~ type) +
        theme_bw() 
    }
  })
  
  output$balances <- renderPrint({
    years <- seq(0, input$years, 1)
    no_contrib <- rep(0, length(years))
    fixed_contrib <- rep(0, length(years))
    growing_contrib <- rep(0, length(years))
    
    for(y in 0:input$years) {
      no_contrib[y + 1] <- future_value(input$amount, input$return / 100, y)
      fixed_contrib[y + 1] <- annuity(contrib = input$contribution, rate = input$return / 100, years = y) + no_contrib[y + 1]
      growing_contrib[y + 1] <- growing_annuity(input$contribution, input$return / 100, input$growth / 100, years = y) + no_contrib[y + 1]
    }
    
    modalities <- data.frame(years, no_contrib, fixed_contrib, growing_contrib)
    
    modalities
  })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

