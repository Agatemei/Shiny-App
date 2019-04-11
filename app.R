library(shiny)
library(ggplot2)

# Some functions 
future_value <- function(amount, rate, years) {
  return(amount * (1 + rate)^years)
  }
annuity <- function(contrib, rate, years) {
  return(contrib * ((1 + rate)^years - 1) / rate)
  }
growing_annuity <- function(contrib, rate, growth, years) {
  return(contrib * (((1 + rate)^years - (1 + growth)^years) / (rate - growth)))
  }


ui <- fluidPage(
   
   titlePanel("Saving-Investing Modalities"),
   
   fluidRow(
     column(width = 4,
            sliderInput("initial",
                     label = "Initial Amount",
                     min = 1,
                     max = 100000,
                     pre = "$",
                     step = 500,
                     value = 1000),
         sliderInput("contrib",
                     label = "Annual Contribution",
                     min = 0,
                     max = 50000,
                     step = 500,
                     pre = "$",
                     value = 2000)
         ),
     column(width = 4,
            sliderInput("rate",
                     label = "Retrun Rate (in %)",
                     min = 0,
                     max = 20,
                     step = 0.1,
                     value = 5),
         sliderInput("growth",
                     label = "Growth Rate (in %)",
                     min = 0,
                     max = 20,
                     step = 0.1,
                     value = 2)
         ),
     column(width = 4,
            sliderInput("year",
                     label = "Years",
                     min = 0,
                     max = 50,
                     round = TRUE,
                     value = 20),
         selectInput("facet",
                     label = "Facet?",
                     choices = list("Yes" = TRUE, "No" = FALSE),
                     selected = FALSE)
         )
      ),
   
   hr(),
   h4("Timelines"),
   plotOutput("ggplot"),
   br(),
   h4("Balances"),
   verbatimTextOutput("balances")
)



server <- function(input, output) {
  
  modalities <- reactive({
    
    # Years in the table
    year <- 0:input$year 
  
    # Mode 1
    no_contrib <- rep(0, length.out = length(year))
    for (i in 1:length(year)) {
      no_contrib[i] <- future_value(amount = input$initial, rate = input$rate / 100, years = year[i])
      }
  
    # Mode 2
    fixed_contrib <- rep(0, length.out = length(year))
    for (i in 1:length(year)) {
      fixed_contrib[i] <- no_contrib[i] + annuity(contrib = input$contrib, rate = input$rate / 100, years = year[i])
      }
  
    # Mode 3
    growing_contrib <- rep(0, length.out = length(year))
    for (i in 1:length(year)) {
      growing_contrib[i] <- no_contrib[i] + growing_annuity(contrib = input$contrib, rate = input$rate / 100, growth = input$growth / 100, years = year[i]) 
      }
  
    modalities <- data.frame(year = year, 
                             no_contrib = no_contrib, 
                             fixed_contrib = fixed_contrib, 
                             growing_contrib = growing_contrib)
  
    return(modalities)
    })
  
  
  modes_investing <- reactive({
    
    modes_investing <- data.frame(value = c(modalities()$no_contrib, 
                                            modalities()$fixed_contrib, 
                                            modalities()$growing_contrib), 
                                  year = rep(0:input$year, times = 3), 
                                  variable = rep(c("no_contrib", "fixed_contrib", "growing_contrib"), each = input$year + 1))
    
    modes_investing$variable <- factor(modes_investing$variable, levels = c("no_contrib", "fixed_contrib", "growing_contrib"))
    
    return(modes_investing)
    
  }) 

  
 output$balances <- renderPrint({
   modalities()
 })
 
 output$ggplot <- renderPlot({
   if (input$facet) {
     ggplot(modes_investing(), aes(x = year, y = value, color = variable, fill = variable)) +
       geom_point() +
       geom_line() +
       geom_area(alpha = 0.5) +
       labs(title = "Three modes of investing") +
       scale_color_manual(values=c("#fc6c89", "#65db55", "#3d94ff")) +
       facet_wrap( ~ variable) +
       theme_bw()
   } else {
     ggplot(modes_investing(), aes(x = year, y = value, color = variable)) +
       geom_point() +
       geom_line() +
       labs(title = "Three modes of investing") +
       scale_color_manual(values=c("#fc6c89", "#65db55", "#3d94ff"))
   }
 })
}


shinyApp(ui = ui, server = server)
