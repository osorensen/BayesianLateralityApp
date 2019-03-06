#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(asymm)
library(ggplot2)
library(dplyr)
library(tidyr)

# Define UI for application
ui <- fluidPage(
   
     
  sidebarPanel(
    HTML("<h3>Parameters</h3>"),
  
    
    sliderInput("rho0",
                "Probability of right hemispheric dominance given adextrality (rho0):",
                min = 0, max = 1, value = 0.31),
    
    sliderInput("rho1",
                "Probability of right hemispheric dominance given dextrality (rho1):",
                min = 0, max = 1, value = 0.115),
    
    textInput("mu",
              "Means (mu00, mu01, mu10, mu11):",
              value = "10, -24, 12, -24"),
    
    textInput("sd",
              "Standard deviations (sigma00, sigma01, sigma10, sigma11):",
              value = "22, 22, 22, 22"),
    
    textInput("mu_sem",
              "Optional standard error of means, e.g., '4.6, 7.0, 4.3, 7.0':",
              value = "")
  ),
     
    
       
       
   # Show a plot of the generated distribution
   mainPanel(
     HTML("<h3>Visualization of Distributions</h3>"),
     plotOutput("distPlot"),
     
     HTML("<h3>Compute probabilities</h3>"),
     HTML("Provide one or more measurements on a given subject."),
     
  
    sidebarPanel(
      textInput("handedness",
                "Handedness A or D:",
                value = "D"),
      
      textInput("listening",
                "Listening (between -100 and 100, comma separated)",
                value = "-10, 0, 10")
    ),
    mainPanel(
         tableOutput("probTab")
       )
  
   )
     
     
     
   
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
   output$distPlot <- renderPlot({
     
     data <- expand.grid(listening = seq(from = -100, to = 100, by = 1), handedness = c("A", "D"), 
                         stringsAsFactors = FALSE) %>% 
       mutate(ID = row_number())
     
     
     as_tibble(predict_asymmetry(data, 
                                 mu = as.numeric(unlist(strsplit(input$mu, split = ","))),
                                 sigma = as.numeric(unlist(strsplit(input$sd, split = ","))),
                                 rho = c(input$rho0, input$rho1),
                                 mu_sem = if(input$mu_sem == "") NULL else as.numeric(unlist(strsplit(input$mu_sem, split = ",")))
                                 )) %>% 
       bind_cols(as_tibble(data)) %>% 
       mutate(
         handedness = if_else(handedness == "A", "Adextral", "Dextral")
         ) %>% 
       ggplot(aes(x = listening, y = LeftDominance, group = handedness, color = handedness)) +
       geom_line() +
       ylab("") +
       xlab("Dichotic Listening Score") +
       ggtitle("Posterior Probability of Left Hemispheric Dominance") +
       theme(legend.title = element_blank(), text = element_text(size = 18))
      
   })
   
   output$probTab <- renderTable({
     tibble(ID = 1,
            listening = as.integer(unlist(strsplit(input$listening, split = ","))),
            handedness = input$handedness) %>% 
       predict_asymmetry(mu = as.numeric(unlist(strsplit(input$mu, split = ","))),
                         sigma = as.numeric(unlist(strsplit(input$sd, split = ","))),
                         rho = c(input$rho0, input$rho1),
                         mu_sem = if(input$mu_sem == "") NULL else as.numeric(unlist(strsplit(input$mu_sem, split = ",")))) %>%
       select(-ID) %>% 
       rename(`P(Left Dominance)` = LeftDominance,
              `P(Right Dominance)` = RightDominance) %>% 
       gather()
   }, colnames = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

