#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(BayesianLaterality)
library(ggplot2)
library(dplyr)
library(tidyr)

# Define UI for application
ui <- fluidPage(
   
     
  sidebarPanel(
    HTML("<h3>Parameters</h3>"),
  
    sliderInput("prob0",
                "Probability of left dominance for right-handers:",
                min = 0, max = 1, value = 0.87),    
    
    sliderInput("prob1",
                "Probability of left dominance for left-handers:",
                min = 0, max = 1, value = 0.31),
    

    
    textInput("mu_right_handers",
              "Mean laterality for right-handers with left and non-left dominance:",
              value = "12, -24"),
    
    textInput("sd_right_handers",
              "Standard deviations of laterality for right-handers with left and non-left dominance:",
              value = "17, 17"),
    
    textInput("mu_left_handers",
              "Mean laterality for left-handers with left and non-left dominance:",
              value = "10, -24"),
    
    textInput("sd_left_handers",
              "Standard deviations of laterality for left-handers with left and non-left dominance:",
              value = "24.9, 24.9"),
    
    
    sliderInput("icc",
                "Correlation between repeated measurements on the same subject:",
                min = 0, max = 1, value = 0.7)   
  ),
     
    
       
       
   # Show a plot of the generated distribution
   mainPanel(
     HTML("<h3>Visualization of Distributions</h3>"),
     plotOutput("distPlot"),
     HTML("<br><br>"),
     
     HTML("<h3>Compute probabilities</h3>"),
     HTML(paste("Provide one or more measurements on a given subject, and specify the handedness of the subject.",
          "The table below states shows the probability of left and non-left hemispheric dominance for the subject,",
          "and the parameters specified in the left side of this page are used.<br><br>")),
     
  
    sidebarPanel(
      radioButtons("handedness",
                   "Handedness:",
                   choices = list("left", "right")),
      
      textInput("laterality",
                "Observed laterality (between -100 and 100, comma separated)",
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
     
     data <- crossing(listening = seq(from = -100, to = 100, by = 3), handedness = c("left", "right")) %>% 
       mutate(ID = row_number())
     
     mu_left_handers <- do.call(as.numeric, strsplit(input$mu_left_handers, split = ","))
     mu_right_handers <- do.call(as.numeric, strsplit(input$mu_right_handers, split = ","))
     sd_left_handers <- do.call(as.numeric, strsplit(input$sd_left_handers, split = ","))
     sd_right_handers <- do.call(as.numeric, strsplit(input$sd_right_handers, split = ","))
     
     
     parameters <- tibble(
       dominance = rep(c("left", "right", "none"), each = 2),
       handedness = rep(c("left", "right"), 3),
       mean_li = c(mu_left_handers[[1]], mu_right_handers[[1]],
                   mu_left_handers[[2]], mu_right_handers[[2]],
                   0, 0),
       sd_li = c(sd_left_handers[[1]], sd_right_handers[[1]],
                 sd_left_handers[[2]], sd_right_handers[[2]],
                 22, 22),
       prob_dominance = c(input$prob1, input$prob0, 1 - input$prob1, 1 - input$prob0, 0, 0)
     )
     
     predict_dominance(data, parameters = parameters) %>% 
       filter(dominance == "left") %>% 
       inner_join(data, by = c("ID", "handedness")) %>% 
       rename(Handedness = handedness) %>% 
       ggplot(aes(x = listening, y = probability, group = Handedness, color = Handedness)) +
       geom_line() +
       ylab("Probability of left hemispheric dominance") +
       xlab("Laterality measurement") +
       theme(text = element_text(size = 18))
      
   })
   

   output$probTab <- renderTable({
     
     mu_left_handers <- do.call(as.numeric, strsplit(input$mu_left_handers, split = ","))
     mu_right_handers <- do.call(as.numeric, strsplit(input$mu_right_handers, split = ","))
     sd_left_handers <- do.call(as.numeric, strsplit(input$sd_left_handers, split = ","))
     sd_right_handers <- do.call(as.numeric, strsplit(input$sd_right_handers, split = ","))
     
     
     parameters <- tibble(
       dominance = rep(c("left", "right", "none"), each = 2),
       handedness = rep(c("left", "right"), 3),
       mean_li = c(mu_left_handers[[1]], mu_right_handers[[1]],
                   mu_left_handers[[2]], mu_right_handers[[2]],
                   0, 0),
       sd_li = c(sd_left_handers[[1]], sd_right_handers[[1]],
                 sd_left_handers[[2]], sd_right_handers[[2]],
                 22, 22),
       prob_dominance = c(input$prob1, input$prob0, 1 - input$prob1, 1 - input$prob0, 0, 0)
     )
     
     tibble(ID = 1,
            listening = as.numeric(unlist(strsplit(input$laterality, split = ","))),
            handedness = input$handedness) %>% 
       predict_dominance(parameters = parameters, icc = input$icc) %>%
       filter(dominance != "none") %>% 
       pivot_wider(names_from = dominance, values_from = probability) %>% 
       select(left, right) %>% 
       rename(`P(Left dominance)` = left,
              `P(Right dominance)` = right) %>% 
       pivot_longer(cols = everything())
   }, colnames = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

