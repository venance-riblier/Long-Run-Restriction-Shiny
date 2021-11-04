library(shiny)


source("./data_prep_BQ.R")


# Define UI ----
ui <- fluidPage( 
  titlePanel("SVAR"),
  
  sidebarPanel(
    selectInput("select_impulse", h3("Impulse shock"), 
                choices = list("Supply" = 1, "Demand" = 2), selected = 1),
  
    selectInput("select_response", h3("Response variable"), 
              choices = list("Output" = 1, "Unemployment" = 2), selected = 1),
    
    sliderInput("select_horizon", h3("Time horizon"),
                min = 10, max = 60, value = 40, step = 2, ticks=F),
    
    radioButtons("sign_shock", h3("Sign of the shock"),
                 choices = list("Positive" = 1, "Negative" = -1), selected = 1),
    
    h3("Bootstrap"),
    checkboxInput("bootstrap", "Compute CI", value = F), 
  
    conditionalPanel(
      condition = "input.bootstrap == true",
      sliderInput("no_boot", h5("Confidence interval band "),
                  min = 0.9, max = 0.99, value = 0.95, ticks=F, step= 0.01)
    )),
    
    mainPanel(p("SVAR model with long run restrictions. Replication of Blanchard, O. J., & Quah, D. (1989). The dynamic effects of aggregate demand and supply disturbances. American Economic Review, 79(4), 655-673.")),
    mainPanel(h4(textOutput("title"))),
    mainPanel(plotOutput("plot")),
    mainPanel(p(textOutput("ci_description")))
  )
  


# Define server logic ----
server <- function(input, output) {
  
  output$title <- renderText({
    switch(input$select_impulse, "1" = "Supply shock", "2" = "Demand shock")
  })
  
  output$ci_description <- renderText({
    if (input$bootstrap==T) {
      description <- paste0(100*input$no_boot, " % confidence intervals computed with 100 bootstrap simulations. ")
    }
  })
  

  output$plot <- renderPlot({
    
    # Unpack inputs
    imp <- switch(input$select_impulse, "1" = "gdp", "2" = "unrate")
    rep <- switch (input$select_response, "1" = "gdp", "2" = "unrate")
    hor <- input$select_horizon
    sign_shock <- as.numeric(input$sign_shock)
    bootstrap <- input$bootstrap
    no_boot <- input$no_boot
    
    # IRF of GDP response must be cumulative to recover level
    cumu <- switch(rep, "gdp" = T, "unrate" = F ) 

    # Compute IRF
    
    irf <- irf(svar_results, impulse = imp, response = rep, n.ahead = hor,
                boot = bootstrap, ci = no_boot, cumulative = cumu)
    sirf <- sign_shock*data.frame(irf$irf)
    
    # Plot
    plot_irf <- ggplot(sirf) +
      geom_line(aes(x = c(0:hor), y = sirf[,1]), size = 0.9) +
      labs(x = "Quarters", y = switch(rep, "gdp" = "Output", "unrate" = "Unemployment"))
                                            
    
    # Option Bootstrap 
    if (bootstrap==T) {
      lower_bound <- sign_shock*data.frame(irf$Lower)
      upper_bound <- sign_shock*data.frame(irf$Upper)
      
      plot_irf <- plot_irf + 
        geom_line(aes(x = c(0:hor), y = lower_bound[,1]),
                  linetype = "dashed", color = "red", size = 0.8, alpha = 0.7) +
        geom_line(aes(x = c(0:hor), y = upper_bound[,1]),
                  linetype = "dashed", color = "red", size = 0.8, alpha = 0.7)
      
    } 
    
    # Display output
    plot_irf
  })
  
}


# Run the app ----
shinyApp(ui = ui, server = server)