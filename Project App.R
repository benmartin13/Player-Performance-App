#You will need to load the "qualified_hitters2016-2018" file as "qualified_hitters"
# and the "qualified_hitters2019" file as "qualified_hitters2019"




library(tidyverse)

qualified_hitters%>%group_by(Name) %>% dplyr::select(Name) -> player_list

library(shiny)
ui <- fluidPage(theme = shinytheme("united"),
                wellPanel("Predicted wOBA From Statcast Metrics"),
                fluidRow(
                  column(3,
                         wellPanel(
                           fluidRow(
                             column(12,
                                    selectInput(
                                      inputId = "Name",
                                      label = "Choose Player:",
                                      choices = arrange(player_list, Name)
                                    ))
                             
                           ),
                         ),
                         wellPanel(
                           fluidRow(
                             column(12,
                                    htmlOutput("legend"))
                           )
                         ),
                         ),
                  column(9, 
                         wellPanel(
                           (DT::dataTableOutput("table2016"))),
                         wellPanel(
                           (DT::dataTableOutput("table2017"))),
                         wellPanel(
                           (DT::dataTableOutput("table2018"))),
                         wellPanel(
                           (DT::dataTableOutput("table2019"))),
                  ),
                ),
                fluidRow(
                  column(12, plotOutput("wOBA"))
                )
  
)


server <- function(input, output) {
  
  output$legend <-  renderUI({
    key <- paste("KEY:")
    hh <- paste("HH% = Hard Hit Rate")
    la_hh <- paste("LA on HH = Avg. Launch Angle on Hard Hit Balls")
    std_la <- paste("LA SD = Std. Dev. of Launch Angle")
    maxEV <- paste("Peak EV = Maximum Exit Velocity")
    avgEV <- paste("Avg EV = Average Exit Velocity")
    barrel_consistency <- paste("Barrel Consistency = Avg EV/Peak EV")
    wOBA <- paste("wOBA = Weighted On Base Average")
   
    HTML(paste(key,hh, la_hh, std_la, maxEV, avgEV, barrel_consistency, wOBA, sep = '<br/>'))
    
  })
  
  output$table2016 <- DT::renderDataTable({
    data <- filter(qualified_hitters, Name %in% input$Name)
    data <- filter(data, year == 2016)
    
    data <- data.frame( "Year" = data$year,"HH%" = round((data$hhrate)*100, 1), "LA on HH" = round(data$la_hh, 1), 
                       "LA SD" = round(data$std_la, 1), "Peak EV" = data$maxEV, 
                       "Avg EV" = round(data$avgEV, 1), "Barrel Consistency" = round((data$barrel_consistency)*100,1),
                       "Age" = data$age, "Tenure" = data$tenure,
                       "wOBA" = data$wOBA,
                       "Predicted Next wOBA" = round(data$est_nextwOBA, 3))
    
    datatable(data, options = list(dom = 't'), colnames = c("Year","HH%", "LA on HH", "LA SD","Peak EV", "Avg EV",
                                                              "Barrel Consistency", "Age", "Tenure", 
                                                                "wOBA", "Predicted 2017 wOBA"), rownames = FALSE)
    
  })
  
  output$table2017 <- DT::renderDataTable({
    data <- filter(qualified_hitters, Name %in% input$Name)
    data <- filter(data, year == 2017)
    
    data <- data.frame("Year" = data$year,"HH%" = round((data$hhrate)*100, 1), "LA on HH" = round(data$la_hh, 1), 
                       "LA SD" = round(data$std_la, 1), "Peak EV" = data$maxEV, 
                       "Avg EV" = round(data$avgEV, 1), "Barrel Consistency" = round((data$barrel_consistency)*100,1),
                       "Age" = data$age, "Tenure" = data$tenure,
                       "wOBA" = data$wOBA,
                       "Predicted Next wOBA" = round(data$est_nextwOBA, 3))
    
    datatable(data, options = list(dom = 't'), colnames = c("Year","HH%", "LA on HH", "LA SD","Peak EV", "Avg EV",
                                                            "Barrel Consistency", "Age", "Tenure", 
                                                            "wOBA", "Predicted 2018 wOBA"), rownames = FALSE)
    
  })
  
  output$table2018 <- DT::renderDataTable({
    data <- filter(qualified_hitters, Name %in% input$Name)
    data <- filter(data, year == 2018)
    
    data <- data.frame("Year" = data$year,"HH%" = round((data$hhrate)*100, 1), "LA on HH" = round(data$la_hh, 1), 
                       "LA SD" = round(data$std_la, 1), "Peak EV" = data$maxEV, 
                       "Avg EV" = round(data$avgEV, 1), "Barrel Consistency" = round((data$barrel_consistency)*100,1),
                       "Age" = data$age, "Tenure" = data$tenure,
                       "wOBA" = data$wOBA,
                       "Predicted Next wOBA" = round(data$est_nextwOBA, 3))
    
    datatable(data, options = list(dom = 't'), colnames = c("Year","HH%", "LA on HH", "LA SD","Peak EV", "Avg EV",
                                                            "Barrel Consistency", "Age", "Tenure", 
                                                            "wOBA", "Predicted 2019 wOBA"), rownames = FALSE)
  })
  
  output$table2019 <- DT::renderDataTable({
    data <- filter(qualified_hitters2019, Name%in%input$Name)
    
    data <- data.frame("Year" = data$year,"HH%" = round((data$hhrate)*100, 1), "LA on HH" = round(data$la_hh, 1), 
                       "LA SD" = round(data$std_la, 1), "Peak EV" = data$maxEV, 
                       "Avg EV" = round(data$avgEV, 1), "Barrel Consistency" = round((data$barrel_consistency)*100,1),
                       "Age" = data$age, "Tenure" = data$tenure,
                       "wOBA" = data$wOBA, "Predicted Next wOBA" = round(data$est_nextwOBA, 3))
    
    datatable(data, options = list(dom = 't'), colnames = c("Year","HH%", "LA on HH", "LA SD","Peak EV", "Avg EV",
                                                            "Barrel Consistency", "Age", "Tenure", 
                                                            "wOBA","Predicted 2020 wOBA"), rownames = FALSE)
    
    })
  
  output$wOBA <- renderPlot({
    data <- filter(qualified_hitters2019, Name %in% input$Name)
    
    plot_data <- filter(qualified_hitters2019, wOBA > 0 & wOBA < .5)
    

    ggplot(plot_data, aes(x = wOBA)) + 
      geom_density(bw = .05) + xlim(c(0, .6)) +
      geom_vline(xintercept = data$wOBA, color = "red", linetype = "dashed") +
      geom_vline(xintercept = data$est_nextwOBA, color = "blue", linetype = "dashed")+
      labs(title = "2019 MLB wOBA Distribution", 
           x = "wOBA",
           y = "Density")+
      geom_label(label = "2019 wOBA", x = data$wOBA, y = 2.8, color = "red")+
      geom_label(label = "Predicted 2020 wOBA", x = data$est_nextwOBA, y = 3.2, color = "blue")
      
  })
  
}


shinyApp(ui = ui, server = server)


