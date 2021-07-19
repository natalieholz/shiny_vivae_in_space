library(shiny)
library(plotly)
library(tidyverse)
library(rgl)

shiny_df  <- readRDS(file = "shiny_df.rds")


ui <- fluidPage(
    headerPanel('VIVAE acoustic patterns'),
    sidebarPanel(

        selectInput('xcol','x variable',
                    choices=c("PC1 (pitch, pitch variation)" = names(shiny_df[1]) , "PC2 (vocal effort, brightness)" = names(shiny_df[2]),
                              "PC3 (perturbation, harshness)" = names(shiny_df[3]), "PC4 (amplitude variation)" = names(shiny_df[4]), 
                              "PC5 (spectral features A)" = names(shiny_df[5]), "PC6 (spectral features B)" = names(shiny_df[6])),
                                                    selected = names(shiny_df[1])),
        selectInput('ycol','y variable', 
                    choices=c("PC1 (pitch, pitch variation)" = names(shiny_df[1]) , "PC2 (vocal effort, brightness)" = names(shiny_df[2]),
                              "PC3 (perturbation, harshness)" = names(shiny_df[3]), "PC4 (amplitude variation)" = names(shiny_df[4]), 
                              "PC5 (spectral features A)" = names(shiny_df[5]), "PC6 (spectral features B)" = names(shiny_df[6])),
                    selected = names(shiny_df[2])),
        selectInput('zcol','z variable',
                    choices=c("PC1 (pitch, pitch variation)" = names(shiny_df[1]) , "PC2 (vocal effort, brightness, F1 mean)" = names(shiny_df[2]),
                              "PC3 (perturbation, harshness)" = names(shiny_df[3]), "PC4 (amplitude variation)" = names(shiny_df[4]), 
                              "PC5 (spectral features A)" = names(shiny_df[5]), "PC6 (spectral features B)" = names(shiny_df[6])),
                    selected = names(shiny_df[3])),
        checkboxGroupInput('value_row','emotions', levels(shiny_df[,8]), selected = levels(shiny_df[,8])[1:6]),
        ),
  

  mainPanel(
    tabsetPanel(id = "tabset",
                tabPanel("color vocalization intensity",
                         actionButton("no_ellipse", label = "hide ellipses"),
                         actionButton("yes_ellipse", label = "show ellipses")),
                tabPanel("color vocalization emotion")),

    plotlyOutput('plot'),
    hr(),
    
    textOutput("warning")

    )
)


server <- function(input, output) {
  
  
        v <- reactiveValues(plotEllipse = FALSE)
        
        observeEvent(input$yes_ellipse, {
            v$plotEllipse <- input$yes_ellipse
        })
        
        observeEvent(input$no_ellipse, {
            v$plotEllipse <- FALSE
        })  
        
    
    selectedData <- reactive({
        a <- shiny_df[shiny_df$emotions %in% input$value_row,]
        #a <- subset(shiny_df, shiny_df$emotions %in% input$value_row)
        a <- droplevels(a)
        return(a)
    })
    
#    selectedData_low <- reactive({
#        shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "low",]
#    })
  
    
    
    x <- reactive({
        shiny_df[shiny_df$emotions %in% input$value_row,input$xcol]
    })
    y <- reactive({
        shiny_df[shiny_df$emotions %in% input$value_row,input$ycol]
    })
    z <- reactive({
        shiny_df[shiny_df$emotions %in% input$value_row,input$zcol]
    })

     
    
  ########################################################################################################################
    # ELLIPSES 
    ellipse_int_low <- reactive({
    ellipse3d(cov(cbind(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "low", input$xcol], 
                        shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "low", input$ycol], 
                        shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "low", input$zcol])), 
              level = .5, 
              centre=c(mean(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "low", input$xcol]), 
                       mean(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "low", input$ycol]),
                       mean(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "low", input$zcol]))
              )$vb
    })

    ellipse_int_mod <- reactive({
        ellipse3d(cov(cbind(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "moderate", input$xcol], 
                            shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "moderate", input$ycol], 
                            shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "moderate", input$zcol])), 
                  level = .5, 
                  centre=c(mean(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "moderate", input$xcol]), 
                           mean(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "moderate", input$ycol]),
                           mean(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "moderate", input$zcol]))
        )$vb
    })
    
    ellipse_int_str <- reactive({
        ellipse3d(cov(cbind(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "strong", input$xcol], 
                            shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "strong", input$ycol], 
                            shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "strong", input$zcol])), 
                  level = .5, 
                  centre=c(mean(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "strong", input$xcol]), 
                           mean(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "strong", input$ycol]),
                           mean(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "strong", input$zcol]))
        )$vb
    })

    ellipse_int_peak <- reactive({
        ellipse3d(cov(cbind(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "peak", input$xcol], 
                            shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "peak", input$ycol], 
                            shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "peak", input$zcol])), 
                  level = .5, 
                  centre=c(mean(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "peak", input$xcol]), 
                           mean(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "peak", input$ycol]),
                           mean(shiny_df[shiny_df$emotions == input$value_row & shiny_df$intensity == "peak", input$zcol]))
        )$vb
    })

    
    
    output$plot <- renderPlotly({
      
      if (input$tabset == "color vocalization intensity") { 
        
        if (v$plotEllipse == FALSE) { 
          
            plot1 <- plot_ly(selectedData(), 
                             x = ~x(),
                             y = ~y(), 
                             z = ~z(),
                             color = ~intensity, 
                             colors = c('#fcc5c0','#f768a1', '#ae017e', '#49006a'),
                             type = "scatter3d", mode = 'markers', marker = list(size = 4)) %>% 
              layout(
                scene = list(
                  xaxis = list(title = "x"),
                  yaxis = list(title = "y"),
                  zaxis = list(title = "z")
                ))
          
            } else {
              
              if( input$xcol == input$ycol | input$xcol == input$zcol | input$ycol == input$zcol | length(input$value_row) == 0){
                plot1 <- plot_ly(selectedData(), 
                         x = ~x(),
                         y = ~y(), 
                         z = ~z(),
                         color = ~intensity, 
                         colors = c('#fcc5c0','#f768a1', '#ae017e', '#49006a'),
                         type = "scatter3d", mode = 'markers', marker = list(size = 4)) %>% 
                  layout(
                    scene = list(
                      xaxis = list(title = "x"),
                      yaxis = list(title = "y"),
                      zaxis = list(title = "z")))
              
                
                } else {
                  
                  plot1 <- plot_ly(selectedData(), colors = c(low = '#fcc5c0',moderate = '#f768a1', strong = '#ae017e', peak = '#49006a')) %>% 
            # Plot raw scatter data points
                    add_trace(selectedData(),
                              x = ~x(),
                              y = ~y(), 
                              z = ~z(), 
                              color = ~intensity, 
                              type = "scatter3d", mode = 'markers', marker = list(size = 4)) %>% 
                    add_trace(
                      x=ellipse_int_low()[1,], 
                      y=ellipse_int_low()[2,], 
                      z=ellipse_int_low()[3,], 
                      name = "low",
                      color =  ~selectedData()[selectedData()$intensity %in%  "low",]$intensity, 
                      type='mesh3d', alphahull = 0, opacity = 0.1) %>% 
                    add_trace(
                      x=ellipse_int_mod()[1,], 
                      y=ellipse_int_mod()[2,], 
                      z=ellipse_int_mod()[3,], 
                      name = "moderate",
                      color = ~selectedData()[selectedData()$intensity %in%  "moderate",]$intensity, 
                      type='mesh3d', alphahull = 0, opacity = 0.1)  %>% 
                    add_trace(
                      x=ellipse_int_str()[1,], 
                      y=ellipse_int_str()[2,], 
                      z=ellipse_int_str()[3,], 
                      name = "strong",
                      color = ~selectedData()[selectedData()$intensity %in% "strong",]$intensity,
                      type='mesh3d', alphahull = 0, opacity = 0.1) %>% 
                    add_trace(
                      x=ellipse_int_peak()[1,], 
                      y=ellipse_int_peak()[2,], 
                      z=ellipse_int_peak()[3,], 
                      name = "peak",
                      color = ~selectedData()[selectedData()$intensity %in% "peak",]$intensity,
                      type='mesh3d', alphahull = 0, opacity = 0.1) %>% 
                    layout(
                      scene = list(
                        xaxis = list(title = "x"),
                        yaxis = list(title = "y"),
                        zaxis = list(title = "z")
                      ))
                }
              }
      
        } else if (input$tabset == "color vocalization emotion") { 
          
          plot1 <- plot_ly(selectedData(), 
                           x = ~x(),
                           y = ~y(), 
                           z = ~z(),
                           color = ~emotions, 
                           colors = c(anger = "#ffd650", fear = "#f07914", pain = "#b90e28", achievement = "#15aba3", pleasure = "#2edcf0", surprise = "#05509f"),
                           type = "scatter3d", mode = 'markers', marker = list(size = 4)) %>% 
            layout(
              scene = list(
                xaxis = list(title = "x"),
                yaxis = list(title = "y"),
                zaxis = list(title = "z")
              ))
          
        
        }  else {

          plot1 <- plot_ly(selectedData(), 
                           x = ~x(),
                           y = ~y(), 
                           z = ~z(),
                           color = ~emotions, 
                           colors = c(anger = "#ffd650", fear = "#f07914", pain = "#b90e28", achievement = "#15aba3", pleasure = "#2edcf0", surprise = "#05509f"),
                           type = "scatter3d", mode = 'markers', marker = list(size = 4)) %>% 
            layout(
              scene = list(
                xaxis = list(title = "x"),
                yaxis = list(title = "y"),
                zaxis = list(title = "z")
              ))

      }

    })
    
   
    output$warning <- renderText({
      if(input$tabset == "color vocalization intensity" &&   input$yes_ellipse && input$xcol == input$ycol ) {
        "Dimensions must differ to view ellipses."
        
      } else if (input$tabset == "color vocalization intensity" &&  input$yes_ellipse && input$xcol == input$zcol  ) {
        "Dimensions must differ to view ellipses"
        
      } else if (input$tabset == "color vocalization intensity" &&  input$yes_ellipse && input$ycol == input$zcol ) {
        "Dimensions must differ to view ellipses"
        
      } else {
        ""
        
      }
    })
    
}

shinyApp(ui,server)

