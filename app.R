library(shiny)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(shinythemes)


#Load data
algerian_forest <- read.csv("data/Algerian_forest_fires_dataset_UPDATE.csv")
portugal_forest_fire <- read.csv("data/forestfires.csv.xls")

source("helpers.R",local = TRUE)

# Define UI for application that draws a histogram
ui <- navbarPage("Forest Fires",
                 tabPanel("About",
                          fluidPage(theme = shinytheme("cerulean"),
                                    fluidRow(
                                      leafletOutput("Zone"),
                                      br(),
                                      br(),
                                      br(),
                                      includeHTML("about.html"),
                                      br(),
                                      br(),
                                      br(),
                                      includeHTML("group_names.html"),
                                      br(),
                                      br()  
                                    )
                                    
                          )
                 ),
                 tabPanel("Summary Statistics",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("Country", "Choose a country:",
                                            choices = c("Algeria",
                                                        "Portugal"), selected = 1)
                                
                              ),
                              mainPanel(
                                verbatimTextOutput("summary")
                              ))
                          )
                          
                 ),
                 navbarMenu("Plots & Analyses",
                            tabPanel("Algeria",
                                     fluidRow(
                                       column(4,
                                              selectInput('in1', 'VARIABLES', choices = list("Temperature","Relative Humidity" = "RH",
                                                                                             "Wind"="Ws","Fine Fuel Moisture Code"="FFMC","Duff Moisture Code"="DMC",
                                                                                             "Drought Code"="DC1","Initial Spread Index"="ISI","Buildup Index"="BUI") , selectize=1)
                                       )
                                     ),
                                     mainPanel(
                                       tabsetPanel(type = "tabs",
                                                   tabPanel("Plot",plotOutput(outputId = "algerian")),
                                                   tabPanel("Analysis", verbatimTextOutput("algeria_analysis"),verbatimTextOutput("algeria_analysislm"),htmlOutput("example"))
                                       )
                                     )
                                     
                            ),
                            tabPanel("Portugal",
                                     fluidRow(
                                       column(4,
                                              selectInput('in2', 'VARIABLES', choices =  list("Temperature"="temp","Relative Humidity"="RH","Wind"="wind",
                                                                                              "Fine Fuel Moisture Code"="FFMC","Duff Moisture Code"="DMC","Drought Code"="DC",
                                                                                              "Initial Spread Index"="ISI") , selected = 1)
                                       )
                                     ),mainPanel(
                                       tabsetPanel(type = "tabs",
                                                   tabPanel("Plot",plotOutput(outputId = "portugal")),
                                                   tabPanel("Analysis",
                                                            fluidPage(
                                                              verbatimTextOutput("portugal_analysis"),verbatimTextOutput("portugal_analysislm"),htmlOutput("example2")
                                                            )
                                                   )
                                       )
                                       
                                     ))
                            
                 ),
                 
                 
                 navbarMenu("Data",
                            tabPanel("Data Sets",
                                     fluidPage(
                                       titlePanel("Data Tables"),
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput("dataset", "Choose a dataset:",
                                                       choices = c("Algeria Dataset",
                                                                   "Portugal Dataset"), selected = 1),
                                           downloadButton("downloadData", "Download")
                                         ),
                                         mainPanel(
                                           tableOutput("table")
                                         )
                                       )
                                       
                                     )
                            ),
                            tabPanel("Data Explanation",
                                     fluidPage(
                                       titlePanel("Data Explanation")
                                     ),
                                     mainPanel(
                                       img(src = "hehe.png", height = 700, width = 900)
                                     )
                                     
                            )),
                 tabPanel("References",
                          fluidPage(
                            br(),
                            includeHTML("references.html"),
                            br(),
                            br()
                          )
                          
                 )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$algerian <- renderPlot({
    if(input$in1=="DC1"){
      dc_intervals <- cut(as.numeric(algerian_forest$DC), breaks = 6)
      bar_data <- data.frame(fire_status = algerian_forest$fire_status, dc_intervals)
      count_data <- table(bar_data)
      bar_df <- as.data.frame(count_data)
      colnames(bar_df) <- c("fire_status", "dc_intervals", "count")
      
      ggplot(bar_df, aes(fill = fire_status, y = count, x = dc_intervals)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))
      
    }else if(input$in1=="Ws"){
      wind_intervals <- cut(algerian_forest$Ws, breaks = 6)
      bar_data <- data.frame(fire_status = algerian_forest$fire_status, wind_intervals)
      count_data <- table(bar_data)
      bar_df <- as.data.frame(count_data)
      colnames(bar_df) <- c("fire_status", "wind_intervals", "count")
      
      ggplot(bar_df, aes(fill = fire_status, y = count, x = wind_intervals)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))
      
    }else if(input$in1=="RH"){
      ggplot(data = algerian_forest, aes_string(x= input$in1, y= "fire_status")) +
        geom_point(color= "red", size = 1.5, shape = 15) + 
        geom_jitter(alpha= 0.5, width= 0.2, height= 0.1, color= "red", size = 1.5, shape = 15) +
        geom_smooth(method=glm, se=FALSE, 
                    method.args=list(family=binomial),
                    color="orange", size = 1.5) +
        labs(x= "Relative Humidity", y= "Fire Status", title = "Algerian Forest RH~Fire Status Logistic Regression") +
        scale_y_continuous(breaks = seq(0, 1, .5)) 
    }else if(input$in1=="Temperature"){
      ggplot(data = algerian_forest, aes_string(x= input$in1, y= "fire_status")) +
        geom_point(color= "purple", size = 2.5) + 
        geom_jitter(alpha= 0.2, width= 0.2, height= 0.1, color= "purple", size = 2) +
        geom_smooth(method=glm, se=FALSE, 
                    method.args=list(family=binomial),
                    color="orange", size = 1.5)+
        labs(x = "Temperature", y= "Fire Status", title = "Algerian Forest Temp~Fire Status Logistic Regression") +
        scale_y_continuous(breaks = seq(0, 1, .5)) 
    }else if(input$in1=="Temperature"){
      ggplot(data = algerian_forest, aes_string(x= input$in1, y= "fire_status")) +
        geom_point(color= "purple", size = 2.5) + 
        geom_jitter(alpha= 0.2, width= 0.2, height= 0.1, color= "purple", size = 2) +
        geom_smooth(method=glm, se=FALSE, 
                    method.args=list(family=binomial),
                    color="orange", size = 1.5)+
        labs(x = "Temperature", y= "Fire Status", title = "Algerian Forest Temp~Fire Status Logistic Regression") +
        scale_y_continuous(breaks = seq(0, 1, .5)) 
    }else if(input$in1 == "FFMC"){
      ggplot(data = algerian_forest, aes_string(x= input$in1, y= "fire_status"))+
        geom_point(color= "purple") + 
        geom_jitter(alpha= 0.2, width= 0.2, height= 0.1, color= "purple") +
        geom_smooth(method=glm, se=FALSE, 
                    method.args=list(family=binomial),
                    color="orange")
      
    }else if(input$in1 == "DMC"){
      dmc_intervals <- cut(algerian_forest$DMC, breaks = 6)
      bar_data <- data.frame(fire_status = algerian_forest$fire_status, dmc_intervals)
      count_data <- table(bar_data)
      bar_df <- as.data.frame(count_data)
      colnames(bar_df) <- c("fire_status", "dmc_intervals", "count")
      
      ggplot(bar_df, aes(fill = fire_status, y = count, x = dmc_intervals)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))
      
    }else if(input$in1 == "BUI"){
      bui_intervals <- cut(algerian_forest$BUI, breaks = 6)
      bar_data <- data.frame(fire_status = algerian_forest$fire_status, bui_intervals)
      count_data <- table(bar_data)
      bar_df <- as.data.frame(count_data)
      colnames(bar_df) <- c("fire_status", "bui_intervals", "count")
      
      ggplot(bar_df, aes(fill = fire_status, y = count, x = bui_intervals)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))
      }
    else if(input$in1 == "ISI"){
      isi_intervals <- cut(algerian_forest$ISI, breaks = 6)
      bar_data <- data.frame(fire_status = algerian_forest$fire_status, isi_intervals)
      count_data <- table(bar_data)
      bar_df <- as.data.frame(count_data)
      colnames(bar_df) <- c("fire_status", "isi_intervals", "count")
      
      ggplot(bar_df, aes(fill = fire_status, y = count, x = isi_intervals)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))
    }
    else if(input$in1 == "FWI"){
      fwi_intervals <- cut(as.numeric(algerian_forest$FWI), breaks = 6)
      bar_data <- data.frame(fire_status = algerian_forest$fire_status, fwi_intervals)
      count_data <- table(bar_data)
      bar_df <- as.data.frame(count_data)
      colnames(bar_df) <- c("fire_status", "fwi_intervals", "count")
      
      ggplot(bar_df, aes(fill = fire_status, y = count, x = fwi_intervals)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))
    }
    else if(input$in1 == "DC1"){
      
    }
    else{
      ggplot(data = algerian_forest, aes_string(x = input$in1, y = "fire_status")) +
        geom_point(color = "purple") + 
        geom_jitter(alpha = 0.2, width = 0.2, height = 0.1, color = "purple") +
        geom_smooth(method = glm, se = FALSE, 
                    method.args = list(family = binomial),
                    color = "orange")
    }
  })
  
  output$portugal <- renderPlot({ #dönüyor
    if(input$in2 == "FFMC"){
      ffmc_intervals <- cut(portugal_forest_fire$FFMC, breaks = 6)
      bar_data <- data.frame(fire_status = portugal_forest_fire$fire_status, ffmc_intervals)
      count_data <- table(bar_data)
      bar_df <- as.data.frame(count_data)
      colnames(bar_df) <- c("fire_status", "ffmc_intervals", "count")
      
      ggplot(bar_df, aes(fill = fire_status, y = count, x = ffmc_intervals)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))
      
    }else if(input$in2 == "DMC"){
      dmc_intervals <- cut(portugal_forest_fire$DMC, breaks = 6)
      bar_data <- data.frame(fire_status = portugal_forest_fire$fire_status, dmc_intervals)
      count_data <- table(bar_data)
      bar_df <- as.data.frame(count_data)
      colnames(bar_df) <- c("fire_status", "dmc_intervals", "count")
      
      ggplot(bar_df, aes(fill = fire_status, y = count, x = dmc_intervals)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))
    }
    else if(input$in2 == "DC"){
      dc_intervals <- cut(as.numeric(portugal_forest_fire$DC), breaks = 6)
      bar_data <- data.frame(fire_status = portugal_forest_fire$fire_status, dc_intervals)
      count_data <- table(bar_data)
      bar_df <- as.data.frame(count_data)
      colnames(bar_df) <- c("fire_status", "dc_intervals", "count")
      
      ggplot(bar_df, aes(fill = fire_status, y = count, x = dc_intervals)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))
    }
    else if(input$in2 == "ISI"){
      isi_intervals <- cut(portugal_forest_fire$ISI, breaks = 6)
      bar_data <- data.frame(fire_status = portugal_forest_fire$fire_status, isi_intervals)
      count_data <- table(bar_data)
      bar_df <- as.data.frame(count_data)
      colnames(bar_df) <- c("fire_status", "isi_intervals", "count")
      
      ggplot(bar_df, aes(fill = fire_status, y = count, x = isi_intervals)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))}
    else if(input$in2 == "wind"){
      wind_intervals <- cut(portugal_forest_fire$wind, breaks = 6)
      bar_data <- data.frame(fire_status = portugal_forest_fire$fire_status, wind_intervals)
      count_data <- table(bar_data)
      bar_df <- as.data.frame(count_data)
      colnames(bar_df) <- c("fire_status", "wind_intervals", "count")
      
      ggplot(bar_df, aes(fill = fire_status, y = count, x = wind_intervals)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))}
    
    else if(input$in2 == "RH"){
      rh_intervals <- cut(portugal_forest_fire$RH, breaks = 6)
      bar_data <- data.frame(fire_status = portugal_forest_fire$fire_status, rh_intervals)
      count_data <- table(bar_data)
      bar_df <- as.data.frame(count_data)
      colnames(bar_df) <- c("fire_status", "rh_intervals", "count")
      ggplot(bar_df, aes(fill = fire_status, y = count, x = rh_intervals)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))
    } else if(input$in2 == "temp"){
      temp_intervals <- cut(portugal_forest_fire$temp, breaks = 6)
      bar_data <- data.frame(fire_status = portugal_forest_fire$fire_status, temp_intervals)
      count_data <- table(bar_data)
      bar_df <- as.data.frame(count_data)
      colnames(bar_df) <- c("fire_status", "temp_intervals", "count")
      ggplot(bar_df, aes(fill = fire_status, y = count, x = temp_intervals)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))
    }
    
  })
  
  datasetInput2 <- reactive({
    switch(input$in1,
           "Temperature" = algerian_forest$Temperature,"RH" = algerian_forest$RH,"Ws" = algerian_forest$Ws,
           "FFMC" = algerian_forest$FFMC,"DMC" = algerian_forest$DMC,"DC1" = algerian_forest$DC1,
           "ISI" = algerian_forest$ISI,"BUI" = algerian_forest$BUI)
  })
  
  output$algeriansummary <- renderPrint({
    data_al <- datasetInput2()
    #data_al <- algerian_forest$ISI
    logit <- glm( as.formula(fire_status~data_al) ,data= algerian_forest, family = "binomial")
    summary(logit)
  })
  
  
  
  
  
  
  
  
  output$Zone <- renderLeaflet({
    leaflet(options = leafletOptions(dragging=FALSE)) %>% 
      setView(lng = -2.934524,lat=34.094107,zoom = 4) %>% 
      addTiles() %>% 
      addProviderTiles("Esri") %>% 
      addMarkers(lat=39.874486, lng= -8.650227,popup = "Portugal") %>% 
      addMarkers(lng = 2.562192 ,lat=28.416473,popup = "Algeria")
    
  })
  datasetInput <- reactive({
    switch(input$dataset,
           "Algeria Dataset"=algerian_forest,
           "Portugal Dataset"=portugal_forest_fire)
  })
  
  output$table <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      input$dataset
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = T)
    }
  )
  
  summaryInput <- reactive({
    switch(input$Country,
           "Algeria"=algerian_forest,
           "Portugal"=portugal_forest_fire)
  })
  
  output$summary <- renderPrint({
    dataset <- summaryInput()
    #dataset <- algerian_forest
    summary(dataset)
  })
  
  # Logistic regression analysis for Algeria
  output$algeria_analysis <- renderPrint({
    formula <- as.formula(paste("fire_status ~", input$in1))
    model <- glm(formula, data = algerian_forest, family = binomial)
    summary(model)
  })
  
  # Logistic regression analysis for Portugal
  output$portugal_analysis <- renderPrint({
    formula <- as.formula(paste("fire_status ~", input$in2))
    model <- glm(formula, data = portugal_forest_fire, family = binomial)
    summary(model)
  })
  #linear regression analysis for Algeria
  output$algeria_analysislm <- renderPrint({
    formula <- as.formula(paste("fire_status ~", input$in1))
    model <- lm(formula, data = algerian_forest)
    summary(model)
  })
  
  #linear regression analysis for Portugal
  output$portugal_analysislm <- renderPrint({
    formula <- as.formula(paste("fire_status ~", input$in2))
    model <- lm(formula, data = portugal_forest_fire)
    summary(model)
  })
  
  observe({
    req(input$in1)
    
    if (input$in1 == "Temperature") {
      output$example <- renderText("This analyse is made with logistic regression. 

In the summary, output of the p-value (intercept) is significantly small, so Ho can be rejected. Moreover, the difference between Null deviance and Residual deviance is high enough, that is to say the model is good for the data.

When beta1 coefficient is exponentiated, the output is 1.47143. Hence, we can say that the association between Fire Status and Temperature is positive.

Odds of fire breaking out increases by 1.47143 for each one unit (degree) change in temperature.")
    } else if(input$in1=="Ws"){
      output$example <- renderText("This analyse is made with linear regression and logistic regression.  

As you can see in the summary part of lm, output of the p-value (intercept) is significantly small, so Ho can be rejected. However, adjusted Rsquared is too small. Hence, the model is not a good fit for the data.

In the summary of glm, the difference between Null deviance and Residual deviance is too small, the model is not a good fit for the data.")
      
    }else if(input$in1=="RH"){
      output$example <- renderText("This analyse is made with logistic regression. 

In the summary, output of the p-value (intercept) is significantly small, so Ho can be rejected. Moreover, the difference between Null deviance and Residual deviance is high enough, the model is good for the data.

When beta1 coefficient is exponentiated, the output is 0.9308206 Hence, we can say that the association between Fire Status and RH is negative.

Odds of fire breaking out changes by 0.9308206 for each one unit (degree) change in RH.")
      
    }else if(input$in1=="ISI"){
      output$example <- renderText("This analyse is made with logistic regression. 

In the summary, output of the p-value (intercept) is significantly small, so Ho can be rejected. Moreover, the difference between Null deviance and Residual deviance is high enough, that is to say the model is good for the data.

When beta1 coefficient is exponentiated, the output is 3591.647  . Hence, we can say that the association between Fire Status and ISI is positive.

Odds of fire breaking out increases by 3591.647   for each one unit (degree) change in ISI.")
    }else if(input$in1=="FFMC"){
      output$example <- renderText("This analyse is made with logistic regression. 

In the summary, output of the p-value (intercept) is significantly small, so Ho can be rejected. Moreover, the difference between Null deviance and Residual deviance is high enough, that is to say the model is good for the data.

When beta1 coefficient is exponentiated, the output is 5.534967 . Hence, we can say that the association between Fire Status and FFMC is positive.

Odds of fire breaking out increases by 5.534967  for each one unit (degree) change in FFMC.")
      
      
    }else if(input$in1=="DMC"){
      output$example <- renderText("This analyse is made with logistic regression. 

In the summary, output of the p-value (intercept) is significantly small, so Ho can be rejected. Moreover, the difference between Null deviance and Residual deviance is high enough, that is to say the model is good for the data.

When beta1 coefficient is exponentiated, the output is 1.323033  . Hence, we can say that the association between Fire Status and DMC is positive.

Odds of fire breaking out increases by 1.323033  for each one unit (degree) change in DMC.")
      
      
    }else if(input$in1=="DC1"){
      output$example <- renderText("This analyse is made with linear regression. 

In the summary, output of the p-value (intercept) is significantly small, so Ho can be rejected. However, adjusted Rsquared is small, the model is not a good fit for the data.")
    }else if(input$in1=="BUI"){
      output$example <- renderText("This analyse is made with logistic regression. 

In the summary, output of the p-value (intercept) is significantly small, so Ho can be rejected. Moreover, the difference between Null deviance and Residual deviance is high enough, that is to say the model is good for the data.

When beta1 coefficient is exponentiated, the output is 1.2673   . Hence, we can say that the association between Fire Status and BUI is positive.

Odds of fire breaking out increases by 1.2673    for each one unit (degree) change in BUI.")
    }
  })
  
  observe({
    req(input$in2)
    
    if (input$in1 == "Temperature") {
      output$example2 <- renderText("This analyse is made with linear regression and logistic regression. The summary of calculated lm and glm can be found in the above part.
Adjusted Rsquared is too small. Hence, the model is not a good fit for the data.
In the summary of glm, output of the p-value (intercept) is great, so Ho cannot be rejected. The difference between Null deviance and Residual deviance is too small, the model is not a good fit for the data."
        
      )
    }
  })
}




# Run the application 
shinyApp(ui = ui, server = server)



