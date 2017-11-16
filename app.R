#####################################################################
# Title: Electorate exploration of same sex marriage survye
# Author: Steph de Silva.
# Permanent Email: steph@rex-analytics.com
# Date created: 16/11/17
# Date last altered: 16/12/17
# Purpose: This script builds a shiny app to explore the same sex marriage survey data
#########################################################################
# Data from: https://marriagesurvey.abs.gov.au/results/downloads.html
# Accessed: 15/11/17
#########################################################################

# the data

library(shiny)
library(readxl)
library(tidyverse)
data_outcome <- read_xls("australian_marriage_law_postal_survey_2017_-_response_final.xls",
                          sheet = "Table 2")
data_outcome <- data_outcome[8:182,]
colnames(data_outcome) <- c("electorate", "clear_yes_num", "clear_yes_pcnt", 
                            "clear_no_num", "clear_no_pcnt", 
                            "clear_total_num", "clear_total_pcnt","
                            blank1",
                            "elig_clear_response_num", "elig_clear_response_pcnt", 
                            "elig_unclear_response_num", "elig_unclear_response_pcnt",
                            "elig_nonresponse_num", "elig_nonresponse_pcnt", 
                            "total_num", "total_pcnt")
data_outcome <- data_outcome[,-8]
data_outcome$state <- NA
data_outcome$state[1:47] <- "NSW"
data_outcome$state[51:87] <- "VIC"
data_outcome$state[91:120] <- "QLD"
data_outcome$state[124:134] <- "SA"
data_outcome$state[138:153] <- "WA"
data_outcome$state[157:161] <-"TAS"
data_outcome$state[165:166] <- "NT"
data_outcome$state[170:171] <- "ACT"

data_outcome <- filter(data_outcome, is.na(state)== FALSE)
data_outcome <- gather(data_outcome, key = "measure", value = value,"clear_yes_num", "clear_yes_pcnt", 
                       "clear_no_num", "clear_no_pcnt", 
                       "clear_total_num", "clear_total_pcnt",
                       "elig_clear_response_num", "elig_clear_response_pcnt", 
                       "elig_unclear_response_num", "elig_unclear_response_pcnt",
                       "elig_nonresponse_num", "elig_nonresponse_pcnt", 
                       "total_num", "total_pcnt")
data_outcome$value <- as.numeric(data_outcome$value)
data_outcome <- data_outcome[order(data_outcome$electorate),]


# The user interface
ui <- fluidPage(
   
   # Application title
   titlePanel("Australia's Same Sex Postal Survey"),
   
   # Sidebar with drop down selection for electorate
   sidebarLayout(
      sidebarPanel(
         selectInput("electorateChosen",
                     "Electorate:",
                     choices = list(NSW = unique(data_outcome$electorate[which(data_outcome[,2] == "NSW")]),
                                    QLD = unique(data_outcome$electorate[which(data_outcome[,2] == "QLD")]),
                                    WA= unique(data_outcome$electorate[which(data_outcome[,2] == "WA")]),
                                    NT = unique(data_outcome$electorate[which(data_outcome[,2] == "NT")]),
                                    SA = unique(data_outcome$electorate[which(data_outcome[,2] == "SA")]) ,              
                                    ACT = unique(data_outcome$electorate[which(data_outcome[,2] == "ACT")]),   
                                    TAS =  unique(data_outcome$electorate[which(data_outcome[,2] == "TAS")]),
                                    VIC = unique(data_outcome$electorate[which(data_outcome[,2] == "VIC")])
                     ),
                     selected = "Warringah"
                       ),
         radioButtons("optionsChosen",
                     "Choose statistics:",
                     choices = c("Results", "Responses"),
                     selected = "Results"),
         submitButton("Go!")
      ),
      
      # Show a plot of the chosen electorate's outcomes
      mainPanel(
         plotOutput("electoratePlot")
      )
   )
)

# Define server 
server <- function(input, output) {
   
   output$electoratePlot <- renderPlot({
      # choose data based on electorate
     plotData <- filter(data_outcome, electorate == input$electorateChosen)
     if (input$optionsChosen == "Results"){
       plotData <- filter(plotData,  (measure == "clear_yes_pcnt" | measure =="clear_no_pcnt"))
       plotData$measure <- factor(plotData$measure, levels = c('clear_yes_pcnt', "clear_no_pcnt"))
       plotData$measure <- recode_factor(plotData$measure, clear_yes_pcnt = "yes", clear_no_pcnt= "no")
       titlePlot <- "Electorate Results"
     } else if (input$optionsChosen == "Responses"){
       plotData <- filter(plotData, (measure =="elig_clear_response_pcnt" | measure =="elig_unclear_response_pcnt" | measure =="elig_nonresponse_pcnt"))
       plotData$measure <- factor(plotData$measure, levels = c("elig_clear_response_pcnt", "elig_unclear_response_pcnt", "elig_nonresponse_pcnt"))
       plotData$measure <- recode_factor(plotData$measure, elig_clear_response_pcnt = "Clear response", 
                                                            elig_unclear_response_pcnt= "Unclear response",
                                                            elig_nonresponse_pcnt = "Non response")
       titlePlot <- "Electorate breakdown"
     } 
    
     p <- ggplot(plotData) +
       geom_bar(aes(measure, value), stat = "identity", 
                fill = "steelblue4")+
       ylab("Percentage of responses") +
       xlab("Measure")+
       theme_light()+
       ggtitle(titlePlot)
     print(p)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

