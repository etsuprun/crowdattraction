library(shiny)

# # Define UI for baltimore aquarium visualization
# shinyUI(fluidPage(
# 
#         # Application title
#         titlePanel("Baltimore National Aquarium Visiting Visulization"),
# 
#         # Sidebar with controls to provide a date and time
#         sidebarLayout(
#                 sidebarPanel(
#                         dateInput("date", "Date to visit aquarium:", value="2015-08-01"),
# 
#                         selectInput("hour", "Select the Time to visit:",
#                                     choices = c(paste0(9:11,"AM"),paste0(c(12,1:8),"PM"))),
# 
#                         helpText("Note: The Aquarium is open everyday from 9AM to 5PM except Friday to 8PM")
#                 ),
# 
# 
#                 # Show the caption, a summary of the dataset and an HTML
#                 # table with the requested number of observations
#                 mainPanel(
# 
#                         tabsetPanel(type = "tabs",
#                                     tabPanel("Plan your Visit", h3("The percentage the aquarium is busier than average is:")),
#                                     tabPanel("Historical Data")
#                         )
#                 )
#         )
# ))



shinyUI(pageWithSidebar(
  headerPanel("Baltimore National Aquarium Visiting Visulization"),
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     
                     dateInput("date", "Date to visit aquarium:", value="2015-08-01"),
                     
                     selectInput("hour", "Select the Time to visit:",
                                 choices = c(paste0(9:11,"AM"),paste0(c(12,1:8),"PM"))),
                     
                     helpText("Note: The Aquarium is open everyday from 9AM to 5PM except Friday to 8PM.")
    ),
    conditionalPanel(condition="input.conditionedPanels==2",
                     
                     dateRangeInput('dateRange',
                                    label = 'Select the date range to view historical result:',
                                    start = "2015-02-08", end = "2015-08-08"
                     ),
                     
                     helpText("Note: After you define the date range, the visualization of 
                              historical data will get automatically updated.")
                ),
                conditionalPanel(condition="input.conditionedPanels==3",
                                 
                                 helpText("Heatmap of Historical Visiting Data by week and hour")
                                 )
        ),
        mainPanel(
                tabsetPanel(
                        tabPanel("Plan your Visit", value=1, plotOutput("combine")), 
                        tabPanel("Historical Data", value=2, 
                                 p("The following is a plot of curves for each day within the user 
                                   specified date range."), br(), plotOutput("histmap")),
                        tabPanel("Visiting Heatmap",value=3, plotOutput("heatmap"))
                        , id = "conditionedPanels"
                )
        )
))
