# ===================================================================
# Title: HW04: Grades Visualizer
# Subtitle: Stat 133, Fall 2017
# Description: UI/server code for "Grade Visualizer" Shiny app.
# Author: Woo Sik (Lewis) Kim
# Date: 11/26/2017
# ===================================================================

library(shiny)
library(ggvis)
source('../code/functions.R')

# Importing cleanscores.csv: 
dataset <- read.csv('../data/cleandata/cleanscores.csv',
                    stringsAsFactors = FALSE)

# List of inputs for the 2nd and 3rd tabs (histogram and scatterplot):
inputs <- c("HW1", "HW2", "HW3", "HW4", "HW5", "HW6", "HW7", "HW8",
                   "HW9", "QZ1", "QZ2", "QZ3", "QZ4", "EX1", "EX2", "Lab",
                   "Homework", "Quiz", "Test1", "Test2", "Overall")

# List of choices for "Show Line" in the 3rd tab (scatterplot):
lineOptions <- c("none", "lm", "loess")

# Creating a data frame GRADE_DIS with Grades, Frequency,
# and Prop. as columns:
dataset$Grade <- factor(dataset$Grade,
                        levels = c('A+', 'A', 'A-',
                                   'B+', 'B', 'B-',
                                   'C+', 'C', 'C-',
                                   'D', 'F'))

freq <- as.data.frame(table(dataset$Grade))
prop <- as.data.frame(prop.table(table(dataset$Grade)))

grade_dis <- data.frame("Grade" = c('A+', 'A', 'A-', 'B+', 'B', 'B-',
                                    'C+', 'C', 'C-', 'D', 'F'),
                        "Freq" = freq[,2], "Prop" = prop[,2])

ui <- shinyUI(fluidPage(
  
  # Header Panel
  titlePanel(title = h3("Grade Visualizer", align="left")),
  
  # Side Panel
  sidebarPanel(
    conditionalPanel(condition='input.tabselected==1', h4("Grades Distribution"),
                     tableOutput('table')),
    
    conditionalPanel(condition='input.tabselected==2',
                     selectInput("xvars1", "X-axis Variable", inputs,
                                 selected="HW1", selectize = TRUE, multiple = FALSE),
                     sliderInput("binwidth", "Bin Width", 1, 10, 10)),
    
    conditionalPanel(condition='input.tabselected==3',
                     selectInput("xvars2", "X-axis Variable", inputs,
                                 selected="Test1", selectize = TRUE, multiple = FALSE),
                     selectInput("yvars", "Y-axis Variable", inputs,
                                 selected="Overall", selectize = TRUE, multiple = FALSE),
                     sliderInput("opacity", "Opacity", 0, 1, 0.5, step=0.1),
                     radioButtons("showline", "Show Line", lineOptions, selected="none"))
  ),
  
  # Main Panel
  mainPanel(
    tabsetPanel(type="tab", id = "tabselected",
                tabPanel("Barchart", value=1, ggvisOutput("bar")),
                
                
                tabPanel("Histogram", value=2, ggvisOutput("hist"),
                         h5("Summary Statistics"), verbatimTextOutput('summary')),
                
                
                tabPanel("Scatterplot", value=3, ggvisOutput("scatter"),
                         h5("Correlation:"), verbatimTextOutput("corr"))
    )
  )
))

server <- function(input, output) {
  
  #
  output$table <- renderTable(grade_dis)
  
  # Creating a bar plot:
  dataset$Grade <- factor(dataset$Grade,
                   levels = c('A+', 'A', 'A-',
                              'B+', 'B', 'B-',
                              'C+', 'C', 'C-',
                              'D', 'F'))
  
  dataset %>% ggvis(~Grade, fill := '33a8ff', opacity := 0.8) %>% layer_bars() %>%
  add_axis("x", title = "Grade") %>% add_axis("y", title = "Frequency") %>% bind_shiny("bar")
  
  # Creating a histogram plot:
  hist_plot <- reactive({
    xvar1 <- prop("x", as.symbol(input$xvars1))
    
    histogram <- dataset %>% 
      ggvis(x = xvar1, fill := 'cc3300', opacity := 0.8) %>%
      layer_histograms(stroke := 'white', width = input$binwidth)
  })
  hist_plot %>% bind_shiny("hist")
  
  # Printing summary statistics:
  output$summary <- renderPrint({
    colName <- input$xvars1
    selected = dataset[[colName]]
    print_stats(summary_stats(selected))
    
  })
  
  # Creating a scatter plot:
  scat_plot <- reactive({
    xvar2 <- prop("x", as.symbol(input$xvars2))
    yvar <- prop("y", as.symbol(input$yvars))
    
    if (input$showline == "loess") {
      scatterplot <- dataset %>%
        ggvis(x = xvar2, y = yvar, opacity := input$opacity) %>%
        layer_points() %>% layer_smooths()
    }
    else if (input$showline == "lm") {
      scatterplot <- dataset %>%
        ggvis(x = xvar2, y = yvar, opacity := input$opacity) %>%
        layer_points() %>% layer_model_predictions(model="lm", se=FALSE)
    }
    else {
      scatterplot <- dataset %>%
        ggvis(x = xvar2, y = yvar, opacity := input$opacity) %>%
        layer_points()
    }
    
  })
  scat_plot %>% bind_shiny("scatter")
  
  # Printing the correlation coefficient:
  output$corr <- renderText({
    col1name <- input$xvars2
    col1 = dataset[[col1name]]
    
    col2name <- input$yvars
    col2 <- dataset[[col2name]]
    
    cor(col1, col2)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
