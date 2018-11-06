# # Call the necessary libraries
# library(shiny)
# library(shinydashboard)
# library(corrplot)
# 
# # If you don't have MathJax, its installation requires devtools
# install.packages("devtools")
# library(devtools)
# install_github("vnijs/MathJaxR")
# library(MathJaxR)
# 
# library(plotly)
# library(ggplot2)
# library(tidyverse)

# The above code assumes you have the common R data wrangling packages
# Otherwise, run line 20 - 40 to install and attach packages depending on existence.

# Define a pkgsummon function to check existence of package before installing
pkgsummon<- function(x){
  if(!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}

pkgsummon('shiny')
pkgsummon('shinydashboard')
pkgsummon('corrplot')
pkgsummon('plotly')
pkgsummon('ggplot2')
pkgsummon('tidyverse')

# If you don't have MathJax, it will install devtools then MathJaxR from Github before attaching
if(!require('MathJaxR', character.only = TRUE)){
  install.packages('devtools', dependencies = TRUE)
  library('devtools', character.only = TRUE)
  install_github("vnijs/MathJaxR")
  library('MathJaxR', character.only = TRUE)
}

# Read in data once
fire <- read.csv("Data/forestfires.csv")

# Turn the spatial coordinates into categorical factors
fire$X <- factor(fire$X)
fire$Y <- factor(fire$Y)

# Categorize months into season
fire$season <- ifelse(fire$month %in% c("dec","jan","feb"),"winter",
                      ifelse(fire$month %in% c("sep", "oct", "nov"),"autumn",
                             ifelse(fire$month %in% c("jun","july","aug"), "summer","spring")))

fire$season <- factor(fire$season, levels = c("spring", "summer", "autumn", "winter"))

# Relevel the existing factors month and day
fire$month <- factor(fire$month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
fire$day <- factor(fire$day, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun"))

# Create log transformed area just like the paper
fire$ln_area <- log(fire$area+1)

ui <- dashboardPage(skin = "red",
                    
  dashboardHeader(title = "Forest Fire Investigation",titleWidth = 250),
  
  dashboardSidebar(
    sidebarMenu(
      checkboxInput("month_filter_option", "Filter by Month"),
      
      conditionalPanel(condition = "input.month_filter_option == true",
                       selectInput("month_filter", "Select Months:", 
                                   choices = levels(fire$month),
                                   multiple = TRUE, selected = NULL)),
      
      checkboxInput("x_filter_option", "Filter by X spatial coordinates"),
      
      conditionalPanel(condition = "input.x_filter_option == true",
                       selectInput("x_filter", "Select X spatial coordinates:", 
                                   choices = levels(fire$X),
                                   multiple = TRUE, selected = NULL)),
      
      checkboxInput("y_filter_option", "Filter by Y spatial coordinates"),
      
      conditionalPanel(condition = "input.y_filter_option == true",
                       selectInput("y_filter", "Select Y spatial coordinates:", 
                                   choices = levels(fire$Y),
                                   multiple = TRUE, selected = NULL)),
      
      checkboxInput("fire_filter_option", "Filter by data with fire area > 0"),
      
      menuItem("Introduction", tabName = "introduction"),
      menuItem("Dataset/Correlation", tabName = "preliminary"),
      menuItem("Boxplot/Distribution", tabName = "distribution"),
      menuItem("Scatter Plot Visualization", tabName = "visualization")
    )
  ),
  
  dashboardBody(
    tabItems(
    
      tabItem(tabName = "introduction",
        fluidRow(
          tabBox(
            tabPanel(
              "Introduction of Dataset",
              "The forest fire dataset is taken from the UCI Machine Learning Repository.
              It was originally used in a data mining research to predict forest fires using meteorological Data.
              The experiments were conducted using a 10-fold (cross-validation) x 30 runs. Two 
              regression metrics were measured: MAD and RMSE.", br(),
              withMathJax(),
              helpText('Mean Absolute Deviation (MAD): $$\\frac{1}{N}\\times\\sum_{i=1}^n|y_i-\\hat{y_i}|$$'),
              helpText('Root Mean Squared Error (RMSE): $$\\sqrt{\\frac{\\sum_{i=1}^n(y_i-\\hat{y_i})^2}{N}}$$'),
              "A Gaussian support vector machine (SVM) fed with only 4 direct weather conditions (temp, RH, wind and rain) obtained the best MAD value: 
              12.71 +- 0.01 (mean and confidence interval within 95% using a t-student distribution). The best RMSE was attained by the naive mean predictor. 
              An analysis to the regression error curve (REC) shows that the SVM model predicts more examples within a lower admitted error.",br(),br(),
              p(strong("Variable Information:")),
              "1. X - x-axis spatial coordinate within the Montesinho park map: 1 to 9", br(),
              "2. Y - y-axis spatial coordinate within the Montesinho park map: 2 to 9", br(),
              "3. month - month of the year: jan to dec", br(), 
              "4. day - day of the week: mon to sun", br(),
              "5. FFMC - FFMC index from the FWI system: 18.7 to 96.20", br(),
              "6. DMC - DMC index from the FWI system: 1.1 to 291.3", br(),
              "7. DC - DC index from the FWI system: 7.9 to 860.6", br(), 
              "8. ISI - ISI index from the FWI system: 0.0 to 56.10", br(),
              "9. temp - temperature in Celsius degrees: 2.2 to 33.30", br(),
              "10. RH - relative humidity in %: 15.0 to 100", br(),
              "11. wind - wind speed in km/h: 0.40 to 9.40", br(), 
              "12. rain - outside rain in mm/m2 : 0.0 to 6.4", br(), 
              "13. area - the burned area of the forest (in ha): 0.00 to 1090.84", br(),
              p(em("(this area output variable is very skewed towards 0.0, thus it undergoes logarithm transform ln(area+1) in the original research).")),
              a("Click here to refer to the research paper.", href = "http://www3.dsi.uminho.pt/pcortez/fires.pdf")
            ),
            tabPanel(
              title = "App Purpose",
              "The Shiny app allows the user to perform exploratory analysis by visually inspecting the data. 
              Because dynamtic options are programmed within the app file, they can just open the browser and start wrangling the data without knowing R, Javascript, D3.JS, etc programming. 
              R shiny has the potential to mimic the dashboard style business intelligence tool (e.g. Tableua) with a great flexibility and low cost when deployed appropriately. 
              This project serves as a practice opportunity to implement ShinyDashBoard, MathJax and different widget functions, as well as tools in tidyverse and ggplot2.
              Later in the semester, we will add some machine learning modeling (e.g. support vector machine) to the app and produce a complete data product.",br(),br(),
              HTML('<center><img src="Montesinho_Map.png"></center>'), br(), br(),
              "In the sidebar menu, the user has the month, X-coordinate and Y-coordinate of the Montesinho forest as filtering parameters. The filtering parameter boxes are defaulted to unchecked, which means no filters is applied.
              When a box is checked, the corresponding multiple values dynamic dropdown will show up for user to choose from. 
              Depending on the combination of filter values, the app will slice the dataset differently and changes the visualization outputs.
              If the user wants to undo the filters, simply uncheck the box and the selections will default to nothing.", br(), br()
            )
          )
        )
      ),
    
      tabItem(tabName = "preliminary",
        # Boxes need to be put in a row (or column)
        fluidRow(
          box(
            title = "Correlation Diagram", width = 5, status = "primary", 
            plotOutput("corrogram", width = 550)
          ),
          
          box(
            title = "Corrogram Style", status = "warning",
            selectizeInput("method", "Select the visualization method:", 
                           selected = "circle", 
                           choices = c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
            "In default circle method, positive correlations are displayed in blue and negative correlations in red color. Color intensity and the size of the circle are proportional to the correlation coefficients.",
            br(),br(),
            selectizeInput("type", "Select the layout types:", 
                           selected = "full", 
                           choices = c("full", "upper", "lower")),
            "The default full shows the full correlation matrix, while the other 2 options display the upper triangle or lower triangle.", br(),
            h5("*If a subset is filtered to 0 or only 1 row of data, the correlation will not be defined and it will output an error message: invalid symbol coordinates", style = "color:red;")
          )
        ),
      
        fluidRow(
          box(
            title = "Data Set Output", status = "info", width = 7,
            
            # Download Button
            downloadButton("downloadData", "Download"),
            br(),br(),
            
            # Dataset Output
            dataTableOutput("table")
          )
        )
      ),
      
      tabItem(tabName = "distribution",
              fluidRow(
                box(
                  title = "Boxplot", width = 6, status = "primary", 
                  plotOutput("box")
                ),
                
                box(
                  title = "Variable Controls", width = 6, status = "warning",
                  selectizeInput("box_x_var", "Select x variable:",
                                 choices = colnames(fire)[sapply(fire, is.factor)],
                                 selected = "X"),
                  selectizeInput("box_y_var", "Select y variable:", 
                                 choices = colnames(fire)[sapply(fire, is.numeric)],
                                 selected = "RH"),
                  selectizeInput("box_fill_var", "Select filling variable:", 
                                 choices = c('season','month'),
                                 selected = "season"),
                  checkboxInput("jitter", "Show jitter to get a random spread of data points"),
                  downloadButton('downloadPlot','Download Box Plot')
                )
              ),
              
              fluidRow(
                box(
                  title = "Histogram", width = 6, status = "primary", 
                  plotOutput("histogram")
                )
              )
              
      ),
      
      tabItem(tabName = "visualization",
        fluidRow(
          box(
            title = "3D Scatter Plot", width = 6, status = "primary", 
            plotlyOutput("plot3d", width = "800px", height = "700px"),
            "*You can click the camera icon to download a static image of the 3D plot if opening the app in browser. This plotly feature won't work in R studio."
          ),
          
         column(width = 6,
          box(
            title = "Variable Controls", width = NULL, status = "warning",
            "You can hover over the scatter plot and the plotly function icons will show up. Try panning and rotating the plot to an angle you want to look at.",
            selectizeInput("x_var", "Select x variable:",
                           choices = colnames(fire)[sapply(fire, is.numeric)],
                           selected = "temp"),
            selectizeInput("y_var", "Select y variable:", 
                           choices = colnames(fire)[sapply(fire, is.numeric)],
                           selected = "wind"),
            selectizeInput("z_var", "Select z variable:", 
                           choices = colnames(fire)[sapply(fire, is.numeric)],
                           selected = "RH"),
            selectizeInput("col_var", "Select color variable:", 
                           choices = colnames(fire)[sapply(fire, is.numeric)],
                           selected = "area")
          ),
         
          box(
            title = "2D Scatter Plot", width = NULL, status = "primary",
            plotOutput("plot2", height = 300,click = "plot2_click"),
            "Coordinate positions of cursor:", br(),
            verbatimTextOutput("click_info")
          )
         )
       )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Filter Data based on UI input
  getData <- reactive({
    filtered_fire <- (fire %>% {if(!is.null(input$month_filter)) filter(., month %in% input$month_filter) else .} 
                           %>% {if(!is.null(input$x_filter)) filter(., X %in% input$x_filter) else .}
                           %>% {if(!is.null(input$y_filter)) filter(., Y %in% input$y_filter) else .}
                           %>% {if(input$fire_filter_option == TRUE) filter(., area > 0) else .})
  })
  
  # Default the choices to NULL when users unselect fitlering
  observe({
    if(input$month_filter_option==FALSE){
      updateSelectInput(session, "month_filter", 
                        choices = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
                        selected = NULL)
    }
    if(input$x_filter_option==FALSE){
      updateSelectInput(session, "x_filter", 
                        choices = levels(fire$X),
                        selected = NULL)
    }
    if(input$y_filter_option==FALSE){
      updateSelectInput(session, "y_filter", 
                        choices = levels(fire$Y),
                        selected = NULL)
    }
  })
  
  # Create Correlation Diagram
  output$corrogram <- renderPlot({
    
    # Get filtered data from getData
    filtered_fire <- getData()
    
    # Compute correlation of quantitative variables after taking away the factors
    M <- cor(filtered_fire[,sapply(filtered_fire, is.numeric)])
    corrplot(M, method = input$method, type = input$type)
    
  })
  
  # Create rendering of observations table    
  output$table <- renderDataTable({
    getData()
    })
  
  # Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = "firedata.csv",
    content = function(file) {
      write.csv(getData(), file, row.names = FALSE)
    }
  )
  
  getboxplot <- reactive({
    filtered_fire <- getData()
    if(input$jitter == TRUE){
      boxp <- (ggplot(filtered_fire, aes(x = get(input$box_x_var), y = get(input$box_y_var), fill = get(input$box_fill_var))) + geom_boxplot() + geom_jitter()
               +labs(fill = input$box_fill_var, x = input$box_x_var, y = input$box_y_var) + theme_grey(base_size = 18))
    } else {
      boxp <- (ggplot(filtered_fire, aes(x = get(input$box_x_var), y = get(input$box_y_var), fill = get(input$box_fill_var))) + geom_boxplot()
               +labs(fill = input$box_fill_var, x = input$box_x_var, y = input$box_y_var) + theme_grey(base_size = 18))
    }
    
  })
  
  # Boxplot
  output$box <- renderPlot({
    hist <- getboxplot()
    hist
  })
  
  # Boxplot for download
  output$downloadPlot <- downloadHandler(
    filename = function(){"boxplot.png"},
    content = function(file){
      ggsave(file,plot=getboxplot(), width = 12, height = 8)
    }
  )
  
  # Histogram
  output$histogram <- renderPlot({
    (ggplot(getData(), aes(x = get(input$box_y_var), color = get(input$box_fill_var))) 
    + geom_histogram(fill = "white") + facet_grid(.~get(input$box_fill_var))+labs(color = input$box_fill_var, x = input$box_y_var) + theme_grey(base_size = 18))
  })
  
  # 3D scatter plot
  output$plot3d <- renderPlotly({
    filtered_fire <- getData()
    (plot_ly(filtered_fire,x=~get(input$x_var),y=~get(input$y_var),z=~get(input$z_var),color=~get(input$col_var), type='scatter3d', marker = list(size = 4)) 
    %>% layout(scene = list(xaxis = list(title = input$x_var),
                            yaxis = list(title = input$y_var),
                            zaxis = list(title = input$z_var))
              ))
  })
  
  # 2D scatter plot
  output$plot2 <- renderPlot({
    filtered_fire <- getData()
    ggplot(filtered_fire, aes(x = get(input$x_var), y = get(input$y_var))) + geom_point() + labs(x = input$x_var, y = input$y_var)
  })
  
  output$click_info <- renderText({
    paste0("x=", input$plot2_click$x, "\ny=", input$plot2_click$y)
  })
}

shinyApp(ui, server)
