library(shiny)
library(tidyverse)
library(ggplot2)
library(tools)
library(gridExtra)

# load data
load("PA_M_2006_2018.RData")
pop_max <- max(PA_M_2016_2018$Population)
PA_M_2016_2018$deficit <- factor(PA_M_2016_2018$deficit, label = c("No", "Yes"))
year_min <- min(PA_M_2016_2018$Reporting_Year)
year_max <- max(PA_M_2016_2018$Reporting_Year)

# Define UI for application that plot public finance data
ui <- fluidPage(

    # Application title
    titlePanel("PA Municipalities Public Finance Data (2006-2018)"),

    # Sidebar inputs for municipality type and year 
    sidebarLayout(
        sidebarPanel(
            
            # slider year
            sliderInput(
                inputId = "year",
                label = "Max Year (Scatterplot & Bar Chart) & Year Range (Line Chart & Data Table)",
                min = year_min,
                max = year_max,
                step = 1,
                value = c(year_min, year_max),
                dragRange = T,
                sep = ""
            ),

            # check box for municipal type
            checkboxGroupInput(
               inputId = "type",
               label = "Municipality Type",
               choices = c("City",
                           "Borough",
                           "First Class Township",
                           "Second Class Township"
                           ),
               selected = c("City",
                            "Borough",
                            "First Class Township",
                            "Second Class Township"
                            )
               ),
            
            # download button
            downloadButton(
                outputId = "download", 
                label = "Download Data Table"
            )
            ),

        # Show plot output
        mainPanel(
            tabsetPanel(
            # scatterplot of revenue per capita vs expenditure per capita
                tabPanel("ScatterPlot", plotOutput("scatterplot")),
                
           # bar chart of median revenue and median expenditure
                tabPanel("Bar Chart", plotOutput("bar")),
           
           # line chart
                tabPanel("Line Chart", plotOutput("line")),
           
           # Show data table 
                tabPanel("Data Table", DT::dataTableOutput(outputId = "table")),
           
           # Sources
                tabPanel("Sources", uiOutput("source"))
           )
           )
    )
    )

# Define server logic 
server <- function(input, output) {
    
    # reactive subsetting for scatterplot and bar chart
    PA_subset <- reactive({
        req(input$year)
        req(input$type) 
        PA_M_2016_2018 %>% 
        filter((Reporting_Year == max(input$year)) & (Municipality_Type %in% input$type))
        })
    
    # create reative subset for line graph and data table
    PA_subset_time <- reactive({
        req(input$year)
        req(input$type) 
        PA_M_2016_2018 %>% 
            filter((Reporting_Year <= max(input$year)) &
                    (Reporting_Year >= min(input$year)) &
                    (Municipality_Type %in% input$type))
        })  
    
    # Create scatterplot
    output$scatterplot <- renderPlot({
        # identify outliers 
        outliers_x <- (boxplot(PA_subset()$Expenditures_Per_Capita))$out
        outliers_y <- (boxplot(PA_subset()$Revenues_Per_Capita))$out
        
    PA_subset() %>% 
            filter(!(Expenditures_Per_Capita %in% outliers_x)) %>%
            filter(!(Revenues_Per_Capita %in% outliers_y)) %>%
            ggplot(aes(x = Expenditures_Per_Capita, y = Revenues_Per_Capita )) +
            geom_point() +
            geom_abline(intercept =0 , slope = 1, size = 1, color = "red",
                        linetype = "dashed") +
            geom_text(aes(x=150, label="Deficit Zone", y= 0), colour= "#D55E00")+
            geom_text(aes(x=150, label="Surplus Zone", y=500), colour= "#0072B2") +        
            xlab("Expenditure per Capita($)") + ylab("Revenue Per Capita($)") +
            labs(title = paste(
            "Revenue Per Capita vs Expenditure per Capita in",
            max(input$year), "\nFor", paste(input$type, collapse = ', '), 
            "\nAbove line (y = x) means presence of surplus and below line means presence of deficit."),
            caption = paste(length(outliers_x), 
                            "outliers were removed for Expenditure per Capita.", 
                            "\n", length(outliers_y), 
                            "outliers were removed for Revenue Per Capita."))
    })
    
    # revenue chart
    output$bar <- renderPlot({
        
    # create table of median values
        med_table <- PA_subset() %>%
            group_by(deficit, Municipality_Type) %>%
            filter(Municipality_Type %in% input$type) %>%
            summarize(
                median_Revenues_Per_Capita = median(Revenues_Per_Capita, na.rm = T),
                median_Expenditures_Per_Capita = median(Expenditures_Per_Capita, 
                                                        na.rm = T)) 
            
    # bar chart output
        b1 <- med_table %>%
            ggplot(aes(x = Municipality_Type, y = median_Revenues_Per_Capita, 
                       fill = deficit)) +
            geom_bar(stat = "identity", position = 'dodge') +        
            xlab("Municipality Type") + 
            ylab("Revenue Per Capita($)") +
            labs(title = paste("Median Revenue Per Capita in",
                               max(input$year)), 
                               caption = "Median is used due to large outliers.") +
            scale_fill_manual(values=c("#0072B2", "#D55E00")) +
            geom_text(aes(label = median_Revenues_Per_Capita), vjust =1.5, 
                      color = "white", 
                      position = position_dodge(.9))
            

        # expenditure chart
        b2 <- med_table %>%
            ggplot(aes(x = Municipality_Type, y = median_Expenditures_Per_Capita, 
                       fill = deficit)) +
            geom_bar(stat = "identity", position = 'dodge') +        
            xlab("Municipality Type") + ylab("Expenditures Per Capita($)") +
            labs(title = paste("Median Expenditures Per Capita in",
                               max(input$year)), 
                 caption = "Median is used due to large outliers.") +
            scale_fill_manual(values=c("#0072B2", "#D55E00")) + 
            geom_text(aes(label = median_Expenditures_Per_Capita), vjust =1.5, 
                      color = "white", 
                      position = position_dodge(.9))
       
        grid.arrange(b1, b2, ncol = 1)
        })
    
        # Create line chart
        output$line <- renderPlot({
                # create table of median        
                med_subset_time_1 <- PA_subset_time() %>% 
                        group_by(Reporting_Year, Municipality_Type, deficit) %>%
                        summarize(median_Fund_Balance_Per_Capita = 
                                median(Fund_Balance_Per_Capita, na.rm = T))
                
                # change to date form. source:https://stackoverflow.com/questions/50935857/ggplot2-keeps-adding-5-at-end-of-year-variable-on-x-axis 
                med_subset_time_1$date <- as.Date(
                    paste(med_subset_time_1$Reporting_Year, 1, 1, sep="-"))
                
                med_subset_time_1 %>%
                    ggplot(aes(x = date, y = median_Fund_Balance_Per_Capita, 
                               color = deficit)) +
                    geom_line() +
                    scale_color_manual(values=c("#0072B2", "#D55E00")) +
                    scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
                    facet_grid(. ~ Municipality_Type) +
                    xlab("Year") + 
                    ylab("Fund Balance($)") +
                    labs(title = paste(
                    "Line Chart of Median End-of-Year Fund Balance From",
                    min(input$year), "To", max(input$year), "\nFund balance is the accumulated financial resources that is still unused over the years."), 
                    caption = "Median is used due to large outliers.") 
        })
        
        # Create data table
        output$table <- DT::renderDataTable({
            DT::datatable(data = PA_subset_time()[, 1:12], 
            options = list(pageLength = 10), 
            rownames = FALSE,
            colnames = c("Year" = 1,
                         "Municipal" = 2,
                         "Type" = 3,
                         "Revenues" = 5,
                         "Revenues\nPer Capita" = 6,
                         "Expenditures" = 7,
                         "Expenditures\nPer Capita" = 8,
                         "Surplus/Loss" = 9,
                         "Surplus/Loss\nPer Capita" = 10,
                         "Fund Balance" = 11,
                         "Fund Balance Per Capita" = 12
                         ),
            style = 'bootstrap'
            )
            })
        
        # download data table
        output$download <- downloadHandler(
            filename = function() {
                paste("PA_From", min(input$year), "To", max(input$year), ".csv", 
                      sep = "")
            },
            content = function(file) {
                write.csv(PA_subset_time(), file, row.names = FALSE)
            }
        )
        
        # source text
        output$source <- renderUI({
            url_1 <- tags$a(href="http://munstats.pa.gov/Reports/ReportInformation2.aspx?report=StatewideMuniAfr", "PA Department of Community & Economic Development")
            url_2 <- tags$a(href="https://www.census.gov/cgi-bin/geo/shapefiles/index.php", "Census Shapefile")
            
            tags$div(
                h1("This Data Set was cleaned and merged using the following sources:"),
                br(),
                tagList("1. Municipal Data:", url_1 ),
                br(),
                tagList("2. Municipal Names:", url_2 ),
                br(),
                p("To get accurate municipal names and its corresponding counties, I have to refer to census data")
                )
            })
        }

# Run the application 
shinyApp(ui = ui, server = server)
