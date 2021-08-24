library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(magrittr)
library(BatchGetSymbols)
library(rvest)
library(xts)
library(plotly)
library(scales)
library(zoo)
library(quantmod)
library(qrmdata)
library(stringr)
library(TTR)
library(DT)
library(RColorBrewer)
library(kableExtra)
library(formattable)

wd <- getwd()
setwd(wd)

source(paste0(wd, "/data_clean.R"), local = TRUE)

DJI_prices <- read_csv(paste0(wd, "/", "DJI_prices.csv", sep = "")) 
DowJones_components_full <- read_csv(paste0(wd, "/", "DowJones_components_full.csv", sep=""))
ETF_full <- read_csv(paste0(wd, "/", "ETF_full.csv", sep=""))
SP_500_prices <- read_csv(paste0(wd, "/", "SP_500_prices.csv", sep=""))
nasdaq_data <- read_csv(paste0(wd, "/", "nasdaq_data.csv", sep=""))

nasdaq_tickers <- c(unique(nasdaq_data$ticker))

#### UI INTERFACE ####
ui <- dashboardPage(
    dashboardHeader(title = "Finance Dashboard"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Stock Market Performance", icon = icon("stats", lib = "glyphicon"),
                     startExpanded = TRUE, 
                     menuSubItem("NASDAQ Components", tabName = "nasdaqcomponents"),
                     menuSubItem("Dow Jones Components", tabName = "DJComponents"),
                     menuSubItem("Index Funds", tabName = "IndexIndicators")
            ),
            menuItem(" Cryptocurrency", icon = icon("btc", lib = "font-awesome")),
            menuItem("Additional Info", tabName = "mainmenu", icon = icon("comment", lib = "font-awesome"))
            )
        ),

    dashboardBody(
        tags$head(tags$style(
            HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}'))),
        tabItems(
            tabItem(tabName = "mainmenu",
                    h1("Some additional Information!"),
                    br(),
                    h4("My name is Ayaka, I generated this dashboard to visualize the performance of the stock market and
                       macroeconomy, and to test my own R skills. This dashboard contains a myriad of indicators reporting daily
                       data within the past year. Use the tabs and subtabs on the left-hand side of the page to navigate the dashboard."),
                    br(),
                    h4("Have any questions or suggestions? Please email me at ayakab@live.com.")),
            tabItem(tabName = "nasdaqcomponents",
                    h2("NASDAQ Composite Firms Performance"),
                    h5(" This page contains the daily performance of the firms 100 firms in the NASDAQ at the time that the Dashboard was refreshed.
                       Use the input selection to the left to select the firm of interest and customize the table below. Multiple firms
                       may be selected to compare performance accross companies."),
                    fluidRow(
                        box(title = "Inputs", status = "warning", height = "450px", 
                            "Use the filters below to customize the layout for all graphs on this page.", br(), 
                            br(),
                            sliderInput("dateInput_nasdaq", "Date", min =Sys.Date()-365, max = Sys.Date(), 
                                        value = c(Sys.Date()-365, Sys.Date()), timeFormat = "%d/%m/%Y"),
                            selectizeInput("TickerInput_nasdaq", "Select Ticker(s)", 
                                           choices = NULL,
                                           multiple = TRUE),
                            br(), br(), br(),
                            "Source: Yahoo Finance",
                            width = 3),
                        tabBox(id = "tabset2", width = 9,
                            tabPanel(plotlyOutput("nasdaq_plot", height = "400px"), title = "NASDAQ Firm Performance", 
                                width = 9),
                            tabPanel(plotlyOutput("nasdaq_line_plot", height = "400px"), title = "NASDAQ Adjusted Price Graph",
                                     width = 9)
                            )
                    ),
                    fluidRow(
                        tableOutput("nasdaq_table")
                    )),
            tabItem(tabName = "IndexIndicators",
                    h2("Index Funds Performance"),
                    h5("This page contains the daily prices and performance of the S&P 500 Index, Dow Jones
                       Industrial Average as well as three ETF's within the last year. Use
                       the input selections on the right hand side of the page to customize
                       the price and date frame of the graphs."),
                        fluidRow(
                            tabBox(
                                id = "tabset1", height = "250px",
                                tabPanel(plotlyOutput("price_plot", height = "350px"), title = "S&P 500 Daily Prices",
                                         width = 6, status = "primary"),
                                tabPanel(plotlyOutput("price_plotI", height = "350px"), title = "DJIA Daily Prices",
                                         width = 6, status = "primary"), width = 8),
                            box(title = "Inputs", status = "warning", height = "400px", 
                                "Use the filters below to customize the layout for all graphs on this page.", br(), 
                                br(),
                                selectInput("priceInput", "Price Option",
                                            choices = c("Opening Price", 
                                                        "Closing Price",
                                                        "High Price",
                                                        "Low Price",
                                                        "Adjusted Price"),
                                            selected = c("Adjusted Price")),
                                sliderInput("dateInput", "Date", min =Sys.Date()-365, max = Sys.Date(), 
                                            value = c(Sys.Date()-365, Sys.Date()), timeFormat = "%d/%m/%Y"),
                                br(), br(), br(),
                                "Source: Yahoo Finance",
                                width = 4)
                            ),
                    fluidRow(
                        valueBoxOutput("YearlyReturnBox", width = 3),
                        valueBoxOutput("MonthlyReturnBox", width = 3),
                        valueBoxOutput("YearlyReturnBoxI", width = 3),
                        valueBoxOutput("MonthlyReturnBoxI", width = 3)),
                    fluidRow(
                        box(plotlyOutput("VCN_plot", height = "250px"), title = "Vanguard FTSE Canada All Cap Index ETF", 
                            width = 6),
                        box(plotlyOutput("VEE_plot", height = "250px"), title = "FTSE Emerging Markets All Cap Index ETF", 
                            width = 6),
                        box(plotlyOutput("XUU_plot", height = "250px"), title = "iShares Core S&P US Total Market Index",
                            width = 12)
                        
                    )
                    ),
            tabItem(
                tabName = "DJComponents",
                h2("Dow Jones Components Performance"),
                h5("This page contains the daily prices and performance of the thirty firms that make up the Dow Jones
                   Industrial Average. The Dow Jones Industrial Average changes the weights and compositions of the 
                   firms that make up the average daily based on a series of factors. The data on this page is representative
                   of the DJIA on the last day that the Dashboard was updated."),
                fluidRow(
                    box(title = "Inputs", status = "warning", height = "350px", width = 4,
                        "Use the filters below to customize the layout for all graphs on this page.", br(), 
                        br(),
                        selectInput("priceInput1", "Price Option",
                                    choices = c("Opening Price", 
                                                "Closing Price",
                                                "High Price",
                                                "Low Price",
                                                "Adjusted Price"),
                                    selected = c("Adjusted Price")),
                        radioButtons("dateInputI", "Date", 
                                     choices = c("1Y", "YTD",
                                                 "1M",
                                                 "5D"), 
                                    selected = c("1Y"), inline = TRUE),
                        selectizeInput("TickerInput", "Select Ticker(s)", 
                                       choices = dowjones_tickers,
                                       selected = dowjones_tickers[1],
                                       multiple = TRUE)
                    ),
                    box(plotlyOutput("DowJonesComponents", height = "300px"), title = "Selected Dow Jones Firm(s)", 
                        width = 8)),
                fluidRow(
                    box(plotlyOutput("DowJonesComponentsPerformance", height = "300px"), title = "Company", 
                        width = 12))
                )
                )
            )
            )

#### DEFINE SERVER ####
server <- function(session, input, output) {
    ## REACTIVE DATA ##
    reactive_data <- reactive({
        filtered <-
            SP_500_prices %>%
            filter(SP_500_prices$Variable %in% input$priceInput,
                   date >= input$dateInput[1],
                   date <= input$dateInput[2])
    })
    
    reactive_dataII <- reactive({
        filtered <-
            DJI_prices %>%
            filter(DJI_prices$Variable %in% input$priceInput,
                   date >= input$dateInput[1],
                   date <= input$dateInput[2])
    })
    
    reactive_dataETF <- reactive({
        filtered <-
            ETF_full %>%
            filter(ETF_full$Variable %in% input$priceInput,
                   date >= input$dateInput[1],
                   date <= input$dateInput[2])
    })
    
    ## PAGE ONE OUTPUT ##
    output$price_plotI <- renderPlotly({
        data <- reactive_dataII() %>%
            filter(type_of_data == "prices") %>%
            mutate(movavg = rollmean(Value, k = 5, fill = NA))
        
        x <- list(
            title = "Date"
        )
        
        plot_ly(data,type="scatter",mode='lines',x=data$date,y=data$Value,color=factor(data$Variable),name=data$Variable,
                line = list(color = 'rgb(74, 135, 240)')) %>%
            layout(legend = list(orientation = 'h', x = 0.5, y = 1,
                                 xanchor = 'center', yref = 'paper',
                                 font = list(size = 10),
                                 bgcolor = 'transparent'),
                   xaxis = x) %>%
            add_lines(x = ~data$date, y = data$movavg, name = "Mv Avg",
                      line = list(color = '#E377C2', width = 0.75),
                      hoverinfo = "none", inherit = F) 
    }) 
    
    output$price_plot <- renderPlotly({
        data <- reactive_data() %>%
            filter(type_of_data == "prices") %>%
            mutate(movavg = rollmean(Value, k = 5, fill = NA))
        
        x <- list(
            title = "Date"
        )
        
        plot_ly(data,type="scatter",mode='lines',x=data$date,y=data$Value,color=factor(data$Variable),name=data$Variable,
                line = list(color = 'rgb(74, 135, 240)')) %>%
            layout(legend = list(orientation = 'h', x = 0.5, y = 1,
                                 xanchor = 'center', yref = 'paper',
                                 font = list(size = 10),
                                 bgcolor = 'transparent'),
                   yaxis = x) %>%
            add_lines(x = ~data$date, y = data$movavg, name = "Mv Avg",
                      line = list(color = '#E377C2', width = 0.75),
                      hoverinfo = "none", inherit = F) 
    }) 
    
    output$YearlyReturnBox <- renderValueBox({
        data <- reactive_data()
        value_last_year <- data %>%
            filter(type_of_data == "prices") %>%
            filter(Variable == data$Variable) %>%
            filter(date == (first(date)))
        
        value_today <- data %>%
            filter(type_of_data == "prices") %>%
            filter(Variable == data$Variable) %>%
            filter(date == last(date))
        change_values <- round((value_today$Value / value_last_year$Value - 1) * 100, digits = 2)
        
        value <- paste(change_values, "%", sep = "")
            
            valueBox(value, "S&P500 Average Yearly Return", icon = icon("glyphicon"),
            color = "olive")
    })
    
    output$MonthlyReturnBox <- renderValueBox({
        data <- reactive_data() 
        value_last_month <- data %>%
            filter(type_of_data == "prices") %>%
            filter(Variable == input$priceInput) %>%
            filter(date == (last(date)-30))
        
        value_today <- data %>%
            filter(type_of_data == "prices") %>%
            filter(Variable == input$priceInput) %>%
            filter(date == last(date))
        
        change_values <- round((value_today$Value / value_last_month$Value - 1) * 100, digits = 2)
        
        value <- paste(change_values, "%", sep = "")
        
            valueBox(value, "S&P500 Average Monthly Return", icon = icon("glyphicon"),
            color = "yellow")
    })
    output$YearlyReturnBoxI <- renderValueBox({
        data <- reactive_dataII()
        value_last_year <- data %>%
            filter(type_of_data == "prices") %>%
            filter(Variable == data$Variable) %>%
            filter(date == first(date))
        
        value_today <- data %>%
            filter(type_of_data == "prices") %>%
            filter(Variable == data$Variable) %>%
            filter(date == last(date))
        change_values <- round((value_today$Value / value_last_year$Value - 1) * 100, digits = 2)
        
        value <- paste(change_values, "%", sep = "")
        
        valueBox(value, "DJIA Average Yearly Return", icon = icon("glyphicon"),
                 color = "light-blue")
    })
    
    output$MonthlyReturnBoxI <- renderValueBox({
        data <- reactive_dataII()
        
        value_last_month <- data %>%
            filter(type_of_data == "prices") %>%
            filter(Variable == input$priceInput) %>%
            filter(date == (last(date)-30))
        
        value_today <- data %>%
            filter(type_of_data == "prices") %>%
            filter(Variable == input$priceInput) %>%
            filter(date == last(date))
        change_values <- round((value_today$Value / value_last_month$Value - 1) * 100, digits = 2)
        
        value <- paste(change_values, "%", sep = "")
        
        valueBox(
            value, "DJIA Average Monthly Return", icon = icon("glyphicon"),
            color = "red")
    })
    
    output$VCN_plot <- renderPlotly({
        data <- reactive_dataETF() %>%
            filter(type_of_data == "prices") %>%
            filter(firm == "VCN")
        plot_ly(data,type="scatter",mode='lines',x=data$date,y=data$Value,color=factor(data$Variable),name=data$Variable,
                line = list(color = 'rgb(125, 208, 209)')) %>%
            layout(showlegend = FALSE)
    }) 
    output$VEE_plot <- renderPlotly({
        data <- reactive_dataETF() %>%
            filter(type_of_data == "prices") %>%
            filter(firm == "VEE")
        plot_ly(data,type="scatter",mode='lines',x=data$date,y=data$Value,color=factor(data$Variable),name=data$Variable,
                line = list(color = 'rgb(172, 224, 153)')) %>%
            layout(showlegend = FALSE)
    })
    output$XUU_plot <- renderPlotly({
        data <- reactive_dataETF() %>%
            filter(type_of_data == "prices") %>%
            filter(firm == "XUU")
        plot_ly(data,type="scatter",mode='lines',x=data$date,y=data$Value,color=factor(data$Variable),name=data$Variable,
                line = list(color = 'rgb(240, 179, 74)')) %>%
            layout(showlegend = FALSE)
    })
    
    ## PAGE TWO OUTPUT ##
    reactive_data_dowjones <- reactive({
        filtered <-
            DowJones_components_full %>%
            filter(DowJones_components_full$Variable %in% input$priceInput1,
                   DowJones_components_full$ticker %in% input$TickerInput,
                   DowJones_components_full$timehorizon %in% input$dateInputI)
    })
    
    reactive_data_dowjonesI <- reactive({
        filtered <-
            DowJones_components_full %>%
            filter(DowJones_components_full$Variable %in% input$priceInput1,
                   DowJones_components_full$timehorizon %in% input$dateInputI)
    })
    
    output$DowJonesComponents <- renderPlotly({
        data <- reactive_data_dowjones() %>%
            group_by(ticker) %>%
            mutate(movavg = rollmean(Value, k = 5, fill = NA))
        nb.cols <- 30
        mycolors <- colorRampPalette(brewer.pal(8, "Accent"))(nb.cols)
        
        x <- list(
            title = "Date"
        )
        
        plot_ly(data,type="scatter",mode='lines',x=data$date,y=data$Value,color=factor(data$ticker),name=data$ticker,
                text = ~Name, 
                hoverinfo = text,
                colors = mycolors) %>%
            layout(legend = list(orientation = 'h', x = 0.5, y = 1,
                                 xanchor = 'center', yref = 'paper',
                                 font = list(size = 10),
                                 bgcolor = 'transparent'),
                   xaxis = x) %>%
            add_lines(x = ~data$date, y = data$movavg, name = "Mv Avg",
                      line = list(color = '#E377C2', width = 0.75),
                      hoverinfo = "none", inherit = F)    }) 
    
    output$DowJonesComponentsPerformance <- renderPlotly({
        reactive_data <- reactive_data_dowjonesI()
        data <- reactive_data %>%
            filter(., date == first(date) | date == last(date)) %>%
            filter(Variable == input$priceInput1) %>%
            select(Name, ticker, date, Value) %>%
            group_by(ticker, Name) %>%
            summarize(yearly_change = ((last(Value) / first(Value)) -1 ) * 100) %>%
            select(Name, ticker, yearly_change) %>%
            arrange(yearly_change)
            
        data$ticker <- factor(data$ticker, levels = unique(data$ticker)[order(data$yearly_change, decreasing = TRUE)])
        
        x <- list(
            title = "Price Return (%)"
            )
        y <- list(
            title = " "
        )
        data %>%
            plot_ly(., type = "bar", x = ~ticker, y = ~yearly_change, text = ~Name, 
                    hoverinfo = text,
                    colors = 'rgb(219, 185, 240)') %>%
            layout(showlegend = FALSE, xaxis =x, yaxis = y)
    })
    #### PAGE ONE ####
    reactive_data_nasdaq <- reactive({
        filtered <-
            nasdaq_data %>%
            filter(nasdaq_data$ticker %in% input$TickerInput_nasdaq,
                          date >= input$dateInput_nasdaq[1],
                          date <= input$dateInput_nasdaq[2])
    })
    
    updateSelectizeInput(
        session = session,
        inputId = "TickerInput_nasdaq",
        choices = nasdaq_tickers,
        selected = nasdaq_tickers[1],
        server = TRUE
    )
    
    output$nasdaq_plot <- renderPlotly({
        data <- reactive_data_nasdaq() %>%
            group_by(ticker) %>%
            mutate(movavg = rollmean(price.adjusted, k = 5, fill = NA)) %>%
            na.omit()
        nb.cols <- 100
        mycolors_nasdaq <- colorRampPalette(brewer.pal(8, "Accent"))(nb.cols)
        
        x <- list(
            title = "Date"
        )
        plot_ly(data,type="candlestick",x=data$date,
                open = data$price.open, close = data$price.close,
                high = data$price.high, low = data$price.low, color=factor(data$ticker),name=data$ticker,
                text = ~company_name, 
                hoverinfo = text,
                colors = mycolors_nasdaq) %>%
            layout(legend = list(orientation = 'h', x = 0.5, y = 1,
                                 xanchor = 'center', yref = 'paper',
                                 font = list(size = 10),
                                 bgcolor = 'transparent'),
                   xaxis = x) %>%
            add_lines(x = ~data$date, y = data$movavg, name = "Mv Avg",
                      line = list(color = '#E377C2', width = 0.75),
                      hoverinfo = "none", inherit = F) 
    }) 
    output$nasdaq_line_plot <- renderPlotly({
        data <- reactive_data_nasdaq() %>%
            group_by(ticker) %>%
            mutate(movavg = rollmean(price.adjusted, k = 5, fill = NA))
        nb.cols <- 100
        mycolors_nasdaq <- colorRampPalette(brewer.pal(8, "Accent"))(nb.cols)
        
        x <- list(
            title = "Date"
        )
        
        plot_ly(data,type="scatter",mode='lines',x=data$date,y=data$price.adjusted,color=factor(data$ticker),name=data$ticker,
                text = ~company_name, 
                hoverinfo = text,
                colors = mycolors_nasdaq) %>%
            layout(legend = list(orientation = 'h', x = 0.5, y = 1,
                                 xanchor = 'center', yref = 'paper',
                                 font = list(size = 10),
                                 bgcolor = 'transparent'),
                   xaxis = x) %>%
            add_lines(x = ~data$date, y = data$movavg, name = "Mv Avg",
                      line = list(color = '#E377C2', width = 0.75),
                      hoverinfo = "none", inherit = F) 
    })
    
    output$nasdaq_table <- function() {
        req(input$TickerInput_nasdaq)
        
        nasdaq_data %>%
            filter(ticker %in% input$TickerInput_nasdaq) %>%
            filter(date >= last(date) - 7) %>%
            group_by(ticker) %>%
            mutate(daily_adjusted_price_return = round((((price.adjusted / lag(price.adjusted)) - 1) * 100), digits = 2)) %>%
            mutate(volume = color_bar("orange")(volume),
                   company_name = cell_spec(company_name, "html", bold = T),
                   daily_adjusted_price_return = ifelse(daily_adjusted_price_return >= 0,
                                                        color_tile("transparent", "Green")(daily_adjusted_price_return),
                                                        color_tile("transparent", "Red")(daily_adjusted_price_return))) %>%
            na.omit() %>%
            select(ticker, company_name, everything()) %>%
            select(., -price.low, -price.high) %>%
            rename(Ticker = ticker, `Company Name` = company_name, 
                   `Opening Price` = price.open, `Closing Price` = price.close,
                   `Adjusted Price` = price.adjusted, Volume = volume,
                   `Daily Adjusted Price Return %` = daily_adjusted_price_return,
                   Date = date) %>%
            knitr::kable("html", escape = F, align = c("l", "l", "l", "c", "c", "c", "r", "c")) %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed",
                                                "responsive"),
                          full_width = TRUE)
        }

}

# Run the application 
shinyApp(ui = ui, server = server)
