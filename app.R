

# load the required packages
library(shiny)
require(shinydashboard)
library(tidyverse)

# set the working directory and load data
currentwd <- getwd()
setwd(currentwd)
bank_names <- read.csv('bank_names.csv',stringsAsFactors = F,header=T)
bank_records <- read.csv('bank_records.csv',stringsAsFactors = F,header=T)

bank_list <- bank_names %>%
    collect() %>%
    split(.$bank) %>%
    map(~ .$bankid)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "DDP Course Project")


#Sidebar content of the dashboard

sidebar <- dashboardSidebar(

    selectInput(
        inputId = "bank",
        label = "Select a Bank:",
        choices = bank_list,
        selected = "1",
        selectize = FALSE
    )
)

frow1 <- fluidRow(
    valueBoxOutput("value1"),
    valueBoxOutput("value2"),
    valueBoxOutput("value3")
)

frow2 <- fluidRow(
    box(
        title = "Revenue Per Selected Bank",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("revenuebyBank", height = "300px")
    ),

    box(
        title = "Revenue Per Region",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("revenuebyRegion", height = "300px")
    )
)

frow3 <- fluidRow(
    box(
        title = "Revenue Per Region",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("revenuebyPrd", height = "300px")
    )
)

# combine the two fluid rows to make the body

body <- dashboardBody(frow1, frow2, frow3)

#completing the ui part with dashboardPage

ui <- dashboardPage(title = 'Bank Dashboard', header, sidebar, body, skin='red')

##################################################
# create the server functions for the dashboard

server <- function(input, output) {

#some data manipulation to derive the values of KPI boxes

    total.revenue   <- sum(bank_records$revenue)

    sales.bank      <- bank_records %>%
                        group_by(bank) %>%
                        summarise(value = sum(revenue)) %>%
                        filter(value==max(value))

    prof.prod       <- bank_records %>%
                        group_by(product) %>%
                        summarise(value = sum(revenue)) %>%
                        filter(value==max(value))

    dat <- reactive({
        plot3data <- bank_records %>%
                        filter(bankid==input$bank)
        #print(plot3data)
        #plot3data

    })

#creating the valueBoxOutput content

    output$value1 <- renderValueBox({
        valueBox(
            formatC(sales.bank$value, format="d", big.mark=','),
            paste('Top Bank:',sales.bank$bank),
            icon = icon("stats",lib='glyphicon'),
            color = "purple")
    })

    output$value2 <- renderValueBox({
        valueBox(
            formatC(total.revenue, format="d", big.mark=','),
            'Total Expected Revenue',
            icon = icon("usd",lib='glyphicon'),
            color = "green")
    })

    output$value3 <- renderValueBox({
        valueBox(
            formatC(prof.prod$value, format="d", big.mark=','),
            paste('Top Product:',prof.prod$product),
            icon = icon("menu-hamburger",lib='glyphicon'),
            color = "yellow")
    })

#creating the plotOutput content

    output$revenuebyPrd <- renderPlot({
        ggplot(data = bank_records,
               aes(x=product, y=revenue, fill=factor(region))) +
            geom_bar(position = "dodge", stat = "identity") +
            ylab("Revenue (in USD)") +
            xlab("Product") +
            theme(legend.position="bottom",
                  plot.title = element_text(size=15, face="bold")) +
            ggtitle("Revenue by Product") + labs(fill = "Region")
        })

    output$revenuebyRegion <- renderPlot({
        ggplot(data = bank_records,
               aes(x=bank, y=revenue, fill=factor(region))) +
            geom_bar(position = "dodge", stat = "identity") +
            ylab("Revenue (in USD)") +
            xlab("Bank") +
            theme(legend.position="bottom",
                  plot.title = element_text(size=15, face="bold")) +
            ggtitle("Revenue by Bank") + labs(fill = "Region")
    })



    output$revenuebyBank <- renderPlot({
        ggplot(data = dat(), aes(x=bank)) +
            geom_bar(aes(y=revenue, fill=factor(region)), position = "dodge", stat = "identity") +
            ylab("Revenue (in USD)") +
            xlab("Selected Bank") +
            theme(legend.position="bottom",
                  plot.title = element_text(size=15, face="bold")) +
            ggtitle("Revenue by Region") + labs(fill = "Region")
    })
}

shinyApp(ui, server)
