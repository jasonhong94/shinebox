library(shiny)          # web app framework 
library(shinyauthr)     # shiny authentication modules
library(shinyjs)        # improve user experience
library(shinymanager)   # shiny authentication modules
library(shinythemes)    # themes for shiny
library(shinydashboard)
library(sodium)         # crypto library
library(tidyverse)
library(dplyr)
library(arules)
library(arulesViz)
library(visNetwork)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(RColorBrewer)
library(plotly)
library(DT)

# load data
data <- read.csv("C:/Users/tansiahong/Desktop/shintech/r-shiny/data/trans.csv",
                 sep = ",")

data$InvoiceDate <- as.Date(data$InvoiceDate, format = "%Y-%m-%d")

# sample logins dataframe with passwords hashed by sodium package
user_base <- readRDS("user_base.rds")

# market basket start---------------------------------------------------
tr <- read.transactions("C:/Users/tansiahong/Desktop/shintech/r-shiny/data/market_basket_transactions.csv",
                        format = 'basket',
                        quote="",
                        sep = ',')


# market basket end-----------------------------------------------------

ui <- dashboardPage(
    
    # put the shinyauthr logout ui module in here
    dashboardHeader(
        title = "ShinTech Analytics",
        tags$li(class = "dropdown", style = "padding: 8px;", actionButton("logout", "Logout"))
    ),
      
    
    # setup a sidebar menu to be rendered server-side
    dashboardSidebar(
        collapsed = TRUE, sidebarMenuOutput("sidebar")
    ),
    
    
    dashboardBody(
        shinyjs::useShinyjs(),
        
        shinyauthr::loginUI("login"),
        
        
        # setup any tab pages you want after login here with uiOutputs
        tabItems(
            
            tabItem("sales-analysis", 
                    fluidRow(
                        valueBoxOutput("totalsales"),
                        valueBoxOutput("totalqty"),
                        valueBoxOutput("totaltrans")
                    ),
                    
                    fluidRow(
                        box(title = "Top Customer Spending", background = "olive",
                            numericInput("num1", "Top N Customer",
                                        value = 10, min = 5, max = 20, step = 5),
                            plotlyOutput("bar1")
                        ),
                        
                        box(title = "Top Product Sold", background = "orange",
                            numericInput("num2", "Top N Product",
                                         value = 10, min = 5, max = 20, step = 5),
                            plotlyOutput("bar2")
                        )
                    ),
                    
                    fluidRow(
                        box(width=3, background = "maroon",
                            dateRangeInput("drange1", "Select Date Range:",
                                           start="2010-12-01", end="2012-12-31")
                        ),
                        box(title = "Total Daily Sales", background = "maroon",
                            solidHeader = TRUE, width = 9,
                            plotlyOutput("line1")
                           
                        )
                    )
            ),
            
            tabItem("market-basket",
                    fluidRow(
                        
                        box(plotOutput("market1")),
                        box(plotlyOutput("market2")),
                        box(visNetworkOutput("market3"))
                    )
            )
        )
    )
)

server <- function(input, output, session) {
    
    # login status and info will be managed by shinyauthr module and stores here
    credentials <- callModule(shinyauthr::login, "login", 
                              data = user_base,
                              user_col = user,
                              pwd_col = password,
                              sodium_hashed = TRUE,
                              log_out = reactive(logout_init()))
    
    # logout status managed by shinyauthr module and stored here
    logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
    
    # this opens or closes the sidebar on login/logout
    observe({
        if(credentials()$user_auth) {
            shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
            
        } else {
            shinyjs::addClass(selector = "body", class = "sidebar-collapse")
        }
    })
    
    observeEvent(input$logout,{
        session$reload()
    })
    
    
    # only when credentials()$user_auth is TRUE, render your desired sidebar menu
    output$sidebar <- renderMenu({
        req(credentials()$user_auth)
        sidebarMenu(
            id = "tabs",
            menuItem("Sales Analysis", tabName = "sales-analysis"),
            menuItem("Market Basket Analysis", tabName = "market-basket")
        )
    })
    
    # sales-analysis UI and output ----------------------------------------
    output$totalsales <- renderValueBox({
        tolsales <- data %>%
            {.[,6]*.[,8]} %>%
            sum %>%
            prettyNum(big.mark = ",")
        
        valueBox(
            tolsales, "Total Sales", icon = icon("credit-card"),
            color = "green"
        )
    })
    
    output$totalqty <- renderValueBox({
        tolqty <- data %>%
            select(Quantity) %>%
            sum %>%
            prettyNum(big.mark = ",")
        
        valueBox(
            tolqty, "Total Quantity Sold", icon = icon("thumbs-up"),
            color = "blue"
        )
    })
    
    output$totaltrans <- renderValueBox({
        toltrans <- nrow(data)
        
        valueBox(
            toltrans, "Total Transactions", icon = icon("list"),
            color = "purple"
        )
    })
    
    output$bar1 <- renderPlotly({
        req(credentials()$user_auth)
        
        data$TotalPrice <- data %>%
            {.[,6]*.[,8]}
        
        topspend <- aggregate(data$TotalPrice, 
                              by=list(Category=data$CustomerID), FUN=sum)
        
        topcust <- head(topspend[order(topspend$x, decreasing= T),], n = input$num1)
        topcust$Category <- as.character(topcust$Category)
        
        p_bar_1 <- ggplot(topcust, aes(x=reorder(Category, x), y=x, fill=Category)) +
            geom_bar(stat="Identity") +
            labs(x = "Customer ID", y = "Sales Amount")
            
        p_bar_1 <- p_bar_1 + coord_flip() + theme(legend.position="none")
        
        ggplotly(p_bar_1)
    })
    
    output$bar2 <- renderPlotly({
        req(credentials()$user_auth)
    
        topitem <- aggregate(data$Quantity, 
                              by=list(Category=data$Description), FUN=sum)
        
        topproduct <- head(topitem[order(topitem$x, decreasing= T),], n = input$num2)
        
        p_bar_2 <- ggplot(topproduct, aes(x=reorder(Category, x), y=x, fill=Category)) +
            geom_bar(stat="Identity") +
            labs(x = "Item", y = "Quantity Sold")
        
        p_bar_2 <- p_bar_2 + coord_flip() + theme(legend.position="none")
        
        ggplotly(p_bar_2)
    })
    
    output$line1 <- renderPlotly({
        req(credentials()$user_auth)
        
        df1 <- data %>%
            select(InvoiceDate, UnitPrice) %>%
            filter(InvoiceDate >= input$drange1[1] & InvoiceDate <= input$drange1[2])
        
        df1_sum <- aggregate(df1$UnitPrice, by=list(df1$InvoiceDate), sum)
        
        colnames(df1_sum) <- c("InvoiceDate", "UnitPrice")
        
        p1 <- ggplot(df1_sum, ) +
            geom_line(mapping=aes(x = InvoiceDate, y = UnitPrice))+
            geom_point(mapping=aes(x = InvoiceDate, y = UnitPrice)) +
            labs(x = "Date", y = "Amount")
        
        ggplotly(p1)
        
    })
    
    # market basket UI and output ----------------------------------------
    output$market1 <- renderPlot({
        req(credentials()$user_auth)
        
        tr1 <- tr
        
        itemFrequencyPlot(tr1, topN=20, type="absolute",
                          col=brewer.pal(8, 'Pastel2'),
                          main="Item Frequency Plot")
    })
    
    output$market2 <- renderPlotly({
        req(credentials()$user_auth)
        
        association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8, maxlen=10))
        
        subRules <- association.rules[quality(association.rules)$confidence > 0.8]
        
        plotly_arules(subRules)
    })
    
    output$market3 <- renderVisNetwork({
        req(credentials()$user_auth)
        
        association.rules2 <- apriori(tr, parameter = list(supp=0.001, conf=0.8, maxlen=10))
        
        subRules <- association.rules2[quality(association.rules2)$confidence > 0.8]
        
        top10subRules <- head(subRules, n = 10, by = "confidence")
        
        plot(top10subRules, method = "graph",  engine = "htmlwidget")
    })
}

shiny::shinyApp(ui, server)