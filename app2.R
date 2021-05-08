library(shiny)
library(shinydashboard)
library(shinyauthr)
library(dplyr)
library(shinyjs)
library(DT)

user_base <- readRDS("user_base.rds")

ui <- dashboardPage(
  
  # put the shinyauthr logout ui module in here
  dashboardHeader(
    title = "shinyauthr",
    tags$li(class = "dropdown", style = "padding: 8px;", shinyauthr::logoutUI("logout"))
  ),
  
  # setup a sidebar menu to be rendered server-side
  dashboardSidebar(
    collapsed = TRUE, sidebarMenuOutput("sidebar")
  ),
  
  
  dashboardBody(
    shinyjs::useShinyjs(),
    
    # put the shinyauthr login ui module here
    shinyauthr::loginUI("login"),
    
    # setup any tab pages you want after login here with uiOutputs
    tabItems(
      tabItem("tab1", uiOutput("tab1_ui")),
      tabItem("tab2", uiOutput("tab2_ui"))
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
  
  # only when credentials()$user_auth is TRUE, render your desired sidebar menu
  output$sidebar <- renderMenu({
    req(credentials()$user_auth)
    sidebarMenu(
      id = "tabs",
      menuItem("Storms Data", tabName = "tab1"),
      menuItem("Starwars Data", tabName = "tab2")
    )
  })
  
  # tab 1 UI and output ----------------------------------------
  output$tab1_ui <- renderUI({
    req(credentials()$user_auth)
    DT::DTOutput("table1")
  })
  
  output$table1 <- DT::renderDT({
    DT::datatable(dplyr::storms, options = list(scrollX = TRUE))
  })
  
  # tab 2 UI and output ----------------------------------------
  output$tab2_ui <- renderUI({
    req(credentials()$user_auth)
    DT::DTOutput("table2")
  })
  
  output$table2 <- DT::renderDT({
    DT::datatable(dplyr::starwars[,1:10], options = list(scrollX = TRUE))
  })
  
}

shiny::shinyApp(ui, server)