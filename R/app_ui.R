#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic
    
    # dashboardPage
    shinydashboard::dashboardPage(
      skin = "purple",
      
      # dashboardHeader
      shinydashboard::dashboardHeader(
        title = "Release Administrator"
      ),
      
      # dashboardSidebar
      shinydashboard::dashboardSidebar(
        
        #sidebarMenu
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Administrative", 
                                   tabName = "administrate",
                                   icon = icon("cog")),
          shinydashboard::menuItem("Dashboard", 
                                   tabName = "dashboard",
                                   icon = icon("dashboard"))
        )
      ),
      
      #dashboardBody
      shinydashboard::dashboardBody(
        
        # initialize shinyjs
        shinyjs::useShinyjs(),
        
        # dashboardTabItems
        shinydashboard::tabItems(
          
          # administrate tab
          shinydashboard::tabItem(tabName = "administrate",
                                  # dataset and file selection
                                  fluidPage(
                                    mod_dataset_selection_ui("dataset_selection_ui_1"),
                                    mod_file_selection_ui("file_selection_ui_1")),
                                  
                                  #set status
                                  wellPanel(
                                    h3("Select Status"),
                                    fluidPage(
                                      radioButtons("select_status",
                                                   label = "Select status",
                                                   choices = c("Quarantine", "Upcoming Release")
                                                   ),
                                      actionButton("status_btn",
                                                   label = "Submit")
                                      )
                                    )
                                  ),
          
          # dashboard tab
          shinydashboard::tabItem(tabName = "dashboard",
                                  h2("Coming Soon: Dashbaord!"))
          )
        )
      )
    )
  }

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'release_administratorUI'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}