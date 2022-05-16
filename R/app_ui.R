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
          shinydashboard::menuItem("Project Overview Dashboard", 
                                   tabName = "overview-dashboard",
                                   icon = icon("dashboard")),
          shinydashboard::menuItem("Dataset Level Dashboard", 
                                   tabName = "dataset-dashboard",
                                   icon = icon("dashboard")),
          shinydashboard::menuItem("Administrative", 
                                   tabName = "administrate",
                                   icon = icon("cog"))
          
        )
      ),
      
      #dashboardBody
      shinydashboard::dashboardBody(
        
        # initialize shinyjs
        shinyjs::useShinyjs(),
        
        # dashboardTabItems
        shinydashboard::tabItems(
          
          # overview dashboard tab
          shinydashboard::tabItem(tabName = "overview-dashboard",
                                  h2("Coming Soon: Overview Dashbaord!")
                                  ),
          
          # dataset view dashboard tab
          shinydashboard::tabItem(tabName = "dataset-dashboard",
                                  fluidPage(
                                    mod_datatable_ui("datatable_1")
                                    )
                                  ),
          
          # administrate tab
          shinydashboard::tabItem(tabName = "administrate",
                                  
                                  fluidPage(
                                    
                                    # dataset_selection module
                                    mod_dataset_selection_ui("dataset_selection_ui_1"),
                                    
                                    # file_selection module
                                    mod_file_selection_ui("file_selection_ui_1"),
                                    
                                    br(),
                                    
                                    # set release status module
                                    mod_set_release_status_ui("set_release_status_ui_1"),
                                    
                                    # for testing purposes: output modified manifest
                                    DT::DTOutput("modified_manifest"))
          )
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