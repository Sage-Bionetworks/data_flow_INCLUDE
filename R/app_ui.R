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
    
    # define colors for icons in datatable
    # green check
    tags$style(".fa-check {color:#58A158}"),
    # red x
    tags$style(".fa-times {color:#B2242A}"),
    
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
          
          # dataset view dashboard tab
          shinydashboard::tabItem(tabName = "dataset-dashboard",
                                  fluidPage(
                                    mod_tabbed_dashboard_ui("tabbed_dashboard_1"))
                                  ),
          
          # administrate tab
          shinydashboard::tabItem(tabName = "administrate",
                                  
                                  fluidPage(
                                    

                                    # initialize waiter + use preloader
                                    waiter::use_waiter(),
                                    waiter::waiter_preloader(html = tagList(
                                      img(src = "www/loading.gif"),
                                      h4("Retrieving Synapse information...")),
                                      color = "#424874"),
                                    
                                    mod_dataset_selection_ui("dataset_selection_1"),
                                    
                                    br(),
                                    
                                    mod_update_data_flow_status_ui("update_data_flow_status_1"))
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