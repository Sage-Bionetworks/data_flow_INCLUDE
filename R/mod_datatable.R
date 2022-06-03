#' datatable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList

mod_datatable_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # tabbox
    shinydashboard::tabBox(
      title = "Dataset Dashboard",
      width = NULL,
      side = "right",
      
      # show different views of dataset on different tab panels
      
      # all unreleased data
      tabPanel("All unreleased datasets", 
               DT::DTOutput(ns("unreleased_dash"))),
      
      # all
      tabPanel("All datasets", 
               DT::DTOutput(ns("all_dash"))),
      
      # unreleased, no embargo, passing all checks
      # aka ready for release
      tabPanel("Ready for release", 
               DT::DTOutput(ns("all_checks_passed_dash"))),
      
      # released_scheduled = NA
      tabPanel("Not scheduled for release", 
               DT::DTOutput(ns("not_scheduled_dash"))),
      
      # released = TRUE
      tabPanel("Previously released datasets", 
               DT::DTOutput(ns("released_dash")))
    )
 
  )
}
    
#' datatable Server Functions
#'
#' @noRd 
mod_datatable_server <- function(id, df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # get todays date
    today <- Sys.Date()
    
    # floor date (only care about month/year)
    today <- lubridate::floor_date(today, unit = "month")
    
    # qc_columns
    qc_cols <- c("Standard_Compliance", "QC_Compliance", "PHI_Detection_Compliance", 
                 "Access_Controls_Compliance", "Data_Portal")
    
    # add a column to df of TRUE/FALSE date is past due
    all_datasets <- reactive({
      data <- df()
      dates <- lubridate::floor_date(data$Release_Scheduled, unit = "month")
      data$past_due <- ifelse(dates < today, "pd", 
                              ifelse(dates == today, "t", NA))
      return(data)
    })
    
    # subset dataframe into various views
    unreleased_datasets <- reactive({
      data <- all_datasets()
      data[ data$Released == FALSE, ]
    })
    
    # not scheduled
    not_scheduled_datasets <- reactive({
      data <- all_datasets()
      data[ is.na(data$Release_Scheduled), ]
    })
    
    # all checks passing / no embargo / unreleased (i.e. ready for release)
    all_checks_passed_datasets <- reactive({
      data <- all_datasets()
      
      # which rows have all qc_cols == TRUE
      passing <- apply(data[ , qc_cols ], 1, all)
      
      # which rows have passed their embargo date or are NA
      no_embargo <- data$Embargo <= today | is.na(data$Embargo)
      
      # which rows are unreleased
      unreleased <- data$Released == FALSE
      
      # which rows are passing qc, past/have no embargo, and are unreleased
      ready <- passing & no_embargo
      
      
      # subset
      data[ ready, ]
    })
    
    # previously released
    released_datasets <- reactive({
      data <- all_datasets()
      data[ data$Released == TRUE, ]
    })
    
    # render datatables
    
    output$all_dash <- DT::renderDataTable({
      create_dashboard(
        prep_df_for_dash(all_datasets())
        )
      })
    
    output$not_scheduled_dash <- DT::renderDataTable({
      create_dashboard(
        prep_df_for_dash(not_scheduled_datasets())
        )
    })
    
    output$unreleased_dash <- DT::renderDataTable({
      create_dashboard(
        prep_df_for_dash(unreleased_datasets())
      )
    })
    
    
    output$released_dash <- DT::renderDataTable({
      create_dashboard(
        prep_df_for_dash(released_datasets()))
    })
    
    output$all_checks_passed_dash <- DT::renderDataTable({
      create_dashboard(prep_df_for_dash(all_checks_passed_datasets()))
    })
  
    
  })
}
    
## To be copied in the UI
# mod_datatable_ui("datatable_1")
    
## To be copied in the server
# mod_datatable_server("datatable_1")
