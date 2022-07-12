#' tabbed_dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tabbed_dashboard_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # tabbox
    shinydashboard::tabBox(
      title = "Dataset Dashboard",
      width = NULL,
      side = "right",
      
      # show different views of dataset on different tab panels
      
      # all unreleased data
      tabPanel("Unreleased", 
               mod_datatable_dashboard_ui(ns("datatable_dashboard_unreleased"))),
      
      # unreleased, no embargo, passing all checks
      # aka ready for release
      tabPanel("Ready for release",
               mod_datatable_dashboard_ui(ns("datatable_dashboard_ready"))),

      # released_scheduled = NA
      tabPanel("Not scheduled",
               mod_datatable_dashboard_ui(ns("datatable_dashboard_not_scheduled"))),

      # all
      tabPanel("All",
               mod_datatable_dashboard_ui(ns("datatable_dashboard_all"))),

      # released = TRUE
      tabPanel("Previously released",
               mod_datatable_dashboard_ui(ns("datatable_dashboard_archive")))
    )
  )
}
    
#' tabbed_dashboard Server Functions
#'
#' @noRd 
mod_tabbed_dashboard_server <- function(id, df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # qc_columns
    qc_cols <- c("Standard_Compliance", "QC_Compliance", "PHI_Detection_Compliance", 
                 "Access_Controls_Compliance", "Data_Portal")
    
    ## FIXME : move this outside of datatable function
    # get todays date
    today <- Sys.Date()
    
    # floor date (only care about month/year)
    today <- lubridate::floor_date(today, unit = "month")
    
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
    
    mod_datatable_dashboard_server("datatable_dashboard_all",
                                   all_datasets,
                                   jsonlite::read_json("inst/datatable_dashboard_config.json"))
    
    mod_datatable_dashboard_server("datatable_dashboard_unreleased",
                                   unreleased_datasets,
                                   jsonlite::read_json("inst/datatable_dashboard_config.json"))
    
    mod_datatable_dashboard_server("datatable_dashboard_not_scheduled",
                                   not_scheduled_datasets,
                                   jsonlite::read_json("inst/datatable_dashboard_config.json"))
    
    mod_datatable_dashboard_server("datatable_dashboard_ready",
                                   all_checks_passed_datasets,
                                   jsonlite::read_json("inst/datatable_dashboard_config.json"))
    
    mod_datatable_dashboard_server("datatable_dashboard_archive",
                                   released_datasets,
                                   jsonlite::read_json("inst/datatable_dashboard_config.json"))
  })
}
    
## To be copied in the UI
# mod_tabbed_dashboard_ui("tabbed_dashboard_1")
    
## To be copied in the server
# mod_tabbed_dashboard_server("tabbed_dashboard_1")
