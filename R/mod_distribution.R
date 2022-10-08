#' distribution UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_distribution_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::plotOutput(ns("distribution_plot"))
  )
}
    
#' distribution Server Functions
#'
#' @noRd 
mod_distribution_server <- function(id,
                                    df,
                                    x_axis_var,
                                    title = NULL,
                                    x_lab = NULL,
                                    y_lab = NULL,
                                    fill = "#0d1c38"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    df <- df()
    
    plot_df <- df %>%
      group_by(contributor) %>%
      tally()
    
    ggplot2::ggplot(plot_df, 
                    aes(x = reorder(.data[[x_axis_var]], -n, ), y = n)) +
      
      geom_bar(stat = "identity", fill = fill) +
      
      labs(title = title,
           x = x_lab,
           y = y_lab) +
      
      theme_minimal() +
      
      theme(axis.text.x=element_text(angle=90,hjust=1)) 
  })
}
    
## To be copied in the UI
# mod_distribution_ui("distribution_1")
    
## To be copied in the server
# mod_distribution_server("distribution_1")
