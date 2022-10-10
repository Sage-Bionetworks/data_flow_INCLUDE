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
                                    group_by_var,
                                    title = NULL,
                                    x_lab = NULL,
                                    y_lab = NULL,
                                    fill = "#0d1c38"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #df <- df()
    
    # group and tally data
    plot_df <- df %>%
      dplyr::group_by(.data[[group_by_var]]) %>%
      dplyr::tally()
    
    # create distribution plot
    dist <- ggplot2::ggplot(plot_df,
                            ggplot2::aes(x = reorder(.data[[group_by_var]], -n, ), y = n)) +
      
      ggplot2::geom_bar(stat = "identity", fill = fill) +
      
      ggplot2::labs(title = title, x = x_lab, y = y_lab) +
      
      ggplot2::theme_minimal() +
      
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90,hjust=1)) 
    
    # render plot
    output$distribution_plot <- shiny::renderPlot({
      dist
    })
  })
}
    
## To be copied in the UI
# mod_distribution_ui("distribution_1")
    
## To be copied in the server
# mod_distribution_server("distribution_1")
