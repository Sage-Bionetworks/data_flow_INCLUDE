#' stacked_bar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_stacked_bar_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::plotOutput(ns("stacked_bar"))
  )
}
    
#' stacked_bar Server Functions
#'
#' @noRd 
mod_stacked_bar_server <- function(id,
                                   df,
                                   x_var,
                                   y_var,
                                   fill_var,
                                   title = NULL,
                                   x_lab = NULL,
                                   y_lab = NULL,
                                   colors = NULL,
                                   x_line = NULL,
                                   coord_flip = FALSE) {
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$stacked_bar <- shiny::renderPlot({
      df <- df()
      
      # base plot
      bar <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill_var]])) +
        
        ggplot2::geom_bar(stat = "identity", position = "fill")
      
      # add x intercept
      if (!is.null(x_line)) {
        bar <- bar +
          ggplot2::geom_vline(xintercept = x_line, linetype = 2, colour = "black")
      }
      
      # add styling/labs
      bar <- bar +
        ggplot2::labs(title = title, x = x_lab, y = y_lab) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
      
      # add custom color
      if (!is.null(colors)) {
        bar <- bar +
          ggplot2::scale_fill_manual(values = colors)
      }
      
      # flip coordinates
      if (coord_flip) {
        bar <- bar +
          ggplot2::coord_flip()
      }
      
      bar
    })
 
  })
}
    
## To be copied in the UI
# mod_stacked_bar_ui("stacked_bar_1")
    
## To be copied in the server
# mod_stacked_bar_server("stacked_bar_1")
