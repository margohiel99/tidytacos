ordination_plot <- function(ord_df, color_by = "firstgenus", legend_order = NULL, 
                            palette = NULL, title = NULL, facet_by = NULL) {
  # Ensure factor
  ord_df[[color_by]] <- as.factor(ord_df[[color_by]])
  
  if (!is.null(legend_order)) {
    ord_df[[color_by]] <- factor(ord_df[[color_by]], levels = legend_order)
  }

  # Select columns to plot (PCOA or TSNE)
  coord_cols <- colnames(ord_df)[1:2]
  xlab <- coord_cols[1]
  ylab <- coord_cols[2]
  
  # Add variance (if its present)
  var_expl <- attr(ord_df, "variance_explained")
  if (!is.null(var_expl)) {
    xlab <- paste0(coord_cols[1], " (", var_expl[1], "%)")
    ylab <- paste0(coord_cols[2], " (", var_expl[2], "%)")
  }
  
  # Plot
  p <- ggplot(ord_df, aes(x = .data[[coord_cols[1]]], y = .data[[coord_cols[2]]], 
                          color = .data[[color_by]])) +
    geom_point(alpha = 0.8, size = 2) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 10)
    ) +
    labs(color = color_by, x = xlab, y = ylab, title = title)
  
  # # Palette
  # n <- length(levels(ord_df[[color_by]]))
  # if (is.function(palette)) {
  #   p <- p + scale_color_manual(values = palette(n))
  # } else if (is.character(palette)) {
  #   if (length(palette) == 1) {
  #     p <- p + scale_color_manual(values = RColorBrewer::brewer.pal(n, palette))
  #   } else {
  #     p <- p + scale_color_manual(values = palette)
  #   }
  # } else if (is.null(palette)) {
  #   p <- p + scale_color_manual(values = RColorBrewer::brewer.pal(n, "Set2"))
  # }

  # Facet
  if (!is.null(facet_by)) {
    p <- p + facet_wrap(vars(.data[[facet_by]]), ncol = 1)
  }
  
  return(p)
}


#' Return a bar plot of the samples
#'
#' Plots a stacked bar plot of the samples in the tidytacos object to inspect the taxonomic profile.
#'
#' @param ta A tidytacos object.
#' @param x The name of the column name used to represent samples on the x-axis
#' @param n An integer, representing the amount of colors used to depict
#' @param pie A boolean, whether or not to represent the profile in a pie chart.
#' Default is FALSE, as pie chart representations can be misleading to interpret.
#' @param order_by an optional column name to order the samples by.
#' For examples order_by=sample would order the x-axis by the sample names instead of by similar profiles.
#' @export
tacowidget_ord <- function(ta) {

  force_optional_dependency("shiny")
  force_optional_dependency("plotly")


  ui <- shiny::fluidPage(shiny::selectInput(
  "color_by", 
  "Color by var:", 
  choices = c("visit","pariteit","ph","vas_sum","vuas_sum","ct_treatment","ofs_by","bc_subtype","primary_tumor.c","lymph_nodes.c","firstgenus","secondgenus", "dominant_taxon"),
  selected = "firstgenus"
),
shiny::br(),
shiny::textInput("palette", "Palette to use (RColorBrewer):", value = "tab20"),
shiny::br(),
shiny::checkboxInput("facet_by_visit", "Facet by visit?", value = FALSE))

server <- function(input, output, session){
  
  output$pca_plot <- plotly::renderPlotly({
  pca_df <- add_ord(ta)
  
  facet_var <- if (input$facet_by_visit) "visit" else NULL

  
  pal <- tab20
  
  p <- ordination_plot(
    pca_df,
    color_by = input$color_by,
    palette = pal,
    facet_by = facet_var,
    title = paste("PCA coloured by", input$color_by)  
  )
  
  plotly::ggplotly(p) %>%
    layout(
    legend = list(
        orientation = "h",
        x = 0,
        y = -0.2
      )
    )})

}

  app <- shiny::shinyApp(ui = ui, server=server)
  shiny::runApp(app, launch.browser = FALSE)
}

# to test: go in tidytacos map, start R, devtools::load_all()
tab20 <- c(
  "#1f77b4","#aec7e8","#ff7f0e","#ffbb78","#2ca02c","#98df8a",
  "#d62728","#ff9896","#9467bd","#c5b0d5","#8c564b","#c49c94",
  "#e377c2","#f7b6d2","#7f7f7f","#c7c7c7","#bcbd22","#dbdb8d",
  "#17becf","#9edae5"
)

tacowidget_test <- function(serve=TRUE) {

app <- shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::numericInput("n", "n", 1),
      shiny::plotOutput("plot")
    ),
    server = function(input, output) {
      output$plot <- shiny::renderPlot( plot(head(cars, input$n)) )
    }
  )

  if (serve) {
    shiny::runApp(app, launch.browser = FALSE)
  }
  app

}