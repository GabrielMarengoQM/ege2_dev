# Plotting utils
box::use(
  shiny[NS, fluidRow, column, req],
  purrr[map_dfr, map],
  dplyr[...],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, toWebGL],
  stats[as.formula],
  bslib[card]
)

# Function to create UI dynamically
#' @export
createUI <- function(plot_ids, id, session) {
  ns <- NS(id)
  # plot_number <- 12/length(plot_ids)
  fluidRow(
    map(
      plot_ids, ~ column(
      width = 3,
      card(plotlyOutput(outputId = session$ns(.x)),
           full_screen = TRUE)
      )
    )
  )
}

# add another func for violin plot thern add if statement to plot barchart if character or factor and
# violin plot if numerical - then workout how to add sidebar

# Function to create server plot logic dynamically
#' @export
plotServer <- function(plot_ids, data, gene_lists, bar_chart_func, violin_plot_func, genes_to_highlight, threshold_value, toggle_option, output) {
  plots <- map(
    plot_ids, ~ {
      data_point <- data[[.x]]
      output[[.x]] <- renderPlotly({
        if (is.factor(data_point) | is.character(data_point)) {
          bar_chart_func(gene_lists, data, .x, .x)
        } else if (is.numeric(data_point)) {
          violin_plot_func(gene_lists, data, .x, .x, genes_to_highlight, threshold_value, toggle_option)
        }
      })
    }
  )
}

#' @export
barChart <- function(gene_list, data_source, data_col, custom_x_axis_title) {
  combined_dataframe <- map_dfr(seq_along(gene_list), function(i) {
    list_name <- names(gene_list)[i]
    num <- length(setdiff(gene_list[[i]], data_source[["gene_symbol"]]))

    new_impc2 <- data_source %>%
      dplyr::filter(gene_symbol %in% gene_list[[i]]) %>%
      dplyr::select(gene_symbol, !! sym(data_col)) %>%
      dplyr::distinct() %>%
      dplyr::count(!! sym(data_col)) %>%
      dplyr::mutate(percentage = round(n/sum(n)*100, 2)) %>%
      dplyr::mutate(name = list_name)
  })
  # print(combined_dataframe)
  plot <- plot_ly(combined_dataframe, x = as.formula(paste("~", data_col)), y = ~percentage, color = ~name,
                  textposition = 'outside', text = ~percentage, type = "bar")

  plot <- plot %>%
    layout(
      xaxis = list(title = custom_x_axis_title),
      yaxis = list(title = "Percentage of genes"),
      showlegend = TRUE
    )
}

#' @export
violinPlot <- function(gene_lists, raw_data, column, custom_y_axis_title, genes_to_highlight, threshold_value, toggle_option) {
  data <- do.call(rbind, lapply(names(gene_lists), function(gene_list_name) {
    genes <- gene_lists[[gene_list_name]]
    metrics_data <- raw_data[raw_data$gene_symbol %in% genes, c('gene_symbol', column)]
    metrics_data$gene_list_name <- gene_list_name
    return(metrics_data)
  }))

  if (toggle_option == TRUE) {
    points_setting <- "all"
  } else {
    points_setting <- "none"
  }

  # Your existing code for creating the violin plots
  violin_plot <- plot_ly(
    data,
    y = as.formula(paste("~", column)),
    x = ~gene_list_name,
    type = "violin",
    box = list(visible = T),
    points = points_setting,
    name = ~gene_list_name,
    text = ~paste("Gene: ", gene_symbol, "<br>", column, ": ", get(column)),
    hoverinfo = "text"  # Include gene symbol and metric value in hover text
  )

  # Add highlight points for individual genes
  if (length(genes_to_highlight > 0)) {
    violin_plot <- violin_plot %>%
      highlight("plotly_selecting") %>%
      # Add points for highlighting
      add_trace(
        data = data[data$gene_symbol %in% genes_to_highlight, ],  # Only include specified genes
        type = "scatter",
        mode = "markers",
        x = ~gene_list_name,
        y = as.formula(paste("~", column)),
        text = ~paste("Gene: ", gene_symbol, "<br>", column, ": ", get(column)),
        marker = list(color = "black", size = 10),
        hoverinfo = "text",
        name = "Searched Genes"  # Legend entry for the added trace
      )
  }

  # Remove x-axis title
  violin_plot <- violin_plot %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = custom_y_axis_title),
      showlegend = TRUE
    )

  hline <- function(y = 0, color = "grey") {
    list(
      type = "line",
      x0 = 0,
      x1 = 1,
      xref = "paper",
      y0 = y,
      y1 = y,
      line = list(dash = "dash", color = color)
    )
  }

  # Modified hline function to accept multiple threshold values
  hlines <- function(y = 0, color = "grey") {
    if (length(y) == 1) {
      return(list(hline(y, color)))
    } else {
      lines <- lapply(y, function(y_val) hline(y_val, color))
      return(lines)
    }
  }

  # Check if threshold_value is not NULL and is a vector
  if (!is.null(threshold_value) && is.vector(threshold_value)) {
    violin_plot <- violin_plot %>%
      layout(
        shapes = hlines(threshold_value)
      )
  }

  violin_plot %>% toWebGL()

  return(violin_plot)
}
