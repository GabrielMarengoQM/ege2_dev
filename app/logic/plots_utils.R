# plots_utils.R
box::use(
  shiny[NS, fluidRow, column, req, selectInput, textAreaInput,
        checkboxInput, p, bindCache],
  purrr[map_dfr, map, imap],
  dplyr[...],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, toWebGL, add_trace,
         highlight],
  stats[as.formula],
  bslib[card, card_body, card_footer, card_title],
  htmltools[a, p, div, h6],
  shinycssloaders[withSpinner]
)

#' @export
barChartUI <- function(plot_ids, select_args, footers, titles, id, session) {
  ns <- NS(id)
  fluidRow(
    selectInput(session$ns('gene_list_picker'), 'Select gene list',
                choices = names(select_args),
                selected = names(select_args),
                multiple = TRUE),
    imap(
      plot_ids, ~ {
        title <- titles[[.y]]
        # print(title)
        url <- footers[[.y]]
        text <- names(footers)[[.y]]
        column(
          width = 3, # 4 vis per row
          card(
            card_title(
              htmltools::h6(title, style = "text-align: center;")
            ),
            card_body(
              plotlyOutput(
                session$ns(.x)
              ) %>% withSpinner(color="#0dc5c1"),
            ),
            card_footer(
              htmltools::div(
                style = "text-align: center;",
                htmltools::p('Source: ', style = "display: inline; margin-right: 5px;"),
                htmltools::a(href = url, text, target = "_blank", style = "display: inline;")
              )
            ),
            full_screen = TRUE
          )
        )
      }
    )
  )
}

#' @export
violinPlotUI <- function(plot_ids, select_args, footers, titles, id, session) {
  ns <- NS(id)
  fluidRow(
    selectInput(session$ns('gene_list_picker'), 'Select gene list',
                choices = names(select_args),
                selected = names(select_args),
                multiple = TRUE),
    textAreaInput(session$ns('highlight_genes'), 'Enter genes to highlight'),
    checkboxInput(session$ns('toggle_points'), 'Show all data points', value = FALSE),
    imap(
      plot_ids, ~ {
        title <- titles[[.y]]
        # print(title)
        url <- footers[[.y]]
        text <- names(footers)[[.y]]
        column(
          width = 3, # 4 vis per row
          card(
            card_title(
              htmltools::h6(title, style = "text-align: center;")
            ),
            card_body(
              plotlyOutput(
                session$ns(.x)
              ) %>% withSpinner(color="#0dc5c1"),
            ),
            card_footer(
              htmltools::div(
                style = "text-align: center;",
                htmltools::p('Source: ', style = "display: inline; margin-right: 5px;"),
                htmltools::a(href = url, text, target = "_blank", style = "display: inline;")
              )
            ),
            full_screen = TRUE
          )
        )
      }
    )
  )
}

#' @export
barChartServer <- function(plot_ids, data, gene_lists, plot_func, output) {
  plots <- purrr::imap(
    plot_ids, ~ {
      data_point <- data[[.x]]
      # axis_title <- axis_titles[[.y]]
      output[[.x]] <- renderPlotly({
        # if (is.factor(data_point) || is.character(data_point)) {
        plot_func(gene_lists, data, .x)
      })
    }
  )
}

#' @export
violinPlotServer <- function(plot_ids, data, gene_lists, plot_func, axis_titles, genes_to_highlight, threshold_values, toggle_option, output) {
  plots <- purrr::imap(
    plot_ids, ~ {
      data_point <- data[[.x]]
      current_threshold_value <- threshold_values[[.y]]
      output[[.x]] <- renderPlotly({
        if (is.numeric(data_point)) {
          # Extract the threshold value using the index, handle NULL or missing indices
          plot_data <- plot_func(gene_lists, data, .x, genes_to_highlight, current_threshold_value, toggle_option)
          plot <- plot_data[[1]]
          data <- plot_data[[2]]
          if (length(genes_to_highlight) > 0) {
            highlighted_data <- data[data$gene_symbol %in% genes_to_highlight, ]
            plot <- plot %>%
              add_trace(
                data = highlighted_data,
                x = ~gene_list_name,
                y = as.formula(paste("~", .x)),
                type = "scatter",
                mode = "markers",
                marker = list(color = "black", size = 10),
                name = "Highlighted Genes"
              )
          } else {
            plot
          }
        }
      }) %>%
        bindCache(
          c(
            genes_to_highlight,
            gene_lists,
            toggle_option
          )
        )
    }
  )
}

#' @export
barChart <- function(gene_list, data_source, data_col) {

  if (is.factor(data_source[[data_col]])) {

    combined_dataframe <- map_dfr(seq_along(gene_list), function(i) {
      list_name <- names(gene_list)[i]
      num <- length(setdiff(gene_list[[i]], data_source[["gene_symbol"]]))
      df <- data_source %>%
        dplyr::filter(gene_symbol %in% gene_list[[i]]) %>%
        dplyr::select(gene_symbol, !! sym(data_col)) %>%
        dplyr::distinct() %>%
        dplyr::count(!! sym(data_col)) %>%
        dplyr::mutate(percentage = round(n/sum(n)*100, 2)) %>%
        dplyr::mutate(name = list_name)
    })

    plot <- plot_ly(combined_dataframe, x = as.formula(paste("~", data_col)), y = ~percentage, color = ~name,
                    textposition = 'outside', text = ~percentage, type = "bar")
  } else if (is.character(data_source[[data_col]])) {
    combined_dataframe <- map_dfr(seq_along(gene_list), function(i) {
      list_name <- names(gene_list)[i]
      num <- length(setdiff(gene_list[[i]], data_source[["gene_symbol"]]))
      df <- data_source %>%
        dplyr::filter(gene_symbol %in% gene_list[[i]]) %>%
        dplyr::select(gene_symbol, !! sym(data_col)) %>%
        dplyr::mutate(current_col = ifelse(is.na(!! sym(data_col)), "NA", "nonNA")) %>%
        dplyr::select(gene_symbol, current_col)  %>%
        dplyr::distinct() %>%
        dplyr::group_by(current_col) %>%
        dplyr::summarize(count = n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(percentage = round((count / sum(count)) * 100, 2)) %>%
        dplyr::mutate(name = list_name)

    })
    # print(combined_dataframe)
    plot <- plot_ly(combined_dataframe, x = ~current_col, y = ~percentage, color = ~name,
                    textposition = 'outside', text = ~percentage, type = "bar")

  }

  plot <- plot %>%
    layout(
      xaxis = list(title = ""),
      # xaxis = list(title = custom_x_axis_title),
      yaxis = list(title = "Percentage of genes"),
      showlegend = TRUE
    )
}

#' @export
violinPlot <- function(gene_lists, raw_data, column, genes_to_highlight, threshold_value, toggle_option) {
  data <- do.call(rbind, lapply(names(gene_lists), function(gene_list_name) {
    genes <- gene_lists[[gene_list_name]]
    metrics_data <- raw_data[raw_data$gene_symbol %in% genes, c('gene_symbol', column)] %>%
      distinct()
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

  # # Add highlight points for individual genes
  # if (length(genes_to_highlight > 0)) {
  #   violin_plot <- violin_plot %>%
  #     highlight("plotly_selecting") %>%
  #     # Add points for highlighting
  #     add_trace(
  #       data = data[data$gene_symbol %in% genes_to_highlight, ],  # Only include specified genes
  #       type = "scatter",
  #       mode = "markers",
  #       x = ~gene_list_name,
  #       y = as.formula(paste("~", column)),
  #       text = ~paste("Gene: ", gene_symbol, "<br>", column, ": ", get(column)),
  #       marker = list(color = "black", size = 10),
  #       hoverinfo = "text",
  #       name = "Searched Genes"  # Legend entry for the added trace
  #     )
  # }

  # Remove x-axis title
  violin_plot <- violin_plot %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Score"),
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

  return(list(violin_plot, data))
}
