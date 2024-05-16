# filters.r
box::use(
  shiny[moduleServer, NS, reactive,
        sidebarLayout, sidebarPanel, mainPanel, tableOutput, renderTable, actionButton,
        uiOutput, renderUI, observe, selectInput, reactiveVal, textInput, updateTextInput,
        observeEvent, updateSliderInput, updateSelectInput, tabsetPanel, tabPanel,
        updateTextAreaInput, req, updateCheckboxInput, fluidRow, column, h4,
        tagList, renderText, textOutput, isolate, p],
  purrr[map, map2, reduce, keep, discard],
  utils[head],
  fst[read.fst]
)

box::use(
  app/logic/filters_utils,
  app/view/gene_list_upload

)

#' @export
ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      # filters ----
      uiOutput(ns("filter")),
      # name list ----
      textInput(ns("name_saved_list"), "Name list"),
      # save list/filters ----
      actionButton(ns("save_filters"), "Save filters"),
      # clear/reset filters ----
      actionButton(ns("clear_filters"), "Clear filters"),
      textOutput(ns("currently_editing")),
      fluidRow(uiOutput(ns("current_filters_display")))

    ),
    mainPanel(
      # show table ----
      tableOutput(ns("data"))
    )
  )
}

#' @export
server <- function(id, saved_lists_and_filters, edited_list, data) {
  moduleServer(id, function(input, output, session) {
    # get data col names ----
    vars <- reactive(names(data))

    # generate filters ui ----
    output$filter <- renderUI({
      ns <- NS(id)
      tabsetPanel(
        id = session$ns("filter_tabs"),
        tabPanel("Gene symbol",
                 lapply(vars()[1:3], function(var) {
                   filters_utils$make_ui(data[[var]], var, id, session)
                 })),
        tabPanel("Mouse",
                 lapply(vars()[4:9], function(var) {
                   filters_utils$make_ui(data[[var]], var, id, session)
                 })),
        tabPanel("Disease",
                 lapply(vars()[10:18], function(var) {
                   filters_utils$make_ui(data[[var]], var, id, session)
                 })),
        tabPanel("Constraint",
                 lapply(vars()[19:29], function(var) {
                   filters_utils$make_ui(data[[var]], var, id, session)
                 })),
        tabPanel("Gene list upload",
                 gene_list_upload$ui(session$ns("file_upload"))
        )
      )
    })
    # I WANT TO ADD GENE LIST AND FILTERS INSTEAD USING 1 and 2 for indexing saved_lists_and_filters ***********
    # currently editing ----
    output$currently_editing <- renderText({
      if (!is.null(edited_list()))
      paste('Editing: ', edited_list()[['name']])
    })

    # filter dataframe ----
    selected <- reactive({
      each_var <- map(vars(), ~ filters_utils$filter_var(data[[.x]], input[[.x]], input[[paste0("na_switch_", .x)]]))
      reduce(each_var, `&`)
    })

    # render datatable ----
    output$data <- renderTable(head(data[selected(), ], 12))

    # current filters ----
    current_filters <- reactive({
      map(vars(), function(var) {
        list(
          var = var,
          value = input[[var]],
          na_switch_value = input[[paste0("na_switch_", var)]]
        )
      })
    })

    # save filters ----
    observeEvent(input$save_filters, {
      # Get the current list name
      list_name <- input$name_saved_list
      req(list_name != "")
      # Retrieve the current list of saved filters
      current_lists <- saved_lists_and_filters()
      # Assign current filters to the list using the list name as the key
      gene_list <- data[selected(), 1]
      # gene_list <- discard(gene_list, is.na)
      saved_data <- list(unique(gene_list), current_filters())
      current_lists[[list_name]] <- saved_data
      # Update saved_lists_and_filters with the new list
      saved_lists_and_filters(current_lists)
      # Update edit button reset val
      edited_list(NULL)
      # reset the name input field
      updateTextInput(session, "name_saved_list", value = "")
    })

    # reset filters ----
    observeEvent(input$clear_filters, {
      # Iterate over variables to reset each filter
      map(vars(), function(var) {
        data_var <- data[[var]]
        if (is.numeric(data_var)) {
          # Reset numeric input: setting range to full data range
          rng <- range(data_var, na.rm = TRUE)
          updateSliderInput(session, inputId = var, min = rng[1], max = rng[2], value = rng)
          updateCheckboxInput(session, inputId = paste0("na_switch_", var), value = TRUE)
        } else if (is.factor(data_var)) {
          # Reset select input: selecting all levels
          levs <- levels(data_var)
          updateSelectInput(session, inputId = var, selected = levs)
          updateCheckboxInput(session, inputId = paste0("na_switch_", var), value = TRUE)
        } else if (is.character(data_var)) {
          updateTextAreaInput(session, inputId = var, value = "")
        }
      })
      # reset the name input field
      updateTextInput(session, "name_saved_list", value = "")
    })

    # edit list ----
    observeEvent(edited_list(), {
      # Iterate over variables to reset each filter
      filters_to_apply <- edited_list()[['filters']]
      map(filters_to_apply, function(filter) {
        var <- filter$var
        value <- filter$value
        na_switch_value <- filter$na_switch_value
        data_var <- data[[var]]
        # Check the type of the data_var to apply the correct input update method
        if (is.numeric(data_var)) {
          # Update numeric input: setting it to the specified value
          updateSliderInput(session, inputId = var, value = value)
          updateCheckboxInput(session, inputId = paste0("na_switch_", var), value = na_switch_value)
        } else if (is.factor(data_var)) {
          # Update select input: setting it to the specified value
          updateSelectInput(session, inputId = var, selected = value)
          updateCheckboxInput(session, inputId = paste0("na_switch_", var), value = na_switch_value)
        } else if (is.character(data_var)) {
          # Update text area input: setting it to the specified value
          updateTextAreaInput(session, inputId = var, value = value)
        }
      })
      # Optionally reset other UI elements like text inputs, etc.
      updateTextInput(session, "name_saved_list", value = edited_list()[['name']])
    })

    # Show current filters ----
    initial_filters <- reactive({
      map(vars(), function(var) {
        data_var <- data[[var]]
        if (is.factor(data_var)) {
          val <- levels(data_var)[levels(data_var) != "" & !is.na(levels(data_var))]
          na_switch_val <- TRUE
        } else if (is.numeric(data_var)) {
          val <- range(data_var, na.rm = TRUE)
          na_switch_val <- TRUE
        } else if (is.character(data_var)) {
          val <- ""
          na_switch_val <- NULL
        }
        list(
          var = var,
          value = val,
          na_switch_value = na_switch_val
        )
      })
    })

    filter_differences <- reactive({
      map2(
        initial_filters(),  # Call to initial_filters reactive
        current_filters(),  # Call to current_filters reactive
        ~ {
          # Directly compare values using all.equal for exact numerical equality
          value_equal <- isTRUE(all.equal(.x$value, .y$value, check.attributes = FALSE))
          na_switch_equal <- identical(.x$na_switch_value, .y$na_switch_value)
          if (!value_equal || !na_switch_equal) {
            .y  # Return the current filter if there are differences
          } else {
            NULL  # Return NULL if there are no differences
          }
        }
      )
    })

    output$current_filters_display <- renderUI({
      differences <- filter_differences() # Fetch the reactive differences
      valid_differences <- discard(differences, is.null) # Discard nulls (no changes)
      # Prepare the UI elements
      ui_elements <- list()
      if (length(valid_differences) > 0) {
        ui_elements[[1]] <- p("Current filters:")
        # Append each changed filter as a new UI element
        filter_ui <- lapply(valid_differences, function(filter) {
          fluidRow(
            column(6, p(filter$var)),
            column(3, p("Value: ", toString(filter$value))),
            column(3, p("NA switch: ", filter$na_switch_value))
          )
        })
        ui_elements <- c(ui_elements, filter_ui) # Combine the header with filter details
      } else {
        # Add a message when no filters have changed
        ui_elements[[1]] <- p("No filters have been changed from the initial state.")
      }
      do.call(tagList, ui_elements) # Combine all UI elements into a tagList
    })

    # file upload server ----
    gene_list_upload$server("file_upload", saved_lists_and_filters, data)
  })
}
