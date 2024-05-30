# filters.r
box::use(
  shiny[moduleServer, NS, reactive,
        sidebarLayout, sidebarPanel, mainPanel, tableOutput, renderTable, actionButton,
        uiOutput, renderUI, observe, selectInput, reactiveVal, textInput, updateTextInput,
        observeEvent, updateSliderInput, updateSelectInput, tabsetPanel, tabPanel,
        updateTextAreaInput, req, updateCheckboxInput, fluidRow, column, h4,
        tagList, renderText, textOutput, isolate, p, showModal, modalDialog],
  purrr[map, map2, imap, reduce, keep, discard],
  utils[head],
  fst[read.fst],
  htmltools[p]
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
server <- function(id, saved_lists_and_filters, edited_list, data, nice_col_names) {
  moduleServer(id, function(input, output, session) {
    # get data col names ----
    vars <- reactive(names(data))

    # generate filters ui ----
    output$filter <- renderUI({
      ns <- NS(id)
      tabsetPanel(
        id = session$ns("filter_tabs"),
        tabPanel("Gene symbol",
                 actionButton(session$ns("gene_modal"), "Filters info"),
                 imap(vars()[1:4], ~filters_utils$make_ui(data[[.x]], .x, nice_col_names[[.y]], id, session))),
        tabPanel("Mouse",
                 actionButton(session$ns("mouse_modal"), "Filters info"),
                 imap(vars()[5:10], ~filters_utils$make_ui(data[[.x]], .x, nice_col_names[[.y + 4]], id, session))),
        tabPanel("Disease",
                 actionButton(session$ns("disease_modal"), "Filters info"),
                 imap(vars()[11:21], ~filters_utils$make_ui(data[[.x]], .x, nice_col_names[[.y + 10]], id, session))),
        tabPanel("Constraint cell",
                 actionButton(session$ns("constraint_cell_modal"), "Filters info"),
                 imap(vars()[22:25], ~filters_utils$make_ui(data[[.x]], .x, nice_col_names[[.y + 21]], id, session))),
        tabPanel("Constraint pop seq",
                 actionButton(session$ns("constraint_pop_seq_modal"), "Filters info"),
                 imap(vars()[26:32], ~filters_utils$make_ui(data[[.x]], .x, nice_col_names[[.y + 25]], id, session))),
        tabPanel("Gene list upload",
                 gene_list_upload$ui(session$ns("file_upload"))
        )
      )
    })

    observeEvent(input$gene_modal, {
      showModal(modalDialog(
        title = "Metadata information",
        p("Gene symbol: The HGNC approved protein coding gene symbol"),
        p("Gene name: HGNC approved name for the gene"),
        p('Gene group: The gene group of related genes'),
        p('Entrez ID: NCBI gene ID'),
        easyClose = TRUE,
      ))
    })

    observeEvent(input$mouse_modal, {
      showModal(modalDialog(
        title = "Metadata information",
        p('IMPC viablity: Outcome of gene knockout on mouse survival or ability to thrive'),
        p('IMPC zygosity: Number of alleles knocked out in a mouse. Homozgous = both alleles knocked out, Heterozygous = one allele knocked out, Hemizygous = one allele knocked out in sex chromosome'),
        p('IMPC phenotypes: Phenotypes observed in mouse knockout. Terms are described in the Mammalian Phenotype Ontology (MP)'),
        p('Window of Lethality: Embryonic day when death occured. Early gestation lethal = E9.5, Mid gestation lethal = E12.5-15.5, Late gestation lethal = E15.5-18.5. Data was curated by Cacheiro et al. 2022'),
        p('MGI viability: Outcome of gene knockout on mouse survival'),
        p('Ortholog mapping: Human-Mouse Ortholog relationship based on GenTaR pipeline'),
        easyClose = TRUE,
      ))
    })

    observeEvent(input$disease_modal, {
      showModal(modalDialog(
        title = "Metadata information",
        p('Mode of inheritance: How the disease is passed to the next generation'),
        p('Disease type: Clinical significance and impact of the disorder'),
        p('Molecular basis: Details of the genetic mapping of the disorder'),
        p('OMIM Phenotype ID: Unique ID linked to disorder'),
        p('Phenotype: Name of disorder'),
        p('Lethal gene: One of three lethality caregories acording to OMIM clinical records that were data mined using a series of API queries of terms linked to lethality. Data was curated by Cacheiro et al. 2024'),
        p('Earliest age of death category: One of seven lethality categories according to the earliest age at which death occured according to OMIM records.
          L1: Prenatal death (HP:0034241); Death before birth
          L1.1: Miscariage (HP:0005268); Spontaneous loss of a fetus before the 22th week of pregnancy
          L1.2: Stillbirth (HP:0003826); Death of the fetus in utero after at least 22 weeks of gestation
          L2: Neonatal death (HP:0003811); Death within the first 28 days of life
          L3: Death in infancy (HP:0001522); Death within the first 24 months of life
          L4: Death in childhood (HP:0003819); Death during childhood, defined here as between the ages of 2 and 10 years
          L5: Death in adolescence (HP:0011421); Death during adolescence, the period between childhood and adulthood (roughly between the ages of 10 and 19 years)
          L6: Death in adulthood (HP:0033763); Cessation of life at the age of 16 years or later
          LU: Age of death undetermined. Data was curated by Cacheiro et al. 2024'),
        p('Disease: Name of disorder'),
        p('Allelic requirement: Number of mutated alleles in a gene needed to cause the disorder'),
        p('Confidence category: Confidence of the gene-phenotype association'),
        p('Organ specificity: Organ systems affected by disorder'),
        easyClose = TRUE,
      ))
    })

    observeEvent(input$constraint_cell_modal, {
      showModal(modalDialog(
        title = "Metadata information",
        p('Percentage of sample essential in: This was computed by calculating the number of different cancer cell lines depleted (using the -0.5 gene effect threshold) amongst all cancer cell lines'),
        p('Mean DepMap Gene Effect score: For gene effect, a score less than -0.5 represents depletion in most cell lines, while less than -1 represents strong killing, outlined here: https://forum.depmap.org/t/depmap-genetic-dependencies-faq/131'),
        p('Bayes factor (MEF): MEF and Laminin refer to the Mouse Embryonic Feeder cells and Laminin substrate used to grow H1-iCas9 human pluripotent stem cells (hPSCs). Bayes Factor (BF) scores represent a confidence measure as to whether a gene is essential, whereby a threshold of BF > 5 can be used to distinguish essential genes'),
        p('Bayes factor (Laminin): MEF and Laminin refer to the Mouse Embryonic Feeder cells and Laminin substrate used to grow H1-iCas9 human pluripotent stem cells (hPSCs). Bayes Factor (BF) scores represent a confidence measure as to whether a gene is essential, whereby a threshold of BF > 5 can be used to distinguish essential genes'),
        easyClose = TRUE,
      ))
    })

    observeEvent(input$constraint_pop_seq_modal, {
      showModal(modalDialog(
        title = "Metadata information",
        p('gnomAD LOEUF: The loss-of-function observed/expected upper bound fraction (LOEUF) score is a continuous metric designed to demonstrate a gene’s intolerance to loss-of-function variation. Constrained genes are defined using the cut-off LOEUF < 0.6'),
        p('gnomAD missense score: observed/expected ratio upper bound fraction for missense variants'),
        p('AlphaMissense mean pathogenicity: This metric represents the probability of a gene being pathogenic, according to the 2023 AlphaMissense deep learning model developed by Cheng et al. 2023. Scores closer to 1 indicate higher likelihood of a gene variant being pathogenic'),
        p('Shet (RGCME): These metrics are derived from the RGC-ME dataset, published by Sun et al. 2023, comprising 985,830 exomes from individuals of diverse ancestry. Here, Shet RGC-ME represents the selectin coefficient estimated per gene from heterozygous probability Loss of Function
               (pLoF) variation amungst the RGC-ME dataset. Highliy constrained heres are genes with Shet greater than 0.075'),
        p('Shet (GeneBayes): These metrics are derived from the machine learning model GeneBayes, created by Zeng et al. 2023. Here, Shet posterior represents the Bayesian estimation of the selectin coefficient from heterzygous LoF carriers from the gnomAD (v2.1.1) dataset. Higher Shet scores indicate more constrained genes'),
        p('DOMINO: This metric was developed by Quinodoz et al. 2017 and assesses the likelihood of a gene to harbor dominant changes using the DOMINO machine learning model'),
        p('SCoNeS: This metric was developed by Rapaport et al. 2021 and predicts the likelihood for a gene to underlie an Autosomal Dominant (AD) or Autosomal Recessive (AD) disorder. Thresholds of SCoNeS score > 0.75 and SCoNeS score < 0.25 are used
               to determine genes are underlying AR and AD disorders respectivley'),
        easyClose = TRUE,
      ))
    })

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
      saved_data <- list('gene_list' = unique(gene_list), 'filters' = current_filters())
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
          updateCheckboxInput(session, inputId = paste0("na_switch_", var), value = TRUE)
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
          updateCheckboxInput(session, inputId = paste0("na_switch_", var), value = na_switch_value)
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
          na_switch_val <- TRUE
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
          index_in_vars <- match(filter$var, vars())
          # Access the corresponding nice name using the index
          nice_name <- nice_col_names[index_in_vars]
          fluidRow(
            column(6, p(nice_name)),  # Display the nice name
            column(3, p("Value: ", toString(filter$value))),
            column(3, p("NAs included: ", filter$na_switch_value))
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
