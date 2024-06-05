# main.R
box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags,
        uiOutput, observe, reactiveVal, fluidRow, column, bookmarkButton,
        setBookmarkExclude, enableBookmarking, onBookmark, onRestore, req,
        navbarPage, tabPanel, navbarMenu, reactive],
  htmltools[p, h4],
  fst[read.fst],
  bslib[navset_card_underline, nav_panel, page_fillable, layout_sidebar, sidebar, page_sidebar],
  conflicted[conflict_prefer, conflict_prefer_all]
)

# conflict_prefer("plot", )

box::use(
  app/view/filters,
  app/view/edit_clear,
  app/view/go_enrichment,
  app/view/reactome_enrichment,
  app/view/odds_ratio,
  app/view/bar_chart_mod,
  app/view/violin_plot_mod,
  app/view/currently_applied_filters_overview,
  app/view/name_and_save_list,
  app/view/user_table
)

# enable Shiny bookmarking ----
enableBookmarking(store = 'server')

#' @export
ui <- function(id) {
  ns <- NS(id)
  navbarPage(
    'EGE',
    # Generate gene lists ----
    # tabPanel(
    #   'Generate gene lists',
    #   fluidRow(
    #     filters$ui(ns("filters"))
    #   ),
    #   fluidRow(
    #     column(1, p("Saved lists")),
    #     column(8, edit_clear$ui(ns("edit_clear"))),
    #     column(3, bookmarkButton())
    #   ),
    #   fluidRow(
    #     currently_applied_filters_overview$ui(ns("currently_applied_filters"))
    #   ),
    #   fluidRow(
    #     name_and_save_list$ui(ns("name_and_save_list"))
    #   )
    # ),
    tabPanel(
      'Generate gene lists',
      page_fillable(
        page_sidebar(
          # sidebar
          sidebar = sidebar(
            filters$ui(ns("filters")),
            width = 600,
            title = h4('Filters'),
            class = 'scrollable-sidebar'
          ),
          # main
          h4('Table'),
          fluidRow(
            user_table$ui(ns("user_table"))
          ),
          h4('Save list'),
          fluidRow(
            name_and_save_list$ui(ns("name_and_save_list"))
          ),
          h4('Saved lists'),
          fluidRow(
            edit_clear$ui(ns("edit_clear"))
          ),
          h4('Current filters'),
          fluidRow(
            currently_applied_filters_overview$ui(ns("currently_applied_filters"))
          ),
          fluidRow(
            column(width=3, bookmarkButton())
          )
        )
      )
    ),
    # Plots ----
    tabPanel(
      'Plots',
      navset_card_underline(
        nav_panel(
          'IMPC',
          bar_chart_mod$ui(ns('impc_bar_charts'))
        ),
        nav_panel(
          'OMIM/DDG2P',
          bar_chart_mod$ui(ns('disease_bar_charts'))
        ),
        nav_panel(
          'Gene constraint metrics cell',
          violin_plot_mod$ui(ns('constraint_plots_cell'))
        ),
        nav_panel(
          'Gene constraint metrics pop seq',
          violin_plot_mod$ui(ns('constraint_plots_pop_seq'))
        )
      )
    ),
    # Analysis ----
    navbarMenu(
      'Analysis',
      tabPanel(
        'Gene Ontology Semantic Similarity',
        go_enrichment$ui(ns("go_enrichment"))
      ),
      tabPanel(
        'Reactome enrichment',
        reactome_enrichment$ui(ns("reactome_enrichment"))
      ),
      tabPanel(
        'Odds Ratio',
        odds_ratio$ui(ns("odds_ratio"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # import data ----
    data <- read.fst("./data/all_data_ege.fst")
    # Index
    index <- list(
      'gene_ids' = 1:4,
      'mouse' = 5:10,
      'disease' = 11:21,
      'constraint_cell' = 22:25,
      'constraint_pop' = 26:32
    )
    # Column headers
    nice_col_names <- c(
      'Gene symbol', 'Gene name', 'Gene group', 'Entrez ID',
      'IMPC viability', 'IMPC zygosity', 'IMPC phenotypes', 'Windows of Lethality', 'MGI viability', 'Ortholog mapping',
      'Mode of Inheritance', 'Disease type', 'Molecular basis', 'OMIM Phenotype ID', 'Phenotype', 'Lethal gene', 'Earliest age of death category', 'Disease', 'Allelic requirement', 'Confidence category', 'Organ specificity',
      'Percentage of samples essential in', 'Mean DepMap Gene Effect score', 'Bayes factor (MEF)', 'Bayes factor (Laminin)',
      'gnomAD LOEUF', 'gnomAD missense score', 'AlphaMissense mean pathogenicity', 'Shet (RGCME)', 'Shet (GeneBayes)', 'DOMINO', 'SCoNeS'
    )

    # reactive values ----
    saved_lists_and_filters <- reactiveVal(list())
    edited_list <- reactiveVal(NULL)
    filter_differences <- reactiveVal(NULL)
    vars_global <- reactiveVal(NULL)
    vars <- reactive(names(data))
    save_filters <- reactiveVal(NULL)
    clear_filters <- reactiveVal(NULL)
    current_genes <- reactiveVal(NULL)

    # Generate gene lists ----
    filters$server("filters", saved_lists_and_filters, edited_list, data, nice_col_names, filter_differences, vars, save_filters, clear_filters, current_genes)
    edit_clear$server("edit_clear", saved_lists_and_filters, edited_list)
    currently_applied_filters_overview$server("currently_applied_filters", filter_differences, vars, nice_col_names, clear_filters)
    name_and_save_list$server("name_and_save_list", save_filters)
    user_table$server("user_table", current_genes)

    # Plots
    bar_chart_mod$server(
      'impc_bar_charts',
      saved_lists_and_filters,
      data,
      plot_ids = names(data)[index$mouse],
      footers = list("IMPC 20.1" = 'http://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/release-20.1/results/viability.csv.gz',
                     "IMPC 20.1" = 'http://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/release-20.1/results/genotype-phenotype-assertions-ALL.csv.gz',
                     'IMPC 20.1' = 'http://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/release-20.1/results/genotype-phenotype-assertions-ALL.csv.gz',
                     "Cacheiro et al. 2022" = 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9563108/',
                     'MGI' = 'https://www.informatics.jax.org/downloads/reports/MGI_GenePheno.rpt',
                     "GenTaR" = 'https://www.gentar.org/orthology-api/api/ortholog/write_to_tsv_file'
                     ),
      titles = nice_col_names[index$mouse]
      )
    bar_chart_mod$server(
      'disease_bar_charts',
      saved_lists_and_filters,
      data,
      plot_ids = names(data)[index$disease],
      footers = list("OMIM" = 'https://data.omim.org/downloads/',
                     "OMIM" = 'https://data.omim.org/downloads/',
                     "OMIM" = 'https://data.omim.org/downloads/',
                     "OMIM" = 'https://data.omim.org/downloads/',
                     "OMIM" = 'https://data.omim.org/downloads/',
                     "Cacheiro et al. 2024" = 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10802756/',
                     "Cacheiro et al. 2024" = 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10802756/',
                     "DDG2P" = 'http://ftp.ebi.ac.uk/pub/databases/gene2phenotype/28_07_2023/DDG2P_28_7_2023.csv.gz',
                     "DDG2P" = 'http://ftp.ebi.ac.uk/pub/databases/gene2phenotype/28_07_2023/DDG2P_28_7_2023.csv.gz',
                     "DDG2P" = 'http://ftp.ebi.ac.uk/pub/databases/gene2phenotype/28_07_2023/DDG2P_28_7_2023.csv.gz',
                     "DDG2P" = 'http://ftp.ebi.ac.uk/pub/databases/gene2phenotype/28_07_2023/DDG2P_28_7_2023.csv.gz'
                     ),
      titles = nice_col_names[index$disease]
    )
    violin_plot_mod$server(
      'constraint_plots_cell',
      saved_lists_and_filters,
      data,

      plot_ids = names(data)[index$constraint_cell],
      threshold_values = list(NULL, c(-0.5, -1), 5, 5),
      footers = list("DepMap" = 'https://depmap.org/portal/download/all/',
                     "DepMap" = 'https://depmap.org/portal/download/all/',
                     'Mair et al. 2019' = 'https://www.sciencedirect.com/science/article/pii/S2211124719302128#app2',
                     'Mair et al. 2019' = 'https://www.sciencedirect.com/science/article/pii/S2211124719302128#app2'
      ),
      titles = nice_col_names[index$constraint_cell]
    )
    violin_plot_mod$server(
      'constraint_plots_pop_seq',
      saved_lists_and_filters,
      data,
      plot_ids = names(data)[index$constraint_pop],
      threshold_values = list(0.6, NULL, 0.9, 0.075, NULL, NULL, c(0.25, 0.75)),
      footers = list("gnomAD v4.1" = 'https://gnomad.broadinstitute.org/downloads#v4-constraint',
                     "gnomAD v4.1" = 'https://gnomad.broadinstitute.org/downloads#v4-constraint',
                     'Cheng et al. 2023' = 'https://www.science.org/doi/10.1126/science.adg7492',
                     "Sun et al. 2023" = 'https://pubmed.ncbi.nlm.nih.gov/37214792/',
                     'Zeng et al 2023' = 'https://www.biorxiv.org/content/10.1101/2023.05.19.541520v1',
                     "Quinodoz et al. 2017" = 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5630195/',
                     "Rapaport et al. 2021" = 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7826345/'
      ),
      titles = nice_col_names[index$constraint_pop]
    )
    # Analysis ----
    go_enrichment$server("go_enrichment", saved_lists_and_filters, data)
    reactome_enrichment$server("reactome_enrichment", saved_lists_and_filters, data)
    odds_ratio$server("odds_ratio", saved_lists_and_filters, data)

    # Handle bookmarking ----
    onBookmark(function(state) {
      state$values$saved_lists_and_filters <- saved_lists_and_filters()
    })
    onRestore(function(state) {
      if (!is.null(state$values$saved_lists_and_filters)) {
        saved_lists_and_filters(state$values$saved_lists_and_filters)
      }
    })

    # Debugging print statements
    observe({
      if (length(saved_lists_and_filters()) > 0) {
        # print(length(unique(saved_lists_and_filters()[[1]][[1]])))
        # print(saved_lists_and_filters())
      }
    })
  })
}
