# enrichment utils
box::use(
  clusterProfiler[enrichGO],
  rrvgo[calculateSimMatrix, reduceSimMatrix, scatterPlot],
  stats[setNames, cmdscale, as.dist],
  ggplot2[...],
  dplyr[...],
  ReactomePA[enrichPathway],
  enrichplot[pairwise_termsim, emapplot],
  epitools[...],
  ReactomePA[enrichPathway],
  enrichplot[pairwise_termsim, emapplot]
)

#' @export
status_message <- function(val) {
  if (val > 0) {
    paste(format(Sys.time()), "tasks in progress :", val)
  } else {
    "All tasks completed"
  }
}

#' @export
goAnalysis <- function(gene_list, background, ontology, pval, qval, percent_slice, threshold) {

  go_analysis <- clusterProfiler::enrichGO(gene          = gene_list,
                          universe      = background,
                          keyType = "SYMBOL",
                          OrgDb         = "org.Hs.eg.db",
                          ont           = ontology,
                          pAdjustMethod = "BH",
                          pvalueCutoff  = pval,
                          qvalueCutoff  = qval,
                          readable      = TRUE)
  class(go_analysis)
  typeof(go_analysis)

  simMatrix <- rrvgo::calculateSimMatrix(go_analysis$ID,
                                  orgdb="org.Hs.eg.db",
                                  ont=ontology,
                                  method="Rel")

  scores <- stats::setNames(-log10(go_analysis$qvalue), go_analysis$ID)
  reducedTerms <- rrvgo::reduceSimMatrix(simMatrix,
                                  scores,
                                  threshold=threshold,
                                  orgdb="org.Hs.eg.db")

  # Slice for top 10% of enriched GO terms to get clearer plots
  x <- 100/percent_slice
  top_x_percent_terms <- nrow(reducedTerms)/x

  # Generate plot
  scat_p <- rrvgo::scatterPlot(simMatrix, reducedTerms[1:top_x_percent_terms, ], algorithm = c("pca"), onlyParents = FALSE)

  # Add labels for top 5 most enriched GO terms
  x <- stats::cmdscale(as.matrix(as.dist(1-simMatrix), eig=TRUE, k=2))

  df <- cbind(as.data.frame(x),
              reducedTerms[match(rownames(x), reducedTerms$go), c("term", "parent", "parentTerm", "size")])

  # Only show top 5 most significantly enriched terms
  p <- scat_p + geom_text(aes(label=parentTerm), data=subset(df[1:5,], parent == rownames(df[1:5,])), size=4, color="black")

  return(list(p, go_analysis))
  # return(p)
}

#' @export
reactomeAnalysis <- function(gene_list, background, pval, no_pathways_shown) {

  # Enrichment
  enriched_pathway <- background %>%
    dplyr::filter(gene_symbol %in% gene_list) %>%
    dplyr::pull(entrez_id) %>%
    ReactomePA::enrichPathway(gene = ., pvalueCutoff = pval, readable = TRUE)

  # Table
  enriched_pathway_table <- data.frame(enriched_pathway, row.names = NULL) %>%
    dplyr::select(ID, Description, qvalue)

  # Plots
  edo <- enrichplot::pairwise_termsim(enriched_pathway)
  plot <- enrichplot::emapplot(edo, showCategory = no_pathways_shown)

  return(list(plot, enriched_pathway_table))
}
# # test ----
# gene_list <- unique(data$gene_symbol)[1:100]
# background <- unique(data$gene_symbol)
# pval <- 0.05
# no_pathways_shown <- 10
# # Enrichment
# enriched_pathway <- background %>%
#   dplyr::filter(gene_symbol %in% gene_list) %>%
#   dplyr::pull(entrez_id) %>%
#   ReactomePA::enrichPathway(gene = ., pvalueCutoff = pval, readable = TRUE)
#
# # Table
# enriched_pathway_table <- data.frame(enriched_pathway, row.names = NULL) %>%
#   dplyr::select(ID, Description, qvalue)
#
# # Plots
# edo <- enrichplot::pairwise_termsim(enriched_pathway)
# plot <- enrichplot::emapplot(edo, showCategory = no_pathways_shown)


# p <- reactomeAnalysis(unique(data$gene_symbol)[1:100], unique(data[,1:2]), 0.05, 10)
# p
#' @export
oddsRatioPlot <- function(compare_list_option, gene_list, dataset, index) {
  if (compare_list_option == "OMIM Disease genes") {
    compare_list <- dataset %>%
      dplyr::filter(number_key == "The molecular basis for the disorder is known; a mutation has been found in the gene") %>%
      dplyr::pull(gene_symbol) %>%
      unique()
  } else if (compare_list_option == "IMPC Lethal genes") {
    compare_list <- dataset %>%
      dplyr::filter(impc_viability == "lethal") %>%
      dplyr::pull(gene_symbol) %>%
      unique()
  } else if (compare_list_option == "DepMap Essential genes") {
    compare_list <- dataset %>%
      dplyr::filter(mean_score_all < -0.5) %>%
      dplyr::pull(gene_symbol) %>%
      unique()
  }

  # print(compare_list)

  df <- dataset %>%
    select(gene_symbol) %>%
    distinct() %>%
    mutate(selected = ifelse(gene_symbol %in% gene_list, "y", "n")) %>%
    mutate(compared = ifelse(gene_symbol %in% compare_list, "y", "n"))

  # print(df)

  contigency_table <- table(df$compared, df$selected)

  # print(contigency_table)

  # Can't continue with incorrect contingency table
  # Check if the dimension is 2x1
  if (all(dim(contigency_table) == c(2, 1))) {
    # Add a new column 'y' with values 0, 0
    contigency_table <- cbind(contigency_table, y = c(0, 0))
    # Printing the updated table
    # print(contigency_table)
  }

  or_all <- oddsratio(contigency_table, method = "wald")
  # change to DP not SF
  or_all_or <- signif(or_all$measure[2], 3)
  or_all_lower <- signif(or_all$measure[4], 3)
  or_all_upper <- signif(or_all$measure[6], 3)
  or_all_pvalue <- signif(or_all$p.value[4], 3)
  or_all_df <- data.frame(
    OR = or_all_or,
    LL = or_all_lower,
    UL = or_all_upper,
    pvalue = or_all_pvalue
    )
  # print(or_all_df)
  return(or_all_df)
}

#' @export
batchOddsRatioPlots <- function(list_of_lists, dataset, compare_list_option, odds_ratio_func) {
  results_list <- list()  # To store results from each iteration

  # Loop through the list of lists
  for (i in seq_along(list_of_lists)) {
    # Extract the gene list and the name
    gene_list <- list_of_lists[[i]][[1]]
    list_name <- names(list_of_lists)[[i]]

    # Print to track which list is currently processing
    # print(paste("Processing list:", list_name))

    # Call your existing function
    results <- odds_ratio_func(compare_list_option = compare_list_option, gene_list = gene_list, dataset = dataset)

    # Add a row with index and list name
    results <- results %>%
      mutate(Index = i, label = list_name)

    # Append results to the list
    results_list[[i]] <- results
  }

  # Combine all results into a single data frame
  final_results <- do.call(rbind, results_list)
  return(final_results)
}

#' @export
forestPlot <- function(dat) {
  plot <- ggplot(dat, aes(y = Index, x = OR)) +
    geom_point(shape = 18, size = 5) +
    geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
    geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
    scale_y_continuous(name = "", breaks=1:length(dat$label), labels = dat$label, trans = "reverse") +
    xlab("Odds Ratio") + # check CI confidence interval
    ylab(" ") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.text.x.bottom = element_text(size = 12, colour = "black"),
          axis.title.x = element_text(size = 12, colour = "black"))
  return(plot)
}


