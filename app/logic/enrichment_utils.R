# enrichment utils
box::use(
  clusterProfiler[enrichGO],
  rrvgo[calculateSimMatrix, reduceSimMatrix, scatterPlot],
  stats[setNames, cmdscale, as.dist],
  ggplot2[...],
  dplyr[...],
  ReactomePA[enrichPathway],
  enrichplot[pairwise_termsim, emapplot],
  epitools[...]
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

  return(p)
}
