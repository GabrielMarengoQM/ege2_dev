# Gene symbol checker and file input handlers ----
box::use(
  utils[read.csv, read.delim],
  tools[file_ext],
  readxl[read_excel],
  fst[read.fst],
  dplyr[...]
)

#' @export
readFile <- function(file_path, pcg) {
  # Get file extension
  file_extension <- tools::file_ext(file_path)
  all_genes <- c(pcg$gene_symbol, pcg$alias_symbol, pcg$prev_symbol)
  
  # Read file based on file extension
  if (file_extension == "fst") {
    # Read fst or RData file
    data <- read.fst(file_path)
  } else if (file_extension == "csv") {
    # Read csv file
    data <- read.csv(file_path)
  } else if (file_extension == "xlsx") {
    # Read Excel file
    data <- readxl::read_excel(file_path)
  } else if (file_extension == "tsv") {
    # Read tab-separated values (TSV) file
    data <- read.delim(file_path, sep = "\t")
  } else if (file_extension == "txt") {
    # Read text (TXT) file
    data <- read.table(file_path) # Adjust arguments based on your TXT format
  } else if (file_extension %in% c("rda", "rds", "RData")) {
    # Read RDS (R Data Serialization) file
    data <- readRDS(file_path)
  } else {
    # Unsupported file type
    stop("Unsupported file type.")
  }
  
  # Check for genes in header
  col_names <- names(data)
  if (any(col_names %in% all_genes)) {
    # Read file based on file extension
    if (file_extension == "fst") {
      # Read fst or RData file
      data <- read.fst(file_path)
    } else if (file_extension == "csv") {
      # Read csv file
      data <- read.csv(file_path, header = FALSE)
    } else if (file_extension == "xlsx") {
      # Read Excel file
      data <- readxl::read_excel(file_path, col_names = FALSE)
    } else if (file_extension == "tsv") {
      # Read tab-separated values (TSV) file
      data <- read.delim(file_path, sep = "\t", col.names = FALSE)
    } else if (file_extension == "txt") {
      # Read text (TXT) file
      data <- read.table(file_path, header = FALSE) # Adjust arguments based on your TXT format
    } else if (file_extension %in% c("rda", "rds", "RData")) {
      # Read RDS (R Data Serialization) file
      data <- readRDS(file_path)
    } else {
      # Unsupported file type
      stop("Unsupported file type.")
    }
  }
  
  # print(data)
  data <- data %>%
    pull(1)  %>%
    trimws()
  
  return(data)
}

#' @export
checkInputs <- function(input_gene_list_path, pcg) {
  
  # Read list
  gene_list <- readFile(input_gene_list_path, pcg)
  
  official_genes <- pcg$gene_symbol
  alias_genes <- pcg$alias_symbol
  prev_genes <- pcg$prev_symbol
  all_genes <- c(pcg$gene_symbol, pcg$alias_symbol, pcg$prev_symbol)
  
  official_genes2 <- intersect(official_genes, gene_list)
  alias_genes2 <- intersect(alias_genes, gene_list)
  # Only retain alias gene to convert (genes can be both alias & official/current)
  alias_genes2 <- setdiff(alias_genes2, official_genes2)
  prev_genes2 <- intersect(prev_genes, gene_list)
  # Only retain alias gene to convert (genes can be both prev & official/current) Another way to do this is only intersect by genes not captured by official_genes2
  prev_genes2 <- setdiff(prev_genes2, official_genes2)
  no_match <- setdiff(gene_list, all_genes)
  
  prev2official_tbl <- pcg %>%
    filter(prev_symbol %in% prev_genes2) %>%
    dplyr::select(gene_symbol, prev_symbol) %>%
    distinct() %>%
    rename(`Previous symbol` = prev_symbol, `Converted to` = gene_symbol) %>%
    select(`Previous symbol`, `Converted to`)
  
  prev2official2 <- pcg %>%
    filter(prev_symbol %in% prev_genes2) %>%
    pull(gene_symbol) %>%
    unique()
  
  alias2official_tbl <- pcg %>%
    filter(alias_symbol %in% alias_genes2) %>%
    dplyr::select(gene_symbol, alias_symbol) %>%
    distinct() %>%
    rename(`Alias symbol` = alias_symbol, `Converted to` = gene_symbol) %>%
    select(`Alias symbol`, `Converted to`)
  
  alias2official2 <- pcg %>%
    filter(alias_symbol %in% alias_genes2) %>%
    pull(gene_symbol) %>%
    unique()
  
  returned_list <- unique(c(official_genes2, prev2official2, alias2official2))
  
  return(
    list(
      "gene_list" = returned_list,
      "prev2official" = prev2official_tbl,
      "alias2official" = alias2official_tbl,
      "no_match" = no_match
    )
  )
}

#' @export
userFilesUploadToList <- function(files_data, pcg) {
  genelist_names_list <- list()
  other_data <- list()
  for (i in 1:nrow(files_data)) {
    file_path <- files_data[i, "datapath"]
    file_extension <- tools::file_ext(file_path)
    
    file_data <- checkInputs(file_path, pcg)
    
    # Remove file extension from the name
    name_without_extension <- sub(sprintf("\\.%s$", file_extension), "", files_data[i, "name"])
    
    genelist_names_list[[name_without_extension]] <- file_data$gene_list
    prev_tbl <- file_data$prev2official
    alis_tbl <- file_data$alias2official
    no_match <- file_data$no_match
    other_data[[name_without_extension]] <- list(prev_tbl, alis_tbl, no_match)
  }
  
  return(
    list(
      "genelist_names_list" = genelist_names_list,
      "other_data" = other_data
    )
  )
}
