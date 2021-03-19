

#' plot_block_stat
#'
#' Get data for plot_block
#'
#' @noRd

plot_block_stat <- function(func_data,
                            tax_data,
                            gene_data,
                            func_split = ";",
                            split_percentage = 10,
                            show_func_topn = 10 ){

  colnames(func_data) <- c("Gene", "Func")
  colnames(tax_data)[1] <- "Gene"
  colnames(gene_data)[1] <- "Gene"

  genes <- func_data$Gene
  gene_func_tax_data <- merge(func_data, tax_data, by = "Gene", all.x = T, all.y = F, sort = F)
  gene_func_tax_data[is.na(gene_func_tax_data)] <- "Unknown"
  gene_data <- subset(gene_data, Gene %in% genes)

  gene_func_tax_data$Func <- lapply(gene_func_tax_data$Func, function(x){unique(unlist(strsplit(x, func_split)))})
  funcs_table <- sort(table(unlist(gene_func_tax_data$Func)), decreasing = T)
  funcs_table <- funcs_table[1:min(length(funcs_table), show_func_topn)]
  funcs <- names(funcs_table)

  level1_table <- table(gene_func_tax_data[[3]])
  max_split_per <- floor(max(level1_table) / sum(level1_table) * 100)
  if(split_percentage > max_split_per){
    stop(paste0("the `split_percentage` should less than ", max_split_per, " according the first level of tax annotation."))
  }

  gene_func_tax_data <- gene_func_tax_data[unlist(lapply(gene_func_tax_data$Func, function(x){any(x %in% funcs)})),]

  levels <- setdiff(colnames(gene_func_tax_data), c("Gene", "Func"))

  gene_n <- length(genes)
  gene_n_threshold <- gene_n * split_percentage * 0.01

  tax_struc_data <- get_tax_struc(gene_func_tax_data[, c("Gene", levels)], 0, "root", gene_n_threshold)
  tax_struc_data$is_leaf <- with(tax_struc_data, tax %in% setdiff(tax, parent))
  tax_struc_data$y <- match(tax_struc_data$level, rev(levels)[rev(levels) %in% unique(tax_struc_data$level)])

  tax_struc_data_colored <- subset(tax_struc_data, is_leaf == TRUE)

  funcs_p_data <- do.call(rbind, lapply(funcs, function(x){
    curr_func_gene <- gene_func_tax_data[unlist(lapply(gene_func_tax_data$Func, function(y){any(y %in% x)})), "Gene"]
    curr_func_data <- tax_struc_data_colored
    curr_func_data$genes <- lapply(curr_func_data$genes, function(x){intersect(x, curr_func_gene)})
    curr_func_data$func <- x
    curr_func_data$func_value <- unlist(lapply(curr_func_data$genes, length))
    sample_data <- do.call(rbind, lapply(curr_func_data$genes, function(x){colSums(subset(gene_data, Gene %in% x)[, -1])}))
    cbind(curr_func_data, sample_data)
  }))

  funcs_p_data$y <- match(funcs_p_data$func, funcs) * (-1)

  list(funcs_p_data = funcs_p_data,
       tax_struc_data = tax_struc_data[, c("level", "tax", "value", "x_start", "x_end", "is_leaf", "y")],
       levels = levels, funcs = funcs, samples = colnames(gene_data)[-1])

}


#'blockPlot
#'
#' The function \code{blockPlot} plots a combination Block Chart. The icicle plot in the upper is
#' used to display taxonomic classification and each row is a taxonomic rank. The number in parentheses
#' represents the number of genes. The bar charts in the lower exhibit function annotations and each row
#' is a function. The length represents the number of genes, and the color represents the taxon which
#' corresponds to the figure above.
#'
#' @usage blockPlot(func_data, tax_data, func_split = ";", split_percentage = 10,
#' show_func_topn = 20, rownames_size = 10, tax_name_size = 3,
#' legend_label_size = 10, avoid_label_overlap = T)
#'
#' @param func_data A data frame contains functional annotation.
#' The first column is the gene name, and the second column is the functional annotation.
#' @param tax_data A data frame contains taxonomic classification.
#' It needs to contain at least three columns. The first column is the gene,
#' and the rest columns are the taxonomic classification.
#' @param gene_data A data frame contains gene profile.
#' The first column is the gene, and the rest columns are the gene profile
#' (0 presents absence and positive number presents presence).
#' @param func_split Separator for multiple functions.
#' @param show_func_topn Show top n of functioanl functions.
#' @param split_percentage Split percentage.
#' @param rownames_size The size of rownames.
#' @param tax_name_size The size of taxa.
#' @param sample_point_size The size of sample points.
#' @param sample_point_alpha The alpha channel for transparency of sample points.
#' @param block_alpha The alpha channel for transparency of blocks.
#' @param legend_label_size The size of legend labels.
#' @param avoid_label_overlap If `TRUE`, the text labels repel away from each other.
#'
#' @examples
#' library (metaFunc)
#' data(simple_demo)
#' blockPlot(func_data = simple_demo$func, tax_data = simple_demo$tax)
#'
#' @export

blockPlot <- function(func_data,
                      tax_data,
                      gene_data,
                      func_split = ";",
                      show_func_topn = 20,
                      split_percentage = 10,
                      rownames_size = 10,
                      tax_name_size = 3,
                      sample_point_size = 1,
                      sample_point_alpha = .7,
                      block_alpha = .3,
                      legend_label_size = 10,
                      avoid_label_overlap = T){

  if(ncol(func_data) != 2) stop("The function annotation file should have two column.")
  if(any(duplicated(func_data[, 1]))) stop("The function anotation file have duplicated genes.")
  if(any(duplicated(tax_data[, 1]))) stop("The tax anotation file have duplicated genes.")
  if(any(duplicated(gene_data[, 1]))) stop("The gene data have duplicated genes.")
  if(!all(apply(gene_data[, -1], 2, is.numeric))) stop("All columns except the first column in the `gene_data` should be numeric.")
  if(ncol(tax_data) < 3) stop("The tax antation file should have more than two columns.")
  if(ncol(gene_data) < 2) stop("The `gene_data` should have at least two column.")
  if(!all(func_data[[1]] %in% gene_data[[1]])) stop("There are genes in `func_data` but not in `gene_data`.")

  for (i in colnames(gene_data)[-1]){
    gene_data[[i]] <- as.numeric(ifelse(gene_data[[i]] == 0, 0, 1))
  }

  tax_level_data <- unique(tax_data[, -1])
  tax_all <- as.vector(unlist(apply(tax_level_data, 2, function(x){unique(setdiff(x, "Unknown"))})))
  if(any(duplicated(tax_all))){
    stop(paste0("'", paste0(tax_all[duplicated(tax_all)], collapse = ","),"'have duplidated in different columns."))
  }
  err <- as.vector(unlist(lapply(1:(ncol(tax_level_data)-1), function(x){
    curr_data <- unique(tax_level_data[, x:(x+1)])
    apply(curr_data, 1, function(y){ if(y[[1]] == "Unknown" && y[[2]] != "Unknown"){x}else{NULL} })
  })))
  if(!is.null(err)) stop(paste0("If a gene in a certain taxonomic rank is 'Unknown', the lower taxonomic rank should be 'Unkown' too."))


  plot_block_stat_res <- plot_block_stat(func_data =func_data,
                                          tax_data = tax_data,
                                         gene_data = gene_data,
                                          func_split = func_split,
                                          split_percentage = split_percentage,
                                          show_func_topn = show_func_topn)

  plot_block(stat_res = plot_block_stat_res,
             rownames_size = rownames_size,
             tax_name_size = tax_name_size,
             sample_point_size = sample_point_size,
             sample_point_alpha = sample_point_alpha,
             block_alpha = block_alpha,
             legend_label_size = legend_label_size,
             avoid_label_overlap = avoid_label_overlap)

}



