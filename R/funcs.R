
#' check_tax_data
#'
#' Check tax data
#'
#' @noRd

check_tax_data <- function(tax_level_data){
  }



#' get_tax_struc
#'
#' Get community structure
#'
#' @noRd

get_tax_struc <- function(curr_gene_tax_data, x_start, curr_parent, gene_n_threshold){
  curr_tax_data <- curr_gene_tax_data[, -1, drop = F]
  curr_table <- table(curr_tax_data[, 1])
  curr_level <- colnames(curr_tax_data)[1]
  if(any(curr_table >= gene_n_threshold)){
    remain_tax_table <- sort(curr_table[curr_table >= gene_n_threshold], decreasing = T)
    curr_res_data <- data.frame(level = curr_level, tax = c(names(remain_tax_table), "Others"), parent = curr_parent,
                                value = c(as.vector(remain_tax_table), sum(curr_table[curr_table < gene_n_threshold])))
    curr_res_data$genes <- lapply(curr_res_data$tax, function(x){
      if(x == "Others"){
        curr_gene_tax_data[!(curr_gene_tax_data[, curr_level] %in% names(remain_tax_table)), 1]
      } else {
        curr_gene_tax_data[(curr_gene_tax_data[, curr_level] == x), 1]
      }
    })
    curr_res_data <- subset(curr_res_data, value > 0)
    curr_res_data$x_start <- x_start + c(0, cumsum(curr_res_data$value)[-nrow(curr_res_data)])
    curr_res_data$x_end <- x_start + cumsum(curr_res_data$value)
    if(ncol(curr_gene_tax_data) > 2){
      remain_tax <- setdiff(names(remain_tax_table), "Unknown")
      if(length(remain_tax) > 0){
        remain_tax_data <- do.call(rbind, lapply(remain_tax, function(x){
          get_tax_struc(curr_gene_tax_data[curr_gene_tax_data[, 2] == x, -2, drop = F],
                        subset(curr_res_data, tax == x)$x_start, x, gene_n_threshold)
        }))
      } else {
        remain_tax_data = NULL
      }
    } else {
      remain_tax_data = NULL
    }
    return(rbind(remain_tax_data, curr_res_data))
  } else {
    return(NULL)
  }
}


#' plot_block
#'
#' Plot block chart
#'
#' @importFrom ggplot2 ggplot geom_point geom_rect geom_segment scale_y_continuous
#' theme_bw labs scale_x_continuous theme expansion geom_text element_blank
#' @importFrom ggrepel geom_text_repel
#'
#' @noRd


plot_block <- function(stat_res,
                       rownames_size = 15,
                       tax_name_size = 5,
                       legend_label_size = 15, avoid_label_overlap = F){

  funcs_p_data = stat_res$funcs_p_data
  tax_struc_data = stat_res$tax_struc_data
  tax_struc_data_colored =  subset(stat_res$tax_struc_data, is_leaf == TRUE)
  levels = stat_res$levels
  funcs = stat_res$funcs

  g <-  ggplot(funcs_p_data) +
    geom_point(aes(x = point_x, y = y), color = "white") +
    geom_rect(data = tax_struc_data,
              aes(xmin = x_start, xmax = x_end, ymin = y - 0.5, ymax = y + 0.5),
              color = "black", fill = "gray90")+
    geom_rect(data = tax_struc_data_colored,
              aes(xmin = x_start, xmax = x_end, ymin = y - 0.5, ymax = y + 0.5, fill = paste0(level, "_", tax)),
              color = "black") +
    geom_rect(data = funcs_p_data,
              aes(xmin = x_start, xmax = x_start + func_value, ymin = y + 0.4, ymax = y - 0.4, fill = paste0(level, "_", tax))) +
    geom_segment(data = funcs_p_data,
                 aes(x = x_start, xend = x_start, y = max(y) + 0.5, yend = min(y) - 0.5)) +
    scale_y_continuous(breaks = c(unique(funcs_p_data$y),  unique(tax_struc_data$y)),
                       labels = c(funcs[unique(funcs_p_data$y * (-1))], rev(levels)[unique(tax_struc_data$y)]),
                       expand = expansion(mult = .01)) +
    theme_bw() + labs(fill = "") +
    scale_x_continuous(limits = range(c(tax_struc_data$x_start, tax_struc_data$x_end)),
                       expand = expansion(mult = c(.01, .1))) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          legend.text = element_text(face = "bold", size = legend_label_size),
          axis.ticks= element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(face = "bold", size = rownames_size),
          axis.title = element_blank())

  if(avoid_label_overlap){
    g <- g +  ggrepel::geom_text_repel(
      data = tax_struc_data,
      aes(x = (x_start + x_end)/2, y = y, label = paste0(tax,"(",value,")")),
      color = "black", fontface = "bold", size = tax_name_size, point.size = NA)
  } else {
    g <- g + geom_text(
      data = tax_struc_data,
      aes(x = (x_start + x_end)/2, y = y, label = paste0(tax,"(",value,")")),
      color = "black", fontface = "bold", size = tax_name_size)
  }
  g
}

