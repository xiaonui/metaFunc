


#' blockShiny
#'
#' The function \code{blockShiny} starts the browser to start anlysis.
#'
#' @usage blockShiny(shiny.maxRequestSize = 100 * 1024^2, launch.browser = T)
#'
#' @param shiny.maxRequestSize The maximum size of the uploaded file.
#' @param launch.browser If `TRUE`, launch browser.
#'
#' @examples
#' library (metaFunc)
#' blockShiny()
#'
#' @importFrom shiny shinyApp navbarPage tabPanel sidebarLayout sidebarPanel mainPanel h4
#' fileInput selectInput actionButton uiOutput numericInput sliderInput downloadButton
#' fluidRow column checkboxInput verbatimTextOutput tabsetPanel hideTab showTab br hr h3
#' reactive req validate need showModal modalDialog observe renderUI withProgress h5 h6
#' incProgress renderPlot downloadHandler reactiveValues updateSliderInput observeEvent
#' @importFrom DT dataTableOutput datatable
#' @importFrom networkD3 sankeyNetworkOutput renderSankeyNetwork saveNetwork
#' @importFrom ggplot2 ggplot geom_bar theme_classic scale_y_continuous expansion
#' scale_fill_manual theme coord_cartesian aes element_text
#' @importFrom tools file_ext
#'
#' @export

blockShiny <- function(shiny.maxRequestSize = 100 * 1024^2, launch.browser = T) {
  if (interactive()) {
    options(shiny.maxRequestSize = shiny.maxRequestSize)
    shinyApp(
      ui <- navbarPage(
        "metaFunc",
        id = "tabs",
        tabPanel(
          "Upload Data",
          sidebarLayout(
            sidebarPanel(width = 4,
                         h3("Use sample data"),
                         actionButton("file_demo", "Load Demo (Xiao L et al.)"), hr(),
                         h3("Upload File"),
                         h5("The file accepts .txt and .csv formats. TXT files should be separated by tabs."),
                         br(),
                         h6(paste0("This file should contain two columns, the first column is the gene name, and the second column is the functional annotation. ",
                                   "If a gene corresponds to multiple functions, use the separator to connect.")),
                         fluidRow(
                           column(width = 6,
                                  fileInput("file_func", "Please upload functional annotation: ", accept = c(".csv", ".tsv", ".txt"))),
                           column(width = 6,
                                  selectInput("func_split", "Separator for multiple functions:", c(";", "/", ","), ";"))),
                         h6(paste0("This file needs to contain at least three columns, the first column is the gene, and the rest are the taxonomic classification. "),
                            "Unknown taxonomic classification label \"Unknown\"."),
                         fileInput("file_tax", "Please upload taxonomic annotation: ", accept = c(".csv", ".tsv", ".txt")),
                         h6(paste0("A data frame contains gene profile.The first column is the gene, and the rest columns are the gene profile.",
                                   " (0 presents absence and positive number presents presence).")),
                         fileInput("file_gene", "Please upload gene profiling: ", accept = c(".csv", ".tsv", ".txt"))),
            mainPanel(width = 8,
                      h4("After the data is uploaded and checked, it will be displayed in the table below, and the result tabs will appear in the top menu."),
                      hr(),
                      h4("The functional annotation:"),
                      DT::dataTableOutput("func_upload_datatable"),
                      h4("The taxonomic annotation:"),
                      DT::dataTableOutput("tax_upload_datatable"),
                      h4("The gene profile:"),
                      DT::dataTableOutput("gene_upload_datatable"))
          )),
        tabPanel(
          "Overview",
          h3("Functional annotation summary"),
          sidebarLayout(
            sidebarPanel(width = 2,
                         numericInput("bar_height", "Height:", min = 10, value = 500),
                         numericInput("bar_width", "Width:", min = 10, value = 1000),
                         numericInput("bar_text_size", "Text Size:", min = 1, value = 13),
                         sliderInput("bar_text_angle", "Text Angle: ", min = 0, max = 270, step = 90, value = 90),
                         numericInput("bar_title_size", "Title Size:", min = 1, value = 15),
                         downloadButton("down_bar_plot", "Download Plot")),
            mainPanel(width = 10, uiOutput("data_summary"), uiOutput("ui_bar_plot"),
                      h6(paste0("## If you double click in a brush on the plot, the chart will be zoomed to the brush bounds.",
                                " And double-clicking again (outside brush) will reset the zoom."))
            )),
          h3("Function selection"),
          fluidRow(
            column(width = 5,
                   h5("Please select the function for the next step."),
                   h6(" You can select the functions by clicking on the rows and using the 'SHIFT' for multiple selections."), br(),
                   DT::dataTableOutput("bar_datatable")),
            column(width = 1),
            column(width = 5,
                   h5("Selected data:"),br(),
                   DT::dataTableOutput("func_selected_datatable"))
          )),
        tabPanel(
          "Block Chart", br(),
          sidebarLayout(
            sidebarPanel(width = 2,
                         sliderInput("split_percentage", "Tax Split Percentage :", min = 1, max = 100, step = 1, value = 10),
                         hr(),
                         numericInput("block_height", "Plot Height", min = 10, value = 500),
                         numericInput("block_width", "Plot Width", min =10, value = 1000),
                         numericInput("rownames_size", "Row Names Size:", min = 1, max = 30, value = 13),
                         numericInput("tax_name_size", "Tax Name Size:", min = 1, max = 30, value = 4),
                         numericInput("sample_point_size", "Sample Points Size:", min = 0, value = 2),
                         sliderInput("sample_point_alpha", "Sample Points Alpha:", min = 0, max = 1, value =.7),
                         sliderInput("block_alpha", "Block Alpha:", min = 0, max = 1, value =.1),
                         numericInput("legend_label_size", "Legend Label Size:", min = 1, max = 30, value = 11),
                         checkboxInput("block_label_repel", "Aviod Label Overlap:", TRUE),
                         downloadButton("down_block_plot", label = "Download Plot")),
            mainPanel(width = 10,
                      uiOutput("ui_block_plot"),
                      h6(paste0("## If you double click in a brush on the plot, the chart will be zoomed to the brush bounds.",
                                " And double-clicking again (outside brush) will reset the zoom.")),
                      h5("Click the center part of the function block in the figure, and the detailed information will be displayed below."),
                      verbatimTextOutput("click_info"),
                      tabsetPanel(
                        tabPanel("Sankey", br(),
                                 sidebarLayout(
                                   sidebarPanel(
                                     width = 2,
                                     selectInput("sankey_level_n", "Show More Ranks:", 1:3, 3),
                                     numericInput("sankey_height", "Plot Height", min = 10, value = 500),
                                     numericInput("sankey_width", "Plot Width", min = 10, value = NA),
                                     numericInput("sankey_node_size", "Node Size", min = 1, value = 12),
                                     downloadButton("down_sankey_plot", label = "Download Plot")),
                                   mainPanel(width = 10,
                                             uiOutput("ui_sankey_plot"))
                                 )),
                        tabPanel("Data",br(), DT::dataTableOutput("click_data"))
                      )
            )
          )
        )
      ),

      server = function(input, output, session) {

        hideTab(inputId = "tabs", target = "Overview")
        hideTab(inputId = "tabs", target = "Block Chart")

        ## ---------------  Tab 1 : Upload Data

        v1 <- reactiveValues(func_data = NULL, tax_data = NULL, gene_data = NULL, check = F)

        func_upload_data <- reactive({
          req(input$file_func)
          ext <- tools::file_ext(input$file_func$datapath)
          validate(need(ext %in%  c("csv", "txt", "tsv"), "Invalid file; Please upload a .csv or a .txt file"))
          if(ext == "csv"){
            read.csv(input$file_func$datapath, header = T, stringsAsFactors = F)
          } else {
            read.table(input$file_func$datapath, header = T, sep = "\t", stringsAsFactors = F)
          }
        })

        tax_upload_data <- reactive({
          req(input$file_tax)
          ext <- tools::file_ext(input$file_tax$datapath)
          validate(need(ext %in%  c("csv", "txt", "tsv"), "Invalid file; Please upload a .csv or a .txt file"))
          if(ext == "csv"){
            read.csv(input$file_tax$datapath, header = T)
          } else {
            read.table(input$file_tax$datapath, header = T, sep = "\t", stringsAsFactors = F)
          }
        })

        gene_upload_data <- reactive({
          req(input$file_gene)
          ext <- tools::file_ext(input$file_gene$datapath)
          validate(need(ext %in%  c("csv", "txt", "tsv"), "Invalid file; Please upload a .csv or a .txt file"))
          if(ext == "csv"){
            read.csv(input$file_gene$datapath, header = T)
          } else {
            read.table(input$file_gene$datapath, header = T, sep = "\t", stringsAsFactors = F)
          }
        })

        observeEvent(input$file_func,{
          req(func_upload_data())
          v1$func_data <- func_upload_data()
        })

        observeEvent(input$file_tax,{
          req(tax_upload_data())
          v1$tax_data <- tax_upload_data()
        })

        observeEvent(input$file_gene,{
          req(gene_upload_data())
          v1$gene_data <- gene_upload_data()
        })

        observeEvent(input$file_demo,{
          withProgress(message = 'Calculation in progress',
                       detail = 'This may take a while...', value = 0, {
                         incProgress(1/2)
                         data <-  data(simple_demo)
                         v1$func_data <- simple_demo$func
                         v1$tax_data <- simple_demo$tax
                         v1$gene_data <- simple_demo$gene
                       })
        })

        observe({
          req(v1$func_data)
          req(v1$tax_data)
          req(v1$gene_data)
          v1$check <- T
          func_data <- v1$func_data
          tax_data <- v1$tax_data
          gene_data <- v1$gene_data

          if(ncol(func_data) != 2){
            v1$check <- F
            showModal(modalDialog("The function annotation file should have two column.", easyClose = TRUE, footer = NULL))
          } else if(any(duplicated(func_data[, 1]))){
            v1$check <- F
            showModal(modalDialog("The function anotation file have duplicated genes.", easyClose = TRUE, footer = NULL))
          } else if(any(duplicated(tax_data[, 1]))){
            v1$check <- F
            showModal(modalDialog("The tax anotation file have duplicated genes.", easyClose = TRUE, footer = NULL))
          } else if (ncol(tax_data) < 3){
            v1$check <- F
            showModal(modalDialog("The tax antation file should have more than two columns.", easyClose = TRUE, footer = NULL))
          } else if(any(duplicated(gene_data[, 1]))){
            v1$check <- F
            showModal(modalDialog("The gene profile have duplicated genes.", easyClose = TRUE, footer = NULL))
          } else if (ncol(gene_data) < 2) {
            v1$check <- F
            showModal(modalDialog("The gene profile should have at least two columns.", easyClose = TRUE, footer = NULL))
          } else if(!all(apply(gene_data[, -1], 2, is.numeric))){
            v1$check <- F
            showModal(modalDialog("All columns except the first column in the `gene_data` should be numeric.", easyClose = TRUE, footer = NULL))
          } else if(!all(func_data[[1]] %in% gene_data[[1]])){
            v1$check <- F
            showModal(modalDialog("There are genes in function anotation file but not in gene profile.", easyClose = TRUE, footer = NULL))
          } else {
            tax_level_data <- unique(tax_data[, -1])
            tax_all <- as.vector(unlist(apply(tax_level_data, 2, function(x){unique(setdiff(x, "Unknown"))})))
            if(any(duplicated(tax_all))){
              v1$check <- F
              showModal(modalDialog(paste0("'", paste0(tax_all[duplicated(tax_all)], collapse = ","),
                                           "'have duplidated in different columns."), easyClose = TRUE, footer = NULL))
            }
            err <- as.vector(unlist(lapply(1:(ncol(tax_level_data)-1), function(x){
              curr_data <- unique(tax_level_data[, x:(x+1)])
              apply(curr_data, 1, function(y){ if(y[[1]] == "Unknown" && y[[2]] != "Unknown"){x}else{NULL} })
            })))
            if(!is.null(err)){
              v1$check <- F
              showModal(modalDialog("If a gene in a certain taxonomic rank is 'Unknown', the lower taxonomic rank should be 'Unkown' too.",
                                    easyClose = TRUE, footer = NULL))
            }
          }

          if(v1$check){
            hideTab(inputId = "tabs", target = "Upload Data")
            showTab(inputId = "tabs", target = "Overview")
            showTab(inputId = "tabs", target = "Block Chart")
            showTab(inputId = "tabs", target = "Upload Data")
          }
        })

        output$func_upload_datatable <- DT::renderDataTable({
          req(v1$func_data)
          if(v1$check)  DT::datatable(v1$func_data, extensions = "FixedHeader", options= list(fixedHeader = TRUE), selection = "none")
        })

        output$tax_upload_datatable <- DT::renderDataTable({
          req(v1$tax_data)
          if(v1$check) DT::datatable(v1$tax_data, extensions = "FixedHeader", options= list(fixedHeader = TRUE), selection = "none")
        })

        output$gene_upload_datatable <- DT::renderDataTable({
          req(v1$gene_data)
          if(v1$check) DT::datatable(v1$gene_data, extensions = "FixedHeader", options= list(fixedHeader = TRUE), selection = "none")
        })

        ## ----------------  Tab 2 : Overview

        v2 <- reactiveValues(uplod_data = FALSE, barplot = NULL, range_x = NULL, range_y = NULL)

        gene_func_tax_data <- reactive({
          req(v1$func_data)
          req(v1$tax_data)

          if(v1$check){
            func_data <- unique(v1$func_data)
            tax_data <- unique(v1$tax_data)

            colnames(func_data) <- c("Gene", "Func")
            colnames(tax_data)[1] <- "Gene"

            gene_func_tax_data <- merge(func_data, tax_data, by = "Gene", all.x = T, all.y = F, sort = F)
            gene_func_tax_data[is.na(gene_func_tax_data)] <- "Unknown"
            gene_func_tax_data$Func <- lapply(gene_func_tax_data$Func, function(x){unique(unlist(strsplit(x, input$func_split)))})
            gene_func_tax_data
          }

        })

        output$data_summary <- renderUI({
          v2$uplod_data <- FALSE
          withProgress(message = 'Calculation in progress',
                       detail = 'This may take a while...', value = 0, {
                         ## check data
                         incProgress(1/3)
                         validate(
                           need(ncol(v1$func_data) == 2, "The function annotation file should have two column."),
                           need(!any(duplicated(v1$func_data[, 1])), "The function anotation file have duplicated genes."),
                           need(!any(duplicated(v1$tax_data[, 1])), "The tax anotation file have duplicated genes.")
                         )
                         incProgress(1/3)
                         v2$uplod_data <- TRUE
                         data <- gene_func_tax_data()
                         incProgress(1/3) #A total of 10 genes have functions and 5 functions are involved
                         h4(paste0("A total of ", nrow(data), " genes have functions, and ",
                                   length(unique(unlist(data$Func)))," functions are involved."))
                       })
        })

        gene_func_table_data <- reactive({
          req(gene_func_tax_data())
          data <- gene_func_tax_data()
          funcs_table <- as.data.frame(sort(table(unlist(data$Func)), decreasing = T), stringsAsFactors = F)
          colnames(funcs_table) <- c("Func", "Freq")
          funcs_table$group <- "all"
          funcs_annotated_table <- as.data.frame(table(unlist(data[data[[3]] != "Unknown", ]$Func)))
          colnames(funcs_annotated_table) <- c("Func", "Freq")
          funcs_annotated_table$group <- "tax_annotated"
          rbind(funcs_table, funcs_annotated_table)
        })

        output$ui_bar_plot <- renderUI({
          plotOutput("bar_plot", height = input$bar_height, width = input$bar_width,
                     dblclick = "bar_dbclick", brush = brushOpts(id = "bar_brush",resetOnNew = TRUE))
        })

        output$bar_plot <- renderPlot({
          if(v2$uplod_data){
            req(gene_func_table_data())
            p_data <- gene_func_table_data()
            p_data <- p_data[order(p_data$Freq, decreasing = T),]
            p_data$Func <- factor(p_data$Func, levels = unique(p_data$Func))
            g <- ggplot() +
              geom_bar(data = subset(p_data, group == "all"),aes(x = Func, y = Freq, fill = group), stat = "identity", width = .6) +
              geom_bar(data = subset(p_data, group == "tax_annotated"),aes(x = Func, y = Freq, fill = group), stat = "identity", width = .4) +
              # coord_flip() +
              theme_classic() +
              scale_y_continuous(expand = expansion(mult = c(0,.1))) + labs(x = "Functions", y = "Gene Number", fill = "") +
              scale_fill_manual(values = c("#009ACD", "#A4D3EE")) +
              theme(axis.text = element_text(size = input$bar_text_size),
                    axis.text.x = element_text(size = input$bar_text_size, angle = input$bar_text_angle, vjust = 0),
                    axis.title = element_text(size = input$bar_title_size),
                    legend.text = element_text(size = input$bar_text_size),
                    legend.position = "right")  +
              coord_cartesian(xlim = v2$range_x, ylim = v2$range_y, expand = FALSE)
            v2$barplot <- g
            g
          }
        })

        observeEvent(input$bar_dbclick, {
          brush <- input$bar_brush
          if (!is.null(brush)) {
            v2$range_x <- c(brush$xmin, brush$xmax)
            v2$range_y <- c(brush$ymin, brush$ymax)
          } else {
            v2$range_x <- NULL
            v2$range_y <- NULL
          }
        })

        output$down_bar_plot <- downloadHandler(
          filename = function() {
            paste("Barplot_", Sys.Date(), ".pdf", sep="")
          }, content = function(file) {
            req(input$bar_height)
            req(input$bar_width)
            pdf(file, height = input$bar_height/72, width = input$bar_width/72)
            print(v2$barplot)
            dev.off()
          })

        bar_show_data <- reactive({
          data <- gene_func_table_data()
          show_data <- merge(
            subset(data, group == "all")[, 1:2],
            subset(data, group == "tax_annotated")[, 1:2], by = "Func" )
          colnames(show_data) <- c("Function", "Gene Number", "Annotated Gene Number")
          show_data <- show_data[order(show_data[[2]], decreasing = T),]
          rownames(show_data) <- NULL
          show_data
        })

        output$bar_datatable <- DT::renderDataTable({
          req(bar_show_data())
          DT::datatable(bar_show_data(),
                        options = list(scrollX = TRUE, pageLength = 10,dom = "lBfrtip"),
                        filter = list(position = "top", clear = FALSE))
        })

        func_selected <- reactive({
          req(input$bar_datatable_rows_selected)
          req(bar_show_data())
          bar_show_data()[input$bar_datatable_rows_selected, ]
        })

        output$func_selected_datatable <- DT::renderDataTable({
          req(func_selected())
          DT::datatable(func_selected(), extensions = "FixedHeader", options= list(fixedHeader = TRUE), selection = "none")
        })


        ## --------------------- Tab 3 : Block Plot

        v3 <- reactiveValues(range_x = NULL, range_y = NULL, blockplot = NULL, sankey = NULL)

        observe({
          req(gene_func_tax_data())
          req(func_selected())
          level1_table <- table(subset(gene_func_tax_data(), Func %in% func_selected()$Func)[[3]])
          update_per <- floor(max(level1_table) / sum(level1_table) * 100)
          updateSliderInput(session, "split_percentage", max = update_per, min = 1, value = 10, step = 1)
        })

        block_stat_res <- reactive({
          req(gene_func_tax_data())
          req(func_selected())
          req(input$split_percentage)

          gene_func_tax_data <- gene_func_tax_data()
          funcs <- func_selected()$Func
          gene_func_tax_data <- subset(gene_func_tax_data, Func %in% funcs)

          gene_data <- v1$gene_data
          colnames(gene_data)[1] <- "Gene"
          gene_data <- subset(gene_data, Gene %in% gene_func_tax_data$Gene)
          for (i in colnames(gene_data)[-1]){
            gene_data[[i]] <- as.numeric(ifelse(gene_data[[i]] == 0, 0, 1))
          }

          levels <- setdiff(colnames(gene_func_tax_data), c("Gene", "Func"))

          gene_n <- nrow(gene_func_tax_data)
          gene_n_threshold <- gene_n * input$split_percentage * 0.01

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
               levels = levels[levels %in% unique(tax_struc_data$level)], funcs = funcs, samples = colnames(gene_data)[-1])

        })

        output$ui_block_plot <- renderUI({
          validate(need(input$block_height > 100, "The height of the plot should be a positive integer > 100."))
          validate(need(input$block_width > 100, "The width of the plot should be a positive integer > 100."))
          plotOutput("block_plot", click = "plot_click", height = input$block_height, width = input$block_width,
                     dblclick = "block_dbclick", brush = brushOpts(id = "block_brush",resetOnNew = TRUE))
        })

        output$block_plot <- renderPlot({
          validate(need(input$bar_datatable_rows_selected, "Please select rows in the previous tab."))
          withProgress(message = 'Calculation in progress',
                       detail = 'This may take a while...', value = 0, {
                         incProgress(1/2)
                         p <-  plot_block(block_stat_res(),
                                          rownames_size = input$rownames_size,
                                          tax_name_size = input$tax_name_size,
                                          sample_point_size = input$sample_point_size,
                                          sample_point_alpha = input$sample_point_alpha,
                                          block_alpha = input$block_alpha,
                                          legend_label_size = input$legend_label_size,
                                          avoid_label_overlap = input$block_label_repel) +
                           coord_cartesian(xlim = v3$range_x, ylim = v3$range_y, expand = FALSE)
                         v3$blockplot <- p
                         p
                       })
        })

        observeEvent(input$block_dbclick, {
          brush <- input$block_brush
          if (!is.null(brush)) {
            v3$range_x <- c(brush$xmin, brush$xmax)
            v3$range_y <- c(brush$ymin, brush$ymax)
          } else {
            v3$range_x <- NULL
            v3$range_y <- NULL
          }
        })

        output$down_block_plot <- downloadHandler(
          filename = function() {
            paste("Block_plot_", Sys.Date(), ".pdf", sep="")
          }, content = function(file) {
            req(input$block_height)
            req(input$block_width)
            pdf(file, height = input$block_height/72, width = input$block_width/72)
            print(v3$blockplot)
            dev.off()
          })

        #  ----------  Click for sankey plot

        point_data <- reactive({
          req(block_stat_res())
          req(input$plot_click)
          funcs_p_data <- block_stat_res()$funcs_p_data
          samples <- block_stat_res()$samples
          sample_data <- reshape2::melt(funcs_p_data[,setdiff(colnames(funcs_p_data),
                                                              c("level", "tax", "parent", "value", "genes", "is_leaf", "func", "func_value"))],
                                        id.vars = c("x_start", "x_end", "y"), variable.name = "sample")
          sample_data$sample <- as.vector(sample_data$sample)
          sample_data$point_x <- sample_data$x_start + sample_data$value
          sample_data$point_y <- sample_data$y + match(sample_data$sample, rev(samples)) / (length(samples) + 1) * .8 -.4
          near_point_data <- nearPoints(sample_data, input$plot_click, threshold = 10, maxpoints = 1)
          curr_data <- subset(funcs_p_data, x_start == near_point_data$x_start & x_end == near_point_data$x_end & y == near_point_data$y)
          list(curr_data = curr_data, sample_name = near_point_data$sample)
        })

        output$click_info <- renderPrint({
          req(point_data())
          curr_data <- point_data()$curr_data
          curr_sample <- point_data()$sample_name
          cat(paste0("Current click : \n  Function : ", curr_data[1, "func"], "   ", "taxonomy : ", curr_data[1, "level"], "-",
                     curr_data[1, "tax"], "   ", "Sample:", curr_sample, "   ", "Gene Number : ", curr_data[1, curr_sample]))
        })

        click_data <- reactive({
          req(point_data())
          req(gene_func_tax_data())
          data <- gene_func_tax_data()
          near_point_data <- point_data()$curr_data
          curr_sample <- point_data()$sample_name
          if(nrow(near_point_data) == 0) return(NULL)
          curr_genes <- intersect(unlist(near_point_data$genes),
                                    as.vector(v1$gene_data[v1$gene_data[[curr_sample]] != 0, 1]))
          curr_level <- near_point_data$level
          res <- data[data[[1]] %in% curr_genes,]
          res[, c(1, (which(colnames(res) == curr_level) : ncol(res)))]
        })

        output$click_data <- DT::renderDataTable({
          req(click_data())
          data <- click_data()
          rownames(data) <- NULL
          datatable(data,  rownames = FALSE)
        })

        output$ui_sankey_plot <- renderUI({
          req(click_data())
          validate(need(any(click_data()[[2]] != "Unknown"), "Unknown."))
          networkD3::sankeyNetworkOutput("sankey_plot", height = input$sankey_height, width = input$sankey_width)
        })

        output$sankey_plot <- networkD3::renderSankeyNetwork({
          req(input$sankey_level_n)
          req(input$sankey_node_size)
          req(click_data())
          tryCatch({
            need(nrow(click_data()) > 0, "Please click a block")
            tax_data <- click_data()
            tax_data[, 1] <- "all"
            sankey_level_n <- as.numeric(input$sankey_level_n) + 1
            if(ncol(tax_data) > sankey_level_n) tax_data <- tax_data[, 1:sankey_level_n]

            node_data <- do.call(rbind,lapply(1:ncol(tax_data), function(x){
              res <- as.data.frame(table(tax_data[,x]), stringsAsFactors = F)
              if(!(x %in% c(1,2))) res <- subset(res, Var1 != "Unknown")
              res$level = colnames(tax_data)[x]
              res
            }))

            edge_data <- do.call(rbind, lapply(2:ncol(tax_data), function(x){
              res <- as.data.frame(table(tax_data[, (x-1):x]), stringsAsFactors = F)
              colnames(res) <- c("from", "to", "value")
              if(x >= 3) res <- subset(res, from != "Unknown" & to != "Unknown")
              subset(res, value != 0)
            }))

            edge_data$from_id <- match(edge_data$from, node_data$Var1) - 1
            edge_data$to_id <- match(edge_data$to, node_data$Var1) - 1

            net <- networkD3::sankeyNetwork(Links = edge_data, Nodes = node_data,
                                            Source = "from_id", Target = "to_id", Value = "value", NodeID = "Var1",
                                            NodeGroup = "level", fontSize =  input$sankey_node_size,
                                            nodePadding = 5, sinksRight = F)
            v3$sankey <- net
            net
          }, error=function(e){
            NULL
          })
        })

        output$down_sankey_plot <- downloadHandler(
          filename = function() {
            paste("Sankey_", Sys.Date(), ".html", sep="")
          }, content = function(file) {
            networkD3::saveNetwork(v3$sankey, file)
          })

      },
      options = list(launch.browser = launch.browser)
    )
  }
}






