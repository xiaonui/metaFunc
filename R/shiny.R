


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
        "metaFuncBlock",
        id = "tabs",
        tabPanel(
          "Upload Data",
          sidebarLayout(
            sidebarPanel(width = 4,
                         h3("Use sample data"),
                         actionButton("file_demo", "Load Demo (Xiao L et al)"), hr(),
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
                         fileInput("file_tax", "Please upload taxonomic profiling: ", accept = c(".csv", ".tsv", ".txt"))),
            mainPanel(width = 8,
                      h4("After the data is uploaded and checked, it will be displayed in the table below, and the result tabs will appear in the top menu."),
                      hr(),
                      h4("The functional annotation:"),
                      DT::dataTableOutput("func_upload_datatable"),
                      h4("The taxonomic profiling:"),
                      DT::dataTableOutput("tax_upload_datatable"))
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
          h3("Funtions selection"),
          fluidRow(
            column(width = 5,
                   h5("Please select the function for the next step."),
                   h6(" You can select the functions by clicking on the rows and using the 'SHIFT' for multiple selection"), br(),
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
                                     selectInput("sankey_level_n", "Show More Rank:", 1:3, 3),
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

        v1 <- reactiveValues(func_data = NULL, tax_data = NULL, check = F)

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
            read.table(input$file_tax$datapath, header = T, sep = "\t")
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

        observeEvent(input$file_demo,{
          withProgress(message = 'Calculation in progress',
                       detail = 'This may take a while...', value = 0, {
                         incProgress(1/2)
                         data <-  data(simple_demo)
                         v1$func_data <- simple_demo$func
                         v1$tax_data <- simple_demo$tax
                       })
        })

        observe({
          req(v1$func_data)
          req(v1$tax_data)
          v1$check <- T
          func_data <- v1$func_data
          tax_data <- v1$tax_data

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
            showTab(inputId = "tabs", target = "Overview")
            showTab(inputId = "tabs", target = "Block Chart")
          }
        })

        output$func_upload_datatable <- DT::renderDataTable({
          req(v1$func_data)
          if(v1$check)  v1$func_data
        })

        output$tax_upload_datatable <- DT::renderDataTable({
          req(v1$tax_data)
          if(v1$check) v1$tax_data
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
          func_selected()
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

          levels <- setdiff(colnames(gene_func_tax_data), c("Gene", "Func"))

          gene_n <- nrow(gene_func_tax_data)
          gene_n_threshold <- gene_n * input$split_percentage * 0.01

          tax_struc_data <- get_tax_struc(gene_func_tax_data[, c("Gene", levels)], 0, "root", gene_n_threshold)
          tax_struc_data$is_leaf <- with(tax_struc_data, tax %in% setdiff(tax, parent))
          tax_struc_data$y <- match(tax_struc_data$level, rev(levels))

          tax_struc_data_colored <- subset(tax_struc_data, is_leaf == TRUE)

          funcs_p_data <- do.call(rbind, lapply(funcs, function(x){
            curr_func_gene <- gene_func_tax_data[unlist(lapply(gene_func_tax_data$Func, function(y){any(y %in% x)})), "Gene"]
            curr_func_data <- tax_struc_data_colored
            curr_func_data$genes <- lapply(curr_func_data$genes, function(x){intersect(x, curr_func_gene)})
            curr_func_data$func <- x
            curr_func_data$func_value <- unlist(lapply(curr_func_data$genes, length))
            curr_func_data
          }))

          funcs_p_data$y <- match(funcs_p_data$func, funcs) * (-1)
          funcs_p_data$point_x <- with(funcs_p_data, x_start + func_value/2)

          list(funcs_p_data = funcs_p_data,
               tax_struc_data = tax_struc_data[, c("level", "tax", "value", "x_start", "x_end", "is_leaf", "y")],
               levels = levels, funcs = funcs)

        })

        output$ui_block_plot <- renderUI({
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

        output$click_info <- renderPrint({
          req(block_stat_res())
          req(input$plot_click)
          near_point_data <- nearPoints(block_stat_res()$funcs_p_data, input$plot_click, threshold = 10, maxpoints = 1)
          cat(paste0("Current click : \n  Function : ", near_point_data[1, "func"], "   ", "taxonomy : ", near_point_data[1, "level"], "-", near_point_data[1, "tax"],
                     "   ", "Gene Number : ", near_point_data[1, "func_value"]))
        })

        click_data <- reactive({
          req(block_stat_res())
          req(input$plot_click)
          req(gene_func_tax_data())
          data <- gene_func_tax_data()
          near_point_data <- nearPoints(block_stat_res()$funcs_p_data, input$plot_click, threshold = 10, maxpoints = 1)
          if(nrow(near_point_data) == 0) return(NULL)
          curr_genes <- unlist(near_point_data$genes)
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






