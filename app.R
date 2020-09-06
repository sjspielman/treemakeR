#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(shinythemes)
library(colourpicker)
library(ape)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
                #shinythemes::themeSelector(),  
                
    # Application title
    titlePanel("treemakeR: A random tree generator"),
    p("Written and maintained by", tags$a(href = "http://spielmanlab.io", "Stephanie J. Spielman, Ph.D."), 
    "Source code on", tags$a(href = "https://github.com/sjspielman/treemakeR", "github.")),
    
  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel( width = 4, # 4 is default, it doesn't like decimals...
                      h3("Tree settings"),
            numericInput("ntaxa",
                        "Number of tips:",
                        min = 3,
                        max = 20, value = 5),
            radioButtons("tipnames_type",
                        "How to label the tips?",
                        choices = c("Letters (A, B, C...)" = "letters", 
                                    "Numbers (1, 2, 3...)" = "numbers", 
                                    "Specify custom tip names" = "custom")),
            conditionalPanel("input.tipnames_type == 'custom'", {
              uiOutput("custom_tip_stuff")
            }),
            #radioButtons("root_type",
            #            "Select rooted or unrooted tree:",
            #            choices = c("Rooted", "Unrooted (sometimes end up looking rooted!)")),
            #conditionalPanel("input.root_type == 'Rooted'", {
                textInput("specify_root",
                          "Which tip should be the root? Leave blank if you don't care."),                
            #}),
            actionButton("refresh_tree", "Generate new random tree topology."),
            br(),br(),
            h3("Display settings"),
            radioButtons("show_bl",
                         "Show branch lengths on tree?",
                         choices = c("Yes", "No"), selected = "No"),
            br(),
            selectInput("tree_orientation",
                         "Tree orientation:", 
                         choices = c("Right", "Left", "Up", "Down"), 
                         selected = "Right"),
            br(),
           # Placement too tricky, let's see if anyone requests it
           # conditionalPanel("input.show_bl == 'Yes'", {
           #   radioButtons("show_scale",
           #             "Show scale bar?", choices = c("Yes", "No"), selected = "Yes")                
           # }),
            sliderInput("space_between", 
                        "Add space before tip label:", min = 0, max = 0.2, value = 0, step = 0.01),
            sliderInput("thickness", 
                        "Branch thickness:", min = 1, max = 10, value = 2, step = 0.5),
            colourpicker::colourInput("branch_color", "Branch color:", value = "#000000"),
            sliderInput("tip_size", 
                        "Tip label size:", min = 1, max = 5, value = 2, step = 0.5),    
            colourpicker::colourInput("tip_color", "Tip color:", value = "#000000")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("treeplot", width = "800", height = "600"),
            br(),br(),br(),br(),
            div(style="display:inline-block;vertical-align:top;margin-right:1em;margin-top:1.75em;",
              downloadButton("download_tree", "Download tree figure as PNG")
            ),
            div(style="display:inline-block;vertical-align:top;margin-right:1em;",
                numericInput("download_height", "Downloaded figure height (inches)", min = 1, max = 15, value = 6, width = "250px")
            ),
            div(style="display:inline-block;vertical-align:top;margin-left:1em;",
                numericInput("download_width", "Downloaded figure width (inches)", min = 1, max = 15, value = 6, width = "250px")
            ),
            br(),br(),
            downloadButton("download_newick", "Download tree as Newick text file")
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  tree <- reactive({
    tip_labels_isolated <- isolate(tip_labels())
    random_tree <- ape::rtree(input$ntaxa, tip.label = tip_labels_isolated)
    compute.brlen(random_tree, runif, min = 0.1, max = 1)
  })
  
  output$custom_tip_stuff <- renderUI({
    list(
      textInput("custom_tipnames",
                "Enter names for tips, separated by commas:"),
      #fluidRow( 
      #  column(6, 
        actionButton("add_custom_tipnames", "Apply tip names.")
      #  ), column(6, 
     # actionButton("clear_custom_tipnames", "Clear tip names."))
    #), br(), br()
    )
  })
  
  
    
  custom_tip_labels <- reactiveVal(NULL)
  observeEvent(input$add_custom_tipnames, {
    split_names <- unique( stringr::str_split(input$custom_tipnames, ",\\s*")[[1]] )
    validate(need( length(split_names) == input$ntaxa,"Incorrect number of custom tip names. Did you repeat one by accident?"))
    custom_tip_labels(split_names)
  })             
 # observeEvent(input$clear_custom_tipnames, {
 #   custom_tip_labels(NULL)
 # })  
  
  tip_labels <- reactive({
    if (!(is.null(custom_tip_labels())) & input$tipnames_type %in% c("letters", "numbers")){
      custom_tip_labels(NULL)
    }
    if (input$tipnames_type == "letters") final <-LETTERS[1:input$ntaxa]
    if (input$tipnames_type == "numbers") final <- 1:input$ntaxa 
    if (input$tipnames_type == "custom") final <- NULL
    final
  })
  
  tree <- reactiveVal( NULL )
  
  observeEvent(input$refresh_tree, {
    tree( ape::rtree(input$ntaxa, br = runif, min = 0.1, max = 0.75) )
 })
  

  use_bl <- reactive({
    ifelse(input$show_bl == "Yes", TRUE, FALSE)
  })
  
  tree_orientation <- reactive({
    paste0(stringr::str_to_lower(input$tree_orientation), "wards") 
  })
  
  final_tip_labels <- reactive({
    tip_labels() # react please?
    
    if (!(is.null(custom_tip_labels()))){
      use_labels <- custom_tip_labels()
    } else {
      use_labels <- tip_labels()
    }
    if (is.null(use_labels)){
      root_taxa <- NULL
    } else {
      if (input$specify_root == "")
      {
        root_taxa <- sample(use_labels, 1)
      } else {
        validate(need(input$specify_root %in% use_labels, "Specified root taxa is not one of your tip labels."))
        root_taxa <- input$specify_root
      }
    }
    list("tip_labels" = use_labels, "root_taxa" = root_taxa)
  })

  output$treeplot <- renderPlot({
    
      plot_this_tree <- isolate(tree())
      plot_this_tree$tip.label <- final_tip_labels()$tip_labels
      
      if (is.null(tree()) | is.null(final_tip_labels()$tip_labels)){
        # lol hack galore.
       ggplot(iris) + theme_void()
      } else { 
          plot(root(plot_this_tree,
                    outgroup = final_tip_labels()$root_taxa,
                    resolve.root=TRUE
                    ),
               type = "phylogram",
               use.edge.length = use_bl(),
               edge.width = input$thickness,
               edge.color = input$branch_color,
               cex = input$tip_size, 
               tip.color = input$tip_color,
               no.margin = TRUE, 
               label.offset = input$space_between,
               direction = tree_orientation()) -> tree_plot
         # if (show_scale & use_bl) add.scale.bar(lwd = 2)
      }

  })

  output$download_tree <- shiny::downloadHandler(
    filename = function() {
      paste0("tree-", Sys.Date(), ".png", sep="")
    },
    content  = function(filename) {
      png(filename, width = input$download_width, height = input$download_height, units = "in", res = 1200)
      
      plot_this_tree <- isolate(tree())
      plot_this_tree$tip.label <- final_tip_labels()$tip_labels
      
      plot(root(plot_this_tree,
                outgroup = final_tip_labels()$root_taxa,
                resolve.root=TRUE
                ),
                type = "phylogram",
                use.edge.length = use_bl(),
                edge.width = input$thickness,
                edge.color = input$branch_color,
                cex = input$tip_size, 
                tip.color = input$tip_color,
                no.margin = TRUE, 
                direction = tree_orientation())
      dev.off()
    })    
  
  
  
  output$download_newick <- shiny::downloadHandler(
    filename = function() {
      paste0("tree-", Sys.Date(), ".nwk", sep="")
    },
    content  = function(filename) {
      plot_this_tree <- isolate(tree())
      plot_this_tree$tip.label <- final_tip_labels()$tip_labels
      write.tree(plot_this_tree, filename)
    })    
  
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
