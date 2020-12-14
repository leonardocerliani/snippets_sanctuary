#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

df <- read.csv('snippets.csv', stringsAsFactors=FALSE)
df_OLE <- read.csv('snippets.csv', stringsAsFactors=FALSE)

library(shiny)
library(dplyr)
library(DT)
library(fresh)
source("libfunc.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    use_googlefont("Quicksand"),
    use_theme(create_theme(
        theme = "default",
        bs_vars_font(
            family_sans_serif = "'Quicksand'"
        )
    )),

    # Application title
    titlePanel("Snippet sanctuary"),

    h3("Add a snippet"),
    textInput(inputId = "description", label = "", placeholder = "description", width = '100%'),
    
    # NB: the inputID MUST be "codebox"
    codeAreaInput(
        inputId = "codebox", label = "", 
        cols = 80, rows = 5, 
        placeholder = "write snippet here"
    ) %>% shiny::tagAppendAttributes(style = 'width: 100%;'),
    
    
    conditionalPanel(
        condition = "input.allsnippets_rows_selected == 0",
        actionButton("addsnippet", "Add snippet"),
    ),
    

    conditionalPanel(
        condition = "input.allsnippets_rows_selected > 0",
        actionButton("editsnippet", "Save edited snippet"),
        actionButton("deletesnippet", "Delete snippet")
    ),


    HTML("<br><br>"),
    

    h3("Find the snippet"),
    div(
        DTOutput('allsnippets'),
        # style="font-family: monospace;"
    ),
    
    HTML("<br><br>"),
    
    # verbatimTextOutput("selectedSnippet"),
    
    actionButton("undoeverything", "Undo everything - All changes will be lost")

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Show the initial table
    output$allsnippets = renderDTfunc(df)
    
    
    # Show selected snippet
    output$selectedSnippet <- renderText({
        req(input$allsnippets_rows_selected)
        idx <- input$allsnippets_rows_selected %>% as.numeric()
        
        paste0(
            paste0('# ',df$description[idx]),'\n',
            df$code[idx],'\n\n\n'
        )
    })
    
    
    # Add snippet when 'Enter' is pressed
    observeEvent(input$addsnippet, {
        
        # Add the new snippet. Note the "<<-" !!!
        df <<- df %>% add_row(
            description = input$description,
            code = input$codebox
        )

        # Reset the input textboxes
        for (ith_id in c("description","codebox")) {
            updateTextAreaInput(
                session, inputId = ith_id, value=""
            )    
        }
        
        # Show the updated table and save the snippet
        output$allsnippets = renderDTfunc(df)
        write.csv(df, 'snippets.csv', row.names = FALSE)
        
    })
    
    
    # Edit snippet (1): display Edit button and populate editor upon row selection
    observeEvent(!is.null(input$allsnippets_rows_selected), {
        
        rownumba = input$allsnippets_rows_selected %>% as.numeric()

        updateTextAreaInput(
            session, 'description',
            value = df$description[rownumba]
        )

        updateTextAreaInput(
            session, 'codebox',
            value = df$code[rownumba]
        )
        
    })
    
    # Edit snippet (2): change the entry in df upon Edit button press
    observeEvent(input$editsnippet, {
        
        rownumba = input$allsnippets_rows_selected %>% as.numeric()
        
        df$description[rownumba] <<- input$description
        df$code[rownumba] <<- input$codebox
        
        # Show the updated table and save the snippet
        output$allsnippets <-  renderDTfunc(df)

        write.csv(df, 'snippets.csv', row.names = FALSE)
    })
    
    
    # Delete snippet
    observeEvent(input$deletesnippet, {
        
        rownumba = input$allsnippets_rows_selected %>% as.numeric()

        df <<- df[-c(rownumba), ]
        
        # Show the updated table and save the snippet
        output$allsnippets = renderDTfunc(df)
        write.csv(df, 'snippets.csv', row.names = FALSE)
    })
    
    
    # Undo everything
    observeEvent(input$undoeverything, {
        df <<- df_OLE
        output$allsnippets = renderDTfunc(df)
        write.csv(df, 'snippets.csv', row.names = FALSE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)





