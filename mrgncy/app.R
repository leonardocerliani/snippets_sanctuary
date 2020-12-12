#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

df <- read.csv('snippets.csv', stringsAsFactors=FALSE)

library(shiny)
library(dplyr)
library(feather)
library(DT)
source("myfunc.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Snippet sanctuary"),

    h3("Add a snippet"),
    textInput(inputId = "description", label = "", placeholder = "description", width = '100%'),
    
    # NB: the inputID MUST be "codebox"
    codeAreaInput(
        inputId = "codebox", label = "", 
        cols = 80, rows = 10, 
        placeholder = "write snippet here"
    ) %>% shiny::tagAppendAttributes(style = 'width: 100%;'),
    
    actionButton("doprint", "Enter"),
    
    HTML("<br><br>"),
    

    h3("Find the snippet"),
    div(
        DTOutput('allsnippets'),
        style="font-family: monospace;"
    ),
    
    HTML("<br><br>"),
    
    verbatimTextOutput("selectedSnippet")


)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # Show the initial table
    output$allsnippets = renderDT(
        datatable(
            df %>% select(description), 
            filter = 'top', rownames = FALSE
        ) 
    )
    

    
    # Show selected snippet
    output$selectedSnippet <- renderText({
        req(input$allsnippets_rows_selected)
        idx <- input$allsnippets_rows_selected %>% as.numeric()
        
        paste0(
            paste0('# ',df$description[idx]),'\n',
            df$code[idx],'\n\n\n'
        )


    })
    
    
    observeEvent(input$doprint, {
        
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
        
        # Show the updated table
        output$allsnippets = renderDT(
            df %>% select(description), 
            filter = 'top', rownames = FALSE
        )
        
        
        # Save the snippet
        write.csv(df, 'snippets.csv', row.names = FALSE)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)









