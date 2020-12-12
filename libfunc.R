
library(shiny)
library(dplyr)


codeInputLabel <- function(inputId, label = NULL) {
  tags$label(
    label,
    class = "control-label",
    class = if (is.null(label)) "shiny-label-null",
    # `id` attribute is required for `aria-labelledby` used by screen readers:
    id = paste0(inputId, "-label"),
    `for` = inputId
  )
}




codeAreaInput <- function (inputId='codebox', label, value = "", width = NULL, height = NULL, 
                           cols = NULL, rows = NULL, placeholder = NULL, resize = NULL) 
{
  value <- restoreInput(id = inputId, default = value)
  if (!is.null(resize)) {
    resize <- match.arg(resize, c("both", "none", "vertical", 
                                  "horizontal"))
  }
  style <- paste(if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), if (!is.null(height)) 
      paste0("height: ", validateCssUnit(height), ";"), if (!is.null(resize)) 
        paste0("resize: ", resize, ";"))
  
  if (length(style) == 0) 
    style <- NULL
  
  div(
    class = "form-group shiny-input-container", 
    codeInputLabel(inputId, label), 
    tags$textarea(
      id = inputId, class = "form-control", 
      placeholder = placeholder, 
      style = "font-family: monospace; tab-size: 3;",
      rows = rows, 
      cols = cols, value
    ),
    tags$script(src = "gettab.js")
  )
}



# Function to render the table. There can be many parameters, so here is better
renderDTfunc <- function(df) {
  renderDT(
    datatable(
      df %>% dplyr::select(description), 
      filter = 'top', 
      rownames = FALSE,
      selection = "single"
    ) 
  )
}






