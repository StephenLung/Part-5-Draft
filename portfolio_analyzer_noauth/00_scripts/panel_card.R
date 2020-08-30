
panel_card <- function(title, ..., footer = NULL){
  
  # sets footer flag as NULL, if there footer is not NULL, execute the code
  ftr <- NULL
  if (!is.null(footer)) ftr <- div(class = "panel-footer",footer)
  
  div(
    class = "panel",
    div(
      class = "panel-header",
      h4(title)
    ),
    div(
      class = "panel-body",
      ...
    ),
    ftr

  )
}