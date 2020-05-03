info_card <- function(title, value, sub_value,
                      main_icon = "chart-line", sub_icon = "arrow-up",
                      bg_color = "default", text_color = "default", sub_text_color = "success"){
  div(
    class = "panel panel-default",
    div(
      class = str_glue("panel-body bg-{bg_color} text-{text_color}"),
      p(class = "pull-right", icon(class = "fa-3x", main_icon)), #fa-3x is 3 times the normal size
      h4(title),
      h5(value),
      p(
        class = str_glue("text-{sub_text_color}"),
        icon(sub_icon),
        tags$small(sub_value)
      )
    )
  )
  
  
  
}