#' Create an Code filed and send to terminal button
#'
#' @export
sendTermOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fillRow(flex = c(7, 1), height = "50px",
            verbatimTextOutput(ns("codeUI")),
            uiOutput(ns("buttonUI"))
    )
  )
}

#' @export
sendTerm <- function(input, output, session, code, term_id,
                     execute = function(){TRUE}) {
  ns <- session$ns

  output$codeUI <- renderText({
    code()
  })

  output$buttonUI <- renderUI({
    actionButton(ns("button"), label = icon("play"), width = "95%",
                 style = "height: 39px; margin-left: 2px;")
  })

  observeEvent(input$button, {
    rstudioapi::terminalActivate(term_id)
    exe_code <- code()
    if (execute()) {
      exe_code <- paste0(exe_code, "\n")
    }
    rstudioapi::terminalSend(term_id, exe_code)
  })
}
