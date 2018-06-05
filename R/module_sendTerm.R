#' Create an Code field and send to terminal button output
#'
#' @description Render a sendTerm shiny module on an application page.
#'
#' @param id output variable to be rendered
#'
#' @export
sendTermOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fillRow(flex = c(7, 1), height = "45px",
            uiOutput(ns("codeUI")),
            uiOutput(ns("buttonUI"))
    )
  )
}

#' Create an Code field and send to terminal button shiny module
#'
#' @param input input for the shiny session
#' @param output output for the shiny session
#' @param session session for the shiny session
#' @param code a reactive object for the code to be printed and executed
#' @param term_id RStudio terminal ID
#' @param execute a reactive object for whether to let the code execute after
#' clicking the play button.
#'
#' @export
sendTerm <- function(input, output, session, code, term_id,
                     execute = function(){TRUE}) {
  ns <- session$ns

  output$codeUI <- renderUI({
    textInput(ns("codeinput"), label = NULL, value = code(), width = "100%")
  })

  output$buttonUI <- renderUI({
    actionButton(ns("button"), label = icon("play"), width = "95%",
                 style = "height: 35px; margin-left: 2px;")
  })

  observeEvent(input$button, {
    rstudioapi::terminalActivate(term_id)
    exe_code <- input$codeinput
    if (execute()) {
      exe_code <- paste0(exe_code, "\n")
    }
    rstudioapi::terminalSend(term_id, exe_code)
  })
}
