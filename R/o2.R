ro2_ui <- miniPage(
  gadgetTitleBar("O2 Cheatsheet"),
  miniTabstripPanel(
    miniTabPanel(
      "Basics", icon = icon("user"),
      miniContentPanel(
        fillRow(flex = c(1, 1, 1), height = "50px",
                uiOutput("o2id"),
                uiOutput("remote"),
                uiOutput("local")),
        hr(),
        h5("Login"), send2termOutput("login"),
        h5("Mount home"), verbatimTextOutput("mount")
      )
    ),
    miniTabPanel(
      "Run", icon = icon("paper-plane"),
      miniContentPanel(
        fillRow(flex = c(1, 1, 1), height = "60px",
                uiOutput("partition"),
                uiOutput("duration"),
                uiOutput("mem")),
        fillRow(flex = c(1, 1, 1, 1, 1), height = "60px",
                uiOutput("n"),
                uiOutput("c"),
                uiOutput("N"),
                uiOutput("o"),
                uiOutput("gpu")),
        hr(),
        h5("Interactive Session"),
        verbatimTextOutput("run_int"),
        h5("Batch Run"),
        shinyFilesButton("file", "Select Script", "Select batch script" , FALSE),
        verbatimTextOutput("run_batch")
      )
    )
  )
)

ro2_server <- function(input, output, session) {
  observeEvent(input$done, {
    invisible(stopApp())
  })

  if (file.exists("~/.ro2-package/o2meta")) {
    init_meta <- readLines("~/.ro2-package/o2meta")
  } else {
    init_meta <- c("", "~", "~/o2-home", "0")
  }

  if (rstudioapi::terminalRunning(init_meta[4])) {
    rstudioapi::terminalActivate(init_meta[4])
    term_id <- init_meta[4]
  } else {
    term_id <- rstudioapi::terminalCreate(caption = "ro2")
  }

  output$o2id <- renderUI({
    textInput(
      "o2id", "eCommons ID", value = init_meta[1], width = "95%"
    )
  })

  output$remote <- renderUI({
    textInput(
      "remote", "Remote Folder", value = init_meta[2], width = "95%"
    )
  })

  output$local <- renderUI({
    textInput(
      "local", "Local Folder", value = init_meta[3], width = "95%"
    )
  })

  meta <- reactive({
    req(input$remote)
    if (input$remote == "~") {
      remote <- paste0("/home/", input$o2id)
    } else {
      remote <- input$remote
    }
    c(input$o2id, remote, input$local, term_id)
  })

  observeEvent(meta(), writeLines(meta(), "~/.ro2-package/o2meta"))

  meta_login <- reactive({
    paste0("ssh ", input$o2id, "@o2.hms.harvard.edu")
  })

  callModule(send2term, "login", code = meta_login, term_id = term_id)

  output$mount <- renderText({
    if (Sys.info()[["sysname"]] == "Darwin") {
      mac_options <- paste0(
        ",defer_permissions,noappledouble,negative_vncache,volname=",
        basename(input$local)
      )
    } else {
      mac_options <- NULL
    }
    paste0("sshfs -p 22 ", input$o2id, "@o2.hms.harvard.edu:",
           meta()[2], " ", input$local,
           " -oauto_cache,reconnect", mac_options)
  })

  if (file.exists("~/.ro2-package/o2job")) {
    init_job <- readLines("~/.ro2-package/o2job")
  } else {
    init_job <- c("short", "0-03:00:00", "2G", "", "", "", "", "1")
  }

  output$partition <- renderUI({
    selectInput(
      "partition", "Partition",
      choices = c("short", "gpu", "medium", "long",
                  "mpi", "priority", "transfer"),
      selected = init_job[1], width = "95%"
    )
  })

  output$duration <- renderUI({
    textInput(
      "duration", "Time Limit", value = init_job[2], width = "95%"
    )
  })

  output$mem <- renderUI({
    textInput(
      "mem", "Memory", value = init_job[3], width = "95%"
    )
  })

  output$n <- renderUI({
    textInput("n", "-n", value = init_job[4], width = "95%")
  })

  output$c <- renderUI({
    textInput("c", "-c", value = init_job[5], width = "95%")
  })

  output$N <- renderUI({
    textInput("N", "-N", value = init_job[6], width = "95%")
  })

  output$o <- renderUI({
    textInput("o", "-o", value = init_job[7], width = "95%")
  })

  output$gpu <- renderUI({
    textInput("gpu", "# GPU", value = init_job[8], width = "95%")
  })

  o2job <- reactive({
    c(input$partition, input$duration, input$mem,
      input$n, input$c, input$N, input$o, input$gpu)
  })

  observeEvent(o2job(), writeLines(o2job(), "~/.ro2-package/o2job"))

  job_options <- reactive({
    options <- c(
      paste("-p", input$partition),
      paste("-t", input$duration),
      paste0("--mem=", input$mem)
    )
    if (input$n != "") options <- c(options, paste("-n", input$n))
    if (input$c != "") options <- c(options, paste("-c", input$c))
    if (input$N != "") options <- c(options, paste("-N", input$N))
    if (input$o != "") options <- c(options, paste("-o", input$o))
    if (input$partition == "gpu") {
      options <- c(options, paste0("--gres=gpu:", input$gpu))
    }
    options <- paste(options, collapse = " ")
    return(options)
  })

  output$run_int <- renderText({
    req(input$partition)
    paste0("srun --pty ", job_options(), " /bin/bash")
  })

  shinyFileChoose(input, 'file', roots=c(root = "~"))

  script_path <- reactive({
    req(input$file)
    paths <- unlist(input$file$files$`0`)
    paths[1] <- "~"
    path <- normalizePath(paste(paths, collapse = .Platform$file.sep))
    path_local <- normalizePath(input$local)
    path <- substr(path, nchar(path_local) + 1, nchar(path))
    paste0(meta()[2], path)
  })

  output$run_batch <- renderText({
    req(input$partition, script_path())
    paste("sbatch", job_options(), script_path())
  })

}

runGadget(ro2_ui, ro2_server, viewer = paneViewer())

# ro2_addin <- function() {
#   runGadget(ro2_ui, ro2_server, viewer = paneViewer())
# }
