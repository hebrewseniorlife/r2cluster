r2c_ui <- miniPage(
  gadgetTitleBar("Cluster Assistant", left = NULL),
  miniTabstripPanel(
    miniTabPanel(
      "Login", icon = icon("user"),
      miniContentPanel(
        fillRow(flex = c(1, 1, 1, 1, 1), height = "60px",
                uiOutput("url"),
                uiOutput("port"),
                uiOutput("id"),
                uiOutput("remote"),
                uiOutput("local")),
        hr(),
        h4("On your local machine:"),
        h5("Login"), sendTermOutput("login"),
        h5("Mount Folder"), sendTermOutput("mount"),
        h5("Unmount Folder"), sendTermOutput("unmount")
      )
    ),
    miniTabPanel(
      "Slurm", icon = icon("paper-plane"),
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
        h4("On the Cluster server:"),
        radioGroupButtons(
          inputId = "module", label = "Module Related",
          choices = c("Default" = "gcc R",
                      "Search possible modules" = "spider",
                      "List loaded modules" = "list",
                      "Save current setups" = "s",
                      "Restore saved setups" = "r")
        ),
        sendTermOutput("run_module"),
        hr(),
        h5("Interactive Session"),
        sendTermOutput("run_int"),
        tags$div(
          h5("Batch Run", style = "float: left;"),
          tags$div(
            style = "float: right;",
            shinyFilesButton("file", "Select Script", "Select batch script" , FALSE)
          )
        ),
        sendTermOutput("run_batch"),
        actionButton("check_batch", "Check Batch Job Status", icon("chalkboard"))
      )
    ),
    miniTabPanel(
      "ssh-keygen", icon = icon("lock"),
      miniContentPanel(
        h4("On your local machine:"),
        h5("1. Generate ssh RSA key"),
        fillRow(
          flex = c(6, 2), height = "70px",
          tagList(
            p("Follow instruction on the screen and create a set of ssh RSA keys (private & public)."),
            p("Do not enter any passphrase for passwordless login. ")
          ),
          div(style = "min-width: 160px;",
              textInput("sshkey_file", "Enter sshkey file name", "id_rsa", width = "100%"))
        ),
        sendTermOutput("run_sshkeygen"),
        h5("2. Setup ssh key config file"),
        p("Put something like below in ~/.ssh/config. Make changes to the last line if necessary."),
        fillRow(
          flex = c(7, 1), height = "100px",
          verbatimTextOutput("sshkey_config"), uiOutput("sshkey_config_btn")
        ),
        h5("3. Copy the ssh pub key file to the O2 server & login"),
        sendTermOutput("run_sshkey_scp"),
        sendTermOutput("run_ssh_login"),
        hr(),
        h4("On O2 server:"),
        h5("4. Put pub key into authorized_keys file"),
        fillRow(
          flex = c(7, 1), height = "180px",
          uiOutput("sshkey_auth"),
          uiOutput("sshkey_auth_btn")
        )

      )
    )
  )
)

r2c_server <- function(input, output, session) {
  observeEvent(input$done, {
    invisible(stopApp())
  })

  # Login =====================================================================

  if (file.exists("~/.r2cmeta")) {
    init_meta <- read.dcf("~/.r2cmeta")
  } else {
    init_meta <- c("", "~", "~/cluster_home", "o2.hms.harvard.edu", "22")
    names(init_meta) <- c("username", "remoteFolder", "localFolder", "url", "port")
  }

  if (length(rstudioapi::terminalList()) != 0) {
    terms <- rstudioapi::terminalList()
    term_caption <- sapply(terms, function(x){terminalContext(x)["caption"]})
    if ("r2c" %in% term_caption) {
      term_id <- terms[term_caption == "r2c"]
    } else {
      term_id <- rstudioapi::terminalCreate(caption = "r2c")
    }
  } else {
    term_id <- rstudioapi::terminalCreate()
  }
  rstudioapi::terminalActivate(term_id)

  output$id <- renderUI({
    textInput(
      "id", "Username", value = init_meta[1], width = "95%"
    )
  })

  output$url <- renderUI({
    textInput(
      "url", "Cluster URL", value = init_meta[4], width = "95%"
    )
  })

  output$port <- renderUI({
    textInput(
      "port", "Cluster port", value = init_meta[5], width = "95%"
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
      remote <- paste0("/home/", input$id)
    } else {
      remote <- input$remote
    }
    out <- c(input$id, remote, input$local, input$url, input$port)
    names(out) <- c("username", "remoteFolder", "localFolder", "url", "port")
    return(out)
  })

  observeEvent(meta(), write.dcf(meta(), "~/.r2cmeta"))

  meta_login <- reactive({
    paste0("ssh ", input$id, "@", input$url)
  })

  callModule(sendTerm, "login", code = meta_login, term_id = term_id)

  meta_mount <- reactive({
    if (Sys.info()[["sysname"]] == "Darwin") {
      extra_options <- paste0(
        ",defer_permissions,noappledouble,negative_vncache,volname=",
        basename(input$local)
      )
    } else {
      extra_options <- ""
    }
    paste0("sshfs -p ", input$port, " ", input$id, "@", input$url, ":",
           meta()[2], " ", input$local,
           " -oauto_cache", extra_options)
  })

  callModule(sendTerm, "mount", code = meta_mount, term_id = term_id)

  meta_unmount <- reactive({
    if (Sys.info()[["sysname"]] == "Darwin") {
      paste("umount", input$local)
    } else {
      paste("fusermount -u", input$local)
    }
  })

  callModule(sendTerm, "unmount", code = meta_unmount, term_id = term_id)

  # Run =======================================================================

  if (file.exists("~/.r2cjob")) {
    init_job <- read.dcf("~/.r2cjob")
  } else {
    init_job <- c("short", "0-03:00:00", "2G", "", "", "", "", "1")
    names(init_job) <- c("partition", "duration", "memory", "n",
                         "c", "N", "o", "gpu")
  }

  output$partition <- renderUI({
    textInput(
      "partition", "Partition", value = init_job[1], width = "95%"
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

  r2cjob <- reactive({
    req(input$partition)
    out <- c(input$partition, input$duration, input$mem,
             input$n, input$c, input$N, input$o, input$gpu)
    names(out) <- c("partition", "duration", "memory", "n", "c", "N", "o", "gpu")
    return(out)
  })

  observeEvent(r2cjob(), write.dcf(r2cjob(), "~/.r2cjob"))

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

  job_int <- reactive({
    req(input$partition)
    paste0("srun --pty ", job_options(), " /bin/bash")
  })

  callModule(sendTerm, "run_int", code = job_int, term_id = term_id)


  shinyFileChoose(input, 'file', roots =  c(home = "~"))

  script_path <- reactive({
    req(input$file)
    paths <- unlist(input$file$files$`0`)
    paths[1] <- "~"
    path <- normalizePath(paste(paths, collapse = .Platform$file.sep))
    path_local <- normalizePath(input$local)
    path <- substr(path, nchar(path_local) + 1, nchar(path))
    paste0(meta()[2], path)
  })

  job_batch <- reactive({
    req(input$partition)
    if (isTruthy(input$file)) {
      return(paste("sbatch", job_options(), script_path(),
                   "-o out.%j -e err.%j"))
    }
    return("Select a .sh Script")
  })

  observeEvent(input$check_batch, {
    rstudioapi::terminalActivate(term_id)
    rstudioapi::terminalSend(term_id, "squeue -u $USER\n")
  })


  callModule(sendTerm, "run_batch", code = job_batch, term_id = term_id)

  module_code <- reactive({
    paste("module", input$module)
  })

  callModule(sendTerm, "run_module", code = module_code, term_id = term_id)


  # sshkey-gen ================================================================
  sshkeygen <- reactive({
    req(input$id)
    paste0('ssh-keygen -t rsa -C ', '"', input$id, '"')
  })

  callModule(sendTerm, "run_sshkeygen", code = sshkeygen, term_id = term_id)

  output$sshkey_config <- renderText({
    paste0("Host o2 o2.hms.harvard.edu\n AddKeysToAgent yes\n HostName o2.hms.harvard.edu\n IdentityFile ~/.ssh/", input$sshkey_file)
  })

  output$sshkey_config_btn <- renderUI({
    actionButton("sshkey_config_file", label = icon("play"), width = "95%",
                 style = "height: 93px; margin-left: 2px;")
  })

  observeEvent(input$sshkey_config_file, {
    file.edit("~/.ssh/config")
  })

  sshkey_scp <- reactive({
    req(input$id)
    paste0('scp ~/.ssh/', input$sshkey_file, ".pub ", input$id, '@o2.hms.harvard.edu:')
  })

  callModule(sendTerm, "run_sshkey_scp", code = sshkey_scp, term_id = term_id)

  callModule(sendTerm, "run_ssh_login", code = meta_login, term_id = term_id)

  output$sshkey_auth <- renderUI({
    tagList(
      textInput("sshkey_auth_1", NULL, "mkdir -p ~/.ssh", width = "100%"),
      textInput("sshkey_auth_2", NULL, "touch ~/.ssh/authorized_keys", width = "100%"),
      textInput("sshkey_auth_3", NULL,
                paste0("cat ~/", input$sshkey_file, ".pub >> ~/.ssh/authorized_keys"), width = "100%"),
      textInput("sshkey_auth_4", NULL, paste0("rm ~/", input$sshkey_file, ".pub"), width = "100%")
    )
  })

  output$sshkey_auth_btn <- renderUI({
    actionButton("sshkey_auth_run", label = icon("play"), width = "95%",
                 style = "height: 180px; margin-left: 2px;")
  })

  observeEvent(input$sshkey_auth_run, {
    rstudioapi::terminalActivate(term_id)
    rstudioapi::terminalSend(term_id, paste0(input$sshkey_auth_1, "\n"))
    rstudioapi::terminalSend(term_id, paste0(input$sshkey_auth_2, "\n"))
    rstudioapi::terminalSend(term_id, paste0(input$sshkey_auth_3, "\n"))
    rstudioapi::terminalSend(term_id, paste0(input$sshkey_auth_4, "\n"))
  })
}

#' r2cluster RStudio Addin
#'
#' @export
r2c_addin <- function() {
  runGadget(r2c_ui, r2c_server, viewer = paneViewer())
}
