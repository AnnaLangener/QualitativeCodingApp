source(file.path("R", "coding_app.R"))
source(file.path("R", "coding_modes.R"))

options(shiny.maxRequestSize = 100 * 1024^2)

file_picker_control <- function(input_id, output_id, label) {
  shiny::tags$div(
    class = "form-group native-file-input",
    shiny::tags$label(label),
    shiny::tags$div(
      class = "native-file-row",
      shiny::actionButton(input_id, "Browse"),
      shiny::textOutput(output_id, inline = TRUE)
    )
  )
}

launcher_content <- function() {
  shiny::tags$div(
    class = "launcher",
    shiny::tags$h1("Qualitative Coding"),
    shiny::tags$p(
      class = "lead",
      "Choose the coding activity you want to use."
    ),
    shiny::tags$div(
      class = "mode-options",
      shiny::radioButtons(
        "coding_mode",
        NULL,
        choices = c(
          "Deductive coding — apply existing codebooks" = "deductive",
          "Inductive coding: Phase 1 — familiarize and propose codes" =
            "inductive_phase1",
          "Inductive coding: Phase 2 — use the developing codebook" =
            "inductive_phase2"
        ),
        selected = "deductive"
      )
    ),
    shiny::tags$div(
      class = "launcher-fields",
      shiny::textInput(
        "coder",
        "Coder (user)",
        placeholder = "Enter your name or coder ID"
      ),
      shiny::textInput(
        "participant_id",
        "Participant ID",
        placeholder = "Enter the participant to code"
      )
    ),
    shiny::tags$div(
      class = "launcher-files",
      file_picker_control(
        "choose_data_file",
        "selected_data_file",
        "Data file"
      ),
      shiny::tags$div(
        class = "codebook-inputs",
        shiny::conditionalPanel(
          condition = "input.coding_mode == 'deductive'",
          shiny::tags$h2("Codebook files"),
          shiny::tags$div(
            class = "codebook-grid",
            shiny::fileInput(
              "thought_codebook",
              "Thought codebook — select the thought codebook file",
              accept = c(".csv", ".xls", ".xlsx"),
              buttonLabel = "Browse",
              placeholder = "No file selected",
              width = "100%"
            ),
            shiny::fileInput(
              "activity_codebook",
              "Activity codebook — select the activity codebook file",
              accept = c(".csv", ".xls", ".xlsx"),
              buttonLabel = "Browse",
              placeholder = "No file selected",
              width = "100%"
            ),
            shiny::fileInput(
              "location_codebook",
              "Location codebook — select the location codebook file",
              accept = c(".csv", ".xls", ".xlsx"),
              buttonLabel = "Browse",
              placeholder = "No file selected",
              width = "100%"
            ),
            shiny::fileInput(
              "company_codebook",
              "Company codebook — select the company codebook file",
              accept = c(".csv", ".xls", ".xlsx"),
              buttonLabel = "Browse",
              placeholder = "No file selected",
              width = "100%"
            )
          )
        )
      ),
      shiny::tags$div(
        class = "codebook-inputs",
        shiny::conditionalPanel(
          condition = "input.coding_mode == 'inductive_phase2'",
          shiny::fileInput(
            "event_codebook",
            "Event codebook — select the event codebook file",
            accept = c(".csv", ".xls", ".xlsx"),
            buttonLabel = "Browse",
            placeholder = "No file selected",
            width = "100%"
          )
        )
      )
    ),
    shiny::actionButton(
      "start_mode",
      "Start Coding",
      class = "btn-primary btn-lg"
    )
  )
}


ui <- shiny::fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      body {
        background: #f7faf9;
      }
      .launcher {
        max-width: 780px;
        margin: 8vh auto 0;
        padding: 2.5rem;
        background: white;
        border: 1px solid #dce7e3;
        border-radius: 1rem;
        box-shadow: 0 0.75rem 2rem rgba(30, 70, 60, 0.08);
      }
      .launcher h1 {
        margin-bottom: 0.5rem;
      }
      .mode-options {
        margin: 2rem 0;
      }
      .mode-options .radio,
      .mode-options .form-check {
        margin: 0.75rem 0;
        padding: 1rem 1.1rem 1rem 2.25rem;
        border: 1px solid #cbdad5;
        border-radius: 0.65rem;
      }
      .mode-options .radio:has(input:checked),
      .mode-options .form-check:has(input:checked) {
        border-color: #78c2ad;
        background: #edf8f5;
      }
      .mode-options label {
        width: 100%;
        cursor: pointer;
      }
      .launcher-fields {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 1rem;
        margin-bottom: 1.5rem;
      }
      .launcher-files {
        margin-bottom: 1.5rem;
      }
      .launcher-files > .form-group,
      .codebook-inputs > .form-group {
        max-width: 100%;
      }
      .native-file-row {
        display: flex;
        align-items: center;
        gap: 0.75rem;
      }
      .native-file-row .shiny-text-output {
        color: #5f6f69;
        overflow-wrap: anywhere;
      }
      .codebook-inputs h2 {
        margin: 1.25rem 0 0.75rem;
        font-size: 1.1rem;
      }
      .codebook-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 0.5rem 1rem;
      }
      .codebook-grid .form-group {
        min-width: 0;
      }
      @media (max-width: 576px) {
        .launcher-fields,
        .codebook-grid {
          grid-template-columns: 1fr;
        }
      }
      .launcher-note {
        margin: 1rem 0 0;
        color: #5f6f69;
      }
      .coding-toolbar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 1rem;
        margin: 1rem 0;
      }
      .coding-app-content {
        width: 100%;
      }
    "))
  ),
  shiny::uiOutput("root_ui")
)


server <- function(input, output, session) {
  active_mode <- shiny::reactiveVal(NULL)
  data_file <- shiny::reactiveVal(NULL)

  require_file <- function(file, label) {
    if (is.null(file) || nrow(file) != 1 || !nzchar(file$datapath[[1]])) {
      stop("Select ", label, " before starting.")
    }

    file
  }

  selected_files <- function(mode) {
    files <- list(
      data = data_file()
    )
    if (is.null(files$data)) {
      stop("Select a data file before starting.")
    }

    files$codebooks <- switch(
      mode,
      deductive = list(
        thought = require_file(
          input$thought_codebook,
          "a thought codebook"
        ),
        activity = require_file(
          input$activity_codebook,
          "an activity codebook"
        ),
        location = require_file(
          input$location_codebook,
          "a location codebook"
        ),
        company = require_file(
          input$company_codebook,
          "a company codebook"
        )
      ),
      inductive_phase1 = NULL,
      inductive_phase2 = list(
        event = require_file(input$event_codebook, "an event codebook")
      )
    )

    files
  }

  output$selected_data_file <- shiny::renderText({
    path <- data_file()
    if (is.null(path)) "No file selected" else basename(path)
  })

  shiny::observeEvent(input$choose_data_file, {
    clicks <- input$choose_data_file
    if (is.null(clicks) || clicks < 1) {
      return()
    }

    path <- tryCatch(
      choose_tabular_file("Select the ESM data file to open"),
      error = function(error) {
        shiny::showNotification(conditionMessage(error), type = "warning")
        NULL
      }
    )
    if (!is.null(path)) {
      data_file(path)
    }
  }, ignoreNULL = FALSE)

  output$root_ui <- shiny::renderUI({
    mode <- active_mode()

    if (is.null(mode)) {
      return(launcher_content())
    }

    shiny::tagList(
      shiny::tags$div(
        class = "coding-toolbar",
        shiny::tags$strong(mode$title),
        shiny::actionButton("change_mode", "Change coding activity")
      ),
      mode$content
    )
  })

  shiny::observeEvent(input$start_mode, {
    shiny::req(input$coding_mode)

    coder <- trimws(input$coder)
    participant_id <- trimws(input$participant_id)

    if (!nzchar(coder) || !nzchar(participant_id)) {
      shiny::showNotification(
        "Enter both a coder and a participant ID before starting.",
        type = "warning"
      )
      return()
    }

    identifiers <- tryCatch(
      list(
        coder = validate_storage_identifier(coder, "Coder"),
        participant_id = validate_storage_identifier(
          participant_id,
          "Participant ID"
        )
      ),
      error = function(error) {
        shiny::showNotification(conditionMessage(error), type = "warning")
        NULL
      }
    )
    if (is.null(identifiers)) {
      return()
    }

    coder <- identifiers$coder
    participant_id <- identifiers$participant_id

    files <- tryCatch(
      selected_files(input$coding_mode),
      error = function(error) {
        shiny::showNotification(conditionMessage(error), type = "warning")
        NULL
      }
    )
    if (is.null(files)) {
      return()
    }

    mode <- tryCatch(
      create_coding_mode(
        input$coding_mode,
        coder,
        participant_id,
        files$data,
        files$codebooks
      ),
      error = function(error) {
        shiny::showModal(shiny::modalDialog(
          title = "The coding activity could not be started",
          shiny::tags$p(conditionMessage(error)),
          shiny::tags$p(paste(
            "Check that the selected files have the expected format",
            "and that the data file's folder is writable."
          )),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        ))
        NULL
      }
    )
    shiny::req(!is.null(mode))

    mode$server(input, output, session)
    active_mode(mode)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$change_mode, {
    session$reload()
  }, ignoreInit = TRUE)
}


shiny::shinyApp(ui, server)
