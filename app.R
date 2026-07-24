source(file.path("R", "coding_app.R"))
source(file.path("R", "coding_modes.R"))

# Change these values when assigning coders and participants.
coding_settings <- list(
  deductive = list(user = "MS", participant_id = 11347),
  inductive_phase1 = list(user = "JC", participant_id = 10254),
  inductive_phase2 = list(user = "JC", participant_id = 27311)
)


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
    shiny::actionButton(
      "start_mode",
      "Choose project folder and start",
      class = "btn-primary btn-lg"
    ),
    shiny::tags$p(
      class = "launcher-note",
      paste(
        "Your data stays on this computer. The selected activity",
        "determines where results are saved."
      )
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

    project_directory <- tryCatch(
      choose_project_directory(),
      error = function(error) {
        shiny::showNotification(
          conditionMessage(error),
          type = "warning"
        )
        NULL
      }
    )
    shiny::req(!is.null(project_directory))

    mode <- tryCatch(
      create_coding_mode(
        input$coding_mode,
        project_directory,
        coding_settings[[input$coding_mode]]
      ),
      error = function(error) {
        shiny::showModal(shiny::modalDialog(
          title = "The coding activity could not be started",
          shiny::tags$p(conditionMessage(error)),
          shiny::tags$p(paste(
            "Check that the selected project folder contains the",
            "expected data and codebook files."
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
