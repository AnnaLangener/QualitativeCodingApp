required_packages <- c(
  "dplyr",
  "readxl",
  "tcltk",
  "shiny",
  "DT",
  "stringr",
  "bslib",
  "shinycssloaders"
)

invisible(lapply(required_packages, library, character.only = TRUE))


choose_project_directory <- function() {
  project_directory <- tcltk::tk_choose.dir(
    default = "",
    caption = "Select directory"
  )
  paste0(project_directory, "/")
}


load_coding_data <- function(project_directory, data_path, columns) {
  readxl::read_excel(file.path(project_directory, data_path)) |>
    dplyr::select(dplyr::all_of(columns)) |>
    dplyr::arrange(Participant_ID, Obs)
}


select_participant_data <- function(data, id_column, participant_id) {
  dplyr::filter(data, .data[[id_column]] == participant_id)
}


prepare_codebook_choices <- function(codebook, has_levels) {
  if (!has_levels) {
    return(codebook$Code)
  }

  codebook <- codebook[, colnames(codebook) %in% c("Level", "Code")]
  styled_codebook <- codebook |>
    dplyr::mutate(
      html = ifelse(
        Level == "1",
        paste0("<span style='color:#9F73AB';>", Code, "</span>"),
        ifelse(
          Level == "2",
          paste0("<span style='color:#19376D';>", Code, "</span>"),
          paste0("<span style='color:#0C7B93';>", Code, "</span>")
        )
      )
    )

  stats::setNames(styled_codebook$Code, styled_codebook$html)
}


coding_text_field <- function(column, id, storage_column, placeholder) {
  list(
    column = column,
    input_prefix = paste0("selectize_", id),
    output_prefix = paste0("selectize_wrap_", id),
    storage_column = storage_column,
    type = "text",
    placeholder = placeholder
  )
}


coding_selectize_field <- function(column, id, storage_column, choices) {
  list(
    column = column,
    input_prefix = paste0("selectize_", id),
    output_prefix = paste0("selectize_wrap_", id),
    storage_column = storage_column,
    type = "selectize",
    choices = choices
  )
}


familiarization_fields <- function(
  proposed_event_id,
  existing_code_choices = NULL
) {
  fields <- list(
    coding_text_field(
      "Familiarization note: beep",
      "general",
      1,
      "Beep level"
    ),
    coding_text_field(
      "Familiarization note: day",
      "depth",
      2,
      "Day level"
    ),
    coding_text_field(
      "Proposed event code",
      proposed_event_id,
      3,
      "Proposed event code"
    )
  )

  if (!is.null(existing_code_choices)) {
    fields <- append(
      fields,
      list(
        coding_selectize_field(
          "Existing event code",
          "event_existing",
          4,
          existing_code_choices
        )
      )
    )
  }

  fields
}


field_columns <- function(fields) {
  vapply(fields, `[[`, character(1), "column")
}


familiarization_column_order <- function(fields) {
  c(
    "Participant_ID", "Day", "Obs", "Time1",
    "Thought", "Activity", "Location", "Company", "Event",
    field_columns(fields)
  )
}


initialize_coding_storage <- function(
  project_directory,
  storage_directory,
  user,
  participant_id,
  participant_data,
  storage_columns
) {
  user_directory <- if (is.null(storage_directory)) {
    file.path(project_directory, user)
  } else {
    file.path(project_directory, storage_directory, user)
  }

  if (!file.exists(user_directory)) {
    dir.create(user_directory)
  }

  coding_path <- file.path(
    user_directory,
    paste0("Act_", participant_id, ".csv")
  )

  if (!file.exists(coding_path)) {
    empty_data <- as.data.frame(
      matrix(
        NA,
        nrow = nrow(participant_data),
        ncol = length(storage_columns)
      )
    )
    names(empty_data) <- make.names(storage_columns, unique = TRUE)
    utils::write.csv(empty_data, coding_path)
  }

  coding_path
}


level_legend <- function() {
  level_style <- function(color) {
    paste0(
      "border: 1px solid ", color,
      "; padding: 5px; margin: 5px; display: inline-block;"
    )
  }

  shiny::tagList(
    shiny::tags$div(
      style = level_style("#0C7B93"),
      shiny::tags$span("Level 3", style = "color: #0C7B93;")
    ),
    shiny::tags$div(
      style = level_style("#19376D"),
      shiny::tags$span("Level 2", style = "color: #19376D;")
    ),
    shiny::tags$div(
      style = level_style("#9F73AB"),
      shiny::tags$span("Level 1", style = "color: #9F73AB;")
    )
  )
}


coding_ui <- function(tab_title, show_levels = FALSE, tab_intro = NULL) {
  tab_contents <- list()

  if (!is.null(tab_intro)) {
    tab_contents <- append(tab_contents, list(tab_intro))
  }
  if (show_levels) {
    tab_contents <- append(tab_contents, list(level_legend()))
  }

  tab_contents <- append(
    tab_contents,
    list(
      shinycssloaders::withSpinner(
        DT::dataTableOutput("Act_participant")
      )
    )
  )

  coding_tab <- do.call(
    shiny::tabPanel,
    c(list(title = tab_title, id = "Week"), tab_contents)
  )

  shiny::navbarPage(
    "Qualitative Coding",
    theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
    coding_tab
  )
}


widget_output <- function(output_id, wrap_html) {
  output <- shiny::uiOutput(output_id)

  if (wrap_html) {
    return(as.character(htmltools::HTML(as.character(output))))
  }

  as.character(output)
}


coding_datatable <- function(
  participant_data,
  fields,
  column_order,
  wrap_html
) {
  table_data <- participant_data

  for (field in fields) {
    output_ids <- paste0(
      field$output_prefix,
      seq_len(nrow(participant_data))
    )
    table_data[[field$column]] <- vapply(
      output_ids,
      widget_output,
      character(1),
      wrap_html = wrap_html
    )
  }

  table_data <- dplyr::select(table_data, dplyr::all_of(column_order))

  DT::datatable(
    table_data,
    escape = FALSE,
    selection = "single",
    options = list(
      paging = TRUE,
      ordering = FALSE,
      searching = FALSE,
      pageLength = 20,
      dom = "tp",
      preDrawCallback = DT::JS(
        "function() { Shiny.unbindAll(this.api().table().node());}"
      ),
      drawCallback = DT::JS(
        "function() { Shiny.bindAll(this.api().table().node()); } "
      )
    )
  )
}


saved_values <- function(value) {
  c(unlist(stringr::str_split(value, " ; ")))
}


selectize_render_options <- function() {
  list(
    render = I("
      {
        item: function(item, escape) {
          return '<div>' + item.label + '</div>';
        },
        option: function(item, escape) {
          return '<div>' + item.label + '</div>';
        }
      }")
  )
}


render_coding_widget <- function(output, row, saved_data, field) {
  input_id <- paste0(field$input_prefix, row)
  output_id <- paste0(field$output_prefix, row)
  selected <- saved_values(saved_data[row, field$storage_column])

  widget <- if (field$type == "selectize") {
    shiny::selectizeInput(
      input_id,
      NULL,
      choices = as.list(field$choices),
      selected = selected,
      multiple = TRUE,
      options = selectize_render_options()
    )
  } else {
    shiny::textInput(
      input_id,
      NULL,
      placeholder = field$placeholder,
      value = selected
    )
  }

  output[[output_id]] <- shiny::renderUI(widget)
}


render_coding_widgets <- function(output, participant_data, saved_data, fields) {
  for (field in fields) {
    for (row in seq_len(nrow(participant_data))) {
      render_coding_widget(output, row, saved_data, field)
    }
  }
}


register_save_observer <- function(input, row, coding_path, field) {
  input_id <- paste0(field$input_prefix, row)
  storage_column <- field$storage_column

  shiny::observeEvent(input[[input_id]], {
    saved_data <- utils::read.csv(coding_path)[, -1, drop = FALSE]
    saved_data[row, storage_column] <- paste(
      input[[input_id]],
      collapse = " ; "
    )
    utils::write.csv(saved_data, coding_path, row.names = TRUE)
  })
}


register_save_observers <- function(
  input,
  participant_data,
  coding_path,
  fields
) {
  for (field in fields) {
    for (row in seq_len(nrow(participant_data))) {
      register_save_observer(input, row, coding_path, field)
    }
  }
}


coding_server <- function(
  participant_data,
  coding_path,
  fields,
  column_order,
  wrap_html = FALSE
) {
  function(input, output, session) {
    output$Act_participant <- DT::renderDataTable({
      coding_datatable(
        participant_data,
        fields,
        column_order,
        wrap_html
      )
    })

    shiny::observeEvent(input$Act_participant_rows_current, {
      saved_data <- utils::read.csv(coding_path)[-1]
      print("Act dataframe reloaded")
      print(utils::head(saved_data))

      render_coding_widgets(
        output,
        participant_data,
        saved_data,
        fields
      )
    })

    register_save_observers(
      input,
      participant_data,
      coding_path,
      fields
    )
  }
}
