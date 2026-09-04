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


choose_tabular_file <- function(caption) {
  filters <- matrix(
    c(
      "CSV and Excel files", "*.csv;*.xls;*.xlsx",
      "All files", "*.*"
    ),
    ncol = 2,
    byrow = TRUE
  )

  data_path <- if (.Platform$OS.type == "windows") {
    utils::choose.files(
      caption = caption,
      multi = FALSE,
      filters = filters,
      index = 1
    )
  } else {
    tcltk::tk_choose.files(
      caption = caption,
      multi = FALSE,
      filters = filters,
      index = 1
    )
  }
  if (
    length(data_path) != 1 ||
      is.na(data_path) ||
      !nzchar(data_path)
  ) {
    return(NULL)
  }

  normalizePath(data_path, winslash = "/", mustWork = TRUE)
}


read_tabular_file <- function(upload, label) {
  if (is.character(upload)) {
    name <- basename(upload)
    path <- upload
  } else {
    name <- upload$name[[1]]
    path <- upload$datapath[[1]]
  }
  extension <- tolower(tools::file_ext(name))

  switch(
    extension,
    csv = utils::read.csv(path, check.names = FALSE),
    xls = readxl::read_excel(path),
    xlsx = readxl::read_excel(path),
    stop(
      label,
      " must be a CSV or Excel file (.csv, .xls, or .xlsx)."
    )
  )
}


read_tabular_column_names <- function(upload, label) {
  if (is.character(upload)) {
    name <- basename(upload)
    path <- upload
  } else {
    name <- upload$name[[1]]
    path <- upload$datapath[[1]]
  }
  extension <- tolower(tools::file_ext(name))

  columns <- switch(
    extension,
    csv = colnames(utils::read.csv(
      path,
      nrows = 0,
      check.names = FALSE
    )),
    xls = colnames(readxl::read_excel(path, n_max = 0)),
    xlsx = colnames(readxl::read_excel(path, n_max = 0)),
    stop(
      label,
      " must be a CSV or Excel file (.csv, .xls, or .xlsx)."
    )
  )

  if (length(columns) == 0) {
    stop(label, " does not contain any columns.")
  }
  if (anyDuplicated(columns)) {
    stop(label, " must have unique column names.")
  }

  columns
}


require_columns <- function(data, columns, label) {
  missing_columns <- setdiff(columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }

  data
}


load_coding_data <- function(
  data_file,
  columns,
  participant_id_column = "Participant_ID"
) {
  read_tabular_file(data_file, "The data file") |>
    require_columns(columns, "The data file") |>
    dplyr::arrange(
      .data[[participant_id_column]],
      .data[["Obs"]]
    )
}


load_codebook <- function(codebook_file, label, has_levels = FALSE) {
  required_columns <- if (has_levels) c("Code", "Level") else "Code"

  read_tabular_file(codebook_file, label) |>
    require_columns(required_columns, label)
}


select_participant_data <- function(data, id_column, participant_id) {
  participant_id <- trimws(as.character(participant_id))
  participant_data <- dplyr::filter(
    data,
    !is.na(.data[[id_column]]) &
      trimws(as.character(.data[[id_column]])) == participant_id
  )

  if (nrow(participant_data) == 0) {
    stop(
      "Participant ID '",
      participant_id,
      "' was not found in the selected data file."
    )
  }

  participant_data
}


participant_id_choices <- function(data, id_column) {
  if (is.null(id_column) || !id_column %in% colnames(data)) {
    return(character())
  }

  values <- trimws(as.character(data[[id_column]]))
  unique(values[!is.na(values) & nzchar(values)])
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


familiarization_column_order <- function(
  fields,
  participant_id_column = "Participant_ID"
) {
  c(
    participant_id_column, "Day", "Obs", "Time1",
    "Thought", "Activity", "Location", "Company", "Event",
    field_columns(fields)
  )
}


initialize_coding_storage <- function(
  data_file,
  user,
  participant_id,
  participant_data,
  storage_columns
) {
  user <- validate_storage_identifier(user, "Coder")
  participant_id <- validate_storage_identifier(
    participant_id,
    "Participant ID"
  )

  data_path <- normalizePath(data_file, winslash = "/", mustWork = TRUE)
  original_name <- tools::file_path_sans_ext(basename(data_path))
  coding_filename <- paste(
    original_name,
    participant_id,
    user,
    sep = "_"
  )
  coding_path <- file.path(
    dirname(data_path),
    paste0(coding_filename, ".csv")
  )
  if (!file.exists(coding_path)) {
    coding_data <- as.data.frame(
      matrix(
        NA,
        nrow = nrow(participant_data),
        ncol = length(storage_columns)
      )
    )
    names(coding_data) <- make.names(storage_columns, unique = TRUE)

    merged_data <- cbind(participant_data, coding_data)
    utils::write.csv(merged_data, coding_path, row.names = FALSE)
  }

  coding_path
}


validate_storage_identifier <- function(value, label) {
  value <- trimws(as.character(value))

  if (
    length(value) != 1 ||
      is.na(value) ||
      !nzchar(value) ||
      value %in% c(".", "..") ||
      grepl("[<>:\"/\\\\|?*]", value)
  ) {
    stop(
      label,
      " must be a non-empty name without these characters: ",
      "< > : \" / \\ | ? *"
    )
  }

  value
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


coding_content <- function(tab_title, show_levels = FALSE, tab_intro = NULL) {
  tab_contents <- list()

  if (!is.null(tab_intro)) {
    tab_contents <- append(
      tab_contents,
      list(shiny::tags$p(tab_intro))
    )
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

  do.call(
    shiny::tags$div,
    c(
      list(
        class = "coding-app-content",
        shiny::tags$h2(tab_title)
      ),
      tab_contents
    )
  )
}

coding_ui <- function(tab_title, show_levels = FALSE, tab_intro = NULL) {
  shiny::navbarPage(
    "Qualitative Coding",
    theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
    shiny::tabPanel(
      tab_title,
      coding_content(tab_title, show_levels, tab_intro),
      value = "coding"
    ),
    id = "Week"
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


read_saved_coding_data <- function(
  coding_path,
  data_column_count,
  storage_column_count
) {
  saved_data <- utils::read.csv(coding_path, check.names = FALSE)
  coding_indices <- data_column_count + seq_len(storage_column_count)
  saved_data[, coding_indices, drop = FALSE]
}


register_save_observer <- function(
  input,
  row,
  coding_path,
  field,
  data_column_count
) {
  input_id <- paste0(field$input_prefix, row)
  storage_column <- field$storage_column

  shiny::observeEvent(input[[input_id]], {
    saved_data <- utils::read.csv(coding_path, check.names = FALSE)
    coding_column <- data_column_count + storage_column
    saved_data[row, coding_column] <- paste(
      input[[input_id]],
      collapse = " ; "
    )
    utils::write.csv(saved_data, coding_path, row.names = FALSE)
  })
}


register_save_observers <- function(
  input,
  participant_data,
  coding_path,
  fields
) {
  data_column_count <- ncol(participant_data)
  for (field in fields) {
    for (row in seq_len(nrow(participant_data))) {
      register_save_observer(
        input,
        row,
        coding_path,
        field,
        data_column_count
      )
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
      saved_data <- read_saved_coding_data(
        coding_path,
        ncol(participant_data),
        length(fields)
      )
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
