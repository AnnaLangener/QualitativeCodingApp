# Mode-specific setup shared by the consolidated launcher.

validate_coding_variables <- function(variables) {
  variables <- trimws(variables)
  if (length(variables) == 0 || anyNA(variables) || any(!nzchar(variables))) {
    stop("Add at least one coding variable and give each variable a name.")
  }
  if (is.null(names(variables)) || anyDuplicated(names(variables)) ||
      any(!nzchar(names(variables)))) {
    stop("Coding variables must have unique identifiers.")
  }
  columns <- tolower(make.names(paste0("Code_", variables)))
  if (anyDuplicated(columns)) {
    stop("Coding variable names must be unique, including after spaces and punctuation are converted to dots.")
  }
  variables
}


deductive_data_columns <- function(
  participant_id_column = "Participant_ID"
) {
  c(
    participant_id_column,
    "Day", "Obs", "Time1"
  )
}


inductive_data_columns <- function(
  participant_id_column = "Participant_ID"
) {
  c(
    participant_id_column,
    "Day", "Obs", "Time1",
    "Thought", "Activity", "Location", "Company", "Event"
  )
}


default_display_columns <- function(
  mode,
  participant_id_column = "Participant_ID"
) {
  switch(
    mode,
    deductive = deductive_data_columns(participant_id_column),
    inductive_phase1 = inductive_data_columns(participant_id_column),
    inductive_phase2 = inductive_data_columns(participant_id_column),
    character()
  )
}


new_coding_mode <- function(
  title,
  participant_data,
  coding_path,
  fields,
  column_order,
  show_levels = FALSE,
  intro = NULL,
  wrap_html = FALSE
) {
  list(
    title = title,
    content = coding_content(
      tab_title = title,
      show_levels = show_levels,
      tab_intro = intro
    ),
    server = coding_server(
      participant_data = participant_data,
      coding_path = coding_path,
      fields = fields,
      column_order = column_order,
      wrap_html = wrap_html
    )
  )
}


coding_variable_fields <- function(
  coding_variables,
  codebook_files,
  storage_offset = 0L
) {
  coding_variables <- validate_coding_variables(coding_variables)
  codebooks <- lapply(names(coding_variables), function(id) {
    label <- coding_variables[[id]]
    if (is.null(codebook_files[[id]])) {
      stop("Select a codebook for ", label, " before starting.")
    }
    load_codebook(
      codebook_files[[id]],
      paste("The", label, "codebook")
    )
  })
  has_levels <- vapply(codebooks, function(book) "Level" %in% names(book), logical(1))

  fields <- lapply(seq_along(coding_variables), function(index) {
    coding_selectize_field(
      paste0("Code_", coding_variables[[index]]),
      paste0("code_variable_", index, "_"),
      storage_offset + index,
      prepare_codebook_choices(codebooks[[index]], has_levels[[index]])
    )
  })

  list(fields = fields, show_levels = any(has_levels))
}


create_deductive_mode <- function(
  user,
  participant_id,
  data_file,
  codebook_files,
  display_columns = deductive_data_columns(participant_id_column),
  participant_id_column = "Participant_ID",
  coding_variables = character(),
  storage = NULL
) {
  coding_variables <- validate_coding_variables(coding_variables)
  variable_fields <- coding_variable_fields(coding_variables, codebook_files)
  data <- load_coding_data(
    data_file,
    unique(c(deductive_data_columns(participant_id_column), display_columns)),
    participant_id_column
  )

  fields <- variable_fields$fields
  fields <- c(fields, list(
    coding_text_field(
      "Comments: general",
      "general",
      length(coding_variables) + 1L,
      "General comment"
    ),
    coding_text_field(
      "Comments: depth",
      "depth",
      length(coding_variables) + 2L,
      "Depth-related comment"
    )
  ))

  participant_data <- select_participant_data(
    data,
    participant_id_column,
    participant_id
  )
  coding_path <- initialize_coding_storage(
    participant_data = participant_data,
    storage_columns = field_columns(fields),
    storage = storage
  )

  column_order <- c(display_columns, field_columns(fields))

  new_coding_mode(
    title = "Deductive coding",
    participant_data = participant_data,
    coding_path = coding_path,
    fields = fields,
    column_order = column_order,
    show_levels = variable_fields$show_levels,
    intro = paste("Apply the selected codebooks to:",
                  paste(coding_variables, collapse = ", "))
  )
}


create_inductive_phase1_mode <- function(
  user,
  participant_id,
  data_file,
  codebook_files = NULL,
  display_columns = inductive_data_columns(participant_id_column),
  participant_id_column = "Participant_ID",
  coding_variables = character(),
  storage = NULL
) {
  coding_variables <- validate_coding_variables(coding_variables)
  data <- load_coding_data(
    data_file,
    unique(c(
      deductive_data_columns(participant_id_column),
      display_columns
    )),
    participant_id_column
  )

  fields <- familiarization_note_fields()
  fields <- c(fields, proposed_coding_variable_fields(
    coding_variables, storage_offset = length(fields)
  ))
  participant_data <- select_participant_data(
    data,
    participant_id_column,
    participant_id
  )
  coding_path <- initialize_coding_storage(
    participant_data = participant_data,
    storage_columns = field_columns(fields),
    storage = storage
  )

  new_coding_mode(
    title = "Inductive coding: Phase 1",
    participant_data = participant_data,
    coding_path = coding_path,
    fields = fields,
    column_order = c(display_columns, field_columns(fields)),
    intro = paste(
      "Familiarize yourself with the data and record notes or",
      "proposed codes for:", paste(coding_variables, collapse = ", ")
    )
  )
}


create_inductive_phase2_mode <- function(
  user,
  participant_id,
  data_file,
  codebook_files,
  display_columns = inductive_data_columns(participant_id_column),
  participant_id_column = "Participant_ID",
  coding_variables = character(),
  storage = NULL
) {
  coding_variables <- validate_coding_variables(coding_variables)
  fields <- familiarization_note_fields()
  fields <- c(fields, proposed_coding_variable_fields(
    coding_variables, storage_offset = length(fields)
  ))
  variable_fields <- coding_variable_fields(
    coding_variables, codebook_files, storage_offset = length(fields)
  )
  fields <- c(fields, variable_fields$fields)

  data <- load_coding_data(
    data_file,
    unique(c(
      deductive_data_columns(participant_id_column),
      display_columns
    )),
    participant_id_column
  )

  participant_data <- select_participant_data(
    data,
    participant_id_column,
    participant_id
  )
  coding_path <- initialize_coding_storage(
    participant_data = participant_data,
    storage_columns = field_columns(fields),
    storage = storage
  )

  new_coding_mode(
    title = "Inductive coding: Phase 2",
    participant_data = participant_data,
    coding_path = coding_path,
    fields = fields,
    column_order = c(display_columns, field_columns(fields)),
    show_levels = variable_fields$show_levels,
    intro = paste(
      "Code the data while refining and applying the developing codebooks for:",
      paste(coding_variables, collapse = ", ")
    ),
    wrap_html = TRUE
  )
}


create_coding_mode <- function(
  mode,
  user,
  participant_id,
  data_file,
  codebook_files = NULL,
  display_columns = default_display_columns(mode, participant_id_column),
  participant_id_column = "Participant_ID",
  coding_variables = character(),
  coding_path = coding_output_path(data_file, user, participant_id),
  resume_settings = NULL
) {
  mode_factory <- switch(
    mode,
    deductive = create_deductive_mode,
    inductive_phase1 = create_inductive_phase1_mode,
    inductive_phase2 = create_inductive_phase2_mode,
    stop("Unknown coding activity: ", mode)
  )

  coding_variables <- validate_coding_variables(coding_variables)
  codebooks <- lapply(names(coding_variables), function(id) {
    if (mode == "inductive_phase1") return(NULL)
    if (is.null(codebook_files[[id]])) stop("Select a codebook for ", coding_variables[[id]], ".")
    codebook_settings(codebook_files[[id]], paste("The", coding_variables[[id]], "codebook"))
  })
  names(codebooks) <- names(coding_variables)
  settings <- new_session_settings(mode, user, data_file, coding_path,
    participant_id_column, participant_id, display_columns, coding_variables, codebooks)
  if (!is.null(resume_settings)) {
    verify_session_files(resume_settings, data_file, coding_path)
    # Existing variables and their codebooks remain intact; new variables may be added.
    expected <- resume_settings
    expected$source <- settings$source
    expected$output <- settings$output
    expected$started_at <- settings$started_at
    previous_count <- length(expected$coding_variables)
    current <- settings
    current$coding_variables <- head(current$coding_variables, previous_count)
    if (length(settings$coding_variables) < previous_count || !identical(expected, current)) {
      stop("Continuation keeps the original settings and coding variables. You may add new variables; use the JSON as a template to remove variables or change other settings.")
    }
    settings$started_at <- resume_settings$started_at
    settings$source <- resume_settings$source
  }
  previous_columns <- if (is.null(resume_settings)) NULL else
    session_storage_columns(resume_settings)
  arguments <- list(
    user = user,
    participant_id = participant_id,
    data_file = data_file,
    codebook_files = codebooks,
    display_columns = display_columns,
    participant_id_column = participant_id_column,
    coding_variables = coding_variables,
    storage = list(path = coding_path, settings = settings, resume = !is.null(resume_settings),
                   previous_columns = previous_columns)
  )
  do.call(mode_factory, arguments)
}
