# Mode-specific setup shared by the consolidated launcher.

standard_deductive_variables <- function() {
  c(thought = "Thought", activity = "Activity", location = "Location",
    company = "Company")
}


validate_deductive_variables <- function(variables) {
  variables <- trimws(variables)
  if (length(variables) == 0 || anyNA(variables) || any(!nzchar(variables))) {
    stop("Select at least one coding variable and give each variable a name.")
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
  participant_id_column = "Participant_ID",
  coding_variables = standard_deductive_variables()
) {
  standard <- intersect(unname(coding_variables), standard_deductive_variables())
  source_columns <- unlist(lapply(standard, function(variable) {
    c(variable, paste0(variable, "_ENG"))
  }), use.names = FALSE)
  c(
    participant_id_column,
    "Day", "Obs", "Time1",
    source_columns
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


deductive_column_order <- function(display_columns, fields) {
  coding_groups <- list(
    Code_Thought = c("Thought", "Thought_ENG"),
    Code_Activity = c("Activity", "Activity_ENG"),
    Code_Location = c("Location", "Location_ENG"),
    Code_Company = c("Company", "Company_ENG")
  )
  order <- character()
  placed_fields <- character()

  for (column in display_columns) {
    order <- c(order, column)
    for (field in intersect(names(coding_groups), field_columns(fields))) {
      selected_group <- intersect(display_columns, coding_groups[[field]])
      if (length(selected_group) > 0 && column == tail(selected_group, 1)) {
        order <- c(order, field)
        placed_fields <- c(placed_fields, field)
      }
    }
  }

  c(order, setdiff(field_columns(fields), placed_fields))
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


create_deductive_mode <- function(
  user,
  participant_id,
  data_file,
  codebook_files,
  display_columns = deductive_data_columns(participant_id_column, coding_variables),
  participant_id_column = "Participant_ID",
  coding_variables = standard_deductive_variables()
) {
  coding_variables <- validate_deductive_variables(coding_variables)
  codebooks <- lapply(names(coding_variables), function(id) {
    label <- coding_variables[[id]]
    if (is.null(codebook_files[[id]])) {
      stop("Select a codebook for ", label, " before starting.")
    }
    load_codebook(
      codebook_files[[id]],
      paste("The", label, "codebook"),
      has_levels = id %in% c("thought", "activity")
    )
  })
  has_levels <- vapply(codebooks, function(book) "Level" %in% names(book), logical(1))

  data <- load_coding_data(
    data_file,
    unique(c(
      deductive_data_columns(participant_id_column, coding_variables),
      display_columns
    )),
    participant_id_column
  )

  fields <- lapply(seq_along(coding_variables), function(index) {
    coding_selectize_field(
      paste0("Code_", coding_variables[[index]]),
      paste0("code_variable_", index, "_"),
      index,
      prepare_codebook_choices(codebooks[[index]], has_levels[[index]])
    )
  })
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
    data_file = data_file,
    user = user,
    participant_id = participant_id,
    participant_data = participant_data,
    storage_columns = field_columns(fields)
  )

  column_order <- deductive_column_order(display_columns, fields)

  new_coding_mode(
    title = "Deductive coding",
    participant_data = participant_data,
    coding_path = coding_path,
    fields = fields,
    column_order = column_order,
    show_levels = any(has_levels),
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
  participant_id_column = "Participant_ID"
) {
  data <- load_coding_data(
    data_file,
    unique(c(
      inductive_data_columns(participant_id_column),
      display_columns
    )),
    participant_id_column
  )

  fields <- familiarization_fields(proposed_event_id = "event")
  participant_data <- select_participant_data(
    data,
    participant_id_column,
    participant_id
  )
  coding_path <- initialize_coding_storage(
    data_file = data_file,
    user = user,
    participant_id = participant_id,
    participant_data = participant_data,
    storage_columns = field_columns(fields)
  )

  new_coding_mode(
    title = "Inductive coding: Phase 1",
    participant_data = participant_data,
    coding_path = coding_path,
    fields = fields,
    column_order = c(display_columns, field_columns(fields)),
    intro = paste(
      "Familiarize yourself with the data and record notes or",
      "proposed event codes."
    )
  )
}


create_inductive_phase2_mode <- function(
  user,
  participant_id,
  data_file,
  codebook_files,
  display_columns = inductive_data_columns(participant_id_column),
  participant_id_column = "Participant_ID"
) {
  codebook <- load_codebook(
    codebook_files$event,
    "The event codebook"
  )
  choices <- prepare_codebook_choices(codebook, FALSE)

  data <- load_coding_data(
    data_file,
    unique(c(
      inductive_data_columns(participant_id_column),
      display_columns
    )),
    participant_id_column
  )

  fields <- familiarization_fields(
    proposed_event_id = "event_new",
    existing_code_choices = choices
  )
  participant_data <- select_participant_data(
    data,
    participant_id_column,
    participant_id
  )
  coding_path <- initialize_coding_storage(
    data_file = data_file,
    user = user,
    participant_id = participant_id,
    participant_data = participant_data,
    storage_columns = field_columns(fields)
  )

  new_coding_mode(
    title = "Inductive coding: Phase 2",
    participant_data = participant_data,
    coding_path = coding_path,
    fields = fields,
    column_order = c(display_columns, field_columns(fields)),
    intro = paste(
      "Code the data while refining and applying the developing",
      "event codebook."
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
  coding_variables = standard_deductive_variables()
) {
  mode_factory <- switch(
    mode,
    deductive = create_deductive_mode,
    inductive_phase1 = create_inductive_phase1_mode,
    inductive_phase2 = create_inductive_phase2_mode,
    stop("Unknown coding activity: ", mode)
  )

  arguments <- list(
    user = user,
    participant_id = participant_id,
    data_file = data_file,
    codebook_files = codebook_files,
    display_columns = display_columns,
    participant_id_column = participant_id_column
  )
  if (mode == "deductive") {
    arguments$coding_variables <- coding_variables
    if (missing(display_columns)) {
      arguments$display_columns <- deductive_data_columns(
        participant_id_column, coding_variables
      )
    }
  }
  do.call(mode_factory, arguments)
}
