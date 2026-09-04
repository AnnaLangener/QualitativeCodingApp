# Mode-specific setup shared by the consolidated launcher.

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
  codebook_files
) {
  thought_codebook <- load_codebook(
    codebook_files$thought,
    "The thought codebook",
    has_levels = TRUE
  )
  activity_codebook <- load_codebook(
    codebook_files$activity,
    "The activity codebook",
    has_levels = TRUE
  )
  location_codebook <- load_codebook(
    codebook_files$location,
    "The location codebook"
  )
  company_codebook <- load_codebook(
    codebook_files$company,
    "The company codebook"
  )

  data <- load_coding_data(
    data_file,
    c(
      "Participant_ID",
      "Day", "Obs", "Time1",
      "Thought", "Thought_ENG",
      "Activity", "Activity_ENG",
      "Location", "Location_ENG",
      "Company", "Company_ENG"
    )
  )

  fields <- list(
    coding_selectize_field(
      "Code_Thought",
      "code_Thought",
      1,
      prepare_codebook_choices(thought_codebook, TRUE)
    ),
    coding_selectize_field(
      "Code_Activity",
      "code_Activity",
      2,
      prepare_codebook_choices(activity_codebook, TRUE)
    ),
    coding_selectize_field(
      "Code_Location",
      "code_Location",
      3,
      prepare_codebook_choices(location_codebook, FALSE)
    ),
    coding_selectize_field(
      "Code_Company",
      "code_Company",
      4,
      prepare_codebook_choices(company_codebook, FALSE)
    ),
    coding_text_field(
      "Comments: general",
      "general",
      5,
      "General comment"
    ),
    coding_text_field(
      "Comments: depth",
      "depth",
      6,
      "Depth-related comment"
    )
  )

  participant_data <- select_participant_data(
    data,
    "Participant_ID",
    participant_id
  )
  coding_path <- initialize_coding_storage(
    data_file = data_file,
    user = user,
    participant_id = participant_id,
    participant_data = participant_data,
    storage_columns = field_columns(fields)
  )

  column_order <- c(
    "Participant_ID", "Day", "Obs", "Time1",
    "Thought", "Thought_ENG", "Code_Thought",
    "Activity", "Activity_ENG", "Code_Activity",
    "Location", "Location_ENG", "Code_Location",
    "Company", "Company_ENG", "Code_Company",
    "Comments: general", "Comments: depth"
  )

  new_coding_mode(
    title = "Deductive coding",
    participant_data = participant_data,
    coding_path = coding_path,
    fields = fields,
    column_order = column_order,
    show_levels = TRUE,
    intro = paste(
      "Apply the existing codebooks to thoughts, activities,",
      "locations, and company."
    )
  )
}


create_inductive_phase1_mode <- function(
  user,
  participant_id,
  data_file,
  codebook_files = NULL
) {
  data <- load_coding_data(
    data_file,
    c(
      "Participant_ID",
      "Day", "Obs", "Time1",
      "Thought", "Activity", "Location", "Company", "Event"
    )
  )

  fields <- familiarization_fields(proposed_event_id = "event")
  participant_data <- select_participant_data(
    data,
    "Participant_ID",
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
    column_order = familiarization_column_order(fields),
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
  codebook_files
) {
  codebook <- load_codebook(
    codebook_files$event,
    "The event codebook"
  )
  choices <- prepare_codebook_choices(codebook, FALSE)

  data <- load_coding_data(
    data_file,
    c(
      "Participant_ID",
      "Day", "Obs", "Time1",
      "Thought", "Activity", "Location", "Company", "Event"
    )
  )

  fields <- familiarization_fields(
    proposed_event_id = "event_new",
    existing_code_choices = choices
  )
  participant_data <- select_participant_data(
    data,
    "Participant_ID",
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
    column_order = familiarization_column_order(fields),
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
  codebook_files = NULL
) {
  mode_factory <- switch(
    mode,
    deductive = create_deductive_mode,
    inductive_phase1 = create_inductive_phase1_mode,
    inductive_phase2 = create_inductive_phase2_mode,
    stop("Unknown coding activity: ", mode)
  )

  mode_factory(
    user = user,
    participant_id = participant_id,
    data_file = data_file,
    codebook_files = codebook_files
  )
}
