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
  project_directory,
  user,
  participant_id
) {
  thought_codebook <- utils::read.csv(file.path(
    project_directory,
    "Codebooks/Thoughts_Delespaul1995.csv"
  ))
  activity_codebook <- utils::read.csv(file.path(
    project_directory,
    "Codebooks/Activities_ATUS2024.csv"
  ))
  location_codebook <- utils::read.csv(file.path(
    project_directory,
    "Codebooks/Locations_Stadel2024.csv"
  ))
  company_codebook <- utils::read.csv(file.path(
    project_directory,
    "Codebooks/SocialContext_Delespaul1995.csv"
  ))

  data <- load_coding_data(
    project_directory,
    "20250819_Swinging_Moods_ESM_data_anonymized.xlsx",
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
    project_directory = project_directory,
    storage_directory = NULL,
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
  project_directory,
  user,
  participant_id
) {
  data <- load_coding_data(
    project_directory,
    "20250819_Swinging_Moods_ESM_data_anonymized.xlsx",
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
    project_directory = project_directory,
    storage_directory = file.path("Inductive coding", "Phase 1"),
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
  project_directory,
  user,
  participant_id
) {
  codebook <- readxl::read_excel(file.path(
    project_directory,
    "Inductive coding/Codebook_ShinyApp_02042026.xlsx"
  ))
  choices <- prepare_codebook_choices(codebook, FALSE)

  data <- load_coding_data(
    project_directory,
    "20250819_Swinging_Moods_ESM_data_anonymized.xlsx",
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
    project_directory = project_directory,
    storage_directory = file.path("Inductive coding", "Phase 2"),
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
  project_directory,
  user,
  participant_id
) {
  mode_factory <- switch(
    mode,
    deductive = create_deductive_mode,
    inductive_phase1 = create_inductive_phase1_mode,
    inductive_phase2 = create_inductive_phase2_mode,
    stop("Unknown coding activity: ", mode)
  )

  mode_factory(
    project_directory = project_directory,
    user = user,
    participant_id = participant_id
  )
}
