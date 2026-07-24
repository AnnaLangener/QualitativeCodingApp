#############################################################
########## Shiny App for Eeske dataset Coding ############
########## Inductive Coding: Data Familiarization ##########
########## Phase 2: coding while building a codebook ##########
#############################################################

source("R/coding_app.R")

# This shiny app can be used for coding activities

# 1. Select the folder in which the codebook is stored and the results will be saved
Projectwd <- choose_project_directory()

# 2. Select the coding scheme
Codebook <- "Inductive coding/Codebook_ShinyApp_02042026.xlsx" # DON'T CHANGE
Codebook_Act <- read_excel(file.path(Projectwd, Codebook)) # DON'T CHANGE

# 3. Read in data
data_path <- "20250819_Swinging_Moods_ESM_data_anonymized.xlsx"
Data <- load_coding_data(
  Projectwd,
  data_path,
  c(
    "Participant_ID",
    # "Beep_ID",
    "Day", "Obs", "Time1",
    "Thought", "Activity", "Location", "Company", "Event"
  )
) # DON'T CHANGE

# 4. Select who is coding (a folder will be created if this is a new person)
User <- "JC" # ADD YOUR NAME HERE

# 5. Indicate how your column is named that includes the participant IDs and select the participant of interest
id_column <- "Participant_ID" # Change the name of the column here # DON'T CHANGE
ppID <- 27311

# 6. Indicate whether your codebook contains different levels
Levels <- FALSE # DON'T CHANGE

# 7. Click "Run App"


## THE REST OF THE CODE DOES NOT NEED TO BE CHANGED ##

Codebook_Act <- prepare_codebook_choices(Codebook_Act, Levels)

Act_participant <- select_participant_data(Data, id_column, ppID)
fields <- familiarization_fields(
  proposed_event_id = "event_new",
  existing_code_choices = Codebook_Act
)

coding_path <- initialize_coding_storage(
  project_directory = Projectwd,
  storage_directory = "Inductive coding",
  user = User,
  participant_id = ppID,
  participant_data = Act_participant,
  storage_columns = field_columns(fields)
)

column_order <- familiarization_column_order(fields)

ui <- coding_ui(
  tab_title = Codebook,
  show_levels = Levels,
  tab_intro = if (Levels) "Data familiarization: event"
)

server <- coding_server(
  participant_data = Act_participant,
  coding_path = coding_path,
  fields = fields,
  column_order = column_order,
  wrap_html = TRUE
)

shinyApp(ui, server)
