#############################################################
########## Shiny App for Eeske dataset Coding ############
#############################################################

source("R/coding_app.R")

# This shiny app can be used for coding activities

# 1. Select the folder in which the codebook is stored and the results will be saved
Projectwd <- choose_project_directory()

# 2. Select the coding schemes
Codebook_Thought <- "Codebooks/Thoughts_Delespaul1995.csv" # DON'T CHANGE
Codebook_Act_Thought <- read.csv(file.path(Projectwd, Codebook_Thought)) # DON'T CHANGE

Codebook_Activity <- "Codebooks/Activities_ATUS2024.csv" # DON'T CHANGE
Codebook_Act_Activity <- read.csv(file.path(Projectwd, Codebook_Activity)) # DON'T CHANGE

Codebook_Location <- "Codebooks/Locations_Stadel2024.csv" # DON'T CHANGE
Codebook_Act_Location <- read.csv(file.path(Projectwd, Codebook_Location)) # DON'T CHANGE

Codebook_Company <- "Codebooks/SocialContext_Delespaul1995.csv" # DON'T CHANGE
Codebook_Act_Company <- read.csv(file.path(Projectwd, Codebook_Company)) # DON'T CHANGE

# 3. Read in data
data_path <- "20250819_Swinging_Moods_ESM_data_anonymized.xlsx"
Data <- load_coding_data(
  Projectwd,
  data_path,
  c(
    "Participant_ID",
    # "Beep_ID",
    "Day", "Obs", "Time1",
    "Thought", "Thought_ENG",
    "Activity", "Activity_ENG",
    "Location", "Location_ENG",
    "Company", "Company_ENG"
  )
) # DON'T CHANGE

# 4. Select who is coding (a folder will be created if this is a new person)
User <- "MS" # ADD YOUR NAME HERE

# 5. Indicate how your column is named that includes the participant IDs and select the participant of interest
id_column <- "Participant_ID" # Change the name of the column here # DON'T CHANGE
ppID <- 11347

# 6. Indicate whether your codebooks contain different levels
Levels_Thought <- TRUE # DON'T CHANGE
Levels_Activity <- TRUE # DON'T CHANGE
Levels_Location <- FALSE # DON'T CHANGE
Levels_Company <- FALSE # DON'T CHANGE

# 7. Click "Run App"


## THE REST OF THE CODE DOES NOT NEED TO BE CHANGED ##

Codebook_Act_Thought <- prepare_codebook_choices(
  Codebook_Act_Thought,
  Levels_Thought
)
Codebook_Act_Activity <- prepare_codebook_choices(
  Codebook_Act_Activity,
  Levels_Activity
)
Codebook_Act_Location <- prepare_codebook_choices(
  Codebook_Act_Location,
  Levels_Location
)
Codebook_Act_Company <- prepare_codebook_choices(
  Codebook_Act_Company,
  Levels_Company
)

Act_participant <- select_participant_data(Data, id_column, ppID)

fields <- list(
  coding_selectize_field(
    "Code_Thought",
    "code_Thought",
    1,
    Codebook_Act_Thought
  ),
  coding_selectize_field(
    "Code_Activity",
    "code_Activity",
    2,
    Codebook_Act_Activity
  ),
  coding_selectize_field(
    "Code_Location",
    "code_Location",
    3,
    Codebook_Act_Location
  ),
  coding_selectize_field(
    "Code_Company",
    "code_Company",
    4,
    Codebook_Act_Company
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

coding_path <- initialize_coding_storage(
  project_directory = Projectwd,
  storage_directory = NULL,
  user = User,
  participant_id = ppID,
  participant_data = Act_participant,
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

ui <- coding_ui(
  tab_title = "Thought, activity, location, and company",
  show_levels = Levels_Thought
)

server <- coding_server(
  participant_data = Act_participant,
  coding_path = coding_path,
  fields = fields,
  column_order = column_order
)

shinyApp(ui, server)
