#############################################################
########## Shiny App for Eeske dataset Coding ############
#############################################################

library(dplyr)
library(readxl)
library(tcltk)
library(shiny)
library(DT)
library(stringr)
library(bslib)
library(shinycssloaders)

# This shiny app can be used for coding activities

# 1. Select the folder in which the codebook is stored and the results will be saved
Projectwd <- tk_choose.dir(default = "", caption = "Select directory")
Projectwd <- paste0(Projectwd, "/")

# 2. Select the coding scheme
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
data <- read_excel(file.path(Projectwd, data_path)) # DON'T CHANGE

# The dataframe should be sorted by Date, to allow for context coding
# DON'T CHANGE
Data <- data |>
  dplyr::select(
    Participant_ID,
    # Beep_ID,
    Day, Obs, Time1,
    Thought, Thought_ENG,
    Activity, Activity_ENG,
    Location, Location_ENG,
    Company, Company_ENG
  ) |>
  # filter(if_any(ends_with("_RP"), ~ !is.na(.))) %>%
  arrange(Participant_ID, Obs)

# 4. Select who is coding (a folder will be created if this is a new person)
User <- "MS" # ADD YOUR NAME HERE

# 5. Indicate how your column is named that includes the participant IDs and select the participant of interest
id_column <- "Participant_ID" # Change the name of the column here # DON'T CHANGE
ppID <- 11347

# 6. Indicate whether your codebook contains different levels?
Levels_Thought <- TRUE # DON'T CHANGE
Levels_Activity <- TRUE # DON'T CHANGE
Levels_Location <- FALSE # DON'T CHANGE
Levels_Company <- FALSE # DON'T CHANGE

# 7. Click "Run App"


## THE REST OF THE CODE DOES NOT NEED TO BE CHANGED ##

# A lot of the code that creates the table is copied from following github question
## https://github.com/rstudio/shiny/issues/1246

######### Create different colors for levels #########
# Here we create a dataframe that colors the different levels in the dropdown menu (if levels are included)
if (Levels_Thought == TRUE) {
  Codebook_Act_Thought <- Codebook_Act_Thought[, colnames(Codebook_Act_Thought) %in% c("Level", "Code")]
  t1_Thought <- Codebook_Act_Thought %>%
    mutate(html = ifelse(Level == "1",
      paste0("<span style='color:#9F73AB';>", Code, "</span>"),
      ifelse(Level == "2",
        paste0("<span style='color:#19376D';>", Code, "</span>"),
        paste0("<span style='color:#0C7B93';>", Code, "</span>")
      )
    ))
  Codebook_Act_Thought <- setNames(t1_Thought$Code, t1_Thought$html)
} else {
  Codebook_Act_Thought <- Codebook_Act_Thought$Code
}

if (Levels_Activity == TRUE) {
  Codebook_Act_Activity <- Codebook_Act_Activity[, colnames(Codebook_Act_Activity) %in% c("Level", "Code")]
  t1_Activity <- Codebook_Act_Activity %>%
    mutate(html = ifelse(Level == "1",
      paste0("<span style='color:#9F73AB';>", Code, "</span>"),
      ifelse(Level == "2",
        paste0("<span style='color:#19376D';>", Code, "</span>"),
        paste0("<span style='color:#0C7B93';>", Code, "</span>")
      )
    ))
  Codebook_Act_Activity <- setNames(t1_Activity$Code, t1_Activity$html)
} else {
  Codebook_Act_Activity <- Codebook_Act_Activity$Code
}

if (Levels_Location == TRUE) {
  Codebook_Act_Location <- Codebook_Act_Location[, colnames(Codebook_Act_Location) %in% c("Level", "Code")]
  t1_Location <- Codebook_Act_Location %>%
    mutate(html = ifelse(Level == "1",
      paste0("<span style='color:#9F73AB';>", Code, "</span>"),
      ifelse(Level == "2",
        paste0("<span style='color:#19376D';>", Code, "</span>"),
        paste0("<span style='color:#0C7B93';>", Code, "</span>")
      )
    ))
  Codebook_Act_Location <- setNames(t1_Location$Code, t1_Location$html)
} else {
  Codebook_Act_Location <- Codebook_Act_Location$Code
}

if (Levels_Company == TRUE) {
  Codebook_Act_Company <- Codebook_Act_Company[, colnames(Codebook_Act_Company) %in% c("Level", "Code")]
  t1_Company <- Codebook_Act_Company %>%
    mutate(html = ifelse(Level == "1",
      paste0("<span style='color:#9F73AB';>", Code, "</span>"),
      ifelse(Level == "2",
        paste0("<span style='color:#19376D';>", Code, "</span>"),
        paste0("<span style='color:#0C7B93';>", Code, "</span>")
      )
    ))
  Codebook_Act_Company <- setNames(t1_Company$Code, t1_Company$html)
} else {
  Codebook_Act_Company <- Codebook_Act_Company$Code
}
######## Data Storage ##########

# Here we check if the specified user already has a subfolder
if (!file.exists(file.path(Projectwd, User))) {
  dir.create(file.path(Projectwd, User))
}

# Next we prepare the dataframe for the selected participant
Act_participant <- Data |>
  filter(.data[[id_column]] == ppID) # 326, 317, 318, 316, 309

# If the participant is selected for the first time we create an empty dataframe
if (!file.exists(paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""))) {
  Empty <- data.frame(
    Code_Thought = rep(NA, nrow(Act_participant)),
    Code_Activity = rep(NA, nrow(Act_participant)),
    Code_Location = rep(NA, nrow(Act_participant)),
    Code_Company = rep(NA, nrow(Act_participant)),
    `Comments: general` = rep(NA, nrow(Act_participant)),
    `Comments: depth` = rep(NA, nrow(Act_participant))
  )
  write.csv(Empty, paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""))
}


########## Shiny App ###########

# if(Levels_Thought | Levels_Activity | Levels_Location | Levels_Event){
if (Levels_Thought == TRUE) {
  ui <- navbarPage("Qualitative Coding",
    theme = bs_theme(version = 5, bootswatch = "minty"),
    tabPanel("Thought, activity, location, and company",
      id = "Week",
      tags$div(
        style = "border: 1px solid #0C7B93; padding: 5px; margin: 5px; display: inline-block;",
        tags$span("Level 3", style = "color: #0C7B93;")
      ),
      tags$div(
        style = "border: 1px solid #19376D; padding: 5px; margin: 5px; display: inline-block;",
        tags$span("Level 2", style = "color: #19376D;")
      ),
      tags$div(
        style = "border: 1px solid #9F73AB; padding: 5px; margin: 5px; display: inline-block;",
        tags$span("Level 1", style = "color: #9F73AB;")
      ),
      withSpinner(DT::dataTableOutput("Act_participant"))
    ),
  )
} else {
  ui <- navbarPage("Qualitative Coding",
    theme = bs_theme(version = 5, bootswatch = "minty"),
    tabPanel("Thought, activity, location, and company",
      id = "Week",
      withSpinner(DT::dataTableOutput("Act_participant"))
    ),
  )
}

server <- function(input, output, session) {
  ###################### Create Datatable #####################
  #############################################################
  output$Act_participant <- DT::renderDataTable({
    a <- Act_participant # the static dataframe
    a$Code_Thought <- sapply(paste0("selectize_wrap_code_Thought", 1:nrow(Act_participant)), function(x) as.character(uiOutput(x)))
    a$Code_Activity <- sapply(paste0("selectize_wrap_code_Activity", 1:nrow(Act_participant)), function(x) as.character(uiOutput(x)))
    a$Code_Location <- sapply(paste0("selectize_wrap_code_Location", 1:nrow(Act_participant)), function(x) as.character(uiOutput(x)))
    a$Code_Company <- sapply(paste0("selectize_wrap_code_Company", 1:nrow(Act_participant)), function(x) as.character(uiOutput(x)))
    a$"Comments: general" <- sapply(paste0("selectize_wrap_general", 1:nrow(Act_participant)), function(x) as.character(uiOutput(x)))
    a$"Comments: depth" <- sapply(paste0("selectize_wrap_depth", 1:nrow(Act_participant)), function(x) as.character(uiOutput(x)))

    # Reorder the columns for easier coding
    a <- a %>%
      dplyr::select(
        Participant_ID, Day, Obs, Time1,
        Thought, Thought_ENG, Code_Thought,
        Activity, Activity_ENG, Code_Activity,
        Location, Location_ENG, Code_Location,
        Company, Company_ENG, Code_Company,
        `Comments: general`, `Comments: depth`
      )


    a <- datatable(a,
      escape = F, selection = "single",
      options = list(
        paging = TRUE, ordering = FALSE, searching = FALSE, pageLength = 20, dom = "tp",
        preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node());}"),
        drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } ")
      )
    )
    return(a)
  })

  ################ rendering fancy selectize widgets ###############
  ##################################################################

  # proxy <- DT::dataTableProxy('Act_participant')

  # Read existing Code/ Comments
  Act <- read.csv(paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""))[-1]

  observeEvent(input$Act_participant_rows_current, {
    Act <- read.csv(paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""))[-1]
    print("Act dataframe reloaded")
    print(head(Act))

    # This has to be in there otherwise its saved but not loaded
    for (i in 1:nrow(Act_participant)) {
      subs_widget_Thought <- substitute(
        {
          selectizeInput(paste0("selectize_code_Thought", i), NULL,
            choices = as.list(Codebook_Act_Thought), selected = c(unlist(str_split(Act[i, 1], " ; "))), multiple = T,
            options = list(render = I("
                                                      {
                                                        item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                                                        option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                                                      }"))
          )
        },
        list(i = i)
      )
      output[[paste0("selectize_wrap_code_Thought", i)]] <- renderUI(subs_widget_Thought, quoted = T)
    }

    for (i in 1:nrow(Act_participant)) {
      subs_widget_Activity <- substitute(
        {
          selectizeInput(paste0("selectize_code_Activity", i), NULL,
            choices = as.list(Codebook_Act_Activity), selected = c(unlist(str_split(Act[i, 2], " ; "))), multiple = T,
            options = list(render = I("
                                                      {
                                                        item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                                                        option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                                                      }"))
          )
        },
        list(i = i)
      )
      output[[paste0("selectize_wrap_code_Activity", i)]] <- renderUI(subs_widget_Activity, quoted = T)
    }

    for (i in 1:nrow(Act_participant)) {
      subs_widget_Location <- substitute(
        {
          selectizeInput(paste0("selectize_code_Location", i), NULL,
            choices = as.list(Codebook_Act_Location), selected = c(unlist(str_split(Act[i, 3], " ; "))), multiple = T,
            options = list(render = I("
                                                      {
                                                        item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                                                        option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                                                      }"))
          )
        },
        list(i = i)
      )
      output[[paste0("selectize_wrap_code_Location", i)]] <- renderUI(subs_widget_Location, quoted = T)
    }

    for (i in 1:nrow(Act_participant)) {
      subs_widget_Company <- substitute(
        {
          selectizeInput(paste0("selectize_code_Company", i), NULL,
            choices = as.list(Codebook_Act_Company), selected = c(unlist(str_split(Act[i, 4], " ; "))), multiple = T,
            options = list(render = I("
                                                      {
                                                        item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                                                        option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                                                      }"))
          )
        },
        list(i = i)
      )
      output[[paste0("selectize_wrap_code_Company", i)]] <- renderUI(subs_widget_Company, quoted = T)
    }

    for (i in 1:nrow(Act_participant)) {
      subs_widget_general <- substitute(
        {
          textInput(paste0("selectize_general", i), NULL,
            placeholder = "General comment",
            value = c(unlist(str_split(Act[i, 5], " ; ")))
          )
        },
        list(i = i)
      )
      output[[paste0("selectize_wrap_general", i)]] <- renderUI(subs_widget_general, quoted = T)
    }


    for (i in 1:nrow(Act_participant)) {
      subs_widget_depth <- substitute(
        {
          textInput(paste0("selectize_depth", i), NULL, placeholder = "Depth-related comment", value = c(unlist(str_split(Act[i, 6], " ; "))))
        },
        list(i = i)
      )
      output[[paste0("selectize_wrap_depth", i)]] <- renderUI(subs_widget_depth, quoted = T)
    }

    # DT::reloadData(proxy, resetPaging = FALSE)
  })


  ########### Save Code and Comments if Input changes ###########
  ###############################################################

  lapply(
    X = 1:nrow(Act_participant),
    FUN = function(i) {
      observeEvent(input[[paste0("selectize_code_Thought", i)]], {
        CodeNew_Thought <- read.csv(paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""))[, -1]
        CodeNew_Thought[i, 1] <- paste(input[[paste0("selectize_code_Thought", i)]], collapse = " ; ")
        write.csv(CodeNew_Thought, file = paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""), row.names = TRUE)
      })
    }
  )

  lapply(
    X = 1:nrow(Act_participant),
    FUN = function(i) {
      observeEvent(input[[paste0("selectize_code_Activity", i)]], {
        CodeNew_Activity <- read.csv(paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""))[, -1]
        CodeNew_Activity[i, 2] <- paste(input[[paste0("selectize_code_Activity", i)]], collapse = " ; ")
        write.csv(CodeNew_Activity, file = paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""), row.names = TRUE)
      })
    }
  )

  lapply(
    X = 1:nrow(Act_participant),
    FUN = function(i) {
      observeEvent(input[[paste0("selectize_code_Location", i)]], {
        CodeNew_Location <- read.csv(paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""))[, -1]
        CodeNew_Location[i, 3] <- paste(input[[paste0("selectize_code_Location", i)]], collapse = " ; ")
        write.csv(CodeNew_Location, file = paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""), row.names = TRUE)
      })
    }
  )

  lapply(
    X = 1:nrow(Act_participant),
    FUN = function(i) {
      observeEvent(input[[paste0("selectize_code_Company", i)]], {
        CodeNew_Company <- read.csv(paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""))[, -1]
        CodeNew_Company[i, 4] <- paste(input[[paste0("selectize_code_Company", i)]], collapse = " ; ")
        write.csv(CodeNew_Company, file = paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""), row.names = TRUE)
      })
    }
  )

  lapply(
    X = 1:nrow(Act_participant),
    FUN = function(i) {
      observeEvent(input[[paste0("selectize_general", i)]], {
        Other_Comments <- read.csv(paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""))[, -1]
        Other_Comments[i, 5] <- paste(input[[paste0("selectize_general", i)]], collapse = " ; ")
        write.csv(Other_Comments, file = paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""), row.names = TRUE)
      })
    }
  )

  lapply(
    X = 1:nrow(Act_participant),
    FUN = function(i) {
      observeEvent(input[[paste0("selectize_depth", i)]], {
        Depth_Comments <- read.csv(paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""))[, -1]
        Depth_Comments[i, 6] <- paste(input[[paste0("selectize_depth", i)]], collapse = " ; ")
        write.csv(Depth_Comments, file = paste(Projectwd, User, "/Act_", ppID, ".csv", sep = ""), row.names = TRUE)
      })
    }
  )

  ####### Reload data if page of the table changes #######
  ########################################################
}

shinyApp(ui, server)