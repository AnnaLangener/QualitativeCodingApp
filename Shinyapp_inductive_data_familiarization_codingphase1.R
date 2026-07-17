#############################################################
########## Shiny App for Eeske dataset Coding ############
########## Inductive Coding: Data Familiarization ##########
#############################################################

library(dplyr)
library(readxl)
library(tcltk)

# This shiny app can be used for coding activities

# 1. Select the folder in which the codebook is stored and the results will be saved
Projectwd <- tk_choose.dir(default = "", caption = "Select directory")
Projectwd <- paste0(Projectwd,"/")

# 2. Read in data 
data_path <- "20250819_Swinging_Moods_ESM_data_anonymized.xlsx"
data <- read_excel(file.path(Projectwd, data_path)) # DON'T CHANGE

# The dataframe should be sorted by Date, to allow for context coding
# DON'T CHANGE
Data <- data %>% 
  dplyr::select(Participant_ID, 
                #Beep_ID, 
                Day, Obs, Time1,
                Thought, 
                Activity, 
                Location, 
                Company,
                Event) %>%
  arrange(Participant_ID, Obs)

# 3. Select who is coding (a folder will be created if this is a new person)
User <- "JC"  # ADD YOUR NAME HERE

# 4. Indicate how your column is named that includes the participant IDs and select the participant of interest
id_column = "Participant_ID" # Change the name of the column here # DON'T CHANGE
ppID <- 10254

# 5. Click "Run App"


## THE REST OF THE CODE DOES NOT NEED TO BE CHANGED ##

# A lot of the code that creates the table is copied from following github question
## https://github.com/rstudio/shiny/issues/1246

###### Load Packages

library(shiny)
library(DT)
library(readxl)
library(stringr)
library(bslib)
library(shinycssloaders)

library(dplyr)
library(shiny)
library(stringi)

#######

######### Create different colors for levels #########
# Here we create a dataframe that colors the different levels in the dropdown menu (if levels are included)
# if(Levels_Thought == TRUE){
#   Codebook_Act_Thought <- Codebook_Act_Thought[,colnames(Codebook_Act_Thought) %in% c("Level","Code")]
#   t1_Thought <- Codebook_Act_Thought %>%
#     mutate(html=ifelse(Level == '1', 
#                        paste0("<span style='color:#9F73AB';>", Code, "</span>"),
#                        ifelse(Level == '2',
#                               paste0("<span style='color:#19376D';>", Code, "</span>"),
#                               paste0("<span style='color:#0C7B93';>", Code, "</span>")
#                        )
#     ))
#   Codebook_Act_Thought <- setNames(t1_Thought$Code, t1_Thought$html)
# }else{
#   Codebook_Act_Thought <- Codebook_Act_Thought$Code
# }
# 
# if(Levels_Activity == TRUE){
#   Codebook_Act_Activity <- Codebook_Act_Activity[,colnames(Codebook_Act_Activity) %in% c("Level","Code")]
#   t1_Activity <- Codebook_Act_Activity %>%
#     mutate(html=ifelse(Level == '1', 
#                        paste0("<span style='color:#9F73AB';>", Code, "</span>"),
#                        ifelse(Level == '2',
#                               paste0("<span style='color:#19376D';>", Code, "</span>"),
#                               paste0("<span style='color:#0C7B93';>", Code, "</span>")
#                        )
#     ))
#   Codebook_Act_Activity <- setNames(t1_Activity$Code, t1_Activity$html)
# }else{
#   Codebook_Act_Activity <- Codebook_Act_Activity$Code
# }
# 
# if(Levels_Location == TRUE){
#   Codebook_Act_Location <- Codebook_Act_Location[,colnames(Codebook_Act_Location) %in% c("Level","Code")]
#   t1_Location <- Codebook_Act_Location %>%
#     mutate(html=ifelse(Level == '1', 
#                        paste0("<span style='color:#9F73AB';>", Code, "</span>"),
#                        ifelse(Level == '2',
#                               paste0("<span style='color:#19376D';>", Code, "</span>"),
#                               paste0("<span style='color:#0C7B93';>", Code, "</span>")
#                        )
#     ))
#   Codebook_Act_Location <- setNames(t1_Location$Code, t1_Location$html)
# }else{
#   Codebook_Act_Location <- Codebook_Act_Location$Code
# }
# 
# if(Levels_Company == TRUE){
#   Codebook_Act_Company <- Codebook_Act_Company[,colnames(Codebook_Act_Company) %in% c("Level","Code")]
#   t1_Company <- Codebook_Act_Company %>%
#     mutate(html=ifelse(Level == '1', 
#                        paste0("<span style='color:#9F73AB';>", Code, "</span>"),
#                        ifelse(Level == '2',
#                               paste0("<span style='color:#19376D';>", Code, "</span>"),
#                               paste0("<span style='color:#0C7B93';>", Code, "</span>")
#                        )
#     ))
#   Codebook_Act_Company <- setNames(t1_Company$Code, t1_Company$html)
# }else{
#   Codebook_Act_Company <- Codebook_Act_Company$Code
# }
######## Data Storage ##########

# Here we check if the specified user already has a subfolder
if(!file.exists(file.path(Projectwd,"Inductive coding/", User))){
  dir.create(paste(Projectwd,"Inductive coding/",User, sep = ""))
}

# Next we prepare the dataframe for the selected participant
Act_participant <- Data[Data[id_column] == ppID,] # 326, 317, 318, 316, 309

# If the participant is selected for the first time we create an empty dataframe
if(!file.exists(paste(Projectwd,"Inductive coding/",User,"/Act_",ppID,".csv",sep = ""))){
  Empty <- data.frame(`Familiarization note: beep` = rep(NA,nrow(Act_participant)),
                      `Familiarization note: day` = rep(NA,nrow(Act_participant)))
  write.csv(Empty, paste(Projectwd,"Inductive coding/", User,"/Act_",ppID,".csv",sep = ""))
}


########## Shiny App ###########

ui <- navbarPage("Qualitative Coding",
                 theme = bs_theme(version = 5, bootswatch = "minty"),
                 tabPanel("Data familiarization: event",id = "Week",
                          withSpinner(DT::dataTableOutput('Act_participant'))),
)

server <- function(session, input, output){
  
  ###################### Create Datatable #####################
  #############################################################
  output$Act_participant <- DT::renderDataTable({
    a <- Act_participant # the static dataframe
    a$'Familiarization note: beep' <- sapply(paste0("selectize_wrap_general",1:nrow(Act_participant)), function(x) as.character(uiOutput(x)))
    a$'Familiarization note: day' <- sapply(paste0("selectize_wrap_depth",1:nrow(Act_participant)), function(x) as.character(uiOutput(x)))
    a$'Proposed event code' <- sapply(paste0("selectize_wrap_event",1:nrow(Act_participant)), function(x) as.character(uiOutput(x)))
    
    # Reorder the columns for easier coding
    a <- a %>%
      dplyr::select(
        Participant_ID, Day, Obs, Time1,
        Thought, 
        Activity, 
        Location, 
        Company,
        Event, 
        `Familiarization note: beep`, `Familiarization note: day`,
        `Proposed event code`
      )
    
    
    a <- datatable(a,
                   escape = F, selection = "single", 
                   options = list(paging = TRUE, ordering = FALSE, searching = FALSE, pageLength = 20,dom = 'tp',
                                  preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node());}'),
                                  drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '))
    )
    return(a)
  })
  
  ################ rendering fancy selectize widgets ###############
  ##################################################################
  
  #proxy <- DT::dataTableProxy('Act_participant')
  
  # Read existing Code/ Comments
  Act <- read.csv(paste(Projectwd,"Inductive coding/",User,"/Act_",ppID,".csv",sep = ""))[-1]
  
  observeEvent(input$Act_participant_rows_current, {
    Act <- read.csv(paste(Projectwd,"Inductive coding/",User,"/Act_",ppID,".csv",sep = ""))[-1]
    print("Act dataframe reloaded")
    print(head(Act))
    
    # This has to be in there otherwise its saved but not loaded    
    for (i in 1:nrow(Act_participant)) {
      subs_widget_general <- substitute({textInput(paste0("selectize_general",i), NULL,
                                                   placeholder = "Beep level",
                                                   value = c(unlist(str_split(Act[i,1]," ; "))))
      }, list(i = i))
      output[[paste0("selectize_wrap_general",i)]] <- renderUI(subs_widget_general, quoted = T)
    }
    
    
    for (i in 1:nrow(Act_participant)) {
      subs_widget_depth <- substitute({
        textInput(paste0("selectize_depth",i), NULL, placeholder = "Day level", value = c(unlist(str_split(Act[i,2]," ; "))))
      }, list(i = i))
      output[[paste0("selectize_wrap_depth",i)]] <- renderUI(subs_widget_depth, quoted = T)
    }
    
    for (i in 1:nrow(Act_participant)) {
      subs_widget_depth <- substitute({
        textInput(paste0("selectize_event",i), NULL, placeholder = "Proposed event code", value = c(unlist(str_split(Act[i,3]," ; "))))
      }, list(i = i))
      output[[paste0("selectize_wrap_event",i)]] <- renderUI(subs_widget_depth, quoted = T)
    }
    # DT::reloadData(proxy, resetPaging = FALSE)
  })
  
  
  
  ########### Save Code and Comments if Input changes ###########
  ###############################################################
  
  lapply(
    X = 1:nrow(Act_participant),
    FUN = function(i){
      observeEvent(input[[paste0("selectize_general", i)]], {
        Other_Comments <- read.csv(paste(Projectwd,"Inductive coding/",User,"/Act_",ppID,".csv",sep = ""))[,-1]
        Other_Comments[i,1] <- paste(input[[paste0("selectize_general", i)]], collapse = " ; ")
        write.csv(Other_Comments, file = paste(Projectwd,"Inductive coding/",User,"/Act_",ppID,".csv",sep = ""),row.names=TRUE)
      })
    })
  
  lapply(
    X = 1:nrow(Act_participant),
    FUN = function(i){
      observeEvent(input[[paste0("selectize_depth", i)]], {
        Depth_Comments <- read.csv(paste(Projectwd,"Inductive coding/",User,"/Act_",ppID,".csv",sep = ""))[,-1]
        Depth_Comments[i,2] <- paste(input[[paste0("selectize_depth", i)]], collapse = " ; ")
        write.csv(Depth_Comments, file = paste(Projectwd,"Inductive coding/",User,"/Act_",ppID,".csv",sep = ""), row.names = TRUE)
      })
    })
  
  lapply(
    X = 1:nrow(Act_participant),
    FUN = function(i){
      observeEvent(input[[paste0("selectize_event", i)]], {
        Event_code <- read.csv(paste(Projectwd,"Inductive coding/",User,"/Act_",ppID,".csv",sep = ""))[,-1]
        Event_code[i,3] <- paste(input[[paste0("selectize_event", i)]], collapse = " ; ")
        write.csv(Event_code, file = paste(Projectwd,"Inductive coding/",User,"/Act_",ppID,".csv",sep = ""), row.names = TRUE)
      })
    })
  ####### Reload data if page of the table changes #######
  ########################################################
  
  
  
}

shinyApp(ui, server)