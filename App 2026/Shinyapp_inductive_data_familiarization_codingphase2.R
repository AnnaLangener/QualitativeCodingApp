#############################################################
########## Shiny App for Eeske dataset Coding ############
########## Inductive Coding: Data Familiarization ##########
########## Phase 2: coding while building a codebook ##########
#############################################################

library(dplyr)
library(readxl)

# This shiny app can be used for coding activities

# 1. Select the folder in which the codebook is stored and the results will be saved
# CHANGE TO WHERE YOUR SURFDRIVE FOLDER IS LOCATED
Projectwd <- "C:/Users/Jenni/Nextcloud/ESMOT-Qq_ProjectFolder/ESMOT-BigQ/"

# 2. Select the coding scheme 
Codebook <- "Inductive coding/Codebook_ShinyApp_02042026.xlsx" # DON'T CHANGE
Codebook_Act <- read_excel(paste(Projectwd,Codebook,sep ="")) # DON'T CHANGE

# 3. Read in data 
data <- read_excel("20250819_Swinging_Moods_ESM_data_anonymized.xlsx") # DON'T CHANGE

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

# 4. Select who is coding (a folder will be created if this is a new person)
User <- "JC"  # ADD YOUR NAME HERE

# 5. Indicate how your column is named that includes the participant IDs and select the participant of interest
id_column = "Participant_ID" # Change the name of the column here # DON'T CHANGE
ppID <- 27311

# 6. Indicate whether your codebook contains different levels?
Levels = FALSE # DON'T CHANGE

# 7. Click "Run App"


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
if(Levels == TRUE){
  Codebook_Act <- Codebook_Act[,colnames(Codebook_Act) %in% c("Level","Code")]
  t1 <- Codebook_Act %>%
    mutate(html=ifelse(Level == '1',
                       paste0("<span style='color:#9F73AB';>", Code, "</span>"),
                       ifelse(Level == '2',
                              paste0("<span style='color:#19376D';>", Code, "</span>"),
                              paste0("<span style='color:#0C7B93';>", Code, "</span>")
                       )
    ))
  Codebook_Act <- setNames(t1$Code, t1$html)
}else{
  Codebook_Act <- Codebook_Act$Code
}

######## Data Storage ##########

# Here we check if the specified user already has a subfolder
if(!file.exists(paste(Projectwd,"Inductive coding/",User, sep = ""))){
  dir.create(paste(Projectwd,"Inductive coding/",User, sep = ""))
}

# Next we prepare the dataframe for the selected participant
Act_participant <- Data[Data[id_column] == ppID,] # 326, 317, 318, 316, 309

# If the participant is selected for the first time we create an empty dataframe
if(!file.exists(paste(Projectwd,"Inductive coding/",User,"/Act_",ppID,".csv",sep = ""))){
  Empty <- data.frame(`Familiarization note: beep` = rep(NA,nrow(Act_participant)),
                      `Familiarization note: day` = rep(NA,nrow(Act_participant)),
                      `Proposed event code` = rep(NA,nrow(Act_participant)),
                      `Existing event code` = rep(NA,nrow(Act_participant)))
  write.csv(Empty, paste(Projectwd,"Inductive coding/", User,"/Act_",ppID,".csv",sep = ""))
}


########## Shiny App ###########

if(Levels == TRUE){
  ui <- navbarPage("Qualitative Coding",
                   theme = bs_theme(version = 5, bootswatch = "minty"),
                   tabPanel(Codebook, "Data familiarization: event",id = "Week",
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
                            withSpinner(DT::dataTableOutput('Act_participant'))),
  )
}else{
  ui <- navbarPage("Qualitative Coding",
                   theme = bs_theme(version = 5, bootswatch = "minty"),
                   tabPanel(Codebook,id = "Week",
                            withSpinner(DT::dataTableOutput('Act_participant'))),
  )
}

server <- function(session, input, output){
  
  ###################### Create Datatable #####################
  #############################################################
  output$Act_participant <- DT::renderDataTable({
    a <- Act_participant # the static dataframe
    a$'Familiarization note: beep' <- sapply(paste0("selectize_wrap_general",1:nrow(Act_participant)), function(x) as.character(htmltools::HTML(as.character(uiOutput(x)))))
    a$'Familiarization note: day' <- sapply(paste0("selectize_wrap_depth",1:nrow(Act_participant)), function(x) as.character(htmltools::HTML(as.character(uiOutput(x)))))
    a$'Proposed event code' <- sapply(paste0("selectize_wrap_event_new",1:nrow(Act_participant)), function(x) as.character(htmltools::HTML(as.character(uiOutput(x)))))
    a$'Existing event code' <- sapply(paste0("selectize_wrap_event_existing",1:nrow(Act_participant)), function(x) as.character(htmltools::HTML(as.character(uiOutput(x)))))
    
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
        `Proposed event code`, `Existing event code`
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
      subs_widget_new_event <- substitute({
        textInput(paste0("selectize_event_new",i), NULL, placeholder = "Proposed event code", value = c(unlist(str_split(Act[i,3]," ; "))))
      }, list(i = i))
      output[[paste0("selectize_wrap_event_new",i)]] <- renderUI(subs_widget_new_event, quoted = T)
    }
    
    for (i in 1:nrow(Act_participant)) {
      subs_widget_ex_event <- substitute({
        selectizeInput(paste0("selectize_event_existing",i), NULL, choices=as.list(Codebook_Act), selected = c(unlist(str_split(Act[i,4]," ; "))), multiple = T,
                       options = list(render = I("
                                                      {
                                                        item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                                                        option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                                                      }"))
        ) 
        
      }, list(i = i))
      output[[paste0("selectize_wrap_event_existing",i)]] <- renderUI(subs_widget_ex_event, quoted = T)
      
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
      observeEvent(input[[paste0("selectize_event_new", i)]], {
        Event_code <- read.csv(paste(Projectwd,"Inductive coding/",User,"/Act_",ppID,".csv",sep = ""))[,-1]
        Event_code[i,3] <- paste(input[[paste0("selectize_event_new", i)]], collapse = " ; ")
        write.csv(Event_code, file = paste(Projectwd,"Inductive coding/",User,"/Act_",ppID,".csv",sep = ""), row.names = TRUE)
      })
    })
  
  lapply(
    X = 1:nrow(Act_participant),
    FUN = function(i){
      observeEvent(input[[paste0("selectize_event_existing", i)]], {
        Event_existing_code <- read.csv(paste(Projectwd,"Inductive coding/",User,"/Act_",ppID,".csv",sep = ""))[,-1]
        Event_existing_code[i,4] <- paste(input[[paste0("selectize_event_existing", i)]], collapse = " ; ")
        write.csv(Event_existing_code, file = paste(Projectwd,"Inductive coding/",User,"/Act_",ppID,".csv",sep = ""), row.names = TRUE)
      })
    })
  
  ####### Reload data if page of the table changes #######
  ########################################################
  
  
  
}

shinyApp(ui, server)