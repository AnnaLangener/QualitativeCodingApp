#####################################################
########## Shiny App for Activity Coding ############
#####################################################

# This shiny app can be used for coding activities

# 1. Select the folder in which the codebook is stored and the results will be saved
Projectwd <- "/Users/annalangener/Nextcloud/Shared/Testing Methods to Capture Social Context/Qualitative context/3. Coding/QualitativeCoding_Activies/"

# 2. Select the coding scheme that you want to use. 
# You can choose our proposed coding scheme, the coding scheme proposed by Skimina et al., or you own by specify the path where the codebook is stored
# IMPORTANT: The codes need to be in a column named "Code" and if levels are included, those need to be in a column called "Level"
#Codebook_Act <- read_excel(paste(Projectwd,"Codebook_shared_activities.xlsx",sep =""), sheet = 1)
Codebook <- "Codebook_Stadeletal.csv" # Needed
Codebook_Act <- read.csv(paste(Projectwd,Codebook,sep =""))

# 3. Select the path where the data is stored
Data <- read.csv(paste(Projectwd,"Data/act_coding_ALL.csv",sep = ""))[,-1]
# The dataframe should be sorted by Date, to allow for context coding
#Data <-  Data %>% arrange(ppID, timeStampStart)
#write.csv(Data,paste(Projectwd,"Data/act_coding_ALL.csv",sep = ""))

# 4. Select who is coding (a folder will be created if this is a new person)
User <- "Anna_TestCoding"  # "Marie_FullCoding", "Marie", "Anna"

# 5. Indicate how you column is named that includes the participant IDs and select the participant of interest
id_column = "ppID" # Change the name of the column here
ppID <- 106 

# 6. Indicate whether your codebook contains different levels?
Levels = TRUE

# 7. Click "Run App"


#####################################################
#####################################################
## THE REST OF THE CODE DOES NOT NEED TO BE CHANGED ##

# A lot of the code that creates the table is copied from following github question
## https://github.com/rstudio/shiny/issues/1246

## THE REST OF THE CODE DOES NOT NEED TO BE CHANGED ##

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
if(!file.exists(paste(Projectwd,User, sep = ""))){
  dir.create(paste(Projectwd,User, sep = ""))
}

# Next we prepare the dataframe for the selected participant
Act_participant <- Data[Data[id_column] == ppID,] # 326, 317, 318, 316, 309

# If the participant is selected for the first time we create an empty dataframe
if(!file.exists(paste(Projectwd,User,"/Act_",ppID,".csv",sep = ""))){
Empty <- data.frame(Code = rep(NA,nrow(Act_participant)), OtherComments = rep(NA,nrow(Act_participant)))
write.csv(Empty, paste(Projectwd,User,"/Act_",ppID,".csv",sep = ""))
}


########## Shiny App ###########

if(Levels == TRUE){
ui <- navbarPage("Qualitative Coding",
                 theme = bs_theme(version = 5, bootswatch = "minty"),
                 tabPanel(Codebook,id = "Week",
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
    a$Code <- sapply(paste0("selectize_wrap_code",1:nrow(Act_participant)), function(x) as.character(uiOutput(x)))
    a$'Additional Information' <- sapply(paste0("selectize_wrap_additionalinfo",1:nrow(Act_participant)), function(x) as.character(uiOutput(x)))
    a$'Other/Comments' <- sapply(paste0("selectize_wrap_other",1:nrow(Act_participant)), function(x) as.character(uiOutput(x)))
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
  Act <- read.csv(paste(Projectwd,User,"/Act_",ppID,".csv",sep = ""))[-1]
  
  observeEvent(input$Act_participant_rows_current, {
    Act <- read.csv(paste(Projectwd,User,"/Act_",ppID,".csv",sep = ""))[-1]
    print("Act dataframe reloaded")
    print(head(Act))
  
    # This has to be in there otherwise its saved but not loaded    
    for (i in 1:nrow(Act_participant)) {
      subs_widget <- substitute({selectizeInput(paste0("selectize_code",i), NULL, choices=as.list(Codebook_Act), selected = c(unlist(str_split(Act[i,1]," ; "))), multiple = T,
                                                options = list(render = I("
                                                      {
                                                        item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                                                        option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                                                      }"))
      ) 
        
      }, list(i = i))
      output[[paste0("selectize_wrap_code",i)]] <- renderUI(subs_widget, quoted = T)
      
    }
    
    for (i in 1:nrow(Act_participant)) {
      subs_widget2 <- substitute({textInput(paste0("selectize_other",i), NULL,value = c(unlist(str_split(Act[i,2]," ; "))))
      }, list(i = i))
      output[[paste0("selectize_wrap_other",i)]] <- renderUI(subs_widget2, quoted = T)
    }
    
    for (i in 1:nrow(Act_participant)) {
      subs_widget3 <- substitute({tagList(
        textInput(paste0("selectize_feelings",i), NULL, placeholder = "Feelings/ Valence",value = c(unlist(str_split(Act[i,3]," ; ")))),
        textInput(paste0("selectize_whom",i),NULL, placeholder ="With whom?",value = c(unlist(str_split(Act[i,4]," ; "))))
      )
      }, list(i = i))
      output[[paste0("selectize_wrap_additionalinfo",i)]] <- renderUI(subs_widget3, quoted = T)
    }
    
   # DT::reloadData(proxy, resetPaging = FALSE)
  })

  
  
  ########### Save Code and Comments if Input changes ###########
  ###############################################################
  
  lapply(
    X = 1:nrow(Act_participant),
    FUN = function(i){
      observeEvent(input[[paste0("selectize_code", i)]], {
        CodeNew <-  read.csv(paste(Projectwd,User,"/Act_",ppID,".csv",sep = ""))[,-1]
        CodeNew[i,1] <- paste(input[[paste0("selectize_code", i)]], collapse = " ; ")
        write.csv(CodeNew, file = paste(Projectwd,User,"/Act_",ppID,".csv",sep = ""),row.names=TRUE)
      })
    })
  
  lapply(
    X = 1:nrow(Act_participant),
    FUN = function(i){
      observeEvent(input[[paste0("selectize_feelings", i)]], {
        Feelings <- read.csv(paste(Projectwd,User,"/Act_",ppID,".csv",sep = ""))[,-1]
        Feelings[i,3] <- paste(input[[paste0("selectize_feelings", i)]], collapse = " ; ")
        write.csv(Feelings, file = paste(Projectwd,User,"/Act_",ppID,".csv",sep = ""),row.names=TRUE)
      })
    })
  
  lapply(
    X = 1:nrow(Act_participant),
    FUN = function(i){
      observeEvent(input[[paste0("selectize_whom", i)]], {
        Whom <- read.csv(paste(Projectwd,User,"/Act_",ppID,".csv",sep = ""))[,-1]
        Whom[i,4] <- paste(input[[paste0("selectize_whom", i)]], collapse = " ; ")
        write.csv(Whom, file = paste(Projectwd,User,"/Act_",ppID,".csv",sep = ""),row.names=TRUE)
      })
    })
  
  
  lapply(
    X = 1:nrow(Act_participant),
    FUN = function(i){
      observeEvent(input[[paste0("selectize_other", i)]], {
        Other_Comments <- read.csv(paste(Projectwd,User,"/Act_",ppID,".csv",sep = ""))[,-1]
        Other_Comments[i,2] <- paste(input[[paste0("selectize_other", i)]], collapse = " ; ")
        write.csv(Other_Comments, file = paste(Projectwd,User,"/Act_",ppID,".csv",sep = ""),row.names=TRUE)
      })
    })
  
  ####### Reload data if page of the table changes #######
  ########################################################
 

  
}

shinyApp(ui, server)


