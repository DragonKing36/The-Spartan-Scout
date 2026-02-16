library(shiny)
library(shinyjs)
library(pins)
library(googledrive)
library(qrcode)
library(shiny.destroy)
library(rclipboard)
library(tippy)
library(tidyverse)

#Pre-Defines----
emailAddress <- Sys.getenv("emailAddress")
drivelink <- Sys.getenv("drivelink")
driveFolder <- Sys.getenv("driveFolder")
pitFolder <- Sys.getenv("pitFolder")



drive_auth(
  token = Sys.getenv("HashToken")


board <- board_gdrive(googledrive::as_id(drivelink),versioned = FALSE)

Match_Matrix <- read_csv(drive_read_string(file = as_id((drive_ls(paste("~/",driveFolder,sep = "")) %>% filter(name == "Match_Schedule.csv"))$id)))
Match_Matrix <- Match_Matrix[,c('match_number','red1','red2','red3','blue1','blue2','blue3')]
config_data <- as.list(read_csv(drive_read_string(file = as_id((drive_ls(paste("~/",driveFolder,sep = "")) %>% filter(name == "2026 Scouting Config - Output.csv"))$id))))


pitData <- bind_rows(lapply(lapply(as_id(drive_find(pattern = "Pit_Scouting", type = "csv")$id), drive_read_string), read_csv))

pitData<-sort_by(pitData,pitData$Capacity)

pitData <- pitData[!duplicated(pitData$Team_Number),]


AutoClimbVector <- c("None","L1")
TeleClimbVector <- c("None","L1","L2","L3")
ScouterVector <- config_data$Scouters
MatchVector <- Match_Matrix$match_number
RobotVector <- c("Red 1","Red 2","Red 3","Blue 1","Blue 2","Blue 3")
color_vector <- c ("red1", "red2", "red3","blue1","blue2","blue3")

#Increment----

IncrementUI <- function(id) {
  tagList(
    fluidRow(
      disabled(actionButton(NS(id,"Minus"),"-")),
      textOutput(NS(id,"Total"),inline = TRUE),
      actionButton(NS(id,"Plus"),"+"),
    )
  )
}

IncrementServer <- makeModuleServerDestroyable(function(id,OutputVal,FirstTime = TRUE){
  moduleServer(id, function(input, output, session) {
    
    
    TotalReactive <- reactiveVal(0)
    
    observe({
      shinyjs::toggleState("Minus", TotalReactive() > 0)
    })
    observeEvent(input$Minus,{
      TotalReactive(TotalReactive()-1)
    })
    
    observeEvent(input$Plus,{
      TotalReactive(TotalReactive()+1)
    })
    
    
    output$Total <- renderText(TotalReactive())
    
    observeEvent(TotalReactive(),
                 {OutputVal[[id]] <- TotalReactive()}
    )
    
  })
})

#Calculate Outputs----

#----

#MainInterface----
InterfaceUi <- fluidPage(
  shinyjs::useShinyjs(),
  tabsetPanel(
    #Main Interface----
    tabPanel("Main Interface",
             titlePanel("Scouting"),
             fluidRow(
               column(3,
                      h4("Prematch"),
                      checkboxInput("TeamSpec","Override Team Number",value=FALSE),
                      checkboxInput("ScouterSpec","Override Scouter",value=FALSE),
                      
                      conditionalPanel("input.ScouterSpec == 0",selectInput("scouter","Scouter",ScouterVector)),
                      
                      conditionalPanel("input.ScouterSpec == 1",textInput("scouterType","Scouter")),
                      
                      selectInput("Robot","Robot", RobotVector),
                      
                      #Automatic Fill (Team Data)
                      conditionalPanel(condition = "input.TeamSpec == 0",
                                       selectInput("MatchNum","Match Number", MatchVector,selected = 1),
                                       h5(strong("Team Number")),
                                       textOutput("DynamicTeam")),
                      
                      #Manual Fill (Team Data)
                      conditionalPanel(condition = "input.TeamSpec == 1",
                                       numericInput("MatchNumSpec","Match Number",0,min=0),
                                       numericInput("TeamNumSpec","Team Number",0,min=0)),
                      
                      checkboxInput("NoShow", "No Show"),
                      uiOutput("Capacity")
               ),
               conditionalPanel(condition = "input.NoShow == 0",
                                column(3,
                                       h4("Auto"),
                                       selectInput("AutoClimb","Climb (Auto)", AutoClimbVector, "None"),
                                       h5("Fuel"),
                                       actionButton("AddAutoCycleGroup","Add Cycle Group"),
                                ),
                                column(3,
                                       h4("Tele"),
                                       selectInput("TeleClimb","Climb (Endgame)", TeleClimbVector, "None"),
                                       h5("Fuel"),
                                       actionButton("AddTeleCycleGroup","Add Cycle Group"),
                                )),
               column(3,
                      h4("Postmatch"),
                      numericInput("Accuracy","Estimated Accuracy %", value = 100, min = 0, max = 100, step = 10),
                      checkboxInput("Defense","Played Defense"),
                      
                      checkboxInput("Bar","Went Under Bar"),
                      checkboxInput("Bump","Went Over Bump"),
                      checkboxInput("died","Died"),
                      checkboxInput("tipped","Tipped"),
                      textAreaInput( 
                        "comments", 
                        "Comments", 
                        value = ""
                      ), 
                      checkboxInput("confirmReset", "Confirm Reset"),
                      checkboxInput("confirmSubmit","Confirm Submit"),
                      actionButton("reset","Reset"),
                      actionButton("submit","Submit digitally"),
                      actionButton("QRsubmit","Submit by QR code")))
    ),
    #Devtools----
    tabPanel("Devtools",
             rclipboardSetup(),
             uiOutput("ColCopy"),
             tippy_this(
               "ColCopy",
               tooltip = "String copied!",
               trigger = "click"
             ),
    )
    
    #----
  )
)
#Server----
InterfaceServer <- function(input, output, session){
  
  onlineEnabled <- reactiveVal(0)
  
  AutoGroupCount <- reactiveVal(0)
  TeleGroupCount <- reactiveVal(0)
  
  MatchMatrix <- reactiveVal()
  
  AutoScoredata <- reactiveValues()
  TeleScoredata <- reactiveValues()
  AutoGroupdata <- reactiveValues()
  TeleGroupdata <- reactiveValues()
  
  
  #Cycle Inputs (This code is a mess, if you open it, BEWARE) ----
  observeEvent(input$AddAutoCycleGroup, {
    AutoGroupCount(AutoGroupCount()+1)
    localCount <- AutoGroupCount()
    insertUI("#AddAutoCycleGroup",
             "beforeBegin",
             {
               div(id="DELETE_AUTO",tagList(
                 h4("Cycle Count"),
                 IncrementUI(NS(localCount,"AutoCycleCount")),
                 numericInput( 
                   NS(localCount,"AutoCycleFuel"), 
                   "Fuel Scored Per Cycle", 
                   value = 0, 
                   min = 0,
                   step = 5
                 )))
             }
    )
    IncrementServer(NS(localCount,"AutoCycleCount"), OutputVal = AutoScoredata)
    observeEvent( input[[NS(localCount,"AutoCycleFuel")]] ,{
      AutoGroupdata[[NS(localCount,"AutoCycleFuel")]] <- input[[NS(localCount,"AutoCycleFuel")]]
    })
    
  })
  
  InsideDynamicTeam <- reactive({
    as.character(Match_Matrix[[as.numeric(input$MatchNum),color_vector[RobotVector == input$Robot]]])
  })
  
  output$DynamicTeam <- renderText(InsideDynamicTeam())
  
  TrueTeam <- reactive({
    ifelse(input$TeamSpec == 0,InsideDynamicTeam(),input$TeamNumSpec)
  })
  
  TrueMatch <- reactive(
    {
      ifelse(input$TeamSpec == 0,input$MatchNum,input$MatchNumSpec)
    }
  )
  
  trueScouter <- reactive(
    {
      ifelse(input$ScouterSpec == 0,input$scouter,input$scouterType)
    }
  )
  
  activeCapacity <- reactive({as.numeric(pitData[which(TrueTeam() == pitData$Team_Number),'Capacity'])
})
  
  output$HopperSize <- renderText(activeCapacity())
  
  output$Capacity <- renderUI(if(is.na(activeCapacity())==TRUE){
    
  } else {
    tagList(
    strong(h5("Hopper Capacity")),
    textOutput("HopperSize")
  )})
  
  
  
  observeEvent(input$AddTeleCycleGroup, {
    TeleGroupCount(TeleGroupCount()+1)
    localCount <- TeleGroupCount()
    insertUI("#AddTeleCycleGroup",
             "beforeBegin",
             {
               div(id="DELETE_TELE",tagList(
                 h4("Cycle Count"),
                 IncrementUI(NS(localCount,"TeleCycleCount")),
                 numericInput( 
                   NS(localCount,"TeleCycleFuel"), 
                   "Fuel Scored Per Cycle", 
                   value = 0, 
                   min = 0,
                   step = 5
                 )))
             }
    )
    
    IncrementServer(NS(localCount,"TeleCycleCount"), OutputVal = TeleScoredata)
    observeEvent(input[[NS(localCount,"TeleCycleFuel")]] ,{
      TeleGroupdata[[NS(localCount,"TeleCycleFuel")]] <- input[[NS(localCount,"TeleCycleFuel")]]
    })
    
  })
  #----
  
  observeEvent(input$confirmReset,{
    shinyjs::toggleState("reset", input$confirmReset==TRUE)
  })
  
  observeEvent(input$confirmSubmit,{
    shinyjs::toggleState("submit", input$confirmSubmit==TRUE)
  })
  
  observeEvent(input$confirmSubmit,{
    shinyjs::toggleState("QRsubmit", input$confirmSubmit==TRUE)
  })
  
  # Calculation of Balls Scored ----
  
  AutoPoints <- reactive(
    {Autofuel <- 0
    for(i in 1:AutoGroupCount())
      Autofuel <- Autofuel+((AutoScoredata[[NS(i,"AutoCycleCount")]])*(AutoGroupdata[[NS(i,"AutoCycleFuel")]]))
    if(length(Autofuel) == 0){
      Autofuel <- 0
    }
    return(Autofuel)}
  )
  
  TelePoints <- reactive(
    {Telefuel <- 0
    for(i in 1:TeleGroupCount())
      Telefuel <- Telefuel+((TeleScoredata[[NS(i,"TeleCycleCount")]])*(TeleGroupdata[[NS(i,"TeleCycleFuel")]]))
    if(length(Telefuel) == 0){
      Telefuel <- 0
    }
    return(Telefuel)}
  )
  
  #----
  
  observeEvent(input$reset, {
    reset("confirmReset")
    reset("confirmSubmit")
    if(AutoGroupCount() != 0){
      for(i in 1:AutoGroupCount()){
        removeUI("#DELETE_AUTO")
        destroyModule(id = NS(i,"AutoCycleCount"))
      }}
    
    
    if(TeleGroupCount() != 0){
      for(i in 1:TeleGroupCount()){
        removeUI("#DELETE_TELE")
        destroyModule(id = NS(i,"TeleCycleCount"))
      }
    }
    AutoGroupCount(0)
    TeleGroupCount(0)
    AutoScoredata <- reactiveValues()
    TeleScoredata <- reactiveValues()
    AutoGroupdata <- reactiveValues()
    TeleGroupdata <- reactiveValues()
    
    reset("NoShow")
    reset("AutoClimb")
    reset("TeleClimb")
    reset("Bar")
    reset("Bump")
    reset("Defense")
    reset("Accuracy")
  })
  
  output_data <- reactive(tibble(
    Scouter = trueScouter(),
    Team = TrueTeam(),
    Match = TrueMatch(),
    Robot = input$Robot,
    Fuel_Auto = AutoPoints(),
    Fuel_Tele = TelePoints(),
    Climb_Auto = input$AutoClimb,
    Climb_Tele = input$TeleClimb,
    No_Show = input$NoShow,
    Bar = input$Bar,
    Bump = input$Bump,
    Defense = input$Defense,
    Accuracy = input$Accuracy,
    died = input$died,
    tipped = input$tipped,
    comments = input$comments
  ))
  
  observeEvent(input$submit,{
    reset("confirmSubmit")
    showModal(modalDialog(
      title = "Submission begun",
      "The data is currently being sent, you may close this message, but do NOT close this app. You will be notified when the submission has succeeded.",
      easyClose = TRUE
    ))
    
    
    file_name=paste("Match",TrueMatch(),"Team",TrueTeam(),sep = "_")
    
    board %>% pin_write(name = file_name, x = output_data(), type = "csv")
    
    showModal(modalDialog(
      title = "Submission Successful",
      "You may now close the app.",
      easyClose = TRUE
    ))
  })

  
  col_name_output <- reactive(paste(colnames(output_data()), collapse = "\t", sep = "/n"))
  
  output$ColCopy <- renderUI({
    rclipButton(
      inputId = "ColCopy", 
      label = "Copy Column names for spreadsheet", 
      clipText = col_name_output(), 
      icon = icon("clipboard"),
      tooltip = "Click me to copy the content of the text field to the clipboard!",
      options = list(delay = list(show = 800, hide = 100), trigger = "hover")
    )
  })
  
  QROutput <- eventReactive(input$QRsubmit,{qr_code(paste(output_data(), collapse = "\t", sep = "/n"))})
  
  output$QRreturn <- renderPlot(plot(QROutput()))
  
  observeEvent(input$QRsubmit,{
    showModal(modalDialog(
      title = "Submission Successful",
      plotOutput("QRreturn"),
      easyClose = FALSE
    ))
  })
}

#AppStart----
shinyApp(InterfaceUi,InterfaceServer)
