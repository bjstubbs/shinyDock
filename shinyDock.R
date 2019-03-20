####################################
#Name: shinyDock
#Goal: To help filter dockstore to find methods
#      based on search term
#
####################################

########################
#load libraries
########################
require(shiny)
require(DT)
require(AnVIL)
########################


########################
#functions
########################


filterDS<-function(mypattern){
  temp=jsonlite::fromJSON(
    httr::content(dockstore$allPublishedWorkflows(filter=mypattern),"text"))
  keep=c("workflowName","description","author","organization","id","path",
  "full_workflow_path","gitUrl","sourceControl","dbCreateDate","last_modified_date")
  temp[,keep]}

getWDL<-function(path){
  path=URLencode(path,reserved=TRUE)
  temp=jsonlite::fromJSON(
    httr::content(dockstore$getPublishedWorkflowByPath(path), "text"))
  temp2=temp$workflowVersions$sourceFiles
  temp2[[length(temp2)]]$content[1]
}

#Setup:

########################
#shiny
########################
shinyDock = function() {


########################
curPath=NA
mytable=NA
  #start shiny app config
  shinyApp(
  ##########
  #Start UI Config
  ##########
    ui = fluidPage(
      titlePanel("shinyDock"),
      sidebarLayout(position = "left",
        sidebarPanel(width=2,
          textInput("filterPattern", "Filter Pattern"),
          actionButton("filterButton", "Filter"),
          actionButton("resetButton", "Reset")
          ),
        mainPanel("",width=10,
          tabsetPanel(
            tabPanel("Data",h3("Current Data"),
            dataTableOutput('mytable')),
            tabPanel("Method",h3("Method"),
            htmlOutput("methodcode")),
            tabPanel("Configure",h3("Configure Method"),
            textInput("input", "input"))
          )
        )
      )
    )
  ,
  ####################
  #Start Server Config
  ####################
  server = function(input, output, session) {
    #set up output
    observeEvent(input$filterButton, {
      subs=filterDS(input$filterPattern)
      #table
      mytable<<-subs
      output$mytable = renderDataTable(subs,options = list(scrollX = TRUE,pageLength=5),selection = 'single')
    })

    output$methodcode=renderText({
      mytext="No Method Selected"
      s = input$mytable_rows_selected
      if (length(s)) {
        curPath<<-mytable[s,"path"]
        if(!is.na(curPath)){
          mytext=getWDL(curPath)
          mytext=gsub("\\n","<br>",mytext)
          mytext
          }
      }
      mytext
    })

    observeEvent(input$resetButton, {
      subs=data.frame(id=NA,workflow=NA)
      output$mytable = renderDataTable({subs},options = list(scrollX = TRUE,pageLength=5))
    })
  }
)
}
