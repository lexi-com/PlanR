library(shiny)
library(tidyverse)
library(DT)
library(RSQLite)

#alles auf Deutsch setzen
options(DT.options=list(language=list(emptyTable="Keine Eintraege vorhanden.",search="Suche:", 
                                      lengthMenu="Zeige _MENU_ Eintraege", info="Eintrag _START_ bis _END_ aus _TOTAL_ Eintraegen.",
                                      paginate=list(previous="vorherige", 'next'="naechste"), infoEmpty="0 bis 0 von 0 Eintraegen")))
Sys.setlocale("LC_ALL", "German")

#Pfad zur Datenbank, statisch gesetzt
pfad <- "data01.sqlite"
table_names<-c("Termin")

loadDB<-function(tabname){
  tryCatch(
    {
      db<-dbConnect(RSQLite::SQLite(), pfad)
      data <- dbGetQuery(db, paste0("SELECT * FROM ",tabname,";"))
      dbDisconnect(db)
      return(data)  
    },
    error = function(e){
      #Rueckkopplung TODO Laden einer lokalen Kopie
      print("Laden der Datenbank fehlgeschlagen")
    })
}

dbTrigger<-reactiveVal(TRUE)

choice1<-c("Arbeit 1"=1, "Arbeit 2"=2, "Arbeit 3"=3, "Familie"=4, "Freizeit"=5)
choice2<-c("Anni"=1, "Ronny"=2,"Flo"=3, "Magda"=4, "Till"=5)

ui <- fluidPage(
  theme = 'minimal.css',
  windowTitle = 'PlanR',
  
  # App title ----
  titlePanel("editable/sqlite-Familienplaner"),
  fluidRow(column(img(src="logo.jpg", height="100pt", style=""), width=3, offset=9)),
  
  DT::DTOutput(outputId = "table"),
  
  shiny::actionButton(inputId = "add", label = "hinzufuegen"),
  shiny::actionButton(inputId = "remove", label = "loeschen"),
  shiny::actionButton(inputId = "edit", label = "bearbeiten"),
  shiny::actionButton(inputId = "reload", label = "DB neu laden"),
  
  uiOutput("work")
  
)



# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  mod_df <- shiny::reactiveValues(x = NULL, y= NULL, z=c())
  
  observe(
    if(dbTrigger()){
      data<-loadDB(table_names[1])
      mod_df$x <- dplyr::tibble(data%>%mutate(Datum=as.Date(Datum)))
      dbTrigger(FALSE)
    }
  )
  
  trowh<-reactive(mod_df$x[input$table_rows_selected,])
  tid<-reactiveVal()
  
  output$table <- DT::renderDT({
    isolate(mod_df$x)
  },    
  selection=list(mode="single", target = "row"), 
  server=TRUE, 
  editable =list(target = 'cell', disable = list(columns = c(1)))
  )
  
  proxy <- DT::dataTableProxy('table')
  
  observeEvent(input$table_cell_edit,{
    #UPDATE in DB
    tryCatch(
      {
        tid(mod_df$x[input$table_cell_edit$row,1])
        cname<-colnames(mod_df$x)[input$table_cell_edit$col]
        db<-dbConnect(RSQLite::SQLite(), pfad)
        data <- dbExecute(db, paste0(
          "UPDATE Termin SET ",cname, "= '",input$table_cell_edit$value,"' WHERE TerminID =",tid(),";"
        ))
        dbDisconnect(db)
      },
      error = function(e){
        #Rueckkopplung
        output$work<-renderUI(paste("Fehler editable."))
      }
    )  
    #modify proxy
    mod_df$x <<-editData(mod_df$x, input$table_cell_edit, 'table')
    #Rueckkopplung
    output$work<-renderUI(paste("Datenbankeintrag geaendert (editable)."))
  })
  
  shiny::observeEvent(input$add, {
    output$work<-renderUI(
      tagList(
        shiny::dateInput(inputId = "datum", label = "Datum"),
        shiny::textInput(inputId = "zeit", label = "Uhrzeit"),
        shiny::textInput(inputId = "dauer", label = "Dauer"),
        shiny::selectInput(inputId = "art", label = "Art", choices = choice1),
        shiny::selectInput(inputId = "person", label = "Person", choices = choice2),
        shiny::textInput(inputId = "eintrag", label = "Eintrag"),
        
        shiny::actionButton(inputId = "add2", label = "Eintrag hinzufuegen"),
        shiny::actionButton(inputId = "back2", label = "zurueck")
      )
    )
  })
  
  shiny::observeEvent(input$add2, {
    #CREATE in DB
    tryCatch(
      {
        db<-dbConnect(RSQLite::SQLite(), pfad)
        data <- dbExecute(db, paste0(
          "INSERT INTO Termin (Datum, Zeit, Dauer, Art, Person, Eintrag) VALUES('",input$datum,"','",input$zeit,"','",input$dauer,"','",names(choice1[as.integer(input$art)]),"','",names(choice2[as.integer(input$person)]),"','",input$eintrag,"');"
        ))
        tid(dbGetQuery(db, paste0(
          "SELECT TerminID FROM Termin WHERE (Datum ='",input$datum,"' AND Zeit ='", input$zeit,"' AND Dauer ='",input$dauer,"' AND Art='",names(choice1[as.integer(input$art)]),"' AND Person ='",names(choice2[as.integer(input$person)]),"' AND Eintrag ='",input$eintrag,"');"
        ))[1,1])                      
        dbDisconnect(db)
      },
      error = function(e){
        #Rueckkopplung
        output$work<-renderUI(paste("Datenbankeintrag fehlgeschlagen."))
      }
    )  
    #modify proxy
    mod_df$x <- mod_df$x %>%
      dplyr::bind_rows(
        dplyr::tibble(TerminID=tid(),
                      Datum=input$datum, 
                      Zeit=input$zeit,
                      Dauer=input$dauer,
                      Art = names(choice1[as.integer(input$art)]), 
                      Person = names(choice2[as.integer(input$person)]),
                      Eintrag = input$eintrag)
      )
    #Rueckkopplung
    output$work<-renderUI(paste("Zeile hinzugefuegt."))
  })
  
  shiny::observeEvent(input$back2, {
    output$work<-renderUI(paste("Keine Aktion."))
  })
  
  
  shiny::observeEvent(input$remove, {
    tid(mod_df$x[input$table_rows_selected,]$TerminID)
    if(length(tid())==0){
      #Rueckkopplung
      output$work<-renderUI(paste("Bitte waehle eine Tabellenzeile aus."))
    }else{
      #modify proxy
      mod_df$x <- mod_df$x[-as.integer(input$table_rows_selected), ]
      #DELETE in DB
      tryCatch(
        {
          db<-dbConnect(RSQLite::SQLite(), pfad)
          data <- dbExecute(db, paste0(
            "DELETE FROM Termin WHERE TerminID = '",tid(),"';"
          ))
          dbDisconnect(db)
        },
        error = function(e){
          #Rueckkopplung
          output$work<-renderUI(paste("Datenbankeintrag fehlgeschlagen."))
        }
      )  
      #Rueckkopplung
      output$work<-renderUI(paste("Zeile geloescht."))
    }
  })
  
  shiny::observeEvent(input$edit, {
    tid(mod_df$x[input$table_rows_selected,]$TerminID)
    if(length(tid())==0){
      #Rueckkopplung
      output$work<-renderUI(paste("Bitte waehle eine Tabellenzeile aus."))
    }else{
      output$work<-renderUI(
        tagList(
          shiny::dateInput(inputId = "datum2", label = "Datum", value = trowh()$Datum),
          shiny::textInput(inputId = "zeit2", label = "Uhrzeit", value = trowh()$Zeit),
          shiny::textInput(inputId = "dauer2", label = "Dauer", value = trowh()$Dauer),
          shiny::selectInput(inputId = "art2", label = "Art", choices = choice1, selected=which(names(choice1)==trowh()$Art)),
          shiny::selectInput(inputId = "person2", label = "Person", choices = choice2, selected=which(names(choice2)==trowh()$Person)),
          shiny::textInput(inputId = "eintrag2", label = "Eintrag", value=trowh()$Eintrag),
          
          shiny::actionButton(inputId = "edit2", label = "diesen Eintrag aendern"),
          shiny::actionButton(inputId = "back", label = "zurueck")
        )
      )
    }
  })
  
  shiny::observeEvent(input$edit2, {
    #UPDATE in DB
    tryCatch(
      {
        db<-dbConnect(RSQLite::SQLite(), pfad)
        data <- dbExecute(db, paste0(
          "UPDATE Termin SET Datum = '",input$datum2,"', Zeit = '", input$zeit2, "', Dauer = '", input$dauer2, "', Art = '", names(choice1[as.integer(input$art2)]), "', Person = '", names(choice2[as.integer(input$person2)]),"', Eintrag = '", input$eintrag2,"' WHERE TerminID =",tid(),";"
        ))
        dbDisconnect(db)
      },
      error = function(e){
        #Rueckkopplung
        output$work<-renderUI(paste("Datenbankeintrag fehlgeschlagen."))
      }
    )  
    # Update proxy
    mod_df$x[mod_df$x$TerminID==tid(),2:7] <- 
      dplyr::tibble(Datum=input$datum2, 
                    Zeit=input$zeit2,
                    Dauer=input$dauer2,
                    Art = names(choice1[as.integer(input$art2)]), 
                    Person = names(choice2[as.integer(input$person2)]),
                    Eintrag = input$eintrag2)
    #Rueckkopplung
    output$work<-renderUI(paste("Zeile geaendert"))
  })
  
  shiny::observeEvent(input$back, {
    output$work<-renderUI(paste("Keine Aktion."))
  })
  
  shiny::observeEvent(input$reload, {
    dbTrigger(TRUE)
    output$work<-renderUI(paste("DB neu geladen."))
  })
  
  
  shiny::observe({
    DT::replaceData(proxy, mod_df$x)
  })
}

shinyApp(ui, server)