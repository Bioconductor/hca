library(DT)
library(shiny)

hca_view <- 
    function(tbl) {
        runGadget(
            ui <- basicPage(
                h2("The HCA data"),
                textOutput("data_selected", inline = TRUE),
                actionButton("done", "Done"),
                hr(),
                DT::dataTableOutput("hca")
            ),
            
            server <- function (input, output) {
                output$hca = DT::renderDataTable({
                    DT::datatable(tbl,
                                  selection = 'multiple',
                                  rownames = FALSE
                    )
                })
                output$data_selected <- renderText({
                    paste(
                        "Number selected:",
                        length(input$hca_rows_selected)
                    )
                })
                observe({
                    if(input$done) {
                        stopApp(tbl[input$hca_rows_selected,])
                    }
                })
            }
        )
    }