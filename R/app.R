#' @importFrom shiny runGadget basicPage textOutput actionButton h2 hr
#'
#' @importFrom DT renderDataTable datatable dataTableOutput
#'
#' @export
hca_view <-
    function(tbl)
{
    runGadget(
        app = basicPage(
            h2("The HCA data"),
            textOutput("data_selected", inline = TRUE),
            actionButton("done", "Done"),
            hr(),
            dataTableOutput("hca")
        ),

        server = function (input, output) {
            output$hca = renderDataTable({
                datatable(
                    tbl,
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
