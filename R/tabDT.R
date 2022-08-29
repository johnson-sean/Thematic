#' Personalized DT Table
#'
#' @param dat           data set that will be used to create the table
#' @param identity      name of row count
#' @param identity_col  the location and name of id column
#' @param cell_colors   color of cells
#' @param table_number  number of the given table
#' @param table_title   title of table
#' @param btn           selection of buttons csv, excel, and pdf
#' @import magrittr
#' @import DT
#' @return
#' @export
#'
#' @examples
tabDT <-function(
    dat = NULL,
    identity = "ID",
    identity_col = c('ID'= 1),
    cell_colors = 'white',
    table_number="Table: ",
    table_title = "title",
    btn = c('csv', 'excel', 'pdf')
){
  dat%>%
    datatable(
      colnames = identity_col,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center;',
        table_number, htmltools::em(table_title)),
      #journal
      # callback=JS('$("button.buttons-csv").css("background-color","#E6E6E6");
      #               $("button.buttons-excel").css("background-color","#E6E6E6");
      #               $("button.buttons-pdf").css("background-color","#E6E6E6");
      #               return table;'),
      callback=JS('$("button.buttons-csv").css("background-color","#91D4FE");
                    $("button.buttons-excel").css("background-color","#91D4FE");
                    $("button.buttons-pdf").css("background-color","#91D4FE");
                    return table;'),
      extensions = c('Buttons','Scroller'),
      options = list(
        dom = 'Bfrtip',
        buttons = btn,
        deferRender = TRUE,
        #searching= FALSE,
        scrollY = 350,
        scroller = TRUE,
        scrollCollapse = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        #journal
        # initComplete = JS("
        #             function(settings, json) {
        #               $(this.api().table().header()).css({
        #                 'background-color': '#C8C8C8',
        #                 'color': 'black'
        #               });
        #             }")
        initComplete = JS("
                    function(settings, json) {
                      $(this.api().table().header()).css({
                        'background-color': '#0197F6',
                        'color': 'black'
                      });
                    }")
      ),
      class = 'row-border hover compact',
      fillContainer = T
    )%>%
    formatStyle(columns = c(identity, colnames(dat)),backgroundColor = cell_colors)
  }
