
#' Formats table with title, subtitle, and source documentation
#'
#' @param table_data data set that will be used to create the table
#' @param table_title title, to be wrote in MD format
#' @param table_subtitle subtitle, to be wrote in MD format
#' @param source_doc documentation, to be wrote in MD format
#' @import magrittr
#' @import gt
#' @return
#' @export
#'
#' @examples
title_note_format <- function(
    table_data = NULL,
    table_title = NULL,
    table_subtitle = NULL,
    source_doc = ""#,
    #...
){
  table_data%>%
    gt()%>%
    tab_header(
      title = md(table_title),
      subtitle = md(table_subtitle)
    )%>%
    cols_align(
      align = c("center"),
      columns = everything()
    )%>%
    tab_source_note(
      source_note = md(source_doc)
    )
}

#' Formats values within cells by column
#'
#' @param table_data data set that will be used to create the table
#' @param dec        decimal place of the number data
#' @param trail      determines if there should be the removal of trailing 0's
#' @import magrittr
#' @import gt
#' @return
#' @export
#'
#' @examples
column_format <-function(
    table_data = NULL,
    dec = 2,
    trail = TRUE,
    date_col = NULL,
    style = 1
){
  table_data%>%
    fmt_number(
      columns = where(is.double),
      decimals = dec,
      drop_trailing_zeros = trail
    )%>%
    fmt_date(
      columns = date_col,
      date_style = style
    )%>%
    fmt_markdown(
      columns = where(is.factor)
    )
}
#' Format cell line
#'
#' @param table_data        data set that will be used to create the table
#' @param cell_line_color   color of cell line
#' @param cell_weight       thickness of line
#' @param cell_style        type of line
#' @import magrittr
#' @import gt
#' @return
#' @export
#'
#' @examples
cell_format <- function(
    table_data = NULL,
    cell_line_color = "black",
    cell_weight = 1,
    cell_style = "solid"
){
  table_data%>%
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = cell_line_color,
        weight = px(cell_weight),
        style = cell_style
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    )
}

#' Footnote format
#'
#' @param table_data            data set that will be used to create the table
#' @param ...                   values to be placed within the tab_foot() function
#' @param footnote_line_color   color of border line
#' @param footnote_weight       thickness of border line
#' @param footnote_style        style of border line
#' @import magrittr
#' @import gt
#' @return
#' @export
#'
#' @examples
footnote_format <- function(
    table_data = NULL,
    ...,
    footnote_line_color = "black",
    footnote_weight = 2,
    footnote_style = "solid"
){
  table_data%>%
    tab_footnote(...)%>%
    #the footnote top border
    tab_style(
      style = cell_borders(
        sides = c("top"),
        color = footnote_line_color,
        weight = px(footnote_weight),
        style = footnote_style
      ),
      locations = cells_footnotes()
    )
}

#' Formats font, color for fill and main border
#'
#' @param table_data          data set that will be used to create the table
#' @param font                font type
#' @param font_color          font color
#' @param main_bgc            background color of title/subtitle
#' @param secondary_bgc       background color of non-title or cells
#' @param main_border_color   color of tables border
#' @import magrittr
#' @import gt
#' @return
#' @export
#'
#' @examples
table_option <-function(
    table_data = NULL,
    #font = 'Perpetua',
    font = 'Bookman',
    font_color="black",
    #journal colors
    # main_bgc = "#C8C8C8",
    # secondary_bgc = "#E6E6E6",
    main_bgc = "#0197F6",
    secondary_bgc = "#91D4FE",
    main_border_color = "black"
){
  table_data%>%
    tab_options(
      table.font.names = font, #Georgia #Bookman Old StylePerpetua
      table.font.color = font_color,

      heading.background.color = main_bgc,
      heading.title.font.size = 26,
      heading.subtitle.font.size = 18,
      heading.align = "center",
      heading.padding = 5,
      heading.border.bottom.style = "solid",
      heading.border.bottom.color = main_border_color,
      heading.border.bottom.width = 3,


      column_labels.background.color = secondary_bgc,
      column_labels.font.size = 16,
      column_labels.font.weight = "bold",
      column_labels.border.top.color = main_border_color,
      ##column_labels.border.top.style = "solid",
      column_labels.border.bottom.color = main_border_color,
      ##column_labels.border.bottom.style = "solid",
      column_labels.padding.horizontal = 15, #20
      column_labels.border.bottom.width = 2,

      row_group.background.color = secondary_bgc,
      row_group.font.size = 16,
      row_group.font.weight = main_border_color,
      row_group.border.top.color = main_border_color,
      ##row_group.top.bottom.style = "solid",
      row_group.border.bottom.color = main_border_color,
      ##row_group.border.bottom.style = "solid",
      row_group.padding.horizontal = 15,

      summary_row.background.color = secondary_bgc,
      summary_row.border.color = main_border_color,
      summary_row.border.width = 1,

      grand_summary_row.background.color = secondary_bgc,
      grand_summary_row.border.color = main_border_color,
      grand_summary_row.border.width = 1,



      table.border.top.color = main_border_color,
      table.border.top.width = 3,

      table.border.bottom.color = main_border_color,
      table.border.bottom.width = 3,

      table.border.left.color = main_border_color,
      table.border.left.width = 3,
      table.border.left.style = "solid",

      table.border.right.color = main_border_color,
      table.border.right.width = 3,
      table.border.right.style = "solid",

      table.font.size = 14,
      data_row.padding = 3,
      container.overflow.x = F,

      table_body.border.bottom.color = main_border_color,
      table_body.border.bottom.width = 1,
      table_body.border.top.color = main_border_color,
      table_body.border.top.width = 1,

      footnotes.background.color =  secondary_bgc,
      footnotes.font.size = 11,
      footnotes.padding.horizontal = 5,
      footnotes.border.bottom.width = 1,
      footnotes.border.bottom.color = main_border_color,

      source_notes.background.color = secondary_bgc,
      source_notes.font.size = 11
    )
}
#' Formats data to static table
#'
#' @param table_data        data set that will be used to create the table
#' @param table_title       title in MD
#' @param table_subtitle    subtitle in MD
#' @param source_doc        source documentation in MD
#' @param dec               decimal place
#' @param trail             selection T/F to allow trailing 0's
#' @param cell_line_color   color of cell line
#' @param cell_weight       cell line thickness
#' @param cell_style        cell line style
#' @param font              font type
#' @param font_color        font color
#' @param main_bgc          background fill color (title/subtitle)
#' @param secondary_bgc     other fill color (not title or cell area)
#' @param main_border_color border color
#' @import magrittr
#' @import gt
#' @return
#' @export
#'
#' @examples
tabGT <-function(
    table_data = NULL,
    table_title = "",
    table_subtitle = "",
    source_doc = "",
    dec = 2,
    trail = TRUE,
    date_col = NULL,
    style = 1,
    cell_line_color = "black",
    cell_weight = 1,
    cell_style = "solid",
    font = 'Perpetua',
    font_color="black",
    #journal colors
    # main_bgc = "#C8C8C8",
    # secondary_bgc = "#E6E6E6",
    main_bgc = "#0197F6",
    secondary_bgc = "#91D4FE",
    main_border_color = "black"

){
  table_data%>%
    title_note_format(
      #table_data,
      table_title,
      table_subtitle,
      source_doc
    )%>%
    column_format(
      #table_data = NULL,
      dec,
      trail,
      date_col,
      style
    )%>%
    cell_format(
      #table_data = NULL,
      cell_line_color,
      cell_weight,
      cell_style
    )%>%
    table_option(
      #table_data = NULL,
      font,
      font_color,
      main_bgc,
      secondary_bgc,
      main_border_color
    )
}
