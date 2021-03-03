#' Make a database diagram using a connection and display to browser
#'
#' @param connection A database connection.
#' @param dbType One of 'sqlserver', or 'postgres'.
#' @param rankdir Direction of diagram default is 'BT'.
#' @param view_type One of 'all', 'keys_only', or 'view_only'.
#' @param focus A character vector of tables to be filtered to.
#' @param wd The working directory to use.
#' @param fileName The fileName.
#' @keywords database diagram
#' @export
#' @examples
#' browse_db_model()

browse_db_model <-
  function(connection = NULL,
           dbType = 'sqlserver',
           rankdir = 'BT',
           view_type = 'keys_only',
           focus = NULL,
           wd = getwd(),
           fileName = 'dbDiagram.svg') {
    query <- datamodelr::dm_re_query(dbType)
    db <- odbc::dbGetQuery(connection, query)

    # convert to a data model
    db <- datamodelr::as.data_model(db)

    if (!is.null(focus)) {
      focus <- list(tables = focus)
    }

    graph <-
      datamodelr::dm_create_graph(db,
                                  rankdir = rankdir,
                                  view_type = view_type,
                                  focus = focus)

    rsvg::rsvg_svg(charToRaw(DiagrammeRsvg::export_svg(DiagrammeR::grViz(graph$dot_code))), fileName)
    browseURL(paste0('file://', file.path(wd, fileName)))
  }
