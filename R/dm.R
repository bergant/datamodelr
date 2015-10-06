#' datamodelr: data model diagrams
#'
#' Provides a simple structure to describe data models,
#' functions to read data model from YAML file,
#' and a function to create DiagrammeR graph objects:
#'
#'
#' \itemize{
#'   \item \pkg{datamodelr}'s data model object is a simple list of data frames which
#'     represent the model entities and include elements and their relations.
#'     See \code{\link{as.data_model}}.
#'   \item Function \code{\link{dm_read_yaml}} reads YAML format and creates a
#'     data model object.
#'   \item Function \code{\link{dm_graph}} creates DiagrammeR graph object from
#'     data model object.
#' }
#'
#' @docType package
#' @name datamodelr-package
#' @aliases datamodelr
NULL

#' Coerce to a data model
#'
#' Functions to coerce an object to a data model if possible.
#'
#' @details Function accepts a named list of data frames or a single data frame
#'   with column 'table'.
#'
#'   Data frame(s) must have an element named 'column'.
#'   Element 'key' (boolean) defines a column as a primary key.
#'   Element 'ref' (character string) defines referenced table name.
#' @param x Object (list or data frame) to be coerced to data model object
#' @aliases as.data_model
#' @return If possible it returns a data model object.
#'   It is a list of data frames with at least the following columns:
#'   \item{ column }{A name of the column in a table}
#'   \item{ key }{A boolean value indicating this column is a primary key or NA.}
#'   \item{ ref }{A character string with a referenced table name.
#'     Not being NA means the column is a foreign key.}
#' @export
as.data_model <- function(x) {
  UseMethod("as.data_model")
}

#' @keywords internal
#' @export
as.data_model.list <- function(x) {

  if(mode(x) != "list") stop("Not a list")
  if(is.null(names(x))) stop("List has no names")
  if(!all(sapply(x, function(tab) "column" %in% names(tab))))
  {
    stop("All data frames in a list must have a 'column' element.")
  }
  class(x) <- c("data_model", class(x))
  x
}

#' @keywords internal
#' @export
as.data_model.data.frame <- function(x) {

  if(!inherits(x, "data.frame")) stop("Not a data.frame")

  if(!all(c("column", "table") %in% names(x)))
  {
    stop("Data frame must have elements named 'table' and 'column'.")
  }
  x <- split(x, x[["table"]])
  as.data_model(x)
}

#' Read YAML
#'
#' Reads a file in YAML format and returns a data model object.
#'
#' @details YAML description should include table names (first level),
#' columns (second level) and column attributes (third level).
#' Expected (but not required) column attributes are
#'   \code{key} (Yes|No),
#'   \code{ref} (Name of referenced table),
#'   \code{comment} (column description).
#'
#' @param file A file in YAML format
#' @param text A YAML formated character string
#' @examples
#' dm <-
#'   dm_read_yaml(text = "
#'
#'     Person:
#'       Person ID: {key: yes}
#'       Name:
#'       E-mail:
#'       Street:
#'       Street number:
#'       City:
#'       ZIP:
#'
#'     Order:
#'       Order ID: {key: yes}
#'       Customer: {ref: Person}
#'       Sales person: {ref: Person}
#'       Order date:
#'       Requested ship date:
#'       Status:
#'
#'     Order Line:
#'       Order ID: {key: yes, ref: Order}
#'       Line number: {key: yes}
#'       Order item: {ref: Item}
#'       Quantity:
#'       Price:
#'
#'     Item:
#'       Item ID: {key: yes}
#'       Item Name:
#'       Description:
#'   ")
#' @export
dm_read_yaml <- function(file = NULL, text = NULL) {

  if( !requireNamespace("yaml", quietly = TRUE)) {
    stop("yaml package needed for this function to work. Please install it.",
         call. = FALSE)
  }


  if(missing(text)) {
    if(!missing(file)) {
      dm <- yaml::yaml.load_file(file)
    } else {
      stop("A file or text needed.")
    }
  } else {
    dm <- yaml::yaml.load(text)
  }


  lapply(dm, function(x) {
    column <- names(x)
    if(any(duplicated(column)))
      stop("Duplicated columns")
    comment <-
      ifelse(sapply(x, function(x) mode(x) == "character"), x, NA)
    attrs <- unique(unname(unlist(sapply(x, function(x) names(x)))))
    ret <- data.frame(
      column = column,
      ..comment = unlist(comment),
      stringsAsFactors = FALSE)
    row.names(ret) <- NULL
    for(col_attr in attrs) {
      # every column attribute is a new column in data model table
      ret[[col_attr]] <- NA
      for(tab_col in ret[["column"]]) {
        if(inherits(x[[tab_col]], "list") &&
           !is.null(x[[tab_col]][[col_attr]])) {
          ret[ret[["column"]] == tab_col, col_attr] <- x[[tab_col]][[col_attr]]
        }
      }
    }
    if(!is.null(ret[["comment"]])) {
      ret[["comment"]] <-
        ifelse(is.na(ret[["comment"]]), ret[["..comment"]], ret[["comment"]])
    } else {
      ret[["comment"]] <-
        ret[["..comment"]]
    }
    ret[["..comment"]] <- NULL
    ret
  })
}


#' Create a graph object from data model object
#'
#' @param x A data model object
#' @param rankdir Graph attribute for direction (eg. 'BT' = bottom --> top)
#' @param graph_name A graph name
#' @param graph_attrs Additional graph attributes
#' @param use_ref_ports If TRUE the outgoing links are positioned from
#'   specific table elements (foreign keys). By default links are positioned
#'   from table to table.
#' @export
dm_graph <- function(x, rankdir = "BT", graph_name = "Data Model",
                     graph_attrs = "", use_ref_ports = FALSE) {

  if( !requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("DiagrammeR package needed for this function to work. Please install it.",
         call. = FALSE)
  }

  g_labels <- sapply(
    names(x),
    function(tab_name) {
      tab <- x[[tab_name]]
      tab_cols <- tab$column
      if(is.null(tab$ref))
        tab$ref <- NA
      if(is.null(tab$key))
        tab$key <- NA
      tab_mark <- ifelse(is.na(tab$ref), "", "~")
      tab_label <- ifelse(is.na(tab$key) | tab$key == 0,
                          tab$column, sprintf("<U>%s</U>", tab$column))
      paste(
        '  <<TABLE ALIGN="LEFT" BORDER="1" CELLBORDER="0" CELLSPACING="0">\n',
        sprintf('    <TR><TD COLSPAN="2" BGCOLOR="#CCCCCC" BORDER="0">%s</TD></TR>\n', tab_name),
        paste(
          sprintf('    <TR><TD ALIGN="RIGHT">%s</TD><TD ALIGN="LEFT" PORT="%s">%s</TD></TR>',
                  tab_mark, tab_cols, tab_label),
          collapse = "\n "
        ),
        '\n   </TABLE>>\n'
      )
    }
  )

  nodes <-
    DiagrammeR::create_nodes(
      nodes = names(x),
      label = g_labels,
      shape = "rectangle",
      type = "lower"
    )

  edges <-
    do.call(
      rbind,
      lapply(names(x), function(tab_name) {
        tab <- x[[tab_name]]
        if(!is.null(tab$ref) && any(!is.na(tab$ref))) {
          references <- with(tab, tab[!is.na(ref), c("column","ref")])
          ret <- data.frame(
            from = if(use_ref_ports) {
              sprintf("'%s':'%s'", tab_name, references$column)
            } else {
              sprintf("%s", tab_name)
            },
            to = references$ref,
            stringsAsFactors = FALSE)
        } else {
          ret <- data.frame()
        }
      })
    )


  graph <-
    DiagrammeR::create_graph(
      graph_attrs = sprintf('rankdir=%s tooltip="%s" %s', rankdir, graph_name, graph_attrs),
      node_attrs = 'margin=0 color="gray" fontcolor = "#444444"',
      nodes_df = nodes,
      edges_df = edges,
      edge_attrs = c('color = "#555555"',"arrowsize = 1"))

  # removing some extra ' from original dot code:
  graph$dot_code <- gsub("' *<<TABLE", "<<TABLE", graph$dot_code)
  graph$dot_code <- gsub("TABLE>>\n'", "TABLE>>", graph$dot_code)
  graph$dot_code <- gsub("''", "'", graph$dot_code)
  graph
}






