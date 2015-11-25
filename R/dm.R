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
#'   \item Function \code{\link{dm_create_graph}} creates DiagrammeR graph object from
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
#' @details Function accepts a data frame with columns info.
#'   Data frame must have 'table' and 'column' elements.
#'   Optional element 'key' (boolean) marks a column as a primary key.
#'   Optional element 'ref' (character string) defines referenced table name.
#'   Optional element 'ref_col' (character string) defines a column in a
#'     referenced table name primary key (only necessery when referenced
#'     table has a compound primary key).
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

  if(mode(x) != "list"){
    stop("Not a list")
  }
  if(!all(c("columns", "references") %in% (names(x)))) {
    stop("Input must have columns and references")
  }

  class(x) <- c("data_model", class(x))
  x
}

#' Check if object is a data model
#'
#' @param x Object to check if it is a data model
#' @keywords internal
#' @export
is.data_model <- function(x) {
  inherits(x, "data_model")
}


#' Coerce a data frame to a data model
#'
#' @keywords internal
#' @export
as.data_model.data.frame <- function(x) {

  if(!inherits(x, "data.frame")) stop("Not a data.frame")

  if(!all(c("column", "table") %in% names(x)))
  {
    stop("Data frame must have elements named 'table' and 'column'.")
  }

  if(!is.null(x[["key"]])) {
    x[is.na(x[,"key"]), "key"] <- FALSE
  } else {
    x[,"key"] <- FALSE
  }

  if(is.null(x[["ref"]])) x[["ref"]] <- NA


  # create references from ref and keys
  ref_table <- dm_create_references(x)

  table_attrs <- attr(x, "tables")
  if(is.null(table_attrs)) {
    table_attrs <-
      data.frame(
        table = unique(x[["table"]]),
        segment = NA,
        display = NA,
        row.names = NULL,
        stringsAsFactors = FALSE
      )
  }
  attr(x, "tables") <- NULL
  ret <- list(
    tables = table_attrs,
    columns = x,
    references = ref_table
  )
  as.data_model(ret)
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
      if(!file.exists(file)) stop("File does not exist.")
      dm <- yaml::yaml.load_file(file)
    } else {
      stop("A file or text needed.")
    }
  } else {
    dm <- yaml::yaml.load(text)
  }
  if(is.null(dm)) {
    return(NULL)
  }

  col_table <- dm_list2coltable(dm)
  return(as.data_model(col_table))
}


#' List to column table
#'
#' Convert a 3 level named list to a data frame with column info
#'
#' @details The funcion is used when creating data model object
#'   from list provided by yaml parser.
#' @param x a named list
#' @export
#' @keywords internal
dm_list2coltable <- function(x) {

  if(!is.list(x)) {
    stop("Input must be a list.")
  }

  if(is.null(names(x))) {
    # parsed yaml with sequences
    x_tables <- x[sapply(x, function(x) !is.null(x[["table"]]))]

    table_names <- sapply(x_tables, function(tab) tab[["table"]])
    columns <- lapply(x_tables, function(tab) {
      tab_name <- tab[["table"]]
      if(!is.null(tab_name)) {
        cols <- tab[["columns"]]
      }
    })
    names(columns) <- table_names

    column_names <- lapply(columns, names)
    column_attributes <- unique( unlist( lapply(columns, sapply, names)))

  } else {
    # Named list (parsed yaml with maps)
    columns <- x
    table_names <- names(columns)
    column_names <- lapply(columns, names)
    column_attributes <- unique( unlist( lapply(columns, sapply, names)))
  }


  table_list <-
    lapply(table_names, function(tab_name) {
      if(is.null(column_names[[tab_name]])) {
        column_names[[tab_name]] <- NA
      }
      tab <- data.frame(
        table = tab_name,
        column = column_names[tab_name],
        stringsAsFactors = FALSE
      )
      names(tab) <- c("table", "column")

      for(a in column_attributes) {
        attr_value <-
          unlist(
            sapply(column_names[[tab_name]], function(cname) {
              if(is.list(columns[[tab_name]][[cname]]))
                value <- columns[[tab_name]][[cname]][[a]]
              else
                value <- NA
              ifelse(is.null(value), NA, value)
            })
          )
        tab[[a]] <- attr_value
      }
      tab
    })

  ret <- do.call(rbind, table_list)

  table_attrs <- dm_get_table_attrs(x)
  if(!is.null(table_attrs) && is.null(table_attrs$segment))
    table_attrs$segment <- NA
  attr(ret, "tables") <- table_attrs

  ret
}

dm_get_table_attrs <- function(x) {

  x_tables <- x[sapply(x, function(x) !is.null(x[["table"]]))]
  table_names <- sapply(x_tables, function(tab) tab[["table"]])
  table_attrs <- unique(unlist(lapply(x_tables, names)))
  table_attrs <- table_attrs[!table_attrs %in% c("columns", "table")]
  names(x_tables) <- table_names

  table_attrs <-
    lapply(table_names, function(tab) {
      ret <-
        data.frame(
          table = tab,
          stringsAsFactors = FALSE
        )
      for(aname in table_attrs) {
        tab_attr <- x_tables[[tab]][[aname]]
        if(is.null(tab_attr)) {
          tab_attr <- NA
        }
        ret[[aname]] <- tab_attr
      }
      ret
    })

  do.call(rbind, table_attrs)
}


#' Create reference info
#'
#' Creates references (foreign keys) based on reference table names in
#' column info table.
#'
#' @param col_table A data frame with table columns
#' @details The function is used when creating data model object.
#'   \code{col_table} must have at least table, column and ref elements.
#'   When referencing to tables with compound primary keys
#'   additional ref_col with primary key columns must be provided.
#' @export
#' @keywords internal
dm_create_references <- function(col_table) {

  if(!inherits(col_table, "data.frame")) stop("Input must be a data frame.")

  if(!all(c("table", "column") %in% names(col_table))) {
    stop("Column info table must have table, column and ref variables.")
  }
  if(!"ref" %in% names(col_table)) {
    return(NULL)
  }
  if(all(is.na(col_table[,"ref"]))) {
    return(NULL)
  }


  if(is.null(col_table[["ref_col"]])) {
    col_table[["ref_col"]] <- NA
  }
  ref_table <- col_table[
    !is.na(col_table[["ref"]]),  # take only rows with reference
    c("table", "column", "ref", "ref_col")]
  col_table[is.na(col_table$key), "key"] <- FALSE

  ref_col <-
    with(ref_table,
         ifelse(is.na(ref_col),
                sapply(ref_table$ref, function(x)
                  col_table[col_table$table == x & col_table$key, "column"][1]
                ),
                ref_col
         )
    )
  ref_table[["ref_col"]] <- ref_col

  # number of columns in primary key
  num_col = sapply(ref_table$ref, function(x)
    length(col_table[col_table$table == x & col_table$key, "column"])
  )
  num_col[num_col == 0L] <- 1L

  key_col_num = {

    # create column index number
    rle1 <- rle(num_col)
    if(lengths(rle1)[1] > 0) {
      unlist(sapply(1:lengths(rle1)[1], function(i) {
        rep(1 : rle1$values[i], rle1$lengths[i] / rle1$values[i])
      }))
    } else {
      NA
    }
  }

  ref_table$ref_id <- cumsum(key_col_num == 1)
  ref_table$ref_col_num <- key_col_num
  ref_table
}

#' Create data model object from R data frames
#'
#' Uses data frame column names to create a data model diagram
#'
#' @param ... Data frames or one list of data frames
#' @export
dm_from_data_frames <- function(...) {

  df_list <- list(...)
  if(length(df_list) == 1 && inherits(df_list[[1]], "list")) {
    df_list <- df_list[[1]]
  } else {
    if(length(names(df_list)) < length(df_list)) {
      names(df_list) <- as.list(match.call( expand.dots = TRUE)[-1])
    }
  }
  tables <- df_list
  names(tables) <- make.names(names(tables))
  dfdm <-
    do.call(rbind,
            lapply(names(tables), function(table_name) {
              t1 <- tables[[table_name]]
              columns <- data.frame(
                column = names(t1),
                type = sapply(t1[0,], class),
                stringsAsFactors = FALSE)
              columns$table <- table_name
              columns
            })
    )
  as.data_model(dfdm)

}

#' Add reference
#'
#' Adds reference to existing data model object
#'
#' @param dm A data model object
#' @param table Table name
#' @param column Column(s) name
#' @param ref Referenced table name
#' @param ref_col Referenced column(s) name
#' @return New data model object
#' @export
dm_add_reference <- function(dm, table, column, ref = NULL, ref_col = NULL) {
  ref_df <-
    data.frame(
      table = table,
      column = column,
      ref = ref,
      ref_col = ref_col,
      ref_id = ifelse(is.null(dm$references), 1, max(dm$references$ref_id) + 1),
      ref_col_num = 1:(length(ref_col)),

      stringsAsFactors = FALSE
    )
  dm$references <- rbind(dm$references, ref_df)
  dm$columns$ref[dm$columns$table == table & dm$columns$column == column] <- ref
  dm
}

#' Set key
#'
#' Set column as a primary key
#'
#' @param dm A data model object
#' @param table Table name
#' @param column Column(s) name
#' @export
dm_set_key <- function(dm, table, column) {
  update_cols <- dm$columns$table == table & dm$columns$column == column
  if(!any(update_cols)) {
    stop("Column not found.")
  }
  dm$columns$key[update_cols] <- TRUE
  dm
}

#' Set column attribute
#'
#' Set column attribute value
#'
#' @param dm A data model object
#' @param table Table name
#' @param column Column(s) name
#' @param attr Column attribute name
#' @param value New value
#' @export
#' @keywords internal
dm_set_col_attr <- function(dm, table, column, attr, value) {
  update_cols <- dm$columns$table == table & dm$columns$column == column
  if(!any(update_cols)) {
    stop("Column not found.")
  }
  dm$columns[update_cols, attr] <- value
  dm
}


#' Reverse engineer query
#'
#' Returns a string with SQL query to reverse engineer a database
#'
#' @param rdbms Which database ("postgres" or "sqlserver")
#' @return A character string with sql query
#' @export
#' @examples
#' \dontrun{
#' library(RPostgreSQL)
#' # dvdrental sample database: http://www.postgresqltutorial.com/postgresql-sample-database
#' con <- dbConnect(dbDriver("PostgreSQL"), dbname="dvdrental", user ="postgres")
#' sQuery <- dm_re_query("postgres")
#' dm_dvdrental <- dbGetQuery(con, sQuery)
#' dbDisconnect(con)
#' }
dm_re_query <- function(rdbms) {
  sql_script <- sprintf("sql/%s.sql", rdbms)
  file_name <- system.file(sql_script, package ="datamodelr")
  if( !file.exists(file_name) ) {
    stop("This rdbs not supported")
  }
  sQuery <- paste(readLines(file_name), collapse = "\n")
  sQuery
}


#' Set table segment
#'
#' Change tables' segment name in a data model
#'
#' @param dm A data model object
#' @param table_segments A named list of vectors with segments as element names
#'   and tables as values in vectors
#' @export
dm_set_segment <- function(dm, table_segments) {

  if(!is.data_model(dm))
    stop("Not a data model object.")
  for(s in names(table_segments)) {
    table_names <- table_segments[[s]]
    dm$tables$segment[dm$tables$table %in% table_names ] <- s
  }
  dm
}

#' Set table display
#'
#' Change tables' display in a data model
#'
#' @param dm A data model object
#' @param display A named list of vectors with display as element names
#'   and tables as values in vectors
#' @export
dm_set_display <- function(dm, display) {

  if(!is.data_model(dm))
    stop("Not a data model object.")
  for(s in names(display)) {
    table_names <- display[[s]]
    dm$tables$display[dm$tables$table %in% table_names ] <- s
  }
  dm
}
