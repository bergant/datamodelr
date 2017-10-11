


html_tag <- function(x, tag, ident = 0, nl = TRUE, atrs = NULL, collapse = "") {
  if(length(x) > 1 && !is.null(collapse)) {
    x <- paste(x, collapse = collapse)
  }
  space <- paste(rep("  ", ident), collapse = "")
  atrs <- paste(sprintf('%s="%s"', names(atrs), atrs), collapse = " ")
  if(nchar(atrs) > 0) atrs <- paste0(" ", atrs)
  htext <-
    if(nl) {
      sprintf("%s<%s%s>\n%s%s</%s>\n", space, tag, atrs, x, space, tag)
    } else {
      sprintf("%s<%s%s>%s</%s>\n", space, tag, atrs, x, tag)
    }
  paste(htext, collapse = "")
}

html_table <- function(x, ...) html_tag(x, tag = "TABLE", ident = 1, ...)
html_tr <- function(x, ...)    html_tag(x, tag = "TR", ident = 2, ...)
html_td <- function(x, ...)    html_tag(x, tag = "TD", ident = 3, nl = FALSE, ...)
html_font <- function(x, ...)    html_tag(x, tag = "FONT", ident = 0, nl = FALSE, ...)

#' Data frame to html table
#'
#' Used to create graphwiz dot HTML table labels
#'
#' @param x A data frame
#' @param attr_table A named list with table attributes
#' @param attr_header A named list with cell attributes in header cell
#' @param attr_td A function with parameters column, current row, value and
#'   returns a named list with current cell attributes
#' @param cols A named vector of columns to include in a table
#' @param trans Value transformation funcion
#' @keywords internal
#' @export
to_html_table <- function (x,
                           title = "Table",
                           attr_table,
                           attr_header,
                           attr_font,
                           attr_td = NULL,
                           trans = NULL,
                           cols = names(x)) {

  html_table(atrs = attr_table, c(
    # header
    html_tr(
      html_td(
        html_font(title, atrs = attr_font),
        atrs = attr_header,
        collapse = NULL
      )
    ),
    # rows
    sapply(seq_len(nrow(x)), function(r)
      html_tr(c(
        # cells
        sapply(cols, function(col_name) {
          value <- x[r, col_name]
          if(!is.null(trans)) value <- trans(col_name, x[r,], value)
          html_td(value, if(is.null(attr_td)) NULL else attr_td(col_name, x[r,], value))
        })
      ))
    )
  ))
}


#' Create a label
#'
#' Create a label for dot HTML shape
#'
#' @param x A data frame with column info
#' @param title A node title
#' @param palette_id Which color palette should be used (default)
#' @param col_attr Column atributes to display.
#'   Only column name (\code{column}) is included by default.
#' @keywords internal
#' @references See \url{http://www.graphviz.org/content/node-shapes}
#' @export
dot_html_label <- function(x, title, palette_id = "default", col_attr = c("column") ) {
  cols <- c("ref", col_attr)
  if(is.null(palette_id)) {
    palette_id <- "default"
  }

  border = ifelse(is.null(dm_color(palette_id, "line_color")), 0, 1)

  attr_table <- list(
    ALIGN="LEFT", BORDER=border, CELLBORDER=0, CELLSPACING=0
  )
  if(!is.null(dm_color(palette_id, "line_color"))) {
    attr_table[["COLOR"]] <- dm_color(palette_id, "line_color")
  }
  attr_header <- list(
    COLSPAN=length(cols), BGCOLOR=dm_color(palette_id, "header_bgcolor"), BORDER=0
  )
  attr_font <- list()
  attr_font <- list(COLOR = dm_color(palette_id, "header_font"))

  attr_td <- function(col_name, row_values, value) {
    list(ALIGN="LEFT", BGCOLOR = dm_color(palette_id, "bgcolor"))
  }

  # value presentation transformation
  trans <- function(col_name, row_values, value) {
    if(col_name == "ref") {
      value <- ifelse(is.na(value), "", "~")
    }
    if(col_name == "column" && row_values[["key"]]) {
      value <- sprintf("<U>%s</U>", value)
    }
    ifelse(is.na(value), "", value)
  }

  ret <- to_html_table(x, title = title,
                       attr_table = attr_table,
                       attr_header = attr_header,
                       attr_font = attr_font,
                       attr_td = attr_td,
                       cols = cols,
                       trans = trans)

  ret <- sprintf("<%s>", trimws(ret))
  ret
}


dm_create_graph_list <- function(dm, view_type = "all",
                                 focus = NULL, col_attr = "column") {

  if(!is.data_model(dm)) stop("Input must be a data model object.")

  # hidden tables

  if(!is.null(focus) && is.list(focus)) {
    if(!is.null(focus[["tables"]])) {
      dm$tables <- dm$tables[dm$tables$table %in% focus$tables,  ]
      dm$columns <- dm$columns[dm$columns$table %in% focus$tables,  ]
      if(is.null(focus[["external_ref"]]) || !focus[["external_ref"]]) {
        dm$references <- dm$references[
          dm$references$table %in% focus$tables &
            dm$references$ref %in% focus$tables, ]
      }
    }
  } else {
    # hide tables with display == "hide" attribute
    if(is.null(dm$tables$display)) dm$tables$display <- NA
    dm$tables$display[is.na(dm$tables$display)] <- "show"
    hidden_tables <- dm$tables[dm$tables$display == "hide", "table"]
    if(!is.null(hidden_tables)) {
      dm$tables <- dm$tables[!dm$tables$table %in% hidden_tables,  ]
      dm$columns <- dm$columns[!dm$columns$table %in% hidden_tables,  ]
      dm$references <- dm$references[
        !dm$references$table %in% hidden_tables &
          !dm$references$ref %in% hidden_tables, ]
    }
  }


  # remove hidden columns
  # dm$columns <-
  #  dm$columns[is.na(dm$columns$display) | dm$columns$display != "hide", ]

  tables <- split(dm$columns, dm$columns$table)

  switch( view_type,
          all = {},

          keys_only = {
            tables <- lapply(tables, function(tab)
              tab[tab[,"key"] == TRUE | !is.na(tab[,"ref"]), ])
          },

          title_only = {
            tables <- lapply(tables, function(tab)
              tab[0L,])
          })

  g_labels <-
    sapply(names(tables), function(x) {
      dot_html_label(
        tables[[x]],
        title = x,
        palette_id = dm$tables[dm$tables$table == x, "display"],
        col_attr = col_attr)
    })

  nodes <-
    data.frame(
      nodes = names(tables),
      label = g_labels,
      shape = "plaintext",
      type = "upper",
      segment = dm$tables[order(dm$tables$table), "segment"],

      stringsAsFactors = FALSE
    )


  if(!is.null(dm$references)) {
    edges <-
      with(dm$references[dm$references$ref_col_num == 1,],
           data.frame(from = table, to = ref, stringsAsFactors = FALSE))
  } else {
    edges <- NULL
  }

  ret <-
    list(nodes = nodes, edges = edges)

  ret
}

#' Create a graph object from data model object
#'
#' @param dm A data model object
#' @param rankdir Graph attribute for direction (eg. 'BT' = bottom --> top)
#' @param graph_name A graph name
#' @param graph_attrs Additional graph attributes
#' @param node_attrs Additional node attributes
#' @param edge_attrs Additional edge attributes
#' @param view_type Can be "all" (by default), "keys_only" or "title_only".
#'   It defines the level of details for the table rendering
#'   (all columns, only primary and foreign keys or no columns)
#' @param focus A list of parameters for rendering (table filter)
#' @param col_attr Column atributes to display.
#'   Only column name (\code{column}) is included by default.
#' @export
dm_create_graph <- function(dm, rankdir = "BT", graph_name = "Data Model",
                     graph_attrs = "",
                     node_attrs = "",
                     edge_attrs = "",
                     view_type = "all", focus = NULL,
                     col_attr = "column") {

  if(!is.data_model(dm)) stop("Input must be a data model object.")


  if(!all(col_attr %in% names(dm$columns) )) {
    stop("Not all col_attr in data model column attributes.")
  }
  g_list <-
    dm_create_graph_list(dm = dm, view_type = view_type,
                         focus = focus, col_attr = col_attr)
  if(length(g_list$nodes$nodes) == 0) {
    warning("The number of tables to render is 0.")
  }
  graph <-
    list(
      graph_attrs = sprintf('rankdir=%s tooltip="%s" %s', rankdir, graph_name, graph_attrs),
      node_attrs = sprintf('margin=0 fontcolor = "#444444" %s', node_attrs),
      nodes_df = g_list$nodes,
      edges_df = g_list$edges,
      edge_attrs = c('color = "#555555"',"arrowsize = 1", edge_attrs)
    )
  class(graph) <- c("datamodelr_graph", class(graph))

  # re-create dot code for data model
  # (DiagrammeR does not support yet the HTML labels and clusters (v.0.8))
  graph$dot_code <- dot_graph(graph)

  graph

}


#' Render graph
#'
#' Using DiagrammeR to render datamodelr graph object
#'
#' @param graph	a graph object
#' @param width	an optional parameter for specifying the width of the resulting
#' graphic in pixels.
#' @param height an optional parameter for specifying the height of the resulting graphic in pixels.
#' @export
dm_render_graph <- function (graph, width = NULL, height = NULL) {

  if( !requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("DiagrammeR package needed for this function to work. Please install it.",
         call. = FALSE)
  }


  if(is.null(graph$dot_code)) {
    graph$dot_code <- dot_graph(graph)
  }

  print(DiagrammeR::grViz(graph$dot_code, allow_subst = FALSE, width, height))
}


dot_graph <- function(graph) {

  graph_type <- "digraph"

  dot_attr <- paste0(
    sprintf("graph [%s]\n\n", paste(graph$graph_attrs, collapse = ", ")),
    sprintf("node [%s]\n\n", paste(graph$node_attrs, collapse = ", ")),
    sprintf("edge [%s]\n\n", paste(graph$edge_attrs, collapse = ", "))
  )
  segments <- unique(graph$nodes_df$segment)
  segments <- segments[!is.na(segments)]
  segments <- stats::setNames(1:(length(segments)), segments)

  dot_nodes <- sapply(seq_len(nrow(graph$nodes_df)), function(n) {
    node <- graph$nodes_df[n,]
    dot_node <- sprintf("  '%s' [label = %s, shape = '%s'] \n", node$nodes, node$label, node$shape)
    if(!is.na(node[["segment"]])) {
      dot_node <- sprintf("subgraph cluster_%s {\nlabel='%s'\ncolor=\"#DDDDDD\"\n%s\n}\n",
                          segments[node[["segment"]]],
                          node[["segment"]],
                          dot_node
                          )

    }
    dot_node
  })

  dot_seg_nodes <- paste(dot_nodes, collapse = "\n")
  dot_edges <-
    paste(
      sprintf("'%s'->'%s'", graph$edges_df$from, graph$edges_df$to),
      collapse = "\n"
    )
  ret <- sprintf("#data_model\n%s {\n%s\n%s\n%s\n}",
                 graph_type,
                 dot_attr,
                 dot_seg_nodes,
                 dot_edges)
  ret
}

#' Datamodel color schema
#'
#' Manage color schema for data model diagrams
#'
#' @param line_color Rectangle color
#' @param header_bgcolor Table header background color
#' @param header_font Table header font color
#' @param bgcolor Table background color
#' @export
#' @examples
#' col_scheme <-
#'   dm_color_scheme(
#'     dm_palette(
#'       line_color = "#787878",
#'       header_bgcolor = "#A5A5A5",
#'       header_font = "#FFFFFF",
#'       bgcolor = "#E4E4E4"
#'     ),
#'     dm_palette(
#'       line_color = "#41719C",
#'       header_bgcolor = "#5B9BD5",
#'       header_font = "#FFFFFF",
#'       bgcolor = "#D6E1F1"
#'     ),
#'     dm_palette(
#'       line_color = "#BC8C00",
#'       header_bgcolor = "#FFC000",
#'       header_font = "#FFFFFF",
#'       bgcolor = "#FFEAD0"
#'     )
#'   )
dm_palette <- function(line_color = NULL, header_bgcolor, header_font, bgcolor) {
  list(
    line_color = line_color,
    header_bgcolor = header_bgcolor,
    header_font = header_font,
    bgcolor = bgcolor
  )
}

#' @param ... Palettes for color schema
#' @export
#' @rdname dm_palette
#' @keywords internal
dm_color_scheme <- function(...) {
  list(...)
}


#' @param color_scheme New colors created with dm_color_scheme
#' @export
#' @rdname dm_palette
dm_add_colors <- function(color_scheme)
{
  old_cs <- dm_get_color_scheme()
  if(any(names(color_scheme) %in% names(old_cs))) {
    old_cs[names(color_scheme)] <- NULL
  }
  dm_set_color_scheme(c(old_cs, color_scheme))
}

#' @export
#' @rdname dm_palette
dm_get_color_scheme <- function() {
  getOption("datamodelr.scheme")
}

#' @export
#' @rdname dm_palette
dm_set_color_scheme <- function(color_scheme) {
  options(datamodelr.scheme = color_scheme)
}

dm_color <- function(palette_id, what) {
  color_scheme <- dm_get_color_scheme()
  if(is.null(color_scheme[[palette_id]])) {
    palette_id <- "default"
  }
  color_scheme[[palette_id]][[what]]
}


.onLoad <- function(libname, pkgname) {

  # initialize default color scheme
  dm_set_color_scheme(

    dm_color_scheme(
      default = dm_palette(
        line_color = "#555555",
        header_bgcolor = "#EFEBDD",
        header_font = "#000000",
        bgcolor = "#FFFFFF"
      ),
      accent1nb = dm_palette(
        header_bgcolor = "#5B9BD5",
        header_font = "#FFFFFF",
        bgcolor = "#D6E1F1"
      ),
      accent2nb = dm_palette(
        header_bgcolor = "#ED7D31",
        header_font = "#FFFFFF",
        bgcolor = "#F9DBD2"
      ),
      accent3nb = dm_palette(
        header_bgcolor = "#FFC000",
        header_font = "#FFFFFF",
        bgcolor = "#FFEAD0"
      ),
      accent4nb = dm_palette(
        header_bgcolor = "#70AD47",
        header_font = "#FFFFFF",
        bgcolor = "#D9E6D4"
      ),
      accent5nb = dm_palette(
        header_bgcolor = "#4472C4",
        header_font = "#FFFFFF",
        bgcolor = "#D4D9EC"
      ),
      accent6nb = dm_palette(
        header_bgcolor = "#A5A5A5",
        header_font = "#FFFFFF",
        bgcolor = "#E4E4E4"
      ),
      accent7nb = dm_palette(
        header_bgcolor = "#787878",
        header_font = "#FFFFFF",
        bgcolor = "#D8D8D8"
      ),
      accent1 = dm_palette(
        line_color = "#41719C",
        header_bgcolor = "#5B9BD5",
        header_font = "#FFFFFF",
        bgcolor = "#D6E1F1"
      ),
      accent2 = dm_palette(
        line_color = "#AE5A21",
        header_bgcolor = "#ED7D31",
        header_font = "#FFFFFF",
        bgcolor = "#F9DBD2"
      ),
      accent3 = dm_palette(
        line_color = "#BC8C00",
        header_bgcolor = "#FFC000",
        header_font = "#FFFFFF",
        bgcolor = "#FFEAD0"
      ),
      accent4 = dm_palette(
        line_color = "#507E32",
        header_bgcolor = "#70AD47",
        header_font = "#FFFFFF",
        bgcolor = "#D9E6D4"
      ),
      accent5 = dm_palette(
        line_color = "#2F528F",
        header_bgcolor = "#4472C4",
        header_font = "#FFFFFF",
        bgcolor = "#D4D9EC"
      ),
      accent6 = dm_palette(
        line_color = "#787878",
        header_bgcolor = "#A5A5A5",
        header_font = "#FFFFFF",
        bgcolor = "#E4E4E4"
      ),
      accent7 = dm_palette(
        line_color = "#000000",
        header_bgcolor = "#787878",
        header_font = "#FFFFFF",
        bgcolor = "#D8D8D8"
      )
    )
  )
}



#' Get graph SVG
#'
#' Convert diagram graph object to SVG format
#'
#' @param graph a graph object
#' @return character in SVG format
#' @export
dm_get_graph_svg <- function(graph) {

  if (!inherits(graph, "datamodelr_graph"))
    "graph must be a datamodelr graph object"

  if (!requireNamespace("V8"))
    stop("V8 is required to export.", call. = FALSE)
  stopifnot(utils::packageVersion("V8") >= "0.10")

  gv <- dm_render_graph(graph)
  ct <- V8::new_context("window")
  invisible(ct$source(system.file("htmlwidgets/lib/viz/viz.js",
                                  package = "DiagrammeR")))
  invisible(
    ct$call("Viz", gv$x$diagram, "svg", gv$x$config$engine, gv$x$config$options)
  )

}


#' Export graph to file
#'
#' Export data model graph object object to PNG, PDF, PS or SVG file.
#'
#' @param graph a graph object
#' @param file_name file name
#' @param file_type file type (if not provided, file name extension is used)
#' @param width width
#' @param height height
#' @export
dm_export_graph <- function(graph, file_name = NULL, file_type = NULL, width = NULL, height = NULL) {

  if(is.null(file_name)) {
    file_name <- format(Sys.time(), "dm_%Y%m%d_%H%M%S")
  }
  if(is.null(file_type) && grepl("\\.", file_name)) {
    file_type <- gsub(".*\\.([A-Za-z])", "\\1", file_name)
  }

  if(is.null(file_type)) {
    stop("File type not defined")
  }

  if (!("rsvg" %in% rownames(utils::installed.packages()))) {
    stop("To use this function to produce an image file, please install the `rsvg` package.")
  }

  render_functions <- list(
    png = rsvg::rsvg_png,
    pdf = rsvg::rsvg_pdf,
    svg = rsvg::rsvg_svg,
    ps = rsvg::rsvg_ps
  )


  if(!tolower(file_type) %in% names(render_functions) ) {
    stop("File type can be only pdf, png or ps.")
  }

  render_function <- render_functions[[tolower(file_type)]]

  render_function(
    charToRaw(
      dm_get_graph_svg(graph)
    ),
    file = file_name,
    width = width,
    height = height
  )

}

#' Print data model graph
#'
#' @param x data model object
#' @param ... parameter passed to \link{dm_render_graph}
#' @export
print.datamodelr_graph <- function(x, ...) {
  dm_render_graph(x, ...)
}
