


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
                           attr_td = NULL,
                           trans = NULL,
                           cols = names(x)) {

  html_table(atrs = attr_table, c(
    # header
    html_tr(html_td(title, atrs = attr_header, collapse = NULL)),
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
#' @param title_bgcolor Title background color
#' @keywords internal
#' @references See \url{http://www.graphviz.org/content/node-shapes}
#' @export
dot_html_label <- function(x, title, title_bgcolor = "#EFEBDD") {
  cols <- c("ref", "column")

  attr_table <- list(
    ALIGN="LEFT", BORDER=1, CELLBORDER=0, CELLSPACING=0
  )
  attr_header <- list(
    COLSPAN=length(cols), BGCOLOR=title_bgcolor, BORDER=0
  )

  attr_td <- function(col_name, row_values, value) {
    list(ALIGN="LEFT")
  }

  # value presentation transformation
  trans <- function(col_name, row_values, value) {
    if(col_name == "ref") {
      value <- ifelse(is.na(value), "", "~")
    }
    if(col_name == "column" && row_values[,"key"]) {
      value <- sprintf("<U>%s</U>", value)
    }
    ifelse(is.na(value), "", value)
  }

  ret <- to_html_table(x, title = title,
                       attr_table = attr_table,
                       attr_header = attr_header,
                       attr_td = attr_td,
                       cols = cols,
                       trans = trans)

  ret <- sprintf("<%s>", trimws(ret))
  ret
}


dm_create_graph_list <- function(dm, view_type = "all",
                                 title_bgcolor = "#CCCCCC",
                                 focus = NULL) {

  if(!is.data_model(dm)) stop("Input must be a data model object.")
  if(!is.null(focus) && is.list(focus)) {
    if(!is.null(focus[["tables"]])) {
      dm$tables <- dm$tables[dm$tables$name %in% focus$tables,  ]
      dm$columns <- dm$columns[dm$columns$table %in% focus$tables,  ]
      if(is.null(focus[["external_ref"]]) || !focus[["external_ref"]]) {
        dm$references <- dm$references[
          dm$references$table %in% focus$tables &
            dm$references$ref %in% focus$tables, ]
      }
    }
  }

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
        title_bgcolor = title_bgcolor)
    })

  nodes <-
    list(
      nodes = names(tables),
      label = g_labels,
      shape = "plaintext",
      type = "upper",
      segment = sapply(tables, function(x) x$segment[1])
    )


  if(!is.null(dm$references)) {
    edges <-
      with(dm$references[dm$references$ref_col_num == 1,],
           DiagrammeR::create_edges(from = table, to = ref))
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
#' @param title_bgcolor The table title background color
#' @param view_type Can be "all" (by default), "keys_only" or "title_only".
#'   It defines the level of details for the table rendering
#'   (all columns, only primary and foreign keys or no columns)
#' @param focus A list of parameters for rendering (table filter)
#' @export
dm_create_graph <- function(dm, rankdir = "BT", graph_name = "Data Model",
                     graph_attrs = "", title_bgcolor = "#EFEBDD",
                     view_type = "all", focus = NULL) {

  if(!is.data_model(dm)) stop("Input must be a data model object.")

  if( !requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("DiagrammeR package needed for this function to work. Please install it.",
         call. = FALSE)
  }

  g_list <-
    dm_create_graph_list(dm, view_type, title_bgcolor, focus)


  graph <-
    DiagrammeR::create_graph(
      graph_attrs = sprintf('rankdir=%s tooltip="%s" %s', rankdir, graph_name, graph_attrs),
      node_attrs = 'margin=0 color="gray" fontcolor = "#444444"',
      nodes_df = do.call(DiagrammeR::create_nodes, g_list$nodes),
      edges_df = do.call(DiagrammeR::create_edges, g_list$edges),
      edge_attrs = c('color = "#555555"',"arrowsize = 1")
    )

  # re-create dot code for data model
  # (DiagrammeR does not support yet the HTML labels and clusters (v.0.8))
  graph$dot_code <- dot_graph(graph)
  sessionInfo()
  graph

}


#' Render graph
#'
#' A wrapper around DiagrammeR::render_graph
#'
#' @param graph	a \pkg{DiagrammeR} dgr_graph object
#' @param  output	string specifying the output type; graph (the default) renders
#'   the graph using the grViz function,
#'   vivagraph renders the graph using the vivagraph function,
#'   visNetwork renders the graph using the visnetwork function,
#'   DOT outputs DOT code for the graph, and SVG provides SVG code for the rendered graph.
#' @param layout	a string specifying a layout type for a vivagraph rendering of the graph,
#'   either forceDirected or constant.
#' @param width	an optional parameter for specifying the width of the resulting
#' graphic in pixels.
#' @param height an optional parameter for specifying the height of the resulting graphic in pixels.
#' @export
dm_render_graph <- function (graph, output = "graph", layout = NULL, width = NULL,
                       height = NULL) {

  if(substring(graph$dot_code, 1, 11) != "#data_model") {
    graph$dot_code <- dot_graph(graph)
  }

  DiagrammeR::render_graph(graph, output, layout, width, height)
}


dot_graph <- function(graph) {

  graph_type <- ifelse(graph$directed, "digraph", "graph")

  dot_attr <- paste0(
    sprintf("graph [%s]\n\n", paste(graph$graph_attrs, collapse = ", ")),
    sprintf("node [%s]\n\n", paste(graph$node_attrs, collapse = ", ")),
    sprintf("edge [%s]\n\n", paste(graph$edge_attrs, collapse = ", "))
  )
  segments <- unique(graph$nodes_df$segment)
  segments <- segments[!is.na(segments)]
  segments <- setNames(1:(length(segments)), segments)

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
