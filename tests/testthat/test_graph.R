
context("Graph")

test_that("Example - graph - example.yml", {

  file_path <- system.file("samples/example.yml", package = "datamodelr")
  dm <- dm_read_yaml(file_path)
  graph <- dm_create_graph(dm)
  expect_is(graph, "dgr_graph")
})
