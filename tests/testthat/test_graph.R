
context("Graph")

test_that("Example - simple", {
  dm <- dm_read_yaml(text ="
    Person:
      Person ID {key: yes}
      Name:
      E-mail:
      Street:
      Street number:
      City:
      ZIP:

    Order:
      Order ID: {key: yes}
      Customer: {ref: Person}
      Sales person: {ref: Person}
      Order date:
      Requested ship date:
      Status:

    Order Line:
      Order ID: {key: yes, ref: Order}
      Line number: {key: yes}
      Order item: {ref: Item}
      Quantity:
      Price:

    Item:
      Item ID: {key: yes}
      Item Name:
      Description:
      Category:
      Size:
      Color:
    ")

  graph <- dm_create_graph(dm)
  expect_is(graph, "datamodelr_graph")
})

test_that("Example - graph - example.yml", {

  file_path <- system.file("samples/example.yml", package = "datamodelr")
  dm <- dm_read_yaml(file_path)
  graph <- dm_create_graph(dm)
  expect_is(graph, "datamodelr_graph")
})
