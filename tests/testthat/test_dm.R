
context("Yaml")

test_that("Simple yaml", {

  dm <- dm_read_yaml(text = "
    foo:
    ")
  expect_is(dm, "data_model")
})

test_that("Simple yaml 2", {

  dm <- dm_read_yaml(text = "
    foo:
      bar:
    ")

  expect_is(dm, "data_model")

})

test_that("Simple yaml 3", {

  dm <- dm_read_yaml(text =
    "
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

  expect_is(dm, "data_model")

})

test_that("Example yaml", {

  file_path <- system.file("samples/example.yml", package = "datamodelr")
  dm <- dm_read_yaml(file_path)

  expect_is(dm, "data_model")
  expect_gt(nrow(dm$columns), 5)
  expect_gt(nrow(dm$references), 1)
})



