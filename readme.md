


<img width="30%" align="right" src="img/sample.png" />

# datamodelr
Data model diagrams with DiagrammeR


## 1. Define your data model in YAML

```yaml
# master data
Person:
  Person ID: {key: yes}
  Name:
  E-mail:
  Street:
  Street number:
  City:
  ZIP:

# Product setup
Item:
  Item ID: {key: yes}
  Item Name:
  Description:
  Category:
  Size:
  Color:
  
# transactions
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

```

## 2. Create data model object


```r
library(datamodelr)
file_path <- system.file("models/example.yml", package = "datamodelr")

dm <- dm_read_yaml(file_path)
```

## 3. Create DiagrammeR graph object


```r
graph <- dm_graph(dm, rankdir = "LR", use_ref_ports = TRUE)

DiagrammeR::render_graph(graph)
```

![](img/sample.png)
