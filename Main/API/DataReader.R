library("R6")


DataReader <- R6Class("DataReader",
  public = list(
    database_path = NULL,
    data = NULL,
    list_of_column_data = NULL,
    col_names = NULL,
    initialize = function(databasePath = NA) {
      self$database_path = databasePath
      list_of_column_data = list()
      self$data = read.table("Dataset.csv",sep=",", header=TRUE)
      col_names = colnames(data)
      for(index in 1:length(col_names))
      {
        columnName = col_names[index]
        list_of_column_data[columnName] = data[,columnName]
      }
      self$col_names = col_names
      self$list_of_column_data = list_of_column_data
      print(list_of_column_data)
    }
  )
)

Data <- DataReader$new("Dataset.csv")
print(Data$list_of_column_data)