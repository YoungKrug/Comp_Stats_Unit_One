library("R6")
DataReader <- R6Class("DataReader",
  public = list(
    database_path = NULL,
    data = NULL,
    list_of_column_data = NULL,
    col_names = NULL,
    initialize = function(databasePath = NA)
    {
      self$database_path = databasePath
      self$data = read.table(databasePath,sep=",", header=TRUE)
      list_of_column_data = list()
      col_names = colnames(self$data)
      for(index in 1:length(col_names))
      {
        column = col_names[index]
        list_of_column_data[[column]] = self$data[,column]
      }
      self$list_of_column_data = list_of_column_data
      self$col_names = col_names
    },
    Get_Data_Frame = function(header)
    {
      values =  self$list_of_column_data[header]
      data_frame <- data.frame(
       data = values
      )
      return(data_frame)
    }
  )
)

# data <- DataReader$new("Comp_Stats_Unit_One/Main/Dataset.csv") #This is how you create objects
