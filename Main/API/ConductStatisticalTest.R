
DataAnalyzer <- R6Class("DataAnalyzer",
public = list(
    data_reader = NULL,
    initialize = function(dataReader = NA)
    {
      self$data_reader = dataReader
    }
  )
)