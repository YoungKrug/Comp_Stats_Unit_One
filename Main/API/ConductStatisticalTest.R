
library("R6")
DataAnalyzer <- R6Class("DataAnalyzer",
public = list(
    data_reader = NULL,
    list_of_sd = NULL,
    list_of_mean = NULL,
    list_of_anovas = NULL,
    list_of_t_test = NULL,
    dictonary_keys = NULL,
    dict_sep = NULL,
    initialize = function(dataReader = NA)
    {
      list_of_mean = list()
      list_of_std = list()
      self$data_reader = dataReader
      dictonary_keys = names(dataReader$list_of_column_data)
      for(key in dictonary_keys)
      {
          list_of_mean[[key]] = mean(self$data_reader[[key]])
          list_of_std[[key]] = sd(self$data_reader[[key]])
      }
      self$dictonary_keys = dictonary_keys
      self$list_of_mean = list_of_mean
      self$list_of_sd = list_of_std
      self$dict_sep = "->"
    },
    conduct_oneway_anovas = function(restricted_to_keys = NA)
    {
      dict_keys = self$dictonary_keys
      # if(restricted_to_keys != NULL)
      # {
      #   dict_keys = restricted_to_keys
      # }
      list_of_values = self$data_reader$list_of_column_data
      list_of_anovas = list()
      for(xKey in dict_keys)
      {
         for(yKey in dict_keys)
         {
           if(xKey == yKey)
             next
           xValue = list_of_values[[xKey]]
           yValue = list_of_values[[yKey]]
           newKey = paste(xKey, yKey, sep = self$dict_sep)
           list_of_anovas[[newKey]] = aov(xValue~yValue)
           #print(list_of_anovas[[newKey]])
         }
      }
      self$list_of_anovas = list_of_anovas
    },
    conduct_t_test_on_all = function(test_variation = NA)
    {
      print(test_variation)
      dict_keys = self$dictonary_keys
      list_of_values = self$data_reader$list_of_column_data
      list_of_t_test = list()
      for(xKey in dict_keys)
      {
         for(yKey in dict_keys)
         {
           if(xKey == yKey)
             next
            xValue = list_of_values[[xKey]]
            yValue = list_of_values[[yKey]]
            newKey = paste(xKey, yKey, sep = self$dict_sep)
            t_val = pairwise.t.test(xValue, yValue, p.adjust.method = test_variation)
            list_of_t_test[[newKey]] = t_val
         }
      }
      self$list_of_t_test = list_of_t_test
    }
  )
)