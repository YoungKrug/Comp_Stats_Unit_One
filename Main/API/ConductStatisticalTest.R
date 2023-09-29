
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
    conduct_oneway_anovas = function(restricted_to_keys = NA, save = FALSE)
    {
      dict_keys = self$dictonary_keys
      # if(restricted_to_keys != NULL)
      # {
      #   dict_keys = restricted_to_keys
      # }
      file_name = "Output_data/anova_data_all"
       if(save == TRUE)
      {
        sink(file_name)
      }
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
      unlink(file_name)
      self$list_of_anovas = list_of_anovas
      return(list_of_anovas)
    },
    conduct_one_way_anova_on = function(first = NA, other = NA, save = FALSE)
    {
      names = paste(first, other, sep = "_")
      file_name = paste("Output_data/anova_data", names, sep = "_")
       if(save == TRUE)
      {
        sink(file_name)
      }
       list_values = self$data_reader$list_of_column_data
       first_value = list_values[[first]]
       other_value = list_values[[other]]
       unlink(file_name)
       return(aov(first_value~other_value))
    },
    conduct_t_test_on_all = function(test_variation = NA, save = FALSE)
    {
      file_name = "Output_data/anova_data"
       if(save == TRUE)
      {
        sink(file_name)
      }
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
           print(newKey)
           print(t_val)
         }
      }
      self$list_of_t_test = list_of_t_test
      unlink(file_name)
      return(list_of_t_test)
    },
     conduct_t_test_on = function(first = NA, other = NA, t_test_type = NA, save = FALSE)
    {
      names = paste(first, other, sep = "_")
      file_name = paste("Output_data/anova_data", names, sep = "_")
      if(save == TRUE)
      {
        sink(file_name)
      }
       list_values = self$data_reader$list_of_column_data
       first_value = list_values[[first]]
       other_value = list_values[[other]]
      t_test = pairwise.t.test(first_value, other_value, p.adjust.method = t_test_type)

      unlink(file_name)
       return(t_test)
    },
      conduct_kruskal_wallis_test = function(first = NA, other = NA,  save = FALSE)
    {
      names = paste(first, other, sep = "_")
      file_name = paste("Output_data/kruskal_data", names, sep = "_")
      if(save == TRUE)
      {
        sink(file_name)
      }
       list_values = self$data_reader$list_of_column_data #make another function for all the
       first_value = list_values[[first]] # column data
       other_value = list_values[[other]]
      kruskal_test = kruskal.test(first_value~other_value)

      unlink(file_name)
       return(kruskal_test)
    },
    correlation_test = function(first = NA, other = NA, type_of_correlation = NA, save = NA)
    {
      names = paste(first, other, sep = "_")
      othername = paste("Output_data/", type_of_correlation)
      file_name = paste(othername, names, sep = "_")
      if(save == TRUE)
      {
        sink(file_name)
      }
       list_values = self$data_reader$list_of_column_data
       first_value = list_values[[first]]
       other_value = list_values[[other]]
       correlation = cor.test(first_value, other_value, method=type_of_correlation, exact = FALSE)
      unlink(file_name)
      return(correlation)
    },
    run_statistical_test = function(first = NA, other = NA, save = FALSE)
    {
      names = paste(first, other, sep = "_")
      othername = "Output_data/ks_test"
      file_name = paste(othername, names, sep = "_")
      if(save == TRUE)
      {
        sink(file_name)
      }
       list_values = self$data_reader$list_of_column_data
       first_value = list_values[[first]]
       other_value = list_values[[other]]
       ks_test = ks.test(first_value, other_value, method=type_of_correlation, exact = FALSE)
      unlink(file_name)
      return(ks_test)
    }
  )
)