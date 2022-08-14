# Import Traces -----------------------------------------------------------

batch_import_files <- function(location) {
  # Imports and appends like excel files of data. Handles four different datasets
  #
  # Args:
  #   location: The path to the file that contains the data to be imported. The 
  #             data should be the only items in the file
  #
  # Returns datasets:
  #   df_trace: a dataframe containing all of the trace data in the directory
  
  files <- dir(location)
  
  df_trace <- data.frame()
  
  for (i in files) {
    if (grepl("trace", i, ignore.case = T)){
      tmp_trace <- 
        read_excel(paste("Data/", i, sep = ""), skip = 1) %>%
        select(-`Source State`, -starts_with("total")) %>%
        mutate(Year = as.numeric(str_extract_all(i, "[:digit:]{4}"))) %>%
        rename(SourceState = ...2) %>% 
        filter(row_number() %in% 1:55) %>%
        gather(key = RecoveryState, value = Guns, ALABAMA:WYOMING) %>%
        filter(SourceState != "TOTAL", SourceState != "TOTALS")
      df_trace <- bind_rows(df_trace, tmp_trace)
    }
  }
  df_trace
  
}

#Call Function to generate dataset

traces <- batch_import_files("Data/")
