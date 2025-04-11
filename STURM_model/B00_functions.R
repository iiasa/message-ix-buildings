
# Define a function to calculate the weighted median
weighted_median <- function(x, w) {
  x <- x[order(x)]
  cum_w <- cumsum(w[order(x)])
  median_idx <- which(cum_w >= sum(w) / 2)[1]
  return(x[median_idx])
}

## Function to manipulate subsets of data with dplyr
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

left_join <- function(...) {
      suppressMessages(dplyr::left_join(...))
}


# Function to select not matching items
`%nin%` = Negate(`%in%`) 

## Convert Long/Short format
fun_toLong <- function(DF, var_name){ ## var_name should be given as quoted
  if("data.frame"  %in% class(DF) == FALSE) stop(paste(deparse(substitute(DF)),'is not a data.frame'))
  #if(class(DF)!="data.frame") stop(paste(deparse(substitute(DF)),'is not a data.frame'))
  if(class(var_name)!="character" | length(var_name)!= 1) stop(paste(deparse(substitute(var_name)),'is not a character unit vector. Please, provide a name in quotes.'))
  DF_L <- gather(data=DF, key="year", value=!!var_name, matches("y\\d{4}")) %>%
    mutate(year=as.integer(substring(year,first=2)))
  output = DF_L
}


fun_toShort <- function(DF_L, var_name){ ## var_name should be given as quoted
  if("data.frame"  %in% class(DF_L) == FALSE) stop(paste(deparse(substitute(DF)),'is not a data.frame'))
  #if(class(DF_L)!="data.frame") stop(paste(deparse(substitute(DF_L)),'is not a data.frame'))
  if(class(var_name)!="character") stop(paste(deparse(substitute(var_name)),'is not a character vector'))
  DF <- DF_L %>% mutate(year = paste0("y", year)) %>% spread(year, !!var_name)
}


# # Function to rename data inputs - script F01_inputs.R
fun_rename <- function(DF,name){ ## DF = dataframe; name = name to relabel the "value" column
  if("data.frame"  %in% class(DF) == FALSE) stop(paste(deparse(substitute(DF)),'is not a data.frame'))
  if(length(grep("value", names(DF)))==0) stop(paste(deparse(substitute(DF)),'no columns named: value'))
  
  # DF_R <- DF %>% rename_with(~paste(name), .cols=value) # Works with only one "value" column per dataframe
  DF_R <- DF %>% rename_with(~gsub("value", name, names(DF)), .cols = everything()) # works also with multiple "value1", "value2", etc. columns
  
  output = DF_R
}

