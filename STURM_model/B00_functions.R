
################
## FUNCTIONS  ##
################

## Function to manipulate subsets of data with dplyr
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

# Function to select not matching items
`%nin%` = Negate(`%in%`) 

# ## Convert DF to list
# # DF = dataframe to convert
# # row_names = names to assign to the vectors in the new list
# 
# 
# 
# fun_DFtoL <- function(DF,names_catg){
#   if(length(select(DF, matches("y\\d{4}")))==0) stop(paste('No valid column names in',deparse(substitute(DF))))
#   if(class(DF)!="data.frame") stop(paste(deparse(substitute(DF)),'is not a data.frame'))
#   L <- lapply(split(select(DF, matches("y\\d{4}")), as.numeric(row.names(DF))), unlist)
#   names(L) <- names_catg
#   output = L
# }
# 
# ## Alternative formulation (with external lapply)
# # fun_DB_to_L <- function(DF,row_names){
# #   L <- split(select(DF, matches("y\\d{4}")), as.numeric(row.names(DF)))
# #   names(L) <- row_names
# #   output = L
# # }
# 
# ## Convert list to DF
# # L = list to convert
# # catg = categories to use to split rows (data.frame or vector of characters)
# # yrs = years to use for the columns (vector of numerics)
# fun_LtoDF <- function(L, catg, yrs){
#   if(class(L)!="list") stop(paste(deparse(substitute(L)),'is not a list'))
#   DF <- as.data.frame(cbind(catg, do.call(rbind, L))) #convert list to dataframe
#   names(DF)[grep("\\d+", names(DF))] <- paste0("y", yrs) 
#   output = DF
# }


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

# ########################################
# ## Building stock evolution functions ##
# ########################################
# 
# # fun_D_dem_init <- function(D_dem, stock.list, p_dem_hist){mapply(
# #   function(D_dem, stock.list, p_dem_hist){
# #     D_dem[,1] = stock.list[1] *p_dem_hist
# #     output=D_dem}
# #   , D_dem, stock.list, p_dem_hist
# #   , SIMPLIFY=FALSE)}
# # 
# # fun_d_dem_init <- function(d_dem, D_dem){mapply(
# #   function(d_dem, D_dem){d_dem[1] <- D_dem[1,1]; output=d_dem}
# #          , d_dem, D_dem
# #          , SIMPLIFY=FALSE)}
# # 
# # fun_D_ren_init <- function(D_ren, stock.list, p_ren_hist){mapply(
# #   function(D_ren, stock.list, p_ren_hist){
# #     D_ren[,1] = stock.list[1] *p_ren_hist
# #     output=D_ren}
# #   , D_ren, stock.list, p_ren_hist
# #   , SIMPLIFY=FALSE)}
# # 
# # fun_d_ren_init <- function(d_ren, D_ren){mapply(
# #   function(d_ren, D_ren){d_ren[1] <- D_ren[1,1]; output=d_ren}
# #   , d_ren, D_ren
# #   , SIMPLIFY=FALSE)}
# # 
# # fun_d_new_init <- function(d_dem, d_new){mapply(
# #   function(d_dem, d_new){d_new[1] <- d_dem[1]; output=d_new}
# #                 , d_dem, d_new
# #                 , SIMPLIFY=FALSE)}
# # 
# # fun_d_new_init_updt <- function(d_new){mapply(
# #   function(d_new){if(d_new[1]<0){d_new[1] <- 0}; output=c(d_new)}
# #                 , d_new
# #                 , SIMPLIFY=FALSE)}
# # 
# # fun_d_empty_init <- function(d_empty, d_new){mapply(
# #   function(d_empty, d_new){if(d_new[1]<0){d_empty[1] <- -d_new[1]}; 
# #     output=c(d_empty)}
# #   , d_empty, d_new
# #   , SIMPLIFY=FALSE)}
# 
# fun_D_dem <- function(D_dem, d_dem, d_new, shr_dem_stp, i){mapply(
#   function(D_dem, d_dem, d_new, shr_dem_stp, i){
#     D_dem[i:nrow(D_dem), i] <-  d_new[i-1] * shr_dem_stp[1:c(nrow(D_dem)-i+1)] #future demolition of d_new[i]  
#     output = D_dem}
#   , D_dem, d_dem, d_new, shr_dem_stp, i
#   , SIMPLIFY=FALSE)}
# 
# fun_d_dem <- function(D_dem, d_dem, i){mapply(
#   function(D_dem, d_dem, i){
#     d_dem[i] <- sum(D_dem[i,1:ncol(D_dem)]) #read from matrix row i+1 the demolition for next iteration
#     output = d_dem}
#     , D_dem, d_dem, i
#   , SIMPLIFY=FALSE)}
# 
# # fun_D_ren <- function(D_ren, d_ren, d_new, shr_ren_stp, i){mapply(
# #   function(D_ren, d_ren, d_new, shr_ren_stp, i){
# #     D_ren[i:nrow(D_ren), i] <-  d_new[i-1] * shr_ren_stp[1:c(nrow(D_ren)-i+1)] #future renovation of d_new[i]  
# #     output = D_ren}
# #   , D_ren, d_ren, d_new, shr_ren_stp, i
# #   , SIMPLIFY=FALSE)}
# # 
# # fun_d_ren <- function(D_ren, d_ren, i){mapply(
# #   function(D_ren, d_ren, i){
# #     d_ren[i] <- sum(D_ren[i,1:ncol(D_ren)]) #read from matrix row i+1 the demolition for next iteration
# #     output = d_ren}
# #   , D_ren, d_ren, i
# #   , SIMPLIFY=FALSE)}
# 
# fun_stock_var <- function(stock.list, stock_var, i){mapply(
#   function(stock.list, stock_var, i){
#     stock_var[i] <- stock.list[i] - stock.list[i-1] #Stock change    
#     output = stock_var}
#     , stock.list, stock_var, i
#   , SIMPLIFY=FALSE)}
# 
# 
# fun_d_new <- function(d_new, d_dem, stock_var, i){mapply(
#   function(d_new, d_dem, stock_var, i){
#     if(stock_var[i] + d_dem[i]>=0) {d_new[i] <- stock_var[i] + d_dem[i] #balance equation
#      } else {d_new[i] <- 0}
#     output = d_new}
#   , d_new, d_dem, stock_var, i
#   , SIMPLIFY=FALSE)}
# 
# 
# fun_d_empty <- function(d_empty, d_dem, stock_var, i){mapply(function(d_empty, d_dem, stock_var, i){
#   if(stock_var[i] + d_dem[i]>=0){d_empty[i] <-0
#   }else{d_empty[i] <- -stock_var[i] -d_dem[i]} # buildings becoming empty with negative stock balance
#   output = d_empty}
#   , d_empty, d_dem, stock_var, i, SIMPLIFY=FALSE)}
# 
# 
# fun_stock_empty <- function(stock_empty, d_empty, i){mapply(
#   function(stock_empty, d_empty, i){
#   stock_empty[i] <- stock_empty[i-1] + d_empty[i] #Stock change
#   output = stock_empty}
#   , stock_empty, d_empty, i, SIMPLIFY=FALSE)}

################################################
## Construction/Renovation Decision Functions ##
################################################

## Life Cycle Cost function (bybuilding type, en. std., fuel and time)
fun_lcc <- function(c_inv, c_op, c_int, r, l) 
{
  lcc <- c_inv + c_op * ((1-(1+r)^(-l))/r) + c_int
  return(lcc)
}

## Market share function
## Formulation: Giraudet et al.
fun_lcc_exp <- function(lcc, nu)
{lcc_exp <- lcc^(-nu)
return(lcc_exp)}
