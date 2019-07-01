#' Method of Counting Factor Levels in a Dataframe
#'
#' To check several variables in a dataframe, it is important to take grasp the density or table of each variable. This fct is for factor variable, return a dataframe showing factor level and count number of each variable.
#'
#' @param data a dataframe that you want to explore
#' @param level an integer which controls the number of factor level that you want to see. Too many factor levels can convolute the whole understanding.
#' @return A dataframe containing the name of factor levels and counts of each levels
#' @examples 
#'    lvcnt_ex <- LvCnt(iris, 2)
#'    View(lvcnt_ex)
#' @export

LvCnt <- function(data, level){
  # pretty simple and there are many parts that can be improved. For the purpose
  # of getting insight about data, please enjoy my LvCnt function
  
  #load package
  lapply(c('dplyr', 'data.table', 'reshape', 'rowr'), require, character.only=T)
  
  data <- data %>% as.data.frame()
  sf <- sapply(data, FUN = is.factor) %>% as.data.frame()
  ss <- sapply(sapply(data, levels), length) %>% as.data.frame()
  rn <- rownames(sf)[sf[,1] == 'TRUE' & ss[,1] < as.numeric(level) ]
  
  res <- c()
  for(i in seq_along(rn)){
    cb <- data[,rn[i]] %>% 
      table() %>% 
      as.data.frame() %>% 
      arrange(desc(.)) %>% 
      rename(c(.=rn[i]))
    res <- cbind.fill(res, cb, fill=NA)
  }
  res <- res[,-1]
  return(res)
}


