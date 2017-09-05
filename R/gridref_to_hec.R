###################################
## gridref_to_hec function deffinition
## 
## wgutil package
## Utility functions that I frequently use
##
## author: Willson Gaul (and others as indicated)
## created: 4 Sep 2017
## last modified: 5 Sep 2017
###################################

## Grid Ref to hectad
gridref_to_hec <- function(gr = NULL) {
  # convert a grid ref (letter form) of any precision to hectad (3 characters)
  # ARGS: gr = character string giving the grid reference (e.g. "B1641", "B161415)
  # VALUE: a character string giving the 3-character hectad grid ref (e.g. "B14")
  # 
  # TODO: eventually vectorize this using C++?
  #
  if(any(is.null(gr)) || any(!is.character(gr))) {
    stop("Provide a character string to 'gr'.")
  }
  
  result <- c()
  
  for(i in 1:length(gr)) { 
    parts <- unlist(strsplit(gr[i], NULL))
    nums <- grep("[[:digit:]]", parts, value = T)
    letters <- paste0(grep("[[:alpha:]]", parts, value = T), collapse = "")
    n <- length(nums)
    
    if(n %% 2 != 0) {
      stop("There must be an even number of digits after the letter(s) in the original grid reference.")
    }
    
    if(n == 2) {
      hec <- gr[i]
    }
    
    if(n > 2) {
      half <- n/2
      keep_n <- paste0(nums[c(1, half + 1)], collapse = "")
      hec <- paste0(letters, keep_n)
    } 
    
    result[i] <- hec
  }
  # return a vector of hectad codes in the form of letter(s) followed by 1 
  # digit each for eastings and northings
  result 
}