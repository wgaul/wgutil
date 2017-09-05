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
gridref_to_hec <- function(GR = "NULL") {
  # convert a grid ref (letter form) of any precision to hectad (3 characters)
  # ARGS: GR = character string giving the grid reference (e.g. "B1641", "B161415)
  # VALUE: a character string giving the 3-character hectad grid ref (e.g. "B14")
  if(is.null(GR) || !is.character(GR)) {
    stop("Provide a character string to 'GR'.")
  }

  parts <- unlist(strsplit(GR, NULL))
  nums <- grep("[[:digit:]]", parts, value = T)
  letters <- paste0(grep("[[:alpha:]]", parts, value = T), collapse = "")
  n <- length(nums)
  
  if(nchar(GR)/2 == 0) {
    stop("Original grid ref should have an even number of characters after the letter(s).")
  }

  if(n > 2) {
    half <- n/2
    keep_n <- paste0(nums[c(1, half + 1)], collapse = "")
    hec <- paste0(letters, keep_n)
  }
  # return hectad code in the form of letter(s) followed by 1 digit each 
  # for eastings and northings
  hec 
}