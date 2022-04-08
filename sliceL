## --------------------------------------------
## Function to subset a nested list into chunks
## --------------------------------------------
sliceL <- function(List, chunks) {
  Chunks <- list()
  for (i in 1:chunks) {
    Chunks[[i]] <-
      List[seq(1, length(List), by = length(List) / chunks)[i]:seq(length(List) /
                                                                     chunks, length(List), by = length(List) / chunks)[i]]
  }
  return(tibble(Chunks))
}
