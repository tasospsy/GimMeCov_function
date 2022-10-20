sort_resid <- function(matrix, cutoff = 0.1) {
  matrix[upper.tri(matrix)] <- 0
  matrix <- as.data.frame(matrix)
  rows <- rownames(matrix)
  cols <- colnames(matrix)
  conds <- expand.grid(rows, cols)
  len <- nrow(conds)
  d <- list()
  for (i in 1:len) {
      x <- matrix[conds[i,1],conds[i,2]]
      rowx <- conds[i,1]
      colx <- conds[i,2]
      d[[i]]  <- data.frame('x' = rowx,
                        '.' = '~~',
                        'y' = colx,
                        'resid' = round(x,3))
      
    }
  d <- do.call(rbind,d)
  d <- d[order(d$resid, decreasing = TRUE),]
  d <- d[which(d$resid > cutoff),]
  d
}
