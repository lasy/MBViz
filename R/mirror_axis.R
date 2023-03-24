#' Mirror selected axes from an `ade4::mbpls` model
#'
#' Useful for comparing several models
#' @param res the output of `ade4::mbpls`
#' @param axis the axis that needs to be mirrored (*-1)
#'
#' @return a list similar to the output of `ade4::mbpls`
#' @export
mirror_axis <- function(res, axis = 1){
  res$lX[,axis] <- -res$lX[, axis]
  res$lY[,axis] <- -res$lY[, axis]
  res$Yc1[,axis] <- -res$Yc1[,axis]
  res$Yco[,axis] <- -res$Yco[,axis]
  res$faX[,axis] <- -res$faX[,axis]

  res
}
