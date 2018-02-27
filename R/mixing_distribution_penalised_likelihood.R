
PenalisedLikelihood <- function(mdObj, x){
  UseMethod("PenalisedLikelihood", mdObj)
}

PenalisedLikelihood.default <- function(mdObj, x){
  return(PriorDraw(mdObj, 1))
}
