#' @title Randomly generate a scrambled cube
#'
#' @description Randomly generate a scrambled cube
#'
#' @return A string representing the current state of the cube
#'
#' @examples
#' x=randcube()
#' #Find the corresponding scramble sequence
#' NISS(cube_solve(x))
#' plotcube(x,type="3D")
#'
#' @export
#' @seealso \code{\link{cube_solve}}  \code{\link{plotcube}} \code{\link{NISS}}

randcube<-function(){
  x=c(sample(7,7),sample(0:2,size=6,replace = T))
  x=c(x,(3-sum(x[8:13])%%3)%%3)
  return(paste0(x,collapse = ""))
}
