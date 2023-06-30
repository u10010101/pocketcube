#' @title Find the inverse scramble
#'
#' @description Reverse the order of the scramble and invert each of the individual turns. For instance, \code{NISS("RUF")="F'U'R'"}.
#'
#' @param scramble A sequence for scrambling. See Details for more information
#'
#' @return The inverse scramble
#'
#' @details The most popular and standard rotation notation is used. Turns for a pocket Cube are represented by the following moves: R, R2, R', U, U2, U', F, F2, F'.
#' These moves correspond to the following actions:
#'
#' R: clockwise rotation of the right face
#'
#' R2: double (180 degrees) clockwise rotation of the right face
#'
#' R': counterclockwise rotation of the right face
#'
#' U: clockwise rotation of the upper face
#'
#' U2: double (180 degrees) clockwise rotation of the upper face
#'
#' U': counterclockwise rotation of the upper face
#'
#' F: clockwise rotation of the front face
#'
#' F2: double (180 degrees) clockwise rotation of the front face
#'
#' F': counterclockwise rotation of the front face
#'
#' When multiple moves are performed in succession, they are executed from left to right.
#'
#' @examples
#' x=randcube()
#' #Find the solution
#' sol=cube_solve(x)
#' #Find the scramble sequence
#' NISS(sol)
#'
#' @export
#' @seealso \code{\link{cube_solve}}  \code{\link{op}}

NISS<-function(scramble){
  scramble=as.character(scramble)
  n=length(scramble)
  a=c()
  for(j in 1:n){
    a=c(a,niss_single(scramble[j]))
  }
  return(a)
}
