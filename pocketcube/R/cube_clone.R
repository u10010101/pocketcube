#' @title Finding a shortest sequence linking one state to another state
#'
#' @description The solution is the shortest and is written in standard rotation notation. The speed of finding the solution is quite fast.
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
#' @param from A string representing the start state
#' @param to A string representing the cloned state
#'
#' @return A string representing the path
#'
#' @examples
#' x=randcube()
#' y=randcube()
#' path=cube_clone(x,y)
#' yy=op(x,path)
#' path
#' y
#' yy
#'
#' @export
#' @seealso \code{\link{plotcube}} \code{\link{cube_solve}}

cube_clone<-function(from,to){
  if(!is_cube(from)){stop("Input is not a cube!")}
  if(!is_cube(to)){stop("Input is not a cube!")}
  x=init_cube()
  x=op(x,cube_solve(from))
  x=op(x,NISS(cube_solve(to)))
  return(NISS(cube_solve(x)))
}
