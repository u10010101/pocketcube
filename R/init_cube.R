#' @title Initialize a pocket cube (cube of size 2x2x2)
#'
#' @description A string is used to represent the current state of the cube. It is not recommended for users to manually modify the string expect for special situations.
#'
#' @return A string representing the solved cube
#'
#' @examples
#' init_cube()
#'
#' @export
#' @seealso \code{\link{op}}


init_cube<-function(){
  x="12345670000000"
  return(x)
}
