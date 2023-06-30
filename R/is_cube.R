#' @title Determining if this string can represent a pocket cube
#'
#' @description Although it is not recommended for users to manually edit strings to represent the state of a cube, we provide a function to check if a string can represent a specific state.
#'
#' @param x A string
#'
#' @return Whether there is a state of the pocket cube that corresponds to \code{x}.
#'
#' @examples
#' is_cube(init_cube())
#'
#' @export
#' @seealso \code{\link{cube_solve}}

is_cube<-function(x){
  if (nchar(x) != 14) {
    return(FALSE)
  }
  front <- c()
  for(i in 1:7)front=c(front,substr(x,i,i))
  front=as.numeric(front)
  if (!all(sort(unique(front)) == 1:7)) {
    return(FALSE)
  }
  back <- c()
  for(i in 8:14)back=c(back,substr(x,i,i))
  back=as.numeric(back)
  if (!all(back %in% 0:2) || sum(back) %% 3 != 0) {
    return(FALSE)
  }
  return(TRUE)
}
