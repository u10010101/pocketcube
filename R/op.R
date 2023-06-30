#' @title Shuffle the pocket Cube in a specific order
#'
#' @description Starting from a given state, perform a series of moves in a specific order to shuffle the pocket cube
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
#' @param cube A string representing the current state of the pocket cube. For the solved state, the string returned by \code{\link{init_cube}} can be used.
#' @param seq A sequence for scrambling. See Details for more information
#'
#' @return A string representing the current state of the cube
#'
#' @examples
#' x=init_cube()
#' seq="R'FR2U'R2FR"
#' cube=op(x,seq)
#' plotcube(cube,type="3D")
#'
#' @export
#' @seealso \code{\link{cube_solve}}  \code{\link{plotcube}} \code{\link{NISS}}

op<-function(cube,seq){
  seq=as.character(seq)
  if(grepl("[RUF]",seq))seq=RUF2digit(seq)
  n=nchar(seq)
  for(i in 1:n){
    k=substr(seq,i,i)
    if(k=="1")cube=r(cube)
    if(k=="2")cube=s(cube)
    if(k=="3")cube=t(cube)
    if(k=="4")cube=u(cube)
    if(k=="5")cube=v(cube)
    if(k=="6")cube=w(cube)
    if(k=="7")cube=f(cube)
    if(k=="8")cube=g(cube)
    if(k=="9")cube=h(cube)
  }
  return(cube)
}
