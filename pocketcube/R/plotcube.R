#' @title Plot the cube in 2D or 3D
#'
#' @description This function refers to rcube:::plot.cube and \code{rcube::plot3dFlat} to plot the cube.

#' @param x A string representing the current state of the pocket cube. For the solved state, the string returned by \code{\link{init_cube}} can be used.
#' @param type Whether the plot is 2D or 3D. Default is \code{"2D"}.
#'
#' @importFrom rcube plot3dFlat
#'
#' @examples
#' x=init_cube()
#' plotcube(x)
#' y=randcube()
#' plotcube(y,type="3D")
#'
#' @export
#'
#' @seealso \code{\link{cube_solve}}  \code{\link{randcube}}

plotcube<-function(x,type="2D"){
  stopifnot(type %in% c("2D","3D"))
  yy=matrix(rep(0,48),ncol=8)
  yy[1:2,3:4]=2
  yy[3:4,1:2]=5
  yy[3:4,3:4]=1
  yy[3:4,5:6]=3
  yy[3:4,7:8]=6
  yy[5:6,3:4]=4
  scheme = c("green","white","red","yellow","orange","blue")
  y=RUF2digit(NISS(cube_solve(x)))
  n=nchar(y)
  if(y=="0"){
    res=list()
    res$cube=yy
    res$scheme=scheme
  }else{
    for(i in 1:n){
      kk=as.numeric(substr(y,i,i))
      if(kk %in% 1:3){
        for(j in 1:kk){yy=rplot(yy)}
      }else if(kk %in% 4:6)
        for(j in 1:(kk-3)){yy=uplot(yy)
        }else if(kk %in% 7:9){
          for(j in 1:(kk-6)){yy=fplot(yy)
          }
        }
    }
    res=list()
    res$cube=yy
    res$scheme=scheme
  }
  if(type=="2D"){
    rcube_plot_cube(res)
  }else{
    res$size=2
    rcube::plot3dFlat(res)
  }
}
