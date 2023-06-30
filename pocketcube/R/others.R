uplot<-function(y){
  z=y
  z[2,4]=y[1,4]
  z[2,3]=y[2,4]
  z[1,3]=y[2,3]
  z[1,4]=y[1,3]
  z[3,1:2]=y[3,3:4]
  z[3,3:4]=y[3,5:6]
  z[3,5:6]=y[3,7:8]
  z[3,7:8]=y[3,1:2]
  return(z)
}
rplot<-function(y){
  z=y
  z[3,6]=y[3,5]
  z[3,5]=y[4,5]
  z[4,5]=y[4,6]
  z[4,6]=y[3,6]
  z[1:2,4]=y[3:4,4]
  z[3:4,4]=y[5:6,4]
  z[5:6,4]=y[c(4,3),7]
  z[c(4,3),7]=y[1:2,4]
  return(z)
}
fplot<-function(y){
  z=y
  z[4,4]=y[3,4]
  z[4,3]=y[4,4]
  z[3,3]=y[4,3]
  z[3,4]=y[3,3]
  z[3:4,5]=y[2,3:4]
  z[2,3:4]=y[c(4,3),2]
  z[c(4,3),2]=y[5,c(4,3)]
  z[5,c(4,3)]=y[3:4,5]
  return(z)
}
u<-function(x){
  x=as.character(x)
  y=paste0(
    substr(x,4,4),
    substr(x,1,1),
    substr(x,2,2),
    substr(x,3,3),
    substr(x,5,7),
    substr(x,11,11),substr(x,8,8),substr(x,9,9),
    substr(x,10,10),substr(x,12,14)
  )
  return(y)
}
v<-function(x){
  (u(u(as.character(x))))
}
w<-function(x){(u(u(u(as.character(x)))))}
r<-function(x){
  x=as.character(x)
  y=paste0(
    substr(x,1,2),
    substr(x,4,4),
    substr(x,7,7),
    substr(x,5,5),
    substr(x,3,3),
    substr(x,6,6),substr(x,8,9),
    (as.numeric(substr(x,11,11))+1)%%3,
    (as.numeric(substr(x,14,14))+2)%%3,
    substr(x,12,12),
    (as.numeric(substr(x,10,10))+2)%%3,
    (as.numeric(substr(x,13,13))+1)%%3
  )
  return(y)
}
s<-function(x){
  (r(r(as.character(x))))
}
t<-function(x){(r(r(r(as.character(x)))))}
f<-function(x){
  x=as.character(x)
  y=paste0(
    substr(x,5,5),
    substr(x,2,3),
    substr(x,1,1),
    substr(x,7,7),
    substr(x,6,6),
    substr(x,4,4),
    (as.numeric(substr(x,12,12))+2)%%3,
    substr(x,9,10),
    (as.numeric(substr(x,8,8))+1)%%3,
    (as.numeric(substr(x,14,14))+1)%%3,
    substr(x,13,13),
    (as.numeric(substr(x,11,11))+2)%%3
  )
  return(y)
}
g<-function(x){
  (f(f(as.character(x))))
}
h<-function(x){(f(f(f(as.character(x)))))}
change<-function(j){
  y=0
  if(j==1)y=3
  if(j==2)y=2
  if(j==3)y=1
  if(j==4)y=6
  if(j==5)y=5
  if(j==6)y=4
  if(j==7)y=9
  if(j==8)y=8
  if(j==9)y=7
  return(y)
}
digit2RUF<-function(x){
  x=as.character(x)
  x=gsub(" ", "", x)
  if(x=="0")return(" ")
  a=as.character(1:9)
  b=c("R","R2","R'","U","U2","U'","F","F2","F'")
  for(i in 1:9){
    x=gsub(a[i], b[i], x)
  }
  x=gsub(" ", "", x)
  return(x)
}
RUF2digit<-function(x){
  x=as.character(x)
  if(x==" ")return("0")
  x=gsub(" ", "", x)
  a=as.character(1:9)
  b=c("R","R2","R'","U","U2","U'","F","F2","F'")
  for(i in c(3,6,9,5,8,4,7,5,2,1)){
    x=gsub(b[i], a[i], x)
  }
  x=gsub(" ", "", x)
  return(x)
}

#' @importFrom graphics par plot
rcube_plot_cube<-function (x, ...) {
  projekt <- x$cube
  kolory <- x$scheme
  wym_w <- dim(projekt)[1]
  wym_s <- dim(projekt)[2]
  par(mar = c(0, 0, 0, 0))
  plot(1, type = "n", axes = F, xlab = "", ylab = "", xlim = c(0.15,
                                                               wym_s + 0.35), ylim = c(0, wym_w + 0.35), asp = 1)
  for (i in 1:(length(kolory))) {
    pkty <- as.data.frame(which(projekt == i, arr.ind = TRUE))
    pkty$row <- wym_w - pkty$row + 1
    if (nrow(pkty) > 0)
      with(pkty, {
        symbols(col, row, squares = rep(1, times = nrow(pkty)),
                inches = FALSE, bg = kolory[i], fg = "black",
                add = TRUE)
      })
  }
}
cube_solve_single<-function(x){
  x=as.character(x)
  ii=which(sysdata$first_seg==substr(x,1,7))
  jj=which(sysdata$second_seg==as.integer(substr(x,8,14)))
  if(length(ii)==0 || length(jj)==0)return("The cube is unsolvable!")
  kk=729*(ii-1)+jj
  kkk=which(sysdata$longind==kk)
  if(length(kkk)>0){
    return(NISS(sysdata$a2[kkk]))
  }else{
    ktm=sum(sysdata$longind<kk)
    return(NISS(sysdata$a1[kk-ktm]))
  }
}
niss_single<-function(x){
  x=as.character(x)
  if(grepl("[RUF]",x))x=RUF2digit(x)
  n=nchar(x)
  y=""
  for(i in 1:n){
    y=paste0(y,change(substr(x,n-i+1,n-i+1)))
  }
  y=digit2RUF(y)
  return(y)
}
