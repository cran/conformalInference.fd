#'@noRd

res.list=function(left,right){

  s=length(left)
  r=length(left[[1]])


  cond=(length(left)!=length(right))|| (length(left[[1]])!=length(right[[1]]))

  if(cond)
    stop("Different dimensions in computing absolute residuals")

  for(ii in 1:s){
    for(jj in 1:r){
      left[[ii]][[jj]]=abs(left[[ii]][[jj]]-right[[ii]][[jj]])
    }
  }

  return(left)

}

depth.max=function(inp){

  n0<-length(inp)


  out<-t(sapply(1:n0, function(i){
    mat<-list2matrix(inp[[i]])
    mat<-abs(scale(mat))
    dep<-1/apply(mat,1,max)
    return(dep)

  }))

  return(out)

}
