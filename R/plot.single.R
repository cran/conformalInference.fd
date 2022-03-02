#' Plot Functional Split Conformal Confidence Bands
#'
#' The function plots the confidence bands provided by the \code{\link{conformal.fun.split}} #'function, \code{\link{conformal.fun.msplit}} and \code{\link{conformal.fun.jackplus}}.
#'
#' @param out The output of the split/msplit/jackknife+ function.
#' @return None
#' @details It exploits the package \code{\link[ggplot2]{ggplot}} and
#' \code{\link[gridExtra]{grid.arrange}}
#' to better visualize the results. It outputs n0=length(x0) plots.
#'
#' @details It plots, for each value in x0, the predicted functional value and bands in all the
#' dimensions of the multivariate functional response.
#'
#' @example inst/examples/ex.split.R
#' @export plot_fun

plot_fun=function(out){

  tn = out$t
  pred = out$pred
  lo=out$lo
  up=out$up

  n0=length(lo)
  p=length(lo[[1]])
  grid_size=vapply(lo[[1]],function(x) length(x),integer(1))
  acc_size=c(0,cumsum(grid_size))
  nCol <- floor(sqrt(p))
  g_list<-vector("list",n0*p)

  if(!is.null(pred)){

  for(jj in 1:n0){
      for(ii in 1:p){

        df=data.frame(td=as.numeric(tn[[ii]]),yg=as.numeric(pred[[jj]][[ii]]),
                      y_min=as.numeric(lo[[jj]][[ii]]), y_max=as.numeric(up[[jj]][[ii]]))

      g_list[[jj]][[ii]]<-ggplot2::ggplot()+ggplot2::geom_line(data=df,ggplot2::aes(x=td,y = yg),size=1) +
        ggplot2::geom_ribbon(data=df, ggplot2::aes(x=td,y = yg,ymin = y_min, ymax = y_max), fill = "red", alpha = 0.2)+
        ggplot2::labs(title = paste("Dimension ",as.character(ii), "Observation ", as.character(jj)),
             x = "t", y = "y")


    }

    do.call(gridExtra::"grid.arrange", c(g_list[[jj]], ncol=nCol))

  }
  }




  if(is.null(pred)){

    for(jj in 1:n0){
      for(ii in 1:p){

        df=data.frame(td=c(tn[[ii]]),
                      y_min=c(lo[[jj]][[ii]]), y_max=c(up[[jj]][[ii]]))

        g_list[[jj]][[ii]]<-ggplot2::ggplot()+
          ggplot2::geom_ribbon(data=df, ggplot2::aes(x=td,ymin = y_min, ymax = y_max), fill = "red", alpha = 0.2)+
          ggplot2::labs(title = paste("Dimension ",as.character(ii), "Observation ", as.character(jj)),
               x = "t", y = "y")


      }

      do.call(gridExtra::"grid.arrange", c(g_list[[jj]], ncol=nCol))

    }
  }


}

utils::globalVariables(c("y_max", "y_min", "yg"))

