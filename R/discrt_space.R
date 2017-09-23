# Discrete grid

discrete.function=function(nbpoints=50,map){
  xmin=floor(10*min(map$F1))/10
  xmax=ceiling(10*max(map$F1))/10
  ymin=floor(2*min(map$F2))/2
  ymax=ceiling(2*max(map$F2))/2
  f1.array=seq(xmin,xmax,length.out=nbpoints)
  f2.array=seq(ymin,ymax,length.out=nbpoints)
  discretspace=expand.grid(F1=f1.array,F2=f2.array)
  return(discretspace)
}


