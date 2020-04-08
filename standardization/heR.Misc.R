#--------------------------------------------------------------------------------------------------
# Title:
#
# Created by:
#
# Modified by: 
#   Adam Garbo, March 15, 2019
#
# Project:
#
# Description: 
#   
# Required file(s): 
#
# Notes: 
#   
#--------------------------------------------------------------------------------------------------

a2df<-
function (a, names=c(paste("Var.",1:length(dim(a)),sep=""),"Response"),
          sel=list(), values=list())

{

# `a2df' : R Function to convert a multi-way array
# to a data frame with one variable for each dimension
# of the array.   The values in `values' are substituted
# for the indices if provided, otherwise a series of 
# integer factors make up the data for each variable.

# `a' : a multi-way array
# `names' : a character vector of names for the variables
#           in each dimension
# `sel' : selected indices for each dimension (variable)
# `values' : a list containing vectors of actual values for
#            each variable (dimension) of the array. Should
#            contain the values for ALL the indices, selected
#            or not.

a <- as.array(a)
d <- dim(a)
n <- length(d)
if (!length(sel)) for (i in 1:n) sel[[i]] <- 1:d[i]
if (!length(values)) for (i in 1:n) values[[i]] <- 1:d[i]
l <- list()
for (i in 1:n) l[[i]] <- sel[[i]]
f <- expand.grid(l)
idx <- as.array(as.matrix(expand.grid(l)))
for (i in 1:n) f[,i] <- values[[i]][f[,i]]
df<-data.frame(f,a[idx])    
if (!is.null(names)) names(df) <- names
df
}
add.lp.bin<-
function (h, l, type = "b") {

# R functions to add binned data (i.e., a histogram)
# to a log-probability plot
# Like bin2lnorm, omits cdf=1 point

if (!is.vector(h) | !is.vector(l))
  stop("`l' and `'h' must be numeric vectors.")
l <- as.numeric(l)
h <- as.numeric(h)
if (length(h) != length(l)-1)
   stop("Number of bins not equal to number of limits minus 1.") 
if (any(diff(l) <= 0))
   stop("Limits must be strictly increasing.")
if (any(l < 0))
   stop("Each limit must be zero or greater.")

cdf <- cumsum(h)/sum(h)
q<-qnorm(cdf)
q<-q[1:(length(q)-1)]
lr <- l[2:(length(l)-1)]

points(q, lr, type=type)

}

add.lp.data<-
function (y, type="b", include.zeroes=TRUE, 
          verbose=TRUE, na.rm=FALSE, ...) {

# This R functions adds continuous numeric data to a log-probability plot
# x axis:  normal probabilities
# y axis:  sample quantiles

# Note:  Ties in the data, e.g., if there is discretization, 
# will produce vertical lines.  Gaps in the data, i.e., intervals
# without any data, will produce horizontal lines.

# UPDATE:  Added a `verbose' option, which is on by default...
#              and return error if missing values...

# UPDATE:  New option include.zeroes, allows for adjusting the
#          probabilities by the number of values less than or
#          equal to zero, by default, or just ignoring them, i.e., 
#          removing them completely as if they never existed.
#                             --NK  8-May-04

# UPDATE:  Coerce y to a vector of numeric type. 
#          First convert to matrix in case someone is passing
#          a dataframe with one row or column.  -NK 13-Mar-04

# Now automatically rejects values <= 0
# Fixed n after NA removal. added more graphic options  20-May-02

if (0 == (n <- length(y))) 
   stop("Sample vector is empty.") 
if (any(is.na(y)))   
  if (na.rm)
    y <- y[!is.na(y)]
  else
    stop("`y' contains missing values.")
if (!is.vector(y) & verbose)
  warning("`y' is a non-vector, but has been coerced to such.")
y <- as.numeric(as.matrix(y))
#0 -> y[y<=0]
y <- y[order(y)]
nremove <- sum(y <= 0)
if (verbose & nremove)
  if (include.zeroes)
    warning(paste(nremove,"elements <=0 retained in probabilities."))
  else    
    warning(paste(nremove,"elements <=0 removed from `y'."))
if (include.zeroes) {
  n <- length(y)
  x <- qnorm(ppoints(n))[order(order(y))]
  x <- x[y > 0]
  y <- y[y > 0]
} else { 
  y <- y[y > 0]
  n <- length(y)
  x <- qnorm(ppoints(n))[order(order(y))]
}  
points(x, y, type=type, ...)

}
add.lp.lnorm<-
function (gm=1, gsd=exp(1), lty=1, lwd=1, col="black") {

# adds a plot of a lognormal distribution (i.e., a line) 
# to a log-probability plot, given a geometric
# mean and geometric standard deviation
# X axis : normal probabilities
# Y axis : data (model) quantiles

slope<-qnorm(0.8413)/log10(gsd)
intercept<--qnorm(0.8413)*log10(gm)/log10(gsd)
x1<-par("usr")[3]
x2<-par("usr")[4]
y1<-x1*slope + intercept
y2<-x2*slope + intercept

lines(c(y1,y2),10^c(x1,x2), lty=lty, col=col, lwd=lwd)
}
annotate <-
function(labels, X, Y=NULL, x=NULL, y=NULL,
         adj=c(0,0), arrow.pos=1, col="black", cex=1,
	 border=TRUE, border.col="black", border.lwd=1, fill=NA,
	 offset=c(0,0),  arrow.lwd=1,
	 arrow.angle=30, arrow.length=0.1, arrow.col="black")
{

# Annotate plots easily with a horizontal
# bit of text (a phrase) with optional filled rectangular box, and an
# optional arrow to a real data point (or area) on the plot, extending
# from the center of the text phrase.

# adj = which point to use in positioning the label, defaults to left/bottom;
# arrow.pos = the side from which the arrow tail emmanates, defaults to
#             bottom (1), numbers go around clockwise: 1-4

# X,Y = position of the label and the tail of the arrow
# x,y = position of the arrowhead; if NULL, then no arrows are drawn

# Either of the X/x can contain a list with x/X y/Y components, in which
#   case the y/Y arguments are ignored.

# This function is a different approach than for strip text, which
#  corresponds single vertical character space to a given x-value
#  on the plot. 

# ----------------------------------------------------------

arrow.pos <- rep(arrow.pos, length=length(labels))
col <- rep(col, length=length(labels))
cex <- rep(cex, length=length(labels))
border <- rep(border, length=length(labels))
border.col <- rep(border.col, length=length(labels))
border.lwd <- rep(border.lwd, length=length(labels))
fill <- rep(fill, length=length(labels))
arrow.lwd <- rep(arrow.lwd, length=length(labels))
arrow.angle <- rep(arrow.angle, length=length(labels))
arrow.length <- rep(arrow.length, length=length(labels))
arrow.col <- rep(arrow.col, length=length(labels))

if (is.list(X)) {
  Y <- X$y
  X <- X$x
}  

if (!is.null(x) & is.list(x)) {
  y <- x$y
  x <- x$x
}

if (is.null(Y)) 
  stop("`Y' should contain the label and arrow y coordinates, respectively, unless they are given as part of `x' or `X' lists.")

heights <- c()
widths <- c()
for (i in 1:length(labels)) {
   heights[i] <- strheight(labels[i], cex=cex[i])
   widths[i] <- strwidth(labels[i], cex=cex[i])
}   
char.width <- widths/nchar(labels)
char.height <- heights
X <- rep(X, length=length(labels))
Y <- rep(Y, length=length(labels))

Xf <- X - adj[1]*widths
Yf <- Y - adj[2]*heights

xleft <- Xf - char.width - offset[1]
xright <- Xf + widths + char.width + offset[1]
ytop <- Yf + 3*char.height/2 + offset[2]
ybottom <- Yf - char.height/2 - offset[2]

width <- xright - xleft
height <- ytop - ybottom

if (!is.null(x) & !is.null(y)) {
  x <- rep(x, length=length(labels))
  y <- rep(y, length=length(labels))
  xtail <- c()
  ytail <- c()
  for (i in 1:length(labels)) {
    if (arrow.pos[i] == 1) {
      xtail[i] <- xleft[i] + width[i]/2
      ytail[i] <- ybottom[i]
    }
    if (arrow.pos[i] == 2) {
      xtail[i] <- xleft[i]
      ytail[i] <- ybottom[i] + height[i]/2
    }
    if (arrow.pos[i] == 3) {
      xtail[i] <- xleft[i] + width[i]/2
      ytail[i] <- ytop[i]
    }
    if (arrow.pos[i] == 4) {
      xtail[i] <- xright[i]
      ytail[i] <- ybottom[i] + height[i]/2
    }
  }
  arrows(xtail, ytail, x, y, angle=arrow.angle,
         lwd=arrow.lwd, col=arrow.col, length=arrow.length) 
}    

for (i in 1:length(labels))
   if (border[i]) 
     rect(xleft=xleft, ybottom=ybottom,
          xright=xright, ytop=ytop,
          col=fill, border=border.col, lwd=border.lwd)
       
text(x=Xf, y=Yf, labels=labels, cex=cex, col=col, adj=c(0,0))

}
areaplot <- 
function (x, y, col="gray", new=FALSE, base=0, 
          density=NULL, angle=NULL, axes=TRUE, format="%H", ...) 
{

# A function to fill in the area under the curve described by 
# a set of x-y points with a specified color. Normally used
# on an existing plot.  Only tested with linear axes.

# Fixed:  Bug that drew funny lines underneath the plot.  NK 25-Oct-2005

#   UPDATE.  Added format argument if x is in time format....

# Updated:  Now we allow for shading with lines of particular
#             density and angle.  NK 6-May-04

# Updated:  Now we add points with baseline (defaulting to zero)
#           y values in beginning and to ensure that the area
#           is draw to the, usually zero, baseline.  We draw to the
#           minimum y point or base, whichever is lower.  8-June-03.

# Updated.  If y is all NAs, then give warning and return NULL.

# Updated.  To deal with NA's in y, we plot multiple polygons for
#    each set of contiguous "non-NA" points.   We cannot deal with
#    NAs in x.

# Any space "under" straight lines connecting the provided points
#    will be filled in.  With widely spaced points, the
#    result may not be what is expected, with large "blocks"
#    of area filled in that does not appear directly under the
#    specfied points.   Use spline() or some other function to
#    pre-smooth the data.

if (all(is.na(y))) {
  warning("`y' contains nothing but missing data, nothing plotted.")
  return()
}
if (par()$xlog | par()$ylog)
  warning("Not intended for use with logarithmic axes.")

if (any(is.na(x)))
  stop("`x' cannot contain any missing values.")
if (length(x) < 2 | length(y) < 2 | length(x) != length(y))
  stop("`x' and `y' should contain coordinates for at least two points.")

if (min(y,na.rm=TRUE) < base) base <- min(y,na.rm=TRUE)

if (new) {
  plot.new()
  plot.window(xlim=range(x, na.rm=TRUE), ylim=range(c(base,y), na.rm=TRUE,
              ...))
  if (axes) {
    box()
    axis(1, format=format)
    axis(2)
  }
}
o <- order(x)
x <- x[o]
y <- y[o]

#x <- c(x, rev(x))
#y <- c(y, rep(base,length=length(y)))  

f <- goodgroups(y)
gx <- split(x, f)
gy <- split(y, f)
for (i in 1:length(gy)) {
   idx <- complete.cases(gx[[i]], gy[[i]])
   x <- gx[[i]][idx]
   y <- gy[[i]][idx]
   x <- c(x, rev(x))
   y <- c(y, rep(base,length=length(y)))  
#   polygon(c(x,rev(x)),c(rep(par()$yaxp[1], length(x)),rev(y)),
#           col=col, angle=angle, density=density, ...)
    polygon(x, y, col=col, angle=angle, density=density)
}
}

areaplot.segmented<-
function(x, y, f, col=c("gray", "black"), new=TRUE, base=0, 
         density=NULL, angle=NULL, axes=TRUE, legend=TRUE, 
         cex.leg=1.2, ...)
{
# Function to plot a series of areaplots, each with a different color and
#   shading, based on an index variable f.

if (length(f) != length(x) || length(f) != length(y))
  stop("`f' index variable length must match that of x and y.")
base <- min(base, min(y))

o <- order(x)
x <- x[o]
y <- y[o]

xlim <- range(x, na.rm=TRUE)
ylim <- range(c(base, y), na.rm=TRUE)

f <- factor(f)
x <- split(x, f=f)
y <- split(y, f=f)

# check that at least 2 points are available for each segment
#     and give warning and skip them if there are not
for (i in names(x))
   if (length(x[[i]]) < 2) {
     warning("Segment `",i,"' has less than 2 points.  Removed from plot.")
     x[[i]] <- NULL
     y[[i]] <- NULL
   }



col <- rep(col, length=length(levels(f)))
if (!is.null(density))
  density <- rep(density, length=length(levels(f)))
if (!is.null(angle))
  angle <- rep(angle, length=length(levels(f)))

if (new) {
  plot.new()
  if (legend)
    par(oma=c(0,0,0,25), mfrow=c(1,1))
  plot.window(xlim = xlim, ylim=ylim)
  if (axes) {
    box()
    axis(1)
    axis(2)
  }
}

if (length(x) == 0 || length(y) == 0) {
  warning("No valid segments to plot.")
  return(0)
}

areaplot(x[[1]], y[[1]], col=col[1], base=base, density=density[1], 
         angle=angle[1], new=FALSE, ...)

if (length(x) > 1)
  for (i in 2:length(x)) 
     areaplot(x[[i]], y[[i]], col=col[i], base=base, density=density[i],
         angle=angle[i], new=FALSE, ...)
 
if (legend) {
  par(xpd=NA)
  coor <- par()$usr   # x1, x2, y1, y2
  #print(coor)
#  legend(locator(1) , legend=levels(f), 
  legend(coor[2]+0.0*coor[2], coor[4] , legend=levels(f), 
           fill=col, cex=cex.leg, 
           bty="o", y.intersp=1.5, xjust=0, yjust=1)
}

}
arrayplot<-
function (a, i, k,
          r, ival, kval, sel.i, sel.k,
          r.integ=NULL, integ=FALSE, FUN=sum,
          col, lty, lwd, pch, axes=TRUE,
          legend=FALSE, man.leg=FALSE, ncol=1, idx.leg=TRUE,
          main.leg=paste("Key Variable, k=",k,sep=""),
          xlim=NULL, ylim=NULL, type="l", log="",
          pretty=TRUE, grid=FALSE, 
          main=NULL, xlab=NULL, ylab=NULL, new=TRUE,
          dup.top=FALSE, persp=FALSE, theta=55, phi=27, p.col=7,
          p.box=TRUE, expand=0.5, shade=0.50, border=NA,
          ltheta=-135, lphi=0, level=FALSE, image=FALSE, nlevels=25)
{
##############################################################
# `arrayplot' : An R function to plot the values
#  of an array `a' against an independent variable with
#  dimensional index `i' for values of a third (key)
#  variable with dimensional index `k'.  The indices
#  of the remaining variables take on the indices
#  given in `r' (conditioning variables).

#  If `integ' = TRUE then the array values are *integrated*
#  over the remaining variables' indices given in the list 
#  `r.integ' instead of conditioned on the *single* indices
#  for each remaining variables given in `r'.  Instead of
#  integration (summing) another function can be specified.

#  If `persp' = TRUE, then a perspective (3D) plot will
#  be produced, or if `image' = TRUE then a 3D R image
#  (or level) plot will be produced, otherwise a 2D figures
#  is produced that has series corresponding to every index
#  in array dimension `k'.

#  The array values will be plotted in "index order".

#  `a' is a multi-way array
#  `i' is the dimension index of the independent variable
#  `ival' is a vector containing all the actual values of `i'
#  `k' is the dimension index of the key variable
#  'kval' is a vector containing all the actual values of `k'
#  `sel.k' is a vector giving the indices of `k' that will
#          be plotted
#  `sel.i' is a vector of the indices of `i' that will
#          be plotted
#  `r' is a vector containing the indices for the remaining
#      variables in `a'. Note: By default they are set to 1.
#  `r.integ' is a list giving the indices for each of the remaining
#            variables over which `a' will be integrated, iff
#            `integ' = TRUE. If duplicate indices are given
#            they will be used to weight the corresponding
#            array values by the number of duplicates.  If `integ'
#            = FALSE, then the plots will be conditioned on the
#            single values for the remaining variables given in `r'.
#  `FUN' is the function applied to the selected 
#        remaining variable values.  Default is `sum'.
#  `col' gives the colors that correspond to each index
#        of the key variable `k' in `sel.k'.
#  `lty', `lwd', `pch' are plotting line style, width, and
#                      symbol style.
#   Note: If too few values are given for colors and plotting
#         symbols, they are recycled.
#  `main', `ylab', and `xlab' are the titles and axis labels
#                             for the plot.
#   If `new' = TRUE then a plot.new() function call is issued
#              before drawing the graph.
#   `dup.top' is this funny little option that duplicates the
#             top array value for the `i' indices.  The function
#             then expects an additional value for `ival' too,
#             if the largest `sel.i' index is the maximum index.
#             If the largest `sel.i' index is not the maximum
#             possible, then the ival is that corresponding to the
#             next highest index.  A warning is issued if `ival'
#             is smaller than the number of `i' indices plus 1,
#             and an NA is introduced if there is no ival
#             available.  Note: This `dup.top' feature is used
#             to include the absolute maximum value on aerosol
#             size distribution plots.
#  `persp' = TRUE to draw a perspective (3D) plot (instead of a
#            2D plot with multiple series for the `k' variable
#            values)
#  `image' = TRUE to draw a "3D" R image plot (or "level" plot)
#            with the array values designated by differently
#            colored blocks.
###############################################################


########## Check Input and Set Defaults ############

d <- dim(a)
if (!(k %in% 1:length(d)))
  stop("`k' must be a dimension index of `a'.")
if (!(i %in% 1:length(d)))
  stop("`i' must be a dimension index of `i'.")
k<-as.integer(k)
i<-as.integer(i)
if (length(k)>1 | length(i)>1)
   stop("`i' and `k' must be the indices of the indep and key variables, respectively.")

# ----------------
if (missing(ival)) ival <- 1:dim(a)[i]
if (missing(kval)) kval <- 1:dim(a)[k]
nk <- d[k]
ni <- d[i] 
if (length(kval) < nk)
   stop("`kval' must contain values of `k' corresponding to all indices in `k'.")
if (length(ival) < ni & !dup.top)
   warning("`ival' should contain values of `i' corresponding to all indices in `i'.")
else
  if (length(ival) < ni+1 & dup.top)
     warning("With `dup.top' option `ival' should contain values corresponding to `i' indices plus one.")

# ----------------
if (missing(sel.i)) sel.i <- 1:dim(a)[i]
if (missing(sel.k)) sel.k <- 1:dim(a)[k]
sel.k<-as.integer(sel.k)
sel.i<-as.integer(sel.i)
#if (dup.top) sel.i <- c(sel.i,max(sel.i)+1)  #add index 1 gt than current max
if (!all(sel.k %in% 1:nk))
   stop("`sel.k' must contain indices of `k'.")
if (!all(sel.i %in% 1:ni))
   stop("`sel.i' must contain indices of `i'.")

# ----------------
if (missing(r)) r <- rep(1,length(dim(a))-2)
r<-as.integer(r)
if (length(r) != length(dim(a))-2)
   stop("`r' must specify indices for remaining variables.")
for (ii in 1:length(dim(a)[-c(i,k)]))
    if (!(r[ii] %in% 1:dim(a)[-c(i,k)][ii]))
       stop("`r' must contain a vector of dimension indices of remaining `a' variables.")

# ----------------
if (integ) {
   if (is.null(r.integ)) {
      r.integ <- list()
      for (ii in 1:length(d[-c(i,k)]))
          r.integ[[ii]] <- 1:d[-c(i,k)][ii]
   }
   r.integ <- as.list(r.integ)
   if (length(r.integ) != length(d[-c(i,k)]))
      stop("`r.integ' must be a list with length equal to the number of remaining variables")
   for (ii in 1:length(d[-c(i,k)]))
       if (!all(r.integ[[ii]] %in% 1:d[-c(i,k)][ii]))
          stop("`r.integ' elements must contain vectors containing indices for the remaining variables over which to integrate; duplicate indices imply correspondinly-weighted array values.")
}

# ----------------
if (missing(col)) col <- rainbow(length(sel.k))
if (length(col) == 1)
   col <- rep(col,length(sel.k))
if (length(col) != length(sel.k))
  stop("`col' must contain a vector of colors for the `sel.k' indices of `k'.")

if (missing(lty)) lty <- rep(1,length(sel.k))
if (length(lty) == 1)
   lty <- rep(lty,length(sel.k))
if (length(lty) != length(sel.k))
  stop("`lty' must contain a vector of line types for the `sel.k' indices of `k'.")

# ------------------
if (missing(lwd)) lwd <- rep(3,length(sel.k))
if (length(lwd) == 1)
   lwd <- rep(lwd,length(sel.k))
if (length(lwd) != length(sel.k))
  stop("`lwd' must contain a vector of line widths for the `sel.k' indices of `k'.")

# -----------------
if (missing(pch)) pch <- 1:length(sel.k)
if (length(pch) == 1)
   pch <- rep(pch,length(sel.k))
if (length(pch) != length(sel.k))
  stop("`pch' must contain a vector of symbol types for the `sel.k' indices of `k'.")

# ------------------
if (is.null(main))
   if (integ)
      main <- paste("Integrated Array Plot:   ",deparse(substitute(a)),"  ~  i[",min(sel.i),",",max(sel.i),"] * k[",min(sel.k),",",max(sel.k),"]  | ... ",sep="")
   else
      main <- paste("Array Plot:   ",deparse(substitute(a)),"  ~   i[",min(sel.i),",",max(sel.i),"] * k[",min(sel.k),",",max(sel.k),"]  |  r={",paste(r,collapse=","),"}",sep="")
if (is.null(xlab))
   xlab <- paste("Independent Variable, i=",i,sep="")
if (is.null(ylab))
   ylab <- paste(deparse(substitute(a))," Array Values")


############  Now Plot It  ###############

if (new) plot.new()

# Set array index
l <- as.list(rep(0,length(d)))
if (integ) 
   l[-c(i,k)] <- r.integ   #r.integ must be a list
else
   l[-c(i,k)] <- as.list(r)
l[[i]] <- sel.i
l[[k]] <- sel.k
idx <- as.array(as.matrix(expand.grid(l)))

# Set plot features
if (is.null(ylim)) 
   if (integ) {
      ylim <- range(tapply(a[idx],list(idx[,i],idx[,k]),FUN))
   }
   else
      ylim <- range(a[idx])
if (is.null(xlim)) 
   if (dup.top) 
      xlim <- range(ival[c(sel.i,max(sel.i)+1)])
   else
      xlim <- range(ival[sel.i])

if (!persp & !level & !image) {
   plot.window(xlim=xlim,ylim=ylim,log=log)
   title(main=main,ylab=ylab,xlab=xlab)
   for (ii in 1:length(sel.k)) {
       l[[k]] <- sel.k[ii]
       idx <- as.array(as.matrix(expand.grid(l)))
       o <- ival[sel.i]
       if (integ)
          p <- as.vector(tapply(a[idx],list(idx[,i]),FUN))
       else
          p <- as.vector(a[idx])
       if (dup.top) {
          o <- c(o,ival[max(sel.i)+1])
          p <- c(p,p[length(sel.i)])
       }
       points(o,p,type=type,col=col[ii],lwd=lwd[ii],lty=lty[ii],pch=pch[ii])
   }
} else {
   if (integ)
      p <- as.matrix(tapply(a[idx],list(idx[,i],idx[,k]),FUN))
   else if (i > k) p <- t(matrix(a[idx],nrow=length(sel.k)))
   else p <- matrix(a[idx],nrow=length(sel.i))
   if (persp)
      persp(x=ival[sel.i],y=kval[sel.k],z=p,theta=theta,phi=phi,
         expand=expand,shade=shade,border=border,ltheta=ltheta,
         lphi=lphi,col=p.col,zlab=deparse(substitute(a)),
         xlab="Independent Variable", ylab="Key Variable",
         main=main, box=p.box)
   else if (level)
      filled.contour(x=ival[sel.i],y=kval[sel.k],
            z=p,xlab="Independent Variable",nlevels=nlevels,
            color.palette=terrain.colors,
            ylab="Key Variable",main=main,axes=axes)
   else if (image)
      image(x=ival[sel.i],y=kval[sel.k],
            z=p,xlab="Independent Variable",
            ylab="Key Variable",main=main,col=terrain.colors(nlevels),
            log=log,axes=axes)
}

if (axes & !persp & !level & !image) {
   if (pretty) {
      if (grid) abline(v=pretty(ival[sel.i]),lty=3,lwd=1,col="gray")
      axis(1,pretty(ival[sel.i]))
   } else {
      if (grid) abline(v=ival[sel.i],lty=3,lwd=1,col="gray")
      axis(1,signif(ival[sel.i],digits=3))
      if (dup.top) axis(1,signif(ival[max(sel.i)+1],digits=3))
   }
   axis(2)
   box()
}

if (legend & !persp & !level & !image) {   # Won't draw a legend for 3D plots
    if (idx.leg)
       leg<-paste(signif(kval[sel.k],digits=3)," [",sel.k,"]",sep="")
    else
       leg<-paste(signif(kval[sel.k],digits=3),sep="")
    if (man.leg) {
       pos <- locator(1)
       if (any(type == c("s","S","l","h","c")))
          legend(pos, leg=leg, lty=lty, lwd=lwd, col=col, ncol=ncol)
       else
          legend(pos, leg=leg, col=col, pch=pch,ncol=ncol)
    } else {
       pos <- list(x=xlim[1],y=ylim[2]-0.01*abs(diff(ylim)))
       if (any(type == c("s","S","l","h","c")))
          legend(pos,leg=leg, lty=lty, lwd=lwd, col=col, ncol=ncol)
       else
          legend(pos,leg=leg, col=col, pch=pch,ncol=ncol)
    }
    text(pos,label=main.leg,adj=c(0,-0.2))
}


}
avertime<-
function (x, n=10, FUN=var, return.raw=FALSE)
{
# `avertime' : R function to calculate and plot the `f' statistic of a
# time series for increasing averaging times -- starting with the
# original time series (with a given number of observations per
# unit time, i.e., its frequency) and ending with `n' observations
# grouped, i.e., the number of grouped observations ranges from
# 1 to n.

# Update:  Now has option of returning the raw grouped data in the form
#     of a dataframe with one column for time series values and one
#     column for a grouping factor; the grouping factor has numeric levels equal
#     to the number of original observations averaged together to
#     make each group, i.e., the averaging time in raw units equal to the
#     interval between each original observation.
#     Another column gives the number of calculated averages that comprise each
#     grouping level.  NK 12-Feb-2004

# Important: This function assumes a regularly-spaced time series.

# Also:  The aggregate function only returns results for complete
#       groups of a given length (i.e., between 1 and n elements long)

# Updated:  Remove plotting stuff.  Plot the result with separate
#           routines.

x <- ts(x)

if (return.raw)
  raw <- list(averages=NULL, avertime=NULL, N=NULL)
else {
  FUN <- match.fun(FUN)
  v <- vector(mode="numeric", length=n)
  vn <- vector(mode="numeric", length=n)

  # Let's use these as the averaging times, since the `aggregate' function
  # uses them to split the data.  The ones I would use would be just 1:n
  # but let's just go along.
  times <- tsp(x)[3]%/%(1/(1:n)) 
  
  #cat("The Averaging Times (Multiples of Original Interval) Are: \n")
  #print(times)

  nn <- length(x)
  v0 <- var(x)  # This is the original variance of the time series
}

for (i in 1:n) {
    m <- aggregate(x, ndeltat=i, FUN=mean, ts.eps=0.1)
    if (return.raw) {
      raw$averages <- c(raw$averages, m) 
      raw$avertime <- c(raw$avertime, rep(i, length(m)))
      raw$N <- c(raw$N, rep(length(m), length(m)))
    } else {
      v[i] <- FUN(m)
      vn[i] <- v0/i  # the theoretical variance assuming independence
                   # i is the number of groups (after split)
    }		   
}


# times is the averaging time, which is the number of groups (i.e., 1:n) if 
# the initial frequency is 1 observation per unit time. 

if (return.raw) 
  data.frame(raw)
else  
  data.frame(avertime=times, N=(nn%/%(1:n)), stat=v, var.N=vn)

}
barchart2<-
function (x, data = parent.frame(), panel = "panel.superbar", 
          prepanel = "prepanel.superbar", strip = TRUE, box.ratio = 2,
	  groups = NULL, beside = FALSE, horizontal = NULL, subset = TRUE,
	  subscripts = !is.null(groups), ...) 
{

# This is a version (replacement?) of barchart that deals with grouped data
#   by plotting stacked or side-by-side bars, just like the barplot function
#   in base R graphics.  The only visible difference in usage from the
#   original lattice barchart function is the `beside' argument. Internally,
#   new panel and prepanel functions (panel.superbar and prepanel.superbar)
#   are used to plot the bars in each panel and specify the default
#   x and y limits for each panel.

#  Update. Something weird with bwplot with factors, so I'm using xyplot and
#   dealing with factors ourselves in panel.superbar.  no, we need bwplot
#   to pass factors to panel.superbar.  Deepayan sent me an updated
#   bwplot, that I am now using and calling bwplot2.  March 3, 2003

#  UPDATE:  Using bwplot in Deepayan's updated lattice package now. 4-Mar-04

#  UPDATE:   Needed to change formula argument to "x" argument.  26-Mar-08

require(lattice)
require(grid)

dots <- list(...)
groups <- eval(substitute(groups), data, parent.frame())
subset <- eval(substitute(subset), data, parent.frame())
if (!is.function(panel)) 
  panel <- eval(panel)
if (!is.function(strip)) 
  strip <- eval(strip)
prepanel <- if (is.function(prepanel)) 
  prepanel
else if (is.character(prepanel)) 
  get(prepanel)
else eval(prepanel)
do.call("bwplot", c(list(x = x, data = data, 
        horizontal = horizontal, beside = beside, groups = groups,
	subscripts = subscripts, 
	subset = subset, panel = panel, prepanel = prepanel,
	strip = strip, box.ratio = box.ratio), dots))
}
bin2lnorm<-
function (l, h, wt, plot=FALSE, density=FALSE, mcol="red", dcol="green",
          lty="dashed", lwd=3, shade.density=15, shade.angle=35,
          rect.border.col=NULL, rect.lwd=1, rect.lty=1, cex=1.4, main,
          xlab, ylab, ...) {

# This function takes binned data (i.e., a histogram) and
# fits a lognormal model to it, using weighted least-squares,
# and optionally plotting the fit and the data together as a CDF.
# If density = TRUE then it will plot a histogram with
# the fitted lognormal function superimposed, otherwise it plots
# a cdf (log-probability plot) with the fitted line through the 
# data points.

# ToDO:  Add possibility for normalization by log of limits
# See the dX.dlogDp functions.....

# Now allows for weighted least-squares, and outputs the results
# of `lsfit()', i.e., the residuals and such

# Now also outputs the normalization constant so that the
# un-normalized pdf can be reconstructed

# l is a vector containing the bin limits, h is the data
# in each bin, e.g., the counts

# Note:  We omit the probability=1 point(s) on the CDF.

# This is kinda like the 'method of quantiles' for 
# estimating the parameters of a lognormal distributtion,
# except we use all except the top point. See
# Ott, 1995, page 268, "Environmental Statistics and Data
# Analysis".

# -> Modified to coerce input into numeric format

h<-as.numeric(h)
l<-as.numeric(l)

if (!is.vector(h) || !is.vector(l)) 
   stop("Count and bin specs must be numeric vectors (or coercable).")
if (length(h) != length(l)-1)
   stop("Number of bins not equal to number of limits minus 1.") 
if (any(diff(l) <= 0))
   stop("Limits must be strictly increasing.")
if (any(l < 0))
   stop("Each limit must be zero or greater.")

# Set weights=1 if missing 
if (missing(wt)) wt <- rep(1,length(h))
if (!missing(wt) & length(wt) != length(h))
   stop("`wt', if specified, must contain weights corresponding to each bin with length the same as that of `h'")

n<-length(l)

pdf <- h/(diff(l)*sum(h))
norm <- sum(h)
cdf <- cumsum(h)/sum(h)

# omit where cdf=1 (could be multiple instances due to 0 counts)
# also omit where cdf=0 (zero counts in beginning)
lr <- l[-1]   #cdf is taken at right bin limits
lr <- lr[cdf<1 & cdf > 0]
wt <- wt[cdf<1 & cdf > 0]
cdf <- cdf[cdf<1 & cdf > 0]  # do cdf last

q<-qnorm(cdf)
ll <- log(lr)

ls.out<-lsfit(ll, q, wt=wt) 
intercept<-ls.out$coefficients[1]
slope<-ls.out$coefficients[2]
names(intercept)<-NULL
names(slope)<-NULL

gm <- exp((qnorm(0.5)-intercept)/slope)
gsd <- exp((qnorm(0.8413)-intercept)/slope)/gm

if (plot) { 
   if (missing(main))
      main <- paste("Bin2LN(GM=", format(gm,digits=3), ",GSD=", format(gsd,digits=3),",Norm=",format(norm,digits=3),")")
#   Some weirdness with resetting par on exit made the axes strange:
#        1:10 for both
#   old.par<-par(no.readonly=TRUE)
#   on.exit(par(old.par))
   if (!density) {
      par(cex=cex, las=1)
      if (missing(xlab)) xlab <- "ln(Bin Limits)"
      if (missing(ylab)) ylab <- "Standard Normal Quantiles"
      plot(ll, q, type = "p", main=main, ylab=ylab, xlab=xlab, col=dcol, pch=16,axes=F, ...) 
      abline(intercept, slope, col=mcol, lwd=2)
   } else {
      if (missing(xlab)) xlab <- "Bin Limits"
      if (missing(ylab)) ylab <- "Probability Density"
      x<-seq(l[1],l[length(l)], by=(l[length(l)]-l[1])/100)
      y <- dlnorm(c(0,x), log(gm), log(gsd))
      par(cex=cex, las=1)
      plot.new()
      plot.window(xlim=c(0,max(l)), ylim=c(0,max(pdf,y)), ...)
      title(main=main, xlab=xlab, ylab=ylab)
      rect(l[1:(length(l)-1)], 0, l[-1], pdf, col=dcol, density=shade.density,
           angle=shade.angle, border=rect.border.col, lwd=rect.lwd,
           lty=rect.lty)
      points(c(0,x), y, type="l", lty=lty, lwd=lwd, col=mcol)
   }
   axis(1); axis(2); box()
}

list(lsfit=ls.out, counts=h, limits=l, loglimits=ll, probabilities=pdf, cumulative.probabilities=cdf, normal.quantiles=q, geometric.mean=gm, geometric.standard.deviation=gsd, normalization=norm)


}

blank.factors<-
function(x)
{

# Function to create an easy-to-read version of factor labels,
#     by blanking labels in each group of nested labels except
#     the leading one, where `x' is a dataframe of factor labels
# We assume that the factors have already been sorted into the
#     desired nested arrangement.

# Update: Use as.integer instead of deprecated `codes'.  NK 12-Mar-04

if (!is.data.frame(x))
  stop("`x' must be a dataframe containing factors to be blanked.")

for (i in 1:length(x)) {
   x[[i]] <- factor(x[[i]])
   nums <- c(1, diff(as.integer(x[[i]])))
   levels(x[[i]]) <- c(levels(x[[i]]), "")
   x[[i]][nums==0] <- ""
}
x
}  
bubbleplot<-
function (x, y, z, fact, mult=5,
          area=FALSE,
          inches=FALSE, fg=1, bg=1,
          main=NULL, ylab=NULL, xlab=NULL,
          xlim=c(min(x)-max(z),max(x)+max(z)),
          ylim=c(min(y)-max(z),max(y)+max(z)),
          axes=TRUE, box=TRUE, add=FALSE, ...)

{

# `bubble.plot' : R function to plot a trivariate
#  x-y-z data set with the value of the third non-negative, z,
#  variable represented by differently-sized, and/or
#  -colored, plotting symbols.  The sizes are scaled
#  from the actual z-values, whereas the colors of each
#  z value must be manually-specified by the user.

#  `mult' gives multiple of largest z-value circles that
#  will fit along the x-axis.  
#  `fact' is the normalizing factor to multiply by each z value
#  Can only specify mult OR fact; fact spec overrides mult spec
#  Use `mult' the first time to get circles right, and use the
#  printed fact value to set `fact' in future calls when a consistent
#  size is needed between plots with different z-values (and most likely 
#  the same x-axis).
#  `inches' overrides `fact' and `mult' by scaling the
#  largest z value to 1 inch if TRUE, or to any other number of inches
#  if set equal to a positive number.
#  If `inches=FALSE' the circle radii are in x-axis units

#  To scale the *area* of the circles (bubbles) according to the
#  data `z' values, then set area=`TRUE', otherwise the *radii*
#  of the circles are scaled as the data `z' values (the default).
#  Keep in mind that the radii of the drawn circles are always adjusted
#  by the `fact' argument before being plotted, regardless of whether
#  the `z' values correspond to circle areas or not.  This is OK since
#  the relationship between areas is preserved when a constant is
#  multiplied by each radius, where the "area" constant is the 
#  the square of the "radius" constant (circle area = pi*radius^2).

#  If `add' = TRUE, then the function adds `bubbles' to an
#  existing plot

#  The function invisibly returns the adjusted values of `z'.

#  Now uses the wonderful `symbol' base R function to
#  draw circles.   No other symbols besides circles are
#  available (Did you ever see a square bubble? ....Well maybe....)
#  Use `symbols' directly if you want more symbols (rectangles, stars, etc.)

x<-as.numeric(x)
y<-as.numeric(y)
z<-as.numeric(z)

if (any(z<0)) stop("`z' values must not be negative.")
if (area) z <- sqrt(z)/sqrt(pi)
if (missing(fact))
    fact <- ifelse(diff(range(x))==0, max(x)/(mult*max(z)),
                        diff(range(x))/(mult*max(z)))
z <- fact * z
cat("Radius scale factor is",fact,"\n")

if (length(x) != length(y) | length(y) != length(z) | length(x) != length(z))
   stop("x, y, and z must be numeric vectors of equal length.")

fg <- rep(fg,length(x))
bg <- rep(bg,length(x))

if (!add) {
   plot.new()
   plot.window(xlim=xlim,ylim=ylim)
   title(main=main,ylab=ylab,xlab=xlab)
   if (axes) {
      axis(1)
      axis(2)
   }
   if (box) box()
}

symbols(x, y, circles=z, inches=inches, fg=fg, bg=bg, main=main,
        xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, add=TRUE, ...)

invisible(z)

}
bwplot2 <-
    function(formula,
             data = parent.frame(),
             aspect = "fill",
             layout = NULL,
             panel = "panel.bwplot",
             prepanel = NULL,
             scales = list(),
             strip = TRUE,
             groups = NULL,
             xlab,
             xlim,
             ylab,
             ylim,
             box.ratio = 1,
             horizontal = NULL,
             ...,
             subscripts = !is.null(groups),
             subset = TRUE)
{

# This is the corrected bwplot function that Deepayan sent me
#   fixing the factor/shingle bug I discovered on 
#   March 2, 2003.  Neil Klepeis

#  UPDATE:  Now unnecesary because the new bwplot works fine.
#            9/6/2004


    ##m <- match.call(expand.dots = FALSE)
    ##dots <- m$...
    ##dots <- lapply(dots, eval, data, parent.frame())

    dots <- list(...)

    if (!is.function(panel)) panel <- eval(panel)
    if (!is.function(strip)) strip <- eval(strip)

    prepanel <-
        if (is.function(prepanel)) prepanel 
        else if (is.character(prepanel)) get(prepanel)
        else eval(prepanel)

    ## Step 1: Evaluate x, y, etc. and do some preprocessing

    formname <- deparse(substitute(formula))
    formula <- eval(substitute(formula), data, parent.frame())

    form <-
        if (inherits(formula, "formula"))
            latticeParseFormula(formula, data)
        else {
            if (!is.numeric(formula)) stop("invalid formula")
            else {
                list(left = rep("", length(formula)),
                     right = formula,
                     condition = NULL,
                     left.name = "",
                     right.name = formname)
            }
        }
    if (is.null(form$left)) form$left <- rep("", length(form$right))

    cond <- form$condition
    number.of.cond <- length(cond)
    y <- form$left
    x <- form$right

    if (is.null(horizontal)) {
        horizontal <-
            if ((is.factor(x) || is.shingle(x)) && is.numeric(y)) FALSE
            else TRUE
    }

    if (number.of.cond == 0) {
        strip <- FALSE
        cond <- list(as.factor(rep(1, length(x))))
        layout <- c(1,1,1)
        number.of.cond <- 1
    }

    groups <- eval(substitute(groups), data, parent.frame())
    subset <- eval(substitute(subset), data, parent.frame())

    if ("subscripts" %in% names(formals(eval(panel)))) subscripts <- TRUE
    if (subscripts) subscr <- seq(along=x)
    x <- x[subset, drop = TRUE]
    y <- y[subset, drop = TRUE]
    if (subscripts) subscr <- subscr[subset, drop = TRUE]

    if (horizontal) {
        if (!(is.numeric(x))) {
            ## x <- as.numeric(x)
            warning("x should be numeric")
        }
        y <- as.factorOrShingle(y)
        is.f.y <- is.factor(y)  # used throughout the rest of the code
        num.l.y <- nlevels(y)

        if (missing(xlab)) xlab <- form$right.name
        if (missing(ylab)) ylab <- if (is.f.y) NULL else form$left.name
    }
    else {
        if (!(is.numeric(y))) {
        ##    y <- as.numeric(y)
            warning("y should be numeric")
        }
        x <- as.factorOrShingle(x)
        is.f.x <- is.factor(x)  # used throughout the rest of the code
        num.l.x <- nlevels(x)

        if (missing(ylab)) ylab <- form$left.name
        if (missing(xlab)) xlab <- if (is.f.x) NULL else form$right.name
    }

    ## create a skeleton trellis object with the
    ## less complicated components:

    foo <- do.call("trellis.skeleton",
                   c(list(aspect = aspect,
                          strip = strip,
                          panel = panel,
                          xlab = xlab,
                          ylab = ylab), dots))

    dots <- foo$dots # arguments not processed by trellis.skeleton
    foo <- foo$foo
    foo$call <- match.call()
    foo$fontsize.normal <- 10
    foo$fontsize.small <- 8

    ## This is for cases like xlab/ylab = list(cex=2)
    if (is.list(foo$xlab) && !is.characterOrExpression(foo$xlab$label))
        foo$xlab$label <- form$right.name
    if (is.list(foo$ylab) && !is.characterOrExpression(foo$ylab$label))
        foo$ylab$label <- form$left.name

    ## Step 2: Compute scales.common (leaving out limits for now)

    ##scales <- 
    ##if (missing(scales)) scales 
    ##else eval(m$scales, data, parent.frame())


    ## The following is to make the default alternating FALSE for factors
    if (is.character(scales)) scales <- list(relation = scales)
    if (is.null(scales$alternating)) {
        if (horizontal) {
            if (is.null(scales$y)) scales$y <- list(alternating = FALSE)
            else if (is.null(scales$y$alternating)) scales$y$alternating <- FALSE
        ## bug if y="free" ? but who cares
        }
        else {
            if (is.null(scales$x)) scales$x <- list(alternating = FALSE)
            else if (is.null(scales$x$alternating)) scales$x$alternating <- FALSE
        ## bug if x="free" ? but who cares
        }
    }
    foo <- c(foo,
             do.call("construct.scales", scales))

    if (horizontal) {
        if (is.logical(foo$y.scales$at)) foo$y.scales$at <- 1:num.l.y
        if (is.f.y && is.logical(foo$y.scales$labels))
            foo$y.scales$labels <- levels(y)
    }
    else {
        if (is.logical(foo$x.scales$at)) foo$x.scales$at <- 1:num.l.x
        if (is.f.x && is.logical(foo$x.scales$labels))
            foo$x.scales$labels <- levels(x)
    }

    ## Step 3: Decide if limits were specified in call:

    have.xlim <- !missing(xlim)
    if (!is.null(foo$x.scales$limit)) {
        have.xlim <- TRUE
        xlim <- foo$x.scales$limit
    }
    have.ylim <- !missing(ylim)
    if (!is.null(foo$y.scales$limit)) {
        have.ylim <- TRUE
        ylim <- foo$x.scales$limit
    }

    ## Step 4: Decide if log scales are being used:

    have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
    have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log
    if (have.xlog) {
        xlog <- foo$x.scales$log
        xbase <-
            if (is.logical(xlog)) 10
            else if (is.numeric(xlog)) xlog
            else if (xlog == "e") exp(1)

        x <- log(x, xbase)
        if (have.xlim) xlim <- log(xlim, xbase)
    }
    if (have.ylog) {
        ## warning("Are you sure you want log scale for y ?")
        ylog <- foo$y.scales$log
        ybase <-
            if (is.logical(ylog)) 10
            else if (is.numeric(ylog)) ylog
            else if (ylog == "e") exp(1)

        y <- log(y, ybase)
        if (have.ylim) ylim <- log(ylim, ybase)
    }
    
    ## Step 5: Process cond

    cond <- lapply(cond, as.factorOrShingle, subset, drop = TRUE)
    cond.max.level <- unlist(lapply(cond, nlevels))


    id.na <- is.na(x)|is.na(y)
    for (var in cond)
        id.na <- id.na | is.na(var)
    if (!any(!id.na)) stop("nothing to draw")
    ## Nothing simpler ?

    foo$condlevels <- lapply(cond, levels)

    ## Step 6: Evaluate layout, panel.args.common and panel.args

    foo$panel.args.common <- dots
    foo$panel.args.common$box.ratio <- box.ratio
    foo$panel.args.common$horizontal <- horizontal
    foo$panel.args.common$levels.fos <- ## fos - the factor/shingle in x/y
        if (horizontal) num.l.y else num.l.x
    if (subscripts) foo$panel.args.common$groups <- groups

    layout <- compute.layout(layout, cond.max.level, skip = foo$skip)
    plots.per.page <- max(layout[1] * layout[2], layout[2])
    number.of.pages <- layout[3]
    foo$skip <- rep(foo$skip, length = plots.per.page)
    foo$layout <- layout
    nplots <- plots.per.page * number.of.pages

    foo$panel.args <- as.list(1:nplots)
    cond.current.level <- rep(1,number.of.cond)
    panel.number <- 1 # this is a counter for panel number
    for (page.number in 1:number.of.pages)
        if (!any(cond.max.level-cond.current.level<0))
            for (plot in 1:plots.per.page) {

                if (foo$skip[plot]) foo$panel.args[[panel.number]] <- FALSE
                else if(!any(cond.max.level-cond.current.level<0)) {

                    id <- !id.na
                    for(i in 1:number.of.cond)
                    {
                        var <- cond[[i]]
                        id <- id &
                        if (is.shingle(var))
                            ((var >=
                              levels(var)[[cond.current.level[i]]][1])
                             & (var <=
                                levels(var)[[cond.current.level[i]]][2]))
                        else (as.numeric(var) == cond.current.level[i])
                    }

                    if (horizontal) {
                        if (is.f.y) {
                            foo$panel.args[[panel.number]] <-
                                list(x = x[id],
                                     ##y = as.numeric(y[id]))
                                     y = y[id])
                            if (subscripts)
                                foo$panel.args[[panel.number]]$subscripts <-
                                    subscr[id]
                        }
                        else {  # shingle
                            panel.x <- numeric(0)
                            panel.y <- numeric(0)
                            panel.subscr <- numeric(0)
                            for (k in 1:num.l.y) {
                                tid <- id & (y >= levels(y)[[k]][1]) & (y <= levels(y)[[k]][2])
                                panel.x <- c(panel.x, x[tid])
                                panel.y <- c(panel.y, rep(k,length(tid[tid])))
                                panel.subscr <- c(panel.subscr, subscr[tid])
                            }
                            foo$panel.args[[panel.number]] <-
                                list(x = panel.x,
                                     y = panel.y)
                            if (subscripts)
                                foo$panel.args[[panel.number]]$subscripts <-
                                    panel.subscr

                        }
                    }
                    else {
                        if (is.f.x) {
                            foo$panel.args[[panel.number]] <-
                                ##list(x = as.numeric(x[id]),
                                list(x = x[id],
                                     y = y[id])
                            if (subscripts)
                                foo$panel.args[[panel.number]]$subscripts <-
                                    subscr[id]
                        }
                        else {  # shingle
                            panel.x <- numeric(0)
                            panel.y <- numeric(0)
                            panel.subscr <- numeric(0)
                            for (k in 1:num.l.x) {
                                tid <- id & (x >= levels(x)[[k]][1]) & (x <= levels(x)[[k]][2])
                                panel.y <- c(panel.y, y[tid])
                                panel.x <- c(panel.x, rep(k,length(tid[tid])))
                                panel.subscr <- c(panel.subscr, subscr[tid])
                            }
                            foo$panel.args[[panel.number]] <-
                                list(x = panel.x,
                                     y = panel.y)
                            if (subscripts)
                                foo$panel.args[[panel.number]]$subscripts <-
                                    panel.subscr
                        }
                    }

                    cond.current.level <-
                        cupdate(cond.current.level,
                                cond.max.level)
                }

                panel.number <- panel.number + 1
            }

    foo <- c(foo,
             limits.and.aspect(prepanel.default.bwplot,
                               prepanel = prepanel, 
                               have.xlim = have.xlim, xlim = xlim, 
                               have.ylim = have.ylim, ylim = ylim, 
                               x.relation = foo$x.scales$relation,
                               y.relation = foo$y.scales$relation,
                               panel.args.common = foo$panel.args.common,
                               panel.args = foo$panel.args,
                               aspect = aspect,
                               nplots = nplots))

    class(foo) <- "trellis"
    foo
}

cat.cols<-
function(n, set="big2", plot=FALSE)

{

# Function to return a set of colors that are (hopefully)
#   appropriate for use in differentiating categories.
# Uses RColorBrewer for eight and fewer and colors
#   and spline.colors() as a backup.

# Updated.  Now added a 27 color palette "big"

# The possible palettes are currently from the RColorBrewer categorical
#    palettes, and only allow for a maximum of eight different colors.
#    If there are more than 8 rooms, spline.colors() is used to
#    get a number of (hopefully) distinct rainbow colors.
#    The sets are: "set1", "set2", "set3", "accent", "dark2", "pastel1",
#       "pastel2"

if (n < 1)
  stop("`n' must be an integer greater than or equal to 1.")
n <- trunc(n)

set1 <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
   "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
set2 <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3",
   "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
set3 <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072",
   "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9",
   "#BC80BD","#CCEBC5")
dark2 <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
   "#66A61E", "#E6AB02", "#A6761D", "#666666")
accent <- c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99",
   "#386CB0", "#F0027F", "#BF5B17", "#666666")
pastel1 <- c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4",
   "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2")
pastel2 <- c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4",
   "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC")
big <- c("#800000", "#C00000", "#FF0000", "#FFC0C0",
"#008000","#00C000","#00FF00","#C0FFC0",
"#000080","#0000C0", "#0000FF","#C0C0FF",
"#808000","#C0C000","#FFFF00","#FFFFC0",
"#008080","#00C0C0","#00FFFF","#C0FFFF",
"#800080","#C000C0","#FF00FF","#FFC0FF",
"#C39004","#FF8000","#FFA858","#FFDCA8")
big2 <- big[c(12,16,25,24,
              2,11,6,15,18,26,23,
	      3,10,7,14,19,27,22,
	      4,8,20,28)]
dark <- big[seq(1,28,by=4)]
med <- big[seq(2,28,by=4)]
reg <- big[seq(3,28,by=4)]
light <- big[seq(4,28,by=4)]

sets<-c("set1","set2","set3","dark2","accent","pastel1","pastel2",
"big","big2","dark","med","reg","light")
if (is.null(set)) set <- "big2"
if (is.na(set) | !set %in% sets) set <- "big2"
if (n <= length(get(set)))
  cols<-get(set)[1:n]
else 
  cols<-spline.colors(n)
if (plot) image(1:n, 1, as.matrix(1:n), col = cols)
cols
}

cdf<-
function (x, xlim,ylim,main = "Empirical Cumulative Density Function (cdf)",
	  xlab = "Sample Values", 
    	  ylab = "Cumulative Probability", type = "l", plot.it = TRUE,
	  exclude = FALSE, exval = 0, oneminus = FALSE,
	  addq = NULL , addp = NULL, new = TRUE, cex=1,...) 
{

# Function to calculate and plot an empirical cdf function for
# continuous numeric data.  Currently doesn't deal correctly with ties,
# hopefully there aren't any.  Need a different function to deal
# with binned data.



    if (0 == (n <- length(x))) 
        stop("Sample vector is empty.") 
    if (exclude)
	NA -> x[x<=exval]
    x <- x[!is.na(x)]
    x <- sort(x)
    if (missing(xlim)) {
	ft <- 10^( round( log10( (max(x)-min(x))/10 ) ) )
	x1 <- ft*floor(min(x)/ft)
	x2 <- ft*ceiling(max(x)/ft)
	xlim <- c(x1,x2)
    }
    y <- cumsum(rep(1/length(x),length(x)))
    if (oneminus) {
	y <- 1 - y 
	ylab <- "1 - Cumulative Probability"
    }
    if (missing(ylim)) ylim<-c(0,1)
    x1 <- NULL; y1 <- NULL    
    qcalc <- NULL; pcalc <- NULL
    if (plot.it) {
        old.par <- par(no.readonly=T)
        on.exit(par(old.par))
	par(cex=cex, xaxs = "r", yaxs = "r", ps=12,ann = TRUE, new = FALSE, las = 1)
	if (!new)
           par(new=TRUE)
 
        plot(x, y, main = main, xlab = xlab, ylab = ylab, type=type, xlim=xlim, ylim=ylim, ...)

	#linearly interpolate to estimate percentiles and quantiles
	if (!is.null(addq)) {
	   for (u in 1:length(addq)) {	
              j<-length(x) - length(x[x > addq[u]])  #left index from quantile  
              y1<-(addq[u]-x[j])*((y[j+1]-y[j])/(x[j+1]-x[j]))+y[j] #est. perc
	      pcalc[u] <- y1
	      lines(c(addq[u],addq[u]),c(0,y1),lty=3)
	      lines(c(x[1],addq[u]),c(y1,y1),lty=3)
	      text(addq[u],0,labels=addq[u],pos=1,col=2,offset=0)
	      text(x[1],y1,labels=formatC(y1,format="f",digits=2),
				pos=2,col=2,offset=0)
  	   }
	}
	if (!is.null(addp)) {
	   for (u in 1:length(addp)) {
	      if (oneminus) {
	      	 i <- trunc((1-addp[u])*length(x)) 
	      } else {	
	         i <- trunc(addp[u]*length(x)) #left index from percentile
	      }
              x1<-(addp[u]-y[i])*((x[i+1]-x[i])/(y[i+1]-y[i]))+x[i] #est.quant 
	      qcalc[u] <- x1
	      lines(c(x1,x1),c(0,addp[u]),lty=3)
	      lines(c(x[1],x1),c(addp[u],addp[u]),lty=3)
	      text(x1,0,labels=formatC(x1,format="e",digits=2),
					pos=1,col=4,offset=0)
	      text(x[1],addp[u],labels=c(addp[u]),pos=2,col=4,offset=0)
	   }
	}
    }
    invisible(list(data = x, cumprob = y, newquant=list(quant=addq,perc=pcalc),
              newperc=list(perc=addp, quant=qcalc) ))
}
collapse <-
function(x)
{

#  Function to return an index that can be used to 
#    condense a vector or factor, so that subsequences of 
#    repeated values are collapsed to a single value.

#  The returned index gives the position of the first occurrence
#    of a series of repeated values

#  For example, for the following vector "x" would return
#   the index "i", which can be used to generate the 
#   collapsed vector "y"
#
#  x <- c(1,1,1,2,2,2,3,4,4,5,1,1,1)
#  i <- c(1,4,7,8,10,11)
#  x[i] = y = c(1,2,3,4,5,1)

#  WRONG!!!!   Make the returned indices the first occurrence not the
#     last so that we can easily use the index to subset associated
#     vectors, e.g., for time breaks.   The ta.coll in the heR.Activities
#     package does it by deciding what events to remove (leaving the last
#     occurrence and then adding 1 to the index for the events.  This
#     works, but is not clear if we are returning an index to use with
#     time vectors (telling user to put a "-" in front of a "removal" index
#     is a little odd....   OK done.  We just reverse the initial vector
#     and subtract indices from (length+1)

# Make it a factor and assign integers for each level
x <- as.integer(as.character(factor(x, levels=unique(x),
                   labels=1:length(unique(x)),exclude=NULL)))
# reverse the order
x <- rev(x)

# save vector of all indices
alli <- 1:length(x)

# which values should be omitted (finds last occurrence in the
# reversed vector)
omit <- which(diff(x) == 0)

# Calculate indices for first occurrences in the original vector
keepi <- length(x) + 1 - which(!alli %in% omit)
rev(keepi)

}
comments<-
function(z) {

# Function to return the comments for all the variables
# in the passed data frame as a list with names
# corresponding to the data frame column names

if (!is.data.frame(z))  
  stop("`z' must be a dataframe object.")

c <- list()
for (i in 1:NCOL(z)) {
    if (!is.null(comment(z[[i]])))
        c[[i]] <- comment(z[[i]])
    else
        c[[i]] <- "..." 
    names(c)[i] <- names(z)[i]
}
c<-data.frame(VAR.NAME=names(c), VAR.COMMENT=unlist(c))
row.names(c)<-1:NROW(c)
c
}
compass2polar<-
function(x)
{

# Function to convert from "compass" degrees to 
#    "polar" degrees

# Compass degrees are:  

#   0  North
#  90  East
# 180 South
# 270 East

# Polar degrees are:

#   0  East
#  90 North
# 180 West
# 270 South

360 - x + 90


}
compass2uv <-
function(dir, mag)
{

# Function to convert from "compass" degrees and wind speed 
#  (magnitude) to the "UV" components of wind

#  I use this to convert the Windsonic (Gill, Inc, UK) readings in
#  "Polar" mode, which really reports wind direction 
#  values in compass degrees, to those in "UV" mode, which
#  are the components of wind speed in the X and Y directions 
#  across a horizonal plane.

# Compass degrees are (wind COMING FROM a certain direction):

#   0 North
#  90 East
# 180 South
# 270 East

# UV coordinates degrees are (defines the vector that wind is BLOWING TO,
#  i.e., the direction of air flow due to wind):

#  U = north/south direction, negative means south
#  V = east/west direction, negative means east

if (any(dir < 0) | any(dir > 360))
  stop("`dir' must be compass degrees between 0 and 360.")
if (any(mag <0)) stop("`mag' values must be zero or positive.")

#  Old stuff before I realized that the wind direction angle is
#    really the direction that wind is COMING FROM (stupid me).
#u <- mag*cos((pi/180)*dir)
#v <- -mag*sin((pi/180)*dir)

#  These formulas are from Gill, Inc. customer support:

u <- mag*sin((pi/180)*dir - pi/2)
v <- mag*cos((pi/180)*dir - pi/2)

data.frame(U=u, V=v)

}
dapply <- 
function(x, by, FUN, ...)
{

#  This function is similar to, and derived from, the tapply and aggregate
#   functions in that it operates on an object `x' that is
#   split by a list of equal-length factors in `by' according to 
#   a user-defined function `FUN'.  Unlike `tapply, and like
#   aggregate.data.frame, it inputs and outputs a dataframe rather
#   than an array.   Except, unlike aggregate,
#   it operates on multiple columns in a dataframe, allowing
#   for their interaction, and the return of multiple values
#   in the form of data.frame row.   Aggregate only operates
#   on multiple data.frame columns independently.
#   This new function `dapply' binds the returned dataframe rows into
#   a single dataframe that also contains unique combinations
#   of the factors passed in `by' corresponding to 
#   each returned row value.
#
#  This function was originally designed to calculate the maximum value
#  e.g., of pollutant concentration, in a given group and return the maximum
#  value as well as corresponding parameters for other variables such
#  as monitoring height, angle of placement, or distance from source.
#       NK 24-Nov-2004
#
#  The function is really just a convenient way to split a dataframe
#      and aggregate parts of it based on one or more response variables
#      and an index list -- nicely keeping track of all the indices in
#      the output.
#   
#-------------------------------------------------------------------

# Stuff from aggregate.data.frame:
if (!is.data.frame(x)) x <- as.data.frame(x)
if (!is.list(by)) stop(paste(sQuote("by"), "must be a list"))
if (is.null(names(by))) 
  names(by) <- paste("Group", seq(along = by), sep = ".")
else {
  nam <- names(by)
  ind <- which(nchar(nam) == 0)
  names(by)[ind] <- paste("Group", ind, sep = ".")
}  

# Stuff from tapply:
FUN <- if (!is.null(FUN)) match.fun(FUN)
nI <- length(by)
namelist <- vector("list", nI)
names(namelist) <- names(by)
extent <- integer(nI)
nx <- NROW(x)
one <- as.integer(1)
group <- rep(one, nx)
ngroup <- one
for (i in seq(by)) {
   index <- as.factor(by[[i]])
   if (length(index) != nx) 
     stop("arguments must have same length")
   namelist[[i]] <- levels(index)
   extent[i] <- nlevels(index)
   group <- group + ngroup * (as.integer(index) - one)
   ngroup <- ngroup * nlevels(index)
}

ans <- lapply(split(x, group), FUN, ...)
index <- as.numeric(names(ans))

if (any(unlist(lapply(ans, NROW)) > 1) |
   (!all(unlist(lapply(ans, is.data.frame)))) |
   (!all(diff(unlist(lapply(ans, length)))) == 0))
  stop("`FUN' must return a single dataframe row with consistent length for each group in `x', with groups defined by the factor list in `by'.")


# More stuff from aggregate.data.frame to get the factors and
#    merge with row results for each group:
w <- NULL
for (i in seq(along = extent)) {
   j <- rep.int(rep.int(seq(1:extent[i]), prod(extent[seq(length = i - 
        1)]) * rep.int(1, extent[i])), prod(extent[seq(from = i + 1, 
         length = length(extent) - i)]))
   w <- cbind(w, namelist[[i]][j])
}
w <- as.data.frame(w[index, ])
names(w) <- names(by)
ans <- do.call("rbind", ans)
ans <- data.frame(w, ans)
ans

}
data2lnorm<-
function (y, plot=TRUE,
  	  forceNA = FALSE, ...) 
{

# This function takes a data vector y and
# fits a lognormal model to it, optionally plotting
# the fit and the data together as a CDF.
# i.e., plots a cdf (log-probability) with the fitted line through the 
# data points.   Based on qqplot/qnorm R functions.

if (forceNA)
    NA->y[y<=0]
y <- y[!is.na(y)]
if (0 == (n <- length(y))) 
    stop("Sample vector is empty.") 
ylim<-range(y)
if (ylim[1] < 0 )
    stop("Samples values must be greater than zero.")

q <- qnorm(ppoints(n))[order(order(y))]
ly<-log(y)

ls.out<-lsfit(ly, q) 
intercept<-ls.out$coefficients[1]
slope<-ls.out$coefficients[2]
names(intercept)<-NULL
names(slope)<-NULL

gm <- exp((qnorm(0.5)-intercept)/slope)
gsd <- exp((qnorm(0.84)-intercept)/slope)/gm

main <- paste("Lognormal Fit to Data: ( GM=", format(gm,digits=3), ", GSD=", format(gsd,digits=3),")")

if (plot) { 
   xlab <- "Log(Data Values)"
   ylab <- "Standard Normal Quantiles"
   plot(ly, q, type = "p", main=main, ylab=ylab, xlab=xlab, ...) 
   abline(intercept, slope)
}

list(geometric.mean=gm, geometric.standard.deviation=gsd)


}
distrib <-
function (values, weights=NULL, simulate)
{

# Function to create a `distrib' object that contains the
# information needed to produce a sample from a distribution
# model, an empirical distribution, or a number of raw values.

# Updated:  Now prints a message and stops if function fails, otherwise
#           simply returns the distrib object (with no other messages)
#                           27-Jan-2003

# Components: SIMULATE, VALUES, WEIGHTS

# There is a method sim.distrib for sampling the distribution, which
# returns 1 or more sampled values.

if (!is.numeric(values) | !is.vector(values))
  stop("`values' must be a numeric vector.")
else
  values <- as.numeric(values)
if (missing(simulate))
  simulate <- "sample"
if (simulate == "sample" & is.null(weights))
  weights <- rep(1, length=length(values))
# Assemble the distribution object  
distrib <- list(values=values, weights=weights, simulate=simulate)
attr(distrib, "class") <- "distrib"
# Now test it
res <- try(sim.distrib(distrib, n=1))
if (inherits(res, "try-error") | is.na(res) | is.nan(res) | !is.finite(res) ) 
  stop(paste("Failed sampling a finite test value of type",simulate))
else 
  return(distrib)
}
fit.ts.empirical<-
function (x, f, init.step=0, par=NULL,
          plot=TRUE, plot.res=FALSE, maxits=100) {

# `fit.ts.empirical' : R function to fit the cyclical parts of
# a time series using empirical patterns in the data, e.g., values
# by time-of-day or day-of-week.
#
# `x' is the observed time series to be modeled
# `f' is a matrix or data frame containing `n' cyclical factor specifications 
# `par', if specified, is a vector of the initial parameters
#    (e.g., shift parameter and factored means).  NOT CHECKED FOR VALIDITY!

x<-as.vector(x)
n <- NCOL(f)
for (i in 1:n) 
    f[[i]] <- as.factor(f[[i]])  #factors: sorted, distinct values of `x'

if (length(x) != NROW(f))
   stop("The cyclical factors in `f' must have the same length as the time series `x'.")

if (is.null(par)) {  # get means if not specified as part of `par'
   l <-  list()  # list to contain the means by each factor
   # Calculate and save the `factored means'
   for (i in 1:n) {
        c<-complete.cases(x,f[[i]])
        l[[i]]<-aggregate.data.frame(x[c], by=as.list(f[c,])[i], FUN=mean)[,2]
   }
   par <- c(init.step,unlist(l))  # set shift to zero in initial parameter list
} else {
   if (!is.vector(par))
      stop("`par', if specified, must be a vector containing the model parameters.")
}

# --------------------------------------------
# Function to calculate the empirical model from the observed
# (or updated) patterns `l' across each specified factor in `f'

fn <- function (l,f) {
   v <- rep(0,NROW(f))
   for (i in 1:n) 
      v <- v + l[[i]][cut(as.numeric(f[[i]]),length(levels(f[[i]])),labels=F)]
   v
}
# ---------------------------------------------

# ---------------------------------------------
# Function to take the parameter list (the flattened set of
# factored means and a shift parameter) and return the
# mean absolute value of the residuals between the empirical
# model and the observed time series.

fnn <- function(par, rf=function (r) mean(abs(r),na.rm=TRUE)) {
   shift <- par[1]  # shift parameter
   par <- par[-1]   # rest of factored mean parameters
   ll <- list()
   for (i in 1:n) { # parse parameter list
       if (i==1)
          a <- 0
       else
          a <- length(levels(f[[i-1]]))
       b <- length(levels(f[[i]]))
       ll[[i]] <- par[(1+a):(a+b)]
   }
   m <- as.ts(shift+fn(ll,f))
   x <- as.ts(x)
   r <- as.ts(m-x)
   if (plot) {
      if (plot.res)
         par(mfrow=c(2,1))
      plot(x, type="l", lty="dashed", ylab="Time Series")
      lines(m, type="l", lty=1)
      if (plot.res) {
         plot(r, type="l", lwd=1, ylab="Residuals")
         par(mfrow=c(1,1))
      }
   }
   rf(r)
}
# ----------------------------------------------------

res<-nlm(fnn, par, iterlim=maxits)       # call the minimization routine

r<-fnn(res$estimate,rf<-function (r) r)  # get the final residuals

invisible(list(data=data.frame(OBSERV=x, RESID=r, f), code=res$code, minimum=res$minimum, iterations=res$iterations, par=res$estimate))

}
fit.ts.sin<-
function (x, plot=TRUE, plot.res=FALSE, maxits=200,
          wave=c(24, 24, 24*7, 24*7*365),
          phase=c(rep(0,length(wave))),
          amp=c(0,rep(1,length(wave))))
{

# `fit.ts.sin' : R function to fit a sum of sinusoids to
# a time series with terms for each different cyclical
# pattern, e.g., diurnal, weekly, yearly, etc.
# Returns a time series of residual values for the
# best fit of the model to the observed time series.
# Also returns the best-fit parameters and other stats.
# The residuals are expected to be a simple autoregressive
# time series that can be fit with the R function `ar'.

# Alternative:  use the R function `stl', but which only seems to 
# examine a single cycle (defined by the time series' `frequency').

# `wave' contains the wavelengths of the sin functions (not optimized)
# `phase' contains the initial phases for the sin functions (optimized)
# 'amp' contains the initial amplitudes for the sin functions (optimized)

if (length(wave) != length(phase))
   stop("`wave' and `phase' must be equal length vectors.")
if (length(wave) != (length(amp)-1)) {
   cat("`wave' has length ", length(wave), "\n")
   cat("`amp' has length ", length(amp), "\n")
   stop("`amp' must be 1 element longer than `wave'")
}
if ((length(amp)-1) != length(phase)) {
   cat("`amp' has length ", length(amp), "\n")
   cat("`phase' has length ", length(phase), "\n")
   stop("`amp' must be 1 element longer than `phase'")
}

n <- length(wave)
x <- as.ts(x)
t <- time(x)

# -------------------------------------
# Function to calculate model time series from `amp', `phase', `wave'
fn <- function (amp, phase, wave) {
   f <- amp[1]
   for (i in 1:n)
      f <- f + amp[i+1]*sin((t-phase[i])*2*pi/wave[i])
   f
}
# -------------------------------------

# --------------------------------------
# Wrapper function to convert a single parameter vector into
# the `amp' and `phase' vectors, and then call `fn'. The values
# for `amp' come first, followed by those for `phase'.
# Note: `wave' doesn't change.  Returns the mean absolute
# value of the residuals, by default, although any
# returned function of the residuals can be specified.
fnp <- function (p, rt=function (r) mean(abs(r), na.rm=T)) {
    amp <- p[1:(n+1)]
    phase <- p[(n+2):(2*n+1)]
    f <- ts(fn(amp, phase, wave), freq=frequency(x))
    r <- ts(f-x, freq=frequency(x))
    if (plot) {
       if (plot.res)
          par(mfrow=c(2,1))
       plot(x, type="l", lty=2)
       lines(f, type="l", lty=1)
       if (plot.res)
          plot(r, type="p", pch=16, ylab="Residuals")
       par(mfrow=c(1,1))
    }
    rt(r)
}
# -------------------------------------

p <- c(amp, phase)
res<-nlm(fnp, p, iterlim=maxits)
r<-fnp(res$estimate, rt=function(x) x)
amp <- res$estimate[1:(n+1)]
phase <- res$estimate[(n+2):(2*n+1)]

list(resid=r, observ=x, min=res$minimum, iterations=res$iterations, amp=amp, phase=phase, wave=wave)
}
freq.tables<-
function (x, tablespecs, latex=FALSE, file="") 
{

# Function to return flat frequency tables in a convenient dataframe format.
#   For each response variable listed, one table is generated across
#   all key variables.  Takes a `tablespecs' object as input that
#   contains all variable, label, etc. specifications for the
#   table.

#  Note we convert the `ftable' format to a dataframe format for
#    easy storage and conversion to LaTeX or HTML with the `xtable' function.

# `x' is a data frame containing the raw `data' output from
#    the `mrd' function.

#  `tablespecs' contains the options described below:

# The variables are cut into intervals with the `cut', right=TRUE/FALSE, 
#   function, according the the breaks given the `response.cuts' or `key.cuts'
#   list arguments. A value of NA means the variable will not be cut.  All
#   variables are converted to factors, whether or not they are cut.
#   Labels for binary variables (i.e., those having only 0's and 1's) are set
#   to "TRUE" and "FALSE" values.

#  Important Note:  The user must check to make sure the specified cuts
#   cover the
#   range of variable values of interest, because values not within the
#   specified breaks will be assigned NA values during the factoring process.

# The frequency table is returned as a probability table if `prob' is TRUE.

# If latex=TRUE, then the tables are printed in LaTeX format 
#     although the `ftable' dataframe object(s) is(are) also returned.  If
#     `file' != "", then the LaTeX output will be written to the named file
#     with one table appended after another.
# --------------------------------------------------------------------------

if (!inherits(tablespecs,"tablespecs"))
  stop("`tablespecs' argument must be an object of class `tablespecs' created using the `tablespecs' function.")

responses <- tablespecs$responses
keys <- tablespecs$keys
response.cuts <- tablespecs$response.cuts
key.cuts <- tablespecs$key.cuts
right <- tablespecs$right
include.lowest <- tablespecs$include.lowest
response.labels <- tablespecs$response.labels
key.labels <- tablespecs$key.labels
blank.labels <- tablespecs$blank.labels
prob <- tablespecs$prob


if (any(!responses %in% names(x)))
  stop(paste("One or more response variables do not exist:",paste(responses,collapse=", ")))
if (!is.list(response.cuts) | length(response.cuts) != length(responses))
  stop("`response.cuts' must contain breaks for cutting each of the response variables, or NA to skip cutting for a particular variable.")

if (any(!keys %in% names(x)))
  stop("One or more key variables do not exist.")
if (!is.list(key.cuts) | length(key.cuts) != length(keys))
  stop("`key.cuts' must contain breaks for cutting each of the key variables, or NA to skip cutting for a particular variable.")

xresponses <- x[responses]
xkeys <- x[keys]

# Cut the response variables
for (i in 1:length(responses)) 
   if (all(!is.na(response.cuts[[i]]))) 
     xresponses[[i]] <- cut(xresponses[[i]], breaks=response.cuts[[i]], 
                            right=right, include.lowest=include.lowest,
			    labels=response.labels[[i]])

# Cut the key variables
for (i in 1:length(keys))
   if (all(!is.na(key.cuts[[i]])))
     xkeys[[i]] <- cut(xkeys[[i]], breaks=key.cuts[[i]], 
                       right=right, include.lowest=include.lowest,
		       labels=key.labels[[i]])

# Function to create an easy-to-read version of the ftable key labels,
#     by blanking labels in each group of nested labels except
#     the leading one, where `x' is a dataframe of key labels
blankLabels <- function (x) {
  for (i in 1:length(x)) {
     x[[i]] <- factor(x[[i]])
     nums <- c(1, diff(codes(x[[i]])))
     levels(x[[i]]) <- c(levels(x[[i]]), "")
     x[[i]][nums==0] <- ""
  }
  x
}

# Function to put the `ftable' into dataframe format, 
#     where `x' is an ftable object, and `blank' is
#     an option to blank all labels except leading ones
#     in a group for each column, see function above
toDataFrame <- function (x, blank=FALSE) {
  row.vars <- attributes(x)$row.vars
  col.var <- attributes(x)$col.vars
  n <- length(row.vars)
  lab <- expand.grid(row.vars[n:1])[n:1]
  if (blank) lab <- blankLabels(lab)
  data <- as.data.frame(matrix(x, nr=NROW(x), nc=NCOL(x)))
  names(data) <- paste(names(col.var), col.var[[1]])
  cbind(lab, data)
}


fts <- list()
for (i in 1:length(responses)) {
   fts[[i]] <- ftable(cbind(xkeys, xresponses[i]), row.vars=names(keys),
                          col.vars=names(responses[i]))
   if (prob) fts[[i]] <- round(fts[[i]] / apply(fts[[i]], 1, sum), digits=2)
   fts[[i]] <- toDataFrame(fts[[i]], blank.labels)
   if (latex) {
     require(xtable)
     if (prob) digits <- rep(2,NCOL(fts[[i]])+1)
       else digits <- rep(0,NCOL(fts[[i]])+1)
     print.xtable(xtable(fts[[i]], digits=digits), file=file, append=TRUE)
   }
}

if (length(fts) == 1) fts <- fts[[1]]
fts
}

get.time.POSIX <-
function (date.time, format="%d/%m/%y %H:%M:%S", units="mins", interval=1,
            index=1, start=NULL,   tolerance=0.00001,
            names=c("Time.POSIX",paste("Elapsed",units,sep="."),"Time.Factor"))
{

#  Function to return a data frame containing the POSIX
#   object of a date.time character or character factor vector,
#   including the elapsed time in 'units'.
#  Elapsed numeric times are calculated starting from the `index', which 
#  is assumed to be the first value in the vector, UNLESS a `start'
#  is specified in the same format as the date.time's, in which case we
#    use that as the reference point for elapsed numeric time units.

#  Also returns a time factor showing which records belong to a given
#   time interval (defined by units),
#   starting at the origin specified by `start' or `index',

#  UPDATE:   Added interval argument, defaulting to 1 unit, so that we can
#        create time factors for multiples of mins or secs or hours, etc.
#                    12-June-2008 NK.

#   UPDATE:   Added tolerance argument, which defaults to the same as the
#      default for time.factor.2 .   NK 25-Jan-2006

#  UPDATE:  Check for all NA's in times and allow for a Date/Time object
#         version of `start' to be passed instead of a char string.  
#                NK 25-Jan-2006
  

time.POSIX <- as.POSIXct(strptime(as.character(date.time), format=format))

if (any(is.na(time.POSIX)))
  stop("Please check your `format' string.")

if (index > NROW(time.POSIX) | index < 1) 
  stop("`index' value was out of range.")

# Start is assigned to `index' time value or converted from
#   character or translated to POSIXlt if a character string using the
#    same `format' or coerced to POSIXct otherwise
if (is.null(start))
  start <- time.POSIX[as.integer(index)]
else if (is.character(start))
  start <- strptime(as.character(start), format=format)
else
  start <- as.POSIXct(start)

if (is.na(start))
  stop("Check the format of `start' -- strings must be in the same format as `date.time'.")

elapsed <- as.numeric(difftime(time.POSIX, start, units=units))

time.factor <- time.factor.2(elapsed, interval=interval, origin=0,
                             integer.levels=TRUE, tolerance=tolerance)

ret <- data.frame(Time.POSIX=time.POSIX, Elapsed=elapsed, Time.Factor=time.factor)
names(ret) <- names
ret

}
gm<-
function (x, na.rm=FALSE, force=FALSE)
{

# Calculate the geometric mean from raw data, by taking in 
#  sequence:  the natural log-transform, the arithmetic mean,
#  and then exponential

# if force==TRUE, then values of zero or smaller are removed, 
#   and a warning given, otherwise a fatal error is issued.

if (any(x <= 0)) 
  if (force) {
    m <- sum(x <= 0)
    x <- x[x > 0]
    warning(paste(m,"value(s) equal to zero or smaller was(were) removed."))
  } else
    stop("`x' must contain only positive values.") 

exp(mean(log(x), na.rm=na.rm))

}
goodgroups<-
function (x) 
{

# Function to return a factor that can be used to split
#    a vector into groups of continguous non-NA values.
#    Returns NA if all passed values are NA. (now it really does)

x <- as.vector(x)
# Get non-NA values
good <- x[!is.na(x)]
if (!length(good)) return(NA)

# Add first non-NA value to the beginning
x <- c(good[1],x)

f <- cumsum(is.na(x))
idx <- which(as.logical(diff(f))) + 1
f[idx] <- NA
f <- f[-1]
n<-length(unique(f[!is.na(f)]))
factor(f, labels=1:n)
}

gplot2<-
function(dat, g=1, gmode="custom", diag=FALSE, labels=c(1:dim(dat)[2]),
         coord=NULL, jitter=FALSE, thresh=0, label.adj=c(1.5,0.5),
	 label.offsets=c(0,0), arrows=1,
	 edge.offsets=NULL, circles=0.05,
	 mode="mds", lower.sel=1, lower.arc=90, 
	 pad=NULL, vertex.pch=20,
	 label.cex=1, vertex.cex=1, label.col=1,
	 display.edge.labels=TRUE, edge.label.pos=1/6, edge.label.cex=1,
	 edge.label.offset=0.03,
	 edge.col=1, vertex.col=1,
	 arrowhead.angle=10, arrowhead.length=0.2,
	 edge.lty=1, edge.lwd=0, 
	 circles.fg=1, circles.bg=NA, circles.lwd=1, axes=FALSE,
	 ylab=expression(lambda[1]), xlab=expression(lambda[2]),
	 main=NULL)
{

# gplot2 - A function for (directed/undirected) graph visualization

# Original `gplot' by Carter Butts, ctb@andrew.cmu.edu, included
#     in the Social Network Analysis (sna) R package version 0.41

# Function revised by Neil Klepeis, October 2002.

# Update:  can adjust the size of the dot for edge labels. --NK 7-Mar-04

# Update;  Remove sna dependence and the whole is.isolates thing.
#              OK finally removed the argument too. NK 5-Nov-04

# Update:  Now we can optionally place labels on edges, one 
#          third of their total length from the head, by default,
#          and below.
#                 27-April-03

# Changes:

#  o We now can place a selected subset of vertices
#      in a lower circular arc of a specified number of degrees,
#      with the remainder in an upper circular arc
#      over the remaining number of degrees; see `mode'="lucircle",
#      `lower.sel', and `lower.arc' arguments.

#  o Add automatic offsets to the arrows and labels, linked by default
#      to the `circles' specification; offset can also be 
#      specified by the user; see `edge.offsets', `label.adj' arguments

#  o Some edges can have arrows, and some just line segments;
#    see the new `arrows' matrix argument that replaces the old
#   `usearrows' argument; by default, line segents are drawn for
#    every edge.

#  o All of the `cex', `col', `lty', `pch', `lwd' specifications
#    can be a vector/matrix so that different vertices/edges can
#    have different line
#    types, sizes, colors, etc.  A single value (or vector) is recycled
#    across all of the edges/vertices.

#  Other: o `edge.type' changed to `edge.lty' for edge line-type specfication.
#         o  Now `jitter=FALSE' by default.
#         o  `axes'=FALSE by default
#         o  axis labels and main title are now options
#         o  edge arrowhead angle is now an option
#         o  gmode is "custom" by default, allowing both segments and arrows
#                 at the same time

#  TODO:  Add loops for each node, i.e., for non-zero diagonal values in `dat'
#
#         Add labels for each edge, including loops, optionally showing
#         the value in `dat', or some other custom label.

#         Optionally have double arrows be parallel, separate lines,
#         or maybe curves/splines.

# Uses:

#    I plan to use this function to draw connections as well as the 
#    magnitude of flow between rooms of a house, the outdoors, and a
#    mechanical ventilation system.  To just display "connections", I 
#    will use segments of differing thicknesses for different levels of
#    connection for rooms and the outdoors (e.g., wall, closed door, open door,
#    etc.) and arrows showing whether the mechanical ventilation is bringing
#    air in (intake), taking it out (return), or both.  To display flow
#    magnitudes, I will have the line thickness be proportional to the total
#    amount of flow between rooms, to/from the outdoors, or to/from the
#    mechanical ventilation.  The length of the arrow heads will show the
#    relative magnitude of flow in either direction (e.g., room 1 -> room 2
#    versus room 2 -> room 1, or outdoors -> 1 versus 1 -> outdoors).

# -----------------------------------------------------------


#Extract the graph to be displayed

if (length(dim(dat))>2)
   d<-dat[g,,]
else
   d<-dat


# Check `dat'

if (dim(d)[1] != dim(d)[2])
  stop("`dat' must be a square matrix containing node ties.")
else
  n <- dim(d)[1]



#Make adjustments for gmode, if required

if (gmode=="graph"){
  arrows <- NULL
} else if (gmode=="digraph") {
  arrows <- matrix(1,nrow=n,ncol=n)
} else if (gmode=="twomode") {
  n<-sum(dim(d))
  temp<-matrix(0,nrow=n,ncol=n)
  temp[1:dim(d)[1],(dim(d)[1]+1):n]<-d
  d<-temp
  if (all(labels==1:dim(dat)[2]))
    labels<-1:n
}


#Replace NAs with 0s

d[is.na(d)]<-0


#Check/set arrows vertex,edge,circles,offsets,pad specifications,
#       recycling if necessary

# Note: Arrows are drawn by default
arrows <- matrix(arrows, nrow=n, ncol=n)
arrowhead.length <- matrix(arrowhead.length, nrow=n, ncol=n)

vertex.col <- rep(vertex.col, length=n)
vertex.pch <- rep(vertex.pch, length=n)
vertex.cex <- rep(vertex.cex, length=n)
edge.col <- matrix(edge.col, nrow=n, ncol=n)
edge.lwd <- matrix(edge.lwd, nrow=n, ncol=n)
edge.lty <- matrix(edge.lty, nrow=n, ncol=n)

if (!is.null(circles)) {
  circles.fg <- rep(circles.fg, length=n)
  circles.bg <- rep(circles.bg, length=n)
  circles.lwd <- rep(circles.lwd, length=n)
  circles <- rep(circles, length=n)
  if (!is.null(edge.offsets))
    edge.offsets <- rep(edge.offsets, length=n)
  else 
    edge.offsets <- circles 
  if (is.null(pad)) pad <- max(circles)
}

label.offsets <- matrix(label.offsets, nrow=2, ncol=n)
label.adj <- matrix(label.adj, nrow=2, ncol=n)
label.cex <- rep(label.cex, length=n)
label.col <- rep(label.col, length=n)


#Determine coordinate placement

   if(!is.null(coord)){ 
      #If the user has specified coords, override all other considerations
      x<-coord[,1]
      y<-coord[,2]
   }else if(mode=="princoord"){ 
      #Place using the eigenstructure of the correlation matrix
      cd<-cor(rbind(d,t(d)),use="pairwise.complete.obs")
      cd<-replace(cd,is.na(cd),0)
      e<-eigen(cd,symmetric=TRUE)
      x<-Re(e$vectors[,1])
      y<-Re(e$vectors[,2])
   }else if(mode=="eigen"){ 
      #Place using the eigenstructure of the adjacency matrix
      e<-eigen(d,symmetric=(gmode!="digraph"))
      x<-Re(e$vectors[,1])
      y<-Re(e$vectors[,2])
   }else if(mode=="mds"){
      #Place using an MDS on euclidean distances by position
      #Build the (Euclidean) distance matrix
      Dmat<-matrix(nrow=n,ncol=n)
      diag(Dmat)<-0
      for(i in 1:n)
         for(j in 1:n)
            if(i>j)
               Dmat[i,j]<-sqrt(sum(abs(d[i,]-d[j,]))+sum(abs(d[,i]-d[,j])))
      Dmat[upper.tri(Dmat)]<-t(Dmat)[upper.tri(Dmat)]
      #Build the identity matrix
      Imat<-matrix(nrow=n,ncol=n)
      Imat[,]<-0
      diag(Imat)<-1
      #Construct the centering Matrix
      Hmat<-Imat-(1/n)*rep(1,n)%*%t(rep(1,n))
      #Construct the A squared distance matrix
      Amat<--0.5*Dmat^2
      #Now, finally, build the matrix of rescaled distances
      Bmat<-Hmat%*%Amat%*%Hmat
      e<-eigen(Bmat)
      x<-Re(e$vectors[,1])
      y<-Re(e$vectors[,2])
   }else if(mode=="random"){   #Uniform random placement
      x<-runif(n,-1,1)
      y<-runif(n,-1,1)
   }else if(mode=="lucircle"){ #Place evenly in lower and upper circular arcs
      if (lower.arc < 0 | lower.arc > 360)
        stop("`lower.arc' must be between 0 and 360 degrees.")
      lower.arc <- (pi/180)*lower.arc  # convert degrees to radians
      upper.arc <- 2*pi - lower.arc
      if (!all(lower.sel %in% 1:n))
        stop(paste("`lower.sel' must be a vector of vertex indices for the lower arc, in the range,",1:n))
      upper.sel <- (1:n)[-lower.sel]
      l <- length(lower.sel)
      u <- n - l 
      u.sep <- upper.arc / (u+1)
      l.sep <- lower.arc / (l+1)
      u.n <- 2*pi / u.sep
      l.n <- 2*pi / l.sep
      x.u <- sin(2*pi*((0:(u-1))/u.n) - upper.arc/2 + u.sep)
      y.u <- cos(2*pi*((0:(u-1))/u.n) - upper.arc/2 + u.sep)
      x.l <- sin(2*pi*((0:(l-1))/l.n) + upper.arc/2 + l.sep)
      y.l <- cos(2*pi*((0:(l-1))/l.n) + upper.arc/2 + l.sep)
      x <- vector(mode="numeric", length=n)
      y <- vector(mode="numeric", length=n)
      x[lower.sel] <- x.l
      x[upper.sel] <- x.u
      y[lower.sel] <- y.l
      y[upper.sel] <- y.u
   }else if(mode=="circle"){    # Place evenly in a circle, in order
      x<-sin(2*pi*((0:(n-1))/n))
      y<-cos(2*pi*((0:(n-1))/n))
   }else if(mode=="circrand"){   #Random placement around the unit circle
      tempd<-rnorm(n,1,0.25)
      tempa<-runif(n,0,2*pi)
      x<-tempd*sin(tempa)
      y<-tempd*cos(tempa)
   }else if(mode=="rmds"){   #MDS from the MVA library
      require(mva)
      tempmds<-cmdscale(dist(d))
      x<-tempmds[,1]
      y<-tempmds[,2]
   }else if(mode=="geodist"){   #MDS of geodesic distances
      require(mva)
      tempmds<-cmdscale(as.dist(geodist(d)$gdist))
      x<-tempmds[,1]
      y<-tempmds[,2]
   }else if(mode=="adj"){   #MDS of adjacency structure as similarities
      require(mva)
      tempmds<-cmdscale(as.dist(-d+max(d)))
      x<-tempmds[,1]
      y<-tempmds[,2]
   }else if(mode=="seham"){   #MDS of SE distance (Hamming)
      require(mva)
      temp<-sedist(d)
      tempmds<-cmdscale(as.dist(temp))
      x<-tempmds[,1]
      y<-tempmds[,2]
   }


#Jitter the coordinates if desired, i.e., add a bit of noise

if (jitter){
  x<-jitter(x)
  y<-jitter(y)
}


#Which nodes should we use?  Forget this and the whole sna package...

#use <- displayisolates | (!is.isolate(d,ego=1:dim(d)[1]))   


#Create plot and add the vertices and, optionally, the circles

if (is.null(pad)) pad <- 0
par(pty="s", xpd=NA) # square plot with device clipping
plot.new()
plot.window(xlim=c(min(x)-pad,max(x)+pad),ylim=c(min(y)-pad,max(y)+pad))
title(xlab=xlab,ylab=ylab,main=main)
if (axes) {axis(1);axis(2);box()}

for (i in 1:n) {
   if (!is.null(circles))
    symbols(x[i], y[i], circles=circles[i], add=TRUE, inches=FALSE, fg=circles.fg[i], bg=circles.bg[i], lwd=circles.lwd[i])

   if (vertex.pch[i] < 1)
     type <- "n"
   else
     type <- "p"
   points(x[i],y[i],type=type,pch=vertex.pch[i],col=vertex.col[i],cex=vertex.cex[i])
}


# Draw the edges (and loops - currently on TODO list)

for (i in 1:(n-1))
   for (j in (i+1):n)
      if (d[i,j] > thresh | d[j,i] > thresh) {

        xi.tmp <- x[i]
	xj.tmp <- x[j]
	yi.tmp <- y[i]
	yj.tmp <- y[j]

        # Apply offsets to x and y, if non-null
	
        if (!is.null(edge.offsets)) {
          if (round(y[j]-y[i],digits=6)==0 & x[j] != x[i]) {
	  
	    ###Perfectly horizontal line; zero slope
	    
	    xi <- x[i] - edge.offsets[i]
	    yi <- y[i]
	    xi[2] <- x[i] + edge.offsets[i]
	    yi[2] <- y[i]
	    xj <- x[j] - edge.offsets[j]
	    yj <- y[j]
	    xj[2] <- x[j] + edge.offsets[j]
	    yj[2] <- y[j]
	  } else if (round(x[j]-x[i],digits=6)==0 & y[j] != y[i]) {
	  
	    ###Perfectly vertical line; infinite slope

	    xi <- x[i]
	    yi <- y[i] - edge.offsets[i]
	    xi[2] <- x[i]
	    yi[2] <- y[i] + edge.offsets[i]
	    xj <- x[j]
	    yj <- y[j] - edge.offsets[j]
	    xj[2] <- x[j]
	    yj[2] <- y[j] + edge.offsets[j]
	  } else {

	    ###Finite slope
	    
            m <- (y[j]-y[i])/(x[j]-x[i])
	    # Find all 4 insections of line and offset circles i,j
	    xi <- (-sqrt(1+m^2)*edge.offsets[i]+x[i]+m^2*x[i])/(1+m^2)
	    yi <- -m*edge.offsets[i]/sqrt(1+m^2)-m*x[i]+x[i]*(m+m^3)/(1+m^2)+y[i]
	    xi[2] <- (sqrt(1+m^2)*edge.offsets[i]+x[i]+m^2*x[i])/(1+m^2)
	    yi[2] <- m*edge.offsets[i]/sqrt(1+m^2)-m*x[i]+x[i]*(m+m^3)/(1+m^2)+y[i]
	    xj <- (-sqrt(1+m^2)*edge.offsets[j]+x[j]+m^2*x[j])/(1+m^2)
	    yj <- -m*edge.offsets[j]/sqrt(1+m^2)-m*x[j]+x[j]*(m+m^3)/(1+m^2)+y[j]
	    xj[2] <- (sqrt(1+m^2)*edge.offsets[j]+x[j]+m^2*x[j])/(1+m^2)
	    yj[2] <- m*edge.offsets[j]/sqrt(1+m^2)-m*x[j]+x[j]*(m+m^3)/(1+m^2)+y[j]
	  }

          # Calculate all 4 distances and pick shortest
	  
	  d11 <- sqrt((xi[1]-xj[1])^2+(yi[1]-yj[1])^2)
	  d12 <- sqrt((xi[1]-xj[2])^2+(yi[1]-yj[2])^2)
	  d21 <- sqrt((xi[2]-xj[1])^2+(yi[2]-yj[1])^2)
	  d22 <- sqrt((xi[2]-xj[2])^2+(yi[2]-yj[2])^2)
	  ind <- list(c(1,1),c(1,2),c(2,1),c(2,2))
	  ind <- ind[[which.min(c(d11,d12,d21,d22))]]
	  xi.tmp <- xi[ind[1]]
	  yi.tmp <- yi[ind[1]]
	  xj.tmp <- xj[ind[2]]
	  yj.tmp <- yj[ind[2]]
        }
	

        # Draw arrows or segments between vertices

	if (d[i,j] > 0 & arrows[i,j] > 0)
          arrows(xi.tmp,yi.tmp,xj.tmp,yj.tmp,length=arrowhead.length[i,j],angle=arrowhead.angle,col=edge.col[i,j],lty=edge.lty[i,j],lwd=edge.lwd[i,j])
	else
          segments(xi.tmp,yi.tmp,xj.tmp,yj.tmp,col=edge.col[i,j],lty=edge.lty[i,j],lwd=edge.lwd[i,j])
	
	if (d[j,i] > 0 & arrows[j,i] > 0)
          arrows(xj.tmp,yj.tmp,xi.tmp,yi.tmp,length=arrowhead.length[j,i],angle=arrowhead.angle,col=edge.col[j,i],lty=edge.lty[j,i],lwd=edge.lwd[j,i])
	else
          segments(xj.tmp,yj.tmp,xi.tmp,yi.tmp,col=edge.col[j,i],lty=edge.lty[j,i],lwd=edge.lwd[j,i])
	

	# Draw edge labels

	if (display.edge.labels) {
	  l <- sqrt((xj.tmp-xi.tmp)^2 + (yj.tmp-yi.tmp)^2)
	  m <- (yj.tmp-yi.tmp)/(xj.tmp-xi.tmp)
	  m2 <- sqrt(m^2+1)
          if (d[i,j] > 0) {
            if (xj.tmp >= xi.tmp) {
	      xl <- xj.tmp - edge.label.pos*l/m2
	      if (yj.tmp >= yi.tmp) xoff <- -edge.label.offset
	        else xoff <- edge.label.offset
	    } else {
	      xl <- xj.tmp + edge.label.pos*l/m2
	      if (yj.tmp >= yi.tmp) xoff <- edge.label.offset
	        else xoff <- -edge.label.offset
	    }
            yl <- m*(xl - xi.tmp) + yi.tmp
	    yoff <- edge.label.offset
	    points(xl,yl,pch=16,cex=edge.label.cex)
	    text(x=xl+xoff, y=yl+yoff, labels=d[i,j], cex=label.cex[i])
	  }
          if (d[j,i] > 0) {
            if (xj.tmp >= xi.tmp) {
	      xl <- xi.tmp + edge.label.pos*l/m2
	      if (yj.tmp >= yi.tmp) xoff <- -edge.label.offset
	        else xoff <- edge.label.offset
	    } else {
	      xl <- xi.tmp - edge.label.pos*l/m2
	      if (yj.tmp >= yi.tmp) xoff <- edge.label.offset
	      else xoff <- -edge.label.offset
	    }
            yl <- m*(xl - xi.tmp) + yi.tmp
	    yoff <- edge.label.offset
	    points(xl,yl,pch=16, cex=edge.label.cex)
	    text(x=xl+xoff, y=yl+yoff, labels=d[j,i], cex=label.cex[j])
	  }
	}
      }



# Add text labels at each vertex; auto placement of labels is really
#     only good for circular placements of nodes

for (i in 1:n) 
   if (!is.null(labels[i])) {
     if (label.offsets[1,i] == "auto" | label.offsets[2,i] == "auto") {
       label.offsets[,i] <- c(edge.offsets[i]*sign(x[i]),edge.offsets[i]*sign(y[i]))
       if (round(x[i],digits=4) == 0) label.offsets[2,i] <- 1.20*as.numeric(label.offsets[2,i])
       if (round(y[i],digits=4) == 0) label.offsets[1,i] <- 1.20*as.numeric(label.offsets[1,i])
       label.adj[,i] <- c(-sign(x[i])+1, -sign(y[i])+1)/2
     }
     text(x[i]+as.numeric(label.offsets[1,i]),y[i]+as.numeric(label.offsets[2,i]),labels[i],adj=label.adj[,i],cex=label.cex[i],col=label.col[i])
   }   

par(pty="m", xpd=FALSE)

}


grid.legend.2<-
function (pch, col, labels, frame = TRUE, hgap = unit(0.5, "lines"), 
    vgap = unit(0.5, "lines"), default.units = "lines", gp = gpar(), 
    draw = TRUE, vp = NULL) 
{


# Adapted from an the grid.legend function in the old grid package
#     Can be used to put a legend in each panel of a lattice plot
#      when used in combination with the new panel.superpose.3 function
#     NK   19-June-2008

    labels <- as.character(labels)
    nkeys <- length(labels)
    if (length(pch) != nkeys) 
        stop("'pch' and 'labels' not the same length")
    if (!is.unit(hgap)) 
        hgap <- unit(hgap, default.units)
    if (length(hgap) != 1) 
        stop("'hgap' must be single unit")
    if (!is.unit(vgap)) 
        vgap <- unit(vgap, default.units)
    if (length(vgap) != 1) 
        stop("'vgap' must be single unit")
    legend.layout <- grid.layout(nkeys, 3, widths = unit.c(unit(2, 
        "lines"), max(unit(rep(1, nkeys), "strwidth", as.list(labels))), 
        hgap), heights = unit.pmax(unit(2, "lines"), vgap + unit(rep(1, 
        nkeys), "strheight", as.list(labels))), just="top")
    fg <- frameGrob(layout = legend.layout, vp = vp, gp = gp)
    for (i in 1:nkeys) {
        gp <- gpar(col=col[i])    # change color for each point
        fg <- placeGrob(fg, pointsGrob(0, 0.5, pch = pch[i], gp=gp), 
            col = 1, row = i)
        fg <- placeGrob(fg, textGrob(labels[i], x = 0, y = 0.5, 
            just = "left", gp=gpar(col="black")), col = 2, row = i)
    }
    if (draw) 
        grid.draw(fg)
    fg
}
gsd <- 
function(x, na.rm=FALSE, force=FALSE)
{

# Calculate the geometric standard deviation of raw data
#   by taking in sequence the natural logarithm, the arithmetic
#   standard deviation, and the exponential

# if force==TRUE, then values of zero or smaller are removed,
#   and a warning given, otherwise a fatal error is issued.

if (any(x <= 0))
  if (force) {
    m <- sum(x <= 0)
    x <- x[x > 0]
    warning(paste(m,"value(s) equal to zero or smaller was(were) removed."))
  } else
    stop("`x' must contain only positive values.")
exp(sd(log(x), na.rm=na.rm))


}
integrate.curve<-
function (x, y, n=50, plot=FALSE) {

# \alias{integrate.curve}
# \title{Integrate along a curve}
# \description{Integrates y-values along a curve}
# \details{The function first fits a spline to the x-y data and then
# integrates the y-values along the curve in small linear segments
# to estimate.   The sum of the resulting values is returned to obtaine
# an overall curve integration.
#  The number of segments (bins) to use is given by \code{n}.

# I'm sure what this is for or if it really works....

if (length(x) != length(y))
   stop("x and y must be of the same length.")

fit <- spline(x,y,n=n)

sr <- sqrt(diff(fit$x)^2 + diff(fit$y)^2) 
ay <- (fit$y[-n] + fit$y[-1])/2
h <- sr * ay / diff(fit$x)

if (plot) {
  par(mfrow=c(2,1))
  plot(fit$x[-n],h, type="b")
  plot(x,y,pch=16)
  points(fit$x,fit$y,type="l")
  par(mfrow=c(1,1))
}
cat("Area under integrated curve:\n")
sum(h)

}
is.distrib<-
function(x)
inherits(x,"distrib")
log.axis <-
function (side, limits, exp.format=TRUE,
          grid=TRUE, grid.lty="solid", grid.col="lightgray", grid.lwd=1,
          major=TRUE, minor=TRUE, label.minor=FALSE,
	  minor.cex=0.75, major.cex=1, ...)
{

# \name{log.axis}

# \title{Add nice log-10 axis tics and labels to a plot}

# \description{Function to add a nice log base-10 axis
# to a plot, with major/minor tics and optional gridlines.
# See \code{log.tics} for details on generating tic positions.}

if (!side %in% 1:4)
   stop("Side must be 1, 2, 3, or 4.")

if ((side == 1 || side == 3) & !par("xlog"))
    stop("x-axis must be on a log scale")
if ((side == 2 || side == 4) & !par("ylog"))
    stop("y-axis must be on a log scale")

if (missing(limits)) {
   if (side == 1 || side == 3)
      limits <- par("usr")[1:2]
   else
      limits <- par("usr")[3:4]
   limits <- 10^limits      
}

# Shrink limits by a buffer
# :: Currently we just plot the lines if they fit inside the
# plot area;  probably better to err on giving more axis info to
# your audience, since log axes can be a bit confusing
#if ((side == 1 || side == 3) & par("xaxs") == "r")
#   limits <- c(limits[1]+0.04*diff(limits),limits[2]-0.04*diff(limits))
#if ((side == 2 || side == 4) & par("yaxs") == "r")
#   limits <- c(limits[1]+0.04*diff(limits),limits[2]-0.04*diff(limits))


tics <- log.tics(limits, exact10=FALSE)
maj <- tics$major.tics
#maj <- maj[maj < limits[2] & maj > limits[1]]
min <- tics$minor.tics
#min <- min[min < limits[2] & min > limits[1]]
# Remove minor tics coinciding with major tics
min <- min[!min %in% maj]
# skip over minor tics by value of `minor.skip', default=1
#min <- min[seq(length(min),1,by=-minor.skip)]

min.exp <- log10(min)
maj.exp <- log10(maj)

# draw major tics
if (major) {
   par(tcl=-0.5)
   if (exp.format)
     axis(side,maj,labels=parse(text=paste("10^",maj.exp)), cex.axis=major.cex,...)
   else
     axis(side,maj,labels=formatC(maj,format="fg"), cex.axis=major.cex,...)
   # draw grid lines at major tic positions if desired
   if (grid){
      if (side == 1 || side == 3)
        abline(v=maj, col=grid.col, lty=grid.lty, lwd=grid.lwd)
      else
        abline(h=maj, col=grid.col, lty=grid.lty, lwd=grid.lwd)
   }
}

# draw minor tics
if (minor) {
   par(tcl=-0.25)
   if (label.minor) { # Draw labels at minor tic positions?
      if (exp.format)
        axis(side, min, labels=parse(text=paste("10^",min.exp)), cex.axis=minor.cex, ...)
      else
        axis(side, min, labels=formatC(min, format="fg"), cex.axis=minor.cex, ...)
   } else 
      axis(side, min, labels=F, ...)
   # draw grid lines at minor tic positions if desired
   if (grid) {
      if (side == 1 || side == 3)
         abline(v=min, col=grid.col, lty=grid.lty, lwd=grid.lwd)
      else
         abline(h=min, col=grid.col, lty=grid.lty, lwd=grid.lwd)
   }
   # reset tic length
   par(tcl=-0.5)
}

}

log.tics<-
function (x, exact10=TRUE)
{

# \name{log.tics}

# \title{Calculate nice tics for log axes}

# \description{Find nice log base-10 axis tics.
# Values in x less than 0 are removed.
# Can set axis tics to exact multiples of
# ten, e.g., 0.1, 1, 10, if \code{exact10},
# or nearest 10^i increment, e.g., 0.2 or 0.3 (for i = -1), otherwise.}

# \arguments{\item{x} a vector of data or data range for which tics will be created}

# \value{a list containing the input data range (from x), the calculated tic range,
# and the calculated major and minor tic values.}

# UPDATE:  tolerance for getting close to an exact10 tic.  See lpplot for
#         more.....

#  UPDATE:  Fixed exact10 calculation for limits equal to exactly 0.1/10
#                          --NK 13-Mar-04

# ------------------

x<-as.numeric(x)
xlim <- range(x, na.rm=TRUE)

if (any(x <= 0)) {
   warning("Some x data <= zero. Setting to NA.")
   x[x <=0] <- NA
   ok <- complete.cases(x)
   x<-x[ok]
   xlim <- range(x, na.rm=T)
}

x2 <- ifelse(xlim[2]<=0.1,trunc(log10(xlim[2])-0.0001),trunc(log10(xlim[2])+0.9999))
x1 <- ifelse(xlim[1]>=10,trunc(log10(xlim[1])),trunc(log10(xlim[1])-0.9999))
if (!exact10) {
   xlim[1] <- 10^x1*trunc(xlim[1]/10^x1)
   xlim[2] <- 10^(x2-1)*ceiling(xlim[2]/10^(x2-1))
} else {
   xlim <- c(10^x1,10^x2)
}
tics.min <- c()
tics.maj <- c()
if (10^x1 >= xlim[1])
   tics.maj <- as.numeric(formatC(10^x1,format="fg"))
for (i in x1:(x2-1)) {
    if (10^(i+1) <= xlim[2])
       tics.maj <- c(tics.maj, as.numeric(formatC(10^(i+1),format="fg")))
    f <- ifelse(i==x1, xlim[1] / 10^x1, 1)
    e <- ifelse(i==(x2-1), xlim[2], 10^(i+1)-10^i)
    tics.min <- c(tics.min, seq(f*10^i, e, by=10^i))
}

list(true.range=range(x), tic.range=range(tics.min),
     minor.tics=tics.min, major.tics=tics.maj)

}
lo<-
function (n, mat=TRUE, land=TRUE, byrow=TRUE, respect=TRUE, show=FALSE) {

# lo - creates a lay-out for n equal-size plots
# n is the number of plots to layout.
# This function sets up the screen to fit the number of
# plots specified using the layout() function. 
# All plot spaces are of equal size.
# The layout is square if possible. If there is not an
# even square root of the number of plots, then the layout
# is rectangular.  In this case, the layout can be either
# landscape (land=TRUE) where there are more plots horizontally
# than vertically, or portrait (land=FALSE) where there
# are more plots vertically than horizontally.
#
# Written by Neil E. Klepeis, 11 Aug 2000

# Updated:  27 Sep 2000

# Now only plots n plots;  before it completed the matrix before
# starting to plot at the top once again

# Now has option to create a layout for a sinlge row or column 
# of plots.  If mat = TRUE a layout for a matrix of plots is
# generated.  Otherwise a single row or column is generated. For
# single row, byrow=TRUE, for a single column, byrow=FALSE

near<-floor(sqrt(n))
root<-sqrt(n)

if (mat) {
  if (near == root) {
      numx<-near
      numy<-near
  } else 
       if ((n-near**2 <=near) && land) {
          numx<-near+1
          numy<-near
       } else 
            if ((n-near**2<=near && !land)) {
               numx<-near
               numy<-near+1
            } else {
               numx<-near+1
               numy<-near+1
            }
  m <- matrix(1:(numx*numy),nrow=numy, ncol=numx, byrow=byrow)
  m[m > n] <- 0
  layout(m,respect=respect)
}
else {
  if (byrow) layout(matrix(1:n,nrow=n), respect=respect)
  else layout(matrix(1:n,ncol=n),respect=respect)
}

if (show) layout.show(n)

#list(nplots=n, xcols=numx, ycols=numy)

}
lpplot.data<-
function (y, ylim, main = "Log-Probability Plot",
		 xlab = "Standard Normal Probability (%)", 
    		ylab = "Sample Quantiles", plot.it = TRUE,
		overlay = FALSE, forceNA = FALSE, ...) 
{

# create a log-probability plot and add data specified by y

    if (forceNA)
	NA->y[y<=0]
    y <- y[!is.na(y)]
    if (0 == (n <- length(y))) 
        stop("Sample vector is empty.") 
    if (missing(ylim)) 
        ylim <- range(y)
    if (ylim[1] < 0 )
        stop("Samples values must be greater than zero.")
    x <- qnorm(ppoints(n))[order(order(y))]
    xlim <- c(qnorm(0.0001),qnorm(0.9999))
    if (plot.it) {
	par(tcl=-0.5,ps=12,font=3,lwd=2,las=0)
	if (!overlay) {
	   par(ann=TRUE,new=FALSE)
	}	
	if (overlay) {
	   par(ann=F,new=T)
	   oldylim<-par()$yaxp
	   ylim[2] <- oldylim[2]
	   ylim[1] <- oldylim[1] 
	}
        y2 <- ifelse(ylim[2]<0.1,trunc(log10(ylim[2])),trunc(log10(ylim[2])+0.99))
	y1 <- ifelse(ylim[1]>10,trunc(log10(ylim[1])),trunc(log10(ylim[1])-0.99))
	ylim <- c(10^y1,10^y2)
        plot(x, y, main = main, xlab = xlab, ylab = ylab, ylim = ylim, 
               axes=F,log="y",xlim = xlim,...)
#	if (overlay) {
	   box()
	   par(lwd=1,tcl=-0.5,ps=8,lty=1)
	   axis(1,qnorm(c(0.0001,0.001,0.01,0.05,0.1,0.25,0.5,
			0.75,0.9,0.95,0.99,0.999,0.9999)),label=F)
    	   par(tck=1,lty=3)
	   axis(1,qnorm(c(0.0001,0.001,0.01,0.05,0.1,0.25,0.5,
		       0.75,0.9,0.95,0.99,0.999,0.9999)),
	           c(0.01,0.1,1,5,10,25,,50,75,90,95,99,99.9,99.99))
	   for (i in y1:(y2-1)) {
	        par(tck=1,lty=3)
	        axis(2,seq(10^i,10^(i+1),by=10^i),labels=F)
	        axis(2,10^i,labels=formatC(10^i,format="fg"))
	        par(tcl=-0.5,lty=1)
	        axis(2,10^i,labels=F)
	   }
	   axis(2,10^y2,labels=formatC(10^y2,format="fg"))
	   par(tcl=-0.5,lty=1)
#        }
    }
    invisible(list(x = x, y = y))
}
lpplot<-
function (xlim, ylim, grid=TRUE, grid.lty="solid", grid.col="lightgray",
          main = "Log-Probability Plot",
	  xlab = "Standard Normal Cumulative Probability (%)", 
     	  ylab = "Sample Quantiles",
	  bty="o", axes = TRUE, cex.main=1, cex.lab=1, cex.axis=1, las=1,
	  tck.minor=-0.009, tck.major=-0.017,
	  xtic.minor=FALSE, ...) 
{

# This function simply creates an empty log-probability
# plot with nice tics and, optionally, grids.

# See the 'add.data.lp', 'add.hist.lp', and 'add.lnorm.lp' R functions
# to add curves from data, histograms, or a lognormal model,
# respectively, to the plot.

# On the x axis are normal quantiles labeled as normal probabilities
# On the y axis are the data (sample) quantiles (corresponding to
# the cumulative probabilities, converted to normal quantiles and
# plotted on the x axis)

# This is a type of Q-Q (quantile-quantile) plot commonly used to
# judge how data conform to a specific model or data vector.

# If the curve appears as a straight line on this plot, then
# the data are lognormally distributed.

# UPDATE:  Changed tolerance for being over an upper even power of
#           ten limit or under a lower one to be less, so that
#           we need to be within something like 1 in 10^4 to 
#           trip a new upper limit to retain a lower limit.

#  UPDATE:  Fixed exact10 calculation for limits equal to exactly 0.1/10
#                          --NK 13-Mar-04

# UPDATE:  Now we allow user-defined limits for the probability axis,
#          and we check the xlim and ylim values. 
#                                        --NK 13-Mar-04

# ------------------------



if (missing(xlim))
  xlim <- qnorm(c(0.0001, 0.9999))
else if (!is.vector(xlim) | !is.numeric(xlim) | 
          length(xlim) != 2 | any(xlim <= 0) | any(xlim >= 1))
       stop("`xlim' must be a two-element numeric vector containing the lower and upper limits of the horizontal probablility axis, with values between 0 and 1.")
     else xlim <- qnorm(xlim)

#xlim <- c(qnorm(0.0001),qnorm(0.9999))

if (missing(ylim))
  ylim <- c(0.1,1000)
else if (!is.vector(ylim) | !is.numeric(ylim) | 
         length(ylim) !=2 | ylim[1] <=0 | diff(ylim) <=0)
       stop("`ylim' must be a two-element numeric vector containing the lower and upper limit of the vertical axis with values greater than 0.")

#if (ylim[1] <=0)
#   stop("Lower y limit must be greater than 0.")

#par(xaxs="i", yaxs="i")
#par(ps=12,font=3,lwd=1,las=0)


# Get closest vertical axis limit to an even power of 10.
y2 <- ifelse(ylim[2]<=0.1,trunc(log10(ylim[2])-0.0001),trunc(log10(ylim[2])+0.9999))
y1 <- ifelse(ylim[1]>=10,trunc(log10(ylim[1])),trunc(log10(ylim[1])-0.9999))
ylim <- c(10^y1,10^y2)

#par(usr=c(qnorm(0.0001),qnorm(0.9999),ylim[1],ylim[2]))

plot.new()
plot.window(xlim, ylim, log="y") 
title(main=main, xlab=xlab, ylab=ylab, cex.main=cex.main, cex.lab=cex.lab)


# x axis tics with labels
xtics <- if (xtic.minor) tck.minor else tck.major

# enough probabilities for anyone
all.probs <- qnorm(c(0.0000001, 0.000001, 0.00001, 0.0001,0.001,0.01,
                     0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,0.999,
		     0.9999,0.99999, 0.999999, 0.9999999))
all.names <- c(0.00001, 0.0001,0.001,0.01,0.1,1,5,10,25,,50,75,90,95,99,99.9,
               99.99,99.999,99.9999, 99.99999)
probs <- all.probs[all.probs >= xlim[1] & all.probs <= xlim[2]]	       
prob.names <- all.names[all.probs >= xlim[1] & all.probs <= xlim[2]]	       

# x axis grid lines 
if (grid) 
   axis(1,probs,label=FALSE,
	lty=grid.lty, col=grid.col, tck=1, cex.axis=cex.axis, las=las, ...)
if (axes)
  axis(1,probs, prob.names,
       lwd=1, lty=1, cex.axis=cex.axis, las=las, tck=xtics, ...)

# y axis tics and grids
for (i in y1:y2) {
    if (grid) 
       # grid lines over minor tics
       axis(2,seq(10^i,10^(i+1),by=10^i),labels=F, lty=grid.lty, col=grid.col, tck=1)
    if (axes) {
      # major y tics
      axis(2,10^i,labels=formatC(10^i,format="fg"), lty=1, tck=tck.major, cex.axis=cex.axis, las=las, ...)
      # minor y tics
      axis(2,seq(10^i,10^(i+1),by=10^i),labels=FALSE, lty=1, tck=tck.minor, cex.axis=cex.axis, las=las, ...)
    }
}

box(bty=bty)
#par(tcl=-0.5,lty=1)

}
mond<-
function (z, colors, levels = 4, pretty=F, dep=T, nlab = 5,  
	  samples=F, plot=T, legend=F, xlab, ylab, main, ...)

{ 

# mond - An R function to create a "Mondrian Plot" from
# an R dataframe object: z.  Variates are placed on the
# horizontal axis with records sorted in a nested fashion
# along the vertical axis.  The user specifies the number
# of levels and the colors used to represent the levels:
# defaults are 4 levels and colors<-0:(levels-1), respectively.
# The user also specifies a frequency or sample size vertical
# axis and the number of labels for the vertical axis.
# Levels should be the number of levels for all variates (scalar)
# or the number of levels for each variate (vector), or the
# actual split points for each variate (list).   If a scalar
# or vector is specified for levels, even splits can be used
# (pretty=F), or the pretty function can be used to create
# nice looking splits (pretty=T) based on the number of
# levels specified.
# ------------------------
# Note:  If the cutting procedure results in NA values
# these will be placed at the "high" end of the resulting
# recoded values (at the top of each color block in the
# plot).  The NA's should, of course, be avoided by choosing
# splits that span the whole range. You will notice them
# because some weird white space keeps showing up where
# it shouldn't be.
# Note 2:  The above problem was fixed by setting
# include.lowest=TRUE in the cut function. Now
# hist properly checks that the splits span the
# whole data range.   8 Sep 00 -nk
# ------------------------------------
# Added capability to plot independent variates in each
# column.  8-Sep-00 -nk
# -----------------------------------------------------
# Copyright (C) 2000  Neil E. Klepeis
# <nklepeis@uclink4.berkeley.edu>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License (GPL)
# as published by the Free Software Foundation (FSF); either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program (see "GNU_GPL.txt" file); if not,
# write to the Free Software Foundation, Inc., 59 Temple Place,
# - Suite 330, Boston, MA 02111-1307, USA.
# -----------------------------------------------------

if (!is.data.frame(z))
   stop("The first argument must be a dataframe with >=2 variates (columns)")
if (missing(main))
   main<-paste("Mondrian Plot: ", deparse(substitute(z)))
n<-length(row.names(z))
m<-length(names(z))

# save raw (uncoded) splits for each variate
splits<-list()
if (is.vector(levels) && !is.list(levels) && length(levels) == 1 ) {
   for (i in 1:m) {
       if (pretty) {splits[[i]]<-pretty(z[[i]],n=levels)}
       else {
         int<-(max(z[[i]])-min(z[[i]]))/levels
         splits[[i]]<-c(min(z[[i]])-0.01*int,seq(min(z[[i]])+int,max(z[[i]])-int,by=int), max(z[[i]]+0.01*int))
       }
   }
}
if (is.vector(levels) && length(levels) > 1 && !is.list(levels) ) {
   if (length(levels) != m)
     stop("Vector levels specification is wrong length")
   int<-vector()
   for (i in 1:m) {
       if (pretty) {splits[[i]]<-pretty(z[[i]], n=levels[i]) }
       else {
         int[i]<-(max(z[[i]])-min(z[[i]]))/levels[i]
         splits[[i]]<-c(min(z[[i]])-0.01*int[i],seq(min(z[[i]])+int[i],max(z[[i]])-int[i],by=int[i]),max(z[[i]])+0.01*int[i])
       }
   }
}
if (is.list(levels)) {
   if ((length(levels) != m) && (length(levels) != 1))
       stop("List levels specification has wrong no. of components")
   if (length(levels) == 1) { 
       warning("A single levels specification will be used for all variates.")
       for (i in 1:m)
           splits[[i]]<-levels[[1]]
   }
   if (length(levels) == m) 
      for (i in 1:m) 
          splits[[i]]<-levels[[i]]
}

# check for maximum number of colors needed
if (missing(colors)) {
   colors<-0:1
   for (i in 1:m) {
       if (length(splits[[i]]-1) > length(colors)) colors<-0:(length(splits[[i]])-2)
   }
}

# use the hist function to check that splits span the data
for (i in 1:m) brks<-hist(z[[i]], breaks=splits[[i]], plot=F)$breaks

# get split counts and boundaries for root variate
cnts<-hist(z[[1]], breaks=splits[[1]], plot=F)$counts
cumcnts<-cumsum(cnts)
cumperc<-cumcnts/n

# now recode each variate according to levels 
for (i in 1:m) 
   z[[i]]<-as.numeric(cut(z[[i]], br=splits[[i]],include.lowest=TRUE, labels=F))

# get indices for nested sort
zord<-do.call("order",z[,1:m])

# set plot attributes
if (missing(xlab))
   xlab<-"Variates"
if (missing(ylab) && samples)
   ylab<-paste("Samples (Data Records), n = ", n)
if (missing(ylab) && !samples)
   ylab<-paste("Sample Proportion, n = ", n)
x<-seq(0,1,by=1/(m-1))  # n values: blocks positioned horiz at midpoint
if (samples)  # n+1 values: blocks positioned vert by boundary 
   y<-0:n     # vert axis is sample number
if (!samples) # n+1 values: blocks positioned vert by boundary
   y<-seq(0,1,by=1/n)  # vert axis is sample proportion


if (plot)
  {
    old.par<-par(no.readonly=TRUE)
    if (legend) {   # divide screen to fit a legend for each variate
       nf<-layout(matrix(c(rep(1,m),2:(m+1)),nrow=2,ncol=m,byrow=T),heights=c(4,1))
       par(mar=c(5,4,4,2)+0.1)
       #layout.show(nf)
    }
    if (!legend) {  # set up space for a single plot
       layout(matrix(1))
       par(mar=c(5,4,4,2)+0.1)
    }
    image(x,y,t(data.matrix(z[zord,])),   # draw the colored squares
       col=colors,
       axes=F,
       main=main,
       xlab=xlab,
       ylab=ylab)
    axis(1,seq(0,1, by=1/(m-1)), names(z))  #horizontal axis
    if (samples) 
       axis(2, pretty(0:(n+1), n=nlab))   #vertical axis
    if (!samples)
       axis(2, pretty(0:1, n=nlab))   #vertical axis
    for (i in seq(1,2*(m-1)-1, by=2))  # vertical lines
        lines(rep(i*1/(2*(m-1)),2),c(0,n), lwd=3, col="lightgray") 
    for (i in 1:length(splits[[1]])) {   # horizontal lines
        if (samples)
           lines(c(-1/(m-1),1+1/(m-1)),rep(cumcnts[i],2),lwd=3,col="lightgray")
        if (!samples)
           lines(c(-1/(m-1),1+1/(m-1)),rep(cumperc[i],2),lwd=3,col="lightgray")
    }
    box(lwd=1, col="black")
    if (legend) {
       par(mar=c(0,3,0,4)+2)
       for (i in 1:m) {
            barplot(rep(1,length(splits[[i]])-1),space=0,hor=T,col=colors,
                    main=paste("Legend: ", names(z)[i]), axes=F)
            axis(4,0:(length(splits[[i]])-1), splits[[i]], las=1)
       }
    }
  }

if (!plot)
  list(z=z[zord,], brks1=splits[[1]], brks2=splits[[2]], cnts=cnts, cumcnts=cumcnts, cumperc=cumperc)

} # end of function
mpr<-
function (x, y, index, main=NULL, right=FALSE, cex.main=3,
          cex.lab=2, cex.text=2, ...) {

# function mpr - plot regressions
# x and y are data frames
# x specifies the key variate(s)
# y specifies the response variate(s)
# multiple scatter plots are created with least
# square linear fits of the y's versus the corresponding x's.
# The variate names are used to label each figure with the
# fitting info placed in each plot's main title area.

# Note:  a vector has a single column

# UPDATE:  Now we use a index variable like lattice does
#          and add results of fit to inside of the graph region.
#          And option to place text to right or left of graph.
#                       NK 1-February-2004
# ---------------------------------------------------
#  Example:
#  mpr(data.frame(HOWDIE=1:10),data.frame(DOODIE=1:10), 
#                 right=TRUE, cex.text=1.4)
#
# ------------------------------------------------------

if (missing(index)) index <- rep(1, length=NROW(x))

if (NROW(x) != NROW(y) || NROW(x) != NROW(index))
   stop("Number of elements for x, y, and index must be equal ")

data <- data.frame(x=x, y=y)
if (!is.null(names(x))) names(data)[1] <- names(x)[1]
if (!is.null(names(y))) names(data)[2] <- names(y)[1]

data <- split(data, f=index)
nplots <- length(data)
lo(nplots)

old.par<-par(no.readonly=TRUE)
par(cex.main=cex.main, cex.lab=cex.lab, font.main=1)
par(mar=c(5,5,4,1)+0.1)

for (i in 1:nplots) {
        x <- data[[i]][[1]]
	y <- data[[i]][[2]]
        xlab <- names(data[[i]])[1]
        ylab <- names(data[[i]])[2]
        ls.out<-lsfit(as.vector(x), as.vector(y)) 
	coeff<-ls.out$coefficients
        pcoeff<-format(ls.out$coefficients, digits=2)
        resids <- as.matrix(ls.out$residuals)
        lsqr <- ls.out$qr
        if (ls.out$intercept) {
            if (is.matrix(lsqr$qt))
                totss <- apply(lsqr$qt[-1, ]^2, 2, sum)
            else totss <- sum(lsqr$qt[-1]^2)
        } else {
             totss <- apply(as.matrix(lsqr$qt^2), 2, sum)
        }
        resss <- apply(resids^2, 2, sum, na.rm = TRUE)
        regss <- totss - resss
        rsquared <- format(regss/totss,digits=2)
        n <- apply(resids, 2, length) - apply(is.na(resids), 2, sum)
	if (is.null(main))
  	  main <- paste("Linear Regression: ",ylab,"~",xlab)
        plot(as.vector(x), as.vector(y), xlab=xlab, ylab=ylab, main=main,...)
        abline(coeff)
	results <- c(paste("Slope = ",pcoeff[2]),
	             paste("Intercept = ",pcoeff[1]),
	             paste("R-Squared = ",rsquared), paste("No. Points =", n))
        th <- diff(par()$usr[c(3,4)])/12
	x1 <- par()$usr[1] + (diff(par()$usr[c(1,2)]))/20
	if (right) x1 <- x1 + diff(par()$usr[1:2])/2
	y2 <- par()$usr[4]
	text(rep(x1,4),c(y2-th,y2-2*th,y2-3*th,y2-4*th),
	          labels=results, pos=4, cex=cex.text) 
}

par(old.par)

}
optgrid<-
function (f, par, incr, lower, upper, verbose=1, ...) {

# `optgrid' : R function to optimize an arbitrary function `f'
# of `n' parameters using a local grid search
# method. User specifies the initial parameter values `par',
# the parameter increments (precision) `incr', the lowest
# values `lower', and the highest values `upper' for
# each parameter. Extra parameters may also be passed to the
# specified function `f', ....

# The advantage of this method is that the best estimates
# of the function parameters are obtained at the precision
# specified by `incr', and the search is contrained by `upper'
# and `lower' bounds for each parameter.  The disadvantage
# is that it is slow, dumb, and inefficient.  But it works :-).

# Set verbose = 0 for no status messages
# Set verbose >= 1 to output more information:
# verbose = 1 to send each best set of parameters and function value
# verbose = 2 to output best index for the parameters (ie, 1=go lower on
#    next iteration;  2=just right;  3=go higher on next iteration
# verbose = 3 to output no. of model runs performed and no. lookups

# to do:  allow percentage increments instead of actual increment
# values;   search along a single parameter before doing the whole
# grid

# ------------------------------------------------------------

par<-as.vector(par)
incr<-as.vector(incr)
lower<-as.vector(lower)
upper<-as.vector(upper)

if (length(incr) != length(par) || 
    length(lower) != length(par) ||
    length(upper) != length(par))
    stop("`par', `incr', `lower', and `upper' must have equal length.")

if (any(upper-lower<=incr))
   stop("`upper' values must be greater than `lower' values by at least `incr'.")

if (any(par-lower<=0 || upper-par<=0))
   stop("Initial values `par' must lie between `upper' and `lower' values.")

if (missing(f))
   stop(paste("Must specify `f' -- a function of",length(par), "parameters."))

if (length(f(par,...)) > 1)
   stop("The result of function `f' must be a scalar (single-valued).")

# -----------------------------------------------------------

fn <- function (par) f(par, ...) 

#save initial parameter values; initialize

n<-length(par)  # no. of parameters
init<-par
parbest<-par
bestidx<-rep(2,n)
fbest<-1e99
a<-array(NA,rep(3,n))  # function value array

# create an array of indices for `a', the function value array

idx<-as.array(as.matrix(expand.grid(rep(list(1:3),n))))

# -----------------------------------------------------

# function to create the 3x3x... parameter value array.
# The vector `par' is considered the current best set of parameters

pva<- function () {
   lpar<-par - incr
   lpar[lpar < lower] <- lower[lower > lpar]

   hpar<-par + incr
   hpar[hpar > upper] <- upper[upper < hpar]

   mpar<-par
   mpar[mpar <= lower] <- lower[mpar <= lower] + incr[mpar <= lower]
   mpar[mpar >= upper] <- upper[mpar >= upper] - incr[mpar >= upper]

   pa<-rbind(lpar,mpar,hpar)

   as.array(pa)
}

# ----------------------------------------------------------

# function to lookup a set of parameter values in a table and return
# the associated function value if it exists.  Otherwise return NA.
# Parameter values are assumed to be concatenated to a single string,
# separated by a space.

lookup <- function (s) {
    p <- pmatch(s, ltab)
    if (!is.na(p))
       return(as.numeric(ltab[p,2]))
    else
       return(p)
}


# -----------------------------------------------------------

# function to fill the array `a' with a multi-D grid of
# function values -- 3 for each parameter.  Also updates
# the best parameter and functions values and updates
# the lookup table.

grid <- function () {
    nn <- 0  # number of new model runs
    nl <- 0  # number of lookups
    pa <- pva()   # get parameter value array
    for (i in 1:NROW(idx)) {
        fidx<- as.array(cbind(c(idx[i,]),c(1:n)))
        lu<-lookup(paste(format(pa[fidx]),collapse=" "))
        if (!is.na(lu)) {
           nl <- nl + 1
           a[array(idx[i,], c(1,n))] <- lu 
        }
        else {   
           nn <- nn + 1
           a[array(idx[i,], c(1,n))] <- fn(pa[fidx])
        }
        if (a[array(idx[i,], c(1,n))] < fbest) {
           parbest <- pa[fidx]
           bestidx <- idx[i,]
           fbest <- a[array(idx[i,], c(1,n))]
        }
        if (is.na(lu))  # add entry to lookup if not there already
           ltab <- rbind(ltab, c(paste(format(pa[fidx]),collapse=" "),
                              a[array(idx[i,], c(1,n))]))
    }
    # Note: `bestidx', `parbest', and `fbest' pass through unchanged,
    # if no better function value is found (ie, than current fbest)
    names(parbest)<-NULL
    names(bestidx)<-NULL 
    list(farray=a, ltab=ltab, fbest=fbest,
         parbestval=parbest, parbestidx=bestidx,
         nn=nn, nl=nl)
}

# --------------------------------------------------------

# function to calculate sensitivity coefficients:
# mean absolute change in optimization surface with a change in
# parameter, given all other parameters are held constant
# at their central value (not necessarily the optimum point)

sensitivity<-function (a) {

     s<-vector(length=n, mode="numeric")
     pa <- pva()  # parameter value array
     for (i in 1:n) {
         z<-array(rep(2,3*n), c(3,n))  # function value index array
         z[1:3,i] <- 1:3
         # normalized changes in surface and parameter
         dc <- mean(abs(diff(a[z])))/a[array(rep(2,n),c(1,n))]
         dp <- mean(abs(diff(pa[,i])))/pa[2,i]
         s[i] <- dc/dp
     } 
     s    
}

# ---------------------------------------------------------

# function to test for final iteration, i.e., minimum found 

test <- function (val, index) {
    if ((all(index == rep(2,n))) ||
       (val[index != rep(2,n)] == upper[index != rep(2,n)] || 
        val[index != rep(2,n)] == lower[index != rep(2,n)]) ) {
       return(TRUE)
    }
    else {
       return(FALSE)
    }
}

# ------------------------------------------------------

# Main loop

initf<-fn(par)

if (verbose >= 1) {
   cat("\n")
   cat("GRID : Initial Parameter Values: ", paste(par)," [ f=",initf,"]","\n\n")
}

# initialize lookup table
ltab <- matrix(c(paste(format(par),collapse=" "),initf), nrow=1)

num<-0
bestidx<-rep(0,n)  # zero the index so we can enter the loop

while (!test(parbest, bestidx)) {
    num <- num + 1
    if (verbose >=2) 
       cat("GRID : Starting Iteration #",num,"...","\n")
    bestidx<-rep(2,n)  #reset best index to center

    res<-grid()

    parbest<-res$parbestval
    par<-parbest
    bestidx<-res$parbestidx
    fbest<-res$fbest
    ltab <- res$ltab
    nn <- res$nn
    nl <- res$nl

    if (verbose >=1)
       cat("GRID : Completed Iteration #", num, ": ", paste(par)," [ f=",fbest,"]","\n")
    if (verbose >=3)
       cat("GRID : New Model Runs =",nn, "   Lookups =", nl, "\n")
    if (verbose >=2) 
       cat("GRID : Best Index : ",paste(bestidx), "\n\n")
}

sens<-sensitivity(res$farray)

# -------------------------------------------------------

return(list(grid=res$farray, lookup=ltab, sens=sens, value=res$fbest,
            initpar=init, par=par, iterations=num))

}
panel.areaplot<-
function (x, y, subscripts, groups, col, col.line = superpose.line$col, 
    lty = superpose.line$lty, lwd = superpose.line$lwd, base=0, ...) 
{

#    Taken from the panel.superpose.2 function, but rewritten to also
#    allow for superimposed "areaplots" -- useful for showing overlapping
#    time series of pollutant concentration or other slowly varying time
#    series.         --- Neil Klepeis, 24-Oct-2005

    if (length(x) > 0) {
        if (!missing(col)) {
            if (missing(col.line)) 
                col.line <- col
        }
        superpose.line <- trellis.par.get("superpose.line")
        x <- as.numeric(x)
        y <- as.numeric(y)
        vals <- sort(unique(groups))
        nvals <- length(vals)
        col.line <- rep(col.line, length = nvals)
        lty <- rep(lty, length = nvals)
        lwd <- rep(lwd, length = nvals)
        base <- rep(base, length = nvals)
        for (i in seq(along = vals)) {
            id <- (groups[subscripts] == vals[i])
            if (any(id)) {
	         X <- x[id]
		 Y <- y[id]
	         if (min(Y,na.rm=TRUE) < base[i])
		    base[i] <- min(Y,na.rm=TRUE)
                 o <- order(X)
                 X <- X[o]
                 Y <- Y[o]
            	 X <- c(X, rev(X))
	         Y <- c(Y, rep(base[i],length=length(Y)))
		 #print(list(X=X,Y=Y))
	         grid.polygon(x=X, y=Y,
		              gp=gpar(fill=col[i], col=col.line[i],
			 	      lwd=lwd[i], lty=lty[i]),
		              default.units="native")
             }		  
        }
    }
}
panel.lsfit<-
function (x, y, intercept=TRUE, pch = plot.symbol$pch, col,
    col.line = plot.line$col,
    col.symbol = plot.symbol$col, font = plot.symbol$font,
    fontfamily = plot.symbol$fontfamily,
    fontface = plot.symbol$fontface, lty = plot.line$lty,
    cex = plot.symbol$cex,
    lwd = plot.line$lwd, horizontal = FALSE, right=FALSE, 
    usr = c(min(x),max(x),min(y),max(y)), text.line=1/12, cex.text = 1, ...)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
#    vp <- current.viewport()
#    print(vp[c("width","height")])
    if (length(x) < 1)
        return()
    if (!missing(col)) {
        if (missing(col.line))
            col.line <- col
        if (missing(col.symbol))
            col.symbol <- col
    }
    plot.symbol <- trellis.par.get("plot.symbol")
    plot.line <- trellis.par.get("plot.line")
    lpoints(x = x, y = y, cex = cex, font = font, fontfamily = fontfamily,
            fontface = fontface, col = col.symbol, pch = pch)
    ls.out <- lsfit(x, y, intercept=intercept)
    coeff <- format(ls.out$coefficients, digits = 2)
    #require(heR.Misc)
    rsquared <- round(rsquared(ls.out), digits=2)
    panel.abline(ls.out, col = col.line, lty = lty, lwd = lwd)
    if (intercept)
      results <- c(paste("Slope = ", coeff[2]), paste("Intercept = ",
                   coeff[1]), paste("R-Squared = ", rsquared))
    else		   
      results <- c(paste("Slope = ", coeff[1]), "Intercept = 0",
                   paste("R-Squared = ", rsquared))
    th <- diff(usr[c(3, 4)])*text.line
    x1 <- usr[1] + (diff(usr[c(1, 2)]))/20
    if (right)
      x1 <- x1 + diff(usr[1:2])/2
    y2 <- usr[4]
    ltext(rep(x1, 3), c(y2 - th, y2 - 2 * th, y2 - 3 * th),
          labels = results, pos = 4, cex = cex.text)

}
panel.superbar<-
function(x, y, box.ratio=10, group.ratio=1, horizontal=TRUE, beside=FALSE, 
         subscripts, groups, col=bar.fill$col, ...)
{

# Update. something screwy going on with bwplot, so we've switched to 
#     xyplot, and deal with factors ourselves.  No we need bwplot to 
#     send in factor levels for panel data in terms of 
#     the global independent variable levels.  Deepayan sent me an updated
#     bwplot, which I am now using and calling bwplot2.

# Lattice panel function for plotting barcharts with grouping variables,
#   where x-y data for grouping factors are plotted as stacked bars.
#   This function is intended to be used with `bwplot' which deals
#   with data where either x or y is to be intepreted as a factor and
#   plotted at even integer positions instead of at arbitrary numeric
#   positions (as with `xyplot').  The factors are passed  to this 
#   function as integers and not as the actual factor level values
#   (character strings).

# Strategy:  put grouped data into a matrix format and then copy
#            the way that barplot.default handles group data (i.e., 
#            by stacking the data or plotting the base side-by-side --
#            something I am still working on).

# If horizontal=TRUE, then y is interpreted as a factor (independent variable)
#    otherwise x is interpreted as a factor.  The bars are plotted at
#    increasing integer  x or y values regardless of their actual values.
#    Bar heights (dependent variable) are matched to unique factor values
#    in each group and heights are stacked.  It is an error if there is
#    a repeated factor in a group (does bwplot check for this?)

# Rules for negative values:  negative bars are OK for side-by-side, but for
#                    stacked, they are coerced to absolute values, since
#                    it doesn't make sense to compare negative/positive
#                    values according to stacked lenghts, but hanging
#                    bars for side-by-side plots may be useful.

# TODO:  write a wrapper barchart2 for bwplot (box-whiskers plot that
#        deals with factors in x or y), which should define 
#        a prepanel function that sets the correct x/y limits for each
#        panel.
#        For side-by-side, we need to set the factor
#        axis limits correctly for each panel. For stacked the dependent
#        variable axis must be set.  Done. Still need to set factor axis
#        limits and label groups properly for side-by-side case.

#        We really should do a nice side-by-side version of this. It would
#        mean reassigning numeric values to the factors, and shifting them
#        for each subsequent group (and skipping one integer value between
#        groups).
# --------------------------------------------------------------------

x <- as.numeric(x)
y <- as.numeric(y)
bar.fill <- trellis.par.get("bar.fill")

# The `groups' argument is the original dataframe element containing the
#     groups to plot in each panel, with vals
#     containing the unique values, and nvals equal to the number of
#     different categories
# The `subscripts' are the indices for the groups corresponding
#     to those points which are to be plotted in the current
#     panel.

# Get group factors over ALL possible groups in whole data set
#    not just the ones passed to this panel
glevels <- levels(factor(groups))
nglevels <- length(glevels)
col <- rep(col, length=nglevels)

# vector of groups levels for current panel
g <- groups[subscripts]

# Assign factors (independent variable) and
#     bar heights (dependent variable)
if (horizontal) {
  f <- y
  flevels <- sort(unique(y))
  h <- x
} else {
  f <- x
  flevels <- sort(unique(x))
  h <- y
}  
nflevels <- length(flevels)

# Only plot absolute values for stacked bars
if (!beside) h <- abs(h)

# Split height and factor panel data by current panel groups
h <- split(h, g)
f <- split(f, g)


# Check for repeated factors in each group.
if (any(sapply(f, function(x) any(duplicated(x)))))
  stop("Duplicate factors (independent variable values) are not allowed within groups.")

# Get complete factor x or y positions for each group level. They're identical
#     for stacked groups, but shifted for side-by-side groups, although
#     centered around the factor integer levels, with 0.5 units in between
#     groups.
if (beside) {
  group.width <- group.ratio/(group.ratio + 1)
  positions <- matrix(nrow=nglevels, ncol=nflevels)
  w <- group.width/(2*nglevels)
  s <- group.width/2
  for (i in 1:NCOL(positions)) 
     positions[,i] <- seq(flevels[i]-s+w, flevels[i]+s-w, by=2*w)
} else     
  positions <- t(matrix(flevels, nrow=nflevels, ncol=nglevels))


# Construct matrices of bar heights and bottom bar coordinates,
#    with groups going down rows  and factors going across columns
# We calculate cumulative sum of heights to get the tops of each of
#    the stacked bars, and then subtract the raw heights to get the
#    bottoms, unless beside=TRUE, where we set all bottoms to zero.
heights <- matrix(nrow=nglevels, ncol=nflevels)
for (i in 1:nglevels)
   heights[i,] <- h[[i]][match(flevels, f[[i]])]
heights[is.na(heights)] <- 0
tops <- apply(heights, 2, cumsum)
if (beside)
  bottoms <- matrix(0, nrow=nglevels, ncol=nflevels)
else
  bottoms <- tops - heights

#xscale <- current.viewport()$xscale
#yscale <- current.viewport()$yscale

# Now draw the stacked bars

bar.width <- box.ratio/(1 + box.ratio)
if (beside) bar.width <- bar.width*group.width/nglevels

for (i in 1:nglevels) 
   if (horizontal) {
     #xmin <- current.viewport()$xscale[1]
     for (j in 1:nflevels) 
        grid.rect(gp = gpar(fill = col[i]), y = positions[i,j],
	          x = bottoms[i,j],
		  height = bar.width, width = heights[i,j], 
                  just = c("left", "centre"), default.units = "native")
     
   } else {
     #ymin <- current.viewport()$yscale[1]
     for (j in 1:nflevels) 
        grid.rect(gp = gpar(fill = col[i]), x = positions[i,j],
	          y = bottoms[i,j],
		  height = heights[i,j], width = bar.width, 
                  just = c("centre", "bottom"), default.units = "native")
   }

}
panel.superpose.2 <- 
function (x, y, subscripts, groups, col, col.line = superpose.line$col, 
    col.symbol = superpose.symbol$col, pch = superpose.symbol$pch, 
    cex = superpose.symbol$cex, lty = superpose.line$lty,
    lwd = superpose.line$lwd, type="p", ...) 
{

#   `panel.superpose.2' :  This is a version of the 'panel.superpose'
#   Trellis panel function that allows the plot `type' to change between
#   superimposed (overlayed) data sets.  See the `panel.xyplot' function
#   for details on the `type' option which is usually a single character,
#   but here is a character vector with each element specifying the
#   plot style of each subsequently-overlayed plot.
#                        ---  Neil Klepeis, 26-Dec-2001

    if (length(x) > 0) {
        if (!missing(col)) {
            if (missing(col.line)) 
                col.line <- col
            if (missing(col.symbol)) 
                col.symbol <- col
        }
        superpose.symbol <- trellis.par.get("superpose.symbol")
        superpose.line <- trellis.par.get("superpose.line")
        x <- as.numeric(x)
        y <- as.numeric(y)
        vals <- sort(unique(groups))
        nvals <- length(vals)
        col.line <- rep(col.line, length = nvals)
        col.symbol <- rep(col.symbol, length = nvals)
        pch <- rep(pch, length = nvals)
        lty <- rep(lty, length = nvals)
        lwd <- rep(lwd, length = nvals)
        cex <- rep(cex, length = nvals)
        type <- rep(type, length = nvals)  # new line here
        for (i in seq(along = vals)) {
            id <- (groups[subscripts] == vals[i])
            if (any(id)) 
                panel.xyplot(x = x[id], y = y[id], pch = pch[i], 
                  cex = cex[i], col.line = col.line[i], col.symbol = col.symbol[i], 
                  lty = lty[i], lwd = lwd[i], type=type[i], ...)
        }
    }
}
panel.superpose.3<-
function (x, y, subscripts, groups, col, col.line = superpose.line$col, 
    col.symbol = superpose.symbol$col, pch = superpose.symbol$pch, 
    cex = superpose.symbol$cex, lty = superpose.line$lty, 
    lwd = superpose.line$lwd, type = "p", ...) 
{

 #  Simple change to define factors only on the levels in the subgroup
 #     of values passed to each panel.  This allows for specifying
 #     colors for each panel series and making a legend for each panel.
 #           NK  19-June-2008

    if (length(x) > 0) {
        if (!missing(col)) {
            if (missing(col.line)) 
                col.line <- col
            if (missing(col.symbol)) 
                col.symbol <- col
        }
        superpose.symbol <- trellis.par.get("superpose.symbol")
        superpose.line <- trellis.par.get("superpose.line")
        x <- as.numeric(x)
        y <- as.numeric(y)
        subgr <- groups[subscripts]
        vals <- sort(unique(subgr))
        nvals <- length(vals)
        col.line <- rep(col.line, length = nvals)
        col.symbol <- rep(col.symbol, length = nvals)
        pch <- rep(pch, length = nvals)
        lty <- rep(lty, length = nvals)
        lwd <- rep(lwd, length = nvals)
        cex <- rep(cex, length = nvals)
        type <- rep(type, length = nvals)
        for (i in seq(along = vals)) {
            id <- (subgr == vals[i])
            if (any(id)) 
                panel.xyplot(x = x[id], y = y[id], pch = pch[i], 
                  cex = cex[i], col.line = col.line[i],
                  col.symbol = col.symbol[i], 
                  lty = lty[i], lwd = lwd[i], type = type[i], 
                  ...)
         }

        grid.legend.2(pch, col.symbol, vals)
    }
}
peaks<-
function(series,span=3)
{

        require(ts)
        z <- embed(series, span)
        result <- max.col(z) == 1 + span %/% 2
        result
}
place.text<-
function(t, l=NULL, adj=c(0.5,1), ...) {

# \title{Place vector of text and expressions}

# \description{Function to let users interactively place any number of
# text and expressions on graphs using the \code{locate} and
# \code{text} functions}

#\details{The user clicks on the graphics device window to place
# each specified text string or expression}

#\value{Invisibly returns a list of places where text was positioned}

# \item{t}{a vector containing text and/or mathematical expressions} 
# \item{l}{an optional list of places to position text automatically}
# \item{...}{arguments to pass to \code{text}}

if (!is.vector(t)) stop("`t' must be a list of character strings and/or expressions")
p <- list()
if (!is.null(l)) p <- l
for (i in 1:length(t)) {
    if (is.null(l)) p[[i]] <- locator(1)
    text(p[[i]], t[[i]], adj=adj, ...)
}

invisible(p)

}
plot.fit.ts.emp<-
function (x, 
          main="Empirical Fit to an Observed Time Series",
          na.action=na.contiguous)

{

# `plot.fit.ts.emp' : R function to plot a `fit.ts.empirical' 
# object, `x', consisting of the observed data, the model,
# the residuals (on a separate plot), and the autocorrelation
# function of the residuals (also a separate plot).

# Note: It's redundant to have `fit.ts.empirical' output the residuals, since
# they can be determined from the observed time series and 
# the model/model parameters.

require(ts)

t <- time(as.ts(x$data$OBSERV))
xlim <- range(t)
f <- x$data[,3:(NCOL(x$data))]
n <- NCOL(f)
for (i in 1:n)
    f[[i]] <- as.factor(f[[i]])
shift <- x$par[1]  # shift parameter
par <- x$par[-1]   # rest of factored mean parameters
l <- list()
for (i in 1:n) { # parse parameter list
    if (i==1)
       a <- 0
    else
       a <- length(levels(f[[i-1]]))
    b <- length(levels(f[[i]]))
    l[[i]] <- par[(1+a):(a+b)]
}
m <- shift + rep(0,NROW(f))
for (i in 1:n)
   m <- m + l[[i]][cut(as.numeric(f[[i]]),length(levels(f[[i]])),labels=F)]
r <- m - x$data$OBSERV

set.pars <- list(mar = c(0,6, 0, 6), oma = c(6, 0, 4, 0),
                tck = -0.01, mfrow = c(3,1))
oldpar <- do.call("par", as.list(names(set.pars)))
on.exit(par(oldpar))
do.call("par", set.pars)

plot(x$data$OBSERV, type="l", lty="dashed", ylab="", xlim=xlim,axes=F)
mtext("Time Series Values", side=2, 3)
box()
axis(2,label=T); axis(4,label=F)
mtext(main, line=2, 3, cex=1.5)
lines(m, type="l", lty=1)

plot(r, lwd=1, type="l", ylab="",xlim=xlim,axes=F)
mtext("Residuals", side=2, 3)
lines(xlim,c(0,0), type="l", lty="dotted")
box()
axis(2,label=F); axis(4,label=T)

acf.out<-acf(r, na.action=na.action, ci=0, lag.max=xlim[2],plot=F)
plot(acf.out$lag, acf.out$acf, axes=F, type="h", ylab="", xlim=xlim)
mtext("ACF of Residuals", side=2, 3)
lines(xlim,c(0,0), type="l", lty="dotted")
box()
axis(1)
axis(2,label=T); axis(4,label=F)
mtext("Time (Series and Residuals) or Time-Lag (ACF)", side = 1, line = 3)

}
plot.fit.ts.sin<-
function (x, xlim=c(start(x$observ)[1],end(x$observ)[1]),
          main="Sinusoidal Fit to an Observed Time Series",
          na.action=na.contiguous)

{

# `plot.fit.sin' : R function to plot a `fit.sin' output
# object, `x', consisting of the observed data, the model,
# the residuals (on a separate plot), and the autocorrelation
# function of the residuals (also a separate plot).

# Note: It's redundant to have fit.sin output the residuals, since
# they can be determined from the observed time series and 
# the model/model parameters.

require(ts)

t <- time(x$observ)   # x$observ must be a time series
f <- sin.sum(t, x$wave,x$phase,x$amp)
r <- f - x$observ

set.pars <- list(mar = c(0,6, 0, 6), oma = c(6, 0, 4, 0),
                tck = -0.01, mfrow = c(3,1))
oldpar <- do.call("par", as.list(names(set.pars)))
on.exit(par(oldpar))
do.call("par", set.pars)

plot(x$observ, type="l", lty="dashed", ylab="", xlim=xlim,axes=F)
mtext("Time Series Values", side=2, 3)
box()
axis(2,label=T); axis(4,label=F)
mtext(main, line=2, 3, cex=1.5)
lines(f, type="l", lty=1)

plot(r, lwd=2, type="l", ylab="",xlim=xlim,axes=F)
mtext("Residuals", side=2, 3)
lines(xlim,c(0,0), type="l", lty="dotted")
box()
axis(2,label=F); axis(4,label=T)

acf.out<-acf(r, na.action=na.action, ci=0, lag.max=xlim[2],plot=F)
plot(acf.out$lag, acf.out$acf, axes=F, type="h", ylab="", xlim=xlim)
mtext("ACF of Residuals", side=2, 3)
lines(xlim,c(0,0), type="l", lty="dotted")
box()
axis(1)
axis(2,label=T); axis(4,label=F)
mtext("Time (Series and Residuals) or Time-Lag (ACF)", side = 1, line = 3)

}
plot.log<-
function (x, y, topx=NULL, ylim=range(y), xlim=range(x), exact10=TRUE,
          type="s", log="x", col="black", grid=TRUE, rect=FALSE,
          shade.col="tan",  ...)
{

# \name{plot.log}

# \title{Plot X-Y Data with Nice Log Axes}

# \description{Function to plot data with nice log base-10 axes
# and gridlines.  Values less than 0 are
# converted to the value NA.  Can set axis tics to exact multiples of 
# ten on the plot, e.g., 0.1, 1, 10, if \code{exact}, or nearest 10^i
# increment, e.g., 0.2 or 0.3 (for i = -1), otherwise.}

x<-as.numeric(x)
y<-as.numeric(y)

if (!is.null(topx)) {
   x <- c(x, topx)
   y <- c(y, y[length(y)])
}

log.v <- unlist(strsplit(log,split=""))

if ("y" %in% log.v & any(y <= 0)) {
   warning("Some y data <= zero. Setting x,y values to NA.")
   y[y <=0] <- NA
   ok <- complete.cases(x,y)
   x<-x[ok]
   y<-y[ok]
   ylim <- range(y)
   xlim <- range(x)
}

if ("x" %in% log.v & any(x <= 0)) {
   warning("Some x data <= zero. Setting x,y values to NA.")
   x[x <=0] <- NA
   ok <- complete.cases(x,y)
   x<-x[ok]
   y<-y[ok]
   xlim <- range(x, na.rm=T)
   ylim <- range(y, na.rm=T)
}


if ("x" %in% log.v) {
   x2 <- ifelse(xlim[2]<0.1,trunc(log10(xlim[2])),trunc(log10(xlim[2])+0.99))
   x1 <- ifelse(xlim[1]>10,trunc(log10(xlim[1])),trunc(log10(xlim[1])-0.99))
   if (!exact10) {
      xlim[1] <- 10^x1*trunc(xlim[1]/10^x1)
      xlim[2] <- 10^(x2-1)*ceiling(xlim[2]/10^(x2-1))
   } else {
      xlim <- c(10^x1,10^x2)
   }
}

if ("y" %in% log.v) {
   y2 <- ifelse(ylim[2]<0.1,trunc(log10(ylim[2])),trunc(log10(ylim[2])+0.99))
   y1 <- ifelse(ylim[1]>10,trunc(log10(ylim[1])),trunc(log10(ylim[1])-0.99))
   if (!exact10) {
      ylim[1] <- 10^y1*trunc(ylim[1]/10^y1)
      ylim[2] <- 10^(y2-1)*ceiling(ylim[2]/10^(y2-1))
   } else {
      ylim <- c(10^y1,10^y2)
   }
}


plot(x, y, type=type, xlim=xlim, ylim=ylim, log=log, col=col, axes=F, ...) 

if ("x" %in% log.v) {
   par(tcl=-0.5)
   if (10^x1 >= xlim[1])
      axis(1,10^x1,labels=formatC(10^x1,format="fg"))
   for (i in x1:(x2-1)) { 
       if (10^(i+1) <= xlim[2])
          axis(1,10^(i+1),labels=formatC(10^(i+1),format="fg"))
       f <- ifelse(i==x1, xlim[1] / 10^x1, 1)
       e <- ifelse(i==(x2-1), xlim[2], 10^(i+1))
       if (grid)  # grid lines over minor tics
          abline(v=seq(f*10^i,e,by=10^i), col="black", lty=3)
       # minor x tics
       par(tcl=-0.25) 
       axis(1,seq(f*10^i,e,by=10^i), labels=F)
       par(tcl=-0.5)
   }
} else {
   axis(1, pretty(x))
   abline(v=pretty(x), col="black", lty=3)
}

if ("y" %in% log.v) {
   par(tcl=-0.5)
   if (10^y1 >= ylim[1])
      axis(2,10^y1,labels=formatC(10^y1,format="fg"))
   for (i in y1:(y2-1)) {
       if (10^(i+1) <= ylim[2])
          axis(2,10^(i+1),labels=formatC(10^(i+1),format="fg"))
       f <- ifelse(i==y1, ylim[1] / 10^y1, 1)
       e <- ifelse(i==(y2-1), ylim[2], 10^(i+1))
       if (grid)  # grid lines over minor tics
          abline(h=seq(f*10^i,e,by=10^i), col="black", lty=3)
       # minor y tics
       par(tcl=-0.25) 
       axis(2,seq(f*10^i,e,by=10^i),labels=F)
       par(tcl=-0.5) 
   }
} else {
   axis(2, pretty(y))
   abline(h=pretty(y), col="black", lty=3)
}

box()

if (rect)
   rect(x[-length(x)], ylim[1], x[-1], y[-length(y)], col=shade.col, border=col)

}
plot.stacked.boxes <- 
function (x, m, ...) {

# Plot the columns in a matrix m as y-axis values for stacked boxes,
# sorting each row of data from lowest to highest.
# x-axis values in x.   Matrix has one less row than length of x.

if (length(x) != NROW(m)+1)
   stop("`x' must have length one greater than rows in `m'")

for (i in 1:NROW(m)) {
   xleft <- rep(x[i],NCOL(m)-1)
   ybottom <- m[i, order(m[i,])][-NCOL(m)]
   xright <- rep(x[i+1],NCOL(m)-1)
   ytop <- m[i, order(m[i,])][-1]

   rect(xleft, ybottom, xright, ytop,   ... )
}

}
polar2compass<-
function(x)
{

# Function to convert from "polar" degrees to
#    "compass" degrees

# Compass degrees are:

#   0  North
#  90  East
# 180 South
# 270 East

# Polar degrees are:

#   0  East
#  90 North
# 180 West
# 270 South

-x + 360 + 90 

}

polar.plot <-
function (r, theta, theta.zero = 0, theta.clw = FALSE, method = 1,
    rlabel.axis = 0, dir = 8, rlimits = NULL, grid.circle.pos = NULL,
    grid.lwd = 1, grid.col = "black", points.pch = 20, points.cex = 1,
    lp.col = "black", lines.lwd = 1, lines.lty = 1, polygon.col = NA,
    polygon.bottom = TRUE, overlay = NULL, pi2.lab = TRUE, text.lab = NULL,
    num.lab = NULL, rlabel.method = 1, rlabel.pos = 3, rlabel.cex = 1,
    rlabel.col = "black", tlabel.offset = 0.1, tlabel.cex = 1.5,
    tlabel.col = "black", main = NULL, sub = NULL)
{

# original code obtained from R-help from:  Karsten Dalsgaard Bjerre

# r: (vector of) radial data.
# theta: (vector of) angular data (in radians).
# theta.zero: angular direction on plot of theta = 0 (in radians).
# theta.clw: clockwise orientation of theta values (default = FALSE).
#
# method: (plotting of (r,theta)-data):
# 1: points (default)
# 2: line
# 3: polygon
#
# rlabel.axis: angular direction on the plot of radial label axis (in radians).
# dir: number of radial grid lines (default=8).
# rlimts: Interval for radial axis as a numeric vector: c(lower,upper). Interval will be extended by the default use of pretty()-function. (default = NULL).
# grid.circle.pos: radial axis position of grid circles as numeric vector of minimum length 2. Overrides the default positioning of grid circles by pretty()-function. (default = NULL).
# grid.lwd. grid line width.
# grid.col: grid line color.
#
# points.pch: points plotting symbol.
# point.cex: character expansion factor for points.
# lp.col: color of points (method 1) or lines (method 2 and method 3).
#    In method 3, set lp.col=0 for polygons without border.
# lines.lwd: line width for plotting methods 2 and 3 (default = 1).
# lines.lty: line type (default = 1).
# polygon.col: color of polygon (defalut = NA).
# polygon.bottom: polygon to the back i.e. behind the grid (default = TRUE).
#
# overlay: NULL (default), no overlay
# 1, overlay data on existing plot
# 2, overlay data, grid and labels on existing plot.
#
# pi2.lab: angular labels in radians (0, pi/2, pi, 3*pi/2) (default).
# text.lab: angular axis labels from a character vector c("N","E","S","W")
#         (default = NULL).
# num.lab: numeric angular axis labels in interval [0;num.lab[
#       (default = NULL). Number of labels: dir.
#
# rlabel.method (plotting of radial axis labels):
# 0: no radial labels.
# 1: labels at pretty radial distances (default).
# 2: exclude label at radial distace 0.
# 3: exclude label at maximum radial distance.
# 4: exclude radial labels at distance 0 and at maximum radial distance.
# rlabel.pos: text position of radial axis labels (NULL,1,2,3,4).
# rlabel.cex: cex for radial axis labels.
# rlabel.col: color of the radial labels.
#
# tlabel.offset: radial offset for angular axis labels in fraction of maximum radial value (default = 0.1).
# tlabel.cex: cex for angular axis labels.
# tlabel.col: angular labels color.
#
# main: plot main title.
# sub: plot sub title.
  
    fit.rad <- function(x, twop = 2 * pi) {
        for (i in 1:length(x)) {
            while (x[i] < 0) x[i] <- x[i] + twop
            while (x[i] >= twop) x[i] <- x[i] - twop
        }
        return(x)
    }
    if (is.null(rlimits))
        rpretty <- pretty(range(abs(r), 0, na.rm = TRUE))
    if (is.numeric(rlimits) & length(rlimits) == 2)
        rpretty <- pretty(range(abs(rlimits[1]), abs(rlimits[2])))
    if (is.numeric(grid.circle.pos) & length(grid.circle.pos) >
        1)
        rpretty <- grid.circle.pos
    lab.dist <- max(rpretty)
    if (!is.null(text.lab) || is.numeric(num.lab) || pi2.lab) {
        lab.dist <- lab.dist * (tlabel.offset + 1)
    }
    if (is.null(overlay)) {
        plot.new()
        ps <- max(lab.dist, max(rpretty))
        plot.window(xlim = c(-ps, ps), ylim = c(-ps, ps), asp = 1)
        title(main = main, sub = sub)
    }
    drawgrid <- function() {
        if (dir > 0) {
            rDir <- seq(0, 2 * pi, length = dir + 1)[-(dir +
                1)]
            segments(0, 0, max(rpretty) * cos(rDir), max(rpretty) *
                sin(rDir), col = grid.col, lwd = grid.lwd)
        }
        grid <- seq(0, 2 * pi, length = 360/4 + 1)
        for (rad in rpretty) {
            if (rad > 0)
                lines(rad * cos(grid), rad * sin(grid), col = grid.col,
                  lwd = grid.lwd)
        }
        if (rlabel.method != 0) {
            if (rlabel.method == 1)
                radLabels <- 1:length(rpretty)
            if (rlabel.method == 2)
                radLabels <- 2:length(rpretty)
            if (rlabel.method == 3)
                radLabels <- 1:(length(rpretty) - 1)
            if (rlabel.method == 4) {
                if (length(rpretty) > 2)
                  radLabels <- 2:(length(rpretty) - 1)
                else radLabels <- NULL
            }
            if (!is.null(radLabels)) {
                xpos <- rpretty[radLabels] * cos(rlabel.axis)
                ypos <- rpretty[radLabels] * sin(rlabel.axis)
                text(xpos, ypos, rpretty[radLabels], cex = rlabel.cex,
                  pos = rlabel.pos, col = rlabel.col)
            }
        }
        if (!is.numeric(num.lab)) {
            if (pi2.lab & !is.character(text.lab))
                t.lab <- expression(0, pi/2, pi, 3 * pi/2)
            if (!pi2.lab & is.character(text.lab))
                t.lab <- text.lab
            labDir <- seq(0, 2 * pi, length = length(t.lab) +
                1)[-(length(t.lab) + 1)]
            labDir <- fit.rad(theta.zero + (!theta.clw) * labDir -
                (theta.clw) * labDir)
            text(lab.dist * cos(labDir), lab.dist * sin(labDir),
                t.lab, cex = tlabel.cex, col = tlabel.col)
        }
        if (!pi2.lab & is.null(text.lab) & is.numeric(num.lab)) {
            labDir <- seq(0, 2 * pi, length = num.lab + 1)[-(num.lab +
                1)]
            labDir <- fit.rad(theta.zero + (!theta.clw) * labDir -
                (theta.clw) * labDir)
            text(lab.dist * cos(labDir), lab.dist * sin(labDir),
                paste(num.lab * labDir/(2 * pi)), cex = tlabel.cex,
                col = tlabel.col)
        }
        if ((is.character(text.lab) & is.numeric(num.lab)) ||
            (is.character(text.lab) & pi2.lab) || (pi2.lab &
            is.numeric(num.lab)))
            print("More than one type of angular labels was requested.")
    }
    theta2 <- fit.rad(theta.zero + (!theta.clw) * theta - (theta.clw) *
        theta)
    cartesian.rt <- cbind(r * cos(theta2), r * sin(theta2))
    if (method == 1) {
        if (is.null(overlay) || overlay == 2)
            drawgrid()
        points(cartesian.rt[, 1], cartesian.rt[, 2], col = lp.col,
            pch = points.pch, cex = points.cex)
    }
    if (method == 2) {
        if (is.null(overlay) || overlay == 2)
            drawgrid()
        lines(cartesian.rt[, 1], cartesian.rt[, 2], lwd = lines.lwd,
            col = lp.col, lty = lines.lty)
    }
    if ((method == 2 || method == 3) & length(r) <= 1)
        print("More than one data point is needed for line and polygon methods.")
    if (method == 3) {
        if (!polygon.bottom & (is.null(overlay) || overlay ==
            2))
            drawgrid()
        polygon(cartesian.rt, lwd = lines.lwd, col = polygon.col,
            border = lp.col, lty = lines.lty)
        if (polygon.bottom & (is.null(overlay) || overlay ==
            2))
            drawgrid()
    }
}
 

###########################
# data
#div<-50
#theta <- seq(0, 2 * pi, length = div + 1)[-(div+1)]
#r<-1:(div)
#r2<-rep(12,times=div)
#r3<-rep(32,times=div)

# use of polar.plot
#polar.plot(9, pi/4, dir = 4, points.cex = 1.5)
#polar.plot(r, theta)
#polar.plot(r, theta, method = 2, lines.lwd = 3, lines.lty=2, lp.col = "blue")
#polar.plot(r, theta, method=3, pi2.lab = FALSE, dir = 7, lines.lwd = 3, polygon.col="blue", lp.col = "red", grid.circle.pos = c(0, 50), rlabel.method=4 )

# overlay of polar plots
#polar.plot(r3, theta, method = 3, rlimits=c(0,50), rlabel.method = 2, lp.col = 0, polygon.col = "blue")
#polar.plot(r2, theta, method = 3, overlay=1, lp.col = 0, polygon.col = "white")
#polar.plot(r/2+15, 2*theta, lp.col = "red", method = 1, rlimits=c(0,50), overlay =2, points.cex = 1.5)
#title(main="Overlay red points on white polygon on blue polygon")

# theta axis labels as text
#textlabels<-c("N","E","S","W")
#polar.plot(r, theta, theta.clw = TRUE, theta.zero = pi/2, text.lab = textlabels , pi2.lab = FALSE, lines.lwd = 3, grid.lwd = 1, grid.col = "darkgreen", rlabel.method = 2, rlabel.axis = -pi/2, rlabel.cex = 2, rlabel.pos = NULL, rlabel.col = "brown", tlabel.col = "darkgreen", points.cex = 3, points.pch = 21, tlabel.cex = 3, tlabel.offset = 0.3, lp.col = "red")
#title(main="A Compas Rose\n Hurray for the R core group!")

# Example by Ross Ihaka at R-help
#group <- sample(3, 100, replace = TRUE)
#theta <- 0.5 * rnorm(100) + 0.5 * group
#r <- rnorm(100, 4)
#polar.plot(r, theta, lp.col = c("red","green4","blue")[group], points.pch = 19)

prec <-
function (x) {

# Function to return the most precise
# digit from a vector of real numbers
# Keep dividing by powers of 10 (pos and neg from trunc(log(max(x)) down)
# until the fractional portion is zero, then we have the highest precision
# digit in terms of a integer power of 10.

# Thanks to Thomas Lumley for help with machine precision

# Note:  Turn this into a standalone function for "regularizing" a
#        time-activity object with irregular time breaks.

init <- trunc(log10(max(x))) + 1
zero <- 0
y <- 1
while (any(y > zero)) {
  init <- init - 1
  x1 <- x*10^(-init)
  y <- x1 - trunc(x1)
  zero <- max(x1)*.Machine$double.eps
}
10^init
}


prepanel.superbar<-
function (x, y, box.ratio, groups = NULL, subscripts = NULL,
          horizontal = TRUE, beside = FALSE, ...) 
{

# For stacked bars, lower limit is always zero (only makes sense for
#  stacked comparisons of absolute values of passed dependent variables), but
#  it can be negative for side-by-side bars, since 'hanging' bars can
#  be illustrative.  Upper limit for stacked bar panels is set to the largest
#  sum of dependent values across the independent variable factor levels.

if (length(x) && length(y)) {
  if (!is.numeric(x)) x <- as.numeric(x)
  if (!is.numeric(y)) y <- as.numeric(y)
  temp <- 0.5
  dx <- 1
  dy <- 1
  if (horizontal) {
    ylim <- c(1 - temp, length(unique(y)) + temp)
    if (!beside) xlim <- c(0,max(sapply(split(x, factor(y)), sum)))
      else xlim <- if (all(x>0)) c(0,max(x)) else range(x[is.finite(x)])
  } else {
    xlim <- c(1 - temp, length(unique(x)) + temp) 
    if (!beside) ylim <- c(0,max(sapply(split(y, factor(x)), sum)))
      else ylim <- if (all(y>0)) c(0,max(y)) else range(y[is.finite(y)])
  }
  list(xlim = xlim, ylim = ylim, dx = dx, dy = dy)
} else
  list(c(NA, NA), c(NA, NA), 1, 1)

}
print.comments<-
function(z, add.lf=TRUE, show.levels=FALSE) {

# Function to print comments out for a dataframe object
# if add.lf is TRUE, then a linefeed is added between 
# elements of character vectors

if (is.data.frame(z)) {
   cat("\n**  Comments for Data Frame: ", substitute(z),"  **\n\n")
   if (!add.lf) {
      cat(paste(comment(z)),"\n")  
   } else {
      for (i in 1:length(comment(z))) 
          cat(comment(z)[i],"\n\n")
   }
   cat("\n")
   cat("Variable","     \t","Variable Label / Comment","\n")
   cat("--------","     \t","------------------------","\n\n")
   for (i in 1:NCOL(z)) {
       c <- paste(comment(z[[i]]))
       if (nchar(c) > 50)
          c <- c(substr(c,1,50)," ...")
       cat(names(z)[i],"     \t", c,"\n")
   }
} else {
  stop("z must be a dataframe object.")
}

if (show.levels) {
   cat("\n\n")
   cat("**  Factor Levels for Each Variable  **","\n")
   for (i in 1:NCOL(z)) {
       if (!is.null(levels(z[[i]]))) {
          cat("\nVariable:",names(z)[i],"\n")
          for (j in 1:nlevels(z[[i]])) 
              cat("\t",levels(z[[i]])[j], "\n")
       }
   }
}
}
prob.axis<-
function(side,  at=c(0.0001,0.001,0.01,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.99,0.999,0.9999), labels=c(0.01,0.1,1,5,10,25,50,75,90,95,99,99.9,99.99), grid=TRUE, grid.lty="solid", grid.col="lightgray", ...)
{

# Function to add a normal probability axis to a 
#     plot

x <- qnorm(at)
if (grid)
  axis(side, at=x, labels=FALSE, tck=1, lty=grid.lty, col=grid.col)
axis(side, at=x, labels, ...)

}
process.files <-
function (type, name) {

#  Simple front end to process.monitors for specific types of
#     data logger files.
#
#    Assume CSV format unless FREE is part of the data file type, in which
#    case free format is assumed (white space separating the fields)...

types <- c("Databear.CO", "HOBO.CO", "SidepakTrakPro", "SidepakTrakPro.FREE", 
           "SidepakLabrat", "SidepakBuddy")

if (!type %in% types) 
  stop ("`type' must be one of ",types)

if (missing(name))
  stop("Please specify a name for the processed files.")

if (type == "Databear.CO") 
  process.monitors(name, ylab="CO Concentration [ppm]",
            prefix="Databear", extension="txt", 
            id.names=c("LoggerType", "StandID","SensorID",
            "X","Y","Z","LoggerID","LoggerChannel"), csv=FALSE,
            format="%d/%m/%y %H:%M:%S", value.prefix="CO",
            interval=5, unit="min", make.agg=TRUE)
else if (type == "HOBO.CO")
   process.monitors(name, ylab="CO Concentration [ppm]",
           prefix="HOBO", extension="csv",
           id.names=c("LoggerType", "StandID"), csv=TRUE, datetime=TRUE,
           format="%m/%d/%y %I:%M:%S %p", value.prefix="CO",
           interval=5, unit="min", make.agg=TRUE)
else if (type == "SidepakTrakPro")
   process.monitors(name, ylab="Particle Concentration [ug/m3]", 
          prefix="SidepakTrakPro", extension="csv",
          id.names=c("LoggerType", "Pollutant", "Location", "Person", "SP.No"),
          csv=TRUE, datetime=FALSE, format="%m/%d/%Y %H:%M:%S",
          value.prefix="SP", interval=5, unit="min", make.agg=TRUE)
else if (type == "SidepakTrakPro.FREE")
   process.monitors(name, ylab="Particle Concentration [ug/m3]", 
          prefix="SidepakTrakPro", extension="txt",
          id.names=c("LoggerType", "Pollutant", "Location", "Person", "SP.No"),
          csv=TRUE, datetime=FALSE, format="%Y-%m-%d %H:%M:%S",
          value.prefix="SP", interval=5, unit="min", make.agg=TRUE)
else if (type == "SidepakLabrat")
    process.monitors(name, ylab="Particle Concentration [ug/m3]",
            prefix="SidepakLabrat", extension="csv", 
            id.names=c("LoggerType", "Source", "SP.No", "USBPort"),
            csv=TRUE, datetime=FALSE, format="%Y-%m-%d %H:%M:%S", 
            value.prefix="SP", interval=10, unit="sec", make.agg=TRUE) 
}
process.monitors <-
function (name, start=NULL,
          prefix="Sidepak", extension="csv",
          skip=0, comment.char="", format="%d/%m/%y %H:%M:%S",
          events.format=format,
          events.prefix="Events", events.extension="csv",
          lookup.format=format,
          lookup.prefix="Lookup", lookup.extension="csv",
          datetime=FALSE, date.prefix=c("Date","date","DATE"),
          time.prefix=c("Time","time","TIME"),
          tz="PST", value.prefix="CO", 
          lookup.value.prefix=c("Pos"),
          event.value.prefix=c("Loc","Act","Event"),
          datetime.name="Time.POSIX", value.name="Value",
          value.id.name="Sensor",  lookup.key=value.id.name,        
          interval=c(5,60), unit="mins",
          csv=FALSE, splitchar="_",
          id.names=c("Type","Number","SN"), 
          plot=TRUE, main=paste(name,"diagnostic plots"),
          plot.times=TRUE, plot.format="%I:%M %p", plot.interval="hour",
          scale.relation="free", ylab="Response",
          xlab=ifelse(plot.times, "Time of Day", paste("Elapsed",unit)), 
          width=11, height=8.5, cex=0.3,
          cex.lab=1.7, cex.axis=1.2, cex.main=1.9,
          col.lines=c("blue","red","black","green","orange","magenta","cyan"),
          col.areas=c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3",
                      "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD",
                      "#CCEBC5", "#FFED6F", col.lines),
          make.svg=FALSE, make.agg=TRUE, make.wide=FALSE,
          merge.events=FALSE, merge.lookup=FALSE, 
          make.stats=FALSE, na.rm=TRUE, value.factor=NULL) {

#  NEW, MORE GENERAL DESIGN:    Parse 1 or more response variables per file
#     and create a new index variable in the master file using their names. 
#     This new index is used to plot diagnostics and make a WIDE format file.   
#     The indices embedded in the file name are meant to apply to ALL the
#     responses present in a given file and are also stored in the master file.
#     ALSO:  We now parse for key PREFIX words in the date, time, or response
#      columns instead of making user specify the column number.  The user
#      simply has to make sure that all the responses in a given file start
#      with the specified keyword (e.g., CO or Aerosol) and 
#      the date and time variables also (e.g., Date and Time).a
#                               --- NK  27-April-2008
#      update:  Can specify multiple value.prefix values and we will
#       select all that match.  now working.  NK 3-June-2008

# Function to read in a number of real-time monitor data files,
#   create a standard time variable, elapsed time, time factor, 
#   id variable, and create a combined file.   Elapsed time keys from
#   time at start of first file if "start" is NULL. Also creates an 
#   aggregate data file, averaging over specified time interval.
#   Optionally create diagnostic plots.
#   Concentration column must be specified as "value.field".
#   "name" must be a character string that is used to name the combined files.
#            Neil Klepeis,  13-Sept-2007
#
#  ----
#
#  INPUT ARGS
#
#  name - base name of resulting file, e.g., "CO" to give "CO.RData" or "CO.csv"
#  start - optional time to start elapsed time variable, default is first time
#           of first file
#  prefix - search for files that begin with this prefix, e.g., "CO"
#  extension - search for files that have this extension, e.g., ".txt"
#  format - format for date-time concatenated variable
#  datetime - whether the date field contains both date & time info (TRUE) or
#      the datetime variable is the concatenation of separate date/time fields
#      (FALSE)
#  date.prefix - the prefix for columns of the DATE or DATE-TIME in the file
#  time.prefix - the prefix of the TIME in the file, if NULL assume date field
#                contains date-time information
#  value.prefix - the prefix of the measured VALUE(S) (response(s)) in the file
#               CAN GIVE MULTIPLE DIFFERENT PREFIXES AND WE SELECT THEM ALL
#  datetime.name - name to assign to the date-time variable in the final file
#  value.name - the name of the column containing the response values 
#                  "Value" by default
#  value.id.name - the name to use for the value ID column in the final file
#                   "Sensor" by default
#  interval - time interval to use in aggregating/calculating elapsed time
#  unit - unit of time used for the interval, e.g., "mins" or "secs"
#  csv - whether or not the input file is in CSV format or space-delim format
#  splitchar - the character used to split the filenames to extract information
#  id.names - the names to use for index values embedded in each file name;
#    length must match the actual number in each filename or an error is
#    returned
#  plot - whether to create a diagnostic plot of the data or not
#  plot.id -  NOT USED!!
#             the index of the id.names to use as a conditional variable in
#             the diagnostic plot
#  main,xlab,ylab - the title and axis labels for the diagnostic plot
#  make.wide - whether to additionally create WIDE format data files with
#             responses in separate columns and a common time and index 
#             variables (TRUE) or to just make the LONG format version of
#             the data where the data are stacked on top of each other (FALSE).
#  XXX align.round - NOT USED !!
#                 number of digits to round the Elapsed variable to before
#                aligning files into the WIDE format, defaults to 0, Note:
#                units of the Elapsed variable are given in "unit" arg. which
#                defaults to "min".    DON'T NEED THIS IF WE ONLY "WIDEN"
#                 THE AGGREGATED FILE....
#  wide.id  -   NOT USED !!
#                the index to use for making the WIDE filing using
#                the reshape "cast" function.
#  ----
#  UPDATE.  Now we can key the lookup table on the value.id.name (by default)
#            Or any of the FILE ID variables.... NK 28-Aug-2008
#  UPDATE.  Add "scale.relation" can be "free" or "same" or "sliced"
#             See ?xyplot in lattice package.
#  UPDATE.  Now date.prefix and time.prefix can have multiple values
#            although only first match is used at the date or time variable.
#           Also, now specifying multiple values for 'interval' produces
#           an aggregated file across each of the specified intervals.
#  UPDATE.  Misc fixes.  Also, add 'plot.times' and 'plot.format' arguments
#             for specifying whether
#             actual times of day are plotted on x-axes (and their formatting)
#             or if the elapsed variable is used.  14-Aug-2008
#  UPDATE.  Fixed bug that didn't correctly add lookup/event factors to each
#             individual value variable.  NK Jul-24-2008
#  UPDATE.  Optional value.factor to multiply by all values.
#  UPDATE.  Now can calculate lots of statistics tables
#  UPDATE.  Replace the 'aggregate' function entirely with reshapes
#               melt and cast.   The built-in aggregate seems to bog
#               down when we use a lot of factors.. NK 22-Jul-2008

#  UPDATE.  Put make.wide stuff back in.   Added lookup file.  
#              Put in different lattice plots for each id name. 
#              Write postscript or SVG graphics files only.   22Jul08
#  UPDATE.  Add "skip" argument to skip lines before reading header/data in
#            each data file (not for event files).
#  UPDATE.  Add an activity-response areaplot for each "value" variable
#               if merge.events=TRUE
#  UPDATE.  Add merging of events into the data base from a separate CSV
#                file.  Done.
#  UPDATE.  Clean up code and make work for CO and Aerosol data files.
#  UPDATE.  Add option to creat AGG files. ....
#  UPDATE.  19-Apr-2008.  Add option to create WIDE format data files.
#                         using the reshape R package.. Only do for the
#                         aggregated file since otherwise aligning them
#                         may be difficult...
#  UPDATE.  27-April-2008.  Now read one or more response variables from
#                         each file and use the column names as a new
#                         index variable, also use this index to make the
#                         WIDE file.  Also use this index to make the
#                         diagnostic plot.
#  UPDATE.  21-May-2008    Now we put the POSIX time value at the beginning of
#                     the time interval into the aggregated file
#
#
# TODO:  Maybe add tolerance for time.factor 0.5*interval so that the
#        specific bin is actually the midpoint of the interval.
#       
#        Optionally have a lookup table (another CSV file) to fold information
#        on each sensor into the database.....

#        Add aggregated files for splits by id.name, value.names, and
#             event.value  (and any other factors...)
# -------------------------------------------------------------------------

if (missing(name)) stop("Must specify a name for the master file.")

if (value.id.name %in% id.names) 
  stop("The `value.id.name' must be different from any of the `id.names' embedded in filenames.")

require(heR.Misc)
#require(reshape)

time.factor.names <- paste("Time.Factor",interval, unit, sep=".")
elapsed.name <- paste("Elapsed", unit, sep=".")

taken.names <- c(time.factor.names, elapsed.name, value.id.name, id.names,
                  value.name, datetime.name)

#  colors used in EVENTS plots
#cols <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
#          "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")

#  col argument is by default this:
# c("blue","red","black","green","orange","magenta","cyan")


# -------------------------------------------------

#  L O O K - U P   F I L E 

# -------------------------------------------------

if (merge.lookup) {

    if (!lookup.key %in% c(id.names, value.id.name))
      stop("The lookup key, currently ",lookup.key, ", must either be the value.id.name (default), ",value.id.name, ", or one of the File ID names, ", paste(id.names, collapse=" "))

    # Load lookup file for merging if available....
    lp <- paste("^(",lookup.prefix,").*[.](",lookup.extension,")$",sep="")
    lfiles <- dir(pattern=lp)

    if (length(lfiles) == 0)
      stop("No look-up file found with prefix ",lookup.prefix," and extension ",lookup.extension,"\n")

    if (length(lfiles) > 1 )
      stop("Multiple look-up files found, please consolidate to a single file.\n")

    cat("\nProcessing Look-up File ",lfiles[1],"...\n")

    # Always to CSV format for lookup table
#    if (csv)   # comma separated file format?
      ldata <- read.csv(file=lfiles[1], comment.char=comment.char)
#    else       # else assume space delimited with a header
#      ldata <- read.table(file=lfiles[1],header=TRUE, comment.char=comment.char)

    print(head(ldata))

    # Get the event index or indices, matching MULTIPLE occurrences
    #   on specified prefix
    lookup.value.index <- grep(paste("^(",lookup.value.prefix,")",sep="",collapse="|"), names(ldata))
    lookup.value.names <- grep(paste("^(",lookup.value.prefix,")",sep="",collapse="|"), names(ldata), value=TRUE)

    if (length(lookup.value.names) == 0)
      stop("No look-up names found.")

    if (!lookup.key %in% names(ldata))
      stop("'lookup.key' (", lookup.key,") not found in look-up file.")

    # Check for duplicate names
    if (any(lookup.value.names %in% taken.names))
      stop("Duplicate name(s) used, please check look-up, event and file id names. Duplicated name(s):  ",taken.names[which(taken.names %in% lookup.value.names)])
    taken.names <- c(taken.names, lookup.value.names)

    # Make lookup items into factors
    for (i in lookup.value.names) 
       ldata[[i]] <- factor(ldata[[i]])

    # Select only matched id names and value.id.name 
    newnames <- c(lookup.key, lookup.value.names)
    ldata <- ldata[newnames]

    o <- order(ldata[[lookup.key]])
    ldata <- ldata[o,]

    cat("\nProcessed Look-up File...\n")

    print(head(ldata))

}




# -------------------------------------------------

#  E V E N T   F I L E 

# -------------------------------------------------

if (merge.events) {

   # Load event file(s) for merging if available....
   ep <- paste("^(",events.prefix,").*[.](",events.extension,")$",sep="")
   efiles <- dir(pattern=ep)

   if (length(efiles) == 0)
     stop("No event file found with prefix ",events.prefix," and extension ",events.extension,"\n")

   if (length(efiles) > 1 )
     stop("Multiple event files found, please consolidate to a single file.\n")

   cat("\nProcessing Event File ",efiles[1],"...\n")

   #alledata <- data.frame()

   #for (i in efiles) {
   # always do CSV for event file....
#    if (csv)   # comma separated file format?
      edata <- read.csv(file=efiles[1], comment.char=comment.char)
#    else       # else assume space delimited with a header
#      edata <- read.table(file=efiles[1],header=TRUE, comment.char=comment.char)

    print(head(edata))

    # Get Date/Time indices, matching 1st occurrence on specified prefix
    #   Now matches multiple possibilities, still choose only first match
    #date.index <- grep(paste("^(",date.prefix,")",sep=""), names(edata))[1]
    date.index <- grep(paste("^(",date.prefix,")",sep="",collapse="|"), names(edata))[1]
    if (datetime || is.null(time.prefix))
      time.index <- NULL
    else
      #time.index <- grep(paste("^(",time.prefix,")",sep=""), names(edata))[1]
      time.index <- grep(paste("^(",time.prefix,")",sep="",collapse="|"), names(edata))[1]

    if (is.na(date.index))
      stop("Date columns not matched.")
    if (!is.null(time.index) && is.na(time.index))
      stop("Date columns not matched.")

    # Get the event index or indices, matching MULTIPLE occurrences
    #   on specified prefix
    event.value.index <- grep(paste("^(",event.value.prefix,")",sep="",collapse="|"), names(edata))
    event.value.names <- grep(paste("^(",event.value.prefix,")",sep="",collapse="|"), names(edata), value=TRUE)

    if (length(event.value.names) == 0)
      stop("No event names found.")

    # Check for duplicate names
    if (any(event.value.names %in% taken.names))
      stop("Duplicate name(s) used, please check event and id names. Duplication name:  ",taken.names[which(taken.names %in% event.value.names)])
    taken.names <- c(taken.names, event.value.names)

    # Make events into factors
    for (i in event.value.names) 
       edata[[i]] <- factor(edata[[i]])

    # Create standard date format
    if (datetime)
      dateraw <- edata[[date.index]]
    else
      dateraw <- paste(edata[[date.index]], edata[[time.index]])

    edata[datetime.name] <- as.POSIXct(strptime(dateraw, 
                                format=events.format, tz=tz))

    if (any(is.na(edata[[datetime.name]])))
      stop("Date/Time not processed correctly.  Please check the date-time format string.")

    # Include the index variable for different response values
    #   if it exists in the event file
    if (value.id.name %in% names(edata))
      newnames <- c(value.id.name, datetime.name, event.value.names)
    else
      newnames <- c(datetime.name, event.value.names)
    edata <- edata[newnames]
#   newdata <- data[newnames]
#    alledata <- rbind(alledata, newdata)
   
#}

#  Sort by time (and "value.id.name" if it exists)
if (value.id.name %in% names(edata)) 
  o <- order(edata[[datetime.name]], edata[[value.id.name]])
else
  o <- order(edata[[datetime.name]])

edata <- edata[o,]

cat("\nProcessed Events File...\n")

print(head(edata))

}



# -------------------------------------

#    D A T A   F I L E S      

# ------------------------------------


cat("\n\nProcessing '",name,"' data files....\n")

# Initialize combined data frame
alldata <- data.frame()

all.value.names <- c()

# Get files starting with "prefix" and ending with "extension"
#p <- paste("^(",prefix,").*[.](",extension,")$",sep="")
# Allow prefix and extension to be vectors with any possible
#    values, we will combine them and seach for the combinations....
p <- paste("^(",prefix,").*[.](",extension,")$",sep="",collapse="|")

files <- dir(pattern=p)

if (length(files) == 0)
  stop("No data files found with prefix ",prefix," and extension ",extension,"\n")

first <- TRUE

for (i in files) {
    cat("\n\n========================================\n")
    cat("Processing file '",i,"'","...\n")
    cat("========================================\n\n")

    if (csv)   # comma separated file format?
      data <- read.csv(file=i, skip=skip, comment.char=comment.char)
    else       # else assume space delimited with a header
      data <- read.table(file=i,header=TRUE,skip=skip,comment.char=comment.char)

    print(head(data))

    # Get number of original columns
    n <- NCOL(data)

    # Get Date/Time indices, matching 1st occurrence on specified prefix
    #    Now match multiple possibilities and take first match....
    date.index <- grep(paste("^(",date.prefix,")",sep="",collapse="|"), names(data))[1]
    #date.index <- grep(paste("^(",date.prefix,")",sep=""), names(data))[1]
    #date.index <- grep(paste("^(",date.prefix,")",sep=""), names(data))
    if (datetime || is.null(time.prefix))
      time.index <- NULL
    else
      #time.index <- grep(paste("^(",time.prefix,")",sep=""), names(data))[1]
       time.index <- grep(paste("^(",time.prefix,")",sep="",collapse="|"), names(data))[1]

    if (is.na(date.index))
      stop("Date columns not matched.")
    if (!is.null(time.index) && is.na(time.index))
      stop("Date columns not matched.")

    # Get the response index or indices, matching MULTIPLE occurrences
    #   on specified prefix
    value.index <- grep(paste("^(",value.prefix,")",sep="",collapse="|"), names(data))
    value.names <- grep(paste("^(",value.prefix,")",sep="",collapse="|"), names(data), value=TRUE)

    #  keep track of value names in all the files
    all.value.names <- unique(c(all.value.names, value.names))

    # Check for duplicate names
    if (any(value.names %in% taken.names))
      stop("Duplicate names used, please check event, id, and value names.  Duplication name:  ",taken.names[which(taken.names %in% value.names)])
  
    # Rename concentration variable.    NOT YET see below...
    # names(data)[value.field] <- value.name

    # Create id variables - take all fields embedded in the file separated by
    #   the splitchar arg and make a separate index variable for each one
    #   using names given in id.names var.
    substring <- paste(".",extension, sep="")
    #ids <- unlist(strsplit(unlist(strsplit(i,"\\."))[1], splitchar))
    ids <- unlist(strsplit(unlist(sub(substring,"",i)), splitchar))
    names(ids) <- id.names
    cat("File Id's parsed: ",ids,"\n")
    if (length(id.names) == length(ids))
      for (j in 1:length(ids))
         data[[id.names[j]]] = ids[j]
    else 
      stop("Mismatch between number of ID names specified and embedded fields in an input filename. ")

    # Create standard date format
    if (datetime) 
      dateraw <- data[[date.index]]
    else
      dateraw <- paste(data[[date.index]], data[[time.index]])

    data[datetime.name] <- as.POSIXct(strptime(dateraw, format=format, tz=tz))

    if (any(is.na(data[[datetime.name]])))
      stop("Date/Time not processed correctly.  Please check the date-time format string.")

    # Assign the starting time for all processed files
    #if (is.null(start) & first) start <- data[[datetime.name]][1]
    if (first) 
      if (is.null(start))
        start <- data[[datetime.name]][1]
      else
        start <- as.POSIXct(strptime(start, format=format))
    if (is.na(start))
      stop("Error processing `start'.  Check Date/Time format.")

    # Create elapsed minutes variable
    data[elapsed.name] <-  as.numeric(difftime(data[[datetime.name]],
                                     start, unit=unit))

    # Create 1 or more time factor variables,
    #         showing values in each specified interval
    for (j in 1:length(time.factor.names)) 
       data[time.factor.names[j]] <- time.factor.2(data[[elapsed.name]],
                                interval=interval[j],
                                origin=0, integer.levels=FALSE)

    #  Create column for value.id.name
    data[value.id.name] <- NA

    #print(head(data))

    # No longer first file being processed for time information
    first <- FALSE

    # ITERATE over every response variable, adding new rows for
    #   each response (with duplicate embedded file indices in id.names).
    for (j in value.names) {
      newnames <- c(elapsed.name, time.factor.names, datetime.name,
                     id.names, value.id.name, j)

      newdata <- data[newnames]
      names(newdata)[NCOL(newdata)] <- value.name
      newdata[[value.id.name]] <- j

      #  LOOK-UP TABLE VARIABLES TO MERGE?
      if (merge.lookup) { 
        #  Get the current value of the lookup key variable
        #    for the current response value
        lookup.key.value <- newdata[[lookup.key]][1]
        for (k in lookup.value.names) {
           if (lookup.key.value %in% ldata[[lookup.key]]) {
             mergedata <- ldata[ldata[[lookup.key]] == lookup.key.value,k]
             lfactor <- rep(mergedata[1], NROW(newdata))
           } else {
             lfactor <- rep(NA, NROW(newdata))
           }
           newdata <- cbind(newdata, lfactor)
           names(newdata)[NCOL(newdata)] <- k
           newnames <- c(newnames, k)
        }
      }


      #  EVENTS TO MERGE?
      if (merge.events) {
        if (value.id.name %in% names(edata) &&
            j %in% edata[[value.id.name]])
          mergedata <- edata[edata[[value.id.name]] == j,]
        else
          mergedata <- edata
        #print(mergedata)
        if (any(diff(newdata[[datetime.name]]) < 0))
           warning("One or more times in data file not increasing.")
        if (any(diff(mergedata[[datetime.name]]) < 0))
           warning("One or more times in events file not increasing.")
        for (k in event.value.names) {
           if (j %in% mergedata[[value.id.name]])
              efactor <- time.segments(newdata[[datetime.name]],
                           mergedata[[datetime.name]],  
                           mergedata[[k]][-NROW(mergedata)])
           else
             efactor <- rep(NA, length(newdata[[datetime.name]]))

           #cat("HERE IT IS ..\n\n")
           
           #print(mergedata[c(datetime.name,k)])
           #print(efactor)
           #print(data[datetime.name])

           #print(edata[[k]][-NROW(edata)])
           #print(efactor)
           newdata <- cbind(newdata, efactor)
           names(newdata)[NCOL(newdata)] <- k
           newnames <- c(newnames, k)

           #cat("\n\nDONE...\n\n")
        }
      }

      #newnames <- c(newnames, value.id.name, j)
      #print(newnames)
      #newdata <- data[newnames]
      #names(newdata)[NCOL(newdata)] <- value.name
      #newdata[[value.id.name]] <- j

      cat("Processed Data for response ",j,":\n")

      print(head(newdata))


      # Add current data to master data frame
      alldata <- rbind(alldata, newdata)
      first <- FALSE
    }
}


#  MULTIPLY VALUES BY A NUMERIC FACTOR IF value.factor IS GIVEN....	

if (!is.null(value.factor))
  if (is.numeric(value.factor)) {
    alldata[[value.name]] <- value.factor * alldata[[value.name]]
    cat("\n\nAll values, (",value.name,") multiplied by:  ",value.factor,"\n\n")
  } else {
    warning("'value.factor' is not numeric. No values changed.")
  }


# --------------------------------------------

#   T R A N S F O R M    D A T A

# ---------------------------------------------


cat("\n=================================================\n")

# Create an aggregated version, averaging over time factor

if (make.agg) {

  require(reshape)

  basenames <- c(id.names, value.id.name)

  if (merge.lookup) 
    basenames <- c(basenames, lookup.value.names)
  if (merge.events) 
    basenames <- c(basenames, event.value.names)

  #  names for all aggregated files, we have separate
  #     agg dataframes for each interval....
  aggnames <- c(time.factor.names, basenames)

  agglist <- list()


  #  =========================================================
  #  Create main aggregated file of values averaged over
  #    groups defined by time factor and all id variables.
  #  =========================================================

  aggmelt1 <- melt(alldata, id.var=aggnames, measure.var=value.name)
  aggmelt2 <- melt(alldata, id.var=aggnames, measure.var=datetime.name)

  #  Now create more "long" data for each time interval specified...

  for (i in 1:length(interval)) {
     formulanames <- c(time.factor.names[i],basenames)
     cat("\nAggregating data in ",interval[i],"-",unit,"intervals...\n")
     f <- paste(c(paste(formulanames,collapse="+"), "variable"), collapse="~")
     agglist[[time.factor.names[i]]] <- cast(aggmelt1, f, fun=mean, na.rm=na.rm)
     print(head(agglist[[i]]))
  }

  #aggdata <- aggregate(alldata[value.name], by=alldata[aggnames], FUN=mean)

  #aggdatetime <- strptime("1970-01-01", "%Y-%m-%d", tz=tz)+aggregate(alldata[datetime.name], by=alldata[aggnames], FUN=function(x) x[1])[[datetime.name]]

  #aggdata <- cbind(aggdatetime, aggdata)
  #names(aggdata)[1] <- datetime.name
  #aggdata[[datetime.name]] <- as.POSIXct(strptime("1970-01-01", "%Y-%m-%d", tz="") + aggdata[[datetime.name]])
  #print(head(aggdata))

  # ==================================================================
  # Optionally create WIDE format versions of raw and aggregated data
  #   1.  Time versus each Value ID (e.g., "Sensor" ID)
  #   2.  All factors versus Time
  #     Table cells contain Values averaged over factors,etc. ....
  # ==================================================================

  if (make.wide) {

    cat("\nMake WIDE-AGG format data...\n\n")

    #aggmelt <- melt(aggdata, measure.var=value.name)
    #print(head(aggmelt))

    widenames <- c(id.names)
    if (merge.events)
      widenames <- c(widenames, event.value.names)
    if (merge.lookup)
      widenames <- c(widenames, lookup.value.names)

   print(paste(c(paste(c(widenames, time.factor.names[1]), collapse="+"),
                value.id.name), collapse="~"))

    # only use first Time Factor name
    widedata.value.in.cols <- cast(aggmelt1, 
         paste(c(paste(c(widenames, time.factor.names[1]), collapse="+"),
               value.id.name), collapse="~"), fun=mean, na.rm=na.rm)
    cat("Wide data Value in Columns:\n")
    print(head(widedata.value.in.cols))

    widenames <- c(widenames, value.id.name)

    #  Only use first Time Factor name
    widedata.time.in.cols <- cast(aggmelt1, 
         paste(c(paste(widenames, collapse="+"), time.factor.names[1]),
               collapse="~"), fun=mean, na.rm=na.rm)
    cat("Wide data Time in Columns:\n")
    print(head(widedata.time.in.cols))
  }

  # ================================================
  # CALCULATE STATISTICS ACROSS EACH ID VARIABLE?
  #   Optionally create statistics tables with values
  #   averaged over all factors (value id's, file id's, lookup, event)
  #       INCLUDING MARGINS ACROSS EACH FACTOR.....
  # ================================================
  if (make.stats) {

    cat("\nCalculating STATISTICS tables by factors...\n\n")

    id.stats <- cast(aggmelt1, 
                     paste(c(paste(c(value.id.name, id.names),collapse="+"),
                          "variable"), collapse="~"),
                          fun=mean, na.rm=TRUE, margins=TRUE)
    cat("File ID Stats:\n")
    print(head(id.stats))
    if (merge.lookup) {
      lookup.id.stats <- cast(aggmelt1,
            paste(c(paste(c(value.id.name, lookup.value.names),collapse="+"),
                  "variable"), collapse="~"),
                  fun=mean, na.rm=TRUE, margins=TRUE)
      cat("Lookup ID Stats:\n")
      print(head(lookup.id.stats))
    }
    if (merge.events) {
      event.id.stats <- cast(aggmelt1,
            paste(c(paste(c(value.id.name, event.value.names),collapse="+"),
                  "variable"), collapse="~"),
                  fun=mean, na.rm=TRUE, margins=TRUE)
      cat("Event ID Stats:\n")
      print(head(event.id.stats))
    }
  }
}


# ---------------------------------------------

#   S A V E   D A T A 

# ------------------------------------------

cat("\nSaving data...\n")

# Save as an R data frame
#assign(quote(name), alldata)

save(alldata, file=paste(name,".RData", sep=""))
if (make.agg) {
  attach(agglist)
  for (i in 1:length(interval)) {
     save(list=time.factor.names[i], file=paste(name,"-Agg-",interval[i],unit,".RData", sep=""))
  }
  if (make.wide) {
    save(widedata.value.in.cols, 
         file=paste(name,"-Agg-WIDE-Value.RData", sep=""))
    save(widedata.time.in.cols, 
         file=paste(name,"-Agg-WIDE-Time.RData", sep=""))
  }
  if (make.stats) {
    save(id.stats, 
         file=paste(name,"-Agg-STATS-ID.RData", sep=""))
    if (merge.lookup)
       save(lookup.id.stats, 
            file=paste(name,"-Agg-STATS-LOOKUP.RData", sep=""))
    if (merge.events) 
      save(event.id.stats, 
           file=paste(name,"-Agg-STATS-EVENT.RData", sep=""))
  }
}

# Write all data in CSV format

write.csv(alldata, file=paste(name,".CSV", sep=""), row.names=FALSE,
           quote=TRUE)

if (make.agg) {
  for (i in 1:length(interval))
      write.csv(agglist[[i]], file=paste(name,"-Agg-",interval[i],unit,".CSV", sep=""), row.names=FALSE, quote=TRUE) 

  # Optionally make the WIDE versions of the CSV files
  if (make.wide) {
    write.csv(widedata.value.in.cols, 
              file=paste(name,"-Agg-WIDE-Value.CSV", sep=""), 
              row.names=FALSE, quote=TRUE)
    write.csv(widedata.time.in.cols, 
              file=paste(name,"-Agg-WIDE-Time.CSV", sep=""), 
              row.names=FALSE, quote=TRUE)
  }


  #  Optionally save the STATS tables
  if (make.stats) {
    write.csv(id.stats, 
              file=paste(name,"-Agg-STATS-ID.CSV", sep=""), 
              row.names=FALSE, quote=TRUE)
    if (merge.lookup)
      write.csv(lookup.id.stats, 
                file=paste(name,"-Agg-STATS-LOOKUP.CSV", sep=""), 
                row.names=FALSE, quote=TRUE)
    if (merge.events)
      write.csv(event.id.stats, 
                file=paste(name,"-Agg-STATS-EVENT.CSV", sep=""), 
                row.names=FALSE, quote=TRUE)
  }

}



# ---------------------------------------------

#   P L O T    D A T A 

# --------------------------------------------

# Create lattice diagnostic plots as a postscript file

cat("\nPlotting data...\n")

#  Create time axis for minimum day at midnight to maximum day+1 at midnight
#    in alldata  by the specified 'plot.interval' argument, 
#    defaulting to "hour".   This makes the time intervals fall on
#    event intervals, e.g., hour, half-hour, 4-h, 6-h, etc.
#  !!! See DateTimeClsses for info on elements of POSIXlt
min.time <- min(as.POSIXlt(alldata[[datetime.name]]))
max.time <- max(as.POSIXlt(alldata[[datetime.name]]))
min.year <- min.time$year + 1900
min.mon <- min.time$mon + 1
min.mday <- min.time$mday
max.year <- max.time$year + 1900
max.mon <- max.time$mon + 1
max.mday <- max.time$mday + 1

time.seq <- seq.POSIXt(ISOdatetime(min.year,min.mon,min.mday,0,0,0,tz=tz), 
                        ISOdatetime(max.year,max.mon,max.mday,0,0,0,tz=tz),
                        by=plot.interval)

#print(time.seq)

if (plot) {
   require(lattice)
   #print(head(alldata))
   plotnames <- c(elapsed.name, datetime.name, value.name, id.names,
                  value.id.name)
   if (merge.lookup) plotnames <- c(plotnames, lookup.value.names)
   if (merge.events) plotnames <- c(plotnames, event.value.names)
   plotdata <- subset(alldata, select=plotnames)

   # Diagnostic plot of all values in a separate panel
   if (make.svg) {
     require(RSvgDevice)
     devSVG(file=paste(name,"-VALUES",".svg",sep=""), 
              width=width, height=height)
   } else {
     postscript(file=paste(name,"-VALUES",".ps",sep=""), paper="letter")
   }
   #bitmap(file=paste(name,"-VALUES.jpeg",sep=""), width=width, height=height,
   #        type="jpeg")
   # jpeg(filename=paste(name,"%03d.jpeg",sep=""), width=width, height=height,
   #       units = "in", res=72)
   print(xyplot(get(value.name) ~ get(elapsed.name) | get(value.id.name),
                 data=plotdata, 
                 main=list(paste(main,"by",value.id.name), cex=cex.main), 
                 as.table=TRUE, cex=cex, type="b", 
                 ylab=list(ylab, cex=cex.lab), 
                 xlab=list(paste("Elapsed",unit), cex=cex.lab),
                 scales=list(cex=cex.axis)))
   dev.off()

   cat("Made overall diagnositc plot...\n")

   # Plots by each factor in the "id.names" variable
   require(grid)
   for (i in id.names) {
     npages <- 1 + length(unique(plotdata[[i]])) %/% 4
     #bitmap(file=paste(name,"-FILEID-",i,".jpeg",sep=""),width=width, height=height,
     #      type="jpeg")
     if (make.svg) {
       require(RSvgDevice)
       devSVG(file=paste(name,"-FILEID-",i,".svg",sep=""), 
             width=width, height=height)
     } else {
        postscript(file=paste(name,"-FILEID-",i,".ps",sep=""), paper="letter")
     }
     print(xyplot(get(value.name) ~ get(elapsed.name) | get(i),
           groups=Sensor, data=plotdata, 
           layout=c(2,2,npages), as.table=TRUE, 
           type="l", pch=16, lty=c("solid"), lwd=c(2,2), col=col.lines,
           panel=panel.superpose.3,
           scales=list(relation=scale.relation),
           ylab=list(ylab, cex=cex.lab), 
           xlab=list(paste("Elapsed",unit), cex=cex.lab),
           main=list(paste(main,"by",i), cex=cex.main)))
     dev.off()
   }


   # Plots by each factor in the Lookup table
   if (merge.lookup) {
     require(grid)
     for (i in lookup.value.names) {
       npages <- 1 + length(unique(plotdata[[i]])) %/% 4
       if (!all(is.na(plotdata[[i]]))) {
         if (make.svg) {
            require(RSvgDevice)
            devSVG(file=paste(name,"-LOOKUP-",i,".svg",sep=""), 
                  width=width, height=height)
         } else {
            postscript(file=paste(name,"-LOOKUP-",i,".ps",sep=""), 
                       paper="letter")
         }
         print(xyplot(get(value.name) ~ get(elapsed.name) | get(i),
             groups=Sensor, data=plotdata,
             layout=c(2,2,npages), as.table=TRUE,
             type="l", pch=16, lty=c("solid"), lwd=c(2,2), col=col.lines,
             panel=panel.superpose.3,
             scales=list(relation=scale.relation),
             ylab=list(ylab, cex=cex.lab),
             xlab=list(paste("Elapsed",unit), cex=cex.lab),
             main=list(paste(main,"by",i), cex=cex.main)))
         dev.off()
       }
     }
   }

   #  Create time plots segmented by Events....
   if (merge.events) {
     for (j in all.value.names) {
        plotdata <- alldata[alldata[[value.id.name]] == j, ]
          for (k in event.value.names) {
             if (!all(is.na(plotdata[[k]]))) {
               #print(j)
               #print(k)
               #print(plotdata[c(elapsed.name, value.name, k)])
               if (make.svg) {
                 require(RSvgDevice)
                 devSVG(file=paste(name,"-EVENTS-",j,"-",k,".svg",sep=""), 
                        width=width, height=height)
               } else {
                 #bitmap(file=paste(name,"-EVENTS-",j,"-",k,".jpeg",sep=""), 
                 #      width=width, height=height, type="jpeg")
                 postscript(file=paste(name,"-EVENTS-",j,"-",k,".ps",sep=""), 
                       paper="letter")
               }
               par(las=1)
               if (plot.times) {
                  areaplot.segmented(plotdata[[datetime.name]],
                           plotdata[[value.name]],
                           plotdata[[k]],
                           col=col.areas[1:length(unique(plotdata[[k]]))],
                           axes=FALSE)
                  axis.POSIXct(1, x=plotdata[datetime.name], 
                               at=time.seq, format=plot.format)
                  axis(2)
                  box()
               } else {
                  areaplot.segmented(plotdata[[elapsed.name]],
                           plotdata[[value.name]],
                           plotdata[[k]],
                           col=col.areas[1:length(unique(plotdata[[k]]))])
               }
               title(main=paste(name,j,k, sep=", "), xlab=xlab, ylab=ylab)
               dev.off()
             }
          }
      }
   }
}


# ----------------------------
#  A L L   D O N E

cat("\n...Finished processing '",name,"' files.\n\nYippee!\n\n")

}
rcdf<-
function (n, data, cumprob) 
{

# Function to randomly sample n values from
# an empirical cdf based on continuous numeric `data' with cumulative
# probabilities `cumprob'.

# Data can be raw continuous data, and cumprob omitted, to calculate
# the cumulative probabilities automatically.  Or data
# can be right bin limits, with the passed cumprob holding
# the corresponding probabilities.

# Note: cdf, the cdf plotting function in heR.Base, and this function
# currently don't deal with ties when raw data are passed.  Hopefully


data <- sort(as.numeric(data))
if (missing(cumprob))
   cumprob <- cumsum(rep(1/length(data),length(data)))

#linearly interpolate to estimate random percentiles 

sim <- vector(mode="numeric", length=n)
s <- runif(n)
for (i in 1:n) {
   j <- ceiling(s[i]*length(data)) #left index from percentile
   if (j == 0) j <- 1
   if (j == length(data)) j<-length(data)-1
   #print(paste(s[i],j))
   sim[i]<-(s[i]-cumprob[j])*((data[j+1]-data[j])/(cumprob[j+1]-cumprob[j])) + data[j]
}

sim

}
read.fwf.mult <- 
function (file, widths, rows, sep = "\t", as.is = FALSE, skip = 0, 
    row.names=NULL, col.names, n = -1, blank.lines.skip=FALSE, ...) 
{

# Read data files that have a different fixed-width format (FWF)
# for each record in a series of related contiguous records (lines).
# The series of records are converted to a single row as part of
# the returned dataframe object.

# Designed so that one can treat multi-line FWF files as file
# with a single large record for each group -- only specifying
# how many lines make up each group.

# This function was largely stolen from `read.fwf' by Brian Ripley.
# I only added the concatenation of multiple-records into a single
# record, and checking of missing/extra portions of a line.

# The function reads multiple contiguous records that `belong' to
# a single `individual', `group', or are somehow associated, and that
# each have a different format -- e.g., contain information
# for different variables.  If each record of your FWF data file 
# has the same format (i.e., the same variables), then you can also
# just use # `read.fwf' and split the data on a grouping/index variable.

# This function assumes that the *same* number of contiguous 
# records exists for each group.
# Each line of the groups are set equal to the sum of the passed
# widths for that line (in `widths') by adding spaces (blank characters) or
# removing characters.

# Blank characters (spaces) for a variable in the input file 
# are converted to NA.

#  Important:  By default blanks lines are *not* skipped, but
# by setting `blank.lines.skip=TRUE, blank lines (i.e., the blank space
# between two adjacent "\n" line feeds) will be ignored (skipped).

# Here `widths' is a list with each component containing the widths
# of the variables in subsequent lines of the multi-line group

# `col.names' is a similar list with each component containing
# the variable names for each group 

# `row.names' is the same as for `read.fwf' -- it specifies the names
# for the rows in the returned dataframe.

# `rows' contains the length (in rows) of the group of associated
# records (it is assumed to be the same for all individuals/groups)

# See `read.fwf' docs for information on the other arguments.

# TODO: Make it so we can have different numbers of rows per group 

    # check widths and col.names
    if (!is.list(widths) | length(widths) != rows | 
       (!missing(col.names) & (length(col.names) != rows
       | !is.list(col.names))))
    stop("`widths' and `col.names' (optional) should be lists containing column widths and names corresponding to each line of a group")
    if (!missing(widths) & !missing(col.names))
       for (i in 1:rows)
           if (length(widths[[i]]) != length(col.names[[i]])) {
              cat("Mismatch between `widths' and `col.names' at line ",i,"\n")
              stop("Check `widths' and `col.names'.")
           }

    # function to parse a "record" in fixed-width format
    doone <- function(x) {
        x <- substring(x, first, last)
        x[nchar(x) == 0] <- "NA"
        x
    }
 
    # function to return a character vector of blank strings n x " " of
    #    varying widths, x = vector of integers = # blanks per element
    # Used to pad lines with missing end-data
    blanks <- function (x) {
        # returns "" for 0 or negative inputs
        b <- character(length=length(x))
        for (i in 1:length(x)) 
            if (x[i] > 0)
               b[i] <- paste(rep(" ", x[i]), collapse="")
        b
    }

    FILE <- tempfile("Rfwf.")
    on.exit(unlink(FILE))

    # suck in all the records; each line is one vector element
    raw <- scan(file, what = "", sep = "\n", quote = "", quiet = TRUE, 
        n = n, skip = skip, blank.lines.skip=blank.lines.skip)

    # check for complete groups
    ngroups <- length(raw) %/% rows
    cat("Rows per group:",rows, "\n")
    cat("Number of total rows:",length(raw), "\n")
    cat("Number of groups:",ngroups, "\n")
    if (ngroups < length(raw)/rows)
       stop("Incomplete multi-line groups. Check input file.")

    # create factor delineating the groups
    f <- expand.grid(1:rows, 1:ngroups)[[2]]

    # either pad end of each line with spaces in case of missing data,
    #    which includes a premature end of line
    # or trim if too long
    # [converting data to a list with one multi-line group per component]
    group.widths <- sapply(widths, sum)
    fix.widths <- function (y) {
        # y is a character vector containing group lines
        p <- group.widths - nchar(y)
        bp <- blanks(p)
        for (i in 1:length(y)) {
            if (p[i] > 0) {
               y[i] <- paste(y[i], bp[i], sep="")
            } else if (p[i] < 0) {
               y[i] <- substr(y[i], 1, group.widths[i])
            }
        }
        y
    }
    raw <- tapply(raw, INDEX=f, FUN="fix.widths")
    
    # concatenate lines of each group
    raw <- lapply(raw, FUN="paste",sep="",collapse="")
     
    # write parsed "records" = concatenated multi-line groups
    #    to temp file
    widths <- unlist(widths)
    st <- c(1, 1 + cumsum(widths))
    first <- st[-length(st)]
    last <- cumsum(widths)
    cat(file = FILE, sapply(raw, doone), sep = c(rep(sep, ,
                   length(widths) - 1), "\n"))

    # finally, read in reformatted file as a regular table
    if (!missing(col.names)) col.names <- unlist(col.names)
    read.table(file = FILE, header = FALSE, sep = sep, as.is = as.is, 
        row.names = row.names, col.names = col.names, 
        quote = "", ...)

}
rearrange.table <-
function (x, stat, hfactors, vfactors, latex=FALSE,
          file="", display="f", digits=1, blank=TRUE) 
{

# Function to take a flat summary statistics
# table consisting of a dataframe containing a number of columns for
# unique factor combinations and columns containing summary statistics.

# We choose a column containing a particular summary statistic and
# a subset of factor combinations to place along a row header.  Thus,
# this function converts a "long" dataframe table into a "wide" one
# for a single statistic.

# `stat' is the columns (name or index) containing the desired statistic
# `hfactors' are the columns (names or indices) containing the factors
#  that will appear horizonally across the table header (i.e.,column names).
# `vfactors' are the columns to keep as the vertical headings (i.e.,row names).

# Important Note:  I don't think this function will work unless there are 
#    complete, sorted combinations of the factors.   If any combination of
#    factors is missing, the function will get confused and choke.  The
#    dataframe should be sorted in the way that data is desired to be 
#    presented.   We could, I suppose, both fill in missing combinations and
#    resort the data based on the order that factors are selected, but we
#    don't do that right now... [the sorting would be pretty easy, I think.]

# Update.  We now sort the combinations.  and optionally write the 
#          table in LaTeX format.
# ----------------------------------------------------------------------

if (!is.data.frame(x)) stop("`x' must be a dataframe.")
if (length(stat) != 1) stop("A single `stat' must be given.")
if (any(hfactors %in% vfactors))
  stop("`hfactors' and `vfactors' must be mutually exclusive.")
s <- x[stat]
hf <- x[hfactors]
vf <- x[vfactors]
# Make sure we have the names and not just indices
stat <- names(s)
hfactors <- names(hf)
vfactors <- names(vf)

temp <- cbind(vf, hf, s)
temp <- temp[do.call("order",temp[c(vfactors,hfactors)]),]
split.temp <- split(temp, f=temp[rev(vfactors)])
if (!all(sapply(split.temp, length) == length(split.temp[[1]])))
  stop("Unequal numbers of entries across vertical factor combinations.")

h <- split.temp[[1]][[stat]]
v <- unique(split.temp[[1]][vfactors])
if (length(split.temp) > 1)
  for (i in 2:length(split.temp)) {
     if (!identical(as.vector(as.matrix(split.temp[[i]][hfactors])),
                    as.vector(as.matrix(split.temp[[i-1]][hfactors]))))
       stop("Mismatch between horizontal factors across vertical groups.")
     else
        h <- rbind(h, split.temp[[i]][[stat]])
     v <- rbind(v, unique(split.temp[[i]][vfactors]))
  }
if (blank) v <- blank.factors(v)  
rownames(h) <- 1:length(split.temp)
n <- split.temp[[1]][hfactors]
for (i in 1:length(n)) n[[i]] <- paste(names(n)[i],n[[i]],sep=".")
colnames(h) <- do.call("paste", c(as.list(n), sep="-"))
res <- cbind(v, h)    
if (latex) {
  require(xtable)
  print.xtable(xtable(res,display=c("d",rep("s",NCOL(v)),rep(display,NCOL(h)) ),digits=c(0,rep(0,NCOL(v)),rep(digits,NCOL(h)))), file=file)
}
res
}


rose2 <- function(dir, mag=NULL, nplumes=16,
                  shrink=0.6, shrink.top=0.55, shrink.bottom=0.7, 
                  fg="black", bg="linen", border="black", lwd.border=1.9,
                  mag.bins=c(0.5,1,2,4,8,16,32,100),
                  mag.col=c("springgreen","cyan","blue","magenta",
		            "yellow","tomato","tan"),
	          fg.lows="black", bg.lows="azure",
                  rscale=NULL, rings=TRUE, ring.labels=TRUE, lwd.ring=1,
		  lty.ring=2, col.ring="black", lwd.axis=2, 
		  cex.lab=1.5, cex.dir=3, ...)
{

#  IMPORTANT NOTE:  The wind rose shows wind direction `dir' in terms of the
#   direction that wind is COMING FROM.  In other words, a wind direction
#   `dir' of NORTH (0 degrees) means that the wind is coming out of the north.

#  shrink.top=0.7, shrink.bottom=1 or 0.9 is OK too -- a little thicker
#          telescope effect

#  UPDATE:  Now deals with NA values properly for dir.  Also we change
#           the midpoints of direction to be centered on the four
#           primary directions: N,S,E,W  --NK 27-10-04

#  UPDATE:  Default number of plumes is now 16, so that if wind direction is
#            in any of primary directions N-S-E-W or 45 degree directions
#            NW, NE, SE, SW, then the plumes point exactly 
#            in these directions.   NK 27-10-04
#                         

# rose -- from R-help archives
#
# rose2:  updated version of David Finlayson's `rose' by Neil Klepeis, 
#        adding space between plumes and segmented
#        plumes representing different magnitudes of wind speed.

# TODO:   Add percent labels to rings
#         Add plume segments for wind speed
#         Add space between plumes as a fraction of total sector angle
#          --> Do overall shrink and then extra shrink for top and bottom
#             of magnitude segments, to give telescoping effect.
#         Center segment is for calm winds (zero)
#          --> OK,  add a center circle corresponding to low winds, i.e., under
#             1 m/s, thereby assigning it equally to all directions
#              (really turbulence eddies), report the percentage of low winds.
#             Low winds cannot really be assigned reliably to any direction, 
#             user determines magnitude bins and what constitutes low winds,i.e.
#             the lowest bin.

# Degrees for direction are interpreted as follows:
#  [Note:  These are directions that wind is COMING FROM]

# North = 0 degrees
# East = 90
# South = 180
# West = 270

# We convert to polar coordinates prior to plotting.....


# --------------------------------------------------------
#
# Directional Vector Histogram (Rose Diagrams).
# -------------------------------------------
# David Finlayson (with help from Joerg Maeder and Ben Bolker)
# david_p_finlayson@hotmail.com
# Version 1.0
# November 23, 2001
#
# Use for plotting directional data such as wind direction or the
# angles of imbricated pebbles in rivers and streams. This is basically
# an extension of the hist(x) function though I did not implement all of
# hist. I have placed limits on the range of bins so that they always
# fall within 0 and 360 (i.e. directions of the compass). The standard
# color and line adjustment commands work as well but you will need to add
# annotation (i.e.. main, xlab, ylab) separately (see par).
#
# bins: Approximate number of bins see hist() function for details
# rscale: Ring Scale, the approximate number of rings for scaling see pretty()
# NULL value will call pretty() with default number of rings
# labels: (T/F) draw labels for the top 10% largest petals and the
#          cardinal dirctions?
# rings: (T/F) draw scale rings?
#
# example:
# dir <- runif(30) * 360
# mag <- runif(30) * 50
# rose2(dir,mag)
#
# ----------------------------------------------------------------------
# ======================================================================


 # Check the wind speed (mag) data, if given, assign to one value, otherwise
 if (!is.null(mag)) {
   plotmag <- TRUE
   if (length(mag.bins) != length(mag.col)+1)
     stop("`mag.bins' must be the same length as `mag.col' plus 1.")
   if (min(mag, na.rm=TRUE) < 0)
     stop("If specified, `mag' must contain non-negative magnitude data.")
 } else {
   plotmag <- FALSE
   mag <- rep(0, length=length(dir))
   mag.col <- rep(mag.col[length(mag.col)], length=length(mag.col))
   mag.bins <- c(0,1)
 }  
    
 ##  Remove Missing Data
 good <- complete.cases(mag, dir)
 mag <- mag[good]
 dir <- dir[good]
 if (NROW(good) != NROW(dir))
   cat(NROW(good)-NROW(dir),"Missing Value(s) Was(Were) Deleted.\n")


 ### Ensure that `direction' is angular data (0-360)
 if (max(dir) > 360 || min(dir) < 0) 
  stop("Direction data in `dir' is out of range (0 - 360)")


 ##  Create factors of wind speed and direction

# Changed to have mids be centered on four primary directions, N S E W
# breaks <- seq(0,360,length=nplumes+1)
# mids <- breaks[-(nplumes+1)] + diff(breaks)/2

 width <- 360 / nplumes
 # convert last half-plume to equivalent negative angles
 dir[dir > 360-width/2] <-  dir[dir > 360-width/2] - 360
 mids <- seq(0, 360-width, by=width)
 breaks <- seq(-width/2, 360-width/2, length=nplumes+1)
 width <- shrink * width

 n <- length(mag)
 lows <- mag < mag.bins[1]
 nlows <- sum(lows)
 plows <- 100 * nlows / n
 mag <- mag[!lows]
 dir <- dir[!lows]

 dir <- cut(dir, breaks=breaks) 
 mag.labels <- paste(mag.bins[-length(mag.bins)],"-",mag.bins[-1],sep="")
 mag <- cut(mag, breaks=mag.bins, labels=mag.labels, include.lowest=TRUE) 
 
# mag.labels[length(mag.labels)] <- paste(mag.bins[length(mag.bins)],"+",sep="")
# mag <- cut(mag, breaks=mag.bins, include.lowest=TRUE) 

 nmags <- length(levels(mag))

 ##  Get list of percentages for each wind direction and
 ##        speed

 tab <- as.data.frame(table(mag=mag, dir=dir))
 tab$Perc <- 100 * tab$Freq / n
 totals <- aggregate(tab$Perc, by=list(tab$dir), FUN=sum)$x
 perc <- split(tab$Perc, tab$dir)
 perc.lows <- plows / nplumes
 cumul <- lapply(perc, FUN=function(x) c(0,cumsum(x)[-length(x)]))


 cat("Direction Midpoints (",nplumes,") :",mids,"\n")
 cat("Direction Breaks (",nplumes,") :", breaks,"\n")
 cat("Magnitude Bins (",nmags,") :",mag.labels,"\n")
 cat(plows,"% of Magnitudes Are Below",mag.bins[1],"\n")
 cat("Low Magnitudes Distributed Among",nplumes,"Directions:", perc.lows,"% each direction \n")
 


 ### Initialize Plot Area

 oldpar <- par()
 par(pty="s", las=1, xpd=FALSE, bg=bg, fg=fg)

 if (!is.null(rscale)){
  rscale <- pretty(rscale)
 } else {
  rscale <- pretty(c(totals+perc.lows, tab$Perc))
 }

 limits <- c(-max(rscale), max(rscale)) * 1.04

 plot(0,0, ylim=limits, xlim=limits, axes=FALSE,
         xlab="", ylab="", type="n")

 abline(h=0, lwd=lwd.axis)
 abline(v=0, lwd=lwd.axis)

 
 ### Plot Rings
 
 if (rings == TRUE) {
   symbols(rep(0,length(rscale)-1), rep(0,length(rscale)-1),
           circles=rscale[-1], inches=FALSE, add=TRUE, lwd=lwd.ring,
           lty=lty.ring, fg=col.ring)
   text(-rscale[-1]*cos(pi/6), -rscale[-1]*sin(pi/6),
        labels=paste(rscale[-1],"%",sep=""), pos=2, offset=0.6,
	cex=cex.lab) 	   
   text(-rscale[-1]*cos(pi/6), rscale[-1]*sin(pi/6),
        labels=paste(rscale[-1],"%",sep=""), pos=2, offset=0.6,
	cex=cex.lab) 	   
   text(rscale[-1]*cos(pi/6), rscale[-1]*sin(pi/6),
        labels=paste(rscale[-1],"%",sep=""), pos=4, offset=0.6,
	cex=cex.lab) 	   
   text(rscale[-1]*cos(pi/6), -rscale[-1]*sin(pi/6),
        labels=paste(rscale[-1],"%",sep=""), pos=4, offset=0.6,
	cex=cex.lab) 	   
 }	   
 

 ### Plot Rose

 # Helper function to draw the pie slices, i.e., individual
 #   plume segments corresponding to each wind magnitude at
 #   a given direction.

 pie <- function(idx, x0, y0, r, direction, angle, magnitude, col,
                 label=FALSE, ...)

 {
    # --- Parameters: -----
    # idx is the index used to select colors and stuff
    # x0,y0 are the coordinates of the origin where plume emanates from
    # r is the starting radial length for the segment being drawn
    # direction is the angle where the plume segment is centered
    # angle is the spread, i.e., sector angle, for the plume segment
    # magnitude is the radial length corresponding to the
    #     current air speed, i.e., current segmen
    # col is the color for the current magnitude segment
    # label determines whether or not segment labels are drawn

    # 0 degrees equals North

    # adjust coordinate from compass to polar
    direction <- 360 - direction + 90

    # calculate theta start and stop
    start.top <- (direction + (0.5) * angle * shrink.top) * pi / 180
    stop.top <- (direction - (0.5) * angle * shrink.top) * pi / 180

    start.bottom <- (direction + 0.5 * angle * shrink.bottom) * pi / 180
    stop.bottom <- (direction - 0.5 * angle * shrink.bottom) * pi / 180

    #print(list(bottom=c(start.bottom,stop.bottom)*180/pi,
    #           top=c(start.top, stop.top)*180/pi))
    
    # Get four points of polygon (slice)

    x1 <- r*cos(start.bottom) + x0
    y1 <- r*sin(start.bottom) + y0
    
    x2 <- r*cos(stop.bottom) + x0
    y2 <- r*sin(stop.bottom) + y0

    x3 <- (magnitude + r)*cos(stop.top) + x0
    y3 <- (magnitude + r)*sin(stop.top) + y0

    x4 <- (magnitude + r)*cos(start.top) + x0
    y4 <- (magnitude + r)*sin(start.top) + y0

    x <- c(x1, x2, x3, x4)
    y <- c(y1, y2, y3, y4)

    polygon(x, y, col=col, lwd=lwd.border, border=border, ...)

    if (label)
      text(x2, y2 + 0.5*(y3-y2), mag.labels[idx], pos=2, offset=0.6,
           cex=cex.lab)

 }


 ##  Plot ring of low magnitudes
 if (plotmag)
   symbols(0,0, circles=perc.lows, inches=FALSE, add=TRUE,
           bg=bg.lows, fg=fg.lows)

 ### Plot the slices, with a base percentage from
 #    low magnitudes spread equally in each direction
 for (i in 1:nplumes)
    for (j in 1:nmags) 
       if (perc[[i]][j] > 0) {
         pie(j, 0, 0, perc.lows + cumul[[i]][j], mids[i], width,
	     perc[[i]][j], mag.col[j])
         }	     


 # Plot Direction Labels

 mtext("N", side=3, line=1.3, cex=cex.dir, adj=0.5)
 mtext("E", side=4, line=1.3, cex=cex.dir, adj=0)
 mtext("S", side=1, line=1.5, cex=cex.dir, adj=0.5)
 mtext("W", side=2, line=1.3, cex=cex.dir, adj=1)


 ### Plot Legends for Magnitudes

 if (plotmag) {
   par(xpd=NA)
   # legend(-max(rscale), -0.5*max(rscale), xjust=1, yjust=0.5,
   #       legend=levels(mag), fill=mag.col, cex=1, bty="n")
   coord <- par()$usr
   leg.perc <- (1/nmags)*abs(coord[3]-coord[4])/2
   leg.cumul <- c(0, cumsum(rep(leg.perc, nmags)))
   for (j in 1:nmags) 
      pie(j, coord[1], -0.15*abs(coord[3]-coord[4])/2,
          leg.cumul[j], 180, 15, leg.perc, mag.col[j], label=TRUE)
   text(coord[2], coord[4], 
        paste(round(plows),"%"," Low Magnitudes", sep=""), pos=2,
	cex=cex.lab)
   symbols(coord[2]-0.06*abs(coord[2]-coord[1]),
           coord[4]-0.06*abs(coord[4]-coord[3]),
           circles=0.03*abs(coord[4]-coord[3]), inches=FALSE, add=TRUE,
  	   fg=fg.lows, bg=bg.lows)
 }	   


 ### Reset the par
 par <- oldpar

 ### Return invisible list of percents for each speed/direction
 invisible(tab)
 
}
# rose -- from R-help archives
#
# Directional Vector Histogram (Rose Diagrams).
# -------------------------------------------
# David Finlayson (with help from Joerg Maeder and Ben Bolker)
# david_p_finlayson@hotmail.com
# Version 1.0
# November 23, 2001
#
# Use for plotting directional data such as wind direction or the
# angles of imbricated pebbles in rivers and streams. This is basically
# an extension of the hist(x) function though I did not implement all of
# hist. I have placed limits on the range of bins so that they always
# fall within 0 and 360 (i.e. directions of the compass). The standard
# color and line adjustment commands work as well but you will need to add
# annotation (i.e.. main, xlab, ylab) separately (see par).
#
# bins: Approximate number of bins see hist() function for details
# rscale: Ring Scale, the approximate number of rings for scaling see pretty()
# NULL value will call pretty() with default number of rings
# labels: (T/F) draw labels for the top 10% largest petals and the
#          cardinal dirctions?
# rings: (T/F) draw scale rings?
#
# example:
# test <- runif(30) * 360
# rose(test)
# rose(test, bins=10, rscale=2, labels=TRUE, rings=TRUE, col="cyan", lwd=2)
#

rose <- function(x, bins=36, rscale=NULL, labels=TRUE, rings=TRUE, ...){

 ### Ensure that this is directional data (0-360)
 if (max(x) > 360 || min(x) < 0) {
  stop("Data is out of range (0 - 360)")
 }

 ### Histogram Data
 histogram.out <- hist(x, breaks=seq(0,360,length=bins+1), plot=FALSE)
 pieMid <- histogram.out$mids # mid points of bins
 pieCount <- histogram.out$counts # count in each bin
 pieWidth <- 360 / length(pieMid) # width of each bin
 pieFreq <- histogram.out$density * pieWidth

 ### Initialize Plot Are
 oldpar <- par()
 par(pty="s")

 if (!is.null(rscale)){
  rscale <- pretty(pieFreq, rscale)
   } else {
  rscale <- pretty(pieFreq)
 }

 plotLimits <- c(-max(rscale), max(rscale)) * 1.04

 plot(0,0,
  ylim=plotLimits,
  xlim=plotLimits,
  axes=FALSE,
  xlab="",
  ylab="")

 abline(h=0)
 abline(v=0)

 ### Plot Rings
 if (rings == TRUE) {
  symbols(rep(0,length(rscale)), rep(0,length(rscale)),
   circles=rscale,inches=FALSE,add=TRUE)
 }

 ### Plot Rose

 # Helper function to draw the pie slices
 pie <- function(h, k, direction, spread, magnitude, ...){

 # adjust coordinate from compass to polar
 direction <- 360 - direction + 90

 # calculate theta start and stop
 start <- (direction + 0.5 * spread) * pi / 180
 stop <- (direction - 0.5 * spread) * pi / 180

 # vertices
 x1 <- h
 y1 <- k

 x2 <- (magnitude * cos(start)) + h
 y2 <- (magnitude * sin(start)) + k

 x3 <- (magnitude * cos(stop)) + h
 y3 <- (magnitude * sin(stop)) + k

 # build pie slice
 x <- c(x1, x2, x3, x1)
 y <- c(y1, y2, y3, y1)

  polygon(x,y, ...)
 }

 # plot the slices
 for (i in 1:length(pieMid)){
   pie(0, 0, pieMid[i], pieWidth, pieFreq[i], ...)
 }

 # Plot Axes Labels

 if (labels == TRUE) {

     mtext("N", side=3, line=1)
     mtext("E", side=4, line=1)
     mtext("S", side=1, line=1)
     mtext("W", side=2, line=1)

     ### Plot top frequency labels

     # calculate indices of top 10 percent of bins
     pie10percent <- round(length(pieMid) * 0.1) + 1 # how many is 10 percent
     pieRank <- length(pieCount) - rank(pieCount) # rank bins
     top10 <- which( pieRank < pie10percent ) # index to top 10 percent

     # Plot labels for these bins
     theta <- 360 - pieMid[top10] + 90 # compass to polar angles
     theta <- theta * pi/180 # degrees to rads

     x <- pieFreq[top10] * cos(theta)
     y <- pieFreq[top10] * sin(theta)

     text(x, y, format(pieFreq[top10], digits=2))

     ### Reset the par
     par <- oldpar

     ### Return Histogram Object
     histogram.out
 }
}
rsquared<-
function(ls.out)
{

#  Function to calculate the "r-squared" statistic, referred
#   to as the coefficient of determination.  Given two matched
#   data vectors x and y, it is the proportion of variation
#   in the reponse, y, that can be attibuted to an approximate
#   linear relationship between x and y, i.e., the amount of variation
#   in y that is "explained" by x.

#  SSresid = the residual sum of squares
#  SStotal = the total sum of squares
#   R-squared = 1 - SSresid/SSTotal

#  Stolen from the ls.print function.  Calculates
#    a bunch of stuff.

#  Takes output of the lsfit function...... 

resids <- as.matrix(ls.out$residuals)
if (!is.null(ls.out$wt)) {
  if (any(ls.out$wt == 0)) 
    warning("Observations with 0 weights not used")
  resids <- resids * ls.out$wt^0.5
}
n <- apply(resids, 2, length) - colSums(is.na(resids))
lsqr <- ls.out$qr
p <- lsqr$rank
if (ls.out$intercept) {
  if (is.matrix(lsqr$qt)) 
    totss <- colSums(lsqr$qt[-1, ]^2)
  else totss <- sum(lsqr$qt[-1]^2)
  degfree <- p - 1
} else {
  totss <- colSums(as.matrix(lsqr$qt^2))
  degfree <- p
}
resss <- colSums(resids^2, na.rm = TRUE)
#resse <- (resss/(n - p))^0.5
regss <- totss - resss
rsquared <- regss/totss
fstat <- (regss/degfree)/(resss/(n - p))
pvalue <- pf(fstat, degfree, (n - p), lower.tail = FALSE)

rsquared

}
																	

run.ci <- 
function(x, b=0.9, e=0.1, ...)
{

# Calculate the running confidence interval half-length of the sample mean
#   starting with just the first value (NA) and ending with all the values.
# All returned vectors/matrices have a length/nrows one less than for `x'
#   Returns a list with three components:
#   1. a vector of confidence interval half-lengths 
#   2. a matrix containing columns for X-ci, X, and X+ci,
#   where X is the sample mean, and ci is the confidence half length,
#   3. a vector of the relative errors calculated as the confidence interval
#      half length divided by the sample mean
#   4. The element of x where the relative error drops below the
#         adjusted`e', which is calculated as e/(1-e).

#  See "Simulation Modeling and Analysis" by Law and Kelton, p. 539 for
#     more information.

#  `x' is the sample vector
#  `b' is the confidence interval bounds, (0,1)
#  `e' is the desired relative error, (0,1)

alpha <- 1 - b
z <- abs(qnorm(alpha/2))
cint <- NA
smean <- NA
for (i in 2:length(x)) {
   cint[i-1] <- z * sd(x[1:i], ...) / sqrt(i)
   smean[i-1] <- mean(x[1:i], ...)
}   
rel <- cint/smean
n <- which(rel <= e/(1-e))[1]
list(ci=cint, m=cbind(smean+cint, as.numeric(smean), smean-cint),
     rel=cint/smean, n=n)
}


runmean <- 
function(x,...)
{

# Calculate the running mean of a vector, starting with just the
#   first value and ending with all the values.  Returns a vector
#   of means equal in length to the original vector.

res <- x[1]
for (i in 2:length(x))
   res[i] <- mean(x[1:i],...)
res
}


runmedian <- 
function(x,...)
{

# Calculate the running median of a vector, starting with just the
#   first value and ending with all the values.  Returns a vector
#   of medians equal in length to the original vector.

res <- x[1]
for (i in 2:length(x))
   res[i] <- median(x[1:i],...)
res
}


runsd <- 
function(x,...)
{

# Calculate the running standard deviation of a vector, starting with just the
#   first value (NA) and ending with all the values.  Returns a vector
#   of standard deviations equal in length to the original vector.

res <- NA
for (i in 2:length(x))
   res[i] <- sd(x[1:i], ...)
res
}


runstderr <- 
function(x,...)
{

# Calculate the running standard error (sample standard deviaton of the mean)
#   starting with just the first value (NA)  and ending with all the values.
#   Returns a vector of standard errors equal in length to the original vector.

res <- NA
for (i in 2:length(x))
   res[i] <- sd(x[1:i], ...) / sqrt(i)
res
}


scoeff<-
function(par.base, par.pert, resp.base, resp.pert)
{

# Calculate normalized sensitivity coefficients given central, base
#   values for parameters and corresponding responses with plus and/or minus
#   perturbations.  The perturbation values can be in any order
#   but the response perturbations must correspond exactly to
#   the parameter perturbations.

if (length(par.base) != 1 | length(resp.base) != 1)
  stop("The base values must be 1 element in length.")
if (length(par.pert) != length(resp.pert))  
  stop("The parameter perturbation must be matched by the response perturbations.")

par <- (par.base - par.pert)/par.base
resp <- (resp.base - resp.pert)/resp.base
sens <- resp/par
list(base=list(params=par.base, response=resp.base),
     pertval=list(params=par.pert, response=resp.pert),
     pert=list(params=par, response=resp),
     sens=sens)



}
sens<-
function (a, p, plot=FALSE, perspective=FALSE) {

# `sens' : R function to calculate sensitivity coefficients from an array
# consisting of low, medium, and high function values corresponding
# to an arbitrary number of parameters

# Current coeff: normalized change in optimization surface with a
# normalized change in parameter, given all other parameters are held
# constant at their central value. [normalization is by the central value]

# a - array of function values, with dimension equal to 3 for
#     each level (corresponding to each parameter)
# p - matrix of low, medium, and high values in columns
#     corresponding to each parameter

# if plot=TRUE, then a matrix of plots is produced of f(p[,i]) vs. p[,i]
# for each parameter

p<-as.array(p)  # parameter value array'
a<-as.array(a)   # function value array
n<-NCOL(p)
smeanabs<-vector(length=n, mode="numeric")  # mean abs sensitivity coeff vector
sall<-array(NA,c(n,2))  # sensitivity coeff array for all 

if (NROW(p) != 3)
   stop("`p' columns must contain low, medium, and high values of each parameter")

if (dim(a) != rep(3,NCOL(p)))
   stop("Dimension of array `a' must be 3 for each parameter in `p'.")

if (plot && !perspective)
   lo(5, land=FALSE)

for (i in 1:n) {
    z<-array(rep(2,3*n), c(3,n))  # function value index array
    z[1:3,i] <- 1:3
    # normalized changes in function surface and parameter
    dc <- mean(abs(diff(a[z])))/a[array(rep(2,n),c(1,n))]
    dp <- mean(abs(diff(p[,i])))/p[2,i]
    smeanabs[i] <- dc/dp
    dc <- diff(a[z])/a[array(rep(2,n),c(1,n))]
    dp <- diff(p[,i])/p[2,i]
    sall[i,] <- dc/dp
    if (plot && !perspective) 
       plot(p[,i],a[z], type="b", main=paste(i,": [",format(sall[i,1],dig=3),",", format(sall[i,2],dig=3),"] :","[",format(smeanabs[i],dig=3),"]"), xlab=paste("Parameter",i, "Values"), ylab="Function Values")
    
} 

if (plot && perspective) {
   old.par<-par(no.readonly<-TRUE)
   on.exit(par(oldpar))
   par(mar=c(0,0,0,0)+1.25)
   idx<-matrix(c(4,5),nrow=1, ncol=2)
   for (i in 1:(n-2))
       idx <- rbind(idx, as.matrix(expand.grid(i,(i+1):n)))
   o<-do.call("order",list(idx[,1], idx[,2]))
   idx<-idx[o,]
   lo(NROW(idx), land=F)
   for (i in 1:NROW(idx)) { 
       z<-array(rep(2,9*n), c(9,n))  # 3x3 grid function value index array
       z[,idx[i,]]<-as.matrix(expand.grid(1:3,1:3))
       persp(p[,idx[i,1]], p[,idx[i,2]], matrix(a[z],ncol=3, nrow=3),
             theta=30, phi=30,
             main=paste(idx[i,1],":",idx[i,2]), xlab="", ylab="", zlab="",
             axes=F )
   }
}

lo(1)

list(meanabs=smeanabs, all=sall)   
 
}
sim.distrib <-
function (x, n=1)
{

# Function to return a sample from an object of type `distrib'

if (!inherits(x, "distrib"))
  stop("`x' must be of class 'distrib'.")

# TODO:  Add a way to "simulate" a point estimate, when only
#        one value is given, and "sample" is given for the
#        `simulate' spec.

if (x$simulate == "sample") {
  if (length(x$values) == 1)
    return(x$values)
  else
    return(sample(x$values, size=n, prob=x$weights))
} else {
  params <- split(x$values, f=1:length(x$values))
  names(params) <- NULL
  return(do.call(x$simulate, c(list(n=n), params)))
}

}
sin.sum<-
function  (t,wave=c(24, 24, 24*7, 24*7*365),
          phase=c(rep(0,length(wave))),
          amp=c(0,rep(1,length(wave))))
{

# `sin.sum' : R function to calculate a time series based on the
# sum of an arbitrary number of sin functions with different
# wavelengths, phaes, and amplitudes.

# `t' is a vector of increasing times to calculate the model
# `wave' contains the wavelengths of the sin functions
# `phase' contains the initial phases for the sin functions
# 'amp' contains the initial amplitudes for the sin functions

if (length(wave) != length(phase))
   stop("`wave' and `phase' must be equal length vectors.")
if (length(wave) != (length(amp)-1)) {
   cat("`wave' has length ", length(wave), "\n")
   cat("`amp' has length ", length(amp), "\n")
   stop("`amp' must be 1 element longer than `wave'")
}
if ((length(amp)-1) != length(phase)) {
   cat("`amp' has length ", length(amp), "\n")
   cat("`phase' has length ", length(phase), "\n")
   stop("`amp' must be 1 element longer than `phase'")
}

n <- length(wave)
f <- amp[1]
for (i in 1:n)
   f <- f + amp[i+1]*sin((t-phase[i])*2*pi/wave[i])
f

}
spline.colors<-
function(n,h=c(0,0.3,0.6),s=c(1,0.3,1),v=1, gamma=1)
{

# Pick `n' evenly spaced colors (in the "hue" direction)
#    along a spline curve fit through the passed values
#    of hue (h) and saturation (s).  "Value" and "gamma" are fixed
#    at the passed values (v and gamma).

fit <- spline(x=h, y=s, n=n)
hsv(h=fit$x, s=fit$y, v=v, gamma=gamma)


}
stack.up.data<-
function(z) {

# convert columns of data in a data frame or matrix
# into stacked data sets with a factor variable
# identifying each of the original data variables
# The first column is taken as the independent variable
# and is repeated for each stacked data set.  Returns
# a data frame containing the stacked data (1 column for
# the independent variable and 1 column for the dependent
# variables), with a 3rd column containing the identifying
# factor.

Id<-c()
df<-data.frame()
names <- names(z[2:NCOL(z)])
for (i in 2:(NCOL(z))) {
    names(z)[i] <- "Data"
    df <- rbind(df, z[,c(1,i)])
    Id <- c(Id,rep(i-1, NROW(z)))
}
Id <- factor(Id, labels=names)
cbind(df, Id)
}
stat.tables<-
function (x, tablespecs, latex=FALSE, file="")
{

# Function to return a dataframe containing summary statistics
#    for number of different subgroups.  Uses a dataframe and a
#    `tablespecs' object as input.  Invalid (missing) values
#    in each subgroup are removed before statistics are calculated.

# UPDATED:  Fixed blankLabels to use as.integer instead of `codes', same
#              as with blank.factor routine.   NK 13-Mar-04

# Updated: Added overall statistic.  -- 15-Jul-03.

# We take a dataframe x and a list of equal-length factors corresponding
#   to each row, and use the `aggregate' function to return a 
#   dataframe containing the statistics.  The `xtable' function is
#   used to optionally print out the data frame in LaTeX format.
#   The dataframe object is returned regardless.

if (!inherits(tablespecs, "tablespecs"))
  stop("`tablespecs' argument must be an object of class `tablespecs' created using the `tablespecs' function.")

responses <- tablespecs$responses
keys <- tablespecs$keys
key.cuts <- tablespecs$key.cuts
right <- tablespecs$right
include.lowest <- tablespecs$include.lowest
key.labels <- tablespecs$key.labels
blank.labels <- tablespecs$blank.labels
stats <- tablespecs$stats
digits <- tablespecs$digits
display <- tablespecs$display


if (any(!responses %in% names(x)))
  stop(paste("One or more response variables do not exist:",paste(responses,collapse=", ")))

if (any(!keys %in% names(x)))
  stop(paste("One or more key variables do not exist:",paste(keys,collapse=", ")))
if (!is.list(key.cuts) | length(key.cuts) != length(keys))
  stop("`key.cuts' must contain breaks for cutting each of the key variables, or NA to skip cutting for a particular variable (assuming it is already in an appropriate factor format.")

# Select dataframe variables
xresponses <- x[responses]
xkeys <- x[keys]

m <- length(responses)
n <- length(keys)
# Cut the key variables
for (i in 1:n) {
   if (!any(is.na(key.cuts[[i]]))) 
     xkeys[[i]] <- cut(xkeys[[i]], breaks=key.cuts[[i]],
                        labels=key.labels[[i]],
		        right=right, include.lowest=include.lowest)

   # Add a level for overall statistics
   if (!is.factor(xkeys[[i]])) xkeys[[i]] <- factor(xkeys[[i]])
#  levels(xkeys[[i]]) <- c(levels(xkeys[[i]]), "Overall")
}	



# Function to create an easy-to-read version of the key labels,
#     by blanking labels in each group of nested labels except
#     the leading one, where `x' is a dataframe of key labels
blankLabels <- function (x) {
  for (i in 1:length(x)) {
     x[[i]] <- factor(x[[i]])
     nums <- c(1, diff(as.numeric(x[[i]])))
     levels(x[[i]]) <- c(levels(x[[i]]), "Overall", "")
     x[[i]][nums==0] <- ""
  }
  x
}


res <- list()
for (i in responses) {
   s <- list()
   os <- list()
   # get complete cases for the current response (no NAs)
   good <- complete.cases(xresponses[i], xkeys)
   for (j in 1:length(stats)) {
      data <- as.data.frame(xresponses[good,])
      names(data) <- names(xresponses)
      index <- as.data.frame(xkeys[good,])
      names(index) <- names(xkeys)
      # Calculate overall statistics
      o <- get(stats[[j]])(data[[i]])
      os <- c(os, o)
      # Calculate subgroup statistics
      r <- aggregate.data.frame(data[i],
                                by=index[n:1],
                                FUN=get(stats[[j]]))
      s <- c(s, r[i])
   }
   names(os) <- stats
   names(s) <- stats
   k <- r[keys]
   if (blank.labels) k <- blankLabels(k)
   ko <- as.data.frame(k[1,]) 
   names(ko) <- names(k)
   ko[1,] <- rep("Overall", n)
   res[[i]] <- rbind(cbind(ko, os), cbind(k, s))
   if (latex) {
     require(xtable)
     print.xtable(xtable(res[[i]], digits=digits, display=display),
                  file=file, append=TRUE)
   }
}

if (length(res) == 1) res <- res[[1]]
res

}
striptext<-
function(labels, Y=0, X=seq(along=labels), x=NULL, y=NULL, as.colors=FALSE,
         rotated=FALSE, vfont=c("sans serif","plain"), col="black", cex=1,
	 border=TRUE, to.edge=TRUE, border.col="black", border.lwd=2, 
	 border.offset=c(0,0), fill=NA, arrow.lwd=1, arrow.angle=30,
	 arrow.length=0.1, arrow.col="black", ...)
{

# Function to plot a strip of labels on a plot at a single Y
#   value and specified x values, with optional border
#   and fill colors

# Idea:  Add a default option to extend the strip border to the edge of the 
#    graph .... Done.

# Also:  Add a default option to stack strips automatically 
#          if a matrix is supplied for x,labels....Not done. Maybe later.

# Also:  Add an option for writing full text labels that are rotated, or
#          to plot the single-character option with an optional legend 
#          giving the full labels....Not Done.  Have user specify margin
#          width and legend themselves using the par and legend functions.

# Also:  Add an option for drawing arrows from the x values to Z values, which
#          correspond to actual data points, thereby providing an easy
#          plot annotation mechanism.....Done.

# Also:  Add an option to fill strip spaces with a different foreground 
#          color for each different character (i.e., a colored box) 
#          instead of plotting a single character...Done.

# Also:  Create a new function to annotate plots easily with a horizontal
#          bit of text (a phrase) with optional filled rectangular box, and an
#          optional arrow to a real data point (or area) on the plot, extending
#          from the center of the text phrase.    This
#          is a different approach than for strip text, which corresponds single
#          vertical character space to a given x-value on the plot. 
#          .....Not done.  You can do some of this here, but a more flexible 
#          function where arrows can come from any part of the text would
#          be a better solution.

if (!is.numeric(Y)) stop("`Y' must be a vertical coordinate.")

Y <- Y[1]

if (rotated) {
  srt <- 90
  pos <- NULL
  adj <- c(0,0.5)
  max.chars <- max(nchar(labels))
} else {
  srt <- 0
  pos <- 3
  adj <- NULL
}

char.height <- strheight("B", cex=cex)
char.width <- strwidth("B", cex=cex)
coord <- par()$usr
X <- rep(X, length=length(labels))

if (to.edge) {
  xleft <- coord[1]
  xright <- coord[2]
} else {
  xleft <- min(X)-char.width-border.offset[1]
  xright <- max(X)+char.width+border.offset[1]
}

if (rotated)
  ytop <- Y+char.height*(max.chars)+char.height/2+border.offset[2]
else    
  ytop <- Y+3*char.height/2+border.offset[2]
  
ybottom <- Y-char.height/2-border.offset[2]

if (border & !as.colors)  
  rect(xleft=xleft, ybottom=ybottom,
       xright=xright, ytop=ytop,
       col=fill, border=border.col, lwd=border.lwd)

if (!is.null(y)) {
  y <- rep(y, length=length(X))
    if (is.null(x)) x <- X
    arrows(X, ybottom, x, y, angle=arrow.angle,
           lwd=arrow.lwd, col=arrow.col, length=arrow.length) 
}    

if (as.colors)
  rect(X-char.width/2, Y, X+char.width/2, Y+char.height, col=labels, border=NA)
else  
  text(x=X, y=Y, labels=labels, cex=cex, vfont=vfont, col=col,
       adj=adj, pos=pos, offset=0, srt=srt, ...)

}
sweights <-
function(f, w, tab, max.levels=25)
{

# Maybe, forget about names of factors since when a single one is
#  passed its names seems to be dropped.  Just match based on 
#  the factor position.  Or match only when multiple factors
#  are specified.  Yes, with only one factor matching is 
#  unnecessary, as they match implicitly.

# Stratification and Post-Stratification Weights

# We now pass the frequency table as a dataframe for convenience, since
#  a frequency tabulation can be easily converted to this...

# Function to calculate per-record stratifcation weights for an arbitrary data
#   set so that the probability of selecting a record in
#   a given subgroup (defined by factors in `f' each the same
#   length as the unspecified data set) is equal to the ratio of its 
#   existing probability and the desired probability as specified by 
#   the flattened frequency table `tab', defaulting to equal
#   probabilities for each
#   subgroup, if `tab' is not given.  Existing probabilities are set to 
#   1 (equal probability for any given record), if `w' is not specified.

#   The function returns a vector of weights equal in length to
#   the factors in `f'.

# Example Usage: One wants to simulate or sample individuals, ending up
#                with an equal number in each combination of a number
#                of factors.  Then apply the function with the default
#                frequency table.  If one wants to calculate weighted
#                statistics from a simulated or sampled data set, correcting
#                for over- or undersampled proportions of individuals in
#                various groups, then one should specify the appropriate
#                frequencies in the 

if (!is.list(f))
  stop("`f' must be a list or data frame containing factors.")
f <- as.data.frame(f)
nfactors <- NCOL(f)
nrecords <- NROW(f)

# Get the existing probabilities given by w, assigned to
#   equal probability for each record by default
if (missing(w))
  w <- rep(1/nrecords, nrecords)
else if (length(w) == nrecords)
       w <- w/sum(w)
     else 
       stop("`w' must contain pre-existing weights for each record.")

# Convert components of f to factors, if they aren't already
for (i in 1:nfactors) {
   f[[i]] <- factor(f[[i]])
   if (length(levels) > max.levels)
     stop(paste("Number of factor levels in factor",i,"exceeds user-definable limit of",max.levels))
}

# Calculate existing group probabilities
oldp <- aggregate(w, f, sum)
ngroups <- NROW(oldp)


# Calculate the new frequency cross-tabulation, if not given, and 
#   convert to desired group probabilities, removing factor
#   combinations not present in f (only for user-specified tab).
if (missing(tab)) {
  p <- as.data.frame.table(table(f))
  p[[nfactors+1]] <- 1/ngroups
} else if (!is.data.frame(tab) | NCOL(tab) != nfactors + 1 | (nfactors != 1 & !identical(names(tab)[1:nfactors], names(f))))
         stop("`tab' must be a dataframe with leading columns having names exactly corresponding to the factor names in `f', plus a column containing the frequencies for each factor combination.")
       else if (NROW(tab) != NROW(oldp) | NROW(unique(tab[1:nfactors])) != NROW(tab))
              stop("`tab' must contain unique factor combinations in its leading columns.")
	      else {
                 p <- tab
 	         for (i in 1:nfactors) 
		    p[[i]] <- factor(p[[i]], levels=levels(f[[i]]))
 	         p <- p[complete.cases(p),]
	         p[[nfactors+1]] <- p[[nfactors+1]]/sum(p[[nfactors+1]])
	      }

#names(oldp)[nfactors+1] <- "p"
#names(p)[nfactors+1] <- "p"

# Calculate the group probability multiplication factor (pmf) by
#   taking the ratio of desired group probabilities to 
#   existing ones.  This factor is multiplied by the 
#   appropriate records in each group to obtain the final probabilities
#   for each record.
# If there is no available new probability for a given group, then  
#   the  probablity is assigned zero. This can only happen
#   with a user-specified frequency table.
idx1 <- do.call("paste", oldp[1:nfactors])
idx2 <- do.call("paste", p[1:nfactors])
pwf <- p[[nfactors+1]][match(idx1,idx2)] / oldp[[nfactors+1]]
pwf[is.na(pwf)] <- 0
idx <- do.call("paste", f)
as.vector(w*pwf[match(idx, idx1)])
}

tablespecs <-
function (...)
{

# Function to create a `tablestats' object containing 
#   specifications for creating a table of statistics,
#   either a flat frequency tabulation, or 
#   a table of statistics, both calculated across a
#   number of factor combinations.

# We don't check the validity of the responseis, keys, the
#   statistical functions, or the latex digit/display specs, 
#   but we check for consistency in their  lengths.

args <- list(...)
if (length(args)==1 & is.list(args)) args <- args[[1]]

# Assign defaults
key.labels<-NULL
response.labels<-NULL
right<-TRUE
include.lowest<-TRUE
blank.labels<-TRUE
prob<-FALSE
stats<-c("length", "mean", "sd", "max", "min")
digits<-c(0,1,1,1,1)
display<-c("d","e","e","e","e")

# Expected arguments
arg.names <- c("responses","keys","response.cuts",
               "key.cuts","key.labels","response.labels",
	       "right","include.lowest",
	       "blank.labels","prob","stats","digits",
	       "display")

if (!all(arg.names[1:4] %in% names(args)))
  stop(paste("Some required argument missing:",paste(arg.names[1:4],collapse=", ")))

# Assign user-specified values
for (i in names(args)) 
   if (!i %in% arg.names)
     warning(paste("Argument not recognized:",i))
   else
     assign(i, args[[i]])


# Check arguments

if (length(responses) != length(response.cuts) | !is.list(response.cuts))
  stop("Invalid `response.cuts'.")
if (length(keys) != length(key.cuts) | !is.list(key.cuts))
  stop("Invalid `key.cuts'.")
if (!is.null(response.labels) & (!is.list(response.labels) | length(response.labels) != length(responses)))  
  stop("Invalid `response.labels'.")
if (!is.null(key.labels) & (!is.list(key.labels) | length(key.labels) != length(keys)))  
  stop("Invalid `key.labels'.")
if (length(digits) != length(stats))
  stop("`digits' should be same length as `stats'.")
if (length(display) != length(stats) | any(!display %in% letters))
  stop("`display' should be same length as `stats' and consits of letters, e.g., 'd', 'e', 'f', giving the display format.")
if (!is.logical(right))
  stop("`right' must have logical a value, i.e., either TRUE or FALSE.")
if (!is.logical(include.lowest))
  stop("`include.lowest' must have a logical value, i.e., either TRUE or FALSE.")
if (!is.logical(blank.labels))
  stop("`blank.labels' must have a logical value, i.e., either TRUE or FALSE.")
if (!is.logical(prob))
  stop("`prob' must have a logical value, i.e., either TRUE or FALSE.")

# Add latex digits/display for rowname (number) and keys (strings)
digits <- c(0, rep(0,length(keys)), as.integer(digits))
display <- c("d", rep("s",length(keys)), display)

# Return the tablespecs list
res <- list(responses=responses, keys=keys, response.cuts=response.cuts,
            key.cuts=key.cuts, key.labels=key.labels,
	    response.labels=response.labels, right=right, 
	    include.lowest=include.lowest, blank.labels=blank.labels,
	    prob=prob, stats=stats, digits=digits, display=display)
	    
attr(res,"class") <- "tablespecs"
res
}
time.factor.2 <-
function (times, interval=1, origin=min(times), integer.levels=FALSE,
           tolerance=0.00001, relax.increasing=FALSE) 
{

#  THIS IS "time.fact.2", which uses the `cut' function instead of 
#     trying to do it ourselves.  Here, we really just set up the
#     proper breaks and labels and pass it along to `cut'.  30Oct04
# ---------------------------------------------------------------
  
# UPDATE: make sure interval is greater than 0

# OK, everything (finally) seems to be working OK.  But we should really
#     just be setting up the cutpoints here and using `cut' for the
#     final factor generation.  See time.factor.2.  NK 30-Oct-04

# UPDATE:  Fixed the use of the tolerance, it was operating on the wrong
#           bin (towards negative from lower bin instead of towards positive
#                 from higher bin).  Also I set a limit on tolerance; it must
#                 be less than the interval.  [Did I ever use the
#                 tolerance feature for anything? Did it work????]
#                       --NK 30-Oct04

# UPDATE:  Add ability to relax the requirement of increasing 
#           values of times, i.e., not check whether the
#           times are increasing or not...

#  OK, like the cut.POSIXt routine, lets use the `cut' function.
#    But we are a bit more flexible than cut.POSIXt because
#    we can specify any interval and the original and a tolerance.
#     Actually, cut.POSIXt _CAN_ cut by any number of secs, mins, etc.
#     but it uses the first value as the origin and can't go below
#     1 sec intervals.  NOT DONE YET.  cut.POSIXt works well enough....


#  IDEA:  It would seem better to use the cut or the cut.POSIXt
#          functions, instead of reinventing the wheel, and faster
#          too?   At least vectorize the process, for god's sake....

#  UPDATE:  OK, now we can specify any origin we want, even if it
#           is outside of the range of times.   In this case, 
#           the integer.levels may not start or end at 1

#  UPDATE:  Added a tolerance for limits of bins that
#          farthest from origin so that times that are
#          within the tolerance will be placest in the next
#          farthest bin.  This is to help sometimes when 
#          times are fractions that don't quite make it to the
#          next highest bin but should really be there anyway.

#  Function to calculate an appropriate time series 
#    grouping factor with fixed-length groups.  The factor
#    can be used to produce a new grouped time
#    series consisting of consecutive averages, or some other statistic,
#    over a fixed time interval.  The beginning of the factor cycle (origin) is
#    at the first value of times, by default.
#    This new time series can be combined with
#    others having a consistent time format.
#    The returned factor consists of consecutive fixed-length time groups
#    designated by consecutive integers, i.e., -2,-1,1,2,3,...,
#    if integer.levels==TRUE,
#    otherwise the factor levels are equal to the left most limit of
#    the interval.

#   This function was originally written to convert a time series
#     of 2s - 15s particle and CO measurements to a time series
#     of 1 minute averages.

#   times = a numeric vector of increasing times
#   interval = the time interval within which elements of `times' will be
#              grouped in the returned factor
#   origin = the time at which grouping will begin

#  Note that times MUST be increasing,because we set up a sequence of 
#     increasing values at even intervals and compare each time to this
#     sequence, assigning them to proper "bins". 

times <- as.numeric(times)

if (!relax.increasing & any(diff(times) <=0))
  stop("`times' is typically a strictly increasing set of times; this check can be disabled with a `relax.increasing=TRUE' argument.")

if (any(is.na(times)))
  stop("`times' must not contain any missing values.")

interval <- as.numeric(interval)[1]
if (interval<=0) stop("`interval' must be a positive number.")

origin <- as.numeric(origin)[1]
n <- length(times)

if (tolerance >= interval)
  stop("The `tolerance' for placing values in the next highest bin must be less than the interval width.")

#if (origin < times[1] | origin > times[n])
#  stop("`origin' must fall within the range of `times'.")

f <- vector(length=n, mode="integer")  

lower <- c()
lidx <- c()
if (origin > min(times)) {
  lower <- seq(origin, min(times)-interval, by=-interval)
  lidx <- -1:-(length(lower)-1)
}  
upper <- c()
uidx <- c()
if (origin <= max(times)) {
  upper <- seq(origin, max(times)+2*interval, by=interval)
  uidx <- 1:(length(upper)-1)
}  
if (length(lower) & length(upper))
  lower <- lower[-1]

breaks <- c(rev(lower), upper)

if (integer.levels)
  labels <- c(rev(lidx), uidx)
else
  labels <- breaks[-length(breaks)]

times <- times + tolerance

#print(breaks)
#print(labels)

cut(times, breaks=breaks, labels=labels)

}
time.factor <-
function (times, interval=1, origin=min(times), integer.levels=FALSE,
           tolerance=0.00001, relax.increasing=FALSE) 
{

#  DEPRECATED in favor of time.factor.2....

# OK, everything (finally) seems to be working OK.  But we should really
#     just be setting up the cutpoints here and using `cut' for the
#     final factor generation.  See time.factor.2.  NK 30-Oct-04

# UPDATE:  Fixed the use of the tolerance, it was operating on the wrong
#           bin (towards negative from lower bin instead of towards positive
#                 from higher bin).  Also I set a limit on tolerance; it must
#                 be less than the interval.  [Did I ever use the
#                 tolerance feature for anything? Did it work????]
#                       --NK 30-Oct04

# UPDATE:  Add ability to relax the requirement of increasing 
#           values of times, i.e., not check whether the
#           times are increasing or not...

#  OK, like the cut.POSIXt routine, lets use the `cut' function.
#    But we are a bit more flexible than cut.POSIXt because
#    we can specify any interval and the original and a tolerance.
#     Actually, cut.POSIXt _CAN_ cut by any number of secs, mins, etc.
#     but it uses the first value as the origin and can't go below
#     1 sec intervals.  NOT DONE YET.  cut.POSIXt works well enough....


#  IDEA:  It would seem better to use the cut or the cut.POSIXt
#          functions, instead of reinventing the wheel, and faster
#          too?   At least vectorize the process, for god's sake....

#  UPDATE:  OK, now we can specify any origin we want, even if it
#           is outside of the range of times.   In this case, 
#           the integer.levels may not start or end at 1

#  UPDATE:  Added a tolerance for limits of bins that
#          farthest from origin so that times that are
#          within the tolerance will be placest in the next
#          farthest bin.  This is to help sometimes when 
#          times are fractions that don't quite make it to the
#          next highest bin but should really be there anyway.

#  Function to calculate an appropriate time series 
#    grouping factor with fixed-length groups.  The factor
#    can be used to produce a new grouped time
#    series consisting of consecutive averages, or some other statistic,
#    over a fixed time interval.  The beginning of the factor cycle (origin) is
#    at the first value of times, by default.
#    This new time series can be combined with
#    others having a consistent time format.
#    The returned factor consists of consecutive fixed-length time groups
#    designated by consecutive integers, i.e., -2,-1,1,2,3,...,
#    if integer.levels==TRUE,
#    otherwise the factor levels are equal to the left most limit of
#    the interval.

#   This function was originally written to convert a time series
#     of 2s - 15s particle and CO measurements to a time series
#     of 1 minute averages.

#   times = a numeric vector of increasing times
#   interval = the time interval within which elements of `times' will be
#              grouped in the returned factor
#   origin = the time at which grouping will begin

#  Note that times MUST be increasing,because we set up a sequence of 
#     increasing values at even intervals and compare each time to this
#     sequence, assigning them to proper "bins". 

times <- as.numeric(times)

if (!relax.increasing & any(diff(times) <=0))
  stop("`times' is typically a strictly increasing set of times; this check can be disabled with a `relax.increasing=TRUE' argument.")

if (any(is.na(times)))
  stop("`times' must not contain any missing values.")

if (tolerance >= interval)
  stop("The `tolerance' for placing values in the next highest bin must be less than the interval width.")

interval <- as.numeric(interval)[1]
origin <- as.numeric(origin)[1]
n <- length(times)

#if (origin < times[1] | origin > times[n])
#  stop("`origin' must fall within the range of `times'.")

f <- vector(length=n, mode="integer")  

lower <- c()
if (origin > min(times))
  lower <- rev(seq(origin, min(times)-interval, by=-interval))
  lidx <- -length(lower):-1
upper <- c()
if (origin <= max(times))
  upper <- seq(origin, max(times)+2*interval, by=interval)
uidx <- 1:length(upper)

print(lower)
print(upper)

times <- times + tolerance

if (length(upper) > 1)
  for (i in 1:(length(upper)-1))
     if (integer.levels)
       f[times >= upper[i] & times < upper[i+1]] <- uidx[i]
     else
       f[times >= upper[i] & times < upper[i+1]] <- upper[i]

if (length(lower) > 1)
  for (i in 1:(length(lower)-1))
     if (integer.levels)
       f[times >= lower[i] & times < lower[i+1]] <- lidx[i]
     else
       f[times >= lower[i] & times < lower[i+1]] <- lower[i]

f     
}
time.segments <- 
function(times, segment.limits, labels, format, add.integers=TRUE,
         sep="-", collapse=TRUE)
{

#  Function to take a vector of increasing times and to create a factor, equal
#     in length this vector, which identifies a set of time segments, e.g.,
#     for different events (action, locations, etc.).    You must
#     specify a set of (increasing) limits for the time segements, and,
#     optionally, labels for each time segment. 
#
#     We sort the segment limits, so they don't technically have to be in order
#     as specified as arguments.
#
#    Tries to convert times to POSIXct using strptime  if `format' is given.
#
#    Includes the lowest limit in assigning the factor level for each interval.
#
#    UPDATE: Now a fatal error is returned if segment.limits are not
#            strictly increasing.   The times do not have to be.
#
#    UPDATE:  Add collapsing by default of the segments if the same factor
#               value is given in sequential segments. 
#
#    UPDATE:  Now we, by default, add sequential integers to the time segment
#            labels so that earlier segments can be distinguished from
#            later ones.   If add.integers=FALSE, then it is possible for
#            later segments to have the same label as earlier ones and may
#            confuse plotting and analysis.   11-June-08 NK
# --------------------------------------------------------------            

if (!missing(format)) {
  times <- as.POSIXct(strptime(times, format=format))
  segment.limits <- as.POSIXct(strptime(segment.limits, format=format))
} else {
  times <- as.POSIXct(times)
  segment.limits <- as.POSIXct(segment.limits)
}
if (any(diff(segment.limits) < 0 ))
  stop("`segment.limits' must be strictly increasing.")

if (any(is.na(times)) || any(is.na(segment.limits)))
  stop("Error converting to POSIX times, check `format' string.")

if (missing(labels)) 

  labels <- 1:length(labels)

else {
   if (length(labels) != length(segment.limits) - 1)
     stop("`labels' must have length 1 element smaller than length of `segment.limits'.")

   # collapse limits and labels if specified
   if (collapse) {
     #cat("Collapse...\n")
     #print(labels)
     #print(segment.limits)
     #print(collapse(labels))
     newi <- collapse(labels)
     labels <- labels[newi]
     segment.limits <- 
          segment.limits[c(newi, length(segment.limits))]
   }

   if (add.integers)
       labels <- paste(labels, 1:length(labels), sep=sep)
}

#cat("Collapsed...\n")
#print(labels)
#print(segment.limits)
  

result <- factor(rep(NA,length(times)),levels=unique(labels))
for (i in 2:length(segment.limits))
   result[times >= segment.limits[i-1] & times < segment.limits[i]] <- 
        labels[i-1]
result
}
tkdensity<-
function(){
    # Interactive density plots. Based on TCL version by Guido Masarotto

    require(tcltk) || stop("tcltk support is absent")

    y <- NULL
    xlim <-NULL
    bw <- 1 # in case replot.maybe is called too early

    replot <- function(...) {
        if (is.null(y)) return() # too early...
        bw <<- b <- as.numeric(tclvar$bw)
        k <- tclvar$kernel
        sz <- as.numeric(tclvar$size)
        eval(substitute(plot(density(y, bw=b,
                     kernel=k),xlim=xlim)))
        points(y,rep(0,sz))
    }

    replot.maybe <- function(...)
    {
        if (as.numeric(tclvar$bw) != bw) replot()
    }

    regen <- function(...) {
        if (tclvar$dist==1) y<<-rnorm(as.numeric(tclvar$size))
        else y<<-rexp(as.numeric(tclvar$size))
        xlim <<- range(y) + c(-2,2)
        replot()
    }



    base <- tktoplevel()
    tkwm.title(base, "Density")

    spec.frm <- tkframe(base,borderwidth=2)
    left.frm <- tkframe(spec.frm)
    right.frm <- tkframe(spec.frm)

    frame1 <- tkframe(left.frm, relief="groove", borderwidth=2)
    tkpack(tklabel(frame1, text="Distribution"))
    tkpack(tkradiobutton(frame1, command=regen, text="Normal",
                         value=1, variable="dist"), anchor="w")
    tkpack(tkradiobutton(frame1, command=regen, text="Exponential",
                         value=2, variable="dist"), anchor="w")

    frame2 <- tkframe(left.frm, relief="groove", borderwidth=2)
    tkpack(tklabel(frame2, text="Kernel"))
    for ( i in c("gaussian", "epanechnikov", "rectangular",
                 "triangular", "cosine") ) {
        tmp <- tkradiobutton(frame2, command=replot,
                             text=i, value=i, variable="kernel")
        tkpack(tmp, anchor="w")
    }

    frame3 <-tkframe(right.frm, relief="groove", borderwidth=2)
    tkpack(tklabel(frame3, text="Sample size"))
    for ( i in c(50,100,200,300) ) {
        tmp <- tkradiobutton(frame3, command=regen,
                             text=i,value=i,variable="size")
        tkpack(tmp, anchor="w")

    }

    frame4 <-tkframe(right.frm, relief="groove", borderwidth=2)
    tkpack(tklabel (frame4, text="Bandwidth"))
    tkpack(tkscale(frame4, command=replot.maybe, from=0.05, to=2.00,
                   showvalue=F, variable="bw",
                   resolution=0.05, orient="horiz"))


    tkpack(frame1, frame2, fill="x")
    tkpack(frame3, frame4, fill="x")
    tkpack(left.frm, right.frm,side="left", anchor="n")

    q.but <- tkbutton(base,text="Quit",
                      command=function()tkdestroy(base))

    tkpack(spec.frm, q.but)

    tclvar$size  <- 50
    tclvar$dist  <- 1
    tclvar$kernel<- "gaussian"
    tclvar$bw    <- 1
    regen()
}



uv2compass<-
function(u, v, tol=.Machine$double.eps)
{

# Function to convert from "UV" components of wind
#  speed to "compass" degrees

#  I use this to convert the Windsonic (Gill, Inc., UK) readings in
#  UV mode to those in "Polar" mode, which actually
#  reports wind direction in compass degrees.

# Compass degrees are (wind COMING FROM a certain direction):

#   0 North
#  90 East
# 180 South
# 270 East

# UV coordinates degrees are (defines the vector that wind is BLOWING TO, 
#  i.e., the direction of air flow due to wind):

#  U = north/south direction, negative means south
#  V = east/west direction, negative means east

if (length(u) != length(v))
  stop("`u' and `v' must contain matched values for U and V components.")

#  This is the old stuff before I realized that the reported
#    "Polar" degrees give the direction that wind is COMING FROM.
#r = sqrt(u^2 + v^2)
#sign <- rep(1,length(u))
#sign[v > 0] <- -1
#plus <- rep(0, length(u))
#plus[v > 0] <- 360
#theta <- (180/pi) * sign * acos(u/r) + plus

#  These are the formulas from Gill, Inc. customer support:
#   (slightly changed for differences between Excel atan2
#     function and R's atan2 function)

#  Update:  If magnitude, r, is approximately zero within a tolerance of the
#    machine precision, by default, then the angle is given
#    a missing, NA, value, otherwise it would be given a value of 180
#    or 360, depending on the values of u and v.

r <- sqrt(u^2 + v^2)
theta <- 180 + (180/pi)*atan2(-v,u)
theta[!r > 360*tol] <- NA

data.frame(theta=theta, r=r)

}
whist<-
function (data, w = NULL, nbins = NULL, h, x0 = -h/1000, plot=TRUE, 
    axes=TRUE, breaks, prob = TRUE, xlim = range(breaks), ymax = max(est), 
    col = 1, xlab = deparse(substitute(data)), bty = "n", main = NULL,
    verbose=T,  ...) 
{

#   whist -- a weighted histogram

#   Function adapted from VR's 'truehist' function in MASS library
#   to calculate a weighted histogram.	

#   Neil Klepeis, Updated:  29 Sep 2000

#   w = vector of weights with same length as the data vector
#   If w is NULL, then the usual histogram is produced.

#   nbins is number of bins.  h is bin widths and takes precedence
#   over nbins.  breaks is actual break points and takes precendence
#   over nbins and breaks

#   Deletes all records that have missing values in either 
#   'data' or 'w'.

    eval(xlab)

    if (verbose) cat("Initial Data Points:",NROW(data),"\n")

    if (is.null(nbins)) {
       # This is the nclass.scott from VR, p. 119
       htmp <- 3.5 * sqrt(var(data)) * length(data)^(-1/3)
       nbins<-ceiling(diff(range(data))/htmp)
    }

    if (!is.vector(data) || (!is.vector(w) && !is.null(w))) 
       stop("'data' and 'w' must be vectors.")

    if (!is.null(w) && NROW(data) != NROW(w))
       stop("'data' and 'w' must be the same length") 

    if (prob)  
       ylab <- "Probability"
    else  
       ylab <- "Frequency"   

    if (is.null(main)) main <- "Unweighted Histogram"

    if (!is.null(w)) {
      if (main == "Unweighted Histogram" ) main <- "Weighted Histogram"
      data0<-NROW(data)
      # must delete NA's from data first
      data<-data[!is.na(w)]
      w<-w[!is.na(w)]
      if (data0 > NROW(data))
         warning(paste("Missing Data in Weights:",data0-NROW(data),
                   "values deleted from 'data' and 'w'."))
    }

    data0<-NROW(data)
    # must delete NA's from w first
    if (!is.null(w)) w<-w[!is.na(data)]
    data<-data[!is.na(data)]
    if (data0>NROW(data))
       warning(paste("Missing Data in Data Vector:",data0-NROW(data),
              "values deleted from 'data' and 'w'."))
    
    if (missing(breaks)) {
        if (missing(h)) 
            h <- diff(pretty(data, nbins))[1]
        first <- floor((min(data) - x0)/h)
        last <- ceiling((max(data) - x0)/h)
        breaks <- x0 + h * c(first:last)
    }

    if (any(diff(breaks) <= 0)) 
        stop("breaks must be strictly increasing")
    if (min(data) < min(breaks) || max(data) > max(breaks)) 
        stop("breaks do not cover the data")

    db <- diff(breaks)

    if (!prob && sqrt(var(db)) > mean(db)/1000) 
        warning("Uneven breaks with prob = F will give a misleading plot")

    # This is the recoded data by integers for each bin
    bin <- cut(data, breaks, include.lowest = TRUE)

    # Number of raw counts inside each bin
    counts <- tabulate(bin, length(levels(bin)))

    # Now we sum the weights for each break using the recoded data as factors
    # and calculate the weighted counts and weighted probabilities
    # Some breaks may have zero instances, then what??
    if (!is.null(w)) {
       wsum <- sapply(split( w, f = bin), sum, na.rm=T,USE.NAMES=FALSE)
       names(wsum)<-NULL
       if (length(counts) != length(wsum))
          warning("Can't calculate weighted histogram if some intervals have zero observations.")
       prob <- wsum/sum(w,na.rm=T)
       true <- wsum/(diff(breaks) * sum(w, na.rm=T))
    }
    else {
       # regular unweighted probabilities
       prob <- counts/length(data)
       # Here are the unweighted normalized probabilities
       true <- counts/(diff(breaks) * length(data))
    }

    est<-counts
    if (prob) est<-true

    n <- length(breaks)

    if (plot) {
       plot(xlim, c(0, ymax), type = "n", xlab = xlab, ylab = ylab, main=main,
           axes=axes, bty = bty)
       box()
       rect(breaks[-n], 0, breaks[-1], est, col = col, ...)
    }

    if (verbose) cat("Number of bins:",NROW(breaks))
    if (verbose) cat("\nData vector length after 'w' and 'data' NA removal: ", NROW(data),"\n\n")

    if (!plot) {
       if (is.null(w)) 
          list(breaks=breaks, prob=prob, norm.prob=true, raw.counts=counts)
       else
          list(breaks=breaks, sum.of.weights=wsum, weighted.prob=prob, weighted.norm.prob=true, raw.counts=counts)
    }
}
xyplot.overlay <- 
function (formula, z, lwd=2, lty=1, col=1:length(z), type="l",
          pch=1:length(z), cex=rep(1,length(z)), strip.style=1,
          strip.bg=trellis.par.get("strip.background")$col,
          strip.fg="black", strip.cex=1,
          panel=function(...) panel.superpose.2(...,lwd=lwd,lty=lty,
                                 col=col,pch=pch,cex=cex),
          strip=function(...,bg) strip.default(...,bg=strip.bg,
                           style=strip.style,strip.names=T),
          as.table=TRUE,...)

{

# `xyplot.overlay' : R function that creates a Trellis
#  plot that overlays a series of data frames in a
#  number of xyplot panels according to the value of 
#  a conditioning variable.

#  [A wrapper for the `xyplot' function with `xyplot.superpose'
#   for the panel function]

# `formula' : the formula that species the form of the
#             plot, i.e., the dependent, independent, and
#             conditioning variables: d ~ i | c 
#             OR:  d ~ i | c1 * c2
# `z' : a list containing the data frame objects. The
#       variables in the formula need to be included in
#       each data frame to be plotted.  The data frames
#       must have matching numbers of columns (variables).
# ... : any number of parameters to be passed to the 
#       `xyplot' Trellis function.

#  Notes:
#  -- To see the variable name/values in the strips, the conditioning
#  variables should be encoded as a factor.
#  -- The `type' option is a character vector that includes any of the normal
#  type plot options, or 'r' for linear regression, or  'smooth' for
#  a loess-smoothed line.
#  -- if `panel.superpose.2' is used as the panel function, then 
#  the plot `type' can change between overlayed plots.   Each element of
#  the character vector `type' specifies the subsequently-plotted
#  plot type.

# This function needs the `lattice' R package, which, in turn,
# requires the `grid' package.

require(lattice)

if (!is.list(z) | is.data.frame(z))
   stop("`z' must be list of one or more data frame objects.")
df <- data.frame()
g <- c() 
for (i in 1:length(z)) {
    df <- rbind(df,z[[i]])
    g <- c(g,rep(i,NROW(z[[i]])))
}
xyplot(formula=formula,data=df,type=type,as.table=as.table,
       panel=panel,strip=strip,groups=g,
       par.strip.text=list(cex=strip.cex,col=strip.fg), ...)
}
