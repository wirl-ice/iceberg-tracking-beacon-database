#------------------------------------------------------------------------------
# Title: Quality added beacon files
#
# Created by: Derek Mueller, Carleton University
#
# Date: 2011
#
# Modified by: 
#   Anna Crawford, March 7, 2014
#   Adam Garbo, June 17, 2019
#
# Project: Compilation and standardization of iceberg tracking beacon data
#
# Description: 
#   - Function to convert standardized beacon CSV to polarplot
#   - Called on by BeaconProcessing.R
#
# Required file(s):
#   - Standardized beacon CSV file 
#------------------------------------------------------------------------------

polar_plot <- function (r, 
                       theta, 
                       theta.zero = 0, 
                       theta.clw = FALSE, 
                       method = 1,
                       rlabel.axis = 0, 
                       dir = 8, 
                       rlimits = NULL, 
                       grid.circle.pos = NULL,
                       grid.lwd = 1, 
                       grid.col = "black", 
                       points.pch = 20, 
                       points.cex = 1,
                       lp.col = "black",
                       lines.lwd = 1,
                       lines.lty = 1,
                       polygon.col = NA,
                       polygon.bottom = TRUE,
                       overlay = NULL,
                       pi2.lab = TRUE, 
                       text.lab = NULL,
                       num.lab = NULL,
                       rlabel.method = 1,
                       rlabel.pos = 3,
                       rlabel.cex = 1,
                       rlabel.col = "black",
                       tlabel.offset = 0.1,
                       tlabel.cex = 1.5,
                       tlabel.col = "black",
                       main = NULL,
                       sub = NULL) {
    # Debug
    message("Executing script: polar_plot.R")
      
    # r:   (vector of) radial data.
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
    # lp.col: color of points (method 1) or lines (method 2 and method 3). In method 3, set lp.col=0 for polygons without border.
    # lines.lwd: line width for plotting methods 2 and 3 (default = 1).
    # lines.lty: line type (default = 1).
    # polygon.col: color of polygon (defalut = NA).
    # polygon.bottom: polygon to the back i.e. behind the grid (default = TRUE).
    #
    # overlay: NULL (default), no overlay
    #          1, overlay data on existing plot
    #          2, overlay data, grid and labels on existing plot.
    #
    # pi2.lab:  angular labels in radians (0, pi/2, pi, 3*pi/2) (default).
    # text.lab: angular axis labels from a character vector c("N","E","S","W") (default = NULL).
    # num.lab:  numeric angular axis labels in interval [0;num.lab[ (default = NULL). Number of labels: dir.
    #
    # rlabel.method (plotting of radial axis labels):
    #    0: no radial labels.
    #    1: labels at pretty radial distances (default).
    #    2: exclude label at radial distace 0.
    #    3: exclude label at maximum radial distance.
    #    4: exclude radial labels at distance 0 and at maximum radial distance.
    # rlabel.pos: text position of radial axis labels (NULL,1,2,3,4).
    # rlabel.cex: cex for radial axis labels.
    # rlabel.col: color of the radial labels.
    #
    # tlabel.offset: radial offset for angular axis labels in fraction of maximum radial value (default = 0.1).
    # tlabel.cex: cex for angular axis labels.
    # tlabel.col: angular labels color.
    #
    # main: plot main title.
    # sub:  plot sub title.

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