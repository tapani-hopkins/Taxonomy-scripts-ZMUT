## Add a graphical visualisation of an identification key to a morphological distance plot
# p:    A plot drawn by plot.dist
# st:   The structure of the identification key. Typically a data frame loaded from a csv file.
# col:  The colours for the lines. Default is a gradient from red to yellow/green
# show.groups:  Whether to show the group names (shown where the lines branch)
# show.lines:   Whether to show the lines
## Returns a list with two data frames:
# groups:   the coordinates of the different groups / line branches
# line.coordinates: the coordinates of the lines (and their suggested colour)
## For finer control of the plot, set show.groups and show.lines to False, and draw the lines and groups based on the returned values


plot.key = function(p, st, col=NULL, show.groups=T, show.lines=T){
    
    # Check the colours of the lines. If none were given, use default colours.
    # The default is a gradient: first steps of the key are red, last steps green/yellow.
    if (is.null(col)){
        col = heat.colors(dim(st)[1]-1)
    }
    if ( length(col) != (dim(st)[1]-1) ){
        warning("There should be ", dim(st)[1]-1, " colours instead of ", length(col), ".")
    }
    
    # Get the coordinates of the different plotted species
    x = p$sites[,1]
    y = p$sites[,2]
    
    ## Save the key structure in 'stf'
    # Combine the steps in 'st' to make sure each row has unique grouping identifiers
    # i.e. "1.1.2.1" instead of just 1, 1, 2, 1
    # (the above is unclear, but opening 'st' and 'stf' will clarify what I'm trying to say)
    stf = st
    for (i in 2:dim(st)[1]){
        stf[i,] = paste(stf[i-1,], st[i,], sep=".")
    }
    
    ## Get the average coordinates of each group
    # xy:   data frame of the group coordinates (column names give the group, e.g. "1.2.1")
    xy = list()
    for (i in 1:dim(st)[1]){
        g = levels(factor(as.character(stf[i,])))   # groups present in row 'i' of 'stf'
        mx = NULL   # x coordinates of the groups
        my = NULL   # y coordinates of the groups
        for (gi in g){
            mx = c( mx, mean(x[stf[i,]==gi]) )
            my = c(my, mean(y[stf[i,]==gi]) )
        }
        # Combine g, mx, and my into a data frame
        xy0 = rbind(mx, my)
        xy0 = data.frame(xy0)
        names(xy0) = g
        xy = c(xy, xy0)
    }
    # Give (corrected) names to the groups and the coordinates
    # Remove NA groups (these are duplicates that appear if the key is shorter for some species than others)
    XY = data.frame(xy)
    names(XY) = names(xy)
    row.names(XY) = c("x","y")
    xy = XY
    xy = xy[,-grep("NA",names(xy))]
    
    ## Draw the lines, group names etc.
    # Draw separately for each species, starting from the first step
    # (this means many lines are drawn several times..)
    # sp:   the species currently being processed
    # r:    the row of the key currently being processed
    xy.lines = NULL
    cols = NULL
    for (sp in 1:dim(st)[2]){
        for (r in 2:dim(st)[1]){
            xy0 = xy[, names(xy)==stf[r-1,sp] ] # starting point of this line
            xy1 = xy[, names(xy)==stf[r,sp] ]   # end point of this line
            # Get the line start and end points, and the line colours
            # Ignore NA values, e.g. only do something if the line continues = haven't reached an end point of the key
            if( !is.na(st[r,sp]) ){
                xy.lines = rbind(xy.lines, c(xy0[1], xy1[1], xy0[2], xy1[2]))
                cols = c(cols, col[r-1])
            }
            
        }
    }
    # Save the line start and end points and colours in one data frame
    xy.lines = data.frame(xy.lines)
    xy.lines = cbind(xy.lines, cols)
    xy.lines[,5] = as.character(xy.lines[,5])
    names(xy.lines) = c("x.start", "x.end", "y.start", "y.end", "colour")
    
    # Save the group coordinates with groups in rows
    XY = t(xy)
    
    # Show the group names, and the starting point of the key
    if (show.groups==T){
        text(XY, labels=row.names(XY), cex=0.5, col="gray60", pos=4)
        points(xy[1,1], xy[2,1], col=col[1], pch=20)
    }
    # Draw the lines
    if(show.lines){
        for (i in 1:dim(xy.lines)[1]){
            lines( c(xy.lines[i,1],xy.lines[i,2]), c(xy.lines[i,3],xy.lines[i,4]), col = xy.lines[i,5])
        }
    }
    
    
    list(group=XY, line.coordinates=xy.lines)
}
