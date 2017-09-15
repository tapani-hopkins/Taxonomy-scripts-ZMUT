## Add a graphical visualisation of an identification key to a morphological distance plot
# p:    A plot drawn by plot.dist
# st:   The structure of the identification key. Typically a data frame loaded from a csv file.
# col:  The colours for the lines. Default is a gradient from red to yellow/green
# show.groups:  Whether to show the group names (shown where the lines branch)
# show.lines:   Whether to show the lines
# convert.groups:   Whether to convert group names (i.e. the names or numbers of the steps of the key) to make sure they are unambiguous. Default is "i" ("if needed") = convert if the key is ambiguous; e.g. there are two steps of the key with the same name. Also accepts True = always convert, and False = do not convert. Conversion happens by combining the steps of the key, so that e.g. steps "1, 2, 1" become "1, 1.2, 1.2.1". Usually there is no need to change the default: it ensures that both shorthand (1, 2, 1) and longhand (1, 1.2, 1.2.1) notations for key structure are accepted and give the same result.
## Returns a list with two data frames:
# groups:   the coordinates of the different groups / line branches
# line.coordinates: the coordinates of the lines (and their suggested colour)
## For finer control of the plot, set show.groups and show.lines to False, and draw the lines and groups based on the returned values


plot.key = function(p, st, col=NULL, show.groups=T, show.lines=T, convert.groups="i"){
    
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
    
    ## Save the key structure into 'stf', either in the original format or (if the key was ambiguous or convert.groups=TRUE) with the steps of the key combined.
    ## Combining allows giving shorthand notation for the steps of the key:
    ## e.g. steps "1, 2, 1" can be combined by the script into "1, 1.2, 1.2.1"
    #
    # Convert factors and NAs in the key structure to characters
    st[] = lapply(st, as.character)
    st[is.na(st)] = "NA"
    # Create a copy of the key structure with combined group names (i.e. steps of the key)
    # Each row of 'st' is a step in the key
    st.combined = st
    for (i in 2:dim(st)[1]){
        st.combined[i,] = paste(st.combined[i-1,], st[i,], sep=".")
    }
    # Choose either the combined key structure (st.combined) or the original (st) for use by the script.
    # Save the selected key structure into 'stf'.
    if ( substr(convert.groups,1,1) == "i" ){
        # Combine if the given key structure (st) is ambiguous, e.g. if different steps of the key have the same name, or the key's branches join.
        # (typically this will be because the key is in shorthand: "1, 2, 1" is ambiguous since there are two steps named "1", it should be combined into "1, 1.2, 1.2.1")
        # In practice this is done by checking, one row at a time, if combining the key makes any difference to its structure. If it does, the key is ambiguous.
        stf = st
        for (i in 1:dim(st)[1]){
            # Compare the two keys one row at a time
            c1 = st[i,]
            c2 = st.combined[i,]
            # Ignore NA values
            nas = grep("NA",c1)
            if (length(nas)>0){
                c1 = c1[-nas]
                c2 = c2[-nas]
            }
            # Check if the structure of this row is the same in both keys
            # If it is not, the key is ambiguous (since combining it creates a different key structure).
            c1 = factor(as.character(c1))
            c2 = factor(as.character(c2))
            differences = sum( as.numeric(c1) != as.numeric(c2) )
            if (differences > 0){
                # Use the combined key if the original is ambiguous
                stf = st.combined
            }
        }
    } else {
        # Combine only if asked to do so
        if (convert.groups){
            stf = st.combined
        } else {
            stf = st
        }
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
    if (length(grep("NA",names(xy)))>0){
        xy = xy[,-grep("NA",names(xy))]
    }
    
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
            if( st[r,sp] != "NA" ){
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
