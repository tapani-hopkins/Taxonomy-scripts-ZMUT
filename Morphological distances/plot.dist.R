## Plot morphological distances between species
# mds:  the metaMDS distances between species (returned e.g. by function 'metaMDS')
# lab:  True if species labels are to be shown
# ...:  other graphical parameters passed to 'plot'. type="n" creates a blank plot for finer control of the plot's appearance

plot.dist = function(mds, lab=T, ...){
    
    if ( !(class(mds)=="metaMDS" || class(mds)=="monoMDS") ){
        warning("mds is not a metaMDS or monoMDS object")
    }
	
    # Widen the margins so that texts fit inside the plot area
    p = suppressWarnings(  plot(mds, type="n")  )
    x2 = par("usr")[2]
    x1 = par("usr")[1]
    add = (x2-x1)*0.05
    
    # Plot
    arg0 = list(type="n", xlim = c(x1-add, x2+add) )
    arg1 = list(...)
    arg0[names(arg1)] = arg1
    p = suppressWarnings(  do.call(plot, c(list(mds), arg0))  )

    tp = "type" %in% names(arg1)
    if(tp){
        points(p$sites, type=list(...)$type)
    } else {
        points(p$sites)
    }
    
     if (lab){
         text(p,"sites", pos=3, cex=0.5)
    }
    
    p
}
