## Calculate a distance matrix for mixed data types
# (i.e. can handle both numeric vectors, such as wing indices, 
#  and factors such as presence/absence of a trait)

dist.mixed = function(m, weights=NULL){
	
    # Input checking: check the weights
    # Give all the characters the same weight if none were specified
    # Also do so if or there are the wrong number of weights
    if (is.null(weights)){
        weights = rep(1,dim(m)[2])
    }
    if ( length(weights) != dim(m)[2] ){
        warning("weights is of the wrong length. There should be ", dim(m)[2], " weights, not ", length(weights), ". All characters have been given the same weight.")
        weights = rep(1,dim(m)[2])
    }
    
	# Create an empty distance table
	d = data.frame( matrix(data=NA, dim(m)[1],dim(m)[1]) , row.names=row.names(m) )
	names(d) = row.names(m)
	
	# Scale numeric columns and convert non-numeric columns to factors
	m = standardise(m)
	
	# Fill in the distance table one column at a time
	# The actual calculation is done by 'compare()'
	for (dcol in 1:dim(d)[2]){
		for (drow in  1:dim(d)[1]){
			d[drow, dcol] = compare(m, dcol, drow, weights)
		}
	}
	
	as.dist(d)
}
