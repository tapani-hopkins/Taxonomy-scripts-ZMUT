# Calculate the distance between two rows of a data frame
# Used by dist.mixed, no need to call separately
compare = function(m, row1, row2, weights=NULL){
	
	# Save the distances in d0 (same length as the rows)
	d0 = rep(NA, dim(m)[2])
    
    # Give all the characters the same weight if no weights were specified
    if (is.null(weights)){
        weights = rep(1,length(d0))
    }

	# Calculate the distance between each pair of items
	# Non-numeric items are treated differently to numeric items
	for (i in 1:dim(m)[2]){
		if (is.factor(m[row1,i])){
			# Factors: distance is 0 if the two values are the same, else 1
			d0[i] = 0 + (m[row1,i]!=m[row2,i])	
		} else {
			# Numeric values: distance is the absolute distance between the values
			d0[i] = abs(m[row1,i]-m[row2,i]) 	
		}
	}

	# Ignore NA values
    nas = which(is.na(d0))
    weights1 = weights
    d1 = d0
    if(length(nas)>0){
        d1 = d0[-nas]
        weights1 = weights[-nas]
    }
    
    # Scale so that the distances are between 0 and 1
	d1 = d1 / sum(weights1)
	
	## Calculate the appropriate distance
	# Currently the Manhattan distance
	d.result = sum(d1*weights1)
	
	d.result
}
