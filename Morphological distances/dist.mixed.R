## Calculate a distance matrix for mixed data types
# (i.e. can handle both numeric vectors, such as wing indices, 
#  and factors such as presence/absence of a trait)

dist.mixed = function(m){
	
	# Create empty distance table
	d = data.frame( matrix(data=NA, dim(m)[1],dim(m)[1]) , row.names=row.names(m) )
	names(d) = row.names(m)
	
	# Scale numeric columns and convert non-numeric columns to factors
	m = standardise(m)
	
	# Fill in the distance table one column at a time
	# The actual calculation is done by 'compare()'
	for (dcol in 1:dim(d)[2]){
		for (drow in  1:dim(d)[1]){
			d[drow, dcol] = compare(m, dcol, drow)		
		}
	}
	
	as.dist(d)
}