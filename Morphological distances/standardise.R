## Scale numeric columns to vary between 0-1
# Convert all other columns to factors
standardise = function(m){
	# Go through each column
	for (i in 1:dim(m)[2]){
		# Only scale numeric columns
		if (is.numeric(m[,i])){
			# Scale to 0-1
			m[,i] = m[,i]-min(m[,i], na.rm=T)
			m[,i] = m[,i] / max(m[,i], na.rm=T)
		} else {
			m[,i] = factor(m[,i])
		}
	}
	m
}