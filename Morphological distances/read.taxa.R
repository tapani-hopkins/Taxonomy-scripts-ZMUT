## Read a table of character traits from a file
## This is a shortcut function, it is faster to use this than 'read.table'
# file: the name of the file to open, passed to read.table
# header, sep, na.string, ...:  other optional arguments passed to read.table. (Modify with caution, not all may work as expected)
## Returns a list with three variables:
# traits:   the table of character traits, with species in columns and characters in rows
# weights:  the weights to be given to each character. NULL if the file did not contain the column "weights".
# heads:    the two columns of description headers. NULL if the file did not contain columns "head1" and "head2".

# NB! The script assumes everything to the RIGHT of the "trait" column is character traits, and everything to the left weights, headers or stuff to be discarded. If there is no column called "trait" the script will assume the whole table is character traits.

read.taxa = function(file, header=T, sep=",", na.string=c("NA","?",""), ...){
    
    # Read the file with read.table
    aa = read.table(file, header=header, sep=sep, na.string=na.string, ...)
    
    # Get the row names from column 'trait' (if no column is called 'trait' this just numbers the rows)
    row.names(aa)=aa$trait
    
    # Get the weights (results in NULL if no column is called 'weight')
    weights = aa$weight
    
    # Get the description headers if those are given (columns head1 and head2)
    if ("head1"%in%names(aa) & "head2"%in%names(aa) ){
        heads = data.frame(aa$head1, aa$head2)
    } else {
        heads = NULL
    }
    
    # Save the remaining table as character traits
    # Everything to the right of the trait column is assumed to be character traits
    if("trait"%in%names(aa)){
        traits = aa[,-(1:which(names(aa)=="trait"))]
    } else {
        traits = aa
        warning("No 'trait' column found, the entire table has been assumed to contain character traits.")
    }


    list(traits=traits, weights=weights, heads=heads)
}
