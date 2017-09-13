## Convert a table of species descriptions into text
# de:   the table of species descriptions, with character traits in rows. The table must contain columns "head1" and "head2".
# species.names:    a vector of species names, or "header" if the species names can be taken from the table header.
# sep0: the separator between Headers 0 (e.g. between diagnosis and description)
# sep1: the separator between Headers 1 (e.g. between head and mesosoma)
# sep2: the separator between Headers 2 (e.g. between frons and face)
# sep3: the separator inside Headers 2 (e.g. between different parts of the frons)
# sep.sp:   the separator between species names (i.e. comes before "Epirhyssa quagga")
# posth0:   the text to place after each Header 0 (to separate it from header 1 and subsequent text)
# posth1:   the text to place after each Header 1 (to separate it from header 2 and subsequent text)
# posth2:   the text to place after each Header 2 (to separate it from subsequent text)
# post.sp:  the separtor after species names (to separate e.g. "Epirhyssa quagga" from the subsequent text)
# end.text: the text to place at the end of each species description (typically "" or the same as sep2)
# return.list:  Set to True to return the species descriptions as a list, with each species forming its own list element. The list elements are named after the species (call "names(returned list)" to get the species names). Default is to return the entire description as one block of text.
## Returns the species descriptions as text, or a list of texts if return.list is set to True.

# NB! The script assumes everything to the RIGHT of the "trait" column (or "head2" if "trait" is missing) is character traits, i.e. stuff to be added to the species descriptions.

# NB2. The separators and posttexts accept both "\n" and "\\n" and treat them the same. The same applies for the tab character "\t". This is sometimes useful when reading separators from a file with read.csv


convert.descriptions = function(de, species.names="header", sep0=".\n\n", sep1=".\n", sep2 = ". ", sep3 = ", ", sep.sp="\n\n\n", posth0="\n\n", posth1="\n", posth2=" ", post.sp="\n\n", end.text="", return.list=F){
    
    # Convert the separators and posttexts to vectors if they were not given in vector form
    if (length(sep0)==1){ sep0 = rep(sep0, dim(de)[1]) }
    if (length(sep1)==1){ sep1 = rep(sep1, dim(de)[1]) }
    if (length(sep2)==1){ sep2 = rep(sep2, dim(de)[1]) }
    if (length(sep3)==1){ sep3 = rep(sep3, dim(de)[1]) }
    if (length(posth0)==1){ posth0 = rep(posth0, dim(de)[1]) }
    if (length(posth1)==1){ posth1 = rep(posth1, dim(de)[1]) }
    if (length(posth2)==1){ posth2 = rep(posth2, dim(de)[1]) }

    # Check the separators and postscripts are the right length
    for (i in list(sep0, sep1, sep2, sep3, posth0, posth1, posth2) ){
        if ( length(i)!= dim(de)[1] ){ stop("One of the separators (sep) or posttexts (posth) is of the wrong length. They should be a vector of length 1 or ", dim(de)[1]) }
    }
    
    # Fix an extremely frustrating and pointless problem which sometimes reads in "\n" as "\\n"
    sep0 = gsub("\\\\n", "\n", sep0)
    sep1 = gsub("\\\\n", "\n", sep1)
    sep2 = gsub("\\\\n", "\n", sep2)
    sep3 = gsub("\\\\n", "\n", sep3)
    posth0 = gsub("\\\\n", "\n", posth0)
    posth1 = gsub("\\\\n", "\n", posth1)
    posth2 = gsub("\\\\n", "\n", posth2)
    # Do the same for "\t" and "\\t"
    sep0 = gsub("\\\\t", "\t", sep0)
    sep1 = gsub("\\\\t", "\t", sep1)
    sep2 = gsub("\\\\t", "\t", sep2)
    sep3 = gsub("\\\\t", "\t", sep3)
    posth0 = gsub("\\\\t", "\t", posth0)
    posth1 = gsub("\\\\t", "\t", posth1)
    posth2 = gsub("\\\\t", "\t", posth2)
    
    # Get the species from the header if they were not provided
    if (length(species.names)==1){
        if (species.names=="header"){
            from.header = T
        }
    } else {
        species = as.character(species.names)
        from.header = F
    }
    
    # Save the three header columns (head0, head1, head2), the species names (species) if not already provided, and the species data (spp)
    # Also check that the header columns exist
    if( "head0"%in%names(de) && "head1"%in%names(de) && "head2"%in%names(de) ) {
        head0 = de$head0
        head1 = de$head1
        head2 = de$head2
        if("trait"%in%names(de)){
            # Get everything to the right of the trait column
            spp = de[,-(1:which(names(de)=="trait"))]
            if (from.header){
                # Get the species names from the header (to the right of the trait column)
                species = names(de)[-(1:which(names(de)=="trait"))]
            }
        } else {
            # Get everything to the right of the head2 column
            spp = de[,-(1:which(names(de)=="head2"))]
            if (from.header){
                # Get the species names from the header (to the right of the head2 column)
                species = names(de)[-(1:which(names(de)=="head2"))]
            }
            warning("No 'trait' column found, everything after column 'head2' has been assumed to be taxa data.")
        }
    } else {
        stop("No 'head0', 'head1' or 'head2' column found. All three columns are needed.")
    }
    
    ## Save the descriptions in text form into 'de.list'
    
    de.list = NULL
    
    for (ii in 1:length(species)){
        
        # First write the species name
        if (ii==1){
            txt = paste(species[ii], post.sp, sep="")
        } else {
            txt = paste(sep.sp, species[ii], post.sp, sep="")
        }

        # Get the data for species ii
        # Remove any blank (i.e. missing) character traits
        # sp0, head00, head10, head20:  data and headers for this species
        # sepx0, posthx0:   separators and posttexts for this species
        sp = spp[,ii]
        blank = which(sp=="")
        if (length(blank)>0){
            sp0 = sp[-blank]
            head00 = head0[-blank]
            head10 = head1[-blank]
            head20 = head2[-blank]
            sep00 = sep0[-blank]
            sep10 = sep1[-blank]
            sep20 = sep2[-blank]
            sep30 = sep3[-blank]
            posth00 = posth0[-blank]
            posth10 = posth1[-blank]
            posth20 = posth2[-blank]
        } else {
            sp0 = sp
            head00 = head0
            head10 = head1
            head20 = head2
            sep00 = sep0
            sep10 = sep1
            sep20 = sep2
            sep30 = sep3
            posth00 = posth0
            posth10 = posth1
            posth20 = posth2
        }

        # Start by writing the first character trait
        if (length(sp0)>0){
            txt0 = paste(head00[1], posth00[1], head10[1], posth10[1], head20[1], posth20[1], sp0[1], sep="")
            # Do not add head1 or head2, nor their posttexts, if they are blank (assumes that head1 is only blank if head2 is also blank)
            if (head20[1]==""){
                if (head10[1]==""){
                    txt0 = paste(head00[1], posth00[1], sp0[1], sep="")
                } else {
                    txt0 = paste(head00[1], posth00[1], head10[1], posth10[1], sp0[1], sep="")
                }
            }
        } else {
            txt0 = ""
        }
        txt = paste(txt, txt0, sep="")

        # Continue by writing the character traits one at a time, adding separators whenever a header changes
        if (length(sp0)>1){
            for ( i in 2:length(sp0) ){
                
                # Check which headers, separators etc to include
                if (head00[i]==head00[i-1]){
                    if (head10[i]==head10[i-1]){
                        if (head20[i]==head20[i-1]){
                            # no header change, simply add the sep3 separator (typically a comma)
                            txt0 = paste(sep30[i], sp0[i], sep="")
                        } else {
                            # head2 has changed, write head2 and add the appropriate separator and posttext
                            txt0 = paste(sep20[i], head20[i], posth20[i], sp0[i], sep="")
                        }
                    } else {
                        # head1 has changed, write head1 and head2, and add the appropriate separator and posttexts
                        txt0 = paste(sep10[i], head10[i], posth10[i], head20[i], posth20[i], sp0[i], sep="")
                        # Do not add head1 or head2, nor their posttexts, if they are blank (assumes that head1 is only blank if head2 is also blank)
                        if (head20[i]==""){
                            if (head10[i]==""){
                                txt0 = paste(sep10[i], sp0[i], sep="")
                            } else {
                                txt0 = paste(sep10[i], head10[i], posth10[i], sp0[i], sep="")
                            }
                        }
                    }
                } else {
                    # head0 has changed, write head0, head1 and head2, and add the appropriate separator and posttexts
                    txt0 = paste(sep00[i], head00[i], posth00[i], head10[i], posth10[i], head20[i], posth20[i], sp0[i], sep="")
                    # Do not add head1 or head2, nor their posttexts, if they are blank (assumes that head1 is only blank if head2 is also blank)
                    if (head20[i]==""){
                        if (head10[i]==""){
                            txt0 = paste(sep00[i], head00[i], posth00[i], sp0[i], sep="")
                        } else {
                            txt0 = paste(sep00[i], head00[i], posth00[i], head10[i], posth10[i], sp0[i], sep="")
                        }
                    }
                }
            
                # add this character trait (txt0) to the main text, then continue to the next trait
                txt = paste(txt, txt0, sep="")
            }
        }
        
        # add a final marker (e.g. full stop) to the end of this species
        if (txt!=""){
            txt = paste(txt, end.text, sep="")
        }
        
        # Add the species description to de.list, then continue to the next species
        de.list = c(de.list, txt)
    }
    
    # Convert the species descriptions from a vector to a list
    # Name each list element after its species
    de.list = as.list(de.list)
    names(de.list) = species
    
    # Return the species descriptions as a list or as one block of text (default)
    if (return.list){
        result = de.list
    } else {
        result = paste(de.list, sep="", collapse="")
    }
    
    result
}
