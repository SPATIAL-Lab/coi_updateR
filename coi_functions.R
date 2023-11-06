
#####
#Finds first and last citations in a list 

f.l = function(cv, stex, etex, eyr){
  #find start and end text in cv
  ps = grep(stex, cv) + 1
  pe.tex = grep(etex, cv) - 1
  
  #extract year for each citation
  yrs.s = regexpr("\\(", cv[ps:pe.tex], perl=TRUE)
  yrs.e = regexpr("\\)", cv[ps:pe.tex], perl=TRUE)
  yrs = substr(cv[ps:pe.tex], yrs.s+1, yrs.e-1)
  yrs = as.numeric(yrs)
  
  #find the first pub outside the 4-year window
  pe.yr = yrs <= eyr
  pe.yr = match(TRUE, pe.yr)
  
  #if there is a pub in the list beyind the window, end there, otherwise use all pubs
  if(!is.na(pe.yr)){
    pe = pe.yr - 2 + ps
  } else{
    pe = pe.tex - 1
  }
  
  return(c(ps, pe))
}

#####
#Extract authors from citation

getpeeps = function(pub, peeps){
  #extract author names 
  ind.s = regexpr("\\.", pub) + 1
  ind.e = regexpr("\\(", pub) - 2
  pub = substr(pub, ind.s, ind.e)
  
  #get rid of asterix and hash
  pub = gsub("\\*", "", pub)
  pub = gsub("\\#", "", pub)
  
  #swap out final author seperator
  pub = sub(" and ", ", ", pub)
  
  #split out authors
  peep = strsplit(pub, ", ")
  
  #add em to the tally
  peeps = c(peeps, peep[[1]])
  
  return(peeps)
}

