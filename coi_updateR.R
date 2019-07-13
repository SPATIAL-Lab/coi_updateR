setwd("C:/Users/gjbowen/Dropbox/Hypomirror/CV/coi_update.R")
library(textreadr)
library(openxlsx)
source("coi_functions.R")

#open CV
cv = read_docx("Bowen_CV_full.docx")

#beginning of 4-year window
ey = as.numeric(format(Sys.Date(), "%Y")) - 5

#get list of papers from CV
pfl = f.l(cv, "Journal Publications", "Peer-Reviewed Books", ey)
pubs = cv[pfl[1]:pfl[2]]

#append list of chapters
pfl = f.l(cv, "Peer-Reviewed Books", "Non-Reviewed Papers", ey)
pubs = c(pubs, cv[pfl[1]:pfl[2]])

#append list of non-reviewed contributions
pfl = f.l(cv, "Non-Reviewed Papers", "Invited Presentations", ey)
pubs = c(pubs, cv[pfl[1]:pfl[2]])

#this will hold names
peeps = NULL

#extract all authors
for(i in 1:length(pubs)){
  #store pub
  pub = pubs[i]
  peeps = getpeeps(pub, peeps)
}

#get rid of replicates and alphabetize, and remove myself
peeps = unique(peeps)
peeps = sort(peeps)
peeps = peeps[peeps != "Bowen G. J."]

#nsf format has commas, my cv doesnt, so pull names and add commas
#this is prone to error but tries to identify the
#first initial based on a pattern match 
ss = regexpr(" .\\.", peeps)
ss = substr(peeps, ss, ss+2)
ss = paste0(",", ss)
nm = NULL
for(i in 1:length(peeps)){
  nm[i] = sub(" .\\.", ss[i], peeps[i])
}

#read in list of affiliations from current COI doc
cois = read.xlsx("COI.xlsx")

#pull names from the existing COI file
cois.nms = cois$`Name.of.the.individual.in.conflict.(Last,.First)`

#find matches
pcm = match(nm, cois.nms)
afilis = cois$Institional.affiliation.of.the.conflict[pcm]

#wrap it up and save it
coi.out = data.frame(name = nm, affiliation = afilis)
write.csv(coi.out, "coi_update.csv")

