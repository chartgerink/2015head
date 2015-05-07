setwd("C:/Users/Chris/Dropbox/projects/2015head")
source("chjh functions.r")

pdat <- read.csv("FILES_FOR_DRYAD/1. TEXT_MINING/raw_data/p.values.csv", row.names=1)
journal.categories <- read.csv("FILES_FOR_DRYAD/1. TEXT_MINING/raw_data/journal.categories.csv", row.names=1)

# journal.categories$journal.name <- journal.categories$Abbreviation
# pdat <- merge(pdat, journal.categories,by="journal.name")

###################################
# Start selection 
###################################
# Head et al selection 1
# Get rid of papers that did not yield any p values
pdatHEAD <- pdat[!is.na(pdat$p.value), ]

# Head et al selection 2
# Removed due to arguments presented in paper

# Head et al selection 3
# "only keep papers for which we have >0 results sections (e.g. reviews and commentaries often have 0 results sections)"
pdatHEAD <- pdatHEAD[which(pdatHEAD$num.results.sections > 0),]

# Head et al selection 4
# "some papers have (legitimately, I've checked) zero authors. Remove these."
pdatHEAD <- pdatHEAD[which(pdatHEAD$num.authors>0),]

# Head et al selection 5
# some journals publish large 'supplements' that contain conference many short conference abstracts
# (with results sections) in one file. Remove these
pdatHEAD <- pdatHEAD[-c(grep("(Suppl)", pdatHEAD$file.name)),]


# we have a few records in this dataset in which the journal.name wasn't extracted properly, we fix that here
pdatHEAD$journal.name <- as.character(pdatHEAD$journal.name)
fixed.names <- as.character(pdatHEAD$folder.name[which(is.na(pdatHEAD$journal.name))])
fixed.names <- gsub("_", " ", fixed.names)
pdatHEAD$journal.name[which(is.na(pdatHEAD$journal.name))] <- fixed.names
pdatHEAD$journal.name <- as.factor(pdatHEAD$journal.name)

# now we add in the FoR categories 
journal.categories$journal.name <- journal.categories$Abbreviation
pdatHEAD <- merge(pdatHEAD, journal.categories,by="journal.name")

# Head et al selection 6
# Adjusted to <=, from <
pdatHEAD <- pdatHEAD[pdatHEAD$p.value <= 0.05, ]

# Head et al selection 7
pdatHEAD <- pdatHEAD[pdatHEAD$operator == "=", ]
###################################
# End selection 
###################################

pdatHEAD.results <- pdatHEAD[pdatHEAD$section == "results", ]
pdatHEAD.abstracts <- pdatHEAD[pdatHEAD$section == "abstract", ]

# Get rid of 'pdatHEAD' to save memory
rm(pdatHEAD)

###################################
# Rerunning alternative selection analyses
# Only adjustment from original code is the renaming of objects
# and adjusted the functions to include p <= .05
# Code is thus almost literally from Head et al.
###################################
# Remove excess factor levels from the datasets 
# (there are plenty of empty ones, since we deleted some rows, 
# and the empty levels would otherwise cause trouble later on)
pdatHEADresults <- trim.levels(pdatHEAD.results)
pdatHEADabstracts <- trim.levels(pdatHEAD.abstracts)

##### Analysis on the entire dataset (i.e. not split by FoR category)
reps <- 1000
results.bias.test <- bootstrap.binomial.bias.test(pdatHEADresults, reps)
abstract.bias.test <- bootstrap.binomial.bias.test(pdatHEADabstracts, reps)

write.csv(results.bias.test, file="results/chjh.results.combinedata.csv")
write.csv(abstract.bias.test, file="results/chjh.results.combinedata.abstracts.csv")


###### Analyses split by FoR category
results.FoR.test <- bootstrap.FoR.test(pdatHEADresults, reps)
abstract.FoR.test <- bootstrap.FoR.test(pdatHEADabstracts, reps)

write.csv(results.FoR.test, file="results/chjh.results.by.category.csv")
write.csv(abstract.FoR.test, file="results/chjh.results.by.category.abstracts.csv")

###################################
# End alternative selection analyses
###################################

###################################
# Start CHJH analyses
###################################
par(mfrow = c(2,2))
print(hist(pdatHEAD.results$p.value[pdatHEAD.results$p.value >= .04 & pdatHEAD.results$p.value <= .05 & pdatHEAD.results$operator == "="], breaks = 2))
print(hist(pdatHEAD.results$p.value[pdatHEAD.results$p.value > .04 & pdatHEAD.results$p.value < .05 & pdatHEAD.results$operator == "="], breaks = 2))
print(hist(pdatHEAD$p.value[pdatHEAD$p.value >= .04 & pdatHEAD$p.value <= .05 & pdatHEAD$operator == "="], breaks = 2))
print(hist(pdatHEAD$p.value[pdatHEAD$p.value > .04 & pdatHEAD$p.value < .05 & pdatHEAD$operator == "="], breaks = 2))












# Binwidth .025
# Ours
x <- table(cut(pdat$p.value[psel & pdat$p.value <= .05], breaks = 2))
bin1 <- x[2]
bin2 <- x[1]
binom.test(bin1, (bin1+bin2), p=.5, alternative = "greater")
# Head
x <- table(cut(pdatHEAD$p.value, breaks = 2))
bin1 <- x[2]
bin2 <- x[1]
binom.test(bin1, (bin1+bin2), p=.5, alternative = "greater")

# Binwidth .00125

# Second decimal place
png('two and three decimals.png', width = 1200, height = 1200)
par(mfrow = c(2,2))
# Our
bin1 <- table(cut(pdat$p.value[pdat$p.value >= .01 & pdat$p.value <= .05 & psel], breaks=seq(.01,.05,.00125)))[32-0]
bin2 <- table(cut(pdat$p.value[pdat$p.value >= .01 & pdat$p.value <= .05 & psel], breaks=seq(.01,.05,.00125)))[32-8]
binom.test(bin1, (bin1+bin2), p=.5, alternative = "greater")

hist(pdat$p.value[psel], breaks = 800, xlim = c(0, .05),
     main = "(A) Exactly reported p-values",
     xlab = "P-value (binwidth = .00125)")

# Head
bin1 <- table(cut(pdatHEAD$p.value[pdatHEAD$p.value >= .01 & pdatHEAD$p.value <= .05], breaks=seq(.01,.05,.00125)))[32-0]
bin2 <- table(cut(pdatHEAD$p.value[pdatHEAD$p.value >= .01 & pdatHEAD$p.value <= .05], breaks=seq(.01,.05,.00125)))[32-8]
binom.test(bin1, (bin1+bin2), p=.5, alternative = "greater")

hist(pdatHEAD$p.value, breaks = 40, xlim = c(0, .05),
     main = "(B) Exactly reported p-values (Head et al.)",
     xlab = "P-value (binwidth = .00125)")

# Only 3 decimal places
# Function retrieved from https://stat.ethz.ch/pipermail/r-help/2012-July/317676.html
decimalnumcount<-function(x){
  stopifnot(class(x) == "character")
  x<-gsub("(.*)(\\.)|([0]*$)", "", x)
  
  nchar(x)
}
pdatsel <- decimalnumcount(as.character(pdat$p.value)) == 3 & psel
HEADsel <- decimalnumcount(as.character(pdatHEAD$p.value)) == 3

# Our
bin1 <- table(cut(pdat$p.value[pdat$p.value >= .01 & pdat$p.value <= .05 & pdatsel], breaks=seq(.01,.05,.00125)))[32-0]
bin2 <- table(cut(pdat$p.value[pdat$p.value >= .01 & pdat$p.value <= .05 & pdatsel], breaks=seq(.01,.05,.00125)))[32-8]
binom.test(bin1, (bin1+bin2), p=.5, alternative = "greater")

hist(pdat$p.value[pdatsel],
     breaks = 800, xlim = c(0, .05), main = "(C) Third decimal place reporting",
     xlab = "P-value (binwidth = .00125)", ylab = "Frequency")

# Head
bin1 <- table(cut(pdatHEAD$p.value[pdatHEAD$p.value >= .01 & pdatHEAD$p.value <= .05 & HEADsel], breaks=seq(.01,.05,.00125)))[32-0]
bin2 <- table(cut(pdatHEAD$p.value[pdatHEAD$p.value >= .01 & pdatHEAD$p.value <= .05 & HEADsel], breaks=seq(.01,.05,.00125)))[32-8]
binom.test(bin1, (bin1+bin2), p=.5, alternative = "greater")

hist(pdatHEAD$p.value[HEADsel],
     breaks = 40, xlim = c(0, .05), main = "(D) Third decimal place reporting (Head et al.)",
     xlab = "P-value (binwidth = .00125)", ylab = "Frequency")
dev.off()


# Exact per category
pdf('categories all exact.pdf', onefile = TRUE, width = 11, height = 9.2)
for(cat in sort(unique(pdat$Category))){
  catsel <- pdat$Category == cat
  par(mfrow = c(2,1))
  
  # All EXACTLY reported values
  psel <- pdat$operator == "="
  
  bin1 <- table(cut(pdat$p.value[pdat$p.value >= .01 & pdat$p.value <= .05 & psel & catsel], breaks=seq(.01,.05,.00125)))[32-0]
  bin2 <- table(cut(pdat$p.value[pdat$p.value >= .01 & pdat$p.value <= .05 & psel & catsel], breaks=seq(.01,.05,.00125)))[32-8]
  x <- binom.test(bin1, (bin1+bin2), p=.5, alternative = "greater")
  
  hist(pdat$p.value[psel & catsel], breaks = 800, xlim = c(0.01, .05),
       main = sprintf("Exactly reported p-values, %s", cat),
       xlab = sprintf("P-value, %s", x$p.value))
  
  bin1 <- table(cut(pdatHEAD$p.value[pdatHEAD$p.value >= .01 & pdatHEAD$p.value <= .05 & catsel], breaks=seq(.01,.05,.00125)))[32-0]
  bin2 <- table(cut(pdatHEAD$p.value[pdatHEAD$p.value >= .01 & pdatHEAD$p.value <= .05 & catsel], breaks=seq(.01,.05,.00125)))[32-8]
  x <- binom.test(bin1, (bin1+bin2), p=.5, alternative = "greater")
  
  hist(pdatHEAD$p.value[catsel], breaks = 40, xlim = c(0, .05),
       main = "(B) Exactly reported p-values (Head et al.)",
       xlab = sprintf("P-value (binwidth = .00125), %s", x$p.value))
  
  # 3 decimal places
  hist(pdat$p.value[pdatsel & catsel],
       breaks = 800, xlim = c(0, .05), 
       main = sprintf("Third decimal place reporting, %s", cat),
       xlab = "P-value (binwidth = .00125)", ylab = "Frequency")
  
  hist(pdatHEAD$p.value[HEADsel & catsel],
       breaks = 40, xlim = c(0, .05), 
       main = sprintf("Third decimal place reporting (Head et al.), %s", cat),
       xlab = "P-value (binwidth = .00125)", ylab = "Frequency")
  
}
dev.off()


