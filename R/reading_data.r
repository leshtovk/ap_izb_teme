library(readr)

M <- read_csv("imena-moski.csv", col_names=c("ime", 2008:2019),
              skip=3, na="-", locale=locale(encoding="Windows-1250"))

Z <- read_csv("imena-zenske.csv", col_names=c("ime", 2008:2019),
              skip=3, na="-", locale=locale(encoding="Windows-1250"))

# get a vector which has the sums of all the names of each year
x <- apply(M[-1], 2, sum, na.rm=TRUE)

library(tidyr) # as in 'tidy R'

# make new dataframes that will be 'tidy'
# read up in detail what the `gather` function does 
# notice what happens if you pass `stevilo` as a key and `leto` as a value

imena.moski <- gather(M, -ime, key=leto, value=stevilo, na.rm=TRUE)
imena.zenske <- gather(Z, -ime, key=leto, value=stevilo, na.rm=TRUE)

# the column `leto` contains numbers -- as strings 
# use `str(imena.moski)` to see the types stored in every column 
# we can converte these to integers

imena.moski$leto <- parse_integer(imena.moski$leto)
imena.zenske$leto <- parse_integer(imena.zenske$leto)

# we add another column to the tables that will contain the gender of every name 
# it is redundant in the separate column, but will be useful when we combine the data 
# into one dataframe -- it categorizes the names in two groups and also 
# lets us see if there are any names that are given to both men and women 

# we declare that something is a qualitative variable with `factor`
imena.moski$spol <- factor("moski", levels = c("moski", "zenske"))
imena.zenske$spol <- factor("zenske", levels = c("moski", "zenske"))

# combine the two datafiles 
imena <- rbind(imena.moski, imena.zenske)

##########################################################################################

# working with 'tidy' data

# it's best to use functions from the `dplyr` library
library(dplyr) # as in 'data plier' 

# compare: 
bojan.dplyr <- filter(imena, ime == 'Bojan')
bojan.ord <- imena[imena$ime == 'Bojan', ]

# compare: 
redke.zenske.dplyr <- filter(imena, leto == 2013, spol == "zenske", stevilo <= 5) %>% select(ime, stevilo)
redke.zenske.ord <- imena[imena$leto == 2013 & imena$spol == "zenske" & imena$stevilo <= 5, c("ime", "stevilo")]

# get a dataframe of the total male and feamle residents of each year 

residents <- imena %>% group_by(leto, spol) %>% summarize(prebivalstvo = sum(stevilo))

# a function that tells you how many years there were more men than women
compare.residents <- function(dat) {
  
  d <- dim(dat)
  men.numbers <- dat$prebivalstvo[seq(1, d[1], by = 2)]
  wamen.numbers <- dat$prebivalstvo[seq(2, d[1], by = 2)]
  bools <- men.numbers > wamen.numbers
  
  return(sum(seq(1, 1, length = d[1]/2)[bools]))
}