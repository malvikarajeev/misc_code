library(maps)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)

setwd("~/Desktop/stat215/STAT-215A-Fall-2019/lab2/")
# load the data
ling_data <- read.table('data/lingData.txt', header = T)
ling_location <- read.table('data/lingLocation.txt', header = T)
# question_data contains three objects: quest.mat, quest.use, all.ans
load("data/question_data.RData")
answers <- all.ans[50:122]


##what if i clean the ling_data?
library(zipcode)
data("zipcode")
###changing ZIPs to add a zero
zip <- ling_data$ZIP
zip <- as.character(zip)
for(i in 1:length(zip)) {
  if(as.numeric(zip[i]) < 10000){
    zip[i] <- paste0("0", zip[i])
  }
}

ling_data$ZIP <- zip
t2 <- merge(ling_data,zipcode, by.x = 'ZIP', by.y = 'zip')

t2 <- t2[, -c(2:4, 72, 73)]

names(t2)[69:72] <- c("CITY", "STATE", "lat", "long")

ling_data <- t2

##putclean
na <- function(col){
  return(sum(is.na(col)))
}


##putclean
h <- function(x) {
  head(x)
}

##ling data is the main dataset. each row represents an inviduals answer to question 51-122.
##. A value of 0 indicates no response.


##i want to make a dataset that has: question number, and number of choices.
l <- length(all.ans)

structure <- matrix(numeric(l*2), l,2)
for (i in 1:l){
  
temp <- all.ans[[i]]
  structure[i,1] <- temp$qnum[1]
  structure[i,2] <- length(temp$ans.let)
}
  
  
structure <- as.data.frame(structure)
names(structure) <- c('ques.num', 'number_choices')



##how to covert this shit into binary?
##consider one person at a time, then you can modularise it.


#so one person has 73 responses.
#for every response, structure$number_choices tells us which response it is. 

##missing: 112, 113, 114, 116, 122


struc <- structure[50:122,]
struc <- struc[-c(63:65,67,73),]
struc <- struc[-59,]
#total_results <- struc %>% summarise(s = sum(number_choices)) %>% pull(s)




##########################################################
##dont need this, already have binary.csv

dummy <- ling_data[,-c(1, 69:72)]

names(dummy) <- struc$ques.num
answer_no <- struc$number_choices
N <- nrow(ling_data)


l <- as.data.frame(matrix(numeric(N), N))

for (i in 1:length(names(dummy))){
df <- dummy[i]
names(df) <- "answers"
df$recode <- list(rep(0, answer_no[i]))
df$recode <- Map(function(x,y) `[<-`(x,y,1), x = df$recode, y = df$answers)

temp <- data.frame(matrix(unlist(df$recode), nrow=length(df$recode), byrow=T))
l <- c(l, temp)
}
l <- data.frame(l)

l <- l[,-1]

write.csv(l, "~/Desktop/stat215/stat-215-a/lab2/data/binary.csv")

  
##yay!!!!!!!!!!!
##########################################################

binary <- read.csv("~/Desktop/stat215/stat-215-a/lab2/data/binary.csv")
binary <- binary[,-1] ##serial numbers



########################################################
##fix the column  names using structure
##creating a column on vector names

create_names <- function(x) {
  return(lapply(x$number_choices, function(x) {seq(1:x)}))
  }

names_col <- create_names(struc)
names_ans <- unlist(sapply(1:67, function(x) {paste(struc$ques.num[[x]], names_col[[x]], sep = "_")}))

names(binary) <- names_ans

######################################################


binary$lat <- ling_data$lat
binary$long <- ling_data$long
binary$id <- ling_data$ID
binary$city <- ling_data$CITY
binary$state <- ling_data$STATE

##keeping first three zips of dataframe

binary$zip <- substr(as.character(ling_data$ZIP),1,nchar(ling_data$ZIP) - 2)

##clear out indivudals who didnt answer all the questions

hmm <- binary[rowSums(binary[,1:468]) == 67,]
binary <- hmm

###########################BY ZIP
##group by . have to remove: state, zip, city, lat, long

na(ling_data$ZIP)
##NO NA's. 



temp <- binary[, -(469:472)]

by_zip <- temp %>% group_by(zip) %>% summarise_all(sum)

##to add columns for lat,long, stat etc, we group ling_data by zip, 
#and then report the MODE of each of the required columns
#group by city, get the first state, most frequent occuring city, and most frequent occuring lat and log
#to make it easier will moduralise it:##na.last =NA removes NAs

##putclean
get_mode <- function(x) {
  #return(names(sort(table(x, use.NA = 'always'), decreasing = T, na.last = T)[1]))
  #return(which.max(tabulate(x)))
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

  

ling_data$newZIP <- substr(as.character(ling_data$ZIP), 1, nchar(ling_data$ZIP) - 2)

temp <- ling_data %>% group_by(newZIP) %>% summarise(state = get_mode(STATE), city = get_mode(CITY),
                                               lat = get_mode(as.numeric(lat)), long = get_mode(as.numeric(long)))


##by_zip_ll has all the added columns

by_zip_ll <- merge(by_zip, temp[,c("lat","long", "newZIP","state","city")], 
                   by.x = 'zip', by.y = 'newZIP', all.x = T)

##adding state info using data(states)
by_zip_ll$state <- as.character(by_zip_ll$state)

data(state)
state_info <- data.frame(stringsAsFactors = F, state.abb, 
                         state.region)

by_zip_ll <- merge(by_zip_ll, state_info, by.x = "state", by.y = "state.abb") 







##finally, remove hawaki and alaska

by_zip_ll <- by_zip_ll %>% filter(!(state == 'AK' | state == 'HI'))
##write.csv(by_zip_ll, "~/Desktop/stat215/stat-215-a/lab2/data/just_zip.csv")


##let's change frequenceies to relative frequencies for PCA and KMEANS, DBSCAN ETC.

temp <- by_zip_ll[,-c(1,2, 471:747)]



temp <- t(apply(temp, 1, function(i) i/sum(i))) ##transpose because R populates by column

##sanity check
rowSums(temp)

by_zip_ll[,-c(1,2, 471:747)] <- temp

write.csv(by_zip_ll, "~/Desktop/stat215/stat-215-a/lab2/data/by_zip.csv")


by_zip_ll <- read.csv("~/Desktop/stat215/stat-215-a/lab2/data/by_zip.csv")
by_zip_ll <- by_zip_ll[,-1]
##getting better city names?

##if any coumn had constant variance, its not telling us muhc, so we would delete it.

temp <- by_zip_ll[,-c(1,2, 471:747)]











