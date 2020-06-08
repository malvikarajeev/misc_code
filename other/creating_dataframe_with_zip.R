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


#########################INDIVIDUAL
write.csv(binary, "~/Desktop/stat215/stat-215-a/lab2/data/individual.csv")
#indi <- binary

indi <- read.csv("~/Desktop/stat215/stat-215-a/lab2/data/individual.csv")
indi <- indi[,-1]
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

just_zip <- merge(by_zip, temp[,c("lat","long", "newZIP","state","city")], 
                   by.x = 'zip', by.y = 'newZIP', all.x = T)

##adding state info using data(states)
just_zip$state <- as.character(by_zip_ll$state)

data(state)
state_info <- data.frame(stringsAsFactors = F, state.abb, 
                         state.region)

just_zip <- merge(just_zip, state_info, by.x = "state", by.y = "state.abb") 


##finally, remove hawaki and alaska

just_zip <- just_zip %>% filter(!(state == 'AK' | state == 'HI'))
indi <- indi %>% filter(!(state == 'AK' | state == 'HI'))




