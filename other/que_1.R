##what do u call a carbonated beverage

q105 <- by_zip_ll[, c("city","105_1","105_2","105_3","105_4",
                      "105_5","105_6","105_7","105_8","105_9","105_10","lat","long","state.region","zip","state")]


myLetters <- letters[1:26]
names(q105)[2:11] <- all.ans$`105`$ans %>% as.character()
ans_105 <- data.frame(option = all.ans$`105`$ans %>% as.character(), no. = match(all.ans$`105`$ans.let, myLetters))




a105 <- merge(ling_data, ans_105, by.x = "Q105", by.y = "no.")
##lets remove other.
a105 <- a105 %>% filter(option != 'other')
a105 <- a105[,-c(2:68)]

##What do you call the insect that flies around in the summer and has a rear section that glows in the dark?

q65 <- by_zip_ll[, c("city","65_1","65_2","65_3","65_4",
                             "65_5","65_6","lat","long","state.region","zip","state")]

names(q65)[2:7] <- all.ans$`65`$ans %>% as.character()

ans_65 <- data.frame(option = all.ans$`65`$ans %>% as.character(), no. = match(all.ans$`65`$ans.let, myLetters))

a65 <- merge(ling_data, ans_105, by.x = "Q065", by.y = "no.")
##lets remove other.
a65 <- a65 %>% filter(option != 'other')
a65 <- a65[,-c(2:68)]

both_ques <- merge(a105, a65, by = c("CITY","lat","long","STATE","newZIP"))


###############################################

#using crosstalk.
library(crosstalk)
library(leaflet)


a105_shared <- SharedData$new(a105)
a65_shared <- SharedData$new(a65)
both_shared <- SharedData$new(both_ques)
pal105 <- colorFactor("viridis", domain = both_ques$option.x)
pal65 <- colorFactor("viridis", domain = both_ques$option.y)

bscols(list(
         filter_checkbox("option", "Answer Options", a105_shared, ~option, inline = TRUE),
         #filter_select("STATE", "Choose the state", a105_shared, ~STATE),
         filter_checkbox("option", "Answer Options", a65_shared, ~option, inline = TRUE)),
         leaflet(a105_shared, width = "100%", height = 300) %>%
           addProviderTiles(providers$Stamen.Tower) %>%
           addCircleMarkers(radius = 4, color = ~pal105(option)),
        # filter_select("STATE", "Choose the state", a65_shared, ~STATE),
         leaflet(a65_shared, width = "100%", height = 300) %>%
           addProviderTiles(providers$Stamen.Tower) %>%
           addCircleMarkers(radius = 4, color = ~pal65(option))
)
pal105 <- colorFactor("viridis", domain = both_ques$option.x)
pal65 <- colorFactor("viridis", domain = both_ques$option.y)



####differences and similarities.

head(both_ques)

unique(both_ques$Q065)
##create UNIQUE ID?



both_ques %<>% 
  mutate(ID = group_indices(., Q065, Q105))

both_ques %>% filter(long > -125) %>%
ggplot(aes(x = long, y= lat, col = as.factor(ID))) + geom_point()











