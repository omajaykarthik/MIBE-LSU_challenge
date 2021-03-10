#In this section, you will analyze the Restaurant information dataset with the objective of answering the following questions:

#1. Present a column chart with the top 10 neighborhoods by the number of restaurants.
#2. Present a column chart with the top 10 neighborhoods by restaurant review score.
#3. Compute the top 10 biggest chains. Present the results in a tabular format. (Use the column rest_brand to determine restaurants of the same brand)
#4. Compute the average menu price and the number of menu items for each restaurant. (The rest_menu_item_price column is a list of characters. You might want to use the by_row and map functions)
#5. Present in a bar chart the number of items on the menu for the five most expensive and cheapest restaurants. (The number of items can be determined by counting the elements in the rest_menu_item_price column)

##################################  Insallation of packages and importing required libraries  ###############
# install.packages('ggplot2')
# install.packages('dplyr')
# install.packages("tidyverse")
# 
# install.packages("xlsx")
# install.packages('expss')
# install.packages('gridExtra')
# install.packages('purrrlyr')
# install.packages('ggpubr')
# install.packages('data.table')
#install.packages("gtable")


library(ggpubr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(purrrlyr)
library(data.table)

################################### Importing data and  basic dataset description ############################

resturants_mibe <- readRDS("C:/Users/Om Ajay Karthik/Desktop/LSU/resturants-mibe.rds")
delivery_mibe <- readRDS("C:/Users/Om Ajay Karthik/Desktop/LSU/delivery-mibe.rds")

# 
# summary(resturants_mibe) 
# 
# summary(delivery_mibe) 
# 
# count(resturants_mibe)
# 
# count(delivery_mibe)

##############################################################################################################

#Unique resturants neighborhood
length(unique(`resturants_mibe`$rest_neighborhood))

#Unique resturants
length(unique(`resturants_mibe`$rest_name))
############################################################################################################
#1. Present a column chart with the top 10 neighborhoods by the number of restaurants.

freq_rest_neighborhood= count(resturants_mibe, rest_neighborhood)
freq_rest_neighborhood_desc <-freq_rest_neighborhood[order(-freq_rest_neighborhood$n),]
freq_rest_neighborhood_desc_top_n = head(freq_rest_neighborhood_desc, n=10)

ggplot(freq_rest_neighborhood_desc_top_n, aes(x = reorder(rest_neighborhood, -n), y = n)) + 
  geom_bar(stat = "identity") +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
  labs(title = "Top 10 neighbourhood by number of restaurant") +
  xlab("Restaurant Neighbourhood ") +
  ylab("Count") +
  theme_test()

###################################################################################################
#2. Present a column chart with the top 10 neighborhoods by restaurant review score.

#rest_reviewscore_topn = head(resturants_mibe %>% filter(!is.na(rest_rating)) %>% arrange(-rest_rating,rest_neighborhood),n=10)


topn_by_review = head(resturants_mibe  %>%
  filter(!is.na(rest_rating))  %>% arrange(-rest_rating,rest_neighborhood) %>% select(rest_neighborhood,rest_rating) %>% unique(),n=10)

ggplot(topn_by_review, aes(x = reorder(rest_neighborhood, -rest_rating), y = rest_rating)) + 
  geom_bar(stat="identity", fill="darkgreen")+
  geom_text(aes(label=rest_rating), vjust=1.6, color="white", size=5)+
  labs(title = "Top 10 Neighbourhood by restaurant review score") +
  xlab("Restaurant Neighbourhood") +
  ylab("Rating") +
  theme_bw()

########################################################################################################
#3. Compute the top 10 biggest chains.
#Present the results in a tabular format. (Use the column rest_brand to determine restaurants of the same brand)

graphics.off()
biggest_chains = resturants_mibe %>% filter(!is.na(rest_brand)) %>%  count(.,rest_brand) 
names(biggest_chains)[1] <- "Restaurant_Brand"
names(biggest_chains)[2] <- "Count"
biggest_chains_desc <-biggest_chains[order(-biggest_chains$Count),]
biggest_chains_desc_top_n = head(biggest_chains_desc, n=10)

grid.draw(biggest_chains_desc_top_n)



                                                                                         

########################################################################################################
#4. Compute the average menu price and the number of menu items for each restaurant
#(The rest_menu_item_price column is a list of characters. You might want to use the by_row and map functions)

resturants_mibe_rest_menu_item_price_mean = resturants_mibe %>% 
  select(rest_name,rest_brand,rest_menu_item_price) %>%
  by_row(..f = function(this_row) {
    this_row[3] %>% unlist %>% mean})

names(resturants_mibe_rest_menu_item_price_mean)[4] <- "Average_price"

resturants_mibe_rest_menu_item_price_length = resturants_mibe %>%
  select(rest_name,rest_brand,rest_menu_item_price) %>%
  by_row(..f = function(this_row) {
    this_row[3] %>% unlist %>% length()})

names(resturants_mibe_rest_menu_item_price_length)[4] <- "Count_of_menu_items"


view(resturants_mibe_rest_menu_item_price_mean)
view(resturants_mibe_rest_menu_item_price_length)

########################################################################################################

#5. Present in a bar chart the number of items on the menu for the five most expensive and cheapest restaurants. 
#(The number of items can be determined by counting the elements in the rest_menu_item_price column)

menu_item_price_sum = resturants_mibe %>% select(rest_name,rest_neighborhood,rest_menu_item_price) %>% filter(!is.na(rest_menu_item_price )) %>% by_row(..f = function(this_row) {
  this_row[3] %>% unlist %>% sum()}) 

names(menu_item_price_sum)[4] <- "sum_of_price"
menu_item_price_sum$sum_of_price = as.numeric(as.character(menu_item_price_sum$sum_of_price))


menu_item_sum_top_5 = head(arrange(menu_item_price_sum,desc(sum_of_price)), n = 5)
menu_item_sum_bottom_5 = head(arrange(menu_item_price_sum,sum_of_price), n = 5)

expensive = ggplot(menu_item_sum_top_5, aes(x = reorder(rest_name, -sum_of_price), y = sum_of_price)) + 
  geom_bar(stat="identity", fill="slateblue")+
  geom_text(aes(label=sum_of_price), vjust=1.5, color="white", size=5)+
  labs(title = "Top 5 most expensive restaurants") +
  xlab("Restaurant Name") +
  ylab("Cost") +
  theme_bw()

cheapest = ggplot(menu_item_sum_bottom_5, aes(x = reorder(rest_name, sum_of_price), y = sum_of_price)) + 
  geom_bar(stat="identity", fill="red")+
  geom_text(aes(label=sum_of_price), vjust=1.4, color="white", size=5)+
  labs(title = "Top 5 most cheap restaurants") +
  xlab("Restaurant Name") +
  ylab("Cost") +
  theme_bw()


grid.arrange(expensive, cheapest)










