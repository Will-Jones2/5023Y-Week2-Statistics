###Stats Week 2 semester 2 
##Unit E 

library(tidyverse)
library(readr)
library(skimr)

darwin <- read_csv("Data/darwin.csv")
#null hypothesis: There will be no significant difference between the height of seedlings of cross and self fertilisation in maize (Zea mays).
view(darwin)
str(darwin)
view(darwin)#look at the table in a viewable format
nrow(darwin)#number of row
length(darwin)#num of columns
head(darwin,6)#view first 6 rows, or whatever number is input, if no number 6 is shown
tail(darwin,5) 

skim(darwin)

#tidy:
#The data is NOT in a tidy format, each row needs to be a unique observation and each column a unique variable, whereas here there are two plants per row
elongation_darwin <- darwin %>% 
  pivot_longer(cols= c("Cross", "Self"), names_to="type", values_to="height")
 view(elongation_dawin)

 #check height distributions 
 elongation_darwin %>% 
   ggplot(aes(x=height))+geom_histogram(bins=20)
 #A quick histogram does not indicate we have any outliers. But we haven’t separated any sub-groups yet.
                                      
ggplot(elongation_darwin, aes(x=type, y=height))+
  geom_jitter(width=0.1, 
               pch=21, 
               aes(fill=type))+
                theme_classic()
##Linear models 

