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

unique()= #find the outlier name
  hist()= #can find continous outlier

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

elongation_darwin %>% 
  summarise(mean=mean(height))
##Linear models 

model1 <- lm(height~1, data=darwin)
model1
#this will take the mean of both of the factors

#We can create a new linear model object for this called model2, tests the difference between the different means
model2 <- lm(height~type, data=elongation_darwin)
model2
#intercept= average of the cross height, typeself= gradient of the slope connecting the two means. 
 #The model formula now actually contains the slope of the line produced by type as well as the intercept indicated by 1 (try it for yourself - R will treat height~1+type & height~type in exactly the same way)
#The important distinction is that now the intercept is no longer the total mean of our plants (this was 18.88 now 20.192).

#So what does the intercept represent now?:
#We can use a process of elimination here. The label of the other number is TypeSelf, since this type has only two levels and typeCross is absent this must be represented by the Intercept.
#Confirm this with group_by() and summarise()

elongation_darwin %>%  summarise(.group(type))

#What does -2.617 represent in the column typeSelf?
#it shows the difference between the mean height of selfed plants and the Intercept.
#The slope and intercept define the linear relationship between the means of these two groups. And this is represented in our simple line equation height=20.19−2.62(typeSelf)

#shows the difference between the mean heighs of the plants which is the typeSef value from lm(formula= height~type, data=.)
elongation_darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1,#starts at factor 1
                   xend=2,#end of factor 2
                   y=20.192,#this is the cross intercept of the two factors 
                   yend=20.192-2.617),#this is the intercept minus the slope of the line accorsing the y=mx+c
               linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)

#set the factor levels and therefore the intercept to whichever group makes the most sense for your analysis.
darwin_relevel <- elongation_darwin %>% 
  mutate(type=factor(type, 
                     levels=c("Self",
                              "Cross")))

lm(height~type, data=darwin_relevel)

summary(model2)

#multiple R squared= 17.5% of the model shows that this much is explaine byy the value 

#anova
anova_test(height~type, data=darwin)
#benefit of linear model= mean height and difference between the crossed heights which direction is there a diffference


