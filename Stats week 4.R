#Week 4 stats
#unit E 

library(tidyverse)
library(readr)
darwin <- read_csv("Data/darwin.csv")

str(darwin)
view(darwin)
 
#tidy:
#The data is NOT in a tidy format, each row needs to be a unique observation and each column a unique variable, whereas here there are two plants per row
elongation_darwin <- darwin %>% 
  pivot_longer(cols= c("Cross", "Self"), names_to="type", values_to="height")
view(elongation_dawin)

#another method , phil method
darwin <- darwin %>% 
  pivot_longer(cols=c("Self":"Cross"), 
               names_to="type", 
               values_to="height") %>% 
  mutate(type=factor(type, 
                     levels=c("Self",
                              "Cross"))) %>% 
  mutate(pair=factor(pair))

#By including pair (as a predictor variable) in our model (along with type) we are including the variance which is explained by the genotype of at least one of the plant’s parents

#Q. What can we interpret from our summary(model)
summary(darwin)
#Well it looks as though when you include variance explained by pairing, the type Cross/Inbred is still significant.
#However none of the pair groups appear to significantly alter the mean height of plants.
#On average within each pair the crossed plant is taller than the selfed plant. There is not a difference in the average height of plants between different pairs.
#Rather than look at each estimate in turn we can collapse and summarise these differences by running summary(aov()) on the model

model <- lm(formula = height ~ type + pair, data = darwin)#does an anova i think whic shows the differnece between pair
## Call:
## lm(formula = height ~ type + pair, data = darwin)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.4958 -0.9021  0.0000  0.9021  5.4958 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  19.1292     2.4364   7.851  1.7e-06 ***
## typeCross     2.6167     1.2182   2.148   0.0497 *  
## pair2        -4.2500     3.3362  -1.274   0.2234    
## pair3         0.0625     3.3362   0.019   0.9853    
## pair4         0.5625     3.3362   0.169   0.8685    
## pair5        -1.6875     3.3362  -0.506   0.6209    
## pair6        -0.3750     3.3362  -0.112   0.9121    
## pair7        -0.0625     3.3362  -0.019   0.9853    
## pair8        -2.6250     3.3362  -0.787   0.4445    
## pair9        -3.0625     3.3362  -0.918   0.3742    
## pair10       -0.6250     3.3362  -0.187   0.8541    
## pair11       -0.6875     3.3362  -0.206   0.8397    
## pair12       -0.9375     3.3362  -0.281   0.7828    
## pair13       -3.0000     3.3362  -0.899   0.3837    
## pair14       -1.1875     3.3362  -0.356   0.7272    
## pair15       -5.4375     3.3362  -1.630   0.1254    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.336 on 14 degrees of freedom
## Multiple R-squared:  0.469,  Adjusted R-squared:  -0.09997 
## F-statistic: 0.8243 on 15 and 14 DF,  p-value: 0.6434
  

 summary(aov(model))

 #This seems to be back up our observation that pair does not significantly affect the mean height, while type does. pair= 0.85 for Pr and type= 0.049 Pr 
 #At this point we can reject a hypothesis that the means of the crossed and self plants are equal within each pair.
# However we cannot reject a hypothesis that the mean heights between the different pairs of plants are equal.

 #Plotting our estimated means
 #Confidence intervals of the mean 
 library(emmeans) # A handy package for estimating means from the fit of our model
 
 estimates <- emmeans(model, specs="type") ### here it will take the average of the values across all the pairs to calculate means for type
 
 estimates %>% 
   as_tibble %>% ### emmeans outputs a grid by default, but can easily be changed
   ggplot(aes(x=type, 
              y=emmean, 
              colour=type))+
   geom_pointrange(aes(ymin=lower.CL, 
                       ymax=upper.CL))+
   geom_pointrange(aes(ymin=emmean-SE, 
                       ymax=emmean+SE), 
                   size=1.2)
#Let’s start by calculating the 95% Confidence Intervals of our two means.
#Thick lines represent the standard error (66% Confidence intervals), thin line are the 95% Confidence intervals

 #Calculate Confidence Intervals
#These are the 95% CI of the average difference between treatments, so these are perfectly complementary to the 5% P -value that is our benchmark for significance

 
  #force your model summary into a dataframe format, then use mutate to calculate the upper & lower CI for each estimate
 library(broom)
tidymodel1 <- broom::tidy(model) %>% 
   mutate(lwr=((estimate-(std.error*2))),
          upr=(estimate+(std.error*2)))

tidymodel1
#If we draw a graph plotting an estimate as a point, and lines with lengths equivalent to 2* the Standard Error then we get whether an estimate mean is significantly different from the intercept at P < 0.05. According to the Normal (Z) distribution.
#If set this to 3 * the Standard Error then this would be P < 0.01.

#Plot the 95% CI of the estimates against the Intercept.
tidymodel1 %>% 
  ggplot(aes(x=estimate, 
             y=term))+
  geom_pointrange(aes(xmin=lwr, 
                      xmax=upr))+
  geom_vline(xintercept=0, 
             linetype="dashed")
#Anything which crosses the 0 is not significantly different at P<0.05 from the mean calculated for the Intercept
#no estimate is significantly different from the intercept except type.

#calculate confidence intervals 
tidymodel2 <- broom::tidy(model, conf.int=T) 
tidymodel2[2,] ## only pull out row 2 

#Q. Compare the confidence intervals for tidymodel1 and tidymodel2 - Why do you think they are not quite the same?
#i think it is because tidy 1 assumes a z distibution with more than 30 samples and 2 uses a t distribution with around or below 30 samples
knitr::include_graphics("insert path")
#we can also use the tcrit function set to a threshold of 0.975 (equivalent to 0.95 for a two-sided test).
t.crit <- qt(0.975, df=14)

upr <- 2.62+t.crit*1.22
lwr <- 2.62-t.crit*1.22

#Reporting
#Note:
#Pairs was included as a fixed effect to account for the paired-experiment design
#we are interested in the type~height relationship
#The confidence intervals of the mean difference allows us to talk about the effect size
#write up:

#if we wanted to do 1 sd from the mean then:
{confint(model, level=0.66)}
