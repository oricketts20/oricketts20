lenses <- read.csv("C:\\Users\\W530\\Downloads\\lenses.csv")
# create a Contingency table to count category vs size
# note the syntax <dataframe>$<column>
table(lenses$impairment, lenses$Coating)
lenses$Coating = factor(lenses$Coating)
#### Descriptive stats
#Ex. 2(b)
# summary will produce descriptive stats for a entire column
summary(lenses$impairment)
library(mosaic)
favstats(impairment ~ Coating, data=lenses)
install.packages("mosaic", lib="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
  Coating min Q1    median Q3  max  mean    sd        n missing
1       A 3.6 3.80    4.0 4.10 4.2 3.942857 0.2225395 7       0
2       B 3.9 4.05    4.2 4.25 4.4 4.157143 0.1718249 7       0
3       C 4.8 5.00    5.1 5.25 5.6 5.142857 0.2636737 7       0
4       D 3.2 3.30    3.4 3.45 3.5 3.371429 0.1112697 7       0



## mean follows the following order C>B>A>D
##median follows the order C>B>A>D
CENTRAL TENDENCY FROM LARGEST TO SMALLEST FOLLOWS THE ORDER
## SD follow a similar pattern C>A>B>D
boxplot(impairment~Coating,data=lenses,
        main="Coating vs Impairment")
#change title names
## Ex. 7(c)
## Same picture, "D" has the smallest impairment after abrasion and
## it is consistent (smallest variance). Coating "C" the biggest with
## bigger variability but not overlap with other groups. Coatings "A"
## and "B" overlap with "A" having a smaller impairment.
## No outliers

## Symmetry: C most symmetrical, A/B/D negative skewed, D has q3/q4
## almost identical, i.e. high concentration of above average/median
## values.
#As a result, it appears that initially d is the most effective coating wrt providing the least
#amount of impairment after abrasian 

# move on to a test for variance
# H0: sigma^2_A = sigma^2_B = sigma^2_C = sigma^2_D
# H1: at least one is different

library(car)
leveneTest(impairment ~ Coating, data=lenses)
# Result:
#levene's Test for Homogeneity of Variance (center = median)
      #Df F value Pr(>F)
#group  3  0.8451 0.4827
      #24   
Conclusion:
  # Since the p-value of the Levene’s test is large 0.4827 > 0.05 we conclude that 
  # there is no evidence to reject the equality of variances
  
  
  #(b) Test whether there is any evidence of a difference in the mean impairment of 
#coating by the 4 manufacturers

  # H_0: mu_A = mu_B = mu_C = mu_D
  # H_1: when subjected to the same degree of similuated abrasion,
#at least one coating produces a performance of protectiveness(measured via impairment) with a 
#different average impairment value

# The significance level I will use is 0.05
  
model <-aov(impairment ~ Coating, data=lenses)
summary(model)

           Df   Sum Sq   Mean Sq  F value   Pr(>F)    
Coating    3    11.444    3.815   94.8     1.91e-13 ***
Residuals  24   0.966    0.040                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Conclusion
# The ANOVA p-value is very small (1.91×10−13) this is much less than 0.01. 
# We conclude that there is at least one population mean unequal to the other(s)
# at the significance level of 5%
# We will now carry out a TukeyHSD to dicover which pairs are different

TukeyHSD(model)
#results
Fit: aov(formula = impairment ~ Coating, data = lenses)

$Coating
       diff         lwr        upr     p adj
B-A  0.2142857 -0.08149837  0.5100698 0.2164491
C-A  1.2000000  0.90421592  1.4957841 0.0000000
D-A -0.5714286 -0.86721265 -0.2756445 0.0001007
C-B  0.9857143  0.68993020  1.2814984 0.0000000
D-B -0.7857143 -1.08149837 -0.4899302 0.0000008
D-C -1.7714286 -2.06721265 -1.4756445 0.0000000


plot(TukeyHSD(model))
#comparisons that dont cross the 0 boundary differ significantly
#we conclucde that at an a of 5%, the performance of coatings
#C-A, D-A, C-B, D-B, D-C ALL DIFFER SIGNIFICANTLY from each other AS THEY DONT CROSS THE
#0 BOUNDARY ON THE PLOT
#this is also demonstrated by the low p values of C-A, D-A, C-B, D-B, and D-C
#B-A Does not DIFFER SIGNIFICANTLY AS IT DOES CORSS THE 0 BOUNDARY ON THE PLOT, HIGHER P VALUE

# Further more, looking at the box plot coating D seems to stand out. 
# The amount of impairment of coating  D Seems to be lower, on average compared to 
# coatings A, B and C.
performance of coatings - D>A>B>C As D HAS THE LOWEST MEAN IMPAIRMENT VALUE

#there is also overlap of the boxplots representing A and B, whereas no other boxplots overlap
#it is therefore no surprise that B-A does not differ significantly



#boxplot(lenses$impairment, main="Lenses Impairment", horizontal=TRUE)

