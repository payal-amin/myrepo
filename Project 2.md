Further Analysis of US States’ Arrest Numbers through the Lens of Rent,
Income, & Urban Population
================

### Payal Amin (pja648)

## Introduction

This report focuses on the fifty states of the United States of America,
looking at each states’ annual income and rent estimates in relation to
the arrest rates and percentage of the total population that is
described as urban. Two datasets were combined for this analysis. The
first dataset, titled *US rent and income data*, was found in the
*tidyr* library. This dataset contains five variables, GEOID (state
identifier code), the name of the state, a variable column containing
labels of median yearly income and median monthly rent, and a column
with the corresponding values for the aforementioned variable’s values,
the final variable is the 90% margin of error for the income and rent
values. The second dataset, titled \* Violent Crime Rates by US State\*
was found in the *datasets* library. This dataset contains four
variables, Murder arrests (per 100,000), Assault arrests (per 100,000),
Rape arrests (per 100,000), and the percent of the population that is
urban.

The *income and rent* dataset is intriguing as it has the potential to
show the association between income levels and rent throughout the U.S.
On the other hand, the *crimes and urban population* dataset provides
insightful information about the rate of arrest within different arrest
types, and how that corresponds to the proportion of the total
population that is urban. Furthermore, the combination of these two
datasets can provide important information about the interaction between
the monetary status of a U.S. state and the proportion of the population
that is urban, providing the possibility to delve into how arrest rates
relate to this aspect of U.S. states.

At first glance, it is expected that the income levels will influence
the arrest numbers in states, and I am intrigued to see how that varies
across the three types of arrest. Additionally, another prediction is
that the urban population will play a role in rent and income values. In
order to study the data further, the original datasets were tailored and
combined in order to offer more insight into the potential associations.

### Tidying & Joining of Datasets

``` r
library(tidyverse) #uploading tidyverse  
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.0     ✓ dplyr   1.0.3
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr) #uploading dplyr

library(tidyr) #uploading tidyr to obtain dataset
?us_rent_income #obtaining information about the dataset
library(datasets) #uploading datasets to obtain USArrests data
?USArrests #obtaining information about the dataset
```

As noted above, the *tidyverse* and *dplyr* libraries were loaded in
order to complete the computations shown below. However, the *tidyr* and
*datasets* libraries were used to call for the datasets of study.

``` r
USArrests_tidy <- USArrests %>% #creating a tidy version of the USArrests dataset
  rownames_to_column("State") #turning the State names into a variable, with their own column
USIncome_tidy <- us_rent_income %>% #creating a tidy version of the us_rent_income dataset
  pivot_wider(names_from = variable, names_sep = ".", values_from = c(estimate, moe)) %>% #using the pivot_wider function to create a separate columns/variables for income and rent using the values in the estimate and moe columns
  rename(State=NAME, "AnnualIncome"=estimate.income, "MonthlyRent"=estimate.rent) %>% #renaming the columns to tidy the appearance
  select(-GEOID, -moe.income, -moe.rent) #removing the GEOID and margin of error columns
head(USArrests_tidy)
```

    ##        State Murder Assault UrbanPop Rape
    ## 1    Alabama   13.2     236       58 21.2
    ## 2     Alaska   10.0     263       48 44.5
    ## 3    Arizona    8.1     294       80 31.0
    ## 4   Arkansas    8.8     190       50 19.5
    ## 5 California    9.0     276       91 40.6
    ## 6   Colorado    7.9     204       78 38.7

``` r
head(USIncome_tidy)
```

    ## # A tibble: 6 x 3
    ##   State      AnnualIncome MonthlyRent
    ##   <chr>             <dbl>       <dbl>
    ## 1 Alabama           24476         747
    ## 2 Alaska            32940        1200
    ## 3 Arizona           27517         972
    ## 4 Arkansas          23789         709
    ## 5 California        29454        1358
    ## 6 Colorado          32401        1125

The *USArrests* dataset initially had each state as a row and not
categorized as a variable. The data was tidied by making the U.S. state
names into a variable, with each state name as an observation. This
resulted in a dataset with 4 variables and 50 observations. The
*us\_rent\_income* dataset was tidied by creating new *income* and
*rent* variables, taking the corresponding values from the estimate and
margin of error columns. Additionally, the columns were renamed to be
more efficient, and the GEOID and both margin of error columns were
removed due to lack of pertinent information. The new dataset contained
3 variables with 52 observations.

``` r
USStates <- right_join(USIncome_tidy, USArrests_tidy, by = "State") #creating a  new dataset by merging both tidied datasets
USStates <- USStates %>% #replacing the newly merged dataset
  pivot_longer(cols = c(`Murder`, `Assault`, `Rape`), names_to = "ArrestType", values_to = "ArrestNumber") #combining all arrest typed under one variable and all arrest numbers under another variable
```

A *right\_join* function was utilized in order to drop 2 observations
from the USIncome\_tidy dataset, as those observations did not have
corresponding values in the USArrests\_tidy dataset. The use of
right\_join meant that any USIncome\_tidy observations that didn’t have
a match in the USArrests\_tidy dataset, would be dropped. The two
observations that were removed were “Puerto Rico” and the “District of
Columbia”. Due to the joining of data, an additional tidying step was
performed. The 3 different arrest types were combined into one variable
called “Arrest Type”. The corresponding number of murder, rape, and
assault arrests were combined into a new variable called “Arrest
Number”.

``` r
USStates <- USStates %>% #making changes to the main combined dataset using a dplyr vector
  mutate(UrbPopQuartile = case_when(UrbanPop<=50 ~ "50% or less", 50<UrbanPop ~ "greater than 50%")) #using mutate function to create a new, categorical variable
```

As seen above, the *mutate* function from dplyr was used to create a new
categorical variable called “UrbPopQuartile”. This variable contains 2
different observations, each corresponding to the proportion of a
state’s population that is urban. If a state has an urban population of
50% or below, that state would have a corresponding UrbPopQuantile value
of “50% or less”. If the urban population greater than 50%, then it
would receive an UrbPopQuantile value of “greater than 50%”. This
variable essentially sorts all U.S. states into each of the 2 halves of
urban population percentage.

``` r
USStates <- USStates %>% #making changes to the main combined dataset using a dplyr vector
  mutate(AnnualRent = MonthlyRent*12) %>% #mutate function to create new variable
  mutate(DisposableIncome = AnnualIncome-AnnualRent) #mutate function to create new variable
```

Here, an two additional variables were created using the *mutate*
function. A variable called “Annual Rent” was created by multiplying all
of the values in the “Monthly Rent” variable by a value of 12 (to
represent 12 months in a year). A second variable titled “Disposable
Income”, was made by subtracting the annual rent values of each state
from each state’s annual income values. The idea behind the Disposable
Income variable was that associations could be drawn between the
percentage of an urban population in a state, the type and numbers of
arrests, and the spending power of each state (represented by the
Disposable Income).

``` r
USStates <- USStates[,c(1,3,8,2,9,4,7,5,6)] #reorder after addition of new columns
USStates <- USStates %>%
  mutate_if(is.character, as.factor) #considering character variables as factors
head(USStates)
```

    ## # A tibble: 6 x 9
    ##   State   MonthlyRent AnnualRent AnnualIncome DisposableIncome UrbanPop
    ##   <fct>         <dbl>      <dbl>        <dbl>            <dbl>    <int>
    ## 1 Alabama         747       8964        24476            15512       58
    ## 2 Alabama         747       8964        24476            15512       58
    ## 3 Alabama         747       8964        24476            15512       58
    ## 4 Alaska         1200      14400        32940            18540       48
    ## 5 Alaska         1200      14400        32940            18540       48
    ## 6 Alaska         1200      14400        32940            18540       48
    ## # … with 3 more variables: UrbPopQuartile <fct>, ArrestType <fct>,
    ## #   ArrestNumber <dbl>

------------------------------------------------------------------------

## EDA

Below, several values and plots were explored in order to assess the
variables that would be tested against one another in the following
MANOVA, randomization, linear regression, and logistic regression tests.

``` r
library(tidyverse) #uploading tidyverse  
library(dplyr) #uploading dplyr
library(ggplot2) #uploading ggplot2
library(psych)#uploading psych
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
pairs.panels(USStates[-1,], # getting univariate and bivariate summaries
             method = "pearson", #method for correlation coefficient 
             hist.col = "yellow", #histogram color
             smooth = FALSE, density = FALSE, ellipses = FALSE)
```
![image](https://user-images.githubusercontent.com/83299916/117582488-d5fc6300-b0c7-11eb-86ed-21e13f904205.png)

![](Project-2_files/figure-gfm/matrix-1.png)<!-- -->

Looking at the correlation matrix above, it seems that annual income and
disposable income have the strongest correlation of 0.73. As the annual
rent variable was created by multiplying the monthly rent variable, the
strength of their correlation will be ignored for now. Annual rent and
annual income also have a fairly good correlation of 0.70.

``` r
#FOR MANOVA
#How do AnnualRent, AnnualIncome, ArrestNumber differ between UrbPopQuartiles
USStates %>%
  group_by(UrbPopQuartile) %>%
  summarise(mean(AnnualRent), mean(AnnualIncome), mean(ArrestNumber))
```

    ## # A tibble: 2 x 4
    ##   UrbPopQuartile   `mean(AnnualRent)` `mean(AnnualIncome)` `mean(ArrestNumber)`
    ## * <fct>                         <dbl>                <dbl>                <dbl>
    ## 1 50% or less                   9901.               27294                  67.5
    ## 2 greater than 50%             11462.               29262.                 66.4

``` r
#graph of means per UrbPopQuartile
USStates %>%
  select(UrbPopQuartile,AnnualRent, AnnualIncome, ArrestNumber) %>% #selecting the variables
  pivot_longer(-1, names_to='DV', values_to = 'measure') %>%
  ggplot(aes(UrbPopQuartile,measure,fill=UrbPopQuartile)) + #coloring by UrbPopQuartile conditions
  geom_bar(stat="summary", fun = "mean") + #getting mean values
  geom_errorbar(stat="summary", fun.data="mean_se", width = .4)+ #error bars of means
  facet_wrap(~DV, nrow=2) +
  coord_flip() +
  ylab("") +
  labs(title="Mean Values Across UrbPopQuartile Conditions")+ #title
  theme(legend.position = "none")
```
![image](https://user-images.githubusercontent.com/83299916/117582500-df85cb00-b0c7-11eb-98ab-66b659d8bddb.png)

![](Project-2_files/figure-gfm/MANOVA-1.png)<!-- -->

Here, the mean values of the Annual Rent, Annual Income, and Arrest
Number variables were calculated across both of the UrbPopQuartile
conditions and then displayed in a graph. The means appear to be
slightly lower for the states with a 50% or less urban population in the
annual income and annual rent variables. It is difficult to observe this
in the arrest number graph due to the comparatively smaller mean values.
Therefore, it is worth assessing if the mean values of Annual Rent,
Annual Income, and Arrest Number differ between both of the
UrbPopQuartile groups for at least one of the variables.

``` r
#FOR RANDOMIZATION TEST
#looking at multivariate plots of response variable for each UrbPopQuartile
ggplot(USStates, aes(x = UrbanPop, y = ArrestNumber)) +
  geom_point(alpha = .5) + 
  facet_wrap(~UrbPopQuartile) + #splitting by UrbPopQuartile conditions
  labs(title="Arrest Numbers Between UrbPopQuartile Conditions", x = "Percentage of Population that is Urban (%)", y = "Arrest Number (per 100,000 residents)") #titles
```
![image](https://user-images.githubusercontent.com/83299916/117582507-e7456f80-b0c7-11eb-90d4-fbfa4797a44b.png)

![](Project-2_files/figure-gfm/randomization-1.png)<!-- -->

In these two graphs, we are visualizing the actual 50% split of the
urban populations within the UrbPopQuartile conditions, and how the
arrest numbers vary between the two groups. It appears that there are
many more arrests in the states with an urban population that is greater
than 50%. Additionally, states with a 50% or less urban population tend
to have either very low arrest numbers, or a few high values. In
contrast, the states with greater than 50% urban populations have a
concentration around lower arrest numbers, in addition to a greater
distribution in the middle and higher values. Based on this visual
observation, it would be a good idea to see if the mean arrest number
differs between the states in the two UrbPopQaurtile conditions.

``` r
ggplot(USStates, aes(x = DisposableIncome, y = ArrestNumber))+ #plot and setting x and y aesthetics
  geom_point(aes(color=ArrestType)) + #mean arrest number, colored by UrbPopQuartile
  labs(title = "Disposable Income and Arrest Number by Arrest Types", x = "Disposable Income ($)", y = "Number of Arrests (per 100,000 residents)") #title and axes labels
```
![image](https://user-images.githubusercontent.com/83299916/117582513-eb718d00-b0c7-11eb-9179-e0b5567905c8.png)

![](Project-2_files/figure-gfm/linear-1.png)<!-- -->

This plot shows that the assault arrests seem to be inflected by
disposable income in that the arrests decrease as the disposable income
increases. It appears that the disposable income has minimal influence
on the murder and rape arrests. An analysis can be performed over the
influence of disposable income upon the arrest numbers, and how that
differs between the three arrest types.

``` r
ggplot(USStates, aes(AnnualRent, AnnualIncome)) + #plot and setting x and y aesthetics
  geom_point(aes(color=UrbPopQuartile)) + #coloring points by UrbPopQuartile
  labs(title = "Annual Rent and Income by UrbPopQuartile", x = "Annual Rent ($)", y = "Annual Income ($)") #title and axes labels
```
![image](https://user-images.githubusercontent.com/83299916/117582515-ef9daa80-b0c7-11eb-9141-19c6e699cc4e.png)

![](Project-2_files/figure-gfm/logistic-1.png)<!-- -->

Overall, this graph displays a linear relationship between annual rent
and annual income. However, it appears that the percentage of the urban
population does not have a large influence upon this pattern. It would
be worth testing if the annual rent and annual income significantly
effect the outcome of whether or not a state has an urban population of
50$ or less, or greater than 50%.

------------------------------------------------------------------------

## MANOVA

A MANOVA test was performed to assess how a subset of the numeric
variables compared across the different groups of UrbPopQuartile
(i.e. 50% or less, more than 50%). The variable of Monthly Rent was
excluded from this test as the Annual Rent Income variable (which is a
mutated version of the Monthly Rent) is better to compare against the
Annual Income. The DisposableIncome variable was also excluded since it
is another mutated version of the Rent and Income variables.
Additionally, as the categorical variable of UrbPopQuartile is based off
of the values in the UrbanPop variable, which shows the exact percentage
of the state population that is urban, the UrbanPop variable was also
excluded. First, we will assess the assumptions needed for the test to
take place.

### Assumptions

``` r
#checking for normality with SHAPIRO (3 variables)
USStates %>%
  group_by(UrbPopQuartile) %>%
  summarize(p.value = shapiro.test(AnnualRent)$p.value)
```

    ## # A tibble: 2 x 2
    ##   UrbPopQuartile       p.value
    ## * <fct>                  <dbl>
    ## 1 50% or less      0.0000924  
    ## 2 greater than 50% 0.000000210

``` r
USStates %>%
  group_by(UrbPopQuartile) %>%
  summarize(p.value = shapiro.test(AnnualIncome)$p.value)
```

    ## # A tibble: 2 x 2
    ##   UrbPopQuartile    p.value
    ## * <fct>               <dbl>
    ## 1 50% or less      0.00630 
    ## 2 greater than 50% 0.000553

``` r
USStates %>%
  group_by(UrbPopQuartile) %>%
  summarize(p.value = shapiro.test(ArrestNumber)$p.value)
```

    ## # A tibble: 2 x 2
    ##   UrbPopQuartile    p.value
    ## * <fct>               <dbl>
    ## 1 50% or less      1.06e- 6
    ## 2 greater than 50% 1.07e-13

``` r
#checking for equal variance
#if there was a ratio of 4 between the variances then we would say it violated
USStates %>%
  group_by(UrbPopQuartile) %>%
  summarize(variance = var(AnnualRent))
```

    ## # A tibble: 2 x 2
    ##   UrbPopQuartile   variance
    ## * <fct>               <dbl>
    ## 1 50% or less      3590001.
    ## 2 greater than 50% 5330768.

``` r
USStates %>%
  group_by(UrbPopQuartile) %>%
  summarize(variance = var(AnnualIncome))
```

    ## # A tibble: 2 x 2
    ##   UrbPopQuartile    variance
    ## * <fct>                <dbl>
    ## 1 50% or less      13103236.
    ## 2 greater than 50% 10462508.

``` r
USStates %>%
  group_by(UrbPopQuartile) %>%
  summarize(variance = var(ArrestNumber))
```

    ## # A tibble: 2 x 2
    ##   UrbPopQuartile   variance
    ## * <fct>               <dbl>
    ## 1 50% or less        10141.
    ## 2 greater than 50%    7378.

``` r
USStates2 <- USStates[,c(1, 2, 3, 4, 9, 5, 6, 7, 8)] #reorder for covariance matrix


#checking homogeneity of (co)variances
covmats <- USStates2 %>%
  group_by(UrbPopQuartile) %>%
  do(covs=cov(.[3:5]))
for(i in 1:3){print(as.character(covmats$UrbPopQuartile[i])); print(covmats$covs[i])} #setting covariance matrices per UrbPopQuartile
```

    ## [1] "50% or less"
    ## [[1]]
    ##              AnnualRent AnnualIncome ArrestNumber
    ## AnnualRent   3590001.23   4413222.00     26208.25
    ## AnnualIncome 4413222.00  13103236.15    -37223.04
    ## ArrestNumber   26208.25    -37223.04     10140.73
    ## 
    ## [1] "greater than 50%"
    ## [[1]]
    ##              AnnualRent AnnualIncome ArrestNumber
    ## AnnualRent   5330768.24   5212352.44     14859.42
    ## AnnualIncome 5212352.44  10462507.75    -16565.17
    ## ArrestNumber   14859.42    -16565.17      7378.48
    ## 
    ## [1] NA
    ## [[1]]
    ## NULL

Using the Shapiro test for normality shows that all three of the numeric
variables (Annual Rent, Annual Income, and Arrest Number) have failed
the assumption of normality, meaning that all of the p-values are less
than 0.05. Thus, allowing for the rejection of the null hypothesis that
the variables are normally distributed. As for the equal variance, since
none of the covariances within a given numerical variable have a ratio
of 4 between the categorical conditions, we can say the variance for
each numerical variable across groups is equal. However, “50% or less”
and “more than 50%” have very different covariances across the three
numeric variables.

### Performing MANOVA test

The null hypothesis states that for the numeric variables of Annual
Rent, Annual Income, and Arrest Number, their respective mean values
will be equal across both of the UrbPopQuartile groups. The alternative
hypothesis states that for the numeric variables of Annual Rent, Annual
Income, and Arrest Number, the mean values different between both of the
UrbPopQuartile groups for at least one of the numeric variables.

``` r
manova_US <- manova(cbind(AnnualRent,AnnualIncome,ArrestNumber) ~ ArrestType, data = USStates) #creating vector for MANOVA output

summary(manova_US) #output of MANOVA
```

    ##             Df  Pillai approx F num Df den Df    Pr(>F)    
    ## ArrestType   2 0.73032   27.993      6    292 < 2.2e-16 ***
    ## Residuals  147                                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#performing one-way ANOVA for each variable that has because MANOVA is significant
  summary.aov(manova_US)
```

    ##  Response AnnualRent :
    ##              Df    Sum Sq Mean Sq F value Pr(>F)
    ## ArrestType    2         0       0       0      1
    ## Residuals   147 797643323 5426145               
    ## 
    ##  Response AnnualIncome :
    ##              Df     Sum Sq  Mean Sq F value Pr(>F)
    ## ArrestType    2          0        0       0      1
    ## Residuals   147 1702888596 11584276               
    ## 
    ##  Response ArrestNumber :
    ##              Df Sum Sq Mean Sq F value    Pr(>F)    
    ## ArrestType    2 818321  409160  174.06 < 2.2e-16 ***
    ## Residuals   147 345541    2351                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#post-hoc analysis of the one significant variable
pairwise.t.test(USStates$ArrestNumber,USStates$ArrestType, p.adj="none")  #For arrest number
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  USStates$ArrestNumber and USStates$ArrestType 
    ## 
    ##        Assault Murder
    ## Murder <2e-16  -     
    ## Rape   <2e-16  0.17  
    ## 
    ## P value adjustment method: none

``` r
    #Ho (ANOVA): the mean Arrest Number  for Murder, Assault, and Rape are all the same
    #Ha: at least one mean arrest number differs from the others
    
#Pr(at least 1 type I error) = 1 - 0.95^7
1 - 0.95^7
```

    ## [1] 0.3016627

``` r
#to interpret the p-values, what is the value for the Bonferonni alpha?
    #tests=7
    #a' = (a/7) 
0.05/7
```

    ## [1] 0.007142857

Here, we reject the null hypothesis and state that for the numeric
variables of Annual Rent, Annual Income, and Arrest Number, the mean
values will be different between both of the UrbPopQuartile groups for
at least one of these numeric variables (F=27.99, df=2, 292,
p-value&lt;0.001).

In order to find which for which numeric variable the mean differed,
one-way ANOVA tests were performed. The ANOVA tests showed that the mean
Annual Rent and mean Annual Income did not differ across UrbPopQuartile
conditions (F=0, df=2, p=1). However, the mean Arrest numbers per
100,000 residents did differ across UrbPopQuartile conditions (F=174.06,
p&lt;0.001). Within the arrest numbers variable, we reject the null
hypothesis that the mean arrest numbers per 100,000 residents between
Murder and Assault, and between Rape and Assault, are the same
(p&lt;0.001). However, we fail to reject the null hypothesis that the
mean arrest numbers per 100,000 residents between Murder and Rape are
the same (p&gt;0.05).

In total, there was 1 MANOVA test, 3 ANOVA tests, and then 3 t-tests
that were conducted. The probability of at least 1 type-I error is
0.302. To keep the overall type I error rate at .05, the adjusted alpha
level (or a’) should be 0.007. The significance of the post hoc tests
did not change after the alpha adjustment.

------------------------------------------------------------------------

## Randomization Test

We will now look at the mean difference in arrest numbers across the two
categories of UrbPopQuartile. This can be accomplished using a
randomization test, which is useful to account for the violation of
assumptions. For this randomization test, the null hypothesis states
that the mean arrest number is the same for states with 50% or less of
an urban population compared to states with more than 50% of an urban
population. The alternative hypothesis states than the mean arrest
number is different for states with 50% or less of an urban population
compared to states with more than 50% of an urban population.

``` r
#checking for normality
USStates %>%
  group_by(UrbPopQuartile) %>%
  summarize(p.value = shapiro.test(ArrestNumber)$p.value)
```

    ## # A tibble: 2 x 2
    ##   UrbPopQuartile    p.value
    ## * <fct>               <dbl>
    ## 1 50% or less      1.06e- 6
    ## 2 greater than 50% 1.07e-13

``` r
#checking for equal variance
USStates %>%
  group_by(UrbPopQuartile) %>%
  summarize(variance = var(ArrestNumber))
```

    ## # A tibble: 2 x 2
    ##   UrbPopQuartile   variance
    ## * <fct>               <dbl>
    ## 1 50% or less        10141.
    ## 2 greater than 50%    7378.

``` r
#showing the distribution of time for each condition
ggplot(USStates, aes(ArrestNumber,fill=UrbPopQuartile)) + #setting the variables
  geom_histogram(bins=6.5) + #setting histograms
  facet_wrap(~UrbPopQuartile,ncol=2) +
  theme(legend.position="none")+  #removing the legend
  labs(title = "Distribution of Arrest Numbers for Each Urban Population Quartile Condition", x = "Arrest Number (per 100,000 residents)", y = "Count") + #titles
  scale_fill_manual("Percentage of Population that is Urban", values = c("50% or less" = "#A180F5", "greater than 50%" = "#FCD2F6"))  #colors for UrbPopQuartile
```
![image](https://user-images.githubusercontent.com/83299916/117582528-ff1cf380-b0c7-11eb-8fe8-90faa1607855.png)

![](Project-2_files/figure-gfm/manual-1.png)<!-- -->

``` r
#calculating the mean difference between the two conditions
true_diff <- USStates %>%
  group_by(UrbPopQuartile) %>%
  summarize(means = mean(ArrestNumber)) %>%
  summarize(mean_diff = diff(means)) %>%
  pull
true_diff
```

    ## [1] -1.132791

``` r
# -1.132791, there is a difference but we need to know if it's chance or not

#resampling the arrest number across UrbPopQuartile
perm1 <- data.frame(UrbPopQuartile = USStates$UrbPopQuartile, ArrestNumber = sample(USStates$ArrestNumber))
head(perm1)
```

    ##     UrbPopQuartile ArrestNumber
    ## 1 greater than 50%         48.0
    ## 2 greater than 50%          3.4
    ## 3 greater than 50%        161.0
    ## 4      50% or less        110.0
    ## 5      50% or less          7.8
    ## 6      50% or less         14.9

``` r
#this is "without replacement" so using only the existing data values


set.seed(348)

mean_diff <- vector() #creating empty vector to store mean differences 

#repeating randomization of mean differences with a for loop
for(i in 1:5000){ 
  USStates1 <- data.frame(UrbPopQuartile = USStates$UrbPopQuartile, ArrestNumber = sample(USStates$ArrestNumber)) 
  
  mean_diff[i] <- USStates1 %>% 
    group_by(UrbPopQuartile) %>%
    summarize(means = mean(ArrestNumber)) %>%
    summarize(mean_diff = diff(means)) %>%
    pull
}

#displaying the distribution of the mean differences with a vertical line that represents the true difference
{hist(mean_diff, main="Distribution of the mean differences"); abline(v =1.132791, col="red")}
```
![image](https://user-images.githubusercontent.com/83299916/117582531-05ab6b00-b0c8-11eb-9543-e56d34208871.png)

![](Project-2_files/figure-gfm/manual-2.png)<!-- -->

``` r
#getting the corresponding two-sided p-value
mean(mean_diff > -true_diff | mean_diff < true_diff)
```

    ## [1] 0.9548

``` r
#mean(mean_diff > 1.132791 | mean_diff < -1.132791)


#comparing the p-value to a Welch's t-test
t.test(data = USStates, ArrestNumber ~ UrbPopQuartile)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  ArrestNumber by UrbPopQuartile
    ## t = 0.054278, df = 34.78, p-value = 0.957
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -41.24582  43.51140
    ## sample estimates:
    ##      mean in group 50% or less mean in group greater than 50% 
    ##                       67.52222                       66.38943

``` r
#test for difference in means, because of hesitance in equal variance
```

The distributions of arrest numbers across the two categories of the the
percentage of the population that is urban, are not normal. Thus,
violating the normality assumption. The equal variance assumption is met
as the two values are not four-fold one another. However, it is worth
using a Welch’s test later on to be sure of the equal variance
assumption. The mean difference between the two categories is -1.133
arrest per 100,000 residents. This value is our test-statistic. To know
if this mean difference was due to chance or not, a randomization test
was performed without replacement. The randomization was performed 5000
times and represented in a histogram of the distribution of mean
differences. The red line on the histogram represents the original mean
difference (test statistic) of 1.133 arrests per 100,000 residents. The
two-sided p-value that represents this distribution is 0.9548. A Welch’s
t-test was also performed to test for a difference in means accounting
for the hesitance of the equal variance assumption. The Welch’s test
provided a p-value of 0.957, which supports the findings of the
randomization test. Both tests show that we fail to reject the null
hypothesis that mean arrest number is the same for states with 50% or
less of an urban population compared to states with more than 50% of an
urban population (t = 0.054, df = 34.78, p-value &gt; 0.05).

``` r
install.packages("vegan", repos="http://cran.us.r-project.org") #install and mirror package
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/xs/zbzlgf8j76x2lg48c3zfp_ch0000gn/T//RtmpqH7cEY/downloaded_packages

``` r
library(vegan) #vegan package for PERMANOVA function
```

    ## Loading required package: permute

    ## Loading required package: lattice

    ## This is vegan 2.5-7

``` r
#calculating Euclidean distances between the observations
dists <- USStates %>%
  select(AnnualRent,AnnualIncome,ArrestNumber) %>%
  dist

#conducting a PERMANOVA on the distance matrix
adonis(dists ~ UrbPopQuartile, data = USStates) #adonis = function
```

    ## 
    ## Call:
    ## adonis(formula = dists ~ UrbPopQuartile, data = USStates) 
    ## 
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ##                 Df  SumsOfSqs   MeanSqs F.Model      R2 Pr(>F)   
    ## UrbPopQuartile   1  139728103 139728103  8.7553 0.05585  0.004 **
    ## Residuals      148 2361967677  15959241         0.94415          
    ## Total          149 2501695780                   1.00000          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#this p-value is bigger than MANOVA, but still small enough to reject (happens bc we're losing all of these assumptions which makes it harder to reject Ho)
```

As a PERMANOVA test is essentially the randomization test for MANOVA, it
was also performed. This test is useful as it does not hold
distributional assumptions, it allows for differences in
variance/covariance.

Here, we reject the null hypothesis and state that for the numeric
variables of Annual Rent, Annual Income, and Arrest Number, the mean
values will be different between both of the UrbPopQuartile groups for
at least one of these numeric variables (F=8.7553, df=1,
p-value&lt;0.01). This p-value is larger than the MANOVA value, but
still small enough to reject the null hypothesis. This occurs because we
are losing all of these assumptions which makes it harder to reject the
null hypothesis.

------------------------------------------------------------------------

## Linear Regression Model

Below, a linear regression model was used to analyze the effect of
disposable income across arrest types upon the arrest numbers.
Disposable income was utilized as it was a variable formed out of the
combination of the rent and income variables, allowing it to capture the
effects of other variables upon the arrest number as well. For the
purpose of this model, the disposable income variable was mean-centered.
For this linear regression model, the null hypothesis state that there
is no linear relationship between disposable income and arrest type upon
arrest number, and the alternative hypoethis state that disposable
income and arrest type are significant linear predictors of arrest
number.

``` r
#MLR: DisposableIncome and arrest type on arrest number

#mean-centering numeric variables involved in the interaction
USStates$DisposableIncome_c <- USStates$DisposableIncome - mean(USStates$DisposableIncome, na.rm = TRUE) #mean centering disposable income
mean(USStates$DisposableIncome_c, na.rm = TRUE) #mean disposable income
```

    ## [1] -2.908281e-13

``` r
#graph to visualize the interaction between 2 variables on the response.
ggplot(USStates, aes(DisposableIncome_c,ArrestNumber)) + #creating the plot
  geom_point(aes(col=ArrestType)) +  #coloring by arrest type
  geom_smooth(method = 'lm', aes(col=ArrestType)) + #placing the linear regression line
  labs(title = "Arrest Numbers across Disposable Income and Arrest Types", x = "Mean Centered Disposable Income ($)", y = "Arrest Number (per 100,000 residents") #titles
```

    ## `geom_smooth()` using formula 'y ~ x'

![image](https://user-images.githubusercontent.com/83299916/117582542-11972d00-b0c8-11eb-9560-2e39b0068e65.png)

![](Project-2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
fit1_c <- lm(ArrestNumber ~ DisposableIncome_c + ArrestType + DisposableIncome_c*ArrestType, data=USStates) #regression model vector
summary(fit1_c) #regression output
```

    ## 
    ## Call:
    ## lm(formula = ArrestNumber ~ DisposableIncome_c + ArrestType + 
    ##     DisposableIncome_c * ArrestType, data = USStates)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -180.056   -5.979   -0.367    4.987  189.981 
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                          1.708e+02  6.084e+00  28.069  < 2e-16 ***
    ## DisposableIncome_c                  -1.647e-02  2.536e-03  -6.494 1.27e-09 ***
    ## ArrestTypeMurder                    -1.630e+02  8.604e+00 -18.942  < 2e-16 ***
    ## ArrestTypeRape                      -1.495e+02  8.604e+00 -17.380  < 2e-16 ***
    ## DisposableIncome_c:ArrestTypeMurder  1.541e-02  3.587e-03   4.298 3.16e-05 ***
    ## DisposableIncome_c:ArrestTypeRape    1.490e-02  3.587e-03   4.154 5.57e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 43.02 on 144 degrees of freedom
    ## Multiple R-squared:  0.771,  Adjusted R-squared:  0.7631 
    ## F-statistic: 96.99 on 5 and 144 DF,  p-value: < 2.2e-16

``` r
#model explains 76.31% of variation
```

For assault arrests in states with an average disposable income, there
is a predicted number of 170.8 arrests per 100,000 residents (t=28.069,
df-144, p&lt;001).

Disposable income is significantly associated with arrest numbers for
assault arrests in that for every $1 increase in disposable income, the
arrest number decreases by 164.7 arrests per 100,000 residents
(t=-6.494, df=144, p &lt; 0.001).

For states with an average disposable income, there is evidence that
murder arrests have, on average, 163 less arrests per 100,000 residents
compared to assault arrests (t=-18.942, df=144, p&lt;0.001).
Furthermore, for states with an average disposable income, there is also
evidence that rape arrests have, on average, 149.5 less arrests per
100,000 residents compared to assault arrests (t=-17.38, df=144,
p&lt;0.001).

The interaction between average disposable income and arrest type was
found to be statistically significant in that the slope for disposable
income on arrest number is 0.0154 arrests per 100,000/$ less for murders
compared to assaults (t=4.298, df=144, p&lt;0.001). The slope for
disposable income on arrest number is 0.0149 arrests per 100,000/$ less
for rapes compared to assaults (t=4.154, df=144, p&lt;0.001). Looking at
the graph, we can predict that there is not a significant interaction
between murder and rape arrests, as their lines are nearly parallel. But
when comparing either murder and assault, or rape and assault, the
difference in the slopes of the lines support the findings of
interaction from the linear regression model output.

Overall, this model explains 76.31% of the variation in arrest numbers
per 100,000 residents.

``` r
#checking assumptions of linearity, normality, and homoscedasticity using a hypothesis test

USStates %>%   #confirming linear relationship of numerical predictor to response
  ggplot(aes(DisposableIncome_c,ArrestNumber)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) + #placing regression line onto the scatterplot
  labs(title = "Arrest Numbers vs. Disposable Income", x = "Mean Centered Disposable Income ($)", y = "Arrest Number (per 100,000 residents") #titles
```

    ## `geom_smooth()` using formula 'y ~ x'

![image](https://user-images.githubusercontent.com/83299916/117582548-1a87fe80-b0c8-11eb-862d-448fb21e35fc.png)

![](Project-2_files/figure-gfm/assumptions-1.png)<!-- -->

``` r
shapiro.test(fit1_c$residuals) #Shapiro-Wilk test for normality
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  fit1_c$residuals
    ## W = 0.80468, p-value = 7.146e-13

``` r
# H0: data is normally distributed
#p-value of  less than 0.05 which means the Ho of normality is rejected

library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
bptest(fit1_c)# Breusch-Pagan test for homoscedasticity
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  fit1_c
    ## BP = 37.976, df = 5, p-value = 3.814e-07

``` r
# H0: homoscedasticity (data has equal variance)
#reject Ho, so equal variance not present
```

Above, the assumption of linearity was tested graphically, and
hypothesis tests were used to asses the normality and homoscedasticity
of the dataset. Looking at the graph, there appears to be a linear
relationship, however the gathering of the data towards the bottom of
the graph may be a sign of the violation of the linearity assumption.
Furthermore, the p&lt;0.001 of the Shapiro-Wilk normality test allows
for the rejection of the null hypothesis that the data was normally
distributed. The p&lt;0.001 of the Breusch-Pagan test also allows for
the rejection of the null hypothesis of homoscedasticity, meaning that
the data does not have equal variance.

``` r
#not a good model, so we need to check the error models

#normal/unchanged standard errors
summary(fit1_c)$coef
```

    ##                                          Estimate  Std. Error    t value
    ## (Intercept)                          170.76000000 6.083657475  28.068641
    ## DisposableIncome_c                    -0.01646856 0.002536067  -6.493741
    ## ArrestTypeMurder                    -162.97200000 8.603590909 -18.942323
    ## ArrestTypeRape                      -149.52800000 8.603590909 -17.379720
    ## DisposableIncome_c:ArrestTypeMurder    0.01541364 0.003586541   4.297634
    ## DisposableIncome_c:ArrestTypeRape      0.01490013 0.003586541   4.154456
    ##                                         Pr(>|t|)
    ## (Intercept)                         2.931884e-60
    ## DisposableIncome_c                  1.269571e-09
    ## ArrestTypeMurder                    6.237442e-41
    ## ArrestTypeRape                      3.559644e-37
    ## DisposableIncome_c:ArrestTypeMurder 3.161782e-05
    ## DisposableIncome_c:ArrestTypeRape   5.565438e-05

``` r
#this output assumes normality and equal variance

#robust standard errors (corrected SEs)
install.packages("sandwich", repos="http://cran.us.r-project.org") #install and mirror package)
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/xs/zbzlgf8j76x2lg48c3zfp_ch0000gn/T//RtmpqH7cEY/downloaded_packages

``` r
library(sandwich)
coeftest(fit1_c, vcov = vcovHC(fit1_c)) #coeftest function using the regression model defined earlier to calculate variance/covariance matrix with Ω sandwiched in between
```

    ## 
    ## t test of coefficients:
    ## 
    ##                                        Estimate  Std. Error  t value  Pr(>|t|)
    ## (Intercept)                          1.7076e+02  1.0692e+01  15.9703 < 2.2e-16
    ## DisposableIncome_c                  -1.6469e-02  4.7056e-03  -3.4998  0.000620
    ## ArrestTypeMurder                    -1.6297e+02  1.0705e+01 -15.2244 < 2.2e-16
    ## ArrestTypeRape                      -1.4953e+02  1.0764e+01 -13.8914 < 2.2e-16
    ## DisposableIncome_c:ArrestTypeMurder  1.5414e-02  4.7104e-03   3.2723  0.001336
    ## DisposableIncome_c:ArrestTypeRape    1.4900e-02  4.7263e-03   3.1526  0.001969
    ##                                        
    ## (Intercept)                         ***
    ## DisposableIncome_c                  ***
    ## ArrestTypeMurder                    ***
    ## ArrestTypeRape                      ***
    ## DisposableIncome_c:ArrestTypeMurder ** 
    ## DisposableIncome_c:ArrestTypeRape   ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#we didn't meet the assumptions, we need to be more conservative about the standard error, less likely to reject Ho
```

Due to the violation of linearity, normality, and homoscedasticity, the
original linear regression is not a good model, so we need to check the
error models using robust standard errors. Comparing to the previous
model, none of the significant effects have changed. However, because we
didn’t meet the assumptions, we need to be more conservative about the
standard error, and are less likely to reject the null hypothesis that
the disposable income and arrest type are not significant linear
predictors of arrest number. This is evident in the p-values of the two
interaction outputs, as the p-values have slightly increased compared to
the original model. This regression output shows larger standard errors
which means that if we are trying to build a confidence interval for
estimates, the interval would be a wider for our coefficients, thus
accounting for the violation of assumptions by being more conservative
in rejecting the null hypothesis.

For assault arrests in states with an average disposable income, there
is a predicted number of 170.8 arrests per 100,000 residents (t=15.9703,
df-144, p&lt;001).

Disposable income is significantly associated with arrest numbers for
assault arrests in that for every $1 increase in disposable income, the
arrest number decreases by 164.7 arrests per 100,000 residents (t=-3.50,
df=144, p &lt; 0.001).

For states with an average disposable income, there is evidence that
murder arrests have, on average, 162.97 less arrests per 100,000
residents compared to assault arrests (t=-15.224, df=144, p&lt;0.001).
Furthermore, for states with an average disposable income, there is also
evidence that rape arrests have, on average, 149.5 less arrests per
100,000 residents compared to assault arrests (t=-13.89, df=144,
p&lt;0.001).

The interaction between average disposable income and arrest type was
found to be statistically significant in that the slope for disposable
income on arrest number is 0.0154 arrests per 100,000/$ less for murders
compared to assaults (t=3.2723, df=144, p&lt;0.01).The slope for
disposable income on arrest number is 0.0149 arrests per 100,000/$ less
for rapes compared to assaults (t=3.1526, df=144, p&lt;0.01).

``` r
set.seed(348)

#assumptions are violated so we use bootstrap samples to estimate coefficients, SEs, fitted values, etc.

#bootstrapping from observations
#using the "replicate" function to repeat the process (similar to a for loop)
samp_SEs <- replicate(5000, {   #bootstrapping 5000 times, saving the coefficients each time
  boot_data <- sample_frac(USStates, replace = TRUE) #bootstrap/resample, but sample_frac replaces only a sample, but bc we want to replace everything, we don't clarify a fraction
  fitboot <- lm(ArrestNumber ~ DisposableIncome_c + ArrestType + DisposableIncome_c*ArrestType, data = boot_data)   #fitting the regression model
  coef(fitboot)    #saving the coefficients for each bootstrap sample
})

#bootstrapped estimated SEs
samp_SEs %>%
  t %>%  #transposing the matrices
  as.data.frame %>%  # making the matrix as a data frame
  summarize_all(sd)  #calculating the standard error 
```

    ##   (Intercept) DisposableIncome_c ArrestTypeMurder ArrestTypeRape
    ## 1    10.31524        0.004657915         10.33238       10.39926
    ##   DisposableIncome_c:ArrestTypeMurder DisposableIncome_c:ArrestTypeRape
    ## 1                          0.00466211                       0.004674495

``` r
coeftest(fit1_c)[,1:2] #comparison with uncorrected SEs
```

    ##                                          Estimate  Std. Error
    ## (Intercept)                          170.76000000 6.083657475
    ## DisposableIncome_c                    -0.01646856 0.002536067
    ## ArrestTypeMurder                    -162.97200000 8.603590909
    ## ArrestTypeRape                      -149.52800000 8.603590909
    ## DisposableIncome_c:ArrestTypeMurder    0.01541364 0.003586541
    ## DisposableIncome_c:ArrestTypeRape      0.01490013 0.003586541

``` r
coeftest(fit1_c, vcov = vcovHC(fit1_c))[,1:2]#comparison with robust SEs
```

    ##                                          Estimate   Std. Error
    ## (Intercept)                          170.76000000 10.692315707
    ## DisposableIncome_c                    -0.01646856  0.004705565
    ## ArrestTypeMurder                    -162.97200000 10.704666315
    ## ArrestTypeRape                      -149.52800000 10.764106374
    ## DisposableIncome_c:ArrestTypeMurder    0.01541364  0.004710397
    ## DisposableIncome_c:ArrestTypeRape      0.01490013  0.004726258

By bootstrapping, we will categorize our sample as the population, and
resample with replacement from this population. Thus, allowing us to
find a sampling distribution for regression estimates. Bootstrapping is
crucial as it doesn’t require the meeting of assumptions. The standard
errors of the normal-theory regression model are the smallest values,
followed by the bootstrapped standards errors, and with the largest
being the robust standard errors. This means that as the standard errors
increase between normal-theory to bootstrap to robust, the p-values also
increase, making it harder to reject Ho due to the violation of
assumptions that leads to less precision.

------------------------------------------------------------------------

## Logistic Regression

For this logistic regression model, the null hypothesis states that
there is no relationship between annual rent and annual income upon of
UrbPopQuartile, and the alternative hypoethis states that annual rent
and annual income are significant predictors of UrbPopQuartile.

``` r
#making UrbPopQuartile a binary variable coded as 0 and 1
USStates <- USStates %>%
  mutate(y = ifelse(UrbPopQuartile == "50% or less", 1,0))

fit3 <- glm(y ~ AnnualRent + AnnualIncome, data = USStates, family = "binomial") #logistic fit
summary(fit3) #output
```

    ## 
    ## Call:
    ## glm(formula = y ~ AnnualRent + AnnualIncome, family = "binomial", 
    ##     data = USStates)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.9404  -0.7497  -0.5129  -0.2400   2.5447  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)   3.936e+00  2.165e+00   1.818   0.0690 .
    ## AnnualRent   -3.687e-04  1.797e-04  -2.052   0.0402 *
    ## AnnualIncome -5.541e-05  9.697e-05  -0.571   0.5677  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 141.42  on 149  degrees of freedom
    ## Residual deviance: 128.42  on 147  degrees of freedom
    ## AIC: 134.42
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
exp(coef(fit3)) #describing the coefficients by considering the odds (inverse of log(odds))
```

    ##  (Intercept)   AnnualRent AnnualIncome 
    ##   51.2234797    0.9996314    0.9999446

Controlling for annual income and annual rent, the log odds of the urban
population percentage being 50% or less is 3.936 (z=1.818, df=149,
p&gt;0.05).

Controlling for annual income, for every dollar increase in annual rent,
the log odds of urban population being 50% or less of the total
population decrease by 0.00037 (z=-2.05, df=149, p&lt;0.05). Controlling
for annual rent, for every dollar increase in the annual income, the log
odds of the urban population comprising 50% or less of the total state
population decreases by 0.000055 (z=-0.057, df=149, p&gt;0.05).

Every dollar increase in annual rent multiplies the odds of the urban
population being 50% or less by 0.9996. Every dollar increase in annual
income multiplies the odds of the urban population being 50% or less by
0.9999.

``` r
USStates$prob <- predict(fit3, type = "response") #adding predicted probabilities to dataset
USStates$predicted <- ifelse(USStates$prob > .2, "50% or less","greater than 50%")  #setting predicted outcome on probability of states being 50% or less urban

table(truth = USStates$UrbPopQuartile, prediction = USStates$predicted) %>% addmargins #getting confusion matrix with sum margins
```

    ##                   prediction
    ## truth              50% or less greater than 50% Sum
    ##   50% or less               21                6  27
    ##   greater than 50%          51               72 123
    ##   Sum                       72               78 150

``` r
(21 + 72)/150 #accuracy (correctly classified cases)
```

    ## [1] 0.62

``` r
21/(21+6) #sensitivity (True Positive Rate, TPR)
```

    ## [1] 0.7777778

``` r
72/(72+51) #specificity (True Negative Rate, TNR)
```

    ## [1] 0.5853659

``` r
21/(21+51) #precision/recall (Positive Predictive Value, PPV)
```

    ## [1] 0.2916667

The purpose of the confusion matrix is to compare the true outcomes to
the predicted outcomes. For the predicted probabilities, a probabilities
of greater than 0.2 was used to classify the “50% or less” of an urban
population outcome, because the cutoff of 0.5 was resulting in an error.
Using the confusion matrix, an accuracy, or the rate of correctly
identified cases was found to be 0.62. The sensitivity, or the true
positive rate, was 0.778. The specificity, or the true negative rate was
0.585. The precision or recall, which is also known as the positive
predictive value, was found to be 0.292. Due to the cutoff being at 0.2,
the classification of a positive case is more liberal. Thus, resulting
in more false positives, but also more truly positive cases. This is
essentially a tradeoff, in that the true positive rate is higher than
under other cutoffs, but the false positive rate also happens to be
greater than usual as well.

``` r
USStates$logit <- predict(fit3) #saving log-odds in the dataset

#making density plot of log-odds for each outcome
USStates %>%
  ggplot() + 
  geom_density(aes(logit, color = UrbPopQuartile, fill = UrbPopQuartile), alpha = .4) + #density plot and color by UrbPopQuartile
  theme(legend.position = c(.25,.85)) + #where the legend is placed
  geom_vline(xintercept = 0,lty = 2) +  #dashed vertical line
  xlab("logit (log-odds)") + #axis title
  ylab("Density") #axis title
```
![image](https://user-images.githubusercontent.com/83299916/117582563-2b387480-b0c8-11eb-95c4-20ec73e55e60.png)

![](Project-2_files/figure-gfm/plot%20of%20log%20odds-1.png)<!-- -->

Above, we use a density plot to display the misidentification of the
outcomes based on the probabilities set in the prior confusion matrix
section. Here, the x-intercept is at 0 because a probability of 0.5 or
greater equates to a 50% or less urban population, where the odds of the
probability being 0.5 equals 1 and the log of 1 is 0. In this plot, the
density of a 50% or less urban population is shown in pink, while the
blue shows the density of the greater than 50% urban populations. The
misclassified values are shown in the grey area.

``` r
library(plotROC) #uploading plotROC library

ROCplot1 <- ggplot(USStates) + 
  geom_roc(aes(d = y, m = prob), cutoffs.at = list(0.2, 0.3, 0.5))
ROCplot1 #getting the ROC plot to be based on y values and probabilities along with 3 cutoffs
```
![image](https://user-images.githubusercontent.com/83299916/117582570-325f8280-b0c8-11eb-92fc-82b5c1881ddc.png)

![](Project-2_files/figure-gfm/ROC%20curve-1.png)<!-- -->

``` r
calc_auc(ROCplot1) #using calc_auc function from plotROC library for AUC
```

    ##   PANEL group       AUC
    ## 1     1    -1 0.7073171

The process of classification helps us to find a good cutoff value upon
which the predicted probabilities of an urban population being 50% or
less are based. The ROC aids in the assessment the specificity and
sensitivity. In terms of the true positive rate, this ROC curve is
rather average.

The AUC is the probability that a randomly selected state with an urban
population of 50% or less has a higher predicted probability than a
randomly selected state with an urban population that is greater than
50%. On average, 70.73% of the time states with an urban population of
50% or less will have higher probabilities than states with an urban
population that is greater than 50%. Using our rule of thumb, we have an
okay, or average AUC percentage.

------------------------------------------------------------------------

## Conclusion

The combination of U.S. States’ income, rent, crime rate, and urban
population data allowed for incredible analysis of patterns that exist
across the variables in the *USStates* dataset. Several crucial findings
came to light through this study. From the MANOVA test, which was
supported by the findings in the PERMANOVA, it was found that for the
numeric variables of Annual Rent, Annual Income, and Arrest Number, the
mean values will be different between both of the UrbPopQuartile groups
for the Arrest Number variables, specifically for the assaults. The
randomization test showed that mean arrest number is the same for states
with 50% or less of an urban population compared to states with more
than 50% of an urban population. The linear regression also supported
the idea that the disposable income and arrest type were significant
linear predictors of the arrest number. The logistic model also gave
insight into the significance of the annual rent and annual income upon
predicting the UrbPopQuartile. The ROC and AUC analyses led to the
conclusion that in terms of the logistic model, annual rent and annual
income were average predictors of the UrbPopQuartile.
