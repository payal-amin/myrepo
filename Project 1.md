Study of US States’ Arrest Rates through the Lens of Rent and Income
================

### Introduction

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

At first glance, it is expected that there will be an association
between the income and rent of a state, being that greater incomes will
indicate greater rent values. It is also proposed that states with
larger urban population percentages will have higher overall crime rates
in comparison to less urban states. In order to study the data further,
the original datasets were tailored and combined in order to offer more
insight into the potential associations.

------------------------------------------------------------------------

### Tidying of Datasets

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

------------------------------------------------------------------------

### Joining the Datasets & Resultant Tidying

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

------------------------------------------------------------------------

### Data Exploration & Summary Statistics

``` r
USStates <- USStates %>%  #making changes to the main combined dataset using a dplyr vector
  mutate(UrbPopQuartile = case_when(UrbanPop<25 ~ "Less than 25%", 25<=UrbanPop & UrbanPop<=50 ~"between 25-50%", 50<UrbanPop & UrbanPop<=75 ~ "greater than 50%, less than 75%", 75<UrbanPop ~ "greater than 75%"))   #using mutate function to create a new, categorical variable
```

As seen above, the *mutate* function from dplyr was used to create a new
categorical variable called “UrbPopQuartile”. This variable contains
four different observations, each corresponding to the proportion of a
state’s population that is urban. If a state has an urban population
below 25%, that state would have a corresponding UrbPopQuantile value of
“Less than 25%”. If the urban population is 25% or greater, and 50% or
less, then it would receive an UrbPopQuantile value of “between 25-50%”,
and so on. This variable essentially sorts all U.S. states into each of
the 4 quartiles of urban population percentage.

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
head(USStates)
```

    ## # A tibble: 6 x 9
    ##   State   MonthlyRent AnnualRent AnnualIncome DisposableIncome UrbanPop
    ##   <chr>         <dbl>      <dbl>        <dbl>            <dbl>    <int>
    ## 1 Alabama         747       8964        24476            15512       58
    ## 2 Alabama         747       8964        24476            15512       58
    ## 3 Alabama         747       8964        24476            15512       58
    ## 4 Alaska         1200      14400        32940            18540       48
    ## 5 Alaska         1200      14400        32940            18540       48
    ## 6 Alaska         1200      14400        32940            18540       48
    ## # … with 3 more variables: UrbPopQuartile <chr>, ArrestType <chr>,
    ## #   ArrestNumber <dbl>

Due to addition of multiple variables, the code above shows how the
USStates dataset was organized by reordering the columns.

``` r
USStates %>% 
  filter(State=="Texas") %>% #choose for a specific observation/row (State--> Texas)
  select(State,ArrestType,ArrestNumber) #choose which variables to show in the output
```

    ## # A tibble: 3 x 3
    ##   State ArrestType ArrestNumber
    ##   <chr> <chr>             <dbl>
    ## 1 Texas Murder             12.7
    ## 2 Texas Assault           201  
    ## 3 Texas Rape               25.5

As part of exploring the dataset, a *filter* function was used to pull
the data for a specific U.S. state; Texas. The reason for the
exploration was to study the level of violent crimes as an inhabitant of
this state. After zooming in on Texas, the *select* function was used to
choose which variables would be visible in the output. The output above
shows that Texas has a murder arrest rate of 12.7 per 100,000. It has an
assault arrest rate of 201 per 100,000 and a rape arrest rate of 25.5
per 100,000.

``` r
USStates %>% 
  group_by(UrbPopQuartile) %>% #group data using a categorical variable
  summarize_all(n_distinct) %>% #find the numerical count of each category of the variable
  select(UrbPopQuartile,State) #choose which variables to show in the output
```

    ## # A tibble: 3 x 2
    ##   UrbPopQuartile                  State
    ##   <chr>                           <int>
    ## 1 between 25-50%                      9
    ## 2 greater than 50%, less than 75%    27
    ## 3 greater than 75%                   14

Using the *group\_by* function, we can study the 4 quartile levels of
urban population more thoroughly. After grouping the states based on
which of the four quartiles their urban populations fall into, the
*summarize\_all* function was used to count how many unique state names
were in each of the four categories. The *select* function denoted which
variables would appear in the output. It was found that there are no
states with an urban population less than 25%. There are 9 states with
an urban population between 25%-50%. Twenty-seven states have an urban
population that is greater than 50% but less than 75% and there are 14
states with an urban population greater than 75%. Therefore, majority of
the states have an urban population greater than 50% but less than 75%,
followed by greater than 75%, with the least amount of states having
between 25-50% of an urban population.

``` r
USStates %>%
  filter(ArrestType=='Murder') %>% #choose specific type of arrest
  arrange(desc(ArrestNumber)) %>% #sort states by highest to lowest murder arrest rates
  select(State,ArrestType,ArrestNumber,UrbPopQuartile) %>% #choose which variables to show in the output
  slice(1:3) #choose top 3 states with highest murder arrest rates
```

    ## # A tibble: 3 x 4
    ##   State       ArrestType ArrestNumber UrbPopQuartile                 
    ##   <chr>       <chr>             <dbl> <chr>                          
    ## 1 Georgia     Murder             17.4 greater than 50%, less than 75%
    ## 2 Mississippi Murder             16.1 between 25-50%                 
    ## 3 Florida     Murder             15.4 greater than 75%

``` r
USStates %>%
  filter(ArrestType=='Assault') %>% #choose specific type of arrest
  arrange(desc(ArrestNumber)) %>% #sort states by highest to lowest assault arrest rates
  select(State,ArrestType,ArrestNumber,UrbPopQuartile) %>% #choose which variables to show in the output
  slice(1:3) #choose top 3 states with highest assault arrest rates
```

    ## # A tibble: 3 x 4
    ##   State          ArrestType ArrestNumber UrbPopQuartile                 
    ##   <chr>          <chr>             <dbl> <chr>                          
    ## 1 North Carolina Assault             337 between 25-50%                 
    ## 2 Florida        Assault             335 greater than 75%               
    ## 3 Maryland       Assault             300 greater than 50%, less than 75%

``` r
USStates %>%
  filter(ArrestType=='Rape') %>% #choose specific type of arrest
  arrange(desc(ArrestNumber)) %>% #sort states by highest to lowest rape arrest rates
  select(State,ArrestType,ArrestNumber,UrbPopQuartile) %>% #choose which variables to show in the output
  slice(1:3) #choose top 3 states with highest rape arrest rates
```

    ## # A tibble: 3 x 4
    ##   State      ArrestType ArrestNumber UrbPopQuartile  
    ##   <chr>      <chr>             <dbl> <chr>           
    ## 1 Nevada     Rape               46   greater than 75%
    ## 2 Alaska     Rape               44.5 between 25-50%  
    ## 3 California Rape               40.6 greater than 75%

The top 3 states with the greatest murder arrest rates (starting from
the greatest) are Georgia, Mississippi, and then Florida. Despite
sharing the similarity of high murder arrest rates, all three states
have urban population percentages in different quartiles. The three
states with the greatest assault arrest rates (starting from the
greatest) are North Carolina, Florida, and Maryland. Similar to the
murder arrests, despite these three states sharing the similarity of
high assault arrest rates, all three have urban population percentages
in different quartiles. The top 3 states with the greatest rape arrest
rates (starting from the greatest) are Nevada, Alaska, and California.
Both Nevada and California have urban populations greater than 75%,
whereas Alaska has an urban population between 25%-50%. Across all three
arrest types, the three greatest arrest numbers are a unique combination
of states.

``` r
install.packages("qwraps2", repos="http://cran.us.r-project.org") #install and mirror "qwraps2" package
```

The downloaded binary packages are in
/var/folders/xs/zbzlgf8j76x2lg48c3zfp\_ch0000gn/T//RtmpQVIxqb/downloaded\_packages

``` r
library(qwraps2)  #uploading qwraps2 for table-making
options(qwraps2_markup = "markdown") #make qwraps suitable for R markdown
summary1 <- list("Monthly Rent (in USD)" =              #creating a vector to store summary statistics for ungrouped numeric variables
       list("min"       = ~ min(USStates$MonthlyRent),   #min,med,max,mean,sd for Monthly Rent
            "median"    = ~ median(USStates$MonthlyRent),
            "max"       = ~ max(USStates$MonthlyRent),
            "mean (sd)" = ~ qwraps2::mean_sd(USStates$MonthlyRent)),
       "Annual Rent (in USD)" =                          #min,med,max,mean,sd for Annual Rent
       list("min"       = ~ min(USStates$AnnualRent),
            "median"    = ~ median(USStates$AnnualRent),
            "max"       = ~ max(USStates$AnnualRent),
            "mean (sd)" = ~ qwraps2::mean_sd(USStates$AnnualRent)),
       "Annual Income (in USD)" =                    #min,med,max,mean,sd for Annual Income
       list("min"       = ~ min(USStates$AnnualIncome),
            "median"    = ~ median(USStates$AnnualIncome),
            "max"       = ~ max(USStates$AnnualIncome),
            "mean (sd)" = ~ qwraps2::mean_sd(USStates$AnnualIncome)),
       "Disposable Income (in USD)" =               #min,med,max,mean,sd for Disposable Income
       list("min"       = ~ min(USStates$DisposableIncome),
            "median"    = ~ median(USStates$DisposableIncome),
            "max"       = ~ max(USStates$DisposableIncome),
            "mean (sd)" = ~ qwraps2::mean_sd(USStates$DisposableIncome)),
       "Urban Population Percentage (of Total Population)" =        #min,med,max,mean,sd for Urban Population Percentage
       list("min"       = ~ min(USStates$UrbanPop),
            "median"    = ~ median(USStates$UrbanPop),
            "max"       = ~ max(USStates$UrbanPop),
            "mean (sd)" = ~ qwraps2::mean_sd(USStates$UrbanPop)),
       "Arrest Numbers (per 100,000)" =            #mean and sd values for each of the 3 arrest types
       list("Murder, (mean, sd)" = ~ qwraps2::mean_sd(USStates[USStates$ArrestType=='Murder',]$ArrestNumber),
            "Assault, (mean, sd)"  = ~ qwraps2::mean_sd(USStates[USStates$ArrestType=='Assault',]$ArrestNumber),
            "Rape, (mean, sd)"  = ~ qwraps2::mean_sd(USStates[USStates$ArrestType=='Rape',]$ArrestNumber)))

print(qwraps2::summary_table(USStates, summary1), rtitle = "Summary Statistics for US States") #using qwraps to make a table of summary statistics according to the "lists" made above, using the summary1 vector
```

| Summary Statistics for US States                      | USStates (N = 150)   |
|:------------------------------------------------------|:---------------------|
| **Monthly Rent (in USD)**                             |                      |
|    min                                                | 681                  |
|    median                                             | 864.5                |
|    max                                                | 1507                 |
|    mean (sd)                                          | 931.78 ± 192.81      |
| **Annual Rent (in USD)**                              |                      |
|    min                                                | 8172                 |
|    median                                             | 10374                |
|    max                                                | 18084                |
|    mean (sd)                                          | 11,181.36 ± 2,313.72 |
| **Annual Income (in USD)**                            |                      |
|    min                                                | 22766                |
|    median                                             | 28872                |
|    max                                                | 37147                |
|    mean (sd)                                          | 28,908.04 ± 3,380.65 |
| **Disposable Income (in USD)**                        |                      |
|    min                                                | 13028                |
|    median                                             | 17414                |
|    max                                                | 23036                |
|    mean (sd)                                          | 17,726.68 ± 2,406.89 |
| **Urban Population Percentage (of Total Population)** |                      |
|    min                                                | 32                   |
|    median                                             | 66                   |
|    max                                                | 91                   |
|    mean (sd)                                          | 65.54 ± 14.38        |
| **Arrest Numbers (per 100,000)**                      |                      |
|    Murder, (mean, sd)                                 | 7.79 ± 4.36          |
|    Assault, (mean, sd)                                | 170.76 ± 83.34       |
|    Rape, (mean, sd)                                   | 21.23 ± 9.37         |

Now we can study the summary statistics for the numeric variables of the
*USStates* dataset. For the monthly rent, the minimum rent was $681, the
median rent was $864.5 ,the maximum rent was $1507, and the mean monthly
rent was $931.78 ± 192.81. For the annual rent, the minimum rent was
$8172, the median rent was $10374 ,the maximum rent was $18084, and the
mean annual rent was $11,181.36 ± 2,313.72. Looking at the annual
income, the minimum income was $22,766, the median income was $28,872
,the maximum income was $37,147, and the mean annual income was
$28,908.04 ± 3,380.65. For the disposable income, the minimum income was
$13,028, the median income was $17,414,the maximum income was $23,036,
and the mean disposable income was $17,726.68 ± 2,406.89. As for the
urban population percentage, the minimum percentage was 32%, the median
percentage was 66%,the maximum percentage was 91%, and the mean
percentage was 65.54% ± 14.38. Looking at the arrest numbers, the mean
number of murder arrests was 7.79 ± 4.36 (per 100,000), the mean number
of assault arrests was 170.76 ± 83.34 (per 100,000), and the mean number
of rape arrests was 21.23 ± 9.37 (per 100,000).

``` r
library(knitr) #loading knitr library
options(knitr.table.format = "html") #make knitr suitable for R markdown

se = function(x) {        #creating standard error function for below summary statistics
  data = na.omit(x)
  sd(data) / sqrt(length(data)) 
}

#monthly rent summary statistics by creating a dplyr vector and using kable function for table
mr <- USStates %>%
  group_by(UrbPopQuartile) %>% 
  summarise(ct=n()/3,mn=mean(MonthlyRent),md=median(MonthlyRent),min=min(MonthlyRent),max=max(MonthlyRent),range=max-min,sd=sd(MonthlyRent),se=se(MonthlyRent))
colnames(mr) =  c('UrbPopQuartile','States Count','Mean','Median','Min','Max','Range','Std Dev','Std Err')
kable(mr, format='markdown', digits=2, caption = "Summary Statistics of Monthly Rent by UrbPopQuartile") #use kable and create table using the vector created above
```

| UrbPopQuartile                  | States Count |    Mean | Median | Min |  Max | Range | Std Dev | Std Err |
|:--------------------------------|-------------:|--------:|-------:|----:|-----:|------:|--------:|--------:|
| between 25-50%                  |            9 |  825.11 |    775 | 681 | 1200 |   519 |  157.89 |   30.39 |
| greater than 50%, less than 75% |           27 |  872.56 |    809 | 713 | 1311 |   598 |  148.43 |   16.49 |
| greater than 75%                |           14 | 1114.57 |   1100 | 948 | 1507 |   559 |  166.29 |   25.66 |

Summary Statistics of Monthly Rent by UrbPopQuartile

Using the *knitr* library, summary statistics were calculated by
grouping the numeric variable by the UrbPopQuartile variable. For the 9
states with an UrbPopQuartile between 25-50%, the minimum monthly rent
was $681, the median rent was $775 ,the maximum rent was $1200, and the
mean monthly rent was $825.11 ± 157.89, with a standard error of $30.39.
For the 27 states with an UrbPopQuartile greater than 50%, but less than
75%, the minimum rent was $713, the median rent was $809 ,the maximum
rent was $1311, and the mean monthly rent was $872.56 ± 148.43, with a
standard error of $16.49. As for the 14 states with an UrbPopQuartile
greater than 75%, the minimum rent was $948, the median rent was $1100
,the maximum rent was $1507, and the mean monthly rent was $1114.57 ±
166.29, with a standard error of $25.66.

``` r
#annual rent summary statistics by creating a dplyr vector and using kable function for table
ar <- USStates %>%
  group_by(UrbPopQuartile) %>% 
  summarise(ct=n()/3,mn=mean(AnnualRent),md=median(AnnualRent),min=min(AnnualRent),max=max(AnnualRent),range=max-min,sd=sd(AnnualRent),se=se(AnnualRent))
colnames(ar) =  c('UrbPopQuartile','States Count','Mean','Median','Min','Max','Range','Std Dev','Std Err')
kable(ar, format='markdown', digits=2, caption = "Summary Statistics of Annual Rent by UrbPopQuartile") #use kable and create table using the vector created above
```

| UrbPopQuartile                  | States Count |     Mean | Median |   Min |   Max | Range | Std Dev | Std Err |
|:--------------------------------|-------------:|---------:|-------:|------:|------:|------:|--------:|--------:|
| between 25-50%                  |            9 |  9901.33 |   9300 |  8172 | 14400 |  6228 | 1894.73 |  364.64 |
| greater than 50%, less than 75% |           27 | 10470.67 |   9708 |  8556 | 15732 |  7176 | 1781.19 |  197.91 |
| greater than 75%                |           14 | 13374.86 |  13200 | 11376 | 18084 |  6708 | 1995.50 |  307.91 |

Summary Statistics of Annual Rent by UrbPopQuartile

For the 9 states with an UrbPopQuartile between 25-50%, the minimum
annual rent was $8172, the median annual rent was $9300 ,the maximum
rent was $14400, and the mean annual rent was $9901.33 ± 1894.73, with a
standard error of $364.64. For the 27 states with an UrbPopQuartile
greater than 50%, but less than 75%, the minimum annual rent was $8556,
the median rent was $9708 ,the maximum rent was $15732, and the mean
annual rent was $10470.67 ± 1781.19, with a standard error of $197.91.
As for the 14 states with an UrbPopQuartile greater than 75%, the
minimum annual rent was $11376, the median rent was $13200 ,the maximum
rent was $18084, and the mean annual rent was $13374.86 ± 1995.50, with
a standard error of $307.91.

``` r
#annual income summary statistics by creating a dplyr vector and using kable function for table
ai <- USStates %>%
  group_by(UrbPopQuartile) %>% 
  summarise(ct=n()/3,mn=mean(AnnualIncome),md=median(AnnualIncome),min=min(AnnualIncome),max=max(AnnualIncome),range=max-min,sd=sd(AnnualIncome),se=se(AnnualIncome))
colnames(ai) =  c('UrbPopQuartile','States Count','Mean','Median','Min','Max','Range','Std Dev','Std Err')
kable(ai, format='markdown', digits=2, caption = "Summary Statistics of Annual Income by UrbPopQuartile") #use kable and create table using the vector created above
```

| UrbPopQuartile                  | States Count |     Mean | Median |   Min |   Max | Range | Std Dev | Std Err |
|:--------------------------------|-------------:|---------:|-------:|------:|------:|------:|--------:|--------:|
| between 25-50%                  |            9 | 27294.00 |  26482 | 22766 | 32940 | 10174 | 3619.84 |  696.64 |
| greater than 50%, less than 75% |           27 | 28522.93 |  27389 | 24457 | 37147 | 12690 | 3177.59 |  353.07 |
| greater than 75%                |           14 | 30688.36 |  30447 | 25952 | 35326 |  9374 | 2875.32 |  443.67 |

Summary Statistics of Annual Income by UrbPopQuartile

For the 9 states with an UrbPopQuartile between 25-50%, the minimum
annual income was $22,766, the median annual income was $26,482 ,the
maximum income was $32,940, and the mean annual income was $27,294.00 ±
3,619.84, with a standard error of $696.64. For the 27 states with an
UrbPopQuartile greater than 50%, but less than 75%, the minimum annual
income was $24,457, the median income was $27,389 ,the maximum income
was $37,147, and the mean annual income was $28,522.93 ± 3,177.59, with
a standard error of $353.07. As for the 14 states with an UrbPopQuartile
greater than 75%, the minimum annual income was $25,952, the median
income was $30,447 ,the maximum income was $35,326, and the mean annual
income was $30,688.36 ± 2,875.32, with a standard error of $443.67.

``` r
#disposable income summary statistics by creating a dplyr vector and using kable function for table
di <- USStates %>%
  group_by(UrbPopQuartile) %>% 
  summarise(ct=n()/3,mn=mean(DisposableIncome),md=median(DisposableIncome),min=min(DisposableIncome),max=max(DisposableIncome),range=max-min,sd=sd(DisposableIncome),se=se(DisposableIncome))
colnames(di) =  c('UrbPopQuartile','States Count','Mean','Median','Min','Max','Range','Std Dev','Std Err')
kable(di, format='markdown', digits=2, caption = "Summary Statistics of Disposable Income by UrbPopQuartile") #use kable and create table using the vector created above
```

| UrbPopQuartile                  | States Count |     Mean | Median |   Min |   Max | Range | Std Dev | Std Err |
|:--------------------------------|-------------:|---------:|-------:|------:|------:|------:|--------:|--------:|
| between 25-50%                  |            9 | 17392.67 |  16354 | 13886 | 23036 |  9150 | 2804.78 |  539.78 |
| greater than 50%, less than 75% |           27 | 18052.26 |  17863 | 14749 | 21862 |  7113 | 2112.37 |  234.71 |
| greater than 75%                |           14 | 17313.50 |  16772 | 13028 | 21850 |  8822 | 2626.80 |  405.32 |

Summary Statistics of Disposable Income by UrbPopQuartile

For the 9 states with an UrbPopQuartile between 25-50%, the minimum
disposable income was $13,886, the median disposable income was $16,354
,the maximum income was $23,036, and the mean disposable income was
$17,392.67 ± 2,804.78, with a standard error of $539.78. For the 27
states with an UrbPopQuartile greater than 50%, but less than 75%, the
minimum disposable income was $14,749, the median income was $17,863
,the maximum income was $21,862, and the mean disposable income was
$18,052.26 ± 2,112.37, with a standard error of $234.71. As for the 14
states with an UrbPopQuartile greater than 75%, the minimum disposable
income was $13028, the median income was $16,772 ,the maximum income was
$21,850, and the mean disposable income was $17,313.50 ± 2,626.80, with
a standard error of $405.32.

``` r
#urban population summary statistics by creating a dplyr vector and using kable function for table
up <- USStates %>%
  group_by(UrbPopQuartile) %>% 
  summarise(ct=n()/3,mn=mean(UrbanPop),md=median(UrbanPop),min=min(UrbanPop),max=max(UrbanPop),range=max-min,sd=sd(UrbanPop),se=se(UrbanPop))
colnames(up) =  c('UrbPopQuartile','States Count','Mean','Median','Min','Max','Range','Std Dev','Std Err')
kable(up, format='markdown', digits=2, caption = "Summary Statistics of Urban Population Percentage by UrbPopQuartile") #use kable and create table using the vector created above
```

| UrbPopQuartile                  | States Count |  Mean | Median | Min | Max | Range | Std Dev | Std Err |
|:--------------------------------|-------------:|------:|-------:|----:|----:|------:|--------:|--------:|
| between 25-50%                  |            9 | 43.89 |     45 |  32 |  50 |    18 |    5.25 |    1.01 |
| greater than 50%, less than 75% |           27 | 63.78 |     66 |  51 |  75 |    24 |    6.97 |    0.77 |
| greater than 75%                |           14 | 82.86 |     82 |  77 |  91 |    14 |    4.10 |    0.63 |

Summary Statistics of Urban Population Percentage by UrbPopQuartile

For the 9 states with an UrbPopQuartile between 25-50%, the minimum
urban population percentage was 32%, the median urban percentage was
45%, the maximum urban percentage was 50%, and the mean urban population
percentage was 43.89% ± 5.25, with a standard error of 1.01%. For the 27
states with an UrbPopQuartile greater than 50%, but less than 75%, the
minimum urban population percentage was 51%, the median urban percentage
was 66%,the maximum urban percentage was 75%, and the mean urban
population percentage was 63.78% ± 6.97, with a standard error of 0.77%.
As for the 14 states with an UrbPopQuartile greater than 75%, the
minimum urban population percentage was 77%, the median urban percentage
was 82, the maximum urban percentage was 91% and the mean urban
population percentage was 82.86% ± 4.10, with a standard error of 0.63%.

``` r
#arrest numbers summary statistics by creating a dplyr vector and using kable function for table
an <- USStates %>%
  group_by(UrbPopQuartile) %>% 
  summarise(ct=n()/3,mn=mean(ArrestNumber),md=median(ArrestNumber),min=min(ArrestNumber),max=max(ArrestNumber),range=max-min,sd=sd(ArrestNumber),se=se(ArrestNumber))
colnames(an) =  c('UrbPopQuartile','States Count','Mean','Median','Min','Max','Range','Std Dev','Std Err')
kable(an, format='markdown', digits=2, caption = "Summary Statistics of Arrest Numbers by UrbPopQuartile") #use kable and create table using the vector created above
```

| UrbPopQuartile                  | States Count |  Mean | Median | Min | Max | Range | Std Dev | Std Err |
|:--------------------------------|-------------:|------:|-------:|----:|----:|------:|--------:|--------:|
| between 25-50%                  |            9 | 67.52 |  16.10 | 0.8 | 337 | 336.2 |  100.70 |   19.38 |
| greater than 50%, less than 75% |           27 | 60.10 |  20.00 | 2.1 | 300 | 297.9 |   77.82 |    8.65 |
| greater than 75%                |           14 | 78.53 |  24.75 | 3.2 | 335 | 331.8 |   99.54 |   15.36 |

Summary Statistics of Arrest Numbers by UrbPopQuartile

------------------------------------------------------------------------

### 4. Visualizations

``` r
library(ggplot2) #uploading ggplot2 to make plots

USStates_num <- USStates %>% #creating new vector/dataset
  select_if(is.numeric) #selecting for only numeric variables
cor(USStates_num, use = "pairwise.complete.obs") #correlation matrix between all numeric variables
```

    ##                  MonthlyRent AnnualRent AnnualIncome DisposableIncome
    ## MonthlyRent       1.00000000 1.00000000   0.70245041       0.02535031
    ## AnnualRent        1.00000000 1.00000000   0.70245041       0.02535031
    ## AnnualIncome      0.70245041 0.70245041   1.00000000       0.72931129
    ## DisposableIncome  0.02535031 0.02535031   0.72931129       1.00000000
    ## UrbanPop          0.57803406 0.57803406   0.37596272      -0.02759268
    ## ArrestNumber      0.08057799 0.08057799  -0.06824338      -0.17331150
    ##                     UrbanPop ArrestNumber
    ## MonthlyRent       0.57803406   0.08057799
    ## AnnualRent        0.57803406   0.08057799
    ## AnnualIncome      0.37596272  -0.06824338
    ## DisposableIncome -0.02759268  -0.17331150
    ## UrbanPop          1.00000000   0.09638715
    ## ArrestNumber      0.09638715   1.00000000

Here, a correlation matrix has been developed in order to numerically
visualize the relation between different numeric variables in the
*USStates* dataset. Annual Income and Disposable Income appear to have
the greatest correlation, followed by the Annual Income and
Annual/Monthly Rent. The Urban Population Percentage and Disposable
Income have the least correlation based on the matrix values. These
correlation values can be modified to be visualized in a more visually
efficient manner as shown below.

``` r
#correlation heatmap
cor(USStates_num, use = "pairwise.complete.obs") %>%  #correlation function
  as.data.frame %>% #saving as a data frame
  rownames_to_column %>%  #converting row names into an explicit variable
  pivot_longer(-1, names_to = "other_var", values_to = "correlation") %>% #pivoting all correlations into the same column
  ggplot(aes(rowname, other_var, fill=correlation)) +  #using ggplot to create the base plot
  geom_tile() + #creating heatmap format
  scale_fill_gradient2(low="red",mid="white",high="purple") + #creating color gradient
  geom_text(aes(label = round(correlation,2)), color = "black", size = 4) +   # Overlay values
  labs(title = "Correlation Heatmap for US States Dataset", x = "Variable 1", y = "Variable 2", aes(size=2)) + #title and axes labels
  theme(axis.text.x = element_text(angle = 90)) #vertically rotate x-axis variables so that they are not overlapping
```
![image](https://user-images.githubusercontent.com/83299916/117582283-e3651d80-b0c6-11eb-9f09-f7f730e8bbeb.png)
![](Project-1_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

The correlation heatmap above shows that the two variables with the
greatest correlation are Annual Income and Disposable Income. Here, the
correlation between Monthly and Annual Rent is ignored as the Annual
Rent was created by multiplying the Monthly Rent values by 12. The least
correlated variables appear to be Disposable Income and Monthly/Annual
Rent, as well as Urban Population Percentage and Disposable Income.
These results support the findings of the aforementioned correlation
matrix.

``` r
ggplot(USStates, aes(AnnualRent, AnnualIncome)) + #plot and setting x and y aesthetics
  geom_point(aes(color=DisposableIncome)) + #coloring points by Disposable Income
  scale_color_gradient(low="blue", high="yellow") + #creating point color gradient
  labs(title = "Annual Rent and Income by Estimated Disposable Income", x = "Annual Rent (in USD)", y = "Annual Income (in USD)") #title and axes labels
```

![image](https://user-images.githubusercontent.com/83299916/117582294-f2e46680-b0c6-11eb-842c-6ecab3a1bd47.png)

![](Project-1_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

This first graph displays the Annual Income against the Annual Rent,
colored by Disposable Income. Visually speaking, as the annual rent
increases, the annual income also increases. It appears that the
disposable income is high when the annual income is high and the annual
rent is low, and decreases as the rent increases. Thus, it is possible
that the disposable income has an inverse association with the annual
rent, and a direct association to the annual income.

``` r
ggplot(USStates, aes(x = ArrestType, y = ArrestNumber))+ #plot and setting x and y aesthetics
  geom_bar(stat="summary", fun="mean", aes(fill=UrbPopQuartile)) + #mean arrest number, colored by UrbPopQuartile
  scale_fill_manual("Percentage of Population that is Urban", values = c("between 25-50%" = "#632BF1", "greater than 50%, less than 75%" = "#A180F5", "greater than 75%" = "#DCD2F6")) + #colors for UrbPopQuartile
  labs(title = "Mean Arrest Number by Urban Population Quartiles", x = "Arrest Type", y = "Mean Number of Arrests (per 100,000)") #title and axes labels
```
![image](https://user-images.githubusercontent.com/83299916/117582302-fe379200-b0c6-11eb-9db0-8fb717cdf916.png)

![](Project-1_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

Early on in this study, it was predicted that as the percentage of urban
population increases, the number of arrests would also increase. It was
for this purpose that this figure was created. The mean arrest numbers
were plotted according to the arrest type (murder, assault, rape), and
were color-coordinated by the UrbPopQuartile variable. Essentially, this
figure displays mean arrest numbers and how the percentage of urban
population corresponds to the mean arrest numbers. Contrary to the
initial expectation, the visual appearance of the figure supports the
idea that the percentage of urban population does not deeply influence
the mean number of arrests. Looking at the assault bar, the “greater
than 75%” section is only slightly greater than that of the other two
urban population quartiles Looking at the murder and rape bars, the mean
number of arrests appears to be similar despite the urban population
quartiles.

------------------------------------------------------------------------

### Principle Component Analysis (PCA)

Through PCA, the aim is to increase variance in order to distinguish
observations from each other, while decreasing covariance so as to
distinguish variables from one another. This method of dimensionality
reduction was chosen due to the high-dimensionality of the *USStates*
dataset. Additionally, this method allowed for the reduction of the
variables into a smaller set of principle components.

``` r
pca <- USStates %>% #creating vector to hold pca data
  select(where(is.numeric)) %>%  #removing categorical variables
  scale() %>% #scaling to 0 mean and unit variance (standardization)
  prcomp() #PCA function

percent <- 100* (pca$sdev^2 / sum(pca$sdev^2)) #finding percentage of variance explained by each PC using sdev
percent 
```

    ## [1] 5.129324e+01 2.481006e+01 1.481912e+01 9.077582e+00 1.791814e-30
    ## [6] 1.873188e-32

``` r
# Scree plot to visualize the percentage of variance explained by each PC
perc_data <- data.frame(percent = percent, PC = 1:length(percent)) #create data vector for scree plot
ggplot(perc_data, aes(x = PC, y = percent)) + #setting aesthetics for scree plot
  geom_col(fill="light blue") + #color of PC bars
  geom_line() + #scree plot line for "elbow" (to determine PC #)
  geom_text(aes(label = round(percent, 2)), size = 4, vjust = -0.5) + #variance text positioning 
  ylim(0, 60) + xlim(0,5) + #setting graph axes limits
  labs(title= "Scree Plot: Percentage of Variance per PC", y = "Percentage of Variance Explained") #setting plot title and axis label
```

    ## Warning: Removed 1 rows containing missing values (position_stack).

    ## Warning: Removed 1 rows containing missing values (geom_col).

    ## Warning: Removed 1 row(s) containing missing values (geom_path).

    ## Warning: Removed 1 rows containing missing values (geom_text).

![image](https://user-images.githubusercontent.com/83299916/117582309-05f73680-b0c7-11eb-88cd-591b7496c987.png)

![](Project-1_files/figure-gfm/PCA-1.png)<!-- -->

Through the dplyr vector functionality, PCA was conducted and each of
the principle components are visible in the scree plot above. Based on
the scree plot used to determine the number of principal components, the
suitable number of principle components to select is two, or two
dimensions. Therefore, the percent vector can be used to describe the
percentage of variance explained by the first two principle components.
PC1 explained about 51.29% of the variance and PC2 explained about
24.81% of the variance. These values of explained variance are supported
by the values in the *percent* vector, as well as in the scree plot.

``` r
#PCA results
names(pca) #PC sdev values, rotation matrix
```

    ## [1] "sdev"     "rotation" "center"   "scale"    "x"

``` r
pca #looking at pca results
```

    ## Standard deviations (1, .., p=6):
    ## [1] 1.754307e+00 1.220083e+00 9.429460e-01 7.380074e-01 3.278854e-16
    ## [6] 3.352481e-17
    ## 
    ## Rotation (n x k) = (6 x 6):
    ##                         PC1        PC2        PC3          PC4           PC5
    ## MonthlyRent      0.53563087  0.1885343 -0.1084329 -0.313909550  4.659637e-01
    ## AnnualRent       0.53563087  0.1885343 -0.1084329 -0.313909550  2.711199e-02
    ## AnnualIncome     0.49402745 -0.3857738  0.1659920 -0.072180259 -7.204475e-01
    ## DisposableIncome 0.17899933 -0.7230826  0.3373830  0.200376081  5.129309e-01
    ## UrbanPop         0.38638522  0.2742452 -0.1341421  0.870345874 -1.665335e-16
    ## ArrestNumber     0.02831445  0.4266490  0.9039413 -0.007686699  6.938894e-17
    ##                            PC6
    ## MonthlyRent      -5.917223e-01
    ## AnnualRent        7.526769e-01
    ## AnnualIncome     -2.351754e-01
    ## DisposableIncome  1.674358e-01
    ## UrbanPop         -1.110223e-16
    ## ArrestNumber      9.020562e-17

``` r
head(pca$x) #looking at rotated data
```

    ##            PC1        PC2        PC3         PC4           PC5           PC6
    ## [1,] -2.058763  0.4081489 -0.7959699  0.06012693 -3.986725e-16 -3.293384e-16
    ## [2,] -1.987384  1.4836932  1.4827860  0.04074945 -2.237492e-16 -1.019380e-16
    ## [3,] -2.056200  0.4467681 -0.7141474  0.05943115 -3.923916e-16 -3.211732e-16
    ## [4,]  1.650421 -0.7876642 -0.4048811 -1.94862373  1.019158e-15  1.817665e-16
    ## [5,]  1.731475  0.4336675  2.1827546 -1.97062779  1.217792e-15  4.399904e-16
    ## [6,]  1.661474 -0.6211190 -0.0520217 -1.95162428  1.046244e-15  2.169788e-16

``` r
pca_data <- data.frame(pca$x, UrbanProportion = USStates$UrbPopQuartile) #putting info about the different groups back into PCA data
head(pca_data) #looking new data
```

    ##         PC1        PC2        PC3         PC4           PC5           PC6
    ## 1 -2.058763  0.4081489 -0.7959699  0.06012693 -3.986725e-16 -3.293384e-16
    ## 2 -1.987384  1.4836932  1.4827860  0.04074945 -2.237492e-16 -1.019380e-16
    ## 3 -2.056200  0.4467681 -0.7141474  0.05943115 -3.923916e-16 -3.211732e-16
    ## 4  1.650421 -0.7876642 -0.4048811 -1.94862373  1.019158e-15  1.817665e-16
    ## 5  1.731475  0.4336675  2.1827546 -1.97062779  1.217792e-15  4.399904e-16
    ## 6  1.661474 -0.6211190 -0.0520217 -1.95162428  1.046244e-15  2.169788e-16
    ##                   UrbanProportion
    ## 1 greater than 50%, less than 75%
    ## 2 greater than 50%, less than 75%
    ## 3 greater than 50%, less than 75%
    ## 4                  between 25-50%
    ## 5                  between 25-50%
    ## 6                  between 25-50%

Looking at the *pca* data, it appears as though Monthly and Annual Rent
have the greatest impact on PC1. Arrest Number appears to have the
greatest influence on PC2. Furthermore, the ability to rotate these
values and change the referential of the PCs, allowed for dimensionality
reduction, as seen in the rotation matrix above. The rotated data can
now be visualized in terms of the two principle components.

``` r
# Plot against PC1 and PC2
ggplot(pca_data, aes(x = PC1, y = PC2, color = UrbanProportion)) + #setting PC1 and PC2 as data, colored by UrbPropQuartile
  geom_point() + #back into two-dimensional space 
  scale_color_manual("Proportion of Population that is Urban", values = c("between 25-50%" = "#0028FE", "greater than 50%, less than 75%" = "#AF92F8", "greater than 75%" = "#E09AE9")) #setting color gradient
```
![image](https://user-images.githubusercontent.com/83299916/117582317-10b1cb80-b0c7-11eb-99d3-a399ac221efc.png)

![](Project-1_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

After plotting the newly rotated data against the two PCs, and grouping
by the proportion of population that is urban (UrbPopQuartile), it does
not appear that there are any clusters or groups of association in the
newly reduced data.

``` r
pca$rotation #looking at rotation matrix
```

    ##                         PC1        PC2        PC3          PC4           PC5
    ## MonthlyRent      0.53563087  0.1885343 -0.1084329 -0.313909550  4.659637e-01
    ## AnnualRent       0.53563087  0.1885343 -0.1084329 -0.313909550  2.711199e-02
    ## AnnualIncome     0.49402745 -0.3857738  0.1659920 -0.072180259 -7.204475e-01
    ## DisposableIncome 0.17899933 -0.7230826  0.3373830  0.200376081  5.129309e-01
    ## UrbanPop         0.38638522  0.2742452 -0.1341421  0.870345874 -1.665335e-16
    ## ArrestNumber     0.02831445  0.4266490  0.9039413 -0.007686699  6.938894e-17
    ##                            PC6
    ## MonthlyRent      -5.917223e-01
    ## AnnualRent        7.526769e-01
    ## AnnualIncome     -2.351754e-01
    ## DisposableIncome  1.674358e-01
    ## UrbanPop         -1.110223e-16
    ## ArrestNumber      9.020562e-17

``` r
rotation_data <- data.frame(pca$rotation, variable = row.names(pca$rotation)) #making data frame for rotation matrix
arrow_style <- arrow(length = unit(0.05, "inches"), type = "closed") #setting an arrow style
```

The code above was performed in preparation of using *ggplot* to graph
the contribution of each variable to the PCs, as shown below.

``` r
# Plot the contribution of variables to PCs using geom_segment() for arrows and geom_text() for labels
library(ggrepel)  #uploading ggrepel to organize text on the ggplot below
ggplot(rotation_data) + #using rotated matrix
  geom_segment(aes(xend = PC1, yend = PC2), x = 0, y = 0, arrow = arrow_style) + #visual settings
  geom_text_repel(aes(x = PC1, y = PC2, label = variable), hjust = 0, size = 3, color = "blue") + #text settings, avoid overlap of Monthly/Annual Rent
  xlim(-1., 1.25) + 
  ylim(-1., 1.) +
  coord_fixed()
```
![image](https://user-images.githubusercontent.com/83299916/117582324-190a0680-b0c7-11eb-865d-5e780557bab9.png)

![](Project-1_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

Looking into the contribution of each variable to the two principal
components, it is important to note that PC1 is along the horizontal
axis and PC2 is along the vertical axis. Therefore, the plot above
visually supports the idea that the Annual and Monthly Rent greatly
contribute to PC1. Both Urban Population Percentage and Annual Income
contribute to PC1 very slightly more than to PC2. Disposable Income and
Arrest Number appear to contribution largely to PC2.

------------------------------------------------------------------------

### Conclusion

The combination of U.S. States’ income, rent, crime rate, and urban
population data allowed for incredible analysis of patterns that exist
across the variables in the *USStates* dataset. Several crucial findings
came to light through this study. Looking at summary statistics grouped
by UrbPopQuartile, the Monthly/Annual Rent, Annual Income appeared to
increase as the percentage of urban population increased. However, the
summary statistics showed that there was no specific pattern of Arrest
Numbers in terms of the percentage of the population that was urban.
This was further supported by the bar graph, in that the urban
population quartile does not greatly determine the number of murder
arrests, assault arrests, or rape arrests. In order to better understand
the variance structure in this newly merged dataset, the PCA unveiled
many findings. Both Monthly and Annual Rent were found to have the
greatest influence on PC1, while Arrest Number had the greatest
influence on PC2. However, plotting the newly rotated data did not
unveil any patterns of association of clustering based on urban
population quartiles.
