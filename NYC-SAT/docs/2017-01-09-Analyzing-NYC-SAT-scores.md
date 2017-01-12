``` r
library(knitr)
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(ggthemes)

knitr::opts_chunk$set(
  cache = TRUE, 
  warning = FALSE, 
  message = FALSE, 
  dpi = 180,
  comment = "#>",
  collapse = TRUE
)
options(width=80)
```

Getting the data
================

All of the data I'll be sourcing comes from NYC's very own open data initiative, [NYC OpenData](https://data.cityofnewyork.us/)! I've been meaning to ramp up on the difficulty/interactivity of these blog posts, and I figure the topic of NYC education has been near and dear to my heart for long enough while also proving accessible for data analysis.

> Last fall (Fall 2015) I interned with the data and analytics team over at Success Academy Charter Schools and got to see how they were using data science and machine learning to improve student experiences. Really cool stuff!

A persistent topic when discussing general education is college preparedness (and beyond, but let's leave that for another time 😊) -- how well do our high schools prepare their seniors for the SAT -- the effective gatekeeper to higher education? We can use SAT scores as a first-pass understanding of how well high schools in the city are, well, doing their job!

Let's take a look at how SAT scores vary around the city, understand what's going on, and draw some pretty graphs along the way.

Exploratory Analysis
====================

[This dataset](https://data.cityofnewyork.us/Education/SAT-Results/f9bf-2cp4) contains the average score, broken down by SAT section, of high school seniors across different NYC public high schools in 2012. Each record represents a high school, with each having a field for identification, name, number of students who took the SAT, along with averaged scores for each section on the exam.

``` r
# School-wise breakdown of test takers, average scores, etc.
scores <- read_csv("../data/SAT_Results.csv", col_types = "ccnnnn")
```

By the power of the tidyverse, we can easily do some work to make the dataset slightly more pleasant and opt to do so.

``` r
# Tidying column and school names
names(scores) <- names(scores) %>% 
  make.names %>% 
  tolower

scores <- scores %>% 
  mutate(school.name = stringr::str_to_title(school.name))
```

Let's start off with a high-level understanding of the data -- or, as Roger Peng puts it, ["check the packaging"](https://bookdown.org/rdpeng/exdata/exploratory-data-analysis-checklist.html#check-the-packaging).

``` r
# Subsetting and summarizing columns of interest.
scores %>%
  select(num.of.sat.test.takers:sat.writing.avg..score) %>%
  rename(n.test.takers = num.of.sat.test.takers,
         reading.avg = sat.critical.reading.avg..score,
         math.avg = sat.math.avg..score,
         writing.avg = sat.writing.avg..score) %>%
  summary %>%
  kable
```

|     | n.test.takers |  reading.avg  |    math.avg   | writing.avg |
|-----|:--------------|:-------------:|:-------------:|:-----------:|
|     | Min. : 6.0    |  Min. :279.0  |  Min. :312.0  |  Min. :286  |
|     | 1st Qu.: 41.0 | 1st Qu.:368.0 | 1st Qu.:371.0 | 1st Qu.:360 |
|     | Median : 62.0 | Median :391.0 | Median :395.0 | Median :381 |
|     | Mean : 110.3  |  Mean :400.9  |  Mean :413.4  |  Mean :394  |
|     | 3rd Qu.: 95.0 | 3rd Qu.:416.0 | 3rd Qu.:437.0 | 3rd Qu.:411 |
|     | Max. :1277.0  |  Max. :679.0  |  Max. :735.0  |  Max. :682  |
|     | NA's :57      |    NA's :57   |    NA's :57   |   NA's :57  |

High-level summary statistics don't reveal anything particularly interesting about the results across the board: there's no skew between medians and modes, and despite *wide* distances between minimum and maximum scores the distributions don't seem all that odd. The 57 NA records represent 57 public high schools where 5 or fewer seniors took the SAT, and thus their scores are not reported for privacy and identification purposes.

It's worrisome that **12% (57/478) of public high schools in the city are unable to prepare and send a meagre 5 students to take the SAT**. I'm not sure how this figure compares to other school systems both nationwide and in other urban areas, but it would be worth evaluating how NYC compares and whether it's a NYC DOE problem or a metropolitan education issue.

Looking at score distributions, there isn't much variation in distribution, either (unimodal around the high 300s, long right tails):

``` r
scores.melted <- scores %>%
  select(sat.critical.reading.avg..score:sat.writing.avg..score) %>%
  melt

section_labels <- c("sat.critical.reading.avg..score" = "Critical Reading", 
                    "sat.math.avg..score" = "Math",
                    "sat.writing.avg..score" = "Writing")

# Draw!
ggplot(scores.melted, aes(value)) + 
  geom_histogram(binwidth = 20) + 
  facet_grid(variable ~ ., scales = "fixed", 
             labeller = as_labeller(section_labels)) +
  labs(y = "Frequency",
       x = "Distribution of average scores") +
  theme_solarized() +
  ggtitle("Distributions across SAT sections",
          subtitle = "Average scores across subsections are relatively standardized")
```

![](2017-01-09-Analyzing-NYC-SAT-scores_files/figure-markdown_github/hist-1.png)

We can also take a look at schools who had the least and most students take the SAT -- is there a strong correlation between number of test takers and cumulative outcome?

``` r
# Obtain top and bottom 10% of number of SAT takers.
top <- scores %>%
  arrange(desc(num.of.sat.test.takers)) %>%
  head(n = 0.10 * nrow(scores)) %>%
  transmute(school.name = school.name,
            num.of.sat.test.takers = num.of.sat.test.takers,
            cumulative = rowSums(.[4:6]), 
            top = 1)

bottom <- scores %>%
  arrange(num.of.sat.test.takers) %>%
  head(n = 0.10 * nrow(scores)) %>%
  transmute(school.name = school.name,
            num.of.sat.test.takers = num.of.sat.test.takers,
            cumulative = rowSums(.[4:6]), 
            top = 0)

# Store our translation labels for nicer facet grid labels.
top_labels <- c("1" = "Top 10%", "0" = "Bottom 10%")

# Join and melt our two dataframes.
all.melted <- rbind(top, bottom) %>%
  melt(id=c("school.name", "num.of.sat.test.takers", "top")) %>%
  arrange(desc(num.of.sat.test.takers)) %>%
  mutate(top = as.factor(top))

# Draw!
ggplot(data = all.melted, 
       aes(x=num.of.sat.test.takers, y=value)) +
  geom_point() +
  stat_smooth() + 
  facet_grid(. ~ top, scales = "free_x", 
             labeller = as_labeller(top_labels)) +
  theme_solarized() +
  labs(x = "Number of SAT Test Takers",
       y = "Cumulative of average scores") +
  ggtitle(label = "SAT scores broken down by borough",
          subtitle = "Bottom versus top 10% of number of students taking SAT")
```

![](2017-01-09-Analyzing-NYC-SAT-scores_files/figure-markdown_github/num-takers-1.png)

There might just be something here! It's worth noting that this isn't apples-to-apples for a number of reasons:

-   the schools that sent the fewest students don't report SAT scores

-   we also expect that having a greater number of students taking the exam will anchor the scores at a certain range: with a smaller sample size we would expect more variance

-   we don't have an understanding of what proportion of students per school take the SAT -- only a raw value -- and so there's no consideration of school population

That said, it's interesting to see the significant difference between the top and bottom 10% of schools. If we could account for the above points (by sourcing an outside dataset or aggregating over more time, perhaps) it would be interesting to see **whether we could support the hypothesis that high schools where a greater number of students take the SAT are more academically rigorous or better-equipped**. Not to bash low-performing high schools, but instead we can imagine a reallocation of resources (academic, financial, etc) that can improve schools in the bottom percentiles.

Spatial Analysis
----------------

The above analysis leaves a lot to be seen: what about location? It's no secret that schools in better neighborhoods are more likely to have better resources for students, but can we quantitatively see that? Sourcing another dataset by NYC OpenData, a [directory of 2014 - 2015 NYC high schools](https://data.cityofnewyork.us/Education/DOE-High-School-Directory-2014-2015/n3p6-zve2), we get more per-school location information, including borough, zipcode, and latitude + longitude.

``` r
# School-wise breakdown on NYC schools, including location!
directory <- read_csv("../data/DOE-High-School-Directory-2013-2014.csv")

# Tidying column and school names
names(directory) <- names(directory) %>% 
  make.names %>% 
  tolower
```

The data itself is already pretty tidy, which is a nice surprise! Using the provided data, however, we note that we lose ~100 records post-join:

``` r
joined <- inner_join(scores, directory, by = "dbn")
not_joined <- anti_join(scores, directory, by = "dbn")

c(nrow(joined), nrow(not_joined))
#> [1] 368 110
```

My suspicion is that I'm crossing dates -- the SAT score data is from 2012-2013 but the directory for high schools is from 2013-2014. That said, I also previously attempted this join with directory data from 2014-2015 and had the same number of records dropped -- it's likely a deeper issue (some schools that reported scores not in directory for whatever reason). Given that we're not in the sample size business (this time, at least) we can keep the analysis going.

``` r
# Isolate columns of interest
schools <- joined %>%
  select(dbn:sat.writing.avg..score, boro,
         zip, total.student.10.26, program.highlights,
         latitude, longitude, grade.span.2014.2015.max,
         grade.span.2014.2015.min) %>%
  mutate(boro = as.factor(boro), zip = as.factor(zip))

schools$boro <- schools$boro %>%
  recode(M = "Manhattan", X = "Bronx", K = "Brooklyn",
         Q = "Queens", R = "Staten Island")

schools <- schools %>%
  mutate(cumulative = rowSums(.[4:6]))
```

We can take a quick look at descriptive statistics now that we have a few more dimensions. Is there any noticable difference between cumulative SAT scores across boroughs?

``` r
borough <- schools %>%
  select(school.name, boro, cumulative,
         num.of.sat.test.takers:sat.writing.avg..score)

ggplot(borough, aes(x = num.of.sat.test.takers, y = cumulative)) + 
  facet_grid(. ~ boro) +
  geom_point(alpha = 1/4) +
  stat_smooth() + 
  theme_solarized() +
  labs(x = "Number of SAT Test Takers",
       y = "Cumulative of average scores") +
  ggtitle(label = "Comparing SAT scores across boroughs",
          subtitle = "Scores at NYC public high schools generally follow similar trends")
```

![](2017-01-09-Analyzing-NYC-SAT-scores_files/figure-markdown_github/by-borough-1.png)

There doesn't seem to be a consistent difference between boroughs -- generally, number of SAT test takers is an "okay" proxy for how well a school does. Most boroughs have public high-performers (Manhattan's is Stuyvesant, Bronx has Bronx Science, etc.), but otherwise there seems to be a tendency for scores to settle at around 1250, which is what we'd expect given the score distribution above.

<!-- Lastly, we can use that nifty latitude and longitude data, join it with a shapefile, and get a geographic plot going! The geographic data also comes from [NYC OpenData](https://data.cityofnewyork.us/Education/2014-2015-School-Zones/mshx-yvwq) and gives us a file we need in particular: ** -->
### The end

It's no secret that the public school system in NYC is flirting with misery. There's an ongoing segregation -- both racial and financial -- in the schools that ties into a lot more than this post can really cover. It's a bit disappointing to see just how *few* high school seniors take the SAT -- not even considering performance -- and how it relates to the school's academic performance culture.

> Feel free to check out any code, data, and notebooks for this analysis on the [Exploratorium](https://github.com/dataframing/exploratorium) repository! Everything's open, but get in touch if you have any questions!

------------------------------------------------------------------------

This is the first of a series of analyses! Follow me on [Twitter](https://twitter.com/dataframing) for more soon! <!-- Something something follow me on [Twitter](https://twitter.com/dataframing) I'm building my brand. -->
