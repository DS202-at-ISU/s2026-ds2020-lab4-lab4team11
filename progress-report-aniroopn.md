progress-report-aniroopn
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.6
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.1     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(Lahman)
```

    ## Warning: package 'Lahman' was built under R version 4.5.3

``` r
library(janitor)
```

    ## Warning: package 'janitor' was built under R version 4.5.3

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

# 1. Scrape the data

``` r
url <- "https://www.baseball-reference.com/awards/hof_2026.shtml"

page <- read_html(url)

tables <- page %>%
  html_table(fill = TRUE)

# Use the FIRST table
hof_raw <- tables[[1]]

head(hof_raw)
```

    ## # A tibble: 6 × 39
    ##   ``    ``           ``    ``    ``    ``    ``    ``    ``    ``    ``    ``   
    ##   <chr> <chr>        <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
    ## 1 Rk    Name         YoB   Votes %vote HOFm  HOFs  Yrs   WAR   WAR7  JAWS  Jpos 
    ## 2 1     Carlos Belt… 4th   358   84.2% 126   50    20    70.0  44.4  57.2  58.0 
    ## 3 2     Andruw Jones 9th   333   78.4% 109   32    17    62.7  46.4  54.6  58.0 
    ## 4 3     Chase Utley  3rd   251   59.1% 94    36    16    64.6  49.3  56.9  56.6 
    ## 5 4     Andy Pettit… 8th   206   48.5% 128   44    18    60.2  34.1  47.2  61.4 
    ## 6 5     Félix Herná… 2nd   196   46.1% 67    31    15    49.8  38.5  44.1  61.4 
    ## # ℹ 27 more variables: `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Pitching Stats` <chr>,
    ## #   `Pitching Stats` <chr>, `Pitching Stats` <chr>, `Pitching Stats` <chr>,
    ## #   `Pitching Stats` <chr>, `Pitching Stats` <chr>, `Pitching Stats` <chr>, …

# 2. Fix the column names

``` r
data <- hof_raw

# First row contains actual column names
colnames(data) <- data[1, ]

# Remove the first row
data <- data[-1, ]

head(data)
```

    ## # A tibble: 6 × 39
    ##   Rk    Name       YoB   Votes `%vote` HOFm  HOFs  Yrs   WAR   WAR7  JAWS  Jpos 
    ##   <chr> <chr>      <chr> <chr> <chr>   <chr> <chr> <chr> <chr> <chr> <chr> <chr>
    ## 1 1     Carlos Be… 4th   358   84.2%   126   50    20    70.0  44.4  57.2  58.0 
    ## 2 2     Andruw Jo… 9th   333   78.4%   109   32    17    62.7  46.4  54.6  58.0 
    ## 3 3     Chase Utl… 3rd   251   59.1%   94    36    16    64.6  49.3  56.9  56.6 
    ## 4 4     Andy Pett… 8th   206   48.5%   128   44    18    60.2  34.1  47.2  61.4 
    ## 5 5     Félix Her… 2nd   196   46.1%   67    31    15    49.8  38.5  44.1  61.4 
    ## 6 6     Alex Rodr… 5th   170   40.0%   390   77    22    117.4 64.3  90.9  55.5 
    ## # ℹ 27 more variables: G <chr>, AB <chr>, R <chr>, H <chr>, HR <chr>,
    ## #   RBI <chr>, SB <chr>, BB <chr>, BA <chr>, OBP <chr>, SLG <chr>, OPS <chr>,
    ## #   `OPS+` <chr>, W <chr>, L <chr>, ERA <chr>, `ERA+` <chr>, WHIP <chr>,
    ## #   G <chr>, GS <chr>, SV <chr>, IP <chr>, H <chr>, HR <chr>, BB <chr>,
    ## #   SO <chr>, `Pos Summary` <chr>

# 3. Clean the data

``` r
hof_clean <- data %>%
  select(Name, Votes, `%vote`) %>%
  mutate(
    year = 2026,
    Votes = as.numeric(Votes),
    percent_of_vote = readr::parse_number(`%vote`),
    inducted = ifelse(percent_of_vote >= 75, "Y", "N")
  ) %>%
  rename(candidate_name = Name)

hof_clean
```

    ## # A tibble: 27 × 6
    ##    candidate_name  Votes `%vote`  year percent_of_vote inducted
    ##    <chr>           <dbl> <chr>   <dbl>           <dbl> <chr>   
    ##  1 Carlos Beltrán    358 84.2%    2026            84.2 Y       
    ##  2 Andruw Jones      333 78.4%    2026            78.4 Y       
    ##  3 Chase Utley       251 59.1%    2026            59.1 N       
    ##  4 Andy Pettitte     206 48.5%    2026            48.5 N       
    ##  5 Félix Hernández   196 46.1%    2026            46.1 N       
    ##  6 Alex Rodriguez    170 40.0%    2026            40   N       
    ##  7 X-Manny Ramirez   165 38.8%    2026            38.8 N       
    ##  8 Bobby Abreu       131 30.8%    2026            30.8 N       
    ##  9 Jimmy Rollins     108 25.4%    2026            25.4 N       
    ## 10 Cole Hamels       101 23.8%    2026            23.8 N       
    ## # ℹ 17 more rows

# 4. Save CSV

``` r
write_csv(hof_clean, "HallOfFame_2026.csv")
```

# 5. Compare with Lahman

``` r
lahman_2026 <- HallOfFame %>%
  filter(yearID == 2026)

hof_clean %>% count(inducted)
```

    ## # A tibble: 2 × 2
    ##   inducted     n
    ##   <chr>    <int>
    ## 1 N           25
    ## 2 Y            2

``` r
lahman_2026 %>% count(inducted)
```

    ##   inducted n
    ## 1        N 7
    ## 2        Y 1

# 6. Conclusion

After comparing the scraped data with the Lahman dataset, I found that
the two sources do not match exactly. The Baseball Reference data
includes the full BBWAA ballot, which contains all candidates who
received votes. In contrast, the Lahman dataset stores official Hall of
Fame voting records and may include fewer entries or different voting
groups.

This difference is reflected in the counts: the scraped dataset contains
more candidates overall, while the Lahman dataset includes only a subset
of those candidates. Additionally, the number of inducted players
differs slightly between the two sources due to how the data is
recorded.

Overall, this activity showed that even when datasets describe the same
real-world event, they can differ significantly in structure and
content. Understanding these differences is important when cleaning,
analyzing, and comparing data from multiple sources.
