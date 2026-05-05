progress-report-lamarri
================

### Import libraries

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
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

# Scrape Data

``` r
url <- "https://www.baseball-reference.com/awards/hof_2026.shtml"
url_page <- read_html(url)
tables <- html_table(url_page)
head(tables)
```

    ## [[1]]
    ## # A tibble: 28 × 39
    ##    ``    ``          ``    ``    ``    ``    ``    ``    ``    ``    ``    ``   
    ##    <chr> <chr>       <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
    ##  1 Rk    Name        YoB   Votes %vote HOFm  HOFs  Yrs   WAR   WAR7  JAWS  Jpos 
    ##  2 1     Carlos Bel… 4th   358   84.2% 126   50    20    70.0  44.4  57.2  58.0 
    ##  3 2     Andruw Jon… 9th   333   78.4% 109   32    17    62.7  46.4  54.6  58.0 
    ##  4 3     Chase Utley 3rd   251   59.1% 94    36    16    64.6  49.3  56.9  56.6 
    ##  5 4     Andy Petti… 8th   206   48.5% 128   44    18    60.2  34.1  47.2  61.4 
    ##  6 5     Félix Hern… 2nd   196   46.1% 67    31    15    49.8  38.5  44.1  61.4 
    ##  7 6     Alex Rodri… 5th   170   40.0% 390   77    22    117.4 64.3  90.9  55.5 
    ##  8 7     X-Manny Ra… 10th  165   38.8% 226   68    19    69.3  40.0  54.6  53.6 
    ##  9 8     Bobby Abreu 7th   131   30.8% 95    52    18    60.2  41.6  50.9  56.0 
    ## 10 9     Jimmy Roll… 5th   108   25.4% 121   42    17    47.9  32.7  40.3  55.5 
    ## # ℹ 18 more rows
    ## # ℹ 27 more variables: `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Pitching Stats` <chr>,
    ## #   `Pitching Stats` <chr>, `Pitching Stats` <chr>, `Pitching Stats` <chr>, …
    ## 
    ## [[2]]
    ## # A tibble: 9 × 38
    ##   ``    ``                 ``    ``    ``    ``    ``    ``    ``    ``    ``   
    ##   <chr> <chr>              <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
    ## 1 Rk    Name               "Vot… "%vo… HOFm  HOFs  Yrs   WAR   WAR7  JAWS  Jpos 
    ## 2 1     Jeff Kent          "14"  ""    123   51    17    55.4  35.8  45.6  56.6 
    ## 3 2     Carlos Delgado     "9"   ""    110   44    17    44.4  34.5  39.4  53.5 
    ## 4 3     Don Mattingly      "6"   ""    134   34    14    42.4  35.8  39.1  53.5 
    ## 5 4     Dale Murphy        "6"   ""    116   33    18    46.5  41.2  43.9  58.0 
    ## 6 5     Barry Bonds        ""    ""    340   75    22    162.8 72.7  117.8 53.6 
    ## 7 6     Roger Clemens      ""    ""    332   73    24    139.2 65.9  102.6 61.4 
    ## 8 7     Gary Sheffield     ""    ""    158   60    22    60.5  38.0  49.3  56.0 
    ## 9 8     Fernando Valenzue… ""    ""    67    25    17    41.5  33.5  37.5  61.4 
    ## # ℹ 27 more variables: `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Pitching Stats` <chr>,
    ## #   `Pitching Stats` <chr>, `Pitching Stats` <chr>, `Pitching Stats` <chr>,
    ## #   `Pitching Stats` <chr>, `Pitching Stats` <chr>, `Pitching Stats` <chr>, …

# Fix column names

``` r
# Need to use first table
data <- tables[[1]]

# Since there is a distinction between Batting Stats and Pitching Stats in the first row, the variable names need to be corrected 
colnames(data) <- data[1,]

# Remove first row
data <- data[-1,]

# Check data
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

# Clean data

``` r
hof2026 <- data %>%
  select(Name, Votes, `%vote`) %>%
  mutate(
    year = 2026,
    Name = gsub("X-", "", Name),
    Votes = as.numeric(Votes), #should be numeric
    VotePercent = readr::parse_number(`%vote`),
    inducted = ifelse(VotePercent >= 75, "Y", "N")
  ) %>%
  rename(name = Name)

# Look at new dataset
hof2026
```

    ## # A tibble: 27 × 6
    ##    name            Votes `%vote`  year VotePercent inducted
    ##    <chr>           <dbl> <chr>   <dbl>       <dbl> <chr>   
    ##  1 Carlos Beltrán    358 84.2%    2026        84.2 Y       
    ##  2 Andruw Jones      333 78.4%    2026        78.4 Y       
    ##  3 Chase Utley       251 59.1%    2026        59.1 N       
    ##  4 Andy Pettitte     206 48.5%    2026        48.5 N       
    ##  5 Félix Hernández   196 46.1%    2026        46.1 N       
    ##  6 Alex Rodriguez    170 40.0%    2026        40   N       
    ##  7 Manny Ramirez     165 38.8%    2026        38.8 N       
    ##  8 Bobby Abreu       131 30.8%    2026        30.8 N       
    ##  9 Jimmy Rollins     108 25.4%    2026        25.4 N       
    ## 10 Cole Hamels       101 23.8%    2026        23.8 N       
    ## # ℹ 17 more rows

``` r
# Identify how many inducted
hof2026 %>% count(inducted)
```

    ## # A tibble: 2 × 2
    ##   inducted     n
    ##   <chr>    <int>
    ## 1 N           25
    ## 2 Y            2

# Look at Lahman data

``` r
lahman2026 <- HallOfFame %>% 
  filter(yearID == 2026)

# Identify how many inducted
lahman2026 %>% count(inducted)
```

    ##   inducted n
    ## 1        N 7
    ## 2        Y 1

# Conclusion

When comparing the scraped data to the Lahman dataset, it becomes clear
that they provide distinct information. The scraped data is more
inclusive by reflecting the entire BBWAA ballot and capturing every
candidate who received a vote, while the Lahman dataset serves as an
official repository that may prioritize specific voting groups or omit
certain entries. This discrepancy can be seen in the raw counts: the
scraped dataset has a larger list of candidates, while the Lahman
records represent a more curated subset. Additionally, variations in the
number of inducted players appear between the two, stemming from
differences in recording protocols. Ultimately, this comparison
demonstrates that datasets describing the same phenomenon can differ
significantly in scope and structure, reinforcing the necessity of
thoroughly auditing sources during the data cleaning and integration
process.
