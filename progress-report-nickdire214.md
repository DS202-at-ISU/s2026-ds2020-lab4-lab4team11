progress-report-nickdire214
================
Dominic DiRe

``` r
library(rvest)
library(dplyr)
library(readr)
library(Lahman)
library(ggplot2)
```

# Scrape the 2026 data

We read the HTML from the Baseball Reference 2026 HOF page and extract
all tables.

``` r
url <- "https://www.baseball-reference.com/awards/hof_2026.shtml"
supersecretsite <- read_html(url)
tables <- html_table(supersecretsite)
```

``` r
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
    ## # A tibble: 2 × 40
    ##   ``    ``       ``    ``    ``    ``    ``    ``    ``    ``    `Batting Stats`
    ##   <chr> <chr>    <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>          
    ## 1 Rk    Name     Indu… HOFm  HOFs  Yrs   WAR   WAR7  JAWS  Jpos  G              
    ## 2 1     Jeff Ke… as P… 123   51    17    55.4  35.8  45.6  56.6  2298           
    ## # ℹ 29 more variables: `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Batting Stats` <chr>, `Batting Stats` <chr>,
    ## #   `Batting Stats` <chr>, `Pitching Stats` <chr>, `Pitching Stats` <chr>,
    ## #   `Pitching Stats` <chr>, `Pitching Stats` <chr>, `Pitching Stats` <chr>,
    ## #   `Pitching Stats` <chr>, `Pitching Stats` <chr>, `Pitching Stats` <chr>, …

Good comms on the row 1 being wack. The first row is the names of the
columns.

``` r
data <- tables[[1]] # pull
actual_col_names <- data[1, ] # set
colnames(data) <- actual_col_names # put
data <- data[-1, ] # scrap
head(data, 3) # money
```

    ## # A tibble: 3 × 39
    ##   Rk    Name       YoB   Votes `%vote` HOFm  HOFs  Yrs   WAR   WAR7  JAWS  Jpos 
    ##   <chr> <chr>      <chr> <chr> <chr>   <chr> <chr> <chr> <chr> <chr> <chr> <chr>
    ## 1 1     Carlos Be… 4th   358   84.2%   126   50    20    70.0  44.4  57.2  58.0 
    ## 2 2     Andruw Jo… 9th   333   78.4%   109   32    17    62.7  46.4  54.6  58.0 
    ## 3 3     Chase Utl… 3rd   251   59.1%   94    36    16    64.6  49.3  56.9  56.6 
    ## # ℹ 27 more variables: G <chr>, AB <chr>, R <chr>, H <chr>, HR <chr>,
    ## #   RBI <chr>, SB <chr>, BB <chr>, BA <chr>, OBP <chr>, SLG <chr>, OPS <chr>,
    ## #   `OPS+` <chr>, W <chr>, L <chr>, ERA <chr>, `ERA+` <chr>, WHIP <chr>,
    ## #   G <chr>, GS <chr>, SV <chr>, IP <chr>, H <chr>, HR <chr>, BB <chr>,
    ## #   SO <chr>, `Pos Summary` <chr>

# Cleaning data

Select the needed cols, convert num vars, take the “-X” out ,put
“inducted” on 75%.

``` r
hof_2026 <- data %>%
  select(Name, Votes, `%vote`) %>%
  mutate(
    Name = gsub("X-", "", Name),
    yearID = 2026,
    Votes = as.numeric(Votes),
    percent_vote = readr::parse_number(`%vote`),
    inducted = ifelse(percent_vote >= 75, "Y", "N")
  ) %>%
  rename(name = Name)

hof_2026
```

    ## # A tibble: 27 × 6
    ##    name            Votes `%vote` yearID percent_vote inducted
    ##    <chr>           <dbl> <chr>    <dbl>        <dbl> <chr>   
    ##  1 Carlos Beltrán    358 84.2%     2026         84.2 Y       
    ##  2 Andruw Jones      333 78.4%     2026         78.4 Y       
    ##  3 Chase Utley       251 59.1%     2026         59.1 N       
    ##  4 Andy Pettitte     206 48.5%     2026         48.5 N       
    ##  5 Félix Hernández   196 46.1%     2026         46.1 N       
    ##  6 Alex Rodriguez    170 40.0%     2026         40   N       
    ##  7 Manny Ramirez     165 38.8%     2026         38.8 N       
    ##  8 Bobby Abreu       131 30.8%     2026         30.8 N       
    ##  9 Jimmy Rollins     108 25.4%     2026         25.4 N       
    ## 10 Cole Hamels       101 23.8%     2026         23.8 N       
    ## # ℹ 17 more rows

# Exporting cleaned (mine not used but this is if)

``` r
# readr::write_csv(hof_2026, "HallOfFame_2026.csv")
```

# Compare with the Lahman

Filter Lahman to 2026 and compare counts

``` r
lahman_2026 <- HallOfFame %>%
  filter(yearID == 2026)

hof_2026 %>%
  count(inducted)
```

    ## # A tibble: 2 × 2
    ##   inducted     n
    ##   <chr>    <int>
    ## 1 N           25
    ## 2 Y            2

``` r
lahman_2026 %>%
  count(votedBy, inducted)
```

    ##                     votedBy inducted n
    ## 1 Contemporary Baseball Era        N 7
    ## 2 Contemporary Baseball Era        Y 1

# Conclusion

The scraped baseball dataset and the Lahman version talk about the same
thing but are not presented the same. You kinda expect differences with
this kinda thing, but I was surprised the Lahman data only included
people voted by the Contemporary Baseball Era

The scraped dataset overall shows every canidate who received one or
more hall of fame vote. The Lahman dataset has a “votedBy” column which
is not in the scraped data at all. Which like I said above inclueded one
group of voting. The scraped data had 25 canidates and 2 above 75%
(labeled inducted), whereas Lahman had 8 canidates and 1 inducted.
