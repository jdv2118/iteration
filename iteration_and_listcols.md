Iteration and list columns
================

## Lists

``` r
vec_numeric = 5:8
vec_logical = c(TRUE, FALSE, TRUE, TRUE)
```

Lets look at a list

``` r
l = list(
  vec_numeric = 5:8,
  mat         = matrix(1:8, 2, 4),
  vec_logical = c(TRUE, FALSE),
  summary     = summary(rnorm(1000))
)
```

Accessing list items

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[3]]
```

    ## [1]  TRUE FALSE

``` r
l[["mat"]]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

## Loops!

Let’s write a `for` loop to take the mean and SD of four samples from a
normal distribution

``` r
list_norm = 
  list(
    a = rnorm(20, 5, 4),
    b = rnorm(20, -12, 3),
    c = rnorm(20, 17, .4),
    d = rnorm(20, 100, 1)
  )
```

Here’s my function

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Z scores only work for numbers")
  }
  
  if (length(x) < 3) {
    stop("Z scores really only work if you have three or more numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

Let’s try to make this work.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.26  3.99

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -13.0  3.22

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.1 0.241

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  100. 0.628

Let’s a `for` loop instead.

``` r
output = vector("list", length = 4)
for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
}
output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.26  3.99
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -13.0  3.22
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.1 0.241
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  100. 0.628

## can we map??

we can map!!

``` r
map(list_norm, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.26  3.99
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -13.0  3.22
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.1 0.241
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  100. 0.628

so … what about other functions?

``` r
map(list_norm, summary)
```

    ## $a
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.3986  0.7949  5.2422  4.2633  7.2869  9.5806 
    ## 
    ## $b
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -18.546 -14.849 -13.549 -12.964 -11.248  -3.343 
    ## 
    ## $c
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   16.70   16.94   17.16   17.13   17.30   17.65 
    ## 
    ## $d
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   99.25   99.71  100.09  100.16  100.58  101.74

map variants …

``` r
map_dbl(list_norm, median)
```

    ##          a          b          c          d 
    ##   5.242225 -13.548538  17.155902 100.091415

``` r
output = map_df(list_norm, mean_and_sd)
```

## list columns …

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    norm = list_norm
  )
listcol_df[["norm"]]
```

    ## $a
    ##  [1]  5.6452483  4.8581041  9.5806257  5.6263453 -0.9667442  8.6021220
    ##  [7] -1.8391810  7.1180702  2.1168555  4.4924154 -0.1616984  8.0635236
    ## [13]  1.1137799  4.6130002  7.7935215 -2.3986168  9.0800602  7.0393040
    ## [19]  6.6834426 -1.7945595
    ## 
    ## $b
    ##  [1] -14.820369 -10.977590 -16.041017 -14.652616 -14.934455 -15.913715
    ##  [7] -11.338440 -13.250693  -9.443469 -10.900288 -13.539021 -14.219816
    ## [13]  -3.343110 -11.711614 -18.546122 -11.392422 -13.558056 -10.597074
    ## [19] -14.355029 -15.736270
    ## 
    ## $c
    ##  [1] 17.01548 17.27026 16.97643 16.94618 17.29893 17.37325 17.22828 17.03648
    ##  [9] 17.35917 17.22988 16.93501 17.64988 16.84262 16.78116 17.17829 17.13351
    ## [17] 17.41250 16.93793 16.70321 17.29536
    ## 
    ## $d
    ##  [1]  99.44040  99.61173 100.76104 100.04640 100.31637 100.13643  99.25006
    ##  [8] 101.73832 100.41641 100.86781 100.93001  99.54740 100.74835  99.81027
    ## [15]  99.73946  99.51185 100.20860  99.88038  99.79559 100.52605

``` r
output = map(listcol_df[["norm"]], mean_and_sd)
```

can we add list columns, and then what

``` r
listcol_df %>% 
  mutate(
    m_sd = map(norm, mean_and_sd)
  ) %>% 
  select(-norm)
```

    ## # A tibble: 4 × 2
    ##   name  m_sd            
    ##   <chr> <named list>    
    ## 1 a     <tibble [1 × 2]>
    ## 2 b     <tibble [1 × 2]>
    ## 3 c     <tibble [1 × 2]>
    ## 4 d     <tibble [1 × 2]>

## What about something more realistic…

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: C:\Users\Justin\AppData\Local/Cache/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2022-09-18 13:28:32 (8.418)

    ## file min/max dates: 1869-01-01 / 2022-09-30

    ## using cached file: C:\Users\Justin\AppData\Local/Cache/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2022-09-18 13:28:42 (1.701)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: C:\Users\Justin\AppData\Local/Cache/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2022-09-18 13:28:46 (0.952)

    ## file min/max dates: 1999-09-01 / 2022-09-30

let’s nest within weather stations…

``` r
weather_nest_df = 
  weather_df %>% 
  nest(data = date:tmin)
```

Really is a list column!

``` r
weather_nest_df[["data"]]
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

``` r
weather_nest_df[["data"]][[1]]
```

    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows

``` r
lm(tmax ~ tmin, data = weather_nest_df[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
lm(tmax ~ tmin, data = weather_nest_df[["data"]][[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509

``` r
lm(tmax ~ tmin, data = weather_nest_df[["data"]][[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

Let’s write a short lil ol function

``` r
weather_lm = function(df) {
  
  lm(tmax ~ tmin, data = df)
  
}
weather_lm(weather_nest_df[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nest_df[["data"]], weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

Can i do all this in a tidy way

``` r
weather_nest_df %>% 
  mutate(
    model = map(data, weather_lm)
  )
```

    ## # A tibble: 3 × 4
    ##   name           id          data               model 
    ##   <chr>          <chr>       <list>             <list>
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>  
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>  
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

YUP

unnesting

``` r
weather_nest_df %>% 
  unnest(data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

## Napoleon!!

Here’s my scraping function that works for a single page

``` r
read_page_reviews = function(url) {
  
  dynamite_html = read_html(url)
  review_titles = 
    dynamite_html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    dynamite_html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    dynamite_html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim() %>% 
    str_subset("The media could not be loaded.", negate = TRUE) %>% 
    str_subset("^$", negate = TRUE)
  
  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
    )
  
  reviews
  
}
```

What we did last time:

``` r
base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_url = str_c(base_url, c(1, 2, 3, 4, 5))
dynamite_reviews = 
  bind_rows(
    read_page_reviews(vec_url[1]),
    read_page_reviews(vec_url[2]),
    read_page_reviews(vec_url[3]),
    read_page_reviews(vec_url[4]),
    read_page_reviews(vec_url[5])
  )
map(vec_url, read_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 × 3
    ##    title                                      stars text                        
    ##    <chr>                                      <dbl> <chr>                       
    ##  1 Goofy movie                                    5 "I used this movie for a mi…
    ##  2 Lol hey it’s Napoleon. What’s not to love…     5 "Vote for Pedro"            
    ##  3 Still the best                                 5 "Completely stupid, absolut…
    ##  4 70’s and 80’s Schtick Comedy                   5 "…especially funny if you h…
    ##  5 Amazon Censorship                              5 "I hope Amazon does not cen…
    ##  6 Watch to say you did                           3 "I know it's supposed to be…
    ##  7 Best Movie Ever!                               5 "We just love this movie an…
    ##  8 Quirky                                         5 "Good family film"          
    ##  9 Funny movie - can't play it !                  1 "Sony 4k player won't even …
    ## 10 A brilliant story about teenage life           5 "Napoleon Dynamite delivers…
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                                           stars text                   
    ##    <chr>                                           <dbl> <chr>                  
    ##  1 "HUHYAH"                                            5 Spicy                  
    ##  2 "Cult Classic"                                      4 Takes a time or two to…
    ##  3 "Sweet"                                             5 Timeless Movie. My Gra…
    ##  4 "Cute"                                              4 Fun                    
    ##  5 "great collectible"                                 5 one of the greatest mo…
    ##  6 "Iconic, hilarious flick ! About friend ship ."     5 Who doesn’t love this …
    ##  7 "Funny"                                             5 Me and my dad watched …
    ##  8 "Low budget but okay"                               3 This has been a classi…
    ##  9 "Disappointing"                                     2 We tried to like this,…
    ## 10 "Favorite movie \U0001f37f"                         5 This is one of my favo…
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                                             stars text 
    ##    <chr>                                                             <dbl> <chr>
    ##  1 none                                                                  5 "thi…
    ##  2 Great movie                                                           5 "Vot…
    ##  3 Get this to improve your nunchuck and bowstaff skills. Dancing i…     5 "Got…
    ##  4 Incredible Movie                                                      5 "Fun…
    ##  5 Always loved this movie!                                              5 "I h…
    ##  6 Great movie                                                           5 "Bou…
    ##  7 The case was damaged                                                  3 "It …
    ##  8 It’s classic                                                          5 "Cle…
    ##  9 Irreverent comedy                                                     5 "If …
    ## 10 Great classic!                                                        5 "Fun…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                                             stars text 
    ##    <chr>                                                             <dbl> <chr>
    ##  1 Most Awesomsomest Movie EVER!!!                                       5 "Thi…
    ##  2 Always a favorite                                                     5 "I r…
    ##  3 It’s not working the disc keeps showing error when I tried other…     1 "It’…
    ##  4 Gosh!                                                                 5 "Eve…
    ##  5 An Acquired Taste                                                     1 "Thi…
    ##  6 What is this ?                                                        4 "Nic…
    ##  7 Napoleon Dynamite                                                     2 "I w…
    ##  8 Great movie                                                           5 "Gre…
    ##  9 Good movie                                                            5 "Goo…
    ## 10 Came as Described                                                     5 "Cam…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                                                           stars text   
    ##    <chr>                                                           <dbl> <chr>  
    ##  1 Oddly on my list of keepers.                                        5 "Good …
    ##  2 Low budget fun                                                      5 "Oddba…
    ##  3 On a scale of 1 to 10 this rates a minus                            1 "This …
    ##  4 I always wondered...                                                5 "what …
    ##  5 Audio/video not synced                                              1 "I tho…
    ##  6 Kind of feels like only a bully would actually laugh at this...     1 "...as…
    ##  7 movie                                                               5 "good …
    ##  8 An Overdose of Comical Cringe                                       5 "Excel…
    ##  9 Glad I never wasted money on this                                   2 "I rem…
    ## 10 A little disappointed                                               3 "The c…

``` r
napoleon_reviews = 
  tibble(
    page = 1:5,
    page_url = str_c(base_url, page)
  ) %>% 
  mutate(
    reviews = map(page_url, read_page_reviews)
  )
napoleon_reviews %>% 
  select(-page_url) %>% 
  unnest(reviews)
```

    ## # A tibble: 50 × 4
    ##     page title                                      stars text                  
    ##    <int> <chr>                                      <dbl> <chr>                 
    ##  1     1 Goofy movie                                    5 "I used this movie fo…
    ##  2     1 Lol hey it’s Napoleon. What’s not to love…     5 "Vote for Pedro"      
    ##  3     1 Still the best                                 5 "Completely stupid, a…
    ##  4     1 70’s and 80’s Schtick Comedy                   5 "…especially funny if…
    ##  5     1 Amazon Censorship                              5 "I hope Amazon does n…
    ##  6     1 Watch to say you did                           3 "I know it's supposed…
    ##  7     1 Best Movie Ever!                               5 "We just love this mo…
    ##  8     1 Quirky                                         5 "Good family film"    
    ##  9     1 Funny movie - can't play it !                  1 "Sony 4k player won't…
    ## 10     1 A brilliant story about teenage life           5 "Napoleon Dynamite de…
    ## # … with 40 more rows
