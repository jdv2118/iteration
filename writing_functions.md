Writing Functions
================

## Z-scores!!

Let’s compute the z-score version of a list of numbers.

``` r
x_vec = rnorm(25, mean = 7, sd = 4)
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.47828758 -1.89411065 -1.18018254  0.29961216 -2.08744966  1.44397538
    ##  [7]  0.13813409  1.37660189 -0.50002107 -0.08419633  1.54385172 -0.19288753
    ## [13] -0.18845845  0.59823306  0.20606673 -0.14732893  1.39322562  0.96623539
    ## [19] -1.40572099 -0.88624340  1.04256081 -0.12157151 -0.53002832 -0.41189052
    ## [25]  0.14330546

Suppose you want to do this often.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Z scores only work for numbers")
  }
  
  if (length(x) < 3) {
    stop("Z scores really only work if you have three or more numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  z
  
}
```

``` r
z_scores(x = x_vec)
z_scores(x = 1:10)
z_scores(x = rbinom(1000, 1, .6))
z_scores(x = 3)
z_scores(x = "my name is jeff")
```

## Let’s have multiple outputs

Let’s just get the mean and sd from the vector input.

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
mean_and_sd(x = x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  8.01  3.80

``` r
mean_and_sd(x = 1:10)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1   5.5  3.03

``` r
mean_and_sd(x = rbinom(1000, 1, .5))
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.479 0.500

## Let’s start with simulations …

``` r
x_vec = rnorm(n = 25000, mean = 17, sd = 4)
tibble(
  mean = mean(x_vec),
  sd = sd(x_vec)
)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.0  4.01

Can I do this using a function … YUP

``` r
sim_mean_sd = function(n_obs, mu = 7, sigma = 4) {
  
  x = rnorm(n = n_obs, mean = mu, sd = sigma)
  tibble(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
  
}
```

does it work?

``` r
sim_mean_sd(mu = 2500, n_obs = 10, 7)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1  2501.      5.65

## Fixing bad stuff

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"
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
  str_trim()
reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Let’s write a function to get reviews.

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

Let’s try with a URL

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=5"
read_page_reviews(url)
```

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

What good does this do?

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
```
