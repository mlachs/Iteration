Writing Functions
================
Miriam Lachs
2024-10-24

## Writing my first function

as an example here is a z-score computatioon

``` r
x_vec = rnorm(n=25, mean = 10, sd=3.5)

(x_vec-mean(x_vec))/sd(x_vec)
```

    ##  [1]  1.111079907 -0.053890062  0.183395340 -0.600115154 -0.276288366
    ##  [6]  0.438624367  0.064807997 -1.166702618  0.724155141 -1.678306271
    ## [11]  0.563370671  0.452389076 -0.858210620  2.065025233  1.043831152
    ## [16] -1.015270408  0.294458328 -0.009250749  0.329407503 -0.037793146
    ## [21] -1.776751943  1.572754747 -0.749467104 -1.569301204  0.948048181

Now I’ll write a function to do this

``` r
z_scores = function(x){
  if (!is.numeric(x)){
    stop("x needs to be numeric")
  }
  if (length(x)<5){
    stop('you need at least 5 numbers to compute the z')
  }
  z= (x - mean(x))/sd(x)
  return(z)
}

z_scores(x=x_vec)
```

    ##  [1]  1.111079907 -0.053890062  0.183395340 -0.600115154 -0.276288366
    ##  [6]  0.438624367  0.064807997 -1.166702618  0.724155141 -1.678306271
    ## [11]  0.563370671  0.452389076 -0.858210620  2.065025233  1.043831152
    ## [16] -1.015270408  0.294458328 -0.009250749  0.329407503 -0.037793146
    ## [21] -1.776751943  1.572754747 -0.749467104 -1.569301204  0.948048181

does this always work?

``` r
z_scores(x=3)
```

    ## Error in z_scores(x = 3): you need at least 5 numbers to compute the z

``` r
z_scores(x=c('my','name','is','miriam'))
```

    ## Error in z_scores(x = c("my", "name", "is", "miriam")): x needs to be numeric

## a new function

``` r
mean_and_sd = function(x){
  mean_x=mean(x)
  sd_x=sd(x)
  
  outdf=
    tibble(
      mean=mean_x,
      sd= sd_x
    )
  return(outdf)
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.22  3.00

## Check stuff using a simulation

``` r
sim_df=
  tibble(
    x=rnorm(30,10,5)
  )

sim_df %>% 
  summarise(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.6  3.93

simulation function to check sample mean and sd

``` r
sim_mean_sd = function(sample_size, true_mean,ture_sd){
  sim_df=
  tibble(
    x=rnorm(sample_size,true_mean,ture_sd)
  )

out_df=sim_df %>% 
  summarise(
    mean = mean(x),
    sd = sd(x))

return(out_df)
}

sim_mean_sd(30,4,12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.10  13.1

## revisit LotR

``` r
fellowship_df=
  read_excel('LotR_Words.xlsx',range = "B3:D6") %>% 
  mutate(movie='fellowship')

two_towers_df=
  read_excel('LotR_Words.xlsx',range = "F3:H6") %>% 
  mutate(movie='two_towers')

return_king_df=
  read_excel('LotR_Words.xlsx',range = "J3:L6") %>% 
  mutate(movie='return_king')
```

``` r
lotr_reader= function(movie,range){
  out_df=read_excel('LotR_Words.xlsx',range = range) %>% 
  mutate(movie=movie) %>% 
  janitor::clean_names()%>% 
    pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words") |> 
  mutate(race = str_to_lower(race))
  
  return(out_df)
}

lotr_reader('fellowship',"B3:D6")
```

    ## # A tibble: 6 × 4
    ##   race   movie      sex    words
    ##   <chr>  <chr>      <chr>  <dbl>
    ## 1 elf    fellowship female  1229
    ## 2 elf    fellowship male     971
    ## 3 hobbit fellowship female    14
    ## 4 hobbit fellowship male    3644
    ## 5 man    fellowship female     0
    ## 6 man    fellowship male    1995

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) %>% 
  mutate(drug= 'marj')
```

``` r
drug_import= function(drug, tablenum,html){
  drug_df = 
  html |> 
  html_table() |> 
  nth(tablenum) |>
  slice(-1) %>% 
  mutate(drug= drug)
  return(drug_df)
}

drug_import('cocaine',4,nsduh_html)
```

    ## # A tibble: 56 × 17
    ##    State     `12+(2013-2014)` `12+(2014-2015)` `12+(P Value)` `12-17(2013-2014)`
    ##    <chr>     <chr>            <chr>            <chr>          <chr>             
    ##  1 Total U.… 1.66a            1.76             0.040          0.60              
    ##  2 Northeast 1.94a            2.18             0.012          0.60              
    ##  3 Midwest   1.37             1.43             0.282          0.48              
    ##  4 South     1.45b            1.56             0.067          0.53              
    ##  5 West      2.03             2.05             0.816          0.82              
    ##  6 Alabama   1.23             1.22             0.995          0.42              
    ##  7 Alaska    1.54a            2.00             0.010          0.51              
    ##  8 Arizona   2.25             2.29             0.861          1.01              
    ##  9 Arkansas  0.93             1.07             0.208          0.41              
    ## 10 Californ… 2.14             2.16             0.883          0.89              
    ## # ℹ 46 more rows
    ## # ℹ 12 more variables: `12-17(2014-2015)` <chr>, `12-17(P Value)` <chr>,
    ## #   `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>, `18-25(P Value)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `26+(P Value)` <chr>,
    ## #   `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>, `18+(P Value)` <chr>,
    ## #   drug <chr>
