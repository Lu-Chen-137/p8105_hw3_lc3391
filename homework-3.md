p8105\_hw3\_lc3391
================
Lu Chen
10/13/2019

Problem 1
=========

Importing dataset
-----------------

``` r
library(p8105.datasets)
data("instacart")

str(instacart)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1384617 obs. of  15 variables:
    ##  $ order_id              : int  1 1 1 1 1 1 1 1 36 36 ...
    ##  $ product_id            : int  49302 11109 10246 49683 43633 13176 47209 22035 39612 19660 ...
    ##  $ add_to_cart_order     : int  1 2 3 4 5 6 7 8 1 2 ...
    ##  $ reordered             : int  1 1 0 0 1 0 0 1 0 1 ...
    ##  $ user_id               : int  112108 112108 112108 112108 112108 112108 112108 112108 79431 79431 ...
    ##  $ eval_set              : chr  "train" "train" "train" "train" ...
    ##  $ order_number          : int  4 4 4 4 4 4 4 4 23 23 ...
    ##  $ order_dow             : int  4 4 4 4 4 4 4 4 6 6 ...
    ##  $ order_hour_of_day     : int  10 10 10 10 10 10 10 10 18 18 ...
    ##  $ days_since_prior_order: int  9 9 9 9 9 9 9 9 30 30 ...
    ##  $ product_name          : chr  "Bulgarian Yogurt" "Organic 4% Milk Fat Whole Milk Cottage Cheese" "Organic Celery Hearts" "Cucumber Kirby" ...
    ##  $ aisle_id              : int  120 108 83 83 95 24 24 21 2 115 ...
    ##  $ department_id         : int  16 16 4 4 15 4 4 16 16 7 ...
    ##  $ aisle                 : chr  "yogurt" "other creams cheeses" "fresh vegetables" "fresh vegetables" ...
    ##  $ department            : chr  "dairy eggs" "dairy eggs" "produce" "produce" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   order_id = col_integer(),
    ##   ..   product_id = col_integer(),
    ##   ..   add_to_cart_order = col_integer(),
    ##   ..   reordered = col_integer(),
    ##   ..   user_id = col_integer(),
    ##   ..   eval_set = col_character(),
    ##   ..   order_number = col_integer(),
    ##   ..   order_dow = col_integer(),
    ##   ..   order_hour_of_day = col_integer(),
    ##   ..   days_since_prior_order = col_integer(),
    ##   ..   product_name = col_character(),
    ##   ..   aisle_id = col_integer(),
    ##   ..   department_id = col_integer(),
    ##   ..   aisle = col_character(),
    ##   ..   department = col_character()
    ##   .. )

### Short description of the dataset

-   In the dataset instacart, there are 1384617 observations of 15 variables. It composes with both numeric and categorical data. Some key variables are 'aisle', 'reordered', 'product name' and so on. One example is that the Asian Chopped Salad with Dressing with product id 18394 located on aisle 123, Packaged Vegetables and Fruits, from department 4, Produce, is reordered once by customer with user id 182389.

Summarize the dataset
---------------------

``` r
instacart %>% 
  group_by(aisle) %>% 
  summarize(n_aisle=n()) %>% 
  arrange(desc(n_aisle))
```

    ## # A tibble: 134 x 2
    ##    aisle                         n_aisle
    ##    <chr>                           <int>
    ##  1 fresh vegetables               150609
    ##  2 fresh fruits                   150473
    ##  3 packaged vegetables fruits      78493
    ##  4 yogurt                          55240
    ##  5 packaged cheese                 41699
    ##  6 water seltzer sparkling water   36617
    ##  7 milk                            32644
    ##  8 chips pretzels                  31269
    ##  9 soy lactosefree                 26240
    ## 10 bread                           23635
    ## # … with 124 more rows

``` r
#little comments: there are 134 aisles, and most intems are ordered from the aisle Fresh Vegetables.

instacart %>% 
  group_by(aisle) %>% 
  summarize(n_aisle=n()) %>% 
  filter(n_aisle > 10000) %>% 
  arrange(aisle) %>% 
  ggplot(aes(x = aisle, y = n_aisle)) +
  geom_point() +
  labs(
    title = "Items ordered in aisle",
    x = "Name of aisle",
    y = "Number of items ordered from the aisle",
    caption = "Data from the p8105 package"
  ) 
```

![](homework-3_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
#Little comments: there are many aisles with more than 10000 items ordered, and the top 3 are: fresh veges, fresh fruits, and packaged vege and fruits.

instacart %>% 
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>% 
  group_by(aisle, product_name) %>% 
  summarize(sum_orders = sum(order_number)) %>% 
  mutate(order_rank = min_rank(desc(sum_orders))) %>%
  filter(order_rank %in% c(1,2,3)) %>%
  arrange(aisle, order_rank) %>% 
  knitr::kable()
```

| aisle                      | product\_name                                 |  sum\_orders|  order\_rank|
|:---------------------------|:----------------------------------------------|------------:|------------:|
| baking ingredients         | Light Brown Sugar                             |         8605|            1|
| baking ingredients         | Cane Sugar                                    |         6244|            2|
| baking ingredients         | Organic Vanilla Extract                       |         6003|            3|
| dog food care              | Standard Size Pet Waste bags                  |          675|            1|
| dog food care              | Beef Stew Canned Dog Food                     |          631|            2|
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |          589|            3|
| packaged vegetables fruits | Organic Baby Spinach                          |       171301|            1|
| packaged vegetables fruits | Organic Raspberries                           |       113932|            2|
| packaged vegetables fruits | Organic Blueberries                           |        86765|            3|

``` r
#little comments: The three most popular items in baking ingredients aisle are Light Brown Sugar, Cane Sugar, and Organic Vanilla Extract; that in dog food care aisle are Standard Size Pet Waste Bags, Beef Stew Canned Dog Food, and Snack Sticks Chicken & Rice Recipe Dog Treats; that in packaged vegetables fruits aisle are Organic Baby Spainach, Organic Raspberries and Organic Blueberries.

instacart %>% 
  mutate(order_dow = recode(order_dow, "0" = "Sunday", "1" = "Monday", "2" = "Tuesday", "3" = "Wednesday", 
                            "4" = "Thursday", "5" = "Friday", "6" = "Saturday")) %>% 
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>% 
  group_by(product_name, order_dow) %>% 
  summarize(mean_hour_of_day = mean(order_hour_of_day)) %>%
  pivot_wider(names_from = order_dow, values_from = mean_hour_of_day) %>% 
  knitr::kable()
```

| product\_name    |    Friday|    Monday|  Saturday|    Sunday|  Thursday|   Tuesday|  Wednesday|
|:-----------------|---------:|---------:|---------:|---------:|---------:|---------:|----------:|
| Coffee Ice Cream |  12.26316|  14.31579|  13.83333|  13.77419|  15.21739|  15.38095|   15.31818|
| Pink Lady Apples |  12.78431|  11.36000|  11.93750|  13.44118|  11.55172|  11.70213|   14.25000|

``` r
#little comments: The mean order hour of the day for both products are concentrated on noon to afternoon (about 11:00 to 15:00).
```

Problem 2
---------

### Importing and cleaning the dataset

``` r
library(p8105.datasets)
data("brfss_smart2010")

brfss_smart2010 = 
  brfss_smart2010 %>% 
  janitor::clean_names() %>% 
  filter(topic %in% c("Overall Health"), response %in% c("Excellent","Very good", "Good", "Fair", "Poor"))
```

### Summarize the dataset

``` r
brfss_smart2010 %>% 
  filter(year %in% c("2002")) %>% 
  group_by(locationabbr, locationdesc) %>% 
  summarize(n_loc = n()) %>% 
  group_by(locationabbr) %>% 
  summarize(n_loc = n()) %>% 
  filter(n_loc >= 7)
```

    ## # A tibble: 6 x 2
    ##   locationabbr n_loc
    ##   <chr>        <int>
    ## 1 CT               7
    ## 2 FL               7
    ## 3 MA               8
    ## 4 NC               7
    ## 5 NJ               8
    ## 6 PA              10

``` r
#little comments: 6 states were observed at 7 or more locations, and they are CT, FL, MA, NC, NJ, and PA.

brfss_smart2010 %>% 
  filter(year %in% c("2010")) %>% 
  group_by(locationabbr, locationdesc) %>% 
  summarize(n_loc = n()) %>% 
  group_by(locationabbr) %>% 
  summarize(n_loc = n()) %>% 
  filter(n_loc >= 7)
```

    ## # A tibble: 14 x 2
    ##    locationabbr n_loc
    ##    <chr>        <int>
    ##  1 CA              12
    ##  2 CO               7
    ##  3 FL              41
    ##  4 MA               9
    ##  5 MD              12
    ##  6 NC              12
    ##  7 NE              10
    ##  8 NJ              19
    ##  9 NY               9
    ## 10 OH               8
    ## 11 PA               7
    ## 12 SC               7
    ## 13 TX              16
    ## 14 WA              10

``` r
#little comments: there are 14 states were observed at 7 or more locations, and they are CA, CO, FL, MA, MD, NC, NE, NJ, NY, OH, PA, SC, TX, and WA.
```
