# Describe the data



Describe the dataset


```r
dat <- read_rds("data/players.rds")
titles <- read_rds("data/titles.rds")
dat <- left_join(titles, dat)
total <- dat %>% 
  group_by(Date) %>% 
  summarise(Players = sum(Players, na.rm = TRUE))
```

## Features

Numbers of features


```r
titles %>% count(`Loot Boxes` = LB)
```

```
## # A tibble: 2 x 2
##   `Loot Boxes`     n
##   <fct>        <int>
## 1 No             406
## 2 Yes             94
```

```r
titles %>% count(Multiplayer = MP)
```

```
## # A tibble: 2 x 2
##   Multiplayer     n
##   <fct>       <int>
## 1 No            155
## 2 Yes           345
```

```r
titles %>% count(COOP)
```

```
## # A tibble: 2 x 2
##   COOP      n
##   <fct> <int>
## 1 No      188
## 2 Yes     312
```

```r
titles %>% count(MP, LB)
```

```
## # A tibble: 4 x 3
##   MP    LB        n
##   <fct> <fct> <int>
## 1 No    No      145
## 2 No    Yes      10
## 3 Yes   No      261
## 4 Yes   Yes      84
```

```r
titles %>% 
  count(Multiplayer = MP, COOP, `Loot Boxes` = LB) %>% 
  arrange(n) %>% 
  mutate(p = scales::percent(n / sum(n), accuracy = 1))
```

```
## # A tibble: 6 x 5
##   Multiplayer COOP  `Loot Boxes`     n p    
##   <fct>       <fct> <fct>        <int> <chr>
## 1 Yes         No    Yes              6 1%   
## 2 No          No    Yes             10 2%   
## 3 Yes         No    No              27 5%   
## 4 Yes         Yes   Yes             78 16%  
## 5 No          No    No             145 29%  
## 6 Yes         Yes   No             234 47%
```

## Titles

How many titles are there per day


```r
dat %>% 
  group_by(Date) %>% 
  summarise(titles = length(unique(appid))) %>% 
  ggplot(aes(Date, titles)) +
  geom_point()
```

<img src="02-Describe_files/figure-html/unnamed-chunk-3-1.png" width="672" />

## Total volume


```r
total %>% 
  ggplot(aes(Date, Players)) +
  geom_line()
```

<img src="02-Describe_files/figure-html/unnamed-chunk-4-1.png" width="672" />

