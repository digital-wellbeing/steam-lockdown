# GAM




```r
# Plotting options
theme_set(
  theme_linedraw() + theme(panel.grid = element_blank())
)
# Modelling options (cores, directory to save model files)
ncores <- parallel::detectCores(logical = FALSE)
options(mc.cores = ncores, loo.cores = ncores)
dir.create("models", FALSE)
# knitr options
opts_chunk$set(echo = TRUE, message = FALSE)
```

GAM


```r
dat <- left_join(
  read_rds("data/titles.rds"), 
  read_rds("data/players.rds")
)

# Add useful time indicators
cd <- as.Date("2020-03-11")
dat <- dat %>% 
  mutate(
    year = factor(year(Date)),
    ymonth = month(Date, label = FALSE) - 1,
    yweek = week(Date) - 1,
    yday = yday(Date) - 1,
    wday = wday(Date, week_start = 1, label = FALSE) - 1, 
    wend = factor(as.numeric(wday %in% c(5, 6)), labels = c("No", "Yes"))
  )
# Limit both years to last yday in 2020
dat <- dat %>% filter(yday <= max(yday[year==2020]))
group_by(dat, year) %>% 
  summarise(max(Date))
```

```
## # A tibble: 2 x 2
##   year  `max(Date)`
##   <fct> <date>     
## 1 2019  2019-07-15 
## 2 2020  2020-07-14
```

## Model totals


```r
total <- dat %>% 
  group_by(across(c(Date, year:wend))) %>% 
  summarise(
    Players = sum(Players, na.rm = TRUE), 
    ngames = length(unique(appid))
  ) %>% 
  ungroup()
years <- total %>% 
  group_by(year) %>% 
  nest()
```

### mgcv


```r
years <- years %>% 
  mutate(
    m0 = map(data, ~gamm(
      Players ~ t2(yday, k = 27) + t2(wday, k = 7),
      data = .x,
      method = "REML"
    )),
    m1 = map(data, ~gamm(
      Players ~ t2(yday, wday, k = c(27, 7)),
      data = .x,
      method = "REML"
    ))
  )
anova(
  years$m0[[2]][["lme"]], 
  years$m1[[2]][["lme"]]
)
```

```
##                        Model df      AIC      BIC    logLik   Test  L.Ratio
## years$m0[[2]][["lme"]]     1  6 5162.805 5182.381 -2575.403                
## years$m1[[2]][["lme"]]     2  8 5132.825 5158.884 -2558.412 1 vs 2 33.98069
##                        p-value
## years$m0[[2]][["lme"]]        
## years$m1[[2]][["lme"]]  <.0001
```

```r
summary(years$m1[[2]][["gam"]])
```

```
## 
## Family: gaussian 
## Link function: identity 
## 
## Formula:
## Players ~ t2(yday, wday, k = c(27, 7))
## 
## Parametric coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4491388       6047   742.8   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##                 edf Ref.df    F p-value    
## t2(yday,wday) 57.02  57.02 53.4  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.974   
##   Scale est. = 7.1659e+09  n = 196
```

```r
acf(residuals(years$m1[[2]][["gam"]]))
```

<img src="03-GAM_files/figure-html/mgcv-years-1.png" width="672" />

```r
gam.check(years$m1[[2]][["gam"]])
```

<img src="03-GAM_files/figure-html/mgcv-years-2.png" width="672" />

```
## 
## 'gamm' based fit - care required with interpretation.
## Checks based on working residuals may be misleading.
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
## 
##                k' edf k-index p-value
## t2(yday,wday) 188  57    1.17       1
```

```r
vis.gam(years$m1[[1]][["gam"]], theta = 30, phi = 10)
```

<img src="03-GAM_files/figure-html/mgcv-years-3.png" width="672" />

Figure


```r
# Prediction dataframe
tmp <- years %>% 
  mutate(
    preds = map(m1, ~as.data.frame(predict(.x[["gam"]], se.fit = TRUE)))
    ) %>% 
  unnest(c(data, preds)) %>% 
  mutate(year = factor(year)) %>% 
  # Make dates same for 2019 and 2020 to put on same plot
  group_by(yday) %>% 
  mutate(Date = last(Date)) %>% 
  ungroup()
# Excess players dataframe
tmp2 <- tmp %>% 
  select(year, yweek, Players, fit) %>% 
  pivot_wider(
    names_from = year, values_from = c(Players, fit), values_fn = mean
  ) %>% 
  mutate(excess_empirical = Players_2020 - Players_2019) %>% 
  mutate(excess_model = fit_2020 - fit_2019) %>% 
  mutate(year = "2020") %>% 
  select(year, yweek, starts_with("excess"))
  # left_join(select(tmp, year, Date, yweek))
# Predictions and data figure
p1 <- tmp %>%
  ggplot(aes(Date, Players, alpha = year, group = year)) +
  # scale_x_date() +
  geom_vline(xintercept = cd, size = .1) +
  scale_alpha_manual("Year", values = c(.3, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(
    breaks = pretty_breaks(), 
    labels = function(x) str_glue("{x/1e6}M")
  ) +
  geom_ribbon(
    aes(ymin = fit-se.fit*2, ymax = fit+se.fit*2), alpha=.1
  ) +
  geom_line(aes(y=fit), size = .2) +
  geom_point(shape = 1, size = 1, fill = "white")
p1
```

<img src="03-GAM_files/figure-html/unnamed-chunk-3-1.png" width="672" />

```r
# Excess gaming figure  
p2 <- tmp2 %>% 
  ggplot(aes(x = yweek, alpha = NA)) +
  # Week of WHO announcement
  geom_vline(xintercept = floor(yday(cd) / 7), size = .1) +
  scale_x_continuous("Week of year", breaks = pretty_breaks()) +
  geom_line(aes(y = excess_model), size = .2) +
  geom_point(
    aes(y = excess_empirical), 
    shape = 1, size = 1, fill = "white"
  ) +
  geom_hline(yintercept = 0, lty = 2, size = .1) +
  scale_y_continuous(
    "Excess players\n(2020 - 2019)",
    breaks = pretty_breaks(), 
    labels = function(x) str_glue("{x/1e6}M")
  )
p2
```

```
## Warning: Using alpha for a discrete variable is not advised.
```

<img src="03-GAM_files/figure-html/unnamed-chunk-3-2.png" width="672" />

```r
(p1 + theme(axis.title.x = element_blank())) / p2 + 
  plot_layout(heights = c(.75, .25), guides = "collect")
```

```
## Warning: Using alpha for a discrete variable is not advised.
```

<img src="03-GAM_files/figure-html/unnamed-chunk-3-3.png" width="672" />

```r
ggsave("Figure1-mgcv.png", width = 8, height = 5)
```

```
## Warning: Using alpha for a discrete variable is not advised.
```

Weekend effect figure


```r
years %>%
  mutate(preds = map(m1, ~as.data.frame(predict(.x[["gam"]], se.fit = TRUE)))) %>%
  unnest(c(data, preds)) %>%
  group_by(year, yweek) %>%
  summarise(
    w_eff = mean(Players[wday %in% 5:6]) - mean(Players[wday %in% 0:4]),
    fit = mean(fit[wday %in% 5:6]) - mean(fit[wday %in% 0:4]),
  ) %>%
  mutate(year = factor(year)) %>% 
  ggplot(aes(yweek, w_eff, alpha = year, group = year)) +
  scale_x_continuous(
    "Week number",
    breaks = pretty_breaks()
  ) +
    scale_y_continuous(
    breaks = pretty_breaks(), 
    labels = function(x) str_glue("{x/1e6}M")
  ) +
  scale_alpha_manual("Year", values = c(.3, 1)) +
  geom_line(aes(y=fit)) +
  geom_point()
```

<img src="03-GAM_files/figure-html/unnamed-chunk-4-1.png" width="672" />

```r
# show it for weeks 5, 15, and 25
years %>%
  mutate(preds = map(m1, ~as.data.frame(predict(.x[["gam"]], se.fit = TRUE)))) %>%
  unnest(c(data, preds)) %>%
  filter(yweek %in% c(5, 15, 25)) %>% 
  # group_by(year, yweek, yday) %>%
  mutate(year = factor(year)) %>% 
  ggplot(aes(wday, Players)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  facet_grid(year~yweek)
```

<img src="03-GAM_files/figure-html/unnamed-chunk-4-2.png" width="672" />

### brms


```r
sampler_0 <- brm(
  Players ~ t2(yday, k = 27) + t2(wday, k = 7),
  # prior = prior(normal(4.5e6, 5e5), class = "Intercept"),
  data = filter(total, year==2020),
  chains = 0,
  file = "models/brm-sampler-0"
)
sampler_1 <- brm(
  Players ~ t2(yday, wday, k = c(27, 7)),
  # prior = prior(normal(4.5e6, 1e5), class = "Intercept"),
  data = filter(total, year==2020),
  chains = 0,
  file = "models/brm-sampler-1"
)
sampler_2 <- brm(
  Players ~ t2(yday, wday, k = c(27, 7)) + ar(time = yday, p = 1),
  # prior = prior(normal(4.5e6, 1e5), class = "Intercept"),
  data = filter(total, year==2020),
  chains = 0,
  file = "models/brm-sampler-2"
)
# This model has a hard time converging so we set inits for the intercept
years <- years %>% 
  mutate(
    bm0 = map2(data, year, ~update(
      sampler_0,
      newdata = .x,
      chains = 4,
      iter = 4000,
      inits = function() {list(Intercept = 4.5e6)},
      control = list(adapt_delta = .99),
      file = str_glue("models/brm-{.y}-m0")
    )),
    bm1 = map2(data, year, ~update(
      sampler_1,
      newdata = .x,
      chains = 4,
      iter = 4000,
      inits = function() {list(Intercept = 4.5e6)},
      control = list(adapt_delta = .99),
      file = str_glue("models/brm-{.y}-m1")
    )),
    bm2 = map2(data, year, ~update(
      sampler_2,
      newdata = .x,
      chains = 4,
      iter = 4000,
      inits = function() {list(Intercept = 4.5e6)},
      control = list(adapt_delta = .99),
      file = str_glue("models/brm-{.y}-m2")
    ))
  )
```


```r
map(years$bm0, summary)
```

```
## [[1]]
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: Players ~ t2(yday, k = 27) + t2(wday, k = 7) 
##    Data: .x (Number of observations: 196) 
## Samples: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
##          total post-warmup samples = 8000
## 
## Smooth Terms: 
##                 Estimate  Est.Error  l-95% CI   u-95% CI Rhat Bulk_ESS Tail_ESS
## sds(t2yday_1) 3140375.41 1931596.04 688118.17 7245725.75 1.01      410     1498
## sds(t2wday_1)  698957.62  246675.28 366244.83 1312733.38 1.00     1949     3205
## 
## Population-Level Effects: 
##             Estimate Est.Error   l-95% CI   u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept 4131476.19   8643.19 4114591.68 4148408.74 1.00     9252     6132
## t2yday_1   137335.09   9750.69  118022.14  155865.67 1.00     2222     4234
## t2wday_1   209829.48   8577.12  192847.56  226648.96 1.00     9171     4882
## 
## Family Specific Parameters: 
##        Estimate Est.Error  l-95% CI  u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma 118933.33   9617.86 101855.88 138252.44 1.01      599     2491
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
## 
## [[2]]
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: Players ~ t2(yday, k = 27) + t2(wday, k = 7) 
##    Data: .x (Number of observations: 196) 
## Samples: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
##          total post-warmup samples = 8000
## 
## Smooth Terms: 
##                 Estimate  Est.Error   l-95% CI   u-95% CI Rhat Bulk_ESS
## sds(t2yday_1) 5933923.97 1009065.17 4237181.33 8237091.24 1.00     1437
## sds(t2wday_1)  642551.30  242753.01  323810.00 1251140.93 1.00     2150
##               Tail_ESS
## sds(t2yday_1)     2296
## sds(t2wday_1)     3306
## 
## Population-Level Effects: 
##             Estimate Est.Error   l-95% CI   u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept 4485180.91   8477.84 4468364.43 4501760.77 1.00    11132     6134
## t2yday_1   -73684.26   9055.69  -91925.39  -55772.09 1.00    10429     6521
## t2wday_1   183460.42   8299.92  167425.26  199806.65 1.00    12741     5561
## 
## Family Specific Parameters: 
##        Estimate Est.Error  l-95% CI  u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma 116553.07   6500.28 104534.40 129848.53 1.00     8073     6257
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

```r
gam.vcomp(years$m0[[2]][["gam"]], rescale = FALSE) # Compare
```

```
##  t2(yday)  t2(wday) 
## 6241236.8  572271.5
```

```r
map(years$bm1, summary)
```

```
## [[1]]
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: Players ~ t2(yday, wday, k = c(27, 7)) 
##    Data: .x (Number of observations: 196) 
## Samples: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
##          total post-warmup samples = 8000
## 
## Smooth Terms: 
##                     Estimate  Est.Error   l-95% CI    u-95% CI Rhat Bulk_ESS
## sds(t2ydaywday_1)  215594.09  160732.73   10966.17   613935.53 1.00     3118
## sds(t2ydaywday_2) 7204025.68 1860116.51 4387471.72 11475105.87 1.00     2447
## sds(t2ydaywday_3) 4514113.25 2321947.75 2216629.37  8742885.48 1.00     1163
##                   Tail_ESS
## sds(t2ydaywday_1)     3242
## sds(t2ydaywday_2)     4008
## sds(t2ydaywday_3)     1068
## 
## Population-Level Effects: 
##                Estimate Est.Error   l-95% CI   u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept    4132036.59   8570.93 4114941.77 4148743.71 1.00    10695     4926
## t2ydaywday_1  205090.46   8417.92  188912.33  221417.15 1.00    13259     5615
## t2ydaywday_2 -130363.57   8650.15 -147152.68 -113497.56 1.00     9735     5780
## t2ydaywday_3   42352.34   8706.46   25321.94   59220.76 1.00    11427     6096
## 
## Family Specific Parameters: 
##        Estimate Est.Error  l-95% CI  u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma 116569.17   7491.92 102658.87 131697.39 1.00     2104     1526
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
## 
## [[2]]
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: Players ~ t2(yday, wday, k = c(27, 7)) 
##    Data: .x (Number of observations: 196) 
## Samples: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
##          total post-warmup samples = 8000
## 
## Smooth Terms: 
##                      Estimate  Est.Error    l-95% CI    u-95% CI Rhat Bulk_ESS
## sds(t2ydaywday_1)   559894.76  235059.21   140808.47  1101095.34 1.00     2039
## sds(t2ydaywday_2)  6540650.59 1665083.83  4031383.32 10495088.39 1.00     2246
## sds(t2ydaywday_3) 28904897.04 3778821.37 22340552.86 37229776.95 1.00     1289
##                   Tail_ESS
## sds(t2ydaywday_1)     1957
## sds(t2ydaywday_2)     3394
## sds(t2ydaywday_3)     2573
## 
## Population-Level Effects: 
##                Estimate Est.Error   l-95% CI   u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept    4485869.84   6444.76 4473136.35 4498590.03 1.00    13886     5987
## t2ydaywday_1  177276.87   6593.10  164357.20  190136.28 1.00     9544     5744
## t2ydaywday_2   74333.12   7011.04   60732.48   88125.07 1.00     9869     6174
## t2ydaywday_3   43303.65   7376.32   28865.14   57713.36 1.00     6988     5815
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma 86674.84   5898.27 76036.38 99046.65 1.00     3792     5218
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

```r
map(years$bm2, summary)
```

```
## [[1]]
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: Players ~ t2(yday, wday, k = c(27, 7)) + ar(time = yday, p = 1) 
##    Data: .x (Number of observations: 196) 
## Samples: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
##          total post-warmup samples = 8000
## 
## Smooth Terms: 
##                     Estimate  Est.Error   l-95% CI    u-95% CI Rhat Bulk_ESS
## sds(t2ydaywday_1)  178364.82  131989.59    7522.88   499142.17 1.00     2966
## sds(t2ydaywday_2) 7978386.63 1743393.33 5263770.16 12184139.14 1.00     1548
## sds(t2ydaywday_3)  844954.99  538037.73  105225.67  2172772.92 1.00     1336
##                   Tail_ESS
## sds(t2ydaywday_1)     3707
## sds(t2ydaywday_2)     3071
## sds(t2ydaywday_3)     2218
## 
## Correlation Structures:
##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## ar[1]     0.79      0.09     0.64     0.98 1.00     1469     1040
## 
## Population-Level Effects: 
##                Estimate Est.Error   l-95% CI   u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept    4170243.96  94248.66 4088092.69 4398734.11 1.00     1780      761
## t2ydaywday_1  203991.02   7453.18  189171.13  218760.86 1.00     9630     6054
## t2ydaywday_2 -182351.83  67708.61 -361790.84 -106738.92 1.00     1991     1819
## t2ydaywday_3   40049.02   7570.60   25359.75   54725.30 1.00     7120     6084
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI  u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma 96083.71   5417.89 86102.29 107405.21 1.00     4183     5190
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
## 
## [[2]]
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: Players ~ t2(yday, wday, k = c(27, 7)) + ar(time = yday, p = 1) 
##    Data: .x (Number of observations: 196) 
## Samples: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
##          total post-warmup samples = 8000
## 
## Smooth Terms: 
##                     Estimate  Est.Error   l-95% CI    u-95% CI Rhat Bulk_ESS
## sds(t2ydaywday_1)  522266.78  224319.32  120551.33  1018342.66 1.00     2170
## sds(t2ydaywday_2) 6897256.71 1623665.15 4424409.11 10543378.73 1.00     2608
## sds(t2ydaywday_3)  645611.57  409913.36   40178.70  1614495.05 1.00     2589
##                   Tail_ESS
## sds(t2ydaywday_1)     1878
## sds(t2ydaywday_2)     3914
## sds(t2ydaywday_3)     2349
## 
## Correlation Structures:
##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## ar[1]     0.98      0.02     0.95     1.01 1.00     4148     5574
## 
## Population-Level Effects: 
##                Estimate Est.Error   l-95% CI   u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept    4460016.96 370265.05 3718170.05 5231356.80 1.00     1477     1158
## t2ydaywday_1  178746.71   8163.91  162417.60  194251.11 1.00    13462     6239
## t2ydaywday_2  -41314.31 223588.54 -490180.26  429033.04 1.00     1556     1214
## t2ydaywday_3   46970.56   8355.87   30582.92   63518.83 1.00    10271     6705
## 
## Family Specific Parameters: 
##        Estimate Est.Error  l-95% CI  u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma 113139.79   6443.98 101533.87 126729.09 1.00     7651     6018
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

```r
gam.vcomp(years$m1[[2]][["gam"]], rescale = FALSE) # Compare
```

```
## t2(yday,wday)rr t2(yday,wday)nr t2(yday,wday)rn 
##        570590.4       7319951.8      30036034.6
```

```r
cbind(years$bm1[[2]][["data"]], fitted(years$bm1[[2]])) %>% 
  ggplot(aes(yday, Players)) + 
  geom_point() + 
  geom_line(aes(y = Estimate))
```

<img src="03-GAM_files/figure-html/unnamed-chunk-6-1.png" width="672" />

```r
acf(residuals(years$bm1[[2]])[,1])
```

<img src="03-GAM_files/figure-html/unnamed-chunk-6-2.png" width="672" />

```r
summary(years$bm1[[2]])
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: Players ~ t2(yday, wday, k = c(27, 7)) 
##    Data: .x (Number of observations: 196) 
## Samples: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
##          total post-warmup samples = 8000
## 
## Smooth Terms: 
##                      Estimate  Est.Error    l-95% CI    u-95% CI Rhat Bulk_ESS
## sds(t2ydaywday_1)   559894.76  235059.21   140808.47  1101095.34 1.00     2039
## sds(t2ydaywday_2)  6540650.59 1665083.83  4031383.32 10495088.39 1.00     2246
## sds(t2ydaywday_3) 28904897.04 3778821.37 22340552.86 37229776.95 1.00     1289
##                   Tail_ESS
## sds(t2ydaywday_1)     1957
## sds(t2ydaywday_2)     3394
## sds(t2ydaywday_3)     2573
## 
## Population-Level Effects: 
##                Estimate Est.Error   l-95% CI   u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept    4485869.84   6444.76 4473136.35 4498590.03 1.00    13886     5987
## t2ydaywday_1  177276.87   6593.10  164357.20  190136.28 1.00     9544     5744
## t2ydaywday_2   74333.12   7011.04   60732.48   88125.07 1.00     9869     6174
## t2ydaywday_3   43303.65   7376.32   28865.14   57713.36 1.00     6988     5815
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## sigma 86674.84   5898.27 76036.38 99046.65 1.00     3792     5218
## 
## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

```r
bayes_R2(years$bm1[[2]])
```

```
##     Estimate   Est.Error      Q2.5     Q97.5
## R2 0.9732051 0.002436581 0.9679154 0.9773106
```


```r
# Compare mgcv and brms
years %>% 
  mutate(
    pred_b = map(bm1, ~as.data.frame(fitted(.))), 
    pred_m = map(m1, ~fitted(.[["gam"]]))
  ) %>% 
  select(year, data, starts_with("pred")) %>% 
  unnest(c(data, pred_m, pred_b)) %>% 
  pivot_longer(c(Estimate, pred_m)) %>% 
  ggplot(aes(Date, Players)) +
  geom_point() +
  geom_line(aes(y = value, col = name)) +
  facet_wrap("year", scales = "free", nrow = 2)
```

<img src="03-GAM_files/figure-html/unnamed-chunk-7-1.png" width="672" />

#### Figure


```r
# Prediction dataframe
tmp <- years %>% 
  mutate(
    p = map2(data, bm1, ~add_fitted_draws(.x, .y))
  ) %>% 
  select(year, p) %>% 
  unnest(p)

# Data with same dates for years for visualizing
xtmp <- total %>% 
  mutate(year = factor(year)) %>% 
  # Make dates same for 2019 and 2020 to put on same plot
  group_by(yday) %>% 
  mutate(Date = last(Date)) %>% 
  ungroup()

# Predictions and data figure
p1 <- tmp %>%
  mutate(year = factor(year)) %>% 
  # Make dates same for 2019 and 2020 to put on same plot
  group_by(yday) %>% 
  mutate(Date = last(Date)) %>% 
  ungroup() %>% 
  group_by(year, Date) %>% 
  mean_qi(.value) %>% 
  ungroup() %>% 
  ggplot(aes(Date, .value, alpha = year, group = year)) +
  # scale_x_date() +
  geom_vline(xintercept = cd, size = .1) +
  scale_alpha_manual("Year", values = c(.3, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(
    "Players",
    breaks = pretty_breaks(), 
    labels = function(x) str_glue("{x/1e6}M")
  ) +
  geom_ribbon(
    aes(ymin = .lower, ymax = .upper), alpha=.1
  ) +
  geom_line(size = .2) +
  geom_point(
    data = xtmp, 
    aes(y=Players), shape = 1, size = 1, fill = "white"
    )
p1
```

<img src="03-GAM_files/figure-html/unnamed-chunk-8-1.png" width="672" />

```r
# Excess players dataframe
tmp2 <- tmp %>% 
  group_by(year, yweek, .draw) %>% 
  summarise(week_mean = mean(.value)) %>% 
  mutate(.draw = 1:n()) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = year, values_from = c(week_mean)
  ) %>% 
  mutate(excess_model = `2020` / `2019`) %>% 
  mutate(year = "2020") %>% 
  select(year, yweek, starts_with("excess"))
tmp3 <- xtmp %>% 
  group_by(year, yweek) %>% 
  summarise(week_mean = mean(Players)) %>% 
  pivot_wider(
    names_from = year, values_from = c(week_mean)
  ) %>% 
  mutate(excess_data = `2020` / `2019`) %>% 
  mutate(year = "2020") %>% 
  select(year, yweek, starts_with("excess"))
  # left_join(select(tmp, year, Date, yweek))
# Excess gaming figure  
p2 <- tmp2 %>% 
  group_by(year, yweek) %>% 
  mean_qi(excess_model) %>% 
  ggplot(aes(x = yweek, alpha = NA)) +
  # Week of WHO announcement
  geom_vline(xintercept = floor(yday(cd) / 7), size = .1) +
  scale_x_continuous("Week of year", breaks = pretty_breaks()) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .2) +
  geom_line(aes(y = excess_model), size = .2) +
  geom_point(
    data = tmp3,
    aes(y = excess_data),
    shape = 1, size = 1, fill = "white"
  ) +
  # geom_hline(yintercept = 0, lty = 2, size = .1) +
  scale_y_continuous(
    "Excess players in 2020",
    breaks = pretty_breaks(),
    labels = scales::percent
  )
p2
```

```
## Warning: Using alpha for a discrete variable is not advised.
```

<img src="03-GAM_files/figure-html/unnamed-chunk-8-2.png" width="672" />

```r
(p1 + theme(axis.title.x = element_blank())) / p2 + 
  plot_layout(heights = c(.75, .25), guides = "collect")
```

```
## Warning: Using alpha for a discrete variable is not advised.
```

<img src="03-GAM_files/figure-html/unnamed-chunk-8-3.png" width="672" />

```r
ggsave("Figure1-brms.png", width = 8, height = 5)
```

```
## Warning: Using alpha for a discrete variable is not advised.
```

#### Weekend effect


```r
tmp <- years %>% 
  mutate(data2 = map(data, ~filter(., yweek %in% c(5, 15, 25)))) %>% 
  mutate(p = map2(data2, bm1, ~add_fitted_draws(.x, .y))) %>% 
  select(year, p) %>% 
  unnest(p) %>% 
  mutate(year = factor(year)) %>% 
  mutate(yweek = fct_inorder(str_glue("Week {yweek}")))
p1 <- tmp %>% 
  mutate(wday = wday(Date, label = TRUE, week_start = 1)) %>% 
  group_by(year, yweek, wday, Players) %>% 
  mean_qi(.value) %>% 
  ggplot(aes(wday, alpha = year, group = year)) +
  scale_y_continuous(
    "Players",
    breaks = pretty_breaks(), 
    labels = function(x) str_glue("{x/1e6}M")
  ) +
  scale_alpha_manual("Year", values = c(.3, 1)) +
  geom_point(aes(y = Players), shape = 1, size = 1, fill = "white") +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .1) +
  geom_line(aes(y = .value)) +
  facet_grid(~yweek)
weffs_data <- total %>% 
  select(year, yweek, wday, Players) %>% 
  pivot_wider(names_from = wday, values_from = Players) %>% 
  mutate(
    Weekend_effect = rowMeans(select(., `5`, `6`)) / 
      rowMeans(select(., `0`, `1`, `2`, `3`, `4`)), 
  ) %>% 
  mutate(year = factor(year))
weffs <- years %>% 
  mutate(p = map2(data, bm1, ~add_fitted_draws(.x, .y))) %>% 
  select(year, p) %>% 
  unnest(p) %>% 
  select(year, yweek, wday, .draw, .value) %>% 
  pivot_wider(names_from = wday, values_from = c(.value)) %>% 
  ungroup() %>% 
  mutate(
    Weekend_effect = rowMeans(select(., `5`, `6`)) / 
      rowMeans(select(., `0`, `1`, `2`, `3`, `4`)), 
  )

p2 <- weffs %>% 
  mutate(year = factor(year)) %>% 
  group_by(year, yweek) %>% 
  mean_qi(Weekend_effect) %>% 
  ggplot(aes(yweek, Weekend_effect, alpha = year, group = year)) +
  geom_vline(xintercept = floor(yday(cd) / 7), size = .1) +
  scale_x_continuous("Week of year", breaks = pretty_breaks()) +
  scale_y_continuous(
    "Weekend effect",
    breaks = pretty_breaks(), 
    labels = scales::percent
  ) +
  scale_alpha_manual("Year", values = c(.3, 1)) +
  geom_line(size = .5) +
  geom_ribbon(
    aes(ymin = .lower, ymax = .upper), alpha = .1
  ) +
  geom_point(data = weffs_data, shape = 1) +
  theme(legend.position = "none")

# Excess weekend effect dataframe
weffs2 <- weffs %>% 
  select(year, yweek, .draw, Weekend_effect) %>% 
  pivot_wider(
    names_from = year, values_from = c(Weekend_effect)
  ) %>% 
  mutate(Difference = `2020` - `2019`) %>% 
  mutate(year = "2020") %>% 
  select(year, yweek, .draw, Difference)
p3 <- weffs2 %>% 
  group_by(year, yweek) %>% 
  mean_qi(Difference) %>% 
  ggplot(aes(yweek, Difference)) +
  geom_vline(xintercept = floor(yday(cd) / 7), size = .1) +
  scale_x_continuous("Week of year", breaks = pretty_breaks()) +
  scale_y_continuous(
    "Difference\n(2020 - 2019)",
    breaks = pretty_breaks(), labels = function(x) str_glue("{x/1e6}M")
  ) +
  geom_hline(lty = 2, yintercept = 0, size = .2) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .2) +
  geom_line() +
  theme(legend.position = "none")
((p1 + theme(axis.title.x = element_blank())) / p2) + 
  plot_layout(heights = c(.4, .6), guides = "collect")
```

<img src="03-GAM_files/figure-html/unnamed-chunk-9-1.png" width="672" />

```r
ggsave("Figure2-brms.png", width = 8, height = 5)
```

## Multiplayer


```r
mp <- dat %>% 
  mutate(year = factor(year)) %>% 
  group_by(across(c(MP, Date, year:wend))) %>% 
  summarise(
    Players = sum(Players, na.rm = TRUE), 
    ngames = length(unique(appid))
  ) %>% 
  ungroup() %>% 
  group_by(MP, year) %>% 
  nest()
mp
```

```
## # A tibble: 4 x 3
## # Groups:   MP, year [4]
##   MP    year  data              
##   <fct> <fct> <list>            
## 1 No    2019  <tibble [196 × 8]>
## 2 No    2020  <tibble [196 × 8]>
## 3 Yes   2019  <tibble [196 × 8]>
## 4 Yes   2020  <tibble [196 × 8]>
```


```r
mp %>% 
  unnest(data) %>% 
  # Make dates same across years for viz
  group_by(yday) %>% 
  mutate(Date = last(Date)) %>% 
  ungroup() %>% 
  ggplot(aes(Date, Players, alpha = year, group = year)) +
  # scale_x_date() +
  geom_vline(xintercept = cd, size = .1) +
  scale_alpha_manual("Year", values = c(.3, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(
    breaks = pretty_breaks(), 
    labels = function(x) str_glue("{x/1e6}M")
  ) +
  geom_line(size = .3) +
  geom_point(shape = 21, size = 1, fill = "white") +
  facet_grid(rows = "MP", scales = "free")
```

<img src="03-GAM_files/figure-html/unnamed-chunk-11-1.png" width="672" />

### mgcv


```r
mp <- mp %>% 
  mutate(
    m0 = map(data, ~gamm(
      Players ~ t2(yday, k = 27) + t2(wday, k = 7),
      data = .x,
      method = "REML"
    )),
    m1 = map(data, ~gamm(
      Players ~ t2(yday, wday, k = c(27, 7)),
      data = .x,
      method = "REML"
    ))
  )
```

Figure


```r
# Prediction dataframe
tmp <- mp %>% 
  ungroup() %>% 
  mutate(MP = factor(MP, labels = c("Single-player", "Multiplayer"))) %>% 
  mutate(
    preds = map(m1, ~as.data.frame(predict(.x[["gam"]], se.fit = TRUE)))
    ) %>% 
  unnest(c(data, preds)) %>% 
  mutate(year = factor(year)) %>% 
  # Make dates same for 2019 and 2020 to put on same plot
  group_by(yday) %>% 
  mutate(Date = last(Date)) %>% 
  ungroup()
# Excess players dataframe
tmp2 <- tmp %>% 
  select(MP, year, yweek, Players, fit) %>% 
  pivot_wider(
    names_from = year, values_from = c(Players, fit), values_fn = mean
  ) %>% 
  mutate(excess_empirical = Players_2020 - Players_2019) %>% 
  mutate(excess_model = fit_2020 - fit_2019) %>% 
  mutate(year = "2020") %>% 
  select(MP, year, yweek, starts_with("excess"))
# Predictions and data figure
p1 <- tmp %>%
  ggplot(aes(Date, Players, alpha = year, group = year)) +
  # scale_x_date() +
  geom_vline(xintercept = cd, size = .1) +
  scale_alpha_manual("Year", values = c(.3, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(
    breaks = pretty_breaks(), 
    labels = function(x) str_glue("{x/1e6}M")
  ) +
  geom_ribbon(
    aes(ymin = fit-se.fit*2, ymax = fit+se.fit*2), alpha=.1
  ) +
  geom_line(aes(y=fit), size = .2) +
  geom_point(shape = 1, size = 1, fill = "white") +
  facet_wrap("MP", nrow = 1, scales = "free")
p1
```

<img src="03-GAM_files/figure-html/unnamed-chunk-12-1.png" width="672" />

```r
# Excess gaming figure  
p2 <- tmp2 %>% 
  ggplot(aes(x = yweek, alpha = NA)) +
  # Week of WHO announcement
  geom_vline(xintercept = floor(yday(cd) / 7), size = .1) +
  scale_x_continuous("Week of year", breaks = pretty_breaks()) +
  geom_line(aes(y = excess_model), size = .2) +
  geom_point(
    aes(y = excess_empirical), 
    shape = 1, size = 1, fill = "white"
  ) +
  geom_hline(yintercept = 0, lty = 2, size = .1) +
  scale_y_continuous(
    "Excess players\n(2020 - 2019)",
    breaks = pretty_breaks(), 
    labels = function(x) str_glue("{x/1e6}M")
  ) +
  facet_wrap("MP", nrow = 1, scales = "free")
p2
```

```
## Warning: Using alpha for a discrete variable is not advised.
```

<img src="03-GAM_files/figure-html/unnamed-chunk-12-2.png" width="672" />

```r
(p1 + theme(axis.title.x = element_blank())) / p2 + 
  plot_layout(heights = c(.75, .25), guides = "collect")
```

```
## Warning: Using alpha for a discrete variable is not advised.
```

<img src="03-GAM_files/figure-html/unnamed-chunk-12-3.png" width="672" />

```r
ggsave("Figure1-mp-mgcv.png", width = 8, height = 5)
```

```
## Warning: Using alpha for a discrete variable is not advised.
```

Weekend effect figure


```r
mp %>%
  mutate(preds = map(m1, ~as.data.frame(predict(.x[["gam"]], se.fit = TRUE)))) %>%
  unnest(c(data, preds)) %>%
  group_by(MP, year, yweek) %>%
  summarise(
    w_eff = mean(Players[wday %in% 5:6]) - mean(Players[wday %in% 0:4]),
    fit = mean(fit[wday %in% 5:6]) - mean(fit[wday %in% 0:4]),
  ) %>%
  mutate(year = factor(year)) %>% 
  ggplot(aes(yweek, w_eff, alpha = year, group = year)) +
  scale_x_continuous(
    "Week number",
    breaks = pretty_breaks()
  ) +
    scale_y_continuous(
    breaks = pretty_breaks(), 
    labels = function(x) str_glue("{x/1e6}M")
  ) +
  scale_alpha_manual("Year", values = c(.3, 1)) +
  geom_line(aes(y=fit)) +
  geom_point() +
  facet_wrap("MP", nrow = 1, scales = "free")
```

<img src="03-GAM_files/figure-html/unnamed-chunk-13-1.png" width="672" />

### brms


```r
sampler_0 <- brm(
  Players ~ t2(yday, k = 27) + t2(wday, k = 7),
  data = mp$data[[1]],
  chains = 0,
  file = "models/brm-sampler-mp-0"
)
sampler_1 <- brm(
  Players ~ t2(yday, wday, k = c(27, 7)),
  data = mp$data[[1]],
  chains = 0,
  file = "models/brm-sampler-mp-1"
)
# This model has a hard time converging so we set inits for the intercept
mp$bm0 <- vector("list", 4)
mp$bm1 <- vector("list", 4)
for (i in 1:4) {
  this_data <- mp$data[[i]]
    mp$bm0[[i]] <- update(
    sampler_0,
    newdata = this_data,
    chains = 4,
    iter = 3000,
    inits = function() {list(Intercept = mean(this_data$Players))},
    control = list(adapt_delta = .99),
    file = str_glue("models/brm-mp-{i}-m0")
    )
  mp$bm1[[i]] <- update(
    sampler_1,
    newdata = this_data,
    chains = 4,
    iter = 3000,
    inits = function() {list(Intercept = mean(this_data$Players))},
    control = list(adapt_delta = .99),
    file = str_glue("models/brm-mp-{i}-m1")
    )
}
```


```r
# Did the inits work?
for (i in 1:4) {
  print(mean(mp$data[[i]][["Players"]]))
  print(mp$bm1[[i]][["fit"]]@inits[[1]][["Intercept"]])
}
```

```
## [1] 314174.4
## [1] 314174.4
## [1] 370239.5
## [1] 370239.5
## [1] 3810860
## [1] 3810860
## [1] 4121149
## [1] 4121149
```

#### Figure


```r
# Prediction dataframe
mp <- mp %>% 
  ungroup() %>% 
  mutate(MP = factor(MP, labels = c("Single-player", "Multiplayer")))
tmp <- mp %>% 
  mutate(
    p = map2(data, bm1, ~add_fitted_draws(.x, .y))
  ) %>% 
  select(MP, year, p) %>% 
  unnest(p)

# Data with same dates for years for visualizing
xtmp <- mp %>% 
  select(MP, year, data) %>% 
  unnest(data) %>% 
  mutate(year = factor(year)) %>% 
  # Make dates same for 2019 and 2020 to put on same plot
  group_by(yday) %>% 
  mutate(Date = last(Date)) %>% 
  ungroup()

# Predictions and data figure
p1 <- tmp %>%
  mutate(year = factor(year)) %>% 
  # Make dates same for 2019 and 2020 to put on same plot
  group_by(yday) %>% 
  mutate(Date = last(Date)) %>% 
  ungroup() %>% 
  group_by(MP, year, Date) %>% 
  mean_qi(.value) %>% 
  ungroup() %>% 
  ggplot(aes(Date, .value, alpha = year, group = year)) +
  # scale_x_date() +
  geom_vline(xintercept = cd, size = .1) +
  scale_alpha_manual("Year", values = c(.3, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(
    "Players",
    breaks = pretty_breaks(), 
    labels = function(x) str_glue("{x/1e6}M")
  ) +
  geom_ribbon(
    aes(ymin = .lower, ymax = .upper), alpha=.1
  ) +
  geom_line(size = .2) +
  geom_point(
    data = xtmp, 
    aes(y=Players), shape = 1, size = 1, fill = "white"
  ) +
  facet_wrap("MP", nrow = 1, scales = "free")

# Excess players dataframe
tmp2 <- tmp %>% 
  group_by(MP, year, yweek, .draw) %>% 
  summarise(week_mean = mean(.value)) %>% 
  mutate(.draw = 1:n()) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = year, values_from = c(week_mean)
  ) %>% 
  mutate(excess_model = `2020` / `2019`) %>% 
  mutate(year = "2020") %>% 
  select(MP, year, yweek, starts_with("excess"))
tmp3 <- xtmp %>% 
  group_by(MP, year, yweek) %>% 
  summarise(week_mean = mean(Players)) %>% 
  pivot_wider(
    names_from = year, values_from = c(week_mean)
  ) %>% 
  mutate(excess_data = `2020` / `2019`) %>% 
  mutate(year = "2020") %>% 
  select(MP, year, yweek, starts_with("excess"))
  # left_join(select(tmp, year, Date, yweek))
# Excess gaming figure  
p2 <- tmp2 %>% 
  group_by(MP, year, yweek) %>% 
  mean_qi(excess_model) %>% 
  ggplot(aes(x = yweek, alpha = NA)) +
  # Week of WHO announcement
  geom_vline(xintercept = floor(yday(cd) / 7), size = .1) +
  scale_x_continuous("Week of year", breaks = pretty_breaks()) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .2) +
  geom_line(aes(y = excess_model), size = .2) +
  geom_point(
    data = tmp3,
    aes(y = excess_data),
    shape = 1, size = 1, fill = "white"
  ) +
  geom_hline(yintercept = 1, lty = 2, size = .1) +
  scale_y_continuous(
    "2020 excess",
    breaks = pretty_breaks(), 
    labels = scales::percent
  ) +
  facet_wrap("MP", nrow = 1, scales = "free") +
  theme(strip.background = element_blank())

(p1 + theme(axis.title.x = element_blank())) / p2 + 
  plot_layout(heights = c(.75, .25), guides = "collect") &
  theme(legend.position = "none")
```

```
## Warning: Using alpha for a discrete variable is not advised.
```

<img src="03-GAM_files/figure-html/unnamed-chunk-16-1.png" width="672" />

```r
ggsave("Figure1-mp-brms.png", width = 8, height = 5)
```

```
## Warning: Using alpha for a discrete variable is not advised.
```

#### Weekend effect


```r
tmp <- mp %>% 
  mutate(data2 = map(data, ~filter(., yweek %in% c(5, 15, 25)))) %>% 
  mutate(p = map2(data2, bm1, ~add_fitted_draws(.x, .y))) %>% 
  select(MP, year, p) %>% 
  unnest(p) %>% 
  mutate(year = factor(year)) %>% 
  mutate(yweek = fct_inorder(str_glue("Week {yweek}")))
p1 <- tmp %>% 
  mutate(wday = wday(Date, label = TRUE, week_start = 1)) %>% 
  group_by(MP, year, yweek, wday, Players) %>% 
  mean_qi(.value) %>% 
  ggplot(aes(wday, alpha = year, group = year)) +
  scale_y_continuous(
    "Players",
    breaks = pretty_breaks(), 
    labels = function(x) str_glue("{x/1e6}M")
  ) +
  scale_alpha_manual("Year", values = c(.3, 1)) +
  geom_point(aes(y = Players), shape = 1, size = 1, fill = "white") +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .1) +
  geom_line(aes(y = .value)) +
  facet_grid(MP~yweek, scales = "free")
p1
```

<img src="03-GAM_files/figure-html/unnamed-chunk-17-1.png" width="672" />

```r
weffs_data <- mp %>% 
  select(MP, year, data) %>% 
  unnest(data) %>% 
  select(MP, year, yweek, wday, Players) %>% 
  pivot_wider(names_from = wday, values_from = Players) %>% 
  mutate(
    Weekend_effect = rowMeans(select(., `5`, `6`)) / 
      rowMeans(select(., `0`, `1`, `2`, `3`, `4`)), 
  ) %>% 
  mutate(year = factor(year))
weffs <- mp %>% 
  mutate(p = map2(data, bm1, ~add_fitted_draws(.x, .y))) %>% 
  select(MP, year, p) %>% 
  unnest(p) %>% 
  select(MP, year, yweek, wday, .draw, .value) %>% 
  pivot_wider(names_from = wday, values_from = c(.value)) %>% 
  ungroup() %>% 
  mutate(
    Weekend_effect = rowMeans(select(., `5`, `6`)) / 
      rowMeans(select(., `0`, `1`, `2`, `3`, `4`)), 
  )

p2 <- weffs %>% 
  mutate(year = factor(year)) %>% 
  group_by(MP, year, yweek) %>% 
  mean_qi(Weekend_effect) %>% 
  ggplot(aes(yweek, Weekend_effect, alpha = year, group = year)) +
  geom_vline(xintercept = floor(yday(cd) / 7), size = .1) +
  scale_x_continuous("Week of year", breaks = pretty_breaks()) +
  scale_y_continuous(
    "Weekend effect",
    breaks = pretty_breaks(), labels = percent_format(accuracy = 1)
  ) +
  scale_alpha_manual("Year", values = c(.3, 1)) +
  geom_line(size = .5) +
  geom_ribbon(
    aes(ymin = .lower, ymax = .upper), alpha = .1
  ) +
  geom_point(data = weffs_data, shape = 1) +
  theme(legend.position = "none") +
  facet_grid(rows = "MP", scales = "free")

# Excess weekend effect dataframe
weffs2 <- weffs %>% 
  select(MP, year, yweek, .draw, Weekend_effect) %>% 
  pivot_wider(
    names_from = year, values_from = c(Weekend_effect)
  ) %>% 
  mutate(Difference = `2020` - `2019`) %>% 
  mutate(year = "2020") %>% 
  select(MP, year, yweek, .draw, Difference)
p3 <- weffs2 %>% 
  group_by(MP, year, yweek) %>% 
  mean_qi(Difference) %>% 
  ggplot(aes(yweek, Difference)) +
  geom_vline(xintercept = floor(yday(cd) / 7), size = .1) +
  scale_x_continuous("Week of year", breaks = pretty_breaks()) +
  scale_y_continuous(
    "Difference\n(2020 - 2019)",
    breaks = pretty_breaks(), labels = function(x) str_glue("{x/1e6}M")
  ) +
  geom_hline(lty = 2, yintercept = 0, size = .2) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .2) +
  geom_line() +
  facet_wrap("MP", scales = "free", nrow = 2) +
  theme(legend.position = "none")
((p1 + theme(axis.title.x = element_blank())) / p2) + 
  plot_layout(heights = c(.4, .6), guides = "collect") &
  theme(legend.position = "none")
```

<img src="03-GAM_files/figure-html/unnamed-chunk-17-2.png" width="672" />

```r
ggsave("Figure2-mp-brms.png", width = 8, height = 6)
```
