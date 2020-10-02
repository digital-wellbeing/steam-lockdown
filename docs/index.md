--- 
title: "Gaming under lockdown: Steam data"
author: "Matti Vuorre"
date: "2020-10-02"
site: bookdown::bookdown_site
documentclass: book
bibliography: []
biblio-style: apalike
link-citations: yes
description: ""
---

# Preface

We study changes in gaming behavior during COVID-19 using data from Steam.

GitHub repository: <https://github.com/digital-wellbeing/steam-lockdown>
Website: <https://digital-wellbeing.github.io/steam-lockdown>
OSF: <https://osf.io/ya9jt/>

## Analyses and reproducibility

The data analyses are organized into separate [R Markdown](https://rmarkdown.rstudio.com/) files for processing, describing, and modelling.

The project is organized as a R [bookdown](https://bookdown.org/yihui/bookdown/) project, so you can reproduce all analyses by building the book (e.g. in RStudio click the "Build Book" button). The results are rendered to `docs/index.html` and can be viewed in a web browser.

## Raw data

Is in data-raw/ which can't yet be pushed online.

## Reproducibility




```r
options(width = 120)
library(sessioninfo)
session_info()
```

```
## ─ Session info ───────────────────────────────────────────────────────────────────────────────────────────────────────
##  setting  value                       
##  version  R version 4.0.2 (2020-06-22)
##  os       macOS Catalina 10.15.7      
##  system   x86_64, darwin17.0          
##  ui       X11                         
##  language (EN)                        
##  collate  en_GB.UTF-8                 
##  ctype    en_GB.UTF-8                 
##  tz       Europe/London               
##  date     2020-10-02                  
## 
## ─ Packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────
##  package     * version date       lib source                              
##  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.0)                      
##  bookdown      0.20    2020-06-23 [1] CRAN (R 4.0.2)                      
##  cli           2.0.2   2020-02-28 [1] CRAN (R 4.0.0)                      
##  crayon        1.3.4   2017-09-16 [1] CRAN (R 4.0.0)                      
##  digest        0.6.25  2020-02-23 [1] CRAN (R 4.0.0)                      
##  evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.0)                      
##  fansi         0.4.1   2020-01-08 [1] CRAN (R 4.0.0)                      
##  glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.2)                      
##  htmltools     0.5.0   2020-06-16 [1] CRAN (R 4.0.1)                      
##  knitr         1.30    2020-09-22 [1] CRAN (R 4.0.2)                      
##  magrittr      1.5     2014-11-22 [1] CRAN (R 4.0.0)                      
##  rlang         0.4.7   2020-07-09 [1] CRAN (R 4.0.2)                      
##  rmarkdown     2.4.0   2020-09-11 [1] Github (cpsievert/rmarkdown@b79fb4d)
##  sessioninfo * 1.1.1   2018-11-05 [1] CRAN (R 4.0.0)                      
##  stringi       1.5.3   2020-09-09 [1] CRAN (R 4.0.2)                      
##  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.0.0)                      
##  withr         2.3.0   2020-09-22 [1] CRAN (R 4.0.2)                      
##  xfun          0.18    2020-09-29 [1] CRAN (R 4.0.2)                      
##  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.0.0)                      
## 
## [1] /Library/Frameworks/R.framework/Versions/4.0/Resources/library
```
