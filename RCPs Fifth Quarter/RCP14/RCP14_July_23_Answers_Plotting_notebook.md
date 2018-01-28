R Notebook
================

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.3.3

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
percentile_80 <- function(x){return(quantile(x, 0.8))[[1]]}
percentile_20 <- function(x){return(quantile(x, 0.2))[[1]]}
```

``` r
answers_to_plot <- read_csv("MultiIter10Answers_for_30_orgs.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
answers_to_plot <- answers_to_plot %>% 
  mutate(Organization = rep(seq(0,29), each = 10)) %>% 
  group_by(Organization) %>%
  summarise_each(funs(mean)) %>%
  ungroup %>% 
  gather(key = answer_num, value = answer_result, 2:22) %>%
  mutate(answer_num = stringr::str_sub(answer_num, 8, -1)) %>%
  mutate(answer_num = as.integer(answer_num))
```

``` r
ggplot(answers_to_plot) + 
  stat_summary(mapping = aes(x = answer_num, y = answer_result), fun.ymin = percentile_20, fun.y = "mean", fun.ymax = percentile_80, size = 0.15, colour = "blue") + 
  scale_y_continuous("Estimate", limits = c(0, 1)) + scale_x_continuous("Question", breaks = seq(1, 21), labels = seq(1, 21)) + ggtitle("Answers to questions (60% intervals)") + theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
```

![](RCP14_July_23_Answers_Plotting_notebook_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
ggsave("MultiIter10Answers_for_July23_30_orgs.png", width = 12, height = 5)
```
