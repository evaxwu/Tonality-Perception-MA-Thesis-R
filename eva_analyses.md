Eva Analyses
================
Eva Wu
2022-07-13

Very helpful
[link](https://www.datanovia.com/en/lessons/mixed-anova-in-r/)!

## Summary Statistics

``` r
# descriptives
summary(data)
```

    ##   qualtrics_id        instrument         tuning_step    chord          
    ##  Min.   :1.044e+09   Length:1225        Min.   :1    Length:1225       
    ##  1st Qu.:2.707e+09   Class :character   1st Qu.:2    Class :character  
    ##  Median :4.476e+09   Mode  :character   Median :3    Mode  :character  
    ##  Mean   :4.802e+09                      Mean   :3                      
    ##  3rd Qu.:6.783e+09                      3rd Qu.:4                      
    ##  Max.   :9.969e+09                      Max.   :5                      
    ##                                                                        
    ##     pct_maj        explicit_rtg   participant        passed_practice
    ##  Min.   :0.0000   Min.   :1.000   Length:1225        Min.   :1      
    ##  1st Qu.:0.1250   1st Qu.:2.000   Class :character   1st Qu.:1      
    ##  Median :0.5000   Median :3.000   Mode  :character   Median :1      
    ##  Mean   :0.5041   Mean   :2.535                      Mean   :1      
    ##  3rd Qu.:0.8750   3rd Qu.:3.000                      3rd Qu.:1      
    ##  Max.   :1.0000   Max.   :4.000                      Max.   :1      
    ##                                                                     
    ##  block_passed_practice practice_score       Age           Gender         
    ##  Min.   :1.000         Min.   : 8.00   Min.   :18.00   Length:1225       
    ##  1st Qu.:1.000         1st Qu.:11.00   1st Qu.:19.00   Class :character  
    ##  Median :1.000         Median :12.00   Median :20.00   Mode  :character  
    ##  Mean   :1.041         Mean   :11.18   Mean   :20.14                     
    ##  3rd Qu.:1.000         3rd Qu.:12.00   3rd Qu.:20.25                     
    ##  Max.   :2.000         Max.   :12.00   Max.   :32.00                     
    ##                                        NA's   :125                       
    ##      Year           Year_6_TEXT           Major                Inst       
    ##  Length:1225        Length:1225        Length:1225        Min.   :0.0000  
    ##  Class :character   Class :character   Class :character   1st Qu.:1.0000  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :1.0000  
    ##                                                           Mean   :0.8367  
    ##                                                           3rd Qu.:1.0000  
    ##                                                           Max.   :1.0000  
    ##                                                                           
    ##     Inst_yr          Read          music_exp       headphone     
    ##  Min.   : 1.0   Min.   :0.0000   Min.   : 1.00   Min.   :0.0000  
    ##  1st Qu.: 4.0   1st Qu.:0.0000   1st Qu.:15.70   1st Qu.:1.0000  
    ##  Median : 6.0   Median :1.0000   Median :22.30   Median :1.0000  
    ##  Mean   : 7.5   Mean   :0.6122   Mean   :22.39   Mean   :0.8367  
    ##  3rd Qu.:10.0   3rd Qu.:1.0000   3rd Qu.:27.90   3rd Qu.:1.0000  
    ##  Max.   :23.0   Max.   :1.0000   Max.   :50.00   Max.   :1.0000  
    ##  NA's   :325                                                     
    ##    test_corr    
    ##  Min.   :0.000  
    ##  1st Qu.:4.000  
    ##  Median :6.000  
    ##  Mean   :4.837  
    ##  3rd Qu.:6.000  
    ##  Max.   :6.000  
    ## 

``` r
# marginal means 
# instrument
desc %>%
  group_by(instrument) %>%
  summarize(mean_pct_inst = mean(mean_pct),
            sd_pct_inst = sd(mean_pct), # is this the right way to calculate sd? or should this be done in previous steps
            mean_rtg_inst = mean(mean_rtg),
            sd_rtg_inst = sd(mean_rtg))
```

    ## # A tibble: 5 × 5
    ##   instrument mean_pct_inst sd_pct_inst mean_rtg_inst sd_rtg_inst
    ##   <chr>              <dbl>       <dbl>         <dbl>       <dbl>
    ## 1 oboe               0.393       0.278          2.24      0.0273
    ## 2 piano              0.521       0.311          2.76      0.157 
    ## 3 trumpet            0.516       0.300          2.71      0.0185
    ## 4 violin             0.494       0.295          2.01      0.130 
    ## 5 xylophone          0.591       0.277          2.96      0.0405

``` r
# tuning step
desc %>%
  group_by(tuning_step) %>%
  summarize(mean_pct_tune = mean(mean_pct), 
            sd_pct_tune = sd(mean_pct),
            mean_rtg_tune = mean(mean_rtg),
            sd_rtg_tune = sd(mean_rtg))
```

    ## # A tibble: 5 × 5
    ##   tuning_step mean_pct_tune sd_pct_tune mean_rtg_tune sd_rtg_tune
    ##         <dbl>         <dbl>       <dbl>         <dbl>       <dbl>
    ## 1           1         0.188      0.0905          2.54       0.386
    ## 2           2         0.236      0.0980          2.54       0.386
    ## 3           3         0.470      0.118           2.54       0.386
    ## 4           4         0.790      0.0993          2.54       0.386
    ## 5           5         0.830      0.0826          2.54       0.386

``` r
# key
desc %>%
  group_by(chord) %>%
  summarize(mean_pct_key = mean(mean_pct), 
            sd_pct_key = sd(mean_pct),
            mean_rtg_key = mean(mean_rtg),
            sd_rtg_key = sd(mean_rtg))
```

    ## # A tibble: 2 × 5
    ##   chord mean_pct_key sd_pct_key mean_rtg_key sd_rtg_key
    ##   <chr>        <dbl>      <dbl>        <dbl>      <dbl>
    ## 1 B            0.52       0.234         2.48      0.375
    ## 2 C            0.486      0.337         2.59      0.363

``` r
data %>%
  group_by(instrument, tuning_step, chord) %>%
  get_summary_stats(pct_maj, explicit_rtg, type = "mean_sd")
```

    ## # A tibble: 100 × 7
    ##    instrument tuning_step chord variable         n  mean    sd
    ##    <chr>            <dbl> <chr> <chr>        <dbl> <dbl> <dbl>
    ##  1 oboe                 1 B     explicit_rtg    26 2.27  0.724
    ##  2 oboe                 1 B     pct_maj         26 0.183 0.232
    ##  3 oboe                 1 C     explicit_rtg    23 2.22  0.6  
    ##  4 oboe                 1 C     pct_maj         23 0.038 0.07 
    ##  5 oboe                 2 B     explicit_rtg    26 2.27  0.724
    ##  6 oboe                 2 B     pct_maj         26 0.183 0.201
    ##  7 oboe                 2 C     explicit_rtg    23 2.22  0.6  
    ##  8 oboe                 2 C     pct_maj         23 0.12  0.116
    ##  9 oboe                 3 B     explicit_rtg    26 2.27  0.724
    ## 10 oboe                 3 B     pct_maj         26 0.389 0.243
    ## # … with 90 more rows

## Visualization

``` r
data %>% 
  ggplot(aes(tuning_step, pct_maj, color = instrument)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~chord) +
  labs(title = "Proportion of major chord categorization across different instruments and tuning steps",
       subtitle = "compared between the key of B and C",
       x = "Tuning step (+0c ~ +100c)", y = "Proportion of major categorization") +
  theme_bw()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](eva_analyses_files/figure-gfm/graph-1.png)<!-- -->

``` r
data %>% 
  ggplot(aes(tuning_step, pct_maj, color = chord)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~instrument) +
  labs(title = "Proportion of major chord categorization across different keys and tuning steps",
       x = "Tuning step (+0c ~ +100c)", y = "Proportion of major categorization") +
  theme_bw()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](eva_analyses_files/figure-gfm/graph-2.png)<!-- -->

``` r
data %>% 
  ggplot(aes(reorder(instrument, explicit_rtg), explicit_rtg, fill = instrument)) +
  geom_col() +
  facet_wrap(~chord) +
  labs(title = "Mean explicit valence rating across different instruments",
       subtitle = "compared between the key of B and C",
       x = "Instrument", y = "Mean explicit valence rating") +
  theme_bw()
```

![](eva_analyses_files/figure-gfm/graph-3.png)<!-- -->

## Check assumptions

### Outliers

``` r
# for cat
data_summary <- data %>%
  group_by(qualtrics_id, tuning_step) %>%
  summarize(mean_pct = mean(pct_maj))
```

    ## `summarise()` has grouped output by 'qualtrics_id'. You can override using the
    ## `.groups` argument.

``` r
data %>%
  group_by(qualtrics_id) %>%
  summarize(mean_pct = mean(pct_maj)) %>%
  identify_outliers(mean_pct)
```

    ## [1] qualtrics_id mean_pct     is.outlier   is.extreme  
    ## <0 rows> (or 0-length row.names)

``` r
# for rtg
data %>%
  group_by(qualtrics_id) %>%
  summarize(mean_rtg = mean(explicit_rtg)) %>%
  ggplot(aes(mean_rtg)) +
  geom_histogram(color = "white")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](eva_analyses_files/figure-gfm/outliers-1.png)<!-- -->

``` r
data %>%
  group_by(qualtrics_id) %>%
  summarize(mean_rtg = mean(explicit_rtg)) %>%
  identify_outliers(mean_rtg)
```

    ## # A tibble: 3 × 4
    ##   qualtrics_id mean_rtg is.outlier is.extreme
    ##          <dbl>    <dbl> <lgl>      <lgl>     
    ## 1   1588756489      3.4 TRUE       FALSE     
    ## 2   6323213291      1.6 TRUE       FALSE     
    ## 3   6444402078      3.4 TRUE       FALSE

``` r
# examine outliers
data %>%
  filter(qualtrics_id == 1588756489 | qualtrics_id == 6323213291 | qualtrics_id == 6444402078) %>%
  select(qualtrics_id, instrument, explicit_rtg) %>%
  unique()
```

    ## # A tibble: 15 × 3
    ##    qualtrics_id instrument explicit_rtg
    ##           <dbl> <chr>             <dbl>
    ##  1   1588756489 xylophone             4
    ##  2   1588756489 trumpet               4
    ##  3   1588756489 piano                 3
    ##  4   1588756489 violin                3
    ##  5   1588756489 oboe                  3
    ##  6   6323213291 xylophone             1
    ##  7   6323213291 trumpet               1
    ##  8   6323213291 piano                 2
    ##  9   6323213291 violin                2
    ## 10   6323213291 oboe                  2
    ## 11   6444402078 xylophone             4
    ## 12   6444402078 trumpet               3
    ## 13   6444402078 piano                 4
    ## 14   6444402078 violin                3
    ## 15   6444402078 oboe                  3

``` r
# ignore b/c not extreme
```

### Normality

``` r
# violated but fine
data %>%
  group_by(instrument, tuning_step, chord) %>%
  shapiro_test(pct_maj)
```

    ## # A tibble: 50 × 6
    ##    instrument tuning_step chord variable statistic           p
    ##    <chr>            <dbl> <chr> <chr>        <dbl>       <dbl>
    ##  1 oboe                 1 B     pct_maj      0.785 0.000101   
    ##  2 oboe                 1 C     pct_maj      0.592 0.000000741
    ##  3 oboe                 2 B     pct_maj      0.835 0.000753   
    ##  4 oboe                 2 C     pct_maj      0.828 0.00111    
    ##  5 oboe                 3 B     pct_maj      0.955 0.304      
    ##  6 oboe                 3 C     pct_maj      0.879 0.00936    
    ##  7 oboe                 4 B     pct_maj      0.901 0.0165     
    ##  8 oboe                 4 C     pct_maj      0.862 0.00462    
    ##  9 oboe                 5 B     pct_maj      0.836 0.000762   
    ## 10 oboe                 5 C     pct_maj      0.672 0.00000615 
    ## # … with 40 more rows

``` r
ggqqplot(data, "pct_maj", ggtheme = theme_bw()) +
  facet_grid(tuning_step ~ instrument, labeller = "label_both")
```

![](eva_analyses_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

### Homogeneity of variance

``` r
data %>% levene_test(pct_maj ~ instrument*factor(tuning_step)*chord)
```

    ## # A tibble: 1 × 4
    ##     df1   df2 statistic        p
    ##   <int> <int>     <dbl>    <dbl>
    ## 1    49  1175      3.07 2.62e-11

No need to transform for assumption violations b/c ANOVA is robust for
these issues. Just report a Greenhouse-Geisser correction (epsilon \*
df).

The assumption of sphericity will be automatically checked during the
computation of the ANOVA test using the R function anova_test()
\[rstatix package\]. The Mauchly’s test is internally used to assess the
sphericity assumption.

By using the function get_anova_table() \[rstatix\] to extract the ANOVA
table, the Greenhouse-Geisser correction is automatically applied to
factors violating the sphericity assumption.

## ANOVA

``` r
chord.aov <- anova_test(data = data, dv = pct_maj, wid = qualtrics_id,
  within = c(instrument, tuning_step), between = chord)
get_anova_table(chord.aov) # sphericity violated but corrected w/ GG
```

    ## ANOVA Table (type III tests)
    ## 
    ##                         Effect   DFn    DFd       F        p p<.05   ges
    ## 1                        chord  1.00  47.00   2.010 1.63e-01       0.005
    ## 2                   instrument  2.76 129.89  13.830 1.71e-07     * 0.063
    ## 3                  tuning_step  1.59  74.57 114.978 3.98e-21     * 0.541
    ## 4             chord:instrument  2.76 129.89   0.483 6.80e-01       0.002
    ## 5            chord:tuning_step  1.59  74.57   4.842 1.60e-02     * 0.047
    ## 6       instrument:tuning_step 10.46 491.53   4.092 1.38e-05     * 0.016
    ## 7 chord:instrument:tuning_step 10.46 491.53   0.784 6.49e-01       0.003

``` r
# no main effect of chord / 3-way int (no change in main effects/int of interest), so left it out for the sake of parsimony

mus.aov <- anova_test(data = data, dv = pct_maj, wid = qualtrics_id,
  within = c(instrument, tuning_step), covariate = Inst_yr)
```

    ## Warning: NA detected in rows: 176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,451,452,453,454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,474,475,701,702,703,704,705,706,707,708,709,710,711,712,713,714,715,716,717,718,719,720,721,722,723,724,725,751,752,753,754,755,756,757,758,759,760,761,762,763,764,765,766,767,768,769,770,771,772,773,774,775,776,777,778,779,780,781,782,783,784,785,786,787,788,789,790,791,792,793,794,795,796,797,798,799,800,876,877,878,879,880,881,882,883,884,885,886,887,888,889,890,891,892,893,894,895,896,897,898,899,900,901,902,903,904,905,906,907,908,909,910,911,912,913,914,915,916,917,918,919,920,921,922,923,924,925,951,952,953,954,955,956,957,958,959,960,961,962,963,964,965,966,967,968,969,970,971,972,973,974,975,1001,1002,1003,1004,1005,1006,1007,1008,1009,1010,1011,1012,1013,1014,1015,1016,1017,1018,1019,1020,1021,1022,1023,1024,1025,1151,1152,1153,1154,1155,1156,1157,1158,1159,1160,1161,1162,1163,1164,1165,1166,1167,1168,1169,1170,1171,1172,1173,1174,1175,1201,1202,1203,1204,1205,1206,1207,1208,1209,1210,1211,1212,1213,1214,1215,1216,1217,1218,1219,1220,1221,1222,1223,1224,1225.
    ## Removing this rows before the analysis.

``` r
get_anova_table(mus.aov)
```

    ## ANOVA Table (type II tests)
    ## 
    ##                           Effect  DFn    DFd       F        p p<.05   ges
    ## 1                        Inst_yr 1.00  34.00   0.545 4.66e-01       0.002
    ## 2                     instrument 2.31  78.63   9.825 7.10e-05     * 0.058
    ## 3                    tuning_step 1.97  66.90 130.579 1.16e-23     * 0.640
    ## 4             Inst_yr:instrument 2.31  78.63   2.062 1.27e-01       0.013
    ## 5            Inst_yr:tuning_step 1.97  66.90   6.158 4.00e-03     * 0.077
    ## 6         instrument:tuning_step 8.65 293.96   4.275 4.15e-05     * 0.024
    ## 7 Inst_yr:instrument:tuning_step 8.65 293.96   0.956 4.75e-01       0.006

``` r
# adding musical training doesn't change effects of main interest, same as chord
# int may be due to steeper slope for more trained participants, but has nothing to do with our hypotheses

# A significant two-way interaction can be followed up by a simple main effect analysis, 
# which can be followed up by simple pairwise comparisons if significant.

aov <- anova_test(data = data, dv = pct_maj, wid = qualtrics_id,
  within = c(instrument, tuning_step))
get_anova_table(aov)
```

    ## ANOVA Table (type III tests)
    ## 
    ##                   Effect   DFn    DFd       F        p p<.05   ges
    ## 1             instrument  2.79 133.91  14.056 1.15e-07     * 0.060
    ## 2            tuning_step  1.55  74.48 104.352 8.42e-20     * 0.522
    ## 3 instrument:tuning_step 10.64 510.95   4.078 1.23e-05     * 0.015

## Post-hoc tests

``` r
# post hoc for main eff of instrument
data %>%
  pairwise_t_test(
    pct_maj ~ instrument, paired = TRUE, 
    p.adjust.method = "bonferroni"
    ) %>% 
  select(-`.y.`, -p)
```

    ## # A tibble: 10 × 8
    ##    group1  group2       n1    n2 statistic    df    p.adj p.adj.signif
    ##    <chr>   <chr>     <int> <int>     <dbl> <dbl>    <dbl> <chr>       
    ##  1 oboe    piano       245   245    -7.57    244 7.77e-12 ****        
    ##  2 oboe    trumpet     245   245    -8.29    244 7.65e-14 ****        
    ##  3 oboe    violin      245   245    -6.63    244 2.17e- 9 ****        
    ##  4 oboe    xylophone   245   245   -10.1     244 3.21e-19 ****        
    ##  5 piano   trumpet     245   245     0.362   244 1   e+ 0 ns          
    ##  6 piano   violin      245   245     1.72    244 8.73e- 1 ns          
    ##  7 piano   xylophone   245   245    -4.42    244 1.47e- 4 ***         
    ##  8 trumpet violin      245   245     1.37    244 1   e+ 0 ns          
    ##  9 trumpet xylophone   245   245    -4.23    244 3.33e- 4 ***         
    ## 10 violin  xylophone   245   245    -5.45    244 1.25e- 6 ****

``` r
# post hoc for main eff of tuning
data %>%
  pairwise_t_test(
    pct_maj ~ tuning_step, paired = TRUE, 
    p.adjust.method = "bonferroni"
    ) %>% 
  select(-`.y.`, -p)
```

    ## # A tibble: 10 × 8
    ##    group1 group2    n1    n2 statistic    df    p.adj p.adj.signif
    ##    <chr>  <chr>  <int> <int>     <dbl> <dbl>    <dbl> <chr>       
    ##  1 1      2        245   245     -4.46   244 1.28e- 4 ***         
    ##  2 1      3        245   245    -14.3    244 3.98e-33 ****        
    ##  3 1      4        245   245    -24.3    244 5.03e-66 ****        
    ##  4 1      5        245   245    -24.9    244 4.84e-68 ****        
    ##  5 2      3        245   245    -11.7    244 2.39e-24 ****        
    ##  6 2      4        245   245    -21.7    244 9.88e-58 ****        
    ##  7 2      5        245   245    -22.4    244 3.39e-60 ****        
    ##  8 3      4        245   245    -15.3    244 2.09e-36 ****        
    ##  9 3      5        245   245    -15.2    244 4.51e-36 ****        
    ## 10 4      5        245   245     -3.48   244 6   e- 3 **

``` r
# post hoc for int
data %>%
  group_by(tuning_step) %>%
  pairwise_t_test(
    pct_maj ~ instrument, paired = TRUE, 
    p.adjust.method = "BH" # try different options
    ) %>% 
  select(-`.y.`, -p)
```

    ## # A tibble: 50 × 9
    ##    tuning_step group1  group2       n1    n2 statistic    df p.adj p.adj.signif
    ##          <dbl> <chr>   <chr>     <int> <int>     <dbl> <dbl> <dbl> <chr>       
    ##  1           1 oboe    piano        49    49    -2.94     48 0.021 *           
    ##  2           1 oboe    trumpet      49    49    -2.45     48 0.036 *           
    ##  3           1 oboe    violin       49    49    -1.61     48 0.144 ns          
    ##  4           1 oboe    xylophone    49    49    -3.91     48 0.003 **          
    ##  5           1 piano   trumpet      49    49     1.26     48 0.239 ns          
    ##  6           1 piano   violin       49    49     1.60     48 0.144 ns          
    ##  7           1 piano   xylophone    49    49    -2.18     48 0.057 ns          
    ##  8           1 trumpet violin       49    49     0.365    48 0.717 ns          
    ##  9           1 trumpet xylophone    49    49    -2.50     48 0.036 *           
    ## 10           1 violin  xylophone    49    49    -2.86     48 0.021 *           
    ## # … with 40 more rows

``` r
# table for significant rows to present
```

## Correlations b/w DV & other predictors

``` r
cor.test(data$pct_maj, data$Inst)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  data$pct_maj and data$Inst
    ## t = -1.198, df = 1223, p-value = 0.2311
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.09007283  0.02181426
    ## sample estimates:
    ##         cor 
    ## -0.03423656

``` r
cor.test(data$pct_maj, data$Inst_yr)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  data$pct_maj and data$Inst_yr
    ## t = -0.77761, df = 898, p-value = 0.437
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.0911340  0.0394745
    ## sample estimates:
    ##         cor 
    ## -0.02594045

``` r
cor.test(data$pct_maj, data$music_exp)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  data$pct_maj and data$music_exp
    ## t = -1.6555, df = 1223, p-value = 0.09809
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.103020514  0.008747799
    ## sample estimates:
    ##         cor 
    ## -0.04728436

``` r
cor.test(data$pct_maj, data$Read) #*
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  data$pct_maj and data$Read
    ## t = -2.0261, df = 1223, p-value = 0.04297
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.113480299 -0.001835886
    ## sample estimates:
    ##         cor 
    ## -0.05783893

``` r
cor.test(data$pct_maj, data$headphone)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  data$pct_maj and data$headphone
    ## t = -1.0432, df = 1223, p-value = 0.2971
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.08568194  0.02623677
    ## sample estimates:
    ##         cor 
    ## -0.02981603

``` r
cor.test(data$pct_maj, data$Age)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  data$pct_maj and data$Age
    ## t = 0.098373, df = 1098, p-value = 0.9217
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.05614801  0.06206477
    ## sample estimates:
    ##         cor 
    ## 0.002968749

``` r
cor.test(data$pct_maj, data$explicit_rtg) #***
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  data$pct_maj and data$explicit_rtg
    ## t = 3.6567, df = 1223, p-value = 0.0002663
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.0482674 0.1590777
    ## sample estimates:
    ##       cor 
    ## 0.1039952

Ability to read music and explicit valence rating were significantly
correlated with percent of major categorization

``` r
corrplot(cor(data %>%
  select(pct_maj, explicit_rtg, practice_score, Age, 16:21), use = "complete.obs"), method = "color")
```

    ## Warning in cor(data %>% select(pct_maj, explicit_rtg, practice_score, Age, : the
    ## standard deviation is zero

![](eva_analyses_files/figure-gfm/corrplot-1.png)<!-- -->

``` r
# why NA?
```

## Logistic regression (wrong, should use selected_major as DV)

1)  Percent major \~ instrument & tuning step

``` r
#glm.fit <- glm(pct_maj ~ instrument + tuning_step, family = binomial) 
# family = binomial tells r to run logistic regression
#summary(glm.fit)
```

2)  Percent major \~ mean explicit rating of each instrument & tuning
    step

``` r
#glm.fit2 <- glm(pct_maj ~ mean_rtg + tuning_step, family = binomial) 
# family= binomial tells r to run logistic regression
#summary(glm.fit2)
```

Linear regression

``` r
#lm.fit2 <- lm(pct_maj ~ tuning_step, data = cat)
#lm.fit <- lm(pct_maj ~ instrument + tuning_step, data = cat) 
#summary(lm.fit)
```

ANOVA exploring 1) whether adding instrument as a predictor
significantly improves model, and 2) whether adding both predictors is
significantly better than null model

``` r
#anova(lm.fit, lm.fit2) # adding instrument significantly improves model
#anova(lm.fit) # adding both predictors significantly better than null model
```
