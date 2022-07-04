Instrument and Chord Perception
================
Stephen Van Hedger
22/02/2022

### Overview

Project examining how instrumental timbre influences categorization of
three-note arpeggios as major versus minor. Middle note of the arpeggios
has five steps, ranging from +0c (minor) to +100c (major) in 25c
increments. Participants heard five instruments in total (randomized
across trial): piano, oboe, violin, trumpet, xylophone.

Judgments should be theoretically independent of instrument. However, if
certain timbres are more strongly associated with positive/negative
affect, then it is possible that this will be reflected in the
categorization task (i.e., “happier” instruments leading to a greater
likelihood of responding “major”), despite no explicit instructions to
base judgments on affective characteristics.

### 1. Load Data

``` r
demo_test <- read_csv("demo_test.csv") %>%
  select(-(2:5))
data <- read_csv("inst-cat-uc-1.csv") %>%
  inner_join(demo_test, by = "participant") # discard duplicates, join demo & mus_exp data
```

### 2. Generalized Linear Models

Here we report our main analyses

``` r
data.cat <- data %>% 
  filter(designation == "MAIN-JUDGMENT") # extract cat data

# first, let's confirm that the inclusion of linear/quadratic/cubic effects is warranted via nested models
# random effects | random factor
# random intercepts (1) and (+) random slopes for instrument (instrument) from participant to participant
main.model3 <- lmer(selected_major ~ poly(tuning_step, 3) * instrument + (1 + instrument | participant), data = data.cat)
main.model2 <- lmer(selected_major ~ poly(tuning_step, 2) * instrument + (1 + instrument | participant), data = data.cat)
main.model1 <- lmer(selected_major ~ poly(tuning_step, 1) * instrument + (1 + instrument | participant), data = data.cat)

anova(main.model3, main.model2) # strong evidence to keep cubic fit (versus just quadratic + linear)
```

    ## Data: data.cat
    ## Models:
    ## main.model2: selected_major ~ poly(tuning_step, 2) * instrument + (1 + instrument | participant)
    ## main.model3: selected_major ~ poly(tuning_step, 3) * instrument + (1 + instrument | participant)
    ##             npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## main.model2   31 10388 10610 -5162.8    10326                         
    ## main.model3   36 10132 10391 -5030.1    10060 265.38  5  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(main.model3, main.model1) # strong evidence to keep cubic fit (versus just linear)
```

    ## Data: data.cat
    ## Models:
    ## main.model1: selected_major ~ poly(tuning_step, 1) * instrument + (1 + instrument | participant)
    ## main.model3: selected_major ~ poly(tuning_step, 3) * instrument + (1 + instrument | participant)
    ##             npar   AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## main.model1   26 10411 10598 -5179.4    10359                         
    ## main.model3   36 10132 10391 -5030.1    10060 298.56 10  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# won't need an anova comparing models 1 & 2 b/c if we include 3, 2 will for sure be included

# logit regression
main.model4 <- glmer(selected_major ~ poly(tuning_step, 3) * instrument + music_exp + (1 + instrument | participant), family = binomial(link = "logit"), data = data.cat)
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0850982 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
summary(main.model4)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: selected_major ~ poly(tuning_step, 3) * instrument + music_exp +  
    ##     (1 + instrument | participant)
    ##    Data: data.cat
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9769.9  10028.7  -4848.9   9697.9     9764 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -11.9973  -0.5280   0.0988   0.5296   5.9003 
    ## 
    ## Random effects:
    ##  Groups      Name                Variance Std.Dev. Corr                   
    ##  participant (Intercept)         0.5283   0.7269                          
    ##              instrumentpiano     1.1102   1.0537   -0.80                  
    ##              instrumenttrumpet   0.5561   0.7457   -0.51  0.71            
    ##              instrumentviolin    0.9332   0.9660   -0.62  0.67  0.61      
    ##              instrumentxylophone 2.8771   1.6962   -0.67  0.78  0.51  0.52
    ## Number of obs: 9800, groups:  participant, 49
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error z value
    ## (Intercept)                                -0.478436   0.197487  -2.423
    ## poly(tuning_step, 3)1                     129.600209   5.393401  24.029
    ## poly(tuning_step, 3)2                      10.363136   7.473825   1.387
    ## poly(tuning_step, 3)3                     -24.515484   5.345857  -4.586
    ## instrumentpiano                             0.818031   0.172491   4.742
    ## instrumenttrumpet                           0.711955   0.134780   5.282
    ## instrumentviolin                            0.604698   0.161111   3.753
    ## instrumentxylophone                         1.320084   0.258573   5.105
    ## music_exp                                  -0.006868   0.007054  -0.974
    ## poly(tuning_step, 3)1:instrumentpiano      15.397716   7.694437   2.001
    ## poly(tuning_step, 3)2:instrumentpiano      13.987555   9.036739   1.548
    ## poly(tuning_step, 3)3:instrumentpiano      -8.728300   7.852988  -1.111
    ## poly(tuning_step, 3)1:instrumenttrumpet     5.174795   8.930976   0.579
    ## poly(tuning_step, 3)2:instrumenttrumpet   -23.144965   9.222454  -2.510
    ## poly(tuning_step, 3)3:instrumenttrumpet   -14.102574   7.585095  -1.859
    ## poly(tuning_step, 3)1:instrumentviolin      6.310142   8.632063   0.731
    ## poly(tuning_step, 3)2:instrumentviolin    -12.946616  11.667974  -1.110
    ## poly(tuning_step, 3)3:instrumentviolin     -0.111609   7.534721  -0.015
    ## poly(tuning_step, 3)1:instrumentxylophone  19.744420   8.433041   2.341
    ## poly(tuning_step, 3)2:instrumentxylophone  -6.360808   9.623440  -0.661
    ## poly(tuning_step, 3)3:instrumentxylophone -12.813894   8.325439  -1.539
    ##                                           Pr(>|z|)    
    ## (Intercept)                               0.015409 *  
    ## poly(tuning_step, 3)1                      < 2e-16 ***
    ## poly(tuning_step, 3)2                     0.165567    
    ## poly(tuning_step, 3)3                     4.52e-06 ***
    ## instrumentpiano                           2.11e-06 ***
    ## instrumenttrumpet                         1.28e-07 ***
    ## instrumentviolin                          0.000175 ***
    ## instrumentxylophone                       3.30e-07 ***
    ## music_exp                                 0.330290    
    ## poly(tuning_step, 3)1:instrumentpiano     0.045376 *  
    ## poly(tuning_step, 3)2:instrumentpiano     0.121657    
    ## poly(tuning_step, 3)3:instrumentpiano     0.266369    
    ## poly(tuning_step, 3)1:instrumenttrumpet   0.562305    
    ## poly(tuning_step, 3)2:instrumenttrumpet   0.012086 *  
    ## poly(tuning_step, 3)3:instrumenttrumpet   0.062992 .  
    ## poly(tuning_step, 3)1:instrumentviolin    0.464772    
    ## poly(tuning_step, 3)2:instrumentviolin    0.267178    
    ## poly(tuning_step, 3)3:instrumentviolin    0.988182    
    ## poly(tuning_step, 3)1:instrumentxylophone 0.019216 *  
    ## poly(tuning_step, 3)2:instrumentxylophone 0.508631    
    ## poly(tuning_step, 3)3:instrumentxylophone 0.123774    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.0850982 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
# turn log odds back to probability
log_mod <- margins(main.model4, data = data.cat)
log_mod
```

    ##  tuning_step music_exp instrumentpiano instrumenttrumpet instrumentviolin
    ##       0.1014 -0.001071          0.1274            0.1224           0.1027
    ##  instrumentxylophone
    ##               0.1994

``` r
cat.emm_0 <- emmeans(main.model4, "instrument")
pairs(cat.emm_0)
```

    ##  contrast            estimate    SE  df z.ratio p.value
    ##  oboe - piano          -0.649 0.196 Inf  -3.319  0.0080
    ##  oboe - trumpet        -0.991 0.166 Inf  -5.984  <.0001
    ##  oboe - violin         -0.761 0.202 Inf  -3.773  0.0015
    ##  oboe - xylophone      -1.397 0.275 Inf  -5.071  <.0001
    ##  piano - trumpet       -0.342 0.156 Inf  -2.199  0.1801
    ##  piano - violin        -0.112 0.167 Inf  -0.670  0.9629
    ##  piano - xylophone     -0.748 0.199 Inf  -3.763  0.0016
    ##  trumpet - violin       0.230 0.165 Inf   1.398  0.6288
    ##  trumpet - xylophone   -0.405 0.242 Inf  -1.673  0.4506
    ##  violin - xylophone    -0.636 0.243 Inf  -2.619  0.0670
    ## 
    ## Results are given on the log odds ratio (not the response) scale. 
    ## P value adjustment: tukey method for comparing a family of 5 estimates

``` r
# cat results from the selected cubic model
summary(main.model3)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## selected_major ~ poly(tuning_step, 3) * instrument + (1 + instrument |  
    ##     participant)
    ##    Data: data.cat
    ## 
    ## REML criterion at convergence: 10066.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0094 -0.6378 -0.0527  0.6665  2.7813 
    ## 
    ## Random effects:
    ##  Groups      Name                Variance Std.Dev. Corr                   
    ##  participant (Intercept)         0.01227  0.1108                          
    ##              instrumentpiano     0.02714  0.1648   -0.77                  
    ##              instrumenttrumpet   0.01395  0.1181   -0.47  0.69            
    ##              instrumentviolin    0.02399  0.1549   -0.58  0.65  0.58      
    ##              instrumentxylophone 0.05813  0.2411   -0.69  0.81  0.50  0.54
    ##  Residual                        0.15788  0.3973                          
    ## Number of obs: 9800, groups:  participant, 49
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error         df
    ## (Intercept)                                  0.39388    0.01819   47.99177
    ## poly(tuning_step, 3)1                       24.35714    0.88849 9540.00026
    ## poly(tuning_step, 3)2                        3.25988    0.88849 9540.00026
    ## poly(tuning_step, 3)3                       -4.96429    0.88849 9540.00026
    ## instrumentpiano                              0.12755    0.02674   47.98876
    ## instrumenttrumpet                            0.12194    0.02111   47.98987
    ## instrumentviolin                             0.10255    0.02551   47.98669
    ## instrumentxylophone                          0.19898    0.03671   47.98967
    ## poly(tuning_step, 3)1:instrumentpiano        2.82143    1.25651 9540.00026
    ## poly(tuning_step, 3)2:instrumentpiano        0.03018    1.25651 9540.00026
    ## poly(tuning_step, 3)3:instrumentpiano       -2.42857    1.25651 9540.00026
    ## poly(tuning_step, 3)1:instrumenttrumpet      1.64286    1.25651 9540.00026
    ## poly(tuning_step, 3)2:instrumenttrumpet     -5.55387    1.25651 9540.00026
    ## poly(tuning_step, 3)3:instrumenttrumpet     -2.75000    1.25651 9540.00026
    ## poly(tuning_step, 3)1:instrumentviolin       1.46429    1.25651 9540.00026
    ## poly(tuning_step, 3)2:instrumentviolin      -3.59191    1.25651 9540.00026
    ## poly(tuning_step, 3)3:instrumentviolin      -0.60714    1.25651 9540.00026
    ## poly(tuning_step, 3)1:instrumentxylophone   -0.39286    1.25651 9540.00026
    ## poly(tuning_step, 3)2:instrumentxylophone   -3.59191    1.25651 9540.00026
    ## poly(tuning_step, 3)3:instrumentxylophone   -1.53571    1.25651 9540.00026
    ##                                           t value Pr(>|t|)    
    ## (Intercept)                                21.653  < 2e-16 ***
    ## poly(tuning_step, 3)1                      27.414  < 2e-16 ***
    ## poly(tuning_step, 3)2                       3.669 0.000245 ***
    ## poly(tuning_step, 3)3                      -5.587 2.37e-08 ***
    ## instrumentpiano                             4.770 1.76e-05 ***
    ## instrumenttrumpet                           5.775 5.52e-07 ***
    ## instrumentviolin                            4.020 0.000205 ***
    ## instrumentxylophone                         5.421 1.90e-06 ***
    ## poly(tuning_step, 3)1:instrumentpiano       2.245 0.024762 *  
    ## poly(tuning_step, 3)2:instrumentpiano       0.024 0.980835    
    ## poly(tuning_step, 3)3:instrumentpiano      -1.933 0.053292 .  
    ## poly(tuning_step, 3)1:instrumenttrumpet     1.307 0.191083    
    ## poly(tuning_step, 3)2:instrumenttrumpet    -4.420 9.98e-06 ***
    ## poly(tuning_step, 3)3:instrumenttrumpet    -2.189 0.028650 *  
    ## poly(tuning_step, 3)1:instrumentviolin      1.165 0.243903    
    ## poly(tuning_step, 3)2:instrumentviolin     -2.859 0.004264 ** 
    ## poly(tuning_step, 3)3:instrumentviolin     -0.483 0.628967    
    ## poly(tuning_step, 3)1:instrumentxylophone  -0.313 0.754548    
    ## poly(tuning_step, 3)2:instrumentxylophone  -2.859 0.004264 ** 
    ## poly(tuning_step, 3)3:instrumentxylophone  -1.222 0.221661    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
cat.emm <- emmeans(main.model3, "instrument")
pairs(cat.emm) # oboe < all others except piano; xylophone > piano
```

    ##  contrast            estimate     SE  df z.ratio p.value
    ##  oboe - piano         -0.1272 0.0307 Inf  -4.137  0.0003
    ##  oboe - trumpet       -0.1890 0.0260 Inf  -7.269  <.0001
    ##  oboe - violin        -0.1459 0.0297 Inf  -4.916  <.0001
    ##  oboe - xylophone     -0.2423 0.0397 Inf  -6.102  <.0001
    ##  piano - trumpet      -0.0618 0.0262 Inf  -2.363  0.1256
    ##  piano - violin       -0.0187 0.0275 Inf  -0.682  0.9603
    ##  piano - xylophone    -0.1152 0.0287 Inf  -4.012  0.0006
    ##  trumpet - violin      0.0431 0.0270 Inf   1.595  0.5005
    ##  trumpet - xylophone  -0.0534 0.0359 Inf  -1.488  0.5703
    ##  violin - xylophone   -0.0964 0.0353 Inf  -2.731  0.0495
    ## 
    ## Degrees-of-freedom method: asymptotic 
    ## P value adjustment: tukey method for comparing a family of 5 estimates

``` r
# assess explicit ratings of instruments
data.exp <- data %>% 
  filter(designation == "INST-VALENCE-RTG")

data.exp$explicit_rtg <- ordered(data.exp$explicit_rtg)

explicit.model <- clmm(explicit_rtg ~ instrument + (1 | participant), data = data.exp)
summary(explicit.model) # clear differences in reported capacity for instruments to play happy/sad
```

    ## Cumulative Link Mixed Model fitted with the Laplace approximation
    ## 
    ## formula: explicit_rtg ~ instrument + (1 | participant)
    ## data:    data.exp
    ## 
    ##  link  threshold nobs logLik  AIC    niter    max.grad cond.H 
    ##  logit flexible  245  -265.56 547.12 460(932) 8.56e-06 3.4e+01
    ## 
    ## Random effects:
    ##  Groups      Name        Variance Std.Dev.
    ##  participant (Intercept) 0.2866   0.5354  
    ## Number of groups:  participant 49 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)    
    ## instrumentpiano       1.2746     0.3909   3.260  0.00111 ** 
    ## instrumenttrumpet     1.1884     0.3940   3.016  0.00256 ** 
    ## instrumentviolin     -0.7076     0.3926  -1.802  0.07149 .  
    ## instrumentxylophone   1.9490     0.4182   4.661 3.15e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Threshold coefficients:
    ##     Estimate Std. Error z value
    ## 1|2  -2.0592     0.3480  -5.917
    ## 2|3   0.6563     0.2913   2.253
    ## 3|4   3.2204     0.3792   8.494

``` r
explicit.emm <- emmeans(explicit.model, "instrument")
pairs(explicit.emm) # violin < all others except oboe; xylophone > oboe
```

    ##  contrast            estimate    SE  df z.ratio p.value
    ##  oboe - piano         -1.2746 0.391 Inf  -3.260  0.0098
    ##  oboe - trumpet       -1.1884 0.394 Inf  -3.016  0.0216
    ##  oboe - violin         0.7076 0.393 Inf   1.802  0.3721
    ##  oboe - xylophone     -1.9490 0.418 Inf  -4.661  <.0001
    ##  piano - trumpet       0.0862 0.385 Inf   0.224  0.9994
    ##  piano - violin        1.9822 0.411 Inf   4.827  <.0001
    ##  piano - xylophone    -0.6744 0.399 Inf  -1.691  0.4394
    ##  trumpet - violin      1.8960 0.413 Inf   4.594  <.0001
    ##  trumpet - xylophone  -0.7607 0.404 Inf  -1.881  0.3274
    ##  violin - xylophone   -2.6567 0.439 Inf  -6.057  <.0001
    ## 
    ## P value adjustment: tukey method for comparing a family of 5 estimates

### 3. Cat-rtg correlation plot

``` r
# how do categorization results map onto explicit ratings? 
# [aggregate = summarize (by mean) in dplyr]
cat.sum <- aggregate(selected_major ~ instrument*participant, FUN = mean, data = data.cat)
exp.sum <- aggregate(as.numeric(explicit_rtg) ~ instrument*participant, FUN = mean, data = data.exp)

#convert to wide format for correlational analyses [= pivot wider]
cat.sum.wide <- dcast(cat.sum, participant ~ instrument) 
exp.sum.wide <- dcast(exp.sum, participant ~ instrument)
cor.data <- cbind(cat.sum.wide[, c(2:6)], exp.sum.wide[, c(2:6)])
```

``` r
names(cor.data) = c("Oboe C", "Piano C", "Trumpet C", "Violin C", "Xylophone C", "Oboe E", "Piano E", "Trumpet E", "Violin E", "Xylophone E")
corvalues <- cor(cor.data) #cor matrix

col <- colorRampPalette(c("#4477AA",  "#77AADD", "#FFFFFF","#BB4444", "#EE9988"))
corrplot(corvalues, method = "color", col=col(200),    
         type = "upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 45, # Text label color and rotation
         # Combine with significance
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)
```

![](instcat-analysis_files/figure-gfm/corr-plot-1.png)<!-- -->

### 4. Categorization smooth plot

``` r
eff.inst <- as.data.frame(effect(term = "poly(tuning_step,3)*instrument", mod = main.model3)) 
str(eff.inst) # shows structure of df
```

    ## 'data.frame':    25 obs. of  6 variables:
    ##  $ tuning_step: num  1 2 3 4 5 1 2 3 4 5 ...
    ##  $ instrument : Factor w/ 6 levels "oboe","piano",..: 1 1 1 1 1 2 2 2 2 2 ...
    ##  $ fit        : num  0.121 0.129 0.355 0.619 0.746 ...
    ##  $ se         : num  0.0254 0.0237 0.0211 0.0237 0.0254 ...
    ##  $ lower      : num  0.0709 0.0829 0.3131 0.5727 0.6959 ...
    ##  $ upper      : num  0.171 0.176 0.396 0.666 0.796 ...
    ##  - attr(*, "type")= chr "response"

``` r
levels(eff.inst$instrument) <- c("Oboe", "Piano", "Trumpet", "Violin", "Xylophone", NA)

efplot.main <- ggplot(eff.inst, aes(x = tuning_step, y = fit, color = instrument)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = fit - se, ymax = fit + se), width = 0.1, lwd = 1) +
  #geom_line(size=1.2) +
  geom_smooth(formula = "y ~ poly(x, 3)", method = "glm", family = binomial, se = FALSE) +
  scale_x_continuous(labels = c("0", "25", "50", "75", "100")) +
  labs(y = "'Major' Response (Prop.)", x = "Tuning Step", color = "Instrument") +
  theme(plot.title = element_text(lineheight = .8, face = "bold", size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.box = "vertical", 
        legend.background = element_rect(color = NA),
        legend.position = c(0.20, 0.75),
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.border = element_rect(color = "black", fill=NA, size=1.25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
        axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"),
        strip.text = element_text(size = 14))
```

    ## Warning: Ignoring unknown parameters: family

``` r
png("inst-cat-plot.png", height = 4, width = 5.5, res = 300, units = "in")
efplot.main
```
