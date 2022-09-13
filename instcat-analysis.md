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
  inner_join(demo_test, by = "participant") # join demo & mus_exp data, discard duplicates

data.cat <- data %>% 
  filter(designation == "MAIN-JUDGMENT") # extract cat data
data.exp <- data %>% 
  filter(designation == "INST-VALENCE-RTG") # extract rtg data
```

### 2. Generalized Linear Models

Here we report our main analyses

``` r
# first, let's confirm that the inclusion of linear/quadratic/cubic effects is warranted via nested models
# random effects | random factor
# random intercepts (1) and (+) random slopes for instrument (instrument) from participant to participant
main.model3 <- glmer(selected_major ~ poly(tuning_step, 3) * instrument + (1 + instrument | participant), data = data.cat, family = binomial)
```

    ## Warning in (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf, :
    ## failure to converge in 10000 evaluations

    ## Warning in optwrap(optimizer, devfun, start, rho$lower, control = control, :
    ## convergence code 4 from Nelder_Mead: failure to converge in 10000 evaluations

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0181057 (tol = 0.002, component 1)

``` r
main.model2 <- glmer(selected_major ~ poly(tuning_step, 2) * instrument + (1 + instrument | participant), data = data.cat, family = binomial)
```

    ## Warning in (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf, :
    ## failure to converge in 10000 evaluations

    ## Warning in optwrap(optimizer, devfun, start, rho$lower, control = control, :
    ## convergence code 4 from Nelder_Mead: failure to converge in 10000 evaluations

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## unable to evaluate scaled gradient

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge: degenerate Hessian with 2 negative eigenvalues

``` r
main.model1 <- glmer(selected_major ~ poly(tuning_step, 1) * instrument + (1 + instrument | participant), data = data.cat, family = binomial)
```

    ## Warning in (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf, :
    ## failure to converge in 10000 evaluations

    ## Warning in optwrap(optimizer, devfun, start, rho$lower, control = control, :
    ## convergence code 4 from Nelder_Mead: failure to converge in 10000 evaluations

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.03801 (tol = 0.002, component 1)

``` r
anova(main.model3, main.model2) # strong evidence to keep cubic fit (versus just quadratic + linear)
```

    ## Data: data.cat
    ## Models:
    ## main.model2: selected_major ~ poly(tuning_step, 2) * instrument + (1 + instrument | participant)
    ## main.model3: selected_major ~ poly(tuning_step, 3) * instrument + (1 + instrument | participant)
    ##             npar    AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## main.model2   30 9911.0 10127 -4925.5   9851.0                         
    ## main.model3   35 9768.8 10020 -4849.4   9698.8 152.26  5  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(main.model3, main.model1) # strong evidence to keep cubic fit (versus just linear)
```

    ## Data: data.cat
    ## Models:
    ## main.model1: selected_major ~ poly(tuning_step, 1) * instrument + (1 + instrument | participant)
    ## main.model3: selected_major ~ poly(tuning_step, 3) * instrument + (1 + instrument | participant)
    ##             npar    AIC   BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## main.model1   25 9932.3 10112 -4941.2   9882.3                         
    ## main.model3   35 9768.8 10020 -4849.4   9698.8 183.53 10  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# won't need an anova comparing models 1 & 2 b/c if we include 3, 2 will for sure be included

# cat results from the selected cubic model
summary(main.model3)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## selected_major ~ poly(tuning_step, 3) * instrument + (1 + instrument |  
    ##     participant)
    ##    Data: data.cat
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   9768.8  10020.4  -4849.4   9698.8     9765 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -12.0311  -0.5291   0.0986   0.5318   5.7927 
    ## 
    ## Random effects:
    ##  Groups      Name                Variance Std.Dev. Corr                   
    ##  participant (Intercept)         0.4901   0.7001                          
    ##              instrumentpiano     1.1044   1.0509   -0.78                  
    ##              instrumenttrumpet   0.5524   0.7432   -0.50  0.71            
    ##              instrumentviolin    0.9299   0.9643   -0.60  0.67  0.61      
    ##              instrumentxylophone 2.8693   1.6939   -0.65  0.78  0.51  0.52
    ## Number of obs: 9800, groups:  participant, 49
    ## 
    ## Fixed effects:
    ##                                            Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                                -0.63209    0.11588  -5.455 4.91e-08
    ## poly(tuning_step, 3)1                     129.31929    5.50495  23.491  < 2e-16
    ## poly(tuning_step, 3)2                      10.42999    4.51597   2.310 0.020912
    ## poly(tuning_step, 3)3                     -24.57485    5.55853  -4.421 9.82e-06
    ## instrumentpiano                             0.81822    0.17176   4.764 1.90e-06
    ## instrumenttrumpet                           0.71172    0.13399   5.312 1.08e-07
    ## instrumentviolin                            0.60487    0.16003   3.780 0.000157
    ## instrumentxylophone                         1.32055    0.25747   5.129 2.91e-07
    ## poly(tuning_step, 3)1:instrumentpiano      15.88918    7.47691   2.125 0.033578
    ## poly(tuning_step, 3)2:instrumentpiano      13.95646    6.62374   2.107 0.035114
    ## poly(tuning_step, 3)3:instrumentpiano      -8.74885    8.77178  -0.997 0.318577
    ## poly(tuning_step, 3)1:instrumenttrumpet     5.37059    7.91591   0.678 0.497483
    ## poly(tuning_step, 3)2:instrumenttrumpet   -23.20038    7.43855  -3.119 0.001815
    ## poly(tuning_step, 3)3:instrumenttrumpet   -13.98823    8.08725  -1.730 0.083690
    ## poly(tuning_step, 3)1:instrumentviolin      6.65697    8.41180   0.791 0.428720
    ## poly(tuning_step, 3)2:instrumentviolin    -13.02081    5.75536  -2.262 0.023674
    ## poly(tuning_step, 3)3:instrumentviolin     -0.05312    7.10672  -0.007 0.994036
    ## poly(tuning_step, 3)1:instrumentxylophone  20.15929    8.09517   2.490 0.012764
    ## poly(tuning_step, 3)2:instrumentxylophone  -6.40921    7.71927  -0.830 0.406377
    ## poly(tuning_step, 3)3:instrumentxylophone -12.73305    8.69511  -1.464 0.143087
    ##                                              
    ## (Intercept)                               ***
    ## poly(tuning_step, 3)1                     ***
    ## poly(tuning_step, 3)2                     *  
    ## poly(tuning_step, 3)3                     ***
    ## instrumentpiano                           ***
    ## instrumenttrumpet                         ***
    ## instrumentviolin                          ***
    ## instrumentxylophone                       ***
    ## poly(tuning_step, 3)1:instrumentpiano     *  
    ## poly(tuning_step, 3)2:instrumentpiano     *  
    ## poly(tuning_step, 3)3:instrumentpiano        
    ## poly(tuning_step, 3)1:instrumenttrumpet      
    ## poly(tuning_step, 3)2:instrumenttrumpet   ** 
    ## poly(tuning_step, 3)3:instrumenttrumpet   .  
    ## poly(tuning_step, 3)1:instrumentviolin       
    ## poly(tuning_step, 3)2:instrumentviolin    *  
    ## poly(tuning_step, 3)3:instrumentviolin       
    ## poly(tuning_step, 3)1:instrumentxylophone *  
    ## poly(tuning_step, 3)2:instrumentxylophone    
    ## poly(tuning_step, 3)3:instrumentxylophone    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## optimizer (Nelder_Mead) convergence code: 4 (failure to converge in 10000 evaluations)
    ## Model failed to converge with max|grad| = 0.0181057 (tol = 0.002, component 1)
    ## failure to converge in 10000 evaluations

``` r
cat.emm <- emmeans(main.model3, "instrument")
# post hoc - paired comparisons
pairs(cat.emm) # oboe < all others except piano; xylophone > piano
```

    ##  contrast            estimate    SE  df z.ratio p.value
    ##  oboe - piano          -0.650 0.183 Inf  -3.543  0.0036
    ##  oboe - trumpet        -0.992 0.156 Inf  -6.356  <.0001
    ##  oboe - violin         -0.762 0.171 Inf  -4.460  0.0001
    ##  oboe - xylophone      -1.398 0.270 Inf  -5.173  <.0001
    ##  piano - trumpet       -0.342 0.160 Inf  -2.145  0.2012
    ##  piano - violin        -0.112 0.159 Inf  -0.706  0.9553
    ##  piano - xylophone     -0.748 0.199 Inf  -3.756  0.0016
    ##  trumpet - violin       0.230 0.159 Inf   1.444  0.5988
    ##  trumpet - xylophone   -0.406 0.243 Inf  -1.670  0.4524
    ##  violin - xylophone    -0.636 0.239 Inf  -2.658  0.0603
    ## 
    ## Results are given on the log odds ratio (not the response) scale. 
    ## P value adjustment: tukey method for comparing a family of 5 estimates

``` r
# assess explicit ratings of instruments
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
# post hoc - paired comparisons
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

This is to explore whether categorization and explicit rating results
are correlated.

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
corrplot(corvalues, method = "color", col = col(200),    
         type = "upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 45, # Text label color and rotation
         # Combine with significance
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)
```

![](instcat-analysis_files/figure-gfm/corr-plot-1.png)<!-- -->

### 4. Categorization smooth plot

This plot shows how the smooth curve of proportion of major
categorization against tuning step differs across instruments.

``` r
eff.inst <- as.data.frame(effect(term = "poly(tuning_step,3)*instrument", mod = main.model3)) 
str(eff.inst) # shows structure of df
```

    ## 'data.frame':    25 obs. of  6 variables:
    ##  $ tuning_step: num  1 2 3 4 5 1 2 3 4 5 ...
    ##  $ instrument : Factor w/ 6 levels "oboe","piano",..: 1 1 1 1 1 2 2 2 2 2 ...
    ##  $ fit        : num  0.102 0.122 0.319 0.641 0.762 ...
    ##  $ se         : num  0.015 0.0155 0.0272 0.0322 0.029 ...
    ##  $ lower      : num  0.0759 0.0951 0.2682 0.5758 0.7009 ...
    ##  $ upper      : num  0.135 0.156 0.375 0.701 0.815 ...
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
# save plot
png("inst-cat-plot.png", height = 4, width = 5.5, res = 300, units = "in")
efplot.main
```
