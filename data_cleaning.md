Data Cleaning & Exploratory Graphs
================
Eva Wu
2022-05-18

## Recap

### Hypothesis

“Happy” instruments would make people more prone to identify the chord
as major, while “sad” instruments might make people more prone to
identify the chord as minor.

### Exploratory research questions

1)  Association between timbre and tonality judgment

2)  Association between timbre and explicit ratings of instrument
    valence

3)  Association between tonality judgment and explicit ratings of
    instrument valence

4)  Association between musical background and tonality judgment and/or
    explicit ratings of instrument valence

### Design

-   IV1 (w/in-subject): instrument (happy \[xylophone, trumpet\]
    vs. neutral \[piano\] vs. sad \[oboe, violin\])

-   IV2 (w/in-subject): tuning of middle note (5 levels, ranging from
    absolute minor to absolute major)

-   IV3 (b/w-subject): key (B vs. C) (to find out absolute-pitch-related
    effects)

-   DV: the likelihood that one categorizes a chord as major/minor

### Procedure

-   Pt 1 Sound calibration & headphone test (choose the quietest sound
    among 3)

-   Pt 2 Training (press the buttons to listen to the chords, practice
    w/ feedback) + testing phase (listen to 12 chords and choose b/w
    major and minor for each, need to correctly answer 8 to pass)

-   only analyze the response of those who pass the assessment w/in 2
    tries

-   Pt 3 Categorization task (jspsych) - listen to 4 blocks of 70 chords
    and choose b/w major and minor for each chord; explicit rating of
    instrument valence at the end

-   Pt 4 Questionnaires (demographics & music experience; Qualtrics)

## Clean Data

``` r
# jspsych data
df_j <- read_csv("inst-cat-uc-1.csv") %>% # load exp1 data
  filter(designation != "NA" & designation != "practice-intro" & designation != "practice-resp" & 
         qualtrics_id != "NA" & qualtrics_id != "9643222579") %>% # delete pilot data
  select(participant, qualtrics_id, chord, designation, response, # delete meta data
         correct, passed_practice, block_passed_practice, practice_score, 
         instrument, valence, tuning_step, selected_major, explicit_rtg) %>% 
  # for some reason 2 participant's qualtrics id was "participant", so I recoded according to their id on Qualtrics
  mutate(qualtrics_id = if_else(participant == "ch4dg75c7th9g1", "1706631024", qualtrics_id), 
         qualtrics_id = if_else(participant == "ept8xz3drgq38w", "4475978126", qualtrics_id),
         instrument = factor(instrument, levels = c("xylophone", "trumpet", "piano", "violin", "oboe")))

# qualtrics data
df_q <- read_csv("Qualtrics_5:4.csv") %>%
  filter(row_number() > 7) %>% # delete pilot data
  select(-(2:16), -50) # delete unnecessary meta data

# join qualtrics & jspsych data by embedded qualtrics id
# discard those who do not have a matching qualtrics_id in qualtrics data, keep everyone in jspsych data
combined <- left_join(df_j, df_q, by = c("qualtrics_id" = "participant"))
```

``` r
# those who only have j data but not q
na_q <- combined %>%
  filter(is.na(Gender)) %>%
  select(1:14) # delete q columns
# there are 4 of them w/ blank qualtrics data but full jspsych data

# those who only have q data but not j
na_j <- df_q %>%
  # assume 22-0159 in q is 5281kj47nhoboj in j
  mutate(jspsych_id = if_else(jspsych_id == "22-0159", "5281kj47nhoboj", jspsych_id)) %>% 
  semi_join(na_q, by = c("jspsych_id" = "participant"))

# join the half-full data together according to jspsych id
na_combined <- full_join(na_q, na_j, by = c("participant" = "jspsych_id"), keep = TRUE) %>%
  select(-participant.y) %>%
  rename(participant = participant.x)

# bind them back to combined, delete the half-full rows
combined_drop_na <- rbind(combined, na_combined) %>%
  filter(!is.na(Gender))
```

``` r
# df for demographics
# need to find a way to summarize musical expertise
demo <- combined_drop_na %>%
  filter(designation == "PRACTICE-PASSED-SUMMARY") %>%
  select(-1, -(3:6), -(10:16), -49) %>% # delete unnecessary data
  mutate(Age = as.numeric(Age))

# df for headphone test
test <- combined_drop_na %>%
  filter(designation == "headphone-test") %>%
  select(2, 6) %>%
  group_by(qualtrics_id) %>%
  summarize(n = sum(correct)) %>%
  # set 4 out of 6 as passed headphone test
  mutate(headphone = if_else(n >= 4, 1, 0),
         test_corr = n) %>%
  select(-n)

# combine headphone test pass/fail result w/ original data
demo_test <- left_join(demo, test, by = "qualtrics_id")

# see how many failed the headphone test
test %>%
  filter(headphone == 0)
```

    ## # A tibble: 8 × 3
    ##   qualtrics_id headphone test_corr
    ##   <chr>            <dbl>     <dbl>
    ## 1 1044403354           0         2
    ## 2 2077443174           0         2
    ## 3 3619423604           0         0
    ## 4 5405144116           0         0
    ## 5 6323213291           0         2
    ## 6 6783315289           0         2
    ## 7 8089229725           0         0
    ## 8 9018846073           0         1

Turns out 8 participants failed the headphone test.

``` r
# df for categorization
cat <- combined_drop_na %>%
  filter(designation == "MAIN-JUDGMENT") %>%
  select(2:3, 10:13) %>%
  group_by(qualtrics_id, instrument, tuning_step) %>%
  summarize(pct_maj = mean(selected_major))
  
# each person's mean major choice proportion 
cat_pivoted <- cat %>%
  group_by(qualtrics_id) %>%
  summarize(mean_pct_maj_across_indvl = mean(pct_maj))

# delete since no point of having 25 cols for major judgment
# pivot_wider(names_from = c(instrument, tuning_step), values_from = pct_maj)

# df for explicit rating
rtg <- combined_drop_na %>%
  filter(designation == "INST-VALENCE-RTG") %>%
  select(qualtrics_id, instrument, explicit_rtg)

# pivot so that one person has one row
rtg_pivoted <- rtg %>%
  pivot_wider(names_from = instrument, values_from = explicit_rtg)

# df for both combined
cat_rtg <- left_join(cat, rtg)

# each instrument's mean cat & rtg
cat_rtg_summary <- cat_rtg %>%
  group_by(instrument) %>%
  summarize(mean_pct = sum(pct_maj) / 245, # 245 = # of times each instrument appears
         mean_rtg = sum(explicit_rtg) / 245)

# both pivoted combined
cat_rtg_pivoted <- cat_pivoted %>% 
  left_join(rtg_pivoted, by = c("qualtrics_id"))

# delete the following since no need
# mutate(inst_id = case_when(instrument == "oboe" ~ 1, instrument == "violin" ~ 2,
# instrument == "piano" ~ 3, instrument == "trumpet" ~ 4, instrument == "xylophone" ~ 5))

# each participant = 1 row of data
all <- left_join(cat_rtg_pivoted, demo_test) %>%
  mutate(Inst = if_else(Inst == "Yes", 1, 0),
         Start = as.integer(Start),
         Inst_now = if_else(Inst_now == "Yes", 1, 0),
         Ens = if_else(Ens == "Yes", 1, 0),
         Course = if_else(Course == "Yes", 1, 0),
         Read = if_else(Read == "Yes", 1, 0),
         `Pitch&Tempo_1` = as.integer(`Pitch&Tempo_1`),
         `Pitch&Tempo_2` = as.integer(`Pitch&Tempo_2`),
         Perf = case_when(Perf == "Yes" ~ 1,
                          Perf == "No" ~ 0,
                          Perf == "Not sure" ~ -1),
         Concert = as.integer(Concert))
```

A snapshot of the data (next step: find a measure to summarize musical
background)

``` r
variable_names = colnames(all)
variable_meaning = c("Random 10-digit ID assigned by Qualtrics, used to join Qualtrics and jspsych data", "Each individual's average percent of major choices",
                "Explicit valence rating of xylophone", "Explicit valence rating of violin", "Explicit valence rating of piano",
                "Explicit valence rating of trumpet", "Explicit valence rating of oboe", "Passed practice or not (1 = pass)", "Number of tries taken to pass", 
                "Practice score (out of 12)", "", "", "", "If not a student or faculty", "", "If not a psych of music major", "Play instrument or not", 
                "At what age did they start playing an instrument", "Still playing now?", "List instruments played", "Participated in ensemble or not",
                "Taken music courses or not", "List music courses taken", "Read music or not", "Absolute pitch?", "Absolute tempo?", "Perfect pitch", 
                "Time spent making music", "Time spent listening to music", "Number of concert attended per month", "Proportion of each genre listened to (total 100)",
                "", "", "", "", "", "", "", "", "", "", "", "Passed headphone test or not (1 = pass)", "Number of right answers in headphone test")
codebook <- data.frame(variable_names, variable_meaning)
kable(codebook)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
variable_names
</th>
<th style="text-align:left;">
variable_meaning
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
qualtrics_id
</td>
<td style="text-align:left;">
Random 10-digit ID assigned by Qualtrics, used to join Qualtrics and
jspsych data
</td>
</tr>
<tr>
<td style="text-align:left;">
mean_pct_maj_across_indvl
</td>
<td style="text-align:left;">
Each individual’s average percent of major choices
</td>
</tr>
<tr>
<td style="text-align:left;">
xylophone
</td>
<td style="text-align:left;">
Explicit valence rating of xylophone
</td>
</tr>
<tr>
<td style="text-align:left;">
violin
</td>
<td style="text-align:left;">
Explicit valence rating of violin
</td>
</tr>
<tr>
<td style="text-align:left;">
piano
</td>
<td style="text-align:left;">
Explicit valence rating of piano
</td>
</tr>
<tr>
<td style="text-align:left;">
trumpet
</td>
<td style="text-align:left;">
Explicit valence rating of trumpet
</td>
</tr>
<tr>
<td style="text-align:left;">
oboe
</td>
<td style="text-align:left;">
Explicit valence rating of oboe
</td>
</tr>
<tr>
<td style="text-align:left;">
passed_practice
</td>
<td style="text-align:left;">
Passed practice or not (1 = pass)
</td>
</tr>
<tr>
<td style="text-align:left;">
block_passed_practice
</td>
<td style="text-align:left;">
Number of tries taken to pass
</td>
</tr>
<tr>
<td style="text-align:left;">
practice_score
</td>
<td style="text-align:left;">
Practice score (out of 12)
</td>
</tr>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Gender
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Year
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Year_6\_TEXT
</td>
<td style="text-align:left;">
If not a student or faculty
</td>
</tr>
<tr>
<td style="text-align:left;">
Major
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Major_5\_TEXT
</td>
<td style="text-align:left;">
If not a psych of music major
</td>
</tr>
<tr>
<td style="text-align:left;">
Inst
</td>
<td style="text-align:left;">
Play instrument or not
</td>
</tr>
<tr>
<td style="text-align:left;">
Start
</td>
<td style="text-align:left;">
At what age did they start playing an instrument
</td>
</tr>
<tr>
<td style="text-align:left;">
Inst_now
</td>
<td style="text-align:left;">
Still playing now?
</td>
</tr>
<tr>
<td style="text-align:left;">
Inst_list
</td>
<td style="text-align:left;">
List instruments played
</td>
</tr>
<tr>
<td style="text-align:left;">
Ens
</td>
<td style="text-align:left;">
Participated in ensemble or not
</td>
</tr>
<tr>
<td style="text-align:left;">
Course
</td>
<td style="text-align:left;">
Taken music courses or not
</td>
</tr>
<tr>
<td style="text-align:left;">
Course_list
</td>
<td style="text-align:left;">
List music courses taken
</td>
</tr>
<tr>
<td style="text-align:left;">
Read
</td>
<td style="text-align:left;">
Read music or not
</td>
</tr>
<tr>
<td style="text-align:left;">
Pitch&Tempo_1
</td>
<td style="text-align:left;">
Absolute pitch?
</td>
</tr>
<tr>
<td style="text-align:left;">
Pitch&Tempo_2
</td>
<td style="text-align:left;">
Absolute tempo?
</td>
</tr>
<tr>
<td style="text-align:left;">
Perf
</td>
<td style="text-align:left;">
Perfect pitch
</td>
</tr>
<tr>
<td style="text-align:left;">
Time_make
</td>
<td style="text-align:left;">
Time spent making music
</td>
</tr>
<tr>
<td style="text-align:left;">
Time_listen
</td>
<td style="text-align:left;">
Time spent listening to music
</td>
</tr>
<tr>
<td style="text-align:left;">
Concert
</td>
<td style="text-align:left;">
Number of concert attended per month
</td>
</tr>
<tr>
<td style="text-align:left;">
Genre_18
</td>
<td style="text-align:left;">
Proportion of each genre listened to (total 100)
</td>
</tr>
<tr>
<td style="text-align:left;">
Genre_8
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Genre_9
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Genre_17
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Genre_10
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Genre_11
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Genre_12
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Genre_13
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Genre_16
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Genre_14
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Genre_15
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Genre_15_TEXT
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
headphone
</td>
<td style="text-align:left;">
Passed headphone test or not (1 = pass)
</td>
</tr>
<tr>
<td style="text-align:left;">
test_corr
</td>
<td style="text-align:left;">
Number of right answers in headphone test
</td>
</tr>
</tbody>
</table>

## Demographics

``` r
demo %>%
  filter(Gender != "NA") %>%
  ggplot(aes(Gender, fill = Gender)) +
  geom_bar() +
  labs(title = "Participant gender distribution",
       x = "Gender",
       y = "Number of participants") +
  theme_minimal() +
  theme(legend.position = "None")
```

![](data_cleaning_files/figure-gfm/demo-1.png)<!-- -->

``` r
demo %>%
  ggplot(aes(Age)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Participant age distribution",
       x = "Age",
       y = "Number of participants") +
  theme_minimal()
```

![](data_cleaning_files/figure-gfm/demo-2.png)<!-- -->

## Practice Score

``` r
demo %>%
  filter(block_passed_practice == 2)
```

    ## # A tibble: 2 × 36
    ##   qualtrics_id passed_practice block_passed_practice practice_score   Age Gender
    ##   <chr>                  <dbl>                 <dbl>          <dbl> <dbl> <chr> 
    ## 1 2701997442                 1                     2              9    19 Female
    ## 2 6783315289                 1                     2              8    20 Male  
    ## # … with 30 more variables: Year <chr>, Year_6_TEXT <chr>, Major <chr>,
    ## #   Major_5_TEXT <chr>, Inst <chr>, Start <chr>, Inst_now <chr>,
    ## #   Inst_list <chr>, Ens <chr>, Course <chr>, Course_list <chr>, Read <chr>,
    ## #   `Pitch&Tempo_1` <chr>, `Pitch&Tempo_2` <chr>, Perf <chr>, Time_make <chr>,
    ## #   Time_listen <chr>, Concert <chr>, Genre_18 <chr>, Genre_8 <chr>,
    ## #   Genre_9 <chr>, Genre_17 <chr>, Genre_10 <chr>, Genre_11 <chr>,
    ## #   Genre_12 <chr>, Genre_13 <chr>, Genre_16 <chr>, Genre_14 <chr>, …

``` r
demo %>%
  ggplot(aes(practice_score)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Practice score distribution among those who passed (almost all did)",
       subtitle = "Pass: getting ≥ 8 out of 12 correct",
       x = "Number of correct responses out of 12",
       y = "Number of participants") +
  theme_minimal()
```

![](data_cleaning_files/figure-gfm/practice-1.png)<!-- -->

Only 2 failed the 1st time. The rest all passed in the 1st try.

Most passed with 12/12.

## Categorization

``` r
cat %>%
  ggplot(aes(tuning_step, pct_maj, color = instrument)) +
  geom_smooth(se = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  # need to come up w/ a better palette
  # scale_color_discrete_sequential("Viridis", rev = TRUE) + 
  labs(title = "Proportion of major choices vs. tuning step & instruments",
       x = "Tuning step", y = "Percept of major choices") +
  theme_minimal()
```

![](data_cleaning_files/figure-gfm/cat-1.png)<!-- -->

Xylophone: overall most likely to be judged as major when tuning is
unclear

Oboe: overall most likely to be judged as minor when tuning is unclear

## Explicit Rating

``` r
rtg %>%
  ggplot(aes(instrument, explicit_rtg, fill = instrument)) +
  geom_col() +
  labs(title = "Explicit valence rating for each instrument",
       x = "Instrument", y = "Mean rating") +
  theme_minimal() +
  theme(legend.position = "None")
```

![](data_cleaning_files/figure-gfm/rating-1.png)<!-- -->

## Compare trend between tonality judgment and explicit rating

``` r
cat_rtg_summary %>%
  pivot_longer(cols = c(mean_pct, mean_rtg), names_to = "type", values_to = "value") %>%
  mutate(type = if_else(type == "mean_pct", "Mean percent proportion of major categorization", 
                        "Mean explicit valence rating")) %>%
  ggplot(aes(instrument, value, group = 1, color = type)) +
  geom_line() +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  labs(title = "Compare trend b/w tonality categorization & valence rating",
       x = "Instrument", y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "None")
```

![](data_cleaning_files/figure-gfm/compare-cat-valence-1.png)<!-- -->

Explicit valence rating: violin lowest, violin \< oboe

Categorization: oboe lowest, violin \> oboe
