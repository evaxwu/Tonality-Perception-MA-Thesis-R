Data Cleaning & Exploratory Graphs
================
Eva Wu
2026-05-12

## Data Cleaning

### Import data

This code chunk is to import concatenated data, delete irrelevant
variables and metadata to prepare for further data cleaning. We have
data from both jspsych and Qualtrics, and the two were linked by
participants’ `qualtrics_id` (10-digit random number assigned by
Qualtrics; this was copied automatically to jspsych) and their
`jspsych_id` (14-digit random letters and numbers assigned at the end of
jspsych task; they were asked to manually copy and paste this code to
Qualtrics to show that they finished the jspsych task; all but one
pasted the right code).

``` r
# load
df_q <- read_csv("qualtrics_raw.csv") %>%
  filter(row_number() > 7) %>% # delete pilot data; arranged by RecordedDate by default; recorded before 2/17 = pilot
  select(-c(2:6, 8:16, 50:51)) %>% # delete unnecessary meta data
  mutate(Age = as.numeric(Age),
         Gender = factor(Gender, levels = c(1:3), labels = c("Male", "Female", "Non-binary")),
         Year = factor(Year, levels = c(1:6), labels = c("Freshman", "Sophomore", "Junior", "Senior", "Graduate", "Other")),
         Major = factor(Major, levels = c(1:3), labels = c("Psychology", "Music", "Other")),
         # extract first number as starting age
         Start = as.numeric(sub("^.*?(\\d+).*", "\\1", Start)),
         # manually inspect and correct the wrong one
         Start = if_else(jspsych_id == "tkp19u9btbvhee", 9, Start),
         Pitch = as.integer(`Pitch&Tempo_1`),
         Tempo = as.integer(`Pitch&Tempo_2`),
         # extract first number as number of concerts attended per year
         Concert = as.integer(sub("^.*?(\\d+).*", "\\1", Concert))) %>%
  # calculate the number of years participants played their most-trained instrument  
  rowwise() %>%
  mutate(Inst_yr = max(as.integer(unlist(str_extract_all(Inst_list, "\\d+"))))) %>%
  ungroup() %>%
  select(-`Pitch&Tempo_1`, -`Pitch&Tempo_2`)

# alternative code:
# for (i in 1:length(df_q$Inst_list)) {df_q$Inst_yr[i] <- max(as.integer(unlist(str_extract_all(df_q$Inst_list[i], "\\d+"))))}

# we got lots of -Inf values due to people not listing their years of training
# for those who indicated they are still playing (Inst_now == 1) and filled out a starting age and their current age, I manually calculated it
# for those who did play something but were not playing anymore, I input NA as their score 
# for those who nvr played, I put 0
df_q %>%
  filter(Inst_yr == -Inf)
```

    ## # A tibble: 16 × 37
    ##    StartD…¹ Recor…² jspsy…³   Age Gender Year  Year_…⁴ Major Major…⁵ Inst  Start
    ##    <chr>    <chr>   <chr>   <dbl> <fct>  <fct> <chr>   <fct> <chr>   <chr> <dbl>
    ##  1 2022-02… 2022-0… w3uh3g…    19 Female Soph… <NA>    Other Biolog… 1         8
    ##  2 2022-02… 2022-0… w3uh3g…    19 Female Soph… <NA>    Other Biolog… 1         8
    ##  3 2022-02… 2022-0… evx5gb…    19 Female Soph… <NA>    Other Busine… 1         4
    ##  4 2022-02… 2022-0… cafntv…    NA Male   Soph… <NA>    Other Econom… 1         5
    ##  5 2022-02… 2022-0… 1zp8rs…    NA Female Soph… <NA>    Other Econom… 1         8
    ##  6 2022-03… 2022-0… uqdy2d…    18 Female Soph… <NA>    Other econom… 1         4
    ##  7 2022-03… 2022-0… 46ak9p…    19 Male   Soph… <NA>    Other Econom… 1         8
    ##  8 2022-03… 2022-0… a48zc7…    19 Male   Fres… <NA>    Other Busine… 1        12
    ##  9 2022-03… 2022-0… h21fw4…    19 Male   Soph… <NA>    Other Econom… 1        16
    ## 10 2022-03… 2022-0… h21fw4…    NA Male   Soph… <NA>    Other Econom… 1        16
    ## 11 2022-03… 2022-0… 7txj1h…    19 Male   Soph… <NA>    Other Econom… 1        10
    ## 12 2022-03… 2022-0… 7txj1h…    19 Male   Soph… <NA>    Other Econom… 1        10
    ## 13 2022-03… 2022-0… 1zs6z0…    21 Male   Soph… <NA>    Other <NA>    1         6
    ## 14 2022-04… 2022-0… 2qldva…    20 Female Soph… <NA>    Other Chemis… 1         8
    ## 15 2022-04… 2022-0… 3er8w4…    20 Male   Juni… <NA>    Other Engine… 1         7
    ## 16 2022-04… 2022-0… zbda5d…    20 Male   Soph… <NA>    Other Molecu… 1         7
    ## # … with 26 more variables: Inst_now <chr>, Inst_list <chr>, Ens <chr>,
    ## #   Course <chr>, Course_list <chr>, Read <chr>, Perf <chr>, Time_make <chr>,
    ## #   Time_listen <chr>, Concert <int>, Genre_18 <chr>, Genre_8 <chr>,
    ## #   Genre_9 <chr>, Genre_17 <chr>, Genre_10 <chr>, Genre_11 <chr>,
    ## #   Genre_12 <chr>, Genre_13 <chr>, Genre_16 <chr>, Genre_14 <chr>,
    ## #   Genre_15 <chr>, Genre_15_TEXT <chr>, participant <chr>, Pitch <int>,
    ## #   Tempo <int>, Inst_yr <dbl>, and abbreviated variable names ¹​StartDate, …

``` r
df_q <- df_q %>%
  # manually correct the wrongly coded ones 
  mutate(Inst_yr = case_when(jspsych_id == "evx5gb3ga2u3qk" ~ 15,
                             jspsych_id == "h21fw4bmslr0vb" ~ 3,
                             jspsych_id == "14vd1s548b9ux8" ~ 5,
                             jspsych_id == "8glm4kxovqneap" ~ 4,
                             jspsych_id == "46ak9p0c90r96o" ~ 1,
                             jspsych_id == "lws6fybx47tsfx" ~ 6,
                             jspsych_id == "cny0e1gcjouwbl" ~ 2,
                             jspsych_id == "r51mv84rhfl9sj" ~ 0.5,
                             jspsych_id == "5qjd88k61078vu" ~ 9,
                             TRUE ~ Inst_yr),
         Inst_yr = ifelse(is.na(Inst_yr), 0, Inst_yr),
         Inst_yr = ifelse(Inst_yr == -Inf, NA, Inst_yr)) %>%
  # flag it if anyone has had training on the instruments used in experiment 1
  mutate(Inst_list = tolower(Inst_list),
         Inst_best = str_extract_all(Inst_list, regex("oboe|piano|violin|trumpet|xylophone")),
         Inst_best = as.character(Inst_best)) %>% # bc can't save list vars in csv
  relocate(Inst_yr, .after = Inst_list) %>%
  relocate(Inst_best, .after = Inst_yr)
```

``` r
# load exp1 data
df_j1 <- read_csv("inst-cat-uc-1.csv") %>% 
  filter(designation != "NA" & 
         designation != "practice-intro" & 
         designation != "practice-resp" & 
         !qualtrics_id %in% c(NA, 9643222579)) %>% # delete 2 pilot data
  select(participant, qualtrics_id, chord, designation, response, # delete meta data
         correct, passed_practice, block_passed_practice, practice_score, 
         instrument, valence, tuning_step, selected_major, explicit_rtg) %>%
  mutate(qualtrics_id = if_else(participant == "ch4dg75c7th9g1", "1706631024", qualtrics_id),
         instrument = factor(instrument, levels = c("xylophone", "piano", "violin", "trumpet", "oboe")),
         correct = as.numeric(correct),
         practice_score = as.numeric(practice_score),
         tuning_step = as.numeric(tuning_step),
         selected_major = as.numeric(selected_major),
         explicit_rtg = factor(explicit_rtg))

df_j2 <- read_csv("inst-cat-uc-2.csv") %>% 
  filter(designation != "NA" & 
         designation != "practice-intro" & 
         designation != "practice-resp" & 
         participant != "participant") %>% 
  select(participant, qualtrics_id, chord, designation, response, # delete meta data
         correct, passed_practice, block_passed_practice, practice_score, 
         instrument, envelope, harmonics, tuning_step, selected_major, explicit_rtg) %>%
  mutate(instrument = factor(instrument, levels = c("T1", "T2", "T3", "T4", "T5", "T6")),
         correct = as.numeric(correct),
         practice_score = as.numeric(practice_score),
         tuning_step = as.numeric(tuning_step),
         selected_major = as.numeric(selected_major),
         explicit_rtg = as.numeric(explicit_rtg))

# examine j IDs
df_j1_id <- df_j1 %>% 
  select(participant, qualtrics_id) %>% 
  unique() %>% # 49 unique jspsych_id in df_j1
  # for some reason 2 participants' q id on j was "participant", so I recoded one according to their q id on q
  # one of them has 2 q ids (duplicated rows) on q so just leave it as "participant"
  mutate(qualtrics_id = if_else(participant == "ch4dg75c7th9g1", "1706631024", qualtrics_id))

df_j2_id <- df_j2 %>% 
  filter(participant != "participant") %>%
  select(participant, qualtrics_id) %>% 
  unique()
# 58 unique jspsych_id in df_j2; no one's q id was recorded
```

### Deduplicate

Check for duplicated Qualtrics entries and delete one of the duplicates.
This might be due to participants closing the Qualtrics tab while
completing the jspsych task, and then being redirected to a new
Qualtrics page after jspsych task was over.

Decision rule: 1) if one row has a qualtrics_id recorded on jspsych,
then keep that row (only possible in exp1); 2) if one row is
blank/partially filled while the other is fully filled, then keep the
more filled row; 3) if no qualtrics_id was recorded on jspsych and both
rows are filled to the same extent, keep the one with earlier StartDate
(this is arbitrarily decided).

``` r
# identify duplicated participants on q by manually examining the data
df_q %>%
  filter(jspsych_id %in% df_j1_id$participant) %>%
  arrange(jspsych_id) # seems like these duplicated rows were both filled and had similar values
```

    ## # A tibble: 50 × 38
    ##    StartD…¹ Recor…² jspsy…³   Age Gender Year  Year_…⁴ Major Major…⁵ Inst  Start
    ##    <chr>    <chr>   <chr>   <dbl> <fct>  <fct> <chr>   <fct> <chr>   <chr> <dbl>
    ##  1 2022-02… 2022-0… 14vd1s…    18 Female Fres… <NA>    Other Public… 1         8
    ##  2 2022-02… 2022-0… 1zp8rs…    NA Female Soph… <NA>    Other Econom… 1         8
    ##  3 2022-02… 2022-0… 2dfy59…    19 Female Soph… <NA>    Other Econom… 1         9
    ##  4 2022-02… 2022-0… 2mm1j8…    NA Female Soph… <NA>    Other Neuros… 0        NA
    ##  5 2022-03… 2022-0… 46ak9p…    19 Male   Soph… <NA>    Other Econom… 1         8
    ##  6 2022-02… 2022-0… 5lrj1r…    19 Female Soph… <NA>    Other econ, … 1         8
    ##  7 2022-02… 2022-0… 5nwv37…    20 Female Soph… <NA>    Other Biology 1        11
    ##  8 2022-02… 2022-0… 5w1v1w…    18 Male   Fres… <NA>    Other Neuros… 1        11
    ##  9 2022-03… 2022-0… 7wr5yq…    19 Female Soph… <NA>    Other Biology 1        12
    ## 10 2022-03… 2022-0… 8glm4k…    21 Female Juni… <NA>    Other Econom… 1         8
    ## # … with 40 more rows, 27 more variables: Inst_now <chr>, Inst_list <chr>,
    ## #   Inst_yr <dbl>, Inst_best <chr>, Ens <chr>, Course <chr>, Course_list <chr>,
    ## #   Read <chr>, Perf <chr>, Time_make <chr>, Time_listen <chr>, Concert <int>,
    ## #   Genre_18 <chr>, Genre_8 <chr>, Genre_9 <chr>, Genre_17 <chr>,
    ## #   Genre_10 <chr>, Genre_11 <chr>, Genre_12 <chr>, Genre_13 <chr>,
    ## #   Genre_16 <chr>, Genre_14 <chr>, Genre_15 <chr>, Genre_15_TEXT <chr>,
    ## #   participant <chr>, Pitch <int>, Tempo <int>, and abbreviated variable …

``` r
# check the 2 duplicated participants' q id on j to determine which row to keep
df_j1_id %>%
  filter(participant == "w3uh3gtrykkjgc" | participant == "ept8xz3drgq38w")
```

    ## # A tibble: 2 × 2
    ##   participant    qualtrics_id
    ##   <chr>          <chr>       
    ## 1 w3uh3gtrykkjgc 9065164044  
    ## 2 ept8xz3drgq38w participant

``` r
# now do the same for exp2
df_q %>%
  filter(jspsych_id %in% df_j2_id$participant) %>%
  arrange(jspsych_id)
```

    ## # A tibble: 64 × 38
    ##    StartD…¹ Recor…² jspsy…³   Age Gender Year  Year_…⁴ Major Major…⁵ Inst  Start
    ##    <chr>    <chr>   <chr>   <dbl> <fct>  <fct> <chr>   <fct> <chr>   <chr> <dbl>
    ##  1 2022-03… 2022-0… 1k76xh…    18 Female Fres… <NA>    Other Law, L… 1         7
    ##  2 2022-04… 2022-0… 1qnukj…    21 Male   Juni… <NA>    Psyc… <NA>    1        10
    ##  3 2022-03… 2022-0… 1ud48q…    20 Male   Soph… <NA>    Other Mathem… 1        17
    ##  4 2022-03… 2022-0… 1zs6z0…    21 Male   Soph… <NA>    Other <NA>    1         6
    ##  5 2022-03… 2022-0… 25s7fa…    NA Male   Fres… <NA>    Other Not de… 1         8
    ##  6 2022-04… 2022-0… 2qldva…    20 Female Soph… <NA>    Other Chemis… 1         8
    ##  7 2022-03… 2022-0… 2ydzvy…    21 Male   Juni… <NA>    Other Econom… 1         7
    ##  8 2022-04… 2022-0… 3er8w4…    20 Male   Juni… <NA>    Other Engine… 1         7
    ##  9 2022-03… 2022-0… 3n34nq…    20 Male   Soph… <NA>    Other Biolog… 0        NA
    ## 10 2022-03… 2022-0… 3ts9x7…    18 Female Soph… <NA>    <NA>  Biology 1         5
    ## # … with 54 more rows, 27 more variables: Inst_now <chr>, Inst_list <chr>,
    ## #   Inst_yr <dbl>, Inst_best <chr>, Ens <chr>, Course <chr>, Course_list <chr>,
    ## #   Read <chr>, Perf <chr>, Time_make <chr>, Time_listen <chr>, Concert <int>,
    ## #   Genre_18 <chr>, Genre_8 <chr>, Genre_9 <chr>, Genre_17 <chr>,
    ## #   Genre_10 <chr>, Genre_11 <chr>, Genre_12 <chr>, Genre_13 <chr>,
    ## #   Genre_16 <chr>, Genre_14 <chr>, Genre_15 <chr>, Genre_15_TEXT <chr>,
    ## #   participant <chr>, Pitch <int>, Tempo <int>, and abbreviated variable …

``` r
df_j2_id %>%
  filter(participant == "a48zc72k379mm5" | participant == "hecbjunkhj9dj8" |
         participant == "4srlrzfe7zrkce" | participant == "h21fw4bmslr0vb" |
           participant == "7txj1h7m1zulc0" | participant == "tkp19u9btbvhee")
```

    ## # A tibble: 6 × 2
    ##   participant    qualtrics_id
    ##   <chr>          <chr>       
    ## 1 a48zc72k379mm5 <NA>        
    ## 2 hecbjunkhj9dj8 <NA>        
    ## 3 4srlrzfe7zrkce participant 
    ## 4 h21fw4bmslr0vb participant 
    ## 5 7txj1h7m1zulc0 participant 
    ## 6 tkp19u9btbvhee participant

``` r
# since q id was not recorded on j for exp2 for some reason, we can only arbitrarily delete one of the two entries (according to the decision rule)
df_q_unique <- df_q %>%
  filter(!(jspsych_id == "a48zc72k379mm5" & StartDate == "2022-03-13 11:44:21" |
             jspsych_id == "hecbjunkhj9dj8" & StartDate == "2022-03-14 11:09:00" |
             jspsych_id == "4srlrzfe7zrkce" & StartDate == "2022-03-28 16:31:30" |
             jspsych_id == "h21fw4bmslr0vb" & StartDate == "2022-03-29 15:21:11" |
             jspsych_id == "7txj1h7m1zulc0" & StartDate == "2022-03-30 15:49:11" |
             jspsych_id == "tkp19u9btbvhee" & StartDate == "2022-04-07 19:30:53" |
             jspsych_id == "ept8xz3drgq38w" & StartDate == "2022-04-15 14:55:23" |
             jspsych_id == "w3uh3gtrykkjgc" & participant == "9143215998")) 
```

### Drop NA

This code chunk exists only because there were problems with the linking
of Qualtrics and jspsych data. This code chunck is to filter out
participants who did not do anything in the survey/categorization task.
Some participants had two rows, one with Qualtrics response but blank
jspsych response, the other with full jspsych response but blank
Qualtrics response. This step is to merge those participants’ Qualtrics
and jspsych data into the same row.

``` r
# see a list of q participants
df_q_id <- df_q_unique %>%
  select(jspsych_id, participant) 

# check if there are any j subs without q for exp2
df_j2_id %>%
  filter(!participant %in% df_q_id$jspsych_id)
```

    ## # A tibble: 0 × 2
    ## # … with 2 variables: participant <chr>, qualtrics_id <chr>

``` r
# nope, luckily everyone on j had a q response in exp2

# yes q no j
na_j <- df_q_id %>%
  filter(is.na(jspsych_id) | jspsych_id == "22-0159")
# 22-0159 is someone who didn't pass the test and therefore wasn't able to 
# proceed to jspsych main task due to error (they were supposed to be able to)

# q-j mismatch (yes j; blank q survey for the row associated w the q id recorded on j)
na_q <- df_j1_id %>%
  filter(qualtrics_id %in% na_j$participant)
# there's a matching error bw q & j - in q, q id is there, but survey blank; the corresponding j id in q has another q id for the filled survey row
# there were 4 subs like this in exp1

# check for blank q rows with q id recorded in j 
na_j %>%
#  filter(participant %in% na_q$qualtrics_id) the following line of code does the same thing as this
  semi_join(na_q, by = c("participant" = "qualtrics_id"))
```

    ## # A tibble: 4 × 2
    ##   jspsych_id participant
    ##   <chr>      <chr>      
    ## 1 <NA>       1044403354 
    ## 2 <NA>       6783315289 
    ## 3 <NA>       4358939887 
    ## 4 <NA>       1475084897

``` r
# check for q rows w j id that has response in j but blank q rows if matched by q id
df_q_id %>%
  filter(jspsych_id %in% na_q$participant) # turns out these responses were recorded under the same j id but different q id
```

    ## # A tibble: 3 × 2
    ##   jspsych_id     participant
    ##   <chr>          <chr>      
    ## 1 wczbffevcxqk45 8761269153 
    ## 2 en7lefm4g8mp17 5711051437 
    ## 3 ghbk58xwj3w7v2 4255152956

``` r
# change their q id to the one recorded on j
df_q_unique <- df_q_unique %>%
  mutate(participant = case_when(jspsych_id == "wczbffevcxqk45" ~ "1044403354",
                                 jspsych_id == "en7lefm4g8mp17" ~ "4358939887",
                                 jspsych_id == "ghbk58xwj3w7v2" ~ "1475084897",
                                 TRUE ~ participant)) %>%
  filter(!is.na(jspsych_id)) # delete blank rows
# this person w jspsych_id == "5281kj47nhoboj", qualtrics_id == "6783315289" does not have a corresponding qualtrics survey filled out
# IDK Y! IDK WHO THAT IS! - how should we report this in the paper?

# for the rest of the blank q rows, I can't figure out which q id matches which j id, bc no q id was recorded on j in exp2
# so i just left them as they are
# doesn't matter bc we can join dfs using j id so we can ignore q

# manually code number of instruments they played
df_q_unique$Inst_num <- c(2, 1, 2, 3, 2, 1, 0, 2, 3, 2, 3, 0, 0, 3, 1, 2, 1, 0, 1, 1, 2, 1, 1, 1, 3, 1, 2, 2, 1, 3, 1, 1, 0, 2, 1, 1, 2, 0, 2, 0, 1, 1, 2, 2, 1, 2, 1, 7, 1, 2, 0, 0, 2, 1, 3, 1, 2, 2, 4, 0, 3, 0, 3, 3, 2, 2, 1, 0, 0, 1, 3, 4, 2, 2, 0, 3, 2, 4, 2, 1, 3, 2, 1, 2, 0, 3, 3, 1, 2, 2, 0, 2, 4, 1, 2, 2, 5, 1, 1, 1, 2, 2, 2, 0, 1, 1, 4)

# move column
df_q_unique <- df_q_unique %>%
  relocate(Inst_num, .after = Inst_yr)
```

### Practice and headphone test scores

The following code chunk extracts headphone test and practice test
(training for major/minor categorization) scores and attaches them to
the qualtrics demographic survey.

``` r
# df for practice score
practice1 <- df_j1 %>%
  filter(designation == "PRACTICE-PASSED-SUMMARY") %>%
  select(participant, passed_practice, block_passed_practice, practice_score)

# df for headphone test
headphone1 <- df_j1 %>%
  filter(designation == "headphone-test") %>%
  select(participant, correct) %>%
  group_by(participant) %>%
  summarize(headphone = sum(correct)) 

# combine headphone test & practice test scores
practice_headphone1 <- left_join(headphone1, practice1)

# now do the same for exp2
practice2 <- df_j2 %>%
  filter(designation == "PRACTICE-PASSED-SUMMARY") %>%
  select(participant, passed_practice, block_passed_practice, practice_score)

headphone2 <- df_j2 %>%
  filter(designation == "headphone-test") %>%
  select(participant, correct) %>%
  group_by(participant) %>%
  summarize(headphone = sum(correct)) 

practice_headphone2 <- left_join(headphone2, practice2)

# combine headphone & practice test scores w demographics
df_q_plus_scores <- left_join(df_q_unique, rbind(practice_headphone1, practice_headphone2), by = c("jspsych_id" = "participant")) %>%
  mutate(passed_practice = factor(passed_practice, levels = c(0, 1), labels = c("No", "Yes")),
         block_passed_practice = factor(block_passed_practice),
         accuracy = practice_score/12)

# save a copy of clean q data
write_csv(df_q_plus_scores, "qualtrics_cleaned.csv")
```

## Cleaned data!

A snapshot of the data (aka codebook):

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

StartDate
</td>

<td style="text-align:left;">

Time of participation
</td>

</tr>

<tr>

<td style="text-align:left;">

RecordedDate
</td>

<td style="text-align:left;">

Time when the survey response was recorded
</td>

</tr>

<tr>

<td style="text-align:left;">

jspsych_id
</td>

<td style="text-align:left;">

Random 14-digit combination of letters and numbers, used to verify
completion of jspsych task
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

Year in university
</td>

</tr>

<tr>

<td style="text-align:left;">

Year_6_TEXT
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

Major_3_TEXT
</td>

<td style="text-align:left;">

If not a psych or music major
</td>

</tr>

<tr>

<td style="text-align:left;">

Inst
</td>

<td style="text-align:left;">

If they have ever played an instrument or not
</td>

</tr>

<tr>

<td style="text-align:left;">

Start
</td>

<td style="text-align:left;">

At what age they started playing an instrument
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

List of instruments played
</td>

</tr>

<tr>

<td style="text-align:left;">

Inst_yr
</td>

<td style="text-align:left;">

Number of years they’ve played their most trained instrument
</td>

</tr>

<tr>

<td style="text-align:left;">

Inst_num
</td>

<td style="text-align:left;">

Number of instruments played (including voice)
</td>

</tr>

<tr>

<td style="text-align:left;">

Inst_best
</td>

<td style="text-align:left;">

List instruments used in exp1 that they’ve played (if any)
</td>

</tr>

<tr>

<td style="text-align:left;">

Ens
</td>

<td style="text-align:left;">

If they have ever participated in an ensemble or not
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

List of music courses taken
</td>

</tr>

<tr>

<td style="text-align:left;">

Read
</td>

<td style="text-align:left;">

Can read music or not
</td>

</tr>

<tr>

<td style="text-align:left;">

Perf
</td>

<td style="text-align:left;">

Has perfect pitch or not (1 = yes, 0 = no, -1 = don’t know
</td>

</tr>

<tr>

<td style="text-align:left;">

Time_make
</td>

<td style="text-align:left;">

Time spent making music per week
</td>

</tr>

<tr>

<td style="text-align:left;">

Time_listen
</td>

<td style="text-align:left;">

Time spent listening to music per day
</td>

</tr>

<tr>

<td style="text-align:left;">

Concert
</td>

<td style="text-align:left;">

Number of concerts attended per year
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

Other genres not mentioned
</td>

</tr>

<tr>

<td style="text-align:left;">

participant
</td>

<td style="text-align:left;">

Random 10-digit Qualtircs ID
</td>

</tr>

<tr>

<td style="text-align:left;">

Pitch
</td>

<td style="text-align:left;">

Self-rated ability to perceive and remember pitch
</td>

</tr>

<tr>

<td style="text-align:left;">

Tempo
</td>

<td style="text-align:left;">

Self-rated ability to perceive and remember tempo
</td>

</tr>

<tr>

<td style="text-align:left;">

headphone
</td>

<td style="text-align:left;">

headphone test score (out of 6)
</td>

</tr>

<tr>

<td style="text-align:left;">

passed_practice
</td>

<td style="text-align:left;">

Passed tonality categorization practice test or not
</td>

</tr>

<tr>

<td style="text-align:left;">

block_passed_practice
</td>

<td style="text-align:left;">

How many attempts it took to pass the practice test (2 at most)
</td>

</tr>

<tr>

<td style="text-align:left;">

practice_score
</td>

<td style="text-align:left;">

Practice test score (out of 12)
</td>

</tr>

<tr>

<td style="text-align:left;">

accuracy
</td>

<td style="text-align:left;">

Number of correct answers in headphone test
</td>

</tr>

</tbody>

</table>
