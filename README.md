# Thesis Tonality Categorization

Eva Wu

Advisors: Dr. Howard Nusbaum & Dr. Stephen Van Hedger

## Hypothesis 

“Happy” instruments would make people more prone to identify the chord as major, 
while “sad” instruments might make people more prone to identify the chord as minor.

## Exploratory research questions 

1) Association between timbre and tonality judgment 

2) Association between timbre and explicit ratings of instrument valence

3) Association between tonality judgment and explicit ratings of instrument valence

4) Association between musical background and tonality judgment and/or explicit ratings of instrument valence

## Design

* IV1 (w/in-subject): instrument (happy [xylophone, trumpet] vs. neutral [piano] vs. sad [oboe, violin]) 

* IV2 (w/in-subject): tuning of middle note (5 levels, ranging from absolute minor to absolute major)

* IV3 (b/w-subject): key (B vs. C) (to find out absolute-pitch-related effects)

* DV: the likelihood that one categorizes a chord as major/minor 

## Procedure

* Pt 1 Sound calibration & headphone test (choose the quietest sound among 3)

* Pt 2 Training (press the buttons to listen to the chords, practice w/ feedback) + 
testing phase (listen to 12 chords and choose b/w major and minor for each, need to correctly answer 8 to pass)

* only analyze the response of those who pass the assessment w/in 2 tries

* Pt 3 Categorization task (jspsych) - listen to 4 blocks of 70 chords and choose b/w major and minor for each chord; 
explicit rating of instrument valence at the end

* Pt 4 Questionnaires (demographics & music experience; Qualtrics)

## File directory

* [cancatenate.Rmd](cancatenate.Rmd): concatenate raw data into 1 csv - by Steve

* [instcat-analysis.Rmd](instcat-analysis.Rmd): GLM analyses - by Steve

* [data_cleaning.md](data_cleaning.md): data cleaning and wrangling & pre-analysis exploratory graphs - by Eva

* [eva_analyses.md](eva_analyses.md): ANOVA of mean categorization and rating across instruments & tuning step - by Eva

* [slope_crossover.md](slope_crossover.md): ANOVA of each individual's regression slope and 50% crossover point - by Eva

* [inst-cat-uc-1.csv](inst-cat-uc-1.csv): raw jspsych data concatenated into one csv file

* [all.csv](all.csv): cleaned data with jspsych and demographics (qualtrics) combined

* [demo_test.csv](demo_test.csv): cleaned data with demographics only