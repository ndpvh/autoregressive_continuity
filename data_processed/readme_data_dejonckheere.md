readme_data_dejonckheere

Data has two conditions a normal fixed beep and a followup beep that should come 20 minutes later. 
Some problems in the dataset the missed beeps dont have a timestamp so they show up at the bottom row of each participant. 
We couldnt recover or get the timestamps anywhere.

To prevent invalid lag times from being computed in downstream analyses, NA rows are inserted before any beep whose time gap from the previous beep falls outside an acceptable range.

The funciton add_na() handles this
 Inserts an NA row when the gap between consecutive beeps is too short or too long:

dataset_followups: gap < 1 min (duplicate/artefact) or > 20 min (too far apart)
dataset_beeps: gap < 45 min or > 175 min

Additionally for dataset_followups there is a problem that some fixed beeps are too close to the followups and indicated as followups. 
We cannot remove the "mood" condition as we need this condition as the first beep of a follow up. 
To avoid that the followup→mood lag is computed we must add another NA rule. 

The function insert_na_for_mood() handles this
Applied after add_nas(). Handles a cross-condition problem that add_nas() cannot catch because it is blind to the type of the preceding beep.
Specifically: a mood beep that arrives less than 20 minutes after a followup beep is too close to be treated as an independent mood measurement. This function scans backwards from each mood row, finds the nearest non-NA preceding row, and inserts an NA break if that row was a followup within the threshold.

The 20-minute threshold matches max_diff = 20 used in add_nas() for consistency.

Notes: 

Participant were supposed to get 7 days but most have more... 

Summary 

print(summary_intervals)
  dataset        min_interval mean_interval sd_interval max_interval
1 long_interval         45.1          112.        34.6          175.
2 short_interval         1.03          10.8        5.47          20 

print(summary_pairs)
  dataset        min_pairs mean_pairs sd_pairs max_pairs
1 long_interval          7       20.1     4.38        30
2 short_interval         2       24.3     7.55        38

print(n_participants)
  dataset        n_participants
1 long_interval              96
2 short_interval             96


After exclusion 

 dataset        min_interval mean_interval sd_interval max_interval
1 long_interval         45.1          113.        34.7          174.
2 short_interval         1.05          11.0        5.44          20 

  dataset        min_pairs mean_pairs sd_pairs max_pairs
1 long_interval         20       23.1     1.99        30
2 short_interval        20       27.6     4.64        38

  dataset        n_participants
1 long_interval              51
2 short_interval             51