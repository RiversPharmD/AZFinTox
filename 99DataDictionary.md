| variable | Meaning | Created In|
| :------------- | :------------- | :----|
| facit_cost_sum_p| Patient FACIT_COST score       |04COSTSurvey.r |
| facit_cost_sum_c| Caregiver FACIT_COST score       |04COSTSurvey.r |
| facit_cost_con_abs| Absolute difference in patient and caregiver FACIT_COST |04COSTSurvey.r |
| facit_cost_sum_dir| Directional difference in patient and caregiver FACIT_COST|04COSTSurvey.r |


## Intermediate Variables

| Variable | Meaning | Created In|Note|
| :------------- | :------------- |:--|:--|
|  Location    | 0: 1000, 1: 2000      |03DemoTidyJoin.r| easier to read|
| can_adj|0: no cancer, 1: cancer| 05ComorbTidyJoin.r| account for patients who said they didn't have cancer|
| comorb_adj| sum of comorb replacing can with can_adj| 05ComorbTidyJoin.r| Account for patients who said they didn't have cancer|


## Variable Recodes

### Stage
Created in 05CancerTidyJoin.r
| Old value | New value |
| :------------- | :------------- |
| Item One       | Item Two       |
