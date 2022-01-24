## Children
  | Observation| Count    |
  | :----------| :------------- |
  | No Children   | 222      |
  |Children, Count| 71|
  |Children, No Count| 67|
  |Missing| 7|

1) Was child count added later?

2) This amount of missingness means that there's unlikely to be something that can be done with the count of children.

## Education
  | Education    | Count    |
  | :------------- | :------------- |
  | Did not complete HS      | 5    |
  | HS or GED| 45|
  |Some College or Tech| 97|
  |4-year Degree | 101|
  |Post-Bacc Degree| 112|
  |Missing| 7|

How representative are these data, if the highest % of patients have a post-bacc degree. Does this actually represent the real world?

## Ethnicity

| Ethnicity| Count    |
| :------------- | :------------- |
| Not Hispanic or Latinx     | 342|
| Hispanic or Latinx| 17|
| Missing | 8|


## Gender

| Gender    | Count    |
| :------------- | :------------- |
| Male       | 136     |
| Female | 223|
|Transgender| 1|
|Missing| 7|

1) Codebook has 0:M\1:F\2:T. Data has no 0s, but 1/2/3/NAs. Assumed that it was shifted?

2) This skew is due to the inclusion of breast cancer. May limit the conclusions we can draw based on gender, and confound dyadic interpretation.

## Income
  |Dollars| Count|
  |:-----|:---|
  |<20k | 10 |
  |20-39.9 | 20 |
  |40-59.9 | 41 |
  |60-79.9 | 40 |
  |80-99.9 | 39 |
  |100-120.9 |46|
  |121+ |150|
  |Missing| 21|

1) Is this correct, or did the scale shift to include an extra 1k?

2) This lines up with the educational attainment, if we assume that degrees -> $

## Marital Status

| Marital Status | Count |
| :------------- | :------------- |
| Not Married, Living with Partner | 26|
|Married |334 |
|Missing| 7|

1) Codebook has 0: Not married, 1: Married. As with gender, assumed shifted.

## Race

| Race | Count |
| :------------- | :------------- |
| AIAN | 5    |
|Asian| 18|
|NH/PI| 2|
|Black/AA| 25|
|White| 296|
|More than one| 13|
|Missing| 8|

1) Same missingness as ethnicity. Should we collapse Asian and NH/PI to match SEER reporting?

## Age

| Age | Patient |
| :------------- | :------------- |
| Item One       | Item Two       |
