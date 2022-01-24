* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.

***PROFILE OF MOOD STATES***

RECODE poms01 poms05 poms07 poms09 poms11 poms17 poms24 poms29 poms32 poms35 (1=5) (2=4) (3=3) 
    (4=2) (5=1) INTO poms01_rec poms05_rec poms07_rec poms09_rec poms11_rec poms17_rec poms24_rec 
    poms29_rec poms32_rec poms35_rec.
EXECUTE.

COMPUTE POMS_TMD_mean=MEAN.26(poms01_rec, poms02, poms03, poms04, poms05_rec, poms06, poms07_rec, poms08, poms09_rec, poms10, poms11_rec, poms12, poms13, 
poms14, poms15, poms16, poms17_rec, poms18, poms19, poms20, poms21, poms22, poms23, poms24_rec, poms25, poms26, poms27, poms28, poms29_rec, poms30, poms31, 
poms32_rec, poms33, poms34, poms35_rec).
EXECUTE.

EXCLUDING FRIENDLINESS SCALE (ITEMS 1, 7, 17, 24 and 29)
    
COMPUTE POMS_30_TMD_mean=MEAN.22(poms02, poms03, poms04, poms05_rec, poms06, poms08, poms09_rec, poms10, poms11_rec, poms12, poms13, 
poms14, poms15, poms16, poms18, poms19, poms20, poms21, poms22, poms23, poms25, poms26, poms27, poms28, poms30, poms31, 
poms32_rec, poms33, poms34, poms35_rec).
EXECUTE.


***FUNCTIONAL ASSESSMENT OF CANCER THERAPY - GENERAL POPULATION***

RECODE fact1 fact2 fact3 fact4 fact5 fact6 fact12 fact13 fact14 fact15 (0=4) (1=3) (2=2) (3=1) 
    (4=0) INTO fact1_rec fact2_rec fact3_rec fact4_rec fact5_rec fact6_rec fact12_rec fact13_rec 
    fact14_rec fact15_rec.
EXECUTE.

COMPUTE FACT_physical_mean=MEAN.4(fact1_rec, fact2_rec, fact3_rec, fact4_rec, fact5_rec, fact6_rec).
EXECUTE.

COMPUTE FACT_social_mean=MEAN.3(fact7, fact8, fact9, fact10, fact11).
EXECUTE.

COMPUTE FACT_emotional_mean=MEAN.3(fact12_rec, fact13_rec, fact14_rec, fact15_rec).
EXECUTE.

COMPUTE FACT_functional_mean=MEAN.4(fact16, fact17, fact18, fact19, fact20, fact21).
EXECUTE.


***CESD***

RECODE cesd05 cesd08 (0=3) (1=2) (2=1) (3=0) INTO cesd05_rec cesd08_rec.
EXECUTE.

COMPUTE CESD_mean=MEAN.7(cesd01, cesd02, cesd03, cesd04, cesd05_rec, cesd06, cesd07, cesd08_rec, cesd09, cesd10).
EXECUTE.

COMPUTE CESD_sum=CESD_mean * 10.
EXECUTE.


***COMBORBID CONDITIONS***

COMPUTE comorbid_sum=SUM(comorbid01,comorbid02,comorbid03,comorbid04,comorbid05,comorbid06,
    comorbid07,comorbid08,comorbid09,comorbid10,comorbid11,comorbid12,comorbid13).
EXECUTE.


***FACIT-COST***

DATASET ACTIVATE DataSet1.
RECODE facit_cost01 facit_cost06 facit_cost07 facit_cost11 (1=5) (2=4) (3=3) (4=2) (5=1) INTO 
    facit_cost01_rec facit_cost06_rec facit_cost07_rec facit_cost11_rec.
EXECUTE.

COMPUTE facit_cost_mean=MEAN.8(facit_cost01_rec,facit_cost02,facit_cost03,facit_cost04,facit_cost05,
    facit_cost06_rec,facit_cost07_rec,facit_cost08,facit_cost09,facit_cost10,facit_cost11_rec).
EXECUTE.
