# Predicting Patient Revisits at the University of Virginia Health System Emergency Department
## Brady Fowler, Monica Rajendiran, Timothy Schroeder, Nicholas Bergh, Abigail Flower, and Hyojung Kang
### University of Virginia Data Science Institute

[Read Full Paper Here.](https://edas.info/showManuscript.php?m=1570341133&ext=pdf&random=476267479&type=final)

*Contact fowler.brady(at)gmail.com with questions*

# ABSTRACT
This study focuses on the predictive identification of patients frequently revisiting the University of Virginia Health System Emergency Department. Identifying these patients can help the Emergency Department formulate strategies that improve patient care and decrease excess Emergency Department utilization. The Health System in particular faces a number of unique challenges in its ongoing mission to reduce extraneous patient revisits. In addition to its status as an academic hospital, it serves a broad geographic region as one of five level-I trauma centers in the Commonwealth of Virginia. In this study we utilized 5 years of data from the University of Virginia Health System data warehouse. These data contain information on 91,297 patients and 196,902 unique encounters, including details on patient demographics, diagnoses and hospital departments visited. From these raw data we engineered features, trained gradient boosted decision trees, and experimented with unsupervised clustering techniques to best approximate 30-day Emergency Department revisit risk at the conclusion of each patient encounter. Our best model for revisit risk resulted in a Receiver Operator Characteristic Area Under the Curve of 0.75. Furthermore, we exhibit the real-time performance of our model as a tool to rank which at-risk patients should receive priority for Emergency Department resources. This test demonstrated a significant improvement over the current allocation of Emergency Department social worker resources with a daily Mean Average Precision of 0.83. The methodologies proposed in this paper exhibit an end-to-end framework to transform raw administrative claims and limited clinical data into predictive models that help the Emergency Department better manage resources and target interventions.


## Directory Structure
```
┗━┳━ root/
  ┗━┳━ predicting_revisits_at_UVA_HS/ * all code and output files *
    ┣━ rawdata/  * contains raw RDS from UVAHS *
    ┣━ cleandata/  * contains cleaned datasets  *
 ```

## Code Overview
* This repository is ordered chronologically (early scripts clean and generate features, later scripts learn models and evaluate.)
* First import raw RDS data files each holding patient data (not publically available).
* Custom features are created for each table, including CCS1-3 and Unsupervised Diagnosis Clustering (with the Word2Vec algorithm).
* Gradient Boosted Trees are learned with the XGBOOST Algorithm using an exhaustive/brute force grid search. 
* Baseline Random Forest and Logistic Regression Models are generated.
* Evaluations include PR-AUC, ROC-AUC, Daily MaP
