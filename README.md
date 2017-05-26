# Machine-Learning-Case-Study
Please modify the data file location when reading or sourcing the file.
1. "Data_pretreatment.R". It generates "training_new.csv" and "validate_new.csv" two files split by orderdate </= "2013-04-20". It also generates 100+ features in the new file.
2. "Prediction_data_treatment.R". It applies the same functions in "Data_engineering.R" to "test.txt" as in "training_new.csv" and "validate_new.csv". A new file "data_test.txt" is generated.
3."Sample_code.R". It contains everything you need to train a simple xgboost model.
4. "Prediction_final.R". It contains everything to apply a trained xgbosst model to final test set.
6. "Down_sample_method.R". It divides "training_new.csv" into 10 smaller training sets "training_new_sub_1-10.csv".  

7. "Down_sample_predict.R". Similar to "Sample_code.R", but train set is from "training_new_sub_1-10.csv". Since we need predictions for all rows, the code has to be modified a little bit.

