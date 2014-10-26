library(caret)
set.seed(12345)
dataset = read.csv("pml-training.csv",na.strings=c(" ","#DIV/0!","NA"));

# remove the columns that have missing data
maskremove = apply(dataset,2,function(x)any(is.na(x)));
maskremove = as.vector(maskremove);

# remove the columns related to time and user identity
maskremove = maskremove | is.element(names(dataset),c("X","user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","new_window","num_window"));

# subset the data
dataset = dataset[,!maskremove];
print(names(dataset))

idxtrain = createDataPartition(dataset$classe,p = 0.7,list = FALSE);
df_train = dataset[idxtrain,]
df_test = dataset[-idxtrain,]

# train QDA model
tc = trainControl(method = "repeatedcv", number=10, repeats=5)
fitmodel = train(classe ~ ., data = df_train, method='qda', 
                 verbose=TRUE,preProcess=c("center","scale"),
                 trControl = tc);
fitmodel

predvals = predict(fitmodel, df_test)
truevals = df_test$classe
xtab = table(predvals,truevals);
cmat = confusionMatrix(xtab);
cmat
cat("out-of-sample error rate is ",1-cmat$overall[[1]]);


# predict data in pml-testing.csv
testset = read.csv("pml-testing.csv",na.strings=c(" ","#DIV/0!","NA"));
predvals = predict(fitmodel,testset);
predvals

# # output
# pml_write_files = function(x){
#   n = length(x)
#   for(i in 1:n){
#     filename = paste0("problem_id_",i,".txt")
#     write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
#   }
# }
# pml_write_files(predvals);















