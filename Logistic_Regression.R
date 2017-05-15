# Reading the dataset
data <- read.csv("E:\\TopGear\\pima_indians_diabetes_study\\Pima Indians Diabetes Binary Classification dataset.csv")
colnames(data)<-c("Number_of_times_pregnant","Plasma_glucose_concentration_a_2_hours_in_an_oral_glucose_tolerance_test","Diastolic_blood_pressure_mm_Hg","Triceps_skin_fold_thickness_mm","X2_Hour_serum_insulin_mu_U_ml","Body_mass_index_weight_in_kg_height_in_m_2","Diabetes_pedigree_function","Age_years","Class_variable_0_or_1")

# Summary
dim(data)
str(data)
summary(data)

#Missing values
sum(is.na(data)) # No missing value

#Making dependent variable as factor
data$Class_variable_0_or_1 <- as.factor(data$Class_variable_0_or_1)

#Splitting the data into development and validation sample
data$Temp_Id<-c(1:dim(data)[1])
set.seed(3) 
sample_dev=data[sample(1:nrow(data),floor(0.7*nrow(data))),]
sample_val=sqldf('select * from data where Temp_Id not in (select Temp_Id from sample_dev)')
row.names(sample_dev)<-NULL
sample_dev$Temp_Id<-NULL
sample_val$Temp_Id<-NULL

# IV 
IV=function(data,dep_var,bin=10)
{
  library("sqldf")
  var_set=setdiff(names(data),dep_var)
  IVs=NULL
  for(var in var_set)
  {
    print(var)
    temp=as.data.frame(data[,which(names(data) %in% c(var,dep_var))])
    if(length(unique(temp[,var==names(temp)])) <= 10)####considering variables  with less than 10 unique values as categorical 
    {
      command=paste("select ",var,", count(*) as Total_Count,sum(",dep_var,
                    ") as Total_reponse ,count(*)-sum(",dep_var,") as total_nonresponse from data group by 1",sep="")
      summary=sqldf(command)
      summary$per_resp=summary$Total_reponse/sum(summary$Total_reponse)
      summary$per_nonresp=summary$total_nonresponse/sum(summary$total_nonresponse)
      summary$WOE=(summary$per_resp- summary$per_nonresp)*log(summary$per_resp/summary$per_nonresp,base=exp(1))
      
      summary$WOE[is.infinite(summary$WOE)] = 0
      IVs=c(IVs,sum(summary$WOE))
      
    }
    if(length(unique(temp[,var==names(temp)])) > 10) ##variables with more than 10 unique values ,hence continuous
    {
      temp=temp[order(temp[var == names(temp)]),]
      row.names(temp)=1:NROW(temp)
      temp$bucket=floor(bin*(as.numeric(row.names(temp))-1)/NROW(temp))+1
      command=paste("select bucket, count(*) as Total_Count, sum(",dep_var,
                    ") as Total_reponse,count(*)-sum(",dep_var,") as total_nonresponse from temp group by 1",sep="")
      summary=sqldf(command)
      summary$per_resp=summary$Total_reponse/sum(summary$Total_reponse)
      summary$per_nonresp=summary$total_nonresponse/sum(summary$total_nonresponse)
      summary$WOE=(summary$per_resp - summary$per_nonresp)*log(summary$per_resp/summary$per_nonresp,base=exp(1))
      
      summary$WOE[is.infinite(summary$WOE)] = 0
      IVs=c(IVs,sum(summary$WOE))
    }
    
  }
  
  Information_Value=data.frame(Variable=var_set,IV=IVs)
  
  return(Information_Value)
}


data$Temp_Id<-NULL
IV2=IV(data=data,"Class_variable_0_or_1")

# Logistic regression

model=glm(Class_variable_0_or_1~.,data=sample_dev,family=binomial("logit"))
summary(model)
step_w=step(model,direction="both",steps=1000) 
 
model1<-glm(formula = Class_variable_0_or_1 ~ Number_of_times_pregnant + 
      Plasma_glucose_concentration_a_2_hours_in_an_oral_glucose_tolerance_test + 
      Diastolic_blood_pressure_mm_Hg + Body_mass_index_weight_in_kg_height_in_m_2 + 
      Diabetes_pedigree_function, family = binomial("logit"), data = sample_dev) # got these variables using stepwise regression

summary(model1) # all variables are significants

Akaike=as.data.frame(AIC(model1))

# prediction
pred <-predict(model1, sample_val, type="response") 
pred<-ifelse(pred > 0.5 , 1, 0)

table(pred,sample_val$Class_variable_0_or_1)
