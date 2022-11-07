Title: Estimating coordinate from a household survey
Name: Bikalpa Baniya



The code below has five parts
1) Loading required libraries and data
2) Prepping the data
3) Creating required functions 
4) Performing the estimation
5) Archive



##################################################################################
#################### Loading required libraries and data #########################
##################################################################################


```{r ,include=FALSE}
library(foreign)
library(tidyverse)
library(randomForest)
library(apcluster)
```


Loading the data
```{r ,include=FALSE}
wm <- as.data.frame(read.spss("Input/MICS6_Datasets/wm.sav", to.data.frame=TRUE))
mn <- as.data.frame(read.spss("Input/MICS6_Datasets/mn.sav", to.data.frame=TRUE))
train <- as.data.frame(read.csv("Processing/Version 2/qgis_final.csv"))
lang <- as.data.frame(read.csv("Processing/Version 2/Language.csv"))
```




The releveant variables for simulation the simulation are


training data

- xcoord	
- ycoord
- World Pop population in 100m x 100m tiles
- Language_main_primary_language	
- Language_main_primary_language_share	
- Language_main_secondary_language
- Language_main_secondary_language_share
- Language_pop_female
- place (size in degrees)
    -"Urban": city, farm, neighbourhood, quarter, state, suburb, town
    -"Rural": hamlet (1km, 0.01), isolated_dwelling (0.001), locality (0.001), yes (0.001), village (0.05)
    else (0.0009)
- admin3Pcod (chiefdome)

Number of hamlet (7526), isolated_dwelling (104), locality (9), yes (4), village (1855)
Weighted Mean average = 0.01791


Relevant variables in wm and mn 
wm
- HH1 (Cluster number)
- HH2 (HH number)
- WM14 (Native language of respondent)
- WB4 (Age, only 15-49)
- HH6 (Area, Urban/Rural)
- HH7 (Region)
- HH7A (District)
- WB5 (Ever attended school)
- WB6A (Highest level of school attended)
- WB6B (Highest grade attended at that level - What is the highest level and grade or year of school you have attended?)
- WB7 (Grade completion - Did you complete that (grade/year)?)
- WB9 (Attended school during current school year)
- WB10A (Level of education attended current school year)
- WB10B (Grade of education attended current school year)
- WB11 (Attended school previous school year)
- WB12A (Level of education attended previous school year)
- WB14 (Can read part of the sentence)
- MT2 (Frequency of listening to the radio)
- MT3 (Frequency of watching TV)
- MT11 (Own a mobile phone)
- MT12 (Mobile phone usage in the last 3 months)
- CM1 (Ever given birth)
- CM2 (Any sons or daughters living with you)
- MA1 (Currently married or living with a man)
- MA3 (Husband/partner has more wives or partners)
- LS1 (Estimation of overall happiness)
- wscore (Combined wealth score)
- wscoreu (Urban wealth score)
- wscorer (Rural wealth score)
- wmweight (Women's sample weight)
- welevel (Education)
- disability (Functional difficulties (age 18-49 years))
- DV1A (If she goes out with out telling husband: wife beating justified)
- DV1B (If she neglects the children: wife beating justified)
- DV1C (If she argues with husband: wife beating justified)
- DV1D (If she refuses sex with husband: wife beating justified)
- DV1E (If she burns the food: wife beating justified)

mn 
- HH1 (Cluster number)
- HH2 (Household number)
- MWM14 (Native language of the Respondent)
- MWB4 (Age of man 15-49)
- HH6 (Area)
- HH7 (Region)
- HH7A (District)
- MWB5 (Ever attended school)
- MWB6A (Highest level of school attended)
- MWB6B (Highest grade attended at that level)
- MWB7 (Grade completion)
- MWB9 (Attended school during current school year)
- MWB10A (Level of education attended current school year)
- MWB10B (Grade of education attended current school year)
- MWB11 (Attended school previous school year)
- MWB12A (Level of education attended previous school year)
- MWB14 (Can read part of the sentence)
- MMT2 (Frequency of listening to the radio)
- MMT3 (Frequency of watching TV)
- MMT11 (Own a mobile phone)
- MMT12 (Mobile phone usage in the last 3 months)
- MCM1 (Ever given birth)
- MCM2 (Any sons or daughters living with you)
- MMA1 (Currently married or living with someone as if married)
- MMA3 (Other wifes or live-in partners)
- MLS1 (Estimation of overall happiness)
- wscore (Combined wealth score)
- wscoreu (Urban wealth score)
- wscorer (Rural wealth score)
- mnweight (Men's sample weight)
- mwelevel (Education)
- mdisability (Functional difficulties (age 18-49 years))
- MDV1A (If she goes out with out telling husband: wife beating justified)
- MDV1B (If she neglects the children: wife beating justified)
- MDV1C (If she argues with husband: wife beating justified)
- MDV1D (If she refuses sex with husband: wife beating justified)
- MDV1E (If she burns the food: wife beating justified)

 


################################################################
#################### Prepping the data #########################
################################################################



Adding Language info into train from Language file 
```{r ,include=FALSE}

train <- train%>%
  left_join(lang, by = c("admin3Pcod"= "admin3_pcode"))%>%
  group_by(admin3Name) %>% 
  mutate(dist_id=cur_group_id())

train<-na.omit(train)

train$District <- toupper(train$admin2Name)


#write.csv(train, file="Processing/train.csv")
```


Creating Proportions for the population group
```{r }
train$wpop_f_total <- train$wpop_f_15sum	+ train$wpop_f_20sum +	train$wpop_f_25sum +	train$wpop_f_30sum +	train$wpop_f_35sum +	train$wpop_f_40sum +	train$wpop_f_45sum

train$wpop_m_total <- train$wpop_m_15sum	+ train$wpop_m_20sum +	train$wpop_m_25sum +	train$wpop_m_30sum +	train$wpop_m_35sum +	train$wpop_m_40sum +	train$wpop_m_45sum

train$wpop_total <- train$wpop_f_total + train$wpop_m_total 

col_start <-which( colnames(train)=="wpop_f_15sum" )
col_end <- which( colnames(train)=="wpop_m_45sum" )
col_total <- which( colnames(train)=="wpop_total" )

for (col_num in col_start:col_end){
  train[,col_num] <- train[,col_num] / train[,col_total] 
}

train$admin3_name<- gsub('-', ' ', train$admin3_name)

```


Cleaning the wm and mn datasets and appending them 
```{r}
wm <- wm%>%
  select(HH1, HH2, WM14, WB4,  HH6,  HH7,  HH7A, WB5  ,WB6A , WB6B ,WB7, WB9, WB10A, WB10B, WB11, WB12A, WB14, MT2, MT3, MT11, MT12, CM1, CM2, MA1, MA3, LS1, wscore, wscoreu, wscorer, wmweight, welevel, disability, DV1A, DV1B, DV1C, DV1D ,DV1E) %>%
  rename(weight = wmweight )
wm$women <- 1

mn <- mn%>%
  select(HH1,HH2,  MWM14 , MWB4 , HH6 , HH7 , HH7A , MWB5 , MWB6A , MWB6B , MWB7 , MWB9 , MWB10A, MWB10B , MWB11,MWB12A, MWB14 , MMT2 , MMT3 , MMT11 , MMT12 , MCM1 , MCM2 , MMA1, MMA3 , MLS1 , wscore , wscoreu , wscorer , mnweight , mwelevel , mdisability ,MDV1A , MDV1B , MDV1C , MDV1D, MDV1E )%>%
   rename(weight = mnweight )

names(mn)[3:4] <- substring(names(mn[3:4]),2)
names(mn)[8:26] <- substring(names(mn[8:26]),2)
names(mn)[31:37] <- substring(names(mn[31:37]),2)
mn$women <- 0

hh_survey <- rbind(wm, mn)
#colnames(hh_survey)
```


Preparing the hh_survey data for Random forest 
```{r}

#Creating dummies for age group
dummy_name <- list ("wpop_f_15sum",	"wpop_f_20sum",	"wpop_f_25sum",	"wpop_f_30sum",	"wpop_f_35sum",	"wpop_f_40sum",	"wpop_f_45sum", "wpop_m_15sum",	"wpop_m_20sum",	"wpop_m_25sum",	"wpop_m_30sum",	"wpop_m_35sum",	"wpop_m_40sum", "wpop_m_45sum")
age_group <- 15
women_dummy = 1

for (name_no in 1:length(dummy_name)){
  hh_survey <- hh_survey %>%
    mutate(dname = ifelse((women == women_dummy  & WB4>=age_group & WB4<age_group+5),1,0))%>%
    rename(!!dummy_name[[name_no]] :=  dname)
  age_group <- age_group+5
  if (age_group == 50){
    age_group=15
    women_dummy = 0}
}

hh_survey<- hh_survey %>%
  mutate(age_group_created = wpop_f_15sum+	wpop_f_20sum+	wpop_f_25sum	+wpop_f_30sum+	wpop_f_35sum	+wpop_f_40sum	+wpop_f_45sum	+wpop_m_15sum	+wpop_m_20sum	+wpop_m_25sum	+wpop_m_30sum+	wpop_m_35sum+	wpop_m_40sum	+wpop_m_45sum)

#252 some rows do not have age data so the age group sum variable was not created. We need to remove these entries. These same rows  and only these rows also do not have language data.
hh_survey<-hh_survey[!(is.na(hh_survey$age_group_created)),]

# hh_survey_test <- hh_survey%>%
#   filter(HH1==329, WB4<40,WB4>=35 ,women==1)


```


```{r}
table(hh_survey$WM14)

col_list <- c("main_primary_language_share","Krio_primary","Mende_primary"   ,           "English_primary"     ,        "Themne_primary" ,             "Maninkakan_primary"    ,     "Loko_primary"      ,          "Sherbro_primary",             "Limba_primary"        ,      "Kissi_primary"       ,        "Kono_primary"     ,           "Susu_primary"           , "Yalunka_primary"      ,       "Kuranko_primary"   ,          "Vai_primary"             ,    "Other_primary"         ,      "wpop_f_15sum"       ,         "wpop_f_20sum"             ,  "wpop_f_25sum"           ,     "wpop_f_30sum"        ,        "wpop_f_35sum"              , "wpop_f_40sum"            ,    "wpop_f_45sum"         ,       "wpop_m_15sum"               , "wpop_m_20sum"             ,   "wpop_m_25sum"          ,      "wpop_m_30sum"               , "wpop_m_35sum"              ,  "wpop_m_40sum"           ,     "wpop_m_45sum"   )
```








#################################################################
#################### Required Function  #########################
#################################################################




Computing OOB error for the Random forest 
```{r ,include=FALSE}

#Computing the Out of Bag error Rate for the random forest

 computeOOBErrEst <- function (x)
{
  cm <- x$confusion
  cm <- cm[, -ncol(cm)]
  1 - sum(diag(cm)) / sum(cm)
 }
```

A function that takes hh survey data by chiefdome and assigns Affinity Propogation Clusters
```{r}

#A function that takes hh survey data by chiefdome and assigns Affinity Propogation Clusters 

AP_clusters <- function(main_data,col_list){
  main_subset <- main_data%>%
    select(col_list)
  
  sim_matrix <- negDistMat(main_subset, r=2) #r=2 is used to obtain negative squared distances as in Frey's and Dueck's demos
  
  ap <- apcluster(sim_matrix)
  main_data$apcluster_no <- -1
  for (cluster_Id in 1:length(ap@clusters)){
    
    for (cluster_element in 1:length(ap@clusters[[cluster_Id]])){
      main_data$apcluster_no[   ap@clusters[[cluster_Id]][[cluster_element ]]   ] <- cluster_Id
      
    }
  }
  ap <- NULL
  sim_matrix <- NULL
  return(main_data)
}


```

A function that gets number of clusters from the datasets with AP cluster info
```{r}
#A function that gets number of clusters from the datasets with AP cluster info

get_APcluster_number <- function(main_data,col_list){
  df <- AP_clusters(main_data,col_list)
  n_clusters <- n_distinct(df$apcluster_no)
  return(n_clusters)
}
  
```


Filtering values into the new dataframe and aggregate #####Need to change accordingly to match training data######
```{r}
# Filtering values into the new dataframe and aggregate

hh_filter_sum <- function(main_data, admin_name){
        hh_filter <- main_data %>%
            filter(HH7A == admin_name)%>%
            add_count(HH1, WM14)%>%   # select max or first occurrence 
            group_by(HH1)%>%
            mutate(Majority = WM14[n == max(n)][1]) %>%   # keep only first TRUE
            select(-n)%>%      # do not keep temp var
            summarise(wpop_total = n(), 
                      main_primary_language_share  = sum( WM14== Majority)/n(),
                      Krio_primary = sum(WM14 == "KRIO")/n(),
                      Mende_primary = sum(WM14 == "MENDE")/n(),
                     English_primary = sum(WM14 == "ENGLISH")/n(),
                     Themne_primary = sum(WM14 == "TEMNE")/n(),
                     Maninkakan_primary = sum(WM14 == "MANDINGO")/n(),
                     Loko_primary = sum(WM14 == "LOKO")/n(),
                     Sherbro_primary = sum(WM14 == "SHERBRO")/n(),
                     Limba_primary = sum(WM14 == "LIMBA")/n(),
                     Kissi_primary = sum(WM14 == "KISSI")/n(),
                     Kono_primary = sum(WM14 == "KONO")/n(),
                     Susu_primary = sum(WM14 == "SUSU")/n(),
                     #Mende_primary = sum(WM14 == "FULLAH")/n(),
                    # Mende_primary = sum(WM14 == "KRIM")/n(),
                     Yalunka_primary = sum(WM14 == "YALUNKA")/n(),
                     Kuranko_primary = sum(WM14 == "KORANKO")/n(),
                     Vai_primary = sum(WM14 == "VAI")/n(),
                     Other_primary = sum(WM14 == "OTHER LANGUAGE")/n(),
            
                      wpop_f_15sum= sum(wpop_f_15sum)/n(),
                      wpop_f_20sum=sum(wpop_f_20sum)/n(),
                      wpop_f_25sum=sum(wpop_f_25sum)/n(),
                      wpop_f_30sum=sum(wpop_f_30sum)/n(),
                      wpop_f_35sum=sum(wpop_f_35sum)/n(),
                      wpop_f_40sum=sum(wpop_f_40sum)/n(),
                      wpop_f_45sum=sum(wpop_f_45sum)/n(),
                      wpop_m_15sum=sum(wpop_m_15sum)/n(),
                      wpop_m_20sum=sum(wpop_m_20sum)/n(),
                      wpop_m_25sum=sum(wpop_m_25sum)/n(),
                      wpop_m_30sum=sum(wpop_m_30sum)/n(),
                      wpop_m_35sum=sum(wpop_m_35sum)/n(),
                      wpop_m_40sum=sum(wpop_m_40sum)/n(),
                      wpop_m_45sum=sum(wpop_m_45sum)/n()
                      )
        return(hh_filter)
  }
   

```


```{r}

get_formula <- function(LHS){
  out <- paste(LHS,"~","wpop_f_15sum+	wpop_f_20sum+	wpop_f_25sum	+wpop_f_30sum+	wpop_f_35sum	+wpop_f_40sum	+wpop_f_45sum	+wpop_m_15sum	+wpop_m_20sum	+wpop_m_25sum	+wpop_m_30sum+	wpop_m_35sum+	wpop_m_40sum	+wpop_m_45sum + main_primary_language_share +	Krio_primary	+Mende_primary+	Themne_primary	+Maninkakan_primary	+Loko_primary+	Sherbro_primary	+Limba_primary+	Kissi_primary+	Kono_primary	+Susu_primary	+	Yalunka_primary+	Kuranko_primary+	Vai_primary+	English_primary	+ Other_primary")
  out2 <- as.formula(out)
  return(out2)
}
```


```{r}
rf_predict <- function(random_forest, data_to_predict, predict_col_name ){
       p1=predict(random_forest, data_to_predict,type="response", predict.all=FALSE, proximity=FALSE, nodes=FALSE)
      data_to_predict$rf_predicted<- "None"
      for (row in 1:nrow(data_to_predict)){
          data_to_predict$rf_predicted[[row]]<- as.character(p1[[row]])
      }
      names(data_to_predict)[names(data_to_predict) == "rf_predicted"] <- predict_col_name
      predicted_data <- data_to_predict #%>%rename( predict_col_name = rf_predicted )
    
  return(predicted_data)
}
```

```{r}

#Randonmly assign coordinates from training data to survey data

random_cord_assign_wo_adminName <- function(main_data, train_data,id, xcord,ycord){
      predicted <- data.frame()%>%
                    mutate(hh_id = -999999999,    #######change value if id is not interger#######
                     xcord_estimate = -999999999, 
                     ycord_estimate = -999999999)
     for (row_no in  1:nrow(main_data)){
                random_no <- sample.int(nrow( train_data), 1)
                predicted[nrow(predicted) + 1, ] <- c(main_data[[id]][row_no],
                                                      train_data[[xcord]][random_no], 
                                                      train_data[[ycord]][random_no]
                                                      )
        }
  return(predicted)
}
random_cord_assign_w_adminName <- function(main_data, train_data,id, xcord,ycord, level_name){
    predicted <- data.frame()%>%
                    mutate(hh_id = -999999999,    #######change value if id is not interger#######
                     levelx_name_estimate = "Placeholder",
                     xcord_estimate = -999999999, 
                     ycord_estimate = -999999999)
        for (row_no in  1:nrow(main_data)){
                random_no <- sample.int(nrow( train_data), 1)
                predicted[nrow(predicted) + 1, ] <- c(main_data[[id]][row_no],
                                                      as.character(train_data[[level_name]][random_no]),
                                                      train_data[[xcord]][random_no], 
                                                      train_data[[ycord]][random_no]
                                                      )
        }
  return(predicted)
}

#The two functions above combined
random_cord_assign <- function(main_data, train_data,id, xcord,ycord, level_name=NULL ){
       if (nrow(main_data)==0 | nrow(train_data)==0){
           stop("One of the data frames is empty")
       }
    if(is.null(level_name)){
      predicted_values <- random_cord_assign_wo_adminName(main_data, train_data,id, xcord,ycord)
    }else{
      predicted_values <-random_cord_assign_w_adminName(main_data, train_data,id, xcord,ycord,level_name)
    }
  return(predicted_values)
}
```






#########################################################################
#################### Performing the estimations #########################
#########################################################################






```{r}

#Create empty shell to be filled later
filtered_count <- 0
filtredout_count <- 0

predicted_geoloc <- data.frame()%>%
  mutate(hh_id = -999999999,    #######change value if id is not interger#######
         levelx_name_estimate = "Placeholder",
         xcord_estimate = -999999999, 
         ycord_estimate = -999999999)

for (d in levels(hh_survey$HH7A)  ) 
  {
  
  #Filtering values into the new dataframe and aggregate
  hh_admin2 <- hh_filter_sum(hh_survey,d)

  train_admin2 <- train %>%filter(District == d)
  train_admin2<-na.omit(train_admin2)
  
  rf <-randomForest(get_formula("factor(admin3Name)") ,data=train_admin2 , ntree=4000, mtry=30)
  
     #if hh_admin2 is not empty. We do not want this to return an error if it is empty
      if (dim(hh_admin2)[1] != 0) {
       
               hh_admin2 <- rf_predict(rf,hh_admin2,"admin3name_rf")
               
              #Now assgining coordinates below the chiefdom level
               for (admin3 in levels(factor(hh_admin2$admin3name_rf))){
                    hh_admin3 <- hh_admin2%>%ungroup()%>%filter(admin3name_rf == admin3)
                    train_admin3 <- train_admin2%>%ungroup()%>%filter(admin3Name == admin3)

                       if (nrow(train_admin3) > 2 & nrow(train_admin3)>nrow(hh_admin3)){
                         
                             apclusters_admin3 <- AP_clusters(train_admin3,col_list)
                             rf_admin3 <- randomForest(get_formula("factor(apcluster_no)") ,data=apclusters_admin3 , ntree=3000, mtry=30)
                            
                             #set barrier for high oob error
                             if(computeOOBErrEst(rf_admin3)<40){
                             
                                  hh_admin3 <- rf_predict(rf_admin3,hh_admin3,"apcluster_admin3_rf")
                                  
                                  cluster_list <- unique(apclusters_admin3$apcluster_no)
                                  for (cluster in 1:length(cluster_list)){
                                      hh_admin3_cluster <- hh_admin3 %>% filter(apcluster_admin3_rf == cluster)
                                      apclusters_admin3_cluster <- apclusters_admin3%>%filter(apcluster_no == cluster)
                                      
                                      if(nrow( hh_admin3_cluster ) > 0 ){
                                          cord_assginment <-random_cord_assign( hh_admin3_cluster ,
                                                                                apclusters_admin3_cluster,
                                                                               "HH1","xcoord","ycoord", "admin3Name")
                                       }
                                      predicted_geoloc <- rbind(predicted_geoloc,cord_assginment)  
                                  }
                            }else{
                               cord_assginment2 <- random_cord_assign( hh_admin3 ,
                                                                     train_admin3,
                                                                     "HH1","xcoord","ycoord", "admin3Name")
                               predicted_geoloc <- rbind(predicted_geoloc,cord_assginment2)  
                             } 
                                
                            filtered_count <- filtered_count +1
                        }else{
                          cord_assginment2 <- random_cord_assign( hh_admin3 ,
                                                                      apclusters_admin3,
                                                                     "HH1","xcoord","ycoord", "admin3Name")
                          predicted_geoloc <- rbind(predicted_geoloc,cord_assginment2)  
                            filtredout_count <- filtredout_count+1
                        }
               }
     }
}

final_estimations <- distinct(predicted_geoloc)
 
```


```{r}

#Join the estimates to the main survey
hh_survey$HH1 <- as.character(hh_survey$HH1 )

hh_survey_final <- hh_survey%>%
  select(HH1, HH2, WM14, WB4,  HH6,  HH7,  HH7A, WB5  ,WB6A , WB6B ,WB7, WB9, WB10A, WB10B, WB11, WB12A, WB14, MT2, MT3, MT11, MT12, CM1, CM2, MA1, MA3, LS1, wscore, wscoreu, wscorer, weight, welevel, disability, DV1A, DV1B, DV1C, DV1D ,DV1E,women)%>%
  left_join(final_estimations, by=c("HH1"="hh_id"))


write.csv(hh_survey_final, file="Output/survey_w_cord.csv")

```


















































######################################################
#################### Archive #########################
######################################################

Perform k-means clustering with the number of cluster from the APclusterring 
```{r}
# Perform k-means clustering with the number of cluster from the APclusterring 

k_means <- function(main_data, train_data , col_list){
   
    train_data_subset  <- train_data  %>%
      ungroup()%>%
      select(col_list)
  
  
    no_clusters <- get_APcluster_number(main_data,col_list)
  
    kmeans_clusters <- kmeans(train_data_subset ,no_clusters,iter.max = 20 )

    train_data$k_means_cluster <- -1
    for (n in 1:length(kmeans_clusters$cluster)){
      train_data$k_means_cluster[   n  ] <- kmeans_clusters$cluster[n]
      #print(kmeans_clusters$cluster[n])
    }

    return(train_data )
}

```


```{r}
#,, , "Rural"
#"KAILAHUN","BO","KENEMA","KONO","BOMBALI","KAMBIA","KOINADUGU","PORT LOKO","BONTHE","MOYAMBA","PUJEHUN","TONKOLILI", "WESTERN AREA RURAL","WESTERN AREA URBAN"

admin_2 <- list("PORT LOKO")

#Create empty shell to be filled later
hh_survey$admin3name <- "None"
hh_survey$xcoord <- -999999999
hh_survey$ycoord <- -999999999

filtered_count <- 0
filtredout_count <- 0


############# Create a flag and change the following function accordingly  ###################

predicted_geoloc <- data.frame()%>%
  mutate(hh_id = -999999999,    #######change value if id is not interger#######
         levelx_name_estimate = "Placeholder",
         xcord_estimate = -999999999, 
         ycord_estimate = -999999999)

#predicted_geoloc <- rbind(predicted_geoloc,a=c(-9,"PH",-9,-9))

for (d in levels(hh_survey$HH7A)  ) #levels(hh_survey$HH7A)
  {
  
  #Filtering values into the new dataframe and aggregate
  hh_admin2 <- hh_filter_sum(hh_survey,d)

  train_admin2 <- train %>%filter(District == d)
  train_admin2<-na.omit(train_admin2)

  #Langagues not in survey : 	Pular_primary	, Bom-Kim_primary	,	French_primary ,	Arabic_primary	
  rf <-randomForest(get_formula("factor(admin3Name)") ,data=train_admin2 , ntree=1000, mtry=30)
  print(paste(d ," , No. of obs = " ,nrow(train_admin2) ,", OOB error = ", round(computeOOBErrEst(rf), digits = 4)*100,"%","############################"))
      
  
  ############# Put the unsupervised part below into a function that it only runs when unsupervied flag is on  ###################
     
     #if hh_admin2 is not empty. We do not want this to return an error if it is empty
      if (dim(hh_admin2)[1] != 0) {
               
         
               hh_admin2 <- rf_predict(rf,hh_admin2,"admin3name_rf")
              
               print(levels(factor(hh_admin2$admin3name_rf)))
               
               

              #Now assgining coordinates below the chiefdom level
               for (admin3 in levels(factor(hh_admin2$admin3name_rf))){
                    hh_admin3 <- hh_admin2%>%ungroup()%>%filter(admin3name_rf == admin3)
                    train_admin3 <- train_admin2%>%ungroup()%>%filter(admin3Name == admin3)
                      
                
                  
                   

                      
                       if (nrow(train_admin3) > 2 & nrow(train_admin3)>nrow(hh_admin3)){


                             print(paste("Chiefdom = ", admin3 ," nrow =", nrow(hh_admin3), ", n cluster = ",get_APcluster_number(train_admin3,col_list),":: training nrow =", nrow(train_admin3)))

           
                             apclusters_admin3 <- AP_clusters(train_admin3,col_list)
                             rf_admin3 <- randomForest(get_formula("factor(apcluster_no)") ,data=apclusters_admin3 , ntree=1000, mtry=30)
                            
                             #set barrier for high oob error
                             if(computeOOBErrEst(rf_admin3)<40){
                             
                                  hh_admin3 <- rf_predict(rf_admin3,hh_admin3,"apcluster_admin3_rf")
                                  
                                  cluster_list <- unique(apclusters_admin3$apcluster_no)
                                  for (cluster in 1:length(cluster_list)){
                                      hh_admin3_cluster <- hh_admin3 %>% filter(apcluster_admin3_rf == cluster)
                                      apclusters_admin3_cluster <- apclusters_admin3%>%filter(apcluster_no == cluster)
                                      
                                      if(nrow( hh_admin3_cluster ) > 0 ){
                                          cord_assginment <-random_cord_assign( hh_admin3_cluster ,
                                                                                apclusters_admin3_cluster,
                                                                               "HH1","xcoord","ycoord", "admin3Name")
                                          
                                        
                                        }
                                      
                                       predicted_geoloc <- rbind(predicted_geoloc,cord_assginment)  
                                      
                                        
                                  }
                                  
                              
                             }else{
                               cord_assginment2 <- random_cord_assign( hh_admin3 ,
                                                                     train_admin3,
                                                                     "HH1","xcoord","ycoord", "admin3Name")
                               predicted_geoloc <- rbind(predicted_geoloc,cord_assginment2)  
                             } 
                                
                            filtered_count <- filtered_count +1
                        }else{
            
                          
                          ############# Add one more  random_cord_assign and rbind function here ###################
                          
                           print(paste("Outfiltred Chiefdom = ", admin3, " , nrow =", nrow(train_admin3) ))
                          cord_assginment2 <- random_cord_assign( hh_admin3 ,
                                                                      apclusters_admin3,
                                                                     "HH1","xcoord","ycoord", "admin3Name")
                          predicted_geoloc <- rbind(predicted_geoloc,cord_assginment2)  
                            filtredout_count <- filtredout_count+1
                        }
                      


               }

                ############# Or maybe add it here, figure this out  ###################
                
      }

}

final_estimations <- distinct(predicted_geoloc)
 
print(paste("filtered_count =" ,filtered_count ," filtredout_count = ",filtredout_count))

# KOINADUGU , Sengbe ap does not converge
```


```{r}

#Join the estimates to the main survey
hh_survey$HH1 <- as.character(hh_survey$HH1 )

hh_survey_final <- hh_survey%>%
  select(HH1, HH2, WM14, WB4,  HH6,  HH7,  HH7A, WB5  ,WB6A , WB6B ,WB7, WB9, WB10A, WB10B, WB11, WB12A, WB14, MT2, MT3, MT11, MT12, CM1, CM2, MA1, MA3, LS1, wscore, wscoreu, wscorer, weight, welevel, disability, DV1A, DV1B, DV1C, DV1D ,DV1E,women)%>%
  left_join(final_estimations, by=c("HH1"="hh_id"))


write.csv(hh_survey_final, file="Output/survey_w_cord.csv")

```






```{r}
hh_admin3_test <- hh_admin2%>%filter(admin3name_rf == "East III")
train_admin3_test <- train_admin2%>%filter(admin3Name == "East III")

```



```{r}

#A function that takes hh survey data by chiefdome and assigns Affinity Propogation Clusters 


AP_clusters <- function(main_data,col_list){
  main_subset <- main_data%>%
    select(col_list)
  
  sim_matrix <- negDistMat(main_subset, r=2) #r=2 is used to obtain negative squared distances as in Frey's and Dueck's demos
  
  ap <- apcluster(sim_matrix)
  main_data$apcluster_exemplar <- -1
  for (cluster_Id in 1:length(ap@clusters)){
    
    for (cluster_element in 1:length(ap@clusters[[cluster_Id]])){
      main_data$apcluster_exemplar[   ap@clusters[[cluster_Id]][[cluster_element ]]   ] <- main_data$HH1[ap@exemplars[cluster_Id]]
      
    }
  }
  print(ap@clusters)
  print(ap@exemplars)
  return(main_data)
}

apclusters_admin3_test <- AP_clusters(hh_admin3_test,col_list)
apclusters_admin3_test  <- apclusters_admin3_test %>%select(HH1,apcluster_exemplar,admin3name_rf )
# print(apclusters_admin3_test@clusters)
```

```{r}
importance(rf)

```



```{r,include= FALSE}
hh_filter_SB <- hh_admin2%>%
  filter(admin3name_rf == "Upper Bambara")

train_filter_SB <- train_admin2 %>%
  filter(admin3Name == "Upper Bambara")

```






```{r}

col_list <- c("main_primary_language_share","Krio_primary","Mende_primary"   ,           "English_primary"     ,        "Themne_primary" ,             "Maninkakan_primary"    ,     "Loko_primary"      ,          "Sherbro_primary",             "Limba_primary"        ,      "Kissi_primary"       ,        "Kono_primary"     ,           "Susu_primary"           , "Yalunka_primary"      ,       "Kuranko_primary"   ,          "Vai_primary"             ,    "Other_primary"         ,      "wpop_f_15sum"       ,         "wpop_f_20sum"             ,  "wpop_f_25sum"           ,     "wpop_f_30sum"        ,        "wpop_f_35sum"              , "wpop_f_40sum"            ,    "wpop_f_45sum"         ,       "wpop_m_15sum"               , "wpop_m_20sum"             ,   "wpop_m_25sum"          ,      "wpop_m_30sum"               , "wpop_m_35sum"              ,  "wpop_m_40sum"           ,     "wpop_m_45sum"   )


test <- AP_clusters(hh_filter_SB, col_list)
#length of clusters can be retrived using length(test@clusters ) but we can just just uniqueN on cluster number


print(get_APcluster_number(test, col_list))
  
```


K_means clusters for the training data 


```{r}

# test2 <- train_filter_SB %>%
#   select(hh_col_list)
# test2$admin3Name <- NULL
# a <- kmeans(test2, 5)
# a$cluster[5]
```

```{r}
# Perform k-means clustering with the number of cluster from the APclusterring 

k_means <- function(main_data, train_data , col_list){
   
    train_data_subset  <- train_data  %>%
      ungroup()%>%
      select(col_list)
  
  
    no_clusters <- get_APcluster_number(main_data,col_list)
  
    kmeans_clusters <- kmeans(train_data_subset ,no_clusters )

    train_data$k_means_cluster <- -1
    for (n in 1:length(kmeans_clusters$cluster)){
      train_data$k_means_cluster[   n  ] <- kmeans_clusters$cluster[n]
      print(kmeans_clusters$cluster[n])
    }

    return(train_data )
}

a<-k_means(hh_filter_SB, train_filter_SB, col_list)




```




# ```{r}
# #,, , "Rural"
# #"KAILAHUN","BO","KENEMA","KONO","BOMBALI","KAMBIA","KOINADUGU","PORT LOKO","BONTHE","MOYAMBA","PUJEHUN","TONKOLILI", "WESTERN AREA RURAL","WESTERN AREA URBAN"
#
# admin_2 <- list("KAILAHUN","BO","KENEMA","KONO","BOMBALI","KAMBIA","KOINADUGU")
#
# #Computing OOB error for the Random forest
#  computeOOBErrEst <- function (x)
# {
#   cm <- x$confusion
#   cm <- cm[, -ncol(cm)]
#   1 - sum(diag(cm)) / sum(cm)
#  }
#
#     for (d in admin_2)
#       {
#
#
#
#           cord_filter <- cord %>%
#           filter(District == d)
#           cord_filter<-na.omit(cord_filter)
#
#           rf <-randomForest(factor(admin3Name)~ 	Area + wpop_total+wpop_f_15sum+	wpop_f_20sum+	wpop_f_25sum	+wpop_f_30sum+	wpop_f_35sum	+wpop_f_40sum	+wpop_f_45sum	+wpop_m_15sum	+wpop_m_20sum	+wpop_m_25sum	+wpop_m_30sum+	wpop_m_35sum+	wpop_m_40sum	+wpop_m_45sum + main_primary_language_share +	Krio_primary	+Mende_primary+	Themne_primary	+Maninkakan_primary	+Loko_primary+	Sherbro_primary	+Limba_primary+	Kissi_primary+	Kono_primary	+Susu_primary	+Pular_primary+	Bom.Kim_primary+	Yalunka_primary+	Kuranko_primary+	Vai_primary+	English_primary	+French_primary+	Arabic_primary + Other_primary,data=cord_filter , ntree=1000, mtry=25)
#           print(paste(d ," " ,", No. of obs = " ,nrow(cord_filter) ,", OOB error = ", round(computeOOBErrEst(rf), digits = 4)*100,"%"))
#
#           }
#
#
# ```
#
# ```{r}
# importance(rf)
# ```






 
 
 
# ```{r, warnings = FALSE, include=FALSE}
# #,, , "Rural"
# #"KAILAHUN","BO","KENEMA","KONO","BOMBALI","KAMBIA","KOINADUGU","PORT LOKO","BONTHE","MOYAMBA","PUJEHUN","TONKOLILI", "WESTERN AREA RURAL","WESTERN AREA URBAN"
# 
# admin_2 <- list("PORT LOKO","BONTHE","MOYAMBA") 
# 
# setting <- list("Urban","Rural")
# 
# #Create empty shell to be filled later
# hh_survey$admin3name <- "None"
# hh_survey$xcoord <- "None"
# hh_survey$ycoord <- "None"
# 
# 
# 
# 
#     for (d in admin_2) 
#       {
#       
#       #Filtering values into the new dataframe and aggregate
#       hh_filter <- hh_survey %>%
#         filter(HH7A == d)%>%
#         add_count(HH1, WM14)%>%   # select max or first occurrence 
#         group_by(HH1)%>%
#         mutate(Majority = WM14[n == max(n)][1]) %>%   # keep only first TRUE
#         select(-n)%>%      # do not keep temp var
#         summarise(pop = n(),Language_main_primary_language_share = sum( WM14== Majority)/n(),
#                   Krio_primary = sum(WM14 == "KRIO")/n(),
# 
# 
#                   )
# 
# 
#           }
# 
# 
# ```
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 





```{r}
#,, , "Rural"
#"KAILAHUN","BO","KENEMA","KONO","BOMBALI","KAMBIA","KOINADUGU","PORT LOKO","BONTHE","MOYAMBA","PUJEHUN","TONKOLILI", "WESTERN AREA RURAL","WESTERN AREA URBAN"

admin_2 <- list("WESTERN AREA URBAN")

#Create empty shell to be filled later
hh_survey$admin3name <- "None"
hh_survey$xcoord <- -999999999
hh_survey$ycoord <- -999999999

filtered_count <- 0
filtredout_count <- 0

for (d in admin_2  ) #levels(hh_survey$HH7A)
  {
  
  #Filtering values into the new dataframe and aggregate
  hh_admin2 <- hh_filter_sum(hh_survey,d)

  train_admin2 <- train %>%filter(District == d)
  train_admin2<-na.omit(train_admin2)

  #Langagues not in survey : 	Pular_primary	, Bom-Kim_primary	,	French_primary ,	Arabic_primary	
  rf <-randomForest(get_formula("factor(admin3Name)") ,data=train_admin2 , ntree=1000, mtry=30)
  print(paste(d ," , No. of obs = " ,nrow(train_admin2) ,", OOB error = ", round(computeOOBErrEst(rf), digits = 4)*100,"%","############################"))
      
  
     
     #if hh_admin2 is not empty. We do not want this to return an error if it is empty
      if (dim(hh_admin2)[1] != 0) {
               
         
               hh_admin2 <- rf_predict(rf,hh_admin2,"admin3name_rf")
              
               print(levels(factor(hh_admin2$admin3name_rf)))

              #Now assgining coordinates below the chiefdom level
               for (admin3 in levels(factor(hh_admin2$admin3name_rf))){
                      hh_admin3 <- hh_admin2%>%filter(admin3name_rf == admin3)
                      train_admin3 <- train_admin2%>%filter(admin3Name == admin3)

                    #& admin3 != "Gbanti Kamarank" & admin3 != "Wara Wara Yagal" & admin3 != "TMS" & admin3 !="Kunike "

                      #Chiefdom with less than 5 survey clusters will not not be assigned ML clusters
                       if (nrow(hh_admin3) > 3 & nrow(train_admin3)>nrow(hh_admin3)){

                             print(paste("Chiefdom = ", admin3 ," nrow =", nrow(hh_admin3), ", n cluster = ",get_APcluster_number(hh_admin3,col_list),":: training nrow =", nrow(train_admin3)))


                             apclusters_admin3 <- AP_clusters(hh_admin3,col_list)
                             kmeans_admin3 <- k_means(hh_admin3,train_admin3,col_list)

                            #Need at least two classes to do the classification
                             if(get_APcluster_number(hh_admin3,col_list)>1){
                                  rf_admin3 <- randomForest( get_formula("factor(k_means_cluster)"),data=kmeans_admin3 , ntree=1000, mtry=30) 
                                 # print(paste(", OOB error = ", round(computeOOBErrEst(rf_admin3), digits = 4)*100,"%"))
                                  
                                 #set barrier for high oob error
                                 if(computeOOBErrEst(rf_admin3)<40){
                                 
                                   hh_admin3 <- rf_predict(rf_admin3,apclusters_admin3,"kmeans_admin3_rf")
                                   
                                  exemplar_list <- unique(hh_admin3$apcluster_exemplar)
                                  for (exemplar in exemplar_list){
                                        hh_admin3_examplar_cluster <- hh_admin3%>%filter(apcluster_exemplar == exemplar)
                                  }
                                 } 
                                
                             }
                             
                             
                            
                            
                             #assign cluster zero 
                             #randomly assign coordinates for this chiefdome
                             #join with bigger file
                             #append 




                            filtered_count <- filtered_count +1
                        }else{


                           print(paste("Outfiltred Chiefdom = ", admin3, " , nrow =", nrow(hh_admin3) ))
                            filtredout_count <- filtredout_count+1
                        }
                      


               }

                
                
      }

}
#print(paste("filtered_count =" ,filtered_count ," filtredout_count = ",filtredout_count))

# KOINADUGU , Sengbe ap does not converge
```
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
```{r, warnings = FALSE, include=FALSE}
#,, , "Rural"
#"KAILAHUN","BO","KENEMA","KONO","BOMBALI","KAMBIA","KOINADUGU","PORT LOKO","BONTHE","MOYAMBA","PUJEHUN","TONKOLILI", "WESTERN AREA RURAL","WESTERN AREA URBAN"

admin_2 <- list("PORT LOKO","BONTHE","MOYAMBA") 

setting <- list("Urban","Rural")

#Create empty shell to be filled later
hh_survey$admin3name <- "None"
hh_survey$xcoord <- "None"
hh_survey$ycoord <- "None"

#hh_survey_area <- hh_survey%>%select(HH1, HH6)%>%rename(Area = HH6) #to ensure hh_filter has area info

for (a in setting)
  {
    for (d in levels(hh_survey$HH7A)) 
      {
      
      #Filtering values into the new dataframe and aggregate
      hh_filter <- hh_survey %>%
        filter(HH7A == d, HH6 ==a)%>%
        add_count(HH1, WM14)%>%   # select max or first occurrence 
        group_by(HH1)%>%
        mutate(Majority = WM14[n == max(n)][1]) %>%   # keep only first TRUE
        select(-n)%>%      # do not keep temp var
        summarise(pop = n(),Language_main_primary_language_share = sum( WM14== Majority)/n(),
                  wpop_f_15sum= sum(wpop_f_15sum)/n(),
                  wpop_f_20sum=sum(wpop_f_20sum)/n(),
                  wpop_f_25sum=sum(wpop_f_25sum)/n(),
                  wpop_f_30sum=sum(wpop_f_30sum)/n(),
                  wpop_f_35sum=sum(wpop_f_35sum)/n(),
                  wpop_f_40sum=sum(wpop_f_40sum)/n(),
                  wpop_f_45sum=sum(wpop_f_45sum)/n(),
                  wpop_m_15sum=sum(wpop_m_15sum)/n(),
                  wpop_m_20sum=sum(wpop_m_20sum)/n(),
                  wpop_m_25sum=sum(wpop_m_25sum)/n(),
                  wpop_m_30sum=sum(wpop_m_30sum)/n(),
                  wpop_m_35sum=sum(wpop_m_35sum)/n(),
                  wpop_m_40sum=sum(wpop_m_40sum)/n(),
                  wpop_m_45sum=sum(wpop_m_45sum)/n()
                  )

          cord_filter <- cord %>%
          filter(Area == a, District == d)
          cord_filter<-na.omit(cord_filter)
     
          rf <-randomForest(factor(admin3Name)~ 	Language_main_primary_language_share+	 wpop_f_15sum+	wpop_f_20sum+	wpop_f_25sum	+wpop_f_30sum+	wpop_f_35sum	+wpop_f_40sum	+wpop_f_45sum	+wpop_m_15sum	+wpop_m_20sum	+wpop_m_25sum	+wpop_m_30sum+	wpop_m_35sum+	wpop_m_40sum	+wpop_m_45sum,data=cord_filter , ntree=1000, mtry=15) 
          print(paste(d ," " ,a ,", No. of obs = " ,nrow(cord_filter) ,", OOB error = ", round(computeOOBErrEst(rf), digits = 4)*100,"%"))
          
         #if hh_filter is not empty. WESTERN AREA URBAN does not have rural entries so we do not want this to return an error
          if (dim(hh_filter)[1] != 0) {
                     
                     p1=predict(rf, hh_filter,type="response", predict.all=FALSE, proximity=FALSE, nodes=FALSE)
                    #assign predicted values to hh_filter and  
                    hh_filter$admin3name_filter <- "None"
                    for (row in 1:nrow(hh_filter)){
                        hh_filter$admin3name_filter[[row]]<- as.character(p1[[row]])
                    }
                    
      
                    
                    #assign values to hh_survey
                    
                    
                    #1) Randomly assign coordinates within the chiefdome
                    
                     hh_filter$xcoord_filter[row] <- 0
                     hh_filter$ycoord_filter[row] <- 0
                     for (row in 1:nrow(hh_filter)){
                        cord_filter_subset <- cord_filter%>%
                        filter(admin3Name == hh_filter$admin3name_filter[row])
                       
                      random_no <- sample.int(nrow(cord_filter_subset), 1)
                       
                     hh_filter$xcoord_filter[row] <- cord_filter_subset$xcoord[random_no]
                     hh_filter$ycoord_filter[row] <- cord_filter_subset$ycoord[random_no]
                        }
          
                     
                     #2) assign cheifdome name and coordinates from hh_filter to hh_survey
                    
                    hh_filter <- hh_filter%>%
                        select(HH1, admin3name_filter, xcoord_filter,ycoord_filter)
                    hh_survey <- hh_survey%>%
                        left_join(hh_filter, by = "HH1")

                    hh_survey$admin3name_filter[is.na(hh_survey$admin3name_filter)] <- "None"
                    hh_survey$xcoord_filter[is.na(hh_survey$xcoord_filter)] <- "None" 
                    hh_survey$ycoord_filter[is.na(hh_survey$ycoord_filter)] <- "None" 
                    #because the code below gives error if in NA form
                    
                    #transfer info from hh_filter join to the main hh_survey df
                    hh_survey <- hh_survey %>%
                      mutate(admin3name=replace(admin3name, admin3name == "None" , admin3name_filter[admin3name == "None"]))%>%
                      mutate(xcoord=replace(xcoord, xcoord == "None" , xcoord_filter[xcoord == "None"]))%>%
                      mutate(ycoord=replace(ycoord, ycoord == "None" , ycoord_filter[ycoord == "None"]))
        
                    #remove the join info 
                    hh_survey$admin3name_filter <- NULL
                    hh_survey$xcoord_filter <- NULL
                    hh_survey$ycoord_filter <- NULL
          }
    }
  }
      
```





```{r}

# hh_survey_test <- hh_survey%>%
#   filter( ycoord =="None")%>%
#   select(HH7A, HH1, HH6, admin3name, xcoord, ycoord)
# 
# write.csv(hh_survey, file="Processing/survey_w_cord.csv")
```


```{r}

hh_survey <- hh_survey%>%
  mutate(phone = ifelse((MT12 == "AT LEAST ONCE A WEEK" | MT12=="ALMOST EVERY DAY"),1,0))%>%
  select(HH1,	HH2	,WM14,	WB4,	HH6,	HH7,	HH7A,	WB5,	WB6A,	WB6B,	WB7,	WB9,	WB10A,	WB10B,	WB11,	WB12A,	WB14,	MT2,	MT3,	MT11,	MT12,	CM1,	CM2,	MA1,	MA3,	LS1,	wscore,	wscoreu,	wscorer,	weight,	welevel,	disability,	DV1A,	DV1B,	DV1C,	DV1D,	DV1E,	women, admin3name, xcoord, ycoord, phone)

write.csv(hh_survey, file="Processing/survey_w_cord.csv")
```











 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 























<!-- Joinig fb_pop files  -->
<!-- ```{r cars} -->
<!-- fb_total_pop <- read.csv("Input/Population Clusters/population_sle_2019-07-01.csv") -->
<!-- fb_under5_pop <- read.csv("Input/Population Clusters/SLE_children_under_five.csv") -->
<!-- fb_60plus_pop <- read.csv("Input/Population Clusters/SLE_elderly_60_plus.csv") -->
<!-- fb_men_pop <- read.csv("Input/Population Clusters/SLE_men.csv") -->
<!-- fb_women15to49_pop <- read.csv("Input/Population Clusters/SLE_women_of_reproductive_age_15_49.csv") -->
<!-- fb_women_pop <- read.csv("Input/Population Clusters/SLE_women.csv") -->
<!-- fb_15to24_pop <- read.csv("Input/Population Clusters/SLE_youth_15_24.csv") -->
<!-- ``` -->

<!-- Rename facebook popuation columns  -->
<!-- ```{r cars} -->
<!-- fb_total_pop <- fb_total_pop %>% -->
<!--   rename(fb_total_pop = Population, latitude = Lat, longitude = Lon)%>% -->
<!--   round( digits = 6) -->

<!-- fb_under5_pop <- fb_under5_pop %>% -->
<!--   rename( fb_under5_pop = population) %>% -->
<!--   round(digits = 6) -->

<!-- fb_60plus_pop <- fb_60plus_pop %>%  -->
<!--   rename( fb_60plus_pop = population)%>% -->
<!--   round(digits=6) -->

<!-- fb_men_pop <- fb_men_pop%>% -->
<!--   rename(fb_men_pop = population)%>% -->
<!--   round(digits=6) -->

<!-- fb_women15to49_pop <- fb_women15to49_pop%>% -->
<!--   rename( fb_women15to49_pop = population)%>% -->
<!--   round(digits=6) -->

<!-- fb_women_pop<- fb_women_pop%>% -->
<!--   rename(fb_women_pop = population)%>% -->
<!--   round(digits=6) -->

<!-- fb_15to24_pop <- fb_15to24_pop %>% -->
<!--   rename( fb_15to24_pop = population)%>% -->
<!--   round(digits=6) -->
<!-- ``` -->

<!-- Inner Join  -->
<!-- ```{r cars} -->
<!-- fb_all_pop <- fb_total_pop %>% -->
<!--   inner_join(fb_under5_pop, by=c("latitude","longitude"))%>% -->
<!--   inner_join(fb_60plus_pop, by=c("latitude","longitude"))%>% -->
<!--   inner_join(fb_men_pop, by=c("latitude","longitude"))%>% -->
<!--   inner_join(fb_women15to49_pop, by=c("latitude","longitude"))%>% -->
<!--   inner_join(fb_women_pop, by=c("latitude","longitude"))%>% -->
<!--   inner_join(fb_15to24_pop, by=c("latitude","longitude"))%>% -->
<!--   distinct(latitude,longitude, .keep_all = TRUE) -->

<!-- ``` -->

<!-- ```{r cars} -->
<!-- write.csv(fb_all_pop, file = 'Processing/fb_all_pop.csv') -->
<!-- ``` -->

<!-- ```{r} -->
<!-- #Checking if cluster number and and hh number match for the same family across survey -->

<!-- cluster = 20 -->
<!-- hh = 13 -->

<!-- wm_test_match <- wm%>% -->
<!--   filter (wm$HH1==cluster , wm$HH2==hh)%>% -->
<!--   select(HH1, HH2, WMHINT, HH6, HH7,HH7A) -->

<!-- mn_test_match <- mn%>% -->
<!--   filter(mn$HH1==cluster,mn$HH2==hh )%>% -->
<!--   select(HH1, HH2, MWMHINT, HH6, HH7,HH7A) -->

<!-- print(head(wm_test_match)) -->
<!-- print(head(mn_test_match)) -->



<!-- ``` -->


<!-- ```{r} -->
<!-- # #Looking at the number of clusters and observation per dustrict and area in wm -->
<!-- #  -->
<!-- for (i in levels(wm$HH7A)) -->
<!--   { -->
<!--   for (j in levels(wm$HH6)) -->
<!--   { -->
<!--      wm_test <- wm %>% -->
<!--     filter(HH7A == i, HH6 ==j) -->
<!--   print (paste(i, " and ", j, n_distinct(wm_test$HH1), nrow(wm_test))) -->
<!--   } -->
<!--   } -->

<!-- ``` -->





<!-- Creating the Region and Disctrict variable for cord  -->
<!-- ```{r cars} -->
<!-- cord$District <- NA  -->
<!-- cord$Area <- NA -->

<!-- cord$Area[cord$place == "city"| cord$place == "farm"| cord$place == "neighbourhood"| cord$place == "quarter"| cord$place == "state"| cord$place == "suburb"| cord$place == "town"] <- "Urban" -->

<!-- cord$Area[cord$place == "hamlet"| cord$place == "isolated_dwelling"| cord$place == "locality"| cord$place == "yes" | cord$place == "village"] <- "Rural" -->

<!-- cord$District <- toupper(cord$Language_admin2_name) -->
<!-- ``` -->
