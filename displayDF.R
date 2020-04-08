# change this to your working directory
FILE_NAME <- "3150 DATASET.xlsx"

open_excel <- function() {
  return (read_excel(file.path(FILE_NAME), skip=6))
}

gender_split <- function(sex, df) {
  return (df[which(df["Sex"] == sex),])
}

plot_cor_with_title <- function(tit, df, args) {
  x <- dummify(df)
  y = cor(x[,args])
  View(y, title=tit)
}

main <- function() {
  library("readxl")
  library("DataExplorer")
  people <- open_excel()
  
  #seperate the male and female data
  females <- gender_split('F',people)
  males <- gender_split('M',people)
  
  # splitting on each age group
  # (18-49, 40-60)
  age_cat1 <- people[(which((people["Age"] <= 49) & (people["Age"] >= 28))),]  
  age_cat2 <- people[(which((people["Age"] <= 60) & (people["Age"] >= 40))),]
  
# sanity checks
  # View(age_cat1)
  # View(age_cat2)
  
  # splitting each catagory into subgroups
  age_cat1_female <- gender_split('F',age_cat1)
  age_cat1_male <- gender_split('M',age_cat1)
  
  age_cat2_female <- gender_split('F',age_cat2)
  age_cat2_male <- gender_split('M',age_cat2)
  
# data exploring
  # DataExplorer::create_report(age_cat1, output_file = "cat1.html")
  # DataExplorer::create_report(age_cat2, output_file = "cat2.html")
  # 
  # DataExplorer::create_report(age_cat1_female, output_file = "cat1_F.html")
  # DataExplorer::create_report(age_cat1_male, output_file = "cat1_M.html")
  # 
  # DataExplorer::create_report(age_cat2_female, output_file = "cat2_F.html")
  # DataExplorer::create_report(age_cat2_male, output_file = "cat2_M.html")
  
# Table figures 1 & 2
  param=c("Colds","Ave.Sleep","Depression_Y")
  plot_cor_with_title("Males Age 40-60",age_cat2_male, param)
  plot_cor_with_title("Females Age 40-60",age_cat2_female, param)
  plot_cor_with_title("Males Age 28-49",age_cat1_male, param)
  plot_cor_with_title("Females Age 28-49",age_cat1_female, param)
  

  # DataExplorer::create_report(people)
  # DataExplorer::create_report(females, output_file = "detailed_report_females.html")
  # DataExplorer::create_report(males, output_file = "detailed_report_males.html")
  
}
