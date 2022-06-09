#code for Geodetector of 
#"Greenspace exposure conducive for the resilience of public sentiment during the COVID-19 pandemic"
#author: Liuyi Song

library('geodetector')
#read the data
data<-read.table('./Wuhan_urban_after.txt',header = TRUE)
str(data)
#Factor Detector
factor_detector ("S3",c("NDVI","Building","Sidewalk"),data)
#Interaction Detector
interaction_detector("S3",c("NDVI","Building","Sidewalk"),data)



