#Setup
library(ggplot2)
library(xtable)
library(dplyr)
library(stringr)
library(data.table)
df0 <- read.csv("countries of the world.csv", dec=",", strip.white=T, head(T))

#Data clean-up
dim(df0[rowSums(is.na(df0))>0,]) #Check for rows containing missing values
#48 out of 227 rows have missing values in one or more columns
df <- na.exclude(df0) #we will remove these rows

df$Region <- str_trim(df$Region) #remove trailing white spaces

#rename columns
oldnames <- c("Area..sq..mi..","Pop..Density..per.sq..mi..","Coastline..coast.area.ratio.","Net.migration","Infant.mortality..per.1000.births.","GDP....per.capita.","Literacy....","Phones..per.1000.","Arable....","Crops....","Other....")
newnames <- c("Area_sqmi","Popdensity_persqmi","Coastline","Netmigration","Infantmortality_per1k","GDP_pc","Literacy","Phones_per1k","Arableland","Crops","Other")
df <- setnames(df,old=oldnames,new=newnames,skip_absent=T)

#rescale literacy and land usage composition from percentage to decimal:
index <- c(10,12:14)
df[,index] <- df[,index]/100

#check factor with highest number of observations
table(df$Region) 
table(df$Climate) 

#Categories of low counts (after observations with missing data):
#N.America-2: Bermuda, USA
#Baltics-2: Estonia, Latvia
#N.Africa-3: Algeria, Egypt, Tunisia
#N.America more economically different to rest of America, retain
#N.Africa more historically and culturally different to Sub-Saharan Africa, retain
#However will merge Baltics into Eastern Europe
df$Region[df$Region == "BALTICS"] <- "EASTERN EUROPE"

#Convert categorical variables to factors
df$Region <- as.factor(df$Region)
df$Climate <- as.factor(df$Climate)

#Redefine reference level to one with most observations:
df$Climate <- relevel(df$Climate,"2")
df$Region <- relevel(df$Region, "SUB-SAHARAN AFRICA")

#Combine Arableland and Crops
#Arablelands = Land cultivated for crops
#Crops = Permenant Crops
#Other = Other land use not arableland nor crops
df$Crops <- df$Arableland + df$Crops #combine crops
df <- subset(df,select=-c(Arableland)) #remove

#Histogram of GDP_pc
par(mfrow=c(1,2)) 
hist(df$GDP_pc, xlab="GDP per capita", main="Histogram of GDP per capita")
hist(log(df$GDP_pc), xlab="log(GDP per capita)", main="Histogram of log(GDP per capita)")

#index for continuous variables
variable_index <- c(3:8,10:13,15:19)

#Exploring the relationship between GDP_pc and each continuous independent variable separately
for (i in variable_index){
  plot1 <- ggplot(aes_string(x=names(df)[i],y=log(df$GDP_pc)),data=df)+
    geom_point()+
    ylab("log(GDPperCap)")+
    geom_smooth(method="lm",se=F)
  print(plot1)
}

#Correlation between GDP_pc and continuous independent variables, comparing with and without log-transform
cor0 <- t(cor(log(df$GDP_pc),df[,variable_index]))
cor1 <- t(cor(log(df$GDP_pc),log(df[,variable_index])))
corr <- cbind(cor0,cor1)
colnames(corr) <- c("Raw", "Log-transform")
xtable(corr, digits=4) #table output

#index for continuous variables that can be log-transformed
logtransformable <- setdiff(variable_index,c(6:7,10,13:14,17))

#Standardised Residuals and Normal Q-Q plot
for (i in variable_index){
  par(mfrow=c(2,2),oma=c(0,0,2,0)) #top wide margin
  plot.new()
  mtext(names(df)[i],outer=TRUE,cex=1.5) #plot title
  model_1 <- lm(paste("log(GDP_pc)~",names(df)[i]),data=df)
  par(mfg=c(1,1)) #plot at top-left
  plot(model_1,1,sub.caption = "") #Standardised residuals
  par(mfg=c(2,1)) #plot at bottom-left
  plot(model_1,2,sub.caption = "") #Normal Q-Q
  #compare with graphs if log-transforming the variable
  if (is.element(i,logtransformable)){
    model_2 <- lm(paste("log(GDP_pc)~log(",names(df)[i],")"),data=df)
    par(mfg=c(1,2)) #plot at top-right
    plot(model_2,1,sub.caption = "") #Normal Q-Q
    mtext("log",adj=1, col="red") #plot label
    par(mfg=c(2,2)) #plot at bottom-right
    plot(model_2,2,sub.caption = "") #Normal Q-Q
    mtext("log",adj=1, col="red") #plot label
  }
}

#Transforming variables (Log-transform)
logvar <- c(3:5,8,11)
df[logvar] <- log(df[logvar])

#Correlation matrix of continuous independent variables
cor2 <- cor(df[,variable_index])
xtable(cor2,digits=4) #table output

#Verifying relationship after transformation
for (i in variable_index){
  plot2 <- ggplot(aes_string(x=names(df)[i],y=df$GDP_pc),data=df)+
    geom_point()+
    ylab("log(GDPperCap)")+
    geom_smooth(method="lm",se=F)
  print(plot2)
}

#Initial fit with all variables (excluding Country as this is a label)
Full<- lm(log(GDP_pc) ~.-Country,data=df)
summary(Full) #adjusted r2 = 0.8786

Null <- lm(log(GDP_pc) ~1,data=df) #Model with only the intercept

#Backward elimination algorithm:
BE <- step(Full,scope=list(lower=Null,upper=Full),direction="backward",trace=F)
summary(BE)
#Coefficients dropped: Population, Area_sqmi, Popdensity_persqmi, Coastline, Literacy, Climate, Industry, Service

#Forward selection algorithm:
FS <- step(Null,scope=list(lower=Null,upper=Full),direction="forward",trace=F)
summary(FS)
#Coefficients dropped: Population, Area_sqmi, Popdesnity_persqmi, Coastline, Literacy, Climate, Industry, Service

#First revision: Drop continuous variables not included in both BE and FS
#Drop Population, Area_sqmi, Popdensity_persqmi, Coastline, Literacy, Industry, Service
df1 <- subset(df,select=-c(Country,Population,Area_sqmi,Popdensity_persqmi,Coastline,Literacy,Industry,Service))
firstmodel <- lm(log(GDP_pc) ~ .,data=df1)
summary(firstmodel) #adjusted r2 = 0.8818
anova(firstmodel,Full) #p-value 0.8867 > 0.05

#Test dropping Climate:
secondmodel <- lm(log(GDP_pc) ~ .-Climate,data=df1)
summary(secondmodel) #adjusted r2 = 0.8833
anova(secondmodel,firstmodel) #p-value 0.7065 > 0.05
df2 <- subset(df1,select=-c(Climate)) #remove

#Test dropping Region:
thirdmodel <- lm(log(GDP_pc) ~ .-Region,data=df2)
summary(thirdmodel) #adjusted r2 = 0.8775
anova(thirdmodel,secondmodel) #p-value 0.05037 ~ 0.05 #keep

#Test dropping Other:
fourthmodel <- lm(log(GDP_pc) ~ .-Other,data=df2)
summary(fourthmodel) #adjusted r2 = 0.8798
anova(fourthmodel,secondmodel) #p-value 0.01686 < 0.05
df3 <- subset(df2,select=-c(Other)) #remove

#Test dropping Birthrate:
fifthmodel <- lm(log(GDP_pc) ~ .-Birthrate,data=df3)
summary(fifthmodel) #adjusted r2 = 0.874
anova(fifthmodel,fourthmodel) #p-value 0.0032 < 0.05 #keep

#final model
finalmodel <- lm(log(GDP_pc) ~ .,data=df3)
summary(finalmodel)

#Check for multi-colinearity
car::vif(finalmodel)
xtable(car::vif(finalmodel),digits=c(0,6,0,6))
#all variables have VIF < 10, so no indication of multi-colinearity

par(mfrow=c(1,3)) #graph output
plot(finalmodel,1,sub.caption = "") #Standardised residuals plot
plot(finalmodel,5,sub.caption = "") #Residuals-Leverage plot
plot(finalmodel,2,sub.caption = "") #Normal Q-Q plot

par(mfrow=c(1,1)) #graph output
plot(finalmodel,4,sub.caption = "") #Cook's distance plot
abline(h=0.02, col="red", lty=2) #threshold 4/n approx 0.02

#Compute new model without outliers:
cd <- cooks.distance(finalmodel)
outliers <- names(cd)[cd > 0.02] #15 outliers
df4 <- df3[!(row.names(df) %in% outliers),]
finalmodel2 <-lm(log(GDP_pc) ~ .,data=df4)
summary(finalmodel2)

mean(finalmodel$residuals) #mean of residuals: near zero
sum(finalmodel$residuals) #sum of residuals: near zero
