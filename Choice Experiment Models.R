
setwd('~/R')

packages <- c("lattice","Icens","DCchoice","MASS","AER","pscl","VGAM","ggplot2","foreign",
              "Matrix","mgcv","boot","car","support.CEs","countreg")
lapply(packages, library, character.only=TRUE)

park.data = read.table(file="park data.txt", header=TRUE, sep=",",dec=".",na.strings ="Y")
attach(park.data)

summary(park.data)
#Basic Model
V <- CHOICE ~ ASC + DIV_S + DIV_L + FAC_T + FAC_TL + LIT_B + LIT_BR + COST + strata(STR)

Basic_model <- clogit(V, data=park.data)
Basic_model

Basic_Coef <- as.numeric(Basic_model$coefficients)



Basic_Coef

#estimating WTP for each attribute
WTP_DIV_S <- -Basic_Coef[2]/Basic_Coef[8]
WTP_DIV_L <- -Basic_Coef[3]/Basic_Coef[8]
WTP_FAC_T <- -Basic_Coef[4]/Basic_Coef[8]
WTP_FAC_TL <- -Basic_Coef[5]/Basic_Coef[8]
WTP_LIT_B <- -Basic_Coef[6]/Basic_Coef[8]
WTP_LIT_BR <- -Basic_Coef[7]/Basic_Coef[8]



WTP_DIV_S
WTP_DIV_L
WTP_FAC_T
WTP_FAC_TL
WTP_LIT_B
WTP_LIT_BR

#goodness of fit measure
gofm(Basic_model)

#Total willingness to Pay
Total_Wtp <- WTP_DIV_L*1 + WTP_FAC_TL *1 + WTP_LIT_B *1
Total_Wtp


#split sample model, testing for socio-economic influence 

summary(park.data)
dim(park.data)
hist(Q8_GARDEN)
hist(Q12_PAST)


NoGardenV <- ifelse(Q8_GARDEN==3, 1,0)
NoGardenV

mean(NoGardenV, na.rm=TRUE)
# 57% are option 3, ( dont have a garden)

# to account for Na
gardendk <- ifelse(is.na(Q8_GARDEN)==TRUE, 1,0)

#add new columns to dataset
park.data <- cbind(park.data, NoGardenV, gardendk)


#create garden subset
nogardendata <- subset(park.data, NoGardenV==1 & gardendk==0)
gardendata <- subset(park.data, NoGardenV==0 & gardendk==0)


dim(nogardendata)
dim(gardendata)

#split garden model
nogarden_model <- clogit(V, data=nogardendata)
nogarden_model

garden_model <- clogit(V, data=gardendata)
garden_model

gofm(garden_model)
gofm(nogarden_model)

NoGarden <- as.numeric(nogarden_model$coefficients)
Garden <- as.numeric(garden_model$coefficients)


WTP_DIV_S_NG <- -NoGarden[2]/NoGarden[8]
WTP_DIV_L_NG <- -NoGarden[3]/NoGarden[8]
WTP_FAC_TL_NG <- -NoGarden[4]/NoGarden[8]
WTP_FAC_T_NG <- -NoGarden[5]/NoGarden[8]
WTP_LIT_BR_NG <- -NoGarden[6]/NoGarden[8]
WTP_LIT_B_NG <- -NoGarden[7]/NoGarden[8]

WTP_DIV_S_NG
WTP_DIV_L_NG
WTP_FAC_TL_NG
WTP_FAC_T_NG
WTP_LIT_BR_NG
WTP_LIT_B_NG

WTP_DIV_S_G <- -Garden[2]/Garden[8]
WTP_DIV_L_G <- -Garden[3]/Garden[8]
WTP_FAC_TL_G <- -Garden[4]/Garden[8]
WTP_FAC_T_G <- -Garden[5]/Garden[8]
WTP_LIT_BR_G <- -Garden[6]/Garden[8]
WTP_LIT_B_G <- -Garden[7]/Garden[8]

WTP_DIV_S_G	
WTP_DIV_L_G	
WTP_FAC_TL_G	
WTP_FAC_T_G	
WTP_LIT_BR_G	
WTP_LIT_B_G	


#Total WTP for no garden respondents
NoGarden_WTP <- WTP_DIV_L_NG + WTP_FAC_TL_NG + WTP_LIT_BR_NG
Garden_WTP <- WTP_DIV_L_G + WTP_FAC_TL_G + WTP_LIT_BR_G

NoGarden_WTP
Garden_WTP


#interactions between gender and attributes

hist(Q7_GEN)
women <- ifelse(Q7_GEN==1, 1, 0)
mean(women, na.rm=TRUE)
#58% women in the sample

#exclude don't no's
gendk <- ifelse(is.na(Q7_GEN)==TRUE, 1, 0)

park.data <- cbind(park.data, women, gendk)
park.data
womendata <- subset(park.data, women==1 & gendk==0)
notwomendata <- subset(park.data, women==0, gendk==0)

V2 <- CHOICE ~ ASC + DIV_S + DIV_L + FAC_T + FAC_TL + LIT_B + LIT_BR + COST + strata(STR)
Model_W <- clogit(V2, data=womendata)
Model_W

Model_NW <- clogit(V2, data=notwomendata)
Model_NW

gofm(Model_W)
gofm(Model_NW)

BIA <- as.numeric(Model_W$coefficients)
Bia2 <- as.numeric(Model_NW$coefficients)

WTP_DIV_S_W <- -BIA[2]/BIA[8]
WTP_DIV_L_W <- -BIA[3]/BIA[8]
WTP_FAC_L_W <- -BIA[4]/BIA[8]
WTP_FAC_TL_W <- -BIA[5]/BIA[8]
WTP_LIT_B_W <- -BIA[6]/BIA[8]
WTP_LIT_BR_W <- -BIA[7]/BIA[8]

WTP_DIV_S_W
WTP_DIV_L_W
WTP_FAC_L_W
WTP_FAC_TL_W
WTP_LIT_B_W
WTP_LIT_BR_W

WTP_DIV_S_NW <- -Bia2[2]/Bia2[8]
WTP_DIV_L_NW <- -Bia2[3]/Bia2[8]
WTP_FAC_L_NW <- -Bia2[4]/Bia2[8]
WTP_FAC_TL_NW <- -Bia2[5]/Bia2[8]
WTP_LIT_B_NW <- -Bia2[6]/Bia2[8]
WTP_LIT_BR_NW <- -Bia2[7]/Bia2[8]

WTP_DIV_S_NW
WTP_DIV_L_NW
WTP_FAC_L_NW
WTP_FAC_TL_NW
WTP_LIT_B_NW
WTP_LIT_BR_NW

#total WTP in terms of gender
Women_WTP <- WTP_DIV_L_W + WTP_FAC_TL_W + WTP_LIT_BR_W
NotWomen_WTP <- WTP_DIV_L_NW + WTP_FAC_TL_NW + WTP_LIT_BR_NW

Women_WTP
NotWomen_WTP
