
# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
  # if I'm on the laptop
  setwd("/home/tim/git/YearsLost/YearsLost")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/YearsLost/YearsLost"))
}

source("R/Functions.R")
HMDcountries <- c("USA","CAN","FRATNP","SWE","NOR","GBRTENW")
CODcountries <- c("USA","CAN","FRA","SWE","NOR","ENW")
library(RCurl)
library(DemogBerkeley)
library(reshape2)

grabCountryHMD <- function(XXX,Yr=2010,.us=us,.pw=pw){
  
    Dx  <- readHMDweb(XXX,"Deaths_1x1",username = .us, password = .pw)
    Ex  <- readHMDweb(XXX,"Exposures_1x1",username = .us, password = .pw)
    .Yr <- ifelse(Yr %in% Dx$Year, Yr, max(Dx$Year))
    Dxm <- Dx$Male[Dx$Year == .Yr]
    Exm <- Ex$Male[Ex$Year == .Yr]
    
    Dxf <- Dx$Female[Dx$Year == .Yr]
    Exf <- Ex$Female[Ex$Year == .Yr]
    
    Mxm <- Dxm / Exm
    Mxf <- Dxf / Exf
    
    # cheap lx
    lxm <- c(1, exp(-cumsum(Mxm)))
    lxf <- c(1, exp(-cumsum(Mxf)))
    
    
    dxm <- lx2dx(lxm)
    dxf <- lx2dx(lxf)
    list(Dxm = Dxm, Dxf = Dxf, Mxm = Mxm, Mxf = Mxf, lxm = lxm, lxf = lxf, dxm = dxm, dxf = dxf)
}
 

# 1) get required HMD data, stick into list
HMD <- lapply(HMDcountries,grabCountryHMD)
head(HMD)
names(HMD) <- CODcountries
save(HMD, file = "Data/HMD.Rdata")
# ---------------------------------------------------------------------------
# 2) get COD data
# ---------------------------------------------------------------------------

# a function to expand ages. a bit overly complex, I admit 
expandAbdridged <- function(M){
    do.call(rbind,list(M[1,],
                    matrix(M[2,],nrow = 4, ncol = ncol(M), byrow = TRUE),
                    do.call(rbind,lapply(
                                    apply(
                                            M[3:nrow(M),],1,function(x){
                                                list(matrix(x,nrow = 5, ncol = length(x), byrow = TRUE))
                                            }),"[[",1)
                    )))
}
# Code List for HMD chapters to custom 8-code
# ---------------------------------------------------------------------------
#chapter	chapterName	            INED6	INED6Name	Code8	Code8Name
# 1	    Certain infectious diseases	    3	Infectious	1	Infectious
# 2	    Malignant neoplasms	            1	Cancer	    2	Cancer
# 3	    Other neoplasms	                1	Cancer	    2	Cancer
# 4	    Blood and blood-forming orga    2	Other	    3	Other
# 5	    Endocrine, nutrition., metab    4	Other	    3	Other
# 6	    Mental and behavioural diso     4	Other	    8	Mental
# 7	    Diseases of the nervous system	4	Other	    3	Other
# 8	    Heart disease	                2	Cardio	    4	Cardio
# 9	    Cerebrovascular disease	        2	Cardio	    4	Cardio
# 10	Other unsp. dis. circ   	    2	Cardio	    4	Cardio
# 11	Respiratory diseases	        3	Infectious	1	Infectious
# 12	Diseases of the digestive       4	Other	    3	Other
# 13	Diseases of the skin	        4	Other	    3	Other
# 14	Diseases of the musculoske      4	Other	    3	Other
# 15	Diseases of the genitourin      4	Other	    3	Other
# 16	Compli preg,  child	            4	Other	    3	Other
# 17	Certain conditions originating  4	Other	    5	Infant/Cong.
# 18	Congenital malformations/anom   4	Other	    5	Infant/Cong.
# 19	Ill-defined or unknown	        6	Ill defined	6	Ill defined
# 20	External causes	                5	Injuries	7	External
# ---------------------------------------------------------------------------
# XXX <- "NOR"

grabCountryCOD <- function(XXX,HMD,Yr = 2010){
  cat(XXX,"\n")
    COD         <- read.csv(file.path("Data","COD5x1",paste0(XXX,"_5x1_chapters.csv")), 
            stringsAsFactors = FALSE)
    .Yr <- ifelse(Yr %in% COD$Year, Yr, max(COD$Year))
    COD         <- COD[COD$Year == .Yr & COD$COD.chap != "All", ]
    COD$Age     <- as.integer(gsub("\\+","",unlist(lapply(strsplit(COD$Age,split = "-"),"[[",1))))
    
    # these code are hard-coded, but could be generalized for custom aggregations.
    # recode causes
    recvec <- c(1,2,2,3,3,8,2,4,4,4,1,3,3,3,3,3,5,5,6,7)
    names(recvec) <- sprintf("%.2d",1:20)
    
    TimCodes <- 1:8
    names(TimCodes) <- c("Infectious","Cancer","Other","Cardio","Inf_Cong","Ill defined","External","Mental")
    
    TimNames      <- names(TimCodes)
    names(TimNames) <- 1:8
    COD$Code8     <- recvec[COD$COD.chap]
    COD$Code8Name <- TimNames[as.character(COD$Code8)]
    # ---------------------------------------------------------------------
  
    # put into ageXcause array
    Mf          <- acast(COD, Age ~ Code8Name, value.var = "Rates.F", sum)/1e5
    Mm          <- acast(COD, Age ~ Code8Name, value.var = "Rates.M", sum)/1e5
    
    # these were re-ordered by acast()...
    TimNames    <- colnames(Mm)
    
    # These are rates per 100000. Do all calcs then divide out back to pure rates.
    # tried to smooth pattern, not worth it
    ind0m       <- Mm == 0
    ind0f       <- Mf == 0
    
    # this us just so the spline doesn't break: we put the zeros back later.
    #Mm[ind0m]   <- 1e-6 # it turns out sometimes this is pernicious
    #Mf[ind0f]   <- 1e-6
    
    MaxA        <- max(COD$Age)
    # for imputation of 0s later
    ind0m1      <- expandAbdridged(ind0m)[1:(MaxA + 1), ]
    ind0f1      <- expandAbdridged(ind0f)[1:(MaxA + 1), ]
    
    # ages for fitting
    ages                       <- sort(unique(COD$Age))
    ages[1:(length(ages) - 1)] <- ages[1:(length(ages) - 1)] + diff(ages) / 2
    ages[1]                    <- 0
    
    
    Mf1 <- apply(log(Mf),2,function(y,MaxA){
        infi <- !is.infinite(y)
        exp(splinefun(y[infi]~ages[infi])(0:MaxA))  
            },MaxA=MaxA) 
    Mm1 <- apply(log(Mm),2,function(y,MaxA){
        infi <- !is.infinite(y)
        exp(splinefun(y[infi]~ages[infi])(0:MaxA))
            },MaxA=MaxA)
    # puts zeros back in where they belong
    Mf1[ind0f1] <- 0
    Mm1[ind0m1] <- 0
    
    # get Fracs (no need to get rid of 1e5)
    Mfe <- Mf1 / rowSums(Mf1)
    Mme <- Mm1 / rowSums(Mm1)
    
    # add on ages 101-110
    Mfe <- rbind(Mfe, t(replicate((110-MaxA), Mfe[nrow(Mfe), ])))
    Mme <- rbind(Mme, t(replicate((110-MaxA), Mme[nrow(Mme), ])))
    rownames(Mfe) <- rownames(Mme) <- 0:110
   
   
    #####################################################################
    # deleted frac:
    # K = Keep
    Kf <- 1 - Mfe
    Km <- 1 - Mme
    
    # deaths from each cause
    Dxfc <- HMD[[XXX]]$Dxf * Mfe
    Dxmc <- HMD[[XXX]]$Dxm * Mme
    
    # Mx of each cause:
    Mxmc <- HMD[[XXX]]$Mxm * Mme
    Mxfc <- HMD[[XXX]]$Mxf * Mfe

    # cause-deleted lx (simplified)
    Klxm <- apply(HMD[[XXX]]$Mxm * Km ,2,function(x){
                c(1,exp(-cumsum(x)))
            })
    Klxf <- apply(HMD[[XXX]]$Mxf * Kf ,2,function(x){
                c(1,exp(-cumsum(x)))
            })
    dimnames(Klxm) <- dimnames(Klxf) <- list(0:111, TimNames)
    # cause-deleted dx
    Kdxm <- apply(Klxm, 2, lx2dx)
    Kdxf <- apply(Klxf, 2, lx2dx)
    dimnames(Kdxm) <- dimnames(Kdxf) <- list(0:110, TimNames)
    
    # stupid list trick. could be done another way I'm sure
    LDm_list <- lapply(apply(Klxm,2,function(x){
                        list(makeLD(x))
                    }
            ),"[[",1)
    LDf_list <- lapply(apply(Klxf,2,function(x){
                        list(makeLD(x))
                    }
            ),"[[",1)
    
    
    DWmc_list <- lapply(names(LDm_list), function(CD, LDm_list, Dxmc){
                t(LDm_list[[CD]])[2:112, 2:112] * Dxmc[,CD]
            }, LDm_list = LDm_list, Dxmc = Dxmc)
    DWfc_list <- lapply(names(LDf_list), function(CD, LDf_list, Dxfc){
                t(LDf_list[[CD]])[2:112, 2:112] * Dxfc[,CD]
            }, LDf_list = LDf_list, Dxfc = Dxfc)
    
    names(DWmc_list) <-  names(DWfc_list) <- names(LDm_list)
            
    invisible(
            list(
            DWmc_list = DWmc_list, DWfc_list = DWfc_list,     
            LDm = LDm_list, LDf = LDf_list, 
            Klxm = Klxm, Klxf = Klxf,
            Kdxm = Kdxm, Kdxf = Kdxf, 
            Mxmc = Mxmc, Mxfc = Mxfc, 
            Dxfc = Dxfc, Dxmc = Dxmc,
            Yr = Yr, MaxA = MaxA))
    
}


COD <- lapply(CODcountries,grabCountryCOD,HMD=HMD)
names(COD) <- CODcountries

save(COD, file = "Data/COD.Rdata")

####################################################################################
# need some code specific for USA data, working at different levels of aggregation.

# USA 92
HMDUSA <- grabCountryHMD("USA")
CODUSA <- local(get(load("Data/USA2010.Rdata")))
#colnames(CODUSA)
#unique(CODUSA$ChapterName)
prepareUSAcause <- function(CODUSA,HMDUSA,VarName="Name8"){

  # put into ageXcause array
  Mf          <- acast(CODUSA, as.formula(paste0("Age~",VarName)), value.var = "Rates.F", sum)/1e5
  Mm          <- acast(CODUSA, as.formula(paste0("Age~",VarName)), value.var = "Rates.M", sum)/1e5
  
  # these were re-ordered by acast()...
  Names    <- colnames(Mm)
  
  # These are rates per 100000. Do all calcs then divide out back to pure rates.
  # tried to smooth pattern, not worth it
  ind0m       <- Mm == 0
  ind0f       <- Mf == 0
  
  # this us just so the spline doesn't break: we put the zeros back later.
  #Mm[ind0m]   <- 1e-6 # it turns out sometimes this is pernicious
  #Mf[ind0f]   <- 1e-6
  
  MaxA        <- max(CODUSA$Age)
  # for imputation of 0s later
  ind0m1      <- expandAbdridged(ind0m)[1:(MaxA + 1), ]
  ind0f1      <- expandAbdridged(ind0f)[1:(MaxA + 1), ]
  
  # ages for fitting
  ages                       <- sort(unique(CODUSA$Age))
  ages[1:(length(ages) - 1)] <- ages[1:(length(ages) - 1)] + diff(ages) / 2
  ages[1]                    <- 0
  
  
  Mf1 <- apply(log(Mf),2,function(y,MaxA){
      if (all(is.infinite(y)) | all(is.na(y))){
        return(rep(0, MaxA+1))
      }
      infi <- !is.infinite(y)
      exp(splinefun(y[infi]~ages[infi])(0:MaxA))  
    },MaxA=MaxA) 
  Mm1 <- apply(log(Mm),2,function(y,MaxA){
      if (all(is.infinite(y)) | all(is.na(y))){
        return(rep(0, MaxA+1))
      }
      infi <- !is.infinite(y)
      exp(splinefun(y[infi]~ages[infi])(0:MaxA))
    },MaxA=MaxA)
  # puts zeros back in where they belong
  Mf1[ind0f1] <- 0
  Mm1[ind0m1] <- 0
  
  # get Fracs (no need to get rid of 1e5)
  Mfe <- Mf1 / rowSums(Mf1)
  Mme <- Mm1 / rowSums(Mm1)
  
  # add on ages 101-110
  Mfe <- rbind(Mfe, t(replicate((110-MaxA), Mfe[nrow(Mfe), ])))
  Mme <- rbind(Mme, t(replicate((110-MaxA), Mme[nrow(Mme), ])))
  rownames(Mfe) <- rownames(Mme) <- 0:110
  
  
  #####################################################################
  # deleted frac:
  # K = Keep
  Kf <- 1 - Mfe
  Km <- 1 - Mme
  
  # deaths from each cause
  Dxfc <- HMDUSA$Dxf * Mfe
  Dxmc <- HMDUSA$Dxm * Mme
  
  # Mx of each cause:
  Mxmc <- HMDUSA$Mxm * Mme
  Mxfc <- HMDUSA$Mxf * Mfe
  
  # cause-deleted lx (simplified)
  Klxm <- apply(HMDUSA$Mxm * Km ,2,function(x){
      c(1,exp(-cumsum(x)))
    })
  Klxf <- apply(HMDUSA$Mxf * Kf ,2,function(x){
      c(1,exp(-cumsum(x)))
    })
  dimnames(Klxm) <- dimnames(Klxf) <- list(0:111, Names)
  # cause-deleted dx
  Kdxm <- apply(Klxm, 2, lx2dx)
  Kdxf <- apply(Klxf, 2, lx2dx)
  dimnames(Kdxm) <- dimnames(Kdxf) <- list(0:110, Names)
  
  # stupid list trick. could be done another way I'm sure
  LDm_list <- lapply(apply(Klxm,2,function(x){
        list(makeLD(x))
      }
    ),"[[",1)
  LDf_list <- lapply(apply(Klxf,2,function(x){
        list(makeLD(x))
      }
    ),"[[",1)
  
  
  DWmc_list <- lapply(names(LDm_list), function(CD, LDm_list, Dxmc){
      t(LDm_list[[CD]])[2:112, 2:112] * Dxmc[,CD]
    }, LDm_list = LDm_list, Dxmc = Dxmc)
  DWfc_list <- lapply(names(LDf_list), function(CD, LDf_list, Dxfc){
      t(LDf_list[[CD]])[2:112, 2:112] * Dxfc[,CD]
    }, LDf_list = LDf_list, Dxfc = Dxfc)
  
  names(DWmc_list) <-  names(DWfc_list) <- names(LDm_list)
  
  invisible(
    list(
      DWmc_list = DWmc_list, DWfc_list = DWfc_list,     
      LDm = LDm_list, LDf = LDf_list, 
      Klxm = Klxm, Klxf = Klxf,
      Kdxm = Kdxm, Kdxf = Kdxf, 
      Mxmc = Mxmc, Mxfc = Mxfc, 
      Dxfc = Dxfc, Dxmc = Dxmc,
      MaxA = MaxA))
  
}

CODUSA8 <- prepareUSAcause(CODUSA,HMDUSA,VarName="Name8")
CODUSAChapter <- prepareUSAcause(CODUSA,HMDUSA,VarName="ChapterName")
CODUSACause <- prepareUSAcause(CODUSA,HMDUSA,VarName="CauseName")

save(CODUSA8, file = "Data/CODUSA8.Rdata")
save(CODUSAChapter, file = "Data/CODUSAChapter.Rdata")
save(CODUSACause, file = "Data/CODUSACause.Rdata")

#matplot(0:110,log(CODUSACause$Mxfc),type='l')
#colnames(CODUSACause$Mxfc)
#plot(0:110,log(CODUSACause$Mxfc[,"Breast"]),type='l',col="red")
#lines(0:110,log(CODUSACause$Mxmc[,"Prostate"]),col="blue")
#
#plot(0:110,CODUSA8$Mxfc[,"Cancer"],type='l',col="red",log='y',ylim=c(5e-6,.5))
#lines(0:110,CODUSA8$Mxmc[,"Cancer"],col="blue")
#lines(0:110,CODUSA8$Mxfc[,"Cardio"],col="red",lty=2)
#lines(0:110,CODUSA8$Mxmc[,"Cardio"],col="blue",lty=2)
#
#head(CODUSA)
#plot(CODUSA$Age[CODUSA$CauseName=="Prostate"],CODUSA$Rates.M[CODUSA$CauseName=="Prostate"]/1e5,type="s",lo='y')
#lines(0:110,CODUSACause$Mxmc[,"Prostate"],col="blue")

plot(CODUSACause$Dxmc[,"Prostate"],col="blue")
names(CODUSACause)