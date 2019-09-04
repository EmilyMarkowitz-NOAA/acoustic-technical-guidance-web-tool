#########INTRODUCTION###################
# Underwater Thresholds for Onset of Permanent and Temporary Threshold Shifts (Version 2.0)
# Writtiten by Emily Markowitz
# August 2018
# written on R version 3.4.1 (2017-06-30) -- "Single Candle"
# Version 1.0.153 - ? 2009-2017 RStudio, Inc.
# https://jmlondon.shinyapps.io/AcousticThresholds/

# #TESTING#####
# input<-list()
# input$ui1<-180
# input$ui2<-10
# input$ui3<-10
# input$ui4<-10
# input$ui5<-10
# input$ui6<-10
# input$ui7<-10
# input$ui8<-10
# input$ui9<-10
# # input$ui2<-NA
# # input$ui3<-NA
# # input$ui4<-NA
# # input$ui5<-NA
# # input$ui6<-NA
# # input$ui7<-NA
# # input$ui8<-NA
# # input$ui9<-NA
# input$ui1w1<-10
# input$ui1w2<-10
# input$ui1w3<-10
# input$ui1w4<-10
# input$ui1w5<-10
# input$SoundSource<-"A"
# input$SoundCatagory<-"A"
# input$methods.BroadNarrow<-"Narrow0"
# input$SoundLevelMetrics<-".1"

#########LIBRARY FUNCTIONS###################
# dir0<-"C:/Users/Emii/Documents/Homework/Knauss/AcousticThresholds/"
# setwd(dir0)

#install.packages("ui8")
# install.packages("shiny")
# install.packages("shinyjs")
# install.packages("knitr")
# install.packages("shinythemes")

library(shiny)
library(knitr)
library(kableExtra)
library(magrittr)
library(dplyr)
library(kableExtra)
library(formattable)
# https://stackoverflow.com/questions/33807945/shiny-updateselectinput-for-multiple-inputs
library(shinyjs)
library(shinyBS)
require(V8)
library(DT)
library(ggplot2)

library(knitr)
library(markdown)
# #https://stackoverflow.com/questions/33499651/rmarkdown-in-shiny-application

library(shinythemes)

# install.packages("devtools")
# devtools::install_github("Appsilon/shiny.router")
# library(shiny.router)
library(shinydashboard)

# install.packages("shiny.router")
library(shiny.router)

#######KNOWNS: FUNCTIONS############

#https://stackoverflow.com/questions/50313540/r-shiny-conditionally-change-numericinput-background-colour/50314686
jsCode1 <- '
shinyjs.backgroundCol = function(params) {
var defaultParams = {
id : null,
col : "red"
};
params = shinyjs.getParams(params, defaultParams);
var el = $("#" + params.id);
el.css("background-color", params.col);
}'

jsCode <- 'shinyjs.winprint = function(){
window.print();
}'

decimalplaces<-function(x, dec){
  xx<-formatC(as.numeric(x), format = "e", digits = dec)
  xxx<-strsplit(x = xx, split = "e")
  if (is.na(x)) {
    xxxx<-NA
  } else if (x%%1==0) {
    xxxx<-round(x, digits = 0)
  } else if (as.numeric(substr(x = xxx[[1]][2], start = 2, stop = nchar(xxx[[1]][2])))>3) {
    xxxx<-xx
  } else {
    xxxx<-round(x, digits = dec)
  }
  return(as.character(xxxx))
}

SoundEquation <- function(SoundSource, SoundCatagory) {
  if (SoundSource!="Other") {
    SoundSource
  } else {
    SoundCatagory
  }
}

MinBoundary<-function(V1) {
  if (#'Weight Function Adjustment (kHz)' == V1 |
    V1=="Low-Frequency Cetaceans" |
    V1=="Mid-Frequency Cetaceans" |
    V1=="High-Frequency Cetaceans" |
    V1=="Phocid Pinnipeds" |
    V1=="Otariid Pinnipeds" ){ #is metric limited for WFA?
    -Inf
  } else {
    0
  }
}

MaxBoundary <- function (V1) {
  if (length(grep(pattern = '24', x = V1))==1) { #is metric limited within 24 hours?
    24.0000000001
  } else if (
    V1=="Low-Frequency Cetaceans" |
    V1=="Mid-Frequency Cetaceans" |
    V1=="High-Frequency Cetaceans" |
    V1=="Phocid Pinnipeds" |
    V1=="Otariid Pinnipeds" ) { #is metric limited for WFA?
    0
  } else {
    Inf
  }
}

MinBoundary.mult<-function(V1) {
  if (#'Weight Function Adjustment (kHz)' == V1 |
    V1=="Low-Frequency Cetaceans" |
    V1=="Mid-Frequency Cetaceans" |
    V1=="High-Frequency Cetaceans" |
    V1=="Phocid Pinnipeds" |
    V1=="Otariid Pinnipeds" ){ #is metric limited for WFA?
    -Inf
  # } else if () {
  #   
  } else {
    0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
  }
}

MaxBoundary.mult <- function (V1) {
  if (length(grep(pattern = '24', x = V1))==1) { #is metric limited within 24 hours?
    24.0000000001
  } else if (
    V1=="Low-Frequency Cetaceans" |
    V1=="Mid-Frequency Cetaceans" |
    V1=="High-Frequency Cetaceans" |
    V1=="Phocid Pinnipeds" |
    V1=="Otariid Pinnipeds" ) { #is metric limited for WFA?
    0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000001
  } else {
    Inf
  }
}

fill.weigthtfuncttable<-function(WeightFactorAdjustment_kHz, weigthtfuncttable){
  for (i in 1:ncol(weigthtfuncttable)){
    weigthtfuncttable["note1",i]<-((WeightFactorAdjustment_kHz / weigthtfuncttable["f1",i] )^(2*weigthtfuncttable["a",i]))
    weigthtfuncttable["note2",i]<-(1+(WeightFactorAdjustment_kHz/weigthtfuncttable["f1",i])^2)^weigthtfuncttable["a",i]
    weigthtfuncttable["note3",i]<-(1+(WeightFactorAdjustment_kHz/weigthtfuncttable["f2",i])^2)^weigthtfuncttable["b",i]
    weigthtfuncttable["note4",i]<-weigthtfuncttable["note1",i]/(weigthtfuncttable["note2",i]*weigthtfuncttable["note3",i])
    weigthtfuncttable["Adjustment (dB)",i]<-(log10(weigthtfuncttable["note4",i])*10)+weigthtfuncttable["C",i]
  }
  return(weigthtfuncttable)
}

fill.weigthtfuncttable2<-function(WeightFactorAdjustment_kHz, #weigthtfuncttable, 
                                  ui1w1, ui1w2, ui1w3, ui1w4, ui1w5, 
                                  methods.BroadNarrow){
  weigthtfuncttable<-matrix(data = c(1,1.6, 1.8, 1,2,
                                     2,2,2,2,2,
                                     0.2, 8.8, 12,1.9,.94,
                                     19,110,140,30,25,
                                     0.13, 1.2, 1.36, 0.75, 0.64,
                                     rep_len(x = NA, length.out = (5*5))),
                            nrow = 10, ncol = 5, byrow = T)
  weigthtfuncttable<-as.data.frame(weigthtfuncttable)
  rownames(weigthtfuncttable)<-c("a", "b", "f1", "f2", "C", "Adjustment (dB)", "note1", "note2", "note3", "note4")
  if (methods.BroadNarrow=="Narrow0") {
    weigthtfuncttable<-fill.weigthtfuncttable(WeightFactorAdjustment_kHz, weigthtfuncttable)
  } else if (methods.BroadNarrow=="Broad0") {
    weigthtfuncttable["Adjustment (dB)",]<-c(ui1w1, ui1w2, ui1w3, ui1w4, ui1w5)
  }
  return(weigthtfuncttable)
}

# WeightWarning<-function(tempweight, Weighting, BroadNarrow, SoundSource, ui1) {
#   tempweight0<-c(4.8, 43, 59, 11, 8.5) #anything above these are non-applicable frequencies
#   if (BroadNarrow[[1]] == "Broad0"  & 
#       sum(is.na(tempweight))==5) { #All fields are empty
#     str1 <- paste0(tags$span(style="color:red", "Please fill in at least one adjustment in Step 5 and all values must be negative."))   
#   }  else  if (BroadNarrow[[1]] == "Broad0"  & 
#                sum(is.na(tempweight))!=0 & #Not all fields are filled
#                sum(tempweight[!is.na(tempweight)]>0)>0 ) { #Some values are not negative
#     str1 <- paste0(tags$span(style="color:red", "All values must be negative. Note: Not all input fields have been entered in Step 5."))   
#   }  else  if (BroadNarrow[[1]] == "Broad0"  & 
#                sum(is.na(tempweight))==0 & #There are no empty fields
#                sum(tempweight>0)>0 ) { #Some values are not negative
#     str1 <- paste0(tags$span(style="color:red", "All input values in step 5 must be negative."))   
#   }  else  if (BroadNarrow[[1]] == "Broad0"  & 
#                sum(is.na(tempweight))!=0 & # not all fields have been filled
#                sum(tempweight[!is.na(tempweight)]>0)==0 ) { #all fields that have been filled are negative
#     str1 <- paste0(tags$span(style="color:red", "Note: Not all input fields have been entered in Step 5."))  
#   }  else if (sum(is.na(tempweight))==5 & 
#               BroadNarrow[[1]] == "Narrow0") {
#     str1 <- paste0(tags$span(style="color:red", "Please fill Step 5."))   
#     
#   }  else if (ui1<0 & 
#               BroadNarrow[[1]] == "Narrow0") {
#     str1 <- paste0(tags$span(style="color:red", "Input values in step 5 must be positive."))   
#   } else if (BroadNarrow[[1]] == "Narrow0" & 
#              sum(tempweight[!is.na(tempweight)]>tempweight0[!is.na(tempweight)])!=0 &
#              (SoundSource!="Other" | 
#               SoundSource!="B" |
#               SoundSource!="D")) {
#     weight4warning0<-paste(paste0(rownames(Weighting)[which(tempweight>tempweight0)], 
#                                   " (<", 
#                                   tempweight0[which(tempweight>tempweight0)], ")" ), 
#                            collapse = ", ")      
#     weight4warning00<-paste0("*The weighting factors for ", weight4warning0,
#                              " are non-applicable frequencies. ",
#                              "This is only for narrowband sources. ")
#     str1 <- paste0(tags$span(style="color:red", weight4warning00))
#   } else {
#     str1 <- paste0(tags$span(style="color:black", "These are all applicable frequencies. "))
#   }
#   return(str1)
# }

Step4Warning<-function(ui2, ui3, ui4, ui5, ui6, ui7, ui8, ui9, html = T) {
  # ui2<-input$ui2; ui3<-input$ui3; ui4<-input$ui4; ui5<-input$ui5; ui6<-input$ui6; ui7<-input$ui7; ui8<-input$ui8; ui9<-input$ui9; 
  temp<-c(ui2, ui3, ui4, ui5, ui6, ui7, ui8, ui9)
  
  if (sum(is.na(temp))>0) { #All or some fields are empty
  #   str1 <- paste0(tags$span(style="color:red", "Please fill step 4."))
  #   # }  else if (!is.null(tempstep4) |
  #   #             sum(tempstep4<=0, na.rm = T)>0) { #Some fields are empty or some values are positive (as they should be)
  #   #   str1 <- paste0(tags$span(style="color:red", "All entries in step 4 must be entered."))
  # }  else if (sum(is.na(temp))!=0 |
  #             sum(temp<=0, na.rm = T)>0) { #All fields are filled and some/all values are negative
    str1 <- "All input fields in step 4 must be entered and positive values."
    if (html == T){
      str1 <- paste0(tags$span(style="color:red", str1))
    }  
    # }  else if (sum(is.na(tempstep4))<length(tempV) |
    #             sum(is.na(tempstep4))>0 |
    #             sum(tempstep4<=0, na.rm = T)<0) { #Some fields are empty or some values are negative
    #   str1 <- paste0(tags$span(style="color:red", "All input fields in step 4 must be entered and positive values."))
  }  else {
    str1 <- "All entries are applicable values."
    if (html==T){
      str1 <- paste0(tags$span(style="color:black", str1))
    } 
  }
  return(str1)
}

Step5Warning<-function(tempweight, BroadNarrow, SoundSource, ui1, html = T){ 
  tempweight0<-c(4.8, 43, 59, 11, 8.5) #anything above these are non-applicable frequencies
  if (BroadNarrow[[1]] == "Broad0"  & 
      sum(is.na(tempweight))==5) { #All fields are empty
      str1 <- "Please fill in at least one adjustment in step 5 and all values must be negative." 
      if (html==T){
        str1 <- paste0(tags$span(style="color:red", str1))
      } 
  }  else  if (BroadNarrow[[1]] == "Broad0"  & 
               sum(is.na(tempweight))!=0 & #Not all fields are filled
               sum(tempweight[!is.na(tempweight)]>0)>0 ) { #Some values are not negative
    str1 <- "All values must be negative. Note: Not all input fields have been entered in step 5."   
    if (html==T){
      str1 <- paste0(tags$span(style="color:red", str1))
    } 
  }  else  if (BroadNarrow[[1]] == "Broad0"  & 
               sum(is.na(tempweight))==0 & #There are no empty fields
               sum(tempweight>0)>0 ) { #Some values are not negative
    str1 <- "All input values in step 5 must be negative."  
    if (html==T){
      str1 <- paste0(tags$span(style="color:red", str1))
    } 
  }  else if (BroadNarrow[[1]] == "Broad0"  & 
               sum(is.na(tempweight))!=0 & # not all fields have been filled
               sum(tempweight[!is.na(tempweight)]>0)==0 ) { #all fields that have been filled are negative
      str1 <- "Note: Not all input fields have been entered in step 5."
      if (html==T){
        str1 <- paste0(tags$span(style="color:red", str1))
      } 
  }  else if (is.na(ui1) & 
              BroadNarrow[[1]] == "Narrow0") {
    str1 <- "Please fill step 5."
    if (html==T){
      str1 <- paste0(tags$span(style="color:red", str1))
    } 
  }  else if (ui1<0 & 
              BroadNarrow[[1]] == "Narrow0") {
      str1 <- paste0("Input value in step 5 must be positive.")   
    if (html==T){
    str1 <- paste0(tags$span(style="color:red", str1))
    }    
    # } else if (BroadNarrow[[1]] == "Narrow0" & 
    #            sum(tempweight[!is.na(tempweight)]>tempweight0[!is.na(tempweight)])!=0 &
    #            (SoundSource!="Other" | 
    #             SoundSource!="B" |
    #             SoundSource!="D")) {
    #   weight4warning0<-paste(paste0(rownames(Weighting)[which(tempweight>tempweight0)], 
    #                                 " (<", 
    #                                 tempweight0[which(tempweight>tempweight0)], ")" ), 
    #                          collapse = ", ")      
    #   weight4warning00<-paste0("*The weighting factors for ", weight4warning0,
    #                            " are non-applicable frequencies. ",
    #                            "This is only for narrowband sources. ")
    #   str1 <- paste0(tags$span(style="color:red", weight4warning00))
  } else {
    str1 <- "All entries are applicable frequencies."
    if (html==T){
      str1 <- paste0(tags$span(style="color:black", str1))
    }  
  }
  return(str1)
}

fill.userout<-function(SourceLevel_RMS_SPL=1, 
                       durationofsoundproduction_hrswithin24hrperiod=0,
                       durationofsoundproduction_sec=0,
                       LogDurationOfSoundProductionx10=0,
                       Propogation_xLogR=1,
                       userout.blank0,
                       weigthtfuncttable=weigthtfuncttable,
                       type=type,
                       SourceFactor=0,
                       SourceVelocity_m_sec=0,
                       SourceLevel_PK_SPL=1, 
                       LogNumberOfPulsesx10=0,
                       NumberOfPulses_24hrs=0,
                       SourceLevel_SingleShotSEL=0,
                       distancefromsourcelevelmeasurement_m=0,
                       distancefromsourcelevelmeasurement_m_RMS=0,
                       distancefromsourcelevelmeasurement_m_PK=0,
                       SourceLevel_1meter=0,
                       UnweightedSELcum_atmeasureddistance=0) {
  
  userout.b<-userout.blank0
  forNA <- NA#99999999
  forNA1<-"NA"#"WFA > Source Level"
  
  if (type=="A.1" | type=="B.1" | type=="B.2") {
    #####A.1, B.1, B.2####
    for (i in 1:ncol(userout.b)){
      userout.b[2,i]<-(10^(((SourceLevel_RMS_SPL+
                               weigthtfuncttable["Adjustment (dB)",i]) + 
                              LogDurationOfSoundProductionx10-
                              userout.b[1,i])/Propogation_xLogR) )
    }
  } else if (type=="A1.1"){
    #####A1.1####
    for (i in 1:ncol(userout.b)){
      userout.b[2,i]<-distancefromsourcelevelmeasurement_m*10^(
        ((SourceLevel_RMS_SPL+weigthtfuncttable["Adjustment (dB)",i]) + 
           LogDurationOfSoundProductionx10-userout.b[1,i])/Propogation_xLogR)
    }
  } else if (type=="C.1" | type=="D.1"| type=="D.2") { 
    #####C.1, D.1, D.2#######
    for (i in 1:ncol(userout.b)){
      userout.b[2,i]<-(SourceFactor*pi)/
        (10^((userout.b[1,i] - 
                weigthtfuncttable["Adjustment (dB)",i])/10)*
           SourceVelocity_m_sec)
    }
  } else if (type=="E.1" | type=="E.2") {
    ####E.1, E.2####
    if (type=="E.1") {
      ####E.1####
      for (i in 1:ncol(userout.b)){
        userout.b[2,i]<-10^((SourceLevel_RMS_SPL+
                               LogDurationOfSoundProductionx10+
                               weigthtfuncttable["Adjustment (dB)",i] - userout.b[1,i])/
                              Propogation_xLogR)
        ####forNA###
        if (is.na(SourceLevel_PK_SPL)) {
          userout.b<-userout.blank0
        } else if (SourceLevel_PK_SPL>userout.b[3,i]) {
          userout.b[4,i]<-10^((SourceLevel_PK_SPL-userout.b[3,i])/Propogation_xLogR)
        } else {
          userout.b[4,i]<-forNA
        }
      }
    }
  } else if (type=="E.2") {
    ####E.2####
    for (i in 1:ncol(userout.b)){
      userout.b[2,i]<-10^((SourceLevel_SingleShotSEL+
                             weigthtfuncttable["Adjustment (dB)",i] +
                             LogNumberOfPulsesx10-
                             userout.b[1,i])/Propogation_xLogR)
      if (is.na(SourceLevel_PK_SPL)) {
      userout.b<-userout.blank0
      } else if (SourceLevel_PK_SPL>userout.b[3,i]) {
        ####forNA####
        userout.b[4,i]<-10^((SourceLevel_PK_SPL-userout.b[3,i])/Propogation_xLogR)
      } else {
        userout.b[4,i]<-forNA
      }
    }
    #E1
  } else if (type=="E1.1" | type=="E1.2"){
    ####E1.1, E1.2####
    if (type=="E1.1") {
      #####E1.1#####
      for (i in 1:ncol(userout.b)){
        userout.b[2,i]<-distancefromsourcelevelmeasurement_m_RMS*
          10^(((SourceLevel_RMS_SPL+
                  weigthtfuncttable["Adjustment (dB)",i])+
                 LogDurationOfSoundProductionx10-
                 userout.b[1,i])/Propogation_xLogR)
      }
    } else if (type=="E1.2") {
      ######E1.2######
      for (i in 1:ncol(userout.b)){
        userout.b[2,i]<-distancefromsourcelevelmeasurement_m_RMS*
          10^(((UnweightedSELcum_atmeasureddistance+
                  weigthtfuncttable["Adjustment (dB)",i])-
                 userout.b[1,i])/
                Propogation_xLogR)
      }}
    for (i in 1:ncol(userout.b)){
      ####forNA####
      if (is.na(SourceLevel_1meter)) {
        userout.b<-userout.blank0
      } else if (SourceLevel_1meter>userout.b[3,i]) {
        userout.b[4,i]<-distancefromsourcelevelmeasurement_m_PK*
          10^((SourceLevel_PK_SPL-userout.b[3,i])/
                Propogation_xLogR)
      } else {userout.b[4,i]<-forNA} #sub ifelse
    }
  } else if (type=="E2.1" | type=="E2.2"){
    ####E2.1, E2.2####
    for (j in c(1,5,9)){
      for (i in 1:ncol(userout.b)){
        userout.b[j+1,i]<-10^(((SourceLevel_RMS_SPL+
                                  weigthtfuncttable["Adjustment (dB)",i])+
                                 LogDurationOfSoundProductionx10-
                                 userout.b[j,i])/
                                Propogation_xLogR)
      }}
    for (j in c(3,7)){
      for (i in 1:ncol(userout.b)){
        if (SourceLevel_PK_SPL>userout.b[j,i]) {
          userout.b[j+1,i]<-10^((SourceLevel_PK_SPL-userout.b[j,i])/
                                  Propogation_xLogR)
        } else {
          userout.b[j+1,i]<-forNA} #sub ifelse
      }}
  } else if (type=="E3.1" | type=="E3.2") {
    ######E3.1, E3.2##########
    if (type=="E3.1") {
      ####E3.1#####
      for (j in c(1,5)){
        for (i in 1:ncol(userout.b)){
          userout.b[j+1,i]<-10^((SourceLevel_RMS_SPL+
                                   weigthtfuncttable["Adjustment (dB)",i]+
                                   LogDurationOfSoundProductionx10-
                                   userout.b[j,i])/
                                  Propogation_xLogR)
        }}
    } else if (type=="E3.2") {
      #####E3.2#####
      for (j in c(1,5)){
        for (i in 1:ncol(userout.b)){
          userout.b[j+1,i]<-10^((SourceLevel_RMS_SPL+
                                   weigthtfuncttable["Adjustment (dB)",i]-
                                   userout.b[j,i])/
                                  Propogation_xLogR)
        }}}
    ####forNA####
    for (j in c(3,7)){
      for (i in 1:ncol(userout.b)){
        if (is.na(SourceLevel_PK_SPL)) {
          userout.b<-userout.blank0
        } else if (SourceLevel_PK_SPL>userout.b[j,i]) {
          userout.b[j+1,i]<-10^((SourceLevel_PK_SPL-userout.b[j,i])/
                                  Propogation_xLogR)
        } else {userout.b[j+1,i]<-forNA} #sub ifelse
      }}
  } else if (type=="F.1" | type=="F.2") {
    ####F.1, F.2####
    for (i in 1:ncol(userout.b)){
      userout.b[2,i]<-(SourceFactor*pi)/
        (10^((userout.b[1,i]-
                weigthtfuncttable["Adjustment (dB)",i])/10)*
           SourceVelocity_m_sec)
    }
    for (i in 1:ncol(userout.b)){
      ####forNA####
      if (is.na(SourceLevel_PK_SPL)) {
        userout.b<-userout.blank0
      } else if (SourceLevel_PK_SPL>userout.b[3,i]) {
        userout.b[4,i]<-10^(
          (SourceLevel_PK_SPL-userout.b[3,i])/20)
      } else {userout.b[4,i]<-forNA} #sub ifelse
    }
  } 
  
  ####Make the output nice
  userout.b0<-userout.b
    #as.data.frame(matrix(data = rep_len(NA, nrow(userout.b)*ncol(userout.b)), 
              #                     nrow=nrow(userout.b)))
  
  for (i in seq(from = 2, to = nrow(userout.b), by = 2) ) {
    for (j in 1:ncol(userout.b)){
      userout.b0[i,j]<-HTML(paste0("<b>", decimalplaces(userout.b[i,j], 1), "</b>"))
    }
  }
  userout.b0[which(userout.b0==forNA, arr.ind = T)]<-forNA1

  colnames(userout.b0)<-colnames(userout.b)
  rownames(userout.b0)<-rownames(userout.b)
  
  # userout.b0<-userout.b
  return(as.data.frame(userout.b0))
}



#Select your sound source for analysis
methods.SoundSource<-list("Impact pile drivers"="E1", 
                          "Vibratory pile drivers"="A1",
                          "Down-the-hole drilling/hammering"="A1",
                          "Mobile seismic airguns" = "F",
                          "Stationary seismic airguns (e.g., Vertical seismic profiling)" ="E",
                          "Stationary drilling vessels or platforms" = "A",
                          "Stationary sonar or sonar-like source" = "B",
                          "Mobile sonar or sonar-like source" = "D",
                          # "Multiple underwater detonations" = "E2",
                          # "Single underwater detonation" = "E3", 
                          "Other" = "Other"
)

# If source is not available in dropdown menu, then choose which source category best represents the source: 
methods.SoundCatagory<-list("Non-impulsive , Stationary, Continuous"="A",
                            "Non-impulsive, Stationary, Intermittent"="B",
                            "Non-impulsive, Mobile, Continuous"="C",
                            "Non-impulsive, Mobile, Intermittent"="D",
                            "Impulsive, Stationary"="E", 
                            "Impulsive, Mobile"="F")

# methods.SoundCatagory_hover<-list("NON-IMPULSIVE: Sound sources that can be broadband, narrowband or tonal, brief or prolonged, continuous or intermittent) and typically do not have a high peak sound pressure with rapid rise time that impulsive sounds do. CONTINUOUS: A sound whose sound pressure level remains above ambient sound during the observation period. A key distinction between continuous and intermittent sounds is that intermittent sounds have a more regular (predictable) pattern of bursts of sounds and silent periods (i.e., duty cycle), which continuous sounds do not."
#                                   = "A",
#                                   "NON-IMPULSIVE: Sound sources that can be broadband, narrowband or tonal, brief or prolonged, continuous or intermittent) and typically do not have a high peak sound pressure with rapid rise time that impulsive sounds do. INTERMITTENT: Interrupted levels of low or no sound or bursts of sounds separated by silent periods. Typically, intermittent sounds have a more regular (predictable) pattern of bursts of sounds and silent periods (i.e., duty cycle). "
#                                   ="B",
#                                   "NON-IMPULSIVE: Sound sources that can be broadband, narrowband or tonal, brief or prolonged, continuous or intermittent) and typically do not have a high peak sound pressure with rapid rise time that impulsive sounds do. CONTINUOUS: A sound whose sound pressure level remains above ambient sound during the observation period. A key distinction between continuous and intermittent sounds is that intermittent sounds have a more regular (predictable) pattern of bursts of sounds and silent periods (i.e., duty cycle), which continuous sounds do not."
#                                   ="C",
#                                   "NON-IMPULSIVE: Sound sources that can be broadband, narrowband or tonal, brief or prolonged, continuous or intermittent) and typically do not have a high peak sound pressure with rapid rise time that impulsive sounds do. INTERMITTENT: Interrupted levels of low or no sound or bursts of sounds separated by silent periods. Typically, intermittent sounds have a more regular (predictable) pattern of bursts of sounds and silent periods (i.e., duty cycle). "
#                                   ="D",
#                                   "IMPULSIVE: Sound sources that are typically transient, brief (less than 1 second), broadband, and consist of high peak sound pressure with rapid rise time and rapid decay. They can occur in repetition or as a single event."
#                                   ="E",
#                                   "IMPULSIVE: Sound sources that are typically transient, brief (less than 1 second), broadband, and consist of high peak sound pressure with rapid rise time and rapid decay. They can occur in repetition or as a single event."
#                                   ="F")
# 
# methods.SoundSource_hover<-list("Please refer to the Glossary"="E1",
#                                 "Please refer to the Glossary"="A1",
#                                 "Please refer to the Glossary" = "F",
#                                 "Please refer to the Glossary" ="E",
#                                 "Please refer to the Glossary" = "A",
#                                 "Please refer to the Glossary" = "B",
#                                 "Please refer to the Glossary" = "D",
#                                 "Please refer to the Glossary" = "E2",
#                                 "Please refer to the Glossary" = "E3"
# )

#for stationary sonar, 
# methods.SoundLevelMetrics.hover<-list(
#   "<i>L</i><sub>RMS</sub>: The square root of the average of the square of the pressure of the sound signal over a given duration"=".1", 
#   "SINGLE STRIKE SEL:  Ten times the logarithm to the base 10 of the ration of a given time integral of squared instantaneous sound pressure over a stated time interval or event to the product of the squared reference sound pressure and reference duration of one second. NOTE: this is the SEL associated with a single strike/ping/shot and is not cumulative. Also, this source level metric is preferred because user is not required to provide pulse duration."=".2"
# )

methods.SoundLevelMetrics<-list("LRMS SPL Source Level"=".1", 
                                "Single Ping/Strike/Pulse/Shot SEL"=".2")

methods.BroadNarrow<-list(#"Narrowband source"="Narrow0",
  "Single frequency (WFA)" = "Narrow0", #o	Which correspond to original first two choices
  "Multiple frequencies (Spectrum)" = "Broad0") #o	Which correspond to original last two choices

#########....STRUCTURE INFO FOR EACH METHOD#########
vA.1 <- list("ui1"="Weight Function Adjustment (kHz)", 
             "ui2"=HTML("Source Level (<i>L</i><sub>RMS</sub> SPL)"),
             "ui3"="Duration of Sound Production (hours) within 24-h period",
             "ui4"="Propagation (xLogR)", 
             "ui5"=NA,
             "ui6"=NA, 
             "ui7"=NA, 
             "ui8"=NA,
             "ui9"=NA, 
             "usero"="userout.blank",
             "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab	",
             "txt1"="???Unless otherwise specified, source levels are referenced 1 m from the source. ")

vA1.1 <- list("ui1"="Weight Function Adjustment (kHz)", 
              "ui2"=HTML("Sound Pressure Level (<i>L</i><sub>RMS</sub> SPL), specified at 'x' meters"),
              "ui3"="Number of piles within 24-h period",
              "ui4"="Duration to drive/drill a single pile (minutes)", 
              "ui5"="Propagation (xLogR)",
              "ui6"="Distance of sound pressure level measurement (meters)", 
              "ui7"=NA, 
              "ui8"=NA,
              "ui9"=NA, 
              "usero"="userout.blank",
              "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab	", 
              "txt1"="???Unless otherwise specified, source levels are referenced 1 m from the source."
)

vB.1 <- list("ui1"="Weight Function Adjustment (kHz)", 
             "ui2"=HTML("Source Level (<i>L</i><sub>RMS</sub> SPL)"),
             "ui3"="Activity Duration (hours) within 24-h period",
             "ui4"="Pulse duration (seconds)", 
             "ui5"="1/Repetition rate (seconds)",
             "ui6"="Propagation (xLogR)", 
             "ui7"=NA, 
             "ui8"=NA,
             "ui9"=NA, 
             "usero"="userout.blank",
             "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab ", 
             "txt1"="^Time between onset of successive pulses."
)


vB.2 <- list("ui1"="Weight Function Adjustment (kHz)", 
             "ui2"="Source Level (Single Ping/Pulse SEL)",
             "ui3"="Activity Duration (hours) within 24-h period",
             "ui4"="Number pulses in 1-h period", 
             "ui5"="Propagation (xLogR)",
             "ui6"=NA, 
             "ui7"=NA, 
             "ui8"=NA,
             "ui9"=NA, 
             "usero"="userout.blank",
             "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab ", 
             "txt1"=""
)

vC.1 <- list("ui1"="Weight Function Adjustment (kHz)", 
             "ui2"=HTML("Source Level (<i>L</i><sub>RMS</sub> SPL)"),
             "ui3"="Source Velocity (meters/second)",
             "ui4"=NA, 
             "ui5"=NA,
             "ui6"=NA, 
             "ui7"=NA, 
             "ui8"=NA,
             "ui9"=NA, 
             "usero"="userout.blank",
             "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab ", 
             "txt1"="Methodology assumes propagation of 20 log R; Activity duration (time) independent	"
)
vD.1 <- list("ui1"="Weight Function Adjustment (kHz)", 
             "ui2"=HTML("Source Level (<i>L</i><sub>RMS</sub> SPL)"),
             "ui3"="Source Velocity (meters/second)",
             "ui4"="Pulse Duration (seconds)", 
             "ui5"="1/Repetition rate (seconds)",
             "ui6"=NA, 
             "ui7"=NA, 
             "ui8"=NA,
             "ui9"=NA, 
             "usero"="userout.blank",
             "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab	", 
             "txt1"=paste("???Methodology assumes propagation of 20 log R; Activity duration (time) independent", 
                          "^Time between onset of successive pulses.")
)
vD.2 <- list("ui1"="Weight Function Adjustment (kHz)", 
             "ui2"="Source Level (Single Ping/Pulse SEL)",
             "ui3"="Source Velocity (meters/second)",
             "ui4"="1/Repetition rate (seconds)", 
             "ui5"=NA,
             "ui6"=NA, 
             "ui7"=NA, 
             "ui8"=NA,
             "ui9"=NA, 
             "usero"="userout.blank",
             "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab	", 
             "txt1"=paste("???Methodology assumes propagation of 20 log R; Activity duration (time) independent", 
                          "^Time between onset of successive pulses.")
)
vE.1 <- list("ui1"="Weight Function Adjustment (kHz)", 
             "ui2"=HTML("Source Level (<i>L</i><sub>RMS</sub> SPL)"),
             "ui3"="Activity Duration (hours) within 24-h period",
             "ui4"="Pulse Duration (seconds)", 
             "ui5"="1/Repetition rate (seconds)",
             "ui6"="Propagation (xLogR)", 
             "ui7"="Source Level (PK SPL)", 
             "ui8"=NA,
             "ui9"=NA, 
             "usero"="userout.blank.e",
             "txtw"="", 
             "txt1"=paste("Window that makes up 90% of total cumulative energy (5%-95%) based on Madsen 2005", 
                          "Time between onset of successive pulses.", 
                          "Impulsive sounds have dual metric thresholds (SELcum & PK). Metric producing largest isopleth should be used.")
)
vE.2 <- list("ui1"="Weight Function Adjustment (kHz)", 
             "ui2"="Source Level (Single shot SEL)",
             "ui3"="Activity Duration (hours) within 24-h period",
             "ui4"="Number of pulses in 1-h period", 
             "ui5"=NA,
             "ui6"="Propagation (xLogR)", 
             "ui7"="Source Level (PK SPL)", 
             "ui8"=NA,
             "ui9"=NA, 
             "usero"="userout.blank.e",
             "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab	", 
             "txt1"="*Impulsive sounds have dual metric thresholds (SELcum & PK). Metric producing largest isopleth should be used."
)
vE1.1 <- list("ui1"="Weight Function Adjustment (kHz)", 
              "ui2"=HTML("Sound Pressure Level (<i>L</i><sub>RMS</sub> SPL), specified at 'x' meters"),
              "ui3"="Number of piles per day",
              "ui4"="Strike (Pulse) Duration (seconds)", 
              "ui5"="Number of strikes per pile",
              "ui6"="Propagation (xLogR)", 
              "ui7"=HTML("Distance of sound pressure level measurement (meters) for <i>L</i><sub>RMS</sub> SPL"), 
              "ui8"="PK SPL specified at 'x' meters",
              "ui9"="Distance of sound pressure level measurement (meters)	for PK SPL", 
              "usero"="userout.blank.e",
              "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab	", 
              "txt1"=paste("Window that makes up 90% of total cumulative energy (5%-95%) based on Madsen 2005",
                           "Unless otherwise specified, source levels are referenced 1 m from the source.", 
                           "*Impulsive sounds have dual metric thresholds (SELcum & PK). Metric producing largest isopleth should be used.")
)
vE1.2 <- list("ui1"="Weight Function Adjustment (kHz)", 
              "ui2"="Single Strike SEL specified at 'x' meters",
              "ui3"="Number of strikes per pile",
              "ui4"="Number of piles per day", 
              "ui5"="Propagation (xLogR)",
              "ui6"="Distance of sound pressure level measurement (meters) for single strike SEL", 
              "ui7"="PK SPL specified at 'x' meters", 
              "ui8"="Distance of sound pressure level measurement (meters) for PK SPL",
              "ui9"=NA, 
              "usero"="userout.blank.e",
              "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab	", 
              "txt1"=paste("Unless otherwise specified, source levels are referenced 1 m from the source.", 
                           "*Impulsive sounds have dual metric thresholds (SELcum & PK). Metric producing largest isopleth should be used.")
)
vE2.1 <- list("ui1"="Weight Function Adjustment (kHz)", 
              "ui2"=HTML("Source Level (<i>L</i><sub>RMS</sub> SPL)"),
              "ui3"="Activity Duration (hours) within 24-h period",
              "ui4"="Shot Duration (seconds)",
              "ui5"="Number of detonations in 1 h period", 
              "ui6"="Propagation (xLogR)",
              "ui7"="Source Level (PK SPL)", 
              "ui8"=NA,
              "ui9"=NA, 
              "usero"="userout.blank.e2",
              "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab	", 
              "txt1"=paste("??Window that makes up 90% of total cumulative energy (5%-95%) based on Madsen 2005", 
                           "*Impulsive sounds have dual metric thresholds (SELcum & PK). Metric producing largest isopleth should be used. Additionally, explosives have thresholds associate with lung and g.i. tract injury that need to be considered.")
)
vE2.2 <- list("ui1"="Weight Function Adjustment (kHz)", 
              "ui2"="Source Level (Single shot SEL)",
              "ui3"="Activity Duration (hours) within 24-h period",
              "ui4"="Number of detonations in 1 h period", 
              "ui5"="Propagation (xLogR)",
              "ui6"="Source Level (PK SPL)", 
              "ui7"=NA, 
              "ui8"=NA,
              "ui9"=NA, 
              "usero"="userout.blank.e2",
              "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab	", 
              "txt1"=paste("*Impulsive sounds have dual metric thresholds (SELcum & PK). Metric producing largest isopleth should be used. Additionally, explosives have thresholds associate with lung and g.i. tract injury that need to be considered.")
)
vE3.1 <- list("ui1"="Weight Function Adjustment (kHz)", 
              "ui2"=HTML("Source Level (<i>L</i><sub>RMS</sub> SPL)"),
              "ui3"="Shot Duration (seconds)",
              "ui4"="Propagation (xLogR)",
              "ui5"="Source Level (PK SPL)", 
              "ui6"=NA, 
              "ui7"=NA, 
              "ui8"=NA,
              "ui9"=NA, 
              "usero"="userout.blank.e",
              "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab", 
              "txt1"=paste("Window that makes up 90% of total cumulative energy (5%-95%) based on Madsen 2005", 
                           "*Impulsive sounds have dual metric thresholds (SELcum & PK). Metric producing largest isopleth should be used. Additionally, explosives have thresholds associate with lung and g.i. tract injury that need to be considered."
              )
)
vE3.2 <- list("ui1"="Weight Function Adjustment (kHz)", 
              "ui2"="Source Level (Single shot SEL)",
              "ui3"="Propagation (xLogR)",
              "ui4"="Source Level (PK SPL)",  
              "ui5"=NA,
              "ui6"=NA, 
              "ui7"=NA, 
              "ui8"=NA,
              "ui9"=NA, 
              "usero"="userout.blank.e",
              "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab	", 
              "txt1"="*Impulsive sounds have dual metric thresholds (SELcum & PK). Metric producing largest isopleth should be used. Additionally, explosives have thresholds associate with lung and g.i. tract injury that need to be considered."
)
vF.1 <- list("ui1"="Weight Function Adjustment (kHz)", 
             "ui2"=HTML("Source Level (<i>L</i><sub>RMS</sub> SPL)"),
             "ui3"="Source Velocity (meters/second)",
             "ui4"="Pulse Duration (seconds)", 
             "ui5"="1/Repetition rate (seconds)",
             "ui6"="Source Level (PK SPL)", 
             "ui7"=NA, 
             "ui8"=NA,
             "ui9"=NA, 
             "usero"="userout.blank.e",
             "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab	", 
             "txt1"=paste("???Methodology assumes propagation of 20 log R; Activity duration (time) independent",
                          "??Window that makes up 90% of total cumulative energy (5%-95%) based on Madsen 2005",
                          "^Time between onset of successive pulses.",
                          "*Impulsive sounds have dual metric thresholds (SELcum & PK). Metric producing largest isopleth should be used."
             )
)

vF.2 <- list("ui1"="Weight Function Adjustment (kHz)", 
             "ui2"="Source Level (Single shot/pulse SEL)",
             "ui3"="Source Velocity (meters/second)",
             "ui4"="1/Repetition rate (seconds)", 
             "ui5"="Source Level (PK SPL)",
             "ui6"="", 
             "ui7"="", 
             "ui8"="",
             "ui9"="", 
             "usero"="userout.blank.e",
             "txtw"="Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz); For appropriate default WFA: See INTRODUCTION tab", 
             "txt1"=paste("Methodology assumes propagation of 20 log R; Activity duration (time) independent",
                          "^Time between onset of successive pulses.",
                          "*Impulsive sounds have dual metric thresholds (SELcum & PK). Metric producing largest isopleth should be used."
             )
)

VV<-list(vA.1, vA1.1, vB.1, vB.2, vC.1, vD.1, vD.2, vE.1, vE.2, vE1.1, vE1.2, vE2.1, vE2.2, vE3.1, vE3.2, vF.1, vF.2)
VVn<-c("A.1", "A1.1", "B.1", "B.2", "C.1", "D.1", "D.2", "E.1", "E.2", "E1.1", "E1.2", "E2.1", "E2.2", "E3.1", "E3.2", "F.1", "F.2")


##User Output: Resultant Isopleths###
userout<-data.frame("SEL<sub>cum</sub> Threshold" = c(199, 198, 173, 201, 219), 
                     "<b>PTS Isopleth to threshold (m)</b>" = rep_len(x = NA, length.out = 5))
colnames(userout)<-c("SEL<sub>cum</sub> Threshold", 
                     "<b>PTS Isopleth to threshold (m)</b>")
userout<-t(userout)
colnames(userout)<-c("Low-Frequency Cetaceans", "Mid-Frequency Cetaceans", "High-Frequency Cetaceans", "Phocid Pinnipeds", "Otariid Pinnipeds")
userout.blank1<-as.data.frame(userout)

#userout.blank.e
userout<-data.frame("SEL<sub>cum</sub> Threshold" = c(183, 185, 155, 185, 203),
                    "<b>PTS SEL<sub>cum</sub> Isopleth to Threshold (m)</b>" = rep_len(x = NA, length.out = 5),
                    "PK Threshold"=c(219, 230, 202, 218, 232),
                    "<b>PTS PK Isopleth to Threshold (m)</b>"=rep_len(x = NA, length.out = 5))
colnames(userout)<-c("SEL<sub>cum</sub> Threshold",
                     "<b>PTS SEL<sub>cum</sub> Isopleth to Threshold (m)</b>",
                     "PK Threshold",
                     "<b>PTS PK Isopleth to Threshold (m)</b>")
userout<-t(userout)
colnames(userout)<-c("Low-Frequency Cetaceans", "Mid-Frequency Cetaceans", "High-Frequency Cetaceans", "Phocid Pinnipeds", "Otariid Pinnipeds")
userout.blank.e<-as.data.frame(userout)

#userout.blank.e2
userout<-data.frame("PTS SEL<sub>cum</sub> Threshold" = c(183, 185, 155, 185, 203),
                    "<b>PTS SEL<sub>cum</sub> Isopleth to Threshold (m)</b>" = rep_len(x = NA, length.out = 5),
                    "PTS PK Threshold"=c(219, 230, 202, 218, 232),
                    "<b>PTS PK Isopleth to Threshold (m)</b>"=rep_len(x = NA, length.out = 5),
                    "TTS SEL<sub>cum</sub> Threshold" = c(168, 170, 140, 170, 188),
                    "<b>TTS SEL<sub>cum</sub> Isopleth to Threshold (m)</b>" = rep_len(x = NA, length.out = 5),
                    "TTS PK Threshold"=c(213, 224, 196, 212, 226),
                    "<b>TST PK Isopleth to Threshold (m)</b>"=rep_len(x = NA, length.out = 5),
                    "Behavior SEL<sub>cum</sub> Threshold" = c(163, 165, 135, 165, 183),
                    "<b>Behavior SEL<sub>cum</sub> Isopleth to Threshold (m)</b>" = rep_len(x = NA, length.out = 5) )
colnames(userout)<-c("PTS SEL<sub>cum</sub> Threshold",
                     "<b>TS SEL<sub>cum</sub> Isopleth to Threshold (m)</b>",
                     "PTS PK Threshold",
                     "<b>PTS PK Isopleth to Threshold (m)</b>",
                     "TTS SEL<sub>cum</sub> Threshold",
                     "<b>TTS SEL<sub>cum</sub> Isopleth to Threshold (m)</b>",
                     "TTS PK Threshold",
                     "<b>TST PK Isopleth to Threshold (m)</b>",
                     "Behavior SEL<sub>cum</sub> Threshold",
                     "<b>Behavior SEL<sub>cum</sub> Isopleth to Threshold (m)</b>")
userout<-t(userout)
colnames(userout)<-c("Low-Frequency Cetaceans", "Mid-Frequency Cetaceans", "High-Frequency Cetaceans", "Phocid Pinnipeds", "Otariid Pinnipeds")
userout.blank.e2<-as.data.frame(userout)

#userout.blank.e3
userout<-data.frame("PTS SEL<sub>cum</sub> Threshold" = c(183, 185, 155, 185, 203),
                    "<b>PTS SEL<sub>cum</sub> Isopleth to Threshold (m)</b>" = rep_len(x = NA, length.out = 5),
                    "PTS PK Threshold"=c(219, 230, 202, 218, 232),
                    "<b>PTS PK Isopleth to Threshold (m)</b>"=rep_len(x = NA, length.out = 5),
                    "TTS SEL<sub>cum</sub> Threshold" = c(168, 170, 140, 170, 188),
                    "<b>TTS SEL<sub>cum</sub> Isopleth to Threshold (m)</b>" = rep_len(x = NA, length.out = 5),
                    "TTS PK Threshold"=c(213, 224, 196, 212, 226),
                    "<b>TTS PK Isopleth to Threshold (m)</b>"=rep_len(x = NA, length.out = 5)
)
colnames(userout)<-c("PTS SEL<sub>cum</sub> Threshold",
                     "<b>PTS SEL<sub>cum</sub> Isopleth to Threshold (m)</b>",
                     "PTS PK Threshold",
                     "<b>PTS PK Isopleth to Threshold (m)</b>",
                     "TTS SEL<sub>cum</sub> Threshold",
                     "<b>TTS SEL<sub>cum</sub> Isopleth to Threshold (m)</b>",
                     "TTS PK Threshold",
                     "<b>TTS PK Isopleth to Threshold (m)</b>")
userout<-t(userout)
colnames(userout)<-c("Low-Frequency Cetaceans", "Mid-Frequency Cetaceans", "High-Frequency Cetaceans", "Phocid Pinnipeds", "Otariid Pinnipeds")
userout.blank.e3<-as.data.frame(userout)

####SHINYAPP()#####
ui <- fluidPage(
  #########***HEADER#########
  useShinyjs(),
  extendShinyjs(text = jsCode1),
  theme = shinytheme("flatly"),
  # shinythemes::themeSelector(),

  titlePanel(
    windowTitle = "Marine Mammal Acoustic Technical Guidance (Web App) | NOAA Fisheries",
    title = tags$head(tags$link(rel="shortcut icon", 
                                href="https://www.noaa.gov/sites/all/themes/custom/noaa/favicon.ico", 
                                type="image/vnd.microsoft.icon"))), 
  navbarPage(
    title = imageOutput(outputId = "Image",width=1000/25,height=1000/25,
                        hover = "National Oceanic and Atmospheric Administration (NOAA)"),  #https://rdrr.io/cran/shiny/man/renderImage.html

# ui <- navbarPage( #fluidPage(
#
#   theme = shinytheme("flatly"),
  # shinythemes::themeSelector(),


  # ),

  # tabsetPanel(id = "panels", ###https://stackoverflow.com/questions/34315485/linking-to-a-tab-or-panel-of-a-shiny-app ###How to insert links to pages in the framework

  # navbarPage(#https://stackoverflow.com/questions/24705431/how-can-i-insert-an-image-into-the-navbar-on-a-shiny-navbarpage
  # title = imageOutput(outputId = "Image",width=1000/25,height=1000/25,
  #                     hover = "National Oceanic and Atmospheric Administration (NOAA)"),  #https://rdrr.io/cran/shiny/man/renderImage.html
  # router_ui(),
  # tabPanel(title = tags$ul(a(class = "item", href = route_link("Introduction"), "Introduction"))),
  # tabPanel(title = tags$ul(a(class = "item", href = route_link("Calculator"), "Optional Calculator"))),
  # tabPanel(title = tags$ul(a(class = "item", href = route_link("WFA"), "Weight Factor Adjustments (WFA)"))),
  # tabPanel(title = tags$ul(a(class = "item", href = route_link("Gloss"), "Glossary and Literature Cited")))
############PAGES###########
#######***TAB CALCULATOR#################
# Calculator0<-
  tabPanel("Calculator",
                      column(2,
                             # tabPanel("PROJECT INFORMATION",
                             wellPanel(
                               #######***---STEP 1########
                               h4(strong("Step 1: PROJECT INFORMATION"),
                                  tags$style(type = "text/css", "#q_step1 {vertical-align: top;}"),
                                  bsButton("q_step1", label = "", icon = icon("question"), style = "info", size = "extra-small")
                               ),

                               bsPopover(id = "q_step1", title = "Project Information",
                                         content = paste0("If the user needs more room to enter their responce, they may expand the extents of the text boxes by dragging the icon in the lower right corner of the box."
                                         ),
                                         placement = "right",
                                         trigger = "focus",
                                         options = list(container = "body")
                               ),

                               textAreaInput(inputId = "Client",
                                             label = "Project Title",
                                             value = "", rows = 5),

                               textAreaInput(inputId = "ProjectName",
                                             label = "Project Contact",
                                             value = "", rows = 5),

                               textAreaInput(inputId = "ProjectDescription",
                                             label = "Project/Source Information (Including Assumptions)",
                                             value = "", rows = 10)
                             )),
                      #######***---STEP 2########
                      column(2,
                             wellPanel(
                               h4(strong("Step 2: SOUND SOURCE AND SOUND METRIC")),

                               h5(strong("Sound Source"),
                                  tags$style(type = "text/css", "#q_step2a {vertical-align: top;}"),
                                  bsButton("q_step2a", label = "", icon = icon("question"), style = "info", size = "extra-small")
                               ),
                               bsPopover(id = "q_step2a", title = "Sound Source/Category",
                                         content = paste0("By clicking <i>Other</i> a new dropdown menu will appear with sound categories so you may choose the category of your sound."
                                         ),
                                         placement = "right",
                                         trigger = "focus",
                                         options = list(container = "body")
                               ),


                               selectInput(inputId = "SoundSource", label="",
                                           choices=methods.SoundSource, selected = "A"),
                               uiOutput("SoundCatagory"),
                               h5(strong("Source Level Metric for Calculating Cumulative Sound Exposure Level"),
                                  tags$style(type = "text/css", "#q_step2b {vertical-align: top;}"),
                                  bsButton("q_step2b", label = "", icon = icon("question"), style = "info", size = "extra-small")
                               ),
                               bsPopover(id = "q_step2b", title = "Source Level Metric for Calculating Cumulative Sound Exposure Level",
                                         content = paste0("For impulsive sound sources, the peak sound pressure level source level is also needed (Step 3)"),
                                         placement = "right",
                                         trigger = "focus",
                                         options = list(container = "body")
                               ),
                               uiOutput("SoundLevelMetrics")
                               # uiOutput("SoundMetric_hover")

                             ),
                             #######***---STEP 3########
                             wellPanel(
                               h4(strong("Step 3: INCORPORATING AUDITORY WEIGHTING FUNCTIONS"),
                                  tags$style(type = "text/css", "#q_step3 {vertical-align: top;}"),
                                  bsButton("q_step3", label = "", icon = icon("question"),
                                           style = "info", size = "extra-small")),

                               bsPopover(id = "q_step3", title = "Incorporating Auditory Weighting Functions",
                                         content = paste0("Additional information associated with weighting  (i.e., user should provide additional information to support previous choice). For example, if able to provide 95% frequency contour or relying upon the source spectrum, the user should provide documentation supporting this decision."
                                         ),
                                         placement = "right",
                                         trigger = "focus",
                                         options = list(container = "body")),

                               # uiOutput("howtoweight_band"),
                               # uiOutput("howtoweight_band_hover"),
                               uiOutput("methods.BroadNarrow")
                               # uiOutput("methods.BroadNarrow_hover"),

                             )
                      ),
                      #######***---STEP 4########
                      column(2,
                             wellPanel(
                               h4(strong("Step 4: THRESHOLD CALCULATION INPUTS"),
                                  tags$style(type = "text/css", "#q_step4 {vertical-align: top;}"),
                                  bsButton("q_step4", label = "", icon = icon("question"), style = "info", size = "extra-small")
                               ),

                               bsPopover(id = "q_step4", title = "Threshold Calculation Inputs",
                                         content = paste0("Missing or incorrect values will be highlighted in orange. Once a acceptable value has been entered, the box field will no longer be highlighted. The range of acceptable values are noted by clicking and hovering over the input box. "
                                         ),
                                         placement = "right",
                                         trigger = "focus",
                                         options = list(container = "body")
                               ),
                               uiOutput("ui2"),
                               uiOutput("ui3"),
                               uiOutput("ui4"),
                               uiOutput("ui5"),
                               uiOutput("ui6"),
                               uiOutput("ui7"),
                               uiOutput("ui8"),
                               uiOutput("ui9")#,
                             ),
                             #######***---STEP 5########
                             wellPanel(
                               h4(strong("Step 5: WEIGHTING FUNCTION PARAMETERS"),
                                  tags$style(type = "text/css", "#q_step5 {vertical-align: top;}"),
                                  bsButton("q_step5", label = "", icon = icon("question"), style = "info", size = "extra-small")
                               ),

                               bsPopover(id = "q_step5", title = "Weighting Function Parameters",
                                         content = paste0("Missing or incorrect values will be highlighted in orange. Once a acceptable value has been entered, the box field will no longer be highlighted. The range of acceptable values are noted by clicking and hovering over the input box. If using <i>multiple frequencies</i> and inputing individual adjustment values, the user does not have to enter a value for each population to recieve an output."
                                         ),
                                         placement = "right",
                                         trigger = "focus",
                                         options = list(container = "body")),
                               uiOutput("ui1wtitle"),
                               uiOutput("ui1w1"),
                               uiOutput("ui1w2"),
                               uiOutput("ui1w3"),
                               uiOutput("ui1w4"),
                               uiOutput("ui1w5"),
                               uiOutput("ui1")
                             )),

                      #######***---RESULTS########
                      column(6,
                             h2("Results"),
                             wellPanel(style = "background-color: #c8e6f4; overflow-x:scroll", #https://www.google.com/search?q=color+%23ffffff&rlz=1C1PRFI_enUS731US731&oq=color+%23ffffff&aqs=chrome..69i57.3855j1j7&sourceid=chrome&ie=UTF-8
                                       h4("WEIGHTING FUNCTION ADJUSTMENTS (dB)"),
                                       # htmlOutput("weight3"),
                                       dataTableOutput(outputId = "weight4"),
                                       uiOutput("weight5warning")
                             ),

                             wellPanel(style = "background-color: #c8e6f4; overflow-x:scroll", #https://www.google.com/search?q=color+%23ffffff&rlz=1C1PRFI_enUS731US731&oq=color+%23ffffff&aqs=chrome..69i57.3855j1j7&sourceid=chrome&ie=UTF-8
                                       h4("THRESHOLD ISOPLETHS RESULTS"),
                                       p("Underwater Acoustic Thresholds"),
                                       dataTableOutput(outputId = "text_calc"),
                                       uiOutput("weight4warning"),

                                       # useShinyjs(),
                                       # extendShinyjs(text = jsCode),
                                       # actionButton("print", "PRINT"),

                                       ###R MARKDOWN REPORT
                                       downloadButton("report", "Generate Report"), 
                                       downloadButton("downloadData", "Download CSV File")

                             )
                      )
),
########***TAB INTRODUCTION################
# Introduction0 <-
tabPanel(title = "Introduction",
         column(12,  wellPanel(
           imageOutput(outputId = "ImageFull",width=600,height=144),
           h2("Optional Web Calculator Tool 2018 Revision (Version 1.0) to:"),
           h1(HTML("Technical Guidance For Assessing the Effects of Anthropogenic Sound on Marine Mammal Hearing")),
           h2(HTML("<i>NOAA Technical Memorandum NMFS-OPR-59</i>"))
         ),
         wellPanel(
           h3("Introduction"),
           p(HTML("NOAA's National Marine Fisheries Service (NMFS) recognizes that the permanent threshold shift (PTS) onset thresholds and marine mammal auditory weighting functions provided in the 2018 Revised Technical Guidance for Assessing the Effects of Anthropogenic Sound on Marine Mammal Hearing are more complex than NMFS' previous thresholds and that different action proponents may have different levels of modeling capabilities. Thus, NMFS has provided a companion optional Web Calculator tool for those action proponents unable to implement the 2018 Revised Technical Guidance's thresholds in the weighted cumulative sound exposure level (SEL<sub>cum</sub>) and peak sound pressure (PK) level metrics and the associated marine mammal auditory weighting functions.")
           ),
           br(),
           p(HTML("There is <i>no obligation</i> to use the <i>optional</i> Web Calculator tool. The use of more sophisticated exposure modeling or consideration of additional activity‐, source-, or location‐specific factors, if possible, is encouraged.")
           )
         ), wellPanel(
           h3("Using the Optional Web Calculator Tool"),
           br(),
           p("The optional Web Calculator Tool consists of four main Tabs provided as a ribbon at the top of this and all tabs:"
           ),

           # tags$ul(
           #   tags$li(a(class = "item", href = route_link("Introduction"), "Introduction"),
           #           HTML(": Current Tab that provides a general introduction to the optional Web Calculator tool")),
           #   tags$li(a(class = "item", href = route_link("Calculator"), "Optional Calculator"),
           #           HTML(": Tab where action proponent enters project and source information to compute threshold isopleths (meters)")),
           #   tags$li(a(class = "item", href = route_link("WFA"), "Weight Factor Adjustments (WFA)"),
           #           HTML(": Tab that provides more information on using Weighting Factor Adjustments (WFA) used to incorporate weighting functions* in to isopleth calculations.")),
           #   tags$li(a(class = "item", href = route_link("Gloss"), "Glossary and Literature Cited"),
           #           HTML(": Tab that provides a list of abbreviations and glossary terms found in the optional Web Calculator Tool. It also provides a list of literature cited."))
           # ),

           tags$ul(
             tags$li(HTML("<b>Introduction</b>: Current Tab that provides a general introduction to the optional Web Calculator tool")),
             tags$li(HTML("<b>Optional Calculator</b>: Tab where action proponent enters project and source information to compute threshold isopleths (meters)")),
             tags$li(HTML("<b>Weight Factor Adjustments (WFA)</b>: Tab that provides more information on using Weighting Factor Adjustments (WFA) used to incorporate weighting functions* in to isopleth calculations.")),
             tags$li(HTML("<b>Glossary and Literature Cited</b>: Tab that provides a list of abbreviations and glossary terms found in the optional Web Calculator Tool. It also provides a list of literature cited."))
           )
         ), wellPanel(
           h3("Marine Mammal Hearing Groups"),
           dataTableOutput(outputId = "marinemammalhearinggroups"),
           br(),
           em(HTML("*WFAs consider marine mammal auditory weighting functions by focusing on a single frequency for those who cannot fully apply auditory weighting functions associated with the SEL<sub>cum</sub> metric thresholds.")
           )
         ), wellPanel(
           h3("NMFS also provides a User Manual for the optional Web Calculator Tool (see links below)."),
           p(a("NOAA Technical Guidance Web Site",
               href="https://www.fisheries.noaa.gov/national/marine-mammal-protection/marine-mammal-acoustic-technical-guidance",
               target="_blank")),
           p(a("2018 Revision to Technical Guidance for Assessing the Effects of Anthropogenic Sound on Marine Mammal Hearing",
               href="https://www.fisheries.noaa.gov/webdam/download/75962998",
               target="_blank")),
           p(a("User Manual for Web Calculator (optional)",
               href="https://www.fisheries.noaa.gov/webdam/download/75963103",
               target="_blank")),
           p(a("User Excel Spreadsheet",
               href="https://www.fisheries.noaa.gov/webdam/download/75963097",
               target="_blank"))

         ), wellPanel(
           h3("Note:"),
           p("This Web Calculator provides a means to estimates distances associated with the Technical Guidance's PTS onset thresholds. Mitigation and monitoring requirements associated with a Marine Mammal Protection Act (MMPA) authorization or an Endangered Species Act (ESA) consultation or permit are independent management decisions made in the context of the proposed activity and comprehensive effects analysis, and are beyond the scope of the Technical Guidance and the Web Calculator."
           ),
           h3("Disclaimer:"),
           p("NMFS has provided this Web Calculator as an optional tool to provide estimated effect distances (i.e., isopleths) where PTS onset thresholds may be exceeded. Results provided by this calculator do not represent the entirety of the comprehensive effects analysis, but rather serve as one tool to help evaluate the effects of a proposed action on marine mammal hearing and make findings required by NOAA's various statutes. Input values are the responsibility of the individual user."),
           br(),

           em("For any comments or questions please contact amy.scholik@noaa.gov.")
         )
         )
         ),

###***TAB WEIGHT####
# WFA0<-
  tabPanel("Weight Factor Adjustments (WFA)",
               column(12, wellPanel(
                 h2("Suggested Broadband WFAs"),
                 br(),
                 p("Table 1: Suggested (default*) weighting factor adjustments (WFA). NMFS acknowledges default WFAs are likely conservative. *Limited data on spectra associated with down-the-hole drilling/hammering exists. Thus, the WFA for vibratory pile driving is recommended as a surrogate for this source."),
                 dataTableOutput("weight5"),
                 br(),
                 p("* NMFS acknowledges default WFAs are likely conservative. ")
               )
               )
           ),

######***TAB GLOSSARY AND LITERATUre CITED########
# Gloss0<-
  tabPanel("Glossary and Literature Cited",
                 h1("Abbreviations, Acronyms, Symbols, and Glossary"),
                 column(3, wellPanel(

                   h3("Abbreviations"),
                   dataTableOutput("acronyms")
                 )),
                 column(9, wellPanel(

                   h3("Glossary"),
                   dataTableOutput("gloss")
                 )),

                 column(12, wellPanel(
                   h3("Literature Cited"),
                   tableOutput("infot2")
                 ))
)
)
  # tags$footer(div(imageOutput(outputId = "Image",width=1000/15,height=1000/15),
  #                 HTML("NOAA Fisheries Technical Guidance For Assessing the Effects of Anthropogenic Sound on Marine Mammal Hearing (NOAA Technical Memorandum NMFS-OPR-59), <i>V1.0, December 2018</i>")),
  #             align = "center", style = "
  #             width:100%;
  #             height:125px;   /* Height of the footer */
  #             color: white;
  #             padding: 10px;
  #             background-color: #2C3E50;
  #             z-index: 1000;"
  # )
)

server <- function(input, output, session) {
  # router(input, output, session)
  
  ########*** HEADER#############
  output$ImageFull <- renderImage({
    filename <- normalizePath(file.path("./www/noaa_fisheries_small.png"))
    list(src = filename, 
         width = session$clientData$output_ImageFull_width,
         height = session$clientData$output_ImageFull_height
    )
  }, deleteFile = FALSE)
  
  output$Image <- renderImage({
    filename <- normalizePath(file.path("./www/noaa_logo.gif"))
    list(src = filename, 
         width = session$clientData$output_Image_width,
         height = session$clientData$output_Image_height
    )
  }, deleteFile = FALSE)
  
  ######***TAB INTRODUCTION##########
  #https://stackoverflow.com/questions/42047422/create-url-hyperlink-in-r-shiny
  # a("https://www.fisheries.noaa.gov/national/marine-mammal-protection/marine-mammal-acoustic-technical-guidance")
  
  output$infot3 <- renderTable({
    infot30<-data.frame("Original"=c(1.0, 1.1), 
                        "Version"=c(1.1, 2.0),
                        "Updated" = c("Sheet A, error with formula for phocid pinniped", 
                                      "Corresponds to 2.0 version of Revised Technical Guidance (2018). Added sheet specific to vibratory pile driving and explosives and added capabilities to calculate peak sound pressure level isopleths for impulsive sources"), 
                        "Posted"	= c("Aug. 22, 2016", "2018")
    )
    colnames(infot30)<-c("Original", "Version", "Updated", "Date Posted")
    return(infot30)
  })
  
  output$marinemammalhearinggroups <- renderDataTable({
    MarineMammalHearingGroup1<-matrix(data = c("<b>Low-Frequency (LF) Cetaceans</b>: Baleen whales", 
                                               "<b>Mid-Frequency (MF) Cetaceans</b>: Dolphins, toothed whales, beaked whales, bottlenose whales",
                                               "<b>High-Frequency (HF) Cetaceans</b>: True porpoises, Kogia, river dolphins, <em>cephalorhynchid</em>, <em>Lagenorhynchus cruciger</em>, and <em>L. australis</em>", 
                                               "<b>Phocid Pinnipeds (PW)</b>: True seals",
                                               "<b>Otariid Pinnipeds (OW)</b>: Sea lions and fur seals"), 
                                      nrow = 5, ncol = 1, byrow = T)
    colnames(MarineMammalHearingGroup1)<-"Hearing Groups"
    return(datatable(MarineMammalHearingGroup1, 
                     rownames = F, 
                     # caption = 'Table 1: Within the optional Web Calculator Tool, marine mammal hearing groups are defined as follows:',
                     escape = FALSE, 
                     options = list(pageLength = 26, dom = 'tip', dom='t',ordering=F, paging=F)))
  })  
  
  ####***TAB CACLULATION####
  ####***---STEP 1####
  ####***---STEP 2####
  output$SoundCatagory <- renderUI({
    if (input$SoundSource == "Other"){
      selectInput(inputId = "SoundCatagory", label = h5("Other: Please Choose Sound Catagory Instead"), 
                  choices=methods.SoundCatagory, selected = "A")
    }  
  })
  
  output$SoundLevelMetrics <- renderUI({
    if ((SoundEquation(input$SoundSource, input$SoundCatagory) == 'B' | 
         SoundEquation(input$SoundSource, input$SoundCatagory) == 'D' | 
         SoundEquation(input$SoundSource, input$SoundCatagory) == 'E'  |
         SoundEquation(input$SoundSource, input$SoundCatagory) == 'E1'  | 
         SoundEquation(input$SoundSource, input$SoundCatagory) == 'E2' | 
         SoundEquation(input$SoundSource, input$SoundCatagory) == 'E3' |
         SoundEquation(input$SoundSource, input$SoundCatagory) == 'F') && 
        (!is.null(SoundEquation(input$SoundSource, input$SoundCatagory)))) {
      selectInput(inputId = "SoundLevelMetrics", label="",#h5("Source Level Metric for Calculating Cumulative Sound Exposure Level"),
                  choices=methods.SoundLevelMetrics, selected = ".1")
    } else {
      selectInput(inputId = "SoundLevelMetrics", label="",#h5("Source Level Metric for Calculating Cumulative Sound Exposure Level"),
                  choices=methods.SoundLevelMetrics[1], selected = ".1")
    }
  })
  
  ####***---STEP 3####
  #Based on the previous choices, to define broad/narrowband
  
  output$methods.BroadNarrow <- renderUI({
    if (input$SoundSource != "Other" &
        substr(x = SoundEquation(input$SoundSource, input$SoundCatagory), start = 1, stop = 1)=="B" |
        substr(x = SoundEquation(input$SoundSource, input$SoundCatagory), start = 1, stop = 1)=="D") {
      selectInput(inputId = "methods.BroadNarrow", label="Only Narrowband Sources Apply",
                  choices=methods.BroadNarrow[1], selected = "Narrow0")
    } else if (input$SoundSource != "Other" &
               # 12.	For the following sources/source categories, you should not be able to choose “Narrowband” under “Incorporating Auditory Weighting Functions”:
               substr(x = SoundEquation(input$SoundSource, input$SoundCatagory), start = 1, stop = 1)!="B" |
               substr(x = SoundEquation(input$SoundSource, input$SoundCatagory), start = 1, stop = 1)!="D") {
      selectInput(inputId = "methods.BroadNarrow", label="Is the Source Broad or Narrowband?",
                  choices=methods.BroadNarrow, selected = "Narrow0")
      
    } else if (input$SoundSource == "Other") { #if other is selected
      selectInput(inputId = "methods.BroadNarrow", label="Is the Source Broad or Narrowband?",
                  choices=methods.BroadNarrow, selected = "Narrow0")
    }
  })
  
  ####***---STEP 4: Single Frequecny####
  ########***------ui1######
  output$ui1 <- renderUI({
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    if  (input$methods.BroadNarrow=="Narrow0") {
      if (is.null(select_AA))
        return()
      
      V<-VV[which(VVn==select_AA)]; V<-V[[1]]
      v0<-"ui1"
      V1<-V[v0]
      switch(select_AA,
             "A.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "A1.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "B.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "B.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "C.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "D.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "D.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "E.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "E.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "E1.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "E1.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "E2.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "E2.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "E3.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "E3.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "F.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
             "F.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1))
      ) 
    }  else if (input$methods.BroadNarrow=="Broad0") {
      if (is.null(select_AA))
        return()
    }
  })
  
  observeEvent(input$ui1, { #Validates Data Entry by changing box color when incorrect value has been entered
    x <- input$ui1
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui1"
    V1<-V[v0]
    if (x >= MaxBoundary(V1) | x <= MinBoundary(V1) | is.na(x))  {
      js$backgroundCol(v0,"coral")
    } else {
      js$backgroundCol(v0,"")
    }
  })
  
  ####***---STEP 4: Multiple Frequecny####
  output$ui1wtitle <- renderUI({ 
    if (input$methods.BroadNarrow=="Broad0") {
      HTML(paste("<b>Adjustment (dB)</b>", sep = '<br/>'))
    }
  })
  
  ########***------ui1w1######
  output$ui1w1 <- renderUI({
    V1<-"Low-Frequency Cetaceans"
    if (is.null(input$methods.BroadNarrow))
      return()
    switch(input$methods.BroadNarrow,
           "Broad0" = numericInput("ui1w1", 
                                   HTML(paste0("<i>",V1,"</i>")), 
                                   value = NULL, min = MinBoundary.mult(V1), max = MaxBoundary.mult(V1))
    )
  })
  
  observeEvent(input$ui1w1, { #Validates Data Entry by changing box color when incorrect value has been entered
    V1<-"Low-Frequency Cetaceans"
    x <- input$ui1w1
    v0<-"ui1w1"
    if (x >= MaxBoundary.mult(V1) | x <= MinBoundary.mult(V1) | is.na(x))  {
      js$backgroundCol(v0,"coral")
    } else {
      js$backgroundCol(v0,"")
    }
  })
  
  ########***------ui1w2######
  output$ui1w2 <- renderUI({
    V1<-"Mid-Frequency Cetaceans"
    if (is.null(input$methods.BroadNarrow))
      return()
    switch(input$methods.BroadNarrow,
           "Broad0" = numericInput("ui1w2", 
                                   HTML(paste0("<i>",V1,"</i>")), 
                                   value = NULL, min = MinBoundary.mult(V1), max = MaxBoundary.mult(V1))    )
  })
  
  observeEvent(input$ui1w2, { #Validates Data Entry by changing box color when incorrect value has been entered
    V1<-"Mid-Frequency Cetaceans"
    x <- input$ui1w2
    v0<-"ui1w2"
    if (x >= MaxBoundary.mult(V1) | x <= MinBoundary.mult(V1) | is.na(x))  {
      js$backgroundCol(v0,"coral")
    } else {
      js$backgroundCol(v0,"")
    }
  })
  
  ########***------ui1w3######
  output$ui1w3 <- renderUI({
    V1<-"High-Frequency Cetaceans"
    if (is.null(input$methods.BroadNarrow))
      return()
    switch(input$methods.BroadNarrow,
           "Broad0" = numericInput("ui1w3",
                                   HTML(paste0("<i>",V1,"</i>")), 
                                   value = NULL, min = MinBoundary.mult(V1), max = MaxBoundary.mult(V1))    )
  })
  
  observeEvent(input$ui1w3, { #Validates Data Entry by changing box color when incorrect value has been entered
    V1<-"High-Frequency Cetaceans"
    x <- input$ui1w3
    v0<-"ui1w3"
    if (x >= MaxBoundary.mult(V1) | x <= MinBoundary.mult(V1) | is.na(x))  {
      js$backgroundCol(v0,"coral")
    } else {
      js$backgroundCol(v0,"")
    }
  })
  
  ########***------ui1w4######
  output$ui1w4 <- renderUI({
    V1<-"Phocid Pinnipeds"
    if (is.null(input$methods.BroadNarrow))
      return()
    switch(input$methods.BroadNarrow,
           "Broad0" = numericInput("ui1w4",
                                   HTML(paste0("<i>",V1,"</i>")), 
                                   value = NULL, min = MinBoundary.mult(V1), max = MaxBoundary.mult(V1))    )
  })
  
  observeEvent(input$ui1w4, { #Validates Data Entry by changing box color when incorrect value has been entered
    V1<-"Phocid Pinnipeds"
    x <- input$ui1w4
    v0<-"ui1w4"
    if (x >= MaxBoundary.mult(V1) | x <= MinBoundary.mult(V1) | is.na(x))  {
      js$backgroundCol(v0,"coral")
    } else {
      js$backgroundCol(v0,"")
    }
  })
  
  ########***------ui1w5######
  output$ui1w5 <- renderUI({
    V1<-"Otariid Pinnipeds"
    if (is.null(input$methods.BroadNarrow))
      return()
    switch(input$methods.BroadNarrow,
           "Broad0" = numericInput("ui1w5",
                                   HTML(paste0("<i>",V1,"</i>")), 
                                   value = NULL, min = MinBoundary.mult(V1), max = MaxBoundary.mult(V1))    )
  })
  
  observeEvent(input$ui1w5, { #Validates Data Entry by changing box color when incorrect value has been entered
    V1<-"Otariid Pinnipeds"
    x <- input$ui1w5
    v0<-"ui1w5"
    if (x >= MaxBoundary.mult(V1) | x <= MinBoundary.mult(V1) | is.na(x))  {
      js$backgroundCol(v0,"coral")
    } else {
      js$backgroundCol(v0,"")
    }
  })
  
  
  # output$ui1_info <- renderUI({
  #   # h4(V1,
  #   #    tags$style(type = "text/css", "#q5 {vertical-align: top;}"),
  #   #    bsButton("q5", label = "", icon = icon("question"),
  #   #             style = "info", size = "extra-small")),
  #   bsPopover(id = "q5", title = "WFA",
  #             content = paste0("Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz)."),
  #             placement = "right",
  #             trigger = "focus",
  #             options = list(container = "body"))
  # })
  
  # output$ui1_info <- renderUI({
  #   bsPopover(id = "q5", title = "WFA",
  #             content = "Broadband: 95% frequency contour percentile (kHz) OR Narrowband: frequency (kHz).",
  #             placement = "right",
  #             trigger = "focus",
  #             options = list(container = "body")
  #   )
  # })
  
  ########***---STEP 5##############
  ########***------ui2######
  output$ui2 <- renderUI({
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui2"
    V1<-V[v0]
    switch(select_AA,
           "A.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "A1.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "B.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "B.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "C.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "D.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "D.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E2.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E2.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E3.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E3.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "F.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "F.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1))
    ) 
  })
  
  observeEvent(input$ui2, { #Validates Data Entry by changing box color when incorrect value has been entered
    x <- input$ui2
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui2"
    V1<-V[v0]
    if (x >= MaxBoundary(V1) | x <= MinBoundary(V1) | is.na(x))  {
      js$backgroundCol(v0,"coral")
    } else {
      js$backgroundCol(v0,"")
    }
  })
  
  ########***------ui3######
  output$ui3 <- renderUI({
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui3"
    V1<-V[v0]
    switch(select_AA,
           "A.1"    = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "A1.1"   = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "B.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "B.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "C.1"    = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "D.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "D.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E2.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E2.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E3.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E3.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "F.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "F.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1))
    )
  })
  
  observeEvent(input$ui3, { #Validates Data Entry by changing box color when incorrect value has been entered
    x <- input$ui3
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui3"
    V1<-V[v0]
    if (x >= MaxBoundary(V1) | x <= MinBoundary(V1) | is.na(x))  {
      js$backgroundCol(v0,"coral")
    } else {
      js$backgroundCol(v0,"")
    }
  })
  
  ########***------ui4######
  output$ui4 <- renderUI({
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    
    v0<-"ui4"
    V1<-V[v0]
    switch(select_AA,
           "A.1"    = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "A1.1"   = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "B.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "B.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "C.1"    = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "D.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "D.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E2.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E2.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E3.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E3.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "F.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "F.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1))
    )
  })
  
  observeEvent(input$ui4, { #Validates Data Entry by changing box color when incorrect value has been entered
    x <- input$ui4
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui4"
    V1<-V[v0]
    if (x >= MaxBoundary(V1) | x <= MinBoundary(V1) | is.na(x))  {
      js$backgroundCol(v0,"coral")
    } else {
      js$backgroundCol(v0,"")
    }
  })
  
  ########***------ui5######
  output$ui5 <- renderUI({
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui5"
    V1<-V[v0]
    switch(select_AA,
           # "A.1"    = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "A1.1"   = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "B.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "B.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "C.1"    = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "D.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "D.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E2.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E2.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E3.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E3.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "F.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "F.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1))
    )
  })
  
  
  observeEvent(input$ui5, { #Validates Data Entry by changing box color when incorrect value has been entered
    x <- input$ui5
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui5"
    V1<-V[v0]
    
    if (x >= MaxBoundary(V1) | x <= MinBoundary(V1) | is.na(x))  {
      js$backgroundCol(v0,"coral")
    } else {
      js$backgroundCol(v0,"")
    }
  })
  
  ########***------ui6######
  output$ui6 <- renderUI({
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui6"
    V1<-V[v0]
    switch(select_AA,
           # "A.1"    = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "A1.1"   = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "B.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "B.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "C.1"    = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "D.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "D.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E2.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E2.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E3.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E3.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "F.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1))#,
           # "F.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1))
    )
  })
  
  
  observeEvent(input$ui6, { #Validates Data Entry by changing box color when incorrect value has been entered
    x <- input$ui6
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui6"
    V1<-V[v0]
    if (x >= MaxBoundary(V1) | x <= MinBoundary(V1) | is.na(x))  {
      js$backgroundCol(v0,"coral")
    } else {
      js$backgroundCol(v0,"")
    }
  })
  
  ########***------ui7######
  output$ui7 <- renderUI({
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui7"
    V1<-V[v0]
    switch(select_AA,
           # "A.1"    = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "A1.1"   = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "B.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "B.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "C.1"    = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "D.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "D.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E2.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1))#,
           # "E2.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E3.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E3.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "F.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "F.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1))
    )
  })
  
  
  observeEvent(input$ui7, { #Validates Data Entry by changing box color when incorrect value has been entered
    x <- input$ui7
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui7"
    V1<-V[v0]
    if (x >= MaxBoundary(V1) | x <= MinBoundary(V1) | is.na(x))  {
      js$backgroundCol(v0,"coral")
    } else {
      js$backgroundCol(v0,"")
    }
  })
  
  ########***------ui8######
  output$ui8 <- renderUI({
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui8"
    V1<-V[v0]
    switch(select_AA,
           # "A.1"    = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "A1.1"   = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "B.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "B.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "C.1"    = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "D.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "D.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1))#,
           # "E2.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E2.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E3.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E3.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "F.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "F.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1))
    )
  })
  
  observeEvent(input$ui8, { #Validates Data Entry by changing box color when incorrect value has been entered
    x <- input$ui8
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui8"
    V1<-V[v0]
    if (x >= MaxBoundary(V1) | x <= MinBoundary(V1) | is.na(x))  {
      js$backgroundCol(v0,"coral")
    } else {
      js$backgroundCol(v0,"")
    }
  })
  
  ########***------ui9######
  output$ui9<- renderUI({
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    
    v0<-"ui9"
    V1<-V[v0]
    switch(select_AA,
           # "A.1"    = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "A1.1"   = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "B.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "B.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "C.1"    = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "D.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "D.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           "E1.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1))#,
           # "E1.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E2.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E2.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E3.1" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "E3.2" = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "F.1"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1)),
           # "F.2"  = numericInput(inputId = v0, label = V1, value = NULL, min = MinBoundary(V1), max = MaxBoundary(V1))
    )
  })
  
  
  observeEvent(input$ui9, { #Validates Data Entry by changing box color when incorrect value has been entered
    x <- input$ui9
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    if (is.null(select_AA))
      return()
    
    V<-VV[which(VVn==select_AA)]; V<-V[[1]]
    v0<-"ui9"
    V1<-V[v0]
    if (x >= MaxBoundary(V1) | x <= MinBoundary(V1) | is.na(x))  {
      js$backgroundCol(v0,"coral")
    } else {
      js$backgroundCol(v0,"")
    }
  })
  
  
  ##########***----RESULTS: THRESHOLDS##########  
  #####-------sformula#####
  sformula <- reactive({
    
    ###Weight Function Calculations
    weigthtfuncttable<-fill.weigthtfuncttable2(WeightFactorAdjustment_kHz=input$ui1, 
                                               ui1w1=input$ui1w1, 
                                               ui1w2=input$ui1w2, 
                                               ui1w3=input$ui1w3, 
                                               ui1w4=input$ui1w4, 
                                               ui1w5=input$ui1w5, 
                                               methods.BroadNarrow=input$methods.BroadNarrow)
    
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    type0=select_AA
    
    ####USE fill.userout function####
    if (type0=="A.1") {
      ######A.1 ######
      SourceLevel_RMS_SPL<-input$ui2
      durationofsoundproduction_hrswithin24hrperiod<-input$ui3
      durationofsoundproduction_sec<-durationofsoundproduction_hrswithin24hrperiod*3600
      LogDurationOfSoundProductionx10<-log10(durationofsoundproduction_sec)*10
      Propogation_xLogR<-input$ui4
      
      userout<-fill.userout(SourceLevel_RMS_SPL=SourceLevel_RMS_SPL, 
                            durationofsoundproduction_hrswithin24hrperiod=durationofsoundproduction_hrswithin24hrperiod,
                            durationofsoundproduction_sec=durationofsoundproduction_sec, 
                            LogDurationOfSoundProductionx10=LogDurationOfSoundProductionx10, 
                            Propogation_xLogR=Propogation_xLogR, 
                            userout.blank0 = userout.blank1,
                            weigthtfuncttable=weigthtfuncttable, 
                            type=type0)
      
    } else if (type0=="A1.1") {
      ###A1.1#######
      SourceLevel_RMS_SPL<-input$ui2
      NoPiles_Within24hr<-input$ui3
      DurationToDriveSinglePile_min<-input$ui4
      durationofsoundproduction_hrswithin24hrperiod<-(NoPiles_Within24hr*DurationToDriveSinglePile_min)*60
      durationofsoundproduction_sec<-durationofsoundproduction_hrswithin24hrperiod
      LogDurationOfSoundProductionx10<-log10(durationofsoundproduction_sec)*10
      Propogation_xLogR<-input$ui5
      distancefromsourcelevelmeasurement_m<-input$ui6
      
      userout<-fill.userout(SourceLevel_RMS_SPL=SourceLevel_RMS_SPL, 
                            durationofsoundproduction_hrswithin24hrperiod=durationofsoundproduction_hrswithin24hrperiod,
                            durationofsoundproduction_sec=durationofsoundproduction_sec, 
                            LogDurationOfSoundProductionx10=LogDurationOfSoundProductionx10, 
                            Propogation_xLogR=Propogation_xLogR, 
                            userout.blank0 = userout.blank1,
                            weigthtfuncttable=weigthtfuncttable, 
                            type=type0, 
                            distancefromsourcelevelmeasurement_m=distancefromsourcelevelmeasurement_m)
      
    } else if (type0=="B.1"){
      ##### B.1####
      SourceLevel_RMS_SPL<-input$ui2
      Activitydurationofsoundproduction_hrswithin24hrperiod<-input$ui3
      PulseDuration_sec<-input$ui4
      InverseRepetitionRate_sec<-input$ui5 #1/Repetition rate^ (seconds) # ^Time between onset of successive pulses.
      DutyCycle<-PulseDuration_sec/InverseRepetitionRate_sec
      durationofsoundproduction_sec<-DutyCycle*3600*Activitydurationofsoundproduction_hrswithin24hrperiod
      LogDurationOfSoundProductionx10<-log10(durationofsoundproduction_sec)*10
      Propogation_xLogR<-input$ui6
      
      userout<-fill.userout(SourceLevel_RMS_SPL=SourceLevel_RMS_SPL, 
                            LogDurationOfSoundProductionx10=LogDurationOfSoundProductionx10, 
                            Propogation_xLogR=Propogation_xLogR, 
                            weigthtfuncttable=weigthtfuncttable, 
                            type=type0,
                            userout.blank0 = userout.blank1)
      
    } else if (type0=="B.2"){
      ##### B.2####
      SourceLevel_RMS_SPL<-input$ui2 #Here (Single Ping/Pulse SEL)
      Activitydurationofsoundproduction_hrswithin24hrperiod<-input$ui3
      NoPulsesIn1hr<-input$ui4
      NoPulsesIn24hr<-NoPulsesIn1hr*Activitydurationofsoundproduction_hrswithin24hrperiod
      LogDurationOfSoundProductionx10<-log10(NoPulsesIn24hr)*10 #Number of Pulses
      Propogation_xLogR<-input$ui5
      
      userout<-fill.userout(SourceLevel_RMS_SPL=SourceLevel_RMS_SPL, 
                            LogDurationOfSoundProductionx10=LogDurationOfSoundProductionx10, 
                            Propogation_xLogR=Propogation_xLogR, 
                            userout.blank0 = userout.blank1,
                            weigthtfuncttable=weigthtfuncttable, 
                            type=type0)
      
    } else if (type0=="C.1"){
      #### C.1####
      SourceLevel_RMS_SPL<-input$ui2
      SourceVelocity_m_sec<-input$ui3
      DutyCycle<-1
      SourceFactor<-(10^(SourceLevel_RMS_SPL/10))*DutyCycle
      
      userout<-fill.userout(SourceLevel_RMS_SPL=SourceLevel_RMS_SPL, 
                            userout.blank0 = userout.blank1,#get(input$usero),
                            weigthtfuncttable=weigthtfuncttable, 
                            type=type0, 
                            SourceFactor = SourceFactor,
                            SourceVelocity_m_sec=SourceVelocity_m_sec)
      
    } else if (type0=="D.1"){
      ####D.1####
      SourceLevel_RMS_SPL<-input$ui2
      SourceVelocity_m_sec<-input$ui3
      PulseDuration_sec<-input$ui4
      InverseRepetitionRate_sec<-input$ui5
      DutyCycle<-PulseDuration_sec/InverseRepetitionRate_sec
      SourceFactor<-(10^(SourceLevel_RMS_SPL/10))*DutyCycle
      
      userout<-fill.userout(SourceLevel_RMS_SPL=SourceLevel_RMS_SPL, 
                            userout.blank0 = userout.blank1,
                            weigthtfuncttable=weigthtfuncttable, 
                            type=type0, 
                            distancefromsourcelevelmeasurement_m=distancefromsourcelevelmeasurement_m, 
                            SourceFactor = SourceFactor,
                            SourceVelocity_m_sec=SourceVelocity_m_sec)
      
    } else if (type0=="D.2"){
      #####D.2####
      SourceLevel_SingleShotSEL=input$ui2
      SourceVelocity_m_sec<-input$ui3
      InverseRepetitionRate_sec<-input$ui4
      SourceFactor<-(10^(SourceLevel_SingleShotSEL/10))/InverseRepetitionRate_sec
      
      userout<-fill.userout(
        userout.blank0 = userout.blank1,
        weigthtfuncttable=weigthtfuncttable, 
        type=type0, 
        SourceFactor = SourceFactor,
        SourceVelocity_m_sec=SourceVelocity_m_sec
      )  
      
    } else if (type0=="E.1"){
      ###E.1####
      SourceLevel_RMS_SPL<-input$ui2
      Activitydurationofsoundproduction_hrswithin24hrperiod<-input$ui3
      PulseDuration_sec<-input$ui4
      InverseRepetitionRate_sec<-input$ui5
      DutyCycle<-PulseDuration_sec/InverseRepetitionRate_sec
      durationofsoundproduction_sec<-(Activitydurationofsoundproduction_hrswithin24hrperiod*3600)*DutyCycle
      LogDurationOfSoundProductionx10<-log10(durationofsoundproduction_sec)*10 #Number of Pulses
      Propogation_xLogR<-input$ui6
      SourceLevel_PK_SPL<-input$ui7
      
      userout<-fill.userout(SourceLevel_RMS_SPL=SourceLevel_RMS_SPL, 
                            durationofsoundproduction_sec=durationofsoundproduction_sec, 
                            LogDurationOfSoundProductionx10=LogDurationOfSoundProductionx10, 
                            Propogation_xLogR=Propogation_xLogR, 
                            userout.blank0 = userout.blank.e, 
                            weigthtfuncttable=weigthtfuncttable, 
                            type=type0, 
                            SourceLevel_PK_SPL=SourceLevel_PK_SPL)      
      
    } else if (type0=="E.2"){
      ####E.2####
      SourceLevel_SingleShotSEL<-input$ui2
      SourceLevel_PK_SPL<-input$ui7
      Activitydurationofsoundproduction_hrswithin24hrperiod<-input$ui3
      NumberOfPulses_1hrs<-input$ui4
      Propogation_xLogR<-input$ui6
      NumberOfPulses_24hrs<-Activitydurationofsoundproduction_hrswithin24hrperiod*NumberOfPulses_1hrs
      LogNumberOfPulsesx10<-log10(NumberOfPulses_24hrs)*10
      
      userout<-fill.userout(
        Propogation_xLogR=Propogation_xLogR, 
        userout.blank0 = userout.blank.e,
        weigthtfuncttable=weigthtfuncttable, 
        LogNumberOfPulsesx10=LogNumberOfPulsesx10,
        SourceLevel_SingleShotSEL=SourceLevel_SingleShotSEL,
        type=type0, 
        SourceLevel_PK_SPL=SourceLevel_PK_SPL)
      
    } else if (type0=="E1.1"){
      #E1.1####
      SourceLevel_RMS_SPL<-input$ui2 
      numberofpiles_24<-input$ui3
      StrikeDuration_sec<-input$ui4 
      NumberOfStrikesPerPile<-input$ui5
      durationofsoundproduction_sec<-numberofpiles_24*NumberOfStrikesPerPile*StrikeDuration_sec
      LogDurationOfSoundProductionx10<-log10(durationofsoundproduction_sec)*10
      Propogation_xLogR<-input$ui6
      SourceLevel_PK_SPL<-input$ui8
      distancefromsourcelevelmeasurement_m_RMS<-input$ui7
      distancefromsourcelevelmeasurement_m_PK<-input$ui9
      SourceLevel_1meter<-SourceLevel_PK_SPL+Propogation_xLogR*log10(distancefromsourcelevelmeasurement_m_PK)
      
      userout<-fill.userout(SourceLevel_RMS_SPL=SourceLevel_RMS_SPL, 
                            durationofsoundproduction_sec=durationofsoundproduction_sec, 
                            LogDurationOfSoundProductionx10=LogDurationOfSoundProductionx10, 
                            Propogation_xLogR=Propogation_xLogR, 
                            userout.blank0 = userout.blank.e,
                            weigthtfuncttable=weigthtfuncttable, 
                            distancefromsourcelevelmeasurement_m_RMS = distancefromsourcelevelmeasurement_m_RMS,
                            distancefromsourcelevelmeasurement_m_PK= distancefromsourcelevelmeasurement_m_PK,
                            type=type0, 
                            SourceLevel_1meter=SourceLevel_1meter,
                            SourceLevel_PK_SPL=SourceLevel_PK_SPL)
      
    } else if (type0=="E1.2"){
      ####E1.2####
      SourceLevel_SingleShotSEL<-input$ui2
      SourceLevel_PK_SPL<-input$ui7
      NumberOfStrikesPerPile<-input$ui3
      numberofpiles_24<-input$ui4
      Propogation_xLogR<-input$ui5
      NumberOfPulses_1hrs<-numberofpiles_24/24
      distancefromsourcelevelmeasurement_m_SEL<-input$ui6
      distancefromsourcelevelmeasurement_m_PK<-input$ui8      
      UnweightedSELcum_atmeasureddistance<-SourceLevel_SingleShotSEL+10*log10(NumberOfStrikesPerPile*numberofpiles_24)
      SourceLevel_1meter<-SourceLevel_PK_SPL+Propogation_xLogR*log10(distancefromsourcelevelmeasurement_m_PK)
      
      userout<-fill.userout(SourceLevel_PK_SPL=SourceLevel_PK_SPL, 
                            Propogation_xLogR=Propogation_xLogR, 
                            userout.blank0 = userout.blank.e,
                            weigthtfuncttable=weigthtfuncttable, 
                            NumberOfPulses_24hrs=NumberOfPulses_24hrs,
                            SourceLevel_RMS_SPL=SourceLevel_SingleShotSEL,
                            type=type0, 
                            SourceLevel_1meter=SourceLevel_1meter,
                            distancefromsourcelevelmeasurement_m_RMS = distancefromsourcelevelmeasurement_m_SEL,
                            distancefromsourcelevelmeasurement_m_PK= distancefromsourcelevelmeasurement_m_PK,
                            UnweightedSELcum_atmeasureddistance=UnweightedSELcum_atmeasureddistance)
      
    } else if (type0=="E2.1"){
      #####E2.1#####
      SourceLevel_RMS_SPL<-input$ui2 
      Activitydurationofsoundproduction_hrswithin24hrperiod<-input$ui3
      ShortDuration_hrswithin24hrs<-input$ui4
      NumberOfDetonations_in1Hr<-input$ui5
      Propogation_xLogR<-input$ui6
      SourceLevel_PK_SPL<-input$ui7
      durationofsoundproduction_sec<-Activitydurationofsoundproduction_hrswithin24hrperiod*NumberOfDetonations_in1Hr*ShortDuration_hrswithin24hrs
      LogDurationOfSoundProductionx10<-log10(durationofsoundproduction_sec)*10
      
      userout<-fill.userout(SourceLevel_RMS_SPL=SourceLevel_RMS_SPL, 
                            SourceLevel_PK_SPL = SourceLevel_PK_SPL,
                            LogDurationOfSoundProductionx10=LogDurationOfSoundProductionx10,
                            Propogation_xLogR=Propogation_xLogR, 
                            userout.blank0 = userout.blank.e2,
                            weigthtfuncttable=weigthtfuncttable, 
                            type=type0)
      
    } else if (type0=="E2.2") {
      ####E2.2#####
      SourceLevel_SingleShotSEL<-input$ui2 #Unless otherwise specified, source levels are referenced 1 m from the source.
      SourceLevel_PK_SPL<-input$ui6
      Activitydurationofsoundproduction_hrswithin24hrperiod<-input$ui3
      Propogation_xLogR<-input$ui5
      NumberOfDetonations_in1Hr<-input$ui4
      NumberOfShots_24hrs<-Activitydurationofsoundproduction_hrswithin24hrperiod*NumberOfDetonations_in1Hr
      LogNumberOfShotsx10<-log10(NumberOfShots_24hrs)*10
      
      userout<-fill.userout(SourceLevel_RMS_SPL=SourceLevel_SingleShotSEL, 
                            SourceLevel_PK_SPL = SourceLevel_PK_SPL,
                            LogDurationOfSoundProductionx10=LogNumberOfShotsx10,
                            Propogation_xLogR=Propogation_xLogR, 
                            userout.blank0 = userout.blank.e2,
                            weigthtfuncttable=weigthtfuncttable, 
                            type=type0)
      
    } else if (type0=="E3.1"){
      #####E3.1#####
      SourceLevel_RMS_SPL<-input$ui2 
      ShotDuration_sec<-input$ui3
      LogDurationOfSoundProductionx10<-log10(ShotDuration_sec)*10
      Propogation_xLogR<-input$ui4
      SourceLevel_PK_SPL<-input$ui5
      
      userout<-fill.userout(SourceLevel_RMS_SPL=SourceLevel_RMS_SPL, 
                            SourceLevel_PK_SPL = SourceLevel_PK_SPL,
                            LogDurationOfSoundProductionx10=LogDurationOfSoundProductionx10,
                            Propogation_xLogR=Propogation_xLogR, 
                            userout.blank0 = userout.blank.e3,#get(input$usero),
                            weigthtfuncttable=weigthtfuncttable, 
                            type=type0)
    } else if (type0=="E3.2"){
      #####E3.2######
      SourceLevel_RMS_SPL<-input$ui2 
      SourceLevel_PK_SPL<-input$ui4
      Propogation_xLogR<-input$ui3
      
      userout<-fill.userout(SourceLevel_RMS_SPL=SourceLevel_RMS_SPL, 
                            SourceLevel_PK_SPL = SourceLevel_PK_SPL,
                            Propogation_xLogR=Propogation_xLogR, 
                            userout.blank0 = userout.blank.e3,
                            weigthtfuncttable=weigthtfuncttable, 
                            type=type0)
    } else if (type0=="F.1"){
      ##F.1###
      SourceLevel_RMS_SPL<-input$ui2 
      SourceVelocity_m_sec<-input$ui3
      SourceLevel_PK_SPL<-input$ui6
      PulseDuration_sec<-input$ui4
      InverseRepetitionRate_sec<-input$ui5
      DutyCycle<-PulseDuration_sec/InverseRepetitionRate_sec
      SourceFactor<-(10^(SourceLevel_RMS_SPL/10))*DutyCycle
      
      userout<-fill.userout(SourceLevel_RMS_SPL=SourceLevel_SingleShotSEL, 
                            SourceLevel_PK_SPL = SourceLevel_PK_SPL,
                            SourceVelocity_m_sec = SourceVelocity_m_sec,
                            userout.blank0 = userout.blank.e,#get(input$usero),
                            SourceFactor=SourceFactor,
                            weigthtfuncttable=weigthtfuncttable, 
                            type=type0)
      
    } else if (type0=="F.2"){
      ##F.2####
      SourceLevel_SingleShotSEL<-input$ui2 
      SourceVelocity_m_sec<-input$ui3
      InverseRepetitionRate_sec<-input$ui4
      SourceLevel_PK_SPL<-input$ui5
      SourceFactor<-(10^(SourceLevel_SingleShotSEL/10))/InverseRepetitionRate_sec
      
      ###User Output: Resultant Isopleths
      userout<-fill.userout(SourceLevel_RMS_SPL=SourceLevel_SingleShotSEL, 
                            SourceLevel_PK_SPL = SourceLevel_PK_SPL,
                            SourceVelocity_m_sec = SourceVelocity_m_sec,
                            userout.blank0 = userout.blank.e,#get(input$usero),
                            SourceFactor=SourceFactor,
                            weigthtfuncttable=weigthtfuncttable, 
                            type=type0)
    }
    return(t(userout))
  })
  
  ####***---RESULTS: Weighting####
  #####-------w4formula#####
  w4formula <- reactive({
    weigthtfuncttable<-fill.weigthtfuncttable2(WeightFactorAdjustment_kHz=input$ui1, 
                                               ui1w1=input$ui1w1, 
                                               ui1w2=input$ui1w2, 
                                               ui1w3=input$ui1w3, 
                                               ui1w4=input$ui1w4, 
                                               ui1w5=input$ui1w5, 
                                               methods.BroadNarrow=input$methods.BroadNarrow)
    
    select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
    
    weigthtfuncttable1<-weigthtfuncttable["Adjustment (dB)",]
    colnames(weigthtfuncttable1)<-c(
      "Low-Frequency cetaceans",
      "Mid-Frequency Cetaceans",
      "High-Frequency Cetaceans",
      "Phocid Pinnipeds",
      "Otariid Pinnipeds")
    
    weigthtfuncttable1<-round(weigthtfuncttable1, digits = 2)
    return(t(as.vector(weigthtfuncttable1)))
  })
  
  output$weight4 <- renderDataTable({
    return(datatable(t(w4formula()), 
                     options = list(pageLength = 10, dom = 'tip', dom='t',ordering=F, paging=F), 
                     rownames = TRUE,
                     escape = FALSE)) 
  }, rownames = TRUE)  
  
  output$weight5warning <- renderUI({
    
    tempweight<-c(as.numeric(as.character(input$ui1w1)), 
                  as.numeric(as.character(input$ui1w2)),
                  as.numeric(as.character(input$ui1w3)),
                  as.numeric(as.character(input$ui1w4)),
                  as.numeric(as.character(input$ui1w5)))
    
    str1<-Step5Warning(tempweight, #Weighting, 
                       BroadNarrow=input$methods.BroadNarrow, 
                       SoundSource=input$SoundSource, 
                       ui1 = input$ui1)
    
    return(HTML(paste(str1, sep = '<br/>')))
    
  })
  
  output$weight4warning <- renderUI({
    str1<-Step4Warning(ui2 = input$ui2, 
                               ui3 = input$ui3, 
                               ui4 = input$ui4, 
                               ui5 = input$ui5, 
                               ui6 = input$ui6, 
                               ui7 = input$ui7, 
                               ui8 = input$ui8, 
                               ui9 = input$ui9)
    
    return(HTML(paste(str1, sep = '<br/>')))
    
  })
  
  
  output$text_calc <- renderDataTable({
    if (is.null(sformula()))
      return()
    return(datatable(t(sformula()), 
                     options = list(pageLength = 10, dom = 'tip', dom='t',ordering=F, paging=F), 
                     rownames = TRUE,
                     escape = FALSE))}, rownames = TRUE)      
  
  #PRINT BUTTON
  observeEvent(input$print, {
    js$winprint()
  })
  
  ###***DOWNLOADABLE CSV OF DATASET####
  output$downloadData <- downloadHandler(
    # filename <- paste0("NOAAAcousticThresholds_", Sys.Date(), ".csv"),
    filename = #function() {
      "downloadData.csv",
    # },
    contentType = "text/csv",
    content = function(file) {
      
      filename0<-file#"downloadData.csv"#file.path(getwd(), "downloadData.csv")
      
      if (input$SoundSource!="Other") {
        meth<-methods.SoundSource
      } else {
        meth<-methods.SoundCatagory
      }
      
      select_AA0<-paste0(#input$zero_submenu, ": ", 
        SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
      spaces<-""
      str1a <- paste0(spaces, input$Client)
      str2a <- paste0(spaces, input$ProjectName)
      str3a <- paste0(spaces, input$ProjectDescription)
      # str41a <- paste0(spaces, names(methods.SoundLevelMetrics[which(methods.SoundLevelMetrics==input$SoundLevelMetrics)]))
      # str41a <- paste0(spaces, names(meth[which(meth==SoundEquation(input$SoundSource, input$SoundCatagory))]))
      str41a <- paste0(spaces, names(meth[which(meth==SoundEquation(input$SoundSource, input$SoundCatagory))]))
      str42a <- paste0(spaces, names(methods.SoundLevelMetrics[which(methods.SoundLevelMetrics==input$SoundLevelMetrics)]))
      str51a <- paste0(spaces, names(which(methods.BroadNarrow==input$methods.BroadNarrow)))
      
      suminputs<-t(data.frame(str1a,str2a,str3a, #str4a,
                              str41a, str42a,#str5a, 
                              str51a, select_AA0))
      rownames(suminputs)<-c("Project Information", 
                             "Project Contact", 
                             "Project/Source Information (Including Assumptions)", 
                             "Sound Source or Source Category",
                             # "Incorporate Auditory Weighting Functions", 
                             "Source Level Metric", 
                             # "Band", 
                             "Weighting", 
                             "Spreadsheet Page Equivalent")
      
      str1 <- "Project Information: "
      str1a <- paste0(spaces, input$Client)
      str2 <- "Project Contact: "
      str2a <- paste0(spaces, input$ProjectName)
      str3 <- "Project/Source Information (Including Assumptions):"
      str3a <- paste0(spaces, "#", input$ProjectNumber)
      str4 <- "The Sound Source or Source Category?:"
      # str4a <- paste0(spaces, names(methods000[(methods00==input$zero_submenu)]))
      str41 <- "Incorporate Auditory Weighting Functions: "
      str41a <- paste0(spaces, names(meth[which(meth==SoundEquation(input$SoundSource, input$SoundCatagory))]))
      
      
      select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
      V<-VV[which(VVn==select_AA)]; V<-V[[1]]
      det<-": "
      str1 <- ifelse(!is.null(input$ui1), paste0(V["ui1"], det), "")
      str1a <- ifelse(!is.null(input$ui1) , input$ui1, "")
      str2 <- ifelse(!is.null(input$ui2), paste0(V["ui2"], det),  "")
      str2a <- ifelse(!is.null(input$ui2) , paste0(input$ui2), "")
      str3 <- ifelse(!is.null(input$ui3), paste0(V["ui3"], det),  "")
      str3a <- ifelse(!is.null(input$ui3) , paste0(input$ui3), "")
      str4 <- ifelse(!is.null(input$ui4), paste0(V["ui4"], det),  "")
      str4a <- ifelse(!is.null(input$ui4) , paste0(input$ui4), "")
      str5 <- ifelse(!is.null(input$ui5), paste0(V["ui5"], det),  "")
      str5a <- ifelse(!is.null(input$ui5) , paste0(input$ui5), "")
      str6 <- ifelse(!is.null(input$ui6), paste0(V["ui6"], det),  "")
      str6a <- ifelse(!is.null(input$ui6) , paste0(input$ui6), "")
      str7 <- ifelse(!is.null(input$ui7), paste0(V["ui7"], det),  "")
      str7a <- ifelse(!is.null(input$ui7) , paste0(input$ui7), "")
      str8 <- ifelse(!is.null(input$ui8), paste0(V["ui8"], det),  "")
      str8a <- ifelse(!is.null(input$ui8) , paste0(input$ui8), "")
      str9 <- ifelse(!is.null(input$ui9), paste0(V["ui9"], det),  "")
      str9a <- ifelse(!is.null(input$ui9) , paste0(input$ui9), "")
      introinfo1<-data.frame(matrix(data=c(str1,str1a, 
                                           str2,str2a, 
                                           str3,str3a, 
                                           str4,str4a, 
                                           str5,str5a, 
                                           str6,str6a, 
                                           str7,str7a, 
                                           str8,str8a, 
                                           str9,str9a), ncol = 2, byrow = T))
      introinfo2<-data.frame(introinfo1[,2])
      introinfo<-(data.frame(introinfo1[,2][which(introinfo1[,2]!="")]))
      rownames(introinfo)<-(introinfo1[which(introinfo1[,2]!=""),1])
      colnames(introinfo)<-c("")
      introinfo<-(introinfo)
      
      ###weight4
      weight4 <- w4formula()
      weight4[,1]<-as.character(weight4)
      
      ###Weight Warning
      tempweight<-c(as.numeric(as.character(input$ui1w1)), 
                    as.numeric(as.character(input$ui1w2)),
                    as.numeric(as.character(input$ui1w3)),
                    as.numeric(as.character(input$ui1w4)),
                    as.numeric(as.character(input$ui1w5)))
      
      step5warning<-Step5Warning(tempweight, #Weighting, 
                                 BroadNarrow=input$methods.BroadNarrow, 
                                 SoundSource=input$SoundSource, 
                                 ui1 = input$ui1, html = F)
      
      ###text_calc
      text_calc0 <- sformula()
      text_calc<-data.frame(matrix(data = NA, 
                                   nrow = nrow(text_calc0), 
                                   ncol = ncol(text_calc0)))
      for(r in 1:nrow(text_calc0)) {
        for(c in 1:ncol(text_calc0)) {
          text_calc[r,c]<-gsub(pattern = "<b>", replacement = "", x = text_calc0[r,c])
          text_calc[r,c]<-gsub(pattern = "</b>", replacement = "", x = text_calc[r,c])
        } 
      }
      
      text_calc_row<-rownames(text_calc0)
      for(c in 1:length(text_calc_row)) {
        text_calc_row[c]<-gsub(pattern = "<b>", replacement = "", x = text_calc_row[c])
        text_calc_row[c]<-gsub(pattern = "</b>", replacement = "", x = text_calc_row[c])
        text_calc_row[c]<-gsub(pattern = "<sub>", replacement = "", x = text_calc_row[c])
        text_calc_row[c]<-gsub(pattern = "</sub>", replacement = "", x = text_calc_row[c])
      } 
      
     rownames(text_calc)<-(text_calc_row)
     colnames(text_calc)<-colnames(text_calc0)
     
      ###text_calc warning
      # select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), 
      #                   input$SoundLevelMetrics)
      # V<-VV[which(VVn==select_AA)]; V<-V[[1]]
      step4warning<-Step4Warning(ui2 = input$ui2, 
                                 ui3 = input$ui3, 
                                 ui4 = input$ui4, 
                                 ui5 = input$ui5, 
                                 ui6 = input$ui6, 
                                 ui7 = input$ui7, 
                                 ui8 = input$ui8, 
                                 ui9 = input$ui9, html = F)
      
      # Set up parameters to pass to Rmd document
      # library(gtools)
      # smartbind(list(
      #   introinfo = introinfo,
      #   suminputs = suminputs,
      #   weight4 = weight4,
      #   howtoweight_band = input$methods.BroadNarrow,
      #   step5warning = step5warning,
      #   text_calc = text_calc,
      #   step4warning = step4warning)
      # )
      
      #Project Information
      write.table("Project Information",
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=FALSE)

      write.table(suminputs,
                  file=filename0,
                  sep=",", row.names=TRUE, col.names=FALSE, append=TRUE)
      
      write.table("", #Space
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      #Input Parameters
      write.table("Input Parameters",
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      write.table(introinfo,
                  file=filename0,
                  sep=",", row.names=TRUE, col.names=FALSE, append=TRUE)
      
      write.table("", #Space
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      # Weighting Function Adjustments (-dB)
      write.table("Weighting Function Adjustments (-dB)",
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      write.table(weight4,
                  file=filename0,
                  sep=",", row.names=TRUE, col.names=TRUE, append=TRUE)
      
      write.table("", #Space
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      # Weighting Function Adjustments (-dB) WARNINGS
      write.table("Weighting Function Adjustments (-dB) Warnings",
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      write.table(step4warning,
                  file=filename0,
                  sep=",", row.names=TRUE, col.names=FALSE, append=TRUE)
      
      write.table("", #Space
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      # Threshold Isopleths Results     
      write.table("Threshold Isopleths Results",
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      write.table(text_calc,
                  file=filename0,
                  sep=",", row.names=TRUE, col.names=TRUE, append=TRUE)
      
      write.table("", #Space
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      # Threshold Isopleths Results WARNINGS    
      write.table("Threshold Isopleths Results Warnings",
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      write.table(step5warning,
                  file=filename0,
                  sep=",", row.names=TRUE, col.names=FALSE, append=TRUE)
      
      write.table("", #Space
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      # DISCLAIMER  
      write.table("DISCLAIMER",
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      write.table("NMFS has provided this Web Calculator as an optional tool to provide estimated effect distances (i.e., isopleths) where PTS onset thresholds may be exceeded. Results provided by this calculator do not represent the entirety of the comprehensive effects analysis, but rather serve as one tool to help evaluate the effects of a proposed action on marine mammal hearing and make findings required by NOAA's various statutes. Input values are the responsibility of the individual user.",
                  file=filename0,
                  sep=",", row.names=TRUE, col.names=FALSE, append=TRUE)
      
      write.table("", #Space
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      # NOTE    
      write.table("NOTE",
                  file=filename0,
                  sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
      
      write.table("This Web Calculator provides a means to estimates distances associated with the Technical Guidance's PTS onset thresholds. Mitigation and monitoring requirements associated with a Marine Mammal Protection Act (MMPA) authorization or an Endangered Species Act (ESA) consultation or permit are independent management decisions made in the context of the proposed activity and comprehensive effects analysis, and are beyond the scope of the Technical Guidance and the Web Calculator.",
                  file=filename0,
                  sep=",", row.names=TRUE, col.names=FALSE, append=TRUE)
      
      write.table("For any comments or questions please contact <amy.scholik@noaa.gov>.",
                  file=filename0,
                  sep=",", row.names=TRUE, col.names=FALSE, append=TRUE)

      
      
      
      
      
      # write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  ###***R MARKDOWN REPORT#####
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    contentType = "text/html",
    
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(getwd(), "report4.Rmd")
      file.copy(from = "report4.Rmd", "report2.Rmd", overwrite = TRUE)
      file.copy("report2.Rmd", tempReport, overwrite = TRUE)
      
      if (input$SoundSource!="Other") {
        meth<-methods.SoundSource
      } else {
        meth<-methods.SoundCatagory
      }
      
      select_AA0<-paste0(#input$zero_submenu, ": ", 
        SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
      spaces<-""
      str1a <- paste0(spaces, input$Client)
      str2a <- paste0(spaces, input$ProjectName)
      str3a <- paste0(spaces, input$ProjectDescription)
      # str41a <- paste0(spaces, names(methods.SoundLevelMetrics[which(methods.SoundLevelMetrics==input$SoundLevelMetrics)]))
      # str41a <- paste0(spaces, names(meth[which(meth==SoundEquation(input$SoundSource, input$SoundCatagory))]))
      str41a <- paste0(spaces, names(meth[which(meth==SoundEquation(input$SoundSource, input$SoundCatagory))]))
      str42a <- paste0(spaces, names(methods.SoundLevelMetrics[which(methods.SoundLevelMetrics==input$SoundLevelMetrics)]))
      str51a <- paste0(spaces, names(which(methods.BroadNarrow==input$methods.BroadNarrow)))
      
      suminputs<-t(data.frame(str1a,str2a,str3a, #str4a,
                              str41a, str42a,#str5a, 
                              str51a, select_AA0))
      rownames(suminputs)<-c("Project Information", 
                             "Project Contact", 
                             "Project/Source Information (Including Assumptions)", 
                             "Sound Source or Source Category",
                             # "Incorporate Auditory Weighting Functions", 
                             "Source Level Metric", 
                             # "Band", 
                             "Weighting", 
                             "Spreadsheet Page Equivalent")
      
      str1 <- paste("<b>", "Project Information: ", "</b>")
      str1a <- paste0(spaces, input$Client)
      str2 <- paste("<b>", "Project Contact: ", "</b>")
      str2a <- paste0(spaces, input$ProjectName)
      str3 <- paste("<b>", "Project/Source Information (Including Assumptions):", "</b>")
      str3a <- paste0(spaces, "#", input$ProjectNumber)
      str4 <- paste("<b>", "The Sound Source or Source Category?:", "</b>")
      # str4a <- paste0(spaces, names(methods000[(methods00==input$zero_submenu)]))
      str41 <- paste("<b>", "Incorporate Auditory Weighting Functions: ", "</b>")
      str41a <- paste0(spaces, names(meth[which(meth==SoundEquation(input$SoundSource, input$SoundCatagory))]))
      
      
      select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), input$SoundLevelMetrics)
      V<-VV[which(VVn==select_AA)]; V<-V[[1]]
      det<-": "
      str1 <- ifelse(!is.null(input$ui1), paste0(V["ui1"], det), "")
      str1a <- ifelse(!is.null(input$ui1) , input$ui1, "")
      str2 <- ifelse(!is.null(input$ui2), paste0(V["ui2"], det),  "")
      str2a <- ifelse(!is.null(input$ui2) , paste0(input$ui2), "")
      str3 <- ifelse(!is.null(input$ui3), paste0(V["ui3"], det),  "")
      str3a <- ifelse(!is.null(input$ui3) , paste0(input$ui3), "")
      str4 <- ifelse(!is.null(input$ui4), paste0(V["ui4"], det),  "")
      str4a <- ifelse(!is.null(input$ui4) , paste0(input$ui4), "")
      str5 <- ifelse(!is.null(input$ui5), paste0(V["ui5"], det),  "")
      str5a <- ifelse(!is.null(input$ui5) , paste0(input$ui5), "")
      str6 <- ifelse(!is.null(input$ui6), paste0(V["ui6"], det),  "")
      str6a <- ifelse(!is.null(input$ui6) , paste0(input$ui6), "")
      str7 <- ifelse(!is.null(input$ui7), paste0(V["ui7"], det),  "")
      str7a <- ifelse(!is.null(input$ui7) , paste0(input$ui7), "")
      str8 <- ifelse(!is.null(input$ui8), paste0(V["ui8"], det),  "")
      str8a <- ifelse(!is.null(input$ui8) , paste0(input$ui8), "")
      str9 <- ifelse(!is.null(input$ui9), paste0(V["ui9"], det),  "")
      str9a <- ifelse(!is.null(input$ui9) , paste0(input$ui9), "")
      introinfo1<-data.frame(matrix(data=c(str1,str1a, 
                                           str2,str2a, 
                                           str3,str3a, 
                                           str4,str4a, 
                                           str5,str5a, 
                                           str6,str6a, 
                                           str7,str7a, 
                                           str8,str8a, 
                                           str9,str9a), ncol = 2, byrow = T))
      introinfo2<-data.frame(introinfo1[,2])
      introinfo<-(data.frame(introinfo1[,2][which(introinfo1[,2]!="")]))
      rownames(introinfo)<-(introinfo1[which(introinfo1[,2]!=""),1])
      colnames(introinfo)<-c("")
      introinfo<-(introinfo)
      
      ###weight4
      weight4 <- w4formula()
      weight4[,1]<-as.character(weight4)
      
      ###Weight Warning
      tempweight<-c(as.numeric(as.character(input$ui1w1)), 
                    as.numeric(as.character(input$ui1w2)),
                    as.numeric(as.character(input$ui1w3)),
                    as.numeric(as.character(input$ui1w4)),
                    as.numeric(as.character(input$ui1w5)))
      
      step5warning<-Step5Warning(tempweight, #Weighting, 
                                 BroadNarrow=input$methods.BroadNarrow, 
                                 SoundSource=input$SoundSource, 
                                 ui1 = input$ui1)
      
      ###text_calc
      text_calc <- sformula()
      
      ###text_calc warning
      # select_AA<-paste0(SoundEquation(input$SoundSource, input$SoundCatagory), 
      #                   input$SoundLevelMetrics)
      # V<-VV[which(VVn==select_AA)]; V<-V[[1]]
      step4warning<-Step4Warning(ui2 = input$ui2, 
                                 ui3 = input$ui3, 
                                 ui4 = input$ui4, 
                                 ui5 = input$ui5, 
                                 ui6 = input$ui6, 
                                 ui7 = input$ui7, 
                                 ui8 = input$ui8, 
                                 ui9 = input$ui9)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        introinfo = introinfo,
        suminputs = suminputs,
        weight4 = weight4,
        howtoweight_band = input$methods.BroadNarrow,
        step5warning = step5warning,
        text_calc = text_calc,
        step4warning = step4warning
      )
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  ###***TAB WEIGHT####
  output$weight2 <- renderDataTable({
    MarineMammalHearingGroup<-matrix(data = c("Low-Frequency (LF) Cetaceans", "Baleen whales", 
                                              "Mid-Frequency (MF) Cetaceans", "Dolphins, toothed whales, beaked whales, bottlenose whales",
                                              "High-Frequency (HF) Cetaceans", "True porpoises, Kogia, river dolphins, <em>cephalorhynchid</em>, <em>Lagenorhynchus cruciger</em>, and <em>L. australis</em>", 
                                              "Phocid Pinnipeds (PW)", "True seals",
                                              "Otariid Pinnipeds (OW)","Sea lions and fur seals"), 
                                     nrow = 5, ncol = 2, byrow = T)
    MarineMammalHearingGroup1<-cbind.data.frame(MarineMammalHearingGroup, 
                                                c("7 Hz to 35 kHz", "150 Hz to 160 kHz", 
                                                  "275 Hz to 160 kHz", "50 Hz to 86 kHz", 
                                                  "60 Hz to 39 kHz"),
                                                c("4.8 kHz and lower", "43 kHz and lower", "59 kHz and lower", 
                                                  "11 kHz and lower", "8.5 kHz and lower"), 
                                                c("Above 4.8 kHz (Use: 1.7 kHz)", 
                                                  "Above 43 kHz (Use: 28 kHz)", 
                                                  "Above 59 kHz (Use: 42 kHz)", 
                                                  "Above 11 kHz (Use: 6.2 kHz)", 
                                                  "Above 8.5 kHz (Use: 4.9 kHz)") 
    )
    colnames(MarineMammalHearingGroup1)<-c("Hearing Group", "Organisms", "Generalized Hearing Range*", 
                                           "Applicable Frequencies", "Non-Applicable Frequencies**")
    return(datatable(MarineMammalHearingGroup1, 
                     rownames = F, 
                     # caption = 'Table 1: Infromation about hearing groups.', 
                     escape = FALSE, 
                     options = list(pageLength = 26, dom = 'tip', dom='t',ordering=F, paging=F)))
  })   
  
  output$weight5 <- renderDataTable({
    infot10<-data.frame("Source"=c("Seismic", "Impact pile driving", "Vibratory pile driving, Down-the-hole drills/harmmers*", "Drilling"),
                        "WFA (kHz)"=c(1, 2, 2.5, 2),
                        "Example Supporting Sources"=c("Breitzke et al. 2008; Tashmukhambetov et al. 2008; Tolstoy et al. 2009",
                                                       "Blackwell 2005; Reinhall and Dahl 2011",
                                                       "Blackwell 2005; Dahl et al. 2015",
                                                       "Greene 1987; Blackwell et al. 2004; Blackwell and Greene 2006")
    )
    colnames(infot10)<-c("Source", "Default WFA (kHz)", "References supporting default WFA")
    return(datatable(infot10, 
                     rownames = T, escape = FALSE, 
                     # catpion = 'Table 1: Suggested (default*) weighting factor adjustments (WFA). NMFS acknowledges default WFAs are likely conservative. *Limited data on spectra associated with down-the-hole drilling/hammering exists. Thus, the WFA for vibratory pile driving is recommended as a surrogate for this source.',
                     # caption = "Table 2: Suggested (default*) weighting factor adjustments (WFA), if input value is unknown for broadband Source:",
                     options = list(pageLength = 26, dom = 'tip', dom='t',ordering=F, paging=F)))
  })
  
  output$WFA_diagram<-renderPlot({    #Recreating plot
    # eq1<-function(x){-29.9248* exp(-18.1016* x)}
    eq2 = function(x){-0.00188883 *x^2 - 0.112352 *x + 0.161179}
    a<-as.data.frame( x= matrix(data = c(
      0.01, -25, 
      0.011, -23,
      0.016, -20,
      0.025, -15,
      0.05, -10,
      # 0.08, -8,
      # 0.09, -9,
      0.1, -6,
      # 0.2, -5, 
      # 0.3, -4,
      # 0.4, -3,
      # 0.5, -2,
      0.6, -1, 
      # 1, -0.1, 
      1, 0,
      3, 0), byrow = T, ncol = 2))
    names(a)<-c("x", "y")
    
    dat<-data.frame(x=c(a$x, seq(.1,150,2)), 
                    y=c(a$y, eq2(seq(.1,150,2))))
    WFADiagram<-ggplot(dat) +
      geom_rect(aes(xmin = 0, xmax = 4, ymin = -Inf, ymax = Inf),
                fill = "green", alpha = 0.003) +
      geom_rect(aes(xmin = 4, xmax = 300, ymin = -Inf, ymax = Inf),
                fill = "red", alpha = 0.003) +
      # geom_line(aes(x = dat$x, y = dat$y), lwd=3, col="gray50") +
      stat_smooth(aes(x = dat$x, y = dat$y), lwd=3, method = lm, formula = y ~ poly(x, 10), se = FALSE) +
      # geom_point(aes(x = dat$x, y = dat$y)) +
      geom_hline(yintercept = 0) +  
      geom_vline(xintercept = 4) +  
      geom_vline(xintercept = 0) +  
      annotate("text", x = .1, y = -15, label = "Appropriate frequencies \n to use WFAs") +
      annotate("text", x = 15.5, y = -15, label = "Inappropriate frequencies \n to use WFAs") +
      ylim(-30, 0)+
      xlim(0, 150) +
      scale_x_log10() +
      ylab("Weighting Function Amplitude (dB)") + 
      xlab("Frequency kHz)") + 
      theme(rect=element_blank(),
            panel.grid = element_blank(),
            panel.background= element_blank(),
            plot.background = element_blank())  + 
      theme_bw() 
    WFADiagram
  }, bg="transparent")
  
  output$WFA <- renderImage({ #https://shiny.rstudio.com/reference/shiny/latest/renderImage.html
    filename <- normalizePath(file.path("./www/WFA.jpg"))
    list(src = filename, 
         width = session$clientData$output_WFA_width,
         height = session$clientData$output_WFA_height
    )
  }, deleteFile = FALSE)
  
  output$weight3 <- renderDataTable({
    MarineMammalHearingGroup<-matrix(data = c("Low-Frequency (LF) Cetaceans", 
                                              "Mid-Frequency (MF) Cetaceans", 
                                              "High-Frequency (HF) Cetaceans", 
                                              "Phocid Pinnipeds (PW)",
                                              "Otariid Pinnipeds (OW)"), 
                                     nrow = 5, ncol = 1, byrow = T)
    MarineMammalHearingGroup1<-cbind(MarineMammalHearingGroup, 
                                     c("0 dB (to make unweighted)", 
                                       "-0.05 dB (weighting function amplitude based on 25 kHz WFA)", 
                                       "-0.53 dB (weighting function amplitude based on 25 kHz WFA)", 
                                       "0 dB (to make unweighted)", 
                                       "0 dB (to make unweighted)"))
    MarineMammalHearingGroup1<-as.data.frame(MarineMammalHearingGroup1)
    colnames(MarineMammalHearingGroup1)<-c("Hearing Group", "Applicable Frequencies")
    return(datatable(MarineMammalHearingGroup1, 
                     rownames = F, escape = FALSE, 
                     caption = 'Table 3: An example of the weighting function amplitude (dB).', 
                     options = list(pageLength = 26, dom = 'tip', dom='t',ordering=F, paging=F)))
  })
  
  output$infot1 <- renderDataTable({
    infot10<-data.frame("Source"=c("Seismic", "Impact pile driving", "Vibratory pile driving", "Drilling"),
                        "WFA (kHz)"=c(1, 2, 2.5, 2),
                        "Example Supporting Sources"=c("Breitzke et al. 2008; Tashmukhambetov et al. 2008; Tolstoy et al. 2009",
                                                       "Blackwell 2005; Reinhall and Dahl 2011",
                                                       "Blackwell 2005; Dahl et al. 2015",
                                                       "Greene 1987; Blackwell et al. 2004; Blackwell and Greene 2006")
    )
    colnames(infot10)<-c("Source", "WFA (kHz)", "Example Supporting Sources")
    return(datatable(infot10, 
                     rownames = T, escape = FALSE, 
                     options = list(pageLength = 26, dom = 'tip', dom='t',ordering=F)))
  })
  
  ######***TAB GLOSSARY########
  output$gloss <- renderDataTable({
    
    gl<-matrix(data = c(
      "95% Frequency contour percentile", "Upper frequency below which 95% of total cumulative energy is contained (Charif et al. 2010).",
      "Accumulation period", "The amount of time a sound accumulates for the SEL<sub>cum</sub> metric.",
      "Acoustic center", "Point from which outgoing wavefronts appear to diverge in the acoustic far field under free-field conditions (ISO 2017).", 
      "Acoustic far field", "Spatial region in a uniform medium where the direct-path field amplitude, compensated by absorption loss, varies inversely with range (ISO 2017).",
      "Auditory weighting function", "Auditory weighting functions take into account what is known about marine mammal hearing sensitivity and susceptibility to noise-induced hearing loss and can be applied to a sound-level measurement to account for frequency-dependent hearing (i.e., an expression of relative loudness as perceived by the ear) (Southall et al. 2007; Finneran 2016). Specifically, this function represents a specified frequency-dependent characteristic of hearing sensitivity in a particular animal, by which an acoustic quantity is adjusted to reflect the importance of that frequency dependence to that animal (ISO 2017). Similar to OSHA (2013), marine mammal auditory weighting functions in this document are used to reflect the risk of noise exposure on hearing and not necessarily capture the most sensitive hearing range of every member of the hearing group.",
      "Bandwidth", "Bandwidth (Hz or kHz) is the range of frequencies over which a sound occurs or upper and lower limits of frequency band (ANSI 2005). Broadband refers to a source that produces sound over a broad range of frequencies (for example, seismic airguns), while narrowband or tonal sources produce sounds over a more narrow frequency range, typically with a spectrum having a localized a peak in amplitude (for example, sonar) (ANSI 1986; ANSI 2005).",
      "Broadband", "See 'bandwidth.'",
      "Continuous sound", "A sound whose sound pressure level remains above ambient sound during the observation period (ANSI 2005).",
      "Decibel (dB)", "One-tenth of a bel. Unit of level when the base of the logarithm is the tenth root of ten, and the quantities concerned are proportional to power (ANSI 2013).",
      "Frequency", "The number of periods occurring over a unit of time (unless otherwise stated, cycles per second or hertz) (Yost 2007).",
      "Hertz (Hz)", "Unit of frequency corresponding to the number of cycles per second. One hertz corresponds to one cycle per second. ",
      "Impulsive sound", "Sound sources that produce sounds that are typically transient, brief (less than 1 second), broadband, and consist of high peak sound pressure with rapid rise time and rapid decay (ANSI 1986; NIOSH 1998; ANSI 2005). They can occur in repetition or as a single event. Examples of impulsive sound sources include explosives, seismic airguns, and impact pile driving hammers.",
      "Intermittent sound", "Interrupted levels of low or no sound (NIOSH 1998) or bursts of sounds separated by silent periods (Richardson and Malme 1993). Typically, intermittent sounds have a more regular (predictable) pattern of bursts of sounds and silent periods (i.e., duty cycle). ",
      "Isopleth", "A line drawn through all points having the same numerical value. In the case of sound, the line has equal sound pressure or exposure levels.",
      "Mean-square sound pressure", "Integral over a specified time interval of squared sound pressure divided by the duration of the time interval, for a specified frequency range (ISO 2017).",
      "Narrowband", "See 'bandwidth.'",
      "Non-impulsive sound", "Sound sources that produce sounds that can be broadband, narrowband or tonal, brief or prolonged, continuous or intermittent) and typically do not have a high peak sound pressure with rapid rise time that impulsive sounds do. Examples of non-impulsive sound sources include marine vessels, machinery operations/construction (e.g., drilling), certain sonar (e.g. tactical, navigational, and scientific), vibratory pile driving hammers, and down-the-hole drills/hammers.",
      "Octave", "The interval between two sounds having a basic frequency ratio of two (Yost 2007). For example, one octave above 400 Hz is 800 Hz. One octave below 400 Hz is 200 Hz.",
      "Omnidirectional", "Receiving or transmitting signals in all directions (i.e., variation with direction is designed to be as small as possible).",
      "One-third octave (base 10)", "The frequency ratio corresponding to a decidecade or one tenth of a decade (ISO 2017).",
      "Peak sound pressure (ppk)", "Greatest magnitude of the sound pressure during a specified time interval, for a specified frequency range. Can arise from a positive or negative sound pressure (ISO 2017). ",
      "Peak sound pressure level (PK; re: 1 μPa)", "Twenty times the logarithm of the base 10 of the ratio of peak sound pressure, ppk, to the specified reference value, p0, in decibels (ISO 2017).",
      "Permanent threshold shift (PTS)", "A permanent, irreversible increase in the threshold of audibility at a specified frequency or portion of an individual’s hearing range above a previously established reference level. The amount of permanent threshold shift is customarily expressed in decibels (ANSI 1995; Yost 2007). Available data from humans and other terrestrial mammals indicate that a 40 dB threshold shift approximates PTS onset (see Ward et al. 1958, 1959; Ward 1960; Kryter et al. 1966; Miller 1974; Ahroon et al. 1996; Henderson et al. 2008).",
      "Power spectral density", "the distribution of acoustic power into frequency components composing that signal.",
      "Pulse duration", "For impulsive sources, window that makes up 90% of total cumulative energy (5%-95%) (Madsen 2005).",
      "Propagation loss (PL)", "Difference between source level in a specified direction and root mean square sound pressure level at a specified position (ISO 2017). Note: Propagation loss is conceptually different from transmission loss (i.e., propagation loss is associated with the source level, while transmission loss is associated with a measurement at a specified distance).",
      "Received level", "The level of sound at a specified distance of interest, r,(i.e., at the animal or receiver). Not: Received level is conceptually different from source level (i.e., different quantities with different reference values). ",
      "Repetition rate", "Number of pulses of a repeating signal in a specific time unit, normally measured in pulses per second.",
      "Root-mean-square sound pressure level (<i>L</i><sub>rms</sub>; re: 1 μPa)", "Ten times the logarithm to the base 10 of the ratio of the mean-square sound pressure to the specified reference value in decibels (ISO 2017).",
      "Sound Exposure Level (SEL<sub>cum</sub>; re: 1μPa<sup>2</sup> s)", "A measure of sound level that takes into account the duration of the signal. Ten times the logarithm to the base 10 of the ratio of time-integrated squared sound pressure to the specified reference value in decibels (ISO 2017). A measure of sound level that takes into account the duration of the signal. ",
      "Sound Pressure Level (SPL)", "A measure of sound level that represents only the pressure component of sound. Ten times the logarithm to the base 10 of the ratio of time-mean square pressure of a sound in a stated frequency band to the square of the reference pressure (1 μPa in water) (ANSI 2013). ",
      "Source Factor", "Product of the square of distance from the acoustic center of a source, in a specified direction, and mean-square sound pressure in the acoustic far field that distance. ",
      "Source Level (SL)", "Sound pressure level measured in a given radian direction, corrected for absorption, and scaled to a reference distance (Morfey 2001). Ten times the logarithm to the base 10 of the ratio of the source factor to the specified reference value in decibels (ISO 2017). Note: Source level is conceptually different from received level (i.e., different quantities with different reference values).",
      "Spectral/spectrum", "Of or relating to frequency component(s) of sound. The spectrum of a function of time is a description of its resolution into components (frequency, amplitude, etc.). The spectrum level of a signal at a particular frequency is the level of that part of the signal contained within a band of unit width and centered at a particular frequency (Yost 2007).",
      "Time-integrated squared sound pressure", "Integral of the square of sound pressure over a specified time interval or event, for a specified frequency range (ISO 2017).",
      "Transmission Loss (TL)", "Reduction in a specified level between two specified points that are within an underwater acoustic field (ISO 2017). Note: Transmission loss is conceptually different from propagation loss (i.e., propagation loss is associated with the source level, while transmission loss is associated with a measurement at a specified distance)."), 
      ncol = 2, byrow = T)
    colnames(gl)<-c("Phrase", "Definition")
    gl<-datatable(gl, options = list(pageLength = 50, dom = 'tip', dom='t',ordering=F, paging=F), 
                  rownames = FALSE,
                  # caption = 'Table 2: Defined terms used in web tool.', 
                  escape = FALSE)
  })
  
  output$acronyms<-renderDataTable({
    # ABBREVIATIONS, ACRONYMS, AND SYMBOLS
    ac<-matrix(data = c(
      # 'ANSI',	'American National Standards Institute',
      #'<em>b</em>',	'High-frequency exponent',
      #'BOEM',	'Bureau of Ocean Energy Management',
      #'<em>C</em>',	'Weighting function gain (dB)',
      'dB',	'Decibel',
      # '<em>f1</em>',	'Low-frequency cutoff (kHz)',
      # '<em>f2</em>',	'High-frequency cutoff (kHz)',
      'ESA',	'Endangered Species Act',
      # 'G&G',	'Geological and Geophysical',
      'h',	'Hour',
      # 'H',	'Water Depth',
      'HF',	'High-frequency',
      'Hz',	'Hertz',
      # 'in<sup>3</sup>',	'Cubic Inches',
      # 'ISO',	'International Organization for Standardization', 
      # '<em>K</em>',	'Exposure function gain (dB)',
      'kHz',	'Kilohertz',
      'LF',	'Low-frequency',
      'm',	'meter',
      'MF',	'Mid-frequency',
      'MMPA', 'Marine Mammal Protection Act',
      'msec', 	'Milliseconds',
      'NMFS',	'National Marine Fisheries Service',
      'NOAA',	'National Oceanic and Atmospheric Administration',
      # 'OTO', 	'One-third octave levels',
      'OW',	'Otariids in water',
      'Pa',	'Pascals',
      'PK',	'Peak sound pressure level',
      # 'PSD',	'Power spectral density levels',
      'PTS',	'Permanent Threshold Shift',
      'PW',	'Phocids in water',
      'R',	'Range from source',
      '<i>L</i><sub>RMS</sub>',	'Root Mean Square',
      'SEL',	'Sound exposure level',
      'SEL<sub>cum</sub>',	'Cumulative sound exposure level',
      'SL',	'Source Level',
      'SPL',	'Sound Pressure Level',
      'TTS',	'Temporary Threshold Shift',
      # '<em>W<sub>aud</sub>(f)</em>',	'Auditory weighting function',
      'WFA',	'Weighting factor adjustments'), 
      ncol = 2, byrow = T)
    colnames(ac) = c('Abbreviation', 'Description')
    ac<-datatable(ac, options = list(pageLength = 26, dom = 'tip', dom='t',ordering=F, paging=F), 
                  rownames = FALSE,
                  # caption = 'Table 1: Abbreviation used in web tool.', 
                  escape = FALSE)
    ac
  })
  
  output$infot2 <- renderTable({
    lr<-data.frame(c(
      "Ahroon, W.A., R.P. Hamernik, and S.-F., Lei. 1996. The effects of reverberant blast waves on the auditory system. Journal of the Acoustical Society of America 100:2247-2257.",
      "ANSI (American National Standards Institute). 1986. Methods of Measurement for Impulse Noise (ANSI S12.7-1986). New York: Acoustical Society of America.",
      "ANSI (American National Standards Institute). 1995. Bioacoustical Terminology (ANSI S3.20-1995).New York: Acoustical Society of America. ",
      "ANSI (American National Standards Institute). 2005. Measurement of Sound Pressure Levels in Air (ANSI S1.13-2005). New York: Acoustical Society of America.",
      "ANSI (American National Standards Institute). 2013. Acoustic Terminology (ANSI S1.1- 2013). New York: Acoustical Society of America.",
      "Blackwell, S.B. 2005. Underwater Measurements of Pile Driving Sounds during the Port MacKenzie Dock Modifications, 13-16 August 2004. Juneau, Alaska: Federal Highway Administration.",
      "Blackwell, S.B., and C.R. Green, Jr. 2006. Sounds from an oil production island in the Beaufort Sea in summer: Characteristics and contribution of vessels. Journal of the Acoustical Society of America 119: 182-196.",
      "Blackwell, S.B., C.R. Greene, Jr., and W.J. Richardson. 2004. Drilling and operational sounds from an oil production island in the ice-covered Beaufort Sea. Journal of the Acoustical Society of America 116: 3199-3211.",
      "Breitzke, M., O. Boebel, S. El Naggar, W. Jokat, and B. Werner. 2008. Broad-band calibration of marine seismic sources used by R/V Polarstern for academic research in polar regions. Geophysical Journal International 174: 505-524.",
      "Charif, R.A., A.M. Waack, and L.M. Strickman. 2010. Raven Pro 1.4 User's Manual. Ithaca, New York: Cornell Lab of Ornithology.",
      "Dahl, P.H., D.R. Dall'Osto, and D.M. Farrell. 2015. The underwater sound field from vibratory pile driving. Journal of the Acoustical Society of America 137: 3544-3554.",
      "EPA (Environmental Protection Agency). 1982. Guidelines for Noise Impact Analysis (EPA Report Number 550/9-82-105).Washington, D.C.: Office of Noise Abatement and Control.",
      "Finneran, J.J. 2016. Auditory weighting functions and TTS/PTS exposure functions for cetaceans and marine carnivores, Technical Report 3026. December 2016. San Diego, California: SPAWAR Systems Center Pacific.",
      "Greene, R. 1987. Characteristics of oil industry dredge and drilling sounds in the Beaufort Sea. Journal of the Acoustical Society of America 82: 1315-1324.",
      "ISO (International Organization for Standardization. 2017. Underwater Acoustics-Terminology, ISO 18405. Geneva, Switzerland: International Organization for Standardization.",
      "Kryter, K.D., W.D. Ward, J.D. Miller, and D.H. Eldredge. 1966. Hazardous Exposure to Intermittent and Steady-State Noise. Journal of the Acoustical Society of America 39:451-464.",
      "Madsen, P.T. 2005. Marine mammals and noise: Problems with root mean square sound pressure levels for transients. Journal of the Acoustical Society of America 117: 3952-3957.",
      "Miller, J.D. 1974. Effects of noise on people. Journal of the Acoustical Society of America 56:729 764.",
      "Morfey, C.L. 2001. Dictionary of Acoustics. New York: Academic Press.",
      "NIOSH (National Institute for Occupational Safety and Health). 1998. Criteria for a recommended standard: Occupational noise exposure. Cincinnati, Ohio: United States Department of Health and Human Services.",
      "OSHA (Occupational Safety & Health Administration). 2013. OSHA Technical Manual. Washington, D.C.: United States Department of Labor.",
      "Reinhall, P.G., and P.H. Dahl. 2011. Underwater Mach wave radiation from impact pile driving: Theory and observation. Journal of the Acoustical Society of America 130: 1209-1216.",
      "Richardson, W.J., and C.I. Malme. 1993. Man-made noise and behavioral responses. Pages 631-700. In Burns, J.J., J.J. Montague, and C.J. Cowles, eds. The Bowhead Whale. The Society for Marine Mammalogy, Special Publication Number 2.",
      "Richardson, W.J., C.R. Greene, Jr., C.I. Malme, and D.H. Thomson. 1995. Marine mammals and noise. New York: Academic Press.",
      "Southall, B.L., A.E. Bowles, W.T. Ellison, J.J. Finneran, R.L. Gentry, C.R. Greene, Jr., D. Kastak, D.R. Ketten, J.H. Miller, P.E. Nachtigall, W.J. Richardson, J.A. Thomas, and P.L. Tyack. 2007. Marine mammal noise exposure criteria: Initial scientific recommendations. Aquatic Mammals 33: 411-521.",
      "Tashmukhambetov, A.M., G.E. Ioup, J.W. Ioup, N.A. Sidorovskaia, and J.J. Newcomb. 2008. Three-dimensional seismic array characterization study: Experiment and modeling. Journal of the Acoustical Society of America 123: 4094-4108.",
      "Tolstoy, M., J. Diebold, L. Doermann, S. Nooner, S.C. Webb, D.R. Bohnenstiehl, T.J. Crone, and R.C. Holmes. 2009. Broadband calibration of the R/V Marcus G. Langseth four-string seismic sources. Geochemistry Geophysics Geosystems 10: 1-15.",
      "Urick, R.J. 1983. Principles of Underwater Sound. New York, New York: McGraw-Hill Book Company.",
      "Ward, W.D. 1960. Recovery from high values of temporary threshold shift. Journal of the Acoustical Society of America 32: 497-500.",
      "Ward, W.D., A. Glorig, and D.L. Sklar. 1958. Dependence of temporary threshold shift at 4 kc on intensity and time. Journal of the Acoustical Society of America 30: 944-954. ",
      "Ward, W.D., A. Glorig, and D.L. Sklar. 1959. Temporary threshold shift from octave-band noise: Application to damage-risk criteria. Journal of the Acoustical Society of America 31: 522-528.",
      "Yost, W.A. 2007. Fundamentals of Hearing: An Introduction. New York: Academic Press."
    ))
    colnames(lr)<-""
    return(lr)
  })
}

###### Run the application ####
# app<-
  #runApp(
shinyApp(ui = ui, server = server)
#, launch.browser = TRUE)
# app

