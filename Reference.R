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
library(writexl)
library(knitr)
library(markdown)
# #https://stackoverflow.com/questions/33499651/rmarkdown-in-shiny-application

library(shinythemes)

# install.packages("devtools")
# devtools::install_github("Appsilon/shiny.router")
# library(shiny.router)
library(shinydashboard)

# install.packages("shiny.router")
# library(shiny.router)

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
