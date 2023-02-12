#' Percentage of missing for each variable
#'  
#' @details
#' Percentage of missing for each variable of a data frame.
#' @param X a data frame
#' @param info2 a data frame with two variables named c("COLUMN_NAME","CONSTRAINT_TYPE")
#' @return a data frame
#' @examples
#' X=cars
#' for(i in 1:40){
#'   X[sample(1:50,1,replace=TRUE),sample(1:2,1,replace=TRUE)]<-NA}
#' missing.summary(X)

missing.summary<-function(X,info2=NULL){
  if(!is.data.frame(X)){X<-as.data.frame(X)}
  A<-plyr::ldply(.data = X, function(x){
    c("missing percentage"=round(mean(is.na(x)),2),
      "blank percentage"=round(mean(try(as.character(x))==""&!is.na(x)),2),
      "zero percentage"=round(mean(x==0&!is.na(x)),2),
      "missing count"=sum(is.na(x)),
      "blank count"=sum(try(as.character(x))=="",na.rm=T),
      "zero count"=sum(x==0,na.rm=T))})
  names(A)[1]<-"COLUMN_NAME"
  if(!is.null(info2)){A<-merge(A,info2[c("COLUMN_NAME","CONSTRAINT_TYPE")],all.x=TRUE)}
  A[order(A[[2]]),]
}

#' Summary for each variable in table.
#'  
#' @details
#' @param .data a data frame
#' @return a list
#' @examples
#' data(cars)
#' var.summary(cars)
var.summary<-function(.data){
  require(ggplot2)
  L<-lapply(names(.data),function(x){
    nl=length(unique(.data[[x]]))
    is.code=try(identical(names(table(table(.data[[x]]))),"1"))
    if(is.element("try-error",class(is.code))){is.code=F}
    list(nlevels=nl,
         is.code=is.code,
         levels=if(nl<300&!is.code){sort(unique(.data[[x]]))}else{sort(unique(.data[[x]])[1:300])},
         counts=if(nl<300&!is.code){table(.data[x],useNA="ifany")}else{NULL},
         densityplot=if(is.numeric(.data[[x]])&nl>30){ggplot2::ggplot(data.frame(x=.data[[x]]),aes(x))+xlab(x)+ geom_density(show.legend = TRUE)}else{NULL},
         hist=if(nl<30){(function(){xx=data.frame(zz=.data[[x]]);ggplot2::qplot(data=xx,x=zz,xlab=x)})()}else{NULL},
         summary=summary(.data[x]))
    })
  names(L)<-names(.data)
  L
  }


#' get data about a file on the server.
#' @details 
#' @param tablename
#' @return a list
#' 
automaticdatafConnect<-function(tablename,
                         folder=getwd(),
                         schema=NULL, 
                         dicoT=NULL, 
                         splitvar=NULL,
                         Connect=NULL,
                         Connectf=NULL,
                         alwaysexclude=NULL){
  if(is.null(Connect)){Connect<-Connectf()}
  variables<-setdiff(sqlColumns(Connect,schema = schema,sqtable = tablename)$COLUMN_NAME,alwaysexclude)
  n<-sqlQuery(Connect,paste0("select count(*) from ",schema,".",tablename))[[1]]
  rate<-round(10000000/n,2)
  X<-sqlQuery(Connect,
              paste0("select ",paste(variables,collapse=",")," from ",schema,".",tablename,if(n>100000){paste0(" SAMPLE(",rate,",1)")}else{character(0)}))
  pk<-sqlQuery(Connect,paste0("SELECT cols.table_name, cols.column_name, cols.position, cons.status, cons.owner
                              FROM all_constraints cons, all_cons_columns cols
                              WHERE cols.table_name = '",tablename,"'
                              AND cons.constraint_type = 'P'
                              AND cons.constraint_name = cols.constraint_name
                              AND cons.owner = cols.owner
                              AND cols.OWNER='",schema,"'
                              ORDER BY cols.table_name, cols.position;"))$COLUMN_NAME
  close(Connect)
  

  
  automaticdata<-
    list(nrow=n,
         variables=variables,
         tab1_variables=dicoT[dicoT$TABLE==tablename,],
         pk=pk,
         splitvar=NULL,
         varsum=var.summary(X),
         missingsum=missing.summary(X),
         missinggraph=ggplot_missing(X,reordonne=T)+coord_flip(),
         missinggraph2=if(any(is.element(splitvar,variables))){
           ggplot_missing2(X,keep=setdiff(splitvar,variables),reordonne=T)+coord_flip()}else{NULL})
  
  automaticdatafile<-file.path(folder,paste0("study_",schema,"_",tablename,"_automatic.rda"))
  save(automaticdata,file=automaticdatafile)
  automaticdatafile
}



#'
#'  
#' @details
#' @param tableA
#' @param tablename
#' @param schema
#' @param dicoT
#' @param splitvar
#' @param alwaysexclude
#' @return 
#' @examples
#' 
#' 
automaticdataf<-function(tablename,tableA=get(tablename),
                                folder=getwd(),
                                schema=NULL, 
                                dicoT=NULL, 
                                splitvar=NULL,
                                alwaysexclude=NULL){
  
  variables<-setdiff(names(tableA),alwaysexclude)
  n<-nrow(tableA)
  splitvar=NULL
#  if(n>1000){tableA<-tableA[sample(n,1000),]}
  automaticdata<-
    list(nrow=n,
         variables=variables,
         tab1_variables=dicoT[dicoT$TABLE==tablename,],
         pk=NULL,
         splitvar=NULL,
         varsum=var.summary(tableA),
         missingsum=missing.summary(tableA),
         missinggraph=ggplot_missing(tableA,reordonne=T)+coord_flip(),
         missinggraph2=if(any(is.element(splitvar,variables))){
           ggplot_missing2(X,keep=setdiff(splitvar,variables),reordonne=T)+coord_flip()
           }else{NULL})
  
  automaticdatafile<-file.path(folder,paste0("study_",schema,"_",tablename,"_automatic.rda"))
  save(automaticdata,file=automaticdatafile)
  automaticdatafile
}



#'
#'  
#' @details
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @return 
#' @examples
#' 
#' 

lefichier<-function(x){if(file.exists(x)){x}else{NULL}}

#' Creates RMD files for all datasets in a specific folder.
#'  
#' @details
#' @param tablename
#' @param folder
#' @param specialprogram
#' @param specialreport
#' @param specialdatafile
#' @param automaticdatafile
#' @param replace
#' @param rerunspecial
#' @param schema
#' @param dicoT
#' @param splitvar
#' @return 
#' @examples
#' tablename="cars"
#' automaticRmd(tablename)
#' 
automaticRmd<-function(tablename,
                       folder=getwd(),
                       schema=NULL,
                       specialprogram   =lefichier(file.path(folder,paste0("study_",schema,"_",tablename,"_special.R"))),
                       specialreport    =lefichier(file.path(folder,paste0("study_",schema,"_",tablename,"_special.Rmd"))),
                       specialdatafile  =lefichier(file.path(folder,paste0("study_",schema,"_",tablename,"_special.rda"))),
                       automaticdatafile=file.path(folder,paste0("study_",schema,"_",tablename,"_automatic.rda")),
                       replace=TRUE,
                       rerunspecial=FALSE,
                       dicoT=NULL,
                       splitvar=NULL,
                       alwaysexclude=NULL,
                       dico=function(){NULL},
                       author=""){
  
  automaticdatafile<-automaticdataf(tablename=tablename,folder=folder,schema=schema,splitvar=splitvar,dicoT=dicoT)
  load(automaticdatafile)
  variables<-automaticdata$variables
  missinggraphfigheight<-length(variables)/5
    
  rm(automaticdata)
  if(replace||!file.exists(file.path(folder,paste0("study_",schema,"_",tablename,"_automatic.Rmd")))){
    
    specialtexte<-if(!is.null(specialreport)){readLines(specialreport)}else{character(0)}
    texte<-paste0(
'---
title: "Study ',tablename,'"
author: "',author,'"
output: github_document
---

```{r setup, include=FALSE, warnings=FALSE, error=FALSE,results="hide"}
knitr::opts_chunk$set(echo = TRUE)
```

```{r r1, echo=FALSE,message=FALSE, warnings=FALSE, error=FALSE,results="hide",include=FALSE}
library("StudyDataTools")
library(printr)
library(knitr)
load("',automaticdatafile,'")
try(load("',specialdatafile,'"))
```


# Quick facts
The number of rows is `r automaticdata$nrow`.
`r if(!is.null(automaticdata$pk)){paste0("The primary keys are ",paste(automaticdata$pk,collapse=", "))}`.
The number of variables  is `r length(automaticdata$variables)`.

```{r listofvar, echo=FALSE,message=FALSE, warnings=FALSE, error=FALSE,results="hide",include=FALSE}
dicoT=dico()   
tab1<-dicoT[dicoT$TABLE=="',tablename,'",]
```
',
paste(specialtexte,collapse="\n")
,'

#Details

`r 
if(automaticdata$nrow>100000){"The following is based on a sample"}else{character(0)}`

## Missing values
``` {r missingsummary,echo=FALSE}
kable(automaticdata$missingsum)
```

``` {r missinggraph,echo=FALSE,message=FALSE, warnings=FALSE, error=FALSE,fig.height=',  missinggraphfigheight,'}
print(automaticdata$missinggraph)
if(!is.null(automaticdata$missinggraph2)){try(print(automaticdata$missinggraph2))}
```

## Variables summary

'
  )  
    
    dicoTsource<-if(!is.null(dicoT)){dicoT$COLUMN_NAME[dicoT$TABLE==tablename]}else{setdiff(variables,alwaysexclude)}
    dicoTsource<-setdiff(dicoTsource,'')  
    texte<-paste0(texte,do.call(paste0,lapply(setdiff(variables,alwaysexclude), function(variable){
      paste0("### ",variable,"
Information: 
```{r, echo=FALSE,message=FALSE,warnings=FALSE}
if(!is.null(dicoT)){try(kable(dicoT[dicoT$TABLE=='",tablename,"'&dicoT$COLUMN_NAME=='",variable,"',]))}
```
Number of levels is `r automaticdata$varsum[['",variable,"']][['nlevels']]`

Table of frequencies:
```{r,echo=FALSE,message=FALSE,warnings=FALSE}
if(!is.null(unlist(automaticdata$varsum[['",variable,"']][['counts']]))){
Frequencies<-as.data.frame(automaticdata$varsum[['",variable,"']][['counts']])
names(Frequencies)<-c('Value','Frequency')
if(nrow(Frequencies)>20){
Frequencies<-rbind(Frequencies[1:19,],
                  data.frame(Value='Other',
                             Frequency=sum(Frequencies$Frequencies[20:(nrow(Frequencies)-1)])),
                  Frequencies[nrow(Frequencies),])}
kable(Frequencies)}
```

`r if(!is.null(automaticdata$varsum[['",variable,"']][['densityplot']])){'Density plot'}`

```{r,echo=FALSE}
x=automaticdata$varsum[['",variable,"']][['densityplot']]
if(!is.null(x)){print(x)}
```

`r if(!is.null(automaticdata$varsum[['",variable,"']][['hist']])){'Histogram'}`

```{r,echo=FALSE,message=FALSE, warnings=FALSE, error=FALSE}
x=automaticdata$varsum[['",variable,"']][['hist']]
if(!is.null(x)){print(x+coord_flip())}
```

")
      
    })))
    Rmdfile<-file.path(folder,paste0("study_",schema,"_",tablename,"_automatic.Rmd"))
    cat(texte,file=Rmdfile)
    rmarkdown::render(input = Rmdfile,
                      output_file = paste0("study_",schema,"_",tablename,"_automatic.md"),
                      output_dir = folder)
  }
}

#'@examples
#'tableA<-cars
#'tableB<-BOD

Compare2TablesRmd<-function(tablename1,tablename2,tableA,tableB,
                       folder=getwd(),
                       specialprogram   =lefichier(file.path(folder,paste0("study_",schema,"_",tablename,"_special.R"))),
                       specialreport    =lefichier(file.path(folder,paste0("study_",schema,"_",tablename,"_special.Rmd"))),
                       specialdatafile  =lefichier(file.path(folder,paste0("study_",schema,"_",tablename,"_special.rda"))),
                       automaticdatafile=file.path(folder,paste0("study_",schema,"_",tablename,"_automatic.rda")),
                       replace=FALSE,
                       rerunspecial=FALSE,
                       schema=NULL,
                       dicoT=NULL,
                       Connectf=NULL,
                       splitvar=NULL){
  allvariables<-union(names(tableA),names(tableB))
  if(length(setdiff(names(tableB),names(tableA)))>0){tableA[[setdiff(names(tableB),names(tableA))]]<-NA}
  if(length(setdiff(names(tableA),names(tableB)))>0){tableB[[setdiff(names(tableA),names(tableB))]]<-NA}
  tableA$tablename<-tablename1
  tableB$tablename<-tablename2
  tableAB<-rbind(tableA[allvariables],tableB[allvariables])
  
  automaticdatafileA<-automaticdataf(tablename,tableA,folder,schema=schema,splitvar=splitvar,dicoT=dicoT,Connectf=Connectf)
  load(automaticdatafile)
  variables<-automaticdata$variables
  rm(automaticdata)
  if(replace||!file.exists(file.path(folder,paste0("study_",schema,"_",tablename,"_automatic.Rmd")))){
    
    specialtexte<-if(!is.null(specialreport)){readLines(specialreport)}else{character(0)}
    texte<-paste0(
      '---
title: "Study ',tablename,'"
author: "Daniel Bonnery"
      
output:
  html_document:
    df_print: paged
---
      
      ```{r setup, include=FALSE, warnings=FALSE, error=FALSE,results="hide"}
      knitr::opts_chunk$set(echo = TRUE)
      ```
      
      ```{r r1, echo=FALSE,message=FALSE, warnings=FALSE, error=FALSE,results="hide",include=FALSE}
      library("BigSyn")
      library(printr)
      library(knitr)
      load("',automaticdatafile,'")
      try(load("',specialdatafile,'"))
      ```
      
      #Summary: 
      
      ## Quick facts
      The number of rows is `r automaticdata$nrow`.
      
      The primary keys are `r paste(automaticdata$pk,collapse=", ")`.
      
      The number of potentially interesting variables  is `r length(automaticdata$variables)`.
      
      ```{r listofvar, echo=FALSE,message=FALSE, warnings=FALSE, error=FALSE,results="hide",include=FALSE}
      #dicoT=dico()   
      tab1<-dicoT[dicoT$TABLE=="',tablename,'",]
      ```
      
      ## Key findings:
      
      ## Questions:
      
      Posted on the tracking spreadsheet: 
      
      ## Recommandations:
      
      Keep:
      
      Move:
      
      Replace:
      
      Drop:
      ',
      paste(specialtexte,collapse="\n")
      
      ,'
      ## Percentage of missing
      
      
      #Details
      
      `r 
      if(automaticdata$nrow>100000){"The following is based on a sample"}else{character(0)}`
      
      ## Missing values
      ``` {r missingsummary,echo=FALSE}
      kable(automaticdata$missingsum)
      ```
      
      ``` {r missinggraph,echo=FALSE,message=FALSE, warnings=FALSE, error=FALSE}
      print(automaticdata$missinggraph)
      if(!is.null(automaticdata$missinggraph2)){try(print(automaticdata$missinggraph2))}
      ```
      
      ## Variables summary
      
      '
    )  
    
    dicoTsource<-if(!is.null(dicoT)){dicoT$COLUMN_NAME[dicoT$TABLE==tablename]}else{setdiff(variables,alwaysexclude)}
    dicoTsource<-setdiff(dicoTsource,'')  
    texte<-paste0(texte,do.call(paste0,lapply(setdiff(variables,alwaysexclude), function(variable){
      paste0("### ",variable,"
             Information: 
             ```{r, echo=FALSE}
             try(kable(dicoT[dicoT$TABLE=='",tablename,"'&dicoT$COLUMN_NAME=='",variable,"',]))
             ```
             
             Number of levels is `r automaticdata$varsum['nlevels','",variable,"']`
             
             Table of frequencies:
             ```{r,echo=FALSE}
             if(!is.null(unlist(automaticdata$varsum['counts','",variable,"']))){
             kable(as.data.frame(automaticdata$varsum['counts','",variable,"'][[1]]))}
             ```
             
             Density plot
             ```{r,echo=FALSE}
             print(automaticdata$varsum['densityplot','",variable,"'])
             ```
             
             Histogram
             ```{r,echo=FALSE,message=FALSE, warnings=FALSE, error=FALSE}
             print(automaticdata$varsum['hist','",variable,"']+coord_flip())
             ```
             
             ")
      
    })))
    Rmdfile<-file.path(folder,paste0("study_",schema,"_",tablename,"_automatic.Rmd"))
    cat(texte,file=Rmdfile)
    rmarkdown::render(input = Rmdfile,
                      output_file = paste0("study_",schema,"_",tablename,"_automatic.html"),
                      output_dir = folder)
  }
  }


