MWplottofile <- function(variable) {
    png(file="mygraphic.png",width=800,height=450)
    MWplot(variable)
    dev.off()
}

## .N counts variables (might need to use data.table instead of data frame)
## df alternative table(df$x)

MWplot <- function(variable, controlcol, controlval) {
    
    ## Read variable data
    ## ***** try quote="" to remove quotes from a few values *****
    colnames <- c("timestamp"
                  ,"orgsize"
                  ,"years"
                  ,"numsites"
                  ,"privacy"
                  ,"users"
                  ,"version"
                  ,"updates"
                  ,"extensions"
                  ,"purpose"
                  ,"coreimprovements"
                  ,"communityimprovements"
                  ,"contributions"
                  ,"barriers")
    df <- read.csv("MWSurvey.csv"
                   , colClasses = "factor"
                   , col.names = colnames
                   , na.strings=c(""," ","NA"))
    if(!is.null(controlcol)){
        split_df <- split(df, df[[controlcol]])
        df <- split_df[[controlval]] 
    }
    
    orgsize <- factor(df[,2],
                      levels = c(#"Not sure",
                                 "More than 500 people"
                                 ,"100-500 people"
                                 ,"25-100 people"
                                 ,"Fewer than 25 people"
                                 ,"Just Me"
                      ), ordered=TRUE)
    years <- factor(df[,3],
                      levels = c("5+ years"
                                 ,"3-5 years"
                                ,"1-2 years"
                                ,"Less than 6 months"
                      ), ordered=TRUE)
    numsites <- factor(df[,4],
                      levels = c("More than 10"
                                 ,"5-10"
                                 ,"2-5"
                                 ,"1"
                                 ,"0"
                      ), ordered=TRUE)
    privacy <- factor(df[,5],
                      levels = c("Mix of Both"
                                 ,"Private"
                                 ,"Public"
                      ), ordered=TRUE)
    users <- factor(df[,6],
                      levels = c("More than 500 people"
                                 ,"100-500 people"
                                 ,"25-100 people"
                                 ,"Fewer than 25 people"
                                 ,"Just me"
                      ), ordered=TRUE)
    version <- factor(df[,7])
    updates <- factor(df[,8],
                      levels = c("Every security release"
                                 ,"Every minor point release"
                                 ,"Every major release"
                                 ,"Every 3 months"
                                 ,"Every 6 months"
                                 ,"OS Packaged Version" ## (e.g. Debian, Redhat)"
                                 ,"When needed"
                      ), ordered=TRUE)
    
    ## Choose col based on "variable"
    ## Check that variable is valid
    col <- NULL
    switch(variable,
           "orgsize" = { col <- 2 #"How.large.is.your.organization."
                rightMargin <- 7
                mainTitle <- "Organization Size"
                plotdataset <- orgsize
           }
           ,"years" = { col <- 3 #"How.long.have.you.been.using.MediaWiki."
                rightMargin <- 5
                mainTitle <- "Years Using MW"
                plotdataset <- years
           }
           ,"numsites" = { col <- 4 #"How.many.MediaWiki.sites.do.you.manage."
                rightMargin <- 3
                mainTitle <- "# of MW Sites Managed"
                plotdataset <- numsites
           }
           ,"privacy" = { col <- 5 #"Are.your.sites.public.or.private."
                rightMargin <- 2
                mainTitle <- "Site Privacy"
                plotdataset <- privacy
           }
           ,"users" = { col <- 6 #"Approximately.how.many.people.are.there.on.your.MediaWiki.sites."
                rightMargin <- 7
                mainTitle <- "# of Users"
                plotdataset <- users
           }
           ,"version" = { col <- 7 #"What.MediaWiki.Version.are.you.currently.using."
                rightMargin <- 38
                mainTitle <- "MW Version in Use"
                plotdataset <- version
           }
           ,"updates" = { col <- 8 #"Which.MediaWiki.update.cycle.is.closest.to.what.you.use."
                rightMargin <- 15
                mainTitle <- "MW Update Cycle"
                plotdataset <- updates
           }
           , stop("invalid variable") ## default
    )
    

    legend.text = levels(plotdataset)
    
    #png(file="mygraphic.png",width=800,height=450)
    #par(xpd = T, mar = par()$mar + c(0,0,7,0)
    par(xpd = T, mar = par()$mar + c(0,rightMargin,-2,0)
        , las = 1 ## las = 2 makes labels turn 90 degrees
    )
    ## consider angled axis labels 
    ## http://stackoverflow.com/questions/10286473/rotating-x-axis-labels-in-r-for-barplot
    
    ## the plot
    na.omit(barplot(table(plotdataset), legend.text = FALSE 
            ##,col=c("red","orange","yellow","green","blue","violet","pink","black")
            ,col = heat.colors(length(legend.text))
            ,horiz = TRUE
            ,main = mainTitle
            #,space=1
    ))
    
#     legend(x = 0
#            ,y = max(table(df[,col]))
#            ,xjust = 0
#            ,yjust = 0
#            ,legend.text
#            ##,fill=c("red","orange","yellow","green","blue","violet","pink","black")
#            ,fill=heat.colors(length(legend.text))
#     )
    par(mar=c(5, 4, 4, 2) + 0.1)
    #

}

MWplotall <- function() {
    png(file="MWallplots.png",width=1600,height=1600,res=140)

#     if(!is.null(control)){switch(control,
#            "orgsize" = { controlcol <- 2 #"How.large.is.your.organization."
#            }
#            ,"years" = { controlcol <- 3 #"How.long.have.you.been.using.MediaWiki."
#            }
#            ,"numsites" = { controlcol <- 4 #"How.many.MediaWiki.sites.do.you.manage."
#            }
#            ,"privacy" = { controlcol <- 5 #"Are.your.sites.public.or.private."
#            }
#            ,"users" = { controlcol <- 6 #"Approximately.how.many.people.are.there.on.your.MediaWiki.sites."
#            }
#            ,"version" = { controlcol <- 7 #"What.MediaWiki.Version.are.you.currently.using."
#            }
#            ,"updates" = { controlcol <- 8 #"Which.MediaWiki.update.cycle.is.closest.to.what.you.use."
#            }
#            , stop("invalid variable") ## default
#     )}
    
    #par(mfrow=c(3,1))
    
#     layout(matrix(c(1,2,3,4,6,5,6,7), 4, 2, byrow = TRUE) 
#            , widths=c(2.5,1.5)#, heights=c(1,2)
#     )
    layout(matrix(c(1,2,3,4,5,7,6,6,6,6,6,6), 6, 2, byrow = TRUE) 
           #, widths=c(2.5,1.5)#, heights=c(1,2)
    )
    
    MWplot("orgsize")   #1
    MWplot("years")     #2
    MWplot("numsites")  #3
    MWplot("privacy")   #4
    MWplot("users")     #5
    MWplot("version")   #6
    MWplot("updates")   #7
    par(mfrow=c(1,1))
    
    dev.off()
}



MWplotconstrained <- function(control, valuename) {
    
    filenamebase <- "MWallplotsconstrained-"
    filename <- paste( filenamebase
                       , control
                       , '-'
                       , valuename
                       , '.png'
                       , sep='' )
    png(file=filename
        ,width=1600,height=1600,res=140)
    
    controlcol <- control #The data column on which to split
    #   "orgsize" = 2 #"How.large.is.your.organization."
    #   ,"years" = 3 #"How.long.have.you.been.using.MediaWiki."
    #   ,"numsites" = 4 #"How.many.MediaWiki.sites.do.you.manage."
    #   ,"privacy" = 5 #"Are.your.sites.public.or.private."
    #   ,"users" = 6 #"Approximately.how.many.people.are.there.on.your.MediaWiki.sites."
    #   ,"version" = 7 #"What.MediaWiki.Version.are.you.currently.using."
    #   ,"updates" = 8 #"Which.MediaWiki.update.cycle.is.closest.to.what.you.use."
    
    controlval <- valuename   #The value of that column to plot
    
    #par(mfrow=c(3,1))
    
    layout(matrix(c(1,2,3,4,5,7,6,6,6,6,6,6), 6, 2, byrow = TRUE) )
    
    MWplot("orgsize", controlcol, controlval)   #1
    MWplot("years", controlcol, controlval)     #2
    MWplot("numsites", controlcol, controlval)  #3
    MWplot("privacy", controlcol, controlval)   #4
    MWplot("users", controlcol, controlval)     #5
    MWplot("version", controlcol, controlval)   #6
    MWplot("updates", controlcol, controlval)   #7
    par(mfrow=c(1,1))
    
    dev.off()
    
}



MWplotallconstrained <- function(control="orgsize") {
    
    # names of values for each of multiple plots for one constraint
    valuename <- switch(control,
    "orgsize" = c(#"Not sure",
                "More than 500 people"
                ,"100-500 people"
                ,"25-100 people"
                ,"Fewer than 25 people"
                ,"Just Me"
                ),
    "years" = c("5+ years"
            ,"3-5 years"
            ,"1-2 years"
            ,"Less than 6 months"
            ),
    "numsites" = c("More than 10"
                ,"5-10"
                ,"2-5"
                ,"1"
                ,"0"
                ),
    "privacy" = c("Mix of Both"
               ,"Private"
               ,"Public"
               ),
    "users" = c("More than 500 people"
            ,"100-500 people"
            ,"25-100 people"
            ,"Fewer than 25 people"
            ,"Just me"
            ),
    "updates" = c("Every security release"
            ,"Every minor point release"
            ,"Every major release"
            ,"Every 3 months"
            ,"Every 6 months"
            ,"OS Packaged Version" ## (e.g. Debian, Redhat)"
            ,"When needed"
            )
    )

    for(i in seq_along(valuename)){
        MWplotconstrained(control, valuename[i])
    }
    
}




MWcompare <- function(variable, num = "best") {
        
    ## Choose col based on "variable"
    ## Check that variable is valid
    col <- NULL
    switch(variable,
           "org size" = { col <- 2 #"How.large.is.your.organization."
           }
           ,"years" = { col <- 3 #"How.long.have.you.been.using.MediaWiki."
           }
           ,"num sites" = { col <- 4 #"How.many.MediaWiki.sites.do.you.manage."
           }
           ,"privacy" = { col <- 5 #"Are.your.sites.public.or.private."
           }
           ,"users" = { col <- 6 #"Approximately.how.many.people.are.there.on.your.MediaWiki.sites."
           }
           ,"version" = { col <- 7 #"What.MediaWiki.Version.are.you.currently.using."
           }
           ,"updates" = { col <- 8 #"Which.MediaWiki.update.cycle.is.closest.to.what.you.use."
           }
           , stop("invalid variable") ## default
    )
    
    ## Read variable data
    df <- read.csv("MWSurvey.csv", colClasses = "factor")

    split_df <- split(df, df$State) # a list
    
    hospital <- sapply(split_df, function(x){
        ## change "Not Available" to NA in order to use functions on NA
        for(i in 1:nrow(x)){
            if(x[i,col] == "Not Available"){
                x[i,col] = NA
            }
        }
        
        ## order hospitals by rank, cut NAs
        rate <- as.numeric(x[,col])
        ranked <- x[order(rate,x[,"Hospital.Name"], na.last=NA),]
        
        ## deal with num cases
        rank <- vector('numeric')
        num_rows <- nrow(ranked)
        if(num == "best"){ 
            rank <- 1
        } else if(num == "worst"){
            rank <- num_rows
        } else {
            rank <- num
        }
        
        ## output result
        if(rank > num_rows){
            #print("NA")
            NA
        } else {
            #print(ranked[rank,"Hospital.Name"])
            ranked[rank,"Hospital.Name"]
        }
        
    }) #end lapply
    state <- names(hospital)
    results <- data.frame(cbind(hospital,state))
    results
}
